library(shiny)
library(shinydashboard)
library(magick)
library(ggplot2)

# Rinse
rm(list=ls())
source("utils.R")

##### UI ===================================================
ui_sidebar <- dashboardSidebar(
  sidebarMenu(
    id="tabs",
    menuItem("Files",      tabName = "files", icon = icon("picture", lib="glyphicon")),
    fileInput("upload", "New image", accept = c('image/png', 'image/jpeg')),
    menuItem("Retouching", tabName="lab",    icon = icon("eye-open", lib="glyphicon"))
  )
)

ui_body <- dashboardBody(
  tabItems(
    ### FILES TAB ##########################################
    tabItem("files",
            h3("Files"),
            # Button to pick a folder
            shinyDirButton('directory', 'Folder select', 'Select the folder containing images'),
            # Current file
            h3("Current file:"),
            verbatimTextOutput("active_file"),
            br(),

            # Button to navigate files
            actionButton("minus", icon("chevron-left")),
            actionButton("plus",  icon("chevron-right")),

            # Display selected folder
            textOutput("dir"),
            DTOutput("lf")
    ),

    ### LAB TAB ############################################
    tabItem("lab",
            # Images =======================================
            fluidRow(
              column(width = 6,
                     h3("Original image"),
                     imageOutput("img", width="50%", height="50%")
              ),
              column(width = 6,
                     h3("Edited image"),
                     imageOutput("img2", width="50%", height="50%"))
            ),

            # Retouching ===================================
            h3("Retouching"),
            fluidRow(
              # Channels and thresholding ------------------
              column(width=4,
                     selectInput("channel", "", choices = useful_channels, selected = "All channels"),
                     sliderInput("threshold", "Threshold", -1, 256, -1)
              ),
              # Colors adjustments ---------------------------
              column(width=4,
                     sliderInput("brightness", "Brightness", 0, 200, 100, 1),
                     sliderInput("saturation", "Saturation", 0, 200, 100, 1),
                     sliderInput("hue", "Hue", 0, 200, 100, 1)
              ),
              # Transform ----------------------------------
              column(width = 4,
                     # sliderInput("height", "Rescale", min=1, value = 1, max = 1, step = 1),
                     sliderInput("rotation", "Rotate", 0, 360, 0, step = 1, round=TRUE),
                     sliderInput("reducenoise", "Denoise", 0, 50, 0),
                     checkboxGroupInput("effects", "Effects",
                                        choices = list("negate", "flip", "flop"))
              )
            )
    )
  )
)
# ui_body <- dashboardBody(
#   tabItems(
#     # Files tab
#     tabItem(tabName = "files",
#             h2("Files"),
#             # Button to pick a folder
#             shinyDirButton('directory', 'Folder select', 'Select the folder containing images'),
#
#             # Current file
#             h3("Current file:"),
#             verbatimTextOutput("active_file"),
#             br(),
#
#             # Button to navigate files
#             actionButton("minus", icon("chevron-left")),
#             actionButton("plus",  icon("chevron-right")),
#
#             # Display selected folder
#             textOutput("dir"),
#             DTOutput("lf")
#     ),
#     # More tab
#     tabItem(tabName = "retouching",
#             h2("Retouching"),
#             p("yooo")
#     )
#   )
# )

# Put them together into a dashboardPage
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Momoshop"),
  ui_sidebar,
  ui_body
)
#
# ui <- dashboardPage(
#   skin = "black",
#   # dashboardHeader(title=tags$img(src='Momoshop_sticker.png', height="40px", align="left")),
#   dashboardHeader(title="Momoshop"),
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Files", tabName = "files", icon = icon("picture", lib="glyphicon")),
#       menuItem("Retouching", tabName = "retouch", icon = icon("eye-open", lib="glyphicon"))
#     ),
#     # File handling ----------------------------------------
#     fileInput("upload", "New image", accept = c('image/png', 'image/jpeg')),
#     actionButton("reload", "Reload", icon=icon("refresh", lib="glyphicon")),
#     # actionButton("write", "Write", icon=icon("floppy-o")),
#     # Post production using magick =========================
#     sidebarMenu(
#       id = "tabs",
#       # Channels and thresholding --------------------------
#       menuItem("Channels and threshold", icon = icon("menu-hamburger", lib="glyphicon"),
#                selectInput("channel", "Channels", choices = useful_channels, selected = "All channels"),
#                sliderInput("threshold", "Threshold", -1, 256, -1)),
#       # Colors ---------------------------------------------
#       menuItem("Colors", icon = icon("cog", lib="glyphicon"),
#                sliderInput("brightness", "Brightness", 0, 200, 100, 1),
#                sliderInput("saturation", "Saturation", 0, 200, 100, 1),
#                sliderInput("hue", "Hue", 0, 200, 100, 1)
#       ),
#       # Transform ---------------------------------------------
#       menuItem("Transform", icon = icon("wrench", lib="glyphicon"),
#                # sliderInput("height", "Rescale", min=1, value = 1, max = 1, step = 1),
#                sliderInput("rotation", "Rotate", 0, 360, 0, step = 1, round=TRUE),
#                sliderInput("reducenoise", "Denoise", 0, 50, 0),
#                checkboxGroupInput("effects", "Effects",
#                                   choices = list("negate", "flip", "flop"))
#       )
#     )
#   ),
#   dashboardBody(
#     # If an image is loaded, then output some info
#     fluidRow(
#       column(width = 6,
#              h3("Original image"),
#              imageOutput("img", width="50%", height="50%")
#       ),
#       column(width = 6,
#              h3("Edited image"),
#              imageOutput("img2", width="50%", height="50%"))
#     ),
#
#     fluidRow(
#       column(12,
#              h3("Debug"),
#              # h4("Histogram"),
#              plotOutput("histogram"),
#              h4("info"),
#              verbatimTextOutput("info"),
#              h4("upload"),
#              verbatimTextOutput("upload"),
#              h4("pipe_cat"),
#              verbatimTextOutput("pipe_cat"),
#              h4("env"),
#              verbatimTextOutput("env"),
#              h4("show_inputs"),
#              tableOutput('show_inputs')
#       )
#     )
#   )
# )


##### Server ===============================================


server <- function(input, output, session) {

  output$res <- renderText({
    paste("You've selected:", input$tabs)
  })



  ### _____Files_____ ######################################
  # select a folder
  volumes <- c(volumes="~")
  shinyDirChoose(input, 'directory', roots=volumes, session=session)

  # print selected folder
  output$dir <- renderPrint({
    if (length(input$directory)){
      cat(paste("Working in", parseDirPath(volumes, input$directory)))

    } else {
      cat("None folder selected")
    }
  })

  # reactive data.frame for images and associated .mom
  lf <- reactive({
    dir <- parseDirPath(volumes, input$directory)

    # if dir has not been selected yet
    if (length(dir)==0)
      return(NULL)

    # list all files in dir
    lf <- list.files(dir, full.names=TRUE, rec=TRUE)
    # images among them
    lf_img <- grep("\\.(jpg)|(png)$", lf, value=TRUE)
    # .mom among them
    lf_mom <- grep("\\.mom$", lf, value=TRUE)

    # prepare a df to store this
    df <- data.frame(name=trim_both(lf_img),
                     path=lf_img,
                     mom=FALSE,
                     stringsAsFactors = FALSE)

    # img with a .mom file receive a TRUE
    img_with_mom <- match(trim_both(lf_mom), trim_both(lf_img))
    df$mom[img_with_mom] <- TRUE

    # handling edited files
    # img_with_edited <- x[x %in% trim_suffix(x)]

    # finally return the table
    df
  })

  # output lf() with DT
  output$lf <- renderDT({
    if (is.null(lf()))
      return(NULL)
    # # prepare the color vector for active file
    # active_color <- rep("white", nrow(lf()))
    # active_color[active_file_id()] <- "red"
    # now render the df
    lf() %>%
      datatable(filter = 'top',
                options = list(autoWidth=TRUE, pageLength = 50)) %>%
      formatStyle('path', 'mom',
                  backgroundColor = styleEqual(c(0, 1), c('gray90', 'greenyellow')))
  }
  )

  # active image id ----------------------------------------
  active_file_id <- reactiveVal(1)

  observeEvent(input$minus, {
    new_file <- active_file_id() - 1
    # prevent below 1 values
    if (new_file < 1)
      new_file <- 1
    active_file_id(new_file)
  })

  observeEvent(input$plus, {
    new_file <- active_file_id() + 1
    # prevent above nrow values
    if (!is.null(lf()) && new_file > nrow(lf()))
      new_file <- nrow(lf())
    active_file_id(new_file)
  })

  # active image -------------------------------------------
  active_file <- reactive({
    if (is.null(lf()))
      return(NULL)
    df <- lf()
    image <<- df$path[active_file_id()]
    df$path[active_file_id()]
  })

  output$active_file <- renderText(
    active_file()
  )



  ### _____Lab______ #######################################
  pipe <- "" # to trigger pipe and downstream reactives
  # Pipe construction --------------------------------------

  # build a reactive pipe
  pipe <- reactive({
    "image %>% " %>%
      pipe_cond_append("negate" %in% input$effects,
                       "image_negate() %>%") %>%
      pipe_cond_append("flip" %in% input$effects,
                       "image_flip() %>%") %>%
      pipe_cond_append("flop" %in% input$effects,
                       "image_flop() %>%") %>%
      pipe_cond_append(input$channel != "All channels",
                       "image_channel('", input$channel, "') %>%") %>%
      pipe_cond_append(input$threshold>-1,
                       "image_threshold(", input$threshold, ") %>%") %>%
      pipe_cond_append(input$reducenoise>0,
                       "image_reducenoise(", input$reducenoise, ") %>% ") %>%
      pipe_cond_append(input$rotation!=0,
                       "image_rotate(", input$rotation, ") %>% ") %>%
      pipe_cond_append(any(c(input$brightness,input$saturation, input$hue)!=100),
                       "image_modulate(", input$brightness, ",", input$saturation, ",", input$hue, ") %>%") #%>%
    # pipe_cond_append(input$height != info$height,
    # "image_resize('", paste(input$height, round(info$width*(input$height/info$height)), sep="x"), "') %>% ")
  })

  output$pipe_cat <- renderPrint(
    pipe() %>% pipe_cat()
  )


  # Image uploading ----------------------------------------
  # Start with placeholder img
  image <- image_read("www/empty.png")

  # reset values when reload
  reset_values <- function(){
    updateCheckboxGroupInput(session, "effects", selected = "")
    updateSliderInput(session, "height", value = info$height, max=info$height*2)
    updateSelectInput(session, "channel", selected="All channels")
    updateSliderInput(session, "rotation", value = 0)
    updateSliderInput(session, "brightness", value = 100)
    updateSliderInput(session, "saturation", value = 100)
    updateSliderInput(session, "hue", value = 100)
    updateCheckboxGroupInput(session, "effects", choices=NULL)
    updateSliderInput(session, "threshold", value=-1)
    updateSliderInput(session, "reducenoise", value=0)
  }

  globalize_when_new_image <- function(){
    if (length(input$upload$datapath)>0){
      path <<- input$upload$datapath
      image <<- image_read(path)
      info <<- image_info(image)
    }

    # image <<- active_file() %>% image_read()
    # info <<- image_info(image)

  }

  # Uploading an image
  observeEvent(input$upload, {
    globalize_when_new_image()
    reset_values()
    updateSliderInput(session, "height", min=1, value = info$height, max = info$height*2)
  })

  # Reloading an image
  observeEvent(input$upload, {
    globalize_when_new_image()
    reset_values()
    # updateSliderInput(session, "height", min=1, value = info$height, max = info$height*2)
  })


  # Image info ----------------------------------------
  output$info <- renderText({
    if (length(input$upload$datapath)){
      paste("Original image: ",
            info$format,
            info$colorspace,
            "-",
            paste0("(", format(structure(info$filesize, class="object_size"), units="auto"), ")"),
            paste(info$width, info$height, sep=" x "))
    } else {
      "No image loaded"
    }
  })

  output$upload <- renderPrint(input$upload)

  # output$histogram <- renderPlot({
  #   pipe() %>%
  #     paste(collapse=" ") %>%
  #     gsub("%>% *$", "", .) %>%
  #     parse(text=.) %>%
  #     eval() %>%
  #     image_histogram()
  # })

  # Image rendering --------------

  # original image
  output$img <- renderImage({
    if (length(input$upload$datapath)==0){
      image <- image_read("www/empty.png")
    }
    image %>%
      image_convert("jpeg") %>%
      image_write(tempfile(fileext='.jpg'), format = 'jpg') -> tmpfile
    list(src = tmpfile, contentType = "image/jpeg")
  })

  # output$img2 <- renderImage({
  #   tmpfile2 <- pipe() %>%
  #     pipe_append("image_write(tempfile(fileext='.jpg'), format = 'jpg')") %>%
  #     paste(collapse=" ") %>%
  #     # gsub("%>% *$", "", .) %>%
  #     parse(text=.) %>%
  #     eval()
  #   # browser()
  #   list(src = tmpfile2, contentType = "image/jpeg")
  # })

  edited <- reactive({
    tmpfile2 <- pipe() %>%
      pipe_append("image_write(tempfile(fileext='.jpg'), format = 'jpg')") %>%
      paste(collapse=" ") %>%
      # gsub("%>% *$", "", .) %>%
      parse(text=.) %>%
      eval()
    # browser()
    list(src = tmpfile2, contentType = "image/jpeg")
  })

  output$img2 <- renderImage({
    edited()
  })


}

shinyApp(ui, server)
