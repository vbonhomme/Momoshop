library(shiny)
library(shinydashboard)
library(magick)

# Rinse
rm(list=ls())
source("utils.R")


ui <- dashboardPage(
  skin = "black",
  # dashboardHeader(title=tags$img(src='Momoshop_sticker.png', height="40px", align="left")),
  dashboardHeader(title="Momoshop"),
  dashboardSidebar(
    # File handling ----------------------------------------
    fileInput("upload", "New image", accept = c('image/png', 'image/jpeg')),
    actionButton("reload", "Reload", icon=icon("repeat")),
    # actionButton("write", "Write", icon=icon("floppy-o")),
    # Post production using magick =========================
    sidebarMenu(
      id = "tabs",
      # Channels and thresholding --------------------------
      menuItem("Channels and threshold", icon = icon("align-justify"),
               selectInput("channel", "Channels", choices = useful_channels, selected = "All channels"),
               sliderInput("threshold", "Threshold", -1, 256, -1)),
      # Colors ---------------------------------------------
      menuItem("Colors", icon = icon("tasks"),
               sliderInput("brightness", "Brightness", 0, 200, 100, 1),
               sliderInput("saturation", "Saturation", 0, 200, 100, 1),
               sliderInput("hue", "Hue", 0, 200, 100, 1)
      ),
      # Transform ---------------------------------------------
      menuItem("Transform", icon = icon("wrench"),
               # sliderInput("height", "Rescale", min=1, value = 1, max = 1, step = 1),
               sliderInput("rotation", "Rotate", 0, 360, 0, step = 1, round=TRUE),
               sliderInput("reducenoise", "Denoise", 0, 50, 0),
               checkboxGroupInput("effects", "Effects",
                                  choices = list("negate", "flip", "flop"))
      )
    )
  ),
  dashboardBody(
    # If an image is loaded, then output some info
    fluidRow(
      column(width = 6,
             h3("Original image"),
             imageOutput("img", width="50%", height="50%")
      ),
      column(width = 6,
             h3("Edited image"),
             imageOutput("img2", width="50%", height="50%"))
    ),

    fluidRow(
      column(12,
             h3("Debug"),
             hr(),
             h4("info"),
             verbatimTextOutput("info"),
             h4("upload"),
             verbatimTextOutput("upload"),
             h4("pipe_cat"),
             verbatimTextOutput("pipe_cat"),
             h4("env"),
             verbatimTextOutput("env"),
             h4("show_inputs"),
             tableOutput('show_inputs')
      )
    )
  )
)



server <- function(input, output, session) {

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

  # Image rendering --------------

  # A plot of fixed size
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
