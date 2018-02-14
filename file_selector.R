# Dependencies -----
library(tidyverse)
library(magick)

# shiny related
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(DT)

# domestic functions
# remove path, keep filename
trim_path <- function(x){
  x %>% strsplit("/") %>% sapply(function(.x) .x[length(.x)])
}

# remove extension, keep filename
trim_ext <- function(x){
  x %>% gsub("\\.[[:alnum:]]{3}$", "", .)
}

# remove path and extension, keep filename
trim_both <- function(x){
  x %>% trim_path() %>% trim_ext()
}

## ui.R ##
ui_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Files", tabName = "files", icon = icon("file-photo-o")),
    menuItem("Widgets", icon = icon("th"), tabName = "more")
  )
)

ui_body <- dashboardBody(
  tabItems(
    # Files tab
    tabItem(tabName = "files",
            h2("Files"),
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
    # More tab
    tabItem(tabName = "more",
            h2("...")
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Folder select"),
  ui_sidebar,
  ui_body
)

server <- function(input, output, session) {
  # Files ==================================================
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
    df$path[active_file_id()]
  })

  output$active_file <- renderText(
    active_file()
  )
  # end Files ==============================================
}

shinyApp(ui, server)


