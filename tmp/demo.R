library(shiny)
library(shinydashboard)
library(magick)

# Rinse
rm(list=ls())
lena <- image_read("Lenna.png")
source("utils.R")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title=tags$img(src='Momoshop_sticker.png', height="40px", align="left")),
  dashboardSidebar(
    # File handling ----------------------------------------
    fileInput("upload", "New image", accept = c('image/png', 'image/jpeg')),
    # actionButton("reload", "Reload", icon=icon("repeat")),
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

               sliderInput("height", "Rescale", min=1, value = 1, max = 1, step = 1),
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
      column(width = 12,
             imageOutput("img", width="100%", height="100%")
      )
    ),

    fluidRow(
      column(12,
             h3("Debug"),
             h5("info"),
             verbatimTextOutput("info"),
             h5("upload"),
             verbatimTextOutput("upload"),
             h5("pipe_cat"),
             verbatimTextOutput("pipe_cat"),
             h5("env"),
             verbatimTextOutput("env"),
             h5("show_inputs"),
             verbatimTextOutput('show_inputs')
      )
    )
  )
)

# server -----------------
server <- function(input, output, session) {

  # image <- image_read("www/empty.png")
  # # info <- image_info(image)
  #
  # # load a new image
  # new_image <- function(){
  #   if (length(input$upload$datapath)){
  #     image <<- image_read(input$upload$datapath) %>% image_convert("jpeg")
  #     info <<- image_info(image)
  #   }
  # }


  #
  # # Image uploading and reload -----------------------------
  # observeEvent(input$upload, {
  #   new_image()
  #   reset_values()
  #   updateSliderInput(session, "height", min=1, value = info$height, max = info$height*2)
  # })
  #
  # observeEvent(input$reload, {
  #   new_image()
  #   reset_values()
  # })
  #

  # output$pipe_eval <- renderImage({
  #   tmpfile3 <- pipe() %>%
  #     paste(collapse=" ") %>%
  #     gsub("%>% *$", "", .) %>%
  #     parse(text=.) %>%
  #     eval()
  #   # browser()
  #   list(src = tmpfile3, contentType = "image/jpeg")
  # })



  # Start with placeholder img
  image <- image_read("www/empty.png")

  # # reset values when reload
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



  # # When uploading new image
  observeEvent(input$upload, {
    globalize_when_new_image()
    reset_values()
    # updateSliderInput(session, "height", min=1, value = info$height, max = info$height*2)
  })

  observeEvent(input$upload, {
    globalize_when_new_image()
    reset_values()
    updateSliderInput(session, "height", min=1, value = info$height, max = info$height*2)
  })

  # A plot of fixed size
  output$img <- renderImage({

    tmpfile <- image %>%
      image_rotate(input$rotation) %>%
      image_write(tempfile(), format = 'jpeg')

    # Return a list
    list(src = tmpfile, contentType = "image/jpeg")
  })


  # # Image uploading and reload -----------------------------
  # observeEvent(input$upload, {
  #   globalize_when_new_image()
  #   reset_values()
  #   updateSliderInput(session, "height", min=1, value = info$height, max = info$height*2)
  # })

  # observeEvent(input$reload, {
  #   globalize_when_new_image()
  #   reset_values()
  # })
  #
  # # A plot of fixed size
  # output$img <- renderImage({
  #   tmpfile <- image %>%
  #     image_write(tempfile(fileext='.jpg'), format = 'jpeg')
  #   # Return a list
  #   list(src = tmpfile, contentType = "image/jpeg")
  #   })


  output$env <- renderPrint(print(names(globalenv())))

  # show allInputs

  AllInputs <- reactive({
    # x <- reactiveValuesToList(input)
    # data.frame(
    #   names = names(x),
    #   values = unlist(x, use.names = FALSE)
    # )
    reactiveValuesToList(input)
  })

  output$show_inputs <- renderPrint({
    AllInputs()
  })


}

shinyApp(ui, server)

