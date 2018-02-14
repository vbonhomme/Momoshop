library(shiny)
library(shinydashboard)
library(magick)
pipe <- ""

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title=tags$img(src='Momoshop_sticker.png', height="50px", align="left")),
  dashboardSidebar(
    # File handling ----------------------------------------
    fileInput("upload", "New image", accept = c('image/png', 'image/jpeg')),
    actionButton("reload", "Reload", icon=icon("repeat")),
    actionButton("write", "Write", icon=icon("floppy-o")),
    ### tabs
    sidebarMenu(
      id = "tabs",
      # Channels and thresholding ----------------------------
      menuItem("Channels and threshold", icon = icon("align-justify"),
               selectInput("channel", "Channels", choices = useful_channels, selected = "All channels"),
               sliderInput("threshold", "Threshold", -1, 256, -1)),
      # Colors -----------------------------------------------
      menuItem("Colors", icon = icon("tasks"),
               sliderInput("brightness", "Brightness", 0, 200, 100, 1),
               sliderInput("saturation", "Saturation", 0, 200, 100, 1),
               sliderInput("hue", "Hue", 0, 200, 100, 1)
      ),

      menuItem("Transform", icon = icon("wrench"),
               # Transform ---------------------------------------------
               sliderInput("height", "Rescale", min=1, value = 1, max = 1, step = 1),
               # checkboxInput("reducenoise_switch", "Denoise?", FALSE),
               sliderInput("reducenoise", "Denoise", 0, 50, 0),
               sliderInput("rotation", "Rotate", 0, 360, 0, step = 1, round=TRUE),
               checkboxGroupInput("effects", "Effects",
                                  choices = list("negate", "flip", "flop"))
      )
    )
  ),
  dashboardBody(
    # If an image is loaded, then output some info
    fluidRow(
      column(width = 12,
             p(textOutput("info")),
             # Render the image
             imageOutput("img", height="100%", width="100%")
      )
    ),
    br(),
    fluidRow(
      # More informations
      verbatimTextOutput("upload"),
      verbatimTextOutput("pipe"),
      imageOutput("img2", height="100%", width="100%")
    )
  )
)
