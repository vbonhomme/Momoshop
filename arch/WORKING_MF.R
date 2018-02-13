
# Minimal example of Shiny widget using 'magick' images
ui <- fluidPage(
  titlePanel("Magick Shiny Demo"),

  sidebarLayout(

    sidebarPanel(
      fileInput("upload", "Upload", accept = c('image/png', 'image/jpeg')),
      sliderInput("rotation", "Rotation", 0, 360, 0)
    ),

    mainPanel(
      imageOutput("img", width="20%"),
      # imageOutput("img2", width="20%"),
      #imageOutput("img", width="10%"),
      verbatimTextOutput("pipe_cat"),
      imageOutput("pipe_eval")
    )
  )
)



server <- function(input, output, session) {

  library(magick)

  # Start with placeholder img
  image <- image_read("https://raw.githubusercontent.com/ThinkR-open/collage/master/inst/tigrou/tigrou.jpg")

  # When uploading new image
  observeEvent(input$upload, {
    if (length(input$upload$datapath))
      image <<- image_convert(image_read(input$upload$datapath), "jpeg")
  })

  # A plot of fixed size
  output$img <- renderImage({

    tmpfile <- image %>%
      image_rotate(input$rotation) %>%
      image_write(tempfile(fileext='.jpg'), format = 'jpg')

    # Return a list
    list(src = tmpfile, contentType = "image/jpeg")
  })

  pipe <- reactive({
    pipe <- c("image %>% ")
    if (input$rotation != 0)
      pipe <- c(pipe, paste0("image_rotate(", input$rotation, ") %>% "))

    pipe <- c(pipe, "image_write(tempfile(fileext='.jpg'), format = 'jpg')")
    pipe
  })

  output$pipe_cat <- renderPrint(
    pipe() %>%
      paste(collapse="\n    ") %>%
      gsub("%>% *$", "", .) %>%
      cat
  )

  output$pipe_eval <- renderImage({
    pipe %>%
      paste(collapse=" ") %>%
      gsub("%>% *$", "", .) %>%
      parse(text=.) %>%
      eval()
  })

}

shinyApp(ui, server)
