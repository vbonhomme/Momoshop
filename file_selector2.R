library("shiny")
library("shinyWidgets")

ui <- fluidPage(
  pickerInput(
    inputId = "files",
    label = "Choose file(s):",
    choices = c("home", "search", "ok-sign"),
    multiple=TRUE,
    choicesOpt = list(
      icon = c("glyphicon-home",
               "glyphicon-search",
               "glyphicon-ok-sign")
    )
  ),
  verbatimTextOutput(outputId = "files")

)

server <- function(input, output, session) {

  output$files <- renderPrint(input$files)

}

shinyApp(ui = ui, server = server)

# and ?selectPicker
