library(shiny)
library(shinydashboard)
library(magick)
# Rinse
rm(list=ls())
source("utils.R")

server <- function(input, output, session) {

  image <- image_read("www/empty.png")

  # reset values -------------------------------------------
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
    if (length(input$upload$datapath)){
      path <<- input$upload$datapath
      image <<- image_read(path)
      info <<- image_info(image)
    }
  }

  # Image uploading and reload -----------------------------
  observeEvent(input$upload, {
    globalize_when_new_image()
    reset_values()
    updateSliderInput(session, "height", min=1, value = info$height, max = info$height*2)
  })

  observeEvent(input$reload, {
    globalize_when_new_image()
    reset_values()
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

  # Image rendering ----------------------------------------
  output$img <- renderImage({

    pipe <<- c("image %>%")

    # Boolean operators
    if("negate" %in% input$effects){
      image <- image_negate(image)
      pipe <<- c(pipe, "image_negate() %>%")
    }

    if("flip" %in% input$effects){
      image <- image_flip(image)
      pipe <<- c(pipe, "image_flip() %>%")
    }

    if("flop" %in% input$effects){
      image <- image_flop(image)
      pipe <<- c(pipe, "image_flop() %>%")
    }

    # channels
    if (input$channel != "All channels"){
      image <- image_channel(image, input$channel)
      pipe <<- c(pipe, paste("image_channel(", input$channel, ") %>% "))
    }

    if (input$threshold>-1){
      image <- image_threshold(image, input$threshold)
      pipe <<- c(pipe, paste("image_threshold(", input$threshold, ") %>% "))
    }

    if (input$reducenoise>0)
      image <- image_reducenoise(image, input$reducenoise)

    tmpfile <- image %>%
      image_rotate(input$rotation) %>%
      image_modulate(input$brightness, input$saturation, input$hue) %>%
      image_resize(paste(input$height, round(info$width*(input$height/info$height)), sep="x")) %>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')

    # Return a list
    list(src = tmpfile, contentType = "image/jpeg")
  })

  # cat pipe -------
  output$pipe <- renderPrint({

    pipe <- c("image %>%")
    # Boolean operators
    if("negate" %in% input$effects)
      pipe <- c(pipe, "image_negate() %>%")

    if("flip" %in% input$effects)
      pipe <- c(pipe, "image_flip() %>%")

    if("flop" %in% input$effects)
      pipe <- c(pipe, "image_flop() %>%")

    # channels
    if (input$channel != "All channels")
      pipe <- c(pipe, paste0("image_channel('", input$channel, "') %>% "))

    if (input$threshold>-1)
      pipe <- c(pipe, paste0("image_threshold(", input$threshold, ") %>% "))

    pipe %>% cat_pipe()
  })

  # eval pipe ---------
  output$img2 <- renderImage({
    pipe <- c("image %>%")
    # Boolean operators
    if("negate" %in% input$effects)
      pipe <- c(pipe, "image_negate() %>%")

    if("flip" %in% input$effects)
      pipe <- c(pipe, "image_flip() %>%")

    if("flop" %in% input$effects)
      pipe <- c(pipe, "image_flop() %>%")

    # channels
    if (input$channel != "All channels")
      pipe <- c(pipe, paste0("image_channel('", input$channel, "') %>% "))

    if (input$threshold>-1)
      pipe <- c(pipe, paste0("image_threshold(", input$threshold, ") %>% "))

    pipe <- c(pipe, "image_write(tempfile(fileext='jpg'), format = 'jpg')")

    tmpfile2 <- pipe %>% eval_pipe()
    list(src = tmpfile2, contentType = "image/jpeg")
  })

}
