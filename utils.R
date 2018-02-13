
# pipe functions -------------------------------------------
# The pipe argument below is defined as a vector of characters
# that looks like a pipe, eg c("image", "image_flip() %>%")
# these functions are used to  build, print and eval pipes

# append ... to a pipe
pipe_append <- function(pipe, ...){
  list(...) %>%
    unlist %>% paste0(., collapse="") %>%
    append(pipe, .)
}

# conditionnally append ... to a pipe
pipe_cond_append <- function(pipe, cond, ...){
  if (cond){
    pipe_append(pipe, ...)
  } else {
    pipe
  }
}

# nicely cat a pipe
pipe_cat <- function(pipe){
  # if (length(e)<2)
  #   return(NULL)
  pipe %>%
    paste(collapse="\n    ") %>%
    gsub("%>% *$", "", .) %>%
    cat
}

# eval a pipe
pipe_eval <- function(pipe){
  pipe %>%
    paste(collapse=" ") %>%
    gsub("%>% *$", "", .) %>%
    parse(text=.) %>%
    eval()
}

# magick related functions ---------------------------------

# given a datapath, return formatted info
info_cat <- function(x){
  if (length(x$datapath)){
    paste("Original image: ",
          info$format,
          info$colorspace,
          "-",
          paste0("(", format(structure(info$filesize, class="object_size"), units="auto"), ")"),
          paste(info$width, info$height, sep=" x "))
  } else {
    "No image loaded"
  }
}


# given a threshold turn an image into a mask
image_threshold <- function(image, threshold){
  x <- image %>% image_data()
  x[,,][which(x[,,] <= threshold)] <- as.raw(0)
  x[,,][which(x[,,] >  threshold)] <- as.raw(0xff)
  x %>% image_read
}

# constants ------------------------------------------------
# to lighten the shiny part

useful_channels <-
  c("All channels", "Alpha", "Blue", "Cyan", "Gray", "Green",
    "Hue", "Lightness", "Luminance", "Luminosity",
    "Magenta", "Red", "Saturation", "Yellow")


# fridge ---------------------------------------------------
is_single_layer <- function(image){
  dim(image_data(image))[1]==1
}


# todo ------
# write a copy
# mosaic many ----

# lf <- list.files("~/Research/VITIS/Vitis/0001_cAbjouB/0001_cAbjouB_Initiale/",
#                  full=T, pattern="jpg")
# imgs <- image_read(lf) %>% image_resize("200x140")
# image_montage(imgs[1:48])








