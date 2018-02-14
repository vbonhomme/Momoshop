library(magick)
library(tidyverse)

x <- image_read("Lenna.png") %>% image_resize("50x") %>% `[[`(1)
n <- x %>% dim %>% `[`(-1) %>% prod
imi <- apply(x, 1, as.integer)
df <- data.frame(y=c(imi[,1], imi[,2], imi[,3]),
                 f=rep(c("r", "g", "b"), each=n))
ggplot(df) + aes(x=y, fill=f) + geom_density(alpha=0.5) + theme_linedraw()

image_histogram <-
  function(image, sampled_geometry=100){

  img <- image %>% image_resize(sampled_geometry) %>% `[[`(1)
  n <- img %>% dim %>% `[`(-1) %>% prod
  data.frame(x=img %>% array %>% as.integer(),
                   f=rep(c("r", "g", "b"), each=n)) %>%
    ggplot() + aes(x=x, fill=f)+ geom_density(alpha=0.25) +
    labs(x="", y="") +
    scale_x_continuous(breaks=c(0, 128, 255)) +
    scale_y_continuous(breaks = NULL) +
    theme_light() + theme(legend.position="none")
  }

image_read('Lenna.png') %>% image_histogram()
