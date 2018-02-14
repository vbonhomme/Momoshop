library(shiny)
library(DT)
library(tidyverse)

df <- Momocs::olea$fac
n <- nrow(df)
df$name <- paste("file6922e0874ba_", 1:n)
df$edited <- sample(c(T, F), n, rep=T, prob=c(0.1, 0.4))
df$digitized <- sample(c(T, F), n, rep=T, prob=c(0.1, 0.4))
df$quant <- rnorm(n, 5, 5) %>% signif(3)
df <- select(df, name, edited, digitized, quant, everything())

shinyApp(
  ui = fluidPage(DTOutput('tbl')),
  server = function(input, output) {
    output$tbl = renderDT(
      datatable(df, filter = 'top', options = list(pageLength = 20)) %>%
        formatStyle(
          'name', 'edited',
          backgroundColor = styleEqual(c(0, 1), c('gray', 'yellow')))

    )
  }
)

