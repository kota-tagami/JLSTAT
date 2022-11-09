library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(shinybusy)

library(tidyverse)
library(plotly)

source_url_main <-
  "https://raw.githubusercontent.com/kota-tagami/JLSTAT-table/main"

source("R/Population/get_plot_population_total.R")
source("R/Population/get_plot_pyramid_total.R")


blue_main <- "#59a9e5"
blue_light <- "#59e5db"
blue_dark <- "#5963e5"

green_main <- "#a9e559"
red_main <- "#e559a9"

graph_text_color <- "#050505"
graph_background_color <- "white"
graph_axis_color <- "#c8c8c8"


busy_spin <- add_busy_spinner(
  spin = "fading-circle",
  color = red_main,
  time = 10,
  position = "top-right",
  margins = c(20, 20),
  onstart = F
)


pl_layout_font <- function(size = 16, 
                           color = graph_text_color) {
  list(
    `size` = size,
    `family` = "Arial",
    `color` = color
  )
}

pl_layout_base <- c(
  paper_bgcolor = graph_background_color,
  plot_bgcolor = graph_background_color,
  font = pl_layout_font()
)
