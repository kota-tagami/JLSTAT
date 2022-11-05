library(shiny)
library(shiny.semantic)
library(semantic.dashboard)

library(tidyverse)
library(plotly)

source_url_main <- 
  "https://raw.githubusercontent.com/kota-tagami/JLSTAT-table/main"

source("R/Population/get_plot_population_total.R")


blue_main <- "#59a9e5"
blue_light <- "#59e5db"
blue_dark <- "#5963e5"

green_main <- "#a9e559"
red_main <- "#e559a9"

graph_text_color <- "#050505"
graph_background_color <- "white"
graph_axis_color <- "#c8c8c8"

