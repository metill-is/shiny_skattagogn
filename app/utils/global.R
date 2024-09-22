library(shiny)
library(plotly)
library(tidyverse)
library(scales)
library(ggtext)
library(here)
library(janitor)
library(DT)
library(bslib)
library(thematic)
library(shinycssloaders)
library(visitalaneysluverds)
library(metill)
library(shinyjs)
library(arrow)

shinyOptions(plot.autocolor = TRUE)

#### DATA ####

d_deciles <- arrow::read_parquet("data/deciles.parquet")

##### Sidebar Info and Plot Captions #####
# This is pasted into the sidebar on each page
sidebar_info <- paste0(
  br(" "),
  p(
    str_c(
      "Hægt er að færa mús (í tölvu) ",
      "eða putta (í snjalltæki) yfir myndir ",
      "til að sjá nákvæmar tölur"
    )
  ),
  h5("Höfundur:"),
  p("Brynjólfur Gauti Guðrúnar Jónsson"),
  HTML("<a href='https://github.com/bgautijonsson/skattagogn' target='_top'> Kóði og gögn </a>")
)
# This is the caption for plots
global_caption <- "Mynd fengin frá metill.is/maelabord/skattagogn"


##### THEMES #####
# Making a light and dark theme in case I want to offer the option later
theme_set(theme_metill())



bs_global_theme(
  bootswatch = "flatly"
)

bs_global_add_variables(
  primary = "#484D6D",
  secondary = "#969696",
  success = "#969696",
  light = "#faf9f9",
  dark = "#484D6D",
  bg = "#faf9f9",
  fg = "#737373",
  "body-bg" = "#faf9f9",
  base_font = "Lato",
  heading_font = "Segoe UI",
  "navbar-brand-font-family" = "Playfair Display",
  code_font = "SFMono-Regular"
)
