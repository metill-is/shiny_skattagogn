library(shiny)
library(plotly)
library(cowplot)
library(tidyverse)
library(scales)
library(ggthemes)
library(kableExtra)
library(gganimate)
library(lubridate)
library(geomtextpath)
library(ggtext)
library(here)
library(readxl)
library(janitor)
library(DT)
library(bslib)
library(thematic)
library(shinycssloaders)
library(DBI)
library(feather)
library(visitalaneysluverds)

shinyOptions(plot.autocolor = TRUE)

#### DATA ####

tiundamork <- read_feather("data/tiundamork_1997_2021.feather") |> 
    filter(tiundarhluti != "Alls") |> 
    inner_join(
        vnv() |> 
            mutate(cpi = cpi / cpi[date == max(date)]) |> 
            group_by(ar = year(date)) |> 
            summarise(cpi = mean(cpi)),
        by = "ar"
    )

aldurshopar <- read_feather("data/aldurshopar.feather") |> 
    inner_join(
        vnv() |> 
            mutate(cpi = cpi / cpi[date == max(date)]) |> 
            group_by(ar = year(date)) |> 
            summarise(cpi = mean(cpi)),
        by = "ar"
    )

skuldahlutfall <- read_feather("data/skuldahlutfall.feather") |> 
    group_by(ar, hlutf_tegund) |> 
    mutate(hlutf = value / sum(value)) |> 
    ungroup() |> 
    group_by(ar, skyribreyta, hlutf_tegund) |> 
    mutate(hlutf_aldur = value / sum(value)) |> 
    group_by(ar, name, hlutf_tegund) |> 
    mutate(hlutf_skuld = value / sum(value)) |> 
    ungroup()

##### Sidebar Info and Plot Captions #####
# This is pasted into the sidebar on each page
sidebar_info <- paste0(
    br(" "),
    h5("Höfundur:"),
    p("Brynjólfur Gauti Guðrúnar Jónsson"),
    HTML("<a href='https://github.com/bgautijonsson/skattagogn'> Kóði og gögn </a>")
)
# This is the caption for plots
global_caption <- "Mynd var fengin frá: https://www.bggj.is/skattagogn"

##### THEMES #####
# Making a light and dark theme in case I want to offer the option later
light <- bs_theme(bootswatch = "flatly", primary = "#08306b",
                  base_font = "Segoe UI", code_font = "SFMono-Regular", heading_font = "Lato")
dark <- bs_theme(bootswatch = "superhero", primary = "#08306b")
theme_set(theme_half_open(font_size = 12))
thematic_shiny()





