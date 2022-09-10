# Load packages -----------------------------------------------------------
#devtools::install_github("lzumeta/injurytools")
library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(shinyWidgets)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(ggpubr)
library(DT)
library(injurytools)
library(knitr)
library(gridExtra)
library(kableExtra)
library(rmarkdown)  ## render()
library(htmltools)  ## includeHTML()


# Load data ---------------------------------------------------------------
## check that this is the last updated data (explain this in: help--> about data)

players_df <- readRDS('../shiny/data/players_df.Rds')
injuries_df <- readRDS('../shiny/data/injuries_df.RDs')
clubs_df <- readRDS('../shiny/data/clubs_df.Rds')
train_df <- readRDS('../shiny/data/train.Rda')
train_and_test_df <- readRDS('../shiny/data/traintest.Rda')

# players_longitudinal_df <- readRDS("data/players_longitudinal_df.Rds") ## THIS DATA IS NOT MORE USED
players_ids <- readRDS("../shiny/data/players_ids.Rds")

options(scipen = 999)


## Make player_names unique (should this go in another place?..)
players_ids$player_name2 <- make.unique(as.character(players_ids$player_name)) %>%
  as.factor()
injuries_df <- left_join(injuries_df, players_ids, by = c("player_id", "player_name"))
players_df <- left_join(players_df, players_ids,
                        by = c("player_id", "player_name"))


# Load self created functions ---------------------------------------------
gg_marketinjury <- function(df_exp, df_inj) {
  ggplot(data = df_exp, aes(group = .data$player_name2)) + 
    geom_point(aes(x = .data$year_stops, y = log(.data$market_value)), colour = "darkblue", size = 2) +   ## changed: year_aux to year_stops, better?
    geom_line(aes(x = .data$year_stops, y = log(.data$market_value)), colour = "darkblue") +
    geom_rect(aes(xmin = from, 
                  xmax = until,
                  ymin = -Inf, ymax = Inf,
                  fill = "Injured"), 
              alpha = 0.2, colour = "red", 
              data = df_inj, inherit.aes = FALSE) +
    facet_wrap(~.data$player_name2,  ## important, since there are "two rafinha"s, two "manu garcia"s etc.
               ncol = 6) +  
    scale_fill_manual("", breaks = "Injured", values ="black") +
    scale_x_date(breaks = unique(df_exp[["year_stops"]])) +  ## changed: year_aux to year_stops, better?
    #scale_y_log10() + 
    xlab("Year") + ylab("log(Market Value)") +
    theme_bw() +
    theme(axis.title = element_text(size = 26),
          axis.text  = element_text(size = 15),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          strip.text = element_text(size = 15),
          legend.text = element_text(size = 24))
}

## Note: the warning message "geom_path: Each group consists of only one
## observation. Do you need to adjust the group aesthetic?" is because some
## players market value is missing, is NA


#  set_shiny_plot_height --------------------------------------------------
## resize automatically shiny plots
## source: https://stackoverflow.com/questions/25752529/how-to-automatically-right-size-ggplot-in-shiny
set_shiny_plot_height <- function(session, output_width_name) {
  width <- function() { 
    session$clientData[[output_width_name]]
  }
  width <- width*300
}


# gg_facet_nrow -----------------------------------------------------------
## Extract number of rows from faceted ggplot
## resource: https://stackoverflow.com/questions/50914398/increase-plot-size-in-shiny-when-using-ggplot-facets
gg_facet_nrow <- function(p) {
  # p %>% ggplot2::ggplot_build()  %>%
  #   magrittr::extract2('layout')       %>%
  #   magrittr::extract2('panel_layout') %>%
  #   magrittr::extract2('ROW')          %>%
  #   unique()                           %>%
  #   length()
  num_panels <- length(unique(ggplot_build(p)$data[[1]]$PANEL)) # get number of panels
  num_cols <- ggplot_build(p)$layout$facet$params$ncol # get number of columns set by user
  num_rows <- wrap_dims(num_panels, ncol=num_cols)[1] # determine number of rows
  return(num_rows)
}



# Selectors ---------------------------------------------------------------
teams <- levels(as.factor(clubs_df$club_name))
players <- levels(as.factor(players_df$player_name))
seasons <- levels(players_df$season)
min_Date <- min(players_df$year)
max_Date <- max(players_df$year)
min_season <- levels(players_df$season)[1]
max_season <- levels(players_df$season)[length(levels(players_df$season))]

## Names of the player ids (for labellers) -------------------------------
label_playerid <- setNames(object = as.character(players_ids$player_name),
                           as.character(players_ids$player_id))


