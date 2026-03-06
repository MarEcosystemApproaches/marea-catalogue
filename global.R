# global settings

# set a color palette to be consistent through all plots
# ESS (equal to 4VN, 4VS, and 4W) = purples
# WSS (~equal to 4X) = oranges

global_cols <- tibble::tribble(
  ~region,  ~color,
  # when ESS / WSS make them just purple and orange
  "ESS",  "purple",
  "WSS",  "orange",
  # when NAFO zones, make them shades of those colors
  # first , ESS zones (4VN/4Vn, 4VS/4Vs, 4W)
  "4Vn",   "#8402C9",
  "4VN",   "#8402C9",
  "4Vs",  "#BB73FF",
  "4VS",  "#BB73FF",
  "4W" , "purple",
  "4X" , "orange" # equal to WSS

)



global_cols2 <- tibble::tribble(
  ~region, ~region_group, ~region_group_label, ~color,
  # when NAFO zones, make them shades of those colors
  # first , ESS zones (4VN/4Vn, 4VS/4Vs, 4W)
  "4Vn", "ESS", "Eastern Scotian Shelf",   "#cc98fa",
  "4VN", "ESS", "Eastern Scotian Shelf",   "#cc98fa",
  "4Vs", "ESS", "Eastern Scotian Shelf",  "#8736cf",
  "4VS", "ESS", "Eastern Scotian Shelf",  "#8736cf",
  "4W" , "ESS", "Eastern Scotian Shelf", "#4c1380",
  "4X" , "WSS", "Western Scotian Shelf", "orange" # equal to WSS

)


global_colors_groups <- c("Eastern Scotian Shelf" = "purple",
                          "Western Scotian Shelf" = "orange")


global_cols3 <- tibble::tribble(
  ~region, ~region_group, ~region_group_label, ~color, ~linetype, ~linewidth,
  # when NAFO zones, make them shades of those colors
  # first , ESS zones (4VN/4Vn, 4VS/4Vs, 4W)
  "4Vn", "ESS", "Eastern Scotian Shelf",   "#cc98fa", "dash", 1,
  "4VN", "ESS", "Eastern Scotian Shelf",   "#cc98fa", "dash", 1,
  "4Vs", "ESS", "Eastern Scotian Shelf",  "#8736cf", "dash", 1,
  "4VS", "ESS", "Eastern Scotian Shelf",  "#8736cf", "dash", 1,
  "4W" , "ESS", "Eastern Scotian Shelf", "#4c1380", "dash", 1,
  "4X" , "WSS", "Western Scotian Shelf", "orange", "dash", 1,
  "ESS", "ESS", "Eastern Scotian Shelf", "purple", "solid", 3,
  "WSS", "WSS", "Western Scotian Shelf", "orange", "solid", 3

)


ss_plot_cols <- c("ESS" = "purple",  "WSS" = "Orange")

# knitr options -----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = F,
  message = F,
  cache = TRUE,
  cache.path = "_cache/",
  autodep = TRUE,   # track object dependencies between chunks (helpful)
  message = FALSE,
  warning = FALSE
)



library(dplyr)
library(marea)
library(ggplot2)


