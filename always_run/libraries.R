# libraries.R - Load packages needed for analysis 
# Code adapted from https://grssnbchr.github.io/rddj-template/#define_packages

library(pacman)

my_packages <- c("here",
                 "lubridate",
                 "tidytable",
                 "tidyverse",
                 "ggtext",
                 "showtext",
                 "feather")

pacman::p_load(my_packages, character.only = TRUE)

rm(my_packages)