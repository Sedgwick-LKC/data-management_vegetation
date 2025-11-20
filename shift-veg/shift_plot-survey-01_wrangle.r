## ------------------------------------------------ ##
# SHIFT Sedgewick Reserve Plot Survey - Wrangle
## ------------------------------------------------ ##
# Purpose:
## Wrangle the relevant SHIFT-associated data
## Initial data structure inherited from WildNote app

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, tidyxl)

# Get set up
source("00_setup.r")

# Clear environment
rm(list = ls()); gc()

# Load custom function(s)
purrr::walk(.x = dir(path = file.path("tools"), pattern = "fxn_"),
  .f = ~ source(file = file.path("tools", .x)))

## ----------------------------- ##
# Load Data ----
## ----------------------------- ##

# Define file path to desired data
plt_file <- file.path("data", "raw", "SHIFT_SR-plot_survey_v6-01-01-2025-06-12-2025-Tabular.xlsx")

# Read in the sheets of that file
plt_list <- read_wildnote(wn_path = plt_file)

# Check structure
dplyr::glimpse(plt_list)
