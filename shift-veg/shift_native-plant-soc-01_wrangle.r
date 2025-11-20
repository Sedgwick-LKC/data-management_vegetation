## ------------------------------------------------ ##
# SHIFT CA Native Plant Soc - Wrangle
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
nps_file <- file.path("data", "raw", "SHIFT_SR-CNPS_vegdat-v20250417-543119-05-02-2025-Tabular.xlsx")

# Read in the sheets of that file
nps_list <- read_wildnote(wn_path = nps_file)

# Check structure
dplyr::glimpse(nps_list)
