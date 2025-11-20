## ------------------------------------------------ ##
# SHIFT TREX Vegetation - Wrangle
## ------------------------------------------------ ##
# Purpose:
## Wrangle the relevant SHIFT-associated TREX veg data
### TREX = Training Exchange
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
trx_file <- file.path("data", "raw", "SHIFT_SR-trex_vegdat-v3-530597-04-03-2025-Tabular.xlsx")

# Read in the sheets of that file
trx_list <- read_wildnote(wn_path = trx_file)

# Check structure
dplyr::glimpse(trx_list)
