## ------------------------------------------------ ##
# Pre-Burn Ground Cover - Wrangle
## ------------------------------------------------ ##
# Purpose:
## Wrangle the 'raw' pre-burn ground cover data
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
cov_file <- file.path("data", "raw", "Ground-Cover-Pre-Burn.xlsx")

# Read in the sheets of that file
cov_list <- read_wildnote(wn_path = cov_file)

# Check structure
dplyr::glimpse(cov_list)

# Join by shared columns
cov_v01 <- dplyr::full_join(x = cov_list[[1]], y = cov_list[[2]],
  by = c("survey.id", "survey.date", "user"))

# Check that out
dplyr::glimpse(cov_v01)
## tibble::view(cov_v01)

# End ----
