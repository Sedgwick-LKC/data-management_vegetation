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

## ----------------------------- ##
# Fix Column Class Issues ----
## ----------------------------- ##

# Fix all column class issues
cov_v02 <- cov_v01 %>% 
  # Strip year out of date information
  dplyr::mutate(survey.year = as.numeric(stringr::str_sub(string = survey.date, start = 1, end = 4)),
    .before = survey.date) %>% 
  # Rename one column slightly
  dplyr::rename(transect.side = activity....x_dist) %>% 
  # Make all columns that can be numeric into numbers
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with(c("unit", "plot", "activity")),
    .fns = as.numeric)) %>% 
  # Fix date class
  dplyr::mutate(survey.date = as.Date(stringr::str_sub(string = survey.date, start = 1, end = 10)))

# Check structure
dplyr::glimpse(cov_v02)

## ----------------------------- ##
# Remove Bad Rows ----
## ----------------------------- ##

# Drop unwanted rows
cov_v03 <- cov_v02 %>% 
  dplyr::filter(!is.na(activity....distance_.meter) & !is.na(transect.side))

# How many rows lost (should be few)?
nrow(cov_v02) - nrow(cov_v03)

# Check structure
dplyr::glimpse(cov_v03)

## ----------------------------- ##
# Streamline Column Names ----
## ----------------------------- ##

# Make the column names simpler / more informative
cov_v04 <- cov_v03 %>% 
  # Manual renaming
  dplyr::rename(distance_m = activity....distance_.meter) %>% 
  # Algorithmic renaming
  dplyr::rename_with(.fn = ~ gsub(pattern = "activity....", replacement = "", x = .)) %>% 
  dplyr::rename_with(.fn = ~ gsub(pattern = "_", replacement = ".", x = .)) %>% 
  dplyr::rename_with(.cols = leaf.litter:perennial.grass,
    .fn = ~ paste0(., "_percent.cover"))

# Check structure
dplyr::glimpse(cov_v04)

## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make a final object
cov_v99 <- cov_v04

# Check structure
dplyr::glimpse(cov_v99)

# Generate a better file name
(cov_tidyname <- paste0("pre-burn_ground-cover_",
  min(cov_v99$survey.date, na.rm = T), "_",
  max(cov_v99$survey.date, na.rm = T), ".csv"))

# Export the ground cover data
write.csv(x = cov_v99, na = '', row.names = F,
  file = file.path("data", "tidy", cov_tidyname))

# End ----
