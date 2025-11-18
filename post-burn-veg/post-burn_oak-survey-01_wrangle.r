## ------------------------------------------------ ##
# Post-Burn Oak Survey - Wrangle
## ------------------------------------------------ ##
# Purpose:
## Wrangle the 'raw' post-burn oak survey data
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
oak_file <- file.path("data", "raw", "Post-burn-Oak.xlsx")

# Read in the sheets of that file
oak_list <- read_wildnote(wn_path = oak_file)

# Check structure
dplyr::glimpse(oak_list)

# Join by shared columns
oak_v01 <- dplyr::full_join(x = oak_list[[1]], y = oak_list[[2]],
  by = c("survey.id", "survey.date", "user"))

# Check that out
dplyr::glimpse(oak_v01)
## tibble::view(oak_v01)

## ----------------------------- ##
# Fix Column Class Issues ----
## ----------------------------- ##

# Fix all column class issues
oak_v02 <- oak_v01 %>% 
  # Strip year out of date information
  dplyr::mutate(survey.year = as.numeric(stringr::str_sub(string = survey.date, start = 1, end = 4)),
    .before = survey.date) %>% 
  # Remove non-number characters from otherwise numeric columns
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
    .fns = ~ gsub(pattern = "%", replacement = "", x = .))) %>% 
  # Make all columns that can be numeric into numbers
  dplyr::mutate(dplyr::across(.cols = dplyr::contains(c("percent", "crown", "height")),
    .fns = as.numeric)) %>% 
  # Fix date class
  dplyr::mutate(survey.date = as.Date(stringr::str_sub(string = survey.date, start = 1, end = 10)))

# Check structure
dplyr::glimpse(oak_v02)

## ----------------------------- ##
# Remove Bad Rows/Columns ----
## ----------------------------- ##

# Drop unwanted rows and columns
oak_v03 <- oak_v02 %>% 
  dplyr::filter(!is.na(activity...tag_number))

# How many rows lost (should be few)?
nrow(oak_v02) - nrow(oak_v03)

# Check structure
dplyr::glimpse(oak_v03)

## ----------------------------- ##
# Streamline Column Names ----
## ----------------------------- ##

# Make the column names simpler / more informative
oak_v04 <- oak_v03 %>% 
  # Algorithmic renaming
  dplyr::rename_with(.fn = ~ gsub(pattern = "activity...", replacement = "", x = .)) %>% 
  dplyr::rename_with(.fn = ~ gsub(pattern = "_", replacement = ".", x = .)) %>% 
  # Manual renaming
  dplyr::rename(crown.alive_percent = crown.alive.percent)

# Check structure
dplyr::glimpse(oak_v04)

## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make a final object
oak_v99 <- oak_v04

# Check structure
dplyr::glimpse(oak_v99)

# Generate a better file name
(oak_tidyname <- paste0("post-burn_oak-survey_",
  min(oak_v99$survey.date, na.rm = T), "_",
  max(oak_v99$survey.date, na.rm = T), ".csv"))

# Export the ground cover data
write.csv(x = oak_v99, na = '', row.names = F,
  file = file.path("data", "tidy", oak_tidyname))

# End ----
