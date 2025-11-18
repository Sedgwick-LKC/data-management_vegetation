## ------------------------------------------------ ##
# Pre-Burn Understory Survey - Wrangle
## ------------------------------------------------ ##
# Purpose:
## Wrangle the 'raw' pre-burn understory survey data
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
udr_file <- file.path("data", "raw", "Understory-veg-material--ID-and-height-class.xlsx")

# Read in the sheets of that file
udr_list <- read_wildnote(wn_path = udr_file)

# Check structure
dplyr::glimpse(udr_list)

# Join by shared columns
udr_v01 <- dplyr::full_join(x = udr_list[[1]], y = udr_list[[2]],
  by = c("survey.id", "survey.date", "user"))

# Check that out
dplyr::glimpse(udr_v01)
## tibble::view(udr_v01)

## ----------------------------- ##
# Fix Column Class Issues ----
## ----------------------------- ##

# Fix all column class issues
udr_v02 <- udr_v01 %>% 
  # Strip year out of date information
  dplyr::mutate(survey.year = as.numeric(stringr::str_sub(string = survey.date, start = 1, end = 4)),
    .before = survey.date) %>% 
  # Remove non-number characters from otherwise numeric columns
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
    .fns = ~ gsub(pattern = "%", replacement = "", x = .))) %>% 
  # Make all columns that can be numeric into numbers
  dplyr::mutate(dplyr::across(.cols = dplyr::contains(c("seedlings", "dist")),
    .fns = as.numeric)) %>% 
  # Fix date class
  dplyr::mutate(survey.date = as.Date(stringr::str_sub(string = survey.date, start = 1, end = 10)))

# Check structure
dplyr::glimpse(udr_v02)

## ----------------------------- ##
# Remove Bad Rows/Columns ----
## ----------------------------- ##

# Drop unwanted rows and columns
udr_v03 <- udr_v02
## No such rows/columns (yet)

# How many rows lost (should be few)?
nrow(udr_v02) - nrow(udr_v03)

# Check structure
dplyr::glimpse(udr_v03)

## ----------------------------- ##
# Streamline Column Names ----
## ----------------------------- ##

# Make the column names simpler / more informative
udr_v04 <- udr_v03 %>% 
  # Algorithmic renaming
  dplyr::rename_with(.fn = ~ gsub(pattern = "activity...", replacement = "", x = .)) %>% 
  dplyr::rename_with(.fn = ~ gsub(pattern = "_", replacement = ".", x = .)) %>% 
  # Manual renaming
  dplyr::rename(distance.start = start.dist,
    distance.stop = stop.dist)

# Check structure
dplyr::glimpse(udr_v04)

## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make a final object
udr_v99 <- udr_v04

# Check structure
dplyr::glimpse(udr_v99)

# Generate a better file name
(udr_tidyname <- paste0("pre-burn_understory-survey_",
  min(udr_v99$survey.date, na.rm = T), "_",
  max(udr_v99$survey.date, na.rm = T), ".csv"))

# Export the ground cover data
write.csv(x = udr_v99, na = '', row.names = F,
  file = file.path("data", "tidy", udr_tidyname))

# End ----
