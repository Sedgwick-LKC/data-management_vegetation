## ------------------------------------------------ ##
# Post-Burn TREX Shrub Fire Severity - Wrangle
## ------------------------------------------------ ##
# Purpose:
## Wrangle the 'raw' post-burn shrub fire severity data
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
trx_file <- file.path("data", "raw", "TREX-surveys-Shrub_Fire_Severity.xlsx")

# Read in the sheets of that file
trx_list <- read_wildnote(wn_path = trx_file)

# Check structure
dplyr::glimpse(trx_list)

# Join by shared columns
trx_v01 <- dplyr::full_join(x = trx_list[[1]], y = trx_list[[2]],
  by = c("survey.id", "survey.date", "user"))

# Check that out
dplyr::glimpse(trx_v01)
## tibble::view(trx_v01)

## ----------------------------- ##
# Fix Column Class Issues ----
## ----------------------------- ##

# Fix all column class issues
trx_v02 <- trx_v01 %>% 
  # Strip year out of date information
  dplyr::mutate(survey.year = as.numeric(stringr::str_sub(string = survey.date, start = 1, end = 4)),
    .before = survey.date) %>% 
  # Remove non-number characters from otherwise numeric columns
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
    .fns = ~ gsub(pattern = "%", replacement = "", x = .))) %>% 
  # Make all columns that can be numeric into numbers
  dplyr::mutate(dplyr::across(.cols = dplyr::contains(c("height", "_mm")),
    .fns = as.numeric)) %>% 
  # The transect segment column has a few special problems that we need to fix manually
  ## Abbreviating column just to make following operations easier to type
  dplyr::rename(xx = activity...transect_segment_m) %>% 
  dplyr::mutate(xx = dplyr::case_when(
    xx == "ARTICALI" ~ NA,
    xx %in% c("13.4E", "22.5T", "23i") ~ gsub(
      pattern = paste(c(letters, LETTERS), collapse = "|"), 
      replacement = "", x = xx),
    xx == "24,95" ~ "24.95",
    stringr::str_detect(string = xx, pattern = "-") ~ gsub(pattern = "-", replacement = "", x = xx),
    T ~ xx)) %>% 
  dplyr::mutate(xx = as.numeric(xx)) %>% 
  dplyr::rename(activity...transect_segment_m = xx) %>% 
  # Fix date class
  dplyr::mutate(survey.date = as.Date(stringr::str_sub(string = survey.date, start = 1, end = 10)))

# Check structure
dplyr::glimpse(trx_v02)

## ----------------------------- ##
# Remove Bad Rows/Columns ----
## ----------------------------- ##

# Drop unwanted rows and columns
trx_v03 <- trx_v02 %>% 
  dplyr::filter(!is.na(activity...transect_segment_m) & !is.na(activity...species)) %>% 
  dplyr::select(-dplyr::starts_with("survey.status."))

# How many rows lost (should be few)?
nrow(trx_v02) - nrow(trx_v03)

# Check structure
dplyr::glimpse(trx_v03)

## ----------------------------- ##
# Streamline Column Names ----
## ----------------------------- ##

# Make the column names simpler / more informative
trx_v04 <- trx_v03 %>% 
  # Separate transect from plot (need to standardize soemwhat first)
  dplyr::mutate(transect.plot = gsub(pattern = "_lake| ", replacement = "", x = transect.plot)) %>% 
  dplyr::mutate(transect.plot = gsub(pattern = "chapm", replacement = "chap_m", x = tolower(transect.plot))) %>% 
  dplyr::mutate(transect.plot = toupper(transect.plot)) %>% 
  tidyr::separate_wider_delim(cols = transect.plot, delim = "_", 
    names = c("transect", "plot")) %>% 
  # Algorithmic renaming
  dplyr::rename_with(.fn = ~ gsub(pattern = "activity...", replacement = "", x = .)) %>% 
  dplyr::rename_with(.fn = ~ gsub(pattern = "_", replacement = ".", x = .)) %>% 
  dplyr::rename_with(.fn = ~ gsub(pattern = "\\.m", replacement = "_m", x = .)) %>% 
  # Manual renaming
  dplyr::rename(height_cm = height.cm,
    resprout = resprout.)

# Check structure
dplyr::glimpse(trx_v04)

## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make a final object
trx_v99 <- trx_v04

# Check structure
dplyr::glimpse(trx_v99)

# Generate a better file name
(trx_tidyname <- paste0("post-burn_trex-survey-shrub-fire-severity_",
  min(trx_v99$survey.date, na.rm = T), "_",
  max(trx_v99$survey.date, na.rm = T), ".csv"))

# Export the ground cover data
write.csv(x = trx_v99, na = '', row.names = F,
  file = file.path("data", "tidy", trx_tidyname))

# End ----
