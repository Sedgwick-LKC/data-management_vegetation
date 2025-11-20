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

# Join by shared columns
nps_v01 <- dplyr::full_join(x = nps_list[[1]], y = nps_list[[2]],
    by = c("survey.id", "survey.date", "user", "survey.status")) %>% 
  dplyr::full_join(x = ., y = nps_list[[3]],
    by = c("survey.id", "survey.date", "user", "survey.status")) %>% 
  dplyr::full_join(x = ., y = nps_list[[4]],
    by = c("survey.id", "survey.date", "user", "survey.status"))

# Check structure
dplyr::glimpse(nps_v01)

## ----------------------------- ##
# Fix Data Shape ----
## ----------------------------- ##

# Do needed repairs to shape
nps_v02 <- nps_v01 %>% 
  # Add a row number
  dplyr::mutate(row.num = 1:nrow(.), .after = survey.date) %>% 
  # Do some standardizing of old column names
  dplyr::rename_with(.fn = ~ gsub("plot_data_", "", x = .)) %>% 
  dplyr::rename_with(.fn = ~ gsub(".cover", "_cover", x = .)) %>% 
  dplyr::rename_with(.fn = ~ gsub("\\.\\.\\.", "__", x = .)) %>% 
  # Flip into long format
  tidyr::pivot_longer(cols = -survey.id:-transect) %>% 
  # Split old column names into component parts
  tidyr::separate_wider_delim(cols = name, delim = "__",
    names = c("distance_m", "xx"), cols_remove = T) %>% 
  tidyr::separate_wider_delim(cols = xx, delim = "_", 
    too_few = "align_start", names = c("spp.num", "yy")) %>% 
  # Collapse rows (necessary to reclaim desired data shape)
  dplyr::select(-yy) %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.),
    y = c("value"))))) %>% 
  dplyr::summarize(sp.cov = paste(unique(value), collapse = "___"),
    .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Split the combination that we just made to get better columns
  dplyr::filter(sp.cov != "NA") %>% 
  tidyr::separate_wider_delim(cols = sp.cov, delim = "___",
    names = c("species", "cover.range_ordinal")) %>% 
  # Do some light housekeeping
  dplyr::select(-spp.num, -row.num)
  
# Check structure
dplyr::glimpse(nps_v02)
## tibble::view(nps_v02)

## ----------------------------- ##
# Fix Column Class Issues ----
## ----------------------------- ##

# Do needed wrangling on this front (see header)
nps_v03 <- nps_v02 %>% 
  # Fix date information
  dplyr::mutate(
    survey.year = as.numeric(stringr::str_sub(string = survey.date, start = 1, end = 4)),
    .before = survey.date) %>% 
  dplyr::mutate(
    survey.date = as.Date(stringr::str_sub(string = survey.date, start = 1, end = 10))) %>% 
  # Pull distance as a number
  dplyr::mutate(distance_m = stringr::str_extract(string = distance_m, pattern = "\\d")) %>% 
  # Get a numeric version of cover
  dplyr::mutate(cover_percent = dplyr::case_when(
    cover.range_ordinal == "<1%" ~ 0.5,
    cover.range_ordinal == "1-10%" ~ 5.5,
    cover.range_ordinal == "10-25%" ~ 17.5,
    cover.range_ordinal == "25-50%" ~ 37.5,
    cover.range_ordinal == "50-75%" ~ 62.5,
    cover.range_ordinal == "75-100%" ~ 87.5,
    T ~ NA))

# Check structure
dplyr::glimpse(nps_v03)

## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make a final object
nps_v99 <- nps_v03

# Check structure
dplyr::glimpse(nps_v99)

# Generate a better file name
(nps_tidyname <- paste0("shift_native-plant-soc-veg_",
  min(nps_v99$survey.date, na.rm = T), "_",
  max(nps_v99$survey.date, na.rm = T), ".csv"))

# Export the ground cover data
write.csv(x = nps_v99, na = '', row.names = F,
  file = file.path("data", "tidy", nps_tidyname))

# End ----