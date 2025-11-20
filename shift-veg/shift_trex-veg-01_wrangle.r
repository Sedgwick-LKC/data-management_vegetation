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

# Join by shared columns
trx_v01 <- dplyr::full_join(x = trx_list[[1]], y = trx_list[[2]],
    by = c("survey.id", "survey.date", "user", "survey.status"))

# Check structure
dplyr::glimpse(trx_v01)

## ----------------------------- ##
# Fix Data Shape ----
## ----------------------------- ##

# Do needed repairs to shape
trx_v02 <- trx_v01 %>% 
  # Add a row number
  dplyr::mutate(row.num = 1:nrow(.), .after = survey.date) %>% 
  # Do some standardizing of old column names
  dplyr::rename_with(.fn = ~ gsub("plot_data...", "", x = .)) %>% 
  # Flip into long format
  tidyr::pivot_longer(cols = -survey.id:-plot.id) %>% 
  # Collapse rows (necessary to reclaim desired data shape)
  dplyr::mutate(name = gsub("\\.cover|_cover", "", x = name)) %>% 
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
  dplyr::select(-name, -row.num)
  
# Check structure
dplyr::glimpse(trx_v02)
## tibble::view(trx_v02)

## ----------------------------- ##
# Fix Column Class Issues ----
## ----------------------------- ##

# Do needed wrangling on this front (see header)
trx_v03 <- trx_v02 %>% 
  # Fix date information
  dplyr::mutate(
    survey.year = as.numeric(stringr::str_sub(string = survey.date, start = 1, end = 4)),
    .before = survey.date) %>% 
  dplyr::mutate(
    survey.date = as.Date(stringr::str_sub(string = survey.date, start = 1, end = 10))) %>% 
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
dplyr::glimpse(trx_v03)

## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make a final object
trx_v99 <- trx_v03

# Check structure
dplyr::glimpse(trx_v99)

# Generate a better file name
(trx_tidyname <- paste0("shift_trex-veg_",
  min(trx_v99$survey.date, na.rm = T), "_",
  max(trx_v99$survey.date, na.rm = T), ".csv"))

# Export the ground cover data
write.csv(x = trx_v99, na = '', row.names = F,
  file = file.path("data", "tidy", trx_tidyname))

# End ----