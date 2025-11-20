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

# Join by shared columns
plt_v01 <- dplyr::full_join(x = plt_list[[1]], y = plt_list[[2]],
    by = c("survey.id", "survey.date", "user", "survey.status"))

# Check structure
dplyr::glimpse(plt_v01)

## ----------------------------- ##
# Fix Data Shape ----
## ----------------------------- ##

# Do needed repairs to shape
plt_v02 <- plt_v01 %>% 
  # Add a row number
  dplyr::mutate(row.num = 1:nrow(.), .after = survey.date) %>% 
  # Do some standardizing of old column names
  dplyr::rename_with(.fn = ~ gsub("plot_data...", "", x = .)) %>% 
  dplyr::rename_with(.fn = ~ gsub(".cover", "_cover", x = .)) %>% 
  # Flip into long format
  tidyr::pivot_longer(cols = -survey.id:-plot.id) %>% 
  # Remove notes entries (needed because they're not clear about what species they pertain to)
  dplyr::filter(name != "notes") %>% 
  # Collapse rows (necessary to reclaim desired data shape)
  dplyr::mutate(name = gsub("_cover", "", x = name)) %>% 
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
dplyr::glimpse(plt_v02)
## tibble::view(plt_v02)

## ----------------------------- ##
# Fix Column Class Issues ----
## ----------------------------- ##

# Do needed wrangling on this front (see header)
plt_v03 <- plt_v02 %>% 
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
dplyr::glimpse(plt_v03)

## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make a final object
plt_v99 <- plt_v03

# Check structure
dplyr::glimpse(plt_v99)

# Generate a better file name
(plt_tidyname <- paste0("shift_sedgewick-plot-veg-survey_",
  min(plt_v99$survey.date, na.rm = T), "_",
  max(plt_v99$survey.date, na.rm = T), ".csv"))

# Export the ground cover data
write.csv(x = plt_v99, na = '', row.names = F,
  file = file.path("data", "tidy", plt_tidyname))

# End ----