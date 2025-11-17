## ------------------------------------------------ ##
# Pre-Burn Ground Cover - Wrangle
## ------------------------------------------------ ##
# Purpose:
## Wrangle the 'raw' pre-burn ground cover data
## Initial data structure inherited from WildNote app

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, readxl)

# Get set up
source("00_setup.r")

# Clear environment
rm(list = ls()); gc()

## ----------------------------- ##
# Load Data ----
## ----------------------------- ##

# Define file path to desired data
cov_file <- file.path("data", "raw", "Ground-Cover-Pre-Burn.xlsx")

# Check existing sheets in the data
(cov_sheets <- readxl::excel_sheets(path = cov_file))

# Read in both
cov_sheet1 <- readxl::read_excel(path = cov_file, sheet = cov_sheets[1], col_type = "text")
cov_sheet2 <- readxl::read_excel(path = cov_file, sheet = cov_sheets[2], col_type = "text")

# Check structure
dplyr::glimpse(cov_sheet1)
dplyr::glimpse(cov_sheet2)

# Join by shared columns
cov_v01 <- dplyr::left_join(x = cov_sheet2, y = cov_sheet1,
  by = c("Survey ID", "Survey Date", "User"))

# Check that out
dplyr::glimpse(cov_v01)
## tibble::view(cov_v01)


# End ----
