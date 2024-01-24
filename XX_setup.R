
# ---- packages ----
# Motus data and data bases
library(motus)
library(DBI)  # Data bases

# Tidy data manipulation
library(purrr)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(ggplot2)
library(readr)
library(assertr) # Check data quality

# Data display
library(gt)

# Spatial data
library(sf)
library(rnaturalearth)
library(ebirdst) # Species ranges
library(units)

# ---- credentials ----
motus:::sessionVariable(name = "userLogin", val = Sys.getenv("URBAN_USER"))
motus:::sessionVariable(name = "userPassword", val = Sys.getenv("URBAN_PASSWORD"))

# ---- projects ----
projects <- c(484, 551, 373, 168)
projects <- setNames(projects, projects)

# ---- data ----
# naturecounts::nc_metadata() # Update naturecounts taxonomy lists

species <- naturecounts::meta_species_taxonomy() |>
  filter(order_taxon %in% c("Passeriformes", "Piciformes"))

# ---- functions ----
source("XX_functions.R")