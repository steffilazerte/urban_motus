
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
library(lubridate)
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
projects <- c(484, 551, 373, 168, # From Barbara and Elizabeth
              352, 364, 393, 417, 464, 515, 607) # Open Motus projects
projects <- setNames(projects, projects)

# ---- db_load ----
dbs <- map(projects, \(x) tagme(x, dir = "Data/Raw", update = FALSE))

# ---- db_species ----
# naturecounts::nc_metadata() # Update naturecounts taxonomy lists
species_list <- naturecounts::meta_species_taxonomy() |>
  filter(order_taxon %in% c("Passeriformes", "Piciformes"))

sp <- tbl(dbs[[1]], "tagDeps") |>
  # Only get species which are in our specific projects
  filter(projectID %in% projects) |>
  semi_join(x = tbl(dbs[[1]], "species"), y = _,
            by = c("id" = "speciesID")) |>
  select(-"group", -"sort") |>
  distinct() |>
  collect() |>
  # Keep only chosen species
  filter(id %in% species_list$species_id) |>
  arrange(id) |>
  # Fix subspecies names for joining with eBird (e.g., myrtle yellow-rumped warbler)
  mutate(
    scientific_motus = scientific,
    scientific = str_replace(scientific, "Setophaga coronata coronata", "Setophaga coronata"))

# ---- functions ----
source("XX_functions.R")
