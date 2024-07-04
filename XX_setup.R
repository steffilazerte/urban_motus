# ---- packages ----
# Motus data and data bases
library(motus)     # Fetch motus data
library(DBI)       # Data bases
library(arrow)     # feather files

# Tidy data manipulation
library(purrr)     # Loops
library(furrr)     # Parallel loops
library(dplyr)     # Manipulate data
library(tidyr)     # Transform data
library(tibble)    # Data frames (ish)
library(stringr)   # Text manipulation/search
library(readr)     # Read data
library(lubridate) # Deal with date/times
library(assertr)   # Check data quality

# Data display
library(ggplot2)   # Figures
library(ggrepel)   # Push labels away from points in figures
library(patchwork) # Combine figures
library(gt)        # HTML tables

# Spatial data
library(sf)        # Work with spatial data
library(rnaturalearth) # Get maps
library(ebirdst)   # Fetch species ranges
library(units)     # Deal with units of distance and speed
library(ggspatial) # Plot spatial data
library(lutz)      # Timezones by location



# ---- constants ----
max_flight_speed <- set_units(72, "m/s")
max_tower_dist <- set_units(50, "km")
bout_cutoff <- set_units(30, "min")

# ---- credentials ----
motus:::sessionVariable(name = "userLogin", val = Sys.getenv("URBAN_USER"))
motus:::sessionVariable(name = "userPassword", val = Sys.getenv("URBAN_PASSWORD"))

# ---- projects ----
projects <- c(484, 551, 373, 168, # From Barbara and Elizabeth
              352, 364, 393, 417, 464, 515, 607) # Open Motus projects
projects <- setNames(projects, projects)


# ---- folders ----
fs::dir_create(c("Data/01_Raw", "Data/02_Datasets", "Data/03_Final"))

# ---- db_load ----
dbs <- map(projects, \(x) tagme(x, dir = "Data/01_Raw", update = FALSE))

# ---- db_species ----
# naturecounts::nc_metadata() # Update naturecounts taxonomy lists as needed
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

# ----- arrow ------
arws <- tibble(file = list.files("Data/02_Datasets/hits/", recursive = TRUE, full.names = TRUE)) |>
  mutate(proj_id = str_extract(file, "(?<=proj_id\\=)\\d+"),
         species_id = str_extract(file, "(?<=speciesID\\=)\\d+"),
         year = str_extract(file, "(?<=year\\=)\\d+")) |>
  mutate(across(-file, as.integer))

# ---- functions ----
source("XX_functions.R")
