source("00_functions.R")

# Motus credentials --------------------------
#
# Here we set the credentials for this session 
# (so you don't have to manually type it in everytime)
# 
# This code expects that you store your Motus login and password as 
# `URBAN_USER` and `URBAN_PASSWORD` in an .Renviron file
#
# - learn more about this file: https://rstats.wtf/r-startup.html#renviron
# - easily edit this file with usethis::edit_r_environ()

motus:::sessionVariable(name = "userLogin", val = Sys.getenv("URBAN_USER"))
motus:::sessionVariable(name = "userPassword", val = Sys.getenv("URBAN_PASSWORD"))


# Define projects -----------------------------------------------------

# Here we're using a small set of pilot projects
#
# - Urban Ecology of Birds in the Lower mainland of BC #484
# - Montreal Urban Forests #551 (191 tags but 62 on resident cardinals)
# - Montreal Connectivity #373 (105 tags)
# - Montreal moult migrant research #168 (113 tags)

projects <- c(484, 551, 373, 168)
projects <- setNames(projects, projects)

# Define data to include -----------------------------------------------

# TODO: What are 'near-passerines'? cf https://en.wikipedia.org/wiki/Near_passerine

# naturecounts::nc_metadata() # Update naturecounts taxonomy lists

species <- naturecounts::meta_species_taxonomy() |>
  filter(order_taxon == "Passeriformes")
