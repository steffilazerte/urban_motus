library(motus)

motus:::sessionVariable(name = "userLogin", val = Sys.getenv("URBAN_USER"))
motus:::sessionVariable(name = "userPassword", val = Sys.getenv("URBAN_PASSWORD"))

motus_vars$dataServerURL
motus_vars$userLogin

# - Urban Ecology of Birds in the Lower mainland of BC #484
# - Montreal Urban Forests #551 (191 tags but 62 on resident cardinals)
# - Montreal Connectivity #373 (105 tags)
# - Montreal moult migrant research #168 (113 tags)

t1 <- tagme(484, new = FALSE, update = TRUE, dir = "Data/Raw")
t2 <- tagme(551, new = FALSE, update = TRUE, dir = "Data/Raw")
t3 <- tagme(373, new = FALSE, update = TRUE, dir = "Data/Raw")
t4 <- tagme(168, new = FALSE, update = TRUE, dir = "Data/Raw")
