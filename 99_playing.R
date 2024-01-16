# Setup ---------------------------------------------------------------
library(motus)
library(DBI)
library(purrr)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
source("00_setup.R")


# Playing with numbers
db <- tagme(projects[1], dir = "Data/Raw", update = FALSE)

dbGetQuery(db, "SELECT COUNT(*) from tags")    # 168         # Super fast
dbGetQuery(db, "SELECT COUNT(*) from runs")    #  1,069,536  # Quite fast

dbGetQuery(db, "SELECT COUNT(*) from allruns") #  1,306,999  # Much slower
dbGetQuery(db, "SELECT COUNT(*) from alltags") # 25,650,756  # Soooooo slooooow

tbl(db, "runs") |>
  pull(runID) |>
  length()  # 1,069,536

tbl(db, "runs") |>
  filter(motusFilter == 1) |>
  pull(runID) |>
  length()  # 587,079




tbl(db, "allruns") |>
  pull(runID) |>
  length()

  

t <- tbl(db, "runs") |>
  filter(row_number() %in% seq(1, 100, 1))

t <- tbl(db, "runs") |>
  select(runID, tsBegin, tsEnd, motusTagID) |>
  collect() |>
  mutate(start = as_datetime(tsBegin),
         motusTagID = factor(motusTagID)) |>
  slice(as.integer(seq(1, n(), length.out = 200)))
  
ggplot(data = t, aes(x = start, y = motusTagID, group = motusTagID)) +
  geom_path()
