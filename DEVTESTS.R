# This is a test script for comparing alternate ways of coding and fetching data


source("XX_setup.R")
x <- dbs[[1]]

max_ts <- round(as.numeric(Sys.time()) + 1000)

r <- tbl(x, "recvDeps") |> 
  select("deviceID", "deployID", "tsStart", "tsEnd") |>
  mutate(tsEnd = if_else(is.na(tsEnd), max_ts, tsEnd))

t <- tbl(x, "tagDeps") |> 
  select("tagID", "speciesID", "tsStart", "tsEnd") |>
  mutate(tsEnd = if_else(is.na(tsEnd), max_ts, tsEnd))

tbl(x, "recvDeps") |>
  filter(deviceID == 2629) |>
  select(deviceID, deployID, projectID, status, name, latitude, longitude, tsStart, tsEnd)

runs <- tbl(x, "runs") |>
  select("runID", "tsBegin", "motusTagID") |>
  left_join(t, by = join_by(motusTagID == tagID, between(tsBegin, tsStart, tsEnd))) |>
  select(-tsStart, -tsEnd) |>
  left_join(tbl(x, "batchRuns"), by = "runID") |>
  left_join(tbl(x, "batches") |> select("batchID", "motusDeviceID"), by = "batchID") |>
  left_join(r, by = join_by(motusDeviceID == deviceID, between(tsBegin, tsStart, tsEnd))) |>
  select(-tsStart, -tsEnd, -motusDeviceID) |>
  filter(!is.na(speciesID)) |>
  distinct() |>
  collect() |>
  rename(recvDeployID = deployID)

comp <- tbl(x, "allruns") |> 
  select("runID", "tsBegin", contains("corrected"), "motusTagID", "speciesID", "batchID", "recvDeployID") |>
  filter(!is.na(speciesID)) |>
  distinct() |>
  collect()


tbl(x, "allruns") |>
  select("runID", "tsBegin", contains("corrected"), "deviceID", "recvDeployID") |>
  filter(deviceID == 2629)

tbl(x, "recvDeps") |>
  select("deviceID", "deployID", "tsStart", "tsEnd") |>
  filter(deviceID == 2629)