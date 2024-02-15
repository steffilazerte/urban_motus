remove_by_ID <- function(src, t, id_type = "batchID", ids) {
  if(length(ids) > 0) {
    if(t %in% DBI::dbListTables(src)) {
      n <- DBI::dbExecute(
        src, 
        glue::glue("DELETE FROM {`t`} WHERE {`id_type`} IN (",
                   glue::glue_collapse(ids, sep = ', '), 
                   ")"))
      if(n > 0) message(msg_fmt("  {n} rows deleted from {t}"))
    }
  }
}


filter_count <- function(dbs, filter) {
  before <- map(dbs, \(x) tbl(x, "runs") |> count(name = "n_before") |> collect()) |>
    list_rbind(names_to = "proj_id")
  after <- imap(dbs, \(x, y) {
    remove <- getRunsFilters(x, filter, y)
    tbl(x, "runs") |>
      anti_join(remove, by = c("runID", "motusTagID")) |>
      count(name = "n_after") |>
      collect()
  }) |>
    list_rbind()

  bind_cols(before, after) |>
    mutate(p_reduced = (n_before - n_after) / n_before * 100)
}

gt_theme <- function(data, ...) {
  data |>
    tab_options(
      table.border.top.width = px(3), 
      column_labels.font.weight = "bold",
      column_labels.border.bottom.width = px(3),
      data_row.padding = px(7),
      ...
    )
}
 
# Collect and transform ts values to date/times 
collect_ts <- function(db) {
  db |>
    collect() |>
    mutate(across(contains("ts", ignore.case = FALSE), as_datetime))
}

# Collect run/tag information for filters
collect_filter <- function(db) {
  db |>
    rename_with(~"motusTagID", .cols = any_of("tagID")) |>
    select(runID, motusTagID) |>
    mutate(probability = 0) |>
    collect()
}

anti_join_quick <- function(x, y, by = NULL, copy = FALSE, motus_filter = TRUE) {
  if(motus_filter) {
    y <- select(y, "runID", "tagID")
    if(is.null(by)) by <- c("runID", "tagID")
  }
  y <- mutate(y, ANTI = 1) 
  left_join(x, y, by = by, copy = copy) |>
    filter(is.na(ANTI)) |>
    select(-"ANTI")
}

# ---- custom_runs ----
custom_runs <- function(db) {
  # Replacement for NULL tsEnd values (i.e. today plus change)
  max_ts <- round(as.numeric(Sys.time()) + 1000)
  
  # Get Receivers
  r <- tbl(db, "recvDeps") |> 
    select("deviceID", "recvDeployID" = "deployID", "tsStartRecv" = "tsStart", "tsEndRecv" = "tsEnd") |>
    mutate(tsEndRecv = if_else(is.na(tsEndRecv), max_ts, tsEndRecv))
  
  # Get tags
  t <- tbl(db, "tagDeps") |> 
    select("tagID", "tagDeployID" = "deployID", "speciesID", "tsStartTag" = "tsStart", "tsEndTag" = "tsEnd") |>
    mutate(tsEndTag = if_else(is.na(tsEndTag), max_ts, tsEndTag))
  
  # Combine with runs
  tbl(db, "runs") |>
    select("runID", "tsBegin", "tsEnd", "tagID" = "motusTagID", "motusFilter") |>
    # Add in tags by tagID *and* overlap of start/end of tag deployment with the beginning of a run
    left_join(t, by = join_by(tagID, between(tsBegin, tsStartTag, tsEndTag))) |>
    # Add in batchRuns by runID (to get the batchID)
    left_join(tbl(db, "batchRuns"), by = "runID") |>
    # Add in batches by batchID (to get the deviceID)
    left_join(tbl(db, "batches") |> select("batchID", "motusDeviceID"), by = "batchID") |>
    # Add in receivers by deviceID *and* overlap of receiver deployment time with the beginning of a run
    left_join(r, by = join_by(motusDeviceID == deviceID, between(tsBegin, tsStartRecv, tsEndRecv))) |>
    # Keep only relevant data
    select(runID, motusFilter, 
           tagID, tagDeployID, speciesID, 
           recvDeployID, recvDeviceID = motusDeviceID, 
           tsBegin, tsEnd) |>
    distinct()
}

# ---- custom_hits ----
custom_hits <- function(db) {
  # Replacement for NULL tsEnd values (i.e. today plus change)
  max_ts <- round(as.numeric(Sys.time()) + 1000)
  
  # Get Receivers
  r <- tbl(db, "recvDeps") |> 
    select("deviceID", "recvDeployID" = "deployID", "tsStartRecv" = "tsStart", "tsEndRecv" = "tsEnd",
           "recvType" = "receiverType") |>
    mutate(tsEndRecv = if_else(is.na(tsEndRecv), max_ts, tsEndRecv))
  
  # Get tags
  t <- tbl(db, "tagDeps") |> 
    select("tagID", "tagDeployID" = "deployID", "speciesID", "tsStartTag" = "tsStart", "tsEndTag" = "tsEnd") |>
    mutate(tsEndTag = if_else(is.na(tsEndTag), max_ts, tsEndTag))  
  
  # Combine with the rest - And OMIT BAD filtered observations from previous step
  tbl(db, "hits") |>
    select(-"validated") |>
    # Add in runs and motusFilters
    left_join(tbl(db, "runs") |> select("runID", "motusFilter", "tagID" = "motusTagID"), by = "runID") |>
    # Add in tags by tagID *and* overlap of start/end of tag deployment with the beginning of a run
    left_join(t, by = join_by(tagID, between(ts, tsStartTag, tsEndTag))) |>
    # Filter out "Bad runs" from previous step
    left_join(tbl(db, "bad_data"), by = c("runID", "tagID")) |>
    filter(is.na(BAD)) |>
    select(-"BAD") |>
    # Add in batches by batchID (to get the deviceID)
    left_join(tbl(db, "batches") |> select("batchID", "motusDeviceID"), by = "batchID") |>
    # Add in receivers by deviceID *and* overlap of receiver deployment time with the beginning of a run
    left_join(r, by = join_by(motusDeviceID == deviceID, between(ts, tsStartRecv, tsEndRecv))) |>
    # Keep only relevant data
    select(-"batchID", -"tsStartRecv", -"tsEndRecv", -"tsStartTag", -"tsEndTag") |>
    rename("recvDeciveID" = "motusDeviceID")
}
