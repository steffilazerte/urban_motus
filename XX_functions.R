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
    select("deviceID" = "deviceID", "recvDeployID" = "deployID", "tsStartRecv" = "tsStart", "tsEndRecv" = "tsEnd",
           "recvType" = "receiverType", "recvDeployLat" = "latitude", "recvDeployLon" = "longitude") |>
    mutate(tsEndRecv = if_else(is.na(tsEndRecv), max_ts, tsEndRecv))
  
  # Get tags
  t <- tbl(db, "tagDeps") |> 
    select("tagID", "tagDeployID" = "deployID", "speciesID", "tsStartTag" = "tsStart", "tsEndTag" = "tsEnd", "test") |>
    mutate(tsEndTag = if_else(is.na(tsEndTag), max_ts, tsEndTag))
  
  # Get batches
  b <- tbl(db, "batchRuns") |>
    distinct()
  
  # Combine with runs
  tbl(db, "runs") |>
    rename("tagID" = "motusTagID") |>
    # Add in tags by tagID *and* overlap of start/end of tag deployment with the beginning of a run
    left_join(t, by = join_by(tagID, between(tsBegin, tsStartTag, tsEndTag))) |>
    slice_max(order_by = tsStartTag, by = "runID", with_ties = FALSE) |> # If multiple deps, take only the one with max start
    # Add in batchRuns by runID (to get the batchID)
    left_join(b, by = "runID") |>
    slice_max(order_by = batchID, by = "runID", with_ties = FALSE) |> # If multiple batches, take only the latest one
    # Add in batches by batchID (to get the deviceID)
    left_join(tbl(db, "batches") |> select("batchID", "motusDeviceID"), by = "batchID") |>
    # Add in receivers by deviceID *and* overlap of receiver deployment time with the beginning of a run
    left_join(r, by = join_by(motusDeviceID == deviceID, between(tsBegin, tsStartRecv, tsEndRecv))) |>
    slice_max(order_by = tsStartRecv, by = "runID", with_ties = FALSE) |> # If multiple deps, take only the one with max start
    rename("recvDeviceID" = "motusDeviceID") |>
    select(-"batchIDbegin") |>
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
    # # Filter out "Bad runs" from previous step
    # left_join(tbl(db, "bad_data"), by = c("runID", "tagID")) |>
    # filter(is.na(BAD)) |>
    # select(-"BAD") |>
    # Add in batches by batchID (to get the deviceID)
    left_join(tbl(db, "batches") |> select("batchID", "motusDeviceID"), by = "batchID") |>
    # Add in receivers by deviceID *and* overlap of receiver deployment time with the beginning of a run
    left_join(r, by = join_by(motusDeviceID == deviceID, between(ts, tsStartRecv, tsEndRecv))) |>
    # Keep only relevant data
    select(-"batchID", -"tsStartRecv", -"tsEndRecv", -"tsStartTag", -"tsEndTag") |>
    rename("recvDeciveID" = "motusDeviceID")
}
plot_bouts <- function(trans, bouts, tagDeployID, runs = NULL, hits = NULL) {
  
  trans <- filter(trans, tagDeployID == .env$tagDeployID)
  bouts <- filter(bouts, tagDeployID == .env$tagDeployID)
  
  g1 <- plot_map(trans)
  
  g2 <- plot_coord(trans, bouts, "lat")
  g3 <- plot_coord(trans, bouts, "lon")
  g_coord <- (g2 + g3 + plot_layout(guides = "collect"))
  
  if(!is.null(runs) & !is.null(hits)) {
    g4 <- plot_check(trans, runs, hits)
    if(!is.null(g4)) g_coord <- g_coord / g4
  }
  
  g1 + g_coord
}

plot_map <- function(trans) {
  recvs <- select(trans, lat1, lon1, stn1) |>
    distinct()
  ggplot(trans, aes(x = lon1, y = lat1)) +
    annotation_map_tile(type = "cartolight", zoomin = 0) +
    geom_spatial_segment(
      aes(xend = lon2, yend = lat2, linetype = problem, linewidth = problem, colour = date),
      arrow = arrow(length = unit(10, "pt"), type = "closed"), crs = 4326, na.rm = TRUE) +
    geom_spatial_label_repel(data = recvs, aes(label = stn1), crs = 4326) +
    scale_size_date(range = c(6, 3)) +
    scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dotted")) +
    scale_linewidth_manual(values = c("FALSE" = 0.75, "TRUE" = 1.55)) +
    scale_colour_viridis_c(option = "inferno", transform = "date", begin = 0.15, end = 0.95)
}

plot_coord <- function(trans, bouts, coord) {
  lim <- range(bouts$dateBegin)
  lim[2] <- lim[2] + difftime(lim[2], lim[1], units = "days") * 0.1
  lim <- as_datetime(lim)
  
  bouts <- mutate(bouts, recvDeployID = factor(recvDeployID))
  
  recvs <- select(bouts, recvDeployLat, recvDeployLon, recvDeployID) |>
    distinct()
  pal <- viridis::magma(n = length(recvs$recvDeployID), begin = 0.1) |>
    set_names(levels(recvs$recvDeployID))
  pal_txt <- farver::decode_colour(pal, "rgb", "hcl")
  pal_txt <- if_else(pal_txt[, "l"] > 50, "black", "white") |>
    set_names(levels(recvs$recvDeployID))
  
  c1 <- str_subset(names(bouts), regex(coord, ignore_case = TRUE))
  c2 <- paste0(coord, 1)
  c3 <- paste0(coord, 2)
  
  ggplot(bouts, aes(x = timeBegin, y = .data[[c1]])) +
    theme_bw() +
    geom_segment(aes(xend = timeEnd, colour = recvDeployID), na.rm = TRUE, linewidth = 3) +
    geom_point(data = arrange(bouts, desc(recvDeployID)), aes(colour = recvDeployID), na.rm = TRUE, size = 2) +
    geom_segment(data = trans, aes(
      x = e1, xend = b2, y = .data[[c2]], yend = .data[[c3]], linetype = problem), na.rm = TRUE) +
    geom_label_repel(data = recvs, aes(label = recvDeployID, y = .data[[c1]], x = lim[2], 
                                       fill = recvDeployID, colour = recvDeployID),
               size = 2.5, direction = "y", point.size = NA, box.padding = 0, label.padding = 0.15,
               colour = pal_txt[recvs$recvDeployID]) +
    scale_fill_manual(values = pal) +
    scale_colour_manual(values = pal) +
    scale_x_datetime(limits = lim)
}

plot_check <- function(trans, runs, hits) {
  p <- trans |>
    filter(problem) |>
    mutate(b0 = if_else(difftime(e1, b1) > difftime(e2, b2), b2, b1) - hours(2),
           e0 = if_else(difftime(e1, b1) > difftime(e2, b2), e2, e1) + hours(2)) |>
    select(tagDeployID, b0, e0)
  
  if(nrow(p) > 1) {
    r <- runs |>
      semi_join(p, join_by(tagDeployID, timeBegin >= b0, timeEnd <= e0)) |>
      unnest(runID) |>
      pull(runID)
    
    h <- map(hits, \(x) {
      x |>
        filter(runID %in% r) |>
        collect() |>
        left_join(select(runs, runID, len, ant), by = "runID")
    }) |>
      list_rbind()
    
    h_lab <- summarize(h, time = min(time), sig = median(sig), .by = c("recvDeployID", "date")) |>
      mutate(recvDeployID = as.factor(recvDeployID))
    
    g <-  ggplot(data = h, aes(x = time, y = sig, colour = factor(recvDeployID))) +
      theme_bw() +
      geom_point(size = 1, aes(shape = ant)) +
      geom_label(data = h_lab, aes(label = recvDeployID), size = 3, hjust = -0.2) +
      scale_colour_viridis_d(option = "magma", end = 0.9) +
      facet_wrap(~date, scales = "free")
  } else g <- NULL
  g
}


# Create bouts from runs by adding them together and then resorting.
# The resorting helps to get at bouts that overlap each other.
create_bouts <- function(x, cutoff) {
  x1 <- x |> 
    mutate(
      lag1 = as_units(difftime(lead(timeBegin), timeEnd, units = "min")),
      lag2 = as_units(difftime(lead(timeEnd), timeEnd, units = "min")),
      lag = pmin(lag1, lag2),
      prev_lag = lag(lag, default = set_units(Inf, "min")),
      start = prev_lag >= .env$cutoff,
      bout = cumsum(start))
  
  x1 |>
    group_by(bout) |>
    summarize(timeBegin = min(timeBegin), timeEnd = max(timeEnd), 
              runID = list(unlist(runID)),
              n_runs = n(), 
              len = list(unlist(len)),
              ant = list(unlist(ant)),
              totalTime = sum(difftime(timeEnd, timeBegin, units = "min")),
              .groups = "drop")
}

# Check function to verify that the bouts created have no overlaps
find_overlaps <- function(x2, type = "logical") {
  #           comp B/E2  vs. B/E1
  m1 <- outer(x2$timeBegin, x2$timeBegin, '>=')
  m2 <- outer(x2$timeEnd, x2$timeEnd, '<=')
  m3 <- outer(x2$timeBegin, x2$timeEnd, '<=')
  m4 <- outer(x2$timeEnd, x2$timeBegin, '>=')
  
  ovlps <- (m1 & m3) | (m2 & m4)
  
  if(type == "df") {
    nms <- paste0(x2$recvDeployID, "_", x2$bout)
    dimnames(ovlps) <- list(nms, nms)
    r <- expand_grid(id1 = nms, id2 = nms) |>
      mutate(overlap = as.vector(ovlps)) |>
      filter(overlap, id1 != id2)
  } else if(type == "logical") {
    r <- any(colSums(ovlps) > 1)
  }
  r
}

calc_trans <- function(x) {
  x |>
    rename(id1 = id) |>
    #group_by(tagDeployID) |>
    mutate(next_stn = lead(recvDeployID), 
           #stn_pair = paste0(recvDeployID, "_", next_stn),
           #lag1 = as_units(difftime(lead(timeBegin), timeEnd, units = "min")),
           #lag2 = as_units(difftime(lead(timeEnd), timeEnd, units = "min")),
           #time_diff = pmin(lag1, lag2),
           id2 = lead(id1), .by = "tagDeployID") |>
    filter(recvDeployID != next_stn) |>
    select(tagDeployID, id1, id2)
}
