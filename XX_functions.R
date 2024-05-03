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

load_hits <- function() {
  map(projects, \(x) {
    f <- filter(arws, proj_id == x) |>
      pull(file)
    open_dataset(f, format = "feather")
  })
}

plot_bouts <- function(trans, trans_clean = NULL, bouts, tagDeployID, save = TRUE) {
  
  trans <- filter(trans, tagDeployID == .env$tagDeployID)
  if(is.null(trans_clean)) trans_clean <- trans
  trans_clean <- filter(trans_clean, tagDeployID == .env$tagDeployID)
  bouts <- filter(bouts, tagDeployID == .env$tagDeployID)

  g1 <- plot_map(trans)
  
  lon <- range(c(trans$lon1, trans$lon2)) |> diff()
  lat <- range(c(trans$lat1, trans$lat2)) |> diff()
  ratio <- abs(lat) / abs(lon)

  g2 <- plot_coord(trans, trans_clean, bouts, "lat")
  g3 <- plot_coord(trans, trans_clean, bouts, "lon")
  
  #if(ratio < 2) g_coord <- g2 + g3 else g_coord <- g2 / g3
  g_coord <- g2 / g3
  g_coord <- g_coord + plot_layout(guides = "collect")
  
  #if(ratio > 1) g <- g1 + g_coord else g <- g1 / g_coord
  g <- g1 + g_coord
  g <- g + 
    plot_annotation(title = paste0("tagDeployID: ", tagDeployID, " (", 
                                   as_date(min(bouts$timeBegin)), " to ", 
                                   as_date(max(bouts$timeEnd)), ")"))
  
  if(save) {
    ggsave(plot = g, filename = paste0("check_", tagDeployID, ".pdf"), width = 20, height = 12)
  }
  g
}

plot_map <- function(trans, tagDeployID = NULL) {
  
  if(!is.null(tagDeployID)) trans <- filter(trans, tagDeployID == .env[["tagDeployID"]])
  if(!"problem" %in% names(trans)) {
    trans$problem <- trans$problem_fast <- trans$problem_manual <- FALSE
    trans$resolved <- TRUE
  }

  recvs <- select(trans, stn1, lat1, lon1, stn2, lat2, lon2) |>
    pivot_longer(cols = c("stn1", "stn2", "lat1", "lat2", "lon1", "lon2"), 
                 names_to = c(".value", "pair"), 
                 names_pattern = "([a-z]{2,3})(\\d+)") |>
    select(-"pair") |>
    distinct()

  bb <- trans |>
    summarize(xmin = min(c(lon1, lon2)), xmax = max(c(lon1, lon2)),
              ymin = min(c(lat1, lat2)), ymax = max(c(lat1, lat2)))
  
  ry <- diff(range(bb[3:4]))
  rx <- diff(range(bb[1:2]))
  
  if(rx > 31 & any(!trans$problem)) {
    bb <- filter(trans, !problem) |>
      summarize(xmin = min(c(lon1, lon2)), xmax = max(c(lon1, lon2)),
                ymin = min(c(lat1, lat2)), ymax = max(c(lat1, lat2)))

    ry <- diff(range(bb[3:4]))
    rx <- diff(range(bb[1:2]))
  }
  
  if(rx < 0.5) {
    add <- (0.5 - rx)/2
    bb[1:2] <- bb[1:2] + (c(-1, 1) * add)
  }
  if(ry < 2) {
    add <- (2 - ry)/2
    bb[3:4] <- bb[3:4] + (c(-1, 1) * add)
  }
  
  ry <- diff(range(bb[3:4]))
  rx <- diff(range(bb[1:2]))
  ratio <- ry/rx
  
  if(ratio > 2) {
    rx1 <- ry/2
    add <- (rx1 - rx)/2
    bb[1:2] <- bb[1:2] + (c(-1, 1) * add)
  } else if (ratio < 2) {
    ry1 <- rx * 2
    add <- (ry1 - ry)/2
    bb[3:4] <- bb[3:4] + (c(-1, 1) * add)
  }
  
  if(any(trans$problem_fast | trans$problem_manual)) {
    labs <- trans |>
      filter(problem_fast | problem_manual) |>
      summarize(
        lon = lon1, 
        lat = lat1,
        stn_pair = stn_pair[1],
        problem_chr = pmap_chr(list(problem_fast, problem_manual),
                               \(x, y) paste0(c("too fast", "manual")[c(x, y)], collapse = ", ")),
        nudge_x = sign(lon2 - lon1) * ((bb[[2]] - bb[[1]]) * 0.1),
        nudge_y = sign(lat1 - lat2) * ((bb[[4]] - bb[[3]]) * 0.05), 
        .by = trans_id) |>
      select(-trans_id) |>
      distinct()
  } else labs <- data.frame()
  
  g <- ggplot(trans, aes(x = lon1, y = lat1)) +
    annotation_map_tile(type = "cartolight", zoomin = 0) +
    geom_spatial_segment(
      data = filter(trans, resolved, problem),
      aes(xend = lon2, yend = lat2), colour = "grey", linewidth = 2,
      arrow = arrow(length = unit(10, "pt"), type = "closed"), crs = 4326, na.rm = TRUE) +
    coord_sf(xlim = unlist(bb[1:2]), ylim = unlist(bb[3:4]), crs = 4326)
  
  if(nrow(labs) > 0) {
    g <- g + geom_spatial_label(data = labs, 
                               aes(x = lon, y = lat, label = problem_chr), 
                               nudge_x = labs$nudge_x, nudge_y = labs$nudge_y, 
                               colour = "grey", crs = 4326)
  }
  g +
    geom_spatial_segment(
      data = filter(trans, (resolved & !problem) | (!resolved)),
      aes(xend = lon2, yend = lat2, colour = mid_date, linetype = problem),
      arrow = arrow(length = unit(10, "pt"), type = "closed"), crs = 4326, na.rm = TRUE) +
    geom_spatial_label_repel(data = recvs, aes(label = stn, x = lon, y = lat), 
                             crs = 4326) +
    
    scale_size_date(range = c(6, 3)) +
    scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dotted"),
                          guide = "none") +
    scale_linewidth_manual(values = c("FALSE" = 0.75, "TRUE" = 1.55),
                           guide = "none") +
    scale_colour_viridis_c(option = "inferno", transform = "date", 
                           begin = 0.20, end = 0.85) +
    guides(colour = guide_colourbar(reverse = TRUE))
}

plot_coord <- function(trans, trans_clean, bouts, coord) {
  lim <- range(bouts$dateBegin)
  lim[2] <- lim[2] + difftime(lim[2], lim[1], units = "days") * 0.1
  lim <- as_datetime(lim)
  
  if(!"bad" %in% names(trans)) trans$bad <- FALSE
  if(!"bad" %in% names(trans_clean)) trans_clean$bad <- FALSE
  if(!"bad" %in% names(bouts)) bouts$bad <- FALSE
  if(!"problem" %in% names(trans)) trans$problem <- FALSE
  if(!"problem" %in% names(trans_clean)) trans_clean$problem <- FALSE
  
  bouts <- mutate(bouts, stn_group = factor(stn_group))
  
  recvs <- select(bouts, recvDeployLat, recvDeployLon, stn_group) |>
    distinct()
  pal <- viridis::magma(n = length(recvs$stn_group), begin = 0.10, end = 0.9) |>
    set_names(levels(recvs$stn_group))
  pal_txt <- farver::decode_colour(pal, "rgb", "hcl")
  pal_txt <- if_else(pal_txt[, "l"] > 50, "black", "white") |>
    set_names(levels(recvs$stn_group))
  
  c1 <- str_subset(names(bouts), regex(coord, ignore_case = TRUE))
  c2 <- paste0(coord, 1)
  c3 <- paste0(coord, 2)
  
  b <- filter(bouts, !bad)
  b_bad <- filter(bouts, bad)
  
  t <- trans_clean #filter(trans, !problem | (problem & !resolved))
  t_bad <- anti_join(trans, trans_clean, by = c("id1", "id2"))
  
  ggplot(b, aes(x = timeBegin, y = .data[[c1]])) +
    theme_bw() +
    
    # Good bouts (or unresolved)
    geom_segment(aes(xend = timeEnd, colour = stn_group), na.rm = TRUE, linewidth = 3) +
    geom_point(data = arrange(b, desc(stn_group)), aes(colour = stn_group), na.rm = TRUE, size = 2) +
    
    # Bad bouts (resolved)
    geom_segment(data = b_bad, aes(xend = timeEnd), colour = "grey", na.rm = TRUE, linewidth = 3) +
    geom_point(data = arrange(b_bad, desc(stn_group)), colour = "grey", na.rm = TRUE, size = 2) +
    
    # Good trans (or unresolved)
    geom_segment(data = t, aes(
      x = e1, xend = b2, y = .data[[c2]], yend = .data[[c3]], linetype = problem), na.rm = TRUE) +
    
    # Bad trans (resolved)
    geom_segment(data = t_bad, aes(
      x = e1, xend = b2, y = .data[[c2]], yend = .data[[c3]], linetype = problem), 
      colour = "grey", na.rm = TRUE) +
    
    geom_label_repel(data = recvs, aes(label = stn_group, y = .data[[c1]], x = lim[2], 
                                       fill = stn_group, colour = stn_group),
               size = 2.5, direction = "y", point.size = NA, box.padding = 0, label.padding = 0.15,
               colour = pal_txt[recvs$stn_group]) +
    scale_fill_manual(values = pal) +
    scale_colour_manual(values = pal) +
    scale_x_datetime(limits = lim)
}

plot_check <- function(hits, tagDeployID) {
  h <- filter(h, tagDeployID == .env[["tagDeployID"]]) |>
    arrange(time)

  if(nrow(h) > 0) {

    sig_range <- function(limits) {
      diff <- abs(diff(limits))
      case_when(
        diff < 5 ~ limits + c(-7.5, 7.5),
        diff < 10 ~ limits + c(-5, 5),
        diff < 15 ~ limits + c(-2.5, 2.5),
      TRUE ~ limits)
    }

    g <- list()
    for(i in unique(h$trans_id)) {
      hh <- filter(h, trans_id == i) |>
        arrange(time) |>
        mutate(problem_pair = factor(problem_pair, levels = unique(problem_pair)))
        
      h_lab <- hh |>
        summarize(time = min(time), sig = median(sig), 
                  .by = c("recvDeployID", "problem_pair")) |>
        mutate(recvDeployID = as.factor(recvDeployID))
      
      g1 <- ggplot(data = hh, aes(x = time, y = sig, colour = factor(recvDeployID),
                                shape = ant)) +
        theme_bw() +
        theme(legend.position = "none", aspect.ratio = 1, plot.title = element_text(hjust = 0.5)) +
        geom_point(size = 1) +
        geom_line(stat = "smooth", alpha = 0.5, method = "gam", formula = y ~ s(x, k = 10, bs = "cs")) +
        geom_label(data = h_lab, aes(label = recvDeployID, shape = NULL), size = 3, hjust = -0.2) +
        scale_colour_viridis_d(option = "magma", end = 0.8) +
        scale_y_continuous(limits = sig_range) +
        scale_x_datetime(date_labels = "%H:%M") +
        facet_wrap(~ problem_pair, scales = "free", axes = "all") +
        labs(title = paste0("Transition ", i))
      g[[length(g) + 1]] <- g1
    }
    
    if(length(g) > 1) ncol <- 2 else ncol <- 1
    
    g <- wrap_plots(g, ncol = ncol) +
      plot_layout(heights = 1) +
      plot_annotation(title = paste0("tagDeployID: ", tagDeployID)) &
      theme(plot.margin = unit(c(1,0,1,1), units = "pt"))
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
              total_time = sum(difftime(timeEnd, timeBegin, units = "min")),
              .groups = "drop")
}


find_overlaps <- function(x2, type = "logical") {
  #if(3551 %in% x2$stn_group) browser()
  #           comp B/E2  vs. B/E1
  
  m1 <- outer(x2$timeBegin, x2$timeBegin, '>=')
  m2 <- outer(x2$timeEnd, x2$timeEnd, '<=')
  
  m3 <- outer(x2$timeBegin, x2$timeEnd, '<=')
  m4 <- outer(x2$timeEnd, x2$timeBegin, '>=')
  
  ovlps <- (m1 & m3) | (m2 & m4)
  ovlps <- ovlps | t(ovlps) # either id1/id2 or id2/id1
  
  if(type == "df") {
    nms <- paste0(x2$stn_group, "_", x2$bout)
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
    mutate(next_stn = lead(stn_group), 
           id2 = lead(id1), .by = "tagDeployID") |>
    filter(stn_group != next_stn) |>
    select(tagDeployID, id1, id2)
}

# ---- load_runs ----
load_runs <- function() {
  noise_runs <- open_dataset("Data/Datasets/noise_runs.feather", format = "feather")
  noise_hits <- open_dataset("Data/Datasets/noise_hits.feather", format = "feather")
  
  open_dataset("Data/Datasets/runs", format = "feather") |>
    anti_join(noise_runs, by = c("runID")) |>
    anti_join(noise_hits, by = c("runID")) |>
    filter(len > 2) |>
    select(runID, timeBegin, timeEnd, tagID, ant, len, motusFilter, tagDeployID, 
           speciesID, starts_with("recvDeploy"), matches("date|month|year")) |>
    arrange(timeBegin) |>
    collect()
}

# ---- create_overlapping ----
create_overlapping <- function(bouts) {
  nest(bouts, data = -tagDeployID) |>
    mutate(overlaps = map(data, \(x) find_overlaps(x, "df"), .progress = interactive())) |>
    select(-"data") |>
    unnest("overlaps") |>
    mutate(id1 = paste0(tagDeployID, "_", id1),
           id2 = paste0(tagDeployID, "_", id2))
}

# ---- create_trans ----
create_trans <- function(bouts, overlapping_bouts, dist) {
  
  # Get transitions between stations
  trans <- bind_rows(
    bouts |> 
      arrange(timeBegin) |>
      calc_trans(),
    bouts |> 
      arrange(timeEnd) |>
      calc_trans()
  ) |>
    distinct() |>
    
    # Add in any missing transitions with overlapping bouts
    full_join(overlapping_bouts, by = c("tagDeployID", "id1", "id2")) |>
    # Add in data related to the first station
    left_join(select(bouts, "stn1" = "stn_group", "id1" = "id", 
                     "b1" = "timeBegin", "e1" = "timeEnd", 
                     "recv1" = "recvDeployID", "lat1" = "recvDeployLat", "lon1" = "recvDeployLon"), 
              by = "id1") |>
    # Add in data related to the second station
    left_join(select(bouts, "stn2" = "stn_group", "id2" = "id", 
                     "b2" = "timeBegin", "e2" = "timeEnd", 
                     "recv2" = "recvDeployID", "lat2" = "recvDeployLat", "lon2" = "recvDeployLon"), 
              by = "id2") |>
    
    # Identify transition pairs
    mutate(stn_pair = paste0(recv1, "_", recv2)) |>
    
    # Add in distance between stations
    left_join(select(dist, "stn_pair" = "next_pair", "next_dist", "min_time"),
              by = "stn_pair") |>
    
    # Calculate the time and speed taken to move between stations
    mutate(trans_id = row_number(),
           overlap = replace_na(overlap, FALSE),
           lag1 = as_units(difftime(b2, e1, units = "min")),
           lag2 = as_units(difftime(e2, e1, units = "min")),
           time_diff = pmin(lag1, lag2),
           time_diff = set_units(time_diff, "s"),
           speed = set_units(next_dist, "m") / time_diff,
           time_diff = as.duration(as.numeric(time_diff)),
           time_diff = if_else(overlap, duration(0), time_diff),
           # Mid-point date
           mid_date = as_date(e1 + (b2 - e1)/2))
  
  if(nrow(trans) > 1) {
    trans <- trans |>
      # Get rid of duplicate transitions when have overlapping pairs
      mutate(id_pair = map2_chr(id1, id2, \(x, y) paste0(sort(c(x, y)), collapse = " - "))) |>
      arrange(b1) |>
      slice(1, .by = "id_pair") |>
      select(-"id_pair") |>
      # Seasonal details
      mutate(migration = case_when(year(e1) != year(b2) ~ NA,
                                   month(e1) %in% 8:12 & month(b2) %in% 8:12 ~ "south",
                                   month(e1) %in% 2:7 & month(b2) %in% 2:7 ~ "north",
                                   TRUE ~ NA),
             connected = speed > set_units(5, "m/s"))
  }  else trans <- mutate(trans, migration = NA_character_, connected = NA)
  
  trans
}

# ---- id_problems ----
id_problems <- function(trans, problems_manual = NULL) {
  
  # Identify suspiciously short bouts related to transitions
  #b <- filter(bouts, n_runs < 5, len_max < 10) |>
  # pull(id)  
  
  # Add manual problems
  if(!is.null(problems_manual)) {
    trans_fix1 <- semi_join(trans, rename(problems_manual, "stn1" = "stn_group"),
                           by = join_by("tagDeployID", "stn1", within("b1", "e1", "start", "end"))) |>
      mutate(problem_manual = TRUE)
    
    trans_fix2 <- semi_join(trans, rename(problems_manual, "stn2" = "stn_group"),
                            by = join_by("tagDeployID", "stn2", within("b2", "e2", "start", "end"))) |>
      mutate(problem_manual = TRUE)
    
    trans <- trans |>
      mutate(problem_manual = FALSE) |>
      rows_update(trans_fix1, by = c("tagDeployID", "stn1", "trans_id")) |>
      rows_update(trans_fix2, by = c("tagDeployID", "stn2", "trans_id"))
      
  } else trans$problem_manual <- FALSE
  
  trans |>
    mutate(
      # Identify problematic transitions (overlaps or too fast)
      problem_fast = next_dist >= max_tower_dist & (time_diff < min_time | overlap),
      
      # TODO: Results in 300+ ids to check... leave for now
      problem_short_bout = FALSE, # TODO: if using, replace with id1 %in% b | id2 %in% b, (add bouts in as arg)
      
      # Direction
      lat_diff = lat2 - lat1, 
      problem_direction = (migration == "south" & lat_diff > 1) |
        (migration == "north" & lat_diff < -1),
      problem_direction = replace_na(problem_direction, FALSE),
      
      # Overall
      problem = problem_fast | problem_short_bout | problem_manual | problem_direction) |>
    select(-matches("\\.(x|y)$"))
}

# ---- resolve_stns ----
resolve_stns <- function(bouts, trans, problems_manual = NULL) {

  b <- summarize(
    bouts,
    len_max = max(len_max),
    #n_bouts = n(),
    #n_runs = sum(n_runs),
    total_time = sum(total_time),
    .by = c("tagDeployID", "stn_group"))
  
  t <- filter(trans, problem) |>
    select(tagDeployID, trans_id, stn1, stn2, id1, id2, b1, e1, b2, e2, lat_diff, contains("problem")) |>
    pivot_longer(cols = c("stn1", "stn2", "id1", "id2", "b1", "e1", "b2", "e2"), 
                 names_to = c(".value", "pair"), 
                 names_pattern = "([a-z]{1,3})(\\d+)") |>
    rename("stn_group" = "stn") |>
    left_join(b, by = c("tagDeployID", "stn_group")) |>
    arrange(tagDeployID, trans_id, total_time) |>
    select(tagDeployID, trans_id, contains("problem"), stn_group, id, 
           b, e,
           len_max, total_time)
  
  # Mark the station to omit within a group that was able to have a (resolved) station
  
  # Mark manually identified problems
  if(!is.null(problems_manual)) {
    t <- left_join(t, select(problems_manual, "tagDeployID", "stn_group", "start", "end", "omit" = "problem_manual"), 
                   by = join_by("tagDeployID", "stn_group", within("b", "e", "start", "end"))) |>
      select(-"b", -"e", -"start", -"end")
  } else t$omit <- NA
  
  t |>
    mutate(
      # Mark directional and short bout problems as FALSE (need to be manually checked)
      omit = if_else(is.na(omit) & (problem_direction | problem_short_bout), FALSE, NA),
      # Choose station to omit otherwise
      omit = if_else(is.na(omit), 
                     total_time <= (suppressWarnings(max(total_time)) / 10) | 
                       len_max <= (suppressWarnings(max(len_max)) / 10), 
                     omit), 
      .by = c("tagDeployID", "trans_id")) |>
    mutate(resolved = any(omit), .by = c("tagDeployID", "trans_id"))
}
