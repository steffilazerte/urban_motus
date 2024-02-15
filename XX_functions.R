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
