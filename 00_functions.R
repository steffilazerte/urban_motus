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


filter_count <- function(dbs) {
  before <- map(dbs, \(x) tbl(x, "runs") |> count(name = "n_before") |> collect()) |>
    list_rbind(names_to = "proj_id")
  after <- map(dbs, \(x) {
    remove <- getRunsFilters(x, "all")
    tbl(x, "runs") |>
      anti_join(remove, by = c("runID", "motusTagID")) |>
      count(name = "n_after") |>
      collect()
  }) |>
    list_rbind()
  
  bind_cols(before, after) |>
    mutate(p_reduced = (n_before - n_after) / n_before * 100)
}
  