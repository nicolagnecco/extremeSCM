# Collect the task results of a run directory into RUN_DIR/results.rds.
# Run from the project root after the tasks have finished:
#   Rscript main/collect_run.R RUN_DIR
# results.rds holds: results (all `results` tibbles, with task_id), extras
# (the other task outputs as list-columns, joined with the task parameters),
# station_info, params and the ids of missing tasks.

source("main/load_packages.R")

args <- commandArgs(trailingOnly = TRUE)
run_dir <- if (length(args) >= 1) args[[1]] else "output/river-bench/YYYYMMDD_HHMMSS"

out <- collect_run(run_dir)

# Task parameters next to the extra outputs (without the RNG streams)
param_cols <- setdiff(names(out$params), c("rng", "settings"))
out$extras <- dplyr::inner_join(out$params[, param_cols], out$extras,
                                by = "task_id")

if ("station_info" %in% names(out$extras)) {
  out$station_info <- dplyr::bind_rows(out$extras$station_info) %>% dplyr::distinct()
  out$extras$station_info <- NULL
}

# Estimated graphs as adjacency matrices (run_exp_river_pc): one tibble
if ("graphs" %in% names(out$extras)) {
  out$graphs <- dplyr::bind_rows(out$extras$graphs)
  out$extras$graphs <- NULL
}


# Checks for runs with several methods per (dataset, repetition)
if (all(c("subsample_idx", "dataset", "repetitions") %in% names(out$extras))) {
  same <- out$extras %>%
    dplyr::group_by(dataset, repetitions) %>%
    dplyr::summarise(ok = dplyr::n_distinct(vapply(subsample_idx, paste, "",
                                                   collapse = ",")) == 1,
                     .groups = "drop")
  if (!all(same$ok)) warning("Some methods saw different subsamples.")
}
if ("causev_n_failed" %in% names(out$results)) {
  message("CausEV failed pairs per fit:")
  print(summary(dplyr::distinct(dplyr::filter(out$results, method == "CausEV"),
                                task_id, causev_n_failed)$causev_n_failed))
}

out_file <- file.path(run_dir, "results.rds")
readr::write_rds(out, out_file)
message("Saved: ", out_file, " (", nrow(out$extras), " tasks, ",
        length(out$missing), " missing)")
