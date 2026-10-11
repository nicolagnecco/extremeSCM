# Collect the task results of a river-benchmark run into one file.
# Run from the project root.

source("main/load_packages.R")

run_dir <- "output/river-bench/20261011_164320"

params <- read_rds(file.path(run_dir, "params.rds"))
res_files <- file.path(run_dir, sprintf("res_%04d.rds", params$task_id))
done <- file.exists(res_files)
if (!all(done)) {
  warning(sum(!done), " of ", length(done), " tasks have no result yet: ",
          paste(head(params$task_id[!done], 20), collapse = ", "))
}

loaded <- lapply(res_files[done], read_rds)

results <- bind_rows(lapply(loaded, `[[`, "results"))

diagnostics <- tibble(
  task_id = params$task_id[done],
  dataset = params$dataset[done],
  bootstrap_rep = params$repetitions[done],
  method = params$method[done],
  diagnostics = lapply(loaded, `[[`, "diagnostics"),
  subsample_idx = lapply(loaded, `[[`, "subsample_idx")
)

# Every method of a (dataset, repetition) must have seen the same subsample
same_subsample <- diagnostics %>%
  group_by(dataset, bootstrap_rep) %>%
  summarise(ok = n_distinct(vapply(subsample_idx, paste, "", collapse = ",")) == 1,
            .groups = "drop")
if (!all(same_subsample$ok)) warning("Some methods saw different subsamples.")

# CausEV pairs where the authors' code failed
if ("causev_n_failed" %in% names(results)) {
  causev_failures <- results %>%
    filter(method == "CausEV") %>%
    distinct(dataset, bootstrap_rep, causev_n_failed)
  message("CausEV failed pairs per fit:")
  print(summary(causev_failures$causev_n_failed))
}

station_info <- bind_rows(lapply(loaded, `[[`, "station_info")) %>% distinct()

out_file <- file.path(run_dir, "results.rds")
write_rds(list(results = results, diagnostics = diagnostics,
               station_info = station_info, params = params), out_file)
message("Saved: ", out_file)
