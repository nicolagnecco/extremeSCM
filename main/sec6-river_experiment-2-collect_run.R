# Collect the task results of a river-experiment run (Extremal Pruning with the
# extremal CI test vs PCM vs HSIC) into one file.
# Run from the project root after all tasks have finished.

source("main/load_packages.R")

run_dir <- "output/river-exp/20261007_165615/"

params <- read_rds(file.path(run_dir, "params.rds"))
res_files <- file.path(run_dir, sprintf("res_%04d.rds", params$task_id))
done <- file.exists(res_files)
if (!all(done)) {
  warning(sum(!done), " of ", length(done), " tasks have no result yet: ",
          paste(head(params$task_id[!done], 20), collapse = ", "))
}

loaded <- lapply(res_files[done], read_rds)

# run_exp_river() does not record the task id: take it from the file name, so
# every graph can be matched to its task (and its true graph) later.
results <- purrr::map2_dfr(
  loaded, params$task_id[done],
  function(res, id) mutate(res$results, task_id = id)
)

station_info <- bind_rows(lapply(loaded, `[[`, "station_info")) %>% distinct()

# Quick overview: graphs per method and tau (should be equal across tasks)
results %>%
  distinct(task_id, method, tau) %>%
  count(method, tau)

out_file <- file.path(run_dir, "results.rds")
write_rds(list(results = results, station_info = station_info,
               params = params, missing = params$task_id[!done]), out_file)
message("Saved: ", out_file)
