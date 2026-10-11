# Local launcher for the river benchmark: runs the tasks of a prepared run
# directory with execute_task(), in parallel with future and a progress bar.
# Run from the project root. For Slurm, use sec6-river_benchmark-1-launch_run_slurm.sh.

source("main/load_packages.R")

options(future.rng.onMisuse = "ignore")
RNGkind("L'Ecuyer-CMRG")

run_dir <- "output/river-bench/20261011_164320"   # from the prepare step
n_workers <- 4L
methods <- c("Extremal Pruning", "CausEV", "QTree")   # subset to run
skip_done <- FALSE # set to TRUE to resume after interruption

params <- readr::read_rds(file.path(run_dir, "params.rds"))
task_ids <- params$task_id[params$method %in% methods]
if (skip_done) {
  done <- file.exists(file.path(run_dir, sprintf("res_%04d.rds", task_ids)))
  task_ids <- task_ids[!done]
}
message("Running ", length(task_ids), " tasks with ", n_workers, " workers.")

future::plan(future::multisession, workers = n_workers)
progressr::handlers(global = TRUE)

progressr::with_progress({
  p <- progressr::progressor(along = task_ids)
  furrr::future_walk(task_ids, function(task_id) {
    # Workers start from a fresh R session: load packages and R/ once each.
    if (!exists("run_exp_river_bench", envir = globalenv())) {
      suppressMessages(source("main/load_packages.R", local = globalenv()))
    }
    execute_task(run_dir = run_dir, task_id = task_id,
                 sim_fn = get("run_exp_river_bench", envir = globalenv()))
    p(sprintf("task %d", task_id))
  }, .options = furrr::furrr_options(
    seed = NULL,   # each task restores its own RNG stream from params.rds
    globals = c("run_dir", "p")))
})

future::plan(future::sequential)
