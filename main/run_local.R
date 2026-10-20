# Run the tasks of a prepared run directory locally, in parallel, with a
# progress bar. The simulation function is read from RUN_DIR/meta.rds.
# Run from the project root:
#   Rscript main/run_local.R RUN_DIR [N_WORKERS] [SKIP_DONE]
# Interactively, set the defaults below and source the file.

source("main/load_packages.R")

options(future.rng.onMisuse = "ignore")
RNGkind("L'Ecuyer-CMRG")

args <- commandArgs(trailingOnly = TRUE)
run_dir <- if (length(args) >= 1) args[[1]] else "output/river-bench/YYYYMMDD_HHMMSS"
n_workers <- if (length(args) >= 2) as.integer(args[[2]]) else 4L
skip_done <- if (length(args) >= 3) as.logical(args[[3]]) else TRUE
task_filter <- NULL    # e.g. quote(method == "QTree") to run a subset

meta <- readr::read_rds(file.path(run_dir, "meta.rds"))
sim_fn_name <- meta$sim_fn

params <- readr::read_rds(file.path(run_dir, "params.rds"))
if (!is.null(task_filter)) params <- dplyr::filter(params, !!task_filter)
task_ids <- params$task_id
if (skip_done) {
  done <- file.exists(file.path(run_dir, sprintf("res_%04d.rds", task_ids)))
  task_ids <- task_ids[!done]
}
message("Running ", length(task_ids), " tasks of ", sim_fn_name,
        " with ", n_workers, " workers.")

future::plan(future::multisession, workers = n_workers)
progressr::handlers(global = TRUE)

progressr::with_progress({
  p <- progressr::progressor(along = task_ids)
  furrr::future_walk(task_ids, function(task_id) {
    # Workers start from a fresh R session: load packages and R/ once each.
    if (!exists(sim_fn_name, envir = globalenv())) {
      suppressMessages(source("main/load_packages.R", local = globalenv()))
    }
    execute_task(run_dir = run_dir, task_id = task_id,
                 sim_fn = get(sim_fn_name, envir = globalenv()))
    p(sprintf("task %d", task_id))
  }, .options = furrr::furrr_options(
    seed = NULL,   # each task restores its own RNG stream from params.rds
    globals = c("run_dir", "sim_fn_name", "p")))
})

future::plan(future::sequential)
