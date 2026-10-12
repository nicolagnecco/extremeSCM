# Prepare a run directory for an experiment registered in R/experiments.R.
# Run from the project root:
#   Rscript main/prepare_run.R EXPERIMENT [CONFIG_FILE]
# CONFIG_FILE overrides the registered config (e.g., a variant of it).
# Interactively, set the defaults below and source the file.

source("main/load_packages.R")

options(future.rng.onMisuse = "ignore")
RNGkind("L'Ecuyer-CMRG")

args <- commandArgs(trailingOnly = TRUE)
experiment <- if (length(args) >= 1) args[[1]] else "river-bench"

exp <- experiments[[experiment]]
if (is.null(exp)) {
  stop("Unknown experiment '", experiment, "'. Registered: ",
       paste(names(experiments), collapse = ", "), call. = FALSE)
}
config_file <- if (length(args) >= 2) args[[2]] else exp$config

run_dir <- prepare_run(run_dir = exp$out_dir,
                       config_file = config_file,
                       data_files = exp$data_files,
                       set_params_fn = get(exp$set_params_fn, mode = "function"),
                       sim_fn_name = exp$sim_fn,
                       add_timestamp = TRUE)

params <- readr::read_rds(file.path(run_dir, "params.rds"))
message("Run directory: ", run_dir)
message("Number of tasks: ", nrow(params))
if ("method" %in% names(params)) print(dplyr::count(params, method))
