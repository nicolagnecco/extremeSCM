# River benchmark: Extremal Pruning vs QTree vs CausEV on the declustered
# Danube branches. Run from the project root.
#
# One-off setup for CausEV (the authors' code is not vendored in this repo):
#   git clone https://github.com/lindamhalla/Causal-mechanism-of-extreme-river-discharges \
#     third_party/Causal-mechanism-of-extreme-river-discharges
# and install the R packages evd, cobs and statmod.

source("main/load_packages.R")

options(future.rng.onMisuse = "ignore")
RNGkind("L'Ecuyer-CMRG")

data_files <- list(
  station_info = "data/StsInfoChose.rds",
  river_declustered = "data/river_clean_declustered.rds",
  river_full = "data/river_clean_full.rds"
)

run_dir <- prepare_run(run_dir = "output/river-bench",
                       config_file = "configs/config-sec6-river_benchmark.R",
                       data_files = data_files,
                       set_params_fn = make_params_exp_river_bench,
                       add_timestamp = TRUE)

params <- readr::read_rds(file.path(run_dir, "params.rds"))
message("Run directory: ", run_dir)
message("Number of tasks: ", nrow(params))
print(dplyr::count(params, method))
