source("main/load_packages.R")

options(future.rng.onMisuse = "ignore")
RNGkind("L'Ecuyer-CMRG")

data_files <- list(
  station_info="data/StsInfoChose.rds",
  river_declustered="data/river_clean_declustered.rds",
  river_full= "data/river_clean_full.rds"
  
)

run_dir <- prepare_run(run_dir = "output/river-exp", 
                       config_file = "configs/config-sec6-river_experiment.R", 
                       data_files = data_files,
                       set_params_fn = make_params_exp_river,
                       add_timestamp = FALSE)
