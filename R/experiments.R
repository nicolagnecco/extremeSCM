## Registry of river experiments: what the generic scripts in main/ need to
## prepare, run and collect each experiment.

river_data_files <- list(
  station_info = "data/StsInfoChose.rds",
  river_declustered = "data/river_clean_declustered.rds",
  river_full = "data/river_clean_full.rds"
)

experiments <- list(

  # Extremal Pruning with the extremal CI test vs PCM vs HSIC (EP, Exp 1)
  "river-exp" = list(
    config = "configs/config-sec6-river_experiment.R",
    set_params_fn = "make_params_exp_river",
    sim_fn = "run_exp_river",
    out_dir = "output/river-exp",
    data_files = river_data_files
  ),
  
  # Extremal PC with the extremal CI test vs PCM vs HSIC (X-PC, Exp 1).
  # Same config and set_params_fn as "river-exp": same subsamples per task.
  "river-pc" = list(
    config = "configs/config-sec6-river_experiment.R",
    set_params_fn = "make_params_exp_river",
    sim_fn = "run_exp_river_pc",
    out_dir = "output/river-pc",
    data_files = river_data_files
  ),

  # Extremal Pruning vs QTree vs CausEV, order known (EP, Exp 2)
  "river-bench" = list(
    config = "configs/config-sec6-river_benchmark.R",
    set_params_fn = "make_params_exp_river_bench",
    sim_fn = "run_exp_river_bench",
    out_dir = "output/river-bench",
    data_files = river_data_files
  ),

  # Extremal PC vs QTree vs CausEV, order unknown (XPC, Exp 2)
  "river-bench-noorder" = list(
    config = "configs/config-sec6-river_benchmark-noorder.R",
    set_params_fn = "make_params_exp_river_bench",
    sim_fn = "run_exp_river_bench",
    out_dir = "output/river-bench-noorder",
    data_files = river_data_files
  )
)
