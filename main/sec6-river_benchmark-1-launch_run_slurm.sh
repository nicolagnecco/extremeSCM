#!/bin/bash
# Submit from the project root, after main/sec6-river_benchmark-0-prepare_run.R.
# 6 datasets x 50 repetitions x 3 methods = 900 tasks.
#
# CausEV tasks are much slower than the others (one copula fit per pair of
# stations), hence the longer time limit. If needed, the task ids of a single
# method can be listed in R with
#   params <- readr::read_rds("RUN_DIR/params.rds"); params$task_id[params$method == "CausEV"]
# and passed to --array as a comma-separated list.

RUN_DIR="output/river-bench/20261009_113540"
mkdir -p logs
sbatch --partition=slim --time=04:00:00 --mem=4G --array=1-900%50 \
  --output='logs/slurm-%A_%a.out' \
  main/launch_slurm.sh "$RUN_DIR" 0 run_exp_river_bench
