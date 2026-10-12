#!/bin/bash
# Submit from the project root, after main/sec6-river_benchmark-0-prepare_run.R.
# 6 datasets x 50 repetitions x 3 methods = 900 tasks.
#
# CausEV and QTree tasks are much slower than the EP.


RUN_DIR="output/river-bench/20261011_165553"
mkdir -p logs

sbatch --partition=slim --time=00:30:00 --mem=4G --array=1-900%100 \
  --output='logs/slurm-%A_%a.out' \
  main/launch_slurm.sh "$RUN_DIR" 0 run_exp_river_bench
