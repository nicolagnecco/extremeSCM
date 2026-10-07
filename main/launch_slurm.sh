#!/bin/bash
# Submit from the project root:
#   sbatch main/launch_slurm.sh output/river-exp_TIMESTAMP 2
# Later, use the same file for an array:
#   sbatch --partition=slim --array=1-600%5 main/launch_slurm.sh output/river-exp_TIMESTAMP
# Each job calls the existing execute_task(); preparation happens beforehand.
#
# An example of an array job:
# sbatch --partition=slim --array=1-900%5 --output='logs/slurm-%A_%a.out' main/launch_slurm.sh output/river-exp/20261007_165615 0 run_exp_river

#SBATCH --job-name=river
#SBATCH --partition=debug-slim
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --time=00:20:00
#SBATCH --output=slurm-%j.out

set -euo pipefail



run_dir="${1:?Usage: sbatch main/launch_slurm.sh RUN_DIR TASK_ID SIM_FN}"
task_id="${SLURM_ARRAY_TASK_ID:-${2:?Pass TASK_ID or submit with --array}}"
sim_fn="$3"

# Relative paths refer to the project root where sbatch was invoked.
cd "$SLURM_SUBMIT_DIR"

# One task uses one CPU; parallelism comes from separate array jobs.
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

# Slurm runs this command on the compute node, using the existing environment.
# The quoted heredoc passes the R code literally, including its $ operators.
exec "$HOME/.local/bin/micromamba" run -p "$HOME/envs/extrescm-v2" \
  Rscript --vanilla - "$run_dir" "$task_id" "$sim_fn" <<'RSCRIPT'

source("main/load_packages.R")

args <- commandArgs(trailingOnly = TRUE)

run_dir <- args[[1]]
task_id <- as.integer(args[[2]])
sim_fn_name <- args[[3]]

sim_fn <- get(sim_fn_name, mode = "function")

future::plan(future::sequential)

cat("Task:", task_id, "Host:", Sys.info()[["nodename"]], "\n")
cat("Run directory:", run_dir, "\n")
cat("R version:", R.version.string, "\n")

starttime <- Sys.time()

execute_task(
  run_dir = run_dir,
  task_id = task_id,
  sim_fn = sim_fn
)

cat("Elapsed:\n")
print(Sys.time() - starttime)
cat("Saved:", file.path(run_dir, sprintf("res_%04d.rds", task_id)), "\n")
RSCRIPT
