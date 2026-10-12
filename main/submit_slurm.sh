#!/bin/bash
# Submit the unfinished tasks of a prepared run directory as a Slurm array.
# Run from the project root, on the login node:
#   bash main/submit_slurm.sh RUN_DIR [THROTTLE] [TIME] [MEM]
# Tasks that already have a res_XXXX.rds are skipped, so the same command
# resubmits whatever failed or timed out.

set -euo pipefail

RUN_DIR="${1:?Usage: bash main/submit_slurm.sh RUN_DIR [THROTTLE] [TIME] [MEM]}"
THROTTLE="${2:-100}"
TIME="${3:-00:30:00}"
MEM="${4:-4G}"

MM="$HOME/.local/bin/micromamba"
ENV="$HOME/envs/extrescm-v2"
r() { "$MM" run -p "$ENV" Rscript --vanilla -e "$1"; }

SIM_FN=$(r "cat(readRDS('$RUN_DIR/meta.rds')\$sim_fn)")
ARRAY=$(r "source('R/simulation_utils.R'); cat(missing_array_spec('$RUN_DIR'))")

if [ -z "$ARRAY" ]; then
  echo "All tasks of $RUN_DIR are done."
  exit 0
fi

mkdir -p logs   # sbatch does not create the folder for --output
echo "Submitting tasks $ARRAY of $SIM_FN from $RUN_DIR"
sbatch --partition=slim --time="$TIME" --mem="$MEM" \
  --array="${ARRAY}%${THROTTLE}" \
  --output='logs/slurm-%A_%a.out' \
  main/launch_slurm.sh "$RUN_DIR" 0 "$SIM_FN"
