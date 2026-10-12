#!/bin/bash
# Submit the unfinished tasks of a prepared run directory as a Slurm array.
#   bash main/submit_slurm.sh RUN_DIR [THROTTLE] [TIME] [MEM]
set -euo pipefail

RUN_DIR="${1:?Usage: bash main/submit_slurm.sh RUN_DIR [THROTTLE] [TIME] [MEM]}"
RUN_DIR="${RUN_DIR%/}"                      # drop a trailing slash
THROTTLE="${2:-100}"
TIME="${3:-00:30:00}"
MEM="${4:-4G}"

SIM_FN=$(cat "$RUN_DIR/sim_fn.txt")
N_TASKS=$(cat "$RUN_DIR/n_tasks.txt")

# Task ids without a result file, as a comma-separated list
TODO=""
for i in $(seq 1 "$N_TASKS"); do
  [ -f "$RUN_DIR/$(printf 'res_%04d.rds' "$i")" ] || TODO="$TODO,$i"
done
TODO="${TODO#,}"

if [ -z "$TODO" ]; then
  echo "All $N_TASKS tasks of $RUN_DIR are done."
  exit 0
fi

mkdir -p logs   # sbatch does not create the folder for --output
echo "Submitting $(echo "$TODO" | tr ',' '\n' | wc -l) of $N_TASKS tasks of $SIM_FN"
sbatch --partition=slim --time="$TIME" --mem="$MEM" \
  --array="${TODO}%${THROTTLE}" \
  --output='logs/slurm-%A_%a.out' \
  main/launch_slurm.sh "$RUN_DIR" 0 "$SIM_FN"
