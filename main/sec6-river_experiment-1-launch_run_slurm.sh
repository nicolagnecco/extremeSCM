#!/bin/bash
# Submit from the project root

# TODO: remove timestamp from river-exp experiment

sbatch --partition=slim --array=1-900%50 --output='logs/slurm-%A_%a.out' main/launch_slurm.sh output/river-exp/20261007_165615 0 run_exp_river
