# Extremes of Structural Causal Models

This repository contains code, data, and scripts for experiments of the manuscript **Extremes of structural causal models** (https://arxiv.org/abs/2503.06536).  
It is organized as an R project, with separate folders for core functions, datasets, analysis scripts, and generated outputs.

## Structure

```
extremeSCM/
├── R/          # Core R functions for simulation, causal discovery, evaluation, and plotting
├── main/       # Top-level experiment scripts (see below)
├── data/       # Input datasets (.rds / .RData)
└── output/     # Generated results and figures from experiments
```

## `main/` Folder

The `main/` folder contains **standalone scripts** that can be run to reproduce experiments and figures from the associated study.  
File names generally follow the pattern `sec<SectionNumber>-<Description>.R` to match sections of the manuscript.

To run the analysis:
1. Run `setup.R` to install dependencies and set global parameters.
2. Run the intended script from the `main/` folder.

```bash
main/
├── load_packages.R                     # Loads all required R packages for the project
├── setup.R                              # Sets up global parameters, seeds, and paths
├── sec1-fig1-scatter.R                  # Generates scatter plot for Section 1, Figure 1
├── sec1-fig3-scatterhist.R              # Generates scatter–histogram plot for Section 1, Figure 3
├── sec5-pc_alg.R                        # Runs the extremal-PC algorithm on specified datasets
├── sec5-extremal-ci-test.R              # Performs extremal conditional independence tests
├── sec5-pruning_random_dags_extended.R  # Runs extremal pruning experiments on random DAGs
├── sec5-pruning-pc-plots.R              # Produces plots for extremal pruning experiment
├── sec6-river_clean.R                   # Cleans and processes the river network dataset
├── sec6-river_experiment-main.R         # Main script for river network experiments
├── sec6-river_plots_map.R               # Produces river network map plots
├── sec6-river_plots_shd.R               # Generates Structural Hamming Distance plots for river experiments
```
