# Extremes of Structural Causal Models

This repository contains code, data, and scripts to reproduce the experiments of the manuscript **Extremes of structural causal models** (https://arxiv.org/abs/2503.06536).
It is organized as an R project, with separate folders for core functions, configurations, datasets, analysis scripts, and generated outputs.

## Structure

```
extremeSCM/
├── R/          # Core R functions for simulation, causal discovery, evaluation, and plotting
├── configs/    # Configurations of the river experiments (Section 6)
├── main/       # Top-level experiment scripts (see below)
├── data/       # Input datasets (.rds / .RData)
├── tests/      # Tests of the QTree and Chu-Liu/Edmonds implementations
└── output/     # Generated results and figures from experiments
```

## Setup

1. Run `main/setup.R` to install the required R packages.
2. For the CausEV benchmark (Section 6), clone the authors' code into `third_party/`:
   ```bash
   git clone https://github.com/lindamhalla/Causal-mechanism-of-extreme-river-discharges \
     third_party/Causal-mechanism-of-extreme-river-discharges
   ```

All scripts are run from the project root.

## `main/` Folder

File names follow the pattern `sec<SectionNumber>-<Description>.R` to match sections of the manuscript.

```bash
main/
├── load_packages.R                       # Loads all required R packages and the functions in R/
├── setup.R                               # Installs the required R packages
├── sec1-fig1-scatter.R                   # Section 1, Figure 1
├── sec1-fig3-scatterhist.R               # Section S4, Figure S1
├── sec5-extremal-ci-test.R               # Section 5.1 and S5: level and power of the extremal CI test
├── sec5-pc_alg.R                         # Section S7: extremal PC algorithm on random DAGs
├── sec5-pruning_random_dags_extended.R   # Section S6: extremal pruning on random DAGs
├── sec5-pruning-pc-plots.R               # Section S6 and S7: plots for the PC and pruning experiments
├── sec6-river_clean.R                    # Section 6: cleans the Danube data (creates data/river_clean*.rds)
├── prepare_run.R                         # Section 6: prepares a run directory for an experiment
├── run_local.R                           # Section 6: runs the tasks of a run directory locally
├── submit_slurm.sh, launch_slurm.sh      # Section 6: runs the tasks on a Slurm cluster (optional)
├── collect_run.R                         # Section 6: collects the task results into results.rds
├── sec6-river_pc-3-plots.R               # Section 6.1 and S8.1, Experiment 1 with extremal PC (run "river-pc")
├── sec6-river_experiment-3-plots_shd.R   # Section 6.1 and S8.1, Experiment 1 with extremal pruning (run "river-exp")
├── sec6-river_experiment-3-plots_map.R   # Section S8.3: maps of the six river branches (true DAGs)
└── sec6-river_benchmark-3-plots.R        # Section 6.2 and S8.2, Experiment 2 vs QTree and CausEV ("river-bench", "river-bench-noorder")
```

## Sections 1 and 5

The Section 1 and Section 5 scripts are standalone: run them directly, e.g. `Rscript main/sec5-pc_alg.R`.

- `sec1-fig1-scatter.R` and `sec1-fig3-scatterhist.R` write their figures to `output/examples/`; create it first with `mkdir -p output/examples`.
- `sec5-extremal-ci-test.R`, `sec5-pc_alg.R` and `sec5-pruning_random_dags_extended.R` have a flag `RUN_SIMULATIONS` at the top. With `RUN_SIMULATIONS <- FALSE` (the default), they load the saved results in `output/pc-alg/`, `output/random-dags/` and `output/random-dags-cond-ind-tests/` and only redraw the figures. Set it to `TRUE` to rerun the simulations (in parallel; the number of workers is set in each script).
- `sec5-pruning-pc-plots.R` draws the combined figure of the extremal PC and pruning experiments from the same saved results.

## Section 6 workflow

The river experiments are specified in `R/experiments.R` (`river-exp`, `river-pc`, `river-bench`, `river-bench-noorder`) with their configurations in `configs/`. Each experiment is run in three steps:

```bash
Rscript main/prepare_run.R river-pc            # creates output/river-pc/<timestamp>/
Rscript main/run_local.R output/river-pc/<timestamp> 4
Rscript main/collect_run.R output/river-pc/<timestamp>
```

Then set `run_dir` (or `run_dirs`) at the top of the corresponding `sec6-*-3-plots*.R` script to the new run directory and run it.

The metrics of the runs shown in the manuscript are included (`output/<experiment>/<timestamp>/metrics.rds`), and the plotting scripts point to these runs, so they reproduce the Section 6 figures without rerunning the experiments. The map script `sec6-river_experiment-3-plots_map.R` only needs the files already in `data/` and `output/river/`.

```mermaid
flowchart TD
    C["Config and experiment name"]

    subgraph PREP["prepare_run"]
        P1["Create experiment/timestamp directory"]
        P2["Save config and code-version information"]
        P3["Build the parameter table (one row per task)"]
        P4["Save parameter table as params.rds"]
        P5["Return run directory"]

        P1 --> P2 --> P3 --> P4 --> P5
    end

    C --> P1

    P5 --> L["Local launcher: run_local.R"]
    P5 --> S["Slurm launcher: submit_slurm.sh"]

    L --> E
    S --> E

    subgraph TASK["execute_task(run_dir, task_id)"]
        E["Load parameter row and data"]
        R["Restore saved RNG state"]
        F["Call the experiment's simulation function"]
        W["Save task result as RDS"]

        E --> R --> F --> W
    end

    W --> A["collect_run.R"]
    A --> O["Combined results and figures"]
