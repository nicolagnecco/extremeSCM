## Configuration for the river benchmark: Extremal Pruning vs QTree vs CausEV.
##
## All methods see the same subsample of the Danube data.

set_config <- function(){
  list(
    declustered = TRUE,          # 428 declustered events (Asadi et al., 2015)
    log_transform = TRUE,        # log(x + 1); used by EP and QTree
    n_reps = 50L,
    subsample_prop = 0.75,       # ~321 of 428 events per repetition
    seed = 4217L,

    methods = c("Extremal Pruning", "QTree", "CausEV"),

    # Extremal Pruning (Algorithm 2 with Markov-blanket separating sets)
    ep = list(
      params_iter = list(
      taus = c(0.8, 0.85, 0.9, 0.95),
      alpha = c(0.001, 0.0025, 0.005, 0.01))
    ),

    # QTree (Tran, Buck & Klueppelberg, 2024), auto-tuned as in their Section 4
    qtree = list(
      q_grid = seq(0.7, 0.9, by = 0.025),
      small_r = 0.05,
      frac = 0.75,
      nrep = 1000L,
      use_order = TRUE
    ),

    # CausEV (Mhalla, Chavez-Demoulin & Dupuis, 2020), settings of their
    # Danube script JRSSC_Danube_causality.R
    causev = list(
      functions_file = file.path(
        "third_party", "Causal-mechanism-of-extreme-river-discharges",
        "Functions", "JRSSC_functions.R"),
      thd = 0.9,
      n_simplex = 800L,
      n_tau = 1L,
      log_transform = FALSE,      # CausEV was run on raw discharges
      use_order = TRUE
    ),

    # Branches of the upper Danube; each is the chain order[1] -> order[2] -> ...
    datasets = list(
      "dataset-1" = 12:1,
      "dataset-2" = c(22:20, 7:1),
      "dataset-3" = c(24:23, 4:1),
      "dataset-4" = c(27:25, 4:1),
      "dataset-5" = c(19:14, 2:1),
      "dataset-6" = c(29:28, 31:30, 13, 1)
    )
  )
}
