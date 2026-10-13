set_config <- function(){
  list(
    simulated = FALSE,
    subsample_mode = "subsample",
    n_reps = 50L,
    subsample_prop = 0.25,
    declustered = FALSE,
    seed = 1321L,
    
    taus = c(0.9, 0.95, 0.96, 0.97, 0.98),
    taus_other_methods = c(0.0, 0.9, 0.95, 0.96, 0.97, 0.98),
    alphas = c(0.005, 0.01, 0.05),
    log_transform = TRUE,
    
    B = 100L,
    allmethods = TRUE,
    
    
    # PC settings, used by run_exp_river_pc() only (X-PC, Exp 1):
    # forbid_marginal: PCM/HSIC never accept S = {} (as the extremal test);
    # m_max: largest conditioning set PC tries (Inf = no limit)
    pc = list(forbid_marginal = TRUE, m_max = Inf),
    
    
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
