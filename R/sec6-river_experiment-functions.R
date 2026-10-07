make_params_exp_river <- function(config) {
  ## named-list -> tibble
  
  required <- c(
    "simulated", "subsample_mode", "n_reps", "subsample_prop",
    "declustered", "seed", "taus", "taus_other_methods",
    "alphas", "log_transform", "B", "allmethods", "datasets"
  )
  
  missing <- setdiff(required, names(config))
  
  if (length(missing) > 0L) {
    stop(
      "Missing config fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  datasets <- tibble::tibble(
    dataset = names(config$datasets),
    causal_order = lapply(
      unname(config$datasets),
      as.integer
    )
  )
  
  params <- datasets %>%
    tidyr::crossing(
      alphas = config$alphas,
      log_transform = config$log_transform,
      repetitions = seq_len(config$n_reps)
    ) %>%
    dplyr::group_by(dataset, repetitions) %>%
    set_rng(config$seed) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      task_id = dplyr::row_number(),
      taus = list(config$taus),
      taus_other = list(config$taus_other_methods),
      B = config$B,
      allmethods = config$allmethods,
      simulated = config$simulated,
      subsample_mode = config$subsample_mode,
      subsample_prop = config$subsample_prop,
      declustered = config$declustered
    ) %>%
    dplyr::relocate(task_id)
  
  params
}

run_exp_river <- function(task_params, other_data){
  ## tibble list-of-named-tibbles -> list-of-named-tibbles
  
  # data_files unwrap
  station_info <- other_data$station_info
  river_declustered <- other_data$river_declustered
  river_full <- other_data$river_full
  
  # task_params unwrap
  dataset_name <- task_params$dataset
  causal_order <- task_params$causal_order[[1]]
  
  alpha <- task_params$alphas
  log_transform <- task_params$log_transform
  
  repetition <- task_params$repetitions
  
  declustered <- task_params$declustered
  simulated <- task_params$simulated
  subsample_mode <- task_params$subsample_mode
  subsample_prop <- task_params$subsample_prop
  taus <- task_params$taus[[1]]
  taus_other <- task_params$taus_other[[1]]
  allmethods <- task_params$allmethods
  B <- task_params$B
  
  
  # Load station info
  station_info <- station_info %>%
    filter(id %in% causal_order) %>%
    mutate(id_name = paste0("station_", id)) %>%
    rename(Lon = Long) %>%
    dplyr::select(id_name, Lat, Lon)
  
  station_names <- paste0("station_", causal_order)
  
  
  # Define full DAG
  full_adjmat <- upper.tri(
    matrix(
      ncol = length(station_names),
      nrow = length(station_names)
    ),
    diag = FALSE
  ) * 1
  colnames(full_adjmat) <- rownames(full_adjmat) <- station_names
  full_graph <- graph_from_adjacency_matrix(full_adjmat, mode = "directed")
  full_tbl <- graph_to_tibble(full_graph, station_info) %>%
    mutate(method = "full")
  
  # Define true DAG
  ind_true_dag <- which(full_adjmat > 0, arr.ind = TRUE) %>%
    as_tibble() %>%
    filter(col - row == 1) %>%
    as.matrix()
  
  true_adjmat <- matrix(0,
                        nrow = length(station_names), ncol = length(station_names)
  )
  true_adjmat[ind_true_dag] <- 1
  colnames(true_adjmat) <- rownames(true_adjmat) <- station_names
  true_graph <- graph_from_adjacency_matrix(true_adjmat, mode = "directed")
  true_tbl <- graph_to_tibble(true_graph, station_info) %>%
    mutate(method = "true")
  
  
  # Load river data
  if (declustered){
    river <- river_declustered
  } else {
    river <- river_full
  }
  
  X_dat <- river[, station_names]
  dat <- as.matrix(X_dat)
  
  if (log_transform){
    dat <- log(dat + 1)
  }
  
  if (simulated){
    n_sim <- nrow(dat)
    dat <- sample_SCM(n = n_sim, 
                      B_full = true_adjmat, 
                      B_0 = true_adjmat, 
                      B_0_w = true_adjmat, 
                      has_nonlinearities = FALSE)
  }
  
  
  # Subsample data
  if (subsample_mode == "bootstrap"){
    idx_bootstrap <- sample(x = nrow(dat), size = nrow(dat),
                            replace = TRUE)
  } else if (subsample_mode == "subsample"){
    idx_bootstrap <- sample(x = nrow(dat), 
                            size = nrow(dat) * subsample_prop,
                            replace = FALSE)
  } else {
    stop("subsample_mode should be one of 'subsample' or 'bootstrap'.")
  }
  
  dat <- dat[idx_bootstrap, ]
  
  
  # Extremal pruning
  full_adjmat <- full_adjmat
  prunbl_prs <- prunable_pairs(full_adjmat) %>%
    arrange(desc(y)) %>%
    transpose()
  
  # set up parameters
  n <- nrow(dat)
  
  params_ <- tidyr::crossing(n, taus)
  
  # Extremal prunining
  xpr_tbl <- purrr::map_dfr(seq_len(nrow(params_)), function(i) {
    tau <- params_$taus[i]
    
    # Prune DAG
    Y <- data2mpareto(dat, p = tau)
    k <- nrow(Y)
    vario <- emp_vario(Y)
    
    ci_test <- purrr::partial(HR_CItest, 
                              suffStat = list(Gamma = vario, n = k))
    
    B_pruned <- prune_edge_algo(
      B_full = full_adjmat, prunbl_prs = prunbl_prs,
      ci_test = ci_test, alpha = alpha,
      base_pruning_algo = purrr::partial(prune_edge_fast, verbose=TRUE),
      separating_set_selector = mb_selector, shuffle = TRUE
    )
    
    tbl_pruned <- igraph::graph_from_adjacency_matrix(B_pruned) %>%
      graph_to_tibble(station_info = station_info)
    
    # return tibble
    tbl_pruned %>%
      mutate(method = "Extremal Pruning", k = k, tau = tau)
  })
  
  results_tbl <- bind_rows(full_tbl, true_tbl, xpr_tbl)
  
  if (allmethods){
    taus <- taus_other
    params_ <- tidyr::crossing(n, taus)
    
    # fix RNG for the sample selection based on tau
    rng_ <- list()
    rng <- task_params$rng[[1]]
    
    for(i in seq_len(nrow(params_))){
      rng <- parallel::nextRNGSubStream(seed = rng)
      rng_[[i]] <- rng
    }
    
    params_$rng <- rng_
   
    
    allmethods_tbl <- purrr::map_dfr(seq_len(nrow(params_)), function(i){
      
      tau <- params_$taus[i]
      rngtools::setRNG(params_$rng[[i]])
      
      if (tau == 0){
        dat_k <- dat
      } else {
        k <- nrow(data2mpareto(dat, p = tau))
        idx_k <- sample.int(nrow(dat), size = k, replace = FALSE)
        dat_k <- dat[idx_k, , drop = FALSE]
      }
      
      k <- nrow(dat_k)
      
      
      # PCM
      reg_method <- purrr::partial(ranger_reg_method, 
                                   num.trees = B, num.threads = 1)
      
      ci_test_pcm_ <- purrr::partial(ci_test_pcm, dat = dat_k, 
                                     reg_method = reg_method)
      
      B_pruned_pcm <- prune_edge_algo(
        B_full = full_adjmat, prunbl_prs = prunbl_prs,
        ci_test = ci_test_pcm_, alpha = alpha,
        base_pruning_algo = purrr::partial(prune_edge_fast, verbose=TRUE),
        separating_set_selector = mb_selector, shuffle = TRUE
      )
      
      pcm_tbl <- igraph::graph_from_adjacency_matrix(B_pruned_pcm) %>%
        graph_to_tibble(station_info = station_info) %>% 
        mutate(method = "PCM", k = k, tau = tau)
      
      # HSIC
      ci_test_hsic_ <- purrr::partial(ci_test_hsic, dat = dat_k, 
                                      reg_method = reg_method)
      
      B_pruned_hsic <- prune_edge_algo(
        B_full = full_adjmat, prunbl_prs = prunbl_prs,
        ci_test = ci_test_hsic_, alpha = alpha,
        base_pruning_algo = purrr::partial(prune_edge_fast, verbose=TRUE),
        separating_set_selector = mb_selector, shuffle = TRUE
      )
      
      hsic_tbl <- igraph::graph_from_adjacency_matrix(B_pruned_hsic) %>%
        graph_to_tibble(station_info = station_info) %>% 
        mutate(method = "HSIC", k = k, tau = tau)
      
      
      bind_rows(pcm_tbl, hsic_tbl)
      
    })
    
    results_tbl <- results_tbl %>% 
      bind_rows(allmethods_tbl)
    
  }
  
  
  # Return both results and station info
  list(
    results = results_tbl %>%
      mutate(alpha = alpha, 
             log_transform = log_transform,
             dataset = dataset_name, 
             bootstrap_rep = repetition),
    station_info = station_info
  )
}
