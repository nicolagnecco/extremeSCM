## Experiment: Extremal PC with the extremal CI test vs PC with PCM vs PC with
## HSIC (X-PC, Exp 1). Counterpart of run_exp_river() (EP, Exp 1).
##
## It uses the same config and set_params_fn (make_params_exp_river): with the
## same seed, a task here sees the same subsample, and the same
## size-matched subsamples for PCM/HSIC, as the corresponding task of
## run_exp_river(). The data steps below repeat those of run_exp_river(), 
## with the random draws in the same order.


run_exp_river_pc <- function(task_params, other_data) {
  ## tibble list-of-tibbles -> list(results, graphs, station_info, subsample_idx)
  ##
  ## results: edge tibble as in run_exp_river() (undirected edges appear in
  ##   both directions; an empty graph has no rows)
  ## graphs: one row per estimated graph with its adjacency matrix, so that
  ##   empty graphs are not lost; use it to compute metrics
  
  # task_params unwrap
  dataset_name <- task_params$dataset
  causal_order <- task_params$causal_order[[1]]
  alpha <- task_params$alphas
  log_transform <- task_params$log_transform
  repetition <- task_params$repetitions
  taus <- task_params$taus[[1]]
  taus_other <- task_params$taus_other[[1]]
  B <- task_params$B
  
  pc_settings <- if ("pc_settings" %in% names(task_params)) task_params$pc_settings[[1]] else NULL
  forbid_marginal <- if (is.null(pc_settings$forbid_marginal)) TRUE else pc_settings$forbid_marginal
  m_max <- if (is.null(pc_settings$m_max)) Inf else pc_settings$m_max
  
  # Stations of this branch, in causal order (upstream first)
  station_info <- other_data$station_info %>%
    dplyr::filter(id %in% causal_order) %>%
    dplyr::mutate(id_name = paste0("station_", id)) %>%
    dplyr::rename(Lon = Long) %>%
    dplyr::select(id_name, Lat, Lon)
  
  station_names <- paste0("station_", causal_order)
  d <- length(station_names)
  
  # True DAG: station_names[i] -> station_names[i + 1]
  true_adjmat <- matrix(0, d, d, dimnames = list(station_names, station_names))
  true_adjmat[cbind(seq_len(d - 1L), 2:d)] <- 1
  
  adj_to_tbl <- function(A) {
    igraph::graph_from_adjacency_matrix(A, mode = "directed") %>%
      graph_to_tibble(station_info = station_info)
  }
  
  # Data (same steps and random draws as run_exp_river)
  river <- if (task_params$declustered) {
    other_data$river_declustered
  } else {
    other_data$river_full
  }
  dat <- as.matrix(river[, station_names])
  if (log_transform) dat <- log(dat + 1)
  
  if (task_params$simulated) {
    dat <- sample_SCM(n = nrow(dat), B_full = true_adjmat, B_0 = true_adjmat,
                      B_0_w = true_adjmat, has_nonlinearities = FALSE)
  }
  
  if (task_params$subsample_mode == "bootstrap") {
    idx <- sample(x = nrow(dat), size = nrow(dat), replace = TRUE)
  } else if (task_params$subsample_mode == "subsample") {
    idx <- sample(x = nrow(dat), size = nrow(dat) * task_params$subsample_prop,
                  replace = FALSE)
  } else {
    stop("subsample_mode should be one of 'subsample' or 'bootstrap'.")
  }
  dat <- dat[idx, , drop = FALSE]
  
  graphs <- list()
  
  # Extremal PC: extremal CI test on the exceedances above each tau
  for (tau in taus) {
    fit <- pc_extremal(dat, tau = tau, alpha = alpha, labels = station_names,
                       m_max = m_max)
    graphs[[length(graphs) + 1L]] <- tibble::tibble(
      method = "Extremal PC", tau = tau, k = fit$k, adjacency = list(fit$adjacency))
  }
  
  # PC with PCM / HSIC on a random subsample of the same size k as the
  # exceedances (tau = 0: all data). One RNG substream per tau, derived from
  # the task stream exactly as in run_exp_river (which iterates over the
  # sorted taus via tidyr::crossing).
  if (isTRUE(task_params$allmethods)) {
    taus_s <- sort(unique(taus_other))
    rng <- task_params$rng[[1]]
    rng_list <- vector("list", length(taus_s))
    for (i in seq_along(taus_s)) {
      rng <- parallel::nextRNGSubStream(seed = rng)
      rng_list[[i]] <- rng
    }
    
    reg_method <- purrr::partial(ranger_reg_method, num.trees = B)
    
    for (i in seq_along(taus_s)) {
      tau <- taus_s[i]
      rngtools::setRNG(rng_list[[i]])
      
      if (tau == 0) {
        dat_k <- dat
      } else {
        k <- nrow(data2mpareto(dat, p = tau))
        idx_k <- sample.int(nrow(dat), size = k, replace = FALSE)
        dat_k <- dat[idx_k, , drop = FALSE]
      }
      suff <- list(dat = dat_k, reg_method = reg_method)
      
      A_pcm <- fit_pc(suff, ci_wrap(ci_test_pcm, forbid_marginal), alpha,
                      station_names, m_max)
      A_hsic <- fit_pc(suff, ci_wrap(ci_test_hsic, forbid_marginal), alpha,
                       station_names, m_max)
      
      graphs[[length(graphs) + 1L]] <- tibble::tibble(
        method = c("PCM", "HSIC"), tau = tau, k = nrow(dat_k),
        adjacency = list(A_pcm, A_hsic))
    }
  }
  
  graphs <- dplyr::bind_rows(graphs) %>%
    dplyr::mutate(alpha = alpha, forbid_marginal = forbid_marginal,
                  m_max = m_max, dataset = dataset_name,
                  bootstrap_rep = repetition, task_id = task_params$task_id)
  
  # Edge tibble in the format of run_exp_river (plus the true graph)
  est_tbl <- purrr::pmap_dfr(
    list(graphs$adjacency, graphs$method, graphs$tau, graphs$k),
    function(A, method, tau, k) {
      adj_to_tbl(A) %>% dplyr::mutate(method = method, tau = tau, k = k)
    })
  
  results <- dplyr::bind_rows(
    adj_to_tbl(true_adjmat) %>% dplyr::mutate(method = "true"),
    est_tbl
  ) %>%
    dplyr::mutate(alpha = alpha, log_transform = log_transform,
                  dataset = dataset_name, bootstrap_rep = repetition,
                  task_id = task_params$task_id)
  
  list(
    results = results,
    graphs = graphs,
    station_info = station_info,
    subsample_idx = idx
  )
}