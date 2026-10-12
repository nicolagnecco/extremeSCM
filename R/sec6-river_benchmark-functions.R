## Experiment: river benchmark (Extremal Pruning vs QTree vs CausEV).
## Plugs into prepare_run() / execute_task() like the river experiment.

make_params_exp_river_bench <- function(config) {
  ## named-list -> tibble
  ## One row per (dataset, repetition, method). The RNG stream is shared by all
  ## methods of the same (dataset, repetition), so that the subsample, which is
  ## drawn first in run_exp_river_bench(), is identical across methods.

  methods_allowed <- c("Extremal Pruning", "Extremal PC", "QTree", "CausEV")
  
  required <- c(
    "declustered", "log_transform", "n_reps", "subsample_prop", "seed",
    "methods", "datasets"
  )
  
  missing <- setdiff(required, names(config))
  if (length(missing) > 0L) {
    stop("Missing config fields: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  
  settings <- config$methods

  if (!is.list(settings) || is.null(names(settings)) || any(names(settings) == "")) {
    stop("config$methods must be a named list: method name -> settings.", call. = FALSE)
  }
  
  methods <- names(settings)
  
  unknown <- setdiff(methods, methods_allowed)
  if (length(unknown) > 0L) {
    stop("Unknown methods: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  
  datasets <- tibble::tibble(
    dataset = names(config$datasets),
    causal_order = lapply(unname(config$datasets), as.integer)
  )
  
  datasets %>%
    tidyr::crossing(
      repetitions = seq_len(config$n_reps),
      method = methods
    ) %>%
    dplyr::group_by(dataset, repetitions) %>%
    set_rng(config$seed) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dataset, repetitions, method) %>%
    dplyr::mutate(
      task_id = dplyr::row_number(),
      settings = unname(settings[method]),
      declustered = config$declustered,
      log_transform = config$log_transform,
      subsample_prop = config$subsample_prop
    ) %>%
    dplyr::relocate(task_id)
}


run_exp_river_bench <- function(task_params, other_data) {
  ## tibble list-of-tibbles -> list
  ## Returns list(results, station_info, diagnostics, subsample_idx).
  
  dataset_name <- task_params$dataset
  causal_order <- task_params$causal_order[[1]]
  repetition <- task_params$repetitions
  method <- task_params$method
  settings <- task_params$settings[[1]]
  
  # Stations of this branch, in causal order (upstream first)
  station_names <- paste0("station_", causal_order)
  station_info <- other_data$station_info %>%
    dplyr::filter(id %in% causal_order) %>%
    dplyr::mutate(id_name = paste0("station_", id)) %>%
    dplyr::rename(Lon = Long) %>%
    dplyr::select(id_name, Lat, Lon)
  
  d <- length(station_names)
  order <- seq_len(d)  # columns of the data below follow the causal order
  
  # True DAG
  true_adjmat <- matrix(0, d, d)
  true_adjmat[cbind(seq_len(d - 1L), 2:d)] <- 1
  dimnames(true_adjmat) <- list(station_names, station_names)
  
  # Data
  river <- if (task_params$declustered) {
    other_data$river_declustered
  } else {
    other_data$river_full
  }
  X_raw <- as.matrix(river[, station_names])
  
  # Subsample: the first random draw of the task, shared across methods
  idx <- sample(x = nrow(X_raw), size = floor(nrow(X_raw) * task_params$subsample_prop),
                replace = FALSE)
  X_raw <- X_raw[idx, , drop = FALSE]
  X <- if (task_params$log_transform) log(X_raw + 1) else X_raw
  
  adj_to_tbl <- function(A) {
    igraph::graph_from_adjacency_matrix(A, mode = "directed") %>%
      graph_to_tibble(station_info = station_info)
  }
  
  diagnostics <- list()
  
  if (method == "Extremal Pruning") {
    ## True DAG on the order
    full_adjmat <- upper.tri(matrix(0, d, d)) * 1
    
    
    dimnames(full_adjmat) <- list(station_names, station_names)
    
    
    prunbl_prs <- prunable_pairs(full_adjmat) %>%
      dplyr::arrange(dplyr::desc(y)) %>%
      purrr::transpose()
    
    
    settings_ <- tidyr::crossing(!!!settings$params_iter)
    
    est_tbl <- purrr::map_dfr(seq_len(nrow(settings_)), function(i) {
      tau <- settings_$taus[i]
      alpha <- settings_$alpha[i]
      
      Y <- data2mpareto(X, p = tau)
      k <- nrow(Y)
      ci_test <- purrr::partial(
        HR_CItest, suffStat = list(Gamma = graphicalExtremes::emp_vario(Y), n = k))
      
      B_pruned <- prune_edge_algo(
        B_full = full_adjmat, prunbl_prs = prunbl_prs,
        ci_test = ci_test, alpha = alpha,
        base_pruning_algo = prune_edge_fast,
        separating_set_selector = mb_selector, shuffle = TRUE
      )
      dimnames(B_pruned) <- dimnames(full_adjmat)
      
      adj_to_tbl(B_pruned) %>%
        dplyr::mutate(tau = tau, k = k, alpha = alpha)
    })
    
  } else if (method == "Extremal PC") {
    settings_ <- tidyr::crossing(!!!settings$params_iter)

    est_tbl <- purrr::map_dfr(seq_len(nrow(settings_)), function(i) {
      tau <- settings_$taus[i]
      alpha <- settings_$alpha[i]

      Y <- graphicalExtremes::data2mpareto(X, p = tau)
      k <- nrow(Y)
      
      vario <- graphicalExtremes::emp_vario(Y)

      pc.fit <- pcalg::pc(
        suffStat = list(Gamma = vario, n = k),
        indepTest = HR_CItest,
        alpha = alpha,
        verbose = FALSE,
        labels = station_names
      )
      adjm <- (pcalg::wgtMatrix(pcalg::getGraph(pc.fit),
                                transpose = FALSE) != 0) * 1
      dimnames(adjm) <- list(station_names, station_names)
      adj_to_tbl(adjm) %>%
        dplyr::mutate(tau = tau, k = k, alpha = alpha)
    })      
    
  } else if (method == "QTree") {
    
    order_ <- if (settings$use_order) order else NULL
    
    fit <- fit_qtree(X, order_,
                     q_grid = settings$q_grid, small_r = settings$small_r,
                     frac = settings$frac, nrep = settings$nrep)
    est_tbl <- adj_to_tbl(fit$adjacency) %>%
      dplyr::mutate(qtree_q = fit$q, qtree_var_T = fit$var_T)
    diagnostics$tuning <- fit$tuning
    
  } else if (method == "CausEV") {
    causev_env <- load_causev(settings$functions_file)
    X_causev <- if (isTRUE(settings$log_transform)) log(X_raw + 1) else X_raw
    
    order_ <- if (settings$use_order) order else NULL
    
    fit <- fit_causev(X_causev, order_, causev_env,
                      thd = settings$thd, n_simplex = settings$n_simplex,
                      n_tau = settings$n_tau)
    est_tbl <- adj_to_tbl(fit$adjacency) %>%
      dplyr::mutate(causev_n_failed = fit$n_failed)
    diagnostics[c("scores", "n_joint", "n_gpd_warnings", "n_cobs_warnings", "errors")] <-
      fit[c("scores", "n_joint", "n_gpd_warnings", "n_cobs_warnings", "errors")]
    
  } else {
    stop("Unknown method: ", method, call. = FALSE)
  }
  
  results <- dplyr::bind_rows(
    adj_to_tbl(true_adjmat) %>% dplyr::mutate(method = "true"),
    est_tbl %>% dplyr::mutate(method = method)
  ) %>%
    dplyr::mutate(dataset = dataset_name, bootstrap_rep = repetition,
                  n = nrow(X), task_id = task_params$task_id)
  
  list(
    results = results,
    station_info = station_info,
    diagnostics = diagnostics,
    subsample_idx = idx
  )
}
