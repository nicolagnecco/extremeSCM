## Metrics for estimated graphs (DAGs or CPDAGs) against a true DAG, and the
## evaluation of all graphs of a collected river run.
##
## Graphs are 0/1 adjacency matrices with A[i, j] = 1 for i -> j; an
## undirected (CPDAG) edge i - j has A[i, j] = A[j, i] = 1.

edges_to_adj <- function(from, to, nodes) {
  ## character character character -> numeric.matrix
  A <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  if (length(from) > 0) A[cbind(from, to)] <- 1
  A
}


graph_metrics <- function(A_true, A_est) {
  ## numeric.matrix numeric.matrix -> one-row tibble
  ##
  ## skel_*: adjacency errors, ignoring direction (fair for DAGs and CPDAGs)
  ## n_undirected: undirected edges of a CPDAG estimate
  ## n_correct_dir / n_wrong_dir: true edges found with the right / reversed
  ##   direction (undirected estimates count in neither)
  ## shd: SHD against the true DAG; a reversal, or an undirected edge where
  ##   the truth is directed, counts as one mistake
  ## sid_lower / sid_upper: SID (unnormalised); equal for DAG estimates,
  ##   bounds over the Markov equivalence class for CPDAG estimates
  d <- nrow(A_true)
  skel <- function(A) (A + t(A)) > 0
  S_true <- skel(A_true)
  S_est <- skel(A_est)
  directed <- A_est == 1 & t(A_est) == 0
  undirected <- A_est == 1 & t(A_est) == 1
  
  sid <- SID::structIntervDist(unname(A_true), unname(A_est))
  sid_lo <- if (is.null(sid$sidLowerBound)) sid$sid else sid$sidLowerBound
  sid_up <- if (is.null(sid$sidUpperBound)) sid$sid else sid$sidUpperBound

  skel_fp <- sum(S_est & !S_true) / 2
  skel_fn <- sum(!S_est & S_true) / 2

  tibble::tibble(
    d = d,
    n_edges = sum(S_est) / 2,
    skel_fp = skel_fp,
    skel_fn = skel_fn,
    skel_shd = skel_fp + skel_fn,
    n_undirected = sum(undirected) / 2,
    n_correct_dir = sum(directed & A_true == 1),
    n_wrong_dir = sum(directed & t(A_true) == 1),
    shd = SID::hammingDist(unname(A_true), unname(A_est)),
    sid_lower = sid_lo,
    sid_upper = sid_up
  )
}


expected_bench_graphs <- function(params) {
  ## tibble -> tibble(task_id, method, tau, alpha)
  ## every graph a river-bench task should have produced: one per (tau, alpha)
  ## of params_iter for EP / X-PC, one for QTree / CausEV
  purrr::pmap_dfr(
    list(params$task_id, params$method, params$settings),
    function(id, method, s) {
      grid <- if (!is.null(s$params_iter)) {
        tidyr::crossing(!!!s$params_iter) %>% dplyr::rename(tau = taus)
      } else {
        tibble::tibble(tau = NA_real_, alpha = NA_real_)
      }
      dplyr::mutate(grid, task_id = id, method = method)
    })
}


evaluate_bench_run <- function(loaded) {
  ## list (results.rds of a river-bench run) -> tibble, one row per graph
  ## Graphs without edges have no rows in `results`; they are recovered from
  ## the expected graphs of each task and evaluated as empty graphs.
  results <- loaded$results
  params <- loaded$params

  expected <- expected_bench_graphs(params) %>%
    dplyr::filter(task_id %in% unique(results$task_id))

  task_info <- params %>%
    dplyr::select(task_id, dataset, repetitions, method)

  purrr::map_dfr(split(results, results$task_id), function(tbl) {
    id <- tbl$task_id[1]
    true_edges <- dplyr::filter(tbl, method == "true")
    nodes <- unique(c(true_edges$from, true_edges$to))
    A_true <- edges_to_adj(true_edges$from, true_edges$to, nodes)

    est <- dplyr::filter(tbl, method != "true")
    keys <- dplyr::filter(expected, task_id == id)

    purrr::pmap_dfr(list(keys$method, keys$tau, keys$alpha),
                    function(m, tau, alpha) {
      e <- est %>%
        dplyr::filter(method == m,
                      if (is.na(tau)) TRUE else .data$tau == !!tau,
                      if (is.na(alpha)) TRUE else .data$alpha == !!alpha)
      A_est <- edges_to_adj(e$from, e$to, nodes)
      graph_metrics(A_true, A_est) %>%
        dplyr::mutate(task_id = id, method = m, tau = tau, alpha = alpha,
                      .before = 1)
    })
  }) %>%
    dplyr::left_join(dplyr::select(task_info, -method), by = "task_id") %>%
    dplyr::relocate(dataset, repetitions, .after = task_id)
}
