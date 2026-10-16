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
  ## shd: SHD against the true DAG; a reversal, or an undirected edge where
  ##   the truth is directed, counts as one mistake
  skel <- function(A) (A + t(A)) > 0
  S_true <- skel(A_true)
  S_est <- skel(A_est)
  skel_fp <- sum(S_est & !S_true) / 2
  skel_fn <- sum(!S_est & S_true) / 2
  
  tibble::tibble(
    d = nrow(A_true),
    skel_fp = skel_fp,
    skel_fn = skel_fn,
    skel_shd = skel_fp + skel_fn,
    shd = SID::hammingDist(unname(A_true), unname(A_est))
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
                    function(m, tau_, alpha_) {
                      # base subsetting: inside dplyr::filter(), `tau` would be the column
                      e <- est[est$method == m, , drop = FALSE]
                      if (!is.na(tau_)) e <- e[!is.na(e$tau) & e$tau == tau_, , drop = FALSE]
                      if (!is.na(alpha_)) e <- e[!is.na(e$alpha) & e$alpha == alpha_, , drop = FALSE]
                      A_est <- edges_to_adj(e$from, e$to, nodes)
                      graph_metrics(A_true, A_est) %>%
                        dplyr::mutate(task_id = id, method = m, tau = tau_, alpha = alpha_,
                                      .before = 1)
                    })
  }) %>%
    dplyr::left_join(dplyr::select(task_info, -method), by = "task_id") %>%
    dplyr::relocate(dataset, repetitions, .after = task_id)
}