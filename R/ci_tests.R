# Taken from pcalg::logQ1pm
# Source: pcalg R package (Markus Kalisch, Alain Hauser, and Martin Mächler)
# Reference: https://rdrr.io/cran/pcalg/src/R/Aaux.R
logQ1pm <- function(r) {
  log1p(2 * r / (1 - r))
}

pcorfromGamma <- function(i, j, k, Gamma, cut.at = 0.9999999) {
  if (length(k) == 0) {
    r <- 1 # is there a better way?
  } else {
    Theta <- graphicalExtremes::Gamma2Theta(Gamma[c(i, j, k), c(i, j, k)])
    r <- -Theta[1, 2] / sqrt(Theta[1, 1] * Theta[2, 2])
  }
  if (is.na(r)) {
    0
  } else {
    min(cut.at, max(-cut.at, r))
  }
}

zStatfromGamma <- function(x, y, S, Gamma, n) {
  r <- pcorfromGamma(x, y, S, Gamma)
  res <- sqrt(n - length(S) + 1 - 3) * 0.5 * logQ1pm(r)
  if (is.na(res)) {
    0
  } else {
    res
  }
}

HR_CItest <- function(x, y, S, suffStat) {
  z <- zStatfromGamma(x, y, S, Gamma = suffStat$Gamma, n = suffStat$n)
  2 * pnorm(abs(z), lower.tail = FALSE)
}


#' Perform Conditional Independence Tests on a Graph
#'
#' Given an adjacency matrix representing a graph, this function performs
#' conditional independence tests for all pairs of variables (x, y) given
#' a conditioning set S. It tests each pair where y > x, considering all
#' possible (non-empty) subsets of remaining variables as S.
#'
#' @param B An adjacency matrix representing a graph, where the presence
#'   of a non-zero entry (i, j) indicates a directed edge from node i to node j.
#' @param ci_test_fun A function that takes arguments (x, y, S) and returns a
#'   p-value from a conditional independence test. The function must be capable
#'   of accepting x and y as the variables to test and S as the conditioning set.
#'
#' @return A tibble containing the results of the conditional independence tests.
#'   Each row corresponds to a test result with the following columns:
#'   - `x`: the first variable in the pair
#'   - `y`: the second variable in the pair
#'   - `S`: a list of variables included in the conditioning set
#'   - `pvalue`: the p-value from the conditional independence test
#'   - `is_dsep`: a boolean value indicating whether the variables x and y are
#'     d-separated by S in the graph.
#'
#' @examples
#' # Define a conditional independence test function
#' ci_test_fun <- function(x, y, S) {
#'     # Here you would normally integrate a statistical test
#'     return(runif(1)) # Dummy p-value
#' }
#'
#' # Create an example adjacency matrix
#' B <- matrix(c(
#'     0, 1, 0, 0,
#'     0, 0, 1, 1,
#'     0, 0, 0, 0,
#'     0, 0, 1, 0
#' ), nrow = 4, byrow = TRUE)
#'
#' # Perform the conditional independence tests
#' results <- perform_citests(B, ci_test_fun)
#' print(results)
#'
perform_citests <- function(B, ci_test_fun) {
  # Create graphNEL object from adjacency matrix
  g <- as(B, "graphNEL")
  
  vars <- seq_len(ncol(B))
  
  # perform all tests
  xy_pairs <- tidyr::crossing(x = vars, y = vars) %>%
    filter(y > x)
  
  purrr::map2_dfr(xy_pairs$x, xy_pairs$y, function(x, y) {
    # get all possible conditioning sets S \subseteq V \setminus {x, y}
    S_vars <- setdiff(vars, c(x, y))
    
    subsets <- lapply(seq_along(S_vars), function(k) {
      combn(S_vars, k, simplify = FALSE)
    }) %>% unlist(recursive = FALSE)
    
    # for each conditioning set S, do CI and d-sep test
    map_dfr(subsets, function(S) {
      p_value <- ci_test_fun(x, y, S)
      is_dsep <- pcalg::dsep(as.character(x), as.character(y),
                             S = as.character(S), g = g
      )
      tibble(x = x, y = y, S = list(S), pvalue = p_value, is_dsep = is_dsep)
    })
  })
}

d_sep_statements <- function(B, x, y) {
  # B: numeric.matrix encoding adjacency matrix over `p` variables
  # x: numeric index of variable in {1, ..., p}
  # y: numeric index of variable in {1, ..., p}
  # -> list with
  # S: numeric.vector variables in conditioning set such that x is d-separated from y given S
  
  
  # Create graphNEL object from adjacency matrix
  g <- as(B, "graphNEL")
  
  vars <- seq_len(ncol(B))
  
  # get all possible conditioning sets S \subseteq V \setminus {x, y}
  S_vars <- setdiff(vars, c(x, y))
  
  subsets <- lapply(seq_along(S_vars), function(k) {
    combn(S_vars, k, simplify = FALSE)
  }) %>% unlist(recursive = FALSE)
  
  # for each conditioning set S, do CI and d-sep test
  map(subsets, function(S) {
    is_dsep <- pcalg::dsep(as.character(x), as.character(y),
                           S = as.character(S), g = g
    )
    
    if (is_dsep) S
  }) %>% discard(is.null)
}

d_sep_statements_vars <- function(B, x, y, vars) {
  # B: numeric.matrix encoding adjacency matrix over `p` variables
  # x: numeric index of variable in {1, ..., p}
  # y: numeric index of variable in {1, ..., p}
  # vars: numeric.vector with variables that can potentially separate x from y
  # -> list with
  # S: numeric.vector variables in conditioning set such that x is d-separated from y given S
  
  
  # Create graphNEL object from adjacency matrix
  g <- as(B, "graphNEL")
  
  # get all possible conditioning sets S \subseteq V \setminus {x, y}
  S_vars <- setdiff(vars, c(x, y))
  
  if (length(S_vars) == 1){
    subsets <- list(S_vars)
  } else {
    subsets <- lapply(seq_along(S_vars), function(k) {
      combn(S_vars, k, simplify = FALSE)
    }) %>% unlist(recursive = FALSE)
  }
  
  
  
  # for each conditioning set S, do CI and d-sep test
  map(subsets, function(S) {
    is_dsep <- pcalg::dsep(as.character(x), as.character(y),
                           S = as.character(S), g = g
    )
    
    if (is_dsep) S
  }) %>% discard(is.null)
}



prunable_pairs <- function(B) {
  # B: numeric.matrix adjacency matrix
  # returns a tibble with cols (`x`, `y`) denoting parent-child pairs to prune
  
  vars <- which(colSums(B) > 1)
  B_0 <- matrix(0, ncol = ncol(B), nrow = nrow(B))
  B_0[, vars] <- B[, vars]
  tbl <- which(B_0 > 0, arr.ind = TRUE) %>% as_tibble()
  
  colnames(tbl) <- c("x", "y")
  return(tbl)
}

# Algorithm 2 from manuscript (order-dependent but graph returned is rooted)
prune_edge <- function(B, pair, ci_test, alpha, 
                       separating_set_selector=NULL,
                       verbose = FALSE) {
  # B: numeric.matrix adjacency matrix
  # pair: list with `x` and `y` denoting parent-child pair
  # ci_test: function(x, y, S) -> [0, 1]
  # alpha: numeric in (0, 1)
  # -> new graph with pruned edge
  
  # set variables
  x <- pair$x
  y <- pair$y
  
  # check if can prune (i.e., y has at least 2 parents)
  if (sum(B[, y]) < 2) {
    return(B)
  }
  
  # prune B
  B_temp <- B
  B_temp[x, y] <- 0
  
  # compare d-sep statements between B_temp and B
  dsep_diff <- setdiff(
    d_sep_statements(B_temp, x, y),
    d_sep_statements(B, x, y)
  )
  
  
  if(verbose) print(paste("Pair", pair))
  if(verbose) print(dsep_diff)
  

  # 3. test all d-sep statements in dsep_diff
  pvals <- purrr::map(dsep_diff, function(S) {
    ci_test(x, y, S)
  })
  
  if(verbose){
    print(pvals)
  }
  # 4. remove edge if there is a test such that p-value is above alpha
  # Recall that H_0: x is conditional independent of y given S. So we
  
  if (any(pvals > alpha)) {
    if(verbose) print("prune")
    return(B_temp)
    
  } else {
    if(verbose) print("don't prune")
    return(B)
    
  }
}

prune_edge_fast <- function(B, pair, ci_test, alpha, 
                            separating_set_selector=mb_selector,
                            verbose = FALSE) {
  # B: numeric.matrix adjacency matrix
  # pair: list with `x` and `y` denoting parent-child pair
  # ci_test: function(x, y, S) -> [0, 1]
  # alpha: numeric in (0, 1)
  # separating_set_selector: function(B, x, y) -> numeric.vector
  # -> new graph with pruned edge
  
  # set variables
  x <- pair$x
  y <- pair$y
  
  # check if can prune (i.e., y has at least 2 parents)
  if (sum(B[, y]) < 2) {
    return(B)
  }
  
  # prune B
  B_temp <- B
  B_temp[x, y] <- 0
  
  # select separating set
  set_nodes <- separating_set_selector(B_temp, x, y)
  root_B <- which(colSums(B_temp) == 0)
  
  if (x == root_B){
    dsep_diff <- list(set_nodes)
  } else {
    dsep_diff <- list(unique(c(set_nodes, root_B)))
  }
  
  
  if(verbose) print(paste("Pair", pair))
  if(verbose) print(dsep_diff)
  
  # 3. test all d-sep statements in dsep_diff
  pvals <- purrr::map(dsep_diff, function(S) {
    ci_test(x, y, S)
  })
  
  if(verbose) print(pvals)
  
  # 4. remove edge if there is a test such that p-value is above alpha
  # Recall that H_0: x is conditional independent of y given S.
  
  if (any(pvals > alpha)) {
    if(verbose) print(paste("prune"))
    return(B_temp)
  } else {
    if(verbose) print(paste("don't prune"))
    return(B)
  }
}


# Helpers
get_parents <- function(B, x){
  which(B[, x] != 0)
}

get_children <- function(B, x){
  which(B[x, ] != 0)
}

get_mb <- function(B, x){
  pa_x <- get_parents(B, x)
  ch_x <- get_children(B, x)
  
  parents_of_ch_x <- get_parents_of_nodes(B, ch_x)
  
  return(setdiff(unique(c(pa_x, ch_x, parents_of_ch_x)), x))
}

get_parents_of_nodes <- function(B, nodes){
  parents_of_nodes <- c()
  for (node in nodes){
    parents_of_nodes <- c(get_parents(B, node), parents_of_nodes)
  }
  return(parents_of_nodes)
}

mb_selector <- function(B_temp, x, y){
  pa_y <- get_parents(B_temp, y)
  ch_y <- get_children(B_temp, y)
  pa_ch_y <- get_parents_of_nodes(B_temp, ch_y)
  mb_y <- setdiff(unique(c(pa_y, ch_y, pa_ch_y)), y)
  
  
  if (x %in% pa_ch_y){
    mb_y <- setdiff(mb_y, x)
    mb_y <- setdiff(mb_y, ch_y)
  }
  return(mb_y)
}

parent_selector <- function(B_temp, x, y){
  
  get_parents(B_temp, y)
  
}
