## CausEV (Mhalla, Chavez-Demoulin & Dupuis, 2020, JRSSC), restricted to a known
## causal order.
##
## Uses the authors' implementation of the causal score (their eq. 15),
## `QCDD_extremes_upper_quad` in Functions/JRSSC_functions.R of
## https://github.com/lindamhalla/Causal-mechanism-of-extreme-river-discharges.
## Clone that repository into third_party/ (see main/sec6-river_benchmark-0-prepare_run.R).
## The file is sourced into a private environment because it calls library()
## at the top level.
##
## As in Tran et al. (2024, Section 4.2), the pairwise scores are turned into a
## root-directed tree. There are two possibilities:
## - `order` is provided: the tree is restricted to such (causal) order,
##   so each non-root station picks the downstream station with the largest;
##   score; the root is the last node in the `order` vector.
## - order is not priovided: the tree is obtained via Chu-Liu/Edmonds
##   algorithm.
##
## Conventions: A[i, j] = 1 for an edge i -> j, i upstream of j.


load_causev <- function(functions_file) {
  ## str -> environment
  if (!file.exists(functions_file)) {
    stop("CausEV functions not found at '", functions_file, "'. Clone ",
         "https://github.com/lindamhalla/Causal-mechanism-of-extreme-river-discharges ",
         "into third_party/.", call. = FALSE)
  }
  for (package in c("evd", "cobs", "statmod")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Install the R package '", package, "' to run CausEV.", call. = FALSE)
    }
  }
  env <- new.env(parent = globalenv())
  sys.source(functions_file, envir = env)
  env
}


causev_scores <- function(X, order, causev_env, thd = 0.9, n_simplex = 800L,
                          n_tau = 1L) {
  ## numeric.matrix integer.vector environment ... -> list
  ## Produces the score matrix: S[i, j] estimates S^ext_{i -> j}
  ## - if `order` is available, the estimates S^ext_{i -> j} are computed for
  ##   i before j in `order` only (NA elsewhere).
  ## - if `order` is null, the estimates S^ext_{i -> j} are computed for
  ##   all pairs (i, j).
  ## Note: Pairs where the authors' code fails get NA.
  
  
  
  S <- matrix(NA_real_, ncol(X), ncol(X), 
              dimnames = list(colnames(X), colnames(X)))
  n_joint <- S
  n_gpd_warnings <- S
  n_cobs_warnings <- S
  errors <- character(0)
  
  if (!is.null(order)){
    # consider pairs according to transitive closure obtained from order
    d <- length(order)
    
    A <- upper.tri(matrix(1, nrow = d, ncol = d)) * 1
    
    inds <- which(A[order(order), order(order)] == 1, arr.ind = TRUE) %>% 
      as_tibble(.name_repair = ~ c("i", "j"))
  } else {
    # consider all pairs except (i, i)
    d <- ncol(S)
    
    A <- upper.tri(matrix(1, nrow = d, ncol = d)) * 1
    
    inds <- which(A == 1, arr.ind = TRUE) %>% 
      as_tibble(.name_repair = ~ c("i", "j"))
  }
  
  
  for (row_id in seq_len(nrow(inds))) {
    
    i <- inds$i[row_id]
    j <- inds$j[row_id]
    
    pair <- X[, c(i, j)]
    # Their Danube script sets the minimum number of joint exceedances to the
    # observed number (JRSSC_Danube_causality.R, function_on_edge).
    n_ext <- sum(pair[, 1] > stats::quantile(pair[, 1], thd + 0.005) &
                   pair[, 2] > stats::quantile(pair[, 2], thd + 0.005))
    n_joint[i, j] <- n_ext
    # The authors' code fits GPDs with evd::fpot many times per pair; count
    # the "optimization may not have succeeded" warnings instead of printing them.
    n_warn <- 0L
    n_cobs <- 0L
    res <- tryCatch(
      withCallingHandlers(
        causev_env$QCDD_extremes_upper_quad(
          pair, method = "gpd", thd = thd, n.simplex = n_simplex,
          n.sim.ext = n_ext, n.tau = n_tau),
        warning = function(w) {
          msg <- conditionMessage(w)
          if (grepl("optimization may not have succeeded", conditionMessage(w))) {
            n_warn <<- n_warn + 1L
            invokeRestart("muffleWarning")
          } else if (grepl("Not all flags are normal", msg)){
            n_cobs <<- n_cobs + 1L
            invokeRestart("muffleWarning")
          }
        }),
      error = function(e) e
    )
    n_gpd_warnings[i, j] <- n_warn
    n_cobs_warnings[i, j] <- n_cobs
    if (inherits(res, "error")) {
      errors <- c(errors, paste0(colnames(X)[i], "->", colnames(X)[j], ": ",
                                 conditionMessage(res)))
    } else {
      S[i, j] <- res$epsilon
    }
  }
  
  if (is.null(order)){
    S_ <- 1 - t(S)
    S_[upper.tri(S_)] <- S[upper.tri(S)]
  } else {
    S_ <- S
  }
  
  list(scores = S_, n_joint = n_joint, n_gpd_warnings = n_gpd_warnings,
       n_cobs_warnings = n_cobs_warnings, errors = errors)
}


fit_causev <- function(X, order, causev_env, thd = 0.9, n_simplex = 800L,
                       n_tau = 1L) {
  ## numeric.matrix integer.vector environment ... -> list
  
  stopifnot(!anyNA(X))
  
  if (!is.null(order)){
    if (is.character(order)) order <- match(order, colnames(X))
    stopifnot(!anyNA(order), length(order) == ncol(X))
  }
  
  sc <- causev_scores(X, order, causev_env, thd, n_simplex, n_tau)
  # Failed pairs are treated as uninformative (score 0.5).
  S <- sc$scores
  
  if (is.null(order)) {
    failed <- is.na(S) & !diag(TRUE, ncol(S))
  } else {
    failed <- is.na(S) & upper_in_order(order, ncol(X))
  }
  
  S[failed] <- 0.5
  
  list(
    adjacency = build_root_directed_tree(S, order, type = "max"), 
    scores = sc$scores,
    n_joint = sc$n_joint,
    n_gpd_warnings = sc$n_gpd_warnings,
    n_cobs_warnings = sc$n_cobs_warnings,
    n_failed = sum(failed),
    errors = sc$errors
  )
}


## Logical matrix with TRUE at [i, j] when i comes before j in `order`.
upper_in_order <- function(order, d) {
  ## integer.vector integer -> logical.matrix
  pos <- integer(d)
  pos[order] <- seq_along(order)
  outer(pos, pos, `<`) & outer(pos > 0, pos > 0, `&`)
}
