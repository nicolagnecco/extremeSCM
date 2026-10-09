## QTree (Tran, Buck & Klueppelberg, 2024, JRSSB), restricted to a known causal
## order.
##
## Port of `quantileDispersion`, `QTree.fitToSamples` and `qtreeAutoSelect`
## from https://github.com/princengoc/qtree (qtree/QTree.py and
## qtree/qtreeAutoSelect.py). Differences from the Python package:
##   - the root-directed spanning tree uses as starting point an ancestral graph
##     according to `order`. Such starting graph has no cycles, so the minimum
##     root-directed spanning tree is obtained by letting every non-root node
##     pick its lowest-score child; the root is the last node of `order`.
##     Without the restriction the package runs Chu-Liu/Edmonds over all roots.
##   - no missing-value weights (the Danube data has no missing values, and the
##     package's weights are constant across columns in that case).
##
## Conventions: adjacency matrices have A[i, j] = 1 for an edge i -> j, where i
## is upstream of j. Data must be on the log scale.



## Mirrors QTree.py::quantileDispersion with source i.
qtree_scores <- function(X, q = 0.8, small_r = 0.05) {
  ## numeric.matrix numeric numeric -> numeric.matrix
  ## produces the score matrix S, where S[i, j] is the score of the
  ## edge i -> j (small = likely edge).
  d <- ncol(X)
  S <- matrix(NA_real_, d, d, dimnames = list(colnames(X), colnames(X)))
  for (i in seq_len(d)) {
    keep <- X[, i] >= stats::quantile(X[, i], q, names = FALSE)
    D <- X[keep, , drop = FALSE] - X[keep, i]
    lower <- apply(D, 2, stats::quantile, probs = small_r, names = FALSE)
    S[i, ] <- (lower - colMeans(D))^2
    S[i, i] <- NA_real_
  }
  S
}



order_restricted_tree <- function(S, order, type = c("min", "max")) {
  ## numeric.matrix integer.vector character -> numeric.matrix
  ## produces the order-restricted root-directed spanning tree from a score
  ## matrix S.
  ## `type = "min"` for QTree scores, `"max"` for agreement counts (centroid).
  type <- match.arg(type)
  d <- length(order)
  A <- matrix(0, nrow(S), ncol(S), dimnames = dimnames(S))
  for (k in seq_len(d - 1L)) {
    i <- order[k]
    downstream <- order[(k + 1L):d]
    s <- S[i, downstream]
    best <- if (type == "min") which.min(s) else which.max(s)
    A[i, downstream[best]] <- 1
  }
  A
}



reachability <- function(A) {
  ## numeric.matrix -> numeric.matrix
  ## produces the reachability matrix R, where R[i, j] = 1 if there is a
  ## directed path i -> ... -> j.
  R <- A
  repeat {
    R_new <- ((R + R %*% A) > 0) * 1
    if (all(R_new == R)) break
    R <- R_new
  }
  dimnames(R) <- dimnames(A)
  R
}


## QTree for fixed (q, small_r) on one data set.
fit_qtree_fixed <- function(X, order, q = 0.8, small_r = 0.05) {
  ## numeric.matrix integer.vector numeric numeric -> numeric.matrix
  order_restricted_tree(qtree_scores(X, q, small_r), order, type = "min")
}
## Centroid of a list of trees (QTree.fitToSamples): the order-restricted tree
## maximising the edge agreement W = mean of the adjacency matrices.

qtree_centroid <- function(trees, order){
  ## list-of-numeric.matrix integer.vector -> numeric.matrix
    W <- Reduce(`+`, trees) / length(trees)
    order_restricted_tree(W, order, type = "max")
}



## Variability Var(T) of Tran et al., eq. (11), as in QTree.fitToSamples.
qtree_variability <- function(trees, centroid){
  ## list-of-numeric.matrix numeric.matrix -> numeric
  R_c <- reachability(centroid)
  
  var_tree <- mean(vapply(trees, function(tr) sum(tr != centroid), 0)) /
    sum(centroid)
  var_reach <- mean(vapply(trees, function(tr) sum(reachability(tr) != R_c), 0)) /
    sum(R_c)

  var_tree + var_reach
}


## Auto-tuned QTree (Algorithm 2 of Tran et al.).
## For each q: fit on `nrep` subsamples of a fraction `frac`, compute the
## centroid tree from the edge agreement matrix and the variability
## Var(T) (their eq. 11). Return the centroid with the smallest variability.
fit_qtree <- function(X, order, q_grid = seq(0.7, 0.9, by = 0.025),
                      small_r = 0.05, frac = 0.75, nrep = 1000L) {
  ## numeric.matrix integer.vector ... -> list
  ## `order` may be integer column positions or column names.
  if (is.character(order)) order <- match(order, colnames(X))
  stopifnot(!anyNA(order), !anyNA(X), length(order) == ncol(X))

  n <- nrow(X)
  m <- floor(n * frac)
  subsamples <- lapply(seq_len(nrep), function(l) sample.int(n, m))

  per_q <- lapply(q_grid, function(q) {
    trees <- lapply(subsamples, function(idx) {
      fit_qtree_fixed(X[idx, , drop = FALSE], order, q, small_r)
    })

    centroid <- qtree_centroid(trees, order)

    list(q = q, var_T = qtree_variability(trees, centroid), centroid = centroid)
  })

  var_T <- vapply(per_q, `[[`, 0, "var_T")
  best <- which.min(var_T)

  list(
    adjacency = per_q[[best]]$centroid,
    q = q_grid[best],
    small_r = small_r,
    var_T = var_T[best],
    tuning = data.frame(q = q_grid, var_T = var_T)
  )
}
