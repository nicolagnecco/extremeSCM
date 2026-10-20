# Checks edmonds_arborescence() against brute force on random small graphs.
# Run from the project root:  Rscript tests/test_edmonds.R

source("main/load_packages.R")

## Brute force over all arborescences rooted at r (for testing; d <= 6).
brute_force_arborescence_cost <- function(W, r) {
  ## numeric.matrix integer -> numeric
  d <- nrow(W)
  W[is.na(W)] <- Inf
  nodes <- setdiff(seq_len(d), r)
  choices <- as.matrix(expand.grid(rep(list(seq_len(d)), length(nodes))))
  best <- Inf
  for (k in seq_len(nrow(choices))) {
    parent <- integer(d)
    parent[nodes] <- choices[k, ]
    if (any(parent[nodes] == nodes)) next             # self-loop
    if (!is.null(find_parent_cycle(parent, r))) next
    best <- min(best, sum(W[cbind(parent[nodes], nodes)]))
  }
  best
}


is_arborescence <- function(A, r) {
  d <- nrow(A)
  if (sum(A[, r]) != 0) return(FALSE)
  if (any(colSums(A[, -r, drop = FALSE]) != 1)) return(FALSE)
  parent <- integer(d)
  others <- setdiff(seq_len(d), r)
  parent[others] <- apply(A[, others, drop = FALSE], 2, which.max)
  is.null(find_parent_cycle(parent, r))
}

# 1. A case where growing the tree greedily from the root is not optimal:
#    greedy takes 1 -> 2 (3) then 1 -> 3 (5), total 8;
#    the optimum is 1 -> 3 (5) and 3 -> 2 (2), total 7.
W <- matrix(NA, 3, 3)
W[1, 2] <- 3; W[1, 3] <- 5
W[2, 1] <- 7; W[2, 3] <- 7
W[3, 1] <- 6; W[3, 2] <- 2
A <- edmonds_arborescence(W, 1)
stopifnot(A[1, 3] == 1, A[3, 2] == 1, sum(A) == 2, sum(W[A == 1]) == 7)

# 2. Random graphs: continuous costs and heavily tied integer costs, every root
set.seed(1)
n_checks <- 0L
for (trial in 1:300) {
  d <- sample(2:6, 1)
  W <- if (trial %% 2 == 0) matrix(runif(d * d), d, d) else
    matrix(sample(0:2, d * d, replace = TRUE), d, d) * 1.0
  diag(W) <- NA
  for (r in seq_len(d)) {
    A <- edmonds_arborescence(W, r)
    stopifnot(is_arborescence(A, r))
    stopifnot(abs(sum(W[A == 1]) - brute_force_arborescence_cost(W, r)) < 1e-9)
    n_checks <- n_checks + 1L
  }
}
message("All ", n_checks, " brute-force checks passed.")


# 3. Against networkx (tests/make_reference_edmonds.py): optimal costs over all
#    roots. Costs, not trees, are compared: ties can give different optimal trees.
best_cost <- function(W) {
  min(vapply(seq_len(nrow(W)), function(r) {
    sum(W[edmonds_arborescence(W, r) == 1])
  }, 0))
}

ref <- jsonlite::read_json("tests/reference/edmonds-random.json", simplifyVector = TRUE)
for (k in seq_along(ref$costs)) {
  W <- ref$matrices[[k]]                       # JSON null (diagonal) -> NA
  stopifnot(abs(best_cost(W) - ref$costs[k]) < 1e-9)
}
message("All ", length(ref$costs), " networkx checks passed (random graphs, d <= 12).")

# 4. Real QTree score matrices: same total score as the QTree package's own
#    unrestricted tree (spanTree in QTree.fit). Sink-directed tree = arborescence
#    of the reversed graph, hence t(S).
qref <- jsonlite::read_json("tests/reference/edmonds-qtree.json", simplifyVector = TRUE)
for (k in seq_len(nrow(qref))) {
  S <- unname(as.matrix(read.csv(
    sprintf("tests/reference/%s-scores-q%s.csv", qref$dataset[k], qref$q[k]),
    header = FALSE, na.strings = c("nan", "NA"))))
  stopifnot(abs(best_cost(t(S)) - qref$cost[k]) < 1e-9)
}
message("All ", nrow(qref), " QTree-package checks passed (real score matrices).")
