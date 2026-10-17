build_root_directed_tree <- function(S, order, type = c("min", "max")){
  ## numeric.matrix integer.vector|NULL character -> numeric.matrix
  ## produces the sink-directed tree using
  ## - order_restricted_tree: if `order` is integer.vector
  ## - liu_chu_edmonds_tree: if `order` is NULL
  type <- match.arg(type)
  if (is.null(order)){
    chu_liu_edmonds_tree(S, type)
  } else {
    order_restricted_tree(S, order, type)
  }
}

chu_liu_edmonds_tree_slow <- function(S, type = c("min", "max")) {
  ## numeric.matrix character -> numeric.matrix
  ## produces the sink-directed minimum/maximum spanning tree
  ## given the score matrix S, where S[i, j] is the weight of edge i -> j
  ## Note: root here denotes the unique note with no children. All other
  ## nodes have exactly one child
  type <- match.arg(type)
  
  if (type == "max"){
    S <- -S
  }
  
  S <- t(S) # edmonds_arborescence swaps child and parent relation
  
  T_r <- list()
  W_r <- numeric(length = ncol(S))
  
  for (r in seq_len(ncol(S))) {
    T_r[[r]] <- edmonds_arborescence(S, r)
    W_r[r] <- sum(S[T_r[[r]] == 1])
  }
  
  opt_r <- which.min(W_r)
  
  A <- t(T_r[[opt_r]])
  dimnames(A) <- dimnames(S)
  A
}

chu_liu_edmonds_tree <- function(S, type = c("min", "max")) {
  ## numeric.matrix character -> numeric.matrix
  ## produces the sink-directed minimum/maximum spanning tree
  ## given the score matrix S, where S[i, j] is the weight of edge i -> j
  ## Note: root here denotes the unique note with no children. All other
  ## nodes have exactly one child
  type <- match.arg(type)
  nm <- dimnames(S)
  if (type == "max") S <- -S
  A <- t(min_arborescence_any_root(t(S)))   # sink-directed = reversed arborescence
  dimnames(A) <- nm
  A
}

min_arborescence_any_root <- function(W) {
  ## numeric.matrix -> numeric.matrix
  ## minimum-cost arborescence over all roots, via one Edmonds run from a dummy root
  d <- nrow(W)
  W[is.na(W)] <- Inf
  diag(W) <- Inf
  M <- 1 + sum(abs(W[is.finite(W)]))
  
  W_big <- matrix(Inf, d + 1, d + 1)       # node 1 = dummy, real nodes = 2..d+1
  W_big[-1, -1] <- W
  W_big[1, -1] <- M                         # dummy -> every real node
  A_big <- edmonds_arborescence(W_big, 1)
  A_big[-1, -1]                             # drop the dummy
}

edmonds_arborescence <- function(W, r) {
  ## numeric.matrix integer -> numeric.matrix
  ## produces the 0/1 adjacency matrix of a minimum-cost arborescence rooted at r
  d <- nrow(W)
  W[is.na(W)] <- Inf
  diag(W) <- Inf                       # no self-loops
  W[, r] <- Inf                        # nothing enters the root
  
  # 1. every node except the root picks its cheapest incoming edge
  parent <- integer(d)
  for (v in seq_len(d)) {
    if (v == r) next
    if (all(is.infinite(W[, v]))) stop("node ", v, " cannot be reached from the root")
    parent[v] <- which.min(W[, v])
  }
  
  # 2. no cycle among the chosen edges: they form the optimal arborescence
  cycle <- find_parent_cycle(parent, r)
  if (is.null(cycle)) {
    A <- matrix(0, d, d)
    others <- setdiff(seq_len(d), r)
    A[cbind(parent[others], others)] <- 1
    return(A)
  }
  
  # 3. contract the cycle into one node (the last one, m), re-weighting the
  #    edges that enter it: entering the cycle at v costs W[u, v] minus the
  #    cost of v's cycle edge, which is dropped. Remember each new edge's
  #    original endpoints, to expand the solution afterwards.
  others <- setdiff(seq_len(d), cycle)
  m <- length(others) + 1L
  new_id <- integer(d)
  new_id[others] <- seq_along(others)
  new_id[cycle] <- m
  
  W2 <- matrix(Inf, m, m)
  orig_from <- matrix(NA_integer_, m, m)
  orig_to <- matrix(NA_integer_, m, m)
  for (u in seq_len(d)) {
    for (v in seq_len(d)) {
      if (is.infinite(W[u, v])) next
      if (u %in% cycle && v %in% cycle) next          # edges inside the cycle vanish
      cost <- if (v %in% cycle) W[u, v] - W[parent[v], v] else W[u, v]
      a <- new_id[u]
      b <- new_id[v]
      if (cost < W2[a, b]) {                           # keep the cheapest parallel edge
        W2[a, b] <- cost
        orig_from[a, b] <- u
        orig_to[a, b] <- v
      }
    }
  }
  
  # 4. solve the smaller problem (the root is never in a cycle: it has no parent)
  A2 <- edmonds_arborescence(W2, new_id[r])
  
  # 5. expand: map the edges back; keep the cycle edges except the one into the
  #    node where the solution enters the cycle
  A <- matrix(0, d, d)
  edges <- which(A2 == 1, arr.ind = TRUE)
  for (k in seq_len(nrow(edges))) {
    a <- edges[k, 1]
    b <- edges[k, 2]
    A[orig_from[a, b], orig_to[a, b]] <- 1
    if (b == m) entered <- orig_to[a, b]
  }
  for (c in cycle) {
    if (c != entered) A[parent[c], c] <- 1
  }
  A
}


find_parent_cycle <- function(parent, r) {
  ## integer.vector integer -> integer.vector|NULL
  ## produces the nodes of a cycle in the parent pointers (parent[v] is v's
  ## parent; the root r has none), or NULL if following parents always reaches r
  for (start in seq_along(parent)) {
    path <- integer(0)
    v <- start
    while (v != r && !(v %in% path)) {
      path <- c(path, v)
      v <- parent[v]
    }
    if (v != r) return(path[match(v, path):length(path)])
  }
  NULL
}

has_directed_cycles <- function(A){
  ## numeric.matrix integer -> boolean
  ## produces true if A has some directed cycles
  
  A <- (A != 0) * 1
  d <- ncol(A)
  
  sum(diag(reachability(A)) != 0) > 0 # some directed cycles
}

is_rooted_directed_tree <- function(A, r){
  ## numeric.matrix integer -> boolean
  ## produces true if A is directed tree rooted at r
  
  test_1 <- !has_directed_cycles(A)
  test_2 <- sum(A[, r]) == 0  # r has no parents
  test_3 <- all(colSums(A[, -r]) == 1) # all other nodes have exacly one parent
  
  test_1 & test_2 & test_3
}



order_restricted_tree <- function(S, order, type = c("min", "max")) {
  ## numeric.matrix integer.vector character -> numeric.matrix
  ## produces the order-restricted sink-directed spanning tree from a score
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
  ## Note: assume entries of A are non-negative
  R <- A
  repeat {
    R_new <- ((R + R %*% A) > 0) * 1
    if (all(R_new == R)) break
    R <- R_new
  }
  dimnames(R) <- dimnames(A)
  R
}
