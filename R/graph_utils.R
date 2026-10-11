build_root_directed_tree <- function(S, order, type = c("min", "max")){
  ## numeric.matrix integer.vector|NULL character -> numeric.matrix
  ## produces the root-directed tree using
  ## - order_restricted_tree: if `order` is integer.vector
  ## - liu_chu_edmonds_tree: if `order` is NULL
  type <- match.arg(type)
  if (is.null(order)){
    chu_liu_edmonds_tree(S, type)
  } else {
    order_restricted_tree(S, order, type)
  }
}

chu_liu_edmonds_tree <- function(S, type = c("min", "max")) {
  ## numeric.matrix character -> numeric.matrix
  ## produces the root-directed minimum/maximum spanning tree
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

  t(T_r[[opt_r]])
}

edmonds_arborescence <- function(W, r, verbose = FALSE){
  ## numeric.matrix integer boolean -> numeric.matrix
  ## produces the minimum weighted spanning arborescence OUT of r
  ## Note: every node except for r has exactly one parent; r has no parents
  ## W[i, j] is the weight for edge i -> j
  ## Note: W[i, i] is always ignored in the minimization as self loops are
  ## not allowed

  W
}

edmonds_arborescence_greedy <- function(W, r, verbose = FALSE){
  ## numeric.matrix integer boolean -> numeric.matrix
  ## produces the minimum weighted spanning arborescence OUT of r
  ## Note: every node except for r has exactly one parent; r has no parents
  ## W[i, j] is the weight for edge i -> j
  ## Note: W[i, i] is always ignored in the minimization as self loops are
  ## not allowed

  W_original <- W
  d <- ncol(W)

  ## remove non-candidate edges 
  diag(W) <- Inf
  W[, r] <- Inf

  A <- matrix(0, nrow = d, ncol = d)

  candidate_parents <- c(r)

  for (i in seq_len(d - 1)) {

    # for each candidate parent choose the minimum weight edge
    res <- purrr::map(seq_along(candidate_parents), function(j){
      p_ <- candidate_parents[j]
      min_w <- min(W[p_, ])
      c_ <- which.min(W[p_, ])

      A_ <- A
      A_[p_, c_] <- 1
      W_ <- W
      W_[, c_] <- Inf

      if (verbose){
        cat("iteration:", i,  "-- candidate (pa, ch):", c(p_,c_),
            "-- score:", min_w,
            "-- A_ has dir cycle:", has_directed_cycles(A_),
            "\n")
      }   
      if (!has_directed_cycles(A_)){
        list(
          score = min_w,
          A = A_,
          W = W_,
          parent = p_,
          child = c_
        )
      }
    }) |>
  purrr::discard(is.null)
    
    ## pick best parent
    idx_opt <- which.min(unlist(purrr::transpose(res)$score))
    
    A <- res[[idx_opt]]$A
    W <- res[[idx_opt]]$W

    cat("optimal (pa, ch):", c(res[[idx_opt]]$parent, res[[idx_opt]]$child),
        "with score:", res[[idx_opt]]$score, "\n")
    print(A)

    ## update candidate parents
    candidate_parents <- which(apply(W, 1, min) != Inf)
  }
  ## return updated weight matrix
  W_opt <- matrix(0, nrow = d, ncol = d)
  W_opt[A == 1] <- W_original[A == 1]
  W_opt
}



W <- matrix(100, 4, 4)
W[1, 2] <- 1;  W[1, 3] <- 50; W[1, 4] <- 50
W[2, 3] <- 5;  W[2, 4] <- 50
W[3, 4] <- 5;  W[4, 3] <- 0
W
W_opt <- edmonds_arborescence_greedy(W, 1, verbose = TRUE)
W_opt
is_rooted_directed_tree(W_opt, 1)



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

W_opt <- edmonds_arborescence(W, 2)

reachability(W_opt)


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
