sample_SCM <- function(n, B_full, B_0, B_0_w, Sigma = diag(nrow(B_full)),
                       has_nonlinearities = TRUE){
  
  d <- nrow(B_full)
  
  # compute causal order and identify root node
  caus_order <- causalXtreme:::compute_caus_order(B_full)
  root_id <- caus_order[1]
  
  # sample exponential for root node
  r_exp <- rexp(n) * (2 * rbinom(n, size = 1, prob = .5) - 1) * 
    (sqrt(Sigma[root_id, root_id]) / sqrt(2))
  
  # sample Gaussian for other nodes
  r_gauss <- mvtnorm::rmvnorm(
    n = n, 
    sigma = Sigma[-root_id, -root_id, drop = FALSE]
  )
  
  # collect noise
  eps <- matrix(nrow = n, ncol = d)
  eps[, root_id] <- r_exp
  eps[, -root_id] <- r_gauss
  
  # generate data
  X <- matrix(0, nrow = n, ncol = d)
  
  X[, caus_order[1]] <- eps[, caus_order[1]]
  
  for (j in caus_order[-1]){
    if (has_nonlinearities){
      X_transformed <- 1 / (1 + X^2)
      X[, j] <- (X %*% B_0_w[, j, drop = FALSE]) +
        (X_transformed %*% (B_full - B_0)[, j, drop = FALSE]) + eps[, j]
    } else {
      X[, j] <- (X %*% B_0_w[, j, drop = FALSE]) + eps[, j]
    }
  }
  
  return(X)
  
}