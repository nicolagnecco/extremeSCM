sample_rooted_dag <- function(p, prob, ...) {
    ## integer numeric(0, 1) ... ->
    ##   list(pardag = GaussParDAG, g = igraph, B = numeric_matrix)
    ## sample a random rooted DAG and returns a list of:
    ##   - rooted DAG as an `igraph` object
    ##   - adjency matrix as a `numeric_matrix`
    randGauss <- pcalg::r.gauss.pardag(p = p, prob = prob, ...)
    B <- randGauss$weight.mat()

    # make it rooted
    roots_id <- get_root(B)

    if (length(roots_id) > 1) {
        root <- roots_id[1]
        rest <- roots_id[-1]
        B[root, rest] <- 1
    }

    # ensure sum incoming edge weights = 1
    for (i in 1:p) {
        if (!(sum(B[, i]) == 0)) {
            indices <- which(B[, i] != 0)
            B[indices[1], i] <- 1 - sum(B[indices[-1], i])
        }
    }

    # transform it to graph
    g <- igraph::graph_from_adjacency_matrix(B, weighted = TRUE)

    return(list(pardag = randGauss, g = g, B = B))
}

get_root <- function(adj_mat) {
  ## numeric_matrix -> numeric_vector
  ## return position of roots for a given adjacency matrix
  ## ASSUMES: underlying graph is a DAG
  unname(which(colSums(adj_mat) == 0))
}


generate_Gamma <- function(B, cov_e, k){
  # B:
  # cov_e: numeric matrix with covariance of error terms (assumed diagonal)
  # k: index where we root DAG
  # -> return variogram matrix given an adjacency matrix, error covariance and root index
  
  p <- ncol(B)
  
  ones <- cbind(rep(1, p))
  e_k <- cbind(rep(0, p))
  e_k[k] <- 1
  
  I_p <- diag(p)
  P_k <- I_p - ones %*% t(e_k)
  
  Theta_k <- (I_p - B)[-k, -k] %*% solve(cov_e[-k, -k]) %*% (I_p - t(B))[-k, -k]
  
  Theta_k_tilde <- matrix(0, nrow = p, ncol = p)
  Theta_k_tilde[-k, -k] <- Theta_k
  
  Theta <- t((P_k)) %*% Theta_k_tilde %*% (P_k)
  
  graphicalExtremes::Theta2Gamma(Theta)
  
}
