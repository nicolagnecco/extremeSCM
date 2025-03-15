# Taken from causalXtreme:::compute_caus_order
# Source: causalXtremes R package from Gnecco N, Meinshausen N, Peters J, Engelke S (2021)
# Reference: https://github.com/nicolagnecco/causalXtreme
dag_to_caus_order <- function(dag){
  
  if (!all(dag %in% c(0, 1))) {
    stop("The entries of dag must be either 0 or 1.")
  }
  p <- dim(dag)[2]
  remaining <- 1:p
  caus_order <- rep(NA, p)
  for (i in 1:(p - 1)) {
    root <- min(which(colSums(dag) == 0))
    caus_order[i] <- remaining[root]
    remaining <- remaining[-root]
    dag <- dag[-root, -root]
  }
  caus_order[p] <- remaining[1]
  return(caus_order)
}
