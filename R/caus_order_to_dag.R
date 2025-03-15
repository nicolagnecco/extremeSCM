# Taken from causalXtremes::caus_order_to_dag
# Source: causalXtremes R package from Gnecco N, Meinshausen N, Peters J, Engelke S (2021)
# Reference: https://github.com/nicolagnecco/causalXtreme
caus_order_to_dag <- function (caus_order) 
{
  if (any(is.na(caus_order))) {
    stop("The causal order caus_order cannot contain NA.")
  }
  p <- length(caus_order)
  dag <- upper.tri(x = matrix(0, nrow = p, ncol = p)) * 1
  inv_caus_order <- order(caus_order)
  return(dag[inv_caus_order, inv_caus_order])
}
