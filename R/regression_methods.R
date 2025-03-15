# Taken from the pcm_code repository
# Source: Anton Rask Lundborg, Ilmun Kim, Rajen D. Shah, Richard J. Samworth (2024)
# Reference: https://github.com/ARLundborg/pcm_code
ranger_reg_method <- function(X, y, 
                              mtry = NULL, 
                              max.depth = NULL, 
                              num.trees = 500,...) {
  n <- length(y)
  X <- as.matrix(X, nrow = n)
  d <- dim(X)[2]
  if (is.null(mtry)) {
    mtry <- d
  }
  W <- cbind(y, X)
  colnames(W) <- c("y", 1:d)
  m <- ranger::ranger(
    data = W, dependent.variable.name = "y",
    num.trees = num.trees, mtry = mtry, max.depth = max.depth, 
    importance = "impurity"
  )
  pred_func <- function(X_new) {
    X_new <- as.matrix(X_new)
    d <- dim(X_new)[2]
    colnames(X_new) <- as.character(1:d)
    as.numeric(predict(m, X_new)$predictions)
  }
  return(pred_func)
}
