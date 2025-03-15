test_method_resit <- function(Y, X, Z, reg_method){
  # Y: numeric vector
  # X: numeric matrix
  # Z: numeric matrix
  # reg_method: function [X, Y] -> prediction_fn[X]
  # 
  # return p-value (0, 1) of hyp test Y indpendent of X given Z
  
  # fit Y using Z
  pred_func <- reg_method(Z, Y)
  
  # compute residuals
  eps <- Y - pred_func(Z)
  
  # test whether residuals r are independent of [Z, X] and return p-value
  dHSIC::dhsic.test(eps, cbind(Z, X), method = "bootstrap", B = 100)$p.value
  
}

test_method_pcm <- function(Y, X, Z, reg_method){
  # Y: numeric vector
  # X: numeric matrix
  # Z: numeric matrix
  # reg_method: function [X, Y] -> prediction_fn[X]
  # 
  # return p-value (0, 1) of hyp test Y indpendent of X given Z
  
  # test whether residuals Y mean-independent of X given Z
  pcm_test(Y, X, Z, reg_method)
  
}

ci_test_hsic <- function(x, y, S, dat, reg_method){
  X <- dat[, x, drop=FALSE]
  Y <- dat[, y]
  Z <- dat[, S, drop = FALSE]
  test_method_resit(Y, X, Z, reg_method)
}


ci_test_pcm <- function(x, y, S, dat, reg_method){
  
  X <- dat[, x, drop=FALSE]
  Y <- dat[, y]
  Z <- dat[, S, drop = FALSE]
  test_method_pcm(Y, X, Z, reg_method)
}

