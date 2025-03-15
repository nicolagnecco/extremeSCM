# Taken from the pcm_code repository
# Source: Anton Rask Lundborg, Ilmun Kim, Rajen D. Shah, Richard J. Samworth (2024)
# Reference: https://github.com/ARLundborg/pcm_code
pcm_test <- function(Y, X, Z, reg_method, gtilde_method = NULL,
                     vhat_reg_method = NULL, var_min = 0.01,
                     reg_params = list()) {
  #' reg_params is a list containing optional regression parameters for "mhat",
  #' "mtilde", "ghat", "gtilde", "vhat" and "mhat_fhat"
  
  
  Y <- as.numeric(Y)
  n <- length(Y)
  X <- as.matrix(X)
  Z <- as.matrix(Z)
  
  direction_indices <- sample(1:n, n / 2)
  main_indices <- sample(setdiff(1:n, direction_indices))
  
  X_dir <- X[direction_indices, ]
  Z_dir <- Z[direction_indices, ]
  Y_dir <- Y[direction_indices]
  
  ghat <- do.call(reg_method, c(
    list(X = cbind(X_dir, Z_dir), y = Y_dir),
    reg_params[["ghat"]]
  ))
  
  if (is.null(gtilde_method)) {
    gtilde <- function(X, Z) ghat(cbind(X, Z))
  } else {
    gtilde <- do.call(gtilde_method, c(
      list(X = X_dir, Z = Z_dir, Y = Y_dir),
      reg_params[["gtilde"]]
    ))
  }
  ghat_dir <- ghat(cbind(X_dir, Z_dir))
  gtilde_dir <- gtilde(X_dir, Z_dir)
  
  mtilde <- do.call(reg_method, c(
    list(X = Z_dir, y = ghat_dir),
    reg_params[["mtilde"]]
  ))
  mtilde_dir <- mtilde(Z_dir)
  
  htilde_dir <- gtilde_dir - mtilde_dir
  
  rho <- mean((Y_dir - ghat_dir + gtilde_dir - mtilde_dir) * htilde_dir)
  sgn <- ifelse((rho < 0), -1, 1)
  
  sqr_resid_dir <- (Y_dir - ghat_dir)^2
  if (is.null(vhat_reg_method)) {
    vtilde <- do.call(
      reg_method,
      c(
        list(X = cbind(X_dir, Z_dir), y = sqr_resid_dir),
        reg_params[["vhat"]]
      )
    )
  } else {
    vtilde <- do.call(
      vhat_reg_method,
      c(
        list(X = cbind(X_dir, Z_dir), y = sqr_resid_dir),
        reg_params[["vhat"]]
      )
    )
  }
  vtilde_dir <- vtilde(cbind(X_dir, Z_dir))
  a <- function(c) mean(sqr_resid_dir / (pmax(vtilde_dir, 0) + c))
  
  if (a(0) <= 1) {
    chat <- 0
  } else {
    chat <- uniroot(function(c) a(c) - 1, c(0, 10), extendInt = "yes")$root
  }
  vhat <- function(X, Z) pmax(vtilde(cbind(X, Z)) + chat, var_min)
  
  Z_main <- Z[main_indices, ]
  X_main <- X[main_indices, ]
  Y_main <- Y[main_indices]
  
  fhat_main <- sgn * (gtilde(X_main, Z_main) - mtilde(Z_main)) /
    vhat(X_main, Z_main)
  
  mhat <- do.call(reg_method, c(
    list(X = Z_main, y = Y_main),
    reg_params[["mhat"]]
  ))
  eps <- Y_main - mhat(Z_main)
  
  mhat_fhat <- do.call(reg_method, c(
    list(X = Z_main, y = fhat_main),
    reg_params[["mhat_fhat"]]
  ))
  xi <- fhat_main - mhat_fhat(Z_main)
  
  
  R <- xi * eps
  test_statistic <- sqrt(length(R)) * mean(R) / stats::sd(R)
  return(1 - pnorm(test_statistic))
}
