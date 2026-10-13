## Helpers to run pcalg::pc() with the extremal CI test or with a
## regression-based CI test (PCM, HSIC).


fit_pc <- function(suffStat, indepTest, alpha, labels, m_max = Inf) {
  ## list function numeric character numeric -> numeric.matrix
  ## produces the estimated CPDAG as a 0/1 matrix with A[i, j] = 1 for i -> j;
  ## an undirected edge i - j has both A[i, j] = A[j, i] = 1
  fit <- pcalg::pc(suffStat = suffStat, indepTest = indepTest, alpha = alpha,
                   labels = labels, m.max = m_max, verbose = FALSE)
  A <- (pcalg::wgtMatrix(pcalg::getGraph(fit), transpose = FALSE) != 0) * 1
  dimnames(A) <- list(labels, labels)
  A
}


ci_wrap <- function(test_fn, forbid_marginal = TRUE) {
  ## [fn: x y S dat reg_method -> p-value] logical -> [fn: x y S suffStat -> p-value]
  ## turns ci_test_pcm() / ci_test_hsic() into a pcalg test. With
  ## forbid_marginal = TRUE, S = {} is always rejected, as in HR_CItest().
  force(test_fn)
  force(forbid_marginal)
  function(x, y, S, suffStat) {
    if (forbid_marginal && length(S) == 0) return(0)
    test_fn(x, y, S, suffStat$dat, suffStat$reg_method)
  }
}


pc_extremal <- function(dat, tau, alpha, labels, m_max = Inf) {
  ## numeric.matrix numeric numeric character numeric -> list(adjacency, k)
  ## PC with the extremal CI test on the multivariate Pareto exceedances
  Y <- data2mpareto(dat, p = tau)
  A <- fit_pc(suffStat = list(Gamma = graphicalExtremes::emp_vario(Y), n = nrow(Y)),
              indepTest = HR_CItest, alpha = alpha, labels = labels,
              m_max = m_max)
  list(adjacency = A, k = nrow(Y))
}