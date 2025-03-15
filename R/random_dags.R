random_dags <- function(d, prob) {
    res <- sample_rooted_dag(d, prob)

    B_resit <- (res$B != 0) * 1
    B_0 <- B_resit
    for (i in 1:d) {
        prs <- prunable_pairs(B_0)
        if (nrow(prs) == 0) {
            break
        }
        id <- sample(x = seq_len(nrow(prs)), size = 1)
        pair <- prs[id, ]
        B_0[pair$x, pair$y] <- 0
    }

    B_0_w <- B_0 * res$B
    for (i in 1:d) {
        if (!(sum(B_0_w[, i]) == 0)) {
            indices <- which(B_0_w[, i] != 0)
            B_0_w[indices[1], i] <- 1 - sum(B_0_w[indices[-1], i])
        }
    }

    list(
        B_0_w = B_0_w,
        B_0 = B_0,
        B_resit = B_resit
    )
}
