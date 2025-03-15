evaluate_dag <- function(B_estimated, B_true) {
    d <- ncol(B_true)
    is_rooted <- sum((colSums(B_estimated) == 0)) == 1
    tpr <- sum(B_estimated * B_true) / sum(B_true) # correctly keep -> reject when I should reject -> power
    fpr <- sum(B_estimated * (1 - B_true)) / sum(1 - B_true) # fail to prune -> reject when I should accept -> type I error/level
    hamming <- causalXtreme:::compute_str_ham_distance(B_true, B_estimated) * d * (d - 1)/2
    g_true <- as(B_true, "graphNEL")
    g_estimated <- as(B_estimated, "graphNEL")
    hamming_pcalg <- pcalg::shd(g_true, g_estimated)
    sid <- causalXtreme:::compute_str_int_distance(B_true, B_estimated)


    tibble::tibble(
        is_rooted = is_rooted,
        tpr = tpr,
        fpr = fpr,
        hamming = hamming,
        hamming_pcalg = hamming_pcalg,
        sid = sid
    )
}
