prune_edge_algo <- function(
    B_full,
    prunbl_prs,
    ci_test,
    alpha,
    base_pruning_algo = prune_edge,
    separating_set_selector = parent_selector,
    shuffle=TRUE) {
    prune_edge_algo_1 <- purrr::partial(
        base_pruning_algo,
        ci_test = ci_test,
        alpha = alpha,
        separating_set_selector = separating_set_selector
    )

    if(shuffle){
      idx <- sample(length(prunbl_prs))
    } else{
      idx <- seq_len(length(prunbl_prs))
    }
    
    B_pruned <- purrr::reduce(prunbl_prs[idx],
        prune_edge_algo_1,
        .init = B_full
    )

    return(B_pruned)
}
