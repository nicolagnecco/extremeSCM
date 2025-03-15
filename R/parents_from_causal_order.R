parents_from_causal_order <- function(causal_order){
  p <- length(causal_order)
  purrr::map(seq_len(p), function(i){
    parents_i <- causal_order[seq_len(which(causal_order == i) - 1)]
  })
}
