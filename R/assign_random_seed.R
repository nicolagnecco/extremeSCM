assign_random_seed <- function(tbl, grouping_vars, seed) {
    ## tibble character_vector integer -> tibble
    ## assign random seed according to the variables in grouping_vars
    grouping_vars <- sort(grouping_vars)

    if (is.null(grouping_vars)) {
        tbl <- tbl %>%
            dplyr::rowwise()
    } else {
        tbl <- tbl %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars)))
    }

    tbl %>%
        set_rng(seed) %>%
        dplyr::ungroup()
}

set_rng <- function(tbl, seed) {
    ## adds to tbl a column with seeds to generate independent streams of random
    ## numbers.
    ##
    ## Args:
    ##     - tbl: a tibble where the columns contain the parameter settings and the
    ##       rows contain the simulation runs.
    ##     - seed: an integer with the seed (or NULL)
    ##
    ## Returns:
    ##     The function returns tbl appending a column with seeds used to generate
    ##     independent streams of random numbers.
    ##
    ## Note:
    ##     This function ensures that the simulations are fully repeatable.
    ##     This is possible because it assigns to each simulation run a unique
    ##     random seed (generated with L'Ecuyer RNG method, which is suitable
    ##     for parallel processing, too).

    m <- dplyr::n_groups(tbl)
    group_idxs <- group_indices_rebased(tbl)

    # create independent RNG streams with L'Ecuyer method
    rng <- rngtools::RNGseq(m, seed = seed, simplify = FALSE)
    curr_seed <- rng[[m]]
    rng <- rng[group_idxs]

    # add RNG streams to tbl
    tbl$rng <- rng

    # return tibble
    return(tbl)
}

group_indices_rebased <- function(tbl) {
    ## tibble -> numeric_vector
    ## produce group labels in ascending order

    group_idxs <- tibble::tibble(idx_dplyr = dplyr::group_indices(tbl))
    group_labels <- dplyr::distinct(group_idxs) %>%
        tibble::rownames_to_column(var = "idx_new") %>%
        dplyr::mutate(idx_new = as.integer(idx_new))

    idxs <- group_idxs %>%
        dplyr::left_join(group_labels, by = "idx_dplyr") %>%
        dplyr::select(idx_new)

    idxs$idx_new
}
