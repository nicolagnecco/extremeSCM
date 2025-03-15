source("main/load_packages.R")

options(future.rng.onMisuse = "ignore")
RNGkind("L'Ecuyer-CMRG")

# Constants
ARGS <- commandArgs(trailingOnly = TRUE)
if (length(ARGS) == 0) {
  N_WORKERS <- 3
} else {
  N_WORKERS <- as.numeric(ARGS[1])
}
RUN_SIMULATIONS <- FALSE
ALLMETHODS <- FALSE
SIMULATED <- FALSE
MODE <- c("subsample", "bootstrap")[1]
N_REPS <- 50
SUBSAMPLE_PROP <- .25
DECLUSTERED <- FALSE
SEED <- 1321
TAUS <- c(0.9, .95, 0.975)
floor((1-TAUS) * SUBSAMPLE_PROP * 4600)
ALPHAS <- c(0.05)
B <- 100 # number of trees in random forest for non-extremal methods
DATASETS <- list(
  list("dataset-1", c(12:1)),
  list("dataset-2", c(22:20, 7:1)),
  list("dataset-3", c(24:23, 4:1)),
  list("dataset-4", c(27:25, 4:1)),
  list("dataset-5", c(19:14, 2:1)),
  list("dataset-6", c(29:28, 31:30, 13, 1))
) %>%
  transpose() %>%
  as_tibble(.name_repair = ~ c("dataset", "causal_order")) %>%
  mutate(dataset = unlist(dataset)) %>%
  tidyr::crossing(alphas = ALPHAS, log_transform = c(TRUE), 
                  repetitions = seq_len(N_REPS)) %>% 
  group_by(dataset, repetitions) %>%
  set_rng(SEED)

# DATASETS <- DATASETS[1, ]

# Set up parallelization
if (RUN_SIMULATIONS){
  plan(multisession, workers = N_WORKERS)
  
  progressr::with_progress(
    {
      p <- progressr::progressor(steps = nrow(DATASETS))
      
      results <- future_map(seq_len(nrow(DATASETS)), function(i) {
        p() # Progress tracking
        
        
        dataset_name <- DATASETS$dataset[i]
        causal_order <- DATASETS$causal_order[[i]]
        alpha <- DATASETS$alphas[i]
        log_transform <- DATASETS$log_transform[i]
        rngtools::setRNG(DATASETS$rng[[i]])
        repetition <- DATASETS$repetitions[i]
        
        
        # Load station info
        station_info <- read_rds("data/StsInfoChose.rds") %>%
          filter(id %in% causal_order) %>%
          mutate(id_name = paste0("station_", id)) %>%
          rename(Lon = Long) %>%
          dplyr::select(id_name, Lat, Lon)
        
        station_names <- paste0("station_", causal_order)
        
        
        # Define full DAG
        full_adjmat <- upper.tri(
          matrix(
            ncol = length(station_names),
            nrow = length(station_names)
          ),
          diag = FALSE
        ) * 1
        colnames(full_adjmat) <- rownames(full_adjmat) <- station_names
        full_graph <- graph_from_adjacency_matrix(full_adjmat, mode = "directed")
        full_tbl <- graph_to_tibble(full_graph, station_info) %>%
          mutate(method = "full")
        
        # Define true DAG
        ind_true_dag <- which(full_adjmat > 0, arr.ind = TRUE) %>%
          as_tibble() %>%
          filter(col - row == 1) %>%
          as.matrix()
        
        true_adjmat <- matrix(0,
                              nrow = length(station_names), ncol = length(station_names)
        )
        true_adjmat[ind_true_dag] <- 1
        colnames(true_adjmat) <- rownames(true_adjmat) <- station_names
        true_graph <- graph_from_adjacency_matrix(true_adjmat, mode = "directed")
        true_tbl <- graph_to_tibble(true_graph, station_info) %>%
          mutate(method = "true")
        
        
        # Load river data
        if (DECLUSTERED){
          river <- read_rds("data/river_clean_declustered.rds")
        } else {
          river <- read_rds("data/river_clean_full.rds")
        }
        
        X_dat <- river[, station_names]
        dat <- as.matrix(X_dat)
        
        if (log_transform){
          dat <- log(dat + 1)
        }
        
        if (SIMULATED){
          n_sim <- nrow(dat)
          dat <- sample_SCM(n = n_sim, 
                            B_full = true_adjmat, 
                            B_0 = true_adjmat, 
                            B_0_w = true_adjmat, 
                            has_nonlinearities = FALSE)
        }
        
        
        # Subsample data
        if (MODE == "bootstrap"){
          idx_bootstrap <- sample(x = nrow(dat), size = nrow(dat),
                                  replace = TRUE)
        } else if (MODE == "subsample"){
          idx_bootstrap <- sample(x = nrow(dat), 
                                  size = nrow(dat) * SUBSAMPLE_PROP,
                                  replace = FALSE)
        } else {
          stop("MODE should be one of 'subsample' or 'bootstrap'.")
        }
        
        dat <- dat[idx_bootstrap, ]
        
        
        # Extremal pruning
        full_adjmat <- full_adjmat
        prunbl_prs <- prunable_pairs(full_adjmat) %>%
          arrange(desc(y)) %>%
          transpose()
        
        # set up parameters
        n <- nrow(dat)
        taus <- TAUS
        
        params <- tidyr::crossing(n, taus)
        
        # Extremal prunining
        xpr_tbl <- purrr::map_dfr(seq_len(nrow(params)), function(i) {
          tau <- params$taus[i]
          
          # Prune DAG
          Y <- data2mpareto(dat, p = tau)
          k <- nrow(Y)
          vario <- emp_vario(Y)
          
          ci_test <- purrr::partial(HR_CItest, 
                                    suffStat = list(Gamma = vario, n = k))
          
          B_pruned <- prune_edge_algo(
            B_full = full_adjmat, prunbl_prs = prunbl_prs,
            ci_test = ci_test, alpha = alpha,
            base_pruning_algo = purrr::partial(prune_edge_fast, verbose=TRUE),
            separating_set_selector = mb_selector, shuffle = TRUE
          )
          
          tbl_pruned <- igraph::graph_from_adjacency_matrix(B_pruned) %>%
            graph_to_tibble(station_info = station_info)
          
          # return tibble
          tbl_pruned %>%
            mutate(method = "Extremal Pruning", k = k, tau = tau)
        })
        
        results_tbl <- bind_rows(full_tbl, true_tbl, xpr_tbl)
        
        if (ALLMETHODS){
          # PCM
          reg_method <- purrr::partial(ranger_reg_method, num.trees = B)
          
          ci_test_pcm_ <- purrr::partial(ci_test_pcm, dat = dat, 
                                         reg_method = reg_method)
          
          B_pruned_pcm <- prune_edge_algo(
            B_full = full_adjmat, prunbl_prs = prunbl_prs,
            ci_test = ci_test_pcm_, alpha = alpha,
            base_pruning_algo = purrr::partial(prune_edge_fast, verbose=TRUE),
            separating_set_selector = mb_selector, shuffle = TRUE
          )
          
          pcm_tbl <- igraph::graph_from_adjacency_matrix(B_pruned_pcm) %>%
            graph_to_tibble(station_info = station_info) %>% 
            mutate(method = "PCM")
          
          # HSIC
          ci_test_hsic_ <- purrr::partial(ci_test_hsic, dat = dat, 
                                          reg_method = reg_method)
          
          B_pruned_hsic <- prune_edge_algo(
            B_full = full_adjmat, prunbl_prs = prunbl_prs,
            ci_test = ci_test_hsic_, alpha = alpha,
            base_pruning_algo = purrr::partial(prune_edge_fast, verbose=TRUE),
            separating_set_selector = mb_selector, shuffle = TRUE
          )
          
          hsic_tbl <- igraph::graph_from_adjacency_matrix(B_pruned_hsic) %>%
            graph_to_tibble(station_info = station_info) %>% 
            mutate(method = "HSIC")
          
          results_tbl <- results_tbl %>% 
            bind_rows(pcm_tbl, hsic_tbl)
        }
        
        
        # Return both results and station info
        list(
          results = results_tbl %>%
            mutate(alpha = alpha, 
                   log_transform = log_transform,
                   dataset = dataset_name, 
                   bootstrap_rep = repetition),
          station_info = station_info
        )
      })
    },
    enable = TRUE
  )
  plan(sequential)
  
  # Extract results and station info
  graph_list <- bind_rows(map(results, "results"))
  station_info_list <- map(results, "station_info")
  
  # Save everything
  write_rds(
    list(results = graph_list, station_info = station_info_list),
    glue("output/river/{format(Sys.time(), 
       '%Y%m%d-%H%M%S')}-B={B}-declustered={DECLUSTERED}-simulated={SIMULATED}-mode={MODE}.rds")
  )
}
