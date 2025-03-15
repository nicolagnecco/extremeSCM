# Dependency imports
source("main/load_packages.R")

# R options
options(future.rng.onMisuse = "ignore")
RNGkind(kind = "L'Ecuyer-CMRG")

# Function definition
compute_metrics_pruning <- function(params) {
  # Extract parameters
  alpha <- params$alpha
  d <- params$d
  e_n <- params$e_n
  n <- params$n
  tau <- params$taus
  prob <- params$prob
  myrng <- params$rng[[1]]
  
  # Set seed
  rngtools::setRNG(myrng)
  
  # Sample random DAGs
  dags <- random_dags(d, prob)
  B_0_w <- dags$B_0_w
  B_0 <- dags$B_0
  B_resit <- dags$B_resit
  prunbl_prs <- prunable_pairs(B_resit) %>% transpose()
  root_id <- which(colSums(B_0) == 0)
  
  # Generate Gamma
  Sigma <- diag(rep(1, d))
  Gamma <- generate_Gamma(B_0_w, Sigma, root_id)
  
  # Initialize results
  results <- list()
  
  # Loop over data types
  for (data_type in c("mp", "ms", "scm")) {
    if (data_type == "mp") {
      # Multivariate Pareto
      k <- floor(n * (1 - tau))
      Y <- rmpareto(n = k, par = Gamma, d = d)
    } else if (data_type == "ms") {
      # Max-stable
      Y <- rmstable(n = n, par = Gamma, d = d)
      Y <- data2mpareto(Y, p = tau)
      k <- nrow(Y)
    } else if (data_type == "scm") {
      # Structural Causal Model
      Y <- sample_SCM(n = n, B_full = B_resit, B_0 = B_0, B_0_w = B_0_w)
      Y <- data2mpareto(Y, p = tau)
      k <- nrow(Y)
    }
    vario <- emp_vario(Y)
    
    # Configure the conditional independence test
    ci_test <- purrr::partial(
      HR_CItest,
      suffStat = list(Gamma = vario, n = k)
    )
    
    # Prune edges
    B_pruned <- prune_edge_algo(
      B_full = B_resit,
      prunbl_prs = prunbl_prs,
      ci_test = ci_test,
      alpha = alpha,
      base_pruning_algo = prune_edge_fast,
      separating_set_selector = mb_selector
    )
    
    # Evaluate DAG
    result <- evaluate_dag(B_pruned, B_0)
    
    # Append results
    results[[data_type]] <- result %>% 
      bind_cols(
        tibble(method = data_type, algoname = "algo-1-fast-mb", k = k)
      )
  }
  
  # Combine results and add parameters
  bind_rows(results) %>% bind_cols(params)
}


# Constants
ARGS <- commandArgs(trailingOnly = TRUE)
if (length(ARGS) == 0) {
  N_WORKERS <- 3
} else {
  N_WORKERS <- as.numeric(ARGS[1])
}

RUN_SIMULATIONS <- FALSE
PLOT <- TRUE

# Get current date and time, format it
datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
foldername <- "output/random-dags/"
pdfname <- "extremal-pruning-results.pdf"


# configure parameters
reps <- seq_len(20)
alpha <- c(0.01)
d <- c(5, 10, 15)
e_n <- c(2, 3.5, 5)
n <- c(10000)
taus <- c(.5, .7, .9, .95, .975, .99, .995)
# floor(n * (1 - taus))
seed <- 42
group_with_constant_seed <- c("reps", "alpha", "d", "e_n", "n")


# Create the file name
file_name <- glue(
  foldername,
  "extremal-pruning-seed={seed}-{datetime}"
)


# perpare tibble with parameters
tbl <- tidyr::crossing(reps, alpha, d, e_n, n, taus) %>% 
  group_by(reps, alpha, d, e_n, n) %>% 
  mutate(prob = min(1, e_n / (d - 1)))

tbl_rng <- assign_random_seed(tbl, 
                              grouping_vars = group_with_constant_seed,
                              seed = seed)


# params <- tbl_rng[1430, ] # to debug compute_metrics_pruning function
# res <- compute_metrics_pruning(params)

if (RUN_SIMULATIONS){
  # Apply the function to all rows with progress tracking
  plan(multisession, workers = N_WORKERS)
  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(tbl_rng))
    results <- furrr::future_map_dfr(seq_len(nrow(tbl_rng)), function(x){
      p()
      compute_metrics_pruning(tbl_rng[x, ])
    })
  }, enable = TRUE)
  plan(sequential)
  
  # Save file
  saveRDS(object = results, file = here(glue(file_name, ".rds")))
}

if (PLOT){
  
  # Read file
  rdsname <- "output/random-dags/extremal-pruning-seed=42-2025-03-06_12-47-04.rds"
  
  res <- read_rds(rdsname) %>%
    mutate(
      tau = factor(taus),
      method = refactor_methods(method, lst_methods)
    ) %>%
    group_by(method, alpha, d, e_n, n, tau) %>%
    summarise(metric = mean(hamming)) %>%
    mutate(
      d = texify_column(d, "d"),
      e_n = texify_column(e_n, "\\E_N")
    )
  
  gg <- ggplot(res) +
    facet_grid(d ~ e_n, scales = "free_y", labeller = label_parsed) +
    stat_summary(aes(x = tau, y = metric, col = method, group = method),
                 fun = mean, geom = "line", alpha = .75
    ) +
    geom_point(aes(x = tau, y = metric, col = method),
               shape = 21, fill = "white", size = 2, stroke = 0.75, alpha = 0.75
    ) +
    scale_color_manual(values = my_palette_methods, name = "Method:",
                       labels = unname(c("(a) MPD", "(b) Max-stable", "(c) Extremal-SCM"))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    xlab(TeX("$\\tau$")) +
    ylab("Structural Hamming Distance")
  gg
  
  save_myplot(gg, plt_nm = here(glue(foldername, pdfname)), 
              height = 1.5, width = 1.5, cairo = FALSE)
  
}

