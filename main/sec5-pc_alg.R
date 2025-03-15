# Dependency imports
source("main/load_packages.R")

# R options
options(future.rng.onMisuse = "ignore")
RNGkind(kind = "L'Ecuyer-CMRG")

# Function definition
compute_shd <- function(params) {
  # This is the computation function that takes as input the parameters 
  # in the row of the tibble
  # Extract parameters from the current row
  alpha <- params$alpha
  d <- params$d
  e_n <- params$e_n
  n <- params$n
  tau <- params$taus
  prob <- params$prob
  myrng <- params$rng[[1]]
  
  # set seed
  rngtools::setRNG(myrng)

  # Sample random DAG
  dags <- random_dags(d, prob)
  B_0_w <- dags$B_0_w
  B_0 <- dags$B_0
  g_0 <- as(B_0, "graphNEL")
  g0cpdag <- pcalg::dag2cpdag(g_0)
  
  # Generate Sigma and Gamma
  Sigma <- diag(rep(1, d))
  Gamma <- generate_Gamma(B_0_w, Sigma, which(colSums(B_0) == 0))
  
  # Sample data and fit PC for multiple cases
  results <- list()
  for (data_type in c("mp", "ms", "scm")) {
    if (data_type == "mp") {
      k <- floor(n * (1 - tau))
      Y <- rmpareto(n = k, par = Gamma, d = d)
    } else if (data_type == "ms") {
      Y <- rmstable(n = n, par = Gamma, d = d)
      Y <- data2mpareto(Y, p = tau)
      k <- nrow(Y)
    } else if (data_type == "scm") {
      Y <- sample_SCM(n = n, B_full = dags$B_resit, B_0 = B_0, B_0_w = B_0_w)
      Y <- data2mpareto(Y, p = tau)
      k <- nrow(Y)
    }
    vario <- emp_vario(Y)
    
    pc.fit <- pc(
      suffStat = list(Gamma = vario, n = k),
      indepTest = HR_CItest,
      alpha = alpha,
      verbose = FALSE,
      p = d
    )
    
    adjm <- wgtMatrix(getGraph(pc.fit), transpose = FALSE)
    g_fitted <- as(adjm, "graphNEL")
    shd_value <- pcalg::shd(g0cpdag, g_fitted)
    
    results[[data_type]] <- tibble(
      method = data_type,
      shd = shd_value,
      k = k
    )
  }
  
  do.call(bind_rows, results) %>% bind_cols(params)
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
foldername <- "output/pc-alg/"
pdfname <- "pcalg-results.pdf"

# configure parameters
reps <- seq_len(20)
alpha <- c(0.01)
d <- c(5, 10, 15)
e_n <- c(2, 3.5, 5)
n <- c(10000)
taus <- c(.5, .7, .9, .95, .975, .99, .995)
seed <- 42
group_with_constant_seed <- c("reps", "alpha", "d", "e_n", "n")


# Create the file name
file_name <- glue(
  foldername,
  "pc-algo-seed={seed}-{datetime}"
)


# perpare tibble with parameters
tbl <- tidyr::crossing(reps, alpha, d, e_n, n, taus) %>% 
  group_by(reps, alpha, d, e_n, n) %>% 
  mutate(prob = min(1, e_n / (d - 1)))

tbl_rng <- assign_random_seed(tbl, 
                              grouping_vars = group_with_constant_seed,
                              seed = seed)

# params <- tbl_rng[1, ] # to debug compute_shd_function

if (RUN_SIMULATIONS){
  # Apply the function to all rows with progress tracking
  plan(multisession, workers = N_WORKERS)
  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(tbl_rng))
    results <- furrr::future_map_dfr(seq_len(nrow(tbl_rng)), function(x){
      p()
      compute_shd(tbl_rng[x, ])
    })
  }, enable = TRUE)
  plan(sequential)
  
  # Save file
  saveRDS(object = results, file = here(glue(file_name, ".rds")))
}


if (PLOT){
  # Read file

  rdsname <- "output/pc-alg/pc-algo-seed=42-2025-03-06_12-55-15.rds"
  res <- read_rds(rdsname) %>% 
    mutate(
      tau = factor(taus),
      method = refactor_methods(method, lst_methods)
    ) %>%
    group_by(method, alpha, d, e_n, n, tau) %>%
    summarise(metric = mean(shd)) %>%
    mutate(
      d = texify_column(d, "d"),
      e_n = texify_column(e_n, "\\E_N")
    )
  
  gg <- ggplot(res %>% filter(alpha == 0.01)) +
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
