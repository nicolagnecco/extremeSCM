# Dependency imports
source("main/load_packages.R")

# Define functions
sample.vec <- function(x, ...){
  x[sample(length(x), ...)]
}

ci_test_random_k <- function(x, y, S, Y){
  
  # compute variogram using a random k in S
  k_random <- sample.vec(x = S, size = 1)
  vario_random <- emp_vario(data = Y, k = k_random)
  n_k <- ceiling(mean(purrr::map_dbl(seq_len(ncol(Y)), function(k){
    sum(Y[, k] > 1)
  })))
    
  HR_CItest(x, y, S,
            suffStat = list(Gamma = vario_random, n = n_k))
}

# Parameters
RUN_SIMULATIONS <- FALSE
RANDOM_DAG <- FALSE
RANDOM_COV <- FALSE
cov_type <- if(RANDOM_COV){'random'}else{'fixed'}
d <- 4
e_n <- 5
prob <- min(1, e_n / (d - 1))
n <- 1000
Sigma <- diag(c(1, 3, 1, 2))
reps <- 50
taus <- c(.9, .95, .975)
n * (1 - taus)
taus <- taus[n * (1 - taus) >  4 * d]
seed <- 1
set.seed(seed)

if (RANDOM_DAG){
  file_name <- glue(
    "output/random-dags-cond-ind-tests/",
    glue("{format(Sys.time(), 
       '%Y%m%d-%H%M%S')}-dag_type=random-cov_type={cov_type}-n={n}-d={d}-e_n={e_n}-seed={seed}")
  )
} else {
  file_name <- glue(
    "output/random-dags-cond-ind-tests/",
    glue("{format(Sys.time(), 
       '%Y%m%d-%H%M%S')}-dag_type=diamond-cov_type={cov_type}-n={n}-d=4-seed={seed}")
  )
}

if (RUN_SIMULATIONS){
  # Set up parallel backend
  plan(multisession, workers = 4)
  # plan(sequential)
  with_progress({
    prog <- progressor(steps = reps * length(taus) * 3) # Define how many steps there are
    
    res <- furrr::future_map_dfr(seq_len(reps), function(rep) {
      suppressPackageStartupMessages({
        library(graph)
        Sigma
      })
      
      
      if (RANDOM_DAG){
        # Sample random DAGs
        dags <- random_dags(d, prob)
        B_0_w <- dags$B_0_w
        B_0 <- dags$B_0
        B_resit <- dags$B_resit
      } else {
        # Sample diamond DAG without 3 -> 4
        B <- rbind(
          c(0, 1, 1, 0),
          c(0, 0, 0, 1),
          c(0, 0, 0, 0),
          c(0, 0, 0, 0)
        )
        colnames(B) <- rownames(B) <- 1:d
        B_0_w <- B
        B_0 <- B
        B_resit <- B
        B_resit[3, 4] <- 1
      }
      
      # Compute root node
      root_id <- which(colSums(B_0) == 0)
      
      if (RANDOM_COV){
        Sigma <- diag(runif(d, min = 1, max = 4))
      }
      
      # Generate Gamma
      Gamma <- generate_Gamma(B_0_w, Sigma, root_id)
      
      
      # 1. Sample data from max-stable
      Y <- rmstable(n = n, par = Gamma, d = d)
      
      res_ms <- purrr::map_dfr(taus, function(tau) {
        prog()
        
        Y_ms <- data2mpareto(Y, p = tau)
        k <- nrow(Y_ms)
        vario_ms <- emp_vario(Y_ms)
        
        ci_test_ms <- purrr::partial(
          HR_CItest,
          suffStat = list(Gamma = vario_ms, n = k)
        )
        
        res1 <- perform_citests(B_0, ci_test_ms) %>% mutate(
          n = n, k = k, tau = tau, method = "ms", 
          vario = "average", rep = rep
        )
        
        ci_test_ms_random <- purrr::partial(
          ci_test_random_k, Y = Y_ms
        )
        
        res2 <- perform_citests(B_0, ci_test_ms_random) %>% mutate(
          n = n, k = k, tau = tau, method = "ms", 
          vario = "random",  rep = rep
        )
        
        
        bind_rows(res1, res2)
      })
      
      # 2. Sample from SCM
      Y <- sample_SCM(n = n, B_full = B_resit, B_0 = B_0, 
                      B_0_w = B_0_w, Sigma = Sigma)
      
      res_scm <- purrr::map_dfr(taus, function(tau) {
        prog()
        
        Y_scm <- data2mpareto(Y, p = tau)
        k <- nrow(Y_scm)
        vario_scm <- emp_vario(Y_scm)
        
        ci_test_scm <- purrr::partial(
          HR_CItest,
          suffStat = list(Gamma = vario_scm, n = k)
        )
        
        res1 <- perform_citests(B_0, ci_test_scm) %>% mutate(
          n = n, k = k, tau = tau, method = "scm",
          vario = "average", rep = rep
        )
        
        ci_test_scm_random <- purrr::partial(
          ci_test_random_k, Y = Y_scm
        )
        
        res2 <- perform_citests(B_0, ci_test_scm_random) %>% mutate(
          n = n, k = k, tau = tau, method = "scm",
          vario = "random", rep = rep
        )
        
        bind_rows(res1, res2)
      })
      
      # 3. Sample data from multivariate Pareto
      res_mp <- purrr::map_dfr(taus, function(tau) {
        prog()
        tau_ <- tau
        n_mp <- floor(n * (1 - tau))
        Y_mp <- rmpareto(n = n_mp, par = Gamma, d = d)
        k <- nrow(Y_mp)
        vario_mp <- emp_vario(Y_mp)
        
        ci_test_mp <- purrr::partial(
          HR_CItest,
          suffStat = list(Gamma = vario_mp, n = k)
        )
        
        res1 <- perform_citests(B_0, ci_test_mp) %>% mutate(
          n = n, k = k, tau = tau, method = "mp", 
          vario = "average", rep = rep
        )
        
        ci_test_mp_random <- purrr::partial(
          ci_test_random_k, Y = Y_mp
        )
        
        res2 <- perform_citests(B_0, ci_test_mp_random) %>% mutate(
          n = n, k = k, tau = tau, method = "mp", 
          vario = "random", rep = rep
        )
        
        bind_rows(res1, res2)
      })
      
      
      return(bind_rows(res_mp, res_ms, res_scm))
    }, .options = furrr::furrr_options(seed = TRUE))
  })
  
  plan(sequential)
  
  saveRDS(object = res, file = here(glue(file_name, ".rds")))
} else {
  res <- read_rds("output/random-dags-cond-ind-tests/20250306-144315-dag_type=diamond-cov_type=fixed-n=1000-d=4-seed=1.rds")
}


alphas <- seq(0, 1, by = 0.01)

# Plot results
dat_plot <- res %>%
  mutate(
    hyp = factor(if_else(is_dsep, "H0", "H1")),
    tau = factor(tau, levels = sort(unique(tau))),
    method = refactor_methods(method, lst_methods)
  ) %>%
  group_by(hyp, tau, method, vario) %>%
  summarise(pvalues = list(pvalue), .groups = "drop") %>%
  expand_grid(alpha = alphas) %>%
  mutate(
    prop_below_alpha = map2_dbl(pvalues, alpha, ~ mean(unlist(.x) <= .y))
  ) %>%
  mutate(
    tau = texify_column(tau, "\\tau")
  ) %>%
  mutate(hyp = if_else(hyp == "H0", "Level", "Power")) %>% 
  mutate(hyp = paste0(hyp, " (", vario, ")"))

gg <- ggplot(dat_plot, aes(x = alpha)) +
  facet_grid(method ~ tau, labeller = label_parsed) +
  geom_line(aes(y = prop_below_alpha, col = hyp, linetype = hyp)) +
  geom_line(aes(x = x, y = y),
            data = tibble(x = c(0, 1), y = c(0, 1)),
            linewidth = .25, linetype = "dotted"
  ) +
  scale_color_manual(values = c(my_palette$blue,
                                my_palette$blue,
                                my_palette$red,
                                my_palette$red)) +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Tilt x-axis labels
  ) +
  labs(
    x = TeX("Significance Level $\\alpha$"),
    y = TeX("% p-values $<\\, \\alpha$"),
    color = "", # Update legend title
    linetype = "" # Ensures merged legend uses this title
  )
gg

save_myplot(gg, 
            plt_nm = here(glue(file_name, ".pdf")), cairo = FALSE,
            height = 1.25, 
            width = 1.25)
