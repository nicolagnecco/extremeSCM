source("main/load_packages.R")

# Load results
B <- 100
ln <- c(TRUE)
alphas <- c(0.01, 0.05)

filename <- "output/river/20250306-163004-B=100-declustered=FALSE-simulated=FALSE-mode=subsample.rds"
loaded_data <- read_rds(filename)

str_datetime <- str_extract(filename, "\\d{8}-\\d{6}")

# Access stored station info
station_info <- loaded_data$station_info

# Access graphs
estimated_graphs <- loaded_data$results %>% 
  as_tibble() %>% 
  # filter(method != "full") %>% 
  filter(if_else(method %in% c("HSIC", "PCM", "true"), 
                 is.na(tau),
                 if_else(method %in% c("Extremal Pruning"),
                         tau %in% c(0.9, .95, .975), FALSE))) %>% 
  mutate(method = case_when(
    method == "Extremal Pruning" ~ paste0("$\\tau=", tau, "$"),  # Correct parse format for LaTeX-style labels
    TRUE ~ method
  )) %>%
  mutate(method = if_else(method == "true", "True DAG", method)) %>% 
  filter(bootstrap_rep == 29) %>% 
  mutate(method = 
           factor(method, 
                  levels = c(
                    "True DAG",
                    "$\\tau=0.9$",
                    "$\\tau=0.95$",
                    "$\\tau=0.975$",
                    "HSIC",
                    "PCM"
                  ),
                  labels = c(
                    "True DAG",
                    "$\\tau=0.9$",
                    "$\\tau=0.95$",
                    "$\\tau=0.975$",
                    "dHSIC",
                    "PCM"
                  )
                  ))
  

gg <- plot_tibble(estimated_graphs) +
  facet_grid(dataset ~ method, labeller = label_tex)
gg

save_myplot(gg, plt_nm = "output/river/river-maps.pdf", 
            width=1.5, height = 1.5, cairo = FALSE)

