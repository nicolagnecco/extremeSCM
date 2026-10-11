# Plots for the river benchmark (Extremal Pruning vs QTree vs CausEV).
# Run from the project root after sec6-river_benchmark-2-collect_run.R.

source("main/load_packages.R")

run_dir <- "output/river-bench/20261011_164320"
plotname <- file.path(run_dir, "river-benchmark-shd.pdf")
plotname_errors <- file.path(run_dir, "river-benchmark-errors.pdf")

loaded <- read_rds(file.path(run_dir, "results.rds"))
graphs <- loaded$results


# Evaluate graphs ---------------------------------------------------------

edges_to_adj <- function(tbl, nodes) {
  A <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  A[cbind(tbl$from, tbl$to)] <- 1
  A
}

res <- graphs %>%
  group_by(dataset, bootstrap_rep) %>%
  group_split() %>%
  map_dfr(function(tbl) {
    true_edges <- tbl %>% filter(method == "true") %>% distinct(from, to)
    nodes <- unique(c(true_edges$from, true_edges$to))
    g_true <- edges_to_adj(true_edges, nodes)
    r_true <- reachability(g_true)

    tbl %>%
      filter(method != "true") %>%
      group_by(method, tau, alpha) %>%
      group_split() %>%
      map_dfr(function(m) {
        g_est <- edges_to_adj(distinct(m, from, to), nodes)
        tibble(
          method = unique(m$method),
          tau = unique(m$tau),
          alpha = unique(m$alpha),
          dataset = unique(m$dataset),
          bootstrap_rep = unique(m$bootstrap_rep),
          d = length(nodes),
          shd = SID::hammingDist(g_true, g_est),
          fp = sum(g_est == 1 & g_true == 0),
          fn = sum(g_est == 0 & g_true == 1),
          shd_reach = sum(reachability(g_est) != r_true)
        )
      })
  })

write_rds(res, file.path(run_dir, "metrics.rds"))


# Summary table -----------------------------------------------------------

ep_levels <- tidyr::crossing(tau = sort(unique(na.omit(res$tau))), 
                             alpha = sort(unique(na.omit(res$alpha)))) %>% 
  mutate(name = paste0("tau = ", tau, " - alpha = ", alpha))


method_levels <- c(
  paste0("Extremal Pruning - ", ep_levels$name),
  "QTree", "CausEV")

res_plot <- res %>%
  # filter(method == "Extremal Pruning") %>% 
  mutate(method = if_else(method == "Extremal Pruning",
                          paste0("Extremal Pruning - ", 
                                 paste0("tau = ", tau, " - alpha = ", alpha)), 
                          method),
         method = factor(method, levels = method_levels))

summary_tbl <- res_plot %>%
  group_by(dataset, d, method) %>%
  summarise(across(c(shd, fp, fn, shd_reach), mean), .groups = "drop")
print(summary_tbl, n = Inf)
write_csv(summary_tbl, file.path(run_dir, "river-benchmark-summary.csv"))


# SHD plot ----------------------------------------------------------------

ep_taus <- sort(unique(na.omit(res$tau)))
labels_legend <- unname(TeX(c(
  paste0("Extremal Pruning - $\\tau = ", ep_taus, "$"),
  "QTree", "CausEV")))
colors <- c(my_palette$blue, my_palette$red, my_palette$green,
            "#E69F00", "#707070")[seq_along(method_levels)]

gg <- ggplot(res_plot %>% filter(alpha == 0.0025 | is.na(alpha)) %>% 
               filter(tau <= .9 | is.na(tau)), 
             aes(x = method, y = shd, color = method)) +
  facet_wrap(~dataset, nrow = 2, scales = "free") +
  geom_boxplot(alpha = .5, size = .65, fatten = 1.25) +
  # scale_color_manual(values = colors, labels = labels_legend) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)),
                     limits = c(0, NA)) +
  labs(y = "Structural Hamming Distance", color = "Method") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
gg

save_myplot(gg, plotname, width = 2, height = 2, cairo = FALSE)


# Error types: extra (FP) vs missing (FN) edges -----------------------------
# The tree methods always return d - 1 edges; EP can also keep extra edges.

dat_errors <- res_plot %>%
  filter(alpha == 0.005 | is.na(alpha)) %>% 
  filter(tau <= .9 | is.na(tau)) %>% 
  pivot_longer(c(fp, fn, shd_reach), names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels = c("fp", "fn", "shd_reach"),
                         labels = c("Extra edges", "Missing edges",
                                    "SHD of reachability graph")))

gg_errors <- ggplot(dat_errors, aes(x = method, y = value, color = method)) +
  facet_grid(metric ~ dataset, scales = "free_y") +
  geom_boxplot(alpha = .5, size = .5, fatten = 1.25) +
  # scale_color_manual(values = colors, labels = labels_legend) +
  labs(y = NULL, color = "Method") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
gg_errors

save_myplot(gg_errors, plotname_errors, width = 3, height = 2, cairo = FALSE)

