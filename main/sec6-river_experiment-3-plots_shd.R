# Plots for the river experiment (Extremal Pruning with the extremal CI test vs
# PCM vs HSIC). Run from the project root after sec6-river_experiment-2-collect_run.R.

source("main/load_packages.R")

run_dir <- "output/river-exp/20261007_165615"
alpha_plot <- 0.01                      # significance level shown in the figure
metric_plot <- "SHD Normalized"         # one of the labels in `metric_labels`
plotname <- file.path(run_dir, paste0("rivers", 
                                      "-alpha=", alpha_plot, 
                                      "-metric=", metric_plot, ".pdf"))

loaded <- read_rds(file.path(run_dir, "results.rds"))
graphs <- loaded$results


# Evaluate graphs ---------------------------------------------------------
# One row per estimated graph. A graph is identified by its task and, within a
# task, by (method, tau); alpha is fixed within a task in this experiment.

edges_to_adj <- function(tbl, nodes) {
  A <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  A[cbind(tbl$from, tbl$to)] <- 1
  A
}

res <- graphs %>%
  group_by(task_id) %>%
  group_split() %>%
  map_dfr(function(tbl) {
    true_edges <- tbl %>% filter(method == "true") %>% distinct(from, to)
    nodes <- unique(c(true_edges$from, true_edges$to))
    g_true <- edges_to_adj(true_edges, nodes)

    tbl %>%
      filter(method != "true") %>%
      group_by(method, tau) %>%
      group_split() %>%
      map_dfr(function(m) {
        g_est <- edges_to_adj(distinct(m, from, to), nodes)
        shd <- SID::hammingDist(g_true, g_est)
        tibble(
          task_id = unique(m$task_id),
          method = unique(m$method),
          tau = unique(m$tau),
          alpha = unique(m$alpha),
          log_transform = unique(m$log_transform),
          dataset = unique(m$dataset),
          bootstrap_rep = unique(m$bootstrap_rep),
          d = length(nodes),
          shd = shd,
          shd_normalized = shd / (length(nodes) - 1),
          fp = sum(g_est == 1 & g_true == 0),
          fn = sum(g_est == 0 & g_true == 1)
        )
      })
  })

write_rds(res, file.path(run_dir, "metrics.rds"))


# Plot: methods side by side, one panel per (dataset) ---------------
# Goal: Compare extremal conditional indpendence test with non-extremal ones
res_exp <- res
res <- res_exp
metric_labels <- c(shd = "SHD", fp = "False Positives", fn = "False Negatives",
                   shd_normalized = "SHD Normalized")

metric_plot <- "SHD"        


dat2plot <- res %>% 
  filter(method != "full", alpha == alpha_plot, log_transform) %>% 
  filter(
   (tau == 0 & method != "Extremal Pruning") |
     (tau %in% c(0.95, 0.96, 0.97, 0.98) & method == "Extremal Pruning")
  ) %>% 
  mutate(method = if_else(method == "Extremal Pruning",
                          paste0("Extremal Pruning - tau = ", tau),
                          method)) %>% pivot_longer(cols = names(metric_labels), names_to = "metric_name",
                                                    values_to = "metric") %>%
  mutate(metric_name = factor(metric_name, levels = names(metric_labels),
                              labels = metric_labels)) %>%
  filter(metric_name == metric_plot)


labels_legend <- unname(TeX(c(
  "Extremal Pruning - $\\tau = 0.95$",
  "Extremal Pruning - $\\tau = 0.96$",
  "Extremal Pruning - $\\tau = 0.97$",
  "Extremal Pruning - $\\tau = 0.98$",
  "dHSIC", 
  "PCM")))

ggplot(dat2plot, aes(x = method, y = metric, color = method)) +
  facet_wrap(~ dataset, nrow = 2, scales = "free_y") +
  geom_boxplot(alpha = .5, size = .65, fatten = 1.25) +
  # scale_color_manual(values = c(my_palette$blue, "#707070", "#B0B0B0")) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)),
                     limits = c(0, NA)) +
  labs(y = metric_plot, color = "Method") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


# Plot: methods side by side, one panel per (dataset, tau) ---------------
# Goal: Sample-matched analysis
# EP uses `taus`; PCM and HSIC use `taus_other`, which also contains 0 (all
# data). Only the taus run by every method are shown.
taus_common <- res %>%
  filter(method != "full", tau > 0) %>%
  distinct(method, tau) %>%
  count(tau) %>%
  filter(n == n_distinct(res$method[res$method != "full"])) %>%
  pull(tau)

dat2plot <- res %>%
  filter(method != "full", alpha == alpha_plot, log_transform,
         tau %in% taus_common) %>%
  pivot_longer(cols = names(metric_labels), names_to = "metric_name",
               values_to = "metric") %>%
  mutate(metric_name = factor(metric_name, levels = names(metric_labels),
                              labels = metric_labels),
         method = factor(method, levels = c("Extremal Pruning", "PCM", "HSIC"),
                         labels = c("Extremal Pruning", "PCM", "dHSIC"))) %>%
  filter(metric_name == metric_plot)

gg <- ggplot(dat2plot, aes(x = method, y = metric, color = method)) +
  facet_grid(dataset ~ factor(tau), scales = "free_y") +
  geom_boxplot(alpha = .5, size = .65, fatten = 1.25) +
  scale_color_manual(values = c(my_palette$blue, "#707070", "#B0B0B0")) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)),
                     limits = c(0, NA)) +
  labs(y = metric_plot, color = "Method") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
gg

save_myplot(gg, plotname, width = 2, height = 2, cairo = FALSE)



