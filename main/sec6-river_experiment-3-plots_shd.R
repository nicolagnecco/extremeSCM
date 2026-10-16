# Plots for EP, Exp 1 on the full Danube data: Extremal Pruning with the
# extremal CI test vs pruning with PCM vs pruning with HSIC (run "river-exp").
# Same layout as sec6-river_pc-3-plots.R (X-PC, Exp 1).
# Run from the project root after main/collect_run.R.

source("main/load_packages.R")

run_dir <- "output/river-exp/20261013_122144"
alpha_plot <- 0.05                  # headline level; others for the supplement
metric_plot <- "shd"                # one of names(metric_labels)


short_lab <- function(x) {
  x <- sub("^Extremal (Pruning|PC), tau = ", "tau == ", x)
  x <- sub(", all data$", "", x)
  parse(text = x)
}

# Metrics -----------------------------------------------------------------
# All outputs are subgraphs of the complete DAG on the true order, so no edge
# can be reversed and SHD = extra + missing edges. Pruning keeps at least one
# parent per node, so no estimated graph is empty and the edge table suffices.

loaded <- read_rds(file.path(run_dir, "results.rds"))

edges_to_adj <- function(from, to, nodes) {
  A <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  if (length(from) > 0) A[cbind(from, to)] <- 1
  A
}

res <- loaded$results %>%
  group_by(task_id) %>%
  group_split() %>%
  purrr::map_dfr(function(tbl) {
    true_edges <- filter(tbl, method == "true")
    nodes <- unique(c(true_edges$from, true_edges$to))
    A_true <- edges_to_adj(true_edges$from, true_edges$to, nodes)
    
    tbl %>%
      filter(!method %in% c("true", "full")) %>%
      group_by(method, tau) %>%
      group_split() %>%
      purrr::map_dfr(function(m) {
        A <- edges_to_adj(m$from, m$to, nodes)
        tibble(
          task_id = m$task_id[1], dataset = m$dataset[1],
          bootstrap_rep = m$bootstrap_rep[1], alpha = m$alpha[1],
          method = m$method[1], tau = m$tau[1], k = m$k[1],
          d = length(nodes),
          fp = sum(A == 1 & A_true == 0),
          fn = sum(A == 0 & A_true == 1),
          shd = fp + fn
        )
      })
  })

write_rds(res, file.path(run_dir, "metrics.rds"))

metric_labels <- c(shd = "SHD", fp = "Extra edges", fn = "Missing edges")
method_levels <- c("Extremal Pruning", "PCM", "HSIC")
method_colors <- c(my_palette$blue, "#707070", "#B0B0B0")

res <- res %>%
  mutate(method = factor(method, levels = method_levels),
         dataset_lab = paste0(dataset, " (d = ", d, ")"))


# Summary table -------------------------------------------------------------

summary_tbl <- res %>%
  group_by(alpha, dataset, d, method, tau) %>%
  summarise(k = mean(k), across(c(shd, fp, fn), mean), .groups = "drop")
print(filter(summary_tbl, alpha == alpha_plot), n = Inf, width = Inf)
write_csv(summary_tbl, file.path(run_dir, "summary.csv"))


# Plot 1: extremal test on exceedances vs PCM / HSIC on all data ----------

dat_all <- res %>%
  filter(alpha == alpha_plot,
         (method == "Extremal Pruning") | (method != "Extremal Pruning" & tau == 0)) %>%
  mutate(label = if_else(method == "Extremal Pruning",
                         paste0("Extremal Pruning, tau = ", tau),
                         paste0(method, ", all data")))

tau_lab <- function(t) sprintf("%s, tau = %s", levels(res$method)[1], t)
col_pal <- c("#D55E00","#0072B2", "#009E73")
dat_all <- dat_all %>%
  mutate(label = factor(label, levels = c(tau_lab(sort(unique(tau[tau > 0]))),
                                          paste0(c("PCM", "HSIC"), ", all data"))))


gg_all <- ggplot(dat_all, aes(x = label, y = .data[[metric_plot]],
                              color = label, fill = label)) +
  facet_wrap(~ dataset_lab, nrow = 2, scales = "free_y") +
  geom_jitter(width = .2, height = 0, size = .5, alpha = .3,
              show.legend = FALSE) +
  geom_boxplot(alpha = .25, linewidth = .5, width = .7,
               outlier.shape = NA) +
  scale_color_manual(values = c(col_pal, "#707070", "#B0B0B0"), name = NULL) +
  scale_fill_manual(values = c(col_pal, "#707070", "#B0B0B0"), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)), limits = c(0, NA)) +
  labs(y = metric_labels[[metric_plot]]) +
  scale_x_discrete(labels = short_lab) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        legend.position = "none")
gg_all
save_myplot(gg_all, file.path(run_dir, paste0("river-exp-alldata-alpha=", alpha_plot,
                                              "-", metric_plot, ".pdf")),
            width = 2, height = 2, cairo = FALSE)


# Plot 2: sample-matched comparison, one panel per (dataset, tau) ---------
dat_matched_ <- res %>%
  filter(alpha == alpha_plot, tau > 0)

ext <- levels(res$method)[1]                     # "Extremal Pruning" / "Extremal PC"
taus_m <- sort(unique(dat_matched_$tau))

dat_matched <- dat_matched_ %>%
  mutate(col_key = if_else(method == ext,
                           paste0(ext, ", tau = ", tau),
                           paste0(method, ", size-matched")),
         col_key = factor(col_key,
                          levels = c(paste0(ext, ", tau = ", taus_m),
                                     paste0(c("PCM", "HSIC"), ", size-matched")))) %>% 
  mutate(tau_lab = texify_column(tau, "\\tau"),
         tau_lab = forcats::fct_reorder(tau_lab, tau))

col_vals <- setNames(c(col_pal[seq_along(taus_m)], "#707070", "#B0B0B0"),
                     levels(dat_matched$col_key))

gg_matched <- ggplot(dat_matched,
                     aes(x = method, y = .data[[metric_plot]],
                         color = col_key, fill = col_key)) +
  facet_grid(dataset_lab ~ tau_lab, scales = "free_y",
             labeller = labeller(tau_lab = label_parsed,
                                 dataset_lab = label_value)) +
  geom_jitter(position = position_jitter(width = .2, height = 0, seed = 1),
              size = .4, alpha = .3, show.legend = FALSE) +
  geom_boxplot(alpha = .25, linewidth = .5, width = .7, outlier.shape = NA) +
  scale_color_manual(values = col_vals, name = NULL) +
  scale_fill_manual(values = col_vals, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)), limits = c(0, NA)) +
  labs(y = metric_labels[[metric_plot]]) +
  scale_x_discrete(labels = c(setNames(if (ext == "Extremal PC") "X-PC" else "EP", ext),
                              PCM = "PCM", HSIC = "HSIC")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7),
        legend.position = "none")
gg_matched
save_myplot(gg_matched, file.path(run_dir, paste0("river-exp-matched-alpha=", alpha_plot,
                                                  "-", metric_plot, ".pdf")),
            width = 1.5, height = 1.5, cairo = FALSE)
