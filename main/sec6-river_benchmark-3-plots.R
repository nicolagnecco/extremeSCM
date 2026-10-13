# Metrics and plots for Exp 2 on the declustered Danube data:
#   EP column:   Extremal Pruning vs QTree vs CausEV, order known  (river-bench)
#   X-PC column: Extremal PC vs QTree vs CausEV, no order          (river-bench-noorder)
# Run from the project root after main/collect_run.R on both run directories.

source("main/load_packages.R")

run_dirs <- c(
  order   = "output/river-bench/YYYYMMDD_HHMMSS",
  noorder = "output/river-bench-noorder/YYYYMMDD_HHMMSS"
)
# Headline setting for EP and X-PC, fixed by rule (k ~ 50-100 exceedances of
# the ~321 events per subsample); the full grid goes to the supplement.
headline <- list(tau = 0.8, alpha = 0.01)
out_dir <- "output/river-bench-figures"
recompute <- FALSE        # TRUE to recompute metrics.rds in the run directories

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# Metrics (cached as metrics.rds in each run directory) -------------------

metrics <- purrr::imap_dfr(run_dirs, function(run_dir, setting) {
  f <- file.path(run_dir, "metrics.rds")
  if (!recompute && file.exists(f)) {
    m <- read_rds(f)
  } else {
    m <- evaluate_bench_run(read_rds(file.path(run_dir, "results.rds")))
    write_rds(m, f)
  }
  mutate(m, setting = setting)
})

method_levels <- c("Extremal Pruning", "Extremal PC", "QTree", "CausEV")
method_colors <- c("Extremal Pruning" = my_palette$blue,
                   "Extremal PC" = my_palette$blue,
                   "QTree" = "#707070", "CausEV" = "#B0B0B0")

metrics <- metrics %>%
  mutate(method = factor(method, levels = method_levels),
         dataset_lab = paste0(dataset, " (d = ", d, ")"))

headline_dat <- metrics %>%
  filter(is.na(tau) | (tau == headline$tau & alpha == headline$alpha))


# Summary tables (headline setting) ---------------------------------------

summary_order <- headline_dat %>%
  filter(setting == "order") %>%
  group_by(dataset, d, method) %>%
  summarise(shd = mean(shd), extra = mean(skel_fp), missing = mean(skel_fn),
            .groups = "drop")

summary_noorder <- headline_dat %>%
  filter(setting == "noorder") %>%
  group_by(dataset, d, method) %>%
  summarise(skel_shd = mean(skel_shd), extra = mean(skel_fp),
            missing = mean(skel_fn), correct_dir = mean(n_correct_dir),
            wrong_dir = mean(n_wrong_dir), undirected = mean(n_undirected),
            sid_lower = mean(sid_lower / (d * (d - 1))), .groups = "drop")

print(summary_order, n = Inf, width = Inf)
print(summary_noorder, n = Inf, width = Inf)
write_csv(summary_order, file.path(out_dir, "summary-ep-order.csv"))
write_csv(summary_noorder, file.path(out_dir, "summary-xpc-noorder.csv"))


# Plot helpers ------------------------------------------------------------

box_by_dataset <- function(dat, metric, ylab) {
  ggplot(dat, aes(x = method, y = .data[[metric]], color = method)) +
    facet_wrap(~ dataset_lab, nrow = 2, scales = "free_y") +
    geom_boxplot(alpha = .5, size = .65, fatten = 1.25) +
    scale_color_manual(values = method_colors, drop = TRUE) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)),
                       limits = c(0, NA)) +
    labs(y = ylab, color = "Method") +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}


# EP column: order known, SHD = extra + missing edges ---------------------

gg_order <- box_by_dataset(filter(headline_dat, setting == "order"),
                           "shd", "Structural Hamming Distance")
gg_order
save_myplot(gg_order, file.path(out_dir, "exp2-ep-order-shd.pdf"),
            width = 2, height = 2, cairo = FALSE)


# X-PC column: no order, skeleton SHD -------------------------------------

gg_noorder <- box_by_dataset(filter(headline_dat, setting == "noorder"),
                             "skel_shd", "Skeleton SHD")
gg_noorder
save_myplot(gg_noorder, file.path(out_dir, "exp2-xpc-noorder-skeleton-shd.pdf"),
            width = 2, height = 2, cairo = FALSE)


# X-PC column: orientation of the true edges ------------------------------
# Share of the d - 1 true edges found with the right direction, the reversed
# direction, undirected, or missed. On a chain, PC cannot orient anything.

orient_dat <- headline_dat %>%
  filter(setting == "noorder") %>%
  mutate(undirected_true = (d - 1) - skel_fn - n_correct_dir - n_wrong_dir) %>%
  group_by(dataset_lab, d, method) %>%
  summarise(Correct = mean(n_correct_dir), Reversed = mean(n_wrong_dir),
            Undirected = mean(undirected_true), Missing = mean(skel_fn),
            .groups = "drop") %>%
  pivot_longer(Correct:Missing, names_to = "status", values_to = "count") %>%
  mutate(share = count / (d - 1),
         status = factor(status, levels = c("Correct", "Reversed",
                                            "Undirected", "Missing")))

gg_orient <- ggplot(orient_dat, aes(x = method, y = share, fill = status)) +
  facet_wrap(~ dataset_lab, nrow = 2) +
  geom_col(width = .7) +
  scale_fill_manual(values = c(Correct = my_palette$green,
                               Reversed = my_palette$red,
                               Undirected = "#B0B0B0",
                               Missing = "#404040")) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "True edges", fill = NULL) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
gg_orient
save_myplot(gg_orient, file.path(out_dir, "exp2-xpc-noorder-orientation.pdf"),
            width = 2, height = 2, cairo = FALSE)


# Supplement: EP / X-PC over the (tau, alpha) grid ------------------------
# Mean error per setting; QTree and CausEV (no tuning grid) as reference lines.

grid_plot <- function(setting_, method_, metric, ylab) {
  dat <- filter(metrics, setting == setting_)
  ours <- dat %>%
    filter(method == method_) %>%
    group_by(dataset_lab, tau, alpha) %>%
    summarise(value = mean(.data[[metric]]), .groups = "drop")
  refs <- dat %>%
    filter(method %in% c("QTree", "CausEV")) %>%
    group_by(dataset_lab, method) %>%
    summarise(value = mean(.data[[metric]]), .groups = "drop")

  ggplot(ours, aes(x = tau, y = value, color = factor(alpha))) +
    facet_wrap(~ dataset_lab, nrow = 2, scales = "free_y") +
    geom_hline(data = refs, aes(yintercept = value, linetype = method),
               color = "#707070") +
    geom_line() +
    geom_point(shape = 21, fill = "white") +
    labs(x = TeX("$\\tau$"), y = ylab, color = TeX("$\\alpha$"),
         linetype = NULL)
}

gg_grid_order <- grid_plot("order", "Extremal Pruning", "shd",
                           "Mean SHD (Extremal Pruning)")
gg_grid_order
save_myplot(gg_grid_order, file.path(out_dir, "exp2-ep-order-grid.pdf"),
            width = 2.5, height = 2, cairo = FALSE)

gg_grid_noorder <- grid_plot("noorder", "Extremal PC", "skel_shd",
                             "Mean skeleton SHD (Extremal PC)")
gg_grid_noorder
save_myplot(gg_grid_noorder, file.path(out_dir, "exp2-xpc-noorder-grid.pdf"),
            width = 2.5, height = 2, cairo = FALSE)
