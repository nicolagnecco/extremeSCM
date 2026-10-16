# Plots for Exp 2 on the declustered Danube data:
#   order known: Extremal Pruning vs QTree vs CausEV, SHD           (river-bench)
#   no order:    Extremal PC vs QTree vs CausEV, skeleton SHD       (river-bench-noorder)
# Mean error over the subsamples against tau, with a +/- 1 SD band (the
# subsamples overlap, so the SE would understate the spread). QTree and CausEV
# have no tau / alpha grid and appear as horizontal reference lines.
# Run from the project root after main/collect_run.R on both run directories.

source("main/load_packages.R")

run_dirs <- c(
  order   = "output/river-bench/20261013_172238/",
  noorder = "output/river-bench-noorder/20261013_184931/"
)
out_dir <- "output/river-bench-figures"
recompute <- TRUE        # TRUE to recompute metrics.rds in the run directories

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# Metrics (cached as metrics.rds in each run directory) -------------------
# evaluate_bench_run() also recovers graphs without edges.

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

# One metric per setting: with the order known every method returns a DAG
# that respects it, so SHD = extra + missing edges; without the order, the
# skeleton SHD compares CPDAGs and DAGs fairly.
metrics <- metrics %>%
  mutate(value = if_else(setting == "order", shd, skel_shd),
         dataset_lab = paste0(dataset, " (d = ", d, ")"))


# Summary: mean and SD per setting ----------------------------------------

summary_tbl <- metrics %>%
  group_by(setting, dataset, dataset_lab, d, method, tau, alpha) %>%
  summarise(mean = mean(value), sd = sd(value), n = n(), .groups = "drop")
print(summary_tbl, n = Inf, width = Inf)
write_csv(summary_tbl, file.path(out_dir, "exp2-summary.csv"))


# Plot ----------------------------------------------------------------------

alpha_colors <- c("#6BAED6", "#2171B5", "#08306B")

bench_plot <- function(setting_, ours, ylab, ref_suffix = "") {
  dat <- filter(summary_tbl, setting == setting_, 
                (alpha > 0.005) | !grepl("Extremal", method))
    
  ep <- dat %>%
    filter(method == ours) %>%
    mutate(alpha = factor(alpha))
  refs <- dat %>%
    filter(method %in% c("QTree", "CausEV")) %>%
    mutate(method = factor(paste0(method, ref_suffix),
                           levels = paste0(c("QTree", "CausEV"), ref_suffix)))
  
  
  gg0 <- ggplot(ep, aes(x = tau, y = mean, group = alpha)) +
    facet_wrap(~ dataset_lab, nrow = 2, scales = "free_y") +
    geom_hline(data = refs, aes(yintercept = mean, linetype = method),
               color = "#707070", linewidth = .5) +
    geom_ribbon(aes(ymin = pmax(mean - sd, 0), ymax = mean + sd, fill = alpha),
                alpha = .12, color = NA) 
  
  
  gg0 +
    geom_line(aes(color = alpha), linewidth = .5) +
    geom_point(aes(color = alpha, shape = alpha), size = 1.2) +
    scale_color_manual(values = alpha_colors) +
    scale_fill_manual(values = alpha_colors) +
    scale_shape_manual(values = c(16, 17, 15)) +
    scale_linetype_manual(values = c("dashed", "dotted")) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)),
                       limits = c(0, NA)) +
    labs(x = TeX("$\\tau$"), y = ylab,
         color = TeX(paste0(ours, ": $\\alpha$")),
         fill = TeX(paste0(ours, ": $\\alpha$")),
         shape = TeX(paste0(ours, ": $\\alpha$")),
         linetype = NULL) +
    guides(color = guide_legend(order = 1), fill = guide_legend(order = 1),
           shape = guide_legend(order = 1), linetype = guide_legend(order = 2)) +
    theme(legend.position = "bottom", legend.box = "horizontal")
}

gg_order <- bench_plot("order", "Extremal Pruning", "Mean SHD",
                       ref_suffix = " (order given)")
gg_order
save_myplot(gg_order, file.path(out_dir, "exp2-ep-order-shd.pdf"),
            width = 2.25, height = 2.25, cairo = FALSE)

gg_noorder <- bench_plot("noorder", "Extremal PC", "Mean skeleton SHD")
gg_noorder
save_myplot(gg_noorder, file.path(out_dir, "exp2-xpc-noorder-skeleton-shd.pdf"),
            width = 2.25, height = 2.25, cairo = FALSE)

# Boxplot at alpha = 0.05 and tau = 0.8
dat_main <- metrics %>%
  filter(setting == "noorder",
         is.na(tau) | (dplyr::near(tau, 0.8) & dplyr::near(alpha, 0.05))) %>%
  mutate(value_normalized = value / (d - 1)) %>% 
  mutate(method = factor(method, levels = c("Extremal PC", "QTree", "CausEV")))

gg_main <- ggplot(dat_main, aes(x = method, y = value_normalized, 
                                color = method, fill = method)) +
  facet_wrap(~ dataset_lab, nrow = 2) +
  geom_hline(aes(yintercept = 1), linetype = "dashed",
             color = "#707070", linewidth = .4) +
  geom_jitter(position = position_jitter(width = .2, height = 0, seed = 1),
              size = .5, alpha = .3, show.legend = FALSE) +
  geom_boxplot(alpha = .25, linewidth = .5, width = .7, outlier.shape = NA) +
  scale_color_manual(values = c("#2171B5", "#707070", "#B0B0B0")) +
  scale_fill_manual(values = c("#2171B5", "#707070", "#B0B0B0")) +
  scale_x_discrete(labels = c("Extremal PC" = "Extremal PC", QTree = "QTree", CausEV = "CausEV")) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)), limits = c(0, NA)) +
  labs(y = "Normalized skeleton SHD") +
  theme(axis.title.x = element_blank(), legend.position = "none")
gg_main
save_myplot(gg_main, file.path(out_dir, "exp2-xpc-noorder-main.pdf"),
            width = 2, height = 2, cairo = FALSE)
