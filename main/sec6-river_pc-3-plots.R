# Plots for X-PC, Exp 1 on the full Danube data: PC with the extremal CI test
# vs PC with PCM vs PC with HSIC (run "river-pc"). Same layout as
# sec6-river_experiment-3-plots_shd.R (EP, Exp 1), so the two can be compared.
# Run from the project root after main/collect_run.R.

source("main/load_packages.R")

run_dir <- "output/river-pc/20261013_122138"
alpha_plot <- 0.05                  # headline level; others for the supplement
metric_plot <- "skel_shd"           # one of names(metric_labels)


short_lab <- function(x) {
  x <- sub("^Extremal (Pruning|PC), tau = ", "tau == ", x)
  x <- sub(", all data$", "", x)
  parse(text = x)
}

# Metrics -----------------------------------------------------------------
# PC returns CPDAGs. On a chain the true CPDAG is the undirected chain, so:
#   skel_*   adjacency errors (extra / missing / total)
#   cpdag_shd SHD to the true CPDAG: skeleton errors plus edges that PC
#             oriented although the true CPDAG leaves them undirected

loaded <- read_rds(file.path(run_dir, "results.rds"))

chain_metrics <- function(A) {
  d <- nrow(A)
  A_true <- matrix(0, d, d)
  A_true[cbind(seq_len(d - 1L), 2:d)] <- 1
  cpdag_true <- A_true + t(A_true)                      # undirected chain
  S_true <- cpdag_true > 0
  S_est <- (A + t(A)) > 0
  skel_fp <- sum(S_est & !S_true) / 2
  skel_fn <- sum(!S_est & S_true) / 2
  tibble(
    d = d,
    n_edges = sum(S_est) / 2,
    skel_fp = skel_fp,
    skel_fn = skel_fn,
    skel_shd = skel_fp + skel_fn,
    n_oriented = sum(A == 1 & t(A) == 0),
    cpdag_shd = SID::hammingDist(cpdag_true, unname(A))
  )
}

res <- loaded$graphs %>%
  mutate(m = purrr::map(adjacency, chain_metrics)) %>%
  dplyr::select(-adjacency) %>%
  tidyr::unnest(m)

write_rds(res, file.path(run_dir, "metrics.rds"))

metric_labels <- c(skel_shd = "Skeleton SHD", skel_fp = "Extra edges",
                   skel_fn = "Missing edges", cpdag_shd = "CPDAG SHD",
                   n_oriented = "Oriented edges")
method_levels <- c("Extremal PC", "PCM", "HSIC")
method_colors <- c(my_palette$blue, "#707070", "#B0B0B0")

res <- res %>%
  mutate(method = factor(method, levels = method_levels),
         dataset_lab = paste0(dataset, " (d = ", d, ")"))


# Summary table -------------------------------------------------------------

summary_tbl <- res %>%
  group_by(alpha, dataset, d, method, tau) %>%
  summarise(k = mean(k), across(c(skel_shd, skel_fp, skel_fn, cpdag_shd,
                                  n_oriented), mean),
            .groups = "drop")
print(filter(summary_tbl, alpha == alpha_plot), n = Inf, width = Inf)
write_csv(summary_tbl, file.path(run_dir, "summary.csv"))


# Plot 1: extremal test on exceedances vs PCM / HSIC on all data ----------
# Extremal PC at each tau; PCM and HSIC at tau = 0 (all observations of the
# subsample): the generous control for the non-extremal tests.

dat_all <- res %>%
  filter(alpha == alpha_plot,
         (method == "Extremal PC") | (method != "Extremal PC" & tau == 0)) %>%
  mutate(method = if_else(method == "HSIC", "dHSIC", method)) %>% 
  mutate(label = if_else(method == "Extremal PC",
                         paste0("Extremal PC, tau = ", tau),
                         paste0(method, ", all data")))

tau_lab <- function(t) sprintf("%s, tau = %s", levels(res$method)[1], t)
col_pal <- c("#D55E00","#0072B2", "#009E73")
dat_all <- dat_all %>%
  mutate(label = factor(label, levels = c(tau_lab(sort(unique(tau[tau > 0]))),
                                          paste0(c("PCM", "dHSIC"), ", all data"))))


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
save_myplot(gg_all, file.path(run_dir, paste0("_river-pc-alldata-alpha=", alpha_plot,
                                              "-", metric_plot, ".pdf")),
            width = 2, height = 2, cairo = FALSE)


# Plot 2: sample-matched comparison, one panel per (dataset, tau) ---------
# PCM and HSIC use a random subsample of the same size k as the exceedances.
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
  scale_x_discrete(labels = c(setNames(if (ext == "Extremal PC") "Extr. PC" else "EP", ext),
                              PCM = "PCM", HSIC = "dHSIC")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7),
        legend.position = "none")
gg_matched
save_myplot(gg_matched, file.path(run_dir, paste0("_river-pc-matched-alpha=", alpha_plot,
                                                  "-", metric_plot, ".pdf")),
            width = 1.5, height = 1.5, cairo = FALSE)


# Plot 3: sample-matched comparison, one panel per dataset at fixed tau=0.975 ---------
# PCM and HSIC use a random subsample of the same size k as the exceedances.
dat_main <- dat_matched %>% filter(dplyr::near(tau, 0.975)) %>% 
  mutate(value = .data[[metric_plot]] / (d-1))

gg_main <- ggplot(dat_main, aes(x = method, y = value,
                                color = method, fill = method)) +
  facet_wrap(~ dataset_lab, nrow = 2) +
  geom_hline(aes(yintercept = 1), linetype = "dashed",
             color = "#707070", linewidth = .5) +
  geom_jitter(position = position_jitter(width = .2, height = 0, seed = 1),
              size = .5, alpha = .3, show.legend = FALSE) +
  geom_boxplot(alpha = .25, linewidth = .5, width = .7, outlier.shape = NA) +
  scale_color_manual(values = c(col_pal[2], "#707070", "#B0B0B0")) +
  scale_fill_manual(values = c(col_pal[2], "#707070", "#B0B0B0")) +
  scale_x_discrete(labels = c("Extremal PC" = "Extr. PC", PCM = "PCM", HSIC = "dHSIC")) +
  scale_y_continuous(breaks = pretty(c(0, 2), 12)) +
  # scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)), limits = c(0, NA)) +
  labs(y = metric_labels[[metric_plot]]) +
  theme(axis.title.x = element_blank(), 
        text = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        legend.position = "none") +
  labs(y = "Normalized skeleton SHD")
gg_main
save_myplot(gg_main, file.path(run_dir, "_river-pc-main-tau0.975.pdf"),
            width = 1, height = 1, cairo = FALSE)

