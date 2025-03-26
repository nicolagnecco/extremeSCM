# Dependency imports
source("main/load_packages.R")

# Constants
foldername <- "output/pc-alg/"
pdfname <- "pcalg-pruning-results.pdf"

# PC alg
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

gg0 <- ggplot(res %>% filter(alpha == 0.01)) +
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
  ylab("Structural Hamming Distance") +
  theme(legend.position = c(0.5, 1), legend.direction = "horizontal") 


gg1 <- gg0 +
  theme(legend.position = "none")

gg_legend <- get_legend(
  # create some space to the left of the legend
  gg0+
    guides(color = guide_legend(nrow = 1))
)



# Pruning
rdsname <- "output/random-dags/extremal-pruning-seed=42-2025-03-06_12-47-04.rds"

res_pruning <- read_rds(rdsname) %>%
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

gg2 <- ggplot(res_pruning) +
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
  ylab("") +
  theme(legend.position = "none", legend.direction = "horizontal")
gg2

gg3 <- ggpubr::ggarrange(gg1, gg2, nrow = 1, 
                  align = "hv",
                  heights = c(1),
                  legend = "bottom", legend.grob = gg_legend)


save_myplot(gg3, plt_nm = here(glue(foldername, pdfname)), 
            height = 4.75, width = 8.5, cairo = FALSE)
