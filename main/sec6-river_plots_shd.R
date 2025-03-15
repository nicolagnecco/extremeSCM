source("main/load_packages.R")

# Load results
filename <- "output/river/20250306-163004-B=100-declustered=FALSE-simulated=FALSE-mode=subsample.rds"
plotname <- "output/river/rivers.pdf"

loaded_data <- read_rds(filename)
str_datetime <- str_extract(filename, "\\d{8}-\\d{6}")
graphs <- loaded_data$results


# evaluate graphs
mylist <- graphs %>%
  group_by(dataset, alpha, log_transform, bootstrap_rep) %>%
  group_split() 


res <- map_dfr(mylist, function(tbl){
  
  # tbl <- mylist[[1]]
  
  # compute true graph
  g_true <- tbl %>% filter(method == "true") %>% 
    dplyr::select(from, to) %>% 
    distinct() %>% 
    graph_from_data_frame(directed = TRUE) %>% 
    as_adjacency_matrix(sparse = FALSE)
  
  # get all nodes
  all_nodes <- unique(rownames(g_true))
  
  methods_k <- tbl %>% filter(method != "true") %>% 
    group_by(method, tau) %>% 
    group_split()
  
  map_dfr(methods_k, function(m){
    # m <- methods_k[[4]]
    # compute graph
    g_estimated <- m %>% dplyr::select(from, to) %>% 
      distinct() %>% 
      graph_from_data_frame(directed = TRUE, vertices = all_nodes) %>% 
      as_adjacency_matrix(sparse = FALSE)
    
    # compute shd
    tibble(
      method = unique(m$method),
      dataset = unique(m$dataset),
      alpha = unique(m$alpha),
      tau = unique(m$tau),
      log_transform = unique(m$log_transform),
      bootstrap_rep = unique(m$bootstrap_rep),
      shd = SID::hammingDist(g_true, g_estimated),
      shd_normalized = SID::hammingDist(g_true, g_estimated) / (ncol(g_true) - 1),
      fp = sum((g_estimated == 1) & (g_true == 0)),
      fn = sum((g_estimated == 0) & (g_true == 1)),
      rev = sum((g_true == 1) & (t(g_estimated) == 1) & (g_estimated == 0))
    )
  })
})

taus <- (res %>% filter(method == "Extremal Pruning") %>% 
           dplyr::select(tau) %>% unique())$tau %>% 
  rev()



dat2plot3 <- res %>% 
  filter(if_else(method %in% c("HSIC", "PCM"), 
                 is.na(tau),
                 if_else(method %in% c("Extremal Pruning"),
                         tau %in% c(0.9, .95, .975), FALSE))) %>% 
  mutate(tau = factor(tau, levels = taus)) %>% 
  filter(alpha == 0.05) %>% 
  mutate(alpha = texify_column(alpha, "\\alpha")) %>% 
  mutate(method = if_else(method == "Extremal Pruning",
                          paste0("Extremal Pruning - tau = ", tau),
                          method))



dat2plot4 <- dat2plot3 %>% 
  pivot_longer(cols = c("shd", "fp", "fn", "shd_normalized"), 
               names_to = "metric_name", 
               values_to = "metric") %>% 
  mutate(metric_name = factor(
    metric_name, 
    levels = c("shd", "fp", "fn", "shd_normalized"),
    labels = c("SHD", "False Positives", "False Negatives", "SHD Normalized"))) %>% 
  filter(metric_name == "SHD") 



labels_legend <- unname(TeX(c(
  "Extremal Pruning - $\\tau = 0.9$",
  "Extremal Pruning - $\\tau = 0.95$",
  "Extremal Pruning - $\\tau = 0.975$",
  "dHSIC", 
  "PCM")))

gg <- ggplot(dat2plot4 %>% filter(log_transform==TRUE), 
             aes(x = dataset, y = metric, color = method, shape = method)) +
  facet_wrap(~dataset, nrow = 2, scale="free") +
  geom_boxplot(alpha = .5, outlier.shape = TRUE, size = .65,
               fatten = 1.25,
               position = position_dodge(width =0.95)) +
  stat_summary(fun = median, geom = "point",
               size = 2, color = "#707070",
               position = position_dodge(width = 0.95)) +
  labs(x = "Dataset",
       y = "Structural Hamming Distance",
       color = "Method")


gg_colors <- gg +
  scale_color_manual(
    values = c(my_palette$blue,  # Extremal Pruning
               my_palette$red,  # Extremal Pruning
               my_palette$green,  # Extremal Pruning
               "#707070",  # HSIC 
               "#707070"),
    labels = labels_legend) + # PCM
  scale_shape_manual(values = c("HSIC" = 17,  # Triangle
                                "PCM" = 15,  # Square
                                "Extremal Pruning - tau = 0.9" = NA,
                                "Extremal Pruning - tau = 0.95" = NA,
                                "Extremal Pruning - tau = 0.975" = NA),
                     labels = labels_legend) 


gg_final <- gg_colors +
  guides(color = guide_legend(title = "Method"), 
         shape = guide_legend(title = "Method")) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)), 
                     limits = c(0, NA)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

gg_final

save_myplot(gg_final, plotname, width=2, height=2, cairo = FALSE)
