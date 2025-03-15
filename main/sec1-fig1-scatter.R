# Dependency imports
source("main/load_packages.R")
figure_dest_folder <- "output/examples/"

# Generate SCM
n <- 1000
p <- 4
q <- .5
cov_e <- 1 * diag(rep(1, p))
t <- 15
my_seed <- 42

# MAXIMUM
set.seed(my_seed)
X1 <- rexp(n) * (2 * rbinom(n, size = 1, prob = .5) - 1)
X2 <- 1 * X1 + sqrt(cov_e[2, 2]) * rnorm(n)
X3 <- 1 * X1 + sqrt(cov_e[3, 3]) * rnorm(n)
X4 <- 1/2 * (X2 + X3 + sqrt((X2 - X3)^2 + 36 / (1 + (X3)^2 + X2^2))) + sqrt(cov_e[4, 4]) * rnorm(n)

X <- tibble(X1, X2, X3, X4, setting = "obs")

X1 <- rexp(n) * (2 * rbinom(n, size = 1, prob = .5) - 1)
X2 <- 1 * X1 + sqrt(cov_e[2, 2]) * rnorm(n)
X3 <- t
X4 <- 1/2 * (X2 + X3 + sqrt((X2 - X3)^2 + 36 / (1 + (X3)^2 + X2^2))) + sqrt(cov_e[4, 4]) * rnorm(n)


X <- X %>% 
  bind_rows(tibble(X1, X2, X3, X4, setting = "int"))


g1 <- ggplot(dat = X) +
  geom_point(aes(x = X3, y = X4, col = setting), alpha = .2) +
  scale_color_manual(values = c(my_palette$blue, "black")) +
  theme(
    legend.position = "none"
  ) +
  labs(colour="Setting:") +
  annotate(
    geom="text", x=t-2.5, y=-15,  # Move x further to place outside
    label=TeX('do$(X_3 = 15)$'), 
    color = my_palette$blue, hjust = 0
  ) +
  coord_cartesian(xlim = c(-10, 20), ylim=c(-10, 20), clip="off") +
  geom_vline(xintercept = t, color = my_palette$blue, linetype = "dotted") +
  theme(
    panel.border = element_rect(linewidth = 1 / 8)
  ) +
  xlab(TeX("$X_3$")) +
  ylab(TeX("$X_4$"))

g1




# LINEAR 
q <- 0
set.seed(my_seed)
X1 <- rexp(n) * (2 * rbinom(n, size = 1, prob = .5) - 1)
X2 <- 1 * X1 + sqrt(cov_e[2, 2]) * rnorm(n)
X3 <- 1 * X1 + sqrt(cov_e[3, 3]) * rnorm(n)
X4 <- (1 - q) * X2 + q * X3 + 3 / (1 + (X3)^2 + X2^2) + sqrt(cov_e[4, 4]) * rnorm(n)

X <- tibble(X1, X2, X3, X4, setting = "obs")

X1 <- rexp(n) * (2 * rbinom(n, size = 1, prob = .5) - 1)
X2 <- 1 * X1 + sqrt(cov_e[2, 2]) * rnorm(n)
X3 <- t
X4 <- (1 - q) * X2 + q * X3 + 3 / (1 + (X3)^2 + X2^2) + sqrt(cov_e[4, 4]) * rnorm(n)


X <- X %>% 
  bind_rows(tibble(X1, X2, X3, X4, setting = "int"))

g2 <- ggplot(dat = X) +
  geom_point(aes(x = X3, y = X4, col = setting), alpha = .2) +
  scale_color_manual(values = c(my_palette$blue, "black")) +
  theme(
    legend.position = "none"
  ) +
  labs(colour="Setting:") +
  annotate(
    geom="text", x=t - 2.5, y=-15,  # Move x further to place outside
    label=TeX('do$(X_3 = 15)$'), 
    color = my_palette$blue, hjust = 0
  ) +
  coord_cartesian(xlim = c(-10, 20), ylim=c(-10, 20), clip="off") +
  geom_vline(xintercept = t, color = my_palette$blue, linetype = "dotted") +
  theme(
    panel.border = element_rect(linewidth = 1 / 8)
  ) +
  xlab(TeX("$X_3$")) +
  ylab(TeX("$X_4$"))

g2


save_myplot(
  plt = g1,
  plt_nm = paste0(figure_dest_folder, "scatter-x3-x4-max", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)

save_myplot(
  plt = g2,
  plt_nm = paste0(figure_dest_folder, "scatter-x3-x4-lin", ".pdf"),
  width = 3,
  height = 3,
  cairo = FALSE
)
