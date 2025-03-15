# Dependency imports
source("main/load_packages.R")
figure_dest_folder <- "output/examples/"

# ggsave size = Saving 4 x 4 in image

# Constants
low_x <- 0
hi_x <- 20
low_y <- 0
hi_y <- 20

# Generate SCM
n <- 500
p <- 2
cov_e <- 1 * diag(rep(1, p)) # diag(runif(p))
xi <- 0
t <- 15
q <- 1
q_noise <- 5

# Gaussian
set.seed(42)
X1 <- rnorm(n)
X2 <- q * X1 + q_noise * rnorm(n)

X1_ <- qexp(pnorm(X1))
X2_ <- qexp(pnorm(X2, sd = sqrt(q_noise^2 + q^2)))


X <- tibble(X1_, X2_, setting = "obs") 

X1_ <- t + xi
X1 <- qnorm(pexp(X1_))
X2 <- q * X1 + q_noise * rnorm(n)
X2_ <- qexp(pnorm(X2, sd = sqrt(q_noise^2 + q^2)))

X <- X %>% 
  bind_rows(tibble(X1_, X2_, setting = "int")) 

X <- X %>% 
  mutate(setting = factor(setting, levels = (c("obs", "int"))))

X$setting

plts <- myggscatterhist(
  X, x = "X1_", y = "X2_",
  color = "setting", size = 1, alpha = 0.15, 
  fill = "setting",
  hist_alpha=.6,
  margin.plot = "hist",
  margin.plot.size = 2,
  palette = rev(c(my_palette$blue, "black")),
  binwidth = .5,
  margin.params = list(fill = "setting", color = "black", size = 0.1),
  ggtheme = theme_bw() +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank(),
      legend.background = element_blank(),
      strip.background = element_rect(fill = "white"),
      plot.caption = element_text(size = 7.5, hjust = 0),
      text = element_text(size = 11),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25)
    )
)


# Remove x-axis histogram and legend
plts$sp <- plts$sp + theme(legend.position = "none")  +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab(TeX("$X_1^{*}$")) +
  ylab(TeX("$X_2^{*}$")) +
  coord_cartesian(ylim = c(0, hi_y), xlim = c(0, hi_x))

empty_plot <- ggplot() +
  theme_void() +  # Use a void theme (no axes, grids, or background)
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "lines"),  # Zero margins
    plot.background = element_blank()  # Transparent background
  )

plts$xplot <- empty_plot

plts$yplot <- plts$yplot + coord_flip(xlim = c(0, hi_x))

print(plts, margin.plot.size = 2)

ggsave(paste0(figure_dest_folder, "example-9.pdf"), device = cairo_pdf, 
       family = "Arial")
knitr::plot_crop(paste0(figure_dest_folder, "example-9.pdf"))


# Generate exponential SCM
n <- 500
p <- 2
cov_e <- 1 * diag(rep(1, p)) # diag(runif(p))
xi <- 0
t <- 15
sigma <- 1

# Exponential
set.seed(42)
X1 <- rexp(n)
X2 <-  X1 + sigma * rnorm(n)

X1_ <- X1
X2_test <- X2 - sigma^2 / 2
X2_ <- qexp(pexGAUS(X2, mu = 0, sigma = sigma, nu = 1))


X <- tibble(X1_, X2_, setting = "obs")

X1_ <- t + xi
X1 <- X1_
X2 <- X1 + sigma * rnorm(n)
X2_test <- X2 - sigma^2 / 2
X2_ <- qexp(pexGAUS(X2, mu = 0, sigma = sigma, nu = 1))

X <- X %>% 
  bind_rows(tibble(X1_, X2_, setting = "int"))

X <- X %>% 
  mutate(setting = factor(setting, levels = (c("obs", "int"))))

X$setting

plts <- myggscatterhist(
  X, x = "X1_", y = "X2_",
  color = "setting", size = 1, alpha = 0.15, 
  fill = "setting",
  hist_alpha=.6,
  margin.plot = "hist",
  margin.plot.size = 2,
  palette = rev(c(my_palette$blue, "black")),
  binwidth = .5,
  margin.params = list(fill = "setting", color = "black", size = 0.1),
  ggtheme = theme_bw() +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank(),
      legend.background = element_blank(),
      strip.background = element_rect(fill = "white"),
      plot.caption = element_text(size = 7.5, hjust = 0),
      text = element_text(size = 11),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25)
    )
)

# Remove x-axis histogram and legend
plts$sp <- plts$sp + theme(legend.position = "none")  +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab(TeX("$X_1^{*}$")) +
  ylab(TeX("$X_2^{*}$")) +
  coord_cartesian(ylim = c(0, hi_y), xlim = c(0, hi_x))

empty_plot <- ggplot() +
  theme_void() +  # Use a void theme (no axes, grids, or background)
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "lines"),  # Zero margins
    plot.background = element_blank()  # Transparent background
  )

plts$xplot <- empty_plot

plts$yplot <- plts$yplot + coord_flip(xlim = c(0, hi_x))

print(plts, margin.plot.size = 2)

ggsave(paste0(figure_dest_folder, "example-10.pdf"), device = cairo_pdf, 
       family = "Arial")
knitr::plot_crop(paste0(figure_dest_folder, "example-10.pdf"))

