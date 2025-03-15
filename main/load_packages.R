# Define required packages
cran_packs <- c(
  "pcalg", "dHSIC", "ranger",
  "tidyverse", "here",
  "graph", "MASS",
  "furrr", "future", "progressr",
  "mvtnorm",
  "glue", "ggpubr", "latex2exp", 
  "VGAM", "skewsamp", "rngtools",
  "ggExtra", "igraph", "geosphere", "egg",
  "gamlss.dist"
)

# 🚀 Automatically load all installed packages
invisible(lapply(cran_packs, library, character.only = TRUE))
library(graphicalExtremes)  # Manually load GitHub package

# Load local functions from "R" directory
purrr::walk(list.files(here::here("R"), full.names = TRUE), source)

# Confirmation message
message("✅ Packages and functions loaded successfully!")
