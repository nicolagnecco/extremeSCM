# options setup
my_palette <- list(
    "red" = "#D55E00",
    "blue" = "#0072B2",
    "green" = "#009E73",
    "yellow" = "#E69F00",
    "pink" = "#CC79A7",
    "light_blue" = "#56B4E9",
    "grey" = "#999999",
    "background" = "#FAFAFA"
)

my_palette2 <- list(
    "blue"   = "#00798c",
    "red"    = "#d1495b",
    "green"  = "#a2d729", # "#66a182",
    "yellow" = "#edae49",
    "navy"   = "#2e4057",
    "grey"   = "#8d96a3"
)


my_palette_vibrant <- list(
  "red" = "#FF6F61",        # Warm red (vibrant)
  "blue" = "#0072B2",       # Deep but playful blue
  "green" = "#7BC96F",      # Soft green
  "yellow" = "#EFB700",     # Warm pastel yellow 
  "pink" = "#F4A7B9",       # Soft pink
  "light_blue" = "#76C7C0", # Light turquoise
  "grey" = "#B0B0B0",       # Soft grey for competitors
  "background" = "#FAFAFA"
)

lst_methods <- list(
  "mp" = "'(a) MPD'",
  "ms" = "'(b) Max-stable'",
  "scm" = "'(c) Extremal-SCM'"
)

my_palette_methods <- list(
  c("method" = lst_methods$mp, "color" = my_palette$blue),
  c("method" = lst_methods$ms, "color" = my_palette$red),
  c("method" = lst_methods$scm, "color" = my_palette$green)
) %>%
  purrr::transpose() %>%
  as_tibble() %>%
  unnest(cols = c(method, color)) %>%
  deframe()

theme_set(theme_bw() +
    theme(
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        strip.background = element_rect(fill = "white"),
        plot.caption = element_text(size = 7.5, hjust = 0),
        # margin=margin(t=15)),
        text = element_text(size = 11),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25)
    ))


# function definitions
branded_pal <- function(palette, primary = "blue", other = "grey",
                        direction = 1) {
    ## character_vector character character integer -> function
    ## returns a function that produces a discrete palette with n colors


    function(n) {
        n_max <- length(palette)

        if (!(primary %in% names(palette))) {
            stop(paste0(
                "The primary color ", primary,
                " must exist in the given palette."
            ))
        }

        if (n > n_max) {
            stop(paste0("The palette you supplied only has ", n_max, " colors."))
        }

        if (other %in% names(palette)) {
            other <- palette[other]
        }
        color_list <- c(other, palette[primary])
        remaining_colors <- palette[!(palette %in% color_list)]
        color_list <- c(color_list, remaining_colors)[1:n]

        color_list <- unname(unlist(color_list))

        if (direction >= 0) {
            color_list
        } else {
            rev(color_list)
        }
    }
}


scale_color_branded <- function(palette, primary = "blue", other = "grey",
                                direction = 1, ...) {
    ## character_vector character character integer ... -> ggplot_constructor
    ## wrapper around ggplot2::discrete_scale
    ggplot2::discrete_scale(
        "colour", "branded",
        branded_pal(palette, primary, other, direction),
        ...
    )
}

scale_fill_branded <- function(palette, primary = "blue", other = "grey",
                               direction = 1, ...) {
    ## character_vector character character integer ... -> ggplot_constructor
    ## wrapper around ggplot2::discrete_scale
    ggplot2::discrete_scale(
        "fill", "branded",
        branded_pal(palette, primary, other, direction),
        ...
    )
}


# Adapted from ggforce::label_tex
# Source: ggforce R package (Thomas Lin Pedersen)
# Reference: https://ggforce.data-imaginist.com/reference/label_tex.html
label_tex <- function(labels, ...) {
  lables_tbl_chr <- labels %>% 
    mutate(across(where(is.factor), as.character))
  # A tibble: 6 × 1
  label_parsed(ggforce:::data_frame0(!!!lapply(lables_tbl_chr, latex2exp::TeX, 
                                               output = "character")), ...)
}


texify_column <- function(column, letter) {
    ## vector character -> factor
    ## paste latex formula of the form "$letter = column$"
    factor(column,
        levels = unique(column),
        labels = TeX(paste0(
            "$", letter, " = ",
            unique(column), "$"
        ))
    )
}

refactor_param <- function(param) {
    ## character_vector -> factor
    ## refactor param

    factor(param,
        levels = unique(param),
        labels = TeX(c("$\\hat{\\sigma}(x)$", "$\\hat{\\xi}(x)$"))
    )
}

refactor_methods <- function(methods, lst_method) {
    ## character_vector list -> factor
    ## refactor column with methods

    unique_methods <- unique(methods)

    new_levels <- names(lst_methods)
    new_labels <- lst_methods %>%
        unlist() %>%
        unname()

    factor(methods,
        levels = new_levels,
        labels = new_labels
    )
}

save_myplot <- function(plt, plt_nm,
                        width, height,
                        width_pdf = 50, height_pdf = 50,
                        crop = TRUE, cairo = TRUE) {
    dir_name <- dirname(plt_nm)
    if (!file.exists(dir_name)) {
        dir.create(dir_name)
    }

    if (cairo) {
        ggsave(plt_nm,
            egg::set_panel_size(
                p = plt,
                width = unit(width, "in"),
                height = unit(height, "in")
            ),
            width = width_pdf, height = height_pdf,
            limitsize = FALSE, units = c("in"),
            device = cairo_pdf, family = "Arial"
        )
    } else {
        ggsave(plt_nm,
            egg::set_panel_size(
                p = plt,
                width = unit(width, "in"),
                height = unit(height, "in")
            ),
            width = width_pdf, height = height_pdf,
            limitsize = FALSE, units = c("in")
        )
    }

    if (crop) {
        knitr::plot_crop(plt_nm)
    }
}
