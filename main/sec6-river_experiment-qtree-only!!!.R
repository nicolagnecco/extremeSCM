# Add QTree to a main experiment's saved results, without rerunning pruning.
# Run from the extremeSCM project root. Requires the existing QTree add-on.
run_qtree_only <- function(filename, n_workers = 3L, nrep = 100L) {
  stopifnot(length(filename) == 1L, file.exists(filename),
            length(n_workers) == 1L, is.finite(n_workers),
            n_workers >= 1L, n_workers == as.integer(n_workers),
            length(nrep) == 1L, is.finite(nrep),
            nrep >= 2L, nrep == as.integer(nrep))
  for (package in c("future", "furrr", "dplyr")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Install the R package '", package, "' before running this script.")
    }
  }
  if (!file.exists("R/fit_qtree.R") ||
      !file.exists("python/run_qtree.py")) {
    stop("Run from the project root with the original QTree add-on installed.")
  }
  source("R/fit_qtree.R", local = environment())
  runner <- normalizePath("python/run_qtree.py", mustWork = TRUE)
  python <- Sys.getenv("QTREE_PYTHON", unset = "")
  if (!nzchar(python)) {
    local_python <- file.path(".venv-qtree", "bin", "python")
    python <- if (file.exists(local_python)) local_python else Sys.which("python3")
  }
  if (!nzchar(python) || !file.exists(python)) {
    stop("Python not found. Set QTREE_PYTHON to the interpreter's absolute path.")
  }
  # Keep a virtual environment's interpreter path: resolving its final symlink
  # would launch the base Python and lose access to the virtualenv packages.
  python <- file.path(normalizePath(dirname(python), mustWork = TRUE),
                      basename(python))

  loaded <- readRDS(filename)
  inputs <- loaded$qtree_inputs
  if (is.null(inputs) || length(inputs) == 0L) {
    stop("This RDS has no saved QTree inputs. Use the checkpoint changes in ",
         "sec6-river_experiment-main.R for your next main run. Old graph-only ",
         "results do not record which observations were sampled.")
  }
  if (!is.data.frame(loaded$results) ||
      !"method" %in% names(loaded$results)) stop("Invalid main results file.")
  if (any(loaded$results$method == "QTree", na.rm = TRUE)) {
    stop("The input already contains QTree. Use the original main results file.")
  }
  if (length(loaded$station_info) != length(inputs)) {
    stop("Saved station information does not match the saved experiment inputs.")
  }
  needed <- c("dataset", "bootstrap_rep", "alpha", "log_transform",
              "sample_indices", "dat", "qtree_seed")
  for (i in seq_along(inputs)) {
    input <- inputs[[i]]
    if (!is.list(input) || !all(needed %in% names(input))) {
      stop("Incomplete saved input for experiment ", i)
    }
    stations <- colnames(input$dat)
    info <- loaded$station_info[[i]]
    stopifnot(is.matrix(input$dat), is.numeric(input$dat),
              all(is.finite(input$dat)), ncol(input$dat) >= 2L,
              nrow(input$dat) == length(input$sample_indices),
              !is.null(stations), !anyDuplicated(stations),
              all(c("id_name", "Lat", "Lon") %in% names(info)),
              !anyDuplicated(info$id_name), all(stations %in% info$id_name))
  }

  message("Running QTree only for ", length(inputs), " saved experiments; ",
          nrep, " internal subsamples, ", n_workers, " workers.")
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::multisession, workers = as.integer(n_workers))
  fits <- furrr::future_map(seq_along(inputs), function(i) {
    input <- inputs[[i]]
    info <- loaded$station_info[[i]]
    fit <- fit_qtree(input$dat, nrep = as.integer(nrep),
                     seed = input$qtree_seed, python = python, runner = runner)
    A <- fit$adjacency
    positions <- which(A == 1L, arr.ind = TRUE)
    from <- rownames(A)[positions[, 1L]]
    to <- colnames(A)[positions[, 2L]]
    from_index <- match(from, info$id_name)
    to_index <- match(to, info$id_name)
    edges <- data.frame(
      from = from, to = to,
      Lat_from = info$Lat[from_index], Lon_from = info$Lon[from_index],
      Lat_to = info$Lat[to_index], Lon_to = info$Lon[to_index],
      method = "QTree", tau = NA_real_,
      qtree_q = fit$q, qtree_smallR = fit$smallR,
      qtree_var = fit$var_T, qtree_nrep = as.integer(nrep),
      alpha = input$alpha, log_transform = input$log_transform,
      dataset = input$dataset, bootstrap_rep = input$bootstrap_rep,
      stringsAsFactors = FALSE
    )
    list(edges = edges, fit = fit, dataset = input$dataset,
         bootstrap_rep = input$bootstrap_rep, seed = input$qtree_seed)
  }, .options = furrr::furrr_options(seed = TRUE))

  loaded$results <- dplyr::bind_rows(
    loaded$results, dplyr::bind_rows(lapply(fits, function(x) x$edges)))
  loaded$qtree_runs <- lapply(fits, function(x) {
    x[c("dataset", "bootstrap_rep", "seed", "fit")]
  })
  loaded$qtree_settings <- list(
    nrep = as.integer(nrep), n_workers = as.integer(n_workers),
    python = python, source_file = normalizePath(filename, mustWork = TRUE))
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  output <- file.path(dirname(filename), paste0(
    tools::file_path_sans_ext(basename(filename)),
    "-QTree-m=", nrep, "-", stamp, ".rds"))
  if (file.exists(output)) stop("Output already exists: ", output)
  saveRDS(loaded, output)
  message("Combined results saved to: ", output)
  invisible(output)
}

# Sourcing defines the function; Rscript runs it with positional arguments.
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1L || length(args) > 3L) {
    stop("Usage: Rscript main/sec6-river_experiment-qtree-only.R ",
         "MAIN_RESULTS.rds [WORKERS=3] [NREP=100]")
  }
  run_qtree_only(
    filename = args[[1L]],
    n_workers = if (length(args) >= 2L) as.numeric(args[[2L]]) else 3L,
    nrep = if (length(args) >= 3L) as.numeric(args[[3L]]) else 100L)
}
