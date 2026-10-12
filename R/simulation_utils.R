prepare_run <- function(run_dir, config_file, data_files, set_params_fn,
                        sim_fn_name, add_timestamp = TRUE) {
  ## str str list-of-str [fn : list -> tibble] str -> str
  ## prepares the running directory
  if (add_timestamp) {
    # timestamp run directory name
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    run_dir_timestamp <- file.path(run_dir, timestamp)
  } else {
    run_dir_timestamp <- run_dir
  }
  ## create run directory
  dir.create(run_dir_timestamp, recursive = TRUE)
  
  ## copy config_file to run directory
  file.copy(config_file, run_dir_timestamp)
  
  ## import config
  source(config_file)
  config <- set_config()
  
  ## instantiate params as tibble
  params <- set_params_fn(config)
  
  ## save params tibble
  readr::write_rds(params,
                   file.path(run_dir_timestamp, "params.rds"))
  
  ## save data_files as other_data.rds
  readr::write_rds(lapply(data_files, readr::read_rds),
                   file.path(run_dir_timestamp, "other_data.rds"))

  ## save metadata about the run
  ## (git_commit identifies the repository: leave meta.rds out of anonymized
  ## material; git_dirty flags uncommitted changes at preparation time)
  git <- function(cmd) {
    out <- tryCatch(suppressWarnings(system(paste("git", cmd), intern = TRUE,
                                            ignore.stderr = TRUE)),
                    error = function(e) character(0))
    if (length(out) == 0) NA_character_ else out
  }
  readr::write_rds(list(sim_fn = sim_fn_name, config_file = config_file,
                        n_tasks = nrow(params),
                        git_commit = git("rev-parse HEAD")[1],
                        git_dirty = !all(is.na(git("status --porcelain"))),
                        created = Sys.time()),
                   file.path(run_dir_timestamp, "meta.rds"))
  
  # return run directory
  return(run_dir_timestamp)
  
}


execute_task <- function(run_dir, task_id, sim_fn){
  ## str int function list of str -> NULL
  ## execute task_id with sim_fn
  ## data_files: named list of files relative to the project root
  ## task_id: row number in the saved parameter table
  
  param_file <- file.path(run_dir, "params.rds") # str
  data_file <- file.path(run_dir, "other_data.rds") # str
  res_file <- file.path(run_dir, sprintf("res_%04d.rds", task_id)) # str
  
  task_params <- readr::read_rds(param_file)[task_id, ] # tibble
  other_data <- readr::read_rds(data_file) # list-of-tibbles
  
  rngtools::setRNG(task_params$rng[[1]])
  
  res <- sim_fn(task_params, other_data) # tibble
  
  readr::write_rds(res, res_file)
  
  invisible(NULL)
}


missing_array_spec <- function(run_dir) {
  ## str -> str
  ## Slurm --array spec of the tasks without a result file, e.g. "1-5,9,12-900"
  n <- nrow(readr::read_rds(file.path(run_dir, "params.rds")))
  ids <- seq_len(n)
  ids <- ids[!file.exists(file.path(run_dir, sprintf("res_%04d.rds", ids)))]
  if (length(ids) == 0) return("")
  breaks <- c(0, which(diff(ids) != 1), length(ids))
  from <- ids[breaks[-length(breaks)] + 1]
  to <- ids[breaks[-1]]
  paste(ifelse(from == to, from, paste0(from, "-", to)), collapse = ",")
}


collect_run <- function(run_dir) {
  ## str -> list
  ## binds the `results` tibbles of all finished tasks (adding task_id) and
  ## keeps every other element of the task outputs as a list-column of `extras`
  params <- readr::read_rds(file.path(run_dir, "params.rds"))
  res_files <- file.path(run_dir, sprintf("res_%04d.rds", params$task_id))
  done <- file.exists(res_files)
  if (!all(done)) {
    warning(sum(!done), " of ", length(done), " tasks have no result yet: ",
            paste(utils::head(params$task_id[!done], 20), collapse = ", "),
            call. = FALSE)
  }

  loaded <- lapply(res_files[done], readr::read_rds)
  ids <- params$task_id[done]

  results <- purrr::map2_dfr(loaded, ids, function(res, id) {
    dplyr::mutate(res$results, task_id = id)
  })

  extras <- tibble::tibble(task_id = ids)
  for (nm in setdiff(unique(unlist(lapply(loaded, names))), "results")) {
    extras[[nm]] <- lapply(loaded, `[[`, nm)
  }

  list(results = results, extras = extras, params = params,
       missing = params$task_id[!done])
}
