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
  readr::write_rds(list(sim_fn = sim_fn_name, config_file = config_file,
                        git_commit = system("git rev-parse HEAD", intern = TRUE),
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
