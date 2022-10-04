suppress <- function(lib, s = T) {
  if (s) {
    suppressMessages(suppressWarnings(lib))
  }
}

create_dir <- function(path_to_dir) {
  if (!dir.exists(path_to_dir)) {
    dir.create(path_to_dir)
  }
}

init_log <- function(logging_dir, today, time) {
  suppress(library(log4r))
  create_dir(logging_dir)
  log <- create.logger()
  logfile(log) <- paste0(logging_dir, "_", today, "_", time, ".log")
  level(log) <- 'INFO'
  info(log, 'Created log file. Dir paths set according to the "main.R" script.')
  return(log)
}

load_libraries <- function() {
  tryCatch(
    expr = {
      source("libraries.R")

      msg <- paste0("Successfully loaded all libraries")
      level(log) <- 'INFO'
      info(log, msg)
    },
    error = function(e) {
      msg <- paste0("Failed to load all libraries", " with ", e)
      level(log) <- 'ERROR'
      error(log, msg)
    }
  )
}

jparse <- function(json_path) {
  tryCatch(
    expr = {
      json_data <- jsonlite::read_json(json_path)
      msg <- paste0("Successfully loaded: ", json_path)
      level(log) <- 'INFO'
      info(log, msg)
      return(json_data)
    },
    error = function(e) {
      msg <- paste0("Failed to load: ", json_path, " with ", e)
      level(log) <- 'ERROR'
      error(log, msg)
    }
  )
}

source_functions <- function(funs) {
  for (f in funs) {
    tryCatch(
      expr = {
        source(f)
        msg <- paste0("Successfully sourced: ", f)
        level(log) <- 'INFO'
        info(log, msg)
      },
      error = function(e) {
        msg <- paste0("Failed to source: ", f, " with ", e)
        level(log) <- 'ERROR'
        error(log, msg)
      }
    )
  }
}

convert_null <- function(var) {
  if (var == "null") {
    return(NULL)
  } else {
    return(var)
  }
}

getSQL <- function(filepath){
  con <- file(filepath, "r")
  sql_string <- ""

  while (TRUE){
    line <- readLines(con, n = 1)

    if ( length(line) == 0 ){
      break
    }

    line <- gsub("\\t", " ", line)

    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }

    sql_string <- paste(sql_string, line)
  }

  close(con)
  return(sql_string)
}