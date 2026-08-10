# Shared, project-local helpers.

required_packages <- c(
  "dplyr",
  "ggplot2",
  "peeblestoolbox",
  "readr",
  "scales",
  "tidycensus",
  "tidyr"
)

assert_project_root <- function() {
  required_file <- file.path("config", "project_config.R")

  if (!file.exists(required_file)) {
    stop(
      "Run the pipeline from the repository root. Missing: ",
      required_file,
      call. = FALSE
    )
  }

  invisible(TRUE)
}

assert_packages_installed <- function(packages = required_packages) {
  missing_packages <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      "Install the following R packages before running the pipeline: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

assert_census_api_key <- function() {
  if (!nzchar(Sys.getenv("CENSUS_API_KEY"))) {
    stop(
      paste(
        "Census API key not found.",
        "Add CENSUS_API_KEY to your user-level .Renviron file,",
        "restart R, and run the pipeline again."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

ensure_project_directories <- function() {
  directories <- c(
    file.path("data", "raw"),
    file.path("data", "processed"),
    file.path("outputs", "charts"),
    file.path("outputs", "data"),
    file.path("outputs", "qa"),
    file.path("outputs", "reports"),
    "logs"
  )

  invisible(vapply(directories, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))
}

pipeline_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
}

log_message <- function(..., log_file = file.path("logs", "pipeline.log")) {
  message_text <- paste0("[", pipeline_timestamp(), "] ", paste(..., collapse = ""))
  message(message_text)
  cat(message_text, "\n", file = log_file, append = TRUE)
  invisible(message_text)
}

write_csv_safely <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, path, na = "")
  log_message("Wrote ", nrow(data), " rows to ", path)
  invisible(path)
}

read_csv_geoid <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(GEOID = readr::col_character()),
    show_col_types = FALSE
  )
}
