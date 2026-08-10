# Download the source ACS estimates and margins of error.

source(file.path("R", "acs_helpers.R"))

geographies <- c("us", "state", "county", "place")

for (geography in geographies) {
  raw_data <- fetch_total_veteran_data(geography, project_config)
  output_path <- file.path(
    "data", "raw",
    paste0("acs_", project_config$acs_year, "_total_veterans_", geography, ".csv")
  )
  write_csv_safely(raw_data, output_path)
}

log_message("Download stage complete.")
