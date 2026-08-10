# Download ACS B21002 estimates and MOEs for period of military service.

source(file.path("R", "period_service_helpers.R"))
geographies <- c("us", "state", "county", "place")

for (geography in geographies) {
  raw_data <- fetch_period_service_data(geography, project_config)
  output_path <- file.path(
    "data", "raw",
    paste0("acs_", project_config$acs_year, "_period_service_", geography, ".csv")
  )
  write_csv_safely(raw_data, output_path)
}

log_message("Period-of-service download stage complete.")
