# Download ACS B21100 service-connected disability rating estimates and MOEs.

source(file.path("R", "service_connected_helpers.R"))
for (geography in c("us", "state", "county", "place")) {
  raw_data <- fetch_service_connected_data(geography, project_config)
  write_csv_safely(raw_data, file.path("data", "raw",
    paste0("acs_", project_config$acs_year, "_service_connected_", geography, ".csv")))
}
log_message("Service-connected rating download stage complete.")
