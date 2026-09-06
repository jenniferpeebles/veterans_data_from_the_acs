# Download ACS B21005 labor-force estimates and MOEs.

source(file.path("R", "labor_force_helpers.R"))
for (geography in c("us", "state", "county", "place")) {
  raw_data <- fetch_labor_force_data(geography, project_config)
  write_csv_safely(raw_data, file.path("data", "raw",
    paste0("acs_", project_config$acs_year, "_labor_force_", geography, ".csv")))
}
log_message("Labor-force download stage complete.")
