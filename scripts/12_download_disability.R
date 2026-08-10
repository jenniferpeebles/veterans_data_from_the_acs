# Download ACS C21007 estimates and MOEs for disability status.

source(file.path("R", "disability_helpers.R"))
geographies <- c("us", "state", "county", "place")

for (geography in geographies) {
  raw_data <- fetch_disability_data(geography, project_config)
  write_csv_safely(
    raw_data,
    file.path("data", "raw", paste0("acs_", project_config$acs_year, "_disability_", geography, ".csv"))
  )
}
log_message("Disability download stage complete.")
