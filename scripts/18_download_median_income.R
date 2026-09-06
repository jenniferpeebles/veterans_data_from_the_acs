# Download ACS B21004 median-income estimates and MOEs.

source(file.path("R", "median_income_helpers.R"))
geographies <- c("us", "state", "county", "place")

for (geography in geographies) {
  raw_data <- fetch_median_income_data(geography, project_config)
  write_csv_safely(
    raw_data,
    file.path("data", "raw", paste0("acs_", project_config$acs_year, "_median_income_", geography, ".csv"))
  )
}
log_message("Median-income download stage complete.")
