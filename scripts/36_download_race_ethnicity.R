# Download ACS C21001A-I race and ethnicity estimates and MOEs.

source(file.path("R", "race_ethnicity_helpers.R"))
for (geography in c("us", "state", "county", "place")) {
  raw_data <- fetch_race_ethnicity_data(geography, project_config)
  write_csv_safely(raw_data, file.path("data", "raw",
    paste0("acs_", project_config$acs_year, "_race_ethnicity_", geography, ".csv")))
}
log_message("Race and ethnicity download stage complete.")
