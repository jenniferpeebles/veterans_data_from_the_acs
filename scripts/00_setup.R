# Validate the environment and create the project folders.

source(file.path("R", "helpers.R"))
source(file.path("R", "reporter_brief_helpers.R"))
assert_project_root()
source(file.path("config", "project_config.R"))

assert_packages_installed()
ensure_project_directories()
assert_census_api_key()

options(
  tigris_use_cache = TRUE,
  scipen = 999,
  digits = 3,
  stringsAsFactors = FALSE
)

log_message(
  "Setup complete. ACS year: ", project_config$acs_year,
  "; survey: ", project_config$acs_survey,
  "; focus state: ", project_config$state_name
)
