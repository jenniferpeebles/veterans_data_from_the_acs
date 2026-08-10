# =========================================================
# PROJECT SETTINGS -- EDIT THESE FIRST
# =========================================================

# Keep reporter-controlled settings here, in tracked code. Only secrets such as
# CENSUS_API_KEY belong in .Renviron.
project_config <- list(
  acs_year = 2024L,
  acs_survey = "acs5",
  state_name = "Georgia",
  state_abbreviation = "GA",
  place_min_population_18_plus = 100L,
  ranking_top_n = 10L,
  high_cv_threshold = 0.30,
  moe_confidence_level = 90L,
  output_watermark = "NOT FOR PUBLICATION"
)
