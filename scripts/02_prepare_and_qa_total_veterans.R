# Preserve uncertainty, create analysis fields, and run QA before findings.

geographies <- c("us", "state", "county", "place")
prepared_data <- list()

for (geography in geographies) {
  input_path <- file.path(
    "data", "raw",
    paste0("acs_", project_config$acs_year, "_total_veterans_", geography, ".csv")
  )
  raw_data <- read_csv_geoid(input_path)
  prepared_data[[geography]] <- prepare_total_veteran_data(raw_data, geography, project_config)
}

all_prepared <- dplyr::bind_rows(prepared_data)

qa_summary <- all_prepared |>
  dplyr::group_by(.data$geography) |>
  dplyr::summarise(
    records = dplyr::n(),
    unique_geoids = dplyr::n_distinct(.data$geoid),
    duplicate_geoids = sum(duplicated(.data$geoid)),
    missing_records = sum(.data$flag_missing),
    nonpositive_denominators = sum(.data$flag_nonpositive_denominator),
    impossible_estimates = sum(.data$flag_impossible_estimate),
    zero_estimates = sum(.data$flag_zero_estimate),
    high_cv_records = sum(.data$flag_high_cv),
    rank_eligible_records = sum(.data$rank_eligible),
    min_population_18_plus = min(.data$population_18_plus_estimate, na.rm = TRUE),
    max_population_18_plus = max(.data$population_18_plus_estimate, na.rm = TRUE),
    .groups = "drop"
  )

qa_exceptions <- all_prepared |>
  dplyr::filter(
    .data$flag_missing |
      .data$flag_nonpositive_denominator |
      .data$flag_impossible_estimate |
      .data$flag_zero_estimate |
      .data$flag_high_cv
  )

data_dictionary <- data.frame(
  field = c(
    "geography", "geoid", "name",
    "population_18_plus_estimate", "population_18_plus_moe",
    "veterans_estimate", "veterans_moe",
    "veteran_share", "veteran_share_moe", "veteran_estimate_cv",
    "veteran_percent", "veteran_percent_moe",
    "veteran_percent_ci_lower", "veteran_percent_ci_upper",
    "veteran_share_se", "veteran_share_cv", "veteran_share_relative_moe",
    "flag_missing", "flag_nonpositive_denominator",
    "flag_impossible_estimate", "flag_zero_estimate", "flag_high_cv", "reliability_class",
    "rank_eligible"
  ),
  definition = c(
    "ACS summary geography level.",
    "Census geographic identifier, preserved as character data.",
    "Census geography name.",
    "ACS estimate for the civilian population age 18 and older (B21001_001).",
    "ACS margin of error for population_18_plus_estimate at the configured confidence level.",
    "ACS estimate for veterans in the civilian population age 18 and older (B21001_002).",
    "ACS margin of error for veterans_estimate at the configured confidence level.",
    "veterans_estimate divided by population_18_plus_estimate.",
    "Derived proportion MOE calculated with tidycensus::moe_prop().",
    "Standard error of veterans_estimate divided by veterans_estimate.",
    "veteran_share expressed on a 0-to-100 percentage scale.",
    "veteran_share_moe expressed in percentage points.",
    "Lower confidence bound for veteran_percent, bounded at zero for presentation.",
    "Upper confidence bound for veteran_percent, bounded at 100 for presentation.",
    "Standard error of veteran_share, derived from its MOE and confidence level.",
    "Standard error of veteran_share divided by veteran_share.",
    "veteran_share_moe divided by veteran_share.",
    "TRUE when an estimate or MOE required for analysis is missing.",
    "TRUE when the population denominator is zero or negative.",
    "TRUE when veterans_estimate is negative or exceeds its population denominator.",
    "TRUE when veterans_estimate is zero; retained but excluded from ranking because its CV is undefined.",
    "TRUE when veteran_share_cv exceeds the configured high-CV threshold.",
    "Project reliability category based on veteran_share_cv; not a Census Bureau classification.",
    "TRUE when the record passes all conditions required for descriptive ranking."
  ),
  units = c(
    "category", "identifier", "text",
    "people", "people", "people", "people",
    "proportion", "proportion", "ratio",
    "percentage points", "percentage points",
    "percentage points", "percentage points",
    "proportion", "ratio", "ratio",
    rep("logical", 5), "category", "logical"
  ),
  stringsAsFactors = FALSE
)

source_metadata <- data.frame(
  item = c(
    "source_agency", "dataset", "acs_year", "acs_period",
    "survey", "table", "moe_confidence_level", "retrieval_package",
    "api_documentation", "retrieved_at"
  ),
  value = c(
    "U.S. Census Bureau",
    "American Community Survey",
    as.character(project_config$acs_year),
    paste0(project_config$acs_year - 4L, "-", project_config$acs_year),
    project_config$acs_survey,
    "B21001: Sex by age by veteran status for the civilian population 18 years and over",
    paste0(project_config$moe_confidence_level, "%"),
    paste0("tidycensus ", as.character(utils::packageVersion("tidycensus"))),
    "https://www.census.gov/data/developers/data-sets/acs-5year.html",
    pipeline_timestamp()
  ),
  stringsAsFactors = FALSE
)

if (any(qa_summary$duplicate_geoids > 0)) {
  stop("QA failed: duplicate GEOIDs found within at least one geography.", call. = FALSE)
}

if (any(qa_summary$impossible_estimates > 0)) {
  stop("QA failed: impossible veteran estimates found.", call. = FALSE)
}

write_csv_safely(
  all_prepared,
  file.path("data", "processed", paste0("acs_", project_config$acs_year, "_total_veterans.csv"))
)
write_csv_safely(qa_summary, file.path("outputs", "qa", "total_veterans_qa_summary.csv"))
write_csv_safely(qa_exceptions, file.path("outputs", "qa", "total_veterans_qa_exceptions.csv"))
write_csv_safely(data_dictionary, file.path("outputs", "data", "total_veterans_data_dictionary.csv"))
write_csv_safely(source_metadata, file.path("outputs", "data", "source_metadata.csv"))

print(qa_summary)
log_message("Preparation and QA stage complete.")
