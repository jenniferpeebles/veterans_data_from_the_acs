# Derive overlapping period composites and verify B21002 internal consistency.

adult_population <- readr::read_csv(
  file.path("data", "processed", paste0("acs_", project_config$acs_year, "_total_veterans.csv")),
  col_types = readr::cols(geoid = readr::col_character()),
  show_col_types = FALSE
)

geographies <- c("us", "state", "county", "place")
prepared_periods <- list()
component_checks <- list()

for (geography in geographies) {
  input_path <- file.path(
    "data", "raw",
    paste0("acs_", project_config$acs_year, "_period_service_", geography, ".csv")
  )
  raw_data <- read_csv_geoid(input_path)
  adult_subset <- adult_population |>
    dplyr::filter(.data$geography == geography)

  prepared_periods[[geography]] <- prepare_period_service_data(
    raw_data, geography, adult_subset, project_config
  )
  component_checks[[geography]] <- qa_period_components(raw_data, geography)
}

all_periods <- dplyr::bind_rows(prepared_periods)
all_component_checks <- dplyr::bind_rows(component_checks)

qa_summary <- all_periods |>
  dplyr::group_by(.data$geography, .data$period_metric, .data$period_label) |>
  dplyr::summarise(
    records = dplyr::n(),
    missing_records = sum(.data$flag_missing),
    impossible_records = sum(.data$flag_impossible),
    zero_estimate_records = sum(.data$flag_zero_estimate),
    high_cv_records = sum(.data$flag_high_cv),
    rank_eligible_records = sum(.data$rank_eligible),
    .groups = "drop"
  )

qa_exceptions <- all_periods |>
  dplyr::filter(
    .data$flag_missing | .data$flag_impossible |
      .data$flag_zero_estimate | .data$flag_high_cv
  )

composite_dictionary <- data.frame(
  period_metric = names(period_service_definitions),
  period_label = unname(period_service_labels[names(period_service_definitions)]),
  metric_type = "overlapping_any_service_composite",
  component_variables = vapply(
    period_service_definitions,
    function(components) paste(unname(period_service_variables[components]), collapse = ";"),
    character(1)
  ),
  primary_denominator = "B21002_001: civilian veterans age 18 and older",
  secondary_denominator = "B21001_001: civilian population age 18 and older",
  moe_method = "Component-sum MOE via tidycensus::moe_sum; proportion MOE via tidycensus::moe_prop",
  overlap_warning = "A veteran may appear in more than one composite; do not add composites together.",
  stringsAsFactors = FALSE
)

if (any(!all_component_checks$components_match_total)) {
  write_csv_safely(
    all_component_checks |>
      dplyr::filter(!.data$components_match_total),
    file.path("outputs", "qa", "period_service_component_failures.csv")
  )
  stop("QA failed: B21002 components do not sum to the published total.", call. = FALSE)
}

if (any(qa_summary$impossible_records > 0)) {
  stop("QA failed: a period composite exceeds the veteran total.", call. = FALSE)
}

write_csv_safely(
  all_periods,
  file.path("data", "processed", paste0("acs_", project_config$acs_year, "_period_service.csv"))
)
write_csv_safely(
  all_component_checks,
  file.path("outputs", "qa", "period_service_component_check.csv")
)
write_csv_safely(qa_summary, file.path("outputs", "qa", "period_service_qa_summary.csv"))
write_csv_safely(qa_exceptions, file.path("outputs", "qa", "period_service_qa_exceptions.csv"))
write_csv_safely(
  composite_dictionary,
  file.path("outputs", "data", "period_service_composite_dictionary.csv")
)

print(qa_summary)
log_message("Period-of-service preparation and QA stage complete.")
