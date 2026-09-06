# Prepare B21004 veteran/nonveteran medians, gaps, MOEs, and QA flags.

geographies <- c("us", "state", "county", "place")
prepared <- list()
for (geography in geographies) {
  raw_data <- read_csv_geoid(file.path(
    "data", "raw", paste0("acs_", project_config$acs_year, "_median_income_", geography, ".csv")
  ))
  prepared[[geography]] <- prepare_median_income_data(raw_data, geography, project_config)
}
all_income <- dplyr::bind_rows(prepared)
qa_summary <- all_income |>
  dplyr::group_by(.data$geography, .data$income_stratum, .data$income_stratum_label) |>
  dplyr::summarise(
    records = dplyr::n(), missing_records = sum(.data$flag_missing),
    nonpositive_records = sum(.data$flag_nonpositive), invalid_moe_records = sum(.data$flag_invalid_moe),
    high_cv_records = sum(.data$flag_high_cv), rank_eligible_records = sum(.data$rank_eligible),
    significant_gap_records = sum(.data$income_gap_significant_90, na.rm = TRUE), .groups = "drop"
  )
qa_exceptions <- all_income |>
  dplyr::filter(.data$flag_missing | .data$flag_nonpositive | .data$flag_invalid_moe | .data$flag_high_cv)

if (any(qa_summary$invalid_moe_records > 0)) {
  stop("QA failed: B21004 contains a negative published MOE.", call. = FALSE)
}

dictionary <- data.frame(
  income_stratum = names(median_income_strata),
  income_stratum_label = vapply(median_income_strata, `[[`, character(1), "label"),
  veteran_variable = vapply(median_income_strata, function(x) unname(median_income_variables[[x$veteran]]), character(1)),
  nonveteran_variable = vapply(median_income_strata, function(x) unname(median_income_variables[[x$nonveteran]]), character(1)),
  gap_moe_method = "Square root of the sum of squared published 90% MOEs",
  dollar_vintage = paste0(project_config$acs_year, " inflation-adjusted dollars"),
  stringsAsFactors = FALSE
)

write_csv_safely(all_income, file.path("data", "processed", paste0("acs_", project_config$acs_year, "_median_income.csv")))
write_csv_safely(qa_summary, file.path("outputs", "qa", "median_income_qa_summary.csv"))
write_csv_safely(qa_exceptions, file.path("outputs", "qa", "median_income_qa_exceptions.csv"))
write_csv_safely(dictionary, file.path("outputs", "data", "median_income_metric_dictionary.csv"))
log_message("Median-income preparation and QA stage complete.")
