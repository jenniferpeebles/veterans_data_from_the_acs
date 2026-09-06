# Derive labor-force rates and verify B21005 table identities.

prepared <- list()
identity_checks <- list()
for (geography in c("us", "state", "county", "place")) {
  raw_data <- read_csv_geoid(file.path("data", "raw",
    paste0("acs_", project_config$acs_year, "_labor_force_", geography, ".csv")))
  prepared[[geography]] <- prepare_labor_force_data(raw_data, geography, project_config)
  identity_checks[[geography]] <- qa_labor_force_identities(raw_data, geography)
}
all_labor <- dplyr::bind_rows(prepared)
all_identities <- dplyr::bind_rows(identity_checks)
qa_summary <- all_labor |>
  dplyr::group_by(.data$geography, .data$age_stratum, .data$labor_metric) |>
  dplyr::summarise(
    records = dplyr::n(), missing_records = sum(.data$flag_missing),
    nonpositive_denominator_records = sum(.data$flag_nonpositive_denominator),
    impossible_records = sum(.data$flag_impossible), high_cv_records = sum(.data$flag_high_cv),
    rank_eligible_records = sum(.data$rank_eligible),
    significant_gap_records = sum(.data$rate_gap_significant_90, na.rm = TRUE), .groups = "drop"
  )
qa_exceptions <- all_labor |>
  dplyr::filter(.data$flag_missing | .data$flag_nonpositive_denominator | .data$flag_impossible | .data$flag_high_cv)
if (any(!all_identities$identity_holds)) {
  write_csv_safely(dplyr::filter(all_identities, !.data$identity_holds),
    file.path("outputs", "qa", "labor_force_identity_failures.csv"))
  stop("QA failed: one or more B21005 parent/child identities do not hold.", call. = FALSE)
}
if (any(qa_summary$impossible_records > 0)) stop("QA failed: a labor-force numerator exceeds its denominator.", call. = FALSE)

dictionary <- tidyr::crossing(
  age_stratum = names(labor_force_strata()),
  labor_metric = names(labor_force_metric_definitions)
) |>
  dplyr::mutate(
    age_label = vapply(.data$age_stratum, function(x) labor_force_strata()[[x]]$label, character(1)),
    metric_label = vapply(.data$labor_metric, function(x) labor_force_metric_definitions[[x]]$label, character(1)),
    denominator = dplyr::if_else(.data$labor_metric == "unemployment_rate", "civilian labor force", "civilian population age 18 to 64"),
    moe_method = "Composite MOEs via tidycensus::moe_sum; rate MOEs via tidycensus::moe_prop; gap MOE by root-sum-square"
  )
write_csv_safely(all_labor, file.path("data", "processed", paste0("acs_", project_config$acs_year, "_labor_force.csv")))
write_csv_safely(all_identities, file.path("outputs", "qa", "labor_force_identity_check.csv"))
write_csv_safely(qa_summary, file.path("outputs", "qa", "labor_force_qa_summary.csv"))
write_csv_safely(qa_exceptions, file.path("outputs", "qa", "labor_force_qa_exceptions.csv"))
write_csv_safely(dictionary, file.path("outputs", "data", "labor_force_metric_dictionary.csv"))
log_message("Labor-force preparation and QA stage complete.")
