# Derive disability prevalence and verify C21007 table identities.

geographies <- c("us", "state", "county", "place")
prepared <- list()
identity_checks <- list()

for (geography in geographies) {
  raw_data <- read_csv_geoid(file.path(
    "data", "raw", paste0("acs_", project_config$acs_year, "_disability_", geography, ".csv")
  ))
  prepared[[geography]] <- prepare_disability_data(raw_data, geography, project_config)
  identity_checks[[geography]] <- qa_disability_identities(raw_data, geography)
}

all_disability <- dplyr::bind_rows(prepared)
all_identity_checks <- dplyr::bind_rows(identity_checks)
qa_summary <- all_disability |>
  dplyr::group_by(.data$geography, .data$disability_metric, .data$disability_label) |>
  dplyr::summarise(
    records = dplyr::n(),
    missing_records = sum(.data$flag_missing),
    impossible_records = sum(.data$flag_impossible),
    zero_estimate_records = sum(.data$flag_zero_estimate),
    high_cv_records = sum(.data$flag_high_cv),
    rank_eligible_records = sum(.data$rank_eligible),
    .groups = "drop"
  )
qa_exceptions <- all_disability |>
  dplyr::filter(.data$flag_missing | .data$flag_nonpositive_denominator |
    .data$flag_impossible | .data$flag_zero_estimate | .data$flag_high_cv)

if (any(!all_identity_checks$identity_holds)) {
  write_csv_safely(
    dplyr::filter(all_identity_checks, !.data$identity_holds),
    file.path("outputs", "qa", "disability_identity_failures.csv")
  )
  stop("QA failed: one or more C21007 parent/child identities do not hold.", call. = FALSE)
}
if (any(qa_summary$impossible_records > 0)) {
  stop("QA failed: a disability numerator exceeds its denominator.", call. = FALSE)
}

dictionary <- data.frame(
  disability_metric = names(disability_metric_definitions),
  disability_label = vapply(disability_metric_definitions, `[[`, character(1), "label"),
  numerator_variables = vapply(disability_metric_definitions, function(x) paste(unname(disability_variables[x$numerator]), collapse = ";"), character(1)),
  denominator_variables = vapply(disability_metric_definitions, function(x) paste(unname(disability_variables[x$denominator]), collapse = ";"), character(1)),
  moe_method = "Composite MOE via tidycensus::moe_sum; proportion MOE via tidycensus::moe_prop",
  stringsAsFactors = FALSE
)

write_csv_safely(all_disability, file.path("data", "processed", paste0("acs_", project_config$acs_year, "_disability.csv")))
write_csv_safely(all_identity_checks, file.path("outputs", "qa", "disability_identity_check.csv"))
write_csv_safely(qa_summary, file.path("outputs", "qa", "disability_qa_summary.csv"))
write_csv_safely(qa_exceptions, file.path("outputs", "qa", "disability_qa_exceptions.csv"))
write_csv_safely(dictionary, file.path("outputs", "data", "disability_metric_dictionary.csv"))
log_message("Disability preparation and QA stage complete.")
