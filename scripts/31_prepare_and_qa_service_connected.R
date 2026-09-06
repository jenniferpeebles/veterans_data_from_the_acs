# Prepare B21100 rating prevalence/distribution and verify identities.

prepared <- list()
identity_checks <- list()
for (geography in c("us", "state", "county", "place")) {
  raw_data <- read_csv_geoid(file.path("data", "raw",
    paste0("acs_", project_config$acs_year, "_service_connected_", geography, ".csv")))
  prepared[[geography]] <- prepare_service_connected_data(raw_data, geography, project_config)
  identity_checks[[geography]] <- qa_service_connected_identities(raw_data, geography)
}
all_ratings <- dplyr::bind_rows(prepared)
all_identities <- dplyr::bind_rows(identity_checks)
qa_summary <- all_ratings |>
  dplyr::group_by(.data$geography, .data$rating_metric, .data$rating_label) |>
  dplyr::summarise(
    records = dplyr::n(), missing_records = sum(.data$flag_missing),
    impossible_records = sum(.data$flag_impossible), zero_estimate_records = sum(.data$flag_zero_estimate),
    high_cv_records = sum(.data$flag_high_cv), rank_eligible_records = sum(.data$rank_eligible), .groups = "drop"
  )
qa_exceptions <- all_ratings |>
  dplyr::filter(.data$flag_missing | .data$flag_nonpositive_denominator |
    .data$flag_impossible | .data$flag_zero_estimate | .data$flag_high_cv)
if (any(!all_identities$identity_holds)) {
  write_csv_safely(dplyr::filter(all_identities, !.data$identity_holds),
    file.path("outputs", "qa", "service_connected_identity_failures.csv"))
  stop("QA failed: one or more B21100 identities do not hold.", call. = FALSE)
}
if (any(qa_summary$impossible_records > 0)) stop("QA failed: a B21100 estimate exceeds its denominator.", call. = FALSE)

dictionary <- data.frame(
  rating_metric = c("any_rating", names(service_connected_rating_labels)),
  rating_label = c("Any service-connected disability rating", unname(service_connected_rating_labels)),
  primary_denominator = c("All civilian veterans age 18 and older", rep("Veterans with a service-connected disability rating", length(service_connected_rating_labels))),
  secondary_denominator = "All civilian veterans age 18 and older",
  moe_method = "Proportion MOE via tidycensus::moe_prop",
  stringsAsFactors = FALSE
)
write_csv_safely(all_ratings, file.path("data", "processed", paste0("acs_", project_config$acs_year, "_service_connected.csv")))
write_csv_safely(all_identities, file.path("outputs", "qa", "service_connected_identity_check.csv"))
write_csv_safely(qa_summary, file.path("outputs", "qa", "service_connected_qa_summary.csv"))
write_csv_safely(qa_exceptions, file.path("outputs", "qa", "service_connected_qa_exceptions.csv"))
write_csv_safely(dictionary, file.path("outputs", "data", "service_connected_metric_dictionary.csv"))
log_message("Service-connected rating preparation and QA stage complete.")
