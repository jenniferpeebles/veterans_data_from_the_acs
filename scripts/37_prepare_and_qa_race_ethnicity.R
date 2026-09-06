# Prepare C21001A-I metrics, uncertainty, and table-identity QA.

prepared <- list()
checks <- list()
overall <- readr::read_csv(file.path("data", "processed",
  paste0("acs_", project_config$acs_year, "_total_veterans.csv")),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE)
for (geography in c("us", "state", "county", "place")) {
  raw <- read_csv_geoid(file.path("data", "raw",
    paste0("acs_", project_config$acs_year, "_race_ethnicity_", geography, ".csv")))
  totals <- overall |> dplyr::filter(.data$geography == geography)
  prepared[[geography]] <- prepare_race_ethnicity_data(raw, totals, geography, project_config)
  checks[[geography]] <- qa_race_ethnicity_identities(raw, geography)
}
all_groups <- dplyr::bind_rows(prepared)
all_checks <- dplyr::bind_rows(checks)
race_sum_checks <- all_groups |>
  dplyr::filter(.data$classification == "mutually_exclusive_race") |>
  dplyr::group_by(.data$geography, .data$geoid, .data$name) |>
  dplyr::summarise(race_population_sum = sum(.data$group_population_estimate),
    race_veteran_sum = sum(.data$group_veterans_estimate),
    overall_veterans = dplyr::first(.data$veterans_estimate), .groups = "drop") |>
  dplyr::left_join(overall |>
      dplyr::select("geography", "geoid", overall_population = "population_18_plus_estimate"),
    by = c("geography", "geoid")) |>
  dplyr::mutate(population_identity_holds = .data$race_population_sum == .data$overall_population,
    veteran_identity_holds = .data$race_veteran_sum == .data$overall_veterans)

if (any(!all_checks$identity_holds)) stop("QA failed: a C21001A-I table identity does not hold.", call. = FALSE)
if (any(!race_sum_checks$population_identity_holds | !race_sum_checks$veteran_identity_holds))
  stop("QA failed: mutually exclusive A-G race cells do not reconcile to B21001.", call. = FALSE)
if (any(all_groups$flag_impossible, na.rm = TRUE)) stop("QA failed: a group veteran estimate exceeds its population.", call. = FALSE)

qa_summary <- all_groups |>
  dplyr::group_by(.data$geography, .data$group_code, .data$group_label) |>
  dplyr::summarise(records = dplyr::n(), missing_records = sum(.data$flag_missing),
    high_cv_records = sum(.data$flag_high_cv, na.rm = TRUE),
    rank_eligible_records = sum(.data$rank_eligible), .groups = "drop")
dictionary <- race_ethnicity_groups |>
  dplyr::mutate(prevalence_denominator = "Civilian population age 18 and older within the named group",
    composition_denominator = "All civilian veterans age 18 and older",
    overlap_note = dplyr::if_else(.data$classification == "mutually_exclusive_race",
      "A-G are mutually exclusive race categories and sum to the all-race total.",
      "H and I overlap race categories and must not be added to A-G or to one another."),
    moe_method = "Component MOE via tidycensus::moe_sum; proportion MOE via tidycensus::moe_prop")
write_csv_safely(all_groups, file.path("data", "processed", paste0("acs_", project_config$acs_year, "_race_ethnicity.csv")))
write_csv_safely(all_checks, file.path("outputs", "qa", "race_ethnicity_identity_check.csv"))
write_csv_safely(race_sum_checks, file.path("outputs", "qa", "race_ethnicity_race_sum_check.csv"))
write_csv_safely(qa_summary, file.path("outputs", "qa", "race_ethnicity_qa_summary.csv"))
write_csv_safely(dictionary, file.path("outputs", "data", "race_ethnicity_metric_dictionary.csv"))
log_message("Race and ethnicity preparation and QA stage complete.")
