# Produce deterministic race and ethnicity findings and module brief.

focus <- readr::read_csv(file.path("outputs", "data", "focus_state_race_ethnicity.csv"),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE)
national <- readr::read_csv(file.path("data", "processed",
  paste0("acs_", project_config$acs_year, "_race_ethnicity.csv")), show_col_types = FALSE) |>
  dplyr::filter(.data$geography == "us") |>
  dplyr::select("group_code", national_prevalence = "veteran_prevalence",
    national_prevalence_moe = "veteran_prevalence_moe")
comparison <- focus |>
  dplyr::left_join(national, by = "group_code") |>
  dplyr::mutate(difference = .data$veteran_prevalence - .data$national_prevalence,
    difference_moe = sqrt(.data$veteran_prevalence_moe^2 + .data$national_prevalence_moe^2),
    significant = abs(.data$difference) > .data$difference_moe)
states_50 <- readr::read_csv(file.path("outputs", "data", "states_race_ethnicity_ranked.csv"),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE) |>
  dplyr::filter(!.data$name %in% c("District of Columbia", "Puerto Rico")) |>
  dplyr::group_by(.data$group_code) |>
  dplyr::mutate(prevalence_rank_50 = dplyr::if_else(.data$rank_eligible,
    dplyr::min_rank(dplyr::desc(dplyr::if_else(.data$rank_eligible, .data$veteran_prevalence, NA_real_))), NA_integer_)) |>
  dplyr::ungroup()
ranks <- states_50 |> dplyr::filter(.data$name == project_config$state_name) |>
  dplyr::select("group_code", "prevalence_rank_50")
comparison <- dplyr::left_join(comparison, ranks, by = "group_code")
format_rate <- function(x) scales::percent(x, accuracy = 0.1)
estimate_sentences <- paste0(comparison$group_label, ": ", scales::comma(comparison$group_veterans_estimate, accuracy = 1),
  " veterans, or ", format_rate(comparison$veteran_prevalence), " of the group's civilian population age 18+",
  " (90% MOE +/-", format_rate(comparison$veteran_prevalence_moe), "); the group represented ",
  format_rate(comparison$veteran_composition), " of all Georgia veterans.")
comparison_sentences <- paste0(comparison$group_label, " veteran prevalence was ",
  format_rate(comparison$veteran_prevalence), " in Georgia versus ", format_rate(comparison$national_prevalence),
  " nationally; the ", scales::number(abs(comparison$difference) * 100, accuracy = 0.1),
  "-percentage-point difference was ", ifelse(comparison$significant, "statistically significant", "not statistically significant"),
  " at 90% confidence (difference MOE +/-", scales::number(comparison$difference_moe * 100, accuracy = 0.1), " points)",
  ifelse(is.na(comparison$prevalence_rank_50), ". Georgia was not rank-eligible under the CV rule.",
    paste0(". Georgia ranked No. ", comparison$prevalence_rank_50, " among the 50 states.")))
findings <- dplyr::bind_rows(
  dplyr::tibble(finding_id = paste0("race_ethnicity_estimate_", comparison$group_code), module = "race_ethnicity",
    section = "race_ethnicity_estimates", display_order = 1000L + seq_len(nrow(comparison)),
    sentence = estimate_sentences, estimate = comparison$veteran_prevalence_percent,
    moe = comparison$veteran_prevalence_percent_moe, unit = "percentage_points", geography = "state", geoid = comparison$geoid),
  dplyr::tibble(finding_id = paste0("race_ethnicity_comparison_", comparison$group_code), module = "race_ethnicity",
    section = "race_ethnicity_comparison", display_order = 1020L + seq_len(nrow(comparison)),
    sentence = comparison_sentences, estimate = comparison$difference * 100,
    moe = comparison$difference_moe * 100, unit = "percentage_points", geography = "state", geoid = comparison$geoid))
caveats <- c(
  "Prevalence means veterans as a share of each named group's civilian population age 18 and older; composition means the group's share of all veterans.",
  "C21001A-G race categories are mutually exclusive and exhaustive. White alone, not Hispanic or Latino (H) and Hispanic or Latino (I) overlap those race categories and must not be added to them or to one another.",
  "Hispanic or Latino is an ethnicity and may be of any race.",
  "A rank is descriptive and does not imply that states are statistically distinguishable from one another."
)
brief_lines <- c("# Veteran Race and Ethnicity Reporter Brief", "",
  paste0("Data vintage: ", project_config$acs_year - 4L, "-", project_config$acs_year, " ACS five-year estimates"), "",
  "## Georgia estimates", "", paste0("- ", estimate_sentences), "",
  "## Georgia compared with the nation", "", paste0("- ", comparison_sentences), "",
  "## Caveats / don't-overstate notes", "", paste0("- ", caveats))
write_lines_deterministically(brief_lines, file.path("outputs", "reports", "race_ethnicity_reporter_brief.md"))
write_findings_csv(findings, file.path("outputs", "reports", "race_ethnicity_findings.csv"))
log_message("Wrote deterministic race and ethnicity brief and findings table.")
