# Produce deterministic labor-force findings and module brief.

focus <- readr::read_csv(file.path("outputs", "data", "focus_state_labor_force.csv"), show_col_types = FALSE)
overall <- focus |>
  dplyr::filter(.data$age_stratum == "all_18_64") |>
  dplyr::mutate(metric_order = match(.data$labor_metric, names(labor_force_metric_definitions))) |>
  dplyr::arrange(.data$metric_order)
age_unemployment <- focus |>
  dplyr::filter(.data$age_stratum != "all_18_64", .data$labor_metric == "unemployment_rate") |>
  dplyr::mutate(age_order = match(.data$age_stratum, names(labor_force_age_definitions))) |>
  dplyr::arrange(.data$age_order)
states_50 <- readr::read_csv(
  file.path("outputs", "data", "states_labor_force_ranked.csv"),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE
) |>
  dplyr::filter(!.data$name %in% c("District of Columbia", "Puerto Rico"), .data$age_stratum == "all_18_64") |>
  dplyr::group_by(.data$labor_metric) |>
  dplyr::mutate(veteran_rate_rank_50 = dplyr::min_rank(dplyr::desc(.data$veteran_rate))) |>
  dplyr::ungroup()
georgia_ranks <- states_50 |> dplyr::filter(.data$name == project_config$state_name)
national <- readr::read_csv(
  file.path("data", "processed", paste0("acs_", project_config$acs_year, "_labor_force.csv")),
  show_col_types = FALSE
) |>
  dplyr::filter(.data$geography == "us", .data$age_stratum == "all_18_64")

format_rate <- function(x) scales::percent(x, accuracy = 0.1)
overall_sentences <- paste0(
  overall$labor_metric_label, ": veterans ", format_rate(overall$veteran_rate),
  " (90% MOE +/-", format_rate(overall$veteran_rate_moe), "); nonveterans ",
  format_rate(overall$nonveteran_rate), " (90% MOE +/-", format_rate(overall$nonveteran_rate_moe),
  "); veteran-minus-nonveteran gap ", scales::number(overall$rate_gap_percentage_points, accuracy = 0.1),
  " points (90% MOE +/-", scales::number(overall$rate_gap_moe_percentage_points, accuracy = 0.1), "), ",
  ifelse(overall$rate_gap_significant_90, "statistically significant", "not statistically significant"), "."
)
age_sentences <- paste0(
  age_unemployment$age_label, " veteran unemployment: ", format_rate(age_unemployment$veteran_rate),
  " (90% MOE +/-", format_rate(age_unemployment$veteran_rate_moe), "); nonveterans ",
  format_rate(age_unemployment$nonveteran_rate), " (90% MOE +/-", format_rate(age_unemployment$nonveteran_rate_moe), ")."
)

comparison_rows <- dplyr::left_join(
  georgia_ranks |>
    dplyr::select("labor_metric", "labor_metric_label", "geoid", "veteran_rate", "veteran_rate_moe", "veteran_rate_rank_50"),
  national |>
    dplyr::select("labor_metric", national_rate = "veteran_rate", national_rate_moe = "veteran_rate_moe"),
  by = "labor_metric"
) |>
  dplyr::mutate(
    national_difference = .data$veteran_rate - .data$national_rate,
    national_difference_moe = sqrt(.data$veteran_rate_moe^2 + .data$national_rate_moe^2),
    national_significant = abs(.data$national_difference) > .data$national_difference_moe,
    metric_order = match(.data$labor_metric, names(labor_force_metric_definitions))
  ) |>
  dplyr::arrange(.data$metric_order)
comparison_sentences <- paste0(
  comparison_rows$labor_metric_label, ": Georgia ranked No. ", comparison_rows$veteran_rate_rank_50,
  " among the 50 states. Georgia's veteran rate was ", format_rate(comparison_rows$veteran_rate),
  " versus ", format_rate(comparison_rows$national_rate), " nationally; the difference was ",
  ifelse(comparison_rows$national_significant, "statistically significant", "not statistically significant"),
  " at 90% confidence (difference MOE +/-",
  scales::number(comparison_rows$national_difference_moe * 100, accuracy = 0.1), " points)."
)

findings <- dplyr::bind_rows(
  dplyr::tibble(
    finding_id = paste0("labor_overall_", overall$labor_metric), module = "labor_force",
    section = "labor_force_estimates", display_order = 800L + overall$metric_order,
    sentence = overall_sentences, estimate = overall$rate_gap_percentage_points,
    moe = overall$rate_gap_moe_percentage_points, unit = "percentage_points",
    geography = "state", geoid = as.character(overall$geoid)
  ),
  dplyr::tibble(
    finding_id = paste0("labor_age_unemployment_", age_unemployment$age_stratum), module = "labor_force",
    section = "labor_force_age_detail", display_order = 820L + age_unemployment$age_order,
    sentence = age_sentences, estimate = age_unemployment$veteran_percent,
    moe = age_unemployment$veteran_percent_moe, unit = "percentage_points",
    geography = "state", geoid = as.character(age_unemployment$geoid)
  ),
  dplyr::tibble(
    finding_id = paste0("labor_state_comparison_", comparison_rows$labor_metric), module = "labor_force",
    section = "labor_force_comparison", display_order = 850L + comparison_rows$metric_order,
    sentence = comparison_sentences, estimate = comparison_rows$national_difference * 100,
    moe = comparison_rows$national_difference_moe * 100, unit = "percentage_points",
    geography = "state", geoid = as.character(comparison_rows$geoid)
  )
)

caveats <- c(
  "B21005 covers the civilian population age 18 to 64; it excludes veterans age 65 and older.",
  "The unemployment rate denominator is the civilian labor force, not the total population.",
  "People not in the labor force are neither employed nor unemployed and must not be counted as unemployed.",
  "Employment-to-population and labor-force participation use the civilian population as denominator.",
  "State ranks run from the highest rate to the lowest; a high unemployment-rate rank is not a favorable outcome.",
  "These cross-sectional associations do not establish that veteran status caused a labor-market outcome."
)
brief_lines <- c(
  "# Labor-Force Status Reporter Brief", "",
  paste0("Data vintage: ", project_config$acs_year - 4L, "-", project_config$acs_year, " ACS five-year estimates"), "",
  "## Georgia ages 18 to 64", "", paste0("- ", overall_sentences), "",
  "## Unemployment by age", "", paste0("- ", age_sentences), "",
  "## Where Georgia stacks up", "", paste0("- ", comparison_sentences), "",
  "## Caveats / don't-overstate notes", "", paste0("- ", caveats)
)
write_lines_deterministically(brief_lines, file.path("outputs", "reports", "labor_force_reporter_brief.md"))
write_findings_csv(findings, file.path("outputs", "reports", "labor_force_findings.csv"))
log_message("Wrote deterministic labor-force brief and findings table.")
