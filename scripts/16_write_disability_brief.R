# Produce deterministic disability findings and module brief.

focus <- readr::read_csv(file.path("outputs", "data", "focus_state_disability.csv"), show_col_types = FALSE) |>
  dplyr::filter(.data$disability_metric %in% disability_headline_metrics) |>
  dplyr::mutate(metric_order = match(.data$disability_metric, disability_headline_metrics)) |>
  dplyr::arrange(.data$metric_order)
top_counties <- readr::read_csv(
  file.path("outputs", "data", "focus_state_counties_disability_top5.csv"),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE
)
state_rankings <- readr::read_csv(
  file.path("outputs", "data", "states_disability_ranked.csv"),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE
) |>
  dplyr::filter(
    !.data$name %in% c("District of Columbia", "Puerto Rico"),
    .data$disability_metric == "all_veterans"
  ) |>
  dplyr::mutate(
    disabled_veteran_count_rank = dplyr::min_rank(dplyr::desc(.data$disabled_estimate)),
    disability_prevalence_rank_50 = dplyr::min_rank(dplyr::desc(.data$disability_share))
  )
georgia_ranking <- state_rankings |>
  dplyr::filter(.data$name == project_config$state_name)
national_disability <- readr::read_csv(
  file.path("data", "processed", paste0("acs_", project_config$acs_year, "_disability.csv")),
  show_col_types = FALSE
) |>
  dplyr::filter(.data$geography == "us", .data$disability_metric == "all_veterans")

state_national_difference <- georgia_ranking$disability_percent - national_disability$disability_percent
state_national_difference_moe <- sqrt(
  georgia_ranking$disability_percent_moe^2 + national_disability$disability_percent_moe^2
)
state_national_significant <- abs(state_national_difference) > state_national_difference_moe
county_examples <- top_counties |>
  dplyr::mutate(metric_order = match(.data$disability_metric, disability_headline_metrics)) |>
  dplyr::arrange(.data$metric_order, dplyr::desc(.data$disability_share), .data$geoid) |>
  dplyr::group_by(.data$disability_metric) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup()

state_sentences <- paste0(
  focus$disability_label, ": ", scales::comma(focus$disabled_estimate, accuracy = 1),
  " people with a disability; ", scales::percent(focus$disability_share, accuracy = 0.1),
  " (90% MOE +/-", scales::percent(focus$disability_share_moe, accuracy = 0.1), ")."
)
county_sentences <- paste0(
  county_examples$disability_label, ": ", county_examples$name,
  " had the highest reliable Georgia county point estimate at ",
  scales::percent(county_examples$disability_share, accuracy = 0.1),
  " (90% MOE +/-", scales::percent(county_examples$disability_share_moe, accuracy = 0.1), ")."
)
ranking_sentence <- paste0(
  project_config$state_name, " ranked No. ", georgia_ranking$disability_prevalence_rank_50,
  " among the 50 states by veteran disability prevalence and No. ",
  georgia_ranking$disabled_veteran_count_rank,
  " by the estimated number of veterans with a disability."
)
national_sentence <- paste0(
  project_config$state_name, "'s veteran disability prevalence was ",
  scales::percent(georgia_ranking$disability_share, accuracy = 0.1),
  " versus ", scales::percent(national_disability$disability_share, accuracy = 0.1),
  " nationally. The ", scales::number(abs(state_national_difference), accuracy = 0.1),
  "-percentage-point difference was ",
  if (state_national_significant) "statistically significant" else "not statistically significant",
  " at the 90% confidence level (difference MOE +/-",
  scales::number(state_national_difference_moe, accuracy = 0.1), " points)."
)

findings <- dplyr::bind_rows(
  dplyr::tibble(
    finding_id = paste0("disability_state_", focus$disability_metric), module = "disability",
    section = "disability_estimates", display_order = 500L + focus$metric_order,
    sentence = state_sentences, estimate = focus$disability_percent,
    moe = focus$disability_percent_moe, unit = "percentage_points",
    geography = "state", geoid = as.character(focus$geoid)
  ),
  dplyr::tibble(
    finding_id = paste0("disability_county_", county_examples$disability_metric), module = "disability",
    section = "disability_county_examples", display_order = 600L + county_examples$metric_order,
    sentence = county_sentences, estimate = county_examples$disability_percent,
    moe = county_examples$disability_percent_moe, unit = "percentage_points",
    geography = "county", geoid = as.character(county_examples$geoid)
  ),
  dplyr::tibble(
    finding_id = c("disability_state_rank", "disability_national_comparison"),
    module = "disability", section = "disability_state_comparison",
    display_order = c(550L, 551L), sentence = c(ranking_sentence, national_sentence),
    estimate = c(
      georgia_ranking$disability_prevalence_rank_50,
      state_national_difference
    ),
    moe = c(NA_real_, state_national_difference_moe),
    unit = c("rank_among_50_states", "percentage_points"),
    geography = "state", geoid = as.character(georgia_ranking$geoid)
  )
)

caveats <- c(
  "C21007 covers the civilian population age 18 and older for whom poverty status is determined.",
  "Disability is self-reported ACS disability status; it is not the same as a service-connected disability rating.",
  "Poverty-stratified results are conditional prevalence estimates, not shares of all veterans.",
  "County examples are reliable point-estimate rankings, not tests of statistical difference.",
  "No missing or suppressed values were imputed."
)
brief_lines <- c(
  "# Veteran Disability Reporter Brief", "",
  paste0("Data vintage: ", project_config$acs_year - 4L, "-", project_config$acs_year, " ACS five-year estimates"), "",
  "## Georgia estimates", "", paste0("- ", state_sentences), "",
  "## Where Georgia stacks up", "", paste0("- ", c(ranking_sentence, national_sentence)), "",
  "## County examples", "", paste0("- ", county_sentences), "",
  "## Caveats / don't-overstate notes", "", paste0("- ", caveats)
)
write_lines_deterministically(brief_lines, file.path("outputs", "reports", "disability_reporter_brief.md"))
write_findings_csv(findings, file.path("outputs", "reports", "disability_findings.csv"))
log_message("Wrote deterministic disability brief and findings table.")
