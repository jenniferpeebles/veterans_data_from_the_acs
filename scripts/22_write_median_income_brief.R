# Produce deterministic median-income findings and module brief.

focus <- readr::read_csv(
  file.path("outputs", "data", "focus_state_median_income.csv"), show_col_types = FALSE
) |>
  dplyr::mutate(stratum_order = match(.data$income_stratum, names(median_income_strata))) |>
  dplyr::arrange(.data$stratum_order)
states_50 <- readr::read_csv(
  file.path("outputs", "data", "states_median_income_ranked.csv"),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE
) |>
  dplyr::filter(
    !.data$name %in% c("District of Columbia", "Puerto Rico"),
    .data$income_stratum == "total"
  ) |>
  dplyr::mutate(
    veteran_median_rank_50 = dplyr::min_rank(dplyr::desc(.data$veteran_median_income)),
    veteran_income_advantage_rank_50 = dplyr::min_rank(dplyr::desc(.data$income_gap))
  )
georgia_total <- states_50 |> dplyr::filter(.data$name == project_config$state_name)
national_total <- readr::read_csv(
  file.path("data", "processed", paste0("acs_", project_config$acs_year, "_median_income.csv")),
  show_col_types = FALSE
) |>
  dplyr::filter(.data$geography == "us", .data$income_stratum == "total")
national_gap <- georgia_total$veteran_median_income - national_total$veteran_median_income
national_gap_moe <- sqrt(
  georgia_total$veteran_median_income_moe^2 + national_total$veteran_median_income_moe^2
)
national_gap_significant <- abs(national_gap) > national_gap_moe

format_dollars <- function(x) scales::dollar(x, accuracy = 1)
state_sentences <- paste0(
  focus$income_stratum_label, ": veteran median ", format_dollars(focus$veteran_median_income),
  " (90% MOE +/-", format_dollars(focus$veteran_median_income_moe),
  "); nonveteran median ", format_dollars(focus$nonveteran_median_income),
  " (90% MOE +/-", format_dollars(focus$nonveteran_median_income_moe),
  "); veteran-minus-nonveteran gap ", format_dollars(focus$income_gap),
  " (90% MOE +/-", format_dollars(focus$income_gap_moe), "), ",
  ifelse(focus$income_gap_significant_90, "statistically significant", "not statistically significant"), "."
)
ranking_sentence <- paste0(
  project_config$state_name, " ranked No. ", georgia_total$veteran_median_rank_50,
  " among the 50 states by veteran median income and No. ",
  georgia_total$veteran_income_advantage_rank_50,
  " by the veteran-minus-nonveteran median-income gap."
)
national_sentence <- paste0(
  project_config$state_name, "'s veteran median income was ",
  format_dollars(georgia_total$veteran_median_income), " versus ",
  format_dollars(national_total$veteran_median_income), " nationally. The ",
  format_dollars(abs(national_gap)), " difference was ",
  if (national_gap_significant) "statistically significant" else "not statistically significant",
  " at the 90% confidence level (difference MOE +/-", format_dollars(national_gap_moe), ")."
)

findings <- dplyr::bind_rows(
  dplyr::tibble(
    finding_id = paste0("income_state_", focus$income_stratum), module = "median_income",
    section = "median_income_estimates", display_order = 700L + focus$stratum_order,
    sentence = state_sentences, estimate = focus$income_gap, moe = focus$income_gap_moe,
    unit = paste0(project_config$acs_year, "_dollars"), geography = "state",
    geoid = as.character(focus$geoid)
  ),
  dplyr::tibble(
    finding_id = c("income_state_rank", "income_national_comparison"), module = "median_income",
    section = "median_income_comparison", display_order = c(750L, 751L),
    sentence = c(ranking_sentence, national_sentence),
    estimate = c(georgia_total$veteran_median_rank_50, national_gap),
    moe = c(NA_real_, national_gap_moe),
    unit = c("rank_among_50_states", paste0(project_config$acs_year, "_dollars")),
    geography = "state", geoid = as.character(georgia_total$geoid)
  )
)

caveats <- c(
  paste0("Dollar values are medians in ", project_config$acs_year, " inflation-adjusted dollars, not means or household income."),
  "B21004 covers the civilian population age 18 and older with income; people without income are outside the universe.",
  "Male and female results are separate published medians and must not be averaged to reconstruct the total median.",
  "Difference MOEs use the square root of the sum of squared published MOEs.",
  "The estimates describe association by veteran status and do not establish that veteran status caused an income difference."
)
brief_lines <- c(
  "# Median Income by Veteran Status Reporter Brief", "",
  paste0("Data vintage: ", project_config$acs_year - 4L, "-", project_config$acs_year,
    " ACS five-year estimates; ", project_config$acs_year, " inflation-adjusted dollars"), "",
  "## Georgia estimates", "", paste0("- ", state_sentences), "",
  "## Where Georgia stacks up", "", paste0("- ", c(ranking_sentence, national_sentence)), "",
  "## Caveats / don't-overstate notes", "", paste0("- ", caveats)
)
write_lines_deterministically(brief_lines, file.path("outputs", "reports", "median_income_reporter_brief.md"))
write_findings_csv(findings, file.path("outputs", "reports", "median_income_findings.csv"))
log_message("Wrote deterministic median-income brief and findings table.")
