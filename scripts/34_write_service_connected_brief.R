# Produce deterministic service-connected rating findings and module brief.

focus <- readr::read_csv(
  file.path("outputs", "data", "focus_state_service_connected.csv"), show_col_types = FALSE
)
any_rating <- focus |> dplyr::filter(.data$rating_metric == "any_rating")
distribution <- focus |>
  dplyr::filter(.data$rating_metric != "any_rating") |>
  dplyr::mutate(rating_order = match(.data$rating_metric, names(service_connected_rating_labels))) |>
  dplyr::arrange(.data$rating_order)
states_50 <- readr::read_csv(
  file.path("outputs", "data", "states_service_connected_ranked.csv"),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE
) |>
  dplyr::filter(!.data$name %in% c("District of Columbia", "Puerto Rico"), .data$rating_metric == "any_rating") |>
  dplyr::mutate(any_rating_rank_50 = dplyr::min_rank(dplyr::desc(.data$primary_share)))
georgia_rank <- states_50 |> dplyr::filter(.data$name == project_config$state_name)
national <- readr::read_csv(
  file.path("data", "processed", paste0("acs_", project_config$acs_year, "_service_connected.csv")),
  show_col_types = FALSE
) |>
  dplyr::filter(.data$geography == "us", .data$rating_metric == "any_rating")
national_difference <- georgia_rank$primary_share - national$primary_share
national_difference_moe <- sqrt(georgia_rank$primary_share_moe^2 + national$primary_share_moe^2)
national_significant <- abs(national_difference) > national_difference_moe

format_rate <- function(x) scales::percent(x, accuracy = 0.1)
headline_sentence <- paste0(
  project_config$state_name, " had an estimated ", scales::comma(any_rating$estimate, accuracy = 1),
  " veterans with a service-connected disability rating, ", format_rate(any_rating$primary_share),
  " of civilian veterans age 18 and older (90% MOE +/-", format_rate(any_rating$primary_share_moe), ")."
)
distribution_sentences <- paste0(
  distribution$rating_label, ": ", scales::comma(distribution$estimate, accuracy = 1),
  " veterans; ", format_rate(distribution$primary_share),
  " of veterans with a rating (90% MOE +/-", format_rate(distribution$primary_share_moe),
  "); ", format_rate(distribution$share_all_veterans), " of all veterans."
)
comparison_sentence <- paste0(
  project_config$state_name, " ranked No. ", georgia_rank$any_rating_rank_50,
  " among the 50 states by the share of veterans with a service-connected disability rating. ",
  project_config$state_name, " was at ", format_rate(georgia_rank$primary_share),
  " versus ", format_rate(national$primary_share), " nationally; the ",
  scales::number(abs(national_difference) * 100, accuracy = 0.1),
  "-percentage-point difference was ",
  if (national_significant) "statistically significant" else "not statistically significant",
  " at 90% confidence (difference MOE +/-",
  scales::number(national_difference_moe * 100, accuracy = 0.1), " points)."
)

findings <- dplyr::bind_rows(
  dplyr::tibble(
    finding_id = "service_connected_any_rating", module = "service_connected",
    section = "service_connected_estimates", display_order = 900L,
    sentence = headline_sentence, estimate = any_rating$primary_percent,
    moe = any_rating$primary_percent_moe, unit = "percentage_points",
    geography = "state", geoid = as.character(any_rating$geoid)
  ),
  dplyr::tibble(
    finding_id = paste0("service_connected_", distribution$rating_metric), module = "service_connected",
    section = "service_connected_distribution", display_order = 910L + distribution$rating_order,
    sentence = distribution_sentences, estimate = distribution$primary_percent,
    moe = distribution$primary_percent_moe, unit = "percentage_points_among_rated_veterans",
    geography = "state", geoid = as.character(distribution$geoid)
  ),
  dplyr::tibble(
    finding_id = "service_connected_state_comparison", module = "service_connected",
    section = "service_connected_comparison", display_order = 950L,
    sentence = comparison_sentence, estimate = national_difference * 100,
    moe = national_difference_moe * 100, unit = "percentage_points",
    geography = "state", geoid = as.character(georgia_rank$geoid)
  )
)

caveats <- c(
  "B21100 covers civilian veterans age 18 and older and reports service-connected disability ratings, not general disability status.",
  "Any-rating prevalence uses all veterans as denominator; severity categories use veterans with a rating as denominator.",
  "The 0 percent category is a published service-connected rating category and is retained as reported.",
  "Rating not reported is retained; it must not be redistributed across known rating bands.",
  "ACS estimates reflect survey responses and are not administrative counts from the Department of Veterans Affairs."
)
brief_lines <- c(
  "# Service-Connected Disability Rating Reporter Brief", "",
  paste0("Data vintage: ", project_config$acs_year - 4L, "-", project_config$acs_year, " ACS five-year estimates"), "",
  "## Georgia prevalence", "", paste0("- ", headline_sentence), "",
  "## Rating distribution among veterans with a rating", "", paste0("- ", distribution_sentences), "",
  "## Where Georgia stacks up", "", paste0("- ", comparison_sentence), "",
  "## Caveats / don't-overstate notes", "", paste0("- ", caveats)
)
write_lines_deterministically(brief_lines, file.path("outputs", "reports", "service_connected_reporter_brief.md"))
write_findings_csv(findings, file.path("outputs", "reports", "service_connected_findings.csv"))
log_message("Wrote deterministic service-connected rating brief and findings table.")
