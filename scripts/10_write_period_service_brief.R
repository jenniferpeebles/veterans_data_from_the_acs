# Produce deterministic period-of-service findings and module brief.

focus_period <- readr::read_csv(
  file.path("outputs", "data", "focus_state_period_service.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(period_order = match(.data$period_metric, names(period_service_definitions))) |>
  dplyr::arrange(.data$period_order)
top_counties <- readr::read_csv(
  file.path("outputs", "data", "focus_state_counties_period_service_top5.csv"),
  col_types = readr::cols(geoid = readr::col_character()),
  show_col_types = FALSE
)

period_sentences <- paste0(
  focus_period$period_label, ": ", scales::comma(focus_period$estimate, accuracy = 1),
  " veterans; ", scales::percent(focus_period$share_of_veterans, accuracy = 0.1),
  " of veterans (90% MOE +/-",
  scales::percent(focus_period$share_of_veterans_moe, accuracy = 0.1), ")."
)

county_examples <- top_counties |>
  dplyr::mutate(period_order = match(.data$period_metric, names(period_service_definitions))) |>
  dplyr::arrange(.data$period_order, dplyr::desc(.data$share_of_veterans), .data$geoid) |>
  dplyr::group_by(.data$period_metric) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup()
county_sentences <- paste0(
  county_examples$period_label, ": ", county_examples$name,
  " had the highest reliable Georgia county point estimate at ",
  scales::percent(county_examples$share_of_veterans, accuracy = 0.1),
  " (90% MOE +/-",
  scales::percent(county_examples$share_of_veterans_moe, accuracy = 0.1), ")."
)

missing_metrics <- setdiff(names(period_service_definitions), county_examples$period_metric)
missing_sentences <- if (length(missing_metrics) == 0) character() else paste0(
  unname(period_service_labels[missing_metrics]),
  ": No Georgia county estimate passed the configured CV <= ",
  scales::percent(project_config$high_cv_threshold),
  " reliability threshold; no county ranking is reported."
)

period_findings <- dplyr::bind_rows(
  dplyr::tibble(
    finding_id = paste0("period_state_", focus_period$period_metric),
    module = "period_service",
    section = "period_estimates",
    display_order = 300L + focus_period$period_order,
    sentence = period_sentences,
    estimate = focus_period$estimate,
    moe = focus_period$moe,
    unit = "people",
    geography = "state",
    geoid = as.character(focus_period$geoid)
  ),
  dplyr::tibble(
    finding_id = paste0("period_county_", county_examples$period_metric),
    module = "period_service",
    section = "period_county_examples",
    display_order = 400L + county_examples$period_order,
    sentence = county_sentences,
    estimate = county_examples$percent_of_veterans,
    moe = county_examples$percent_of_veterans_moe,
    unit = "percentage_points",
    geography = "county",
    geoid = as.character(county_examples$geoid)
  ),
  dplyr::tibble(
    finding_id = paste0("period_county_unavailable_", missing_metrics),
    module = "period_service",
    section = "period_county_examples",
    display_order = 450L + match(missing_metrics, names(period_service_definitions)),
    sentence = missing_sentences,
    estimate = NA_real_,
    moe = NA_real_,
    unit = "unavailable",
    geography = "county",
    geoid = NA_character_
  )
)

caveats <- c(
  "The five 'any service' periods overlap; they must not be added and do not sum to 100%.",
  "B21002 reports period of service, not combat-zone or combat participation.",
  "County examples are ordered by reliable point estimate; the ordering is not a significance test.",
  "No missing or suppressed values were imputed."
)

brief_lines <- c(
  "# Period-of-Service Reporter Brief",
  "",
  paste0("Data vintage: ", project_config$acs_year - 4L, "-", project_config$acs_year, " ACS five-year estimates"),
  "",
  "## Georgia estimates",
  "",
  paste0("- ", period_findings$sentence[period_findings$section == "period_estimates"]),
  "",
  "## County examples",
  "",
  paste0("- ", period_findings$sentence[period_findings$section == "period_county_examples"]),
  "",
  "## Caveats / don't-overstate notes",
  "",
  paste0("- ", caveats)
)

brief_path <- file.path("outputs", "reports", "period_service_reporter_brief.md")
findings_path <- file.path("outputs", "reports", "period_service_findings.csv")
write_lines_deterministically(brief_lines, brief_path)
write_findings_csv(period_findings, findings_path)
log_message("Wrote deterministic period-service brief and findings table.")
