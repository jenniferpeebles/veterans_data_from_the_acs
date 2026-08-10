# Produce deterministic total-veteran findings and module brief.

focus_state <- readr::read_csv(
  file.path("outputs", "data", "focus_state_total_veterans.csv"),
  show_col_types = FALSE
)
top_counties <- readr::read_csv(
  file.path("outputs", "data", "focus_state_counties_highest_veteran_share.csv"),
  col_types = readr::cols(geoid = readr::col_character()),
  show_col_types = FALSE
) |>
  dplyr::arrange(dplyr::desc(.data$veteran_share), .data$geoid)
qa_summary <- readr::read_csv(
  file.path("outputs", "qa", "total_veterans_qa_summary.csv"),
  show_col_types = FALSE
)

format_count <- function(value) scales::comma(value, accuracy = 1)
format_percent <- function(value) paste0(format(round(value, 1), nsmall = 1), "%")

state_sentence <- paste0(
  project_config$state_name, " had an estimated ",
  format_count(focus_state$veterans_estimate), " veterans, representing ",
  format_percent(focus_state$veteran_percent),
  " of its civilian population age 18 and older."
)
state_moe_sentence <- paste0(
  "The state estimate's 90% margin of error was +/-",
  format_count(focus_state$veterans_moe), " veterans and +/-",
  format_percent(focus_state$veteran_percent_moe), " for the percentage."
)
county_sentences <- paste0(
  top_counties$name, ": ", format_percent(top_counties$veteran_percent),
  " (90% MOE +/-", format_percent(top_counties$veteran_percent_moe), ")."
)

total_findings <- dplyr::bind_rows(
  dplyr::tibble(
    finding_id = c("total_state_estimate", "total_state_moe"),
    module = "total_veterans",
    section = c("top_findings", "story_ready_statistics"),
    display_order = c(100L, 110L),
    sentence = c(state_sentence, state_moe_sentence),
    estimate = c(focus_state$veterans_estimate, focus_state$veteran_percent),
    moe = c(focus_state$veterans_moe, focus_state$veteran_percent_moe),
    unit = c("people", "percentage_points"),
    geography = "state",
    geoid = as.character(focus_state$geoid)
  ),
  dplyr::tibble(
    finding_id = paste0("total_county_", top_counties$geoid),
    module = "total_veterans",
    section = "county_examples",
    display_order = 200L + seq_len(nrow(top_counties)),
    sentence = county_sentences,
    estimate = top_counties$veteran_percent,
    moe = top_counties$veteran_percent_moe,
    unit = "percentage_points",
    geography = "county",
    geoid = as.character(top_counties$geoid)
  )
)

caveats <- c(
  paste0("These are ", project_config$acs_year, " five-year ACS estimates, not exact counts."),
  paste0(
    "Rankings exclude estimates whose coefficient of variation exceeds ",
    scales::percent(project_config$high_cv_threshold),
    "; this is a project QA threshold, not a Census Bureau rule."
  ),
  "Overlapping margins of error are not, by themselves, a formal significance test.",
  "No missing or suppressed values were imputed."
)

brief_lines <- c(
  "# Total Veteran Population Reporter Brief",
  "",
  paste0("Data vintage: ", project_config$acs_year - 4L, "-", project_config$acs_year, " ACS five-year estimates"),
  "",
  "## Top findings",
  "",
  paste0("- ", total_findings$sentence[total_findings$section == "top_findings"]),
  "",
  "## Best story-ready statistics",
  "",
  paste0("- ", total_findings$sentence[total_findings$section == "story_ready_statistics"]),
  "",
  "## Biggest county point estimates",
  "",
  paste0("- ", total_findings$sentence[total_findings$section == "county_examples"]),
  "",
  "## Caveats / don't-overstate notes",
  "",
  paste0("- ", caveats),
  "",
  "## QA status",
  "",
  paste0("- Total records reviewed: ", sum(qa_summary$records), "."),
  paste0("- Impossible estimates found: ", sum(qa_summary$impossible_estimates), ".")
)

brief_path <- file.path("outputs", "reports", "total_veterans_reporter_brief.md")
findings_path <- file.path("outputs", "reports", "total_veterans_findings.csv")
write_lines_deterministically(brief_lines, brief_path)
write_findings_csv(total_findings, findings_path)
log_message("Wrote deterministic total-veteran brief and findings table.")
