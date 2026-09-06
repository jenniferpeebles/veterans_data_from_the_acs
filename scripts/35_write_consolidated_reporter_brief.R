# Build the canonical deterministic brief from module findings tables.

total_findings <- readr::read_csv(
  file.path("outputs", "reports", "total_veterans_findings.csv"),
  col_types = readr::cols(geoid = readr::col_character()),
  show_col_types = FALSE
)
period_findings <- readr::read_csv(
  file.path("outputs", "reports", "period_service_findings.csv"),
  col_types = readr::cols(geoid = readr::col_character()),
  show_col_types = FALSE
)
disability_findings <- readr::read_csv(
  file.path("outputs", "reports", "disability_findings.csv"),
  col_types = readr::cols(geoid = readr::col_character()),
  show_col_types = FALSE
)
income_findings <- readr::read_csv(
  file.path("outputs", "reports", "median_income_findings.csv"),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE
)
labor_findings <- readr::read_csv(
  file.path("outputs", "reports", "labor_force_findings.csv"),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE
)
service_connected_findings <- readr::read_csv(
  file.path("outputs", "reports", "service_connected_findings.csv"),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE
)
race_ethnicity_findings <- readr::read_csv(
  file.path("outputs", "reports", "race_ethnicity_findings.csv"),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE
)

story_angles <- c(
  "Where are veteran populations most concentrated, and how well do services align with that geography?",
  "How do recent-service and older veteran populations differ between military-base counties, metropolitan Georgia and rural Georgia?",
  "How does disability prevalence differ by veteran age and poverty status, and where are reliable county estimates highest?",
  "How large is Georgia's veteran–nonveteran median-income gap, and does it differ by sex?",
  "Do working-age Georgia veterans have different labor-force participation, employment, or unemployment rates than nonveterans?",
  "How common are service-connected disability ratings in Georgia, and how are ratings distributed by severity?",
  "How do veteran prevalence and the racial composition of veterans differ across Georgia's race and ethnicity groups?",
  "Which apparent geographic differences remain meaningful after accounting for ACS uncertainty?"
)
caveats <- c(
  paste0("These are ", project_config$acs_year - 4L, "-", project_config$acs_year, " ACS five-year estimates, not exact counts."),
  "The five period-of-service composites overlap; they must not be added and do not sum to 100%.",
  "Period of service does not establish combat-zone service or participation in combat.",
  "ACS disability status is not the same as a service-connected disability rating.",
  "C21007 poverty-stratified results cover people for whom poverty status is determined.",
  paste0("B21004 income values are medians in ", project_config$acs_year, " inflation-adjusted dollars for adults with income."),
  "B21005 unemployment rates use the labor force as denominator and exclude people not in the labor force.",
  "Labor-force state ranks run from highest rate to lowest; a high unemployment-rate rank is not favorable.",
  "B21100 service-connected ratings are distinct from general ACS disability status and are not VA administrative counts.",
  "C21001A-G race groups are mutually exclusive; White-alone non-Hispanic and Hispanic/Latino overlap race and must not be added to those categories.",
  paste0(
    "Rankings exclude estimates with CV above ",
    scales::percent(project_config$high_cv_threshold),
    "; this is a project QA threshold, not a Census Bureau classification."
  ),
  "A descriptive ranking is not proof that estimates are statistically distinguishable.",
  "No missing or suppressed values were imputed."
)

text_rows <- function(prefix, module, section, start_order, sentences) {
  dplyr::tibble(
    finding_id = paste0(prefix, seq_along(sentences)),
    module = module,
    section = section,
    display_order = start_order + seq_along(sentences),
    sentence = sentences,
    estimate = NA_real_,
    moe = NA_real_,
    unit = "text",
    geography = "not_applicable",
    geoid = NA_character_
  )
}

all_findings <- dplyr::bind_rows(
  total_findings,
  period_findings,
  disability_findings,
  income_findings,
  labor_findings,
  service_connected_findings,
  race_ethnicity_findings,
  text_rows("story_angle_", "cross_module", "story_angles", 800L, story_angles),
  text_rows("caveat_", "cross_module", "caveats", 900L, caveats)
) |>
  dplyr::arrange(.data$display_order, .data$finding_id)

top_findings <- all_findings |>
  dplyr::filter(.data$section %in% c("top_findings", "period_estimates", "disability_estimates", "median_income_estimates", "labor_force_estimates", "service_connected_estimates", "race_ethnicity_estimates"))
story_statistics <- all_findings |>
  dplyr::filter(.data$section %in% c("story_ready_statistics", "disability_state_comparison", "median_income_comparison", "labor_force_comparison", "service_connected_comparison", "race_ethnicity_comparison"))
county_examples <- all_findings |>
  dplyr::filter(.data$section %in% c("county_examples", "period_county_examples", "disability_county_examples"))
angles <- all_findings |>
  dplyr::filter(.data$section == "story_angles")
caveat_rows <- all_findings |>
  dplyr::filter(.data$section == "caveats")

brief_lines <- c(
  "# Reporter Brief",
  "",
  paste0("Data vintage: ", project_config$acs_year - 4L, "-", project_config$acs_year, " ACS five-year estimates"),
  "",
  "## Top findings",
  "",
  paste0("- ", top_findings$sentence),
  "",
  "## Best story-ready statistics",
  "",
  paste0("- ", story_statistics$sentence),
  "",
  "## County examples and reliability limits",
  "",
  paste0("- ", county_examples$sentence),
  "",
  "## Possible story angles",
  "",
  paste0("- ", angles$sentence),
  "",
  "## Caveats / don't-overstate notes",
  "",
  paste0("- ", caveat_rows$sentence),
  "",
  "## Suggested charts",
  "",
  "- County veteran share with 90% MOE bars.",
  "- Georgia period-of-service composition with overlapping-period warning.",
  "- Georgia veteran disability prevalence by age and poverty status with 90% MOE bars.",
  "- Georgia veteran and nonveteran median incomes with 90% MOE bars.",
  "- Georgia labor-force outcomes by veteran status with 90% MOE bars.",
  "- Georgia service-connected disability rating distribution with 90% MOE bars.",
  "- Georgia veteran prevalence by race and ethnicity with 90% MOE bars.",
  "- Georgia veteran race composition using the mutually exclusive A-G categories.",
  "- A future county map paired with a visible reliability class or CV layer."
)

write_lines_deterministically(
  brief_lines,
  file.path("outputs", "reports", "reporter_brief.md")
)
write_findings_csv(
  all_findings,
  file.path("outputs", "reports", "reporter_brief.csv")
)
log_message("Wrote canonical deterministic reporter brief and findings CSV.")
