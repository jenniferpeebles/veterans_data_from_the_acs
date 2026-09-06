# ACS B21004 median-income helpers.

median_income_variables <- c(
  total = "B21004_001",
  veteran_total = "B21004_002",
  veteran_male = "B21004_003",
  veteran_female = "B21004_004",
  nonveteran_total = "B21004_005",
  nonveteran_male = "B21004_006",
  nonveteran_female = "B21004_007"
)

median_income_strata <- list(
  total = list(
    label = "Civilian population age 18 and older with income",
    veteran = "veteran_total",
    nonveteran = "nonveteran_total"
  ),
  male = list(
    label = "Men age 18 and older with income",
    veteran = "veteran_male",
    nonveteran = "nonveteran_male"
  ),
  female = list(
    label = "Women age 18 and older with income",
    veteran = "veteran_female",
    nonveteran = "nonveteran_female"
  )
)

fetch_median_income_data <- function(geography, config) {
  log_message("Downloading B21004 median-income data for ", geography)
  tidycensus::get_acs(
    geography = geography,
    year = config$acs_year,
    survey = config$acs_survey,
    variables = median_income_variables,
    output = "wide",
    geometry = FALSE,
    moe_level = config$moe_confidence_level,
    key = Sys.getenv("CENSUS_API_KEY")
  )
}

prepare_median_income_data <- function(data, geography, config) {
  required <- c(
    "GEOID", "NAME",
    as.vector(outer(names(median_income_variables), c("E", "M"), paste0))
  )
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("B21004 response missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  z_score <- switch(
    as.character(config$moe_confidence_level),
    "90" = 1.645, "95" = 1.960, "99" = 2.576,
    stop("Unsupported confidence level.", call. = FALSE)
  )

  rows <- lapply(names(median_income_strata), function(stratum) {
    definition <- median_income_strata[[stratum]]
    veteran_estimate <- data[[paste0(definition$veteran, "E")]]
    veteran_moe <- data[[paste0(definition$veteran, "M")]]
    nonveteran_estimate <- data[[paste0(definition$nonveteran, "E")]]
    nonveteran_moe <- data[[paste0(definition$nonveteran, "M")]]
    data.frame(
      geography = geography,
      geoid = as.character(data$GEOID),
      name = data$NAME,
      income_stratum = stratum,
      income_stratum_label = definition$label,
      veteran_variable = unname(median_income_variables[[definition$veteran]]),
      nonveteran_variable = unname(median_income_variables[[definition$nonveteran]]),
      veteran_median_income = veteran_estimate,
      veteran_median_income_moe = veteran_moe,
      nonveteran_median_income = nonveteran_estimate,
      nonveteran_median_income_moe = nonveteran_moe,
      stringsAsFactors = FALSE
    )
  }) |>
    dplyr::bind_rows()

  rows |>
    dplyr::mutate(
      income_gap = .data$veteran_median_income - .data$nonveteran_median_income,
      income_gap_moe = sqrt(.data$veteran_median_income_moe^2 + .data$nonveteran_median_income_moe^2),
      income_gap_ci_lower = .data$income_gap - .data$income_gap_moe,
      income_gap_ci_upper = .data$income_gap + .data$income_gap_moe,
      income_gap_significant_90 = !is.na(.data$income_gap) & !is.na(.data$income_gap_moe) &
        abs(.data$income_gap) > .data$income_gap_moe,
      veteran_median_ci_lower = pmax(0, .data$veteran_median_income - .data$veteran_median_income_moe),
      veteran_median_ci_upper = .data$veteran_median_income + .data$veteran_median_income_moe,
      nonveteran_median_ci_lower = pmax(0, .data$nonveteran_median_income - .data$nonveteran_median_income_moe),
      nonveteran_median_ci_upper = .data$nonveteran_median_income + .data$nonveteran_median_income_moe,
      veteran_median_cv = calculate_coefficient_of_variation(
        .data$veteran_median_income, .data$veteran_median_income_moe,
        config$moe_confidence_level
      ),
      nonveteran_median_cv = calculate_coefficient_of_variation(
        .data$nonveteran_median_income, .data$nonveteran_median_income_moe,
        config$moe_confidence_level
      ),
      flag_missing = is.na(.data$veteran_median_income) | is.na(.data$veteran_median_income_moe) |
        is.na(.data$nonveteran_median_income) | is.na(.data$nonveteran_median_income_moe),
      flag_nonpositive = (!is.na(.data$veteran_median_income) & .data$veteran_median_income <= 0) |
        (!is.na(.data$nonveteran_median_income) & .data$nonveteran_median_income <= 0),
      flag_invalid_moe = (!is.na(.data$veteran_median_income_moe) & .data$veteran_median_income_moe < 0) |
        (!is.na(.data$nonveteran_median_income_moe) & .data$nonveteran_median_income_moe < 0),
      flag_high_cv = (!is.na(.data$veteran_median_cv) & .data$veteran_median_cv > config$high_cv_threshold) |
        (!is.na(.data$nonveteran_median_cv) & .data$nonveteran_median_cv > config$high_cv_threshold),
      reliability_class = dplyr::case_when(
        .data$flag_missing | .data$flag_nonpositive | .data$flag_invalid_moe ~ "unavailable",
        is.na(.data$veteran_median_cv) | is.na(.data$nonveteran_median_cv) ~ "unavailable",
        .data$flag_high_cv ~ "high_cv",
        pmax(.data$veteran_median_cv, .data$nonveteran_median_cv) > 0.15 ~ "moderate_cv",
        TRUE ~ "lower_cv"
      ),
      rank_eligible = !.data$flag_missing & !.data$flag_nonpositive &
        !.data$flag_invalid_moe & !.data$flag_high_cv &
        !is.na(.data$veteran_median_cv) & !is.na(.data$nonveteran_median_cv)
    )
}
