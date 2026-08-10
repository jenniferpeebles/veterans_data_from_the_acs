# Helpers specific to ACS estimates and margins of error.

total_veteran_variables <- c(
  population_18_plus = "B21001_001",
  veterans = "B21001_002"
)

fetch_total_veteran_data <- function(geography, config) {
  log_message("Downloading ", config$acs_year, " ", config$acs_survey, " data for ", geography)

  tidycensus::get_acs(
    geography = geography,
    year = config$acs_year,
    survey = config$acs_survey,
    variables = total_veteran_variables,
    output = "wide",
    geometry = FALSE,
    moe_level = config$moe_confidence_level,
    key = Sys.getenv("CENSUS_API_KEY")
  )
}

calculate_coefficient_of_variation <- function(estimate, moe, confidence_level = 90L) {
  z_score <- switch(
    as.character(confidence_level),
    "90" = 1.645,
    "95" = 1.960,
    "99" = 2.576,
    stop("Unsupported MOE confidence level: ", confidence_level, call. = FALSE)
  )

  dplyr::if_else(
    is.na(estimate) | is.na(moe) | estimate <= 0,
    NA_real_,
    (moe / z_score) / estimate
  )
}

prepare_total_veteran_data <- function(data, geography, config) {
  required_columns <- c(
    "GEOID", "NAME", "population_18_plusE", "population_18_plusM",
    "veteransE", "veteransM"
  )
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      "ACS response is missing expected columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  z_score <- switch(
    as.character(config$moe_confidence_level),
    "90" = 1.645,
    "95" = 1.960,
    "99" = 2.576,
    stop("Unsupported MOE confidence level: ", config$moe_confidence_level, call. = FALSE)
  )

  data |>
    dplyr::transmute(
      geography = geography,
      geoid = as.character(.data$GEOID),
      name = .data$NAME,
      population_18_plus_estimate = .data$population_18_plusE,
      population_18_plus_moe = .data$population_18_plusM,
      veterans_estimate = .data$veteransE,
      veterans_moe = .data$veteransM,
      veteran_share = dplyr::if_else(
        .data$population_18_plusE > 0,
        .data$veteransE / .data$population_18_plusE,
        NA_real_
      ),
      veteran_share_moe = dplyr::if_else(
        .data$population_18_plusE > 0,
        tidycensus::moe_prop(
          .data$veteransE,
          .data$population_18_plusE,
          .data$veteransM,
          .data$population_18_plusM
        ),
        NA_real_
      ),
      veteran_estimate_cv = calculate_coefficient_of_variation(
        .data$veteransE,
        .data$veteransM,
        config$moe_confidence_level
      )
    ) |>
    dplyr::mutate(
      veteran_percent = .data$veteran_share * 100,
      veteran_percent_moe = .data$veteran_share_moe * 100,
      veteran_percent_ci_lower = pmax(0, (.data$veteran_share - .data$veteran_share_moe) * 100),
      veteran_percent_ci_upper = pmin(100, (.data$veteran_share + .data$veteran_share_moe) * 100),
      veteran_share_se = .data$veteran_share_moe / z_score,
      veteran_share_cv = dplyr::if_else(
        is.na(.data$veteran_share) | .data$veteran_share <= 0,
        NA_real_,
        .data$veteran_share_se / .data$veteran_share
      ),
      veteran_share_relative_moe = dplyr::if_else(
        is.na(.data$veteran_share) | .data$veteran_share <= 0,
        NA_real_,
        .data$veteran_share_moe / .data$veteran_share
      ),
      flag_missing = is.na(.data$population_18_plus_estimate) |
        is.na(.data$veterans_estimate) |
        is.na(.data$population_18_plus_moe) |
        is.na(.data$veterans_moe),
      flag_nonpositive_denominator = !is.na(.data$population_18_plus_estimate) &
        .data$population_18_plus_estimate <= 0,
      flag_impossible_estimate = !is.na(.data$veterans_estimate) &
        !is.na(.data$population_18_plus_estimate) &
        (.data$veterans_estimate < 0 |
          .data$veterans_estimate > .data$population_18_plus_estimate),
      flag_zero_estimate = !is.na(.data$veterans_estimate) & .data$veterans_estimate == 0,
      flag_high_cv = !is.na(.data$veteran_share_cv) &
        .data$veteran_share_cv > config$high_cv_threshold,
      reliability_class = dplyr::case_when(
        .data$flag_missing | .data$flag_nonpositive_denominator | .data$flag_impossible_estimate ~ "unavailable",
        .data$flag_zero_estimate ~ "zero_estimate",
        is.na(.data$veteran_share_cv) ~ "unavailable",
        .data$veteran_share_cv > config$high_cv_threshold ~ "high_cv",
        .data$veteran_share_cv > 0.15 ~ "moderate_cv",
        TRUE ~ "lower_cv"
      ),
      rank_eligible = !.data$flag_missing &
        !.data$flag_nonpositive_denominator &
        !.data$flag_impossible_estimate &
        !.data$flag_zero_estimate &
        !.data$flag_high_cv &
        !is.na(.data$veteran_share_cv)
    )
}

rank_veteran_share <- function(data) {
  eligible <- data |>
    dplyr::filter(.data$rank_eligible) |>
    dplyr::mutate(
      veteran_share_rank = dplyr::min_rank(dplyr::desc(.data$veteran_share))
    ) |>
    dplyr::select("geoid", "veteran_share_rank")

  data |>
    dplyr::left_join(eligible, by = "geoid")
}
