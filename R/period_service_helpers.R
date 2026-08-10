# ACS B21002 period-of-military-service helpers.

period_service_variables <- c(
  veteran_total = "B21002_001",
  sep2001_or_later_only = "B21002_002",
  sep2001_and_gulf1990_no_vietnam = "B21002_003",
  sep2001_and_gulf1990_and_vietnam = "B21002_004",
  gulf1990_no_vietnam = "B21002_005",
  gulf1990_and_vietnam = "B21002_006",
  vietnam_no_korea_no_wwii = "B21002_007",
  vietnam_and_korea_no_wwii = "B21002_008",
  vietnam_and_korea_and_wwii = "B21002_009",
  korea_no_vietnam_no_wwii = "B21002_010",
  korea_and_wwii_no_vietnam = "B21002_011",
  wwii_no_korea_no_vietnam = "B21002_012",
  between_gulf_and_vietnam_only = "B21002_013",
  between_vietnam_and_korea_only = "B21002_014",
  between_korea_and_wwii_only = "B21002_015",
  pre_wwii_only = "B21002_016"
)

period_service_definitions <- list(
  sep2001_or_later_any = c(
    "sep2001_or_later_only",
    "sep2001_and_gulf1990_no_vietnam",
    "sep2001_and_gulf1990_and_vietnam"
  ),
  gulf1990_2001_any = c(
    "sep2001_and_gulf1990_no_vietnam",
    "sep2001_and_gulf1990_and_vietnam",
    "gulf1990_no_vietnam",
    "gulf1990_and_vietnam"
  ),
  vietnam_any = c(
    "sep2001_and_gulf1990_and_vietnam",
    "gulf1990_and_vietnam",
    "vietnam_no_korea_no_wwii",
    "vietnam_and_korea_no_wwii",
    "vietnam_and_korea_and_wwii"
  ),
  korea_any = c(
    "vietnam_and_korea_no_wwii",
    "vietnam_and_korea_and_wwii",
    "korea_no_vietnam_no_wwii",
    "korea_and_wwii_no_vietnam"
  ),
  wwii_any = c(
    "vietnam_and_korea_and_wwii",
    "korea_and_wwii_no_vietnam",
    "wwii_no_korea_no_vietnam"
  )
)

period_service_labels <- c(
  sep2001_or_later_any = "September 2001 or later",
  gulf1990_2001_any = "Gulf War period, August 1990 to August 2001",
  vietnam_any = "Vietnam War period",
  korea_any = "Korean War period",
  wwii_any = "World War II period"
)

fetch_period_service_data <- function(geography, config) {
  log_message("Downloading B21002 period-of-service data for ", geography)

  tidycensus::get_acs(
    geography = geography,
    year = config$acs_year,
    survey = config$acs_survey,
    variables = period_service_variables,
    output = "wide",
    geometry = FALSE,
    moe_level = config$moe_confidence_level,
    key = Sys.getenv("CENSUS_API_KEY")
  )
}

derive_period_sum <- function(data, component_names) {
  estimate_columns <- paste0(component_names, "E")
  moe_columns <- paste0(component_names, "M")
  missing_columns <- setdiff(c(estimate_columns, moe_columns), names(data))

  if (length(missing_columns) > 0) {
    stop("Missing B21002 columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  estimate_matrix <- as.matrix(data[estimate_columns])
  moe_matrix <- as.matrix(data[moe_columns])
  estimate <- apply(estimate_matrix, 1, function(values) {
    if (anyNA(values)) NA_real_ else sum(values)
  })
  moe <- vapply(seq_len(nrow(data)), function(row_number) {
    row_estimates <- estimate_matrix[row_number, ]
    row_moes <- moe_matrix[row_number, ]

    if (anyNA(row_estimates) || anyNA(row_moes)) {
      return(NA_real_)
    }

    if (any(row_estimates == 0)) {
      tidycensus::moe_sum(
        moe = row_moes,
        estimate = row_estimates,
        na.rm = FALSE
      )
    } else {
      suppressWarnings(tidycensus::moe_sum(moe = row_moes, na.rm = FALSE))
    }
  }, numeric(1))

  list(estimate = estimate, moe = moe)
}

prepare_period_service_data <- function(data, geography, adult_population, config) {
  required <- c("GEOID", "NAME", "veteran_totalE", "veteran_totalM")
  missing_required <- setdiff(required, names(data))
  if (length(missing_required) > 0) {
    stop("B21002 response missing: ", paste(missing_required, collapse = ", "), call. = FALSE)
  }

  base <- data.frame(
    geography = geography,
    geoid = as.character(data$GEOID),
    name = data$NAME,
    veteran_total_estimate = data$veteran_totalE,
    veteran_total_moe = data$veteran_totalM,
    stringsAsFactors = FALSE
  ) |>
    dplyr::left_join(
      adult_population |>
        dplyr::select(
          "geoid", "population_18_plus_estimate", "population_18_plus_moe"
        ),
      by = "geoid"
    )

  metrics <- lapply(names(period_service_definitions), function(metric_name) {
    derived <- derive_period_sum(data, period_service_definitions[[metric_name]])
    data.frame(
      geoid = as.character(data$GEOID),
      period_metric = metric_name,
      period_label = unname(period_service_labels[[metric_name]]),
      metric_type = "overlapping_any_service_composite",
      component_variables = paste(
        unname(period_service_variables[period_service_definitions[[metric_name]]]),
        collapse = ";"
      ),
      estimate = derived$estimate,
      moe = derived$moe,
      stringsAsFactors = FALSE
    )
  }) |>
    dplyr::bind_rows()

  z_score <- switch(
    as.character(config$moe_confidence_level),
    "90" = 1.645,
    "95" = 1.960,
    "99" = 2.576,
    stop("Unsupported confidence level.", call. = FALSE)
  )

  metrics |>
    dplyr::left_join(base, by = "geoid") |>
    dplyr::mutate(
      share_of_veterans = dplyr::if_else(
        .data$veteran_total_estimate > 0,
        .data$estimate / .data$veteran_total_estimate,
        NA_real_
      ),
      share_of_veterans_moe = dplyr::if_else(
        .data$veteran_total_estimate > 0,
        tidycensus::moe_prop(
          .data$estimate, .data$veteran_total_estimate,
          .data$moe, .data$veteran_total_moe
        ),
        NA_real_
      ),
      share_of_adults = dplyr::if_else(
        .data$population_18_plus_estimate > 0,
        .data$estimate / .data$population_18_plus_estimate,
        NA_real_
      ),
      share_of_adults_moe = dplyr::if_else(
        .data$population_18_plus_estimate > 0,
        tidycensus::moe_prop(
          .data$estimate, .data$population_18_plus_estimate,
          .data$moe, .data$population_18_plus_moe
        ),
        NA_real_
      ),
      percent_of_veterans = .data$share_of_veterans * 100,
      percent_of_veterans_moe = .data$share_of_veterans_moe * 100,
      percent_of_adults = .data$share_of_adults * 100,
      percent_of_adults_moe = .data$share_of_adults_moe * 100,
      share_of_veterans_se = .data$share_of_veterans_moe / z_score,
      share_of_veterans_cv = dplyr::if_else(
        .data$share_of_veterans > 0,
        .data$share_of_veterans_se / .data$share_of_veterans,
        NA_real_
      ),
      flag_missing = is.na(.data$estimate) | is.na(.data$moe) |
        is.na(.data$veteran_total_estimate) | is.na(.data$veteran_total_moe),
      flag_impossible = !is.na(.data$estimate) & !is.na(.data$veteran_total_estimate) &
        (.data$estimate < 0 | .data$estimate > .data$veteran_total_estimate),
      flag_zero_estimate = !is.na(.data$estimate) & .data$estimate == 0,
      flag_high_cv = !is.na(.data$share_of_veterans_cv) &
        .data$share_of_veterans_cv > config$high_cv_threshold,
      reliability_class = dplyr::case_when(
        .data$flag_missing | .data$flag_impossible ~ "unavailable",
        .data$flag_zero_estimate ~ "zero_estimate",
        is.na(.data$share_of_veterans_cv) ~ "unavailable",
        .data$share_of_veterans_cv > config$high_cv_threshold ~ "high_cv",
        .data$share_of_veterans_cv > 0.15 ~ "moderate_cv",
        TRUE ~ "lower_cv"
      ),
      rank_eligible = !.data$flag_missing & !.data$flag_impossible &
        !.data$flag_zero_estimate & !.data$flag_high_cv &
        !is.na(.data$share_of_veterans_cv)
    )
}

qa_period_components <- function(data, geography) {
  component_names <- setdiff(names(period_service_variables), "veteran_total")
  component_sum <- apply(as.matrix(data[paste0(component_names, "E")]), 1, function(values) {
    if (anyNA(values)) NA_real_ else sum(values)
  })

  data.frame(
    geography = geography,
    geoid = as.character(data$GEOID),
    name = data$NAME,
    published_total = data$veteran_totalE,
    component_sum = component_sum,
    difference = component_sum - data$veteran_totalE,
    components_match_total = !is.na(component_sum) &
      !is.na(data$veteran_totalE) & component_sum == data$veteran_totalE,
    stringsAsFactors = FALSE
  )
}
