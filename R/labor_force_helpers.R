# ACS B21005 labor-force helpers for the civilian population age 18 to 64.

labor_force_variables <- stats::setNames(
  paste0("B21005_", sprintf("%03d", 1:34)),
  paste0("cell_", sprintf("%03d", 1:34))
)

labor_force_age_definitions <- list(
  age_18_34 = list(
    label = "Age 18 to 34",
    veteran = c(population = "cell_003", labor_force = "cell_004", employed = "cell_005", unemployed = "cell_006", not_labor_force = "cell_007"),
    nonveteran = c(population = "cell_008", labor_force = "cell_009", employed = "cell_010", unemployed = "cell_011", not_labor_force = "cell_012")
  ),
  age_35_54 = list(
    label = "Age 35 to 54",
    veteran = c(population = "cell_014", labor_force = "cell_015", employed = "cell_016", unemployed = "cell_017", not_labor_force = "cell_018"),
    nonveteran = c(population = "cell_019", labor_force = "cell_020", employed = "cell_021", unemployed = "cell_022", not_labor_force = "cell_023")
  ),
  age_55_64 = list(
    label = "Age 55 to 64",
    veteran = c(population = "cell_025", labor_force = "cell_026", employed = "cell_027", unemployed = "cell_028", not_labor_force = "cell_029"),
    nonveteran = c(population = "cell_030", labor_force = "cell_031", employed = "cell_032", unemployed = "cell_033", not_labor_force = "cell_034")
  )
)

labor_force_metric_definitions <- list(
  participation_rate = list(label = "Labor-force participation rate", numerator = "labor_force", denominator = "population"),
  employment_population_ratio = list(label = "Employment-to-population ratio", numerator = "employed", denominator = "population"),
  unemployment_rate = list(label = "Unemployment rate", numerator = "unemployed", denominator = "labor_force")
)

fetch_labor_force_data <- function(geography, config) {
  log_message("Downloading B21005 labor-force data for ", geography)
  tidycensus::get_acs(
    geography = geography, year = config$acs_year, survey = config$acs_survey,
    variables = labor_force_variables, output = "wide", geometry = FALSE,
    moe_level = config$moe_confidence_level, key = Sys.getenv("CENSUS_API_KEY")
  )
}

combine_labor_force_cells <- function(data, cells) {
  estimate_columns <- paste0(cells, "E")
  moe_columns <- paste0(cells, "M")
  missing <- setdiff(c(estimate_columns, moe_columns), names(data))
  if (length(missing) > 0) stop("B21005 response missing: ", paste(missing, collapse = ", "), call. = FALSE)
  estimate_matrix <- as.matrix(data[estimate_columns])
  moe_matrix <- as.matrix(data[moe_columns])
  estimate <- apply(estimate_matrix, 1, function(x) if (anyNA(x)) NA_real_ else sum(x))
  moe <- vapply(seq_len(nrow(data)), function(i) {
    row_estimates <- as.numeric(estimate_matrix[i, ])
    row_moes <- as.numeric(moe_matrix[i, ])
    if (anyNA(row_estimates) || anyNA(row_moes)) return(NA_real_)
    if (length(cells) == 1) return(row_moes[[1]])
    if (any(row_estimates == 0)) {
      tidycensus::moe_sum(moe = row_moes, estimate = row_estimates, na.rm = FALSE)
    } else {
      suppressWarnings(tidycensus::moe_sum(moe = row_moes, na.rm = FALSE))
    }
  }, numeric(1))
  list(estimate = estimate, moe = moe)
}

labor_force_strata <- function() {
  c(labor_force_age_definitions, list(
    all_18_64 = list(
      label = "Age 18 to 64",
      veteran = lapply(names(labor_force_age_definitions[[1]]$veteran), function(component) {
        vapply(labor_force_age_definitions, function(x) unname(x$veteran[[component]]), character(1))
      }) |> stats::setNames(names(labor_force_age_definitions[[1]]$veteran)),
      nonveteran = lapply(names(labor_force_age_definitions[[1]]$nonveteran), function(component) {
        vapply(labor_force_age_definitions, function(x) unname(x$nonveteran[[component]]), character(1))
      }) |> stats::setNames(names(labor_force_age_definitions[[1]]$nonveteran))
    )
  ))
}

prepare_labor_force_data <- function(data, geography, config) {
  if (length(setdiff(c("GEOID", "NAME"), names(data))) > 0) stop("B21005 response is missing GEOID or NAME.", call. = FALSE)
  z_score <- switch(as.character(config$moe_confidence_level), "90" = 1.645, "95" = 1.960, "99" = 2.576,
    stop("Unsupported confidence level.", call. = FALSE))
  strata <- labor_force_strata()

  rows <- lapply(names(strata), function(stratum_name) {
    stratum <- strata[[stratum_name]]
    lapply(names(labor_force_metric_definitions), function(metric_name) {
      metric <- labor_force_metric_definitions[[metric_name]]
      veteran_numerator <- combine_labor_force_cells(data, stratum$veteran[[metric$numerator]])
      veteran_denominator <- combine_labor_force_cells(data, stratum$veteran[[metric$denominator]])
      nonveteran_numerator <- combine_labor_force_cells(data, stratum$nonveteran[[metric$numerator]])
      nonveteran_denominator <- combine_labor_force_cells(data, stratum$nonveteran[[metric$denominator]])
      data.frame(
        geography = geography, geoid = as.character(data$GEOID), name = data$NAME,
        age_stratum = stratum_name, age_label = stratum$label,
        labor_metric = metric_name, labor_metric_label = metric$label,
        veteran_numerator_estimate = veteran_numerator$estimate,
        veteran_numerator_moe = veteran_numerator$moe,
        veteran_denominator_estimate = veteran_denominator$estimate,
        veteran_denominator_moe = veteran_denominator$moe,
        nonveteran_numerator_estimate = nonveteran_numerator$estimate,
        nonveteran_numerator_moe = nonveteran_numerator$moe,
        nonveteran_denominator_estimate = nonveteran_denominator$estimate,
        nonveteran_denominator_moe = nonveteran_denominator$moe,
        stringsAsFactors = FALSE
      )
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()

  rows |>
    dplyr::mutate(
      veteran_rate = dplyr::if_else(.data$veteran_denominator_estimate > 0,
        .data$veteran_numerator_estimate / .data$veteran_denominator_estimate, NA_real_),
      veteran_rate_moe = dplyr::if_else(.data$veteran_denominator_estimate > 0,
        tidycensus::moe_prop(.data$veteran_numerator_estimate, .data$veteran_denominator_estimate,
          .data$veteran_numerator_moe, .data$veteran_denominator_moe), NA_real_),
      nonveteran_rate = dplyr::if_else(.data$nonveteran_denominator_estimate > 0,
        .data$nonveteran_numerator_estimate / .data$nonveteran_denominator_estimate, NA_real_),
      nonveteran_rate_moe = dplyr::if_else(.data$nonveteran_denominator_estimate > 0,
        tidycensus::moe_prop(.data$nonveteran_numerator_estimate, .data$nonveteran_denominator_estimate,
          .data$nonveteran_numerator_moe, .data$nonveteran_denominator_moe), NA_real_),
      veteran_percent = .data$veteran_rate * 100,
      veteran_percent_moe = .data$veteran_rate_moe * 100,
      nonveteran_percent = .data$nonveteran_rate * 100,
      nonveteran_percent_moe = .data$nonveteran_rate_moe * 100,
      rate_gap = .data$veteran_rate - .data$nonveteran_rate,
      rate_gap_moe = sqrt(.data$veteran_rate_moe^2 + .data$nonveteran_rate_moe^2),
      rate_gap_percentage_points = .data$rate_gap * 100,
      rate_gap_moe_percentage_points = .data$rate_gap_moe * 100,
      rate_gap_significant_90 = !is.na(.data$rate_gap) & !is.na(.data$rate_gap_moe) & abs(.data$rate_gap) > .data$rate_gap_moe,
      veteran_percent_ci_lower = pmax(0, (.data$veteran_rate - .data$veteran_rate_moe) * 100),
      veteran_percent_ci_upper = pmin(100, (.data$veteran_rate + .data$veteran_rate_moe) * 100),
      nonveteran_percent_ci_lower = pmax(0, (.data$nonveteran_rate - .data$nonveteran_rate_moe) * 100),
      nonveteran_percent_ci_upper = pmin(100, (.data$nonveteran_rate + .data$nonveteran_rate_moe) * 100),
      veteran_rate_cv = dplyr::if_else(.data$veteran_rate > 0, (.data$veteran_rate_moe / z_score) / .data$veteran_rate, NA_real_),
      nonveteran_rate_cv = dplyr::if_else(.data$nonveteran_rate > 0, (.data$nonveteran_rate_moe / z_score) / .data$nonveteran_rate, NA_real_),
      flag_missing = is.na(.data$veteran_rate) | is.na(.data$veteran_rate_moe) |
        is.na(.data$nonveteran_rate) | is.na(.data$nonveteran_rate_moe),
      flag_nonpositive_denominator = (!is.na(.data$veteran_denominator_estimate) & .data$veteran_denominator_estimate <= 0) |
        (!is.na(.data$nonveteran_denominator_estimate) & .data$nonveteran_denominator_estimate <= 0),
      flag_impossible = (!is.na(.data$veteran_numerator_estimate) & !is.na(.data$veteran_denominator_estimate) &
        (.data$veteran_numerator_estimate < 0 | .data$veteran_numerator_estimate > .data$veteran_denominator_estimate)) |
        (!is.na(.data$nonveteran_numerator_estimate) & !is.na(.data$nonveteran_denominator_estimate) &
        (.data$nonveteran_numerator_estimate < 0 | .data$nonveteran_numerator_estimate > .data$nonveteran_denominator_estimate)),
      flag_high_cv = (!is.na(.data$veteran_rate_cv) & .data$veteran_rate_cv > config$high_cv_threshold) |
        (!is.na(.data$nonveteran_rate_cv) & .data$nonveteran_rate_cv > config$high_cv_threshold),
      reliability_class = dplyr::case_when(
        .data$flag_missing | .data$flag_nonpositive_denominator | .data$flag_impossible ~ "unavailable",
        .data$flag_high_cv ~ "high_cv",
        pmax(.data$veteran_rate_cv, .data$nonveteran_rate_cv, na.rm = TRUE) > 0.15 ~ "moderate_cv",
        TRUE ~ "lower_cv"
      ),
      rank_eligible = !.data$flag_missing & !.data$flag_nonpositive_denominator &
        !.data$flag_impossible & !.data$flag_high_cv &
        !is.na(.data$veteran_rate_cv) & !is.na(.data$nonveteran_rate_cv)
    )
}

labor_force_identity_definitions <- list(
  age_18_34_status = list(parent = "cell_002", children = c("cell_003", "cell_008")),
  age_18_34_veteran_labor = list(parent = "cell_003", children = c("cell_004", "cell_007")),
  age_18_34_veteran_employment = list(parent = "cell_004", children = c("cell_005", "cell_006")),
  age_18_34_nonveteran_labor = list(parent = "cell_008", children = c("cell_009", "cell_012")),
  age_18_34_nonveteran_employment = list(parent = "cell_009", children = c("cell_010", "cell_011")),
  age_35_54_status = list(parent = "cell_013", children = c("cell_014", "cell_019")),
  age_35_54_veteran_labor = list(parent = "cell_014", children = c("cell_015", "cell_018")),
  age_35_54_veteran_employment = list(parent = "cell_015", children = c("cell_016", "cell_017")),
  age_35_54_nonveteran_labor = list(parent = "cell_019", children = c("cell_020", "cell_023")),
  age_35_54_nonveteran_employment = list(parent = "cell_020", children = c("cell_021", "cell_022")),
  age_55_64_status = list(parent = "cell_024", children = c("cell_025", "cell_030")),
  age_55_64_veteran_labor = list(parent = "cell_025", children = c("cell_026", "cell_029")),
  age_55_64_veteran_employment = list(parent = "cell_026", children = c("cell_027", "cell_028")),
  age_55_64_nonveteran_labor = list(parent = "cell_030", children = c("cell_031", "cell_034")),
  age_55_64_nonveteran_employment = list(parent = "cell_031", children = c("cell_032", "cell_033")),
  total_age = list(parent = "cell_001", children = c("cell_002", "cell_013", "cell_024"))
)

qa_labor_force_identities <- function(data, geography) {
  lapply(names(labor_force_identity_definitions), function(identity_name) {
    definition <- labor_force_identity_definitions[[identity_name]]
    parent <- data[[paste0(definition$parent, "E")]]
    children <- as.matrix(data[paste0(definition$children, "E")])
    child_sum <- apply(children, 1, function(x) if (anyNA(x)) NA_real_ else sum(x))
    data.frame(
      geography = geography, geoid = as.character(data$GEOID), name = data$NAME,
      identity = identity_name, parent_estimate = parent, child_sum = child_sum,
      difference = child_sum - parent,
      identity_holds = !is.na(parent) & !is.na(child_sum) & parent == child_sum,
      stringsAsFactors = FALSE
    )
  }) |> dplyr::bind_rows()
}
