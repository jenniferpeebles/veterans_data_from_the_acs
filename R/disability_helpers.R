# ACS C21007 disability-status helpers.

disability_variables <- stats::setNames(
  paste0("C21007_", sprintf("%03d", 1:31)),
  paste0("cell_", sprintf("%03d", 1:31))
)

disability_metric_definitions <- list(
  all_veterans = list(
    label = "All veterans age 18 and older",
    numerator = c("cell_005", "cell_008", "cell_020", "cell_023"),
    denominator = c("cell_003", "cell_018")
  ),
  veterans_18_64 = list(
    label = "Veterans age 18 to 64",
    numerator = c("cell_005", "cell_008"),
    denominator = "cell_003"
  ),
  veterans_65_plus = list(
    label = "Veterans age 65 and older",
    numerator = c("cell_020", "cell_023"),
    denominator = "cell_018"
  ),
  all_veterans_below_poverty = list(
    label = "Veterans below the poverty level",
    numerator = c("cell_005", "cell_020"),
    denominator = c("cell_004", "cell_019")
  ),
  all_veterans_at_or_above_poverty = list(
    label = "Veterans at or above the poverty level",
    numerator = c("cell_008", "cell_023"),
    denominator = c("cell_007", "cell_022")
  ),
  veterans_18_64_below_poverty = list(
    label = "Veterans age 18 to 64 below poverty",
    numerator = "cell_005",
    denominator = "cell_004"
  ),
  veterans_18_64_at_or_above_poverty = list(
    label = "Veterans age 18 to 64 at or above poverty",
    numerator = "cell_008",
    denominator = "cell_007"
  ),
  veterans_65_plus_below_poverty = list(
    label = "Veterans age 65 and older below poverty",
    numerator = "cell_020",
    denominator = "cell_019"
  ),
  veterans_65_plus_at_or_above_poverty = list(
    label = "Veterans age 65 and older at or above poverty",
    numerator = "cell_023",
    denominator = "cell_022"
  )
)

disability_headline_metrics <- c(
  "all_veterans", "veterans_18_64", "veterans_65_plus",
  "all_veterans_below_poverty", "all_veterans_at_or_above_poverty"
)

fetch_disability_data <- function(geography, config) {
  log_message("Downloading C21007 disability data for ", geography)
  tidycensus::get_acs(
    geography = geography,
    year = config$acs_year,
    survey = config$acs_survey,
    variables = disability_variables,
    output = "wide",
    geometry = FALSE,
    moe_level = config$moe_confidence_level,
    key = Sys.getenv("CENSUS_API_KEY")
  )
}

combine_disability_cells <- function(data, cells) {
  estimate_columns <- paste0(cells, "E")
  moe_columns <- paste0(cells, "M")
  missing <- setdiff(c(estimate_columns, moe_columns), names(data))
  if (length(missing) > 0) {
    stop("C21007 response missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  estimate_matrix <- as.matrix(data[estimate_columns])
  moe_matrix <- as.matrix(data[moe_columns])
  estimates <- apply(estimate_matrix, 1, function(x) if (anyNA(x)) NA_real_ else sum(x))
  moes <- vapply(seq_len(nrow(data)), function(i) {
    if (anyNA(estimate_matrix[i, ]) || anyNA(moe_matrix[i, ])) return(NA_real_)
    if (length(cells) == 1) return(as.numeric(moe_matrix[i, 1]))
    row_estimates <- as.numeric(estimate_matrix[i, ])
    row_moes <- as.numeric(moe_matrix[i, ])
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
  list(estimate = estimates, moe = moes)
}

prepare_disability_data <- function(data, geography, config) {
  required <- c("GEOID", "NAME")
  if (length(setdiff(required, names(data))) > 0) {
    stop("C21007 response is missing GEOID or NAME.", call. = FALSE)
  }
  z_score <- switch(
    as.character(config$moe_confidence_level),
    "90" = 1.645, "95" = 1.960, "99" = 2.576,
    stop("Unsupported confidence level.", call. = FALSE)
  )

  metrics <- lapply(names(disability_metric_definitions), function(metric_name) {
    definition <- disability_metric_definitions[[metric_name]]
    numerator <- combine_disability_cells(data, definition$numerator)
    denominator <- combine_disability_cells(data, definition$denominator)
    data.frame(
      geography = geography,
      geoid = as.character(data$GEOID),
      name = data$NAME,
      disability_metric = metric_name,
      disability_label = definition$label,
      disabled_estimate = numerator$estimate,
      disabled_moe = numerator$moe,
      denominator_estimate = denominator$estimate,
      denominator_moe = denominator$moe,
      numerator_variables = paste(unname(disability_variables[definition$numerator]), collapse = ";"),
      denominator_variables = paste(unname(disability_variables[definition$denominator]), collapse = ";"),
      stringsAsFactors = FALSE
    )
  }) |>
    dplyr::bind_rows()

  metrics |>
    dplyr::mutate(
      disability_share = dplyr::if_else(
        .data$denominator_estimate > 0,
        .data$disabled_estimate / .data$denominator_estimate,
        NA_real_
      ),
      disability_share_moe = dplyr::if_else(
        .data$denominator_estimate > 0,
        tidycensus::moe_prop(
          .data$disabled_estimate, .data$denominator_estimate,
          .data$disabled_moe, .data$denominator_moe
        ),
        NA_real_
      ),
      disability_percent = .data$disability_share * 100,
      disability_percent_moe = .data$disability_share_moe * 100,
      disability_percent_ci_lower = pmax(0, (.data$disability_share - .data$disability_share_moe) * 100),
      disability_percent_ci_upper = pmin(100, (.data$disability_share + .data$disability_share_moe) * 100),
      disability_share_se = .data$disability_share_moe / z_score,
      disability_share_cv = dplyr::if_else(
        .data$disability_share > 0,
        .data$disability_share_se / .data$disability_share,
        NA_real_
      ),
      flag_missing = is.na(.data$disabled_estimate) | is.na(.data$disabled_moe) |
        is.na(.data$denominator_estimate) | is.na(.data$denominator_moe),
      flag_nonpositive_denominator = !is.na(.data$denominator_estimate) & .data$denominator_estimate <= 0,
      flag_impossible = !is.na(.data$disabled_estimate) & !is.na(.data$denominator_estimate) &
        (.data$disabled_estimate < 0 | .data$disabled_estimate > .data$denominator_estimate),
      flag_zero_estimate = !is.na(.data$disabled_estimate) & .data$disabled_estimate == 0,
      flag_high_cv = !is.na(.data$disability_share_cv) &
        .data$disability_share_cv > config$high_cv_threshold,
      reliability_class = dplyr::case_when(
        .data$flag_missing | .data$flag_nonpositive_denominator | .data$flag_impossible ~ "unavailable",
        .data$flag_zero_estimate ~ "zero_estimate",
        is.na(.data$disability_share_cv) ~ "unavailable",
        .data$flag_high_cv ~ "high_cv",
        .data$disability_share_cv > 0.15 ~ "moderate_cv",
        TRUE ~ "lower_cv"
      ),
      rank_eligible = !.data$flag_missing & !.data$flag_nonpositive_denominator &
        !.data$flag_impossible & !.data$flag_zero_estimate & !.data$flag_high_cv &
        !is.na(.data$disability_share_cv)
    )
}

disability_identity_definitions <- list(
  total_age = list(parent = "cell_001", children = c("cell_002", "cell_017")),
  age_18_64_status = list(parent = "cell_002", children = c("cell_003", "cell_010")),
  age_18_64_veteran_poverty = list(parent = "cell_003", children = c("cell_004", "cell_007")),
  age_18_64_veteran_below_disability = list(parent = "cell_004", children = c("cell_005", "cell_006")),
  age_18_64_veteran_above_disability = list(parent = "cell_007", children = c("cell_008", "cell_009")),
  age_18_64_nonveteran_poverty = list(parent = "cell_010", children = c("cell_011", "cell_014")),
  age_18_64_nonveteran_below_disability = list(parent = "cell_011", children = c("cell_012", "cell_013")),
  age_18_64_nonveteran_above_disability = list(parent = "cell_014", children = c("cell_015", "cell_016")),
  age_65_plus_status = list(parent = "cell_017", children = c("cell_018", "cell_025")),
  age_65_plus_veteran_poverty = list(parent = "cell_018", children = c("cell_019", "cell_022")),
  age_65_plus_veteran_below_disability = list(parent = "cell_019", children = c("cell_020", "cell_021")),
  age_65_plus_veteran_above_disability = list(parent = "cell_022", children = c("cell_023", "cell_024")),
  age_65_plus_nonveteran_poverty = list(parent = "cell_025", children = c("cell_026", "cell_029")),
  age_65_plus_nonveteran_below_disability = list(parent = "cell_026", children = c("cell_027", "cell_028")),
  age_65_plus_nonveteran_above_disability = list(parent = "cell_029", children = c("cell_030", "cell_031"))
)

qa_disability_identities <- function(data, geography) {
  lapply(names(disability_identity_definitions), function(identity_name) {
    definition <- disability_identity_definitions[[identity_name]]
    parent <- data[[paste0(definition$parent, "E")]]
    child_matrix <- as.matrix(data[paste0(definition$children, "E")])
    child_sum <- apply(child_matrix, 1, function(x) if (anyNA(x)) NA_real_ else sum(x))
    data.frame(
      geography = geography,
      geoid = as.character(data$GEOID),
      name = data$NAME,
      identity = identity_name,
      parent_estimate = parent,
      child_sum = child_sum,
      difference = child_sum - parent,
      identity_holds = !is.na(parent) & !is.na(child_sum) & parent == child_sum,
      stringsAsFactors = FALSE
    )
  }) |>
    dplyr::bind_rows()
}
