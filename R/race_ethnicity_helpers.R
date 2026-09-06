# ACS C21001A-I race and ethnicity helpers.

race_ethnicity_groups <- data.frame(
  group_code = LETTERS[1:9],
  group_label = c(
    "White alone", "Black or African American alone",
    "American Indian and Alaska Native alone", "Asian alone",
    "Native Hawaiian and Other Pacific Islander alone",
    "Some other race alone", "Two or more races",
    "White alone, not Hispanic or Latino", "Hispanic or Latino"
  ),
  classification = c(rep("mutually_exclusive_race", 7),
    "overlapping_race_ethnicity", "overlapping_race_ethnicity"),
  stringsAsFactors = FALSE
)

race_ethnicity_cells <- c(
  total = 1, male = 2, male_18_64 = 3, male_18_64_veteran = 4,
  male_18_64_nonveteran = 5, male_65_plus = 6, male_65_plus_veteran = 7,
  male_65_plus_nonveteran = 8, female = 9, female_18_64 = 10,
  female_18_64_veteran = 11, female_18_64_nonveteran = 12,
  female_65_plus = 13, female_65_plus_veteran = 14,
  female_65_plus_nonveteran = 15
)

race_ethnicity_variables <- function() {
  variables <- unlist(lapply(race_ethnicity_groups$group_code, function(group_code) {
    stats::setNames(
      sprintf("C21001%s_%03d", group_code, race_ethnicity_cells),
      paste0(tolower(group_code), "_", names(race_ethnicity_cells))
    )
  }))
  unname(variables) |> stats::setNames(names(variables))
}

fetch_race_ethnicity_data <- function(geography, config) {
  log_message("Downloading C21001A-I race and ethnicity data for ", geography)
  tidycensus::get_acs(
    geography = geography, year = config$acs_year, survey = config$acs_survey,
    variables = race_ethnicity_variables(), output = "wide", geometry = FALSE,
    moe_level = config$moe_confidence_level, key = Sys.getenv("CENSUS_API_KEY")
  )
}

sum_acs_components <- function(data, stems, suffix) {
  columns <- paste0(stems, suffix)
  apply(data[columns], 1, function(values) {
    if (anyNA(values)) return(NA_real_)
    if (suffix == "E") return(sum(values))
    suppressWarnings(tidycensus::moe_sum(moe = values, na.rm = FALSE))
  })
}

prepare_race_ethnicity_data <- function(data, totals, geography, config) {
  expected <- c("GEOID", "NAME", as.vector(outer(names(race_ethnicity_variables()), c("E", "M"), paste0)))
  missing <- setdiff(expected, names(data))
  if (length(missing) > 0) stop("C21001 response missing: ", paste(missing, collapse = ", "), call. = FALSE)
  total_columns <- c("geoid", "veterans_estimate", "veterans_moe")
  if (!all(total_columns %in% names(totals))) stop("Overall-veteran totals are missing required columns.", call. = FALSE)
  z_score <- switch(as.character(config$moe_confidence_level), "90" = 1.645, "95" = 1.960,
    "99" = 2.576, stop("Unsupported confidence level.", call. = FALSE))

  rows <- lapply(seq_len(nrow(race_ethnicity_groups)), function(index) {
    group <- race_ethnicity_groups[index, ]
    prefix <- paste0(tolower(group$group_code), "_")
    veteran_stems <- paste0(prefix, c("male_18_64_veteran", "male_65_plus_veteran",
      "female_18_64_veteran", "female_65_plus_veteran"))
    data.frame(
      geography = geography, geoid = as.character(data$GEOID), name = data$NAME,
      group_code = group$group_code, group_label = group$group_label,
      classification = group$classification,
      group_population_estimate = data[[paste0(prefix, "totalE")]],
      group_population_moe = data[[paste0(prefix, "totalM")]],
      group_veterans_estimate = sum_acs_components(data, veteran_stems, "E"),
      group_veterans_moe = sum_acs_components(data, veteran_stems, "M"),
      stringsAsFactors = FALSE
    )
  }) |> dplyr::bind_rows() |>
    dplyr::left_join(totals |> dplyr::select(dplyr::all_of(total_columns)), by = "geoid")

  rows |>
    dplyr::mutate(
      veteran_prevalence = dplyr::if_else(.data$group_population_estimate > 0,
        .data$group_veterans_estimate / .data$group_population_estimate, NA_real_),
      veteran_prevalence_moe = dplyr::if_else(.data$group_population_estimate > 0,
        tidycensus::moe_prop(.data$group_veterans_estimate, .data$group_population_estimate,
          .data$group_veterans_moe, .data$group_population_moe), NA_real_),
      veteran_composition = dplyr::if_else(.data$veterans_estimate > 0,
        .data$group_veterans_estimate / .data$veterans_estimate, NA_real_),
      veteran_composition_moe = dplyr::if_else(.data$veterans_estimate > 0,
        tidycensus::moe_prop(.data$group_veterans_estimate, .data$veterans_estimate,
          .data$group_veterans_moe, .data$veterans_moe), NA_real_),
      veteran_prevalence_percent = .data$veteran_prevalence * 100,
      veteran_prevalence_percent_moe = .data$veteran_prevalence_moe * 100,
      veteran_prevalence_ci_lower = pmax(0, (.data$veteran_prevalence - .data$veteran_prevalence_moe) * 100),
      veteran_prevalence_ci_upper = pmin(100, (.data$veteran_prevalence + .data$veteran_prevalence_moe) * 100),
      veteran_composition_percent = .data$veteran_composition * 100,
      veteran_composition_percent_moe = .data$veteran_composition_moe * 100,
      veteran_composition_ci_lower = pmax(0, (.data$veteran_composition - .data$veteran_composition_moe) * 100),
      veteran_composition_ci_upper = pmin(100, (.data$veteran_composition + .data$veteran_composition_moe) * 100),
      veteran_prevalence_cv = dplyr::if_else(.data$veteran_prevalence > 0,
        (.data$veteran_prevalence_moe / z_score) / .data$veteran_prevalence, NA_real_),
      veteran_composition_cv = dplyr::if_else(.data$veteran_composition > 0,
        (.data$veteran_composition_moe / z_score) / .data$veteran_composition, NA_real_),
      flag_missing = is.na(.data$group_veterans_estimate) | is.na(.data$group_veterans_moe) |
        is.na(.data$group_population_estimate) | is.na(.data$group_population_moe) |
        is.na(.data$veterans_estimate) | is.na(.data$veterans_moe),
      flag_nonpositive_denominator = !is.na(.data$group_population_estimate) & .data$group_population_estimate <= 0,
      flag_impossible = !is.na(.data$group_veterans_estimate) & !is.na(.data$group_population_estimate) &
        (.data$group_veterans_estimate < 0 | .data$group_veterans_estimate > .data$group_population_estimate),
      flag_zero_estimate = !is.na(.data$group_veterans_estimate) & .data$group_veterans_estimate == 0,
      flag_high_cv = (!is.na(.data$veteran_prevalence_cv) & .data$veteran_prevalence_cv > config$high_cv_threshold) |
        (!is.na(.data$veteran_composition_cv) & .data$veteran_composition_cv > config$high_cv_threshold),
      reliability_class = dplyr::case_when(
        .data$flag_missing | .data$flag_nonpositive_denominator | .data$flag_impossible ~ "unavailable",
        .data$flag_zero_estimate ~ "zero_estimate", .data$flag_high_cv ~ "high_cv",
        pmax(.data$veteran_prevalence_cv, .data$veteran_composition_cv, na.rm = TRUE) > 0.15 ~ "moderate_cv",
        TRUE ~ "lower_cv"),
      rank_eligible = !.data$flag_missing & !.data$flag_nonpositive_denominator &
        !.data$flag_impossible & !.data$flag_zero_estimate & !.data$flag_high_cv
    )
}

qa_race_ethnicity_identities <- function(data, geography) {
  definitions <- list(
    total = list(parent = "total", children = c("male", "female")),
    male = list(parent = "male", children = c("male_18_64", "male_65_plus")),
    male_18_64 = list(parent = "male_18_64", children = c("male_18_64_veteran", "male_18_64_nonveteran")),
    male_65_plus = list(parent = "male_65_plus", children = c("male_65_plus_veteran", "male_65_plus_nonveteran")),
    female = list(parent = "female", children = c("female_18_64", "female_65_plus")),
    female_18_64 = list(parent = "female_18_64", children = c("female_18_64_veteran", "female_18_64_nonveteran")),
    female_65_plus = list(parent = "female_65_plus", children = c("female_65_plus_veteran", "female_65_plus_nonveteran"))
  )
  lapply(race_ethnicity_groups$group_code, function(group_code) {
    prefix <- paste0(tolower(group_code), "_")
    lapply(names(definitions), function(identity) {
      definition <- definitions[[identity]]
      parent <- data[[paste0(prefix, definition$parent, "E")]]
      children <- as.matrix(data[paste0(prefix, definition$children, "E")])
      child_sum <- apply(children, 1, function(x) if (anyNA(x)) NA_real_ else sum(x))
      data.frame(geography = geography, geoid = as.character(data$GEOID), name = data$NAME,
        group_code = group_code, identity = identity, parent_estimate = parent,
        child_sum = child_sum, difference = child_sum - parent,
        identity_holds = !is.na(parent) & !is.na(child_sum) & parent == child_sum)
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()
}
