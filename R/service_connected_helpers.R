# ACS B21100 service-connected disability rating helpers.

service_connected_variables <- c(
  veteran_total = "B21100_001",
  no_rating = "B21100_002",
  any_rating = "B21100_003",
  rating_0 = "B21100_004",
  rating_10_20 = "B21100_005",
  rating_30_40 = "B21100_006",
  rating_50_60 = "B21100_007",
  rating_70_plus = "B21100_008",
  rating_not_reported = "B21100_009"
)

service_connected_rating_labels <- c(
  rating_0 = "0 percent",
  rating_10_20 = "10 or 20 percent",
  rating_30_40 = "30 or 40 percent",
  rating_50_60 = "50 or 60 percent",
  rating_70_plus = "70 percent or higher",
  rating_not_reported = "Rating not reported"
)

fetch_service_connected_data <- function(geography, config) {
  log_message("Downloading B21100 service-connected rating data for ", geography)
  tidycensus::get_acs(
    geography = geography, year = config$acs_year, survey = config$acs_survey,
    variables = service_connected_variables, output = "wide", geometry = FALSE,
    moe_level = config$moe_confidence_level, key = Sys.getenv("CENSUS_API_KEY")
  )
}

prepare_service_connected_data <- function(data, geography, config) {
  required <- c("GEOID", "NAME", as.vector(outer(names(service_connected_variables), c("E", "M"), paste0)))
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) stop("B21100 response missing: ", paste(missing, collapse = ", "), call. = FALSE)
  z_score <- switch(as.character(config$moe_confidence_level), "90" = 1.645, "95" = 1.960, "99" = 2.576,
    stop("Unsupported confidence level.", call. = FALSE))

  metric_names <- c("any_rating", names(service_connected_rating_labels))
  rows <- lapply(metric_names, function(metric_name) {
    is_any <- metric_name == "any_rating"
    denominator_name <- if (is_any) "veteran_total" else "any_rating"
    data.frame(
      geography = geography, geoid = as.character(data$GEOID), name = data$NAME,
      rating_metric = metric_name,
      rating_label = if (is_any) "Any service-connected disability rating" else unname(service_connected_rating_labels[[metric_name]]),
      metric_type = if (is_any) "prevalence_among_all_veterans" else "distribution_among_rated_veterans",
      estimate = data[[paste0(metric_name, "E")]],
      moe = data[[paste0(metric_name, "M")]],
      denominator_estimate = data[[paste0(denominator_name, "E")]],
      denominator_moe = data[[paste0(denominator_name, "M")]],
      veteran_total_estimate = data$veteran_totalE,
      veteran_total_moe = data$veteran_totalM,
      numerator_variable = unname(service_connected_variables[[metric_name]]),
      denominator_variable = unname(service_connected_variables[[denominator_name]]),
      stringsAsFactors = FALSE
    )
  }) |> dplyr::bind_rows()

  rows |>
    dplyr::mutate(
      primary_share = dplyr::if_else(.data$denominator_estimate > 0,
        .data$estimate / .data$denominator_estimate, NA_real_),
      primary_share_moe = dplyr::if_else(.data$denominator_estimate > 0,
        tidycensus::moe_prop(.data$estimate, .data$denominator_estimate, .data$moe, .data$denominator_moe), NA_real_),
      share_all_veterans = dplyr::if_else(.data$veteran_total_estimate > 0,
        .data$estimate / .data$veteran_total_estimate, NA_real_),
      share_all_veterans_moe = dplyr::if_else(.data$veteran_total_estimate > 0,
        tidycensus::moe_prop(.data$estimate, .data$veteran_total_estimate, .data$moe, .data$veteran_total_moe), NA_real_),
      primary_percent = .data$primary_share * 100,
      primary_percent_moe = .data$primary_share_moe * 100,
      primary_percent_ci_lower = pmax(0, (.data$primary_share - .data$primary_share_moe) * 100),
      primary_percent_ci_upper = pmin(100, (.data$primary_share + .data$primary_share_moe) * 100),
      percent_all_veterans = .data$share_all_veterans * 100,
      percent_all_veterans_moe = .data$share_all_veterans_moe * 100,
      primary_share_cv = dplyr::if_else(.data$primary_share > 0,
        (.data$primary_share_moe / z_score) / .data$primary_share, NA_real_),
      flag_missing = is.na(.data$estimate) | is.na(.data$moe) |
        is.na(.data$denominator_estimate) | is.na(.data$denominator_moe),
      flag_nonpositive_denominator = !is.na(.data$denominator_estimate) & .data$denominator_estimate <= 0,
      flag_impossible = !is.na(.data$estimate) & !is.na(.data$denominator_estimate) &
        (.data$estimate < 0 | .data$estimate > .data$denominator_estimate),
      flag_zero_estimate = !is.na(.data$estimate) & .data$estimate == 0,
      flag_high_cv = !is.na(.data$primary_share_cv) & .data$primary_share_cv > config$high_cv_threshold,
      reliability_class = dplyr::case_when(
        .data$flag_missing | .data$flag_nonpositive_denominator | .data$flag_impossible ~ "unavailable",
        .data$flag_zero_estimate ~ "zero_estimate",
        is.na(.data$primary_share_cv) ~ "unavailable",
        .data$flag_high_cv ~ "high_cv",
        .data$primary_share_cv > 0.15 ~ "moderate_cv",
        TRUE ~ "lower_cv"
      ),
      rank_eligible = !.data$flag_missing & !.data$flag_nonpositive_denominator &
        !.data$flag_impossible & !.data$flag_zero_estimate & !.data$flag_high_cv &
        !is.na(.data$primary_share_cv)
    )
}

qa_service_connected_identities <- function(data, geography) {
  definitions <- list(
    rating_status = list(parent = "veteran_total", children = c("no_rating", "any_rating")),
    rating_distribution = list(parent = "any_rating", children = names(service_connected_rating_labels))
  )
  lapply(names(definitions), function(identity_name) {
    definition <- definitions[[identity_name]]
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
