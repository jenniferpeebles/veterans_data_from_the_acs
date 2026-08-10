# Rank reliable estimates and create focus-state period-of-service tables.

all_periods <- readr::read_csv(
  file.path("data", "processed", paste0("acs_", project_config$acs_year, "_period_service.csv")),
  col_types = readr::cols(geoid = readr::col_character()),
  show_col_types = FALSE
)

rank_period_metric <- function(data) {
  ranks <- data |>
    dplyr::filter(.data$rank_eligible) |>
    dplyr::group_by(.data$period_metric) |>
    dplyr::mutate(
      rank_share_of_veterans = dplyr::min_rank(dplyr::desc(.data$share_of_veterans)),
      rank_share_of_adults = dplyr::min_rank(dplyr::desc(.data$share_of_adults))
    ) |>
    dplyr::ungroup() |>
    dplyr::select("geoid", "period_metric", "rank_share_of_veterans", "rank_share_of_adults")

  data |>
    dplyr::left_join(ranks, by = c("geoid", "period_metric"))
}

states_period <- all_periods |>
  dplyr::filter(.data$geography == "state") |>
  rank_period_metric()
counties_period <- all_periods |>
  dplyr::filter(.data$geography == "county") |>
  rank_period_metric()
places_period <- all_periods |>
  dplyr::filter(
    .data$geography == "place",
    !grepl(" CDP,", .data$name, fixed = TRUE),
    .data$population_18_plus_estimate >= project_config$place_min_population_18_plus
  ) |>
  rank_period_metric()

focus_state_period <- states_period |>
  dplyr::filter(.data$name == project_config$state_name)
focus_counties_period <- counties_period |>
  dplyr::filter(grepl(paste0(", ", project_config$state_name, "$"), .data$name))
focus_places_period <- places_period |>
  dplyr::filter(grepl(paste0(", ", project_config$state_name, "$"), .data$name))

top_focus_counties_by_period <- focus_counties_period |>
  dplyr::filter(.data$rank_eligible) |>
  dplyr::arrange(.data$period_metric, dplyr::desc(.data$share_of_veterans), .data$geoid) |>
  dplyr::group_by(.data$period_metric) |>
  dplyr::slice_head(n = 5) |>
  dplyr::ungroup()

top_focus_places_by_period <- focus_places_period |>
  dplyr::filter(.data$rank_eligible) |>
  dplyr::arrange(.data$period_metric, dplyr::desc(.data$share_of_veterans), .data$geoid) |>
  dplyr::group_by(.data$period_metric) |>
  dplyr::slice_head(n = 5) |>
  dplyr::ungroup()

write_csv_safely(states_period, file.path("outputs", "data", "states_period_service_ranked.csv"))
write_csv_safely(focus_state_period, file.path("outputs", "data", "focus_state_period_service.csv"))
write_csv_safely(
  top_focus_counties_by_period,
  file.path("outputs", "data", "focus_state_counties_period_service_top5.csv")
)
write_csv_safely(
  top_focus_places_by_period,
  file.path("outputs", "data", "focus_state_places_period_service_top5.csv")
)

log_message("Period-of-service analysis stage complete.")
