# Rank reliable disability-prevalence estimates and isolate the focus state.

all_disability <- readr::read_csv(
  file.path("data", "processed", paste0("acs_", project_config$acs_year, "_disability.csv")),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE
)

rank_disability <- function(data) {
  ranks <- data |>
    dplyr::filter(.data$rank_eligible) |>
    dplyr::group_by(.data$disability_metric) |>
    dplyr::mutate(disability_prevalence_rank = dplyr::min_rank(dplyr::desc(.data$disability_share))) |>
    dplyr::ungroup() |>
    dplyr::select("geoid", "disability_metric", "disability_prevalence_rank")
  dplyr::left_join(data, ranks, by = c("geoid", "disability_metric"))
}

states <- all_disability |> dplyr::filter(.data$geography == "state") |> rank_disability()
counties <- all_disability |> dplyr::filter(.data$geography == "county") |> rank_disability()
places <- all_disability |>
  dplyr::filter(.data$geography == "place", !grepl(" CDP,", .data$name, fixed = TRUE)) |>
  rank_disability()

focus_state <- states |> dplyr::filter(.data$name == project_config$state_name)
focus_counties <- counties |>
  dplyr::filter(grepl(paste0(", ", project_config$state_name, "$"), .data$name))
top_focus_counties <- focus_counties |>
  dplyr::filter(.data$rank_eligible, .data$disability_metric %in% disability_headline_metrics) |>
  dplyr::arrange(.data$disability_metric, dplyr::desc(.data$disability_share), .data$geoid) |>
  dplyr::group_by(.data$disability_metric) |>
  dplyr::slice_head(n = 5) |>
  dplyr::ungroup()

write_csv_safely(states, file.path("outputs", "data", "states_disability_ranked.csv"))
write_csv_safely(focus_state, file.path("outputs", "data", "focus_state_disability.csv"))
write_csv_safely(top_focus_counties, file.path("outputs", "data", "focus_state_counties_disability_top5.csv"))
log_message("Disability analysis stage complete.")
