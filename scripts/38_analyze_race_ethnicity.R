# Rank reliable race/ethnicity metrics and isolate Georgia and its counties.

groups <- readr::read_csv(file.path("data", "processed",
  paste0("acs_", project_config$acs_year, "_race_ethnicity.csv")),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE)
rank_metrics <- function(data) {
  ranks <- data |>
    dplyr::filter(.data$rank_eligible) |>
    dplyr::group_by(.data$group_code) |>
    dplyr::mutate(prevalence_rank = dplyr::min_rank(dplyr::desc(.data$veteran_prevalence)),
      composition_rank = dplyr::min_rank(dplyr::desc(.data$veteran_composition))) |>
    dplyr::ungroup() |>
    dplyr::select("geoid", "group_code", "prevalence_rank", "composition_rank")
  dplyr::left_join(data, ranks, by = c("geoid", "group_code"))
}
states <- groups |> dplyr::filter(.data$geography == "state") |> rank_metrics()
focus_state <- states |> dplyr::filter(.data$name == project_config$state_name)
focus_counties <- groups |>
  dplyr::filter(.data$geography == "county",
    grepl(paste0(", ", project_config$state_name, "$"), .data$name)) |> rank_metrics()
top_counties <- focus_counties |>
  dplyr::filter(.data$rank_eligible) |>
  dplyr::group_by(.data$group_code) |>
  dplyr::arrange(dplyr::desc(.data$veteran_prevalence), .data$geoid, .by_group = TRUE) |>
  dplyr::slice_head(n = project_config$ranking_top_n) |>
  dplyr::ungroup()
write_csv_safely(states, file.path("outputs", "data", "states_race_ethnicity_ranked.csv"))
write_csv_safely(focus_state, file.path("outputs", "data", "focus_state_race_ethnicity.csv"))
write_csv_safely(focus_counties, file.path("outputs", "data", "focus_state_counties_race_ethnicity.csv"))
write_csv_safely(top_counties, file.path("outputs", "data", "focus_state_counties_race_ethnicity_top.csv"))
log_message("Race and ethnicity analysis stage complete.")
