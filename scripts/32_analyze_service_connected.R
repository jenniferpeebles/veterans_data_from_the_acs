# Rank reliable service-connected rating metrics and isolate Georgia.

all_ratings <- readr::read_csv(
  file.path("data", "processed", paste0("acs_", project_config$acs_year, "_service_connected.csv")),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE
)
rank_ratings <- function(data) {
  ranks <- data |>
    dplyr::filter(.data$rank_eligible) |>
    dplyr::group_by(.data$rating_metric) |>
    dplyr::mutate(primary_share_rank = dplyr::min_rank(dplyr::desc(.data$primary_share))) |>
    dplyr::ungroup() |>
    dplyr::select("geoid", "rating_metric", "primary_share_rank")
  dplyr::left_join(data, ranks, by = c("geoid", "rating_metric"))
}
states <- all_ratings |> dplyr::filter(.data$geography == "state") |> rank_ratings()
counties <- all_ratings |> dplyr::filter(.data$geography == "county") |> rank_ratings()
focus_state <- states |> dplyr::filter(.data$name == project_config$state_name)
focus_counties <- counties |>
  dplyr::filter(grepl(paste0(", ", project_config$state_name, "$"), .data$name))
top_focus_counties <- focus_counties |>
  dplyr::filter(.data$rank_eligible, .data$rating_metric == "any_rating") |>
  dplyr::arrange(dplyr::desc(.data$primary_share), .data$geoid) |>
  dplyr::slice_head(n = project_config$ranking_top_n)

write_csv_safely(states, file.path("outputs", "data", "states_service_connected_ranked.csv"))
write_csv_safely(focus_state, file.path("outputs", "data", "focus_state_service_connected.csv"))
write_csv_safely(top_focus_counties, file.path("outputs", "data", "focus_state_counties_service_connected_top.csv"))
log_message("Service-connected rating analysis stage complete.")
