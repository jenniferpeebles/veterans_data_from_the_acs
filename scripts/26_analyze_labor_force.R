# Rank reliable labor-force rates and isolate Georgia results.

all_labor <- readr::read_csv(
  file.path("data", "processed", paste0("acs_", project_config$acs_year, "_labor_force.csv")),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE
)
rank_labor <- function(data) {
  ranks <- data |>
    dplyr::filter(.data$rank_eligible) |>
    dplyr::group_by(.data$age_stratum, .data$labor_metric) |>
    dplyr::mutate(
      veteran_rate_rank = dplyr::min_rank(dplyr::desc(.data$veteran_rate)),
      veteran_nonveteran_gap_rank = dplyr::min_rank(dplyr::desc(.data$rate_gap))
    ) |>
    dplyr::ungroup() |>
    dplyr::select("geoid", "age_stratum", "labor_metric", "veteran_rate_rank", "veteran_nonveteran_gap_rank")
  dplyr::left_join(data, ranks, by = c("geoid", "age_stratum", "labor_metric"))
}
states <- all_labor |> dplyr::filter(.data$geography == "state") |> rank_labor()
counties <- all_labor |> dplyr::filter(.data$geography == "county") |> rank_labor()
focus_state <- states |> dplyr::filter(.data$name == project_config$state_name)
focus_counties <- counties |>
  dplyr::filter(grepl(paste0(", ", project_config$state_name, "$"), .data$name))
top_focus_counties <- focus_counties |>
  dplyr::filter(.data$rank_eligible, .data$age_stratum == "all_18_64") |>
  dplyr::arrange(.data$labor_metric, dplyr::desc(.data$veteran_rate), .data$geoid) |>
  dplyr::group_by(.data$labor_metric) |>
  dplyr::slice_head(n = 5) |>
  dplyr::ungroup()

write_csv_safely(states, file.path("outputs", "data", "states_labor_force_ranked.csv"))
write_csv_safely(focus_state, file.path("outputs", "data", "focus_state_labor_force.csv"))
write_csv_safely(top_focus_counties, file.path("outputs", "data", "focus_state_counties_labor_force_top5.csv"))
log_message("Labor-force analysis stage complete.")
