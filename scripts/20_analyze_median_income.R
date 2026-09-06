# Rank reliable state and focus-state county median-income estimates.

all_income <- readr::read_csv(
  file.path("data", "processed", paste0("acs_", project_config$acs_year, "_median_income.csv")),
  col_types = readr::cols(geoid = readr::col_character()), show_col_types = FALSE
)
rank_income <- function(data) {
  ranks <- data |>
    dplyr::filter(.data$rank_eligible) |>
    dplyr::group_by(.data$income_stratum) |>
    dplyr::mutate(
      veteran_median_rank = dplyr::min_rank(dplyr::desc(.data$veteran_median_income)),
      veteran_income_advantage_rank = dplyr::min_rank(dplyr::desc(.data$income_gap))
    ) |>
    dplyr::ungroup() |>
    dplyr::select("geoid", "income_stratum", "veteran_median_rank", "veteran_income_advantage_rank")
  dplyr::left_join(data, ranks, by = c("geoid", "income_stratum"))
}
states <- all_income |> dplyr::filter(.data$geography == "state") |> rank_income()
counties <- all_income |> dplyr::filter(.data$geography == "county") |> rank_income()
focus_state <- states |> dplyr::filter(.data$name == project_config$state_name)
focus_counties <- counties |>
  dplyr::filter(grepl(paste0(", ", project_config$state_name, "$"), .data$name))
top_focus_counties <- focus_counties |>
  dplyr::filter(.data$rank_eligible, .data$income_stratum == "total") |>
  dplyr::arrange(dplyr::desc(.data$veteran_median_income), .data$geoid) |>
  dplyr::slice_head(n = project_config$ranking_top_n)

write_csv_safely(states, file.path("outputs", "data", "states_median_income_ranked.csv"))
write_csv_safely(focus_state, file.path("outputs", "data", "focus_state_median_income.csv"))
write_csv_safely(top_focus_counties, file.path("outputs", "data", "focus_state_counties_median_income_top.csv"))
log_message("Median-income analysis stage complete.")
