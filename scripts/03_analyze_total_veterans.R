# Create findings tables only after QA has completed.

processed_path <- file.path(
  "data", "processed",
  paste0("acs_", project_config$acs_year, "_total_veterans.csv")
)
all_prepared <- readr::read_csv(processed_path, show_col_types = FALSE)

states_ranked <- all_prepared |>
  dplyr::filter(.data$geography == "state") |>
  rank_veteran_share()

counties_ranked <- all_prepared |>
  dplyr::filter(.data$geography == "county") |>
  rank_veteran_share()

places_ranked <- all_prepared |>
  dplyr::filter(
    .data$geography == "place",
    !grepl(" CDP,", .data$name, fixed = TRUE),
    .data$population_18_plus_estimate >= project_config$place_min_population_18_plus
  ) |>
  rank_veteran_share()

focus_state <- states_ranked |>
  dplyr::filter(.data$name == project_config$state_name)

if (nrow(focus_state) != 1) {
  stop("Expected exactly one focus-state record; found ", nrow(focus_state), ".", call. = FALSE)
}

focus_share <- focus_state$veteran_share[[1]]
focus_share_moe <- focus_state$veteran_share_moe[[1]]

states_ranked <- states_ranked |>
  dplyr::mutate(
    difference_from_focus_state_pp = (.data$veteran_share - focus_share) * 100,
    difference_from_focus_state_moe_pp = sqrt(
      .data$veteran_share_moe^2 + focus_share_moe^2
    ) * 100,
    statistically_different_from_focus_state_90 = dplyr::if_else(
      is.na(.data$difference_from_focus_state_pp) |
        is.na(.data$difference_from_focus_state_moe_pp),
      NA,
      abs(.data$difference_from_focus_state_pp) > .data$difference_from_focus_state_moe_pp
    ),
    comparison_with_focus_state = dplyr::case_when(
      .data$name == project_config$state_name ~ "focus_state",
      !.data$statistically_different_from_focus_state_90 ~ "not_statistically_distinguishable",
      .data$difference_from_focus_state_pp > 0 ~ "higher",
      .data$difference_from_focus_state_pp < 0 ~ "lower",
      TRUE ~ NA_character_
    )
  )

focus_state <- states_ranked |>
  dplyr::filter(.data$name == project_config$state_name)

focus_counties <- counties_ranked |>
  dplyr::filter(grepl(paste0(", ", project_config$state_name, "$"), .data$name))

focus_places <- places_ranked |>
  dplyr::filter(grepl(paste0(", ", project_config$state_name, "$"), .data$name))

top_focus_counties <- focus_counties |>
  dplyr::filter(.data$rank_eligible) |>
  stable_top_n("veteran_share", project_config$ranking_top_n, descending = TRUE)

bottom_focus_counties <- focus_counties |>
  dplyr::filter(.data$rank_eligible) |>
  stable_top_n("veteran_share", project_config$ranking_top_n, descending = FALSE)

top_focus_places <- focus_places |>
  dplyr::filter(.data$rank_eligible) |>
  stable_top_n("veteran_share", project_config$ranking_top_n, descending = TRUE)

write_csv_safely(states_ranked, file.path("outputs", "data", "states_total_veterans_ranked.csv"))
write_csv_safely(focus_state, file.path("outputs", "data", "focus_state_total_veterans.csv"))
write_csv_safely(top_focus_counties, file.path("outputs", "data", "focus_state_counties_highest_veteran_share.csv"))
write_csv_safely(bottom_focus_counties, file.path("outputs", "data", "focus_state_counties_lowest_veteran_share.csv"))
write_csv_safely(top_focus_places, file.path("outputs", "data", "focus_state_places_highest_veteran_share.csv"))

log_message("Analysis stage complete.")
