# Visualize the mutually exclusive race composition of Georgia veterans.

race_composition <- readr::read_csv(file.path("outputs", "data", "focus_state_race_ethnicity.csv"),
  show_col_types = FALSE) |>
  dplyr::filter(.data$classification == "mutually_exclusive_race") |>
  dplyr::mutate(group_label = factor(.data$group_label,
    levels = rev(race_ethnicity_groups$group_label[1:7])))
composition_chart <- ggplot2::ggplot(race_composition,
    ggplot2::aes(x = .data$veteran_composition_percent, y = .data$group_label)) +
  ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$veteran_composition_ci_lower,
    xmax = .data$veteran_composition_ci_upper), width = 0.18, orientation = "y", color = "gray45") +
  ggplot2::geom_point(size = 2.8, color = "#7F5539") +
  ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  ggplot2::labs(title = paste("Race composition of civilian veterans in", project_config$state_name),
    subtitle = paste0(project_config$acs_year - 4L, "-", project_config$acs_year,
      " ACS 5-year; A-G race categories sum to 100%; bars show 90% MOEs"),
    x = "Share of all civilian veterans age 18+", y = NULL,
    caption = "Source: U.S. Census Bureau ACS tables C21001A-G.\nHispanic/Latino is excluded here because ethnicity overlaps race.") +
  peeblestoolbox::theme_peebles_chart(angle_x_labels = 0) +
  peeblestoolbox::add_peebles_watermark(project_config$output_watermark)
peeblestoolbox::save_peebles_plot(composition_chart, "focus_state_veteran_race_composition.png",
  folder = file.path("outputs", "charts"), height = 6.5)
log_message("Veteran race-composition visualization complete.")
