# Visualize Georgia veteran prevalence within race and ethnicity groups.

focus <- readr::read_csv(file.path("outputs", "data", "focus_state_race_ethnicity.csv"), show_col_types = FALSE) |>
  dplyr::mutate(group_label = factor(.data$group_label,
    levels = rev(race_ethnicity_groups$group_label)))
prevalence_chart <- ggplot2::ggplot(focus,
    ggplot2::aes(x = .data$veteran_prevalence_percent, y = .data$group_label)) +
  ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$veteran_prevalence_ci_lower,
    xmax = .data$veteran_prevalence_ci_upper), width = 0.18, orientation = "y", color = "gray45") +
  ggplot2::geom_point(ggplot2::aes(shape = .data$classification), size = 2.8, color = "#31572C") +
  ggplot2::scale_shape_manual(values = c(mutually_exclusive_race = 16, overlapping_race_ethnicity = 17),
    labels = c(mutually_exclusive_race = "Race category", overlapping_race_ethnicity = "Overlapping race/ethnicity lens")) +
  ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  ggplot2::labs(title = paste0("Veteran prevalence within race and ethnicity groups\nin ", project_config$state_name),
    subtitle = paste0(project_config$acs_year - 4L, "-", project_config$acs_year,
      " ACS 5-year; bars show 90% MOEs"),
    x = "Veterans as a share of each group's civilian population age 18+", y = NULL, shape = NULL,
    caption = "Source: U.S. Census Bureau ACS tables C21001A-I.\nHispanic/Latino and White-alone non-Hispanic overlap the race categories.") +
  peeblestoolbox::theme_peebles_chart(angle_x_labels = 0) +
  ggplot2::theme(legend.position = "bottom") +
  peeblestoolbox::add_peebles_watermark(project_config$output_watermark)
peeblestoolbox::save_peebles_plot(prevalence_chart, "focus_state_race_ethnicity_prevalence.png",
  folder = file.path("outputs", "charts"), height = 7.5)
log_message("Race and ethnicity prevalence visualization complete.")
