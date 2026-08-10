# Visualize disability prevalence among focus-state veterans.

chart_data <- readr::read_csv(
  file.path("outputs", "data", "focus_state_disability.csv"), show_col_types = FALSE
) |>
  dplyr::filter(.data$disability_metric %in% disability_headline_metrics) |>
  dplyr::mutate(disability_label = stats::reorder(.data$disability_label, .data$disability_percent))

disability_chart <- ggplot2::ggplot(
  chart_data, ggplot2::aes(x = .data$disability_percent, y = .data$disability_label)
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = .data$disability_percent_ci_lower, xmax = .data$disability_percent_ci_upper),
    width = 0.2, orientation = "y", color = "gray45"
  ) +
  ggplot2::geom_point(size = 2.8, color = "#5B4B8A") +
  ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  ggplot2::labs(
    title = paste("Disability prevalence among", project_config$state_name, "veterans"),
    subtitle = paste0(project_config$acs_year - 4L, "-", project_config$acs_year,
      " ACS 5-year; bars show 90% margins of error"),
    x = "Share with a disability", y = NULL,
    caption = "Source: U.S. Census Bureau ACS table C21007 via tidycensus. Poverty universe is people for whom poverty status is determined."
  ) +
  peeblestoolbox::theme_peebles_chart(angle_x_labels = 0) +
  peeblestoolbox::add_peebles_watermark(project_config$output_watermark)

peeblestoolbox::save_peebles_plot(
  disability_chart, "focus_state_veteran_disability.png",
  folder = file.path("outputs", "charts"), height = 6.5
)
log_message("Disability visualization stage complete.")
