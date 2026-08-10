# Visualize overlapping service-period composites for the focus state.

chart_data <- readr::read_csv(
  file.path("outputs", "data", "focus_state_period_service.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(
    period_label = stats::reorder(.data$period_label, .data$percent_of_veterans)
  )

period_chart <- ggplot2::ggplot(
  chart_data,
  ggplot2::aes(x = .data$percent_of_veterans, y = .data$period_label)
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      xmin = pmax(0, .data$percent_of_veterans - .data$percent_of_veterans_moe),
      xmax = pmin(100, .data$percent_of_veterans + .data$percent_of_veterans_moe)
    ),
    width = 0.2,
    orientation = "y",
    color = "gray45"
  ) +
  ggplot2::geom_point(size = 2.8, color = "#D1495B") +
  ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  ggplot2::labs(
    title = paste("Military-service periods among", project_config$state_name, "veterans"),
    subtitle = paste0(
      project_config$acs_year - 4L, "-", project_config$acs_year,
      " ACS 5-year; overlapping periods; bars show 90% MOEs"
    ),
    x = "Share of civilian veterans age 18 and older",
    y = NULL,
    caption = paste(
      "Source: U.S. Census Bureau ACS table B21002 via tidycensus.",
      "Periods overlap; percentages do not sum to 100%."
    )
  ) +
  peeblestoolbox::theme_peebles_chart(angle_x_labels = 0) +
  peeblestoolbox::add_peebles_watermark(project_config$output_watermark)

peeblestoolbox::save_peebles_plot(
  period_chart,
  "focus_state_period_service.png",
  folder = file.path("outputs", "charts"),
  height = 6.5
)
log_message("Period-of-service visualization stage complete.")
