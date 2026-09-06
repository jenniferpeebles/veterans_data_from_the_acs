# Visualize Georgia service-connected rating distribution among rated veterans.

chart_data <- readr::read_csv(
  file.path("outputs", "data", "focus_state_service_connected.csv"), show_col_types = FALSE
) |>
  dplyr::filter(.data$rating_metric != "any_rating") |>
  dplyr::mutate(
    rating_label = factor(.data$rating_label, levels = rev(unname(service_connected_rating_labels)))
  )

rating_chart <- ggplot2::ggplot(
  chart_data, ggplot2::aes(x = .data$primary_percent, y = .data$rating_label)
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = .data$primary_percent_ci_lower, xmax = .data$primary_percent_ci_upper),
    width = 0.18, orientation = "y", color = "gray45"
  ) +
  ggplot2::geom_point(size = 2.8, color = "#9C6644") +
  ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  ggplot2::labs(
    title = paste("Service-connected disability ratings among rated veterans in", project_config$state_name),
    subtitle = paste0(project_config$acs_year - 4L, "-", project_config$acs_year,
      " ACS 5-year; categories sum to 100%; bars show 90% MOEs"),
    x = "Share of veterans with a service-connected disability rating", y = NULL,
    caption = "Source: U.S. Census Bureau ACS table B21100 via tidycensus. Rating-not-reported is retained as a published category."
  ) +
  peeblestoolbox::theme_peebles_chart(angle_x_labels = 0) +
  peeblestoolbox::add_peebles_watermark(project_config$output_watermark)

peeblestoolbox::save_peebles_plot(
  rating_chart, "focus_state_service_connected_rating_distribution.png",
  folder = file.path("outputs", "charts"), height = 6.5
)
log_message("Service-connected rating visualization stage complete.")
