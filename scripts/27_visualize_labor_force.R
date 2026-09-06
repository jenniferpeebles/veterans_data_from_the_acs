# Compare Georgia veteran and nonveteran labor-force rates with 90% MOEs.

chart_data <- readr::read_csv(
  file.path("outputs", "data", "focus_state_labor_force.csv"), show_col_types = FALSE
) |>
  dplyr::filter(.data$age_stratum == "all_18_64") |>
  dplyr::select(
    "labor_metric", "labor_metric_label", "veteran_percent", "veteran_percent_moe",
    "nonveteran_percent", "nonveteran_percent_moe"
  ) |>
  tidyr::pivot_longer(
    cols = c("veteran_percent", "nonveteran_percent"),
    names_to = "veteran_status", values_to = "percent"
  ) |>
  dplyr::mutate(
    percent_moe = dplyr::if_else(.data$veteran_status == "veteran_percent",
      .data$veteran_percent_moe, .data$nonveteran_percent_moe),
    veteran_status = dplyr::recode(.data$veteran_status,
      veteran_percent = "Veteran", nonveteran_percent = "Nonveteran"),
    labor_metric_label = factor(.data$labor_metric_label,
      levels = rev(vapply(labor_force_metric_definitions, `[[`, character(1), "label")))
  )

labor_chart <- ggplot2::ggplot(
  chart_data, ggplot2::aes(x = .data$percent, y = .data$labor_metric_label, color = .data$veteran_status)
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = pmax(0, .data$percent - .data$percent_moe), xmax = pmin(100, .data$percent + .data$percent_moe)),
    width = 0.18, orientation = "y", position = ggplot2::position_dodge(width = 0.45)
  ) +
  ggplot2::geom_point(size = 2.8, position = ggplot2::position_dodge(width = 0.45)) +
  ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  ggplot2::labs(
    title = paste("Labor-force outcomes by veteran status in", project_config$state_name),
    subtitle = paste0(project_config$acs_year - 4L, "-", project_config$acs_year,
      " ACS 5-year, ages 18-64; bars show 90% MOEs"),
    x = "Rate", y = NULL, color = NULL,
    caption = paste(
      "Source: U.S. Census Bureau ACS table B21005 via tidycensus.",
      "Unemployment denominator is the labor force; other denominators are the civilian population."
    )
  ) +
  peeblestoolbox::theme_peebles_chart(angle_x_labels = 0) +
  peeblestoolbox::add_peebles_watermark(project_config$output_watermark)

peeblestoolbox::save_peebles_plot(
  labor_chart, "focus_state_labor_force_by_veteran_status.png",
  folder = file.path("outputs", "charts"), height = 6.5
)
log_message("Labor-force visualization stage complete.")
