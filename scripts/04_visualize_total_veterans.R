# Build an uncertainty-aware review chart for the focus state's counties.

chart_data <- readr::read_csv(
  file.path("outputs", "data", "focus_state_counties_highest_veteran_share.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(
    county = sub(paste0(" County, ", project_config$state_name, "$"), "", .data$name),
    county = stats::reorder(.data$county, .data$veteran_percent)
  )

veteran_share_chart <- ggplot2::ggplot(
  chart_data,
  ggplot2::aes(x = .data$veteran_percent, y = .data$county)
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      xmin = pmax(0, .data$veteran_percent - .data$veteran_percent_moe),
      xmax = .data$veteran_percent + .data$veteran_percent_moe
    ),
    width = 0.2,
    orientation = "y",
    color = "gray45"
  ) +
  ggplot2::geom_point(size = 2.6, color = "#D1495B") +
  ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  ggplot2::labs(
    title = paste("Counties with the highest estimated veteran shares in", project_config$state_name),
    subtitle = paste0(
      project_config$acs_year, " ", project_config$acs_survey,
      "; bars show 90% margins of error; high-CV estimates excluded"
    ),
    x = "Veterans as a share of the civilian population age 18 and older",
    y = NULL,
    caption = "Source: U.S. Census Bureau American Community Survey via tidycensus"
  ) +
  peeblestoolbox::theme_peebles_chart(angle_x_labels = 0) +
  peeblestoolbox::add_peebles_watermark(project_config$output_watermark)

peeblestoolbox::save_peebles_plot(
  veteran_share_chart,
  "focus_state_counties_veteran_share.png",
  folder = file.path("outputs", "charts")
)
log_message("Wrote chart to outputs/charts/focus_state_counties_veteran_share.png")
log_message("Visualization stage complete.")
