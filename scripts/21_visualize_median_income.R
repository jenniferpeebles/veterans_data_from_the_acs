# Compare Georgia veteran and nonveteran median incomes with 90% MOEs.

chart_data <- readr::read_csv(
  file.path("outputs", "data", "focus_state_median_income.csv"), show_col_types = FALSE
) |>
  dplyr::select(
    "income_stratum", "income_stratum_label",
    "veteran_median_income", "veteran_median_income_moe",
    "nonveteran_median_income", "nonveteran_median_income_moe"
  ) |>
  tidyr::pivot_longer(
    cols = c("veteran_median_income", "nonveteran_median_income"),
    names_to = "veteran_status", values_to = "median_income"
  ) |>
  dplyr::mutate(
    median_income_moe = dplyr::if_else(
      .data$veteran_status == "veteran_median_income",
      .data$veteran_median_income_moe, .data$nonveteran_median_income_moe
    ),
    veteran_status = dplyr::recode(
      .data$veteran_status,
      veteran_median_income = "Veteran", nonveteran_median_income = "Nonveteran"
    ),
    income_stratum_label = factor(
      .data$income_stratum_label,
      levels = rev(vapply(median_income_strata, `[[`, character(1), "label"))
    )
  )

income_chart <- ggplot2::ggplot(
  chart_data,
  ggplot2::aes(x = .data$median_income, y = .data$income_stratum_label, color = .data$veteran_status)
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      xmin = pmax(0, .data$median_income - .data$median_income_moe),
      xmax = .data$median_income + .data$median_income_moe
    ), width = 0.18, orientation = "y", position = ggplot2::position_dodge(width = 0.45)
  ) +
  ggplot2::geom_point(size = 2.8, position = ggplot2::position_dodge(width = 0.45)) +
  ggplot2::scale_x_continuous(labels = scales::label_dollar()) +
  ggplot2::labs(
    title = paste("Median income by veteran status in", project_config$state_name),
    subtitle = paste0(project_config$acs_year - 4L, "-", project_config$acs_year,
      " ACS 5-year; ", project_config$acs_year, " inflation-adjusted dollars; bars show 90% MOEs"),
    x = "Median income in the past 12 months", y = NULL, color = NULL,
    caption = "Source: U.S. Census Bureau ACS table B21004 via tidycensus. Universe: civilian population age 18 and older with income."
  ) +
  peeblestoolbox::theme_peebles_chart(angle_x_labels = 0) +
  peeblestoolbox::add_peebles_watermark(project_config$output_watermark)

peeblestoolbox::save_peebles_plot(
  income_chart, "focus_state_median_income_by_veteran_status.png",
  folder = file.path("outputs", "charts"), height = 6.5
)
log_message("Median-income visualization stage complete.")
