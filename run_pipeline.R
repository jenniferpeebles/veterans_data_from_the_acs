# Run the migrated Peebles Pipeline module from the repository root.

pipeline_scripts <- c(
  file.path("scripts", "00_setup.R"),
  file.path("scripts", "01_download_total_veterans.R"),
  file.path("scripts", "02_prepare_and_qa_total_veterans.R"),
  file.path("scripts", "03_analyze_total_veterans.R"),
  file.path("scripts", "04_visualize_total_veterans.R"),
  file.path("scripts", "05_write_reporter_brief.R"),
  file.path("scripts", "06_download_period_service.R"),
  file.path("scripts", "07_prepare_and_qa_period_service.R"),
  file.path("scripts", "08_analyze_period_service.R"),
  file.path("scripts", "09_visualize_period_service.R"),
  file.path("scripts", "10_write_period_service_brief.R"),
  file.path("scripts", "12_download_disability.R"),
  file.path("scripts", "13_prepare_and_qa_disability.R"),
  file.path("scripts", "14_analyze_disability.R"),
  file.path("scripts", "15_visualize_disability.R"),
  file.path("scripts", "16_write_disability_brief.R"),
  file.path("scripts", "18_download_median_income.R"),
  file.path("scripts", "19_prepare_and_qa_median_income.R"),
  file.path("scripts", "20_analyze_median_income.R"),
  file.path("scripts", "21_visualize_median_income.R"),
  file.path("scripts", "22_write_median_income_brief.R"),
  file.path("scripts", "24_download_labor_force.R"),
  file.path("scripts", "25_prepare_and_qa_labor_force.R"),
  file.path("scripts", "26_analyze_labor_force.R"),
  file.path("scripts", "27_visualize_labor_force.R"),
  file.path("scripts", "28_write_labor_force_brief.R"),
  file.path("scripts", "30_download_service_connected.R"),
  file.path("scripts", "31_prepare_and_qa_service_connected.R"),
  file.path("scripts", "32_analyze_service_connected.R"),
  file.path("scripts", "33_visualize_service_connected.R"),
  file.path("scripts", "34_write_service_connected_brief.R"),
  file.path("scripts", "36_download_race_ethnicity.R"),
  file.path("scripts", "37_prepare_and_qa_race_ethnicity.R"),
  file.path("scripts", "38_analyze_race_ethnicity.R"),
  file.path("scripts", "39_visualize_race_ethnicity.R"),
  file.path("scripts", "40_visualize_race_composition.R"),
  file.path("scripts", "41_write_race_ethnicity_brief.R"),
  file.path("scripts", "35_write_consolidated_reporter_brief.R")
)

for (script in pipeline_scripts) {
  message("\n--- Running ", script, " ---")
  source(script, local = globalenv(), echo = FALSE)
}

session_info_path <- file.path("outputs", "reports", "session_info.txt")
capture.output(sessionInfo(), file = session_info_path)
log_message("Wrote session information to ", session_info_path)

if (requireNamespace("beepr", quietly = TRUE)) {
  beepr::beep()
}

message("Pipeline completed successfully.")
