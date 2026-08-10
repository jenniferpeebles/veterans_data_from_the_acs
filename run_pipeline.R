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
  file.path("scripts", "17_write_consolidated_reporter_brief.R")
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
