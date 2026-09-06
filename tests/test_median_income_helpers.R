# Offline tests for B21004 gaps, MOEs, significance, and reliability.

source(file.path("R", "helpers.R"))
source(file.path("config", "project_config.R"))
source(file.path("R", "acs_helpers.R"))
source(file.path("R", "median_income_helpers.R"))

synthetic <- data.frame(GEOID = "13", NAME = "Georgia", stringsAsFactors = FALSE)
values <- c(
  total = 50000, veteran_total = 60000, veteran_male = 65000, veteran_female = 52000,
  nonveteran_total = 48000, nonveteran_male = 54000, nonveteran_female = 43000
)
for (variable in names(values)) {
  synthetic[[paste0(variable, "E")]] <- values[[variable]]
  synthetic[[paste0(variable, "M")]] <- 1000
}
prepared <- prepare_median_income_data(synthetic, "state", project_config)
overall <- prepared[prepared$income_stratum == "total", ]
stopifnot(
  nrow(prepared) == 3,
  overall$income_gap == 12000,
  abs(overall$income_gap_moe - sqrt(2 * 1000^2)) < 1e-9,
  overall$income_gap_significant_90,
  !overall$flag_missing,
  !overall$flag_nonpositive,
  !overall$flag_invalid_moe,
  overall$rank_eligible
)

not_significant <- synthetic
not_significant$veteran_totalE <- 48500
not_significant_result <- prepare_median_income_data(not_significant, "state", project_config)
stopifnot(!not_significant_result$income_gap_significant_90[not_significant_result$income_stratum == "total"])

missing <- synthetic
missing$veteran_totalE <- NA_real_
missing_result <- prepare_median_income_data(missing, "state", project_config)
stopifnot(missing_result$flag_missing[missing_result$income_stratum == "total"])

message("All median-income helper tests passed.")
