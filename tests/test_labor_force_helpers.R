# Offline tests for B21005 identities, rates, MOEs, gaps, and denominators.

source(file.path("R", "helpers.R"))
source(file.path("config", "project_config.R"))
source(file.path("R", "acs_helpers.R"))
source(file.path("R", "labor_force_helpers.R"))

synthetic <- data.frame(GEOID = "13", NAME = "Georgia", stringsAsFactors = FALSE)
for (cell in names(labor_force_variables)) {
  synthetic[[paste0(cell, "E")]] <- 0
  synthetic[[paste0(cell, "M")]] <- 1
}
leaf_values <- c(
  cell_005 = 60, cell_006 = 5, cell_007 = 35,
  cell_010 = 70, cell_011 = 10, cell_012 = 20,
  cell_016 = 80, cell_017 = 4, cell_018 = 16,
  cell_021 = 75, cell_022 = 5, cell_023 = 20,
  cell_027 = 50, cell_028 = 5, cell_029 = 45,
  cell_032 = 60, cell_033 = 5, cell_034 = 35
)
for (cell in names(leaf_values)) synthetic[[paste0(cell, "E")]] <- leaf_values[[cell]]
age_identities <- labor_force_identity_definitions[names(labor_force_identity_definitions) != "total_age"]
for (definition in rev(age_identities)) {
  synthetic[[paste0(definition$parent, "E")]] <- sum(
    vapply(definition$children, function(cell) synthetic[[paste0(cell, "E")]], numeric(1))
  )
}
total_definition <- labor_force_identity_definitions$total_age
synthetic[[paste0(total_definition$parent, "E")]] <- sum(
  vapply(total_definition$children, function(cell) synthetic[[paste0(cell, "E")]], numeric(1))
)

prepared <- prepare_labor_force_data(synthetic, "state", project_config)
overall_participation <- prepared[
  prepared$age_stratum == "all_18_64" & prepared$labor_metric == "participation_rate",
]
overall_unemployment <- prepared[
  prepared$age_stratum == "all_18_64" & prepared$labor_metric == "unemployment_rate",
]
expected_veteran_labor <- 65 + 84 + 55
expected_veteran_population <- 100 + 100 + 100
expected_veteran_unemployed <- 5 + 4 + 5
stopifnot(
  nrow(prepared) == 12,
  abs(overall_participation$veteran_rate - expected_veteran_labor / expected_veteran_population) < 1e-9,
  abs(overall_unemployment$veteran_rate - expected_veteran_unemployed / expected_veteran_labor) < 1e-9,
  overall_unemployment$veteran_denominator_estimate == expected_veteran_labor,
  !overall_participation$flag_missing,
  !overall_participation$flag_impossible,
  all(qa_labor_force_identities(synthetic, "state")$identity_holds)
)

missing <- synthetic
missing$cell_005E <- NA_real_
missing_result <- prepare_labor_force_data(missing, "state", project_config)
stopifnot(any(missing_result$flag_missing))

message("All labor-force helper tests passed.")
