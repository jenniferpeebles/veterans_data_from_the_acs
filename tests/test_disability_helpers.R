# Offline tests for C21007 composites, MOEs, reliability flags, and identities.

source(file.path("R", "helpers.R"))
source(file.path("config", "project_config.R"))
source(file.path("R", "acs_helpers.R"))
source(file.path("R", "disability_helpers.R"))

synthetic <- data.frame(GEOID = "13", NAME = "Georgia", stringsAsFactors = FALSE)
for (cell in names(disability_variables)) {
  synthetic[[paste0(cell, "E")]] <- 0
  synthetic[[paste0(cell, "M")]] <- 1
}
leaf_values <- c(
  cell_005 = 10, cell_006 = 20, cell_008 = 30, cell_009 = 40,
  cell_012 = 50, cell_013 = 60, cell_015 = 70, cell_016 = 80,
  cell_020 = 15, cell_021 = 25, cell_023 = 35, cell_024 = 45,
  cell_027 = 55, cell_028 = 65, cell_030 = 75, cell_031 = 85
)
for (cell in names(leaf_values)) synthetic[[paste0(cell, "E")]] <- leaf_values[[cell]]
for (definition in rev(disability_identity_definitions)) {
  synthetic[[paste0(definition$parent, "E")]] <- sum(
    vapply(definition$children, function(cell) synthetic[[paste0(cell, "E")]], numeric(1))
  )
}

prepared <- prepare_disability_data(synthetic, "state", project_config)
overall <- prepared[prepared$disability_metric == "all_veterans", ]
expected_disabled <- 10 + 30 + 15 + 35
expected_denominator <- synthetic$cell_003E + synthetic$cell_018E
stopifnot(
  nrow(prepared) == length(disability_metric_definitions),
  overall$disabled_estimate == expected_disabled,
  overall$denominator_estimate == expected_denominator,
  abs(overall$disabled_moe - 2) < 1e-9,
  abs(overall$disability_share - expected_disabled / expected_denominator) < 1e-9,
  !overall$flag_missing,
  !overall$flag_impossible,
  all(qa_disability_identities(synthetic, "state")$identity_holds)
)

missing_data <- synthetic
missing_data$cell_005E <- NA_real_
missing_prepared <- prepare_disability_data(missing_data, "state", project_config)
stopifnot(missing_prepared$flag_missing[missing_prepared$disability_metric == "all_veterans"])

zero_data <- synthetic
zero_data$cell_005E <- 0
zero_data$cell_008E <- 0
zero_metric <- prepare_disability_data(zero_data, "state", project_config)
zero_age <- zero_metric[zero_metric$disability_metric == "veterans_18_64", ]
stopifnot(zero_age$flag_zero_estimate, !zero_age$rank_eligible)

message("All disability helper tests passed.")
