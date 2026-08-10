# Offline tests for B21002 composites, MOEs, missingness, and QA identities.

source(file.path("R", "helpers.R"))
source(file.path("config", "project_config.R"))
source(file.path("R", "acs_helpers.R"))
source(file.path("R", "period_service_helpers.R"))

component_names <- setdiff(names(period_service_variables), "veteran_total")
component_estimates <- seq_along(component_names)
synthetic <- data.frame(GEOID = "13", NAME = "Georgia", stringsAsFactors = FALSE)
synthetic$veteran_totalE <- sum(component_estimates)
synthetic$veteran_totalM <- 8
for (index in seq_along(component_names)) {
  synthetic[[paste0(component_names[[index]], "E")]] <- component_estimates[[index]]
  synthetic[[paste0(component_names[[index]], "M")]] <- 1
}

adult <- data.frame(
  geoid = "13",
  population_18_plus_estimate = 1000,
  population_18_plus_moe = 20
)
prepared <- prepare_period_service_data(synthetic, "state", adult, project_config)
post2001 <- prepared[prepared$period_metric == "sep2001_or_later_any", ]

stopifnot(
  nrow(prepared) == 5,
  post2001$estimate == sum(component_estimates[1:3]),
  abs(post2001$moe - sqrt(3)) < 1e-9,
  !post2001$flag_missing,
  !post2001$flag_impossible,
  !post2001$flag_zero_estimate,
  all(qa_period_components(synthetic, "state")$components_match_total)
)

synthetic_missing <- synthetic
synthetic_missing$sep2001_or_later_onlyE <- NA_real_
missing_sum <- derive_period_sum(
  synthetic_missing,
  period_service_definitions$sep2001_or_later_any
)
stopifnot(is.na(missing_sum$estimate), is.na(missing_sum$moe))

zero_synthetic <- synthetic
for (component_name in period_service_definitions$wwii_any) {
  zero_synthetic[[paste0(component_name, "E")]] <- 0
}
zero_synthetic$veteran_totalE <- sum(
  unlist(zero_synthetic[paste0(component_names, "E")], use.names = FALSE)
)
zero_prepared <- prepare_period_service_data(zero_synthetic, "state", adult, project_config)
zero_wwii <- zero_prepared[zero_prepared$period_metric == "wwii_any", ]
stopifnot(zero_wwii$flag_zero_estimate, !zero_wwii$rank_eligible)

message("All period-of-service helper tests passed.")
