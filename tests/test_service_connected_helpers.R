# Offline tests for B21100 identities, denominators, MOEs, and missingness.

source(file.path("R", "helpers.R"))
source(file.path("config", "project_config.R"))
source(file.path("R", "acs_helpers.R"))
source(file.path("R", "service_connected_helpers.R"))

synthetic <- data.frame(GEOID = "13", NAME = "Georgia", stringsAsFactors = FALSE)
estimates <- c(
  veteran_total = 1000, no_rating = 700, any_rating = 300,
  rating_0 = 10, rating_10_20 = 40, rating_30_40 = 60,
  rating_50_60 = 70, rating_70_plus = 100, rating_not_reported = 20
)
for (variable in names(estimates)) {
  synthetic[[paste0(variable, "E")]] <- estimates[[variable]]
  synthetic[[paste0(variable, "M")]] <- 10
}
prepared <- prepare_service_connected_data(synthetic, "state", project_config)
any_rating <- prepared[prepared$rating_metric == "any_rating", ]
rating_70 <- prepared[prepared$rating_metric == "rating_70_plus", ]
stopifnot(
  nrow(prepared) == 7,
  abs(any_rating$primary_share - 0.30) < 1e-9,
  abs(rating_70$primary_share - 1 / 3) < 1e-9,
  abs(rating_70$share_all_veterans - 0.10) < 1e-9,
  rating_70$denominator_variable == "B21100_003",
  all(qa_service_connected_identities(synthetic, "state")$identity_holds),
  !any_rating$flag_missing,
  any_rating$rank_eligible
)

missing <- synthetic
missing$rating_70_plusE <- NA_real_
missing_result <- prepare_service_connected_data(missing, "state", project_config)
stopifnot(missing_result$flag_missing[missing_result$rating_metric == "rating_70_plus"])

message("All service-connected rating helper tests passed.")
