# Lightweight tests that do not require an API key or network connection.

source(file.path("R", "helpers.R"))
source(file.path("config", "project_config.R"))
source(file.path("R", "acs_helpers.R"))

synthetic <- data.frame(
  GEOID = c("01", "02", "03"),
  NAME = c("Alpha", "Beta", "Gamma"),
  population_18_plusE = c(1000, 500, 0),
  population_18_plusM = c(40, 30, 0),
  veteransE = c(100, 25, 0),
  veteransM = c(15, 20, 0)
)

prepared <- prepare_total_veteran_data(synthetic, "state", project_config)

stopifnot(
  identical(prepared$geoid, c("01", "02", "03")),
  abs(prepared$veteran_percent[[1]] - 10) < 1e-9,
  is.na(prepared$veteran_percent[[3]]),
  prepared$flag_nonpositive_denominator[[3]],
  prepared$flag_high_cv[[2]],
  !prepared$rank_eligible[[2]],
  !any(prepared$flag_impossible_estimate)
)

ranked <- rank_veteran_share(prepared)
stopifnot(ranked$veteran_share_rank[[1]] == 1, is.na(ranked$veteran_share_rank[[2]]))

message("All ACS helper tests passed.")
