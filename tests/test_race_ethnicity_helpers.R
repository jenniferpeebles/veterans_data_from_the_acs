# Offline tests for C21001A-I identities, denominators, overlap metadata, and MOEs.

source(file.path("R", "helpers.R"))
source(file.path("config", "project_config.R"))
source(file.path("R", "acs_helpers.R"))
source(file.path("R", "race_ethnicity_helpers.R"))
synthetic <- data.frame(GEOID = "13", NAME = "Georgia", stringsAsFactors = FALSE)
pattern <- c(total = 100, male = 50, male_18_64 = 40, male_18_64_veteran = 4,
  male_18_64_nonveteran = 36, male_65_plus = 10, male_65_plus_veteran = 5,
  male_65_plus_nonveteran = 5, female = 50, female_18_64 = 40,
  female_18_64_veteran = 2, female_18_64_nonveteran = 38,
  female_65_plus = 10, female_65_plus_veteran = 3, female_65_plus_nonveteran = 7)
for (code in LETTERS[1:9]) for (cell in names(pattern)) {
  synthetic[[paste0(tolower(code), "_", cell, "E")]] <- pattern[[cell]]
  synthetic[[paste0(tolower(code), "_", cell, "M")]] <- 2
}
totals <- data.frame(geoid = "13", veterans_estimate = 98, veterans_moe = 8)
prepared <- prepare_race_ethnicity_data(synthetic, totals, "state", project_config)
stopifnot(nrow(prepared) == 9, all(qa_race_ethnicity_identities(synthetic, "state")$identity_holds),
  all(abs(prepared$veteran_prevalence - 0.14) < 1e-9),
  all(abs(prepared$veteran_composition - 1 / 7) < 1e-9),
  all(prepared$classification[1:7] == "mutually_exclusive_race"),
  all(prepared$classification[8:9] == "overlapping_race_ethnicity"))
message("All race and ethnicity helper tests passed.")
