# Verify stable ordering and byte-for-byte deterministic output.

source(file.path("R", "helpers.R"))
source(file.path("R", "reporter_brief_helpers.R"))

unsorted <- data.frame(
  geoid = c("003", "001", "002"),
  value = c(10, 10, 9)
)
stable <- stable_top_n(unsorted, "value", 2, descending = TRUE)
stopifnot(identical(stable$geoid, c("001", "003")))

lines <- c("# Brief", "", "- Same inputs produce the same bytes.")
path_one <- tempfile(fileext = ".md")
path_two <- tempfile(fileext = ".md")
write_lines_deterministically(lines, path_one)
write_lines_deterministically(lines, path_two)
stopifnot(identical(readBin(path_one, "raw", n = file.info(path_one)$size), readBin(path_two, "raw", n = file.info(path_two)$size)))

findings <- dplyr::tibble(
  finding_id = c("b", "a"),
  module = "test",
  section = "test",
  display_order = c(2L, 1L),
  sentence = c("Second", "First"),
  estimate = c(2, 1),
  moe = c(0.2, 0.1),
  unit = "test",
  geography = "test",
  geoid = c("002", "001")
)
csv_one <- tempfile(fileext = ".csv")
csv_two <- tempfile(fileext = ".csv")
write_findings_csv(findings, csv_one)
write_findings_csv(findings, csv_two)
stopifnot(identical(readBin(csv_one, "raw", n = file.info(csv_one)$size), readBin(csv_two, "raw", n = file.info(csv_two)$size)))

message("All reporter-brief determinism tests passed.")
