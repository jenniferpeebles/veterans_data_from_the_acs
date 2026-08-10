# Helpers for deterministic, reporter-facing outputs.

stable_top_n <- function(data, value_column, n, descending = TRUE) {
  direction <- if (descending) dplyr::desc else identity

  data |>
    dplyr::arrange(direction(.data[[value_column]]), .data$geoid) |>
    dplyr::slice_head(n = n)
}
write_lines_deterministically <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  normalized_lines <- enc2utf8(gsub("\r", "", lines, fixed = TRUE))
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeChar(
    paste0(paste(normalized_lines, collapse = "\n"), "\n"),
    connection,
    eos = NULL,
    useBytes = TRUE
  )
  invisible(path)
}

validate_findings_table <- function(findings) {
  required <- c(
    "finding_id", "module", "section", "display_order", "sentence",
    "estimate", "moe", "unit", "geography", "geoid"
  )
  missing <- setdiff(required, names(findings))
  if (length(missing) > 0) {
    stop("Findings table missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(findings$finding_id)) {
    stop("Findings table contains duplicate finding_id values.", call. = FALSE)
  }
  invisible(TRUE)
}

write_findings_csv <- function(findings, path) {
  validate_findings_table(findings)
  ordered <- findings |>
    dplyr::arrange(.data$display_order, .data$finding_id)
  readr::write_csv(ordered, path, na = "", eol = "\n")
  invisible(path)
}
