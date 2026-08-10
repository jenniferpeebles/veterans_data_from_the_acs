# Veterans data from the American Community Survey

This project turns U.S. Census Bureau American Community Survey data into
reproducible, uncertainty-aware reporting material about veteran populations.
It is being rebuilt around the Peebles Pipeline principle: **findings before
figures, QA before conclusions, and reusable assets instead of one-off output.**

## Modernization status

The original 2021 report remains in `r_script` as a legacy reference. Its first
three modules—total veteran population from B21001, period of military service
from B21002, and veteran disability prevalence from C21007—have been migrated
into a multi-script pipeline. The remaining
legacy subjects will be migrated in later
increments:

- median income by veteran status;
- labor-force status;
- service-connected disability ratings; and
- veteran status by race and ethnicity.

Do not treat the legacy report as publication-ready. Among other limitations,
it discarded ACS margins of error. The migrated pipeline preserves uncertainty
and applies explicit QA rules.

## What the migrated module produces

- Raw ACS estimates and margins of error for the U.S., states, counties, and
  places.
- A processed analysis table containing estimates, MOEs, standard errors,
  coefficients of variation, relative MOEs, and 90% confidence intervals.
- QA summaries and exception tables.
- A field-level data dictionary and source-vintage metadata.
- Reliability-aware rankings that exclude missing, impossible, zero-denominator,
  and high-CV observations.
- A state comparison table that separates descriptive rank from statistically
  distinguishable differences.
- Review charts that display uncertainty.
- `reporter_brief.md` and `reporter_brief.csv`.
- Deterministic module briefs and findings tables for audit and reuse.
- A run log and R session information.

No missing or suppressed value is imputed.

## Configure the analysis

Reporter-controlled settings are together in `config/project_config.R`:

- ACS year and survey;
- focus state;
- minimum place population;
- number of ranked observations;
- high-CV threshold;
- MOE confidence level; and
- draft watermark text.

These settings belong in tracked code. Do not put ordinary project settings in
`.Renviron`, where an invisible local override could change a published result.

The current default is the 2024 five-year ACS. The year is fixed explicitly so
an updated package default cannot silently change the analysis vintage.

## Census API key

A free Census API key is required. Request one at
<https://api.census.gov/data/key_signup.html>.

Open your user-level `.Renviron` file from the R console:

```r
usethis::edit_r_environ(scope = "user")
```

Add the following line, replacing the placeholder with your key:

```text
CENSUS_API_KEY=YOUR_ACTUAL_KEY_HERE
```

Save the file and restart R. Never paste the key into a tracked script. This
repository's `.gitignore` excludes `.Renviron` and common `.env` variants.

More information: <https://walker-data.com/tidycensus/reference/census_api_key.html>

## Required R packages

The migrated module uses:

```r
install.packages(c(
  "dplyr",
  "ggplot2",
  "readr",
  "scales",
  "tidycensus",
  "tidyr"
))
```

It also uses the public `peeblestoolbox` package for chart themes, watermarks,
and plot export. Install the tagged release used by this project:

```r
install.packages("pak")
pak::pak("jenniferpeebles/peeblestoolbox@v0.1.0")
```

## Run the pipeline

Open R in the repository root and run:

```r
source("run_pipeline.R")
```

The pipeline stops early if the working directory, required packages, API key,
expected ACS columns, duplicate GEOIDs, or logical consistency checks fail.
Each stage prints diagnostics and appends to `logs/pipeline.log`.

## Project structure

```text
config/project_config.R              Reporter-controlled settings
R/helpers.R                          General project helpers
R/acs_helpers.R                      ACS/MOE/reliability helpers
R/reporter_brief_helpers.R           Stable ordering and byte-stable writers
scripts/00_setup.R                   Environment checks
scripts/01_download_total_veterans.R Acquisition
scripts/02_prepare_and_qa_total_veterans.R
                                      Processing and QA gate
scripts/03_analyze_total_veterans.R  Rankings and comparisons
scripts/04_visualize_total_veterans.R
                                      Uncertainty-aware chart
scripts/05_write_reporter_brief.R    Story-facing findings
scripts/06_download_period_service.R B21002 acquisition
scripts/07_prepare_and_qa_period_service.R
                                      Composite MOEs and identity checks
scripts/08_analyze_period_service.R  Reliability-aware period rankings
scripts/09_visualize_period_service.R
                                      Overlapping-period chart
scripts/10_write_period_service_brief.R
                                      Period-service reporting brief
scripts/12_download_disability.R      C21007 acquisition
scripts/13_prepare_and_qa_disability.R
                                      Composite MOEs and table identities
scripts/14_analyze_disability.R       Reliability-aware disability rankings
scripts/15_visualize_disability.R     Disability-prevalence MOE chart
scripts/16_write_disability_brief.R   Deterministic disability brief
scripts/17_write_consolidated_reporter_brief.R
                                      Canonical cross-module brief
tests/test_acs_helpers.R             Offline synthetic-data checks
tests/test_period_service_helpers.R  Composite/MOE/missingness tests
tests/test_disability_helpers.R      C21007 composite/MOE/identity tests
tests/test_reporter_brief_helpers.R  Byte-for-byte determinism tests
run_pipeline.R                       Pipeline runner
r_script                             Legacy report; not publication-ready
```

Regenerable data, outputs, charts, reports, and logs are ignored by Git. Empty
directory markers remain tracked so a fresh clone has the expected structure.

## Statistical interpretation

ACS values are estimates, not exact population counts. The pipeline retains the
ACS 90% MOE and derives the MOE for the veteran proportion with
`tidycensus::moe_prop()`. Standard errors are calculated from the configured
confidence level. The coefficient of variation is the standard error divided by
the estimate.

The default `high_cv_threshold` is 0.30. Observations above that threshold are
retained and flagged but excluded from rankings. This is a transparent project
QA rule, not an official Census Bureau cutoff. A descriptive rank does not prove
that two estimates differ statistically. State comparisons therefore include
the MOE of the difference and a separate 90% significance indicator.

Period-of-service composites use the mutually exclusive cells in B21002.
Composite MOEs use `tidycensus::moe_sum()` and percentage MOEs use
`tidycensus::moe_prop()`. The five headline periods overlap; a veteran can be
represented in more than one period, so the composites cannot be added and do
not form a 100% distribution.

Confidence intervals are bounded to the logical range of 0% to 100% for
presentation. The underlying estimate and MOE remain unchanged.

Disability prevalence uses C21007 and is reported among veterans overall and
within explicit age and poverty-status denominators. Composite counts retain
quadrature MOEs, and percentage MOEs use `tidycensus::moe_prop()`. C21007's
disability concept is not a service-connected disability rating. The pipeline
verifies all 15 published parent/child table identities before producing
findings.

## Deterministic reporter briefs

Reporter briefs are deterministic for a fixed set of input files and project
settings. They contain the ACS data vintage rather than the wall-clock run time.
Run timestamps belong in `logs/pipeline.log` and source metadata, not in the
substantive findings.

Ranked examples are sorted by the requested statistic and then by GEOID, which
provides a stable secondary key when estimates tie. Module Markdown and CSV
outputs are generated from standardized findings tables. The consolidated
`reporter_brief.md` and `reporter_brief.csv` are generated from those same
ordered rows. Automated tests verify byte-for-byte repeatability.

## Geography and comparability

GEOIDs are preserved as character values. Every output identifies the ACS year
and survey. Before making longitudinal comparisons, reporters must check for
changes in Census geography, table definitions, ACS methodology, annexations,
and metropolitan-area delineations. These first modules are cross-sectional and
does not claim longitudinal comparability.

## Source

U.S. Census Bureau American Community Survey, accessed through
[`tidycensus`](https://walker-data.com/tidycensus/).
