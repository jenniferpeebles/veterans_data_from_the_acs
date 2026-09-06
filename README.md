# Veterans data from the American Community Survey

This project turns U.S. Census Bureau American Community Survey data into
reproducible, uncertainty-aware reporting material about veteran populations.

## Modernization status

This project started out as one long script that looked at multiple veterans-related data points in the ACS. Its seven
subjects -- total veteran population from B21001, period of military service
from B21002, veteran disability prevalence from C21007, median income by
veteran status from B21004, labor-force status from B21005, service-connected
disability ratings from B21100, and race/ethnicity from C21001A-I -- have been
migrated into a multi-script pipeline here.

- Raw ACS estimates and margins of error for the U.S., states, counties, and
  places.
- A processed analysis table containing estimates, MOEs, standard errors,
  coefficients of variation, relative MOEs, and 90 percent confidence intervals.
- Quqlity Assurance summaries and exception tables.
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

These settings belong in tracked code. Don't put ordinary project settings in
`.Renviron`, where an invisible local override could change a published result.

The current default is the 2024 five-year ACS. The year is fixed explicitly so an updated package default cannot silently change the analysis vintage.

## Census API key

A free Census API key is required. Request one at
<https://api.census.gov/data/key_signup.html>.

You'll want to put your key in your .Renviron file and not in these scripts. To do that, open your user-level `.Renviron` file from the R console:

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
R/median_income_helpers.R            B21004 median, gap and significance helpers
R/labor_force_helpers.R              B21005 rate, MOE and denominator helpers
R/service_connected_helpers.R        B21100 rating and denominator helpers
R/race_ethnicity_helpers.R           C21001A-I overlap, MOE and QA helpers
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
scripts/18_download_median_income.R   B21004 acquisition
scripts/19_prepare_and_qa_median_income.R
                                      Median gaps, MOEs and QA
scripts/20_analyze_median_income.R    State and county income rankings
scripts/21_visualize_median_income.R  Veteran/nonveteran median chart
scripts/22_write_median_income_brief.R
                                      Deterministic median-income brief
scripts/24_download_labor_force.R     B21005 acquisition
scripts/25_prepare_and_qa_labor_force.R
                                      Rates, composite MOEs and identities
scripts/26_analyze_labor_force.R      State and county labor rankings
scripts/27_visualize_labor_force.R    Veteran/nonveteran labor chart
scripts/28_write_labor_force_brief.R  Deterministic labor-force brief
scripts/30_download_service_connected.R
                                      B21100 acquisition
scripts/31_prepare_and_qa_service_connected.R
                                      Rating shares, MOEs and identities
scripts/32_analyze_service_connected.R
                                      State and county rating rankings
scripts/33_visualize_service_connected.R
                                      Rating-distribution chart
scripts/34_write_service_connected_brief.R
                                      Deterministic rating brief
scripts/36_download_race_ethnicity.R C21001A-I acquisition
scripts/37_prepare_and_qa_race_ethnicity.R
                                      Prevalence, composition, MOEs and identities
scripts/38_analyze_race_ethnicity.R  State and county rankings
scripts/39_visualize_race_ethnicity.R
                                      Within-group veteran-prevalence chart
scripts/40_visualize_race_composition.R
                                      Mutually exclusive race-composition chart
scripts/41_write_race_ethnicity_brief.R
                                      Deterministic race/ethnicity brief
scripts/35_write_consolidated_reporter_brief.R
                                      Canonical cross-module brief
tests/test_acs_helpers.R             Offline synthetic-data checks
tests/test_period_service_helpers.R  Composite/MOE/missingness tests
tests/test_disability_helpers.R      C21007 composite/MOE/identity tests
tests/test_median_income_helpers.R   B21004 gap/MOE/significance tests
tests/test_labor_force_helpers.R     B21005 denominator/MOE/identity tests
tests/test_service_connected_helpers.R
                                      B21100 denominator/identity tests
tests/test_race_ethnicity_helpers.R   C21001A-I overlap/MOE/identity tests
tests/test_reporter_brief_helpers.R  Byte-for-byte determinism tests
run_pipeline.R                       Pipeline runner
r_script                             Legacy report; not publication-ready
```

Regenerable data, outputs, charts, reports, and logs are ignored by Git. Empty
directory markers remain tracked so a fresh clone has the expected structure.

## Statistical interpretation

ACS values are estimates, not exact population counts. The pipeline retains the
ACS 90 percent MOE and derives the MOE for the veteran proportion with
`tidycensus::moe_prop()`. Standard errors are calculated from the configured
confidence level. The coefficient of variation is the standard error divided by
the estimate.

The default `high_cv_threshold` is 0.30. Observations above that threshold are
retained and flagged but excluded from rankings. This is a transparent project
QA rule, not an official Census Bureau cutoff. A descriptive rank does not prove
that two estimates differ statistically. State comparisons therefore include
the MOE of the difference and a separate 90 percent significance indicator.

Period-of-service composites use the mutually exclusive cells in B21002.
Composite MOEs use `tidycensus::moe_sum()` and percentage MOEs use
`tidycensus::moe_prop()`. The five headline periods overlap; a veteran can be
represented in more than one period, so the composites cannot be added and do
not form a 100 percent distribution.

Confidence intervals are bounded to the logical range of zero percent to 100 percent for
presentation. The underlying estimate and MOE remain unchanged.

Disability prevalence uses C21007 and is reported among veterans overall and
within explicit age and poverty-status denominators. Composite counts retain
quadrature MOEs, and percentage MOEs use `tidycensus::moe_prop()`. C21007's
disability concept is not a service-connected disability rating. The pipeline
verifies all 15 published parent/child table identities before producing
findings.

Median-income analysis uses the directly published B21004 medians and MOEs in
the ACS year's inflation-adjusted dollars. Veteran and nonveteran medians are
not added or averaged. The MOE for their difference is the square root of the
sum of their squared MOEs; a gap is flagged as statistically distinguishable
from zero when its absolute value exceeds that difference MOE. B21004 covers
the civilian population age 18 and older with income, not all adults or
households.

Labor-force analysis uses B21005 for the civilian population age 18 to 64.
Labor-force participation and the employment-to-population ratio use the
civilian population as denominator. The unemployment rate uses the civilian
labor force as denominator, so people not in the labor force are not classified
as unemployed. Veteran–nonveteran rate gaps carry root-sum-square MOEs and an
explicit 90% significance flag. Age-band counts are summed with
`tidycensus::moe_sum()` before rates are calculated; age-specific rates are
never averaged to construct the overall rate. State rankings order rates from
highest to lowest, which means a high unemployment-rate rank is not a favorable
outcome.

Service-connected disability analysis uses B21100 for civilian veterans age 18
and older. Any-rating prevalence uses all veterans as denominator. Rating bands
use veterans with a service-connected rating as denominator and retain the
published 0 percent and rating-not-reported categories. The rating bands are
mutually exclusive and exhaust the published any-rating total. These ACS survey
estimates are not Department of Veterans Affairs administrative counts and are
distinct from the broader C21007 disability-status measure.

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

## License

This project is released under an MIT License. See the license file in the repo for more information.

## Special thanks
This project uses a number of R packages, including the [tidyverse family of packages](https://tidyverse.tidyverse.org/index.html) created by [Hadley Wickham](https://hadley.nz/) et al and the [tidycensus](https://walker-data.com/tidycensus/) and [tigris](https://cran.r-project.org/web/packages/tigris/index.html) packages created by [Kyle Walker](https://walker-data.com/) that downloads and works with U.S. Census Bureau data and geographic files. I am also very grateful for packages including [janitor](https://cran.r-project.org/web/packages/janitor/index.html) and [sf](https://cran.r-project.org/web/packages/sf/index.html), among others. Thank you to the brilliant people behind these packages who wrote all that code and keep it maintained.

## Authorship

[Jennifer Peebles](https://www.ajc.com/staff/jennifer-peebles/) / [Atlanta Journal-Constitution](https://www.ajc.com/)

A note from JP: I built this project with help from ChatGPT/Codex, which drafted this README from the project's code, outputs and my instructions (and to which I have made edits). I want to be transparent about the help I received.

