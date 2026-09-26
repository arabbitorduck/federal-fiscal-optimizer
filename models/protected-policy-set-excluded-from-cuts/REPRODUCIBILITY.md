# Reproducibility and Technical Requirements

## 1. Required repository inputs

The published model includes the following fixed inputs in its own directory:

- `Data/input_manifest.csv`
- `Data/reference/release_reference.json`
- `Data/source_raw/cbo_spending_detail_2026_06.xlsx`
- `Data/source_raw/jct_tax_expenditures_2025_2029.pdf`
- the complete `Data/fiscal_policy_data_pack_v1/` directory

The two fixed files under `Data/source_raw/` are validated against SHA-256 values in `Data/input_manifest.csv`.

The policy-data pack carries its own source manifest, coverage audit, pack manifest, and file hashes. Those records are checked during execution.

These fixed files define part of the published model vintage. Replacing them with newer source vintages changes the reproduction target.

## 2. R and solver environment

The validated release was run with:

- **R 4.6.1** (`R version 4.6.1 (2026-06-24 ucrt)`);
- **HiGHS 1.14.0.2** through the R `highs` package;
- **x86_64-w64-mingw32**;
- **8 HiGHS solver threads**;
- requested relative MIP gap **0**.

The script uses repository-relative paths and does not depend on a hard-coded local working directory.

Two optional environment variables control execution settings:

- `FISCAL_SOLVER_THREADS` sets the requested solver thread count. If it is not supplied, the script derives a bounded thread count from the available hardware and applies its memory-aware limits.
- `FISCAL_EVIDENCE_MODE` selects the evidence mode. The validated release uses `OFFICIAL_OLDER`. The other recognized values are `OFFICIAL_CURRENT` and `EXPLORATORY`.

Changing the evidence mode changes the admissible evidence set and is not a reproduction of the validated release.

## 3. Required R packages

The script checks the following **22 direct R package dependencies** before execution. In an interactive R session, it asks once whether missing packages should be installed. In noninteractive execution, it calls `install.packages(..., dependencies = TRUE)` for missing packages and verifies that installation succeeded before continuing.

| Package | Validated version | Principal use |
| --- | ---: | --- |
| `httr2` | 1.2.3 | HTTP requests to official APIs and source endpoints |
| `jsonlite` | 2.0.0 | JSON parsing, API responses, and release-reference input |
| `readr` | 2.2.0 | CSV and delimited-file input and output |
| `dplyr` | 1.2.1 | Data transformation, joins, filtering, and aggregation |
| `tidyr` | 1.3.2 | Reshaping and completion of annual policy and baseline data |
| `purrr` | 1.2.2 | Iteration across policies, scenarios, validation steps, and solves |
| `stringr` | 1.6.0 | Text normalization, source parsing, and policy classification |
| `lubridate` | 1.9.5 | Date handling |
| `digest` | 0.6.39 | SHA-256 and related reproducibility hashing |
| `data.table` | 1.18.4 | High-volume data operations |
| `Matrix` | 1.7.5 | Sparse MILP constraint matrices |
| `highs` | 1.14.0.2 | HiGHS MILP solver interface |
| `testthat` | 3.3.2 | Programmatic validation tests |
| `ggplot2` | 4.0.3 | Presentation-figure generation |
| `scales` | 1.4.0 | Plot scale and label formatting |
| `ragg` | 1.5.2 | Raster graphics output |
| `tibble` | 3.3.1 | Structured tabular objects |
| `zip` | 3.0.2 | Local audit-archive creation |
| `curl` | 7.1.0 | Download and connection support |
| `readxl` | 1.5.0 | Official Excel workbook input |
| `rvest` | 1.0.5 | HTML parsing for official-source acquisition and validation |
| `xml2` | 1.6.0 | XML and HTML document handling |

The versions above are the direct package versions recorded by the validated run. The script checks package availability but does not require these exact versions at startup. A changed package version can therefore alter behavior even when the model source is unchanged.

If exact restoration of the validated dependency graph is required, an `renv.lock` should be generated from the validated R environment and committed with the model. It should be produced by `renv` from that environment rather than reconstructed manually from the direct-package list.

## 4. Required API keys

The validated model requires three API credentials for independent historical validation:

- `BEA_API_KEY` — U.S. Bureau of Economic Analysis Data API
- `BLS_API_KEY` — U.S. Bureau of Labor Statistics Public Data API
- `FRED_API_KEY` — Federal Reserve Bank of St. Louis FRED API

`CENSUS_API_KEY` is recognized by the script but is not required by the current core model.

The script requires the BEA, BLS, and FRED keys for the validated release. If one is absent, the corresponding required validation cannot be completed and the run stops rather than silently omitting it.

### Obtaining the keys

Use the official registration pages:

- **BEA:** `https://apps.bea.gov/api/signup/`
- **BLS:** `https://data.bls.gov/registrationEngine/`
- **FRED:** `https://fred.stlouisfed.org/docs/api/api_key.html`

BEA issues a key after API registration. BLS Version 2 registration provides a registration key by email and requires periodic renewal under BLS's current registration policy. FRED requires a FRED account before a key can be requested or viewed.

### Setting the keys

The script reads credentials from environment variables. A local `.Renviron` file in the model directory is the simplest persistent configuration:

```text
BEA_API_KEY=your_key_here
BLS_API_KEY=your_key_here
FRED_API_KEY=your_key_here
CENSUS_API_KEY=your_key_here
```

The repository includes `.Renviron.example` with the variable names but no credentials.

After creating or editing `.Renviron`, restart the R session so the variables are loaded. They can be verified without printing the keys themselves:

```r
nzchar(Sys.getenv("BEA_API_KEY"))
nzchar(Sys.getenv("BLS_API_KEY"))
nzchar(Sys.getenv("FRED_API_KEY"))
```

Each expression should return `TRUE`.

A populated `.Renviron` contains credentials and must not be committed to the repository.

## 5. Supplied inputs versus runtime acquisition

The public repository deliberately contains only fixed inputs that are required to reproduce the published model vintage and that should not be rediscovered or silently replaced at runtime. Other public data are acquired or queried during execution.

### Supplied with the repository

The following are supplied with the model:

- the June 2026 CBO spending-by-budget-account workbook;
- the 2025-2029 JCT tax-expenditure PDF;
- the complete frozen fiscal-policy data pack;
- the input manifest and expected hashes;
- the release-reference file containing the exact selected candidate membership and principal validated metrics.

The June 2026 CBO workbook and JCT PDF are required repository inputs. The script stops if either required fixed input is absent or fails its integrity checks.

### Downloaded or queried at runtime

The model obtains additional official data during execution. The principal live acquisitions are:

**Congressional Budget Office**

- February 2026 ten-year budget projections;
- February 2026 long-term budget data;
- February 2026 historical budget data;
- February 2026 historical and projected economic data;
- February 2026 revenue-detail data;
- February 2026 spending-by-budget-account data used in the account-control pipeline;
- February 2026 tax-parameter data;
- February 2026 trust-fund data;
- current CBO budget-option index material used to validate scored-option coverage and score vintage;
- current CBO publication material used to validate the debt-service and macro-sensitivity inputs.

The machine-readable February 2026 CBO files are acquired from CBO's public Open Data repository. CBO remains the originating agency and canonical publisher.

**U.S. Department of the Treasury**

- Debt to the Penny data used for the independent historical debt check;
- the current Treasury Revenue Proposals index;
- the FY2025 Greenbook PDF used for scored Treasury policy coefficients;
- the Greenbook machine-readable revenue table when available;
- current Treasury tax-expenditure material used as inventory evidence.

**Social Security Administration, Office of the Chief Actuary**

- the current Trustees Report summary used to validate the 4.42% taxable-payroll solvency target;
- 2026 Trustees Table VI.G1 taxable-payroll data;
- current and relevant prior OACT provision-summary pages;
- detailed annual OACT provision tables used for applicable Social Security response functions.

**Federal Reserve Bank of St. Louis**

- FRED debt-held-by-the-public data used for an independent historical cross-check.

**Bureau of Economic Analysis**

- NIPA data used to construct an independent FY2025 nominal-GDP cross-check.

**Bureau of Labor Statistics**

- monthly unemployment data used to construct the FY2025 unemployment cross-check.

Some live sources are cached under the model's local data directories after acquisition. Sources that the script explicitly requests with forced refresh are re-queried on a new run.

The fixed repository inputs listed above remain the reproduction inputs for those specific sources even if newer official files later become available.

## 6. Network access

A clean reproduction requires outbound HTTPS access to the official public endpoints used by the model.

The fixed CBO spending-detail workbook, JCT PDF, policy-data pack, input manifest, and release reference do not require a live download because they are supplied with the repository.

A transient failure of an optional evidence source is handled according to the source's role. Failure of a required baseline, validation, fixed-input, or scored-policy source stops the relevant model stage rather than silently substituting unverified data.

## 7. Running in RStudio

Open the model directory and run:

```r
source("federal_fiscal_optimizer.R", echo = TRUE)
```

The script resolves the directory containing itself, so it can also be sourced when RStudio's current working directory is elsewhere.

## 8. Running from a shell

With R available on the system path:

```text
Rscript federal_fiscal_optimizer.R
```

## 9. Startup checks

Before fiscal optimization proceeds, the script:

1. checks and, when authorized, installs required R packages;
2. resolves repository-relative paths;
3. validates the recognized evidence-mode setting;
4. verifies the fixed repository inputs and expected hashes;
5. validates the required API credentials;
6. runs a small known MILP through the R `highs` interface to verify that the solver interface is functioning.

Failure of a required startup or validation check stops the run.

## 10. Fixed-source integrity

`Data/input_manifest.csv` records the two repository-supplied official source files, their source URLs, agencies, publication dates, acquisition mode, SHA-256 values, purpose, and provenance notes.

The validated hashes are:

- `cbo_spending_detail_2026_06.xlsx`  
  `ff7744a0fc813789a6dae708270968bb2e870c40e933c4068b96d9d8488fc778`
- `jct_tax_expenditures_2025_2029.pdf`  
  `d1532ca4ee461292f206344f790a6869cf60b990d45c635e180a6a3c858c20ab`

The policy-data pack separately validates its constituent files against `Data/fiscal_policy_data_pack_v1/file_hashes.csv`.

## 11. Release reference

`Data/reference/release_reference.json` stores the numerical and exact-membership reference used by the release-parity check.

The validated reference is:

- active solver candidates: **1,116**;
- selected policies: **21**;
- implementation-complexity score: **31.996850**;
- central debt/GDP, 2036: **86.335057%**;
- central debt/GDP, 2046: **66.549700%**;
- worst required-scenario debt/GDP, 2036: **89.991493%**;
- worst required-scenario debt/GDP, 2046: **74.662096%**;
- Social Security actuarial improvement: **4.50% of taxable payroll**.

The reference also contains the exact selected candidate identifiers. The release-parity check therefore tests package membership as well as the principal numerical results.

## 12. Numerical tolerances

The principal configured numerical controls are:

| Control | Value |
| --- | ---: |
| Requested HiGHS relative MIP gap | 0 |
| Post-solve relative gap audit | `1e-8` |
| Post-solve absolute objective-gap audit | `1e-6` |
| Primal feasibility tolerance | `1e-7` |
| Dual feasibility tolerance | `1e-7` |
| Accepted constraint residual | `1e-5` |
| Accepted variable-bound residual | `1e-6` |
| Accepted integrality residual | `1e-6` |
| Independent simulation tolerance | $2 billion |
| Debt identity tolerance | $0.01 billion |
| Debt/GDP reconstruction tolerance | 0.02 percentage point |
| CBO debt-service-kernel validation tolerance | $3 billion |

The post-solve gap tolerances are numerical audits applied only after HiGHS reports `Optimal`. They do not permit a time-limited, node-limited, interrupted, or otherwise unfinished solve to be treated as optimal.

## 13. Validation sequence

The validated model requires successful completion of:

- CBO baseline accounting identities;
- Treasury, FRED, BEA, and BLS historical cross-checks;
- reciprocal leave-one-out validation of the CBO debt-service kernel;
- reconstruction checks for the CBO macro-sensitivity profiles;
- current CBO option-index validation;
- current Social Security actuarial-target validation;
- policy parameterization validation;
- score-basis contract checks;
- protection regression;
- material-overlap validation;
- solver terminal-status and numerical-gap audits;
- independent post-solve simulation;
- exact release-reference membership and metric checks.

These checks are part of the executable model rather than manual review steps.

## 14. Validated computational scale

The Expanded package-ready MILP contains **21,411 variables**, **21,922 constraints**, and **1,568,494 nonzero coefficients**. The validated model enforces five required robustness scenarios inside each applicable MILP.

The complete validated analysis attempted **121 MILP solves**. Of those, **106** terminated `Optimal` and **15** terminated `Infeasible` in feasibility or counterfactual diagnostics. No solve ended with an unfinished terminal status.

The model uses memory-aware schedule reduction for complexity solves. Candidate policies are not removed to reduce memory. The reduction removes only timing paths that are proven duplicated or componentwise dominated for the same policy.

## 15. Runtime files

A run writes working and audit material locally, including caches, acquired source and validation data, solver diagnostics, source-provenance records, policy and scenario tables, independent-simulation results, and the presentation plot.

The principal runtime locations are:

- `Cache/`
- `Data/source_raw/` for runtime-acquired source files in addition to the two fixed repository inputs;
- `Data/validation_raw/`;
- `analysis_output/`;
- `analysis_output.zip`.

The repository `.gitignore` excludes generated runtime material while retaining the two fixed source files that are part of the published model.

## 16. Reproduction standard

A successful reproduction should:

- complete the required validation pipeline;
- reproduce the exact selected candidate membership;
- reproduce the release-reference metrics within the configured tolerance;
- satisfy the required debt and Social Security constraints;
- pass the protection and material-overlap checks;
- pass the solver optimality and numerical-gap checks for every solve reported as optimal;
- independently re-simulate every retained package within the configured tolerance.

Differences caused by a changed official source vintage, changed model source, changed fixed input, changed evidence mode, or materially changed software dependency behavior are not reproductions of this published model.
