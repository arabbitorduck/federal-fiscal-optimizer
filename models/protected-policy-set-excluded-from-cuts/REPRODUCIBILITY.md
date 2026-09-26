# Reproducibility

## Required files

This model directory is self-contained for the fixed repository inputs required by the public release.

Required committed inputs are:

- `Data/input_manifest.csv`
- `Data/reference/release_reference.json`
- `Data/source_raw/cbo_spending_detail_2026_06.xlsx`
- `Data/source_raw/jct_tax_expenditures_2025_2029.pdf`
- all files under `Data/fiscal_policy_data_pack_v1/`

The two official files under `Data/source_raw/` are validated against SHA-256 values in `Data/input_manifest.csv`.

The frozen local policy pack is independently hash-validated by the model.

Do not replace these fixed files inside a published model directory with newer vintages. A substantive change in evidence or model rules belongs in a separately named model directory or a separately tagged repository state.

## API keys

The model reads API credentials from environment variables.

Supported variables are:

- `BEA_API_KEY`
- `BLS_API_KEY`
- `FRED_API_KEY`
- `CENSUS_API_KEY`

A local `.Renviron` file may be used, but populated credential files must not be committed.

An example file is included as `.Renviron.example`.

## Running in RStudio

From this model directory:

```r
source("federal_fiscal_optimizer.R", echo = TRUE)
```

The script also resolves its own directory when sourced from elsewhere in RStudio.

## Running from a shell

```text
Rscript federal_fiscal_optimizer.R
```

## Packages

The script checks all required R packages at startup.

In an interactive R session, if required packages are missing, the user is prompted once to install them. In noninteractive execution, missing required packages are installed automatically.

## Runtime files

A successful run can create local runtime material including:

- `Cache/`
- `Data/validation_raw/`
- additional downloaded source files under `Data/source_raw/`
- `analysis_output/`
- `analysis_output.zip`

These runtime outputs are not part of the committed publication package.

## Frozen parity check

`Data/reference/release_reference.json` stores the validated release target used by the final acceptance layer.

The validated public release reproduces:

- 1,116 active solver candidates;
- 21 selected policies;
- complexity score 31.996850;
- central debt/GDP of 86.335057% in 2036;
- central debt/GDP of 66.549700% in 2046;
- worst required-scenario debt/GDP of 89.991493% in 2036;
- worst required-scenario debt/GDP of 74.662096% in 2046;
- Social Security actuarial improvement of 4.50% of taxable payroll;
- zero R warnings.

These values are parity targets for the published model, not a substitute for the generated audit outputs from a fresh run.

## Publication rule

Git tags should identify published repository states. The descriptive model-directory name identifies the substantive model, while tags identify a frozen publication state of the repository.
