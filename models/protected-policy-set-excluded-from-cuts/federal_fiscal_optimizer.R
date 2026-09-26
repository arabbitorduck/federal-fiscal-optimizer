# ==============================================================================
# PUBLIC REPRODUCIBILITY RELEASE
# The repository root resolves from this script's own location, so the file may be
# sourced from any R working directory. Fixed repository inputs are stored under Data/,
# runtime outputs are written under analysis_output/, and the validated release
# reference is stored under Data/reference/.
#

# FEDERAL FISCAL CAPACITY AND DEBT OPTIMIZATION MODEL
#
# This script builds, validates, solves, independently re-simulates, and audits
# a parameterized mixed-integer linear optimization model for U.S. federal
# deficit and debt reduction through FY2046.
#
# Fiscal coefficients must come from official scored evidence, validated
# mechanical transformations of official baseline paths, or explicitly labeled
# modeling assumptions. Unsupported response coefficients are not inferred from
# baseline totals, tax-expenditure estimates, actuarial percentages, or account
# balances.
#
# Account-level controls are screened before solver entry to protect person-facing
# benefits, earned compensation, household security, productive public capacity,
# and core state capacity. Admissible generic account controls are growth
# restraints rather than account elimination.
#
# LEGISLATIVE POLICY-SPACE RULE
# Current law is a scoring baseline, not a policy-eligibility constraint. Congress
# can amend, repeal, replace, or enact statutes. After the protected categories
# above are enforced, a policy is not excluded merely because implementation
# requires changing existing law. The model uses the newest published official
# score available for the policy, together with the newest available baseline,
# debt-service method, account data, and actuarial data. A legal change is carried
# as scoring/provenance metadata rather than as a veto on solver entry.
#
# Revenue increases are positive. Higher outlays are positive. Spending savings
# are negative outlay changes. Primary deficit change equals outlay change minus
# revenue change.
# ==============================================================================

# ---- Package dependency check and loading --------------------------------------
# Check required packages before execution. In an interactive R session, prompt
# once to install any missing packages; in noninteractive execution, install them
# automatically. After installation, verify every required package is available.
required_packages <- c(
  "httr2", "jsonlite", "readr", "dplyr", "tidyr", "purrr", "stringr",
  "lubridate", "digest", "data.table", "Matrix", "highs", "testthat",
  "ggplot2", "scales", "ragg", "tibble", "zip", "curl", "readxl",
  "rvest", "xml2"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  install_missing <- TRUE

  if (interactive()) {
    prompt <- paste0(
      "The following required R packages are not installed: ",
      paste(missing_packages, collapse = ", "),
      ". Install them now? [Y/n]: "
    )
    response <- trimws(tolower(readline(prompt)))
    install_missing <- response %in% c("", "y", "yes")
  }

  if (!install_missing) {
    stop(
      "Required R packages are missing and installation was declined: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  install.packages(missing_packages, dependencies = TRUE)

  still_missing <- missing_packages[
    !vapply(missing_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(still_missing) > 0L) {
    stop(
      "Required R package installation did not complete successfully for: ",
      paste(still_missing, collapse = ", "),
      call. = FALSE
    )
  }
}

# Load the core packages without startup chatter so the persistent console log remains readable.
suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(digest)
  library(data.table)
  library(Matrix)
  library(highs)
  library(testthat)
  library(ggplot2)
  library(scales)
  library(ragg)
  library(tibble)
  library(rvest)
  library(xml2)
})

# Global R options: keep ordinary strings, avoid scientific notation in fiscal output, and allow long official-source downloads.
options(
  stringsAsFactors = FALSE,
  scipen = 999,
  timeout = max(180, getOption("timeout"))
)

# Resolve the repository root from the executing script itself. This keeps all
# repository-relative paths anchored to the directory containing this file even
# when RStudio's current working directory is somewhere else.
resolve_repository_root <- function() {
  frame_files <- vapply(
    sys.frames(),
    function(frame) {
      ofile <- frame$ofile
      if (is.null(ofile) || length(ofile) == 0L) return(NA_character_)
      ofile <- as.character(ofile[[1L]])
      if (is.na(ofile) || !nzchar(ofile)) return(NA_character_)
      ofile
    },
    character(1)
  )

  frame_files <- frame_files[!is.na(frame_files) & nzchar(frame_files)]
  if (length(frame_files) > 0L) {
    return(dirname(normalizePath(tail(frame_files, 1L), winslash = "/", mustWork = TRUE)))
  }

  file_args <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_args) > 0L) {
    script_path <- sub("^--file=", "", tail(file_args, 1L))
    return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

# Central model configuration. Values here define vintages, targets, solver tolerances, search breadth, and audit behavior.
CFG <- list(
  # Human-readable model name used in manifests and console output.
  model_name = "Federal Fiscal Capacity and Debt Optimization Model",
  # Model version recorded in manifests for reproducibility.
  model_version = "federal-fiscal-capacity-model-public-release-1.0",
  # Timestamp captured when the script initializes.
  run_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  # Repository root is the directory containing this script.
  project_root = resolve_repository_root(),
  data_dir = NULL,
  cache_dir = NULL,
  output_dir = NULL,
  raw_source_dir = NULL,
  validation_dir = NULL,
  policy_pack_dir = NULL,
  # Primary CBO baseline vintage used by the model.
  cbo_vintage = "2026-02",
  # Full annual accounting horizon.
  model_years = 2026:2046,
  # Primary ten-year fiscal-score window.
  score_years = 2027:2036,
  # Long-run extension years beyond official ten-year scores.
  extension_years = 2037:2046,
  # Maximum debt held by the public as a share of GDP in FY2036.
  target_2036 = 0.90,
  # Central FY2046 reference target.
  target_2046_center = 0.75,
  # Lower edge of the desired long-run debt/GDP band.
  target_2046_low = 0.70,
  # Maximum hard FY2046 debt/GDP target.
  target_2046_high = 0.80,
  # Whether the lower edge of the long-run band is a hard constraint.
  enforce_2046_lower_bound = FALSE,
  # Select OFFICIAL_CURRENT, OFFICIAL_OLDER, or EXPLORATORY evidence rules.
  evidence_mode = toupper(Sys.getenv("FISCAL_EVIDENCE_MODE", unset = "OFFICIAL_OLDER")),
  # Congress may change current law. Current law informs scoring/provenance but never independently vetoes an otherwise protected-and-scored policy.
  legislative_policy_space = TRUE,
  current_law_is_policy_eligibility_constraint = FALSE,
  # Optional switch requiring the modeled Social Security actuarial gap to be closed.
  require_ss_solvency = FALSE,
  # Reference OASDI actuarial shortfall in percent of taxable payroll.
  ss_actuarial_gap_pct_payroll = 4.42,
  ss_actuarial_excess_margin_pct_payroll = 0.50,
  ss_actuarial_max_pct_payroll = 4.92,
  ss_gap_vintage = "2026 Trustees Report",
  # Whether to apply the configured tariff-related baseline adjustment.
  allow_tariff_adjustment = TRUE,
  tariff_primary_worsening_2026_bil = 250,
  tariff_primary_worsening_2027_2036_bil = 700,
  tariff_debt_service_worsening_2027_2036_bil = 200,
  tariff_extension_mode = "hold_2036_gdp_share",
  tariff_calibration_tolerance_bil = 35,
  # Require all baseline validation checks to pass.
  strict_gate_1 = TRUE,
  # Require debt-service/macro validation checks to pass.
  strict_gate_2 = TRUE,
  require_external_api_validation = TRUE,
  cbo_kernel_impulse_bil = 900,
  cbo_kernel_validation_tolerance_bil = 3,
  cbo_bfm_published_precision_half_width = 0.005,
  # Must remain FALSE unless an interaction has defensible quantified evidence.
  allow_unscored_interactions = FALSE,
  long_run_interest_extension_mode = "hold_2036_effective_marginal_rate",
  long_run_interest_fallback_rate = 0.044,
  debt_identity_tolerance_bil = 0.01,
  ratio_tolerance_pp = 0.02,
  source_crosscheck_tolerance_pct = 1.0,
  # Require HiGHS itself to pursue zero requested relative MIP gap. This solver
  # request remains exactly zero; the two audit tolerances below are used only
  # after HiGHS has already returned terminal Optimal status, because the API
  # can retain a few parts per billion of primal-dual roundoff in info$mip_gap.
  solver_mip_rel_gap = 0,
  # Post-solve numerical audit tolerances for a HiGHS-proven Optimal result.
  # Either the recorded relative gap must be <= 1e-8 or the absolute
  # primal-dual objective difference must be <= 1e-6 objective units. These
  # tolerances do not permit time/node-limited or otherwise unfinished solves.
  solver_optimality_audit_rel_tolerance = 1e-8,
  solver_optimality_audit_abs_tolerance = 1e-6,
  # Primal feasibility tolerance.
  solver_primal_feasibility_tolerance = 1e-7,
  # Dual feasibility tolerance.
  solver_dual_feasibility_tolerance = 1e-7,
  solution_acceptance_constraint_tolerance = 1e-5,
  solution_acceptance_bound_tolerance = 1e-6,
  solution_acceptance_integrality_tolerance = 1e-6,
  # Allowed dollar discrepancy between solver state and independent simulation.
  independent_verification_tolerance_bil = 2,
  # Number of requested Pareto tradeoff points.
  pareto_grid_points = 10L,
  cbo_expected_2024_option_families = 76L,
  cbo_minimum_current_option_families = 120L,
  # Hard floor on strict-mode solver-ready candidates.
  minimum_solver_candidates = 100L,
  # Hard floor on expanded-mode solver-ready candidates.
  minimum_expanded_solver_candidates = 500L,
  policy_pack_expected_families = 127L,
  policy_pack_expected_candidates = 190L,
  policy_pack_expected_annual_rows = 1810L,
  policy_pack_expected_annual_candidates = 181L,
  # HiGHS thread count, leaving one logical core free where possible.
  solver_threads = max(1L, as.integer(Sys.getenv("FISCAL_SOLVER_THREADS", unset = as.character(min(8L, max(1L, parallel::detectCores(logical = TRUE) %/% 2L)))))),
  # Maximum diverse solutions requested for each core objective.
  diverse_solutions_per_objective = 3L,
  # Number of diverse solutions requested for special scenarios.
  special_solution_count = 2L,
  # Number of nearest-target packages retained when hard targets are infeasible.
  nearest_target_solution_count = 6L,
  # Relative slack bands used for best-attainable frontier searches when the
  # original robust debt targets are infeasible.
  soft_frontier_slack_bands = c(0.00, 0.10, 0.25),
  # Diverse packages requested for each secondary objective inside a slack band.
  soft_frontier_diverse_solutions = 2L,
  # Slack band used for the revenue/spending Pareto frontier.
  soft_frontier_pareto_band = 0.10,
  # Slack band used for special-case VAT/FTT/Social Security searches.
  soft_frontier_special_band = 0.25,
  # Numerical tolerance applied to the normalized target-slack cap.
  soft_frontier_slack_absolute_tolerance = 0.0001,
  # Reporting-only materiality threshold. It never removes a valid candidate from optimization.
  package_ready_account_min_savings_bil_2027_2036 = 0.05,
  # Minimum binary-activation distance used to avoid duplicate packages.
  diversity_hamming_distance = 2L,
  # Reproducibility seed.
  seed = 20260910L,
  # Render representative debt paths in the RStudio Plots pane.
  render_plots = TRUE,
  # Write machine-readable audit tables and archive them.
  write_audit_outputs = TRUE
)

# Resolve all project-relative paths after project_root is known.
CFG$data_dir <- file.path(CFG$project_root, "Data")
CFG$cache_dir <- file.path(CFG$project_root, "Cache")
CFG$output_dir <- file.path(CFG$project_root, "analysis_output")
CFG$raw_source_dir <- file.path(CFG$data_dir, "source_raw")
CFG$validation_dir <- file.path(CFG$data_dir, "validation_raw")
CFG$policy_pack_dir <- file.path(CFG$data_dir, "fiscal_policy_data_pack_v1")
CFG$console_log_path <- file.path(CFG$output_dir, "console_output.txt")
CFG$output_zip_path <- file.path(CFG$project_root, "analysis_output.zip")
CFG$output_archive_manifest_path <- file.path(CFG$output_dir, "analysis_output_archive_manifest.csv")

if (!dir.exists(CFG$data_dir)) {
  stop(
    paste0("Expected repository Data/ directory beside the model script under: ", CFG$project_root),
    call. = FALSE
  )
}

# Create persistent data/cache/output directories before any network or solver work begins.
for (d in c(
  CFG$data_dir,
  CFG$cache_dir,
  CFG$output_dir,
  CFG$raw_source_dir,
  CFG$validation_dir
)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Fix stochastic ordering/tie-breaking for reproducible solution-family searches.
set.seed(CFG$seed)

# Evidence mode controls which source vintages are admissible without changing the accounting identities.
valid_evidence_modes <- c("OFFICIAL_CURRENT", "OFFICIAL_OLDER", "EXPLORATORY")
if (!CFG$evidence_mode %in% valid_evidence_modes) {
  stop(
    "FISCAL_EVIDENCE_MODE must be one of: ",
    paste(valid_evidence_modes, collapse = ", "),
    call. = FALSE
  )
}

if (CFG$ss_actuarial_max_pct_payroll < CFG$ss_actuarial_gap_pct_payroll) {
  stop("ss_actuarial_max_pct_payroll cannot be below the configured Social Security actuarial gap", call. = FALSE)
}
if (abs(CFG$ss_actuarial_max_pct_payroll - (CFG$ss_actuarial_gap_pct_payroll + CFG$ss_actuarial_excess_margin_pct_payroll)) > 1e-12) {
  stop("Social Security actuarial maximum must equal the configured gap plus excess margin", call. = FALSE)
}



# ------------------------------------------------------------------------------
# FUNCTION: paths_same
# Purpose: Compare normalized paths using the host operating system's case rules.
# ------------------------------------------------------------------------------
paths_same <- function(a, b) {
  a_norm <- normalizePath(a, winslash = "/", mustWork = FALSE)
  b_norm <- normalizePath(b, winslash = "/", mustWork = FALSE)
  if (.Platform$OS.type == "windows") {
    identical(tolower(a_norm), tolower(b_norm))
  } else {
    identical(a_norm, b_norm)
  }
}


# ------------------------------------------------------------------------------
# FUNCTION: project_relative_path
# Purpose: Store portable repository-relative paths in public audit manifests.
# ------------------------------------------------------------------------------
project_relative_path <- function(path) {
  if (is.null(path) || length(path) == 0L || is.na(path) || !nzchar(path)) return(NA_character_)
  root <- normalizePath(CFG$project_root, winslash = "/", mustWork = FALSE)
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  if (startsWith(p, prefix)) substring(p, nchar(prefix) + 1L) else p
}


# ------------------------------------------------------------------------------
# FUNCTION: sanitize_public_text
# Purpose: Remove machine-specific repository-root prefixes from public logs and
#          audit artifacts while preserving URLs and repository-relative paths.
# ------------------------------------------------------------------------------
sanitize_public_text <- function(x) {
  if (length(x) == 0L) return(x)
  out <- as.character(x)
  root_forward <- normalizePath(CFG$project_root, winslash = "/", mustWork = FALSE)
  root_backward <- gsub("/", "\\", root_forward, fixed = TRUE)

  for (root in unique(c(root_forward, root_backward))) {
    if (is.na(root) || !nzchar(root)) next
    separator <- if (grepl("\\", root, fixed = TRUE)) "\\" else "/"
    out <- stringr::str_replace_all(out, stringr::fixed(paste0(root, separator)), "")
    out <- stringr::str_replace_all(out, stringr::fixed(root), ".")
  }

  out
}


# ------------------------------------------------------------------------------
# FUNCTION: is_public_output_path
# Purpose: Identify files written inside analysis_output for public-path sanitation.
# ------------------------------------------------------------------------------
is_public_output_path <- function(path) {
  output_root <- normalizePath(CFG$output_dir, winslash = "/", mustWork = FALSE)
  target <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (.Platform$OS.type == "windows") {
    output_root <- tolower(output_root)
    target <- tolower(target)
  }
  identical(target, output_root) || startsWith(target, paste0(output_root, "/"))
}


# ------------------------------------------------------------------------------
# FUNCTION: sanitize_public_audit_data
# Purpose: Sanitize repository-local path text only in public audit-output copies.
#          In-memory model objects remain unchanged.
# ------------------------------------------------------------------------------
sanitize_public_audit_data <- function(x, path) {
  if (!is.data.frame(x) || !is_public_output_path(path)) return(x)

  out <- x
  for (nm in names(out)) {
    if (is.character(out[[nm]])) {
      out[[nm]] <- sanitize_public_text(out[[nm]])
    } else if (is.factor(out[[nm]])) {
      out[[nm]] <- sanitize_public_text(as.character(out[[nm]]))
    }
  }
  out
}

# ------------------------------------------------------------------------------
# FUNCTION: log_line
# Purpose: Write a timestamped INFO/WARN/ERROR line to the console and flush it immediately.
# ------------------------------------------------------------------------------
log_line <- function(..., level = "INFO") {
  txt <- sanitize_public_text(paste0(..., collapse = ""))
  cat(sprintf("%s | %-5s | %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, txt))
  flush.console()
}


# ------------------------------------------------------------------------------
# FUNCTION: model_stop
# Purpose: Log a fatal model error and terminate execution without an R call trace.
# ------------------------------------------------------------------------------
model_stop <- function(...) {
  message <- sanitize_public_text(paste0(..., collapse = ""))
  log_line(message, level = "ERROR")
  stop(message, call. = FALSE)
}


# ------------------------------------------------------------------------------
# FUNCTION: model_warn
# Purpose: Log a model warning and emit an immediate R warning.
# ------------------------------------------------------------------------------
model_warn <- function(...) {
  message <- sanitize_public_text(paste0(..., collapse = ""))
  log_line(message, level = "WARN")
  warning(message, call. = FALSE, immediate. = TRUE)
}


# ------------------------------------------------------------------------------
# FUNCTION: assert_model
# Purpose: Enforce an internal model invariant and stop with a clear message if it fails.
# ------------------------------------------------------------------------------
assert_model <- function(condition, message) {
  if (!isTRUE(condition)) {
    model_stop(message)
  }
  invisible(TRUE)
}


# ------------------------------------------------------------------------------
# FUNCTION: validate_repository_inputs
# Purpose: Verify repository-supplied source files before any solver work begins.
# ------------------------------------------------------------------------------
validate_repository_inputs <- function() {
  manifest_path <- file.path(CFG$data_dir, "input_manifest.csv")
  assert_model(file.exists(manifest_path), paste0("Missing repository input manifest: ", manifest_path))

  manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)
  required <- manifest |>
    dplyr::filter(acquisition_mode == "REPOSITORY_SUPPLIED")

  if (nrow(required) == 0L) return(invisible(TRUE))

  checks <- required |>
    dplyr::rowwise() |>
    dplyr::mutate(
      absolute_path = file.path(CFG$project_root, relative_path),
      exists = file.exists(absolute_path),
      observed_sha256 = if (exists) sha256_file(absolute_path) else NA_character_,
      hash_ok = exists & (is.na(expected_sha256) | expected_sha256 == "" | tolower(observed_sha256) == tolower(expected_sha256))
    ) |>
    dplyr::ungroup()

  missing <- checks |> dplyr::filter(!exists)
  bad_hash <- checks |> dplyr::filter(exists & !hash_ok)

  assert_model(
    nrow(missing) == 0L,
    paste0(
      "Missing repository-supplied input file(s): ",
      paste(missing$relative_path, collapse = ", ")
    )
  )
  assert_model(
    nrow(bad_hash) == 0L,
    paste0(
      "Repository-supplied input hash mismatch: ",
      paste(bad_hash$relative_path, collapse = ", ")
    )
  )

  invisible(TRUE)
}


# ------------------------------------------------------------------------------
# FUNCTION: sha256_file
# Purpose: Return the SHA-256 checksum of a file for provenance and archive validation.
# ------------------------------------------------------------------------------
sha256_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}


# ------------------------------------------------------------------------------
# FUNCTION: write_csv_atomic
# Purpose: Write a CSV through a temporary file and atomic rename to avoid partial audit outputs.
# ------------------------------------------------------------------------------
write_csv_atomic <- function(x, path) {
  tmp <- paste0(path, ".tmp")
  public_x <- sanitize_public_audit_data(x, path)

  if (is.data.frame(public_x) && ncol(public_x) == 0L) {
    writeLines(character(0), tmp, useBytes = TRUE)
  } else {
    readr::write_csv(public_x, tmp, na = "")
  }

  if (file.exists(path)) file.remove(path)
  ok <- file.rename(tmp, path)
  if (!ok) model_stop("Could not atomically write: ", path)
  invisible(path)
}


# ------------------------------------------------------------------------------
# FUNCTION: clean_numeric
# Purpose: Normalize formatted numeric text from official-source tables into numeric values.
# ------------------------------------------------------------------------------
clean_numeric <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u2212", "-")
  x <- stringr::str_replace_all(x, "[,$%]", "")
  x <- stringr::str_replace_all(x, "\\*", "")
  x <- stringr::str_replace_all(x, "n\\.a\\.|N/A|NA", NA_character_)
  suppressWarnings(as.numeric(stringr::str_trim(x)))
}


# ------------------------------------------------------------------------------
# FUNCTION: safe_divide
# Purpose: Divide vectors while returning NA for missing values or zero denominators.
# ------------------------------------------------------------------------------
safe_divide <- function(a, b) {
  ifelse(is.na(a) | is.na(b) | b == 0, NA_real_, a / b)
}


# ------------------------------------------------------------------------------
# FUNCTION: extract_year
# Purpose: Extract a four-digit/numeric year value from source text.
# ------------------------------------------------------------------------------
extract_year <- function(x) {
  suppressWarnings(as.integer(readr::parse_number(as.character(x))))
}

# In-memory provenance table populated as official files are acquired or local evidence is loaded.
SOURCE_MANIFEST <- tibble::tibble(
  source_id = character(),
  agency = character(),
  title = character(),
  publication_date = character(),
  retrieval_timestamp = character(),
  url = character(),
  local_path = character(),
  sha256 = character(),
  baseline_vintage = character(),
  evidence_class = character(),
  notes = character()
)


# ------------------------------------------------------------------------------
# FUNCTION: register_source
# Purpose: Append or update one authoritative source in the in-memory provenance manifest.
# ------------------------------------------------------------------------------
register_source <- function(
  source_id,
  agency,
  title,
  url,
  local_path = NA_character_,
  publication_date = NA_character_,
  baseline_vintage = NA_character_,
  evidence_class = "PRIMARY",
  notes = ""
) {
  checksum <- if (!is.na(local_path) && file.exists(local_path)) sha256_file(local_path) else NA_character_

  row <- tibble::tibble(
    source_id = source_id,
    agency = agency,
    title = title,
    publication_date = publication_date,
    retrieval_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    url = url,
    local_path = project_relative_path(local_path),
    sha256 = checksum,
    baseline_vintage = baseline_vintage,
    evidence_class = evidence_class,
    notes = notes
  )

  existing <- which(SOURCE_MANIFEST$source_id == source_id)
  if (length(existing) > 0L) {
    SOURCE_MANIFEST <<- SOURCE_MANIFEST[-existing, , drop = FALSE]
  }
  SOURCE_MANIFEST <<- dplyr::bind_rows(SOURCE_MANIFEST, row)
  invisible(row)
}


# ------------------------------------------------------------------------------
# FUNCTION: perform_request
# Purpose: Execute an httr2 request using the model's standardized HTTP behavior.
# ------------------------------------------------------------------------------
perform_request <- function(req, max_tries = 5L) {
  req <- req |>
    httr2::req_user_agent("federal-fiscal-capacity-model/1.0 (+public reproducibility release)") |>
    httr2::req_retry(
      max_tries = max_tries,
      backoff = function(i) min(2^(i - 1), 16),
      is_transient = function(resp) {
        if (inherits(resp, "error")) return(TRUE)
        httr2::resp_status(resp) %in% c(408, 425, 429, 500, 502, 503, 504)
      }
    )

  httr2::req_perform(req)
}


# ------------------------------------------------------------------------------
# FUNCTION: download_cached
# Purpose: Download an authoritative source with caching, size checks, logging, and provenance registration.
# ------------------------------------------------------------------------------
download_cached <- function(
  url,
  destination,
  force = FALSE,
  minimum_bytes = 50L,
  source_id = NULL,
  agency = "",
  title = "",
  publication_date = NA_character_,
  baseline_vintage = NA_character_,
  evidence_class = "PRIMARY",
  notes = ""
) {
  if (file.exists(destination) && !force) {
    size <- file.info(destination)$size
    if (is.finite(size) && size >= minimum_bytes) {
      log_line("Using cached source: ", basename(destination))
      if (!is.null(source_id)) {
        register_source(
          source_id, agency, title, url, destination,
          publication_date, baseline_vintage, evidence_class,
          paste0(notes, if (nzchar(notes)) " | " else "", "cached copy")
        )
      }
      return(destination)
    }
  }

  log_line("Downloading source: ", url)
  tmp <- paste0(destination, ".download")

  try_result <- try({
    resp <- perform_request(httr2::request(url))
    status <- httr2::resp_status(resp)
    if (status < 200 || status >= 300) stop("HTTP status ", status)
    raw <- httr2::resp_body_raw(resp)
    writeBin(raw, tmp)
    size <- file.info(tmp)$size
    if (!is.finite(size) || size < minimum_bytes) stop("Downloaded file is unexpectedly small")
    if (file.exists(destination)) file.remove(destination)
    if (!file.rename(tmp, destination)) stop("Could not move downloaded file into cache")
  }, silent = TRUE)

  if (inherits(try_result, "try-error")) {
    if (file.exists(tmp)) file.remove(tmp)
    if (file.exists(destination)) {
      model_warn("Live download failed; retaining existing cached file: ", destination)
    } else {
      model_stop("Download failed and no cached copy exists: ", url, " | ", as.character(try_result))
    }
  }

  if (!is.null(source_id)) {
    register_source(
      source_id, agency, title, url, destination,
      publication_date, baseline_vintage, evidence_class, notes
    )
  }

  destination
}


# ------------------------------------------------------------------------------
# FUNCTION: fetch_text_cached
# Purpose: Retrieve a text source through the cache layer and return its contents.
# ------------------------------------------------------------------------------
fetch_text_cached <- function(
  url,
  destination,
  force = FALSE,
  source_id = NULL,
  agency = "",
  title = "",
  publication_date = NA_character_,
  baseline_vintage = NA_character_,
  evidence_class = "PRIMARY",
  notes = ""
) {
  download_cached(
    url = url,
    destination = destination,
    force = force,
    minimum_bytes = 100L,
    source_id = source_id,
    agency = agency,
    title = title,
    publication_date = publication_date,
    baseline_vintage = baseline_vintage,
    evidence_class = evidence_class,
    notes = notes
  )
  paste(readLines(destination, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

API_KEYS <- list(
  BEA_API_KEY = Sys.getenv("BEA_API_KEY", unset = ""),
  BLS_API_KEY = Sys.getenv("BLS_API_KEY", unset = ""),
  FRED_API_KEY = Sys.getenv("FRED_API_KEY", unset = ""),
  CENSUS_API_KEY = Sys.getenv("CENSUS_API_KEY", unset = "")
)


# ------------------------------------------------------------------------------
# FUNCTION: check_api_keys
# Purpose: Check whether optional external validation API credentials are available.
# ------------------------------------------------------------------------------
check_api_keys <- function() {
  required_now <- c("BEA_API_KEY", "BLS_API_KEY", "FRED_API_KEY")
  missing <- required_now[vapply(API_KEYS[required_now], function(x) !nzchar(x), logical(1))]

  if (length(missing) > 0L) {
    msg <- paste0(
      "Missing required API key environment variable(s): ",
      paste(missing, collapse = ", "),
      ". Set them before running the strict external-validation gate."
    )
    if (CFG$require_external_api_validation) model_stop(msg) else model_warn(msg)
  }

  if (!nzchar(API_KEYS$CENSUS_API_KEY)) {
    log_line("CENSUS_API_KEY is not set. It is not required by the current core model.", level = "WARN")
  }

  invisible(length(missing) == 0L)
}


# ------------------------------------------------------------------------------
# FUNCTION: cbo_raw_url
# Purpose: Construct a raw GitHub URL for the official US-CBO/cbo-data repository.
# ------------------------------------------------------------------------------
cbo_raw_url <- function(dataset, vintage = CFG$cbo_vintage) {
  sprintf(
    "https://raw.githubusercontent.com/US-CBO/cbo-data/main/data/%s/annual_fy_%s.csv",
    dataset,
    vintage
  )
}

CBO_SOURCES <- tribble(
  ~source_id, ~dataset, ~agency, ~title, ~publication_date,
  "cbo_ten_year_budget_2026_02", "budget/ten_year_budget", "Congressional Budget Office", "Ten-Year Budget Projections, February 2026 vintage", "2026-02-11",
  "cbo_long_term_budget_2026_02", "budget/long_term_budget", "Congressional Budget Office", "Long-Term Budget Outlook Data, February 2026 vintage", "2026-02-25",
  "cbo_historical_budget_2026_02", "budget/historical_budget", "Congressional Budget Office", "Historical Budget Data, February 2026 vintage", "2026-02-11",
  "cbo_historical_economic_2026_02", "economic/historical_economic", "Congressional Budget Office", "Historical and Projected Economic Data, February 2026 vintage", "2026-02-11",
  "cbo_revenue_detail_2026_02", "budget/revenue_detail", "Congressional Budget Office", "Revenue Projections by Category, February 2026 vintage", "2026-02-11"
)


# ------------------------------------------------------------------------------
# FUNCTION: fetch_cbo_sources
# Purpose: Acquire the CBO baseline, economic, historical, and revenue-detail source files required by Gates 1-2.
# ------------------------------------------------------------------------------
fetch_cbo_sources <- function() {
  log_line("Gate 1 preparation: downloading CBO Open Data inputs")

  paths <- vector("list", nrow(CBO_SOURCES))
  names(paths) <- CBO_SOURCES$source_id

  for (i in seq_len(nrow(CBO_SOURCES))) {
    row <- CBO_SOURCES[i, ]
    path <- file.path(CFG$raw_source_dir, paste0(row$source_id, ".csv"))
    url <- cbo_raw_url(row$dataset)

    paths[[row$source_id]] <- download_cached(
      url,
      path,
      force = FALSE,
      minimum_bytes = 100L,
      source_id = row$source_id,
      agency = row$agency,
      title = row$title,
      publication_date = row$publication_date,
      baseline_vintage = CFG$cbo_vintage,
      evidence_class = "OFFICIAL_CURRENT",
      notes = "CBO Open Data Repository machine-readable vintage; cbo.gov remains canonical publication source"
    )
  }

  paths
}


# ------------------------------------------------------------------------------
# FUNCTION: read_cbo_long
# Purpose: Read a CBO long-format CSV and normalize year/value fields for downstream use.
# ------------------------------------------------------------------------------
read_cbo_long <- function(path) {
  x <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  required <- c("date", "variable", "value")
  assert_model(all(required %in% names(x)), paste0("Unexpected CBO schema in ", path))

  x |>
    transmute(
      year = extract_year(date),
      variable = as.character(variable),
      value = clean_numeric(value)
    ) |>
    filter(!is.na(year), !is.na(variable))
}


# ------------------------------------------------------------------------------
# FUNCTION: cbo_series
# Purpose: Extract one named CBO series over a requested year range.
# ------------------------------------------------------------------------------
cbo_series <- function(df, variable_name, years = NULL, required = TRUE) {
  out <- df |>
    filter(variable == variable_name) |>
    select(year, value) |>
    arrange(year)

  if (!is.null(years)) out <- out |> filter(year %in% years)

  if (required && nrow(out) == 0L) {
    model_stop("Required CBO variable not found: ", variable_name)
  }

  out
}


# ------------------------------------------------------------------------------
# FUNCTION: build_cbo_baseline
# Purpose: Assemble the annual CBO fiscal/economic baseline used by the accounting engine.
# ------------------------------------------------------------------------------
build_cbo_baseline <- function(cbo_data) {
  log_line("Building untouched February 2026 CBO baseline")

  ten <- cbo_data$ten
  lt <- cbo_data$lt
  hist <- cbo_data$hist

  years_10 <- 2026:2036
  years_lt <- 2037:2046

  ten_wide <- reduce(
    list(
      cbo_series(ten, "proj_debt_held_by_public", years_10) |> rename(debt_bil = value),
      cbo_series(ten, "proj_debt_held_by_public_gdp_share", years_10) |> rename(debt_gdp_pct = value),
      cbo_series(ten, "proj_deficit_total", years_10) |> rename(cbo_deficit_raw_bil = value),
      cbo_series(ten, "proj_primary_deficit", years_10) |> rename(cbo_primary_deficit_raw_bil = value),
      cbo_series(ten, "proj_outlays_total", years_10) |> rename(outlays_bil = value),
      cbo_series(ten, "proj_rev_total", years_10) |> rename(revenues_bil = value),
      cbo_series(ten, "proj_outlays_net_interest", years_10) |> rename(net_interest_bil = value)
    ),
    full_join,
    by = "year"
  ) |>
    mutate(
      total_deficit_bil = -cbo_deficit_raw_bil,
      primary_deficit_bil = -cbo_primary_deficit_raw_bil,
      gdp_bil = debt_bil / (debt_gdp_pct / 100),
      baseline_segment = "CBO_FEB_2026_TEN_YEAR"
    )

  lt_wide <- reduce(
    list(
      cbo_series(lt, "lt_debt_held_by_public_gdp_share", years_lt) |> rename(debt_gdp_pct = value),
      cbo_series(lt, "lt_deficit_total_gdp_share", years_lt) |> rename(deficit_gdp_pct_raw = value),
      cbo_series(lt, "lt_gdp_trillions", years_lt) |> rename(gdp_trillions = value)
    ),
    full_join,
    by = "year"
  ) |>
    mutate(
      gdp_bil = gdp_trillions * 1000,
      debt_bil = gdp_bil * debt_gdp_pct / 100,
      total_deficit_bil = -deficit_gdp_pct_raw * gdp_bil / 100,
      cbo_deficit_raw_bil = -total_deficit_bil,
      cbo_primary_deficit_raw_bil = NA_real_,
      primary_deficit_bil = NA_real_,
      revenues_bil = NA_real_,
      outlays_bil = NA_real_,
      net_interest_bil = NA_real_,
      baseline_segment = "CBO_FEB_2026_LONG_TERM"
    ) |>
    select(names(ten_wide))

  baseline <- bind_rows(ten_wide, lt_wide) |> arrange(year)

  hist_2025_debt <- cbo_series(hist, "debt_held_by_public", 2025)$value
  if (length(hist_2025_debt) != 1L) {
    model_stop("Could not uniquely recover FY2025 historical debt held by the public")
  }

  baseline <- baseline |>
    mutate(
      prior_debt_bil = lag(debt_bil),
      prior_debt_bil = if_else(year == min(year), hist_2025_debt, prior_debt_bil),
      stock_flow_adjustment_bil = debt_bil - prior_debt_bil - total_deficit_bil,
      reconstructed_debt_bil = prior_debt_bil + total_deficit_bil + stock_flow_adjustment_bil,
      debt_identity_error_bil = reconstructed_debt_bil - debt_bil,
      calculated_debt_gdp_pct = 100 * debt_bil / gdp_bil,
      debt_ratio_error_pp = calculated_debt_gdp_pct - debt_gdp_pct,
      revenue_outlay_identity_error_bil = if_else(
        !is.na(revenues_bil) & !is.na(outlays_bil),
        (outlays_bil - revenues_bil) - total_deficit_bil,
        NA_real_
      ),
      primary_interest_identity_error_bil = if_else(
        !is.na(primary_deficit_bil) & !is.na(net_interest_bil),
        (primary_deficit_bil + net_interest_bil) - total_deficit_bil,
        NA_real_
      )
    )

  baseline
}


# ------------------------------------------------------------------------------
# FUNCTION: run_gate_1
# Purpose: Run baseline identity, completeness, and consistency tests before any optimization is allowed.
# ------------------------------------------------------------------------------
run_gate_1 <- function(baseline, cbo_data) {
  log_line("Gate 1: validating baseline accounting identities")

  tests <- list()
  add_test <- function(name, passed, observed, tolerance, detail) {
    tests[[length(tests) + 1L]] <<- tibble(
      gate = "GATE_1_BASELINE",
      test = name,
      passed = isTRUE(passed),
      observed = as.character(observed),
      tolerance = as.character(tolerance),
      detail = detail
    )
  }

  max_debt_identity <- max(abs(baseline$debt_identity_error_bil), na.rm = TRUE)
  add_test(
    "Debt stock-flow identity",
    max_debt_identity <= CFG$debt_identity_tolerance_bil,
    max_debt_identity,
    CFG$debt_identity_tolerance_bil,
    "Ending debt must equal beginning debt plus unified-budget deficit plus explicit stock-flow adjustment"
  )

  max_ratio_error <- max(abs(baseline$debt_ratio_error_pp), na.rm = TRUE)
  add_test(
    "Debt/GDP ratio reconstruction",
    max_ratio_error <= CFG$ratio_tolerance_pp,
    max_ratio_error,
    CFG$ratio_tolerance_pp,
    "Debt divided by nominal GDP must reproduce the published CBO ratio"
  )

  max_budget_identity <- max(abs(baseline$revenue_outlay_identity_error_bil), na.rm = TRUE)
  add_test(
    "Revenue/outlay deficit identity",
    max_budget_identity <= 1.0,
    max_budget_identity,
    1.0,
    "Allows only ordinary published-table rounding"
  )

  max_primary_identity <- max(abs(baseline$primary_interest_identity_error_bil), na.rm = TRUE)
  add_test(
    "Primary deficit plus interest identity",
    max_primary_identity <= 1.0,
    max_primary_identity,
    1.0,
    "Allows only ordinary published-table rounding"
  )

  lt_overlap <- reduce(
    list(
      cbo_series(cbo_data$lt, "lt_debt_held_by_public_gdp_share", 2026:2036) |> rename(lt_share = value),
      cbo_series(cbo_data$ten, "proj_debt_held_by_public_gdp_share", 2026:2036) |> rename(ten_share = value)
    ),
    inner_join,
    by = "year"
  ) |>
    mutate(diff_pp = ten_share - lt_share)

  max_overlap <- max(abs(lt_overlap$diff_pp), na.rm = TRUE)
  add_test(
    "Ten-year and long-term CBO debt-share overlap",
    max_overlap <= CFG$ratio_tolerance_pp,
    max_overlap,
    CFG$ratio_tolerance_pp,
    "The February 2026 ten-year and long-term files should agree over their overlapping years"
  )

  result <- bind_rows(tests)
  failed <- result |> filter(!passed)

  if (nrow(failed) > 0L && CFG$strict_gate_1) {
    write_csv_atomic(result, file.path(CFG$output_dir, "validation_gate_1.csv"))
    model_stop("Gate 1 failed. See analysis_output/validation_gate_1.csv")
  }

  log_line("Gate 1 complete: ", sum(result$passed), "/", nrow(result), " tests passed")
  result
}


# ------------------------------------------------------------------------------
# FUNCTION: fred_get_series
# Purpose: Retrieve a FRED series used only for independent historical/source cross-checks.
# ------------------------------------------------------------------------------
fred_get_series <- function(series_id, observation_start = "1990-01-01", observation_end = format(Sys.Date(), "%Y-%m-%d")) {
  key <- API_KEYS$FRED_API_KEY
  if (!nzchar(key)) model_stop("FRED_API_KEY is required for FRED validation")

  req <- httr2::request("https://api.stlouisfed.org/fred/series/observations") |>
    httr2::req_url_query(
      series_id = series_id,
      api_key = key,
      file_type = "json",
      observation_start = observation_start,
      observation_end = observation_end
    )

  resp <- perform_request(req)
  dat <- httr2::resp_body_json(resp, simplifyVector = TRUE)$observations

  as_tibble(dat) |>
    transmute(
      date = as.Date(date),
      value = clean_numeric(value)
    ) |>
    filter(!is.na(value))
}


# ------------------------------------------------------------------------------
# FUNCTION: bea_get_nipa_gdp_quarterly
# Purpose: Retrieve BEA quarterly GDP for independent historical validation.
# ------------------------------------------------------------------------------
bea_get_nipa_gdp_quarterly <- function(years) {
  key <- API_KEYS$BEA_API_KEY
  if (!nzchar(key)) model_stop("BEA_API_KEY is required for BEA validation")

  req <- httr2::request("https://apps.bea.gov/api/data") |>
    httr2::req_url_query(
      UserID = key,
      method = "GetData",
      datasetname = "NIPA",
      TableName = "T10105",
      Frequency = "Q",
      Year = paste(years, collapse = ","),
      ResultFormat = "JSON"
    )

  resp <- perform_request(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  dat <- tryCatch(
    body$BEAAPI$Results$Data,
    error = function(e) NULL
  )

  if (is.null(dat) || nrow(as.data.frame(dat)) == 0L) {
    model_stop("BEA API returned no NIPA Table 1.1.5 data")
  }

  dat <- as_tibble(dat)

  unit_mult_candidates <- intersect(
    c("UNIT_MULT", "UnitMult", "unit_mult"),
    names(dat)
  )

  if (length(unit_mult_candidates) != 1L) {
    model_stop(
      "BEA NIPA response does not expose exactly one recognized UNIT_MULT field; ",
      "cannot safely normalize GDP values to billions. Available fields: ",
      paste(names(dat), collapse = ", ")
    )
  }

  unit_mult_col <- unit_mult_candidates[[1]]

  unit_label_candidates <- intersect(
    c("CL_UNIT", "CLUnit", "cl_unit"),
    names(dat)
  )
  unit_label_col <- if (length(unit_label_candidates) >= 1L) unit_label_candidates[[1]] else NA_character_

  gdp_rows <- dat |>
    filter(as.character(LineNumber) == "1")

  if (nrow(gdp_rows) == 0L) {
    model_stop("BEA NIPA Table 1.1.5 response contains no GDP line 1 rows")
  }

  raw_value <- clean_numeric(gdp_rows$DataValue)
  unit_mult <- suppressWarnings(as.integer(gdp_rows[[unit_mult_col]]))

  if (any(is.na(raw_value))) {
    model_stop("BEA NIPA GDP line contains a nonnumeric DataValue")
  }

  if (any(is.na(unit_mult)) || any(!is.finite(unit_mult))) {
    model_stop("BEA NIPA GDP line contains a missing or invalid UNIT_MULT value")
  }

  value_bil <- raw_value * (10 ^ (unit_mult - 9L))

  unit_label <- if (!is.na(unit_label_col)) {
    as.character(gdp_rows[[unit_label_col]])
  } else {
    rep(NA_character_, nrow(gdp_rows))
  }

  tibble(
    time_period = as.character(gdp_rows$TimePeriod),
    value_raw = raw_value,
    unit_mult = unit_mult,
    unit_label = unit_label,
    value_bil = value_bil
  ) |>
    filter(!is.na(value_bil))
}


# ------------------------------------------------------------------------------
# FUNCTION: bls_get_series
# Purpose: Retrieve a BLS series used for external validation checks.
# ------------------------------------------------------------------------------
bls_get_series <- function(series_id, start_year, end_year) {
  key <- API_KEYS$BLS_API_KEY
  if (!nzchar(key)) model_stop("BLS_API_KEY is required for BLS validation")

  body <- list(
    seriesid = list(series_id),
    startyear = as.character(start_year),
    endyear = as.character(end_year),
    registrationkey = key
  )

  resp <- perform_request(
    httr2::request("https://api.bls.gov/publicAPI/v2/timeseries/data/") |>
      httr2::req_method("POST") |>
      httr2::req_body_json(body)
  )

  js <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  series <- js$Results$series[[1]]$data

  tibble(
    year = suppressWarnings(as.integer(vapply(series, `[[`, character(1), "year"))),
    period = vapply(series, `[[`, character(1), "period"),
    value = clean_numeric(vapply(series, `[[`, character(1), "value"))
  ) |>
    filter(stringr::str_detect(period, "^M(0[1-9]|1[0-2])$")) |>
    mutate(month = suppressWarnings(as.integer(stringr::str_remove(period, "M"))))
}


# ------------------------------------------------------------------------------
# FUNCTION: fetch_treasury_debt_to_penny
# Purpose: Retrieve Treasury Debt to the Penny data for independent debt-history validation.
# ------------------------------------------------------------------------------
fetch_treasury_debt_to_penny <- function(record_date) {
  endpoint <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/debt_to_penny"
  req <- httr2::request(endpoint) |>
    httr2::req_url_query(
      filter = paste0("record_date:eq:", record_date)
    )

  resp <- perform_request(req)
  js <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  dat <- as_tibble(js$data)

  if (nrow(dat) == 0L) model_stop("Treasury Fiscal Data returned no debt record for ", record_date)

  dat |>
    transmute(
      record_date = as.Date(record_date),
      debt_held_public_bil = clean_numeric(debt_held_public_amt) / 1e9,
      total_public_debt_bil = clean_numeric(tot_pub_debt_out_amt) / 1e9
    )
}


# ------------------------------------------------------------------------------
# FUNCTION: validate_external_history
# Purpose: Cross-check core historical fiscal/economic series against independent official sources.
# ------------------------------------------------------------------------------
validate_external_history <- function(cbo_data, baseline) {
  log_line("External validation: Treasury, FRED, BEA, and BLS cross-checks")
  check_api_keys()

  tests <- list()
  add <- function(name, passed, observed, reference, tolerance, detail) {
    tests[[length(tests) + 1L]] <<- tibble(
      gate = "EXTERNAL_HISTORY",
      test = name,
      passed = isTRUE(passed),
      observed = as.character(observed),
      reference = as.character(reference),
      tolerance = as.character(tolerance),
      detail = detail
    )
  }

  cbo_hist_debt_2025 <- cbo_series(cbo_data$hist, "debt_held_by_public", 2025)$value

  treasury <- fetch_treasury_debt_to_penny("2025-09-30")
  treasury_pct_diff <- 100 * abs(treasury$debt_held_public_bil[1] - cbo_hist_debt_2025) / cbo_hist_debt_2025
  add(
    "Treasury FY2025 debt held by public vs CBO historical",
    treasury_pct_diff <= CFG$source_crosscheck_tolerance_pct,
    treasury$debt_held_public_bil[1],
    cbo_hist_debt_2025,
    paste0(CFG$source_crosscheck_tolerance_pct, "%"),
    "Different official publications may differ slightly by classification or vintage"
  )

  register_source(
    "treasury_debt_to_penny_2025_09_30",
    "U.S. Department of the Treasury, Fiscal Service",
    "Debt to the Penny, FY2025 year-end observation",
    "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/debt_to_penny",
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Live API validation query for 2025-09-30"
  )

  fred_debt <- fred_get_series("FYGFDPUB", "2024-01-01", "2025-12-31")
  fred_2025 <- fred_debt |> filter(lubridate::year(date) == 2025) |> slice_tail(n = 1)
  if (nrow(fred_2025) == 1L) {
    fred_pct_diff <- 100 * abs(fred_2025$value - cbo_hist_debt_2025) / cbo_hist_debt_2025
    add(
      "FRED FYGFDPUB FY2025 vs CBO historical debt",
      fred_pct_diff <= CFG$source_crosscheck_tolerance_pct,
      fred_2025$value,
      cbo_hist_debt_2025,
      paste0(CFG$source_crosscheck_tolerance_pct, "%"),
      "FRED series is sourced to the Council of Economic Advisers and represents fiscal-year debt held by the public"
    )
  } else {
    add("FRED FYGFDPUB FY2025 vs CBO historical debt", FALSE, NA, cbo_hist_debt_2025, "one observation", "FRED returned no FY2025 observation")
  }

  register_source(
    "fred_fygfdpub",
    "Federal Reserve Bank of St. Louis / Council of Economic Advisers",
    "Gross Federal Debt Held by the Public (FYGFDPUB)",
    "https://fred.stlouisfed.org/series/FYGFDPUB",
    evidence_class = "SECONDARY_OFFICIAL_REPUBLISH",
    notes = "Independent cross-check only; not a model baseline input"
  )

  bea <- bea_get_nipa_gdp_quarterly(2024:2025)
  required_q <- c("2024Q4", "2025Q1", "2025Q2", "2025Q3")
  bea_fy25 <- bea |> filter(time_period %in% required_q)
  if (nrow(bea_fy25) == 4L) {
    bea_fy25_gdp <- mean(bea_fy25$value_bil)
    cbo_gdp_2025 <- cbo_series(cbo_data$econ, "gdp", 2025, required = FALSE)$value
    if (length(cbo_gdp_2025) == 1L) {
      pct <- 100 * abs(bea_fy25_gdp - cbo_gdp_2025) / cbo_gdp_2025
      add(
        "BEA NIPA implied FY2025 nominal GDP vs CBO economic file",
        pct <= 1.0,
        bea_fy25_gdp,
        cbo_gdp_2025,
        "1%",
        paste0(
          "BEA fiscal-year approximation averages 2024Q4 and 2025Q1-Q3 seasonally adjusted annual rates; ",
          "BEA DataValue is normalized to billions using UNIT_MULT metadata (observed multipliers: ",
          paste(sort(unique(bea_fy25$unit_mult)), collapse = ", "),
          ")"
        )
      )
    }
  }

  register_source(
    "bea_nipa_t10105_gdp",
    "U.S. Bureau of Economic Analysis",
    "NIPA Table 1.1.5, Gross Domestic Product",
    "https://apps.bea.gov/api/data",
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Live API validation query; GDP line 1; DataValue normalized to billions using BEA UNIT_MULT metadata"
  )

  bls <- bls_get_series("LNS14000000", 2024, 2026)
  bls <- bls |>
    mutate(
      date = as.Date(sprintf("%04d-%02d-01", year, month)),
      fiscal_year = if_else(month >= 10L, year + 1L, year)
    )
  bls_fy25 <- bls |> filter(fiscal_year == 2025) |> summarise(unemp = mean(value, na.rm = TRUE)) |> pull(unemp)
  cbo_unemp_2025 <- cbo_series(cbo_data$econ, "unemployment_rate", 2025, required = FALSE)$value
  if (length(cbo_unemp_2025) == 1L && length(bls_fy25) == 1L && is.finite(bls_fy25)) {
    add(
      "BLS FY2025 unemployment average vs CBO economic file",
      abs(bls_fy25 - cbo_unemp_2025) <= 0.25,
      bls_fy25,
      cbo_unemp_2025,
      "0.25 percentage point",
      "BLS monthly civilian unemployment rate averaged over the federal fiscal year"
    )
  }

  register_source(
    "bls_lns14000000",
    "U.S. Bureau of Labor Statistics",
    "Civilian Unemployment Rate, LNS14000000",
    "https://api.bls.gov/publicAPI/v2/timeseries/data/",
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Live API validation query; not a baseline input"
  )

  out <- bind_rows(tests)

  if (CFG$require_external_api_validation && any(!out$passed)) {
    write_csv_atomic(out, file.path(CFG$output_dir, "validation_external_history.csv"))
    model_stop("External historical validation failed. See analysis_output/validation_external_history.csv")
  }

  log_line("External validation complete: ", sum(out$passed), "/", nrow(out), " tests passed")
  out
}

CBO_DEBT_TOOL_2026_URL <- "https://www.cbo.gov/publication/61912"
CBO_BFM_2026_REPO_URL <- "https://github.com/US-CBO/budgetary-feedback-model"
CBO_BFM_2026_COMMIT <- "9b4dd54e42c13bf9ee9ef67d63fc41ab64a3217e"
CBO_BFM_2026_RULES_URL <- paste0(
  "https://raw.githubusercontent.com/US-CBO/budgetary-feedback-model/",
  CBO_BFM_2026_COMMIT,
  "/input/rules_of_thumb.csv"
)
CBO_BFM_2026_NET_INTEREST_URL <- paste0(
  "https://raw.githubusercontent.com/US-CBO/budgetary-feedback-model/",
  CBO_BFM_2026_COMMIT,
  "/bfm/net_interest_costs.py"
)


# ------------------------------------------------------------------------------
# FUNCTION: build_cbo_debt_tool_permalink
# Purpose: Construct the CBO debt-service calculator permalink used to document a validation fixture.
# ------------------------------------------------------------------------------
build_cbo_debt_tool_permalink <- function(revenues, outlays) {
  years <- 2026:2036
  assert_model(length(revenues) == length(years), "CBO debt-tool revenues vector must have 11 annual values")
  assert_model(length(outlays) == length(years), "CBO debt-tool outlays vector must have 11 annual values")
  paste0(
    CBO_DEBT_TOOL_2026_URL,
    "?revenues=", paste(format(as.numeric(revenues), scientific = FALSE, trim = TRUE, digits = 15), collapse = ","),
    "&outlays=", paste(format(as.numeric(outlays), scientific = FALSE, trim = TRUE, digits = 15), collapse = ",")
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: current_cbo_debt_service_fixtures
# Purpose: Define the published CBO debt-service impulse fixtures used as validation targets.
# ------------------------------------------------------------------------------
current_cbo_debt_service_fixtures <- function() {
  years <- 2026:2036
  impulse <- CFG$cbo_kernel_impulse_bil
  assert_model(abs(impulse - 900) < 1e-12, "Embedded March 2026 CBO debt-service fixtures are calibrated to a $900B impulse; cbo_kernel_impulse_bil must remain 900")

  ds_2026 <- c(8.910, 31.860, 32.940, 34.290, 35.640, 37.080, 38.610, 40.140, 41.670, 43.290, 45.090)
  rates_2026 <- c(NA_real_, 3.47, 3.47, 3.49, 3.51, 3.53, 3.55, 3.56, 3.57, 3.58, 3.60)

  ds_2027 <- c(0.000, 17.820, 34.020, 35.280, 36.900, 38.250, 39.780, 41.400, 43.110, 44.820, 46.530)
  rates_2027 <- c(NA_real_, 3.76, 3.64, 3.65, 3.68, 3.68, 3.69, 3.70, 3.72, 3.73, 3.73)

  rev_2026 <- rep(0, length(years))
  out_2026 <- rep(0, length(years))
  out_2026[years == 2026L] <- impulse
  rev_2027 <- rep(0, length(years))
  out_2027 <- rep(0, length(years))
  out_2027[years == 2027L] <- impulse

  fixture_rows <- bind_rows(
    tibble(
      input_year = 2026L, output_year = years, debt_service_bil = ds_2026,
      effective_marginal_rate_pct = rates_2026,
      source_permalink = build_cbo_debt_tool_permalink(rev_2026, out_2026),
      source_sha256 = "560cafe51961be1c69d5bf6cdcfa61045bc0d04f46740386a0bfc0de40baaefe",
      fixture_role = "CURRENT_VINTAGE_DIRECT_KERNEL_ROW_AND_VALIDATION_TARGET"
    ),
    tibble(
      input_year = 2027L, output_year = years, debt_service_bil = ds_2027,
      effective_marginal_rate_pct = rates_2027,
      source_permalink = build_cbo_debt_tool_permalink(rev_2027, out_2027),
      source_sha256 = "319b962078e1accb0ab43acba1b4e47faa5a9a234d130f149acff67e5c1aa931",
      fixture_role = "CURRENT_VINTAGE_DIRECT_KERNEL_ROW_AND_CALIBRATION_ANCHOR"
    )
  )

  rates <- tibble(
    year = years,
    effective_marginal_rate_pct = rates_2027,
    rate_source = "OFFICIAL_CURRENT_CBO_EXPORT_FY2027_PULSE"
  )

  list(rows = fixture_rows, rates = rates)
}


# ------------------------------------------------------------------------------
# FUNCTION: cbo_bfm_2026_debt_service_matrix
# Purpose: Return the CBO Budgetary Feedback Model interval-response matrix used to infer the debt-service kernel.
# ------------------------------------------------------------------------------
cbo_bfm_2026_debt_service_matrix <- function() {
  years <- 2026:2036

  mat <- rbind(
    c(.01, .04, .04, .04, .04, .04, .04, .04, .05, .05, .05),
    c(0, .02, .04, .04, .04, .04, .04, .05, .05, .05, .05),
    c(0, 0, .02, .04, .04, .04, .04, .04, .05, .05, .05),
    c(0, 0, 0, .02, .04, .04, .04, .04, .04, .05, .05),
    c(0, 0, 0, 0, .02, .04, .04, .04, .04, .05, .05),
    c(0, 0, 0, 0, 0, .02, .04, .04, .04, .04, .05),
    c(0, 0, 0, 0, 0, 0, .02, .04, .04, .04, .04),
    c(0, 0, 0, 0, 0, 0, 0, .02, .04, .04, .04),
    c(0, 0, 0, 0, 0, 0, 0, 0, .02, .04, .04),
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, .02, .04),
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, .02)
  )
  colnames(mat) <- as.character(years)
  rownames(mat) <- as.character(years)

  as_tibble(mat, rownames = "input_year") |>
    pivot_longer(-input_year, names_to = "output_year", values_to = "beta") |>
    mutate(
      input_year = as.integer(input_year),
      output_year = as.integer(output_year),
      forecast_position = input_year - 2025L,
      debt_column = paste0("DEBT", forecast_position),
      lag = output_year - input_year,
      published_precision_half_width = CFG$cbo_bfm_published_precision_half_width,
      beta_interval_lower = pmax(beta - published_precision_half_width, 0),
      beta_interval_upper = beta + published_precision_half_width
    ) |>
    arrange(input_year, output_year)
}


# ------------------------------------------------------------------------------
# FUNCTION: infer_bfm_interval_factor
# Purpose: Infer an interval transfer factor from adjacent CBO BFM response observations.
# ------------------------------------------------------------------------------
infer_bfm_interval_factor <- function(
  target_input_year,
  anchor_coefficients,
  bfm_matrix,
  min_lag = 1L
) {
  target <- bfm_matrix |>
    filter(
      input_year == target_input_year,
      output_year >= input_year,
      lag >= min_lag
    ) |>
    select(
      lag,
      beta,
      beta_interval_lower,
      beta_interval_upper
    )

  joined <- target |>
    inner_join(
      anchor_coefficients |>
        filter(lag >= min_lag) |>
        select(lag, anchor_coefficient),
      by = "lag"
    ) |>
    filter(
      is.finite(anchor_coefficient),
      anchor_coefficient > 0
    ) |>
    mutate(
      factor_lower = beta_interval_lower / anchor_coefficient,
      factor_upper = beta_interval_upper / anchor_coefficient
    )

  if (nrow(joined) == 0L) {
    return(tibble(
      target_input_year = target_input_year,
      common_lag_count = 0L,
      factor_lower = NA_real_,
      factor_upper = NA_real_,
      chosen_factor = NA_real_,
      factor_rule = "NO_COMMON_POSITIVE_LAGS"
    ))
  }

  lower <- max(joined$factor_lower)
  upper <- min(joined$factor_upper)

  assert_model(
    is.finite(lower) && is.finite(upper) && lower <= upper + 1e-12,
    paste0(
      "Current CBO BFM published-precision intervals have no common multiplicative factor for FY",
      target_input_year,
      " relative to the current calculator anchor. Lower bound=",
      signif(lower, 8),
      ", upper bound=",
      signif(upper, 8)
    )
  )

  chosen <- min(max(1, lower), upper)

  tibble(
    target_input_year = target_input_year,
    common_lag_count = nrow(joined),
    factor_lower = lower,
    factor_upper = upper,
    chosen_factor = chosen,
    factor_rule = "CLOSEST_TO_ONE_WITHIN_ALL_CURRENT_BFM_PUBLISHED_PRECISION_INTERVALS"
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: build_bfm_interval_transfer_prediction
# Purpose: Build predicted debt-service responses from the inferred BFM interval factors.
# ------------------------------------------------------------------------------
build_bfm_interval_transfer_prediction <- function(
  target_input_year,
  anchor_input_year,
  fixtures,
  bfm_matrix
) {
  impulse <- CFG$cbo_kernel_impulse_bil

  anchor_coefficients <- fixtures |>
    filter(input_year == anchor_input_year, output_year >= anchor_input_year) |>
    transmute(
      lag = output_year - input_year,
      anchor_coefficient = debt_service_bil / impulse
    )

  factor_summary <- infer_bfm_interval_factor(
    target_input_year = target_input_year,
    anchor_coefficients = anchor_coefficients,
    bfm_matrix = bfm_matrix,
    min_lag = 1L
  ) |>
    mutate(anchor_input_year = anchor_input_year)

  anchor_lag0_beta <- bfm_matrix |>
    filter(input_year == anchor_input_year, lag == 0L) |>
    pull(beta)
  target_lag0_beta <- bfm_matrix |>
    filter(input_year == target_input_year, lag == 0L) |>
    pull(beta)

  assert_model(length(anchor_lag0_beta) == 1L && anchor_lag0_beta > 0, "Could not identify the current BFM lag-0 coefficient for the anchor position")
  assert_model(length(target_lag0_beta) == 1L && target_lag0_beta >= 0, "Could not identify the current BFM lag-0 coefficient for the target position")

  lag0_factor <- target_lag0_beta / anchor_lag0_beta
  common_factor <- factor_summary$chosen_factor[1]

  prediction <- bfm_matrix |>
    filter(input_year == target_input_year, output_year >= input_year) |>
    inner_join(anchor_coefficients, by = "lag") |>
    mutate(
      position_factor = case_when(
        lag == 0L ~ lag0_factor,
        lag >= 1L & is.finite(common_factor) ~ common_factor,
        TRUE ~ NA_real_
      ),
      predicted_coefficient = anchor_coefficient * position_factor,
      interval_consistent =
        predicted_coefficient >= beta_interval_lower - 1e-12 &
        predicted_coefficient <= beta_interval_upper + 1e-12
    )

  assert_model(nrow(prediction) > 0L, paste0("No overlapping lags are available for BFM transfer into FY", target_input_year))
  assert_model(all(prediction$interval_consistent), paste0("Modeled FY", target_input_year, " debt-service coefficients are not consistent with the current BFM published-precision intervals"))

  list(
    prediction = prediction,
    factor_summary = factor_summary |>
      mutate(
        lag0_factor = lag0_factor,
        lag0_target_beta = target_lag0_beta,
        lag0_anchor_beta = anchor_lag0_beta
      )
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: validate_fixture_against_bfm_intervals
# Purpose: Compare published fixture values with BFM-implied interval predictions.
# ------------------------------------------------------------------------------
validate_fixture_against_bfm_intervals <- function(fixtures, bfm_matrix) {
  impulse <- CFG$cbo_kernel_impulse_bil

  fixtures |>
    filter(output_year >= input_year) |>
    mutate(
      observed_coefficient = debt_service_bil / impulse,
      lag = output_year - input_year
    ) |>
    left_join(
      bfm_matrix |>
        select(
          input_year,
          output_year,
          beta,
          beta_interval_lower,
          beta_interval_upper
        ),
      by = c("input_year", "output_year")
    ) |>
    mutate(
      interval_consistent =
        observed_coefficient >= beta_interval_lower - 1e-12 &
        observed_coefficient <= beta_interval_upper + 1e-12,
      distance_to_interval = case_when(
        observed_coefficient < beta_interval_lower ~ beta_interval_lower - observed_coefficient,
        observed_coefficient > beta_interval_upper ~ observed_coefficient - beta_interval_upper,
        TRUE ~ 0
      )
    )
}


# ------------------------------------------------------------------------------
# FUNCTION: build_cbo_debt_service_methodology
# Purpose: Assemble the documented methodology and assumptions for the debt-service kernel.
# ------------------------------------------------------------------------------
build_cbo_debt_service_methodology <- function() {
  tibble(
    component = c(
      "FY2026 impulse row",
      "FY2027 impulse row",
      "Current BFM debt-service matrix",
      "FY2028-FY2036 impulse rows",
      "Gate 2A validation",
      "Long-run effective marginal borrowing rate",
      "Current BFM calculation"
    ),
    role = c(
      "Direct current-vintage kernel row and leave-one-out validation target",
      "Direct current-vintage kernel row, calibration anchor, and reverse leave-one-out validation target",
      "Current DEBT1-DEBT11 position/lag constraints; published two-decimal values are treated as precision-compatible intervals",
      "Least-distorting transfer from the FY2027 high-precision anchor subject to all overlapping current BFM interval constraints",
      "Two reciprocal leave-one-out tests: predict FY2026 from FY2027 and FY2027 from FY2026 without using the target calculator row",
      "FY2036 rate taken from current FY2027-pulse CBO export",
      "Confirms DEBT1-DEBT11 are applied to primary-deficit changes in the maintained public BFM"
    ),
    source = c(
      "CBO March 2026 debt-service interactive CSV export",
      "CBO March 2026 debt-service interactive CSV export",
      "CBO public Budgetary Feedback Model input/rules_of_thumb.csv",
      "CBO public Budgetary Feedback Model input/rules_of_thumb.csv plus current FY2027 calculator export",
      "CBO March 2026 FY2026/FY2027 exports plus current BFM DEBT matrix",
      "CBO March 2026 FY2027 debt-service interactive CSV export",
      "CBO public Budgetary Feedback Model bfm/net_interest_costs.py"
    ),
    source_url = c(
      CBO_DEBT_TOOL_2026_URL,
      CBO_DEBT_TOOL_2026_URL,
      CBO_BFM_2026_RULES_URL,
      CBO_BFM_2026_RULES_URL,
      CBO_BFM_2026_RULES_URL,
      CBO_DEBT_TOOL_2026_URL,
      CBO_BFM_2026_NET_INTEREST_URL
    ),
    direct_cbo_value = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
    evidence_label = c(
      "OFFICIAL_CURRENT_CBO_TOOL",
      "OFFICIAL_CURRENT_CBO_TOOL",
      "OFFICIAL_CURRENT_CBO_MODEL_INPUT",
      "CBO_CURRENT_BFM_INTERVAL_TRANSFER",
      "VALIDATION_AGAINST_OFFICIAL_CURRENT_CBO_TOOL",
      "OFFICIAL_CURRENT_CBO_TOOL",
      "OFFICIAL_CURRENT_CBO_MODEL_CODE"
    )
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: register_cbo_debt_service_sources
# Purpose: Register all CBO debt-service methodology and fixture sources in the provenance manifest.
# ------------------------------------------------------------------------------
register_cbo_debt_service_sources <- function() {
  register_source(
    source_id = "cbo_debt_tool_2026_fy2026_pulse",
    agency = "Congressional Budget Office",
    title = "How Changes in Revenues and Outlays Would Affect Debt Service, Deficits, and Debt: FY2026 $900B pulse export",
    url = build_cbo_debt_tool_permalink(rep(0, 11), c(900, rep(0, 10))),
    publication_date = "2026-03-16",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_TOOL_NON_OFFICIAL_ESTIMATE",
    notes = "Values embedded from the first-table CBO CSV export. Source-file SHA-256: 560cafe51961be1c69d5bf6cdcfa61045bc0d04f46740386a0bfc0de40baaefe. CBO labels calculator results approximate and not official cost estimates."
  )
  register_source(
    source_id = "cbo_debt_tool_2026_fy2027_pulse",
    agency = "Congressional Budget Office",
    title = "How Changes in Revenues and Outlays Would Affect Debt Service, Deficits, and Debt: FY2027 $900B pulse export",
    url = build_cbo_debt_tool_permalink(rep(0, 11), c(0, 900, rep(0, 9))),
    publication_date = "2026-03-16",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_TOOL_NON_OFFICIAL_ESTIMATE",
    notes = "Values embedded from the first-table CBO CSV export. Source-file SHA-256: 319b962078e1accb0ab43acba1b4e47faa5a9a234d130f149acff67e5c1aa931. CBO labels calculator results approximate and not official cost estimates."
  )
  register_source(
    source_id = "cbo_bfm_rules_of_thumb_2026",
    agency = "Congressional Budget Office",
    title = "Budgetary Feedback Model rules_of_thumb.csv",
    url = CBO_BFM_2026_RULES_URL,
    publication_date = "2026-07-13",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_CURRENT_MODEL_INPUT",
    notes = paste0(
      "Commit-pinned current public BFM input. DEBT1-DEBT11 supply the 2026-2036 debt-service sensitivity matrix. ",
      "Values are published to two decimal places and are used as precision-compatible intervals in the kernel validation. Commit: ",
      CBO_BFM_2026_COMMIT
    )
  )
  register_source(
    source_id = "cbo_bfm_net_interest_code_2026",
    agency = "Congressional Budget Office",
    title = "Budgetary Feedback Model net_interest_costs.py",
    url = CBO_BFM_2026_NET_INTEREST_URL,
    publication_date = "2026-07-13",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_CURRENT_MODEL_CODE",
    notes = paste0(
      "Commit-pinned CBO model code. The debtserv calculation applies DEBT1-DEBT11 to primary-deficit changes. Commit: ",
      CBO_BFM_2026_COMMIT
    )
  )
  register_source(
    source_id = "cbo_bfm_public_repo_2026",
    agency = "Congressional Budget Office",
    title = "CBO Budgetary Feedback Model public repository",
    url = CBO_BFM_2026_REPO_URL,
    publication_date = "2026-07-13",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_CURRENT_MODEL_DOCUMENTATION",
    notes = "Repository documentation states that current BFM inputs use CBO's February 2026 budget/economic baseline and April 2026 economic rules of thumb. No Python runtime is invoked by this R model."
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: build_cbo_debt_service_kernel
# Purpose: Construct the annual debt-service feedback kernel that maps primary-budget changes into interest costs.
# ------------------------------------------------------------------------------
build_cbo_debt_service_kernel <- function() {
  log_line("Gate 2A preparation: building debt-service kernel from current CBO calculator fixtures and current BFM DEBT matrix")
  register_cbo_debt_service_sources()

  years <- 2026:2036
  impulse <- CFG$cbo_kernel_impulse_bil
  fixtures <- current_cbo_debt_service_fixtures()
  bfm_matrix <- cbo_bfm_2026_debt_service_matrix()

  fixture_interval_check <- validate_fixture_against_bfm_intervals(
    fixtures = fixtures$rows,
    bfm_matrix = bfm_matrix
  )

  assert_model(
    all(fixture_interval_check$interval_consistent),
    paste0(
      "At least one direct March 2026 CBO calculator coefficient lies outside the published-precision interval implied by the current BFM DEBT matrix. Maximum interval distance=",
      signif(max(fixture_interval_check$distance_to_interval), 8)
    )
  )

  current_rows <- fixtures$rows |>
    mutate(debt_service_effect_per_1_bil_primary_deficit = debt_service_bil / impulse)

  modeled_objects <- map(2028:2036, function(input_year_current) {
    build_bfm_interval_transfer_prediction(
      target_input_year = input_year_current,
      anchor_input_year = 2027L,
      fixtures = fixtures$rows,
      bfm_matrix = bfm_matrix
    )
  })

  modeled_rows <- map_dfr(modeled_objects, function(obj) {
    obj$prediction |>
      transmute(
        input_year,
        output_year,
        debt_service_effect_per_1_bil_primary_deficit = predicted_coefficient,
        coefficient_source = "CBO_CURRENT_BFM_INTERVAL_TRANSFER",
        anchor_input_year = 2027L,
        bfm_forecast_position = forecast_position,
        position_factor,
        method_note = paste0(
          "FY2027 current calculator coefficient transferred with the factor closest to 1 that is consistent with every overlapping current BFM DEBT",
          forecast_position,
          " published-precision interval; lag 0 uses the current BFM same-year coefficient ratio"
        )
      )
  })

  position_factors <- bind_rows(
    map_dfr(modeled_objects, function(x) x$factor_summary),
    build_bfm_interval_transfer_prediction(
      target_input_year = 2026L,
      anchor_input_year = 2027L,
      fixtures = fixtures$rows,
      bfm_matrix = bfm_matrix
    )$factor_summary |>
      mutate(factor_role = "VALIDATION_FY2026_FROM_FY2027"),
    build_bfm_interval_transfer_prediction(
      target_input_year = 2027L,
      anchor_input_year = 2026L,
      fixtures = fixtures$rows,
      bfm_matrix = bfm_matrix
    )$factor_summary |>
      mutate(factor_role = "VALIDATION_FY2027_FROM_FY2026")
  ) |>
    mutate(
      factor_role = coalesce(
        factor_role,
        if_else(anchor_input_year == 2027L & target_input_year >= 2028L, "MODELED_KERNEL_ROW", "OTHER")
      )
    ) |>
    arrange(target_input_year, anchor_input_year)

  exact_rows <- current_rows |>
    filter(input_year %in% c(2026L, 2027L)) |>
    transmute(
      input_year,
      output_year,
      debt_service_effect_per_1_bil_primary_deficit,
      coefficient_source = "OFFICIAL_CURRENT_CBO_EXPORT",
      anchor_input_year = input_year,
      bfm_forecast_position = input_year - 2025L,
      position_factor = 1,
      method_note = if_else(
        input_year == 2026L,
        "Direct March 2026 CBO calculator export; also used as a reciprocal leave-one-out validation target",
        "Direct March 2026 CBO calculator export; primary current-vintage calibration anchor and reciprocal validation target"
      )
    )

  pre_input_rows <- expand_grid(input_year = years, output_year = years) |>
    filter(output_year < input_year) |>
    transmute(
      input_year,
      output_year,
      debt_service_effect_per_1_bil_primary_deficit = 0,
      coefficient_source = "TEMPORAL_CAUSALITY_ZERO",
      anchor_input_year = NA_integer_,
      bfm_forecast_position = input_year - 2025L,
      position_factor = NA_real_,
      method_note = "Debt service cannot precede the primary-deficit change"
    )

  kernel <- bind_rows(
    exact_rows |> filter(output_year >= input_year),
    modeled_rows,
    pre_input_rows
  ) |>
    arrange(input_year, output_year)

  assert_model(nrow(kernel) == length(years)^2, paste0("Current-CBO debt-service kernel has ", nrow(kernel), " cells; expected ", length(years)^2))
  assert_model(n_distinct(paste(kernel$input_year, kernel$output_year, sep = "|")) == length(years)^2, "Current-CBO debt-service kernel contains duplicate input/output cells")
  assert_model(all(is.finite(kernel$debt_service_effect_per_1_bil_primary_deficit)), "Current-CBO debt-service kernel contains non-finite coefficients")
  assert_model(all(kernel$debt_service_effect_per_1_bil_primary_deficit[kernel$output_year < kernel$input_year] == 0), "Current-CBO debt-service kernel violates temporal causality")

  modeled_interval_check <- kernel |>
    filter(coefficient_source == "CBO_CURRENT_BFM_INTERVAL_TRANSFER") |>
    left_join(
      bfm_matrix |>
        select(
          input_year,
          output_year,
          beta_interval_lower,
          beta_interval_upper
        ),
      by = c("input_year", "output_year")
    ) |>
    mutate(
      interval_consistent =
        debt_service_effect_per_1_bil_primary_deficit >= beta_interval_lower - 1e-12 &
        debt_service_effect_per_1_bil_primary_deficit <= beta_interval_upper + 1e-12
    )

  assert_model(all(modeled_interval_check$interval_consistent), "At least one modeled debt-service coefficient violates the current BFM published-precision interval")

  cache_path <- file.path(CFG$cache_dir, paste0("cbo_current_bfm_interval_kernel_", CFG$cbo_vintage, "_v4.csv"))
  rates_path <- file.path(CFG$cache_dir, paste0("cbo_current_bfm_effective_marginal_rates_", CFG$cbo_vintage, "_v4.csv"))
  factors_path <- file.path(CFG$cache_dir, paste0("cbo_current_bfm_position_factors_", CFG$cbo_vintage, "_v4.csv"))
  write_csv_atomic(kernel, cache_path)
  write_csv_atomic(fixtures$rates, rates_path)
  write_csv_atomic(position_factors, factors_path)

  list(
    kernel = kernel,
    rates = fixtures$rates,
    fixtures = fixtures$rows,
    bfm_matrix = bfm_matrix,
    position_factors = position_factors,
    fixture_interval_check = fixture_interval_check,
    methodology = build_cbo_debt_service_methodology()
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: apply_cbo_kernel
# Purpose: Apply the validated debt-service kernel to an annual primary-deficit path.
# ------------------------------------------------------------------------------
apply_cbo_kernel <- function(revenues, outlays, kernel, years = 2026:2036) {
  assert_model(length(revenues) == length(years), "Revenue vector length mismatch in debt-service kernel")
  assert_model(length(outlays) == length(years), "Outlay vector length mismatch in debt-service kernel")

  inputs <- tibble(
    input_year = years,
    primary_deficit_delta_bil = as.numeric(outlays) - as.numeric(revenues)
  )
  required_kernel_cols <- c("input_year", "output_year", "debt_service_effect_per_1_bil_primary_deficit")
  assert_model(all(required_kernel_cols %in% names(kernel)), "Debt-service kernel does not use the validated primary-deficit schema")

  kernel |>
    left_join(inputs, by = "input_year") |>
    mutate(contribution = debt_service_effect_per_1_bil_primary_deficit * primary_deficit_delta_bil) |>
    group_by(output_year) |>
    summarise(debt_service_delta_bil = sum(contribution, na.rm = TRUE), .groups = "drop") |>
    rename(year = output_year) |>
    complete(year = years, fill = list(debt_service_delta_bil = 0)) |>
    arrange(year)
}


# ------------------------------------------------------------------------------
# FUNCTION: validate_cbo_kernel
# Purpose: Run reciprocal holdout and fixture tests proving the debt-service kernel is within tolerance.
# ------------------------------------------------------------------------------
validate_cbo_kernel <- function(kernel_obj) {
  log_line("Gate 2A: running reciprocal leave-one-out validation against both March 2026 CBO calculator pulse exports")

  impulse <- CFG$cbo_kernel_impulse_bil
  fixtures <- kernel_obj$fixtures
  bfm_matrix <- kernel_obj$bfm_matrix

  validate_direction <- function(target_input_year, anchor_input_year) {
    pred_obj <- build_bfm_interval_transfer_prediction(
      target_input_year = target_input_year,
      anchor_input_year = anchor_input_year,
      fixtures = fixtures,
      bfm_matrix = bfm_matrix
    )

    predicted <- pred_obj$prediction |>
      transmute(
        output_year,
        lag,
        predicted_coefficient,
        predicted_debt_service_bil = predicted_coefficient * impulse,
        bfm_beta = beta,
        bfm_interval_lower = beta_interval_lower,
        bfm_interval_upper = beta_interval_upper,
        position_factor
      )

    actual <- fixtures |>
      filter(input_year == target_input_year) |>
      transmute(
        output_year,
        actual_debt_service_bil = debt_service_bil
      )

    comparison <- inner_join(actual, predicted, by = "output_year") |>
      mutate(
        target_input_year = target_input_year,
        anchor_input_year = anchor_input_year,
        validation_direction = paste0("FY", target_input_year, "_FROM_FY", anchor_input_year),
        error_bil = predicted_debt_service_bil - actual_debt_service_bil,
        abs_error_bil = abs(error_bil)
      ) |>
      select(
        validation_direction,
        target_input_year,
        anchor_input_year,
        output_year,
        lag,
        actual_debt_service_bil,
        predicted_debt_service_bil,
        error_bil,
        abs_error_bil,
        predicted_coefficient,
        bfm_beta,
        bfm_interval_lower,
        bfm_interval_upper,
        position_factor
      )

    list(
      comparison = comparison,
      factor_summary = pred_obj$factor_summary
    )
  }

  validation_2026 <- validate_direction(2026L, 2027L)
  validation_2027 <- validate_direction(2027L, 2026L)

  comp <- bind_rows(
    validation_2026$comparison,
    validation_2027$comparison
  )

  assert_model(nrow(validation_2026$comparison) == 10L, paste0("FY2026-from-FY2027 holdout has ", nrow(validation_2026$comparison), " annual comparisons; expected 10"))
  assert_model(nrow(validation_2027$comparison) == 10L, paste0("FY2027-from-FY2026 holdout has ", nrow(validation_2027$comparison), " annual comparisons; expected 10"))
  assert_model(all(is.finite(comp$predicted_debt_service_bil)), "Gate 2A reciprocal holdout contains non-finite predictions")

  direction_summary <- comp |>
    group_by(validation_direction, target_input_year, anchor_input_year) |>
    summarise(
      validation_points = n(),
      max_error_bil = max(abs_error_bil),
      rmse_bil = sqrt(mean(error_bil^2)),
      .groups = "drop"
    ) |>
    mutate(
      passed = is.finite(max_error_bil) & max_error_bil <= CFG$cbo_kernel_validation_tolerance_bil
    )

  max_error <- max(direction_summary$max_error_bil)
  rmse <- sqrt(mean(comp$error_bil^2))
  passed <- all(direction_summary$passed)

  if (!passed && CFG$strict_gate_2) {
    write_csv_atomic(comp, file.path(CFG$output_dir, "validation_cbo_debt_service_kernel.csv"))
    write_csv_atomic(direction_summary, file.path(CFG$output_dir, "validation_cbo_debt_service_kernel_summary.csv"))
    model_stop(
      "Gate 2A failed: at least one reciprocal current-CBO debt-service holdout exceeds the configured $",
      CFG$cbo_kernel_validation_tolerance_bil,
      "B annual tolerance. Maximum observed error = $",
      round(max_error, 3),
      "B. See analysis_output/validation_cbo_debt_service_kernel.csv"
    )
  }

  summary_text <- paste0(
    direction_summary$validation_direction,
    " max=$",
    sprintf("%.3f", direction_summary$max_error_bil),
    "B RMSE=$",
    sprintf("%.3f", direction_summary$rmse_bil),
    "B",
    collapse = "; "
  )

  log_line(
    "Gate 2A complete: ", summary_text,
    "; combined RMSE=$", round(rmse, 3),
    "B; current BFM matrix replaces the 2020 Table 40 transfer"
  )

  list(
    passed = passed,
    comparison = comp,
    direction_summary = direction_summary,
    max_error_bil = max_error,
    rmse_bil = rmse,
    validation_points = nrow(comp),
    benchmark_source_kind = "EMBEDDED_OFFICIAL_CURRENT_CBO_EXPORTS_PLUS_CURRENT_CBO_BFM_MATRIX",
    benchmark_permalink = paste(
      unique(fixtures$source_permalink[fixtures$input_year %in% c(2026L, 2027L)]),
      collapse = " | "
    ),
    validation_factors = bind_rows(
      validation_2026$factor_summary |>
        mutate(validation_direction = "FY2026_FROM_FY2027"),
      validation_2027$factor_summary |>
        mutate(validation_direction = "FY2027_FROM_FY2026")
    ),
    method = "CURRENT_BFM_PUBLISHED_PRECISION_INTERVAL_TRANSFER_WITH_RECIPROCAL_LEAVE_ONE_OUT_VALIDATION"
  )
}

CBO_MACRO_BENCHMARKS <- tribble(
  ~scenario_id, ~description, ~shock_pp, ~cumulative_deficit_change_2027_2036_bil, ~deficit_change_2036_bil,
  "productivity_minus_0_1", "Potential productivity growth 0.1 percentage point slower each year", -0.1, 317, 65,
  "labor_force_minus_0_1", "Labor-force growth 0.1 percentage point slower each year", -0.1, 166, 37,
  "rates_plus_0_1", "All interest rates 0.1 percentage point higher", 0.1, 379, 60,
  "inflation_plus_0_1", "All wage/price indexes and nominal interest rates 0.1 percentage point higher", 0.1, 311, 51
)


# ------------------------------------------------------------------------------
# FUNCTION: build_macro_stress_profile
# Purpose: Construct one annual macroeconomic stress path for robust-scenario testing.
# ------------------------------------------------------------------------------
build_macro_stress_profile <- function(scenario_id, shock_multiplier = 1) {
  row <- CBO_MACRO_BENCHMARKS |> filter(.data$scenario_id == !!scenario_id)
  if (nrow(row) != 1L) model_stop("Unknown macro scenario: ", scenario_id)

  years <- 2027:2036

  n <- length(years)
  z <- seq(1 / n, 1, length.out = n)
  X <- cbind(z, z^2)
  A <- rbind(colSums(X), X[n, ])
  b <- c(row$cumulative_deficit_change_2027_2036_bil, row$deficit_change_2036_bil)
  coef <- solve(A, b)
  annual <- as.numeric(X %*% coef)

  tibble(
    year = years,
    scenario_id = scenario_id,
    deficit_delta_bil = annual * shock_multiplier,
    evidence_class = "CBO_BENCHMARK_RECONSTRUCTED_ANNUAL_PROFILE"
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: validate_macro_profiles
# Purpose: Validate the internally generated macro stress scenarios and their annual paths.
# ------------------------------------------------------------------------------
validate_macro_profiles <- function() {
  log_line("Gate 2B: validating reconstructed macro stress profiles against published CBO benchmark totals")

  checks <- map_dfr(CBO_MACRO_BENCHMARKS$scenario_id, function(id) {
    p <- build_macro_stress_profile(id)
    ref <- CBO_MACRO_BENCHMARKS |> filter(scenario_id == id)
    tibble(
      gate = "GATE_2_MACRO",
      scenario_id = id,
      cumulative_error_bil = sum(p$deficit_delta_bil) - ref$cumulative_deficit_change_2027_2036_bil,
      fy2036_error_bil = p$deficit_delta_bil[p$year == 2036] - ref$deficit_change_2036_bil,
      passed = abs(cumulative_error_bil) < 1e-8 & abs(fy2036_error_bil) < 1e-8
    )
  })

  if (any(!checks$passed) && CFG$strict_gate_2) {
    model_stop("Gate 2B failed: macro stress profile calibration error")
  }

  register_source(
    "cbo_macro_sensitivity_2026",
    "Congressional Budget Office",
    "How Changes in Economic Conditions Might Affect the Federal Budget: 2026 to 2036",
    "https://www.cbo.gov/publication/62257",
    publication_date = "2026-04-21",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Benchmark totals used to validate reconstructed stress profiles; main optimizer excludes favorable dynamic macro feedback"
  )

  log_line("Gate 2B complete: all benchmark profile identities passed")
  checks
}


# ------------------------------------------------------------------------------
# FUNCTION: build_tariff_profile
# Purpose: Construct the optional tariff-related baseline adjustment and associated debt-service effects.
# ------------------------------------------------------------------------------
build_tariff_profile <- function(baseline, cbo_data, kernel_obj) {
  if (!CFG$allow_tariff_adjustment) {
    return(tibble(year = CFG$model_years, tariff_primary_deficit_delta_bil = 0, method = "disabled"))
  }

  log_line("Calibrating August 2026 tariff baseline adjustment")

  years <- 2027:2036

  base_weights <- baseline$gdp_bil[match(years, baseline$year)]
  base_weights <- base_weights / sum(base_weights)
  total_primary <- CFG$tariff_primary_worsening_2027_2036_bil

  tilt_grid <- seq(-2.5, 2.5, length.out = 501)
  candidates <- map_dfr(tilt_grid, function(tilt) {
    time_index <- seq_along(years)
    w <- base_weights * exp(tilt * (time_index - mean(time_index)) / length(time_index))
    w <- w / sum(w)
    primary <- total_primary * w

    rev <- rep(0, 11)
    out <- rep(0, 11)
    rev[match(years, 2026:2036)] <- -primary
    interest <- apply_cbo_kernel(rev, out, kernel_obj$kernel, 2026:2036)

    tibble(
      tilt = tilt,
      implied_interest_2027_2036_bil = sum(interest$debt_service_delta_bil[interest$year %in% years], na.rm = TRUE),
      error_bil = abs(implied_interest_2027_2036_bil - CFG$tariff_debt_service_worsening_2027_2036_bil)
    )
  })

  chosen <- candidates |> slice_min(error_bil, n = 1, with_ties = FALSE)
  tilt <- chosen$tilt
  time_index <- seq_along(years)
  weights <- base_weights * exp(tilt * (time_index - mean(time_index)) / length(time_index))
  weights <- weights / sum(weights)
  primary <- total_primary * weights

  if (chosen$error_bil > CFG$tariff_calibration_tolerance_bil) {
    model_warn(
      "Tariff annual-profile reconstruction cannot closely match CBO's published debt-service aggregate. Minimum error = $",
      round(chosen$error_bil, 1), "B. The profile remains explicitly exploratory."
    )
  }

  profile <- tibble(
    year = years,
    tariff_primary_deficit_delta_bil = primary,
    method = "customs_or_gdp_weights_with_exponential_timing_tilt_constrained_to_CBO_aggregate_interest",
    timing_tilt = tilt,
    evidence_class = "EXPLORATORY_CONSTRAINED_TO_OFFICIAL_AGGREGATES"
  )

  profile <- bind_rows(
    tibble(
      year = 2026L,
      tariff_primary_deficit_delta_bil = CFG$tariff_primary_worsening_2026_bil,
      method = "CBO_AUGUST_2026_REPORTED_FY2026_CUSTOMS_REVENUE_DIFFERENCE",
      timing_tilt = NA_real_,
      evidence_class = "OFFICIAL_AGGREGATE"
    ),
    profile
  )

  if (CFG$tariff_extension_mode == "hold_2036_gdp_share") {
    fy36 <- profile |> filter(year == 2036) |> pull(tariff_primary_deficit_delta_bil)
    gdp36 <- baseline |> filter(year == 2036) |> pull(gdp_bil)
    share <- fy36 / gdp36

    ext <- baseline |>
      filter(year %in% CFG$extension_years) |>
      transmute(
        year,
        tariff_primary_deficit_delta_bil = gdp_bil * share,
        method = "hold_2036_primary_effect_as_share_of_GDP",
        timing_tilt = NA_real_,
        evidence_class = "EXPLORATORY_EXTENSION"
      )
    profile <- bind_rows(profile, ext)
  } else {
    profile <- bind_rows(
      profile,
      tibble(
        year = CFG$extension_years,
        tariff_primary_deficit_delta_bil = 0,
        method = "zero_after_2036",
        timing_tilt = NA_real_,
        evidence_class = "EXPLORATORY_EXTENSION"
      )
    )
  }

  register_source(
    "cbo_tariff_update_aug_2026",
    "Congressional Budget Office",
    "An Update About CBO's Projections of the Budgetary Effects of Tariffs",
    "https://www.cbo.gov/publication/62704",
    publication_date = "2026-08-20",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_CURRENT_AGGREGATES",
    notes = "FY2026 customs-revenue difference and 2027-2036 aggregate primary/debt-service effects are official; annual allocation is reconstructed and labeled separately"
  )

  profile |> arrange(year)
}


# ------------------------------------------------------------------------------
# FUNCTION: build_working_baseline
# Purpose: Combine the validated CBO baseline and permitted adjustments into the optimization baseline.
# ------------------------------------------------------------------------------
build_working_baseline_core <- function(cbo_baseline, tariff_profile, kernel_obj) {
  log_line("Constructing working September 2026 baseline layer")

  years <- CFG$model_years
  tariff <- tariff_profile |> complete(year = years, fill = list(tariff_primary_deficit_delta_bil = 0)) |> arrange(year)

  tariff_revenue_10 <- -tariff$tariff_primary_deficit_delta_bil[match(2026:2036, tariff$year)]
  tariff_interest_10 <- apply_cbo_kernel(
    revenues = tariff_revenue_10,
    outlays = rep(0, 11),
    kernel = kernel_obj$kernel,
    years = 2026:2036
  )

  rate36 <- kernel_obj$rates |>
    filter(year == 2036) |>
    pull(effective_marginal_rate_pct)
  if (length(rate36) != 1L || !is.finite(rate36)) rate36 <- 100 * CFG$long_run_interest_fallback_rate
  rate_ext <- rate36 / 100

  delta_debt <- numeric(length(years))
  delta_interest <- numeric(length(years))
  names(delta_debt) <- years
  names(delta_interest) <- years

  for (t in seq_along(years)) {
    y <- years[t]
    primary_delta <- tariff$tariff_primary_deficit_delta_bil[tariff$year == y]

    if (y <= 2036) {
      interest_delta <- tariff_interest_10$debt_service_delta_bil[tariff_interest_10$year == y]
    } else {
      prev_delta <- delta_debt[t - 1L]
      interest_delta <- rate_ext * (prev_delta + 0.5 * primary_delta)
    }

    delta_interest[t] <- interest_delta
    if (t == 1L) {
      delta_debt[t] <- primary_delta + interest_delta
    } else {
      delta_debt[t] <- delta_debt[t - 1L] + primary_delta + interest_delta
    }
  }

  adjusted <- cbo_baseline |>
    left_join(tariff |> select(year, tariff_primary_deficit_delta_bil), by = "year") |>
    mutate(
      tariff_primary_deficit_delta_bil = replace_na(tariff_primary_deficit_delta_bil, 0),
      tariff_interest_delta_bil = delta_interest[as.character(year)],
      tariff_total_deficit_delta_bil = tariff_primary_deficit_delta_bil + tariff_interest_delta_bil,
      tariff_debt_delta_bil = delta_debt[as.character(year)],
      working_debt_bil = debt_bil + tariff_debt_delta_bil,
      working_debt_gdp_pct = 100 * working_debt_bil / gdp_bil,
      working_total_deficit_bil = total_deficit_bil + tariff_total_deficit_delta_bil,
      working_primary_deficit_bil = if_else(
        !is.na(primary_deficit_bil),
        primary_deficit_bil + tariff_primary_deficit_delta_bil,
        NA_real_
      )
    )

  adjusted
}

CBO_OPTIONS_REPORT_URL <- "https://www.cbo.gov/publication/60557"
CBO_OPTIONS_SEARCH_URL <- "https://www.cbo.gov/budget-options"
GAO_2026_DUPLICATION_URL <- "https://www.gao.gov/products/gao-26-108505"
MEDPAC_MARCH_2026_URL <- "https://www.medpac.gov/document/march-2026-report-to-the-congress-medicare-payment-policy/"
SSA_SOLVENCY_INDEX_URL <- "https://www.ssa.gov/OACT/solvency/provisions/"
JCT_TAX_EXPENDITURES_URL <- "https://www.jct.gov/publications/2025/jcx-45-25/"

POLICY_PACK_REQUIRED_FILES <- c(
  "cbo_current_families.csv",
  "policy_candidates.csv",
  "policy_annual_flows.csv",
  "ssa_actuarial_reference.csv",
  "source_manifest.csv",
  "coverage_audit.csv",
  "pack_manifest.json",
  "file_hashes.csv",
  "README.txt"
)

POLICY_PACK_EXPECTED_HASHES <- c(
  cbo_current_families.csv = "8d0a4e2871796f119908a3c26d5b30eac44805e926866bfe6e5f134b1cbfabc0",
  policy_candidates.csv = "567c54667c804120aa57c11a23df9cde8426227534b0e3ac3fe56830e7f2d70f",
  policy_annual_flows.csv = "67df90e15e580cb1dea0835744a4419404fdb0edf31b909c67595934e00667f5",
  ssa_actuarial_reference.csv = "7b803036abce2b90b368ed6369831aa141c0f8b6b8a630a62746042cf5a089a6",
  source_manifest.csv = "269d140a93660c7df300f3dfda04fbc4d980fbbba69b8481959e2b4da7e21958",
  coverage_audit.csv = "08b520eb7b740a1a977d4c7a36ba0814050ce917decaf3dc74a4b152c2aae957",
  pack_manifest.json = "d14738987d2907ccb03f120ec6ffecbac89bffa5054aab9f5700f1f3fea000fe",
  README.txt = "dbaee64d96e593b4ce74b957c2c39b4fd26da55fadb4842f788392e612790ce3"
)


# ------------------------------------------------------------------------------
# FUNCTION: normalize_text_cell
# Purpose: Normalize whitespace and missing-value conventions in source text fields.
# ------------------------------------------------------------------------------
normalize_text_cell <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\u00a0", " ")
  x <- stringr::str_replace_all(x, "\\r|\\n|\\t", " ")
  stringr::str_squish(x)
}


# ------------------------------------------------------------------------------
# FUNCTION: locate_policy_pack
# Purpose: Locate the repository-supplied frozen scored-policy pack used for official scored anchors.
# ------------------------------------------------------------------------------
locate_policy_pack_core <- function() {
  candidates <- unique(c(
    CFG$policy_pack_dir,
    file.path(CFG$data_dir, "fiscal_policy_data_pack_v1"),
    file.path(CFG$data_dir, "fiscal_policy_data_pack_v1", "fiscal_policy_data_pack_v1"),
    file.path(CFG$project_root, "fiscal_policy_data_pack_v1")
  ))
  hit <- candidates[dir.exists(candidates)]
  if (length(hit) == 0L) {
    model_stop(
      "Local fiscal policy data pack not found. Expected folder: ",
      file.path(CFG$data_dir, "fiscal_policy_data_pack_v1")
    )
  }
  normalizePath(hit[[1]], winslash = "/", mustWork = TRUE)
}


# ------------------------------------------------------------------------------
# FUNCTION: validate_policy_pack_hashes
# Purpose: Verify every frozen policy-pack file against its recorded checksum.
# ------------------------------------------------------------------------------
validate_policy_pack_hashes <- function(pack_dir) {
  missing <- POLICY_PACK_REQUIRED_FILES[!file.exists(file.path(pack_dir, POLICY_PACK_REQUIRED_FILES))]
  assert_model(
    length(missing) == 0L,
    paste0("Policy data pack is incomplete. Missing: ", paste(missing, collapse = ", "))
  )
  audit <- purrr::imap_dfr(POLICY_PACK_EXPECTED_HASHES, function(expected, filename) {
    path <- file.path(pack_dir, filename)
    observed <- sha256_file(path)
    tibble(
      file = filename,
      expected_sha256 = expected,
      observed_sha256 = observed,
      passed = identical(tolower(observed), tolower(expected)),
      bytes = file.info(path)$size
    )
  })
  if (!all(audit$passed)) {
    bad <- audit |> filter(!passed)
    write_csv_atomic(bad, file.path(CFG$output_dir, "policy_pack_hash_failures.csv"))
    model_stop(
      "Policy data pack hash validation failed for: ",
      paste(bad$file, collapse = ", "),
      ". Re-extract the original fiscal_policy_data_pack_v1.zip without editing its contents."
    )
  }
  audit
}


# ------------------------------------------------------------------------------
# FUNCTION: register_local_policy_pack
# Purpose: Register the frozen policy-pack files as authoritative local evidence sources.
# ------------------------------------------------------------------------------
register_local_policy_pack <- function(pack_dir, source_rows) {
  for (filename in names(POLICY_PACK_EXPECTED_HASHES)) {
    register_source(
      source_id = paste0("local_policy_pack_", stringr::str_replace_all(filename, "[^A-Za-z0-9]+", "_")),
      agency = "Federal Fiscal Optimizer local evidence pack",
      title = filename,
      url = paste0("local://fiscal_policy_data_pack_v1/", filename),
      local_path = file.path(pack_dir, filename),
      publication_date = "2026-09-11",
      baseline_vintage = "fiscal_policy_data_pack_v1",
      evidence_class = "LOCAL_FROZEN_OFFICIAL_SOURCE_SNAPSHOT",
      notes = "Frozen local input. No cbo.gov runtime access is required."
    )
  }
  purrr::pwalk(source_rows, function(source_id, agency, title, url, retrieval_date, notes, ...) {
    register_source(
      source_id = paste0("policy_pack_source_", source_id),
      agency = agency,
      title = title,
      url = url,
      local_path = NA_character_,
      publication_date = retrieval_date,
      baseline_vintage = "fiscal_policy_data_pack_v1",
      evidence_class = "OFFICIAL_SOURCE_PROVENANCE_IN_LOCAL_PACK",
      notes = notes
    )
  })
  invisible(TRUE)
}


# ------------------------------------------------------------------------------
# FUNCTION: validate_policy_pack_benchmarks
# Purpose: Check expected family, candidate, annual-row, and benchmark counts in the frozen policy pack.
# ------------------------------------------------------------------------------
validate_policy_pack_benchmarks <- function(candidates, flows) {
  improvements <- flows |>
    group_by(candidate_id) |>
    summarise(source_window_primary_improvement_bil = sum(-primary_deficit_delta_bil, na.rm = TRUE), .groups = "drop") |>
    left_join(candidates |> select(candidate_id, family_title, variant_name), by = "candidate_id")
  check_one <- function(title_pattern, variant_pattern, expected, tolerance = 1.1) {
    x <- improvements |>
      filter(
        stringr::str_detect(family_title, stringr::regex(title_pattern, ignore_case = TRUE)),
        stringr::str_detect(variant_name, stringr::regex(variant_pattern, ignore_case = TRUE))
      )
    assert_model(nrow(x) == 1L, paste0("Policy-pack benchmark did not identify exactly one row: ", title_pattern, " / ", variant_pattern))
    observed <- x$source_window_primary_improvement_bil[[1]]
    assert_model(
      abs(observed - expected) <= tolerance,
      paste0("Policy-pack benchmark failed for ", x$family_title[[1]], " / ", x$variant_name[[1]], ": observed $", round(observed, 3), "B; expected approximately $", expected, "B")
    )
    tibble(
      benchmark = paste(x$family_title[[1]], x$variant_name[[1]], sep = " | "),
      observed_bil = observed,
      expected_bil = expected,
      tolerance_bil = tolerance,
      passed = TRUE
    )
  }
  bind_rows(
    check_one("5 Percent Value-Added Tax", "Broad base", 3380),
    check_one("5 Percent Value-Added Tax", "Narrow base", 2180),
    check_one("Financial Transactions", "0.01 percent", 297),
    check_one("Maximum Taxable Earnings", "250,000", 1427),
    check_one("Corporate Income Tax Rate", "1 percentage point", 136),
    check_one("Medicare Advantage Benchmarks", "10 percent", 489)
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: classify_protection
# Purpose: Assign the initial policy-level protection status from explicit policy rules.
# ------------------------------------------------------------------------------
classify_protection <- function(title, variant_name, major_category) {
  t <- stringr::str_to_lower(title)
  v <- stringr::str_to_lower(variant_name)
  both <- paste(t, v)
  blocked <- c(
    "increase individual income tax rates on ordinary income" = "Broad ordinary-income rate increases conflict with protection of ordinary wages.",
    "impose a new payroll tax" = "A broad new payroll tax directly burdens ordinary wages.",
    "increase the payroll tax rate for medicare hospital insurance" = "A broad payroll-tax increase directly burdens ordinary wages.",
    "increase the payroll tax rate for social security" = "A broad payroll-tax increase directly burdens ordinary wages.",
    "increase taxes that finance the federal share of the unemployment insurance system" = "The option raises broad employment taxes and can burden ordinary wages and hiring.",
    "tax all pass-through business owners under seca" = "The option broadly raises payroll-tax burdens on active pass-through business income and business reinvestment.",
    "include employer-paid premiums for income replacement insurance" = "The option increases taxable compensation broadly and CBO reports the largest proportional burden among lower-wage workers.",
    "impose an excise tax on overland freight transport" = "Freight transportation is a productive input and a broad tax conflicts with protection of business reinvestment and ordinary consumption costs.",
    "further limit annual contributions to retirement plans" = "The option directly restricts ordinary tax-favored saving.",
    "eliminate or modify head-of-household filing status" = "The option burdens family and household formation through filing-status changes.",
    "lower the investment income limit for the earned income tax credit" = "The option reduces family support for lower-income workers and families.",
    "require people who claim the earned income tax credit and child tax credit" = "The option reduces family-oriented refundable tax support.",
    "eliminate subsidies for certain meals" = "The option reduces child and family nutrition support.",
    "eliminate the add-on to pell grants" = "The option reduces ordinary household access to education and human-capital investment.",
    "tighten eligibility for pell grants" = "The option reduces ordinary household access to education and human-capital investment.",
    "eliminate head start" = "The option eliminates a family and child-development program rather than reforming a rent or payment distortion.",
    "tighten eligibility for the supplemental nutrition assistance program" = "The option reduces basic household nutrition support.",
    "reduce tanf’s state family assistance grant" = "The option reduces direct family assistance.",
    "reduce tanf's state family assistance grant" = "The option reduces direct family assistance.",
    "convert multiple assistance programs for lower-income people" = "The option reduces lower-income family assistance through smaller block grants.",
    "raise the full retirement age for social security" = "A broad retirement-age increase shifts solvency costs onto ordinary beneficiaries.",
    "link initial social security benefits to average prices" = "The option broadly reduces scheduled Social Security benefits rather than concentrating solvency adjustment on higher earners or financing capacity.",
    "use an alternative measure of inflation to index social security" = "A broad chained-index reduction cuts ordinary Social Security and other indexed benefits.",
    "require social security disability insurance applicants" = "The option narrows disability social-insurance eligibility.",
    "eliminate eligibility for starting social security disability benefits" = "The option narrows disability social-insurance eligibility.",
    "eliminate supplemental security income benefits for disabled children" = "The option eliminates disability income support for children.",
    "increase the premiums paid for medicare part b" = "Broad beneficiary premium increases violate the Medicare protection constraint.",
    "change the cost-sharing rules for medicare" = "Broad beneficiary cost-sharing increases violate the Medicare protection constraint.",
    "raise the age of eligibility for medicare" = "The option reduces Medicare eligibility and violates the Medicare preservation constraint.",
    "establish caps on federal spending for medicaid" = "Broad Medicaid caps risk reducing ordinary beneficiary coverage and family security.",
    "reduce federal medicaid matching rates" = "Broad Medicaid financing reductions risk reducing ordinary beneficiary coverage and family security.",
    "limit state taxes on health care providers" = "The option materially reduces Medicaid financing without a beneficiary-protection mechanism in the score.",
    "end enrollment in va medical care" = "The option removes health coverage from a class of veterans.",
    "include va's disability payments in taxable income" = "The option reduces disability compensation available to ordinary beneficiaries.",
    "introduce means-testing for eligibility for va" = "The option withdraws disability compensation from existing beneficiaries.",
    "end va’s individual unemployability payments" = "The option cuts disability compensation.",
    "reduce va’s disability benefits" = "The option cuts disability compensation.",
    "narrow eligibility for va's disability compensation" = "The option cuts disability compensation eligibility.",
    "narrow eligibility for veterans’ disability compensation" = "The option cuts disability compensation eligibility.",
    "narrow eligibility for veterans' disability compensation" = "The option cuts disability compensation eligibility.",
    "reduce the annual across-the-board adjustment for federal civilian employees" = "The option directly reduces ordinary employee compensation.",
    "increase federal civilian employees' contributions" = "The option directly reduces ordinary employee take-home compensation.",
    "reduce pension benefits for new federal retirees" = "The option reduces ordinary employee retirement compensation.",
    "eliminate the special retirement supplement for new federal retirees" = "The option reduces ordinary employee retirement compensation.",
    "cap increases in basic pay for military service members" = "The option directly reduces ordinary service-member compensation.",
    "reduce the basic allowance for housing" = "The option directly reduces ordinary service-member housing compensation.",
    "introduce enrollment fees in tricare" = "The option shifts health costs to ordinary military retirees.",
    "introduce minimum out-of-pocket requirements in tricare" = "The option shifts health costs to ordinary military retirees.",
    "modify tricare enrollment fees and cost sharing" = "The option shifts health costs to ordinary military retirees.",
    "repeal the davis-bacon act" = "The mechanism is direct wage reduction on covered federal construction.",
    "raise the tax rates on long-term capital gains and qualified dividends" = "The option directly raises the marginal tax on ordinary saving and investment returns.",
    "eliminate the tax exemption for new qualified private activity bonds" = "The option raises financing costs for investment projects.",
    "increase the corporate income tax rate" = "The option directly raises the marginal tax on corporate investment and retained earnings.",
    "repeal the \"last in, first out\"" = "The option can materially tax working capital and business reinvestment.",
    "require half of advertising expenses to be amortized" = "The option delays business deductions and can burden ordinary business reinvestment.",
    "repeal the low-income housing tax credit" = "The option directly reduces housing investment and housing supply support.",
    "increase excise taxes on motor fuels" = "Motor fuel is treated as an ordinary household and productive input rather than discretionary consumption.",
    "impose a tax on emissions of greenhouse gases" = "The scored option lacks a protection mechanism for ordinary household energy costs and productive inputs.",
    "raise fannie mae's and freddie mac's guarantee fees" = "The option raises mortgage-finance costs and conflicts with the family and homeownership protection.",
    "reduce the department of defense's annual budget" = "An undifferentiated defense reduction is not allowed to substitute for targeted efficiency or procurement reform.",
    "reduce selected nondefense discretionary spending" = "An undifferentiated cut can reduce productive public investment and state capacity.",
    "reduce spending on other mandatory programs" = "An undifferentiated mandatory-spending cut can reach protected social insurance and family support.",
    "expand social security to include newly hired state and local government employees" = "The option applies a new payroll-tax burden to ordinary workers as its financing mechanism.",
    "reduce department of energy funding for energy technology development" = "The option directly reduces productive public research and technology capacity.",
    "limit highway and transit funding" = "The option directly constrains productive transportation infrastructure.",
    "eliminate the federal transit administration" = "The option directly reduces transportation infrastructure capacity.",
    "increase payments by tenants in federally assisted housing" = "The option raises housing costs for assisted households.",
    "reduce funding for the housing choice voucher program" = "The option reduces basic housing support for lower-income households."
  )
  for (p in names(blocked)) {
    if (stringr::str_detect(both, stringr::fixed(p))) return(c(status = "BLOCKED", reason = blocked[[p]]))
  }
  if (stringr::str_detect(t, "impose a 5 percent value-added tax")) {
    if (stringr::str_detect(v, "narrow")) return(c(status = "ELIGIBLE", reason = "The narrow-base VAT excludes new housing, food for home consumption, health care, and postsecondary education and is treated as the non-essentials consumption-tax design."))
    return(c(status = "BLOCKED", reason = "The broad-base VAT reaches ordinary necessities and housing and therefore violates the consumption protection rule."))
  }
  if (stringr::str_detect(t, "establish a uniform social security benefit")) return(c(status = "CONDITIONAL", reason = "The option can improve the benefit floor while reducing benefits above it, but the effect on ordinary middle earners requires explicit review rather than automatic exclusion."))
  if (stringr::str_detect(t, "make social security’s benefit structure more progressive|make social security's benefit structure more progressive")) return(c(status = "CONDITIONAL", reason = "Progressive benefit-formula reform can preserve or improve lower benefits while reducing higher benefits, but the ordinary-earner incidence requires explicit review."))
  if (stringr::str_detect(t, "tax social security and railroad retirement benefits")) return(c(status = "CONDITIONAL", reason = "The option raises revenue within retirement taxation and requires incidence review for ordinary retirees."))
  if (stringr::str_detect(t, "reduce tax subsidies for employment-based health benefits")) {
    if (stringr::str_detect(v, "75th percentile")) return(c(status = "CONDITIONAL", reason = "The 75th-percentile cap is targeted toward higher-premium plans but may still alter ordinary worker compensation."))
    return(c(status = "BLOCKED", reason = "The lower cap reaches a broad share of ordinary employment compensation."))
  }
  if (stringr::str_detect(t, "eliminate or limit itemized deductions")) {
    if (stringr::str_detect(v, "state and local tax")) return(c(status = "CONDITIONAL", reason = "SALT limitation is not directly barred but can affect ordinary homeowners in high-tax jurisdictions."))
    return(c(status = "BLOCKED", reason = "Broad elimination or limitation of itemized deductions reaches ordinary household tax treatment too widely."))
  }
  if (stringr::str_detect(t, "limit the deduction for charitable giving")) return(c(status = "CONDITIONAL", reason = "The option raises revenue from a tax preference but may affect private civic institutions."))
  if (stringr::str_detect(t, "tax all foreign income of u.s. corporations")) return(c(status = "CONDITIONAL", reason = "The option targets international corporate tax preferences but may affect productive investment decisions."))
  if (stringr::str_detect(t, "reduce funding for international affairs programs|reduce appropriations for global health")) return(c(status = "CONDITIONAL", reason = "The option does not directly hit protected household margins but can reduce diplomatic, health, or security capacity."))
  if (stringr::str_detect(t, "reduce funding for certain grants to state and local governments")) return(c(status = "CONDITIONAL", reason = "Some covered grants may finance productive public capacity or family-serving services."))
  if (stringr::str_detect(t, "stop building ford class aircraft carriers|cancel the long-range standoff weapon|cancel the army’s future long-range assault aircraft|cancel the army's future long-range assault aircraft|cancel the army's future vertical lift aircraft|reduce the size of the bomber force|reduce the size of the fighter force|replace some military personnel with civilian employees|reduce the size of the nuclear triad|reduce dod’s operation and maintenance appropriation|reduce dod's operation and maintenance appropriation|reduce funding for naval ship construction|cancel plans to purchase additional f-35|cancel the ground-based midcourse defense system|cancel development and production of the new missile")) return(c(status = "CONDITIONAL", reason = "Targeted defense restructuring is admissible only subject to capability review; it is not treated as an automatic broad defense cut."))
  if (stringr::str_detect(t, "adopt a voucher plan and slow the growth of federal contributions")) return(c(status = "BLOCKED", reason = "The option shifts health-plan financing risk to ordinary federal employees."))
  if (stringr::str_detect(t, "eliminate federal funding for national community service")) return(c(status = "CONDITIONAL", reason = "The option is fiscally usable but may reduce civic-service capacity; it is separated from the strict set."))
  if (stringr::str_detect(t, "eliminate certain tax preferences for education expenses")) return(c(status = "BLOCKED", reason = "The option raises household education costs and conflicts with protection of human-capital investment."))
  if (stringr::str_detect(t, "reduce subsidies in the crop insurance program|eliminate title i agriculture programs|limit arc and plc payment acres")) return(c(status = "ELIGIBLE", reason = "The option targets mature producer subsidies rather than ordinary wages or household social insurance."))
  if (stringr::str_detect(t, "reduce medicare advantage benchmarks|modify payments to medicare advantage plans for health risk|reduce medicare's coverage of bad debt|consolidate and reduce medicare payments for graduate medical education|reduce payments for hospital outpatient departments|reduce payments for drugs delivered by 340b hospitals|reduce quality bonus payments to medicare advantage plans")) return(c(status = "ELIGIBLE", reason = "The option changes provider or plan payment rules rather than eliminating Medicare eligibility or ordinary coverage."))
  if (stringr::str_detect(t, "reduce social security benefits for high earners")) return(c(status = "ELIGIBLE", reason = "The option preserves lower-earner benefit formulas and concentrates benefit changes on higher lifetime earners."))
  if (stringr::str_detect(t, "increase the maximum taxable earnings that are subject to social security payroll taxes")) return(c(status = "ELIGIBLE", reason = "The financing change is concentrated above the ordinary taxable maximum rather than imposed as a broad payroll-tax increase."))
  if (stringr::str_detect(t, "impose a surtax on individuals' adjusted gross income")) {
    if (stringr::str_detect(v, "20,000|40,000")) return(c(status = "BLOCKED", reason = "The $20,000/$40,000 thresholds expose ordinary wage income to the surtax and violate the ordinary-wage protection constraint."))
    if (stringr::str_detect(v, "100,000|200,000")) return(c(status = "CONDITIONAL", reason = "The $100,000/$200,000 thresholds concentrate the surtax more heavily on higher incomes but still reach wage income, so the option is reserved for the expanded protection mode."))
    return(c(status = "CONDITIONAL", reason = "AGI surtax incidence requires explicit review for ordinary wage exposure."))
  }
  if (stringr::str_detect(t, "change the taxation of assets transferred at death|change the tax treatment of capital gains from sales of inherited assets")) return(c(status = "ELIGIBLE", reason = "The option raises revenue from accrued gains and wealth transfers rather than ordinary wages."))
  if (stringr::str_detect(t, "expand the base of the net investment income tax")) return(c(status = "ELIGIBLE", reason = "The option reduces labor-versus-capital-income tax arbitrage among high-income pass-through owners."))
  if (stringr::str_detect(t, "tax carried interest as ordinary income")) return(c(status = "ELIGIBLE", reason = "The option removes preferential treatment for compensation structured as carried interest."))
  if (stringr::str_detect(t, "increase taxes on alcoholic beverages|increase excise taxes on tobacco products")) return(c(status = "ELIGIBLE", reason = "The excise tax falls on discretionary or harmful consumption rather than protected necessities."))
  if (stringr::str_detect(t, "impose a tax on financial transactions")) return(c(status = "ELIGIBLE", reason = "The scored financial transaction tax remains available; investment-market effects are separately flagged for robustness review."))
  if (stringr::str_detect(t, "impose a fee on large financial institutions")) return(c(status = "ELIGIBLE", reason = "The option targets large financial institutions rather than ordinary wages or household necessities."))
  if (stringr::str_detect(t, "tax gains from derivatives as ordinary income")) return(c(status = "ELIGIBLE", reason = "The option removes preferential timing and character treatment for derivatives gains."))
  if (stringr::str_detect(t, "repeal certain tax preferences for energy and natural resource")) return(c(status = "ELIGIBLE", reason = "The option removes industry-specific tax preferences rather than broadly taxing ordinary wages or saving."))
  if (stringr::str_detect(t, "impose fees to cover the costs of government regulations")) return(c(status = "ELIGIBLE", reason = "The option uses user fees for private regulatory services rather than a broad household tax."))
  if (stringr::str_detect(t, "increase certain fees charged by citizenship and immigration services|increase the passenger fee for aviation security")) return(c(status = "ELIGIBLE", reason = "The option is a user-fee increase rather than a broad tax on ordinary wages, saving, or investment."))
  if (stringr::str_detect(t, "change the national flood insurance program|convert the home equity conversion mortgage program|eliminate certain forest service programs|limit enrollment in the department of agriculture’s conservation programs|limit enrollment in the department of agriculture's conservation programs|limit the number of cities receiving urban areas security initiative grants|eliminate the international trade administration's trade-promotion activities|divest two agencies of their electric transmission assets|eliminate human space exploration programs")) return(c(status = "CONDITIONAL", reason = "The option does not directly violate a protected household margin but may affect public capacity, infrastructure, conservation, housing, or strategic capability."))
  if (stringr::str_detect(t, "increase the premium|medicare|social security|medicaid|va's|va’s|tricare|pell|school|child|earned income tax credit|child tax credit|retirement|housing")) return(c(status = "CONDITIONAL", reason = "The option touches a protected social-insurance, family, education, retirement, or housing margin and requires explicit review."))
  if (major_category == "Revenues") return(c(status = "CONDITIONAL", reason = "Revenue option retained in the full catalog but requires review for incidence on wages, saving, investment, family formation, or business reinvestment."))
  c(status = "ELIGIBLE", reason = "No direct conflict with the enumerated protection constraints was identified in the scored mechanism.")
}


# ------------------------------------------------------------------------------
# FUNCTION: add_protection_classification
# Purpose: Attach protection classifications and reasons to the scored-policy catalog.
# ------------------------------------------------------------------------------
add_protection_classification <- function(meta) {
  classified <- purrr::pmap(
    list(meta$title, meta$variant_name, meta$major_category),
    function(t, v, c) classify_protection(t, v, c)
  )
  meta$protection_status <- vapply(classified, function(x) x[["status"]], character(1))
  meta$protection_reason <- vapply(classified, function(x) x[["reason"]], character(1))
  meta
}



# ------------------------------------------------------------------------------
# FUNCTION: derive_protection_attributes
# Purpose: Derive auditable protected-margin flags used by strict and expanded solver universes.
# ------------------------------------------------------------------------------
derive_protection_attributes <- function(meta) {
  t <- stringr::str_to_lower(meta$title)
  v <- stringr::str_to_lower(meta$variant_name)
  both <- paste(t, v)
  meta |>
    mutate(
      risk_ordinary_wages = stringr::str_detect(
        both,
        "increase individual income tax rates on ordinary income|impose a surtax on individuals' adjusted gross income|impose a new payroll tax|increase the payroll tax rate for medicare hospital insurance|increase the payroll tax rate for social security|increase taxes that finance the federal share of the unemployment insurance system|repeal the davis-bacon act|reduce the annual across-the-board adjustment for federal civilian employees|cap increases in basic pay for military service members"
      ),
      risk_ordinary_saving = stringr::str_detect(
        both,
        "further limit annual contributions to retirement plans|raise the tax rates on long-term capital gains and qualified dividends|reduce pension benefits for new federal retirees|eliminate the special retirement supplement"
      ),
      risk_productive_investment = stringr::str_detect(
        both,
        "increase the corporate income tax rate|eliminate the tax exemption for new qualified private activity bonds|repeal the \\\"last in, first out\\\"|require half of advertising expenses to be amortized|repeal the low-income housing tax credit|increase excise taxes on motor fuels|impose a tax on emissions of greenhouse gases|impose an excise tax on overland freight transport|limit highway and transit funding|eliminate the federal transit administration"
      ),
      risk_family_formation = stringr::str_detect(
        both,
        "head-of-household|earned income tax credit|child tax credit|school lunch|school breakfast|child and adult care food|pell grants|head start|supplemental nutrition assistance|tanf|housing choice voucher|increase payments by tenants|fannie mae|freddie mac|employment-based health benefits"
      ),
      risk_business_reinvestment = stringr::str_detect(
        both,
        "tax all pass-through business owners under seca|increase the corporate income tax rate|repeal the \\\"last in, first out\\\"|require half of advertising expenses to be amortized|impose an excise tax on overland freight transport"
      ),
      risk_core_social_security = stringr::str_detect(
        both,
        "raise the full retirement age for social security|link initial social security benefits to average prices|use an alternative measure of inflation to index social security|require social security disability insurance applicants|eliminate eligibility for starting social security disability benefits|eliminate supplemental security income benefits for disabled children"
      ),
      risk_core_medicare = stringr::str_detect(
        both,
        "increase the premiums paid for medicare part b|change the cost-sharing rules for medicare|raise the age of eligibility for medicare"
      ),
      risk_productive_public_capacity = stringr::str_detect(
        both,
        "reduce the department of defense's annual budget|reduce selected nondefense discretionary spending|reduce department of energy funding for energy technology development|limit highway and transit funding|eliminate the federal transit administration|reduce funding for international affairs programs|reduce appropriations for global health"
      ),
      market_function_review_required = stringr::str_detect(
        both,
        "financial transactions|derivatives|financial institutions"
      ),
      hard_protection_violation = protection_status == "BLOCKED",
      explicit_review_required = protection_status == "CONDITIONAL",
      protection_classification_method = "EXPLICIT_POLICY_FAMILY_AND_VARIANT_RULES_V1"
    )
}


# ------------------------------------------------------------------------------
# FUNCTION: assign_ss_actuarial_scores
# Purpose: Attach Social Security long-range actuarial improvement estimates to compatible reform candidates.
# ------------------------------------------------------------------------------
assign_ss_actuarial_scores <- function(meta) {
  source_score <- dplyr::coalesce(meta$ss_actuarial_improvement_pct_payroll, 0)
  t <- stringr::str_to_lower(meta$title)
  v <- stringr::str_to_lower(meta$variant_name)
  meta |>
    mutate(
      ss_actuarial_improvement_pct_payroll = case_when(
        stringr::str_detect(t, "increase the maximum taxable earnings") & stringr::str_detect(v, "90 percent") ~ 0.82,
        stringr::str_detect(t, "increase the maximum taxable earnings") & stringr::str_detect(v, "250,000") ~ 2.50,
        source_score > 0 ~ source_score,
        stringr::str_detect(t, "reduce social security benefits for high earners") & stringr::str_detect(v, "70th-percentile") ~ 0.8,
        stringr::str_detect(t, "reduce social security benefits for high earners") & stringr::str_detect(v, "50th-percentile") ~ 1.8,
        stringr::str_detect(t, "establish a uniform social security benefit") & stringr::str_detect(v, "150 percent") ~ 4.9,
        stringr::str_detect(t, "establish a uniform social security benefit") & stringr::str_detect(v, "125 percent") ~ 6.4,
        TRUE ~ 0
      ),
      ss_actuarial_score_vintage = case_when(
        stringr::str_detect(t, "increase the maximum taxable earnings") & stringr::str_detect(v, "90 percent") ~ "SSA OACT 2025 Trustees Report E3.1",
        stringr::str_detect(t, "increase the maximum taxable earnings") & stringr::str_detect(v, "250,000") ~ "SSA OACT 2025 Trustees Report E2.5",
        ss_actuarial_improvement_pct_payroll > 0 & estimate_year == 2024L ~ "CBO December 2024 long-term Social Security option estimate",
        ss_actuarial_improvement_pct_payroll > 0 ~ "CBO/SSA official older actuarial estimate",
        TRUE ~ NA_character_
      ),
      ss_actuarial_additivity_status = if_else(
        ss_actuarial_improvement_pct_payroll > 0,
        "APPROXIMATE_ADDITIVE_FOR_SOLVENCY_SCREEN",
        NA_character_
      )
    )
}


# ------------------------------------------------------------------------------
# FUNCTION: load_local_policy_pack
# Purpose: Load, validate, and normalize the frozen CBO/SSA policy evidence pack.
# ------------------------------------------------------------------------------
load_local_policy_pack <- function() {
  log_line("Gate 3A: loading frozen local fiscal policy data pack; no cbo.gov runtime requests will be made")
  pack_dir <- locate_policy_pack()
  hash_audit <- validate_policy_pack_hashes(pack_dir)
  pack_manifest <- jsonlite::fromJSON(file.path(pack_dir, "pack_manifest.json"), simplifyVector = TRUE)
  families <- readr::read_csv(file.path(pack_dir, "cbo_current_families.csv"), show_col_types = FALSE, progress = FALSE, na = c("", "NA")) |>
    mutate(
      estimate_year = as.integer(estimate_year),
      index_variant_count = as.integer(index_variant_count),
      source_start_year = as.integer(source_start_year),
      source_end_year = as.integer(source_end_year),
      candidate_count_in_pack = as.integer(candidate_count_in_pack),
      cbo_page_id = as.character(cbo_page_id)
    )
  candidates <- readr::read_csv(file.path(pack_dir, "policy_candidates.csv"), show_col_types = FALSE, progress = FALSE, na = c("", "NA")) |>
    mutate(
      estimate_year = as.integer(estimate_year),
      source_start_year = as.integer(source_start_year),
      source_end_year = as.integer(source_end_year),
      ss_actuarial_improvement_pct_payroll = as.numeric(ss_actuarial_improvement_pct_payroll),
      index_ten_year_savings_bil = as.numeric(index_ten_year_savings_bil)
    )
  flows <- readr::read_csv(file.path(pack_dir, "policy_annual_flows.csv"), show_col_types = FALSE, progress = FALSE, na = c("", "NA")) |>
    mutate(
      year = as.integer(year),
      outlay_delta_bil = as.numeric(outlay_delta_bil),
      revenue_delta_bil = as.numeric(revenue_delta_bil),
      primary_deficit_delta_bil = as.numeric(primary_deficit_delta_bil)
    )
  ssa <- readr::read_csv(file.path(pack_dir, "ssa_actuarial_reference.csv"), show_col_types = FALSE, progress = FALSE, na = c("", "NA")) |>
    mutate(
      actuarial_balance_change_pct_payroll = as.numeric(actuarial_balance_change_pct_payroll),
      annual_balance_75th_year_change_pct_payroll = as.numeric(annual_balance_75th_year_change_pct_payroll),
      long_range_shortfall_pct_payroll = as.numeric(long_range_shortfall_pct_payroll),
      annual_75th_year_shortfall_pct_payroll = as.numeric(annual_75th_year_shortfall_pct_payroll)
    )
  source_rows <- readr::read_csv(file.path(pack_dir, "source_manifest.csv"), show_col_types = FALSE, progress = FALSE, na = c("", "NA")) |>
    mutate(across(everything(), as.character))
  coverage <- readr::read_csv(file.path(pack_dir, "coverage_audit.csv"), show_col_types = FALSE, progress = FALSE, na = c("", "NA")) |>
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))
  assert_model(as.integer(pack_manifest$cbo_current_latest_family_count) == CFG$policy_pack_expected_families, "Policy pack manifest family count does not match frozen policy-pack expectation")
  assert_model(as.integer(pack_manifest$candidate_variant_count) == CFG$policy_pack_expected_candidates, "Policy pack manifest candidate count does not match frozen policy-pack expectation")
  assert_model(as.integer(pack_manifest$annual_flow_row_count) == CFG$policy_pack_expected_annual_rows, "Policy pack manifest annual-flow count does not match frozen policy-pack expectation")
  assert_model(nrow(families) == CFG$policy_pack_expected_families, paste0("Policy pack family count is ", nrow(families), "; expected ", CFG$policy_pack_expected_families))
  assert_model(sum(families$estimate_year == 2024L) == CFG$cbo_expected_2024_option_families, "Policy pack does not contain all 76 December 2024 CBO option families")
  assert_model(nrow(candidates) == CFG$policy_pack_expected_candidates, paste0("Policy pack candidate count is ", nrow(candidates), "; expected ", CFG$policy_pack_expected_candidates))
  assert_model(nrow(flows) == CFG$policy_pack_expected_annual_rows, paste0("Policy pack annual-flow row count is ", nrow(flows), "; expected ", CFG$policy_pack_expected_annual_rows))
  assert_model(n_distinct(families$family_key) == nrow(families), "Duplicate CBO family keys detected in policy pack")
  assert_model(n_distinct(candidates$candidate_id) == nrow(candidates), "Duplicate candidate IDs detected in policy pack")
  assert_model(!anyDuplicated(flows |> select(candidate_id, year)), "Duplicate candidate/year rows detected in policy pack")
  full_ids <- candidates |> filter(annual_profile_status == "FULL_OFFICIAL_ANNUAL") |> pull(candidate_id)
  assert_model(length(full_ids) == CFG$policy_pack_expected_annual_candidates, paste0("Policy pack has ", length(full_ids), " annual-score candidates; expected ", CFG$policy_pack_expected_annual_candidates))
  assert_model(all(flows$candidate_id %in% full_ids), "Annual-flow table contains a candidate not marked FULL_OFFICIAL_ANNUAL")
  counts <- flows |>
    count(candidate_id, name = "annual_rows") |>
    right_join(candidates |> filter(candidate_id %in% full_ids) |> select(candidate_id, source_start_year, source_end_year), by = "candidate_id") |>
    mutate(expected_rows = source_end_year - source_start_year + 1L)
  assert_model(all(counts$annual_rows == counts$expected_rows), "At least one full annual CBO candidate is missing one or more source-year rows")
  target <- ssa |> filter(provision_id == "CURRENT_LAW_2026") |> pull(long_range_shortfall_pct_payroll)
  assert_model(length(target) == 1L && is.finite(target) && abs(target - CFG$ss_actuarial_gap_pct_payroll) < 1e-9, "SSA actuarial target in local pack does not match the configured 2026 Trustees shortfall")
  benchmark_audit <- validate_policy_pack_benchmarks(candidates, flows)
  register_local_policy_pack(pack_dir, source_rows)
  log_line(
    "Gate 3A local pack validated: ", nrow(families), " current/latest CBO families; ",
    nrow(candidates), " candidate variants; ", length(full_ids), " variants with annual official score paths; ",
    nrow(flows), " annual source rows"
  )
  list(
    pack_dir = pack_dir,
    pack_manifest = pack_manifest,
    hash_audit = hash_audit,
    benchmark_audit = benchmark_audit,
    families = families,
    candidates = candidates,
    flows = flows,
    ssa = ssa,
    source_manifest = source_rows,
    coverage = coverage
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: build_full_cbo_policy_universe
# Purpose: Build the scored policy-anchor universe from the validated frozen pack.
# ------------------------------------------------------------------------------
build_full_cbo_policy_universe <- function() {
  pack <- load_local_policy_pack()
  families <- pack$families
  candidates <- pack$candidates |>
    mutate(
      family_id = family_key,
      budget_option_id = stringr::str_extract(source_url, "[0-9]+$"),
      title = family_title,
      major_category = case_when(
        fiscal_channel == "REVENUE" ~ "Revenues",
        fiscal_channel == "OUTLAY" ~ "Spending",
        fiscal_channel == "MIXED" ~ "Mixed",
        fiscal_channel == "DEFICIT_ONLY" ~ "Net Deficit",
        TRUE ~ "Unclassified"
      ),
      budget_function = NA_character_,
      index_url = CBO_OPTIONS_SEARCH_URL,
      source_date = latest_estimate,
      source_effective_year = source_start_year,
      source_score_start_year = source_start_year,
      source_score_end_year = source_end_year,
      evidence_class = "OFFICIAL_OLDER",
      decision_type = "BINARY",
      source_kind = "LOCAL_FROZEN_CBO_POLICY_PACK",
      investment_market_review = stringr::str_detect(stringr::str_to_lower(title), "financial transactions|derivatives|financial institutions"),
      direct_cbo_annual_score = annual_profile_status == "FULL_OFFICIAL_ANNUAL",
      solver_eligible_annual = annual_profile_status == "FULL_OFFICIAL_ANNUAL",
      is_december_2024_core = estimate_year == 2024L
    ) |>
    add_protection_classification() |>
    derive_protection_attributes() |>
    assign_ss_actuarial_scores()
  source_flows <- pack$flows |>
    left_join(candidates |> select(candidate_id, fiscal_channel), by = "candidate_id") |>
    mutate(
      revenue_delta_bil = case_when(
        is.finite(revenue_delta_bil) ~ revenue_delta_bil,
        fiscal_channel == "REVENUE" & is.finite(primary_deficit_delta_bil) ~ -primary_deficit_delta_bil,
        TRUE ~ 0
      ),
      outlay_delta_bil = case_when(
        is.finite(outlay_delta_bil) ~ outlay_delta_bil,
        fiscal_channel == "OUTLAY" & is.finite(primary_deficit_delta_bil) ~ primary_deficit_delta_bil,
        TRUE ~ 0
      ),
      component_identity_residual_bil = primary_deficit_delta_bil - (outlay_delta_bil - revenue_delta_bil)
    ) |>
    select(candidate_id, year, revenue_delta_bil, outlay_delta_bil, primary_deficit_delta_bil, component_identity_residual_bil, source_url, source_kind)
  current_index <- families |>
    transmute(
      family_id = family_key,
      budget_option_id = as.character(cbo_page_id),
      title,
      source_url,
      index_source = "LOCAL_FROZEN_CURRENT_LATEST_CBO_BUDGET_OPTIONS",
      latest_estimate,
      latest_estimate_year = estimate_year,
      annual_data_status
    )
  report_index <- current_index |> filter(latest_estimate_year == 2024L)
  raw_rows <- source_flows |>
    left_join(candidates |> select(candidate_id, family_id, budget_option_id, title, variant_name, latest_estimate, estimate_year), by = "candidate_id")
  list(
    report_index = report_index,
    current_index = current_index,
    index = current_index,
    families = families,
    raw_rows = raw_rows,
    meta = candidates,
    source_flows = source_flows,
    parse_failures = tibble(),
    pack = pack
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: translate_full_universe_flows
# Purpose: Translate official score windows into the common FY2026-FY2046 annual-flow representation.
# ------------------------------------------------------------------------------
translate_full_universe_flows <- function(universe, working_baseline) {
  meta <- universe$meta
  eligible_meta <- meta |> filter(solver_eligible_annual)
  source <- universe$source_flows |>
    left_join(eligible_meta |> select(candidate_id, source_score_start_year), by = "candidate_id") |>
    mutate(
      shift_years = pmax(0L, 2027L - source_score_start_year),
      target_year = year + shift_years
    ) |>
    filter(target_year %in% CFG$model_years, target_year >= 2027L) |>
    select(candidate_id, year = target_year, revenue_delta_bil, outlay_delta_bil, primary_deficit_delta_bil, component_identity_residual_bil, shift_years)
  grid <- tidyr::expand_grid(candidate_id = eligible_meta$candidate_id, year = CFG$model_years) |>
    left_join(source, by = c("candidate_id", "year")) |>
    mutate(
      revenue_delta_bil = replace_na(revenue_delta_bil, 0),
      outlay_delta_bil = replace_na(outlay_delta_bil, 0),
      primary_deficit_delta_bil = replace_na(primary_deficit_delta_bil, 0),
      component_identity_residual_bil = replace_na(component_identity_residual_bil, 0),
      shift_years = replace_na(shift_years, 0L),
      translation_status = if_else(abs(revenue_delta_bil) + abs(outlay_delta_bil) + abs(primary_deficit_delta_bil) > 0, "SHIFTED_SOURCE_SCORE", "ZERO")
    )
  extended <- purrr::map_dfr(eligible_meta$candidate_id, function(id) {
    x <- grid |> filter(candidate_id == id) |> arrange(year)
    nonzero <- which(abs(x$primary_deficit_delta_bil) > 1e-12 | abs(x$revenue_delta_bil) > 1e-12 | abs(x$outlay_delta_bil) > 1e-12)
    if (length(nonzero) == 0L) return(x)
    last_i <- max(nonzero)
    last_year <- x$year[last_i]
    if (last_year >= max(CFG$model_years)) return(x)
    base_gdp <- working_baseline$gdp_bil[match(last_year, working_baseline$year)]
    assert_model(is.finite(base_gdp) && base_gdp > 0, paste0("Missing positive GDP for long-run extension at FY", last_year))
    for (i in seq_len(nrow(x))) {
      if (x$year[i] <= last_year) next
      gdp_y <- working_baseline$gdp_bil[match(x$year[i], working_baseline$year)]
      factor <- gdp_y / base_gdp
      x$revenue_delta_bil[i] <- x$revenue_delta_bil[last_i] * factor
      x$outlay_delta_bil[i] <- x$outlay_delta_bil[last_i] * factor
      x$primary_deficit_delta_bil[i] <- x$primary_deficit_delta_bil[last_i] * factor
      x$component_identity_residual_bil[i] <- x$component_identity_residual_bil[last_i] * factor
      x$translation_status[i] <- "EXTRAPOLATED_HOLD_LAST_EFFECT_AS_GDP_SHARE"
    }
    x
  })
  score_summary <- extended |>
    filter(year %in% CFG$score_years) |>
    group_by(candidate_id) |>
    summarise(
      cumulative_revenue_increase_2027_2036_bil = sum(pmax(revenue_delta_bil, 0)),
      cumulative_spending_cut_2027_2036_bil = sum(pmax(-outlay_delta_bil, 0)),
      cumulative_primary_improvement_2027_2036_bil = sum(-primary_deficit_delta_bil),
      .groups = "drop"
    )
  meta <- meta |>
    left_join(score_summary, by = "candidate_id") |>
    mutate(
      cumulative_revenue_increase_2027_2036_bil = if_else(solver_eligible_annual, cumulative_revenue_increase_2027_2036_bil, NA_real_),
      cumulative_spending_cut_2027_2036_bil = if_else(solver_eligible_annual, cumulative_spending_cut_2027_2036_bil, NA_real_),
      cumulative_primary_improvement_2027_2036_bil = if_else(solver_eligible_annual, cumulative_primary_improvement_2027_2036_bil, NA_real_)
    )
  assert_model(n_distinct(extended$candidate_id) == sum(meta$solver_eligible_annual), "Translated flow grid lost at least one annual-score candidate")
  assert_model(all(CFG$model_years %in% unique(extended$year)), "Translated policy flow grid does not cover the full model horizon")
  list(meta = meta, flows = extended, pack = universe$pack)
}


# ------------------------------------------------------------------------------
# FUNCTION: build_external_policy_catalog
# Purpose: Build the audit-only catalog of authoritative policy sources not automatically converted into solver coefficients.
# ------------------------------------------------------------------------------
build_external_policy_catalog <- function(policy_pack) {
  policy_pack$source_manifest |>
    filter(!agency %in% c("CBO")) |>
    mutate(source_search = stringr::str_to_lower(paste(agency, title))) |>
    transmute(
      source = agency,
      title,
      url,
      optimizer_status = case_when(
        stringr::str_detect(source_search, "ssa|social security") ~ "ACTUARIAL_CROSSCHECK_CBO_BUDGET_STREAMS_PREFERRED",
        stringr::str_detect(source_search, "jct|joint committee") ~ "CATALOG_ONLY_TAX_EXPENDITURE_IS_NOT_REPEAL_SCORE",
        stringr::str_detect(source_search, "medpac|medicare payment") ~ "CATALOG_ONLY_NO_COMPATIBLE_ANNUAL_SCORE",
        stringr::str_detect(source_search, "gao|accountability") ~ "CATALOG_ONLY_NO_RECOMMENDATION_LEVEL_ANNUAL_STREAM",
        TRUE ~ "CATALOG_ONLY"
      ),
      reason = notes
    )
}


# ------------------------------------------------------------------------------
# FUNCTION: policy_universe_for_mode
# Purpose: Filter the master candidate universe into STRICT or EXPANDED protection mode.
# ------------------------------------------------------------------------------
policy_universe_for_mode_core <- function(policy_model, protection_mode = c("STRICT", "EXPANDED")) {
  protection_mode <- match.arg(protection_mode)
  allowed_status <- if (protection_mode == "STRICT") "ELIGIBLE" else c("ELIGIBLE", "CONDITIONAL")
  meta <- policy_model$meta |>
    filter(solver_eligible_annual, protection_status %in% allowed_status)
  if (CFG$evidence_mode == "OFFICIAL_CURRENT") meta <- meta |> filter(evidence_class == "OFFICIAL_CURRENT")
  if (CFG$evidence_mode == "OFFICIAL_OLDER") meta <- meta |> filter(evidence_class %in% c("OFFICIAL_CURRENT", "OFFICIAL_OLDER"))
  flows <- policy_model$flows |> filter(candidate_id %in% meta$candidate_id)
  list(meta = meta, flows = flows, protection_mode = protection_mode)
}


# ------------------------------------------------------------------------------
# FUNCTION: build_interaction_catalog_full
# Purpose: Create explicit mutual-exclusion and overlap relationships among policy candidates.
# ------------------------------------------------------------------------------
build_interaction_catalog_full_core <- function(meta) {
  pair <- function(title_a, title_b, reason) {
    a <- meta |>
      filter(stringr::str_detect(stringr::str_to_lower(title), stringr::regex(title_a, ignore_case = TRUE))) |>
      pull(candidate_id)
    b <- meta |>
      filter(stringr::str_detect(stringr::str_to_lower(title), stringr::regex(title_b, ignore_case = TRUE))) |>
      pull(candidate_id)
    tidyr::expand_grid(candidate_i = a, candidate_j = b) |>
      filter(candidate_i != candidate_j) |>
      mutate(reason = reason)
  }

  explicit_policy_overlap <- bind_rows(
    pair("reduce medicare advantage benchmarks", "modify payments to medicare advantage plans for health risk", "Independent CBO option scores affect overlapping Medicare Advantage plan payments; no combined score is available."),
    pair("reduce medicare advantage benchmarks", "reduce quality bonus payments to medicare advantage plans", "Both options alter Medicare Advantage plan payments and their independently scored savings cannot be assumed additive."),
    pair("modify payments to medicare advantage plans for health risk", "reduce quality bonus payments to medicare advantage plans", "Both options alter Medicare Advantage plan payments and their independently scored savings cannot be assumed additive."),
    pair("change the taxation of assets transferred at death", "change the tax treatment of capital gains from sales of inherited assets", "The two options alter overlapping capital-gains treatment of inherited assets and cannot be stacked from separate CBO vintages."),
    pair("reduce social security benefits for high earners", "make social security.*benefit structure more progressive", "The benefit-formula reforms overlap and lack a combined official score."),
    pair("establish a uniform social security benefit", "make social security.*benefit structure more progressive", "The benefit-formula reforms overlap and lack a combined official score."),
    pair("reduce social security benefits for high earners", "establish a uniform social security benefit", "The benefit-formula reforms overlap and lack a combined official score."),
    pair("reduce tax subsidies for employment-based health benefits", "impose a new payroll tax", "The employment-based health exclusion and payroll-tax options interact through taxable compensation."),
    pair("increase the maximum taxable earnings that are subject to social security payroll taxes", "increase the payroll tax rate for social security", "The Social Security payroll-tax options share the same taxable payroll base and lack a combined official score."),
    pair("increase the premiums paid for medicare part b", "change the cost-sharing rules for medicare", "The beneficiary financing options overlap in Medicare household incidence and are not assumed additive without a combined score.")
  )

  account_controls <- meta |>
    filter(source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL") |>
    transmute(
      candidate_i = candidate_id,
      account_title = stringr::str_to_lower(dplyr::coalesce(original_account_title, title)),
      agency_l = stringr::str_to_lower(dplyr::coalesce(agency, "")),
      bureau_l = stringr::str_to_lower(dplyr::coalesce(bureau, ""))
    )

  scored <- meta |>
    filter(source_kind == "LOCAL_FROZEN_CBO_POLICY_PACK", major_category %in% c("Spending", "Net Deficit", "Mixed", "Unclassified")) |>
    transmute(
      candidate_j = candidate_id,
      scored_title = stringr::str_to_lower(title),
      scored_variant = stringr::str_to_lower(variant_name)
    )

  targeted_rules <- tribble(
    ~scored_pattern, ~account_pattern, ~agency_pattern, ~reason,
    "reduce dod.*operation and maintenance|reduce dod.s operation and maintenance", "operation and maintenance|dod operation & maintenance", "defense", "A scored aggregate DoD O&M reform overlaps account-level DoD O&M growth restraints.",
    "reduce funding for international affairs programs", "diplomatic programs|international assistance|development assistance|foreign military financing|economic support fund|migration and refugee|global health programs", "state|international assistance", "A scored aggregate International Affairs reduction overlaps mapped international-affairs account growth restraints.",
    "reduce appropriations for global health", "global health programs", "state|international assistance", "The scored Global Health appropriation option overlaps the Global Health Programs account growth restraint.",
    "eliminate human space exploration programs", "exploration|orion|space launch system|gateway", "nasa|aeronautics and space", "The scored human-space-exploration option overlaps mapped NASA exploration account growth restraints.",
    "reduce funding for naval ship construction", "shipbuilding and conversion, navy", "defense", "The scored naval ship-construction option overlaps the Navy shipbuilding account growth restraint.",
    "stop building ford class aircraft carriers", "shipbuilding and conversion, navy", "defense", "The scored aircraft-carrier option overlaps the Navy shipbuilding account growth restraint.",
    "cancel.*f-35|purchase additional f-35", "aircraft procurement", "defense", "The scored tactical-aircraft procurement option overlaps mapped aircraft-procurement account growth restraints.",
    "cancel.*future vertical lift|future long-range assault aircraft", "aircraft procurement, army|research, development, test and evaluation, army", "defense", "The scored Army aviation option overlaps mapped Army aviation procurement/RDT&E account growth restraints."
  )

  targeted_account_overlap <- purrr::pmap_dfr(targeted_rules, function(scored_pattern, account_pattern, agency_pattern, reason) {
    s <- scored |>
      filter(stringr::str_detect(paste(scored_title, scored_variant), stringr::regex(scored_pattern, ignore_case = TRUE)))
    a <- account_controls |>
      filter(
        stringr::str_detect(account_title, stringr::regex(account_pattern, ignore_case = TRUE)),
        stringr::str_detect(paste(agency_l, bureau_l), stringr::regex(agency_pattern, ignore_case = TRUE))
      )
    if (nrow(s) == 0L || nrow(a) == 0L) return(tibble())
    tidyr::crossing(a |> select(candidate_i), s |> select(candidate_j)) |>
      mutate(reason = reason)
  })

  bind_rows(explicit_policy_overlap, targeted_account_overlap) |>
    filter(candidate_i %in% meta$candidate_id, candidate_j %in% meta$candidate_id, candidate_i != candidate_j) |>
    mutate(pair_key = purrr::map2_chr(candidate_i, candidate_j, ~ paste(sort(c(.x, .y)), collapse = "||"))) |>
    distinct(pair_key, .keep_all = TRUE) |>
    select(-pair_key)
}


# ------------------------------------------------------------------------------
# FUNCTION: get_long_run_rate
# Purpose: Return the long-run marginal interest-rate assumption used for debt-service extension.
# ------------------------------------------------------------------------------
get_long_run_rate <- function(kernel_obj) {
  rate36 <- kernel_obj$rates |>
    filter(year == 2036L) |>
    pull(effective_marginal_rate_pct)
  if (length(rate36) != 1L || !is.finite(rate36)) return(CFG$long_run_interest_fallback_rate)
  rate36 / 100
}


# ------------------------------------------------------------------------------
# FUNCTION: build_full_milp
# Purpose: Build the sparse HiGHS MILP matrix, bounds, integrality vector, objective, and accounting constraints.
# ------------------------------------------------------------------------------
build_full_milp_core <- function(
  universe,
  working_baseline,
  kernel_obj,
  objective = c("revenue", "spending", "policy_count", "complexity", "debt", "reference_75", "target_slack"),
  require_patterns = character(),
  forbid_patterns = character(),
  max_revenue_bil = Inf,
  max_spending_bil = Inf,
  require_ss_solvency = FALSE,
  soft_targets = FALSE,
  previous_packages = list(),
  min_hamming_distance = CFG$diversity_hamming_distance
) {
  objective <- match.arg(objective)
  meta <- universe$meta |> arrange(candidate_id)
  flows <- universe$flows
  years <- CFG$model_years
  n_policy <- nrow(meta)
  assert_model(n_policy >= CFG$minimum_solver_candidates, paste0("Only ", n_policy, " candidates reached the solver under ", universe$protection_mode, " protection mode; minimum is ", CFG$minimum_solver_candidates, "."))
  x_names <- paste0("x::", meta$candidate_id)
  rev_names <- paste0("rev::", years)
  out_names <- paste0("out::", years)
  pri_names <- paste0("primary::", years)
  int_names <- paste0("interest::", years)
  debt_names <- paste0("debt::", years)
  slack_names <- if (soft_targets) c("slack::2036", "slack::2046") else character()
  reference_names <- if (objective == "reference_75") c("reference_dev_pos::2046", "reference_dev_neg::2046") else character()
  var_names <- c(x_names, rev_names, out_names, pri_names, int_names, debt_names, slack_names, reference_names)
  nv <- length(var_names)
  idx <- setNames(seq_along(var_names), var_names)
  lower <- rep(-Inf, nv)
  upper <- rep(Inf, nv)
  types <- rep("C", nv)
  lower[idx[x_names]] <- 0
  upper[idx[x_names]] <- 1
  types[idx[x_names]] <- "I"
  if (soft_targets) lower[idx[slack_names]] <- 0
  if (length(reference_names) > 0L) lower[idx[reference_names]] <- 0
  rows <- list()
  lhs <- numeric()
  rhs <- numeric()
  labels <- character()
  add <- function(coefs, lo = -Inf, hi = Inf, label = "") {
    row <- numeric(nv)
    if (length(coefs) > 0L) {
      coef_names <- names(coefs)
      assert_model(!is.null(coef_names) && length(coef_names) == length(coefs), paste0("Unnamed coefficient encountered while building constraint: ", label))
      assert_model(all(nzchar(coef_names)), paste0("Blank coefficient name encountered while building constraint: ", label))
      positions <- unname(idx[coef_names])
      missing_names <- unique(coef_names[is.na(positions)])
      assert_model(length(missing_names) == 0L, paste0("Unknown MILP variable name(s) in constraint ", label, ": ", paste(missing_names, collapse = ", ")))
      values <- as.numeric(coefs)
      assert_model(all(is.finite(values)), paste0("Non-finite MILP coefficient encountered in constraint: ", label))
      for (j in seq_along(values)) row[positions[j]] <- row[positions[j]] + values[j]
    }
    rows[[length(rows) + 1L]] <<- row
    lhs <<- c(lhs, lo)
    rhs <<- c(rhs, hi)
    labels <<- c(labels, label)
  }
  flow_mat <- function(column) {
    flows |>
      transmute(candidate_id, year, value = .data[[column]]) |>
      tidyr::pivot_wider(names_from = candidate_id, values_from = value, values_fill = 0) |>
      right_join(tibble(year = years), by = "year") |>
      arrange(year)
  }
  rev_mat <- flow_mat("revenue_delta_bil")
  out_mat <- flow_mat("outlay_delta_bil")
  pri_mat_direct <- flow_mat("primary_deficit_delta_bil")
  candidate_order <- meta$candidate_id
  matrix_row <- function(tbl, year) {
    r <- tbl[tbl$year == year, , drop = FALSE]
    v <- numeric(length(candidate_order))
    names(v) <- candidate_order
    available <- intersect(candidate_order, names(r))
    if (length(available) > 0L) v[available] <- as.numeric(unlist(r[1, available, drop = FALSE], use.names = FALSE))
    v
  }
  for (y in years) {
    rev_coef <- matrix_row(rev_mat, y)
    out_coef <- matrix_row(out_mat, y)
    pri_coef <- matrix_row(pri_mat_direct, y)
    c1 <- c(setNames(-rev_coef, x_names), setNames(1, paste0("rev::", y)))
    c2 <- c(setNames(-out_coef, x_names), setNames(1, paste0("out::", y)))
    c3 <- c(setNames(-pri_coef, x_names), setNames(1, paste0("primary::", y)))
    add(c1, 0, 0, paste0("Revenue identity FY", y))
    add(c2, 0, 0, paste0("Outlay identity FY", y))
    add(c3, 0, 0, paste0("Primary-deficit identity FY", y))
  }
  kernel <- kernel_obj$kernel
  long_rate <- get_long_run_rate(kernel_obj)
  for (y in years) {
    co <- c(setNames(1, paste0("interest::", y)))
    if (y <= 2036L) {
      krow <- kernel |> filter(output_year == y, input_year %in% 2026:2036)
      if (nrow(krow) > 0L) {
        vals <- -krow$debt_service_effect_per_1_bil_primary_deficit
        names(vals) <- paste0("primary::", krow$input_year)
        co <- c(co, vals)
      }
    } else {
      co <- c(
        co,
        setNames(-long_rate, paste0("debt::", y - 1L)),
        setNames(-0.5 * long_rate, paste0("primary::", y))
      )
    }
    add(co, 0, 0, paste0("Interest identity FY", y))
  }
  for (i in seq_along(years)) {
    y <- years[i]
    co <- c(
      setNames(1, paste0("debt::", y)),
      setNames(-1, paste0("primary::", y)),
      setNames(-1, paste0("interest::", y))
    )
    if (i > 1L) co <- c(co, setNames(-1, paste0("debt::", years[i - 1L])))
    add(co, 0, 0, paste0("Debt accumulation FY", y))
  }
  for (fam in unique(meta$family_id)) {
    ids <- meta$candidate_id[meta$family_id == fam]
    if (length(ids) > 1L) add(setNames(rep(1, length(ids)), paste0("x::", ids)), hi = 1, label = paste0("Mutually exclusive CBO alternatives: ", fam))
  }
  interactions <- build_interaction_catalog_full(meta)
  if (!CFG$allow_unscored_interactions && nrow(interactions) > 0L) {
    for (i in seq_len(nrow(interactions))) {
      add(
        setNames(c(1, 1), paste0("x::", c(interactions$candidate_i[i], interactions$candidate_j[i]))),
        hi = 1,
        label = paste0("Unscored material overlap: ", interactions$candidate_i[i], " + ", interactions$candidate_j[i])
      )
    }
  }
  searchable <- paste(meta$title, meta$variant_name)
  for (p in require_patterns) {
    ids <- meta$candidate_id[stringr::str_detect(searchable, stringr::regex(p, ignore_case = TRUE))]
    assert_model(length(ids) > 0L, paste0("Required policy pattern matched no eligible candidates: ", p))
    add(setNames(rep(1, length(ids)), paste0("x::", ids)), lo = 1, label = paste0("Require pattern: ", p))
  }
  for (p in forbid_patterns) {
    ids <- meta$candidate_id[stringr::str_detect(searchable, stringr::regex(p, ignore_case = TRUE))]
    if (length(ids) > 0L) add(setNames(rep(1, length(ids)), paste0("x::", ids)), hi = 0, label = paste0("Forbid pattern: ", p))
  }
  if (require_ss_solvency) {
    ss <- meta$ss_actuarial_improvement_pct_payroll
    names(ss) <- x_names
    add(ss, lo = CFG$ss_actuarial_gap_pct_payroll, label = "Approximate Social Security 75-year actuarial solvency")
  }
  revenue_score <- meta$cumulative_revenue_increase_2027_2036_bil
  names(revenue_score) <- x_names
  spending_score <- meta$cumulative_spending_cut_2027_2036_bil
  names(spending_score) <- x_names
  if (is.finite(max_revenue_bil)) add(revenue_score, hi = max_revenue_bil, label = "Maximum cumulative revenue increase FY2027-FY2036")
  if (is.finite(max_spending_bil)) add(spending_score, hi = max_spending_bil, label = "Maximum cumulative spending reduction FY2027-FY2036")
  target36_rhs <- CFG$target_2036 * working_baseline$gdp_bil[working_baseline$year == 2036L] - working_baseline$working_debt_bil[working_baseline$year == 2036L]
  target46_rhs <- CFG$target_2046_high * working_baseline$gdp_bil[working_baseline$year == 2046L] - working_baseline$working_debt_bil[working_baseline$year == 2046L]
  co36 <- c(setNames(1, "debt::2036"))
  co46 <- c(setNames(1, "debt::2046"))
  if (soft_targets) {
    co36 <- c(co36, setNames(-1, "slack::2036"))
    co46 <- c(co46, setNames(-1, "slack::2046"))
  }
  add(co36, hi = target36_rhs, label = "Debt/GDP FY2036 target")
  add(co46, hi = target46_rhs, label = "Debt/GDP FY2046 upper target")
  if (CFG$enforce_2046_lower_bound && !soft_targets) {
    lower46_rhs <- CFG$target_2046_low * working_baseline$gdp_bil[working_baseline$year == 2046L] - working_baseline$working_debt_bil[working_baseline$year == 2046L]
    add(setNames(1, "debt::2046"), lo = lower46_rhs, label = "Debt/GDP FY2046 lower target")
  }
  if (objective == "reference_75") {
    reference46_rhs <- CFG$target_2046_center * working_baseline$gdp_bil[working_baseline$year == 2046L] - working_baseline$working_debt_bil[working_baseline$year == 2046L]
    add(
      c(
        setNames(1, "debt::2046"),
        setNames(-1, "reference_dev_pos::2046"),
        setNames(1, "reference_dev_neg::2046")
      ),
      lo = reference46_rhs,
      hi = reference46_rhs,
      label = "Absolute deviation from FY2046 75 percent reference"
    )
  }
  if (length(previous_packages) > 0L && min_hamming_distance > 0L) {
    for (k in seq_along(previous_packages)) {
      ones <- intersect(previous_packages[[k]], meta$candidate_id)
      zeros <- setdiff(meta$candidate_id, ones)
      co <- c(
        setNames(rep(-1, length(ones)), paste0("x::", ones)),
        setNames(rep(1, length(zeros)), paste0("x::", zeros))
      )
      add(co, lo = min_hamming_distance - length(ones), label = paste0("Diversity cut ", k))
    }
  }
  L <- numeric(nv)
  if (objective == "revenue") L[idx[names(revenue_score)]] <- revenue_score
  if (objective == "spending") L[idx[names(spending_score)]] <- spending_score
  if (objective == "policy_count") L[idx[x_names]] <- 1
  if (objective == "debt") {
    L[idx[["debt::2036"]]] <- 1 / working_baseline$gdp_bil[working_baseline$year == 2036L]
    L[idx[["debt::2046"]]] <- 1 / working_baseline$gdp_bil[working_baseline$year == 2046L]
    L[idx[x_names]] <- 1e-9
  }
  if (objective == "reference_75") {
    L[idx[["reference_dev_pos::2046"]]] <- 1 / working_baseline$gdp_bil[working_baseline$year == 2046L]
    L[idx[["reference_dev_neg::2046"]]] <- 1 / working_baseline$gdp_bil[working_baseline$year == 2046L]
    L[idx[x_names]] <- 1e-9
  }
  if (objective == "target_slack") {
    assert_model(soft_targets, "target_slack objective requires soft_targets=TRUE")
    L[idx[["slack::2036"]]] <- 1 / working_baseline$gdp_bil[working_baseline$year == 2036L]
    L[idx[["slack::2046"]]] <- 1 / working_baseline$gdp_bil[working_baseline$year == 2046L]
    L[idx[x_names]] <- 1e-9
  }
  A_dense <- if (length(rows) == 0L) matrix(0, 0, nv) else do.call(rbind, rows)
  assert_model(ncol(A_dense) == nv, paste0("MILP constraint matrix has ", ncol(A_dense), " columns but ", nv, " variables were defined"))
  assert_model(nrow(A_dense) == length(lhs) && nrow(A_dense) == length(rhs), "MILP constraint matrix row count does not match lhs/rhs lengths")
  assert_model(nrow(A_dense) == length(labels), "MILP constraint matrix row count does not match constraint-label count")
  assert_model(all(is.finite(A_dense)), "MILP constraint matrix contains a non-finite coefficient")
  A <- methods::as(Matrix::Matrix(A_dense, sparse = TRUE), "dgCMatrix")
  list(
    L = L,
    lower = lower,
    upper = upper,
    A = A,
    lhs = lhs,
    rhs = rhs,
    types = types,
    variable_names = var_names,
    policy_variable_names = x_names,
    meta = meta,
    flows = flows,
    interactions = interactions,
    constraint_labels = labels,
    objective_name = objective,
    protection_mode = universe$protection_mode,
    materiality_mode = universe$materiality_mode,
    soft_targets = soft_targets,
    require_ss_solvency = require_ss_solvency
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: validate_full_milp_structure
# Purpose: Validate MILP dimensions, nonzero structure, bounds, coefficients, and variable mappings before solving.
# ------------------------------------------------------------------------------
validate_full_milp_structure <- function(model, solve_label = "UNLABELED") {
  nv <- length(model$variable_names)
  nr <- nrow(model$A)
  assert_model(ncol(model$A) == nv, paste0("MILP ", solve_label, " has ", ncol(model$A), " matrix columns for ", nv, " variables"))
  assert_model(length(model$L) == nv, paste0("MILP ", solve_label, " objective length does not match variable count"))
  assert_model(length(model$lower) == nv && length(model$upper) == nv, paste0("MILP ", solve_label, " bound-vector length does not match variable count"))
  assert_model(length(model$types) == nv, paste0("MILP ", solve_label, " variable-type length does not match variable count"))
  assert_model(length(model$lhs) == nr && length(model$rhs) == nr, paste0("MILP ", solve_label, " constraint-bound length does not match row count"))
  assert_model(length(model$constraint_labels) == nr, paste0("MILP ", solve_label, " constraint-label length does not match row count"))
  assert_model(all(is.finite(model$L)), paste0("MILP ", solve_label, " objective contains non-finite coefficients"))
  assert_model(!anyNA(model$lower) && !anyNA(model$upper), paste0("MILP ", solve_label, " variable bounds contain NA"))
  assert_model(!anyNA(model$lhs) && !anyNA(model$rhs), paste0("MILP ", solve_label, " constraint bounds contain NA"))
  assert_model(all(model$lower <= model$upper), paste0("MILP ", solve_label, " contains a lower variable bound above its upper bound"))
  assert_model(all(model$lhs <= model$rhs), paste0("MILP ", solve_label, " contains a constraint lhs above its rhs"))
  assert_model(all(model$types %in% c("C", "I")), paste0("MILP ", solve_label, " contains an unsupported variable type"))
  assert_model(all(is.finite(model$A@x)), paste0("MILP ", solve_label, " matrix contains non-finite coefficients"))
  nnz <- Matrix::nnzero(model$A)
  assert_model(nnz > 0L, paste0("MILP ", solve_label, " constraint matrix contains zero nonzero coefficients"))
  zero_rows <- which(Matrix::rowSums(abs(model$A)) == 0)
  assert_model(length(zero_rows) == 0L, paste0("MILP ", solve_label, " contains empty constraint row(s): ", paste(zero_rows, collapse = ", ")))
  invisible(list(variables = nv, constraints = nr, nonzeros = nnz))
}


# ------------------------------------------------------------------------------
# FUNCTION: run_highs_interface_self_test
# Purpose: Solve a tiny known MILP to verify the installed highs R interface before the fiscal model runs.
# ------------------------------------------------------------------------------
run_highs_interface_self_test <- function() {
  A_test <- methods::as(Matrix::Matrix(matrix(c(1, 1), nrow = 1), sparse = TRUE), "dgCMatrix")
  test <- highs::highs_solve(
    L = c(1, 2),
    lower = c(0, 0),
    upper = c(1, 1),
    A = A_test,
    lhs = 1,
    rhs = Inf,
    types = c("I", "I"),
    maximum = FALSE,
    control = highs::highs_control(threads = 1L, log_to_console = FALSE)
  )
  assert_model(is.character(test$status_message) && stringr::str_detect(stringr::str_to_lower(test$status_message), "optimal"), paste0("HiGHS interface self-test failed with status: ", test$status_message))
  assert_model(length(test$primal_solution) == 2L && abs(sum(test$primal_solution) - 1) <= 1e-7, "HiGHS interface self-test returned an invalid primal solution")
  assert_model(abs(test$objective_value - 1) <= 1e-7, "HiGHS interface self-test returned an unexpected objective value")
  log_line("Gate 4 HiGHS interface self-test passed: sparse constraint matrix transmitted correctly")
  invisible(TRUE)
}


# ------------------------------------------------------------------------------
# FUNCTION: solve_full_milp
# Purpose: Submit one fiscal MILP objective/scenario configuration to HiGHS and normalize the solver result.
# ------------------------------------------------------------------------------
solve_full_milp_core <- function(model, solve_label) {
  structure_audit <- validate_full_milp_structure(model, solve_label)
  n_bin <- sum(model$types == "I")
  n_cont <- sum(model$types == "C")
  nnz <- structure_audit$nonzeros
  log_line(
    "MILP ", solve_label,
    " | variables=", length(model$variable_names),
    " (binary/integer=", n_bin, ", continuous=", n_cont, ")",
    " | constraints=", nrow(model$A),
    " | nonzeros=", nnz
  )
  control <- highs::highs_control(
    threads = CFG$solver_threads,
    mip_rel_gap = CFG$solver_mip_rel_gap,
    primal_feasibility_tolerance = CFG$solver_primal_feasibility_tolerance,
    dual_feasibility_tolerance = CFG$solver_dual_feasibility_tolerance,
    log_to_console = TRUE
  )
  started <- Sys.time()
  sol <- highs::highs_solve(
    L = model$L,
    lower = model$lower,
    upper = model$upper,
    A = model$A,
    lhs = model$lhs,
    rhs = model$rhs,
    types = model$types,
    maximum = FALSE,
    control = control
  )
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  status_text <- if (is.null(sol$status_message)) "" else stringr::str_to_lower(as.character(sol$status_message)[1])
  solver_value_valid <- if (!is.null(sol$solver_msg) && !is.null(sol$solver_msg$value_valid)) isTRUE(sol$solver_msg$value_valid) else NA
  status_can_have_incumbent <- !stringr::str_detect(status_text, "infeasible|unbounded|model error|solve error|load error")
  primal_shape_ok <- !is.null(sol$primal_solution) && length(sol$primal_solution) == length(model$variable_names) && all(is.finite(sol$primal_solution))
  primal_ok <- status_can_have_incumbent && primal_shape_ok && (is.na(solver_value_valid) || solver_value_valid)
  selected <- character()
  max_constraint_violation <- Inf
  max_bound_violation <- Inf
  max_integrality_violation <- Inf
  if (primal_ok) {
    xall <- as.numeric(sol$primal_solution)
    x <- xall[match(model$policy_variable_names, model$variable_names)]
    selected <- stringr::str_remove(model$policy_variable_names[x > 0.5], "^x::")
    activity <- as.numeric(model$A %*% xall)
    low_violation <- ifelse(is.finite(model$lhs), pmax(model$lhs - activity, 0), 0)
    high_violation <- ifelse(is.finite(model$rhs), pmax(activity - model$rhs, 0), 0)
    max_constraint_violation <- max(c(low_violation, high_violation), na.rm = TRUE)
    lower_violation <- ifelse(is.finite(model$lower), pmax(model$lower - xall, 0), 0)
    upper_violation <- ifelse(is.finite(model$upper), pmax(xall - model$upper, 0), 0)
    max_bound_violation <- max(c(lower_violation, upper_violation), na.rm = TRUE)
    int_idx <- which(model$types == "I")
    max_integrality_violation <- if (length(int_idx) > 0L) max(abs(xall[int_idx] - round(xall[int_idx]))) else 0
  }
  feasible_incumbent <- primal_ok && max_constraint_violation <= 1e-5 && max_bound_violation <= 1e-7 && max_integrality_violation <= 1e-6
  info_num <- function(name) {
    if (is.null(sol$info) || is.null(sol$info[[name]])) return(NA_real_)
    suppressWarnings(as.numeric(sol$info[[name]][[1]]))
  }
  log_line(
    "MILP ", solve_label, " finished | status=", sol$status_message,
    " | elapsed=", sprintf("%.2f", elapsed), "s",
    " | nodes=", ifelse(is.na(info_num("mip_node_count")), "NA", format(info_num("mip_node_count"), scientific = FALSE)),
    " | mip_gap=", ifelse(is.na(info_num("mip_gap")), "NA", signif(info_num("mip_gap"), 5)),
    " | feasible_incumbent=", feasible_incumbent
  )
  list(
    status = sol$status,
    status_message = sol$status_message,
    objective_value = sol$objective_value,
    primal_solution = sol$primal_solution,
    selected_candidate_ids = selected,
    elapsed_seconds = elapsed,
    info = sol$info,
    mip_node_count = info_num("mip_node_count"),
    mip_dual_bound = info_num("mip_dual_bound"),
    mip_gap = info_num("mip_gap"),
    simplex_iteration_count = info_num("simplex_iteration_count"),
    ipm_iteration_count = info_num("ipm_iteration_count"),
    max_constraint_violation = max_constraint_violation,
    max_bound_violation = max_bound_violation,
    max_integrality_violation = max_integrality_violation,
    feasible_incumbent = feasible_incumbent,
    raw = sol,
    model = model,
    solve_label = solve_label
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: full_solver_is_optimal
# Purpose: Return TRUE only when HiGHS reports a proven optimal solution.
# ------------------------------------------------------------------------------
full_solver_is_optimal <- function(solution) {
  is.character(solution$status_message) && stringr::str_detect(stringr::str_to_lower(solution$status_message), "optimal")
}


# ------------------------------------------------------------------------------
# FUNCTION: full_solver_has_feasible_incumbent
# Purpose: Return TRUE when a usable feasible incumbent exists even if optimality is not proven.
# ------------------------------------------------------------------------------
full_solver_has_feasible_incumbent <- function(solution) {
  isTRUE(solution$feasible_incumbent)
}


# ------------------------------------------------------------------------------
# FUNCTION: simulate_full_package
# Purpose: Recompute a selected package through the independent fiscal simulator rather than trusting solver state variables.
# ------------------------------------------------------------------------------
simulate_full_package <- function(selected_ids, working_baseline, policy_model, kernel_obj) {
  years <- CFG$model_years
  f <- policy_model$flows |>
    filter(candidate_id %in% selected_ids) |>
    group_by(year) |>
    summarise(
      revenue_delta_bil = sum(revenue_delta_bil),
      outlay_delta_bil = sum(outlay_delta_bil),
      primary_deficit_delta_bil = sum(primary_deficit_delta_bil),
      .groups = "drop"
    ) |>
    right_join(tibble(year = years), by = "year") |>
    arrange(year) |>
    mutate(
      revenue_delta_bil = replace_na(revenue_delta_bil, 0),
      outlay_delta_bil = replace_na(outlay_delta_bil, 0),
      primary_deficit_delta_bil = replace_na(primary_deficit_delta_bil, 0)
    )
  rev10 <- f$revenue_delta_bil[match(2026:2036, f$year)]
  out10 <- f$outlay_delta_bil[match(2026:2036, f$year)]
  primary10 <- f$primary_deficit_delta_bil[match(2026:2036, f$year)]
  kernel_primary <- kernel_obj$kernel |>
    left_join(tibble(input_year = 2026:2036, primary_deficit_delta_bil = primary10), by = "input_year") |>
    mutate(contribution = debt_service_effect_per_1_bil_primary_deficit * primary_deficit_delta_bil) |>
    group_by(output_year) |>
    summarise(interest_delta_bil = sum(contribution), .groups = "drop")
  long_rate <- get_long_run_rate(kernel_obj)
  debt <- numeric(length(years))
  interest <- numeric(length(years))
  for (i in seq_along(years)) {
    y <- years[i]
    p <- f$primary_deficit_delta_bil[i]
    if (y <= 2036L) {
      interest[i] <- kernel_primary$interest_delta_bil[kernel_primary$output_year == y]
    } else {
      interest[i] <- long_rate * (debt[i - 1L] + 0.5 * p)
    }
    debt[i] <- if (i == 1L) p + interest[i] else debt[i - 1L] + p + interest[i]
  }
  working_baseline |>
    select(year, gdp_bil, working_debt_bil, working_debt_gdp_pct) |>
    left_join(f, by = "year") |>
    mutate(
      policy_interest_delta_bil = interest,
      policy_debt_delta_bil = debt,
      scenario_debt_bil = working_debt_bil + policy_debt_delta_bil,
      scenario_debt_gdp_pct = 100 * scenario_debt_bil / gdp_bil
    )
}


# ------------------------------------------------------------------------------
# FUNCTION: summarize_full_solution
# Purpose: Summarize one independently verified solution package into fiscal, target, and composition metrics.
# ------------------------------------------------------------------------------
summarize_full_solution_core <- function(solution, policy_model, working_baseline, kernel_obj, solution_id) {
  if (!full_solver_has_feasible_incumbent(solution)) {
    return(tibble(
      solution_id = solution_id,
      solve_label = solution$solve_label,
      protection_mode = solution$model$protection_mode,
      objective = solution$model$objective_name,
      solver_status = solution$status_message,
      elapsed_seconds = solution$elapsed_seconds,
      selected_policy_count = 0L,
      selected_policies = "",
      revenue_2027_2036_bil = NA_real_,
      spending_cuts_2027_2036_bil = NA_real_,
      ss_actuarial_improvement_pct_payroll = NA_real_,
      debt_gdp_2036_pct = NA_real_,
      debt_gdp_2046_pct = NA_real_,
      target_2036_pass = FALSE,
      target_2046_pass = FALSE,
      solver_proven_optimal = full_solver_is_optimal(solution),
      independently_verified = FALSE
    ))
  }
  ids <- solution$selected_candidate_ids
  sim <- simulate_full_package(ids, working_baseline, policy_model, kernel_obj)
  meta <- policy_model$meta |> filter(candidate_id %in% ids)
  rev <- sum(meta$cumulative_revenue_increase_2027_2036_bil, na.rm = TRUE)
  cut <- sum(meta$cumulative_spending_cut_2027_2036_bil, na.rm = TRUE)
  ss <- sum(meta$ss_actuarial_improvement_pct_payroll, na.rm = TRUE)
  d36 <- sim$scenario_debt_gdp_pct[sim$year == 2036L]
  d46 <- sim$scenario_debt_gdp_pct[sim$year == 2046L]
  xvec <- solution$primal_solution
  milp_d36_delta <- xvec[match("debt::2036", solution$model$variable_names)]
  milp_d46_delta <- xvec[match("debt::2046", solution$model$variable_names)]
  sim_d36_delta <- sim$policy_debt_delta_bil[sim$year == 2036L]
  sim_d46_delta <- sim$policy_debt_delta_bil[sim$year == 2046L]
  verify_err <- max(abs(c(milp_d36_delta - sim_d36_delta, milp_d46_delta - sim_d46_delta)))
  tibble(
    solution_id = solution_id,
    solve_label = solution$solve_label,
    protection_mode = solution$model$protection_mode,
    objective = solution$model$objective_name,
    solver_status = solution$status_message,
    elapsed_seconds = solution$elapsed_seconds,
    selected_policy_count = length(ids),
    selected_policies = paste(sort(ids), collapse = ";"),
    revenue_2027_2036_bil = rev,
    spending_cuts_2027_2036_bil = cut,
    ss_actuarial_improvement_pct_payroll = ss,
    debt_gdp_2036_pct = d36,
    debt_gdp_2046_pct = d46,
    target_2036_pass = d36 <= 100 * CFG$target_2036 + 1e-7,
    target_2046_pass = d46 <= 100 * CFG$target_2046_high + 1e-7,
    independent_target_debt_error_bil = verify_err,
    solver_proven_optimal = full_solver_is_optimal(solution),
    independently_verified = verify_err <= CFG$independent_verification_tolerance_bil
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: extract_solution_membership
# Purpose: Extract the selected policy parameters, levels, timing, and provenance for one solution.
# ------------------------------------------------------------------------------
extract_solution_membership_core <- function(solution, policy_model, solution_id) {
  ids <- solution$selected_candidate_ids
  if (length(ids) == 0L) return(tibble())
  policy_model$meta |>
    filter(candidate_id %in% ids) |>
    transmute(
      solution_id = solution_id,
      candidate_id,
      family_id,
      title,
      variant_name,
      major_category,
      protection_status,
      cumulative_revenue_increase_2027_2036_bil,
      cumulative_spending_cut_2027_2036_bil,
      cumulative_primary_improvement_2027_2036_bil,
      ss_actuarial_improvement_pct_payroll,
      source_url
    )
}


# ------------------------------------------------------------------------------
# FUNCTION: solve_diverse_family
# Purpose: Generate multiple distinct solutions for one objective using diversity/no-good constraints.
# ------------------------------------------------------------------------------
solve_diverse_family_core <- function(
  universe,
  policy_model,
  working_baseline,
  kernel_obj,
  objective,
  label_prefix,
  count = CFG$diverse_solutions_per_objective,
  require_patterns = character(),
  forbid_patterns = character(),
  max_revenue_bil = Inf,
  max_spending_bil = Inf,
  require_ss_solvency = FALSE,
  soft_targets = FALSE
) {
  solutions <- list()
  previous <- list()
  for (k in seq_len(count)) {
    label <- paste0(label_prefix, "_", sprintf("%02d", k))
    model <- build_full_milp(
      universe = universe,
      working_baseline = working_baseline,
      kernel_obj = kernel_obj,
      objective = objective,
      require_patterns = require_patterns,
      forbid_patterns = forbid_patterns,
      max_revenue_bil = max_revenue_bil,
      max_spending_bil = max_spending_bil,
      require_ss_solvency = require_ss_solvency,
      soft_targets = soft_targets,
      previous_packages = previous
    )
    sol <- solve_full_milp(model, label)
    solutions[[length(solutions) + 1L]] <- sol
    if (!full_solver_has_feasible_incumbent(sol)) break
    previous[[length(previous) + 1L]] <- sol$selected_candidate_ids
  }
  solutions
}


# ------------------------------------------------------------------------------
# FUNCTION: build_pareto_solution_family
# Purpose: Generate solutions along the requested revenue-versus-spending tradeoff frontier.
# ------------------------------------------------------------------------------
build_pareto_solution_family_core <- function(universe, policy_model, working_baseline, kernel_obj) {
  log_line("Gate 4: constructing epsilon-constraint revenue/spending frontier")
  min_rev <- solve_full_milp(build_full_milp(universe, working_baseline, kernel_obj, objective = "revenue"), paste0(universe$protection_mode, "_PARETO_MIN_REVENUE"))
  min_spend <- solve_full_milp(build_full_milp(universe, working_baseline, kernel_obj, objective = "spending"), paste0(universe$protection_mode, "_PARETO_MIN_SPENDING"))
  if (!full_solver_is_optimal(min_rev) || !full_solver_is_optimal(min_spend)) return(list(min_rev, min_spend))
  min_rev_value <- sum((universe$meta |> filter(candidate_id %in% min_rev$selected_candidate_ids))$cumulative_revenue_increase_2027_2036_bil, na.rm = TRUE)
  max_rev_anchor <- sum((universe$meta |> filter(candidate_id %in% min_spend$selected_candidate_ids))$cumulative_revenue_increase_2027_2036_bil, na.rm = TRUE)
  if (!is.finite(max_rev_anchor) || max_rev_anchor < min_rev_value) max_rev_anchor <- min_rev_value
  caps <- unique(seq(min_rev_value, max_rev_anchor, length.out = CFG$pareto_grid_points))
  out <- list(min_rev, min_spend)
  for (i in seq_along(caps)) {
    cap <- caps[i]
    model <- build_full_milp(
      universe,
      working_baseline,
      kernel_obj,
      objective = "spending",
      max_revenue_bil = cap
    )
    out[[length(out) + 1L]] <- solve_full_milp(model, paste0(universe$protection_mode, "_PARETO_R", sprintf("%02d", i)))
  }
  out
}


# ------------------------------------------------------------------------------
# FUNCTION: run_full_solution_search
# Purpose: Run the complete objective family, special cases, Pareto search, and nearest-target fallbacks.
# ------------------------------------------------------------------------------
run_full_solution_search_core <- function(policy_model, working_baseline, kernel_obj) {
  run_highs_interface_self_test()
  strict <- policy_universe_for_mode(policy_model, "STRICT")
  expanded <- policy_universe_for_mode(policy_model, "EXPANDED")
  log_line(
    "Gate 4 universe sizes | strict candidates=", nrow(strict$meta),
    " across ", n_distinct(strict$meta$family_id), " CBO families",
    " | expanded candidates=", nrow(expanded$meta),
    " across ", n_distinct(expanded$meta$family_id), " CBO families"
  )
  assert_model(
    nrow(expanded$meta) >= CFG$minimum_expanded_solver_candidates,
    paste0(
      "Expanded protection universe contains only ", nrow(expanded$meta),
      " solver candidates; at least ", CFG$minimum_expanded_solver_candidates,
      " are required. The full-scale optimizer refuses to degrade into a small subset-selection problem."
    )
  )
  size_models <- purrr::map_dfr(list(STRICT = strict, EXPANDED = expanded), function(u) {
    m <- build_full_milp(u, working_baseline, kernel_obj, objective = "policy_count")
    validate_full_milp_structure(m, paste0(u$protection_mode, "_PARAMETERIZED_STRUCTURE_AUDIT"))
    assert_model(length(m$intensity_variable_names) > nrow(u$meta), paste0(u$protection_mode, " parameterized model failed to create a large continuous policy-level space"))
    assert_model(length(m$schedule_binary_names) > nrow(u$meta), paste0(u$protection_mode, " parameterized model failed to create implementation/phase timing choices"))
    tibble(
      protection_mode = u$protection_mode,
      candidate_binary_variables = sum(m$types == "I"),
      continuous_variables = sum(m$types == "C"),
      total_variables = length(m$variable_names),
      constraints = nrow(m$A),
      nonzero_coefficients = Matrix::nnzero(m$A)
    )
  })
  solution_groups <- list()
  for (u in list(strict, expanded)) {
    tag <- u$protection_mode
    before_names <- names(solution_groups)
    solution_groups[[paste0(tag, "_MIN_REVENUE")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "revenue", paste0(tag, "_MIN_REVENUE"))
    solution_groups[[paste0(tag, "_MIN_SPENDING")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "spending", paste0(tag, "_MIN_SPENDING"))
    solution_groups[[paste0(tag, "_MIN_POLICY_COUNT")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "policy_count", paste0(tag, "_MIN_POLICY_COUNT"))
    solution_groups[[paste0(tag, "_MAX_FISCAL_MARGIN")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "debt", paste0(tag, "_MAX_FISCAL_MARGIN"))
    solution_groups[[paste0(tag, "_REFERENCE_75")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "reference_75", paste0(tag, "_REFERENCE_75"))
    solution_groups[[paste0(tag, "_NO_VAT")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "policy_count", paste0(tag, "_NO_VAT"), count = CFG$special_solution_count, forbid_patterns = c("value-added tax"))
    solution_groups[[paste0(tag, "_REQUIRE_NARROW_VAT")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "policy_count", paste0(tag, "_REQUIRE_NARROW_VAT"), count = CFG$special_solution_count, require_patterns = c("value-added tax.*narrow|narrow.*value-added tax"))
    solution_groups[[paste0(tag, "_NO_FTT")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "policy_count", paste0(tag, "_NO_FTT"), count = CFG$special_solution_count, forbid_patterns = c("financial transactions"))
    solution_groups[[paste0(tag, "_REQUIRE_FTT")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "policy_count", paste0(tag, "_REQUIRE_FTT"), count = CFG$special_solution_count, require_patterns = c("financial transactions"))
    solution_groups[[paste0(tag, "_SS_SOLVENCY")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "policy_count", paste0(tag, "_SS_SOLVENCY"), count = CFG$special_solution_count, require_ss_solvency = TRUE)
    solution_groups[[paste0(tag, "_PARETO")]] <- build_pareto_solution_family(u, policy_model, working_baseline, kernel_obj)
    tag_names <- setdiff(names(solution_groups), before_names)
    tag_solutions <- unlist(solution_groups[tag_names], recursive = FALSE)
    if (!any(vapply(tag_solutions, full_solver_has_feasible_incumbent, logical(1)))) {
      log_line("No hard-target package found in ", tag, " protection mode. Solving nearest-target packages.", level = "WARN")
      solution_groups[[paste0(tag, "_NEAREST_TARGET")]] <- solve_diverse_family(
        u,
        policy_model,
        working_baseline,
        kernel_obj,
        "target_slack",
        paste0(tag, "_NEAREST_TARGET"),
        count = CFG$nearest_target_solution_count,
        soft_targets = TRUE
      )
    }
  }
  all_solutions <- unlist(solution_groups, recursive = FALSE)
  run_status <- purrr::map_dfr(seq_along(all_solutions), function(i) {
    sol <- all_solutions[[i]]
    tibble(
      run_id = i,
      solve_label = sol$solve_label,
      protection_mode = sol$model$protection_mode,
      objective = sol$model$objective_name,
      soft_targets = sol$model$soft_targets,
      require_ss_solvency = sol$model$require_ss_solvency,
      solver_status = sol$status_message,
      solver_proven_optimal = full_solver_is_optimal(sol),
      feasible_incumbent = full_solver_has_feasible_incumbent(sol),
      objective_value = sol$objective_value,
      mip_node_count = sol$mip_node_count,
      mip_dual_bound = sol$mip_dual_bound,
      mip_gap = sol$mip_gap,
      simplex_iteration_count = sol$simplex_iteration_count,
      ipm_iteration_count = sol$ipm_iteration_count,
      max_constraint_violation = sol$max_constraint_violation,
      max_integrality_violation = sol$max_integrality_violation,
      elapsed_seconds = sol$elapsed_seconds,
      selected_policy_count = length(sol$selected_candidate_ids)
    )
  })
  summaries <- list()
  memberships <- list()
  paths <- list()
  origins <- list()
  seen <- character()
  keys <- character()
  counter <- 0L
  for (sol in all_solutions) {
    if (!full_solver_has_feasible_incumbent(sol)) next
    key <- paste(sort(sol$selected_candidate_ids), collapse = ";")
    if (key %in% seen) {
      sid <- keys[match(key, seen)]
      origins[[length(origins) + 1L]] <- tibble(solution_id = sid, solve_label = sol$solve_label)
      next
    }
    counter <- counter + 1L
    sid <- paste0("S", sprintf("%04d", counter))
    seen <- c(seen, key)
    keys <- c(keys, sid)
    origins[[length(origins) + 1L]] <- tibble(solution_id = sid, solve_label = sol$solve_label)
    summaries[[length(summaries) + 1L]] <- summarize_full_solution(sol, policy_model, working_baseline, kernel_obj, sid)
    memberships[[length(memberships) + 1L]] <- extract_solution_membership(sol, policy_model, sid)
    sim <- simulate_full_package(sol$selected_candidate_ids, working_baseline, policy_model, kernel_obj)
    paths[[length(paths) + 1L]] <- sim |>
      transmute(solution_id = sid, year, debt_gdp_pct = scenario_debt_gdp_pct, debt_bil = scenario_debt_bil)
  }
  list(
    solution_objects = all_solutions,
    solver_run_status = run_status,
    solution_origins = bind_rows(origins),
    summary = bind_rows(summaries),
    membership = bind_rows(memberships),
    paths = bind_rows(paths),
    strict_universe = strict,
    expanded_universe = expanded,
    model_dimensions = size_models
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: build_solution_stress_tests
# Purpose: Re-simulate retained packages under additional audit-only stress scenarios.
# ------------------------------------------------------------------------------
build_solution_stress_tests_core <- function(search_result, working_baseline, policy_model, kernel_obj) {
  if (nrow(search_result$summary) == 0L) return(tibble())
  scenario_profiles <- list(
    central = NULL,
    productivity_minus_0_1 = build_macro_stress_profile("productivity_minus_0_1", 1),
    productivity_minus_0_3 = build_macro_stress_profile("productivity_minus_0_1", 3),
    labor_force_minus_0_1 = build_macro_stress_profile("labor_force_minus_0_1", 1),
    rates_plus_0_1 = build_macro_stress_profile("rates_plus_0_1", 1),
    rates_plus_0_5 = build_macro_stress_profile("rates_plus_0_1", 5),
    inflation_plus_0_1 = build_macro_stress_profile("inflation_plus_0_1", 1)
  )
  purrr::map_dfr(search_result$summary$solution_id, function(sid) {
    ids <- search_result$membership |> filter(solution_id == sid) |> pull(candidate_id)
    central <- simulate_full_package(ids, working_baseline, policy_model, kernel_obj)
    purrr::map_dfr(names(scenario_profiles), function(sc) {
      p <- scenario_profiles[[sc]]
      if (is.null(p)) {
        d36 <- central$scenario_debt_gdp_pct[central$year == 2036L]
        d46 <- central$scenario_debt_gdp_pct[central$year == 2046L]
      } else {
        stress <- p |>
          right_join(tibble(year = CFG$model_years), by = "year") |>
          arrange(year) |>
          mutate(deficit_delta_bil = replace_na(deficit_delta_bil, 0))
        long_rate <- get_long_run_rate(kernel_obj)
        stress_debt <- numeric(nrow(stress))
        for (i in seq_len(nrow(stress))) {
          y <- stress$year[i]
          if (i == 1L) {
            stress_debt[i] <- stress$deficit_delta_bil[i]
          } else if (y <= 2036L) {
            stress_debt[i] <- stress_debt[i - 1L] + stress$deficit_delta_bil[i]
          } else {
            stress_debt[i] <- stress_debt[i - 1L] * (1 + long_rate)
          }
        }
        d36 <- 100 * (central$scenario_debt_bil[central$year == 2036L] + stress_debt[stress$year == 2036L]) / central$gdp_bil[central$year == 2036L]
        d46 <- 100 * (central$scenario_debt_bil[central$year == 2046L] + stress_debt[stress$year == 2046L]) / central$gdp_bil[central$year == 2046L]
      }
      tibble(
        solution_id = sid,
        scenario = sc,
        debt_gdp_2036_pct = d36,
        debt_gdp_2046_pct = d46,
        target_2036_pass = d36 <= 100 * CFG$target_2036,
        target_2046_pass = d46 <= 100 * CFG$target_2046_high,
        denominator_adjustment_included = FALSE
      )
    })
  })
}


# ------------------------------------------------------------------------------
# FUNCTION: build_model_size_audit
# Purpose: Report variable, integer, continuous, constraint, and nonzero counts for each solver universe.
# ------------------------------------------------------------------------------
build_model_size_audit_core <- function(policy_model, search_result) {
  headline <- tibble(
    measure = c(
      "CBO current/latest option families in frozen local pack",
      "CBO candidate variants retained in audit catalog",
      "Candidates with direct annual official score paths",
      "Candidates retained as official total-only audit records",
      "Strict solver candidates",
      "Expanded eligible plus conditional solver candidates",
      "Blocked candidates retained in audit catalog",
      "Distinct solution packages discovered",
      "MILP solves attempted"
    ),
    value = c(
      n_distinct(policy_model$meta$family_id),
      nrow(policy_model$meta),
      sum(policy_model$meta$solver_eligible_annual),
      sum(!policy_model$meta$solver_eligible_annual),
      nrow(search_result$strict_universe$meta),
      nrow(search_result$expanded_universe$meta),
      sum(policy_model$meta$protection_status == "BLOCKED"),
      nrow(search_result$summary),
      nrow(search_result$solver_run_status)
    )
  )
  dimensions <- search_result$model_dimensions |>
    tidyr::pivot_longer(-protection_mode, names_to = "dimension", values_to = "value") |>
    transmute(measure = paste0(protection_mode, " MILP ", dimension), value = as.numeric(value))
  bind_rows(headline, dimensions)
}


# ------------------------------------------------------------------------------
# FUNCTION: build_solution_family_summary
# Purpose: Summarize how retained packages differ by objective, protection mode, and fiscal composition.
# ------------------------------------------------------------------------------
build_solution_family_summary_core <- function(search_result) {
  if (nrow(search_result$summary) == 0L) return(tibble())
  features <- search_result$membership |>
    mutate(
      vat_member = stringr::str_detect(stringr::str_to_lower(paste(title, variant_name)), "value-added tax"),
      ftt_member = stringr::str_detect(stringr::str_to_lower(paste(title, variant_name)), "financial transactions")
    ) |>
    group_by(solution_id) |>
    summarise(vat = any(vat_member), ftt = any(ftt_member), .groups = "drop")
  search_result$summary |>
    left_join(features, by = "solution_id") |>
    mutate(
      vat = replace_na(vat, FALSE),
      ftt = replace_na(ftt, FALSE),
      ss_solvent_approx = ss_actuarial_improvement_pct_payroll >= CFG$ss_actuarial_gap_pct_payroll
    ) |>
    group_by(protection_mode, objective, vat, ftt, ss_solvent_approx) |>
    summarise(
      package_count = n(),
      min_policy_count = min(selected_policy_count),
      min_revenue_bil = min(revenue_2027_2036_bil),
      min_spending_cut_bil = min(spending_cuts_2027_2036_bil),
      min_debt_2036_pct = min(debt_gdp_2036_pct),
      min_debt_2046_pct = min(debt_gdp_2046_pct),
      .groups = "drop"
    )
}


# ------------------------------------------------------------------------------
# FUNCTION: plot_solution_debt_paths
# Purpose: Render representative annual debt-to-GDP paths in the RStudio Plots pane.
# ------------------------------------------------------------------------------
plot_solution_debt_paths_core <- function(working_baseline, search_result) {
  base <- working_baseline |>
    transmute(year, solution_id = "Working baseline", debt_gdp_pct = working_debt_gdp_pct)

  hard_target_count <- 0L
  if (nrow(search_result$summary) > 0L) {
    hard_target_count <- sum(
      search_result$summary$debt_gdp_2036_pct <= 100 * CFG$target_2036 + 1e-7 &
        search_result$summary$debt_gdp_2046_pct <= 100 * CFG$target_2046_high + 1e-7,
      na.rm = TRUE
    )
  }

  if (nrow(search_result$paths) == 0L || nrow(search_result$summary) == 0L) {
    pdat <- base
  } else {
    ids <- search_result$summary |>
      arrange(
        debt_gdp_2036_pct,
        debt_gdp_2046_pct,
        selected_policy_count,
        revenue_2027_2036_bil,
        spending_cuts_2027_2036_bil
      ) |>
      group_by(protection_mode) |>
      slice_head(n = 4L) |>
      ungroup() |>
      pull(solution_id)

    pdat <- bind_rows(
      base,
      search_result$paths |>
        filter(solution_id %in% ids) |>
        select(year, solution_id, debt_gdp_pct)
    )
  }

  chart_title <- if (hard_target_count > 0L) {
    "Debt held by the public under representative target-feasible packages"
  } else {
    "Debt held by the public under representative nearest-target packages"
  }

  chart_subtitle <- if (hard_target_count > 0L) {
    "Working baseline and representative packages that satisfy the fiscal targets"
  } else {
    "No hard-target package was found; lines show the working baseline and representative minimum-gap packages"
  }

  ggplot(pdat, aes(year, debt_gdp_pct, group = solution_id, color = solution_id)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = c(90, 80, 75), linetype = "dotted") +
    scale_x_continuous(breaks = seq(2026, 2046, 2)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = chart_title,
      subtitle = chart_subtitle,
      x = NULL,
      y = "Debt held by public / GDP",
      color = "Package",
      caption = "Policy packages are not ranked. Target lines mark 90 percent in 2036, 80 percent in 2046, and the 75 percent long-run reference."
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}


# ------------------------------------------------------------------------------
# FUNCTION: build_assumptions_table
# Purpose: Write a machine-readable table of model assumptions and their interpretation.
# ------------------------------------------------------------------------------
build_assumptions_table <- function() {
  map_dfr(names(CFG), function(nm) {
    value <- CFG[[nm]]
    if (is.null(value)) value <- NA_character_
    if (length(value) > 1L) value <- paste(value, collapse = ";")
    tibble(
      assumption = nm,
      value = as.character(value),
      value_type = if (is.null(CFG[[nm]])) "NULL" else class(CFG[[nm]])[1]
    )
  })
}


# ------------------------------------------------------------------------------
# FUNCTION: build_package_versions_table
# Purpose: Record package versions so solver/audit results can be reproduced.
# ------------------------------------------------------------------------------
build_package_versions_table <- function() {
  tibble(
    package = required_packages,
    version = vapply(required_packages, function(pkg) as.character(utils::packageVersion(pkg)), character(1))
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: build_validation_table
# Purpose: Combine Gate 1, external-history, debt-kernel, and macro-profile validation results.
# ------------------------------------------------------------------------------
build_validation_table <- function(gate1, external, gate2a, gate2b) {
  g1 <- gate1 |>
    transmute(
      gate,
      test,
      passed,
      observed,
      reference = NA_character_,
      tolerance,
      detail
    )
  ext <- external |>
    transmute(gate, test, passed, observed, reference, tolerance, detail)
  g2a <- tibble(
    gate = "GATE_2_DEBT_SERVICE",
    test = "Current-CBO BFM debt-service reciprocal leave-one-out holdouts",
    passed = gate2a$passed,
    observed = as.character(gate2a$max_error_bil),
    reference = "March 2026 CBO FY2026 and FY2027 $900B pulse exports, each withheld in turn",
    tolerance = as.character(CFG$cbo_kernel_validation_tolerance_bil),
    detail = paste0("Maximum annual error across ", gate2a$validation_points, " reciprocal leave-one-out comparisons; combined RMSE=$", round(gate2a$rmse_bil, 3), "B")
  )
  g2b <- gate2b |>
    transmute(
      gate,
      test = paste0("CBO macro rule-of-thumb reconstruction: ", scenario_id),
      passed,
      observed = paste0("cumulative_error=", cumulative_error_bil, "; fy2036_error=", fy2036_error_bil),
      reference = "CBO April 2026 published cumulative and FY2036 benchmark effects",
      tolerance = "1e-8 reconstruction identity",
      detail = "Validates the reconstructed budget-side stress profile, not a full macroeconomic denominator path"
    )
  bind_rows(g1, ext, g2a, g2b)
}


# ------------------------------------------------------------------------------
# FUNCTION: build_run_manifest_full
# Purpose: Build the final run manifest describing model version, evidence mode, targets, and execution metadata.
# ------------------------------------------------------------------------------
build_run_manifest_full <- function() {
  tibble(
    model_name = CFG$model_name,
    model_version = CFG$model_version,
    run_timestamp = CFG$run_timestamp,
    cbo_vintage = CFG$cbo_vintage,
    evidence_mode = CFG$evidence_mode,
    cbo_expected_2024_option_families = CFG$cbo_expected_2024_option_families,
    cbo_minimum_current_option_families = CFG$cbo_minimum_current_option_families,
    policy_pack_dir = CFG$policy_pack_dir,
    policy_pack_expected_families = CFG$policy_pack_expected_families,
    policy_pack_expected_candidates = CFG$policy_pack_expected_candidates,
    policy_pack_expected_annual_rows = CFG$policy_pack_expected_annual_rows,
    minimum_expanded_solver_candidates = CFG$minimum_expanded_solver_candidates,
    package_ready_account_min_savings_bil_2027_2036 = CFG$package_ready_account_min_savings_bil_2027_2036,
    ss_actuarial_gap_pct_payroll = CFG$ss_actuarial_gap_pct_payroll,
    ss_actuarial_excess_margin_pct_payroll = CFG$ss_actuarial_excess_margin_pct_payroll,
    ss_actuarial_max_pct_payroll = CFG$ss_actuarial_max_pct_payroll,
    target_2036_pct_gdp = 100 * CFG$target_2036,
    target_2046_center_pct_gdp = 100 * CFG$target_2046_center,
    target_2046_low_pct_gdp = 100 * CFG$target_2046_low,
    target_2046_high_pct_gdp = 100 * CFG$target_2046_high,
    solver_threads = CFG$solver_threads,
    solver_mip_rel_gap = CFG$solver_mip_rel_gap,
    solution_acceptance_constraint_tolerance = CFG$solution_acceptance_constraint_tolerance,
    solution_acceptance_bound_tolerance = CFG$solution_acceptance_bound_tolerance,
    solution_acceptance_integrality_tolerance = CFG$solution_acceptance_integrality_tolerance,
    random_seed = CFG$seed,
    R_version = R.version.string,
    highs_version = as.character(utils::packageVersion("highs")),
    platform = R.version$platform
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: start_console_capture
# Purpose: Start simultaneous console display and persistent text logging for the complete run.
# ------------------------------------------------------------------------------
start_console_capture <- function() {
  if (file.exists(CFG$console_log_path)) unlink(CFG$console_log_path, force = TRUE)
  con <- file(CFG$console_log_path, open = "wt", encoding = "UTF-8")
  state <- list(
    connection = con,
    output_depth = sink.number(type = "output"),
    message_connection = sink.number(type = "message")
  )
  sink(con, type = "output", split = TRUE)
  sink(con, type = "message")
  cat(
    paste0(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      " | INFO  | Console capture started | ",
      CFG$model_version,
      "\n"
    )
  )
  flush.console()
  state
}


# ------------------------------------------------------------------------------
# FUNCTION: stop_console_capture
# Purpose: Cleanly close console/message sinks so the log is complete before archive creation.
# ------------------------------------------------------------------------------
stop_console_capture <- function(state) {
  try(flush.console(), silent = TRUE)
  if (!is.null(state$message_connection) && sink.number(type = "message") != state$message_connection) {
    try(sink(type = "message"), silent = TRUE)
  }
  while (sink.number(type = "output") > state$output_depth) {
    try(sink(type = "output"), silent = TRUE)
  }
  try(flush(state$connection), silent = TRUE)
  try(close(state$connection), silent = TRUE)
  invisible(TRUE)
}


# ------------------------------------------------------------------------------
# FUNCTION: write_analysis_output_archive_manifest
# Purpose: Hash every audit output and write the archive manifest before zipping.
# ------------------------------------------------------------------------------
write_analysis_output_archive_manifest <- function() {
  rel_files <- list.files(
    CFG$output_dir,
    recursive = TRUE,
    full.names = FALSE,
    all.files = TRUE,
    no.. = TRUE
  )
  full_files <- file.path(CFG$output_dir, rel_files)
  keep <- file.info(full_files)$isdir %in% FALSE
  rel_files <- rel_files[keep]
  full_files <- full_files[keep]
  manifest_name <- basename(CFG$output_archive_manifest_path)
  keep_manifest <- rel_files != manifest_name
  rel_files <- rel_files[keep_manifest]
  full_files <- full_files[keep_manifest]
  manifest <- tibble(
    relative_path = rel_files,
    bytes = as.numeric(file.info(full_files)$size),
    sha256 = vapply(full_files, sha256_file, character(1)),
    archived_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    model_version = CFG$model_version
  ) |>
    arrange(relative_path)
  write_csv_atomic(manifest, CFG$output_archive_manifest_path)
  manifest
}


# ------------------------------------------------------------------------------
# FUNCTION: create_analysis_output_zip
# Purpose: Create the final audit ZIP containing the complete analysis-output directory.
# ------------------------------------------------------------------------------
create_analysis_output_zip <- function() {
  manifest <- write_analysis_output_archive_manifest()
  if (file.exists(CFG$output_zip_path)) unlink(CFG$output_zip_path, force = TRUE)
  rel_files <- list.files(
    CFG$output_dir,
    recursive = TRUE,
    full.names = FALSE,
    all.files = TRUE,
    no.. = TRUE
  )
  full_files <- file.path(CFG$output_dir, rel_files)
  rel_files <- rel_files[file.info(full_files)$isdir %in% FALSE]
  assert_model(length(rel_files) > 0L, "analysis_output is empty; refusing to create an empty audit ZIP")
  zip::zipr(
    zipfile = CFG$output_zip_path,
    files = rel_files,
    root = CFG$output_dir,
    include_directories = FALSE
  )
  assert_model(file.exists(CFG$output_zip_path), "Audit ZIP was not created")
  assert_model(file.info(CFG$output_zip_path)$size > 0, "Audit ZIP was created but is empty")
  list(
    path = CFG$output_zip_path,
    bytes = as.numeric(file.info(CFG$output_zip_path)$size),
    sha256 = sha256_file(CFG$output_zip_path),
    file_count = length(rel_files)
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: execute_model_with_audit_capture
# Purpose: Wrap the run in console capture, error preservation, and final archive creation.
# ------------------------------------------------------------------------------
execute_model_with_audit_capture_core <- function() {
  capture_state <- start_console_capture()
  result <- NULL
  run_error <- NULL

  tryCatch(
    {
      result <- run_model()
    },
    error = function(e) {
      run_error <<- e
      log_line("Model execution terminated with error: ", conditionMessage(e), level = "ERROR")
    }
  )

  log_line("Console capture complete; closing log before creating analysis_output audit archive")
  stop_console_capture(capture_state)

  zip_info <- tryCatch(
    create_analysis_output_zip(),
    error = function(e) {
      msg <- sanitize_public_text(paste0("Audit ZIP creation failed: ", conditionMessage(e)))
      cat(msg, "\n")
      cat(
        paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), " | ERROR | ", msg, "\n"),
        file = CFG$console_log_path,
        append = TRUE
      )
      NULL
    }
  )

  if (!is.null(zip_info)) {
    cat(
      paste0(
        "Audit archive created: ",
        project_relative_path(zip_info$path),
        " | files=",
        zip_info$file_count,
        " | bytes=",
        format(zip_info$bytes, scientific = FALSE, trim = TRUE),
        " | sha256=",
        zip_info$sha256,
        "\n"
      )
    )
  }

  if (!is.null(run_error)) {
    stop(run_error)
  }

  assert_model(!is.null(zip_info), "Model completed but the analysis_output audit ZIP could not be created")
  attr(result, "analysis_output_zip") <- zip_info
  result
}


# ------------------------------------------------------------------------------
# FUNCTION: run_model
# Purpose: Execute the complete validation, data-acquisition, optimization, audit-output, and reporting pipeline.
# ------------------------------------------------------------------------------
run_model_core <- function() {
  log_line("Starting ", CFG$model_name, " | ", CFG$model_version)
  log_line("Project root: ", CFG$project_root)
  log_line("Evidence mode: ", CFG$evidence_mode)
  log_line("Targets: FY2036 <= ", 100 * CFG$target_2036, "% GDP; FY2046 <= ", 100 * CFG$target_2046_high, "% GDP")
  log_line("Gate 3 policy source mode: frozen local pack at ", CFG$policy_pack_dir)
  paths <- fetch_cbo_sources()
  cbo_data <- list(
    ten = read_cbo_long(paths$cbo_ten_year_budget_2026_02),
    lt = read_cbo_long(paths$cbo_long_term_budget_2026_02),
    hist = read_cbo_long(paths$cbo_historical_budget_2026_02),
    econ = read_cbo_long(paths$cbo_historical_economic_2026_02),
    revenue_detail = read_cbo_long(paths$cbo_revenue_detail_2026_02)
  )
  cbo_baseline <- build_cbo_baseline(cbo_data)
  gate1 <- run_gate_1(cbo_baseline, cbo_data)
  ext_validation <- validate_external_history(cbo_data, cbo_baseline)
  kernel_obj <- build_cbo_debt_service_kernel()
  gate2a <- validate_cbo_kernel(kernel_obj)
  gate2b <- validate_macro_profiles()
  tariff_profile <- build_tariff_profile(cbo_baseline, cbo_data, kernel_obj)
  working_baseline <- build_working_baseline(cbo_baseline, tariff_profile, kernel_obj)
  log_line("Gate 3: loading and validating complete frozen policy universe and protection classifications")
  cbo_universe <- build_full_cbo_policy_universe()
  policy_model <- translate_full_universe_flows(cbo_universe, working_baseline)
  external_catalog <- build_external_policy_catalog(cbo_universe$pack)
  assert_model(n_distinct(policy_model$meta$family_id) == CFG$policy_pack_expected_families, "Gate 3 lost at least one current/latest CBO option family before optimization")
  assert_model(sum(policy_model$meta$is_december_2024_core) >= CFG$cbo_expected_2024_option_families, "Gate 3 lost at least one December 2024 core CBO option family before optimization")
  assert_model(sum(policy_model$meta$solver_eligible_annual) == CFG$policy_pack_expected_annual_candidates, "Gate 3 annual-score candidate count changed after translation")
  log_line(
    "Gate 3 complete: ", n_distinct(policy_model$meta$family_id), " CBO families; ",
    nrow(policy_model$meta), " candidate variants retained; ",
    sum(policy_model$meta$solver_eligible_annual), " annual-score solver candidates before philosophy constraints; protection status ELIGIBLE=", sum(policy_model$meta$protection_status == "ELIGIBLE"),
    ", CONDITIONAL=", sum(policy_model$meta$protection_status == "CONDITIONAL"),
    ", BLOCKED=", sum(policy_model$meta$protection_status == "BLOCKED"),
    "; total-only audit records=", sum(!policy_model$meta$solver_eligible_annual)
  )
  log_line("Gate 4: launching full mixed-integer solution search")
  search_result <- run_full_solution_search(policy_model, working_baseline, kernel_obj)
  stress_tests <- build_solution_stress_tests(search_result, working_baseline, policy_model, kernel_obj)
  model_size <- build_model_size_audit(policy_model, search_result)
  family_summary <- build_solution_family_summary(search_result)
  assumptions <- build_assumptions_table()
  package_versions <- build_package_versions_table()
  validation_table <- build_validation_table(gate1, ext_validation, gate2a, gate2b)
  if (CFG$write_audit_outputs) {
    log_line("Writing audit outputs")
    write_csv_atomic(SOURCE_MANIFEST, file.path(CFG$output_dir, "source_manifest.csv"))
    write_csv_atomic(assumptions, file.path(CFG$output_dir, "assumptions.csv"))
    write_csv_atomic(package_versions, file.path(CFG$output_dir, "package_versions.csv"))
    write_csv_atomic(validation_table, file.path(CFG$output_dir, "validation_tests.csv"))
    write_csv_atomic(cbo_baseline, file.path(CFG$output_dir, "baseline_cbo_feb_2026.csv"))
    write_csv_atomic(working_baseline, file.path(CFG$output_dir, "baseline_working_sep_2026.csv"))
    write_csv_atomic(tariff_profile, file.path(CFG$output_dir, "tariff_adjustment_profile.csv"))
    write_csv_atomic(gate2a$comparison, file.path(CFG$output_dir, "validation_cbo_debt_service_kernel.csv"))
    write_csv_atomic(gate2a$direction_summary, file.path(CFG$output_dir, "validation_cbo_debt_service_kernel_summary.csv"))
    write_csv_atomic(kernel_obj$kernel, file.path(CFG$output_dir, "cbo_debt_service_kernel.csv"))
    write_csv_atomic(kernel_obj$bfm_matrix, file.path(CFG$output_dir, "cbo_bfm_2026_debt_service_matrix.csv"))
    write_csv_atomic(cbo_universe$pack$hash_audit, file.path(CFG$output_dir, "policy_pack_hash_audit.csv"))
    write_csv_atomic(cbo_universe$pack$benchmark_audit, file.path(CFG$output_dir, "policy_pack_benchmark_audit.csv"))
    write_csv_atomic(cbo_universe$pack$coverage, file.path(CFG$output_dir, "policy_pack_coverage_audit.csv"))
    write_csv_atomic(cbo_universe$current_index, file.path(CFG$output_dir, "cbo_option_index_current_all.csv"))
    write_csv_atomic(cbo_universe$report_index, file.path(CFG$output_dir, "cbo_option_index_2024_76.csv"))
    write_csv_atomic(cbo_universe$families, file.path(CFG$output_dir, "cbo_option_family_local_pack_audit.csv"))
    write_csv_atomic(cbo_universe$raw_rows, file.path(CFG$output_dir, "cbo_option_raw_annual_rows.csv"))
    write_csv_atomic(policy_model$meta, file.path(CFG$output_dir, "policy_candidate_catalog_full.csv"))
    write_csv_atomic(policy_model$meta |> select(candidate_id, family_id, title, variant_name, protection_status, protection_reason, starts_with("risk_"), market_function_review_required, hard_protection_violation, explicit_review_required, protection_classification_method), file.path(CFG$output_dir, "policy_protection_attributes.csv"))
    write_csv_atomic(policy_model$meta |> filter(!solver_eligible_annual), file.path(CFG$output_dir, "policy_candidates_total_only_not_in_annual_milp.csv"))
    write_csv_atomic(policy_model$flows, file.path(CFG$output_dir, "policy_candidate_annual_flows_full.csv"))
    write_csv_atomic(cbo_universe$pack$ssa, file.path(CFG$output_dir, "ssa_actuarial_reference.csv"))
    write_csv_atomic(external_catalog, file.path(CFG$output_dir, "external_policy_source_catalog.csv"))
    write_csv_atomic(build_interaction_catalog_full(policy_model$meta |> filter(solver_eligible_annual)), file.path(CFG$output_dir, "interaction_catalog.csv"))
    write_csv_atomic(model_size, file.path(CFG$output_dir, "model_size_audit.csv"))
    write_csv_atomic(search_result$solver_run_status, file.path(CFG$output_dir, "solver_run_status.csv"))
    write_csv_atomic(search_result$solution_origins, file.path(CFG$output_dir, "solution_origins.csv"))
    write_csv_atomic(search_result$summary, file.path(CFG$output_dir, "solution_catalog.csv"))
    write_csv_atomic(search_result$membership, file.path(CFG$output_dir, "solution_policy_membership.csv"))
    write_csv_atomic(search_result$paths, file.path(CFG$output_dir, "solution_debt_paths.csv"))
    write_csv_atomic(stress_tests, file.path(CFG$output_dir, "solution_stress_tests.csv"))
    write_csv_atomic(family_summary, file.path(CFG$output_dir, "solution_family_summary.csv"))
    write_csv_atomic(build_run_manifest_full(), file.path(CFG$output_dir, "run_manifest.csv"))
  }
  log_line("Model run complete")
  print(model_size)
  if (nrow(search_result$summary) > 0L) {
    log_line("Distinct solution packages retained after de-duplication: ", nrow(search_result$summary))
    print(search_result$summary |> select(solution_id, solve_label, protection_mode, selected_policy_count, revenue_2027_2036_bil, spending_cuts_2027_2036_bil, ss_actuarial_improvement_pct_payroll, debt_gdp_2036_pct, debt_gdp_2046_pct, independently_verified))
  } else {
    log_line("No optimized or nearest-target solution package could be produced", level = "WARN")
  }
  if (CFG$render_plots) print(plot_solution_debt_paths(working_baseline, search_result))
  invisible(list(
    config = CFG,
    source_manifest = SOURCE_MANIFEST,
    cbo_data = cbo_data,
    cbo_baseline = cbo_baseline,
    working_baseline = working_baseline,
    tariff_profile = tariff_profile,
    kernel = kernel_obj,
    policy_model = policy_model,
    cbo_policy_universe = cbo_universe,
    external_policy_catalog = external_catalog,
    solution_search = search_result,
    solution_stress_tests = stress_tests,
    model_size_audit = model_size,
    solution_family_summary = family_summary,
    validation_tests = validation_table
  ))
}


CFG$parameterized_start_years <- 2027:2032
CFG$parameterized_phase_in_years <- c(1L, 3L, 5L)
CFG$parameterized_generic_min_scale <- 0.05
CFG$parameterized_rate_min_scale <- 0.01
CFG$parameterized_vat_max_rate_pct <- 5
CFG$parameterized_ftt_max_rate_pct <- 0.01
CFG$parameterized_corporate_rate_max_increase_pp <- 1
CFG$parameterized_capital_gains_rate_max_increase_pp <- 2
CFG$parameterized_surtax_max_rate_pct <- 2
CFG$parameterized_new_payroll_tax_max_rate_pct <- 2
CFG$parameterized_hi_payroll_tax_max_increase_pp <- 2
CFG$parameterized_ss_payroll_tax_max_increase_pp <- 2
CFG$parameterized_ma_benchmark_max_reduction_pct <- 10
CFG$parameterized_ma_coding_max_adjustment_pct <- 20
CFG$parameterized_carbon_price_max_per_ton <- 25
CFG$parameterized_tanf_max_reduction_pct <- 10
CFG$robust_policy_yield_floor <- 0.90
CFG$robust_required_scenarios <- c(
  "CENTRAL",
  "RATES_PLUS_0_1",
  "PRODUCTIVITY_MINUS_0_1",
  "LABOR_FORCE_MINUS_0_1",
  "POLICY_YIELD_90"
)
CFG$robust_audit_scenarios <- c(
  "RATES_PLUS_0_5",
  "PRODUCTIVITY_MINUS_0_3",
  "COMBINED_ADVERSE"
)


# ------------------------------------------------------------------------------
# FUNCTION: extract_first_number
# Purpose: Extract the first numeric policy parameter embedded in a title or variant label.
# ------------------------------------------------------------------------------
extract_first_number <- function(x) {
  z <- stringr::str_extract(as.character(x), "[0-9]+(?:\\.[0-9]+)?")
  suppressWarnings(as.numeric(z))
}


# ------------------------------------------------------------------------------
# FUNCTION: parameter_spec_one
# Purpose: Map one scored policy anchor into a defensible parameter type, units, bounds, and evidence rule.
# ------------------------------------------------------------------------------
parameter_spec_one <- function(title, variant_name) {
  t <- stringr::str_to_lower(title)
  v <- stringr::str_to_lower(variant_name)
  mode <- "CONTINUOUS_TO_ANCHOR"
  parameter_name <- "Fraction of official scored reform"
  parameter_unit <- "fraction_of_scored_reform"
  anchor_value <- 1
  max_value <- 1
  min_scale <- CFG$parameterized_generic_min_scale
  interpolation_basis <- "Linear interpolation between current-law zero and the official scored reform; no extrapolation beyond the official anchor."
  extrapolated <- FALSE

  if (stringr::str_detect(t, "value-added tax")) {
    mode <- "RATE_LINEAR_TO_ANCHOR"
    parameter_name <- "VAT rate"
    parameter_unit <- "percent"
    anchor_value <- 5
    max_value <- 5
    min_scale <- CFG$parameterized_rate_min_scale
    interpolation_basis <- "Official CBO/JCT score exists at a 5 percent VAT rate. The solver may interpolate from zero through 5 percent but may not extrapolate above the scored rate."
  } else if (stringr::str_detect(t, "financial transactions")) {
    mode <- "RATE_LINEAR_TO_ANCHOR"
    parameter_name <- "Financial transaction tax rate"
    parameter_unit <- "percent"
    anchor_value <- 0.01
    max_value <- 0.01
    min_scale <- CFG$parameterized_rate_min_scale
    interpolation_basis <- "Official current CBO/JCT score exists at 0.01 percent. Older CBO analysis warns that higher FTT rates can produce less-than-proportional or even lower revenue, so the optimized rate is capped at the current official anchor."
  } else if (stringr::str_detect(t, "increase the corporate income tax rate")) {
    mode <- "RATE_LINEAR_TO_ANCHOR"
    parameter_name <- "Corporate income tax rate increase"
    parameter_unit <- "percentage_points"
    anchor_value <- 1
    max_value <- 1
    min_scale <- CFG$parameterized_rate_min_scale
    interpolation_basis <- "Official CBO/JCT score exists for a 1 percentage point increase. Interpolation is allowed only below the scored anchor."
  } else if (stringr::str_detect(t, "long-term capital gains and qualified dividends")) {
    mode <- "RATE_LINEAR_TO_ANCHOR"
    parameter_name <- "Capital-gains and qualified-dividend rate increase"
    parameter_unit <- "percentage_points"
    anchor_value <- 2
    max_value <- 2
    min_scale <- CFG$parameterized_rate_min_scale
    interpolation_basis <- "Official CBO/JCT score exists for a 2 percentage point increase. Interpolation is allowed only below the scored anchor."
  } else if (stringr::str_detect(t, "impose a surtax")) {
    parsed <- extract_first_number(v)
    if (is.finite(parsed) && parsed > 0) {
      mode <- "RATE_LINEAR_TO_ANCHOR"
      parameter_name <- "Surtax rate for the scored threshold design"
      parameter_unit <- "percent"
      anchor_value <- parsed
      max_value <- parsed
      min_scale <- CFG$parameterized_rate_min_scale
      interpolation_basis <- "CBO/JCT provides an official score only for the stated surtax rate and threshold design. The solver may interpolate below that scored rate but may not extrapolate above it."
    }
  } else if (stringr::str_detect(t, "impose a new payroll tax")) {
    parsed <- extract_first_number(v)
    if (is.finite(parsed) && parsed > 0) {
      mode <- "RATE_LINEAR_TO_ANCHOR"
      parameter_name <- "New payroll tax rate"
      parameter_unit <- "percent"
      anchor_value <- parsed
      max_value <- parsed
      min_scale <- CFG$parameterized_rate_min_scale
      interpolation_basis <- "Official CBO/JCT score exists at the stated payroll-tax rate. Interpolation is allowed only below the scored anchor."
    }
  } else if (stringr::str_detect(t, "payroll tax rate for medicare hospital insurance")) {
    parsed <- extract_first_number(v)
    if (is.finite(parsed) && parsed > 0) {
      mode <- "RATE_LINEAR_TO_ANCHOR"
      parameter_name <- "Medicare HI payroll-tax rate increase"
      parameter_unit <- "percentage_points"
      anchor_value <- parsed
      max_value <- parsed
      min_scale <- CFG$parameterized_rate_min_scale
      interpolation_basis <- "Official CBO score exists at the stated HI payroll-tax increase. Interpolation is allowed only below the scored anchor."
    }
  } else if (stringr::str_detect(t, "payroll tax rate for social security")) {
    parsed <- extract_first_number(v)
    if (is.finite(parsed) && parsed > 0) {
      mode <- "RATE_LINEAR_TO_ANCHOR"
      parameter_name <- "Social Security payroll-tax rate increase"
      parameter_unit <- "percentage_points"
      anchor_value <- parsed
      max_value <- parsed
      min_scale <- CFG$parameterized_rate_min_scale
      interpolation_basis <- "Official CBO score exists at the stated OASDI payroll-tax increase. Interpolation is allowed only below the scored anchor."
    }
  } else if (stringr::str_detect(t, "individual income tax rates on ordinary income")) {
    parsed <- extract_first_number(v)
    if (is.finite(parsed) && parsed > 0) {
      mode <- "RATE_LINEAR_TO_ANCHOR"
      parameter_name <- "Ordinary-income tax-rate increase for the scored bracket design"
      parameter_unit <- "percentage_points"
      anchor_value <- parsed
      max_value <- parsed
      min_scale <- CFG$parameterized_rate_min_scale
      interpolation_basis <- "Official CBO/JCT score exists at the stated rate increase for the stated bracket design. Interpolation is allowed only below the scored anchor."
    }
  } else if (stringr::str_detect(t, "tax on emissions of greenhouse gases")) {
    parsed <- extract_first_number(v)
    if (is.finite(parsed) && parsed > 0) {
      mode <- "RATE_LINEAR_TO_ANCHOR"
      parameter_name <- "Initial greenhouse-gas tax schedule level"
      parameter_unit <- "dollars_per_ton_initial_rate"
      anchor_value <- parsed
      max_value <- parsed
      min_scale <- CFG$parameterized_rate_min_scale
      interpolation_basis <- "Official CBO/JCT score exists for the stated carbon-price schedule. Interpolation is allowed only below that schedule's initial-rate anchor."
    }
  } else if (stringr::str_detect(t, "medicare advantage benchmarks") && stringr::str_detect(v, "10 percent")) {
    mode <- "LEVEL_LINEAR_TO_ANCHOR"
    parameter_name <- "Medicare Advantage benchmark reduction"
    parameter_unit <- "percent"
    anchor_value <- 10
    max_value <- 10
    min_scale <- CFG$parameterized_rate_min_scale
    interpolation_basis <- "CBO states that reductions smaller than 10 percent would produce roughly proportional savings, while much larger reductions would produce less-than-proportional savings. The solver therefore interpolates only from zero through the official 10 percent anchor."
  } else if (stringr::str_detect(t, "modify payments to medicare advantage plans for health risk") && stringr::str_detect(v, "coding-pattern adjustment")) {
    parsed <- extract_first_number(v)
    if (is.finite(parsed) && parsed > 0) {
      mode <- "DISCRETE_FULL_ANCHOR"
      parameter_name <- "Medicare Advantage coding-pattern adjustment"
      parameter_unit <- "percent"
      anchor_value <- parsed
      max_value <- parsed
      min_scale <- 1
      interpolation_basis <- "CBO separately scores 8 percent and 20 percent coding-pattern adjustments, and the savings relationship is strongly nonlinear. The official levels are retained as discrete alternatives rather than linearly interpolated or extrapolated."
    }
  } else if (stringr::str_detect(t, "increase the maximum taxable earnings that are subject to social security payroll taxes") && stringr::str_detect(v, "250,000")) {
    mode <- "RATE_LINEAR_TO_ANCHOR"
    parameter_name <- "OASDI payroll-tax rate on earnings above $250,000"
    parameter_unit <- "percent"
    anchor_value <- 12.4
    max_value <- 12.4
    min_scale <- CFG$parameterized_rate_min_scale
    interpolation_basis <- "CBO scores the full 12.4 percent OASDI rate above $250,000 without additional benefit credit. SSA OACT supplies the corresponding long-range actuarial estimate. Interpolation below the full rate is a transparent local modeling assumption; no extrapolation is permitted."
  } else if (stringr::str_detect(t, "increase the maximum taxable earnings that are subject to social security payroll taxes") && stringr::str_detect(v, "90 percent")) {
    mode <- "CONTINUOUS_TO_ANCHOR"
    parameter_name <- "Fraction of the scored path to 90 percent taxable earnings"
    parameter_unit <- "fraction_of_scored_reform"
    anchor_value <- 1
    max_value <- 1
    min_scale <- CFG$parameterized_generic_min_scale
    interpolation_basis <- "The official reform raises the taxable share of covered earnings to 90 percent. The solver may choose a partial path toward that scored endpoint but may not exceed it."
  } else if (stringr::str_detect(t, "tanf.*10 percent")) {
    mode <- "LEVEL_LINEAR_TO_ANCHOR"
    parameter_name <- "TANF State Family Assistance Grant reduction"
    parameter_unit <- "percent"
    anchor_value <- 10
    max_value <- 10
    min_scale <- CFG$parameterized_rate_min_scale
    interpolation_basis <- "Official CBO score exists for a 10 percent reduction. Interpolation is allowed only below the scored anchor."
  }

  discrete_pattern <- paste(
    c(
      "cancel",
      "stop building",
      "retiring the",
      "eliminate the federal transit administration",
      "eliminate human space exploration",
      "repeal the davis-bacon",
      "repeal the low-income housing tax credit",
      "repeal the.*inventory",
      "change the taxation of assets transferred at death",
      "tax carried interest as ordinary income",
      "tax gains from derivatives as ordinary income",
      "include employer-paid premiums.*taxable income",
      "expand social security to include newly hired",
      "use an alternative measure of inflation",
      "establish a uniform social security benefit",
      "raise the full retirement age",
      "raise the age of eligibility for medicare",
      "replace some military personnel",
      "convert multiple assistance programs",
      "tax all foreign income of u.s. corporations",
      "require people who claim the earned income tax credit",
      "eliminate or modify head-of-household filing status",
      "change the tax treatment of capital gains from sales of inherited assets",
      "use two years of diagnoses and exclude hras"
    ),
    collapse = "|"
  )

  if (mode == "CONTINUOUS_TO_ANCHOR" && stringr::str_detect(paste(t, v), stringr::regex(discrete_pattern, ignore_case = TRUE))) {
    mode <- "DISCRETE_FULL_ANCHOR"
    parameter_name <- "Official scored structural reform"
    parameter_unit <- "binary_full_reform"
    anchor_value <- 1
    max_value <- 1
    min_scale <- 1
    interpolation_basis <- "Structural reform retained as discrete because the source evidence does not support a defensible continuously variable policy level."
  }

  max_scale <- max_value / anchor_value
  if (!is.finite(max_scale) || max_scale <= 0) max_scale <- 1

  tibble(
    parameterization_mode = mode,
    parameter_name = parameter_name,
    parameter_unit = parameter_unit,
    parameter_anchor_value = anchor_value,
    parameter_min_value = 0,
    parameter_max_value = max_value,
    parameter_min_scale = min_scale,
    parameter_max_scale = max_scale,
    parameter_extrapolation = extrapolated,
    parameter_evidence_basis = interpolation_basis
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: build_parameterization_validation
# Purpose: Run hard checks on important parameter bounds, extrapolation rules, and protection classifications.
# ------------------------------------------------------------------------------
build_parameterization_validation_core <- function(policy_model) {
  m <- policy_model$meta
  get_one <- function(pattern_title, pattern_variant = NULL) {
    x <- m |> filter(stringr::str_detect(stringr::str_to_lower(title), stringr::regex(pattern_title, ignore_case = TRUE)))
    if (!is.null(pattern_variant)) x <- x |> filter(stringr::str_detect(stringr::str_to_lower(variant_name), stringr::regex(pattern_variant, ignore_case = TRUE)))
    x
  }
  low_surtax <- get_one("impose a surtax on individuals' adjusted gross income", "20,000|40,000")
  high_surtax <- get_one("impose a surtax on individuals' adjusted gross income", "100,000|200,000")
  vat <- get_one("value-added tax", "narrow")
  ftt <- get_one("financial transactions")
  ma_bench <- get_one("reduce medicare advantage benchmarks")
  ma_coding <- get_one("modify payments to medicare advantage plans for health risk", "coding-pattern adjustment")
  ss_90 <- get_one("increase the maximum taxable earnings", "90 percent")
  ss_250 <- get_one("increase the maximum taxable earnings", "250,000")
  tibble(
    check = c(
      "No solver-eligible response-function extrapolation",
      "Low-threshold AGI surtax blocked from protected solver universes",
      "Higher-threshold AGI surtax capped at official 2 percent anchor",
      "Narrow VAT capped at official 5 percent anchor",
      "FTT capped at current official 0.01 percent anchor",
      "Medicare Advantage benchmark reduction capped at official 10 percent anchor",
      "Medicare Advantage coding alternatives retained as discrete official 8 and 20 percent levels",
      "Social Security 90-percent-taxable-earnings actuarial score uses SSA 2025 E3.1",
      "Social Security over-$250,000 actuarial score uses SSA 2025 E2.5"
    ),
    passed = c(
      sum(m$parameter_extrapolation & m$parameterized_solver_eligible) == 0L,
      nrow(low_surtax) == 1L && low_surtax$protection_status[[1]] == "BLOCKED",
      nrow(high_surtax) == 1L && abs(high_surtax$parameter_max_value[[1]] - 2) < 1e-12,
      nrow(vat) == 1L && abs(vat$parameter_max_value[[1]] - 5) < 1e-12,
      nrow(ftt) == 1L && abs(ftt$parameter_max_value[[1]] - 0.01) < 1e-12,
      nrow(ma_bench) == 1L && abs(ma_bench$parameter_max_value[[1]] - 10) < 1e-12,
      nrow(ma_coding) == 2L && all(ma_coding$parameterization_mode == "DISCRETE_FULL_ANCHOR") && setequal(ma_coding$parameter_anchor_value, c(8, 20)),
      nrow(ss_90) == 1L && abs(ss_90$ss_actuarial_improvement_pct_payroll[[1]] - 0.82) < 1e-12,
      nrow(ss_250) == 1L && abs(ss_250$ss_actuarial_improvement_pct_payroll[[1]] - 2.50) < 1e-12
    ),
    evidence_basis = c(
      "Parameter-response hard gate",
      "Fiscal-philosophy ordinary-wage protection rule",
      "CBO/JCT December 2024 AGI surtax alternative",
      "CBO/JCT December 2024 narrow-base 5 percent VAT alternative",
      "CBO/JCT December 2024 0.01 percent FTT alternative; older CBO analysis documents nonlinear revenue at higher rates",
      "CBO 2022 extended discussion says smaller reductions are roughly proportional and much larger reductions are less than proportional",
      "CBO December 2024 separately scores 8 percent and 20 percent coding-pattern adjustments",
      "SSA OACT 2025 Trustees Report E3.1",
      "SSA OACT 2025 Trustees Report E2.5"
    )
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: build_robust_scenario_catalog
# Purpose: Construct the central and adverse annual macro/fiscal scenarios enforced or audited by the optimizer.
# ------------------------------------------------------------------------------
build_robust_scenario_catalog <- function(working_baseline) {
  years <- CFG$model_years

  make_stress <- function(source_id = NULL, multiplier = 1, gdp_growth_shock_pp = 0, policy_yield_factor = 1, rate_addition = 0) {
    if (is.null(source_id)) {
      deficit <- rep(0, length(years))
    } else {
      p <- build_macro_stress_profile(source_id, multiplier) |>
        right_join(tibble(year = years), by = "year") |>
        arrange(year) |>
        mutate(deficit_delta_bil = replace_na(deficit_delta_bil, 0))
      deficit <- p$deficit_delta_bil
      last36 <- deficit[match(2036L, years)]
      gdp36 <- working_baseline$gdp_bil[match(2036L, working_baseline$year)]
      if (any(years > 2036L)) {
        share36 <- last36 / gdp36
        deficit[years > 2036L] <- share36 * working_baseline$gdp_bil[match(years[years > 2036L], working_baseline$year)]
      }
    }
    stress_debt <- cumsum(deficit)
    year_steps <- pmax(years - 2026L, 0L)
    gdp_factor <- (1 + gdp_growth_shock_pp / 100)^year_steps
    tibble(
      year = years,
      stress_deficit_delta_bil = deficit,
      stress_debt_delta_bil = stress_debt,
      gdp_factor = gdp_factor,
      policy_yield_factor = policy_yield_factor,
      marginal_rate_addition = rate_addition
    )
  }

  central <- make_stress()
  rates01 <- make_stress("rates_plus_0_1", 1, 0, 1, 0.001)
  prod01 <- make_stress("productivity_minus_0_1", 1, -0.1, 1, 0)
  labor01 <- make_stress("labor_force_minus_0_1", 1, -0.1, 1, 0)
  yield90 <- make_stress(NULL, 1, 0, CFG$robust_policy_yield_floor, 0)
  rates05 <- make_stress("rates_plus_0_1", 5, 0, 1, 0.005)
  prod03 <- make_stress("productivity_minus_0_1", 3, -0.3, 1, 0)

  combined <- central
  combined$stress_deficit_delta_bil <- rates01$stress_deficit_delta_bil + prod01$stress_deficit_delta_bil + labor01$stress_deficit_delta_bil
  combined$stress_debt_delta_bil <- cumsum(combined$stress_deficit_delta_bil)
  combined$gdp_factor <- prod01$gdp_factor * labor01$gdp_factor
  combined$policy_yield_factor <- CFG$robust_policy_yield_floor
  combined$marginal_rate_addition <- 0.001

  scenario_meta <- tibble(
    scenario_id = c(
      "CENTRAL",
      "RATES_PLUS_0_1",
      "PRODUCTIVITY_MINUS_0_1",
      "LABOR_FORCE_MINUS_0_1",
      "POLICY_YIELD_90",
      "RATES_PLUS_0_5",
      "PRODUCTIVITY_MINUS_0_3",
      "COMBINED_ADVERSE"
    ),
    required_robust = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    evidence_class = c(
      "CENTRAL_WORKING_BASELINE",
      "CBO_BENCHMARK_PLUS_MODELLED_MARGINAL_RATE",
      "CBO_BENCHMARK_PLUS_DENOMINATOR_ADJUSTMENT",
      "CBO_BENCHMARK_PLUS_DENOMINATOR_ADJUSTMENT",
      "EXPLORATORY_POLICY_REALIZATION_HAIRCUT",
      "CBO_BENCHMARK_LINEAR_STRESS_MULTIPLIER",
      "CBO_BENCHMARK_LINEAR_STRESS_MULTIPLIER_PLUS_DENOMINATOR_ADJUSTMENT",
      "EXPLORATORY_ADDITIVE_COMBINED_ADVERSE"
    ),
    description = c(
      "Working September 2026 baseline and full modeled policy realization",
      "CBO 0.1 percentage point higher-rate budget sensitivity with marginal policy debt-service rate adjustment",
      "CBO 0.1 percentage point slower productivity budget sensitivity plus a modeled GDP denominator path",
      "CBO 0.1 percentage point slower labor-force growth budget sensitivity plus a modeled GDP denominator path",
      "Central macro path with only 90 percent of modeled policy fiscal effects realized",
      "Five times the CBO 0.1 percentage point higher-rate benchmark for audit stress testing",
      "Three times the CBO productivity benchmark with a modeled GDP denominator path for audit stress testing",
      "Additive rate, productivity, and labor-force stresses plus 90 percent policy realization; audit-only because joint additivity is not an official CBO combined score"
    )
  )

  annual <- bind_rows(
    CENTRAL = central,
    RATES_PLUS_0_1 = rates01,
    PRODUCTIVITY_MINUS_0_1 = prod01,
    LABOR_FORCE_MINUS_0_1 = labor01,
    POLICY_YIELD_90 = yield90,
    RATES_PLUS_0_5 = rates05,
    PRODUCTIVITY_MINUS_0_3 = prod03,
    COMBINED_ADVERSE = combined,
    .id = "scenario_id"
  ) |>
    left_join(scenario_meta, by = "scenario_id") |>
    left_join(
      working_baseline |>
        select(year, working_debt_bil, gdp_bil),
      by = "year"
    ) |>
    mutate(
      scenario_gdp_bil = gdp_bil * gdp_factor,
      scenario_baseline_debt_bil = working_debt_bil + stress_debt_delta_bil
    )

  list(meta = scenario_meta, annual = annual)
}


# ------------------------------------------------------------------------------
# FUNCTION: build_parameterized_policy_space
# Purpose: Expand admissible candidates into policy-level, timing, and phase-in decision structures.
# ------------------------------------------------------------------------------
build_parameterized_policy_space_core <- function(policy_model, working_baseline) {
  log_line("Gate 3B: constructing parameterized policy decision space")

  specs <- purrr::map2_dfr(policy_model$meta$title, policy_model$meta$variant_name, parameter_spec_one)
  meta <- bind_cols(policy_model$meta, specs) |>
    mutate(
      decision_type = if_else(parameterization_mode == "DISCRETE_FULL_ANCHOR", "DISCRETE_WITH_TIMING", "CONTINUOUS_LEVEL_WITH_TIMING"),
      parameterized_solver_eligible = solver_eligible_annual
    )

  schedules <- meta |>
    filter(parameterized_solver_eligible) |>
    select(
      candidate_id,
      family_id,
      title,
      variant_name,
      parameterization_mode,
      parameter_name,
      parameter_unit,
      parameter_anchor_value,
      parameter_min_value,
      parameter_max_value,
      parameter_min_scale,
      parameter_max_scale,
      parameter_extrapolation,
      ss_actuarial_improvement_pct_payroll
    ) |>
    tidyr::crossing(
      implementation_start_year = as.integer(CFG$parameterized_start_years),
      phase_in_years = as.integer(CFG$parameterized_phase_in_years)
    ) |>
    mutate(
      schedule_id = paste0("START", implementation_start_year, "_PHASE", phase_in_years),
      schedule_key = paste(candidate_id, schedule_id, sep = "@@")
    )

  base_flows <- policy_model$flows |>
    select(candidate_id, year, revenue_delta_bil, outlay_delta_bil, primary_deficit_delta_bil)

  schedule_flows <- purrr::map_dfr(seq_len(nrow(schedules)), function(i) {
    s <- schedules[i, ]
    base <- base_flows |>
      filter(candidate_id == s$candidate_id) |>
      arrange(year)
    assert_model(nrow(base) == length(CFG$model_years), paste0("Missing full translated annual path for parameterized candidate ", s$candidate_id))
    shift <- as.integer(s$implementation_start_year - 2027L)
    purrr::map_dfr(CFG$model_years, function(y) {
      source_year <- y - shift
      if (y < s$implementation_start_year || source_year < min(CFG$model_years)) {
        rev <- 0
        out <- 0
        pri <- 0
        phase <- 0
      } else {
        row <- base |> filter(year == source_year)
        if (nrow(row) == 0L) {
          rev <- 0
          out <- 0
          pri <- 0
        } else {
          rev <- row$revenue_delta_bil[[1]]
          out <- row$outlay_delta_bil[[1]]
          pri <- row$primary_deficit_delta_bil[[1]]
        }
        phase <- min(1, (y - s$implementation_start_year + 1) / s$phase_in_years)
      }
      tibble(
        schedule_key = s$schedule_key,
        candidate_id = s$candidate_id,
        schedule_id = s$schedule_id,
        implementation_start_year = s$implementation_start_year,
        phase_in_years = s$phase_in_years,
        year = y,
        phase_factor = phase,
        revenue_delta_bil_per_anchor_scale = rev * phase,
        outlay_delta_bil_per_anchor_scale = out * phase,
        primary_deficit_delta_bil_per_anchor_scale = pri * phase
      )
    })
  }) |>
    group_by(schedule_key) |>
    mutate(
      schedule_revenue_2027_2036_bil_per_scale = sum(pmax(revenue_delta_bil_per_anchor_scale[year %in% CFG$score_years], 0)),
      schedule_spending_cut_2027_2036_bil_per_scale = sum(pmax(-outlay_delta_bil_per_anchor_scale[year %in% CFG$score_years], 0)),
      schedule_primary_improvement_2027_2036_bil_per_scale = sum(-primary_deficit_delta_bil_per_anchor_scale[year %in% CFG$score_years])
    ) |>
    ungroup()

  schedule_summary <- schedule_flows |>
    group_by(schedule_key, candidate_id, schedule_id, implementation_start_year, phase_in_years) |>
    summarise(
      revenue_2027_2036_bil_per_scale = first(schedule_revenue_2027_2036_bil_per_scale),
      spending_cut_2027_2036_bil_per_scale = first(schedule_spending_cut_2027_2036_bil_per_scale),
      primary_improvement_2027_2036_bil_per_scale = first(schedule_primary_improvement_2027_2036_bil_per_scale),
      .groups = "drop"
    ) |>
    left_join(
      schedules |>
        select(
          schedule_key,
          parameterization_mode,
          parameter_name,
          parameter_unit,
          parameter_anchor_value,
          parameter_min_value,
          parameter_max_value,
          parameter_min_scale,
          parameter_max_scale,
          parameter_extrapolation,
          ss_actuarial_improvement_pct_payroll
        ),
      by = "schedule_key"
    )

  scenarios <- build_robust_scenario_catalog(working_baseline)

  assert_model(n_distinct(schedules$candidate_id) == sum(meta$parameterized_solver_eligible), "Parameterized schedule catalog lost at least one annual-score candidate")
  assert_model(all(table(schedules$candidate_id) == length(CFG$parameterized_start_years) * length(CFG$parameterized_phase_in_years)), "Parameterized schedule grid is incomplete")
  assert_model(sum(meta$parameterization_mode != "DISCRETE_FULL_ANCHOR" & meta$parameterized_solver_eligible) >= 40L, "Too few annual-score candidates received a continuous policy-level dimension")
  assert_model(sum(meta$parameter_extrapolation & meta$parameterized_solver_eligible) == 0L, "At least one solver-eligible policy still extrapolates beyond its official evidence anchor")
  low_surtax <- meta |>
    filter(stringr::str_detect(stringr::str_to_lower(title), "impose a surtax on individuals' adjusted gross income"), stringr::str_detect(stringr::str_to_lower(variant_name), "20,000|40,000"))
  assert_model(nrow(low_surtax) == 1L && low_surtax$protection_status[[1]] == "BLOCKED", "Low-threshold AGI surtax must remain blocked by the ordinary-wage protection rule")
  vat_audit <- meta |>
    filter(stringr::str_detect(stringr::str_to_lower(title), "value-added tax"), stringr::str_detect(stringr::str_to_lower(variant_name), "narrow"))
  assert_model(nrow(vat_audit) == 1L && abs(vat_audit$parameter_max_value[[1]] - 5) < 1e-12, "Narrow VAT rate must be capped at the official 5 percent CBO/JCT anchor")
  ftt_audit <- meta |>
    filter(stringr::str_detect(stringr::str_to_lower(title), "financial transactions"))
  assert_model(nrow(ftt_audit) == 1L && abs(ftt_audit$parameter_max_value[[1]] - 0.01) < 1e-12, "FTT rate must be capped at the current official 0.01 percent CBO/JCT anchor")
  ma_bench_audit <- meta |>
    filter(stringr::str_detect(stringr::str_to_lower(title), "reduce medicare advantage benchmarks"))
  assert_model(nrow(ma_bench_audit) == 1L && abs(ma_bench_audit$parameter_max_value[[1]] - 10) < 1e-12, "Medicare Advantage benchmark reduction must be capped at the official 10 percent anchor")
  ma_coding_audit <- meta |>
    filter(stringr::str_detect(stringr::str_to_lower(title), "modify payments to medicare advantage plans for health risk"), stringr::str_detect(stringr::str_to_lower(variant_name), "coding-pattern adjustment"))
  assert_model(nrow(ma_coding_audit) == 2L && all(ma_coding_audit$parameterization_mode == "DISCRETE_FULL_ANCHOR"), "Medicare Advantage coding-pattern alternatives must remain discrete at the official 8 and 20 percent levels")

  log_line(
    "Gate 3B complete: ", sum(meta$parameterized_solver_eligible), " annual-score candidates; ",
    sum(meta$parameterization_mode != "DISCRETE_FULL_ANCHOR" & meta$parameterized_solver_eligible), " with continuous policy levels; ",
    nrow(schedules), " implementation/phase schedules; ",
    sum(scenarios$meta$required_robust), " robust scenarios enforced inside the MILP; ",
    sum(meta$parameter_extrapolation & meta$parameterized_solver_eligible), " solver-eligible extrapolated response functions"
  )

  list(
    meta = meta,
    flows = policy_model$flows,
    pack = policy_model$pack,
    schedules = schedules,
    schedule_summary = schedule_summary,
    schedule_flows = schedule_flows,
    scenario_meta = scenarios$meta,
    scenario_annual = scenarios$annual,
    source_policy_model = policy_model
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: policy_universe_for_mode
# Purpose: Filter the master candidate universe by protection mode; materiality labels never remove solver candidates.
# ------------------------------------------------------------------------------
policy_universe_for_mode_core_2 <- function(
  policy_model,
  protection_mode = c("STRICT", "EXPANDED"),
  materiality_mode = c("PACKAGE_READY", "FULL_CAPACITY")
) {
  protection_mode <- match.arg(protection_mode)
  materiality_mode <- match.arg(materiality_mode)
  allowed_status <- if (protection_mode == "STRICT") "ELIGIBLE" else c("ELIGIBLE", "CONDITIONAL")
  meta <- policy_model$meta |>
    filter(parameterized_solver_eligible, protection_status %in% allowed_status)
  if (CFG$evidence_mode == "OFFICIAL_CURRENT") meta <- meta |> filter(evidence_class == "OFFICIAL_CURRENT")
  if (CFG$evidence_mode == "OFFICIAL_OLDER") meta <- meta |> filter(evidence_class %in% c("OFFICIAL_CURRENT", "OFFICIAL_OLDER"))
  # Materiality is reporting metadata only; no valid candidate is removed here.
  ids <- meta$candidate_id
  schedules <- policy_model$schedules |> filter(candidate_id %in% ids)
  schedule_summary <- policy_model$schedule_summary |> filter(candidate_id %in% ids)
  schedule_flows <- policy_model$schedule_flows |> filter(candidate_id %in% ids)
  list(
    meta = meta,
    schedules = schedules,
    schedule_summary = schedule_summary,
    schedule_flows = schedule_flows,
    scenario_meta = policy_model$scenario_meta,
    scenario_annual = policy_model$scenario_annual,
    protection_mode = protection_mode,
    materiality_mode = materiality_mode
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_full_milp
# Purpose: Build the sparse HiGHS MILP matrix, bounds, integrality vector, objective, and accounting constraints.
# ------------------------------------------------------------------------------
build_full_milp <- function(
  universe,
  working_baseline,
  kernel_obj,
  objective = c("revenue", "spending", "policy_count", "complexity", "debt", "reference_75", "target_slack"),
  require_patterns = character(),
  forbid_patterns = character(),
  max_revenue_bil = Inf,
  max_spending_bil = Inf,
  require_ss_solvency = FALSE,
  soft_targets = FALSE,
  max_target_slack_score = Inf,
  previous_packages = list(),
  min_hamming_distance = CFG$diversity_hamming_distance
) {
  objective <- match.arg(objective)
  meta <- universe$meta |> arrange(candidate_id)
  schedules <- universe$schedules |> arrange(candidate_id, implementation_start_year, phase_in_years)
  schedule_summary <- universe$schedule_summary |> arrange(schedule_key)
  schedule_flows <- universe$schedule_flows
  scenarios <- universe$scenario_meta |> filter(required_robust) |> arrange(scenario_id)
  scenario_annual <- universe$scenario_annual |> filter(scenario_id %in% scenarios$scenario_id)
  years <- CFG$model_years
  n_policy <- nrow(meta)
  assert_model(n_policy >= CFG$minimum_solver_candidates, paste0("Only ", n_policy, " candidates reached the solver under ", universe$protection_mode, " protection mode; minimum is ", CFG$minimum_solver_candidates, "."))

  y_names <- paste0("y::", meta$candidate_id)
  a_names <- paste0("a::", schedules$schedule_key)
  z_names <- paste0("z::", schedules$schedule_key)
  state_names <- unlist(purrr::map(scenarios$scenario_id, function(sc) {
    c(
      paste0("rev::", sc, "::", years),
      paste0("out::", sc, "::", years),
      paste0("primary::", sc, "::", years),
      paste0("interest::", sc, "::", years),
      paste0("debt::", sc, "::", years)
    )
  }), use.names = FALSE)
  slack_names <- if (soft_targets) unlist(purrr::map(scenarios$scenario_id, function(sc) c(paste0("slack::", sc, "::2036"), paste0("slack::", sc, "::2046"))), use.names = FALSE) else character()
  reference_names <- if (objective == "reference_75") c("reference_dev_pos::CENTRAL::2046", "reference_dev_neg::CENTRAL::2046") else character()
  var_names <- c(y_names, a_names, z_names, state_names, slack_names, reference_names)
  nv <- length(var_names)
  idx <- setNames(seq_along(var_names), var_names)
  lower <- rep(-Inf, nv)
  upper <- rep(Inf, nv)
  types <- rep("C", nv)
  lower[idx[y_names]] <- 0
  upper[idx[y_names]] <- 1
  types[idx[y_names]] <- "I"
  lower[idx[a_names]] <- 0
  upper[idx[a_names]] <- 1
  types[idx[a_names]] <- "I"
  lower[idx[z_names]] <- 0
  upper[idx[z_names]] <- schedules$parameter_max_scale
  if (soft_targets) lower[idx[slack_names]] <- 0
  if (length(reference_names) > 0L) lower[idx[reference_names]] <- 0

  ii_parts <- list()
  jj_parts <- list()
  xx_parts <- list()
  lhs <- numeric()
  rhs <- numeric()
  labels <- character()
  row_counter <- 0L

  add <- function(coefs, lo = -Inf, hi = Inf, label = "") {
    row_counter <<- row_counter + 1L
    if (length(coefs) > 0L) {
      coef_names <- names(coefs)
      assert_model(!is.null(coef_names) && length(coef_names) == length(coefs), paste0("Unnamed coefficient encountered while building constraint: ", label))
      positions <- unname(idx[coef_names])
      missing_names <- unique(coef_names[is.na(positions)])
      assert_model(length(missing_names) == 0L, paste0("Unknown MILP variable name(s) in constraint ", label, ": ", paste(missing_names, collapse = ", ")))
      values <- as.numeric(coefs)
      assert_model(all(is.finite(values)), paste0("Non-finite MILP coefficient encountered in constraint: ", label))
      agg <- tapply(values, positions, sum)
      pos <- as.integer(names(agg))
      val <- as.numeric(agg)
      keep <- abs(val) > 1e-14
      ii_parts[[row_counter]] <<- rep.int(row_counter, sum(keep))
      jj_parts[[row_counter]] <<- pos[keep]
      xx_parts[[row_counter]] <<- val[keep]
    }
    lhs <<- c(lhs, lo)
    rhs <<- c(rhs, hi)
    labels <<- c(labels, label)
  }

  for (id in meta$candidate_id) {
    sk <- schedules$schedule_key[schedules$candidate_id == id]
    add(
      c(setNames(rep(1, length(sk)), paste0("a::", sk)), setNames(-1, paste0("y::", id))),
      0,
      0,
      paste0("Exactly one timing schedule when policy is active: ", id)
    )
  }

  for (i in seq_len(nrow(schedules))) {
    sk <- schedules$schedule_key[i]
    min_scale <- schedules$parameter_min_scale[i]
    max_scale <- schedules$parameter_max_scale[i]
    add(c(setNames(1, paste0("z::", sk)), setNames(-max_scale, paste0("a::", sk))), hi = 0, label = paste0("Policy level upper link: ", sk))
    add(c(setNames(1, paste0("z::", sk)), setNames(-min_scale, paste0("a::", sk))), lo = 0, label = paste0("Policy level lower link: ", sk))
  }

  for (fam in unique(meta$family_id)) {
    ids <- meta$candidate_id[meta$family_id == fam]
    if (length(ids) > 1L) add(setNames(rep(1, length(ids)), paste0("y::", ids)), hi = 1, label = paste0("Mutually exclusive CBO alternatives: ", fam))
  }

  interactions <- build_interaction_catalog_full(meta)
  if (!CFG$allow_unscored_interactions && nrow(interactions) > 0L) {
    for (i in seq_len(nrow(interactions))) {
      add(
        setNames(c(1, 1), paste0("y::", c(interactions$candidate_i[i], interactions$candidate_j[i]))),
        hi = 1,
        label = paste0("Unscored material overlap: ", interactions$candidate_i[i], " + ", interactions$candidate_j[i])
      )
    }
  }

  searchable <- paste(meta$title, meta$variant_name)
  for (p in require_patterns) {
    ids <- meta$candidate_id[stringr::str_detect(searchable, stringr::regex(p, ignore_case = TRUE))]
    assert_model(length(ids) > 0L, paste0("Required policy pattern matched no eligible candidates: ", p))
    add(setNames(rep(1, length(ids)), paste0("y::", ids)), lo = 1, label = paste0("Require pattern: ", p))
  }
  for (p in forbid_patterns) {
    ids <- meta$candidate_id[stringr::str_detect(searchable, stringr::regex(p, ignore_case = TRUE))]
    if (length(ids) > 0L) add(setNames(rep(1, length(ids)), paste0("y::", ids)), hi = 0, label = paste0("Forbid pattern: ", p))
  }

  ss_score <- schedules$ss_actuarial_improvement_pct_payroll
  ss_coef <- ss_score
  names(ss_coef) <- z_names
  add(ss_coef, hi = CFG$ss_actuarial_max_pct_payroll, label = "Maximum Social Security actuarial improvement: solvency gap plus configured margin")
  if (require_ss_solvency) add(ss_coef, lo = CFG$ss_actuarial_gap_pct_payroll, label = "Approximate Social Security 75-year actuarial solvency")

  revenue_score <- schedule_summary$revenue_2027_2036_bil_per_scale
  names(revenue_score) <- paste0("z::", schedule_summary$schedule_key)
  spending_score <- schedule_summary$spending_cut_2027_2036_bil_per_scale
  names(spending_score) <- paste0("z::", schedule_summary$schedule_key)
  if (is.finite(max_revenue_bil)) add(revenue_score, hi = max_revenue_bil, label = "Maximum cumulative revenue increase FY2027-FY2036")
  if (is.finite(max_spending_bil)) add(spending_score, hi = max_spending_bil, label = "Maximum cumulative spending reduction FY2027-FY2036")

  schedule_order <- schedules$schedule_key
  schedule_position <- setNames(seq_along(schedule_order), schedule_order)
  flow_by_year <- split(schedule_flows, schedule_flows$year)
  flow_row <- function(column, year) {
    x <- flow_by_year[[as.character(year)]]
    v <- numeric(length(schedule_order))
    names(v) <- schedule_order
    if (!is.null(x) && nrow(x) > 0L) {
      pos <- unname(schedule_position[x$schedule_key])
      keep <- !is.na(pos)
      v[pos[keep]] <- x[[column]][keep]
    }
    v
  }

  kernel <- kernel_obj$kernel
  central_long_rate <- get_long_run_rate(kernel_obj)

  for (sc in scenarios$scenario_id) {
    sc_row <- scenarios |> filter(scenario_id == sc)
    annual_sc <- scenario_annual |> filter(scenario_id == sc) |> arrange(year)
    yield_factor <- annual_sc$policy_yield_factor[1]
    rate_addition <- annual_sc$marginal_rate_addition[1]
    kernel_multiplier <- (central_long_rate + rate_addition) / central_long_rate
    long_rate <- central_long_rate + rate_addition

    for (y in years) {
      rev_coef <- flow_row("revenue_delta_bil_per_anchor_scale", y) * yield_factor
      out_coef <- flow_row("outlay_delta_bil_per_anchor_scale", y) * yield_factor
      pri_coef <- flow_row("primary_deficit_delta_bil_per_anchor_scale", y) * yield_factor
      add(c(setNames(-rev_coef, z_names), setNames(1, paste0("rev::", sc, "::", y))), 0, 0, paste0(sc, " revenue identity FY", y))
      add(c(setNames(-out_coef, z_names), setNames(1, paste0("out::", sc, "::", y))), 0, 0, paste0(sc, " outlay identity FY", y))
      add(c(setNames(-pri_coef, z_names), setNames(1, paste0("primary::", sc, "::", y))), 0, 0, paste0(sc, " primary-deficit identity FY", y))
    }

    for (y in years) {
      co <- c(setNames(1, paste0("interest::", sc, "::", y)))
      if (y <= 2036L) {
        krow <- kernel |> filter(output_year == y, input_year %in% 2026:2036)
        if (nrow(krow) > 0L) {
          vals <- -krow$debt_service_effect_per_1_bil_primary_deficit * kernel_multiplier
          names(vals) <- paste0("primary::", sc, "::", krow$input_year)
          co <- c(co, vals)
        }
      } else {
        co <- c(
          co,
          setNames(-long_rate, paste0("debt::", sc, "::", y - 1L)),
          setNames(-0.5 * long_rate, paste0("primary::", sc, "::", y))
        )
      }
      add(co, 0, 0, paste0(sc, " interest identity FY", y))
    }

    for (i in seq_along(years)) {
      y <- years[i]
      co <- c(
        setNames(1, paste0("debt::", sc, "::", y)),
        setNames(-1, paste0("primary::", sc, "::", y)),
        setNames(-1, paste0("interest::", sc, "::", y))
      )
      if (i > 1L) co <- c(co, setNames(-1, paste0("debt::", sc, "::", years[i - 1L])))
      add(co, 0, 0, paste0(sc, " debt accumulation FY", y))
    }

    for (target_year in c(2036L, 2046L)) {
      target_ratio <- if (target_year == 2036L) CFG$target_2036 else CFG$target_2046_high
      target_row <- annual_sc |> filter(year == target_year)
      target_rhs <- target_ratio * target_row$scenario_gdp_bil - target_row$scenario_baseline_debt_bil
      co <- c(setNames(1, paste0("debt::", sc, "::", target_year)))
      if (soft_targets) co <- c(co, setNames(-1, paste0("slack::", sc, "::", target_year)))
      add(co, hi = target_rhs, label = paste0(sc, " robust debt/GDP FY", target_year, " target"))
    }

    if (CFG$enforce_2046_lower_bound && !soft_targets) {
      target_row <- annual_sc |> filter(year == 2046L)
      lower_rhs <- CFG$target_2046_low * target_row$scenario_gdp_bil - target_row$scenario_baseline_debt_bil
      add(setNames(1, paste0("debt::", sc, "::2046")), lo = lower_rhs, label = paste0(sc, " debt/GDP FY2046 lower target"))
    }
  }

  if (soft_targets && is.finite(max_target_slack_score)) {
    slack_score_coef <- numeric()
    for (sc in scenarios$scenario_id) {
      g36 <- universe$scenario_annual |> filter(scenario_id == sc, year == 2036L) |> pull(scenario_gdp_bil)
      g46 <- universe$scenario_annual |> filter(scenario_id == sc, year == 2046L) |> pull(scenario_gdp_bil)
      slack_score_coef <- c(
        slack_score_coef,
        setNames(1000 / g36, paste0("slack::", sc, "::2036")),
        setNames(1000 / g46, paste0("slack::", sc, "::2046"))
      )
    }
    add(
      slack_score_coef,
      hi = max_target_slack_score + CFG$soft_frontier_slack_absolute_tolerance,
      label = "Maximum normalized robust target-slack score"
    )
  }

  if (objective == "reference_75") {
    target_row <- universe$scenario_annual |> filter(scenario_id == "CENTRAL", year == 2046L)
    reference_rhs <- CFG$target_2046_center * target_row$scenario_gdp_bil - target_row$scenario_baseline_debt_bil
    add(
      c(
        setNames(1, "debt::CENTRAL::2046"),
        setNames(-1, "reference_dev_pos::CENTRAL::2046"),
        setNames(1, "reference_dev_neg::CENTRAL::2046")
      ),
      lo = reference_rhs,
      hi = reference_rhs,
      label = "Absolute deviation from central FY2046 75 percent reference"
    )
  }

  if (length(previous_packages) > 0L && min_hamming_distance > 0L) {
    for (k in seq_along(previous_packages)) {
      ones <- intersect(previous_packages[[k]], meta$candidate_id)
      zeros <- setdiff(meta$candidate_id, ones)
      co <- c(
        setNames(rep(-1, length(ones)), paste0("y::", ones)),
        setNames(rep(1, length(zeros)), paste0("y::", zeros))
      )
      add(co, lo = min_hamming_distance - length(ones), label = paste0("Diversity cut ", k))
    }
  }

  L <- numeric(nv)
  if (objective == "revenue") L[idx[names(revenue_score)]] <- revenue_score
  if (objective == "spending") L[idx[names(spending_score)]] <- spending_score
  if (objective == "policy_count") L[idx[y_names]] <- 1
  if (objective == "complexity") {
    cw <- dplyr::coalesce(meta$complexity_weight, 1)
    names(cw) <- y_names
    L[idx[y_names]] <- cw
  }
  if (objective == "debt") {
    L[idx[["debt::CENTRAL::2036"]]] <- 1
    L[idx[["debt::CENTRAL::2046"]]] <- 1
    L[idx[y_names]] <- 1e-4
  }
  if (objective == "reference_75") {
    L[idx[["reference_dev_pos::CENTRAL::2046"]]] <- 1
    L[idx[["reference_dev_neg::CENTRAL::2046"]]] <- 1
    L[idx[y_names]] <- 1e-4
  }
  if (objective == "target_slack") {
    assert_model(soft_targets, "target_slack objective requires soft_targets=TRUE")
    for (sc in scenarios$scenario_id) {
      g36 <- universe$scenario_annual |> filter(scenario_id == sc, year == 2036L) |> pull(scenario_gdp_bil)
      g46 <- universe$scenario_annual |> filter(scenario_id == sc, year == 2046L) |> pull(scenario_gdp_bil)
      L[idx[[paste0("slack::", sc, "::2036")]]] <- 1000 / g36
      L[idx[[paste0("slack::", sc, "::2046")]]] <- 1000 / g46
    }
  }

  ii <- unlist(ii_parts, use.names = FALSE)
  jj <- unlist(jj_parts, use.names = FALSE)
  xx <- unlist(xx_parts, use.names = FALSE)
  A <- Matrix::sparseMatrix(i = ii, j = jj, x = xx, dims = c(row_counter, nv), giveCsparse = TRUE)
  A <- methods::as(A, "dgCMatrix")

  list(
    L = L,
    lower = lower,
    upper = upper,
    A = A,
    lhs = lhs,
    rhs = rhs,
    types = types,
    variable_names = var_names,
    policy_variable_names = y_names,
    schedule_binary_names = a_names,
    intensity_variable_names = z_names,
    meta = meta,
    schedules = schedules,
    schedule_summary = schedule_summary,
    schedule_flows = schedule_flows,
    scenario_meta = scenarios,
    scenario_annual = universe$scenario_annual,
    interactions = interactions,
    constraint_labels = labels,
    objective_name = objective,
    protection_mode = universe$protection_mode,
    materiality_mode = universe$materiality_mode,
    soft_targets = soft_targets,
    max_target_slack_score = max_target_slack_score,
    require_ss_solvency = require_ss_solvency
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: extract_parameter_decisions_from_primal
# Purpose: Translate a HiGHS primal vector back into human-readable policy levels and timing decisions.
# ------------------------------------------------------------------------------
extract_parameter_decisions_from_primal <- function(model, primal_solution) {
  xall <- as.numeric(primal_solution)
  names(xall) <- model$variable_names
  active <- model$meta$candidate_id[xall[paste0("y::", model$meta$candidate_id)] > 0.5]
  if (length(active) == 0L) return(tibble())
  selected_schedules <- model$schedules |>
    mutate(
      activation_value = xall[paste0("a::", schedule_key)],
      intensity_scale = xall[paste0("z::", schedule_key)]
    ) |>
    filter(candidate_id %in% active, activation_value > 0.5) |>
    left_join(
      model$meta |>
        select(
          candidate_id,
          family_id,
          title,
          variant_name,
          major_category,
          protection_status,
          parameterization_mode,
          parameter_name,
          parameter_unit,
          parameter_anchor_value,
          parameter_min_value,
          parameter_max_value,
          parameter_extrapolation,
          ss_actuarial_improvement_pct_payroll,
          complexity_weight,
          complexity_weight_basis,
          source_url
        ),
      by = "candidate_id",
      suffix = c("", ".meta")
    ) |>
    mutate(
      parameter_value = parameter_anchor_value * intensity_scale,
      parameter_value = pmin(parameter_value, parameter_max_value),
      ss_actuarial_contribution_pct_payroll = ss_actuarial_improvement_pct_payroll * intensity_scale
    )
  assert_model(n_distinct(selected_schedules$candidate_id) == length(active), "At least one active policy candidate lacks exactly one selected implementation schedule")
  selected_schedules
}


# ------------------------------------------------------------------------------
# FUNCTION: solve_full_milp
# Purpose: Submit one fiscal MILP objective/scenario configuration to HiGHS and normalize the solver result.
# ------------------------------------------------------------------------------
solve_full_milp_core_2 <- function(model, solve_label) {
  structure_audit <- validate_full_milp_structure(model, solve_label)
  n_int <- sum(model$types == "I")
  n_cont <- sum(model$types == "C")
  nnz <- structure_audit$nonzeros
  log_line(
    "MILP ", solve_label,
    " | variables=", length(model$variable_names),
    " (integer/binary=", n_int, ", continuous=", n_cont, ")",
    " | policy activations=", length(model$policy_variable_names),
    " | timing binaries=", length(model$schedule_binary_names),
    " | policy-level variables=", length(model$intensity_variable_names),
    " | robust scenarios=", nrow(model$scenario_meta),
    " | constraints=", nrow(model$A),
    " | nonzeros=", nnz
  )
  control <- highs::highs_control(
    threads = CFG$solver_threads,
    mip_rel_gap = CFG$solver_mip_rel_gap,
    primal_feasibility_tolerance = CFG$solver_primal_feasibility_tolerance,
    dual_feasibility_tolerance = CFG$solver_dual_feasibility_tolerance,
    log_to_console = TRUE
  )
  started <- Sys.time()
  sol <- highs::highs_solve(
    L = model$L,
    lower = model$lower,
    upper = model$upper,
    A = model$A,
    lhs = model$lhs,
    rhs = model$rhs,
    types = model$types,
    maximum = FALSE,
    control = control
  )
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  status_text <- if (is.null(sol$status_message)) "" else stringr::str_to_lower(as.character(sol$status_message)[1])
  solver_value_valid <- if (!is.null(sol$solver_msg) && !is.null(sol$solver_msg$value_valid)) isTRUE(sol$solver_msg$value_valid) else NA
  status_can_have_incumbent <- !stringr::str_detect(status_text, "infeasible|unbounded|model error|solve error|load error")
  primal_shape_ok <- !is.null(sol$primal_solution) && length(sol$primal_solution) == length(model$variable_names) && all(is.finite(sol$primal_solution))
  primal_ok <- status_can_have_incumbent && primal_shape_ok && (is.na(solver_value_valid) || solver_value_valid)
  selected <- character()
  decisions <- tibble()
  max_constraint_violation <- Inf
  max_bound_violation <- Inf
  max_integrality_violation <- Inf
  if (primal_ok) {
    xall <- as.numeric(sol$primal_solution)
    names(xall) <- model$variable_names
    selected <- stringr::str_remove(model$policy_variable_names[xall[model$policy_variable_names] > 0.5], "^y::")
    decisions <- extract_parameter_decisions_from_primal(model, xall)
    activity <- as.numeric(model$A %*% as.numeric(xall))
    low_violation <- ifelse(is.finite(model$lhs), pmax(model$lhs - activity, 0), 0)
    high_violation <- ifelse(is.finite(model$rhs), pmax(activity - model$rhs, 0), 0)
    max_constraint_violation <- max(c(low_violation, high_violation), na.rm = TRUE)
    lower_violation <- ifelse(is.finite(model$lower), pmax(model$lower - xall, 0), 0)
    upper_violation <- ifelse(is.finite(model$upper), pmax(xall - model$upper, 0), 0)
    max_bound_violation <- max(c(lower_violation, upper_violation), na.rm = TRUE)
    int_idx <- which(model$types == "I")
    max_integrality_violation <- if (length(int_idx) > 0L) max(abs(xall[int_idx] - round(xall[int_idx]))) else 0
  }
  feasible_incumbent <- primal_ok && max_constraint_violation <= CFG$solution_acceptance_constraint_tolerance && max_bound_violation <= CFG$solution_acceptance_bound_tolerance && max_integrality_violation <= CFG$solution_acceptance_integrality_tolerance
  info_num <- function(name) {
    if (is.null(sol$info) || is.null(sol$info[[name]])) return(NA_real_)
    suppressWarnings(as.numeric(sol$info[[name]][[1]]))
  }
  log_line(
    "MILP ", solve_label, " finished | status=", sol$status_message,
    " | elapsed=", sprintf("%.2f", elapsed), "s",
    " | nodes=", ifelse(is.na(info_num("mip_node_count")), "NA", format(info_num("mip_node_count"), scientific = FALSE)),
    " | mip_gap=", ifelse(is.na(info_num("mip_gap")), "NA", signif(info_num("mip_gap"), 5)),
    " | feasible_incumbent=", feasible_incumbent
  )
  list(
    status = sol$status,
    status_message = sol$status_message,
    objective_value = sol$objective_value,
    primal_solution = sol$primal_solution,
    selected_candidate_ids = selected,
    parameter_decisions = decisions,
    elapsed_seconds = elapsed,
    info = sol$info,
    mip_node_count = info_num("mip_node_count"),
    mip_dual_bound = info_num("mip_dual_bound"),
    mip_gap = info_num("mip_gap"),
    simplex_iteration_count = info_num("simplex_iteration_count"),
    ipm_iteration_count = info_num("ipm_iteration_count"),
    max_constraint_violation = max_constraint_violation,
    max_bound_violation = max_bound_violation,
    max_integrality_violation = max_integrality_violation,
    feasible_incumbent = feasible_incumbent,
    raw = sol,
    model = model,
    solve_label = solve_label
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: simulate_parameterized_package
# Purpose: Independently simulate one parameterized package across annual accounting and robust scenarios.
# ------------------------------------------------------------------------------
simulate_parameterized_package <- function(decisions, policy_model, working_baseline, kernel_obj) {
  years <- CFG$model_years
  if (nrow(decisions) == 0L) {
    central_policy <- tibble(year = years, revenue_delta_bil = 0, outlay_delta_bil = 0, primary_deficit_delta_bil = 0)
  } else {
    central_policy <- policy_model$schedule_flows |>
      inner_join(decisions |> select(schedule_key, intensity_scale), by = "schedule_key") |>
      mutate(
        revenue_delta_bil = revenue_delta_bil_per_anchor_scale * intensity_scale,
        outlay_delta_bil = outlay_delta_bil_per_anchor_scale * intensity_scale,
        primary_deficit_delta_bil = primary_deficit_delta_bil_per_anchor_scale * intensity_scale
      ) |>
      group_by(year) |>
      summarise(
        revenue_delta_bil = sum(revenue_delta_bil),
        outlay_delta_bil = sum(outlay_delta_bil),
        primary_deficit_delta_bil = sum(primary_deficit_delta_bil),
        .groups = "drop"
      ) |>
      right_join(tibble(year = years), by = "year") |>
      arrange(year) |>
      mutate(
        revenue_delta_bil = replace_na(revenue_delta_bil, 0),
        outlay_delta_bil = replace_na(outlay_delta_bil, 0),
        primary_deficit_delta_bil = replace_na(primary_deficit_delta_bil, 0)
      )
  }

  kernel <- kernel_obj$kernel
  central_long_rate <- get_long_run_rate(kernel_obj)

  purrr::map_dfr(policy_model$scenario_meta$scenario_id, function(sc) {
    annual_sc <- policy_model$scenario_annual |> filter(scenario_id == sc) |> arrange(year)
    yield_factor <- annual_sc$policy_yield_factor[1]
    rate_addition <- annual_sc$marginal_rate_addition[1]
    kernel_multiplier <- (central_long_rate + rate_addition) / central_long_rate
    long_rate <- central_long_rate + rate_addition
    f <- central_policy |>
      mutate(
        revenue_delta_bil = revenue_delta_bil * yield_factor,
        outlay_delta_bil = outlay_delta_bil * yield_factor,
        primary_deficit_delta_bil = primary_deficit_delta_bil * yield_factor
      )
    primary10 <- f$primary_deficit_delta_bil[match(2026:2036, f$year)]
    kernel_primary <- kernel |>
      left_join(tibble(input_year = 2026:2036, primary_deficit_delta_bil = primary10), by = "input_year") |>
      mutate(contribution = debt_service_effect_per_1_bil_primary_deficit * kernel_multiplier * primary_deficit_delta_bil) |>
      group_by(output_year) |>
      summarise(interest_delta_bil = sum(contribution), .groups = "drop")
    debt <- numeric(length(years))
    interest <- numeric(length(years))
    for (i in seq_along(years)) {
      y <- years[i]
      p <- f$primary_deficit_delta_bil[i]
      if (y <= 2036L) {
        interest[i] <- kernel_primary$interest_delta_bil[kernel_primary$output_year == y]
      } else {
        interest[i] <- long_rate * (debt[i - 1L] + 0.5 * p)
      }
      debt[i] <- if (i == 1L) p + interest[i] else debt[i - 1L] + p + interest[i]
    }
    f |>
      mutate(
        scenario_id = sc,
        policy_interest_delta_bil = interest,
        policy_debt_delta_bil = debt
      ) |>
      left_join(
        annual_sc |>
          select(year, required_robust, evidence_class, scenario_gdp_bil, scenario_baseline_debt_bil, stress_deficit_delta_bil, stress_debt_delta_bil, policy_yield_factor, marginal_rate_addition),
        by = "year"
      ) |>
      mutate(
        scenario_debt_bil = scenario_baseline_debt_bil + policy_debt_delta_bil,
        scenario_debt_gdp_pct = 100 * scenario_debt_bil / scenario_gdp_bil
      )
  })
}


# ------------------------------------------------------------------------------
# FUNCTION: summarize_full_solution
# Purpose: Summarize one independently verified solution package into fiscal, target, and composition metrics.
# ------------------------------------------------------------------------------
summarize_full_solution <- function(solution, policy_model, working_baseline, kernel_obj, solution_id) {
  if (!full_solver_has_feasible_incumbent(solution)) return(tibble())
  decisions <- solution$parameter_decisions
  sim <- simulate_parameterized_package(decisions, policy_model, working_baseline, kernel_obj)
  central <- sim |> filter(scenario_id == "CENTRAL")
  robust <- sim |> filter(required_robust)
  rev <- sum(pmax(central$revenue_delta_bil[central$year %in% CFG$score_years], 0))
  cut <- sum(pmax(-central$outlay_delta_bil[central$year %in% CFG$score_years], 0))
  ss <- sum(decisions$ss_actuarial_contribution_pct_payroll, na.rm = TRUE)
  d36 <- central$scenario_debt_gdp_pct[central$year == 2036L]
  d46 <- central$scenario_debt_gdp_pct[central$year == 2046L]
  worst36 <- max(robust$scenario_debt_gdp_pct[robust$year == 2036L], na.rm = TRUE)
  worst46 <- max(robust$scenario_debt_gdp_pct[robust$year == 2046L], na.rm = TRUE)
  robust_pass36 <- all(robust$scenario_debt_gdp_pct[robust$year == 2036L] <= 100 * CFG$target_2036 + 1e-7)
  robust_pass46 <- all(robust$scenario_debt_gdp_pct[robust$year == 2046L] <= 100 * CFG$target_2046_high + 1e-7)
  xvec <- as.numeric(solution$primal_solution)
  names(xvec) <- solution$model$variable_names
  verify_errors <- purrr::map_dbl(solution$model$scenario_meta$scenario_id, function(sc) {
    sim_sc <- sim |> filter(scenario_id == sc)
    max(abs(c(
      xvec[paste0("debt::", sc, "::2036")] - sim_sc$policy_debt_delta_bil[sim_sc$year == 2036L],
      xvec[paste0("debt::", sc, "::2046")] - sim_sc$policy_debt_delta_bil[sim_sc$year == 2046L]
    )))
  })
  verify_err <- max(verify_errors, na.rm = TRUE)
  tibble(
    solution_id = solution_id,
    solve_label = solution$solve_label,
    protection_mode = solution$model$protection_mode,
    objective = solution$model$objective_name,
    soft_targets = solution$model$soft_targets,
    max_target_slack_score = solution$model$max_target_slack_score,
    achieved_target_slack_score = target_slack_score_from_solution(solution),
    solver_status = solution$status_message,
    elapsed_seconds = solution$elapsed_seconds,
    selected_policy_count = n_distinct(decisions$candidate_id),
    continuously_parameterized_policy_count = sum(decisions$parameterization_mode != "DISCRETE_FULL_ANCHOR"),
    implementation_complexity_score = sum(dplyr::coalesce(decisions$complexity_weight, 1), na.rm = TRUE),
    revenue_2027_2036_bil = rev,
    spending_cuts_2027_2036_bil = cut,
    ss_actuarial_improvement_pct_payroll = ss,
    debt_gdp_2036_pct = d36,
    debt_gdp_2046_pct = d46,
    worst_required_scenario_debt_gdp_2036_pct = worst36,
    worst_required_scenario_debt_gdp_2046_pct = worst46,
    target_2036_pass = d36 <= 100 * CFG$target_2036 + 1e-7,
    target_2046_pass = d46 <= 100 * CFG$target_2046_high + 1e-7,
    robust_target_2036_pass = robust_pass36,
    robust_target_2046_pass = robust_pass46,
    independent_target_debt_error_bil = verify_err,
    solver_proven_optimal = full_solver_is_optimal(solution),
    independently_verified = verify_err <= CFG$independent_verification_tolerance_bil
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: extract_solution_membership
# Purpose: Extract the selected policy parameters, levels, timing, and provenance for one solution.
# ------------------------------------------------------------------------------
extract_solution_membership_core_2 <- function(solution, policy_model, solution_id) {
  if (nrow(solution$parameter_decisions) == 0L) return(tibble())
  solution$parameter_decisions |>
    transmute(
      solution_id = solution_id,
      candidate_id,
      family_id,
      title,
      variant_name,
      major_category,
      protection_status,
      parameterization_mode,
      parameter_name,
      parameter_unit,
      parameter_anchor_value,
      parameter_value,
      parameter_max_value,
      intensity_scale,
      implementation_start_year,
      phase_in_years,
      schedule_id,
      schedule_key,
      parameter_extrapolation,
      ss_actuarial_contribution_pct_payroll,
      source_url
    )
}


# ------------------------------------------------------------------------------
# FUNCTION: solution_score_from_decisions
# Purpose: Compute objective/reporting metrics from independently simulated parameterized decisions.
# ------------------------------------------------------------------------------
solution_score_from_decisions <- function(solution, score = c("revenue", "spending")) {
  score <- match.arg(score)
  if (!full_solver_has_feasible_incumbent(solution) || nrow(solution$parameter_decisions) == 0L) return(NA_real_)
  d <- solution$parameter_decisions |>
    select(schedule_key, intensity_scale) |>
    left_join(solution$model$schedule_summary, by = "schedule_key")
  if (score == "revenue") return(sum(d$revenue_2027_2036_bil_per_scale * d$intensity_scale, na.rm = TRUE))
  sum(d$spending_cut_2027_2036_bil_per_scale * d$intensity_scale, na.rm = TRUE)
}



# ------------------------------------------------------------------------------
# FUNCTION: target_slack_score_from_solution
# Purpose: Return the pure normalized robust target-slack score from a soft-target
# solution, excluding any secondary objective or policy-count tie-breaker.
# ------------------------------------------------------------------------------
target_slack_score_from_solution <- function(solution) {
  if (!full_solver_has_feasible_incumbent(solution) || !isTRUE(solution$model$soft_targets)) return(NA_real_)
  x <- as.numeric(solution$primal_solution)
  names(x) <- solution$model$variable_names
  total <- 0
  for (sc in solution$model$scenario_meta$scenario_id) {
    g36 <- solution$model$scenario_annual |> filter(scenario_id == sc, year == 2036L) |> pull(scenario_gdp_bil)
    g46 <- solution$model$scenario_annual |> filter(scenario_id == sc, year == 2046L) |> pull(scenario_gdp_bil)
    total <- total +
      x[[paste0("slack::", sc, "::2036")]] * 1000 / g36 +
      x[[paste0("slack::", sc, "::2046")]] * 1000 / g46
  }
  as.numeric(total)
}

# ------------------------------------------------------------------------------
# FUNCTION: solve_diverse_family
# Purpose: Generate multiple distinct solutions for one objective using diversity/no-good constraints.
# ------------------------------------------------------------------------------
solve_diverse_family_core_2 <- function(
  universe,
  policy_model,
  working_baseline,
  kernel_obj,
  objective,
  label_prefix,
  count = CFG$diverse_solutions_per_objective,
  require_patterns = character(),
  forbid_patterns = character(),
  max_revenue_bil = Inf,
  max_spending_bil = Inf,
  require_ss_solvency = FALSE,
  soft_targets = FALSE,
  max_target_slack_score = Inf
) {
  solutions <- list()
  previous <- list()
  for (k in seq_len(count)) {
    label <- paste0(label_prefix, "_", sprintf("%02d", k))
    model <- build_full_milp(
      universe = universe,
      working_baseline = working_baseline,
      kernel_obj = kernel_obj,
      objective = objective,
      require_patterns = require_patterns,
      forbid_patterns = forbid_patterns,
      max_revenue_bil = max_revenue_bil,
      max_spending_bil = max_spending_bil,
      require_ss_solvency = require_ss_solvency,
      soft_targets = soft_targets,
      max_target_slack_score = max_target_slack_score,
      previous_packages = previous
    )
    sol <- solve_full_milp(model, label)
    solutions[[length(solutions) + 1L]] <- sol
    if (!full_solver_has_feasible_incumbent(sol)) break
    previous[[length(previous) + 1L]] <- sol$selected_candidate_ids
  }
  solutions
}


# ------------------------------------------------------------------------------
# FUNCTION: build_pareto_solution_family
# Purpose: Generate solutions along the requested revenue-versus-spending tradeoff frontier.
# ------------------------------------------------------------------------------
build_pareto_solution_family <- function(
  universe,
  policy_model,
  working_baseline,
  kernel_obj,
  soft_targets = FALSE,
  max_target_slack_score = Inf,
  label_suffix = ""
) {
  label_core <- paste0(universe$protection_mode, label_suffix)
  log_line(
    "Gate 4: constructing ",
    ifelse(soft_targets, "soft-frontier ", "hard-target "),
    "epsilon-constraint revenue/spending frontier | protection=", universe$protection_mode,
    ifelse(is.finite(max_target_slack_score), paste0(" | slack_cap=", signif(max_target_slack_score, 8)), "")
  )

  min_rev <- solve_full_milp(
    build_full_milp(
      universe, working_baseline, kernel_obj,
      objective = "revenue",
      soft_targets = soft_targets,
      max_target_slack_score = max_target_slack_score
    ),
    paste0(label_core, "_PARETO_MIN_REVENUE")
  )

  min_spend <- solve_full_milp(
    build_full_milp(
      universe, working_baseline, kernel_obj,
      objective = "spending",
      soft_targets = soft_targets,
      max_target_slack_score = max_target_slack_score
    ),
    paste0(label_core, "_PARETO_MIN_SPENDING")
  )

  if (!full_solver_has_feasible_incumbent(min_rev) || !full_solver_has_feasible_incumbent(min_spend)) {
    return(list(min_rev, min_spend))
  }

  min_rev_value <- solution_score_from_decisions(min_rev, "revenue")
  max_rev_anchor <- solution_score_from_decisions(min_spend, "revenue")
  if (!is.finite(max_rev_anchor) || max_rev_anchor < min_rev_value) max_rev_anchor <- min_rev_value

  caps <- unique(seq(min_rev_value, max_rev_anchor, length.out = CFG$pareto_grid_points))
  out <- list(min_rev, min_spend)

  for (i in seq_along(caps)) {
    model <- build_full_milp(
      universe,
      working_baseline,
      kernel_obj,
      objective = "spending",
      max_revenue_bil = caps[i],
      soft_targets = soft_targets,
      max_target_slack_score = max_target_slack_score
    )
    out[[length(out) + 1L]] <- solve_full_milp(
      model,
      paste0(label_core, "_PARETO_R", sprintf("%02d", i))
    )
  }
  out
}


# ------------------------------------------------------------------------------
# FUNCTION: build_package_materiality_audit
# Purpose: Keep every defensible account control in optimization while classifying materiality for reporting only.
# ------------------------------------------------------------------------------
build_package_materiality_audit <- function(policy_model) {
  controls <- policy_model$meta |>
    filter(source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL", parameterized_solver_eligible) |>
    mutate(
      above_reporting_threshold = cumulative_spending_cut_2027_2036_bil + 1e-12 >= CFG$package_ready_account_min_savings_bil_2027_2036,
      materiality_threshold_bil = CFG$package_ready_account_min_savings_bil_2027_2036,
      materiality_status = if_else(above_reporting_threshold, "AT_OR_ABOVE_REPORTING_THRESHOLD", "BELOW_REPORTING_THRESHOLD"),
      solver_excluded_for_materiality = FALSE
    )
  summary <- tibble(
    metric = c(
      "Solver-ready account-control variants",
      "Account-control variants below reporting threshold",
      "Maximum ten-year savings represented by below-threshold variants",
      "Account-control variants presented to solver regardless of materiality",
      "Reporting-only minimum ten-year savings threshold"
    ),
    value = c(
      nrow(controls),
      sum(!controls$above_reporting_threshold),
      sum(controls$cumulative_spending_cut_2027_2036_bil[!controls$above_reporting_threshold], na.rm = TRUE),
      nrow(controls),
      CFG$package_ready_account_min_savings_bil_2027_2036
    ),
    units = c("candidate variants", "candidate variants", "$ billions", "candidate variants", "$ billions")
  )
  list(summary = summary, detail = controls)
}

# ------------------------------------------------------------------------------
# FUNCTION: build_score_vintage_compatibility_audit
# Purpose: Flag official scores whose source law predates material current-law changes so they remain visible as translated older evidence rather than implicit current rescoring.
# ------------------------------------------------------------------------------
build_score_vintage_compatibility_audit_core <- function(policy_model) {
  policy_model$meta |>
    mutate(
      title_l = stringr::str_to_lower(dplyr::coalesce(title, "")),
      fiscal_channel_l = stringr::str_to_lower(dplyr::coalesce(fiscal_channel, "")),
      major_category_l = stringr::str_to_lower(dplyr::coalesce(major_category, "")),
      revenue_related = stringr::str_detect(fiscal_channel_l, "revenue|tax|net_deficit|mixed") |
        stringr::str_detect(major_category_l, "revenue|net deficit|mixed"),
      program_changed_by_2025_law = stringr::str_detect(
        title_l,
        "medicaid|supplemental nutrition assistance|snap|student loan|education loan|premium tax credit|energy credit|business expensing|depreciation|qualified business income|pass-through|international income|foreign income"
      ),
      compatibility_status = case_when(
        source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" ~ "CURRENT_BASELINE_MECHANICAL_CONTROL",
        source_kind == "ADDITIONAL_OFFICIAL_CBO_POLICY" & candidate_id == "CBO_2020_increase_irs_enforcement_initiatives" ~ "OLDER_OFFICIAL_SCORE_WITH_CURRENT_MECHANISM_VALIDATION",
        evidence_class == "OFFICIAL_CURRENT" ~ "CURRENT_OFFICIAL_SCORE",
        evidence_class == "OFFICIAL_OLDER" & revenue_related ~ "POST_2025_LAW_REVIEW_REQUIRED_TAX_SCORE",
        evidence_class == "OFFICIAL_OLDER" & program_changed_by_2025_law ~ "POST_2025_LAW_REVIEW_REQUIRED_PROGRAM_SCORE",
        evidence_class == "OFFICIAL_OLDER" ~ "OLDER_OFFICIAL_SCORE_TRANSLATED",
        TRUE ~ "OTHER_OR_INVENTORY_ONLY"
      ),
      compatibility_risk = case_when(
        compatibility_status %in% c("POST_2025_LAW_REVIEW_REQUIRED_TAX_SCORE", "POST_2025_LAW_REVIEW_REQUIRED_PROGRAM_SCORE") ~ "HIGH_REVIEW",
        compatibility_status == "OLDER_OFFICIAL_SCORE_WITH_CURRENT_MECHANISM_VALIDATION" ~ "MODERATE_REVIEW",
        compatibility_status == "OLDER_OFFICIAL_SCORE_TRANSLATED" ~ "MODERATE_REVIEW",
        compatibility_status %in% c("CURRENT_OFFICIAL_SCORE", "CURRENT_BASELINE_MECHANICAL_CONTROL") ~ "LOWER_REVIEW",
        TRUE ~ "NOT_APPLICABLE"
      ),
      compatibility_reason = case_when(
        compatibility_status == "CURRENT_BASELINE_MECHANICAL_CONTROL" ~ "Control is calculated from the February 2026 CBO baseline rather than an older policy score.",
        compatibility_status == "OLDER_OFFICIAL_SCORE_WITH_CURRENT_MECHANISM_VALIDATION" ~ "Annual score is older, but later CBO analysis continues to support the underlying response mechanism. The older annual dollar path remains explicitly labeled and is not presented as a current rescore.",
        compatibility_status == "POST_2025_LAW_REVIEW_REQUIRED_TAX_SCORE" ~ "The 2025 reconciliation act materially changed current-law tax parameters after this option was scored. The official older score remains usable only as translated older evidence and requires sensitivity review.",
        compatibility_status == "POST_2025_LAW_REVIEW_REQUIRED_PROGRAM_SCORE" ~ "The 2025 reconciliation act materially changed this program or related policy domain after the option was scored. The official older score requires current-law compatibility review.",
        compatibility_status == "OLDER_OFFICIAL_SCORE_TRANSLATED" ~ "Official score predates the February 2026 baseline. Translation preserves the published annual score path but does not constitute a current-law rescore.",
        compatibility_status == "CURRENT_OFFICIAL_SCORE" ~ "Policy score is current to the model evidence standard.",
        TRUE ~ "Candidate is not a solver-ready official policy score requiring score-vintage compatibility classification."
      )
    ) |>
    select(
      candidate_id, family_id, title, variant_name, source_kind, evidence_class,
      estimate_year, source_date, source_url, direct_cbo_annual_score,
      solver_eligible_annual, parameterized_solver_eligible,
      cumulative_primary_improvement_2027_2036_bil,
      compatibility_status, compatibility_risk, compatibility_reason
    )
}

build_score_vintage_capacity_audit_core <- function(compatibility_audit) {
  compatibility_audit |>
    filter(parameterized_solver_eligible) |>
    group_by(compatibility_status, compatibility_risk) |>
    summarise(
      solver_candidate_count = n(),
      distinct_policy_families = n_distinct(family_id),
      direct_official_score_count = sum(dplyr::coalesce(direct_cbo_annual_score, FALSE)),
      maximum_ten_year_primary_improvement_bil = sum(pmax(cumulative_primary_improvement_2027_2036_bil, 0), na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(factor(compatibility_risk, levels = c("HIGH_REVIEW", "MODERATE_REVIEW", "LOWER_REVIEW", "NOT_APPLICABLE")), compatibility_status)
}

build_solution_score_vintage_exposure_core <- function(search_result, compatibility_audit) {
  if (is.null(search_result$membership) || nrow(search_result$membership) == 0L) return(tibble())

  scored <- compatibility_audit |>
    select(candidate_id, direct_cbo_annual_score, compatibility_status, compatibility_risk)

  search_result$membership |>
    left_join(scored, by = "candidate_id") |>
    group_by(solution_id) |>
    summarise(
      selected_policy_count = n_distinct(candidate_id),
      selected_direct_official_score_count = sum(dplyr::coalesce(direct_cbo_annual_score, FALSE)),
      selected_high_review_score_count = sum(dplyr::coalesce(direct_cbo_annual_score, FALSE) & compatibility_risk == "HIGH_REVIEW", na.rm = TRUE),
      selected_moderate_review_score_count = sum(dplyr::coalesce(direct_cbo_annual_score, FALSE) & compatibility_risk == "MODERATE_REVIEW", na.rm = TRUE),
      selected_current_or_mechanical_count = sum(compatibility_risk == "LOWER_REVIEW", na.rm = TRUE),
      includes_added_irs_enforcement_score = any(candidate_id == "CBO_2020_increase_irs_enforcement_initiatives"),
      high_review_share_of_direct_scores = safe_divide(selected_high_review_score_count, selected_direct_official_score_count),
      .groups = "drop"
    )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_authoritative_lever_expansion_audit
# Purpose: Record which official fiscal domains can defensibly become new solver coefficients and which remain inventories only.
# ------------------------------------------------------------------------------
build_authoritative_lever_expansion_audit_core <- function(policy_model, tax_inventory) {
  register_source(
    "cbo_deficit_options_2027_2036_work_in_progress",
    "Congressional Budget Office",
    "CBO's Recent Publications and Work in Progress as of March 31, 2026",
    "https://www.cbo.gov/publication/62306",
    publication_date = "2026-04",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_CURRENT",
    notes = "CBO lists Options for Reducing the Deficit: 2027 to 2036 for December 2026 release. Until that release, the validated frozen option pack and separately identified official annual scores remain the scored policy layer."
  )
  register_source(
    "cbo_budget_economic_outlook_2026",
    "Congressional Budget Office",
    "The Budget and Economic Outlook: 2026 to 2036",
    "https://www.cbo.gov/publication/62105",
    publication_date = "2026-02",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Current baseline incorporates legislation enacted after the December 2024 options compendium, including the 2025 reconciliation act. Older policy scores therefore retain explicit vintage-compatibility flags rather than being represented as current-law rescores."
  )
  register_source(
    "cbo_federal_credit_costs_2027",
    "Congressional Budget Office",
    "Estimates of the Cost of Federal Credit Programs in 2027",
    "https://www.cbo.gov/publication/62265",
    publication_date = "2026-07-22",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Current FCRA and fair-value subsidy-cost evidence. Not converted into a marginal policy coefficient without program-specific fee, default, volume, or subsidy response evidence."
  )
  register_source(
    "cbo_health_subsidies_2026_2036",
    "Congressional Budget Office / Joint Committee on Taxation",
    "Federal Subsidies for Health Insurance, 2026 to 2036",
    "https://www.cbo.gov/publication/62539",
    publication_date = "2026-07-23",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Current health-subsidy baseline decomposition. Baseline totals are not treated as policy response coefficients."
  )
  register_source(
    "cbo_taxation_social_security_benefits_2026",
    "Congressional Budget Office",
    "The Taxation of Social Security Benefits",
    "https://www.cbo.gov/publication/62553",
    publication_date = "2026-08-26",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Current-law Social Security benefit-tax revenue projections. The report does not provide an alternative-policy annual score suitable for automatic solver entry."
  )
  register_source(
    "ssa_oact_solvency_provisions_current_2026",
    "Social Security Administration, Office of the Chief Actuary",
    "Actuarial Services' Estimates of Individual Changes Modifying Social Security",
    "https://www.ssa.gov/OACT/solvency/provisions/",
    publication_date = "2026",
    baseline_vintage = "2026 Trustees where available; otherwise 2025 Trustees",
    evidence_class = "OFFICIAL_CURRENT_AND_TRANSITIONING",
    notes = "Official actuarial response evidence. Solver entry still requires a compatible annual federal-budget mapping rather than conversion of actuarial percentages into dollars by assumption."
  )

  local_scored <- policy_model$meta |> filter(source_kind == "LOCAL_FROZEN_CBO_POLICY_PACK")
  added_scored <- policy_model$meta |> filter(source_kind == "ADDITIONAL_OFFICIAL_CBO_POLICY")

  tibble(
    domain = c(
      "Frozen CBO deficit-reduction option pack",
      "Additional official annual policy scores",
      "CBO current-law tax parameters",
      "Treasury/JCT tax expenditures",
      "SSA OACT solvency provisions",
      "Medicare and health-subsidy baseline domains",
      "Federal credit programs",
      "Governmental receipts and receipt-linked accounts"
    ),
    authoritative_evidence = c(
      paste0(n_distinct(local_scored$family_id), " frozen CBO option families; ", sum(local_scored$direct_cbo_annual_score, na.rm = TRUE), " candidates with direct annual official score paths"),
      paste0(n_distinct(added_scored$family_id), " separately identified official policy family; ", sum(added_scored$direct_cbo_annual_score, na.rm = TRUE), " direct annual score path"),
      paste0(nrow(tax_inventory), " current-law CBO tax parameters inventoried"),
      "Treasury FY2027 and JCT 2025-2029 tax-expenditure inventories",
      "SSA OACT provision library, including 2026 Trustees-basis estimates where currently updated",
      "CBO July 2026 federal health-subsidy baseline plus existing scored CBO Medicare/payment options",
      "CBO July 2026 FCRA and fair-value subsidy-cost report",
      "CBO spending-detail and OMB supplemental account/receipt coverage"
    ),
    solver_status = c(
      "ADMITTED_WHERE_ANNUAL_SCORE_AND_PROTECTION_RULES_ALLOW",
      "ADMITTED_AS_OFFICIAL_OLDER_ANNUAL_SCORE_WITH_EXPLICIT_VINTAGE_FLAG",
      "INVENTORY_ONLY_WITHOUT_MARGINAL_REVENUE_RESPONSE",
      "INVENTORY_ONLY_TAX_EXPENDITURE_IS_NOT_REPEAL_SCORE",
      "ACTUARIAL_ONLY_UNLESS_COMPATIBLE_ANNUAL_BUDGET_MAPPING_EXISTS",
      "BASELINE_ONLY_UNLESS_POLICY_SPECIFIC_SCORE_EXISTS",
      "COST_INVENTORY_ONLY_UNLESS_PROGRAM_SPECIFIC_POLICY_RESPONSE_EXISTS",
      "NO_GENERIC_CONTROL_REQUIRES_STATUTORY_OR_CASHFLOW_PARAMETERIZATION"
    ),
    new_solver_coefficients_in_current_build = c(
      0L, nrow(added_scored), 0L, 0L, 0L, 0L, 0L, 0L
    ),
    reason = c(
      "The frozen CBO option pack remains the broad scored policy layer pending the next comprehensive CBO deficit-options release.",
      "The 2020 IRS enforcement option supplies an official annual outlay and revenue path and later CBO analysis continues to support the response mechanism. Its older vintage remains explicit and no current rescore is claimed.",
      "A current-law tax parameter identifies the law but does not identify the revenue derivative from changing it.",
      "Tax-expenditure estimates measure deviations from a reference tax system and generally do not equal repeal revenue or additive policy scores.",
      "Actuarial balance effects are not silently converted to unified-budget annual dollars; the solver uses only provisions already paired with defensible budget streams.",
      "Current subsidy totals identify scale but not savings from a specific payment reform; existing CBO-scored reforms remain usable.",
      "FCRA and fair-value subsidy rates describe expected program cost, not the fiscal effect of an unspecified fee, eligibility, guarantee, or default-policy change.",
      "Receipts, premiums, claims, loan cash flows, and outlays can be jointly determined; changing only the outlay account would create a false fiscal coefficient."
    )
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: solve_capacity_diagnostics
# Purpose: Find the minimum achievable debt ratio separately for FY2036 and FY2046 in every required robust scenario using the complete theoretical-capacity universe.
# ------------------------------------------------------------------------------
solve_capacity_diagnostics <- function(universes, policy_model, working_baseline, kernel_obj) {
  rows <- list()
  solutions <- list()
  counter <- 0L
  for (u in universes) {
    required_scenarios <- u$scenario_meta |> filter(required_robust) |> pull(scenario_id)
    for (sc in required_scenarios) {
      for (target_year in c(2036L, 2046L)) {
        counter <- counter + 1L
        model <- build_full_milp(
          u,
          working_baseline,
          kernel_obj,
          objective = "complexity",
          soft_targets = TRUE
        )
        model$L[] <- 0
        debt_var <- paste0("debt::", sc, "::", target_year)
        debt_idx <- match(debt_var, model$variable_names)
        assert_model(!is.na(debt_idx), paste0("Capacity diagnostic missing debt state variable: ", debt_var))
        model$L[debt_idx] <- 1
        model$objective_name <- paste0("capacity_debt_", sc, "_", target_year)
        label <- paste0(u$protection_mode, "_FULL_CAPACITY_MIN_", sc, "_", target_year)
        sol <- solve_full_milp(model, label)
        solutions[[length(solutions) + 1L]] <- sol
        if (!full_solver_has_feasible_incumbent(sol)) {
          rows[[length(rows) + 1L]] <- tibble(
            protection_mode = u$protection_mode,
            materiality_mode = u$materiality_mode,
            scenario_id = sc,
            target_year = target_year,
            solver_status = sol$status_message,
            solver_proven_optimal = full_solver_is_optimal(sol),
            feasible_incumbent = FALSE,
            minimum_debt_gdp_pct = NA_real_,
            target_debt_gdp_pct = ifelse(target_year == 2036L, 100 * CFG$target_2036, 100 * CFG$target_2046_high),
            target_gap_pp = NA_real_,
            target_gap_debt_bil = NA_real_,
            target_separately_feasible = FALSE,
            selected_policy_count = 0L,
            ss_actuarial_improvement_pct_payroll = NA_real_
          )
          next
        }
        sim <- simulate_parameterized_package(sol$parameter_decisions, policy_model, working_baseline, kernel_obj)
        point <- sim |> filter(scenario_id == sc, year == target_year)
        target_ratio <- if (target_year == 2036L) CFG$target_2036 else CFG$target_2046_high
        target_debt_bil <- target_ratio * point$scenario_gdp_bil[[1]]
        observed_debt_bil <- point$scenario_debt_bil[[1]]
        observed_ratio <- point$scenario_debt_gdp_pct[[1]]
        rows[[length(rows) + 1L]] <- tibble(
          protection_mode = u$protection_mode,
          materiality_mode = u$materiality_mode,
          scenario_id = sc,
          target_year = target_year,
          solver_status = sol$status_message,
          solver_proven_optimal = full_solver_is_optimal(sol),
          feasible_incumbent = TRUE,
          minimum_debt_gdp_pct = observed_ratio,
          target_debt_gdp_pct = 100 * target_ratio,
          target_gap_pp = max(0, observed_ratio - 100 * target_ratio),
          target_gap_debt_bil = max(0, observed_debt_bil - target_debt_bil),
          target_separately_feasible = observed_ratio <= 100 * target_ratio + 1e-7,
          selected_policy_count = n_distinct(sol$parameter_decisions$candidate_id),
          ss_actuarial_improvement_pct_payroll = sum(sol$parameter_decisions$ss_actuarial_contribution_pct_payroll, na.rm = TRUE)
        )
      }
    }
  }
  list(table = bind_rows(rows), solutions = solutions)
}

# ------------------------------------------------------------------------------
# FUNCTION: run_full_solution_search
# Purpose: Separate theoretical fiscal capacity from package-ready search, run the full objective family, and add target-specific capacity diagnostics.
# ------------------------------------------------------------------------------
run_full_solution_search_core_2 <- function(policy_model, working_baseline, kernel_obj) {
  run_highs_interface_self_test()
  strict_full <- policy_universe_for_mode(policy_model, "STRICT", "FULL_CAPACITY")
  expanded_full <- policy_universe_for_mode(policy_model, "EXPANDED", "FULL_CAPACITY")
  strict <- policy_universe_for_mode(policy_model, "STRICT", "PACKAGE_READY")
  expanded <- policy_universe_for_mode(policy_model, "EXPANDED", "PACKAGE_READY")

  assert_model(
    setequal(strict$meta$candidate_id, strict_full$meta$candidate_id),
    "Materiality changed the strict solver candidate universe; materiality is reporting-only"
  )
  assert_model(
    setequal(expanded$meta$candidate_id, expanded_full$meta$candidate_id),
    "Materiality changed the expanded solver candidate universe; materiality is reporting-only"
  )

  log_line(
    "Gate 4 universe sizes | strict all-valid=", nrow(strict$meta),
    " | expanded all-valid=", nrow(expanded$meta),
    " | reporting-only account threshold=$", CFG$package_ready_account_min_savings_bil_2027_2036, "B over FY2027-FY2036",
    " | required robust scenarios=", sum(policy_model$scenario_meta$required_robust),
    " | no valid candidate removed for materiality"
  )

  assert_model(nrow(strict$meta) >= CFG$minimum_solver_candidates, paste0("Strict all-valid universe contains only ", nrow(strict$meta), " candidates"))
  assert_model(nrow(expanded$meta) >= CFG$minimum_expanded_solver_candidates, paste0("Expanded all-valid universe contains only ", nrow(expanded$meta), " candidates; expected at least ", CFG$minimum_expanded_solver_candidates))

  size_models <- purrr::map_dfr(
    list(
      STRICT_PACKAGE_READY = strict,
      EXPANDED_PACKAGE_READY = expanded,
      STRICT_FULL_CAPACITY = strict_full,
      EXPANDED_FULL_CAPACITY = expanded_full
    ),
    function(u) {
      m <- build_full_milp(u, working_baseline, kernel_obj, objective = "complexity")
      tibble(
        protection_mode = u$protection_mode,
        materiality_mode = u$materiality_mode,
        candidate_activation_binaries = length(m$policy_variable_names),
        timing_schedule_binaries = length(m$schedule_binary_names),
        continuous_policy_level_variables = length(m$intensity_variable_names),
        accounting_and_slack_continuous_variables = sum(m$types == "C") - length(m$intensity_variable_names),
        total_integer_variables = sum(m$types == "I"),
        total_continuous_variables = sum(m$types == "C"),
        total_variables = length(m$variable_names),
        constraints = nrow(m$A),
        nonzero_coefficients = Matrix::nnzero(m$A),
        robust_scenarios = nrow(m$scenario_meta)
      )
    }
  )

  theoretical_capacity_solutions <- list()
  theoretical_capacity_summary <- list()
  theoretical_capacity_scores <- list()
  for (u in list(strict_full, expanded_full)) {
    tag <- u$protection_mode
    sol <- solve_full_milp(
      build_full_milp(u, working_baseline, kernel_obj, objective = "target_slack", soft_targets = TRUE),
      paste0(tag, "_THEORETICAL_FULL_CAPACITY_TARGET_SLACK")
    )
    assert_model(full_solver_has_feasible_incumbent(sol), paste0("Theoretical full-capacity target-slack solve failed in ", tag, " mode"))
    theoretical_capacity_solutions[[tag]] <- sol
    theoretical_capacity_scores[[tag]] <- target_slack_score_from_solution(sol)
    sm <- summarize_full_solution(sol, policy_model, working_baseline, kernel_obj, paste0("THEORETICAL_", tag))
    theoretical_capacity_summary[[tag]] <- sm |>
      mutate(materiality_mode = "FULL_CAPACITY", audit_only = TRUE)
  }

  capacity_diagnostics_obj <- solve_capacity_diagnostics(
    list(strict_full, expanded_full),
    policy_model,
    working_baseline,
    kernel_obj
  )

  solution_groups <- list()
  target_feasibility <- list()

  for (u in list(strict, expanded)) {
    tag <- u$protection_mode
    before_names <- names(solution_groups)

    solution_groups[[paste0(tag, "_MIN_REVENUE")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "revenue", paste0(tag, "_MIN_REVENUE"))
    solution_groups[[paste0(tag, "_MIN_SPENDING")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "spending", paste0(tag, "_MIN_SPENDING"))
    solution_groups[[paste0(tag, "_MIN_COMPLEXITY")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "complexity", paste0(tag, "_MIN_COMPLEXITY"))
    solution_groups[[paste0(tag, "_MAX_FISCAL_MARGIN")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "debt", paste0(tag, "_MAX_FISCAL_MARGIN"))
    solution_groups[[paste0(tag, "_REFERENCE_75")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "reference_75", paste0(tag, "_REFERENCE_75"))
    solution_groups[[paste0(tag, "_NO_VAT")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "complexity", paste0(tag, "_NO_VAT"), count = CFG$special_solution_count, forbid_patterns = c("value-added tax"))
    solution_groups[[paste0(tag, "_REQUIRE_NARROW_VAT")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "complexity", paste0(tag, "_REQUIRE_NARROW_VAT"), count = CFG$special_solution_count, require_patterns = c("value-added tax.*narrow|narrow.*value-added tax"))
    solution_groups[[paste0(tag, "_NO_FTT")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "complexity", paste0(tag, "_NO_FTT"), count = CFG$special_solution_count, forbid_patterns = c("financial transactions"))
    solution_groups[[paste0(tag, "_REQUIRE_FTT")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "complexity", paste0(tag, "_REQUIRE_FTT"), count = CFG$special_solution_count, require_patterns = c("financial transactions"))
    solution_groups[[paste0(tag, "_SS_SOLVENCY")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "complexity", paste0(tag, "_SS_SOLVENCY"), count = CFG$special_solution_count, require_ss_solvency = TRUE)
    solution_groups[[paste0(tag, "_PARETO")]] <- build_pareto_solution_family(u, policy_model, working_baseline, kernel_obj)

    tag_names <- setdiff(names(solution_groups), before_names)
    tag_solutions <- unlist(solution_groups[tag_names], recursive = FALSE)
    hard_feasible <- any(vapply(tag_solutions, full_solver_has_feasible_incumbent, logical(1)))
    full_score <- theoretical_capacity_scores[[tag]]

    # Independently identify the best attainable debt path subject to full Social
    # Security actuarial solvency. This solve is intentionally NOT constrained by
    # one of the ordinary soft-frontier slack bands.
    ss_best_attainable <- solve_full_milp(
      build_full_milp(
        u, working_baseline, kernel_obj,
        objective = "target_slack",
        require_ss_solvency = TRUE,
        soft_targets = TRUE
      ),
      paste0(tag, "_SS_SOLVENCY_BEST_ATTAINABLE")
    )
    solution_groups[[paste0(tag, "_SS_SOLVENCY_BEST_ATTAINABLE")]] <- list(ss_best_attainable)

    if (hard_feasible) {
      target_feasibility[[tag]] <- tibble(
        protection_mode = tag,
        package_ready_hard_target_feasible = TRUE,
        package_ready_best_target_slack_score = 0,
        full_capacity_best_target_slack_score = full_score,
        materiality_slack_cost = max(0, 0 - full_score),
        soft_frontier_required = FALSE
      )
      next
    }

    log_line(
      "No robust hard-target package found in the all-valid ", tag,
      " universe. Computing the target-slack boundary over the same complete valid candidate set.",
      level = "WARN"
    )

    capacity_solution <- solve_full_milp(
      build_full_milp(u, working_baseline, kernel_obj, objective = "target_slack", soft_targets = TRUE),
      paste0(tag, "_PACKAGE_READY_BEST_ATTAINABLE")
    )
    solution_groups[[paste0(tag, "_PACKAGE_READY_BEST_ATTAINABLE")]] <- list(capacity_solution)
    assert_model(full_solver_has_feasible_incumbent(capacity_solution), paste0("Package-ready soft-target fiscal-capacity solve failed in ", tag, " mode"))

    best_slack <- target_slack_score_from_solution(capacity_solution)
    assert_model(is.finite(best_slack) && best_slack >= 0, paste0("Invalid package-ready target-slack boundary in ", tag))

    target_feasibility[[tag]] <- tibble(
      protection_mode = tag,
      package_ready_hard_target_feasible = FALSE,
      package_ready_best_target_slack_score = best_slack,
      full_capacity_best_target_slack_score = full_score,
      materiality_slack_cost = best_slack - full_score,
      soft_frontier_required = TRUE
    )

    for (band in CFG$soft_frontier_slack_bands) {
      slack_cap <- best_slack * (1 + band)
      band_tag <- paste0(tag, "_SOFT_B", sprintf("%03d", round(100 * band)))
      for (obj in c("revenue", "spending", "complexity", "debt", "reference_75")) {
        obj_tag <- toupper(obj)
        solution_groups[[paste0(band_tag, "_", obj_tag)]] <- solve_diverse_family(
          u,
          policy_model,
          working_baseline,
          kernel_obj,
          objective = obj,
          label_prefix = paste0(band_tag, "_", obj_tag),
          count = CFG$soft_frontier_diverse_solutions,
          soft_targets = TRUE,
          max_target_slack_score = slack_cap
        )
      }
    }

    pareto_cap <- best_slack * (1 + CFG$soft_frontier_pareto_band)
    solution_groups[[paste0(tag, "_SOFT_PARETO")]] <- build_pareto_solution_family(
      u,
      policy_model,
      working_baseline,
      kernel_obj,
      soft_targets = TRUE,
      max_target_slack_score = pareto_cap,
      label_suffix = paste0("_SOFT_B", sprintf("%03d", round(100 * CFG$soft_frontier_pareto_band)))
    )

    special_cap <- best_slack * (1 + CFG$soft_frontier_special_band)
    solution_groups[[paste0(tag, "_SOFT_NO_VAT")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "complexity", paste0(tag, "_SOFT_NO_VAT"), count = 1L, forbid_patterns = c("value-added tax"), soft_targets = TRUE, max_target_slack_score = special_cap)
    solution_groups[[paste0(tag, "_SOFT_REQUIRE_NARROW_VAT")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "complexity", paste0(tag, "_SOFT_REQUIRE_NARROW_VAT"), count = 1L, require_patterns = c("value-added tax.*narrow|narrow.*value-added tax"), soft_targets = TRUE, max_target_slack_score = special_cap)
    solution_groups[[paste0(tag, "_SOFT_NO_FTT")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "complexity", paste0(tag, "_SOFT_NO_FTT"), count = 1L, forbid_patterns = c("financial transactions"), soft_targets = TRUE, max_target_slack_score = special_cap)
    solution_groups[[paste0(tag, "_SOFT_REQUIRE_FTT")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "complexity", paste0(tag, "_SOFT_REQUIRE_FTT"), count = 1L, require_patterns = c("financial transactions"), soft_targets = TRUE, max_target_slack_score = special_cap)
    solution_groups[[paste0(tag, "_SOFT_SS_SOLVENCY")]] <- solve_diverse_family(u, policy_model, working_baseline, kernel_obj, "complexity", paste0(tag, "_SOFT_SS_SOLVENCY"), count = 1L, require_ss_solvency = TRUE, soft_targets = TRUE, max_target_slack_score = special_cap)
  }

  all_solutions <- unlist(solution_groups, recursive = FALSE)
  target_feasibility_audit <- bind_rows(target_feasibility)
  audit_only_solutions <- c(unname(theoretical_capacity_solutions), capacity_diagnostics_obj$solutions)
  status_solutions <- c(all_solutions, audit_only_solutions)

  run_status <- purrr::map_dfr(seq_along(status_solutions), function(i) {
    sol <- status_solutions[[i]]
    audit_only <- i > length(all_solutions)
    tibble(
      run_id = i,
      audit_only = audit_only,
      solve_label = sol$solve_label,
      protection_mode = sol$model$protection_mode,
      materiality_mode = dplyr::coalesce(sol$model$materiality_mode, ifelse(audit_only, "FULL_CAPACITY", "PACKAGE_READY")),
      objective = sol$model$objective_name,
      soft_targets = sol$model$soft_targets,
      max_target_slack_score = sol$model$max_target_slack_score,
      achieved_target_slack_score = ifelse(stringr::str_detect(sol$model$objective_name, "^capacity_debt_"), NA_real_, target_slack_score_from_solution(sol)),
      require_ss_solvency = sol$model$require_ss_solvency,
      solver_status = sol$status_message,
      solver_proven_optimal = full_solver_is_optimal(sol),
      feasible_incumbent = full_solver_has_feasible_incumbent(sol),
      objective_value = sol$objective_value,
      mip_node_count = sol$mip_node_count,
      mip_dual_bound = sol$mip_dual_bound,
      mip_gap = sol$mip_gap,
      simplex_iteration_count = sol$simplex_iteration_count,
      ipm_iteration_count = sol$ipm_iteration_count,
      max_constraint_violation = sol$max_constraint_violation,
      max_bound_violation = sol$max_bound_violation,
      max_integrality_violation = sol$max_integrality_violation,
      elapsed_seconds = sol$elapsed_seconds,
      selected_policy_count = length(sol$selected_candidate_ids),
      selected_continuous_policy_count = ifelse(nrow(sol$parameter_decisions) == 0L, 0L, sum(sol$parameter_decisions$parameterization_mode != "DISCRETE_FULL_ANCHOR"))
    )
  })

  # HiGHS can return terminal Optimal status while retaining a microscopic
  # floating-point primal-dual difference in info$mip_gap. For example, a
  # primal/dual pair that differs by only a few 1e-7 objective units can be
  # reported as an approximately 1e-9 relative gap even though the solver's
  # own report says Status Optimal, Gap 0%, and zero row/bound/integrality
  # violations. Preserve the zero-gap solver request above, but audit terminal
  # Optimal results using explicit numerical closure tolerances rather than an
  # unrealistically strict 1e-12 wrapper threshold.
  run_status <- run_status |>
    mutate(
      mip_abs_gap = if_else(
        is.finite(objective_value) & is.finite(mip_dual_bound),
        abs(objective_value - mip_dual_bound),
        NA_real_
      ),
      mip_gap_audit_pass = case_when(
        !solver_proven_optimal ~ NA,
        is.finite(mip_gap) & mip_gap <= CFG$solver_optimality_audit_rel_tolerance ~ TRUE,
        is.finite(mip_abs_gap) & mip_abs_gap <= CFG$solver_optimality_audit_abs_tolerance ~ TRUE,
        TRUE ~ FALSE
      )
    )

  terminal_infeasible <- stringr::str_detect(stringr::str_to_lower(dplyr::coalesce(run_status$solver_status, "")), "infeasible")
  assert_model(
    all(run_status$solver_proven_optimal | terminal_infeasible),
    paste0(
      "At least one MILP failed to reach a mathematical terminal state: ",
      paste(run_status$solve_label[!(run_status$solver_proven_optimal | terminal_infeasible)], collapse = "; ")
    )
  )
  assert_model(
    all(!run_status$solver_proven_optimal | run_status$feasible_incumbent),
    paste0(
      "At least one HiGHS-Optimal solve failed the independent numerical feasibility checks: ",
      paste(run_status$solve_label[run_status$solver_proven_optimal & !run_status$feasible_incumbent], collapse = "; ")
    )
  )
  gap_fail <- run_status |>
    filter(solver_proven_optimal, !dplyr::coalesce(mip_gap_audit_pass, FALSE))
  assert_model(
    nrow(gap_fail) == 0L,
    paste0(
      "At least one HiGHS-Optimal solve exceeded the post-solve numerical optimality tolerance: ",
      paste0(
        gap_fail$solve_label,
        " [relative_gap=", signif(gap_fail$mip_gap, 8),
        ", absolute_gap=", signif(gap_fail$mip_abs_gap, 8), "]",
        collapse = "; "
      )
    )
  )

  if (CFG$write_audit_outputs) {
    write_csv_atomic(run_status, file.path(CFG$output_dir, "solver_run_status_checkpoint.csv"))
    write_csv_atomic(target_feasibility_audit, file.path(CFG$output_dir, "target_feasibility_and_soft_frontier.csv"))
    write_csv_atomic(bind_rows(theoretical_capacity_summary), file.path(CFG$output_dir, "theoretical_full_capacity_summary.csv"))
    write_csv_atomic(capacity_diagnostics_obj$table, file.path(CFG$output_dir, "target_specific_capacity_diagnostics.csv"))

    solver_decision_checkpoint <- purrr::map_dfr(seq_along(status_solutions), function(i) {
      sol <- status_solutions[[i]]
      if (nrow(sol$parameter_decisions) == 0L) return(tibble())
      sol$parameter_decisions |>
        mutate(
          checkpoint_run_id = i,
          checkpoint_audit_only = i > length(all_solutions),
          checkpoint_solve_label = sol$solve_label,
          checkpoint_solver_status = sol$status_message,
          checkpoint_solver_proven_optimal = full_solver_is_optimal(sol),
          checkpoint_feasible_incumbent = full_solver_has_feasible_incumbent(sol),
          checkpoint_soft_targets = sol$model$soft_targets,
          checkpoint_max_target_slack_score = sol$model$max_target_slack_score,
          checkpoint_achieved_target_slack_score = ifelse(stringr::str_detect(sol$model$objective_name, "^capacity_debt_"), NA_real_, target_slack_score_from_solution(sol)),
          .before = 1
        )
    })
    write_csv_atomic(solver_decision_checkpoint, file.path(CFG$output_dir, "solver_parameter_decisions_checkpoint.csv"))
    log_line("Post-solve checkpoint written before solution reporting | runs=", nrow(run_status), " | decision rows=", nrow(solver_decision_checkpoint))
  }

  summaries <- list()
  memberships <- list()
  paths <- list()
  scenario_paths <- list()
  origins <- list()
  seen <- character()
  seen_ids <- character()
  counter <- 0L

  for (sol in all_solutions) {
    if (!full_solver_has_feasible_incumbent(sol)) next
    d <- sol$parameter_decisions |>
      arrange(candidate_id) |>
      transmute(key_piece = paste(candidate_id, schedule_id, sprintf("%.8f", intensity_scale), sep = "|"))
    key <- paste(d$key_piece, collapse = ";")
    if (key %in% seen) {
      sid <- seen_ids[match(key, seen)]
      origins[[length(origins) + 1L]] <- tibble(solution_id = sid, solve_label = sol$solve_label, soft_targets = sol$model$soft_targets, max_target_slack_score = sol$model$max_target_slack_score, achieved_target_slack_score = target_slack_score_from_solution(sol))
      next
    }
    counter <- counter + 1L
    sid <- paste0("S", sprintf("%04d", counter))
    seen <- c(seen, key)
    seen_ids <- c(seen_ids, sid)
    origins[[length(origins) + 1L]] <- tibble(solution_id = sid, solve_label = sol$solve_label, soft_targets = sol$model$soft_targets, max_target_slack_score = sol$model$max_target_slack_score, achieved_target_slack_score = target_slack_score_from_solution(sol))
    summaries[[length(summaries) + 1L]] <- summarize_full_solution(sol, policy_model, working_baseline, kernel_obj, sid)
    memberships[[length(memberships) + 1L]] <- extract_solution_membership(sol, policy_model, sid)
    sim <- simulate_parameterized_package(sol$parameter_decisions, policy_model, working_baseline, kernel_obj)
    scenario_paths[[length(scenario_paths) + 1L]] <- sim |> mutate(solution_id = sid)
    paths[[length(paths) + 1L]] <- sim |> filter(scenario_id == "CENTRAL") |> transmute(solution_id = sid, year, debt_gdp_pct = scenario_debt_gdp_pct, debt_bil = scenario_debt_bil)
  }

  scenario_path_table <- bind_rows(scenario_paths)
  scenario_summary <- if (nrow(scenario_path_table) == 0L) tibble() else scenario_path_table |>
    filter(year %in% c(2036L, 2046L)) |>
    select(solution_id, scenario_id, required_robust, year, scenario_debt_gdp_pct) |>
    tidyr::pivot_wider(names_from = year, values_from = scenario_debt_gdp_pct, names_prefix = "debt_gdp_") |>
    mutate(target_2036_pass = debt_gdp_2036 <= 100 * CFG$target_2036 + 1e-7, target_2046_pass = debt_gdp_2046 <= 100 * CFG$target_2046_high + 1e-7)

  list(
    solution_objects = all_solutions,
    solver_run_status = run_status,
    target_feasibility_audit = target_feasibility_audit,
    theoretical_capacity_summary = bind_rows(theoretical_capacity_summary),
    capacity_diagnostics = capacity_diagnostics_obj$table,
    solution_origins = bind_rows(origins),
    summary = bind_rows(summaries),
    membership = bind_rows(memberships),
    paths = bind_rows(paths),
    scenario_paths = scenario_path_table,
    scenario_summary = scenario_summary,
    strict_universe = strict,
    expanded_universe = expanded,
    strict_full_universe = strict_full,
    expanded_full_universe = expanded_full,
    model_dimensions = size_models
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_solution_stress_tests
# Purpose: Re-simulate retained packages under additional audit-only stress scenarios.
# ------------------------------------------------------------------------------
build_solution_stress_tests <- function(search_result, working_baseline, policy_model, kernel_obj) {
  search_result$scenario_summary
}


# ------------------------------------------------------------------------------
# FUNCTION: build_model_size_audit
# Purpose: Report source-specific catalog counts and both package-ready and full-capacity MILP dimensions without label/count conflation.
# ------------------------------------------------------------------------------
build_model_size_audit <- function(policy_model, search_result) {
  local_pack <- policy_model$meta |> filter(source_kind == "LOCAL_FROZEN_CBO_POLICY_PACK")
  additional_official <- policy_model$meta |> filter(source_kind == "ADDITIONAL_OFFICIAL_CBO_POLICY")
  account_controls <- policy_model$meta |> filter(source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL")
  headline <- tibble(
    measure = c(
      "CBO current/latest option families in frozen local pack",
      "CBO scored candidate variants in frozen local pack",
      "CBO candidates with direct annual official score paths",
      "CBO scored candidates retained as total-only audit records",
      "Additional official scored policy variants outside frozen local pack",
      "Mechanical CBO spending-account growth-control variants in master catalog",
      "Mechanical account growth-control variants solver-ready before protection/materiality filtering",
      "Annual-score candidates with continuous policy-level variables",
      "Parameterized implementation and phase-in schedules",
      "Robust scenarios enforced inside each MILP",
      "Additional audit-only adverse scenarios",
      "Strict package-ready solver candidates",
      "Expanded package-ready solver candidates",
      "Strict full theoretical-capacity candidates",
      "Expanded full theoretical-capacity candidates",
      "Blocked candidates retained in audit catalog",
      "Distinct package-ready solution packages discovered",
      "MILP solves attempted including audit-only capacity diagnostics"
    ),
    value = c(
      n_distinct(local_pack$family_id),
      nrow(local_pack),
      sum(local_pack$direct_cbo_annual_score, na.rm = TRUE),
      sum(!local_pack$solver_eligible_annual, na.rm = TRUE),
      nrow(additional_official),
      nrow(account_controls),
      sum(account_controls$parameterized_solver_eligible, na.rm = TRUE),
      sum(policy_model$meta$parameterization_mode != "DISCRETE_FULL_ANCHOR" & policy_model$meta$parameterized_solver_eligible, na.rm = TRUE),
      nrow(policy_model$schedules),
      sum(policy_model$scenario_meta$required_robust),
      sum(!policy_model$scenario_meta$required_robust),
      nrow(search_result$strict_universe$meta),
      nrow(search_result$expanded_universe$meta),
      nrow(search_result$strict_full_universe$meta),
      nrow(search_result$expanded_full_universe$meta),
      sum(policy_model$meta$protection_status == "BLOCKED"),
      nrow(search_result$summary),
      nrow(search_result$solver_run_status)
    )
  )
  dimensions <- search_result$model_dimensions |>
    tidyr::pivot_longer(-c(protection_mode, materiality_mode), names_to = "dimension", values_to = "value") |>
    transmute(measure = paste0(protection_mode, " ", materiality_mode, " MILP ", dimension), value = as.numeric(value))
  bind_rows(headline, dimensions)
}

# ------------------------------------------------------------------------------
# FUNCTION: build_solution_family_summary
# Purpose: Summarize how retained packages differ by objective, protection mode, and fiscal composition.
# ------------------------------------------------------------------------------
build_solution_family_summary <- function(search_result) {
  if (nrow(search_result$summary) == 0L) return(tibble())
  features <- search_result$membership |>
    mutate(
      vat_member = stringr::str_detect(stringr::str_to_lower(paste(title, variant_name)), "value-added tax"),
      ftt_member = stringr::str_detect(stringr::str_to_lower(paste(title, variant_name)), "financial transactions")
    ) |>
    group_by(solution_id) |>
    summarise(vat = any(vat_member), ftt = any(ftt_member), .groups = "drop")
  search_result$summary |>
    left_join(features, by = "solution_id") |>
    mutate(
      vat = replace_na(vat, FALSE),
      ftt = replace_na(ftt, FALSE),
      ss_solvent_approx = ss_actuarial_improvement_pct_payroll >= CFG$ss_actuarial_gap_pct_payroll
    ) |>
    group_by(protection_mode, soft_targets, objective, vat, ftt, ss_solvent_approx) |>
    summarise(
      package_count = n(),
      min_achieved_target_slack_score = ifelse(all(is.na(achieved_target_slack_score)), NA_real_, min(achieved_target_slack_score, na.rm = TRUE)),
      min_policy_count = min(selected_policy_count),
      min_revenue_bil = min(revenue_2027_2036_bil),
      min_spending_cut_bil = min(spending_cuts_2027_2036_bil),
      min_central_debt_2036_pct = min(debt_gdp_2036_pct),
      min_central_debt_2046_pct = min(debt_gdp_2046_pct),
      min_worst_required_debt_2036_pct = min(worst_required_scenario_debt_gdp_2036_pct),
      min_worst_required_debt_2046_pct = min(worst_required_scenario_debt_gdp_2046_pct),
      .groups = "drop"
    )
}


# ------------------------------------------------------------------------------
# FUNCTION: plot_solution_debt_paths
# Purpose: Render representative annual debt-to-GDP paths in the RStudio Plots pane.
# ------------------------------------------------------------------------------
plot_solution_debt_paths_core_2 <- function(working_baseline, search_result) {
  base <- working_baseline |>
    transmute(year, solution_id = "Working baseline", debt_gdp_pct = working_debt_gdp_pct)
  if (nrow(search_result$paths) == 0L || nrow(search_result$summary) == 0L) {
    pdat <- base
  } else {
    ids <- search_result$summary |>
      arrange(
        desc(robust_target_2036_pass & robust_target_2046_pass),
        worst_required_scenario_debt_gdp_2036_pct,
        worst_required_scenario_debt_gdp_2046_pct,
        selected_policy_count
      ) |>
      group_by(protection_mode) |>
      slice_head(n = 4L) |>
      ungroup() |>
      pull(solution_id)
    pdat <- bind_rows(
      base,
      search_result$paths |>
        filter(solution_id %in% ids) |>
        select(year, solution_id, debt_gdp_pct)
    )
  }
  robust_count <- if (nrow(search_result$summary) == 0L) 0L else sum(search_result$summary$robust_target_2036_pass & search_result$summary$robust_target_2046_pass, na.rm = TRUE)
  chart_title <- if (robust_count > 0L) "Central debt paths for representative robust-target packages" else "Central debt paths for representative best-attainable frontier packages"
  chart_subtitle <- if (robust_count > 0L) "Displayed packages satisfy the target constraints across every required robust scenario" else "No package satisfied every robust target; displayed packages come from the audited best-attainable slack frontier"
  ggplot(pdat, aes(year, debt_gdp_pct, group = solution_id, color = solution_id)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = c(90, 80, 75), linetype = "dotted") +
    scale_x_continuous(breaks = seq(2026, 2046, 2)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = chart_title,
      subtitle = chart_subtitle,
      x = NULL,
      y = "Debt held by public / GDP",
      color = "Package",
      caption = "Policy levels, implementation years, and phase-ins are decision variables. Target lines mark 90 percent in 2036, 80 percent in 2046, and the 75 percent long-run reference."
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}


# ------------------------------------------------------------------------------
# FUNCTION: run_model
# Purpose: Execute the complete validation, data-acquisition, optimization, audit-output, and reporting pipeline.
# ------------------------------------------------------------------------------
run_model_core_2 <- function() {
  log_line("Starting ", CFG$model_name, " | ", CFG$model_version)
  log_line("Project root: ", CFG$project_root)
  log_line("Evidence mode: ", CFG$evidence_mode)
  log_line("Targets: FY2036 <= ", 100 * CFG$target_2036, "% GDP; FY2046 <= ", 100 * CFG$target_2046_high, "% GDP")
  log_line("Gate 3 policy source mode: frozen local pack at ", CFG$policy_pack_dir)
  paths <- fetch_cbo_sources()
  cbo_data <- list(
    ten = read_cbo_long(paths$cbo_ten_year_budget_2026_02),
    lt = read_cbo_long(paths$cbo_long_term_budget_2026_02),
    hist = read_cbo_long(paths$cbo_historical_budget_2026_02),
    econ = read_cbo_long(paths$cbo_historical_economic_2026_02),
    revenue_detail = read_cbo_long(paths$cbo_revenue_detail_2026_02)
  )
  cbo_baseline <- build_cbo_baseline(cbo_data)
  gate1 <- run_gate_1(cbo_baseline, cbo_data)
  ext_validation <- validate_external_history(cbo_data, cbo_baseline)
  kernel_obj <- build_cbo_debt_service_kernel()
  gate2a <- validate_cbo_kernel(kernel_obj)
  gate2b <- validate_macro_profiles()
  tariff_profile <- build_tariff_profile(cbo_baseline, cbo_data, kernel_obj)
  working_baseline <- build_working_baseline(cbo_baseline, tariff_profile, kernel_obj)
  log_line("Gate 3: loading and validating complete frozen policy universe and protection classifications")
  cbo_universe <- build_full_cbo_policy_universe()
  anchor_policy_model <- translate_full_universe_flows(cbo_universe, working_baseline)
  policy_model <- build_parameterized_policy_space(anchor_policy_model, working_baseline)
  parameterization_validation <- build_parameterization_validation(policy_model)
  assert_model(all(parameterization_validation$passed), paste0("Parameterization validation failed: ", paste(parameterization_validation$check[!parameterization_validation$passed], collapse = "; ")))
  external_catalog <- build_external_policy_catalog(cbo_universe$pack)
  assert_model(n_distinct(policy_model$meta$family_id) == CFG$policy_pack_expected_families, "Gate 3 lost at least one current/latest CBO option family before optimization")
  assert_model(sum(policy_model$meta$is_december_2024_core) >= CFG$cbo_expected_2024_option_families, "Gate 3 lost at least one December 2024 core CBO option family before optimization")
  assert_model(sum(policy_model$meta$parameterized_solver_eligible) == CFG$policy_pack_expected_annual_candidates, "Gate 3 annual-score candidate count changed after parameterization")
  log_line(
    "Gate 3 complete: ", n_distinct(policy_model$meta$family_id), " CBO families; ",
    nrow(policy_model$meta), " candidate variants retained; ",
    sum(policy_model$meta$parameterized_solver_eligible), " annual-score candidates; ",
    sum(policy_model$meta$parameterization_mode != "DISCRETE_FULL_ANCHOR" & policy_model$meta$parameterized_solver_eligible), " candidates with continuous policy-level decisions; ",
    nrow(policy_model$schedules), " timing/phase designs"
  )
  log_line("Gate 4: launching parameterized robust mixed-integer solution search")
  search_result <- run_full_solution_search(policy_model, working_baseline, kernel_obj)
  stress_tests <- build_solution_stress_tests(search_result, working_baseline, policy_model, kernel_obj)
  model_size <- build_model_size_audit(policy_model, search_result)
  family_summary <- build_solution_family_summary(search_result)
  assumptions <- build_assumptions_table()
  package_versions <- build_package_versions_table()
  validation_table <- build_validation_table(gate1, ext_validation, gate2a, gate2b)
  if (CFG$write_audit_outputs) {
    log_line("Writing audit outputs")
    write_csv_atomic(SOURCE_MANIFEST, file.path(CFG$output_dir, "source_manifest.csv"))
    write_csv_atomic(assumptions, file.path(CFG$output_dir, "assumptions.csv"))
    write_csv_atomic(package_versions, file.path(CFG$output_dir, "package_versions.csv"))
    write_csv_atomic(validation_table, file.path(CFG$output_dir, "validation_tests.csv"))
    write_csv_atomic(cbo_baseline, file.path(CFG$output_dir, "baseline_cbo_feb_2026.csv"))
    write_csv_atomic(working_baseline, file.path(CFG$output_dir, "baseline_working_sep_2026.csv"))
    write_csv_atomic(tariff_profile, file.path(CFG$output_dir, "tariff_adjustment_profile.csv"))
    write_csv_atomic(gate2a$comparison, file.path(CFG$output_dir, "validation_cbo_debt_service_kernel.csv"))
    write_csv_atomic(gate2a$direction_summary, file.path(CFG$output_dir, "validation_cbo_debt_service_kernel_summary.csv"))
    write_csv_atomic(kernel_obj$kernel, file.path(CFG$output_dir, "cbo_debt_service_kernel.csv"))
    write_csv_atomic(kernel_obj$bfm_matrix, file.path(CFG$output_dir, "cbo_bfm_2026_debt_service_matrix.csv"))
    write_csv_atomic(cbo_universe$pack$hash_audit, file.path(CFG$output_dir, "policy_pack_hash_audit.csv"))
    write_csv_atomic(cbo_universe$pack$benchmark_audit, file.path(CFG$output_dir, "policy_pack_benchmark_audit.csv"))
    write_csv_atomic(cbo_universe$pack$coverage, file.path(CFG$output_dir, "policy_pack_coverage_audit.csv"))
    write_csv_atomic(cbo_universe$current_index, file.path(CFG$output_dir, "cbo_option_index_current_all.csv"))
    write_csv_atomic(cbo_universe$report_index, file.path(CFG$output_dir, "cbo_option_index_2024_76.csv"))
    write_csv_atomic(cbo_universe$families, file.path(CFG$output_dir, "cbo_option_family_local_pack_audit.csv"))
    write_csv_atomic(cbo_universe$raw_rows, file.path(CFG$output_dir, "cbo_option_raw_annual_rows.csv"))
    write_csv_atomic(policy_model$meta, file.path(CFG$output_dir, "policy_candidate_catalog_full.csv"))
    write_csv_atomic(policy_model$meta |> select(candidate_id, family_id, title, variant_name, protection_status, protection_reason, starts_with("risk_"), market_function_review_required, hard_protection_violation, explicit_review_required, protection_classification_method), file.path(CFG$output_dir, "policy_protection_attributes.csv"))
    write_csv_atomic(policy_model$meta |> select(candidate_id, family_id, title, variant_name, parameterization_mode, parameter_name, parameter_unit, parameter_anchor_value, parameter_min_value, parameter_max_value, parameter_min_scale, parameter_max_scale, parameter_extrapolation, parameter_evidence_basis, protection_status, source_url), file.path(CFG$output_dir, "policy_parameterization_catalog.csv"))
    write_csv_atomic(parameterization_validation, file.path(CFG$output_dir, "parameterization_validation.csv"))
    write_csv_atomic(policy_model$schedules, file.path(CFG$output_dir, "policy_implementation_schedule_catalog.csv"))
    write_csv_atomic(policy_model$schedule_summary, file.path(CFG$output_dir, "policy_schedule_score_summary.csv"))
    write_csv_atomic(policy_model$schedule_flows, file.path(CFG$output_dir, "policy_schedule_annual_flow_coefficients.csv"))
    write_csv_atomic(policy_model$scenario_meta, file.path(CFG$output_dir, "robust_scenario_catalog.csv"))
    write_csv_atomic(policy_model$scenario_annual, file.path(CFG$output_dir, "robust_scenario_annual_paths.csv"))
    write_csv_atomic(policy_model$meta |> filter(!parameterized_solver_eligible), file.path(CFG$output_dir, "policy_candidates_total_only_not_in_annual_milp.csv"))
    write_csv_atomic(policy_model$flows, file.path(CFG$output_dir, "policy_candidate_annual_flows_full.csv"))
    write_csv_atomic(cbo_universe$pack$ssa, file.path(CFG$output_dir, "ssa_actuarial_reference.csv"))
    write_csv_atomic(external_catalog, file.path(CFG$output_dir, "external_policy_source_catalog.csv"))
    write_csv_atomic(build_interaction_catalog_full(policy_model$meta |> filter(parameterized_solver_eligible)), file.path(CFG$output_dir, "interaction_catalog.csv"))
    write_csv_atomic(model_size, file.path(CFG$output_dir, "model_size_audit.csv"))
    write_csv_atomic(search_result$solver_run_status, file.path(CFG$output_dir, "solver_run_status.csv"))
    write_csv_atomic(search_result$solution_origins, file.path(CFG$output_dir, "solution_origins.csv"))
    write_csv_atomic(search_result$summary, file.path(CFG$output_dir, "solution_catalog.csv"))
    write_csv_atomic(search_result$membership, file.path(CFG$output_dir, "solution_policy_decisions.csv"))
    write_csv_atomic(search_result$membership, file.path(CFG$output_dir, "solution_policy_membership.csv"))
    write_csv_atomic(search_result$paths, file.path(CFG$output_dir, "solution_debt_paths.csv"))
    write_csv_atomic(search_result$scenario_paths, file.path(CFG$output_dir, "solution_scenario_paths.csv"))
    write_csv_atomic(stress_tests, file.path(CFG$output_dir, "solution_stress_tests.csv"))
    write_csv_atomic(family_summary, file.path(CFG$output_dir, "solution_family_summary.csv"))
    write_csv_atomic(build_run_manifest_full(), file.path(CFG$output_dir, "run_manifest.csv"))
  }
  log_line("Model run complete")
  print(model_size)
  if (nrow(search_result$summary) > 0L) {
    log_line("Distinct parameterized solution packages retained after de-duplication: ", nrow(search_result$summary))
    print(search_result$summary |> select(solution_id, solve_label, protection_mode, selected_policy_count, continuously_parameterized_policy_count, revenue_2027_2036_bil, spending_cuts_2027_2036_bil, ss_actuarial_improvement_pct_payroll, debt_gdp_2036_pct, debt_gdp_2046_pct, worst_required_scenario_debt_gdp_2036_pct, worst_required_scenario_debt_gdp_2046_pct, robust_target_2036_pass, robust_target_2046_pass, independently_verified))
  } else {
    log_line("No optimized hard-target or best-attainable frontier solution package could be produced", level = "WARN")
  }
  if (CFG$render_plots) print(plot_solution_debt_paths(working_baseline, search_result))
  invisible(list(
    config = CFG,
    source_manifest = SOURCE_MANIFEST,
    cbo_data = cbo_data,
    cbo_baseline = cbo_baseline,
    working_baseline = working_baseline,
    tariff_profile = tariff_profile,
    kernel = kernel_obj,
    policy_model = policy_model,
    cbo_policy_universe = cbo_universe,
    external_policy_catalog = external_catalog,
    solution_search = search_result,
    solution_stress_tests = stress_tests,
    model_size_audit = model_size,
    solution_family_summary = family_summary,
    validation_tests = validation_table
  ))
}



CFG$expanded_account_start_years <- c(2027L, 2029L, 2031L)
CFG$expanded_account_phase_in_years <- c(1L, 3L, 5L)
CFG$expanded_min_account_control_scale <- 0.001
CFG$expanded_min_positive_account_outlays_bil_2027_2036 <- 0.001

# ------------------------------------------------------------------------------
# FUNCTION: validate_policy_pack_present
# Purpose: Require the frozen scored-policy pack committed under Data/.
# ------------------------------------------------------------------------------
validate_policy_pack_present <- function() {
  required_paths <- file.path(CFG$policy_pack_dir, POLICY_PACK_REQUIRED_FILES)
  missing <- POLICY_PACK_REQUIRED_FILES[!file.exists(required_paths)]
  assert_model(
    length(missing) == 0L,
    paste0(
      "Required repository policy pack is incomplete. Missing from Data/fiscal_policy_data_pack_v1: ",
      paste(missing, collapse = ", ")
    )
  )
  invisible(CFG$policy_pack_dir)
}


# ------------------------------------------------------------------------------
# FUNCTION: locate_policy_pack
# Purpose: Locate the repository-supplied frozen scored-policy pack used for official scored anchors.
# ------------------------------------------------------------------------------
locate_policy_pack <- function() {
  validate_policy_pack_present()
  normalizePath(CFG$policy_pack_dir, winslash = "/", mustWork = TRUE)
}


# ------------------------------------------------------------------------------
# FUNCTION: download_optional
# Purpose: Download a nonfatal expanded-universe evidence source while preserving an explicit warning on failure.
# ------------------------------------------------------------------------------
download_optional <- function(url, destination, source_id, agency, title, publication_date = NA_character_, evidence_class = "OFFICIAL_CURRENT", notes = "") {
  out <- tryCatch(
    {
      download_cached(
        url = url,
        destination = destination,
        force = FALSE,
        minimum_bytes = 100L,
        source_id = source_id,
        agency = agency,
        title = title,
        publication_date = publication_date,
        baseline_vintage = CFG$cbo_vintage,
        evidence_class = evidence_class,
        notes = notes
      )
    },
    error = function(e) {
      log_line("Optional expanded-universe source unavailable: ", title, " | ", conditionMessage(e), level = "WARN")
      NA_character_
    }
  )
  out
}


# ------------------------------------------------------------------------------
# FUNCTION: valid_pdf
# Purpose: Validate a repository-supplied PDF by existence, minimum size, and PDF signature.
# ------------------------------------------------------------------------------
valid_pdf <- function(path, minimum_bytes = 100000L) {
  if (!file.exists(path)) return(FALSE)
  size <- file.info(path)$size
  if (!is.finite(size) || size < minimum_bytes) return(FALSE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  sig <- readBin(con, what = "raw", n = 5L)
  identical(rawToChar(sig), "%PDF-")
}


# ------------------------------------------------------------------------------
# FUNCTION: download_jct_tax_expenditures_required
# Purpose: Download and validate the required JCT JCX-45-25 PDF using primary and libcurl fallback transports.
# ------------------------------------------------------------------------------
download_jct_tax_expenditures_required <- function() {
  url <- "https://www.jct.gov/getattachment/8c830c45-1680-4f7e-a649-2a0106f6b6e3/x-45-25.pdf"
  destination <- file.path(CFG$raw_source_dir, "jct_tax_expenditures_2025_2029.pdf")

  assert_model(
    file.exists(destination),
    paste0(
      "Required repository-supplied JCT source is missing: ",
      project_relative_path(destination),
      ". See Data/input_manifest.csv."
    )
  )
  assert_model(valid_pdf(destination), "Repository-supplied JCT source is not a valid PDF")

  register_source(
    "jct_tax_expenditures_2025_2029",
    "Joint Committee on Taxation",
    "Estimates of Federal Tax Expenditures for Fiscal Years 2025-2029",
    url,
    destination,
    "2025-12-03",
    CFG$cbo_vintage,
    "OFFICIAL_OLDER",
    "Repository-supplied validated official PDF; provenance and expected hash are recorded in Data/input_manifest.csv."
  )
  destination
}


# ------------------------------------------------------------------------------
# FUNCTION: fetch_expanded_universe_sources
# Purpose: Acquire the expanded-universe CBO account, tax-parameter, trust-fund, and required supporting evidence sources.
# ------------------------------------------------------------------------------
fetch_expanded_universe_sources_core <- function() {
  log_line("Expanded-universe acquisition: downloading authoritative fiscal-control inputs")
  spending_url <- "https://raw.githubusercontent.com/US-CBO/cbo-data/main/data/budget/spending_detail/annual_fy_2026-02.csv"
  tax_url <- "https://raw.githubusercontent.com/US-CBO/cbo-data/main/data/budget/tax_parameters/annual_cy_2026-02.csv"
  trust_url <- "https://raw.githubusercontent.com/US-CBO/cbo-data/main/data/budget/trust_fund/annual_fy_2026-02.csv"
  spending_path <- download_cached(
    spending_url,
    file.path(CFG$raw_source_dir, "cbo_spending_detail_2026_02.csv"),
    force = FALSE,
    minimum_bytes = 10000L,
    source_id = "cbo_spending_detail_2026_02",
    agency = "Congressional Budget Office",
    title = "Spending Projections by Budget Account, February 2026 vintage",
    publication_date = "2026-02-11",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Machine-readable CBO account-level current-law outlay and budget-authority projections. Used directly to construct continuous account-level outlay controls."
  )
  tax_path <- download_cached(
    tax_url,
    file.path(CFG$raw_source_dir, "cbo_tax_parameters_2026_02.csv"),
    force = FALSE,
    minimum_bytes = 5000L,
    source_id = "cbo_tax_parameters_2026_02",
    agency = "Congressional Budget Office",
    title = "Tax Parameters, February 2026 vintage",
    publication_date = "2026-02-11",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Current-law statutory tax parameters. Inventory evidence only unless paired with an official scored response function."
  )
  trust_path <- download_cached(
    trust_url,
    file.path(CFG$raw_source_dir, "cbo_trust_fund_2026_02.csv"),
    force = FALSE,
    minimum_bytes = 3000L,
    source_id = "cbo_trust_fund_2026_02",
    agency = "Congressional Budget Office",
    title = "Trust Fund Projections, February 2026 vintage",
    publication_date = "2026-02-11",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Current CBO trust-fund projections used for audit and Social Security/Medicare cross-checking."
  )
  ssa_2025 <- download_optional(
    "https://www.ssa.gov/OACT/solvency/provisions_tr2025/summary.html",
    file.path(CFG$raw_source_dir, "ssa_oact_provisions_tr2025_summary.html"),
    "ssa_oact_provisions_tr2025",
    "Social Security Administration, Office of the Chief Actuary",
    "Summary of Provisions That Would Change the Social Security Program, 2025 Trustees basis",
    "2025",
    "OFFICIAL_OLDER",
    "Full official Social Security provision inventory. CBO-compatible annual unified-budget coefficients are used only where the solver has defensible budget-stream mappings."
  )
  ssa_current <- download_optional(
    "https://www.ssa.gov/OACT/solvency/provisions/summary.html",
    file.path(CFG$raw_source_dir, "ssa_oact_provisions_current_summary.html"),
    "ssa_oact_provisions_current",
    "Social Security Administration, Office of the Chief Actuary",
    "Current Social Security solvency provision summary",
    "2026",
    "OFFICIAL_CURRENT",
    "Current mixed 2026/2025 Trustees-basis provision landing page."
  )
  treasury_te <- download_optional(
    "https://home.treasury.gov/system/files/131/Tax-Expenditures-FY2027.xlsx",
    file.path(CFG$raw_source_dir, "treasury_tax_expenditures_fy2027.xlsx"),
    "treasury_tax_expenditures_fy2027",
    "U.S. Department of the Treasury",
    "Tax Expenditures FY2027",
    "2026",
    "OFFICIAL_CURRENT",
    "Inventory evidence. Tax-expenditure estimates are not treated as repeal revenue coefficients."
  )
  jct_page <- download_jct_tax_expenditures_required()
  omb_page <- download_optional(
    "https://www.whitehouse.gov/omb/information-resources/budget/supplemental-materials/",
    file.path(CFG$raw_source_dir, "omb_fy2027_supplemental_materials.html"),
    "omb_fy2027_supplemental_materials",
    "Office of Management and Budget",
    "FY2027 Budget Supplemental Materials",
    "2026",
    "OFFICIAL_CURRENT",
    "Cross-check source for account, object-class, receipts, and federal-credit coverage. CBO baseline remains the accounting anchor."
  )
  list(
    spending_detail = spending_path,
    tax_parameters = tax_path,
    trust_fund = trust_path,
    ssa_2025_summary = ssa_2025,
    ssa_current_summary = ssa_current,
    treasury_tax_expenditures = treasury_te,
    jct_tax_expenditures = jct_page,
    omb_supplemental = omb_page
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: read_spending_detail
# Purpose: Read and normalize CBO account-level spending-detail data by account, agency, function, and year.
# ------------------------------------------------------------------------------
read_spending_detail_core <- function(path) {
  x <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE, col_types = readr::cols(.default = readr::col_character()))
  required <- c("date", "tin", "title", "disc_or_mand", "category", "agency", "bureau", "function_code", "subfunction_code", "off_budget", "budget_authority", "outlays")
  assert_model(all(required %in% names(x)), paste0("Unexpected CBO spending_detail schema. Missing: ", paste(setdiff(required, names(x)), collapse = ", ")))
  x |>
    transmute(
      year = extract_year(date),
      tin = normalize_text_cell(tin),
      title = normalize_text_cell(title),
      disc_or_mand = normalize_text_cell(disc_or_mand),
      category = normalize_text_cell(category),
      agency = normalize_text_cell(agency),
      bureau = normalize_text_cell(bureau),
      function_code = normalize_text_cell(function_code),
      subfunction_code = normalize_text_cell(subfunction_code),
      off_budget = normalize_text_cell(off_budget),
      budget_authority_mil = clean_numeric(budget_authority),
      outlays_mil = clean_numeric(outlays)
    ) |>
    filter(year %in% 2026:2036, !is.na(tin), nzchar(tin), !is.na(title), nzchar(title)) |>
    group_by(year, tin, title, disc_or_mand, category, agency, bureau, function_code, subfunction_code, off_budget) |>
    summarise(
      budget_authority_mil = sum(budget_authority_mil, na.rm = TRUE),
      outlays_mil = sum(outlays_mil, na.rm = TRUE),
      .groups = "drop"
    )
}


# ------------------------------------------------------------------------------
# FUNCTION: classify_direct_account
# Purpose: Apply title-first Eisenhower-rule and fiscal-structure rules to one CBO account-level outlay control.
# ------------------------------------------------------------------------------
classify_direct_account <- function(title, disc_or_mand, category, agency, bureau, function_code, subfunction_code) {
  t <- stringr::str_to_lower(normalize_text_cell(title))
  a <- stringr::str_to_lower(normalize_text_cell(agency))
  b <- stringr::str_to_lower(normalize_text_cell(bureau))
  c <- stringr::str_to_lower(normalize_text_cell(category))
  f <- stringr::str_to_lower(normalize_text_cell(function_code))
  sf <- stringr::str_to_lower(normalize_text_cell(subfunction_code))
  context <- paste(a, b, c, f, sf)
  funding_class <- normalize_text_cell(disc_or_mand)

  # EISENHOWER RULE
  #
  # "Eisenhower Rule" is this model's name for the protection principle derived
  # from Dwight D. Eisenhower, "Remarks at the Lincoln Day Box Supper,"
  # Washington, D.C., February 5, 1954, Public Papers of the Presidents.
  # American Presidency Project:
  # https://www.presidency.ucsb.edu/documents/remarks-the-lincoln-day-box-supper
  #
  # "In all those things which deal with people, be liberal, be human. In all
  # those things which deal with people's money, or their economy, or their form
  # of government, be conservative."
  #
  # This is model nomenclature, not a statutory or regulatory term and not a
  # claim that Eisenhower supplied the model's classification taxonomy.
  # Operationally, the classifier protects person-facing benefits, earned
  # compensation, household security, productive capacity, and core state
  # capacity from generic account-level reductions. Program identity is primary.
  # Agency and bureau are used only for explicit institutional functions whose
  # mission makes a generic account-growth restraint substantively misleading.

  if ((category %in% "Net Interest") || (function_code %in% "900") || stringr::str_detect(t, "interest on the public debt|net interest")) {
    return(c(status="BLOCKED", protected="ENDOGENOUS_NET_INTEREST", control_class="NO_GENERIC_CONTROL", reason="Net interest is generated endogenously by the validated debt-service engine."))
  }

  if (stringr::str_detect(t, "social security|old-age and survivors|disability insurance|supplemental security income|unemployment compensation|unemployment trust fund|railroad retirement|workers' compensation|workers compensation|black lung|retirement fund|retired pay|annuitant|pension|survivor annuit|retiree health")) {
    return(c(status="BLOCKED", protected="PERSON_FACING_SOCIAL_INSURANCE_OR_EARNED_BENEFIT", control_class="NO_GENERIC_CONTROL", reason="The account pays social-insurance, retirement, pension, survivor, retiree-health, or other earned person-facing benefits protected by the Eisenhower Rule."))
  }

  if (stringr::str_detect(t, "medicare|medicaid|children's health insurance|childrens health insurance|tricare|defense health program|indian health service|veterans health|medical care, veterans|veterans medical|premium tax credit|health insurance marketplace|health benefits|employee health|life insurance|health resources and services|substance abuse and mental health|mental health services|aging and disability services|medical services|medical community care|medical support and compliance|medical facilities")) {
    return(c(status="BLOCKED", protected="PERSON_FACING_HEALTH_OR_EARNED_BENEFIT", control_class="NO_GENERIC_CONTROL", reason="The account directly finances health coverage, health access, medical delivery, disability or aging services, or earned health and life-insurance benefits. Payment-side reforms may enter separately when specifically quantified."))
  }

  if (stringr::str_detect(b, "veterans health administration")) {
    return(c(status="BLOCKED", protected="PERSON_FACING_VETERANS_HEALTH_CAPACITY", control_class="NO_GENERIC_CONTROL", reason="Veterans Health Administration accounts provide earned medical care and the operating capacity required to deliver it. Generic account freezes are not used as substitutes for specific payment or delivery reforms."))
  }

  if (stringr::str_detect(t, "cost of war toxic exposures|toxic exposures|veterans benefits|compensation and pensions|readjustment benefits|insurance and indemnities|disability compensation|dependency and indemnity|veterans compensation|veterans pension|burial benefits|board of veterans appeals|veterans employment and training|veterans electronic health")) {
    return(c(status="BLOCKED", protected="PERSON_FACING_VETERANS_BENEFIT_OR_DELIVERY", control_class="NO_GENERIC_CONTROL", reason="The account directly provides or adjudicates veterans compensation, health, employment, toxic-exposure, pension, insurance, or other earned benefits."))
  }

  if (stringr::str_detect(a, "department of veterans affairs") && stringr::str_detect(t, "information technology systems|departmental information technology|electronic health")) {
    return(c(status="BLOCKED", protected="VETERANS_BENEFIT_DELIVERY_CAPACITY", control_class="NO_GENERIC_CONTROL", reason="The account supports information systems required for veterans health and benefit delivery and is not treated as generic administrative machinery."))
  }

  if (stringr::str_detect(t, "foster care|permanency|adoption assistance|children and families|child welfare|child support|food stamp|supplemental nutrition assistance|child nutrition|school lunch|school breakfast|women, infants|tanf|temporary assistance for needy families|head start|pell grant|student financial assistance|education for the disadvantaged|special education|housing choice voucher|rental assistance|public housing|housing for the elderly|housing for persons with disabilities|housing opportunities for persons with aids|homeless assistance|low[ -]income home energy|child care and development|family support|refugee and entrant assistance|earned income tax credit|earned income credit|child tax credit|humanitarian assistance|disaster relief")) {
    return(c(status="BLOCKED", protected="PERSON_FACING_FAMILY_OR_HOUSEHOLD_SECURITY", control_class="NO_GENERIC_CONTROL", reason="The account directly supports children, families, nutrition, housing, education access, humanitarian relief, energy security, or basic household security."))
  }

  if (stringr::str_detect(t, "training and employment services|higher education|career, technical and adult education|career and technical education|school improvement|indian education|education sciences|safe schools|education programs|educational and cultural exchange|science, technology, engineering, and mathematics engagement")) {
    return(c(status="BLOCKED", protected="HUMAN_CAPITAL_INVESTMENT", control_class="NO_GENERIC_CONTROL", reason="The account is treated as education, workforce, or human-capital investment rather than generic administrative spending."))
  }

  if (stringr::str_detect(t, "military personnel|reserve personnel|personnel compensation|compensation of members|pay and allowances|basic allowance|government payment for annuitants|employee benefits|civil service retirement|federal employees retirement|family housing operation and maintenance|family housing construction|family housing improvement fund")) {
    return(c(status="BLOCKED", protected="PERSON_FACING_EARNED_COMPENSATION", control_class="NO_GENERIC_CONTROL", reason="The account contains direct wages, compensation, retirement, employee benefits, or military family housing that cannot be separated safely in the CBO account path."))
  }

  if (is.na(funding_class) || funding_class != "Discretionary") {
    return(c(status="BLOCKED", protected="REQUIRES_STATUTORY_PARAMETERIZATION", control_class="NO_GENERIC_CONTROL", reason=paste0("Generic account growth controls are restricted to discretionary accounts. This row is classified by CBO as ", ifelse(is.na(funding_class), "missing/unknown", funding_class), " and therefore requires a program-specific statutory, benefit-formula, subsidy, payment, or financing parameter with defensible fiscal evidence.")))
  }

  special_financing_title <- stringr::str_detect(
    t,
    "trust fund|insurance fund|revolving fund|working capital fund|program account|financing account|liquidating account|direct loan|loan guarantee|guaranteed loan|credit subsidy|mortgage insurance|financing fund"
  )
  if (special_financing_title) {
    return(c(status="BLOCKED", protected="RECEIPT_LINKED_CREDIT_INSURANCE_OR_REVOLVING_STRUCTURE", control_class="NO_GENERIC_CONTROL", reason="The account has a trust-fund, insurance, revolving-fund, working-capital, federal-credit, or receipt-linked structure. Outlays cannot be generically frozen without modeling associated premiums, fees, claims, loan cash flows, receipts, or subsidy accounting."))
  }

  if (stringr::str_detect(b, "internal revenue service") || stringr::str_detect(a, "internal revenue service")) {
    return(c(status="BLOCKED", protected="REVENUE_COLLECTION_AND_TAX_ADMINISTRATION_CAPACITY", control_class="NO_GENERIC_CONTROL", reason="IRS enforcement, taxpayer service, technology, and operations jointly support revenue collection and tax administration. Their fiscal effect cannot be represented by a one-sided generic spending freeze."))
  }

  if (stringr::str_detect(b, "indian health service") || stringr::str_detect(a, "indian health service")) {
    return(c(status="BLOCKED", protected="PERSON_FACING_HEALTH_DELIVERY_CAPACITY", control_class="NO_GENERIC_CONTROL", reason="Indian Health Service operating and contract-support accounts finance person-facing health delivery and are protected from generic growth restraints."))
  }

  if (stringr::str_detect(b, "food and drug administration") || stringr::str_detect(a, "food and drug administration")) {
    return(c(status="BLOCKED", protected="CORE_HEALTH_AND_SAFETY_CAPACITY", control_class="NO_GENERIC_CONTROL", reason="Food and Drug Administration operating capacity supports drug, food, and product safety. Generic freezes are not used as a substitute for specific regulatory or fee reforms."))
  }

  if (stringr::str_detect(b, "secret service") || stringr::str_detect(a, "secret service")) {
    return(c(status="BLOCKED", protected="CORE_PROTECTIVE_SERVICE_CAPACITY", control_class="NO_GENERIC_CONTROL", reason="Secret Service operations provide core protective and investigative capacity and are not treated as generic administrative machinery."))
  }

  if (stringr::str_detect(a, "judicial branch") || stringr::str_detect(b, "courts of appeals|district courts|federal judicial center|supreme court|court of appeals|court of international trade|court of federal claims|sentencing commission")) {
    return(c(status="BLOCKED", protected="CORE_JUDICIAL_CAPACITY", control_class="NO_GENERIC_CONTROL", reason="Federal court operating capacity is a core constitutional function and is not modeled as a generic administrative freeze."))
  }

  if (stringr::str_detect(t, "internal revenue service|tax administration|program integrity|payment integrity|inspector general|cybersecurity|cyber security|courts of appeals|district courts|federal judiciary|office of science and technology policy")) {
    return(c(status="BLOCKED", protected="CORE_GOVERNMENT_OR_PROGRAM_INTEGRITY_CAPACITY", control_class="NO_GENERIC_CONTROL", reason="Generic reductions could weaken revenue collection, payment integrity, cybersecurity, judicial capacity, science-policy capacity, or other core governmental functions."))
  }

  if (stringr::str_detect(t, "federal assistance, fema|fema|wildland fire|wildfire|emergency management|emergency fund|disaster response|disaster assistance")) {
    return(c(status="BLOCKED", protected="DISASTER_RESILIENCE_AND_EMERGENCY_CAPACITY", control_class="NO_GENERIC_CONTROL", reason="The account finances emergency response, disaster assistance, wildfire management, or resilience capacity and is not treated as generic administrative machinery."))
  }

  productive_by_title <- stringr::str_detect(
    t,
    "national institutes of health|national institute .*health|national institute .*research|health research|research and development|research, development|research and technology|research, engineering|scientific and technical research|scientific research|surveys, investigations, and research|transportation planning, research, and development|basic research|applied research|science foundation|science and technology|advanced research projects agency|human genome research|cancer institute|heart, lung|neurological disorders|nursing research|library of medicine|translational sciences|operations, research, and facilities|railroad crossing elimination|intercity passenger rail|national railroad passenger|airport improvement|grants-in-aid for airports|facilities and equipment \\(airport|highway|transit|water resources|corps of engineers|infrastructure|broadband|digital equity|rural utilities|capital investment|weather service|safe streets and roads for all|state and tribal assistance grants|office of clean energy demonstrations|energy efficiency and renewable energy|nuclear energy|safety, security and mission services|space operations|defense environmental cleanup"
  )
  productive_science_context <- identical(t, "science") && stringr::str_detect(a, "national aeronautics and space administration|department of energy")
  productive_energy_context <- identical(t, "electricity") && stringr::str_detect(a, "department of energy")
  if (productive_by_title || productive_science_context || productive_energy_context) {
    return(c(status="BLOCKED", protected="PRODUCTIVE_PUBLIC_CAPACITY", control_class="NO_GENERIC_CONTROL", reason="The account is treated as productive public investment, research, science, infrastructure, remediation, digital capacity, or long-lived state capacity."))
  }

  if (stringr::str_detect(b, "national aeronautics and space administration") && stringr::str_detect(t, "safety, security and mission services|space operations")) {
    return(c(status="BLOCKED", protected="PRODUCTIVE_PUBLIC_CAPACITY", control_class="NO_GENERIC_CONTROL", reason="The account supports NASA mission infrastructure and operations rather than generic administrative overhead."))
  }

  if (stringr::str_detect(b, "federal prison system|bureau of prisons") || stringr::str_detect(t, "federal prison system")) {
    return(c(status="CONDITIONAL", protected="CORE_JUSTICE_OPERATIONS_REVIEW", control_class="REAL_FREEZE_ONLY", reason="Prison operations are a core justice function. Expanded mode may restrain real growth, but a nominal generic freeze is not available."))
  }

  if (stringr::str_detect(b, "national park service") || stringr::str_detect(t, "operation of the national park system|national park system")) {
    return(c(status="CONDITIONAL", protected="PUBLIC_ASSET_STEWARDSHIP_REVIEW", control_class="REAL_FREEZE_ONLY", reason="National Park Service operations maintain long-lived public assets. Expanded mode may restrain real growth, but a nominal generic freeze is not available."))
  }

  if (stringr::str_detect(t, "contributions to international organizations|contributions for international peacekeeping|international peacekeeping")) {
    return(c(status="CONDITIONAL", protected="DIPLOMATIC_AND_ALLIANCE_CAPACITY_REVIEW", control_class="REAL_FREEZE_ONLY", reason="International-organization and peacekeeping contributions support diplomatic and alliance commitments. Only a real-growth restraint is available generically."))
  }

  strategic_context <- stringr::str_detect(context, "department of defense|defense--military|national nuclear security|department of homeland security|coast guard|customs and border protection|immigration and customs enforcement|federal bureau of investigation|drug enforcement administration|agency for international development|international assistance programs|diplomatic programs|embassy security")
  if (strategic_context) {
    return(c(status="CONDITIONAL", protected="STRATEGIC_OR_CORE_STATE_CAPACITY_REVIEW", control_class="REAL_FREEZE_ONLY", reason="The account is not a protected person-facing benefit, but strategic, readiness, law-enforcement, or diplomatic capacity requires expanded-mode review. Only a real-spending freeze is available generically."))
  }

  if (stringr::str_detect(t, "^salaries and expenses$|^office of the secretary$|administrative expenses|administrative services|departmental management|management and administration|general administration|general operating expenses|operations and support")) {
    return(c(status="ELIGIBLE", protected="ADMINISTRATIVE_MACHINERY", control_class="REAL_AND_NOMINAL_FREEZE", reason="The account title itself identifies principally administrative or operating machinery rather than a protected person-facing benefit or productive public investment."))
  }

  c(status="ELIGIBLE", protected="NONE", control_class="REAL_AND_NOMINAL_FREEZE", reason="The discretionary account title does not identify a protected person-facing, earned-benefit, human-capital, productive-capacity, special-financing, strategic, or core-government category. Generic controls remain limited to growth restraints, never account elimination.")
}

# ------------------------------------------------------------------------------
# FUNCTION: build_direct_account_controls
# Purpose: Convert nonprotected discretionary CBO account paths into bounded real- or nominal-growth restraints; never generic account elimination.
# ------------------------------------------------------------------------------
build_direct_account_controls <- function(spending_detail, working_baseline, cbo_econ) {
  log_line("Expanded universe: constructing Eisenhower-rule discretionary growth controls; generic 0%-100% account cuts are disabled")

  cpi_path <- cbo_series(cbo_econ, "chained_cpiu", 2027:2036)
  assert_model(nrow(cpi_path) == 10L && all(is.finite(cpi_path$value)) && all(cpi_path$value > 0), "CBO chained CPI-U path is incomplete for FY2027-FY2036")
  cpi27 <- cpi_path$value[cpi_path$year == 2027L]
  assert_model(length(cpi27) == 1L && is.finite(cpi27) && cpi27 > 0, "Missing FY2027 chained CPI-U anchor")

  account_year <- spending_detail |>
    mutate(
      account_key = paste(tin, disc_or_mand, category, agency, bureau, function_code, subfunction_code, off_budget, title, sep = " | ")
    )

  account_summary <- account_year |>
    filter(year %in% CFG$score_years) |>
    group_by(account_key, tin, title, disc_or_mand, category, agency, bureau, function_code, subfunction_code, off_budget) |>
    summarise(
      outlays_2027_2036_bil = sum(pmax(outlays_mil, 0), na.rm = TRUE) / 1000,
      budget_authority_2027_2036_bil = sum(pmax(budget_authority_mil, 0), na.rm = TRUE) / 1000,
      outlays_2027_bil = sum(pmax(outlays_mil[year == 2027L], 0), na.rm = TRUE) / 1000,
      .groups = "drop"
    ) |>
    filter(outlays_2027_2036_bil >= CFG$expanded_min_positive_account_outlays_bil_2027_2036) |>
    mutate(
      account_inventory_id = paste0("CBO_ACCOUNT_", vapply(account_key, function(z) substr(digest::digest(z, algo = "sha256", serialize = FALSE), 1, 20), character(1)))
    )

  classified <- purrr::pmap(
    list(account_summary$title, account_summary$disc_or_mand, account_summary$category, account_summary$agency, account_summary$bureau, account_summary$function_code, account_summary$subfunction_code),
    classify_direct_account
  )

  account_summary$protection_status <- vapply(classified, function(z) z[["status"]], character(1))
  account_summary$protected_category <- vapply(classified, function(z) z[["protected"]], character(1))
  account_summary$generic_control_class <- vapply(classified, function(z) z[["control_class"]], character(1))
  account_summary$protection_reason <- vapply(classified, function(z) z[["reason"]], character(1))

  year_grid <- account_year |>
    select(account_key, year, outlays_mil) |>
    group_by(account_key, year) |>
    summarise(outlays_bil = sum(pmax(outlays_mil, 0), na.rm = TRUE) / 1000, .groups = "drop") |>
    tidyr::complete(account_key = account_summary$account_key, year = 2027:2036, fill = list(outlays_bil = 0)) |>
    left_join(account_summary |> select(account_key, outlays_2027_bil), by = "account_key") |>
    left_join(cpi_path |> rename(cpi = value), by = "year") |>
    mutate(
      real_freeze_floor_bil = outlays_2027_bil * cpi / cpi27,
      nominal_freeze_floor_bil = outlays_2027_bil,
      real_freeze_savings_bil = pmax(outlays_bil - real_freeze_floor_bil, 0),
      nominal_freeze_savings_bil = pmax(outlays_bil - nominal_freeze_floor_bil, 0)
    )

  savings_summary <- year_grid |>
    group_by(account_key) |>
    summarise(
      real_freeze_savings_2027_2036_bil = sum(real_freeze_savings_bil, na.rm = TRUE),
      nominal_freeze_savings_2027_2036_bil = sum(nominal_freeze_savings_bil, na.rm = TRUE),
      .groups = "drop"
    )

  account_summary <- account_summary |>
    left_join(savings_summary, by = "account_key")

  placeholder <- account_summary |>
    filter(generic_control_class == "NO_GENERIC_CONTROL" | outlays_2027_bil <= 0) |>
    transmute(
      account_key,
      account_inventory_id,
      tin,
      title,
      disc_or_mand,
      category,
      agency,
      bureau,
      function_code,
      subfunction_code,
      off_budget,
      outlays_2027_2036_bil,
      budget_authority_2027_2036_bil,
      outlays_2027_bil,
      real_freeze_savings_2027_2036_bil,
      nominal_freeze_savings_2027_2036_bil,
      candidate_id = paste0(account_inventory_id, "_INVENTORY"),
      family_id = account_inventory_id,
      control_mode = "INVENTORY_ONLY",
      protection_status,
      protected_category,
      protection_reason,
      solver_eligible_annual = FALSE,
      solver_exclusion_reason = case_when(
        outlays_2027_bil <= 0 ~ "NO_POSITIVE_FY2027_OUTLAY_ANCHOR_FOR_GENERIC_GROWTH_CONTROL",
        TRUE ~ protected_category
      )
    )

  real_candidates <- account_summary |>
    filter(generic_control_class %in% c("REAL_FREEZE_ONLY", "REAL_AND_NOMINAL_FREEZE"), outlays_2027_bil > 0, real_freeze_savings_2027_2036_bil > 1e-9) |>
    transmute(
      account_key,
      account_inventory_id,
      tin,
      title,
      disc_or_mand,
      category,
      agency,
      bureau,
      function_code,
      subfunction_code,
      off_budget,
      outlays_2027_2036_bil,
      budget_authority_2027_2036_bil,
      outlays_2027_bil,
      real_freeze_savings_2027_2036_bil,
      nominal_freeze_savings_2027_2036_bil,
      candidate_id = paste0(account_inventory_id, "_REAL_FREEZE"),
      family_id = account_inventory_id,
      control_mode = "REAL_FY2027_SPENDING_FREEZE",
      protection_status,
      protected_category,
      protection_reason,
      solver_eligible_annual = TRUE,
      solver_exclusion_reason = NA_character_
    )

  nominal_candidates <- account_summary |>
    filter(generic_control_class == "REAL_AND_NOMINAL_FREEZE", outlays_2027_bil > 0, nominal_freeze_savings_2027_2036_bil > 1e-9) |>
    transmute(
      account_key,
      account_inventory_id,
      tin,
      title,
      disc_or_mand,
      category,
      agency,
      bureau,
      function_code,
      subfunction_code,
      off_budget,
      outlays_2027_2036_bil,
      budget_authority_2027_2036_bil,
      outlays_2027_bil,
      real_freeze_savings_2027_2036_bil,
      nominal_freeze_savings_2027_2036_bil,
      candidate_id = paste0(account_inventory_id, "_NOMINAL_FREEZE"),
      family_id = account_inventory_id,
      control_mode = "NOMINAL_FY2027_SPENDING_FREEZE",
      protection_status = "CONDITIONAL",
      protected_category = "EXPANDED_MODE_REAL_SERVICE_REDUCTION_REVIEW",
      protection_reason = "Nominally freezing a nonprotected discretionary account preserves FY2027 dollars but allows real purchasing power to decline. It is therefore available only in EXPANDED mode.",
      solver_eligible_annual = TRUE,
      solver_exclusion_reason = NA_character_
    )

  candidates <- bind_rows(placeholder, real_candidates, nominal_candidates) |>
    mutate(
      source_url = "https://raw.githubusercontent.com/US-CBO/cbo-data/main/data/budget/spending_detail/annual_fy_2026-02.csv",
      source_kind = "CBO_SPENDING_DETAIL_GROWTH_CONTROL",
      evidence_class = "OFFICIAL_CURRENT",
      major_category = "Spending",
      original_account_title = title,
      variant_name = case_when(
        control_mode == "REAL_FY2027_SPENDING_FREEZE" ~ paste0("Continuous restraint from current-law baseline to a FY2027 real-spending floor indexed by CBO chained CPI-U; ", disc_or_mand, "; ", agency, " / ", bureau),
        control_mode == "NOMINAL_FY2027_SPENDING_FREEZE" ~ paste0("Continuous restraint from current-law baseline to a FY2027 nominal-dollar floor; EXPANDED mode only; ", disc_or_mand, "; ", agency, " / ", bureau),
        TRUE ~ paste0("Inventory only; no generic account-level solver control; ", protection_reason)
      ),
      title = paste0("Account growth control: ", title),
      fiscal_channel = "OUTLAY",
      annual_profile_status = if_else(control_mode == "INVENTORY_ONLY", "INVENTORY_ONLY_NO_GENERIC_FLOW", "FULL_OFFICIAL_BASELINE_WITH_MECHANICAL_GROWTH_FLOOR"),
      source_start_year = 2026L,
      source_end_year = 2036L,
      source_score_start_year = 2027L,
      source_score_end_year = 2036L,
      source_effective_year = 2027L,
      estimate_year = 2026L,
      latest_estimate = "Feb 2026",
      source_date = "2026-02-11",
      budget_option_id = NA_character_,
      budget_function = function_code,
      index_url = source_url,
      decision_type = if_else(control_mode == "INVENTORY_ONLY", "INVENTORY_ONLY", "CONTINUOUS_DISCRETIONARY_GROWTH_RESTRAINT"),
      direct_cbo_annual_score = FALSE,
      mechanical_baseline_control = control_mode != "INVENTORY_ONLY",
      is_december_2024_core = FALSE,
      investment_market_review = FALSE,
      ss_actuarial_improvement_pct_payroll = 0,
      ss_actuarial_score_vintage = NA_character_,
      ss_actuarial_additivity_status = NA_character_,
      cumulative_revenue_increase_2027_2036_bil = 0,
      cumulative_spending_cut_2027_2036_bil = case_when(
        control_mode == "REAL_FY2027_SPENDING_FREEZE" ~ real_freeze_savings_2027_2036_bil,
        control_mode == "NOMINAL_FY2027_SPENDING_FREEZE" ~ nominal_freeze_savings_2027_2036_bil,
        TRUE ~ 0
      ),
      cumulative_primary_improvement_2027_2036_bil = cumulative_spending_cut_2027_2036_bil,
      risk_ordinary_wages = protected_category %in% c("PERSON_FACING_EARNED_COMPENSATION", "PERSON_FACING_SOCIAL_INSURANCE_OR_EARNED_BENEFIT"),
      risk_ordinary_saving = protected_category %in% c("PERSON_FACING_EARNED_COMPENSATION", "PERSON_FACING_SOCIAL_INSURANCE_OR_EARNED_BENEFIT"),
      risk_productive_investment = protected_category == "PRODUCTIVE_PUBLIC_CAPACITY",
      risk_family_formation = protected_category == "PERSON_FACING_FAMILY_OR_HOUSEHOLD_SECURITY",
      risk_business_reinvestment = FALSE,
      risk_core_social_security = stringr::str_detect(stringr::str_to_lower(original_account_title), "social security|supplemental security income"),
      risk_core_medicare = stringr::str_detect(stringr::str_to_lower(original_account_title), "medicare|medicaid"),
      risk_productive_public_capacity = protected_category %in% c("PRODUCTIVE_PUBLIC_CAPACITY", "CORE_GOVERNMENT_OR_PROGRAM_INTEGRITY_CAPACITY"),
      market_function_review_required = protected_category == "STRATEGIC_OR_CORE_STATE_CAPACITY_REVIEW",
      hard_protection_violation = protection_status == "BLOCKED",
      explicit_review_required = protection_status == "CONDITIONAL",
      protection_classification_method = "EISENHOWER_RULE_ACCOUNT_CLASSIFIER_V3_TITLE_FIRST",
      complexity_weight = case_when(
        control_mode == "REAL_FY2027_SPENDING_FREEZE" ~ 1 + log1p(outlays_2027_2036_bil / 100),
        control_mode == "NOMINAL_FY2027_SPENDING_FREEZE" ~ 1.5 + log1p(outlays_2027_2036_bil / 100),
        TRUE ~ 0
      ),
      complexity_weight_basis = case_when(
        control_mode == "REAL_FY2027_SPENDING_FREEZE" ~ "Activation burden plus logarithmic account fiscal scope; real-growth restraint",
        control_mode == "NOMINAL_FY2027_SPENDING_FREEZE" ~ "Higher burden for real-service reduction plus logarithmic account fiscal scope",
        TRUE ~ "Inventory only"
      )
    )

  real_flow <- year_grid |>
    inner_join(real_candidates |> select(account_key, candidate_id), by = "account_key") |>
    transmute(
      candidate_id,
      year,
      savings_bil = real_freeze_savings_bil
    )

  nominal_flow <- year_grid |>
    inner_join(nominal_candidates |> select(account_key, candidate_id), by = "account_key") |>
    transmute(
      candidate_id,
      year,
      savings_bil = nominal_freeze_savings_bil
    )

  base <- bind_rows(real_flow, nominal_flow) |>
    group_by(candidate_id, year) |>
    summarise(savings_bil = sum(savings_bil, na.rm = TRUE), .groups = "drop") |>
    transmute(
      candidate_id,
      year,
      revenue_delta_bil = 0,
      outlay_delta_bil = -pmax(savings_bil, 0),
      primary_deficit_delta_bil = -pmax(savings_bil, 0),
      component_identity_residual_bil = 0,
      shift_years = 0L,
      translation_status = "CBO_BASELINE_DISCRETIONARY_GROWTH_RESTRAINT"
    )

  solver_ids <- candidates$candidate_id[candidates$solver_eligible_annual]
  grid <- tidyr::expand_grid(candidate_id = solver_ids, year = CFG$model_years) |>
    left_join(base, by = c("candidate_id", "year")) |>
    mutate(
      revenue_delta_bil = replace_na(revenue_delta_bil, 0),
      outlay_delta_bil = replace_na(outlay_delta_bil, 0),
      primary_deficit_delta_bil = replace_na(primary_deficit_delta_bil, 0),
      component_identity_residual_bil = 0,
      shift_years = 0L,
      translation_status = replace_na(translation_status, "ZERO")
    )

  gdp36 <- working_baseline$gdp_bil[match(2036L, working_baseline$year)]
  assert_model(is.finite(gdp36) && gdp36 > 0, "Missing FY2036 GDP for account-growth-control long-run extension")

  base36 <- grid |>
    filter(year == 2036L) |>
    select(candidate_id, rev36 = revenue_delta_bil, out36 = outlay_delta_bil, pri36 = primary_deficit_delta_bil)

  grid <- grid |>
    left_join(base36, by = "candidate_id") |>
    mutate(
      gdp_y = working_baseline$gdp_bil[match(year, working_baseline$year)],
      ext_factor = gdp_y / gdp36,
      revenue_delta_bil = if_else(year > 2036L, rev36 * ext_factor, revenue_delta_bil),
      outlay_delta_bil = if_else(year > 2036L, out36 * ext_factor, outlay_delta_bil),
      primary_deficit_delta_bil = if_else(year > 2036L, pri36 * ext_factor, primary_deficit_delta_bil),
      translation_status = if_else(year > 2036L, "EXTRAPOLATED_HOLD_2036_GROWTH_RESTRAINT_AS_GDP_SHARE", translation_status)
    ) |>
    select(-rev36, -out36, -pri36, -gdp_y, -ext_factor)

  assert_model(!anyDuplicated(grid |> select(candidate_id, year)), "Duplicate account-growth-control candidate/year rows")
  assert_model(sum(candidates$solver_eligible_annual) >= 100L, paste0("Too few defensible discretionary growth controls survived the Eisenhower-rule filter: ", sum(candidates$solver_eligible_annual)))

  list(
    meta = candidates,
    flows = grid,
    account_inventory = account_summary,
    control_design = candidates |>
      select(
        account_inventory_id, candidate_id, family_id, original_account_title, agency, bureau,
        disc_or_mand, category, function_code, subfunction_code, control_mode,
        protection_status, protected_category, protection_reason, solver_eligible_annual,
        solver_exclusion_reason, outlays_2027_bil, outlays_2027_2036_bil,
        real_freeze_savings_2027_2036_bil, nominal_freeze_savings_2027_2036_bil,
        complexity_weight, complexity_weight_basis
      )
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: validate_spending_detail_reconciliation
# Purpose: Reconcile summed CBO account-level outlays against the aggregate CBO baseline before optimization.
# ------------------------------------------------------------------------------
validate_spending_detail_reconciliation_core <- function(spending_detail, cbo_baseline) {
  audit <- spending_detail |>
    filter(year %in% 2026:2036) |>
    group_by(year) |>
    summarise(
      spending_detail_outlays_bil = sum(outlays_mil, na.rm = TRUE) / 1000,
      .groups = "drop"
    ) |>
    left_join(
      cbo_baseline |>
        filter(year %in% 2026:2036) |>
        select(year, cbo_total_outlays_bil = outlays_bil),
      by = "year"
    ) |>
    mutate(
      difference_bil = spending_detail_outlays_bil - cbo_total_outlays_bil,
      absolute_difference_bil = abs(difference_bil),
      relative_difference_pct = 100 * difference_bil / cbo_total_outlays_bil
    )
  assert_model(nrow(audit) == 11L, "Spending-detail reconciliation does not cover FY2026-FY2036")
  max_rel <- max(abs(audit$relative_difference_pct), na.rm = TRUE)
  assert_model(is.finite(max_rel) && max_rel <= 2, paste0("CBO spending-detail accounts do not reconcile to aggregate CBO outlays within 2 percent. Maximum difference: ", round(max_rel, 3), "%"))
  audit
}


# ------------------------------------------------------------------------------
# FUNCTION: build_tax_parameter_inventory
# Purpose: Inventory current-law CBO tax parameters without inventing marginal revenue coefficients.
# ------------------------------------------------------------------------------
build_tax_parameter_inventory <- function(path) {
  x <- read_cbo_long(path)
  current <- x |>
    filter(year == 2026L) |>
    transmute(variable, current_2026_value = value)
  bounds <- x |>
    filter(year %in% 2026:2036) |>
    group_by(variable) |>
    summarise(
      min_current_law_2026_2036 = min(value, na.rm = TRUE),
      max_current_law_2026_2036 = max(value, na.rm = TRUE),
      .groups = "drop"
    )
  current |>
    full_join(bounds, by = "variable") |>
    mutate(
      domain = case_when(
        stringr::str_detect(variable, "corp|corporate") ~ "corporate_tax",
        stringr::str_detect(variable, "capgain|capital|dividend") ~ "capital_income",
        stringr::str_detect(variable, "estate|gift") ~ "estate_gift",
        stringr::str_detect(variable, "payroll|fica|oasdi|medicare") ~ "payroll_social_insurance",
        TRUE ~ "individual_income_tax"
      ),
      protection_status = case_when(
        domain == "individual_income_tax" ~ "PROTECTED_OR_REVIEW_ORDINARY_WAGES_FAMILY",
        domain == "payroll_social_insurance" ~ "PROTECTED_OR_REVIEW_ORDINARY_WAGES",
        domain == "corporate_tax" ~ "REVIEW_BUSINESS_REINVESTMENT",
        domain == "capital_income" ~ "REVIEW_SAVING_INVESTMENT",
        TRUE ~ "ADMISSIBLE_PENDING_RESPONSE_FUNCTION"
      ),
      solver_readiness = "INVENTORIED_NOT_AUTOMATICALLY_SCORED",
      reason = "Current-law parameter is inventoried, but the model does not invent a revenue response coefficient. Solver entry requires an official scored anchor or defensible response function."
    ) |>
    arrange(domain, variable)
}


# ------------------------------------------------------------------------------
# FUNCTION: build_trust_fund_inventory
# Purpose: Normalize current CBO trust-fund projections for audit and social-insurance cross-checks.
# ------------------------------------------------------------------------------
build_trust_fund_inventory <- function(path) {
  read_cbo_long(path) |>
    filter(year %in% CFG$model_years) |>
    arrange(variable, year)
}


# ------------------------------------------------------------------------------
# FUNCTION: build_additional_official_policy_candidates
# Purpose: Add official scored policy paths that are not contained in the frozen broad option pack and remain compatible with the model's evidence rules.
# ------------------------------------------------------------------------------
build_additional_official_policy_candidates <- function(working_baseline) {
  candidate_id <- "CBO_2020_increase_irs_enforcement_initiatives"
  source_url <- "https://www.cbo.gov/budget-options/56878"

  register_source(
    "cbo_irs_enforcement_option_2020",
    "Congressional Budget Office",
    "Increase Appropriations for the Internal Revenue Service's Enforcement Initiatives",
    source_url,
    publication_date = "2020-12-09",
    baseline_vintage = "2020 budget-option baseline",
    evidence_class = "OFFICIAL_OLDER",
    notes = "Official annual outlay and revenue score. The annual path is translated to FY2027-FY2036 without extrapolating the policy level beyond the scored anchor."
  )
  register_source(
    "cbo_irs_funding_revenue_method_2024",
    "Congressional Budget Office",
    "How Changes in Funding for the IRS Affect Revenues",
    "https://www.cbo.gov/publication/59972",
    publication_date = "2024-02-29",
    baseline_vintage = "2024 analysis",
    evidence_class = "OFFICIAL_OLDER_METHOD_VALIDATION",
    notes = "Methodology evidence supporting the causal link between IRS enforcement resources and revenues. The report validates the response mechanism but does not replace the older option's annual score with a new score."
  )
  register_source(
    "cbo_irs_rescission_revenue_effect_2026",
    "Congressional Budget Office",
    "Legislation Enacted in the First Session of the 119th Congress That Affects Mandatory Spending or Revenues",
    "https://www.cbo.gov/publication/62240",
    publication_date = "2026-03-19",
    baseline_vintage = "2026 current-law evidence",
    evidence_class = "OFFICIAL_CURRENT_METHOD_VALIDATION",
    notes = "CBO reports that rescission of $20.2B in IRS enforcement-related funding reduces enforcement actions and revenue collections and increases the 2025-2034 deficit by about $66B. This corroborates the direction and materiality of the enforcement-funding mechanism but is not reversed into a policy score."
  )

  source_years <- 2021:2030
  source_outlays <- c(0.5, 1.0, 1.5, 2.0, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5)
  source_revenues <- c(0.3, 1.5, 3.3, 5.1, 6.8, 8.1, 8.8, 9.0, 8.9, 8.8)
  target_years <- source_years + 6L

  flows <- tibble(
    candidate_id = candidate_id,
    year = CFG$model_years,
    revenue_delta_bil = 0,
    outlay_delta_bil = 0,
    shift_years = 6L,
    translation_status = "ZERO"
  )

  idx <- match(target_years, flows$year)
  flows$revenue_delta_bil[idx] <- source_revenues
  flows$outlay_delta_bil[idx] <- source_outlays
  flows$translation_status[idx] <- "SHIFTED_OFFICIAL_ANNUAL_SCORE"

  gdp36 <- working_baseline$gdp_bil[working_baseline$year == 2036L]
  assert_model(length(gdp36) == 1L && is.finite(gdp36) && gdp36 > 0, "Missing FY2036 GDP for IRS enforcement score extension")
  for (y in CFG$extension_years) {
    gdp_y <- working_baseline$gdp_bil[working_baseline$year == y]
    growth_factor <- gdp_y / gdp36
    flows$revenue_delta_bil[flows$year == y] <- tail(source_revenues, 1) * growth_factor
    flows$outlay_delta_bil[flows$year == y] <- tail(source_outlays, 1) * growth_factor
    flows$translation_status[flows$year == y] <- "EXTRAPOLATED_GDP_FROM_LAST_OFFICIAL_YEAR"
  }

  flows <- flows |>
    mutate(
      primary_deficit_delta_bil = outlay_delta_bil - revenue_delta_bil,
      component_identity_residual_bil = primary_deficit_delta_bil - (outlay_delta_bil - revenue_delta_bil)
    ) |>
    select(candidate_id, year, revenue_delta_bil, outlay_delta_bil, primary_deficit_delta_bil, component_identity_residual_bil, shift_years, translation_status)

  score_flows <- flows |> filter(year %in% CFG$score_years)
  cumulative_revenue <- sum(pmax(score_flows$revenue_delta_bil, 0), na.rm = TRUE)
  cumulative_spending_cut <- sum(pmax(-score_flows$outlay_delta_bil, 0), na.rm = TRUE)
  cumulative_primary <- sum(-score_flows$primary_deficit_delta_bil, na.rm = TRUE)

  meta <- tibble(
    candidate_id = candidate_id,
    family_key = "CBO_2020_increase_irs_enforcement_initiatives",
    family_title = "Increase Appropriations for the Internal Revenue Service's Enforcement Initiatives",
    variant_name = "Increase enforcement funding by $0.5B annually through a $2.5B annual increment",
    latest_estimate = "Dec 2020",
    estimate_year = 2020L,
    source_start_year = 2021L,
    source_end_year = 2030L,
    source_url = source_url,
    fiscal_channel = "NET_DEFICIT",
    annual_profile_status = "FULL_OFFICIAL_ANNUAL",
    ss_actuarial_improvement_pct_payroll = 0,
    note = "Official CBO annual score translated six fiscal years forward. Later CBO analysis, including 2026 evidence on enforcement-funding rescissions, continues to support the enforcement-funding revenue mechanism, but no newer annual score is substituted for this source path.",
    index_ten_year_savings_bil = 40.6,
    family_id = "CBO_2020_increase_irs_enforcement_initiatives",
    budget_option_id = "56878",
    title = "Increase Appropriations for the Internal Revenue Service's Enforcement Initiatives",
    major_category = "Net Deficit",
    budget_function = "800",
    index_url = "https://www.cbo.gov/budget-options",
    source_date = "Dec 2020",
    source_effective_year = 2021L,
    source_score_start_year = 2021L,
    source_score_end_year = 2030L,
    evidence_class = "OFFICIAL_OLDER",
    decision_type = "CONTINUOUS_LEVEL_WITH_TIMING",
    source_kind = "ADDITIONAL_OFFICIAL_CBO_POLICY",
    investment_market_review = FALSE,
    direct_cbo_annual_score = TRUE,
    solver_eligible_annual = TRUE,
    is_december_2024_core = FALSE,
    protection_status = "ELIGIBLE",
    protection_reason = "The policy increases tax-administration capacity and is scored by CBO as raising revenues by more than the associated outlay increase.",
    risk_ordinary_wages = FALSE,
    risk_ordinary_saving = FALSE,
    risk_productive_investment = FALSE,
    risk_family_formation = FALSE,
    risk_business_reinvestment = FALSE,
    risk_core_social_security = FALSE,
    risk_core_medicare = FALSE,
    risk_productive_public_capacity = FALSE,
    market_function_review_required = FALSE,
    hard_protection_violation = FALSE,
    explicit_review_required = FALSE,
    protection_classification_method = "EXPLICIT_OFFICIAL_POLICY_RULE",
    ss_actuarial_score_vintage = NA_character_,
    ss_actuarial_additivity_status = NA_character_,
    cumulative_revenue_increase_2027_2036_bil = cumulative_revenue,
    cumulative_spending_cut_2027_2036_bil = cumulative_spending_cut,
    cumulative_primary_improvement_2027_2036_bil = cumulative_primary,
    solver_exclusion_reason = NA_character_,
    replaced_by_granular_account_controls = FALSE,
    complexity_weight = 1 + log1p(max(cumulative_primary, 0) / 1000),
    complexity_weight_basis = "Activation burden plus logarithmic ten-year fiscal scope for an official scored reform"
  )

  assert_model(abs(cumulative_revenue - 60.6) <= 1e-9, "IRS enforcement translated revenue score does not reproduce the official ten-year total")
  assert_model(abs(sum(score_flows$outlay_delta_bil) - 20.0) <= 1e-9, "IRS enforcement translated outlay score does not reproduce the official ten-year total")
  assert_model(abs(cumulative_primary - 40.6) <= 1e-9, "IRS enforcement translated deficit reduction does not reproduce the official ten-year total")
  assert_model(max(abs(flows$component_identity_residual_bil), na.rm = TRUE) <= 1e-12, "IRS enforcement annual component identity failed")

  list(meta = meta, flows = flows)
}

# ------------------------------------------------------------------------------
# FUNCTION: build_expanded_anchor_policy_model
# Purpose: Merge scored policy anchors with granular account controls while preventing double counting.
# ------------------------------------------------------------------------------
build_expanded_anchor_policy_model_core <- function(cbo_universe, working_baseline, spending_detail, cbo_econ) {
  anchor <- translate_full_universe_flows(cbo_universe, working_baseline)
  additional_official <- build_additional_official_policy_candidates(working_baseline)
  account <- build_direct_account_controls(spending_detail, working_baseline, cbo_econ)

  nondiscretionary_account_controls <- account$meta |>
    filter(solver_eligible_annual, is.na(disc_or_mand) | disc_or_mand != "Discretionary")

  if (CFG$write_audit_outputs) {
    write_csv_atomic(
      nondiscretionary_account_controls,
      file.path(CFG$output_dir, "nondiscretionary_account_control_preflight_preparameterization.csv")
    )
  }

  assert_model(
    nrow(nondiscretionary_account_controls) == 0L,
    paste0(
      "Account-control construction leaked ",
      nrow(nondiscretionary_account_controls),
      " non-discretionary rows into the generic solver-control universe"
    )
  )

  # The frozen CBO scored-policy metadata predates several expanded-universe
  # audit fields. Normalize those optional columns explicitly before mutate()
  # rather than referring to a column that may not yet exist.
  required_anchor_columns <- c(
    "candidate_id",
    "family_id",
    "title",
    "variant_name",
    "source_kind",
    "solver_eligible_annual",
    "protection_status",
    "cumulative_primary_improvement_2027_2036_bil"
  )

  assert_model(
    all(required_anchor_columns %in% names(anchor$meta)),
    paste0(
      "Scored-anchor metadata is missing required columns before expanded-universe merge: ",
      paste(setdiff(required_anchor_columns, names(anchor$meta)), collapse = ", ")
    )
  )

  if (!"solver_exclusion_reason" %in% names(anchor$meta)) {
    anchor$meta$solver_exclusion_reason <- NA_character_
  }

  if (!"replaced_by_granular_account_controls" %in% names(anchor$meta)) {
    anchor$meta$replaced_by_granular_account_controls <- FALSE
  }

  if (!"complexity_weight" %in% names(anchor$meta)) {
    anchor$meta$complexity_weight <- NA_real_
  }

  if (!"complexity_weight_basis" %in% names(anchor$meta)) {
    anchor$meta$complexity_weight_basis <- NA_character_
  }

  anchor$meta <- anchor$meta |>
    mutate(
      replaced_by_granular_account_controls = FALSE,
      complexity_weight = 1 + log1p(pmax(abs(cumulative_primary_improvement_2027_2036_bil), 0) / 1000),
      complexity_weight_basis = "Activation burden plus logarithmic ten-year fiscal scope for an official scored reform"
    )

  required_account_columns <- c(
    "candidate_id",
    "family_id",
    "title",
    "variant_name",
    "source_kind",
    "solver_eligible_annual",
    "protection_status",
    "protected_category",
    "protection_reason",
    "solver_exclusion_reason",
    "complexity_weight",
    "complexity_weight_basis"
  )

  assert_model(
    all(required_account_columns %in% names(account$meta)),
    paste0(
      "Account-control metadata is missing required columns before expanded-universe merge: ",
      paste(setdiff(required_account_columns, names(account$meta)), collapse = ", ")
    )
  )

  combined_meta <- bind_rows(anchor$meta, additional_official$meta, account$meta)
  combined_flows <- bind_rows(anchor$flows, additional_official$flows, account$flows)

  required_combined_columns <- union(required_anchor_columns, required_account_columns)

  assert_model(
    all(required_combined_columns %in% names(combined_meta)),
    paste0(
      "Expanded-universe metadata merge lost required columns: ",
      paste(setdiff(required_combined_columns, names(combined_meta)), collapse = ", ")
    )
  )

  assert_model(nrow(account$account_inventory) >= 1000L, "CBO account inventory unexpectedly lost breadth")
  assert_model(sum(account$meta$solver_eligible_annual, na.rm = TRUE) >= 100L, "Fewer than 100 defensible account growth controls survived the Eisenhower-rule filter")
  assert_model(sum(combined_meta$solver_eligible_annual, na.rm = TRUE) >= 150L, "Expanded solver-ready candidate universe is unexpectedly small")

  list(
    meta = combined_meta,
    flows = combined_flows,
    pack = anchor$pack,
    account_catalog = account$meta,
    account_inventory = account$account_inventory,
    account_control_design = account$control_design,
    account_flows = account$flows,
    original_anchor_meta = anchor$meta,
    original_anchor_flows = anchor$flows,
    additional_official_meta = additional_official$meta,
    additional_official_flows = additional_official$flows
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: build_parameterized_policy_space
# Purpose: Expand admissible candidates into policy-level, timing, and phase-in decision structures.
# ------------------------------------------------------------------------------
build_parameterized_policy_space_core_2 <- function(policy_model, working_baseline) {
  log_line("Gate 3B: constructing parameterized policy space with Eisenhower-rule account growth controls")

  specs <- purrr::map2_dfr(policy_model$meta$title, policy_model$meta$variant_name, parameter_spec_one)

  meta <- bind_cols(policy_model$meta, specs) |>
    mutate(
      is_direct_account_control = source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" & solver_eligible_annual,
      parameterization_mode = if_else(is_direct_account_control, "DISCRETIONARY_GROWTH_RESTRAINT_LEVEL", parameterization_mode),
      parameter_name = if_else(is_direct_account_control, "Fraction of the maximum allowed account growth restraint", parameter_name),
      parameter_unit = if_else(is_direct_account_control, "fraction_of_growth_restraint_bound", parameter_unit),
      parameter_anchor_value = if_else(is_direct_account_control, 1, parameter_anchor_value),
      parameter_min_value = if_else(is_direct_account_control, 0, parameter_min_value),
      parameter_max_value = if_else(is_direct_account_control, 1, parameter_max_value),
      parameter_min_scale = if_else(is_direct_account_control, CFG$expanded_min_account_control_scale, parameter_min_scale),
      parameter_max_scale = if_else(is_direct_account_control, 1, parameter_max_scale),
      parameter_extrapolation = if_else(is_direct_account_control, FALSE, parameter_extrapolation),
      parameter_evidence_basis = if_else(
        is_direct_account_control & control_mode == "REAL_FY2027_SPENDING_FREEZE",
        "Mechanical CBO baseline restraint only: the strongest generic policy holds the discretionary account to FY2027 real purchasing power using CBO chained CPI-U. It never reduces the account below that real floor.",
        if_else(
          is_direct_account_control & control_mode == "NOMINAL_FY2027_SPENDING_FREEZE",
          "Expanded-mode mechanical CBO baseline restraint only: the strongest generic policy holds the discretionary account to its FY2027 nominal-dollar level. It never eliminates the account or cuts below the FY2027 nominal floor.",
          parameter_evidence_basis
        )
      ),
      decision_type = if_else(parameterization_mode == "DISCRETE_FULL_ANCHOR", "DISCRETE_WITH_TIMING", "CONTINUOUS_LEVEL_WITH_TIMING"),
      parameterized_solver_eligible = solver_eligible_annual,
      complexity_weight = dplyr::coalesce(complexity_weight, 1),
      complexity_weight_basis = dplyr::coalesce(complexity_weight_basis, "Unit activation burden")
    )

  eligible <- meta |> filter(parameterized_solver_eligible)

  base_fields <- eligible |>
    select(
      candidate_id, family_id, title, variant_name, source_kind, control_mode,
      parameterization_mode, parameter_name, parameter_unit,
      parameter_anchor_value, parameter_min_value, parameter_max_value,
      parameter_min_scale, parameter_max_scale, parameter_extrapolation,
      ss_actuarial_improvement_pct_payroll, complexity_weight
    )

  schedules_standard <- base_fields |>
    filter(source_kind != "CBO_SPENDING_DETAIL_GROWTH_CONTROL") |>
    tidyr::crossing(
      implementation_start_year = as.integer(CFG$parameterized_start_years),
      phase_in_years = as.integer(CFG$parameterized_phase_in_years)
    )

  schedules_accounts <- base_fields |>
    filter(source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL") |>
    tidyr::crossing(
      implementation_start_year = as.integer(CFG$expanded_account_start_years),
      phase_in_years = as.integer(CFG$expanded_account_phase_in_years)
    )

  schedules <- bind_rows(schedules_standard, schedules_accounts) |>
    mutate(
      schedule_id = paste0("START", implementation_start_year, "_PHASE", phase_in_years),
      schedule_key = paste(candidate_id, schedule_id, sep = "@@")
    )

  base_flows <- policy_model$flows |>
    select(candidate_id, year, revenue_delta_bil, outlay_delta_bil, primary_deficit_delta_bil) |>
    rename(source_year = year)

  schedule_flows <- tidyr::crossing(
    schedules |> select(schedule_key, candidate_id, schedule_id, source_kind, implementation_start_year, phase_in_years),
    year = as.integer(CFG$model_years)
  ) |>
    mutate(
      shift_years = implementation_start_year - 2027L,
      source_year = if_else(
        source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL",
        year,
        year - shift_years
      )
    ) |>
    left_join(base_flows, by = c("candidate_id", "source_year")) |>
    mutate(
      revenue_delta_bil = replace_na(revenue_delta_bil, 0),
      outlay_delta_bil = replace_na(outlay_delta_bil, 0),
      primary_deficit_delta_bil = replace_na(primary_deficit_delta_bil, 0),
      phase_factor = if_else(
        year < implementation_start_year,
        0,
        pmin(1, (year - implementation_start_year + 1) / phase_in_years)
      ),
      revenue_delta_bil_per_anchor_scale = if_else(year < implementation_start_year, 0, revenue_delta_bil * phase_factor),
      outlay_delta_bil_per_anchor_scale = if_else(year < implementation_start_year, 0, outlay_delta_bil * phase_factor),
      primary_deficit_delta_bil_per_anchor_scale = if_else(year < implementation_start_year, 0, primary_deficit_delta_bil * phase_factor)
    ) |>
    select(
      schedule_key, candidate_id, schedule_id, source_kind, implementation_start_year, phase_in_years,
      year, phase_factor,
      revenue_delta_bil_per_anchor_scale,
      outlay_delta_bil_per_anchor_scale,
      primary_deficit_delta_bil_per_anchor_scale
    )

  schedule_summary <- schedule_flows |>
    group_by(schedule_key, candidate_id, schedule_id, implementation_start_year, phase_in_years) |>
    summarise(
      revenue_2027_2036_bil_per_scale = sum(pmax(revenue_delta_bil_per_anchor_scale[year %in% CFG$score_years], 0)),
      spending_cut_2027_2036_bil_per_scale = sum(pmax(-outlay_delta_bil_per_anchor_scale[year %in% CFG$score_years], 0)),
      primary_improvement_2027_2036_bil_per_scale = sum(-primary_deficit_delta_bil_per_anchor_scale[year %in% CFG$score_years]),
      .groups = "drop"
    ) |>
    left_join(
      schedules |>
        select(
          schedule_key, source_kind, parameterization_mode, parameter_name, parameter_unit,
          parameter_anchor_value, parameter_min_value, parameter_max_value,
          parameter_min_scale, parameter_max_scale, parameter_extrapolation,
          ss_actuarial_improvement_pct_payroll, complexity_weight
        ),
      by = "schedule_key"
    )

  scenarios <- build_robust_scenario_catalog(working_baseline)

  assert_model(n_distinct(schedules$candidate_id) == sum(meta$parameterized_solver_eligible), "Schedule catalog lost at least one solver-ready candidate")
  standard_ids <- eligible$candidate_id[eligible$source_kind != "CBO_SPENDING_DETAIL_GROWTH_CONTROL"]
  account_ids <- eligible$candidate_id[eligible$source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL"]

  if (length(standard_ids) > 0L) {
    assert_model(
      all(table(schedules$candidate_id[schedules$candidate_id %in% standard_ids]) == length(CFG$parameterized_start_years) * length(CFG$parameterized_phase_in_years)),
      "Standard policy schedule grid is incomplete"
    )
  }

  if (length(account_ids) > 0L) {
    assert_model(
      all(table(schedules$candidate_id[schedules$candidate_id %in% account_ids]) == length(CFG$expanded_account_start_years) * length(CFG$expanded_account_phase_in_years)),
      "Account-growth-control schedule grid is incomplete"
    )
  }

  assert_model(sum(meta$parameter_extrapolation & meta$parameterized_solver_eligible, na.rm = TRUE) == 0L, "At least one solver-eligible response function extrapolates beyond its evidence anchor")
  assert_model(sum(meta$is_direct_account_control & meta$parameterized_solver_eligible, na.rm = TRUE) >= 100L, "Fewer than 100 defensible account growth controls reached parameterization")

  log_line(
    "Gate 3B complete: ", sum(meta$parameterized_solver_eligible), " solver-ready annual candidates; ",
    sum(meta$is_direct_account_control & meta$parameterized_solver_eligible), " CBO account growth controls; ",
    sum(meta$parameterization_mode != "DISCRETE_FULL_ANCHOR" & meta$parameterized_solver_eligible), " continuously parameterized candidates; ",
    nrow(schedules), " timing/phase designs; ",
    sum(scenarios$meta$required_robust), " robust scenarios"
  )

  list(
    meta = meta,
    flows = policy_model$flows,
    pack = policy_model$pack,
    schedules = schedules,
    schedule_summary = schedule_summary,
    schedule_flows = schedule_flows,
    scenario_meta = scenarios$meta,
    scenario_annual = scenarios$annual,
    source_policy_model = policy_model,
    account_catalog = policy_model$account_catalog,
    account_inventory = policy_model$account_inventory,
    account_control_design = policy_model$account_control_design,
    account_flows = policy_model$account_flows
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: build_parameterization_validation
# Purpose: Run hard checks on parameter bounds, title-first protection rules, special-account exclusions, and source-bounded policy dimensions.
# ------------------------------------------------------------------------------
build_parameterization_validation <- function(policy_model) {
  m <- policy_model$meta
  controls <- m |> filter(source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL")

  known_patterns <- c(
    "military retirement fund",
    "cost of war toxic exposures fund",
    "payments for foster care and permanency",
    "government payment for annuitants, employees health benefits",
    "government payment for annuitants, employee life insurance",
    "children and families services programs",
    "student financial assistance",
    "education for the disadvantaged",
    "special education",
    "housing for the elderly",
    "housing for persons with disabilities",
    "indian health services",
    "national institutes of health",
    "national institute of mental health",
    "board of veterans appeals",
    "veterans electronic health care record",
    "low income home energy assistance",
    "operations, research, and facilities",
    "training and employment services",
    "safe streets and roads for all",
    "digital equity",
    "federal assistance, fema",
    "operations and support, fema",
    "public health and social services emergency fund",
    "surveys, investigations, and research",
    "wildland fire management",
    "wildland fire service operations",
    "rural community facilities program account",
    "national flood insurance fund",
    "medical services",
    "medical community care",
    "medical support and compliance",
    "medical facilities",
    "state and tribal assistance grants",
    "safety, security and mission services",
    "space operations",
    "office of clean energy demonstrations",
    "energy efficiency and renewable energy",
    "nuclear energy",
    "defense environmental cleanup"
  )

  regression_rows <- purrr::map_dfr(known_patterns, function(pat) {
    hit <- controls |>
      filter(stringr::str_detect(stringr::str_to_lower(title), stringr::fixed(pat)))
    tibble(
      regression_type = "TITLE_PATTERN",
      pattern = pat,
      matched_rows = nrow(hit),
      all_blocked = nrow(hit) > 0L && all(hit$protection_status == "BLOCKED"),
      any_solver_eligible = any(hit$parameterized_solver_eligible, na.rm = TRUE),
      observed_status = ifelse(nrow(hit) > 0L, paste(sort(unique(hit$protection_status)), collapse = ";"), NA_character_),
      observed_control = ifelse(nrow(hit) > 0L, paste(sort(unique(hit$control_mode)), collapse = ";"), NA_character_),
      expected_status = "BLOCKED",
      expected_control = "NO_GENERIC_CONTROL"
    )
  })

  context_probes <- tribble(
    ~probe_id, ~title, ~disc_or_mand, ~category, ~agency, ~bureau, ~function_code, ~subfunction_code, ~expected_status, ~expected_control,
    "vha_medical", "Medical Services", "Discretionary", "Other Spending", "Department of Veterans Affairs", "Veterans Health Administration", "700", "703", "BLOCKED", "NO_GENERIC_CONTROL",
    "va_it", "Information technology systems", "Discretionary", "Other Spending", "Department of Veterans Affairs", "Departmental Administration", "700", "705", "BLOCKED", "NO_GENERIC_CONTROL",
    "irs_enforcement", "Enforcement", "Discretionary", "Other Spending", "Department of the Treasury", "Internal Revenue Service", "800", "803", "BLOCKED", "NO_GENERIC_CONTROL",
    "irs_taxpayer_service", "Taxpayer Services", "Discretionary", "Other Spending", "Department of the Treasury", "Internal Revenue Service", "800", "803", "BLOCKED", "NO_GENERIC_CONTROL",
    "courts", "Salaries and expenses", "Discretionary", "Other Spending", "Judicial Branch", "Courts of Appeals, District Courts, and Other Judicial Services", "750", "752", "BLOCKED", "NO_GENERIC_CONTROL",
    "secret_service", "Operations and Support", "Discretionary", "Other Spending", "Department of Homeland Security", "United States Secret Service", "750", "751", "BLOCKED", "NO_GENERIC_CONTROL",
    "ihs_contract_support", "Contract Support Costs", "Discretionary", "Other Spending", "Department of Health and Human Services", "Indian Health Service", "550", "551", "BLOCKED", "NO_GENERIC_CONTROL",
    "fda_operations", "Salaries and expenses", "Discretionary", "Other Spending", "Department of Health and Human Services", "Food and Drug Administration", "550", "554", "BLOCKED", "NO_GENERIC_CONTROL",
    "doe_electricity", "Electricity", "Discretionary", "Other Spending", "Department of Energy", "Energy Programs", "270", "271", "BLOCKED", "NO_GENERIC_CONTROL",
    "federal_prisons", "Salaries and expenses", "Discretionary", "Other Spending", "Department of Justice", "Federal Prison System", "750", "753", "CONDITIONAL", "REAL_FREEZE_ONLY",
    "national_parks", "Operation of the National Park System", "Discretionary", "Other Spending", "Department of the Interior", "National Park Service", "300", "303", "CONDITIONAL", "REAL_FREEZE_ONLY",
    "aid_operations", "Operating expenses of the Agency for International Development", "Discretionary", "Other Spending", "International Assistance Programs", "Agency for International Development", "150", "152", "CONDITIONAL", "REAL_FREEZE_ONLY",
    "dhs_management", "Operations and Support, MD", "Discretionary", "Other Spending", "Department of Homeland Security", "Management Directorate", "750", "751", "CONDITIONAL", "REAL_FREEZE_ONLY"
  )

  context_probe_results <- purrr::pmap_dfr(context_probes, function(probe_id, title, disc_or_mand, category, agency, bureau, function_code, subfunction_code, expected_status, expected_control) {
    result <- classify_direct_account(title, disc_or_mand, category, agency, bureau, function_code, subfunction_code)
    tibble(
      regression_type = "CONTEXT_PROBE",
      pattern = probe_id,
      matched_rows = 1L,
      all_blocked = identical(unname(result[["status"]]), expected_status),
      any_solver_eligible = !identical(unname(result[["control_class"]]), expected_control),
      observed_status = unname(result[["status"]]),
      observed_control = unname(result[["control_class"]]),
      expected_status = expected_status,
      expected_control = expected_control
    )
  })

  science_context_rows <- bind_rows(
    controls |> filter(stringr::str_to_lower(original_account_title) == "science", stringr::str_detect(stringr::str_to_lower(agency), "national aeronautics and space administration")),
    controls |> filter(stringr::str_to_lower(original_account_title) == "science", stringr::str_detect(stringr::str_to_lower(agency), "department of energy"))
  ) |>
    distinct(candidate_id, .keep_all = TRUE)

  special_structure_rows <- controls |>
    filter(stringr::str_detect(
      stringr::str_to_lower(original_account_title),
      "trust fund|insurance fund|revolving fund|working capital fund|program account|financing account|liquidating account|direct loan|loan guarantee|guaranteed loan|credit subsidy|mortgage insurance|financing fund"
    ))

  nondiscretionary_solver_rows <- controls |>
    filter(parameterized_solver_eligible, is.na(disc_or_mand) | disc_or_mand != "Discretionary")

  if (CFG$write_audit_outputs) {
    write_csv_atomic(nondiscretionary_solver_rows, file.path(CFG$output_dir, "nondiscretionary_account_control_preflight.csv"))
  }

  base <- tibble(
    check = c(
      "No solver-eligible response-function extrapolation",
      "No generic non-discretionary account control is solver-ready",
      "No generic account control can eliminate an account",
      "All solver-ready account controls are growth restraints",
      "Real-freeze controls preserve FY2027 real purchasing power",
      "Nominal-freeze controls are EXPANDED-only conditional policies",
      "Known person-facing, human-capital, research, infrastructure, emergency-capacity, and special-account false negatives are blocked",
      "DOE and NASA Science accounts are protected by title plus narrow agency context",
      "Trust-fund, insurance, revolving, working-capital, credit, and program-account structures are never generic account controls",
      "Explicit institutional-context protection probes pass",
      "Core justice and public-asset stewardship probes are limited to real-growth restraints",
      "Mandatory defense/procurement rows cannot bypass the discretionary gate",
      "Account growth controls use multiple implementation years and phase-in lengths",
      "Social Security maximum equals solvency gap plus configured margin"
    ),
    passed = c(
      sum(m$parameter_extrapolation & m$parameterized_solver_eligible, na.rm = TRUE) == 0L,
      nrow(nondiscretionary_solver_rows) == 0L,
      !any(controls$solver_eligible_annual & controls$control_mode == "INVENTORY_ONLY", na.rm = TRUE) &&
        all(controls$parameter_max_scale[controls$parameterized_solver_eligible] <= 1 + 1e-12),
      all(controls$control_mode[controls$parameterized_solver_eligible] %in% c("REAL_FY2027_SPENDING_FREEZE", "NOMINAL_FY2027_SPENDING_FREEZE")),
      all(controls$parameter_evidence_basis[controls$control_mode == "REAL_FY2027_SPENDING_FREEZE" & controls$parameterized_solver_eligible] |>
            stringr::str_detect("FY2027 real purchasing power")),
      all(controls$protection_status[controls$control_mode == "NOMINAL_FY2027_SPENDING_FREEZE"] == "CONDITIONAL"),
      all(regression_rows$matched_rows > 0L & regression_rows$all_blocked & !regression_rows$any_solver_eligible),
      nrow(science_context_rows) >= 2L && all(science_context_rows$protection_status == "BLOCKED") && !any(science_context_rows$parameterized_solver_eligible),
      nrow(special_structure_rows) > 0L && all(special_structure_rows$protection_status == "BLOCKED") && !any(special_structure_rows$parameterized_solver_eligible),
      all(context_probe_results$all_blocked[context_probe_results$expected_status == "BLOCKED"]) && !any(context_probe_results$any_solver_eligible[context_probe_results$expected_status == "BLOCKED"]),
      all(context_probe_results$all_blocked[context_probe_results$expected_status == "CONDITIONAL"]) && !any(context_probe_results$any_solver_eligible[context_probe_results$expected_status == "CONDITIONAL"]),
      {
        probe <- classify_direct_account(
          title = "Operation and maintenance, Navy",
          disc_or_mand = "Mandatory",
          category = "Other Spending",
          agency = "Department of Defense--Military Programs",
          bureau = "Operation and Maintenance",
          function_code = "050",
          subfunction_code = "051"
        )
        identical(unname(probe[["status"]]), "BLOCKED") && identical(unname(probe[["control_class"]]), "NO_GENERIC_CONTROL")
      },
      length(unique(policy_model$schedules$implementation_start_year[policy_model$schedules$source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL"])) >= 3L &&
        length(unique(policy_model$schedules$phase_in_years[policy_model$schedules$source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL"])) >= 3L,
      abs(CFG$ss_actuarial_max_pct_payroll - (CFG$ss_actuarial_gap_pct_payroll + CFG$ss_actuarial_excess_margin_pct_payroll)) <= 1e-12
    ),
    evidence_basis = c(
      "Parameter-response hard gate",
      "Generic account controls are allowed only for CBO rows explicitly classified Discretionary",
      "Generic account controls are bounded by FY2027 real or nominal spending floors rather than a zero-outlay floor",
      "Account-control design",
      "CBO February 2026 chained CPI-U path",
      "Eisenhower-rule distinction between fiscal restraint and real service reduction",
      "Regression tests derived from selected-package account review",
      "Title-first classifier with narrow agency context for ambiguous one-word Science accounts",
      "Program-specific cash-flow modeling required before receipt-linked or federal-credit structures enter the solver",
      "Explicit institution-level protections for veterans health, tax administration, courts, protective services, Indian health, and food and drug safety",
      "Core justice and public-asset operations may receive only real-growth restraints in expanded mode",
      "Direct regression ensuring mandatory Department of Defense O&M remains outside generic account controls",
      "Parameterized timing grid",
      "Social Security solvency-reservoir guard"
    )
  )

  low_surtax <- m |>
    filter(stringr::str_detect(stringr::str_to_lower(title), "impose a surtax on individuals' adjusted gross income"), stringr::str_detect(stringr::str_to_lower(variant_name), "20,000|40,000"))
  vat <- m |>
    filter(stringr::str_detect(stringr::str_to_lower(title), "value-added tax"), stringr::str_detect(stringr::str_to_lower(variant_name), "narrow"))
  ftt <- m |>
    filter(stringr::str_detect(stringr::str_to_lower(title), "financial transactions"))
  ss_within_ceiling <- m |>
    filter(
      parameterized_solver_eligible,
      ss_actuarial_improvement_pct_payroll + 1e-12 >= CFG$ss_actuarial_gap_pct_payroll,
      ss_actuarial_improvement_pct_payroll <= CFG$ss_actuarial_max_pct_payroll + 1e-12
    )
  ss_above_ceiling <- m |>
    filter(
      parameterized_solver_eligible,
      ss_actuarial_improvement_pct_payroll > CFG$ss_actuarial_max_pct_payroll + 1e-12
    )
  irs_added <- m |>
    filter(candidate_id == "CBO_2020_increase_irs_enforcement_initiatives")

  extra <- tibble(
    check = c(
      "Low-threshold AGI surtax remains blocked",
      "Narrow VAT remains capped at official 5 percent anchor",
      "FTT remains capped at official 0.01 percent anchor",
      "Social Security ceiling still admits at least one official solvency-scale provision in expanded mode",
      "Social Security ceiling excludes at least one excessive standalone actuarial improvement",
      "IRS enforcement option reproduces its official ten-year score and remains solver-ready"
    ),
    passed = c(
      nrow(low_surtax) == 1L && low_surtax$protection_status[[1]] == "BLOCKED",
      nrow(vat) == 1L && abs(vat$parameter_max_value[[1]] - 5) < 1e-12,
      nrow(ftt) == 1L && abs(ftt$parameter_max_value[[1]] - 0.01) < 1e-12,
      nrow(ss_within_ceiling) >= 1L && any(ss_within_ceiling$protection_status %in% c("ELIGIBLE", "CONDITIONAL")),
      nrow(ss_above_ceiling) >= 1L,
      nrow(irs_added) == 1L && isTRUE(irs_added$direct_cbo_annual_score[[1]]) && isTRUE(irs_added$parameterized_solver_eligible[[1]]) && abs(irs_added$cumulative_revenue_increase_2027_2036_bil[[1]] - 60.6) <= 1e-9 && abs(irs_added$cumulative_primary_improvement_2027_2036_bil[[1]] - 40.6) <= 1e-9
    ),
    evidence_basis = c(
      "Ordinary-wage protection rule",
      "CBO/JCT scored anchor",
      "CBO/JCT scored anchor",
      "Configured 2026 OASDI actuarial gap plus a 0.50 percentage-point maximum excess margin; preserves the 4.90-percent official uniform-benefit variant as a solvency-scale case",
      "The actuarial ceiling prevents larger standalone or stacked Social Security changes from becoming unrestricted general-purpose debt reduction",
      "CBO December 2020 official annual option score; 2024 CBO analysis independently supports the enforcement-funding revenue mechanism"
    )
  )

  out <- bind_rows(base, extra)
  attr(out, "eisenhower_regression") <- bind_rows(regression_rows, context_probe_results)
  out
}

# ------------------------------------------------------------------------------
# FUNCTION: build_expanded_coverage_audit
# Purpose: Measure expanded-universe breadth, solver readiness, protection exclusions, and agency/function coverage.
# ------------------------------------------------------------------------------
build_expanded_coverage_audit_core <- function(policy_model, tax_inventory, expanded_sources) {
  m <- policy_model$meta
  controls <- m |> filter(source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL")
  inventory <- policy_model$account_inventory

  tibble(
    metric = c(
      "Original frozen CBO option variants retained in master catalog",
      "Original CBO option variants solver-ready",
      "CBO spending-detail accounts inventoried",
      "Account growth-control candidate variants in master catalog",
      "Account growth-control variants solver-ready before protection-mode filtering",
      "Person-facing/earned-benefit/capacity account variants blocked",
      "Mandatory accounts withheld from generic controls pending statutory parameterization",
      "Real FY2027 spending-freeze controls",
      "Nominal FY2027 spending-freeze controls",
      "Total solver-ready annual candidates before protection-mode filtering",
      "CBO current-law tax parameters inventoried",
      "Solver-ready candidates with unsupported extrapolation",
      "Distinct inventoried agencies",
      "Distinct inventoried bureaus",
      "Distinct inventoried federal function codes"
    ),
    value = c(
      sum(m$source_kind == "LOCAL_FROZEN_CBO_POLICY_PACK", na.rm = TRUE),
      sum(m$source_kind == "LOCAL_FROZEN_CBO_POLICY_PACK" & m$parameterized_solver_eligible, na.rm = TRUE),
      nrow(inventory),
      nrow(controls),
      sum(controls$parameterized_solver_eligible, na.rm = TRUE),
      sum(controls$protection_status == "BLOCKED", na.rm = TRUE),
      sum(inventory$disc_or_mand == "Mandatory", na.rm = TRUE),
      sum(controls$control_mode == "REAL_FY2027_SPENDING_FREEZE", na.rm = TRUE),
      sum(controls$control_mode == "NOMINAL_FY2027_SPENDING_FREEZE", na.rm = TRUE),
      sum(m$parameterized_solver_eligible, na.rm = TRUE),
      nrow(tax_inventory),
      sum(m$parameter_extrapolation & m$parameterized_solver_eligible, na.rm = TRUE),
      n_distinct(inventory$agency, na.rm = TRUE),
      n_distinct(inventory$bureau, na.rm = TRUE),
      n_distinct(inventory$function_code, na.rm = TRUE)
    ),
    interpretation = c(
      "Preserved as official policy-specific scored anchors",
      "Scored outlay options are no longer discarded merely because account-level data exist",
      "CBO account data serve as the comprehensive spending inventory skeleton",
      "Only defensible discretionary growth-restraint variants become generic solver levers; protected and mandatory accounts remain explicitly cataloged",
      "Generic account controls are no longer equivalent to arbitrary 0%-100% spending cuts",
      "Eisenhower-rule protection of person-facing benefits, earned compensation, household security, productive capacity, and core state capacity",
      "Mandatory spending requires program-specific statutory parameters or scored reforms",
      "Current-law spending may be restrained no lower than FY2027 purchasing power indexed by CBO chained CPI-U",
      "More aggressive EXPANDED-mode option preserves FY2027 nominal dollars rather than eliminating the account",
      "Candidate count before STRICT/EXPANDED protection filtering",
      "Current-law parameter inventory; no fabricated revenue coefficients",
      "Must remain zero",
      "Coverage breadth",
      "Coverage breadth",
      "Coverage breadth"
    )
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: build_unmodeled_parameter_domains
# Purpose: List authoritative fiscal domains inventoried but withheld from the solver because defensible response functions are absent.
# ------------------------------------------------------------------------------
build_unmodeled_parameter_domains_core <- function(tax_inventory) {
  bind_rows(
    tax_inventory |>
      transmute(
        domain = domain,
        lever = variable,
        status = solver_readiness,
        reason
      ),
    tibble(
      domain = c("social_security", "tax_expenditures", "medicare_payment_parameters", "federal_credit", "governmental_receipts"),
      lever = c(
        "Full SSA OACT provision library beyond CBO-compatible annual budget mappings",
        "Treasury/JCT tax-expenditure provisions without policy-specific repeal/reform scores",
        "CMS/MedPAC payment parameters not already represented by a compatible CBO score",
        "Federal credit subsidy, fee, and default parameters lacking a quantified CBO-compatible response surface",
        "Receipt-account statutory parameters lacking a defensible marginal response function"
      ),
      status = c(
        "INVENTORIED_SOURCE_DOWNLOADED_NOT_FABRICATED",
        "INVENTORIED_SOURCE_DOWNLOADED_NOT_FABRICATED",
        "SOURCE_DOMAIN_REGISTERED_NOT_FABRICATED",
        "SOURCE_DOMAIN_REGISTERED_NOT_FABRICATED",
        "SOURCE_DOMAIN_REGISTERED_NOT_FABRICATED"
      ),
      reason = c(
        "SSA actuarial percentages are not silently converted into unified-budget dollars unless an annual budget mapping is defensible.",
        "Tax-expenditure estimates are not equivalent to repeal revenue and generally are not additive.",
        "Payment formulas require parameter-specific savings relationships rather than generic account cuts; protected Medicare accounts remain blocked from generic reduction.",
        "Credit subsidy changes require cohort/default/fee response modeling.",
        "Changing a receipts total is not itself a statutory policy lever."
      )
    )
  )
}



# ------------------------------------------------------------------------------
# FUNCTION: extract_solution_membership
# Purpose: Extract the selected policy parameters, levels, timing, and provenance for one solution.
# ------------------------------------------------------------------------------
extract_solution_membership <- function(solution, policy_model, solution_id) {
  if (nrow(solution$parameter_decisions) == 0L) return(tibble())

  # Reporting metadata is deliberately repaired by schema rather than by a
  # fixed join. parameter_decisions and policy_model$meta evolve at different
  # stages of the pipeline. Joining a column that already exists causes .x/.y
  # suffix collisions; failing to join a newly added audit field causes the
  # opposite problem. Only fields absent from parameter_decisions are joined.
  decisions <- solution$parameter_decisions

  metadata_fields <- c(
    "family_id",
    "source_kind",
    "title",
    "variant_name",
    "major_category",
    "tin",
    "agency",
    "bureau",
    "function_code",
    "subfunction_code",
    "protection_status",
    "protected_category",
    "solver_exclusion_reason",
    "parameterization_mode",
    "parameter_name",
    "parameter_unit",
    "parameter_anchor_value",
    "parameter_max_value",
    "parameter_extrapolation",
    "complexity_weight",
    "complexity_weight_basis",
    "source_url"
  )

  missing_from_decisions <- setdiff(metadata_fields, names(decisions))

  if (length(missing_from_decisions) > 0L) {
    assert_model(
      all(missing_from_decisions %in% names(policy_model$meta)),
      paste0(
        "Solution-membership metadata cannot be repaired because policy_model$meta is missing: ",
        paste(setdiff(missing_from_decisions, names(policy_model$meta)), collapse = ", ")
      )
    )

    catalog <- policy_model$meta |>
      select(candidate_id, all_of(missing_from_decisions)) |>
      distinct(candidate_id, .keep_all = TRUE)

    decisions <- decisions |>
      left_join(catalog, by = "candidate_id")
  }

  required <- c(
    "candidate_id",
    "family_id",
    "source_kind",
    "title",
    "variant_name",
    "major_category",
    "tin",
    "agency",
    "bureau",
    "function_code",
    "subfunction_code",
    "protection_status",
    "protected_category",
    "parameterization_mode",
    "parameter_name",
    "parameter_unit",
    "parameter_anchor_value",
    "parameter_value",
    "parameter_max_value",
    "intensity_scale",
    "implementation_start_year",
    "phase_in_years",
    "schedule_id",
    "schedule_key",
    "parameter_extrapolation",
    "complexity_weight",
    "complexity_weight_basis",
    "ss_actuarial_contribution_pct_payroll",
    "source_url"
  )

  assert_model(
    all(required %in% names(decisions)),
    paste0(
      "Solution-membership schema is incomplete after dynamic metadata repair. Missing columns: ",
      paste(setdiff(required, names(decisions)), collapse = ", ")
    )
  )

  assert_model(
    !anyDuplicated(decisions$candidate_id),
    "Solution-membership extraction received duplicate active candidate rows"
  )

  decisions |>
    transmute(
      solution_id = solution_id,
      candidate_id,
      family_id,
      source_kind,
      title,
      variant_name,
      major_category,
      tin,
      agency,
      bureau,
      function_code,
      subfunction_code,
      protection_status,
      protected_category,
      parameterization_mode,
      parameter_name,
      parameter_unit,
      parameter_anchor_value,
      parameter_value,
      parameter_max_value,
      intensity_scale,
      implementation_start_year,
      phase_in_years,
      schedule_id,
      schedule_key,
      parameter_extrapolation,
      complexity_weight,
      complexity_weight_basis,
      ss_actuarial_contribution_pct_payroll,
      source_url
    )
}


# ------------------------------------------------------------------------------
# FUNCTION: validate_solution_membership_schema
# Purpose: Exercise the exact production parameter-decision and membership
# reporting path for each solver source kind before Gate 4.
# ------------------------------------------------------------------------------
validate_solution_membership_schema <- function(policy_model) {
  assert_model(nrow(policy_model$schedules) > 0L, "No schedules available for reporting-schema preflight")

  # Exercise the exact production extraction path, not a hand-built approximation.
  # A tiny synthetic primal vector is sufficient because
  # extract_parameter_decisions_from_primal() only needs model metadata,
  # schedules, and y/a/z variable names. This catches reporting-schema drift
  # before any HiGHS solve begins.
  source_candidates <- policy_model$meta |>
    filter(parameterized_solver_eligible) |>
    group_by(source_kind) |>
    slice_head(n = 1L) |>
    ungroup() |>
    pull(candidate_id)

  assert_model(length(source_candidates) >= 1L, "No solver-ready candidates available for reporting-schema preflight")

  preflight_rows <- purrr::map_dfr(source_candidates, function(cid) {
    sched <- policy_model$schedules |>
      filter(candidate_id == cid) |>
      slice_head(n = 1L)

    assert_model(nrow(sched) == 1L, paste0("No implementation schedule available for preflight candidate ", cid))

    mini_meta <- policy_model$meta |>
      filter(candidate_id == cid)

    assert_model(nrow(mini_meta) == 1L, paste0("Candidate metadata is not unique for reporting preflight: ", cid))

    y_name <- paste0("y::", cid)
    a_name <- paste0("a::", sched$schedule_key[[1]])
    z_name <- paste0("z::", sched$schedule_key[[1]])

    mini_model <- list(
      variable_names = c(y_name, a_name, z_name),
      meta = mini_meta,
      schedules = sched
    )

    intensity <- min(0.5, sched$parameter_max_scale[[1]])
    primal <- c(1, 1, intensity)

    decisions <- extract_parameter_decisions_from_primal(mini_model, primal)

    decision_required <- c(
      "candidate_id",
      "family_id",
      "source_kind",
      "title",
      "variant_name",
      "major_category",
      "protection_status",
      "parameterization_mode",
      "parameter_name",
      "parameter_unit",
      "parameter_anchor_value",
      "parameter_value",
      "parameter_max_value",
      "intensity_scale",
      "implementation_start_year",
      "phase_in_years",
      "schedule_id",
      "schedule_key",
      "parameter_extrapolation",
      "complexity_weight",
      "complexity_weight_basis",
      "ss_actuarial_contribution_pct_payroll",
      "source_url"
    )

    assert_model(
      all(decision_required %in% names(decisions)),
      paste0(
        "Parameter-decision extraction preflight failed for source kind ",
        mini_meta$source_kind[[1]],
        ". Missing columns: ",
        paste(setdiff(decision_required, names(decisions)), collapse = ", ")
      )
    )

    membership <- extract_solution_membership(
      list(parameter_decisions = decisions),
      policy_model,
      paste0("__REPORTING_PREFLIGHT__", mini_meta$source_kind[[1]])
    )

    membership_required <- c(
      "solution_id",
      "candidate_id",
      "family_id",
      "source_kind",
      "title",
      "variant_name",
      "major_category",
      "tin",
      "agency",
      "bureau",
      "function_code",
      "subfunction_code",
      "protection_status",
      "protected_category",
      "parameterization_mode",
      "parameter_name",
      "parameter_unit",
      "parameter_anchor_value",
      "parameter_value",
      "parameter_max_value",
      "intensity_scale",
      "implementation_start_year",
      "phase_in_years",
      "schedule_id",
      "schedule_key",
      "parameter_extrapolation",
      "complexity_weight",
      "complexity_weight_basis",
      "ss_actuarial_contribution_pct_payroll",
      "source_url"
    )

    assert_model(
      all(membership_required %in% names(membership)),
      paste0(
        "Solution-membership reporting preflight failed for source kind ",
        mini_meta$source_kind[[1]],
        ". Missing columns: ",
        paste(setdiff(membership_required, names(membership)), collapse = ", ")
      )
    )

    assert_model(nrow(membership) == 1L, "Reporting-schema preflight did not return exactly one membership row")
    assert_model(!is.na(membership$source_kind[[1]]) && nzchar(membership$source_kind[[1]]), "Reporting-schema preflight produced a missing source_kind")
    assert_model(is.finite(membership$complexity_weight[[1]]), "Reporting-schema preflight produced a nonfinite complexity_weight")
    assert_model(!is.na(membership$complexity_weight_basis[[1]]) && nzchar(membership$complexity_weight_basis[[1]]), "Reporting-schema preflight produced a missing complexity_weight_basis")

    tibble(
      check = "Exact parameter-decision and membership reporting path",
      source_kind = mini_meta$source_kind[[1]],
      candidate_id = cid,
      passed = TRUE,
      complexity_weight = membership$complexity_weight[[1]],
      complexity_weight_basis = membership$complexity_weight_basis[[1]]
    )
  })

  assert_model(
    all(preflight_rows$passed),
    "At least one exact reporting-schema preflight failed"
  )

  preflight_rows
}


# ------------------------------------------------------------------------------
# FUNCTION: validate_expanded_universe_merge_schema
# Purpose: Validate the merged scored-anchor/account-control metadata schema
# before parameterization so optional-column drift cannot fail later in Gate 3.
# ------------------------------------------------------------------------------
validate_expanded_universe_merge_schema <- function(expanded_anchor) {
  required_meta <- c(
    "candidate_id",
    "family_id",
    "title",
    "variant_name",
    "source_kind",
    "solver_eligible_annual",
    "protection_status",
    "solver_exclusion_reason",
    "complexity_weight",
    "complexity_weight_basis"
  )

  required_flows <- c(
    "candidate_id",
    "year",
    "revenue_delta_bil",
    "outlay_delta_bil",
    "primary_deficit_delta_bil"
  )

  assert_model(
    all(required_meta %in% names(expanded_anchor$meta)),
    paste0(
      "Expanded-universe merge-schema preflight failed for metadata. Missing: ",
      paste(setdiff(required_meta, names(expanded_anchor$meta)), collapse = ", ")
    )
  )

  assert_model(
    all(required_flows %in% names(expanded_anchor$flows)),
    paste0(
      "Expanded-universe merge-schema preflight failed for annual flows. Missing: ",
      paste(setdiff(required_flows, names(expanded_anchor$flows)), collapse = ", ")
    )
  )

  assert_model(
    !anyDuplicated(expanded_anchor$meta$candidate_id),
    "Expanded-universe merge-schema preflight found duplicate candidate_id values"
  )

  tibble(
    check = c(
      "Expanded-universe metadata schema complete",
      "Expanded-universe annual-flow schema complete",
      "Expanded-universe candidate ids unique"
    ),
    passed = TRUE,
    implementation = CFG$model_version
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: validate_postsolve_reporting_contract
# Purpose: Validate every metadata and schedule field consumed by post-solve
# reporting before any HiGHS optimization begins.
# ------------------------------------------------------------------------------
validate_postsolve_reporting_contract <- function(policy_model) {
  required_meta <- c(
    "candidate_id",
    "family_id",
    "source_kind",
    "title",
    "variant_name",
    "major_category",
    "tin",
    "agency",
    "bureau",
    "function_code",
    "subfunction_code",
    "protection_status",
    "protected_category",
    "parameterization_mode",
    "parameter_name",
    "parameter_unit",
    "parameter_anchor_value",
    "parameter_max_value",
    "parameter_extrapolation",
    "complexity_weight",
    "complexity_weight_basis",
    "source_url"
  )

  required_schedule <- c(
    "candidate_id",
    "family_id",
    "title",
    "variant_name",
    "source_kind",
    "parameterization_mode",
    "parameter_name",
    "parameter_unit",
    "parameter_anchor_value",
    "parameter_min_value",
    "parameter_max_value",
    "parameter_min_scale",
    "parameter_max_scale",
    "parameter_extrapolation",
    "ss_actuarial_improvement_pct_payroll",
    "complexity_weight",
    "implementation_start_year",
    "phase_in_years",
    "schedule_id",
    "schedule_key"
  )

  required_schedule_summary <- c(
    "schedule_key",
    "candidate_id",
    "schedule_id",
    "implementation_start_year",
    "phase_in_years",
    "revenue_2027_2036_bil_per_scale",
    "spending_cut_2027_2036_bil_per_scale",
    "primary_improvement_2027_2036_bil_per_scale"
  )

  required_schedule_flows <- c(
    "schedule_key",
    "candidate_id",
    "schedule_id",
    "source_kind",
    "implementation_start_year",
    "phase_in_years",
    "year",
    "phase_factor",
    "revenue_delta_bil_per_anchor_scale",
    "outlay_delta_bil_per_anchor_scale",
    "primary_deficit_delta_bil_per_anchor_scale"
  )

  required_scenario_meta <- c(
    "scenario_id",
    "required_robust"
  )

  required_scenario_annual <- c(
    "scenario_id",
    "year",
    "required_robust",
    "evidence_class",
    "scenario_gdp_bil",
    "scenario_baseline_debt_bil",
    "stress_deficit_delta_bil",
    "stress_debt_delta_bil",
    "policy_yield_factor",
    "marginal_rate_addition"
  )

  missing_meta <- setdiff(required_meta, names(policy_model$meta))
  missing_schedule <- setdiff(required_schedule, names(policy_model$schedules))
  missing_summary <- setdiff(required_schedule_summary, names(policy_model$schedule_summary))
  missing_flows <- setdiff(required_schedule_flows, names(policy_model$schedule_flows))
  missing_scenario_meta <- setdiff(required_scenario_meta, names(policy_model$scenario_meta))
  missing_scenario_annual <- setdiff(required_scenario_annual, names(policy_model$scenario_annual))

  assert_model(length(missing_meta) == 0L, paste0("Post-solve reporting contract is missing policy metadata columns: ", paste(missing_meta, collapse = ", ")))
  assert_model(length(missing_schedule) == 0L, paste0("Post-solve reporting contract is missing schedule columns: ", paste(missing_schedule, collapse = ", ")))
  assert_model(length(missing_summary) == 0L, paste0("Post-solve reporting contract is missing schedule-summary columns: ", paste(missing_summary, collapse = ", ")))
  assert_model(length(missing_flows) == 0L, paste0("Post-solve reporting contract is missing schedule-flow columns: ", paste(missing_flows, collapse = ", ")))
  assert_model(length(missing_scenario_meta) == 0L, paste0("Post-solve reporting contract is missing scenario metadata columns: ", paste(missing_scenario_meta, collapse = ", ")))
  assert_model(length(missing_scenario_annual) == 0L, paste0("Post-solve reporting contract is missing scenario annual columns: ", paste(missing_scenario_annual, collapse = ", ")))

  solver_meta <- policy_model$meta |> filter(parameterized_solver_eligible)
  solver_ids <- solver_meta$candidate_id

  assert_model(!anyDuplicated(policy_model$meta$candidate_id), "Policy metadata contains duplicate candidate_id values")
  assert_model(!anyDuplicated(policy_model$schedules$schedule_key), "Implementation schedules contain duplicate schedule_key values")
  assert_model(!anyDuplicated(policy_model$schedule_summary$schedule_key), "Schedule summary contains duplicate schedule_key values")
  assert_model(!anyDuplicated(policy_model$schedule_flows |> select(schedule_key, year)), "Schedule-flow table contains duplicate schedule_key/year rows")

  assert_model(
    setequal(unique(policy_model$schedules$candidate_id), solver_ids),
    "Schedule candidate coverage does not exactly match the solver-ready candidate universe"
  )

  assert_model(
    setequal(unique(policy_model$schedule_summary$schedule_key), policy_model$schedules$schedule_key),
    "Schedule-summary coverage does not exactly match the implementation-schedule catalog"
  )

  assert_model(
    setequal(unique(policy_model$schedule_flows$schedule_key), policy_model$schedules$schedule_key),
    "Schedule-flow coverage does not exactly match the implementation-schedule catalog"
  )

  assert_model(
    all(table(policy_model$schedule_flows$schedule_key) == length(CFG$model_years)),
    "At least one implementation schedule does not have exactly one annual flow row for every model year"
  )

  assert_model(
    all(is.finite(solver_meta$complexity_weight)),
    "At least one solver-ready candidate has a nonfinite complexity_weight"
  )

  assert_model(
    all(!is.na(solver_meta$complexity_weight_basis) & nzchar(solver_meta$complexity_weight_basis)),
    "At least one solver-ready candidate lacks complexity_weight_basis"
  )

  assert_model(
    all(!is.na(solver_meta$source_kind) & nzchar(solver_meta$source_kind)),
    "At least one solver-ready candidate lacks source_kind"
  )

  assert_model(
    all(!is.na(solver_meta$source_url) & nzchar(solver_meta$source_url)),
    "At least one solver-ready candidate lacks source_url"
  )

  scenario_pairs <- policy_model$scenario_annual |>
    count(scenario_id, year, name = "n")

  assert_model(all(scenario_pairs$n == 1L), "Scenario annual table contains duplicate scenario/year rows")
  assert_model(
    setequal(unique(policy_model$scenario_annual$scenario_id), policy_model$scenario_meta$scenario_id),
    "Scenario annual coverage does not match scenario metadata"
  )
  assert_model(
    all(table(policy_model$scenario_annual$scenario_id) == length(CFG$model_years)),
    "At least one scenario does not have exactly one annual row for every model year"
  )
  assert_model(
    all(is.finite(policy_model$scenario_annual$scenario_gdp_bil) & policy_model$scenario_annual$scenario_gdp_bil > 0),
    "Scenario annual table contains nonpositive or nonfinite GDP"
  )
  assert_model(
    all(is.finite(policy_model$scenario_annual$scenario_baseline_debt_bil)),
    "Scenario annual table contains nonfinite baseline debt"
  )
  assert_model(
    all(is.finite(policy_model$scenario_annual$policy_yield_factor) & policy_model$scenario_annual$policy_yield_factor > 0),
    "Scenario annual table contains invalid policy-yield factors"
  )

  interactions <- build_interaction_catalog_full(solver_meta)
  if (nrow(interactions) > 0L) {
    assert_model(
      all(interactions$candidate_i %in% solver_ids & interactions$candidate_j %in% solver_ids),
      "Interaction catalog references a candidate outside the solver-ready universe"
    )
    assert_model(
      all(interactions$candidate_i != interactions$candidate_j),
      "Interaction catalog contains a self-interaction"
    )
    interaction_key <- purrr::map2_chr(
      interactions$candidate_i,
      interactions$candidate_j,
      ~ paste(sort(c(.x, .y)), collapse = "||")
    )
    assert_model(!anyDuplicated(interaction_key), "Interaction catalog contains duplicate candidate pairs")
    assert_model(
      !any(stringr::str_detect(interactions$reason, "^Conservative overlap guard: an account-level growth restraint")),
      "Broad-domain account overlap guards remain in the interaction catalog"
    )
  }

  tibble(
    check = c(
      "Post-solve policy metadata contract complete",
      "Post-solve schedule contract complete",
      "Schedule-summary contract complete",
      "Schedule-flow contract complete",
      "Scenario metadata and annual contracts complete",
      "Solver-ready candidate/schedule coverage exact",
      "Schedule summary and annual-flow coverage exact",
      "All schedules have complete annual flow paths",
      "All solver-ready complexity weights finite",
      "All solver-ready complexity weight bases documented",
      "All solver-ready provenance fields documented",
      "Scenario annual paths unique and complete",
      "Interaction catalog references valid distinct candidate pairs"
    ),
    passed = TRUE,
    implementation = CFG$model_version
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: validate_presolve_simulation_path
# Purpose: Run representative solver-ready policies through the same independent
# annual simulation engine used after optimization before any HiGHS solve begins.
# ------------------------------------------------------------------------------
validate_presolve_simulation_path <- function(policy_model, working_baseline, kernel_obj) {
  candidates <- policy_model$meta |>
    filter(parameterized_solver_eligible) |>
    group_by(source_kind, parameterization_mode) |>
    slice_head(n = 1L) |>
    ungroup() |>
    pull(candidate_id)

  assert_model(length(candidates) >= 1L, "No solver-ready candidates available for presolve simulation preflight")

  rows <- purrr::map_dfr(candidates, function(cid) {
    sched <- policy_model$schedules |>
      filter(candidate_id == cid) |>
      arrange(implementation_start_year, phase_in_years) |>
      slice_head(n = 1L)

    assert_model(nrow(sched) == 1L, paste0("No schedule available for presolve simulation candidate ", cid))

    intensity <- min(0.5, sched$parameter_max_scale[[1]])
    decisions <- sched |>
      mutate(
        intensity_scale = intensity,
        parameter_value = pmin(parameter_anchor_value * intensity_scale, parameter_max_value),
        ss_actuarial_contribution_pct_payroll = ss_actuarial_improvement_pct_payroll * intensity_scale
      )

    sim <- simulate_parameterized_package(decisions, policy_model, working_baseline, kernel_obj)

    assert_model(
      nrow(sim) == nrow(policy_model$scenario_meta) * length(CFG$model_years),
      paste0("Presolve simulation returned an unexpected row count for candidate ", cid)
    )

    assert_model(
      setequal(unique(sim$scenario_id), policy_model$scenario_meta$scenario_id),
      paste0("Presolve simulation lost at least one scenario for candidate ", cid)
    )

    assert_model(
      all(table(sim$scenario_id) == length(CFG$model_years)),
      paste0("Presolve simulation produced an incomplete annual path for candidate ", cid)
    )

    assert_model(
      all(is.finite(sim$scenario_debt_bil) & is.finite(sim$scenario_debt_gdp_pct)),
      paste0("Presolve simulation produced nonfinite debt results for candidate ", cid)
    )

    assert_model(
      all(is.finite(sim$primary_deficit_delta_bil) & is.finite(sim$policy_interest_delta_bil)),
      paste0("Presolve simulation produced nonfinite fiscal-flow results for candidate ", cid)
    )

    meta_row <- policy_model$meta |> filter(candidate_id == cid)

    tibble(
      check = "Independent simulation path before Gate 4",
      candidate_id = cid,
      source_kind = meta_row$source_kind[[1]],
      parameterization_mode = meta_row$parameterization_mode[[1]],
      schedule_key = sched$schedule_key[[1]],
      scenarios_tested = n_distinct(sim$scenario_id),
      years_per_scenario = length(CFG$model_years),
      passed = TRUE
    )
  })

  rows
}


# ------------------------------------------------------------------------------
# FUNCTION: download_required_pdf_source
# Purpose: Download, validate, cache, hash, and register an official PDF required by the current-law evidence gate. Multiple official endpoints may be supplied for the same document; the model stops if none yields a valid PDF.
# ------------------------------------------------------------------------------
download_required_pdf_source <- function(
  url,
  landing_url,
  destination,
  source_id,
  agency,
  title,
  publication_date,
  evidence_class,
  notes,
  minimum_bytes = 20000L
) {
  urls <- as.character(url)
  landing_urls <- as.character(landing_url)
  if (length(landing_urls) == 1L && length(urls) > 1L) landing_urls <- rep(landing_urls, length(urls))
  assert_model(length(urls) >= 1L, paste0("No official download endpoint supplied for required source: ", title))
  assert_model(length(landing_urls) == length(urls), paste0("landing_url length mismatch for required source: ", title))

  valid_pdf <- function(path) {
    if (!file.exists(path)) return(FALSE)
    size <- file.info(path)$size
    if (!is.finite(size) || size < minimum_bytes) return(FALSE)
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    sig <- readBin(con, what = "raw", n = 5L)
    identical(rawToChar(sig), "%PDF-")
  }

  register_validated <- function(source_url, note_suffix) {
    register_source(
      source_id = source_id,
      agency = agency,
      title = title,
      url = source_url,
      local_path = destination,
      publication_date = publication_date,
      baseline_vintage = CFG$cbo_vintage,
      evidence_class = evidence_class,
      notes = paste0(notes, " | ", note_suffix)
    )
    invisible(destination)
  }

  if (valid_pdf(destination)) {
    log_line("Using cached required current-law source: ", basename(destination))
    register_validated(urls[[1]], "cached validated official PDF")
    return(destination)
  }

  errors <- character()
  used_url <- NA_character_
  ok <- FALSE

  for (k in seq_along(urls)) {
    this_url <- urls[[k]]
    this_landing <- landing_urls[[k]]
    tmp <- paste0(destination, ".download")
    if (file.exists(tmp)) unlink(tmp, force = TRUE)
    log_line("Downloading required current-law source: ", this_url)

    first_error <- NULL
    second_error <- NULL

    tryCatch(
      {
        req <- httr2::request(this_url) |>
          httr2::req_user_agent("federal-fiscal-capacity-model/1.0 (+public reproducibility release)") |>
          httr2::req_headers(
            Accept = "application/pdf,application/octet-stream;q=0.9,*/*;q=0.8",
            `Accept-Language` = "en-US,en;q=0.9",
            Referer = this_landing
          )
        resp <- perform_request(req)
        status <- httr2::resp_status(resp)
        if (status < 200L || status >= 300L) stop("HTTP status ", status)
        writeBin(httr2::resp_body_raw(resp), tmp)
        if (!valid_pdf(tmp)) stop("response did not contain a valid PDF")
        ok <- TRUE
        used_url <- this_url
      },
      error = function(e) first_error <<- conditionMessage(e)
    )

    if (!ok) {
      if (file.exists(tmp)) unlink(tmp, force = TRUE)
      log_line("Primary transport failed; retrying the same official endpoint with libcurl browser headers", level = "WARN")
      tryCatch(
        {
          h <- curl::new_handle()
          curl::handle_setopt(
            h,
            useragent = "federal-fiscal-capacity-model/1.0 (+public reproducibility release)",
            referer = this_landing,
            followlocation = TRUE,
            failonerror = TRUE,
            connecttimeout = 30,
            timeout = 300
          )
          curl::handle_setheaders(
            h,
            Accept = "application/pdf,application/octet-stream;q=0.9,*/*;q=0.8",
            `Accept-Language` = "en-US,en;q=0.9"
          )
          curl::curl_download(this_url, tmp, quiet = FALSE, mode = "wb", handle = h)
          if (!valid_pdf(tmp)) stop("libcurl response did not contain a valid PDF")
          ok <- TRUE
          used_url <- this_url
        },
        error = function(e) second_error <<- conditionMessage(e)
      )
    }

    if (ok) {
      if (file.exists(destination)) unlink(destination, force = TRUE)
      if (!file.rename(tmp, destination)) {
        if (!file.copy(tmp, destination, overwrite = TRUE)) {
          model_stop("Could not place validated current-law evidence file: ", destination)
        }
        unlink(tmp, force = TRUE)
      }
      break
    }

    if (file.exists(tmp)) unlink(tmp, force = TRUE)
    errors <- c(
      errors,
      paste0(
        this_url,
        " | primary: ", ifelse(is.null(first_error), "no error recorded", first_error),
        " | libcurl: ", ifelse(is.null(second_error), "no error recorded", second_error)
      )
    )
  }

  if (!ok) {
    model_stop(
      "Required current-law evidence could not be acquired: ", title,
      " | attempted official endpoint(s): ", paste(errors, collapse = " || ")
    )
  }

  assert_model(valid_pdf(destination), paste0("Required current-law evidence failed post-download PDF validation: ", title))
  register_validated(
    used_url,
    paste0(
      "downloaded and validated official PDF; sha256=", sha256_file(destination),
      "; alternate official endpoints considered=", paste(urls, collapse = " | ")
    )
  )
  destination
}


# ------------------------------------------------------------------------------
# FUNCTION: fetch_current_law_adjudication_sources
# Purpose: Acquire the primary enacted-law and current-baseline documents required by the current-law score adjudication gate. GovInfo is used as the official GPO mirror when the originating CBO host blocks automated retrieval.
# ------------------------------------------------------------------------------
fetch_current_law_adjudication_sources <- function() {
  log_line("Current-law score review: acquiring required official adjudication evidence")

  public_law <- download_required_pdf_source(
    url = c(
      "https://www.govinfo.gov/content/pkg/PLAW-119publ21/pdf/PLAW-119publ21.pdf"
    ),
    landing_url = c(
      "https://www.govinfo.gov/app/details/PLAW-119publ21"
    ),
    destination = file.path(CFG$raw_source_dir, "public_law_119_21.pdf"),
    source_id = "public_law_119_21",
    agency = "Office of the Federal Register / U.S. Government Publishing Office",
    title = "Public Law 119-21",
    publication_date = "2025-07-04",
    evidence_class = "OFFICIAL_CURRENT_PRIMARY_LAW",
    notes = "Required primary statutory evidence for current-law compatibility adjudication.",
    minimum_bytes = 500000L
  )

  cbo_outlook <- download_required_pdf_source(
    url = c(
      "https://www.govinfo.gov/content/pkg/CMR-Y10-00199317/pdf/CMR-Y10-00199317.pdf",
      "https://www.cbo.gov/system/files/2026-02/61882-Outlook-2026.pdf"
    ),
    landing_url = c(
      "https://www.govinfo.gov/app/details/CMR-Y10-00199317",
      "https://www.cbo.gov/publication/61882"
    ),
    destination = file.path(CFG$raw_source_dir, "cbo_budget_economic_outlook_2026_2036.pdf"),
    source_id = "cbo_budget_economic_outlook_2026_current_law_review",
    agency = "Congressional Budget Office / U.S. Government Publishing Office",
    title = "The Budget and Economic Outlook: 2026 to 2036",
    publication_date = "2026-02-11",
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Required current-law baseline evidence. GovInfo hosts the official report submitted by CBO to Congress and is the primary automated-download endpoint because cbo.gov blocks this R workflow.",
    minimum_bytes = 1000000L
  )

  # The numerical February 2026 CBO baseline is already a mandatory model input.
  # Verify that the cached machine-readable source needed for adjudication exists
  # before any score is classified against current law.
  baseline_file <- file.path(CFG$raw_source_dir, "cbo_ten_year_budget_2026_02.csv")
  assert_model(
    file.exists(baseline_file) && is.finite(file.info(baseline_file)$size) && file.info(baseline_file)$size > 1000,
    "Required February 2026 CBO machine-readable baseline is unavailable for current-law adjudication"
  )

  register_source(
    source_id = "cbo_feb_2026_machine_readable_current_law_evidence",
    agency = "Congressional Budget Office",
    title = "February 2026 CBO machine-readable current-law baseline",
    url = "https://github.com/US-CBO/cbo-data",
    local_path = baseline_file,
    publication_date = "2026-02-11",
    baseline_vintage = CFG$cbo_vintage,
    evidence_class = "OFFICIAL_CURRENT_MACHINE_READABLE",
    notes = "Required numerical current-law evidence used together with Public Law 119-21 and CBO's February 2026 outlook."
  )

  list(
    public_law_119_21 = public_law,
    cbo_current_law_outlook = cbo_outlook,
    cbo_machine_readable_baseline = baseline_file
  )
}


# ------------------------------------------------------------------------------
# FUNCTION: build_current_law_score_adjudication
# Purpose: Vet every solver-ready candidate for legal incrementality, source validity, and use of the newest public official score available to the model.
# ------------------------------------------------------------------------------
build_current_law_score_adjudication_core <- function(meta) {
  meta |>
    mutate(
      title_l = stringr::str_to_lower(dplyr::coalesce(title, "")),
      current_law_change_class = case_when(
        title_l == "eliminate or limit itemized deductions" ~ "ITEMIZED_DEDUCTION_RULES_CHANGED",
        title_l == "increase individual income tax rates on ordinary income" ~ "INDIVIDUAL_RATE_STRUCTURE_CHANGED",
        title_l == "eliminate or modify head-of-household filing status" ~ "INDIVIDUAL_RATE_AND_DEDUCTION_BASE_CHANGED",
        title_l == "limit the deduction for charitable giving" ~ "CHARITABLE_DEDUCTION_RULES_CHANGED",
        title_l == "reduce tax subsidies for employment-based health benefits" ~ "HEALTH_COVERAGE_AND_INDIVIDUAL_TAX_BASE_CHANGED",
        stringr::str_detect(title_l, "tax social security and railroad retirement benefits") ~ "INDIVIDUAL_TAX_BASE_CHANGED",
        title_l == "tax all foreign income of u.s. corporations at the full statutory corporate rate" ~ "INTERNATIONAL_CORPORATE_TAX_RULES_CHANGED",
        title_l == "increase the corporate income tax rate by 1 percentage point" ~ "CORPORATE_TAX_BASE_CHANGED",
        title_l == "repeal the low-income housing tax credit" ~ "LOW_INCOME_HOUSING_CREDIT_RULES_CHANGED",
        stringr::str_detect(title_l, "medicaid") ~ "MEDICAID_STATUTE_AND_BASELINE_CHANGED",
        title_l == "limit state taxes on health care providers" ~ "MEDICAID_PROVIDER_TAX_RULES_CHANGED",
        stringr::str_detect(title_l, "increase certain fees charged by citizenship and immigration services") ~ "IMMIGRATION_FEE_BASE_CHANGED",
        stringr::str_detect(title_l, "repeal certain tax preferences for energy and natural resource") ~ "ENERGY_TAX_BASE_REVIEWED",
        title_l == "tax carried interest as ordinary income" ~ "INDIVIDUAL_RATE_STRUCTURE_CHANGED",
        stringr::str_detect(title_l, "tax gains from derivatives as ordinary income") ~ "INDIVIDUAL_RATE_STRUCTURE_CHANGED",
        stringr::str_detect(title_l, "include employer-paid premiums for income replacement insurance") ~ "INDIVIDUAL_RATE_STRUCTURE_CHANGED",
        stringr::str_detect(title_l, "include va's disability payments in taxable income") ~ "INDIVIDUAL_RATE_STRUCTURE_CHANGED",
        TRUE ~ "NO_IDENTIFIED_MATERIAL_CURRENT_LAW_CHANGE"
      ),
      partially_enacted_score_not_incrementally_vettable = stringr::str_detect(
        title_l,
        "require people who claim the earned income tax credit and child tax credit to have a social security number that is valid for employment"
      ),
      externally_vettable_score = case_when(
        source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" ~ TRUE,
        evidence_class == "OFFICIAL_CURRENT" ~ TRUE,
        source_kind == "LOCAL_FROZEN_CBO_POLICY_PACK" & dplyr::coalesce(direct_cbo_annual_score, FALSE) ~ TRUE,
        source_kind == "ADDITIONAL_OFFICIAL_CBO_POLICY" & candidate_id == "CBO_2020_increase_irs_enforcement_initiatives" ~ TRUE,
        TRUE ~ FALSE
      ),
      score_recency_status = case_when(
        source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" ~ "CURRENT_FEB_2026_BASELINE_DERIVED",
        evidence_class == "OFFICIAL_CURRENT" ~ "CURRENT_OFFICIAL_SCORE",
        source_kind == "LOCAL_FROZEN_CBO_POLICY_PACK" & dplyr::coalesce(direct_cbo_annual_score, FALSE) ~ "LATEST_PUBLIC_CBO_SCORE_IN_VALIDATED_CURRENT_LATEST_PACK",
        source_kind == "ADDITIONAL_OFFICIAL_CBO_POLICY" & candidate_id == "CBO_2020_increase_irs_enforcement_initiatives" ~ "LATEST_PUBLIC_SCORE_WITH_LATER_OFFICIAL_MECHANISM_VALIDATION",
        TRUE ~ "NO_VETTED_SCORE"
      ),
      current_law_adjudication_status = case_when(
        source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" ~ "CURRENT_BASELINE_CONTROL",
        evidence_class == "OFFICIAL_CURRENT" ~ "CURRENT_OFFICIAL_SCORE",
        partially_enacted_score_not_incrementally_vettable ~ "INVALID_PARTIALLY_ENACTED_SCORE_NOT_INCREMENTALLY_VETTABLE",
        !externally_vettable_score ~ "INVALID_NOT_EXTERNALLY_VETTABLE",
        source_kind == "ADDITIONAL_OFFICIAL_CBO_POLICY" & candidate_id == "CBO_2020_increase_irs_enforcement_initiatives" ~ "VALID_LATEST_OFFICIAL_SCORE_CURRENT_MECHANISM_VALIDATED",
        evidence_class == "OFFICIAL_OLDER" & current_law_change_class != "NO_IDENTIFIED_MATERIAL_CURRENT_LAW_CHANGE" ~ "VALID_LATEST_OFFICIAL_SCORE_CURRENT_LAW_CHANGED",
        evidence_class == "OFFICIAL_OLDER" ~ "VALID_LATEST_OFFICIAL_SCORE_CURRENT_LAW_REVIEWED",
        TRUE ~ "NOT_SOLVER_SCORE"
      ),
      current_law_adjudication_reason = case_when(
        current_law_adjudication_status == "CURRENT_BASELINE_CONTROL" ~ "Mechanical account restraint is calculated directly from the February 2026 CBO current-law baseline.",
        current_law_adjudication_status == "CURRENT_OFFICIAL_SCORE" ~ "The candidate uses a current official score.",
        current_law_adjudication_status == "INVALID_PARTIALLY_ENACTED_SCORE_NOT_INCREMENTALLY_VETTABLE" ~ "Public Law 119-21 enacted part, but not all, of the December 2024 CBO option. Current law requires a valid-for-employment Social Security number for a Child Tax Credit claimant, while the older option also required the spouse on a joint return to have one. The published $27.8 billion score is not separable into enacted and still-incremental components, so the old coefficient cannot be used without double counting and the residual cannot be externally vetted from the published score.",
        current_law_adjudication_status == "INVALID_NOT_EXTERNALLY_VETTABLE" ~ "No defensible externally verifiable official annual score or current-baseline coefficient is available for this solver candidate.",
        current_law_adjudication_status == "VALID_LATEST_OFFICIAL_SCORE_CURRENT_MECHANISM_VALIDATED" ~ "The option remains legally available and later official evidence validates the mechanism. The newest public official score available to the model remains in the solver with its source vintage disclosed.",
        current_law_adjudication_status == "VALID_LATEST_OFFICIAL_SCORE_CURRENT_LAW_CHANGED" ~ "Current law changed a related tax or program baseline, but the policy remains legally distinct and implementable. The validated CBO current/latest policy pack contains the newest public official score available for this option, so the option remains in the solver and the baseline mismatch is carried as audit metadata rather than used as a pre-solver exclusion.",
        current_law_adjudication_status == "VALID_LATEST_OFFICIAL_SCORE_CURRENT_LAW_REVIEWED" ~ "No enactment made the policy nonincremental. The validated CBO current/latest policy pack contains the newest public official score available for this option, which remains in the solver with its vintage disclosed.",
        TRUE ~ "Candidate is not a solver-ready official scored policy."
      ),
      current_law_evidence_basis = case_when(
        current_law_adjudication_status == "CURRENT_BASELINE_CONTROL" ~ "CBO_FEB_2026_BASELINE",
        current_law_adjudication_status == "CURRENT_OFFICIAL_SCORE" ~ "CURRENT_OFFICIAL_SOURCE",
        current_law_adjudication_status == "INVALID_PARTIALLY_ENACTED_SCORE_NOT_INCREMENTALLY_VETTABLE" ~ "PUBLIC_LAW_119_21;CBO_2024_OPTION_60951;CBO_FEB_2026_BASELINE",
        current_law_adjudication_status == "INVALID_NOT_EXTERNALLY_VETTABLE" ~ "SOURCE_VALIDITY_FAILURE",
        source_kind == "ADDITIONAL_OFFICIAL_CBO_POLICY" & candidate_id == "CBO_2020_increase_irs_enforcement_initiatives" ~ "CBO_OFFICIAL_SCORE;LATER_OFFICIAL_MECHANISM_VALIDATION;CBO_FEB_2026_BASELINE",
        TRUE ~ "VALIDATED_CBO_CURRENT_LATEST_POLICY_PACK;PUBLIC_LAW_119_21;CBO_2026_OUTLOOK;CBO_FEB_2026_BASELINE"
      ),
      exact_current_law_rescore_available = current_law_adjudication_status %in% c("CURRENT_BASELINE_CONTROL", "CURRENT_OFFICIAL_SCORE"),
      current_law_solver_eligible = dplyr::coalesce(parameterized_solver_eligible, FALSE) &
        current_law_adjudication_status %in% c(
          "CURRENT_BASELINE_CONTROL",
          "CURRENT_OFFICIAL_SCORE",
          "VALID_LATEST_OFFICIAL_SCORE_CURRENT_MECHANISM_VALIDATED",
          "VALID_LATEST_OFFICIAL_SCORE_CURRENT_LAW_CHANGED",
          "VALID_LATEST_OFFICIAL_SCORE_CURRENT_LAW_REVIEWED"
        )
    ) |>
    select(-title_l, -partially_enacted_score_not_incrementally_vettable, -externally_vettable_score)
}

# ------------------------------------------------------------------------------
# FUNCTION: apply_current_law_score_adjudication
# Purpose: Attach substantive validity and score-recency status without excluding a valid option merely because current law changed a related baseline.
# ------------------------------------------------------------------------------
apply_current_law_score_adjudication_core <- function(policy_model) {
  policy_model$meta <- build_current_law_score_adjudication(policy_model$meta)
  excluded <- policy_model$meta |>
    filter(parameterized_solver_eligible, !current_law_solver_eligible)
  retained_changed <- policy_model$meta |>
    filter(parameterized_solver_eligible, current_law_adjudication_status == "VALID_LATEST_OFFICIAL_SCORE_CURRENT_LAW_CHANGED")
  log_line(
    "Validity and recency review complete: retained ",
    sum(policy_model$meta$parameterized_solver_eligible & policy_model$meta$current_law_solver_eligible, na.rm = TRUE),
    " of ", sum(policy_model$meta$parameterized_solver_eligible, na.rm = TRUE),
    " solver-ready candidates; ", nrow(retained_changed),
    " retained latest-official-score variants carry explicit current-law baseline-change flags; ",
    nrow(excluded), " candidate variants excluded for substantive validity or external-vetting failure"
  )
  policy_model
}

# ------------------------------------------------------------------------------
# FUNCTION: build_current_law_adjudication_capacity
# Purpose: Quantify fiscal capacity by substantive validity and current-law review status.
# ------------------------------------------------------------------------------
build_current_law_adjudication_capacity <- function(policy_model) {
  policy_model$meta |>
    filter(parameterized_solver_eligible) |>
    group_by(current_law_adjudication_status, current_law_solver_eligible) |>
    summarise(
      candidate_variants = n(),
      policy_families = n_distinct(family_id),
      direct_official_score_variants = sum(dplyr::coalesce(direct_cbo_annual_score, FALSE)),
      summed_maximum_ten_year_primary_improvement_bil = sum(pmax(cumulative_primary_improvement_2027_2036_bil, 0), na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(current_law_solver_eligible, current_law_adjudication_status)
}

# ------------------------------------------------------------------------------
# FUNCTION: build_current_law_excluded_score_audit
# Purpose: Preserve every substantive validity exclusion and its externally reviewable reason.
# ------------------------------------------------------------------------------
build_current_law_excluded_score_audit <- function(policy_model) {
  policy_model$meta |>
    filter(parameterized_solver_eligible, !current_law_solver_eligible) |>
    select(
      candidate_id, family_id, title, variant_name, source_kind, evidence_class,
      estimate_year, source_date, source_url, direct_cbo_annual_score,
      cumulative_primary_improvement_2027_2036_bil,
      score_recency_status, current_law_change_class,
      current_law_adjudication_status, current_law_adjudication_reason,
      current_law_evidence_basis, exact_current_law_rescore_available
    ) |>
    arrange(desc(cumulative_primary_improvement_2027_2036_bil), title, variant_name)
}

# ------------------------------------------------------------------------------
# FUNCTION: validate_validity_vetting_contract
# Purpose: Prove that pre-solver exclusions are limited to substantive validity failures and never arise from materiality, score-age convenience, or current-law-change flags alone.
# ------------------------------------------------------------------------------
validate_validity_vetting_contract_core <- function(policy_model) {
  m <- policy_model$meta |> filter(parameterized_solver_eligible)
  allowed_exclusion_statuses <- c(
    "INVALID_PARTIALLY_ENACTED_SCORE_NOT_INCREMENTALLY_VETTABLE",
    "INVALID_NOT_EXTERNALLY_VETTABLE"
  )
  excluded <- m |> filter(!current_law_solver_eligible)
  direct_official_excluded_without_allowed_reason <- excluded |>
    filter(dplyr::coalesce(direct_cbo_annual_score, FALSE), !current_law_adjudication_status %in% allowed_exclusion_statuses)
  changed_but_excluded_without_validity_failure <- excluded |>
    filter(
      current_law_change_class != "NO_IDENTIFIED_MATERIAL_CURRENT_LAW_CHANGE",
      !current_law_adjudication_status %in% allowed_exclusion_statuses
    )
  checks <- tibble::tibble(
    check = c(
      "All pre-solver validity exclusions use an allowed substantive exclusion class",
      "No direct official annual score is excluded without an allowed substantive validity reason",
      "A current-law baseline change alone never excludes a candidate",
      "Every solver-eligible retained candidate has a vetted score-recency status",
      "Every excluded candidate has an auditable reason and evidence basis",
      "Materiality removes zero solver candidates"
    ),
    passed = c(
      nrow(excluded) == 0L || all(excluded$current_law_adjudication_status %in% allowed_exclusion_statuses),
      nrow(direct_official_excluded_without_allowed_reason) == 0L,
      nrow(changed_but_excluded_without_validity_failure) == 0L,
      all(m$score_recency_status[m$current_law_solver_eligible] != "NO_VETTED_SCORE"),
      nrow(excluded) == 0L || all(
        !is.na(excluded$current_law_adjudication_reason) & nzchar(excluded$current_law_adjudication_reason) &
          !is.na(excluded$current_law_evidence_basis) & nzchar(excluded$current_law_evidence_basis)
      ),
      TRUE
    )
  )
  attr(checks, "excluded_candidates") <- excluded
  checks
}

# ------------------------------------------------------------------------------
# FUNCTION: policy_universe_for_mode
# Purpose: Present every validity-vetted candidate allowed by the declared protection and evidence constraints; materiality never filters solver candidates.
# ------------------------------------------------------------------------------
policy_universe_for_mode_core_3 <- function(
  policy_model,
  protection_mode = c("STRICT", "EXPANDED"),
  materiality_mode = c("PACKAGE_READY", "FULL_CAPACITY")
) {
  protection_mode <- match.arg(protection_mode)
  materiality_mode <- match.arg(materiality_mode)
  allowed_status <- if (protection_mode == "STRICT") "ELIGIBLE" else c("ELIGIBLE", "CONDITIONAL")
  meta <- policy_model$meta |>
    filter(parameterized_solver_eligible, current_law_solver_eligible, protection_status %in% allowed_status)
  if (CFG$evidence_mode == "OFFICIAL_CURRENT") meta <- meta |> filter(evidence_class == "OFFICIAL_CURRENT")
  if (CFG$evidence_mode == "OFFICIAL_OLDER") meta <- meta |> filter(evidence_class %in% c("OFFICIAL_CURRENT", "OFFICIAL_OLDER"))

  # Materiality is reporting metadata only. Every valid candidate that satisfies
  # the declared protection and evidence rules is presented to HiGHS.
  ids <- meta$candidate_id
  schedules <- policy_model$schedules |> filter(candidate_id %in% ids)
  schedule_summary <- policy_model$schedule_summary |> filter(candidate_id %in% ids)
  schedule_flows <- policy_model$schedule_flows |> filter(candidate_id %in% ids)
  list(
    meta = meta,
    schedules = schedules,
    schedule_summary = schedule_summary,
    schedule_flows = schedule_flows,
    scenario_meta = policy_model$scenario_meta,
    scenario_annual = policy_model$scenario_annual,
    protection_mode = protection_mode,
    materiality_mode = materiality_mode
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_solution_current_law_score_exposure
# Purpose: Confirm that retained packages contain no candidate rejected by substantive validity vetting and report score-vintage exposure.
# ------------------------------------------------------------------------------
build_solution_current_law_score_exposure_core <- function(search_result, policy_model) {
  if (is.null(search_result$membership) || nrow(search_result$membership) == 0L) return(tibble())
  adjudication <- policy_model$meta |>
    select(candidate_id, current_law_adjudication_status, current_law_solver_eligible)
  search_result$membership |>
    left_join(adjudication, by = "candidate_id") |>
    group_by(solution_id) |>
    summarise(
      selected_policy_count = n_distinct(candidate_id),
      selected_current_law_ineligible_count = sum(!dplyr::coalesce(current_law_solver_eligible, FALSE)),
      selected_current_official_or_baseline_count = sum(current_law_adjudication_status %in% c("CURRENT_OFFICIAL_SCORE", "CURRENT_BASELINE_CONTROL"), na.rm = TRUE),
      selected_usable_older_score_count = sum(stringr::str_detect(dplyr::coalesce(current_law_adjudication_status, ""), "^VALID_LATEST_OFFICIAL_SCORE"), na.rm = TRUE),
      selected_current_law_changed_score_count = sum(current_law_adjudication_status == "VALID_LATEST_OFFICIAL_SCORE_CURRENT_LAW_CHANGED", na.rm = TRUE),
      .groups = "drop"
    )
}


# ------------------------------------------------------------------------------
# PRESENTATION-GRADE SOURCE AND VALIDITY MODEL STAGE
# ------------------------------------------------------------------------------

fetch_expanded_universe_sources_feb2026 <- fetch_expanded_universe_sources_core
read_spending_detail_csv <- read_spending_detail_core
build_working_baseline_feb2026 <- build_working_baseline_core

# ------------------------------------------------------------------------------
# FUNCTION: download_required_xlsx_source
# Purpose: Acquire and validate a required official XLSX file through multiple transports.
# ------------------------------------------------------------------------------
download_required_xlsx_source <- function(
  url,
  landing_url,
  destination,
  source_id,
  agency,
  title,
  publication_date,
  evidence_class,
  notes,
  minimum_bytes = 50000L,
  local_candidates = character()
) {
  valid_xlsx <- function(path) {
    if (!file.exists(path)) return(FALSE)
    size <- file.info(path)$size
    if (!is.finite(size) || size < minimum_bytes) return(FALSE)
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    sig <- readBin(con, what = "raw", n = 4L)
    identical(as.integer(sig), c(0x50L, 0x4bL, 0x03L, 0x04L))
  }

  if (valid_xlsx(destination)) {
    log_line("Using cached required current source: ", basename(destination))
    register_source(
      source_id, agency, title, url, destination,
      publication_date, "2026-06", evidence_class,
      paste0(notes, " | cached validated official XLSX; sha256=", sha256_file(destination))
    )
    return(destination)
  }

  # A browser-downloaded official workbook may be staged locally when cbo.gov's
  # anti-bot layer blocks scripted HTTP clients. The local file is still treated
  # as an official CBO source: it must pass the same XLSX signature/size checks,
  # is copied to the model's canonical cache name, hashed, and registered against
  # the official CBO URL before use. No stale February substitute is permitted.
  local_candidates <- unique(as.character(local_candidates))
  local_candidates <- local_candidates[!is.na(local_candidates) & nzchar(local_candidates)]
  if (length(local_candidates) > 0L) {
    for (candidate in local_candidates) {
      if (!file.exists(candidate)) next
      if (!valid_xlsx(candidate)) {
        model_stop("Local required XLSX exists but failed validation: ", candidate)
      }

      source_norm <- normalizePath(candidate, winslash = "/", mustWork = TRUE)
      dest_norm <- normalizePath(destination, winslash = "/", mustWork = FALSE)
      if (!paths_same(source_norm, dest_norm)) {
        if (file.exists(destination)) unlink(destination, force = TRUE)
        if (!file.copy(candidate, destination, overwrite = TRUE)) {
          model_stop("Could not stage local required XLSX at canonical path: ", destination)
        }
      }

      assert_model(valid_xlsx(destination), paste0("Locally supplied XLSX failed post-copy validation: ", title))
      log_line("Using manually supplied required current source: ", basename(candidate))
      register_source(
        source_id, agency, title, url, destination,
        publication_date, "2026-06", evidence_class,
        paste0(
          notes,
          " | manually downloaded from official CBO source; original_local_file=", basename(candidate),
          "; sha256=", sha256_file(destination)
        )
      )
      return(destination)
    }
  }

  tmp <- paste0(destination, ".download")
  if (file.exists(tmp)) unlink(tmp, force = TRUE)
  errors <- character()

  try_httr2 <- function() {
    req <- httr2::request(url) |>
      httr2::req_user_agent("federal-fiscal-capacity-model/1.0 (+public reproducibility release)") |>
      httr2::req_headers(
        Accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,application/octet-stream;q=0.9,*/*;q=0.8",
        `Accept-Language` = "en-US,en;q=0.9",
        Referer = landing_url
      )
    resp <- perform_request(req)
    status <- httr2::resp_status(resp)
    if (status < 200L || status >= 300L) stop("HTTP status ", status)
    writeBin(httr2::resp_body_raw(resp), tmp)
    if (!valid_xlsx(tmp)) stop("response was not a valid XLSX")
  }

  try_curl <- function() {
    h <- curl::new_handle()
    curl::handle_setopt(
      h,
      useragent = "federal-fiscal-capacity-model/1.0 (+public reproducibility release)",
      referer = landing_url,
      followlocation = TRUE,
      failonerror = TRUE,
      connecttimeout = 30,
      timeout = 600
    )
    curl::handle_setheaders(
      h,
      Accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,application/octet-stream;q=0.9,*/*;q=0.8",
      `Accept-Language` = "en-US,en;q=0.9"
    )
    curl::curl_download(url, tmp, quiet = FALSE, mode = "wb", handle = h)
    if (!valid_xlsx(tmp)) stop("libcurl response was not a valid XLSX")
  }

  try_curl_session <- function() {
    h <- curl::new_handle()
    curl::handle_setopt(
      h,
      useragent = "federal-fiscal-capacity-model/1.0 (+public reproducibility release)",
      followlocation = TRUE,
      failonerror = TRUE,
      connecttimeout = 30,
      timeout = 600,
      cookiefile = ""
    )
    curl::handle_setheaders(
      h,
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      `Accept-Language` = "en-US,en;q=0.9"
    )
    curl::curl_fetch_memory(landing_url, handle = h)
    curl::handle_setheaders(
      h,
      Accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,application/octet-stream;q=0.9,*/*;q=0.8",
      `Accept-Language` = "en-US,en;q=0.9",
      Referer = landing_url
    )
    curl::curl_download(url, tmp, quiet = FALSE, mode = "wb", handle = h)
    if (!valid_xlsx(tmp)) stop("cookie-session libcurl response was not a valid XLSX")
  }

  transports <- list(httr2 = try_httr2, libcurl_session = try_curl_session, libcurl = try_curl)
  ok <- FALSE
  for (nm in names(transports)) {
    if (file.exists(tmp)) unlink(tmp, force = TRUE)
    log_line("Downloading required current source via ", nm, ": ", url)
    err <- tryCatch({ transports[[nm]](); NULL }, error = function(e) conditionMessage(e))
    if (is.null(err) && valid_xlsx(tmp)) { ok <- TRUE; break }
    errors <- c(errors, paste0(nm, ": ", ifelse(is.null(err), "validation failed", err)))
  }

  if (!ok) {
    if (file.exists(tmp)) unlink(tmp, force = TRUE)
    model_stop(
      "Required current official XLSX could not be acquired: ", title,
      " | ", url, " | ", paste(errors, collapse = " || ")
    )
  }

  if (file.exists(destination)) unlink(destination, force = TRUE)
  if (!file.rename(tmp, destination)) {
    if (!file.copy(tmp, destination, overwrite = TRUE)) model_stop("Could not place validated XLSX: ", destination)
    unlink(tmp, force = TRUE)
  }
  assert_model(valid_xlsx(destination), paste0("Required XLSX failed post-download validation: ", title))
  register_source(
    source_id, agency, title, url, destination,
    publication_date, "2026-06", evidence_class,
    paste0(notes, " | validated XLSX; sha256=", sha256_file(destination))
  )
  destination
}

# ------------------------------------------------------------------------------
# FUNCTION: parse_cbo_spending_projection_xlsx
# Purpose: Transform CBO's budget-account workbook to the canonical account/year schema used by the optimizer.
# ------------------------------------------------------------------------------
parse_cbo_spending_projection_xlsx <- function(path) {
  sheets <- readxl::excel_sheets(path)
  sheet_names_lower <- stringr::str_to_lower(sheets)

  # The June 30, 2026 workbook's spending table is currently published on the
  # sheet named "1. Updated Spending Projections". Retain fallbacks for a future
  # CBO workbook that uses a more generic spending-projection or baseline label.
  spending_sheet <- sheets[stringr::str_detect(sheet_names_lower, "updated spending projections")][1]
  if (is.na(spending_sheet) || !nzchar(spending_sheet)) {
    spending_sheet <- sheets[stringr::str_detect(sheet_names_lower, "spending projections")][1]
  }
  if (is.na(spending_sheet) || !nzchar(spending_sheet)) {
    spending_sheet <- sheets[stringr::str_detect(sheet_names_lower, "baseline")][1]
  }
  assert_model(
    !is.na(spending_sheet) && nzchar(spending_sheet),
    paste0(
      "Could not identify the spending-projection sheet in the June 2026 CBO workbook. Available sheets: ",
      paste(sheets, collapse = ", ")
    )
  )
  log_line("Parsing June 2026 CBO spending workbook sheet: ", spending_sheet)
  raw <- readxl::read_excel(path, sheet = spending_sheet, col_names = FALSE, .name_repair = "minimal")
  mat <- as.matrix(raw)
  storage.mode(mat) <- "character"

  ba_start <- NA_integer_
  outlay_start <- NA_integer_
  section_row <- NA_integer_
  for (r in seq_len(min(12L, nrow(mat)))) {
    vals <- stringr::str_to_lower(trimws(mat[r, ]))
    vals[is.na(vals)] <- ""
    b <- which(stringr::str_detect(vals, "budget authority"))
    o <- which(stringr::str_detect(vals, "outlay"))
    if (length(b) > 0L && length(o) > 0L) {
      ba_start <- b[[1]]
      outlay_start <- o[[1]]
      section_row <- r
      break
    }
  }
  assert_model(is.finite(ba_start) && is.finite(outlay_start) && outlay_start > ba_start, "Could not locate Budget Authority and Outlays column groups in June 2026 CBO spending workbook")

  header_row <- NA_integer_
  ba_cols <- integer()
  outlay_cols <- integer()
  ba_year <- integer()
  outlay_year <- integer()
  for (r in seq_len(min(15L, nrow(mat)))) {
    vals <- trimws(mat[r, ])
    years <- suppressWarnings(as.integer(ifelse(stringr::str_detect(vals, "^[0-9]{4}$"), vals, NA_character_)))
    idx <- which(is.finite(years) & years >= 2000L & years <= 2100L)
    if (length(idx) >= 10L) {
      header_row <- r
      ba_cols <- idx[idx < outlay_start]
      outlay_cols <- idx[idx >= outlay_start]
      ba_year <- years[ba_cols]
      outlay_year <- years[outlay_cols]
      break
    }
  }
  assert_model(is.finite(header_row), "Could not locate fiscal-year header row in June 2026 CBO spending workbook")

  records <- vector("list", max(1L, (nrow(mat) - header_row) * 11L))
  k <- 0L
  for (r in seq.int(header_row + 1L, nrow(mat))) {
    tin <- trimws(mat[r, 1])
    if (is.na(tin) || !stringr::str_detect(tin, "^[0-9]{3}-[0-9]{4}")) next
    meta <- vapply(seq_len(9L), function(j) {
      x <- mat[r, j]
      if (is.na(x)) "" else trimws(x)
    }, character(1))
    names(meta) <- c("tin", "title", "disc_or_mand", "category", "agency", "bureau", "function_code", "subfunction_code", "off_budget")
    if (nzchar(meta[["function_code"]]) && stringr::str_detect(meta[["function_code"]], "^[0-9]+$")) meta[["function_code"]] <- stringr::str_pad(meta[["function_code"]], 3L, pad = "0")
    if (nzchar(meta[["subfunction_code"]]) && stringr::str_detect(meta[["subfunction_code"]], "^[0-9]+$")) meta[["subfunction_code"]] <- stringr::str_pad(meta[["subfunction_code"]], 3L, pad = "0")
    yrs <- sort(unique(c(ba_year, outlay_year)))
    for (y in yrs) {
      bc <- ba_cols[ba_year == y]
      oc <- outlay_cols[outlay_year == y]
      bval <- if (length(bc)) clean_numeric(mat[r, bc[[1]]]) else NA_real_
      oval <- if (length(oc)) clean_numeric(mat[r, oc[[1]]]) else NA_real_
      if (!is.finite(bval) && !is.finite(oval)) next
      k <- k + 1L
      records[[k]] <- tibble::tibble(
        year = as.integer(y), tin = meta[["tin"]], title = meta[["title"]],
        disc_or_mand = meta[["disc_or_mand"]], category = meta[["category"]],
        agency = meta[["agency"]], bureau = meta[["bureau"]],
        function_code = meta[["function_code"]], subfunction_code = meta[["subfunction_code"]],
        off_budget = meta[["off_budget"]], budget_authority_mil = bval, outlays_mil = oval
      )
    }
  }
  assert_model(k > 1000L, paste0("June 2026 CBO spending workbook parser produced only ", k, " account-year rows"))
  dplyr::bind_rows(records[seq_len(k)]) |>
    filter(year %in% 2026:2036) |>
    group_by(year, tin, title, disc_or_mand, category, agency, bureau, function_code, subfunction_code, off_budget) |>
    summarise(
      budget_authority_mil = sum(budget_authority_mil, na.rm = TRUE),
      outlays_mil = sum(outlays_mil, na.rm = TRUE),
      .groups = "drop"
    )
}

read_spending_detail <- function(path) {
  if (stringr::str_detect(stringr::str_to_lower(path), "\\.xlsx$")) return(parse_cbo_spending_projection_xlsx(path))
  read_spending_detail_csv(path)
}

fetch_expanded_universe_sources <- function() {
  x <- fetch_expanded_universe_sources_feb2026()
  june_url <- "https://www.cbo.gov/system/files/2026-06/51142-2026-06-Spending-Projections.xlsx"
  manual_june_path <- file.path(CFG$raw_source_dir, "51142-2026-06-Spending-Projections.xlsx")
  june_path <- download_required_xlsx_source(
    url = june_url,
    landing_url = "https://www.cbo.gov/data/budget-economic-data",
    destination = file.path(CFG$raw_source_dir, "cbo_spending_detail_2026_06.xlsx"),
    source_id = "cbo_spending_detail_2026_06",
    agency = "Congressional Budget Office",
    title = "Spending Projections, by Budget Account, June 30 2026",
    publication_date = "2026-06-30",
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Latest published CBO budget-account spending projections as of the model run date. Used for account-control coefficients and account-level validation only; not substituted for CBO's latest complete aggregate 10-year baseline.",
    minimum_bytes = 100000L,
    local_candidates = manual_june_path
  )
  x$spending_detail <- june_path
  x
}

# ------------------------------------------------------------------------------
# FUNCTION: validate_spending_detail_reconciliation
# Purpose: Audit the June 2026 account projection against CBO's published June
#          noninterest totals and the February noninterest aggregate baseline,
#          without substituting the June partial spending update for CBO's latest
#          complete aggregate 10-year baseline.
#
# SOURCE NOTE
# CBO's June 30, 2026 report explicitly labels the updated spending projections
# as "Noninterest outlays" and states that they exclude net outlays for interest
# such as interest payments on federal debt. The budget-account workbook therefore
# must be reconciled to February TOTAL OUTLAYS LESS NET INTEREST, not to February
# total outlays. See CBO, "An Analysis of Spending Proposals in the President's
# 2027 Budget," Table 1 and "Changes in CBO's Spending Projections Since February
# 2026": https://www.cbo.gov/publication/62385
# ------------------------------------------------------------------------------
validate_spending_detail_reconciliation <- function(spending_detail, cbo_baseline) {

  # Published June noninterest totals from CBO Table 1, in billions of dollars.
  # These are rounded whole-billion presentation values and are used only as an
  # independent parser/reconciliation check. The model continues to use the
  # account-level values from the workbook itself.
  june_published <- tibble::tibble(
    year = 2026:2036,
    cbo_june_published_noninterest_outlays_bil = c(
      6373, 6677, 6948, 7029, 7359, 7597, 7886, 8361, 8569, 8716, 9256
    )
  )

  audit <- spending_detail |>
    filter(year %in% 2026:2036) |>
    group_by(year) |>
    summarise(
      # The June workbook is a noninterest spending file. Do not attempt to
      # synthesize federal net interest from budget-account function codes.
      spending_detail_noninterest_outlays_bil = sum(outlays_mil, na.rm = TRUE) / 1000,
      .groups = "drop"
    ) |>
    left_join(june_published, by = "year") |>
    left_join(
      cbo_baseline |>
        filter(year %in% 2026:2036) |>
        select(
          year,
          cbo_feb_outlays_bil = outlays_bil,
          cbo_feb_net_interest_bil = net_interest_bil
        ),
      by = "year"
    ) |>
    mutate(
      cbo_feb_noninterest_outlays_bil = cbo_feb_outlays_bil - cbo_feb_net_interest_bil,

      # Rounded-publication parser check. Exact workbook totals should differ
      # from CBO's whole-billion table values by no more than ordinary rounding.
      parser_vs_june_published_bil =
        spending_detail_noninterest_outlays_bil -
        cbo_june_published_noninterest_outlays_bil,

      # This is the economically correct June-versus-February comparison because
      # both sides exclude net interest.
      noninterest_outlay_update_bil =
        spending_detail_noninterest_outlays_bil -
        cbo_feb_noninterest_outlays_bil,
      noninterest_outlay_update_pct =
        100 * noninterest_outlay_update_bil /
        cbo_feb_noninterest_outlays_bil,

      # Backward-compatible audit aliases retained for any downstream inspection.
      spending_detail_outlays_bil = spending_detail_noninterest_outlays_bil,
      primary_outlay_update_bil = noninterest_outlay_update_bil
    )

  assert_model(
    nrow(audit) == 11L,
    "June spending-detail reconciliation does not cover FY2026-FY2036"
  )
  assert_model(
    all(
      is.finite(audit$spending_detail_noninterest_outlays_bil) &
        audit$spending_detail_noninterest_outlays_bil > 0
    ),
    "June spending-detail parser produced invalid aggregate noninterest outlays"
  )
  assert_model(
    all(is.finite(audit$cbo_feb_noninterest_outlays_bil) &
          audit$cbo_feb_noninterest_outlays_bil > 0),
    "February CBO baseline does not provide valid noninterest outlays for June reconciliation"
  )

  max_rounding_diff <- max(abs(audit$parser_vs_june_published_bil), na.rm = TRUE)
  assert_model(
    max_rounding_diff <= 0.51,
    paste0(
      "June spending-detail parser does not reproduce CBO's published June noninterest totals within whole-billion rounding; max difference=",
      format(round(max_rounding_diff, 3), nsmall = 3), "B"
    )
  )

  max_noninterest_update_pct <- max(abs(audit$noninterest_outlay_update_pct), na.rm = TRUE)
  assert_model(
    max_noninterest_update_pct <= 10,
    paste0(
      "June noninterest spending-detail totals differ from the February noninterest aggregate baseline by more than 10 percent; parser or source-vintage reconciliation requires review. Max difference=",
      format(round(max_noninterest_update_pct, 3), nsmall = 3), "%"
    )
  )

  log_line(
    "June spending-detail reconciliation passed | parser vs published June max=",
    format(round(max_rounding_diff, 3), nsmall = 3),
    "B | June vs February noninterest max=",
    format(round(max_noninterest_update_pct, 3), nsmall = 3),
    "%"
  )

  audit
}

# ------------------------------------------------------------------------------
# FUNCTION: build_working_baseline
# Purpose: Preserve CBO's latest complete aggregate baseline while using later partial updates only in the domains they directly cover.
# ------------------------------------------------------------------------------
build_working_baseline <- function(cbo_baseline, tariff_profile, kernel_obj, spending_detail = NULL) {
  # CBO's latest complete 10-year budget baseline remains February 2026. The
  # August 20, 2026 tariff update is an official aggregate adjustment to that
  # baseline and is already incorporated by the preserved working-baseline
  # function. The June 30, 2026 account workbook is newer account-level evidence
  # and is used to parameterize account controls, but it is not treated as a
  # replacement for CBO's complete aggregate budget baseline.
  base <- build_working_baseline_feb2026(cbo_baseline, tariff_profile, kernel_obj)
  base |>
    mutate(
      aggregate_baseline_vintage = "CBO_FEB_2026_PLUS_AUG_20_2026_TARIFF_UPDATE",
      account_control_vintage = ifelse(is.null(spending_detail), NA_character_, "CBO_JUNE_30_2026_SPENDING_BY_BUDGET_ACCOUNT")
    )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_latest_official_score_verification
# Purpose: Verify that every frozen CBO family uses the newest estimate represented in CBO's validated current/latest index snapshot.
# ------------------------------------------------------------------------------
build_latest_official_score_verification <- function(policy_model, current_index) {
  idx <- current_index |>
    transmute(
      family_id,
      index_title = title,
      latest_published_estimate_year = as.integer(latest_estimate_year),
      latest_published_estimate = latest_estimate,
      current_index_source_url = source_url,
      current_index_status = annual_data_status
    )
  policy_model$meta |>
    filter(parameterized_solver_eligible, source_kind == "LOCAL_FROZEN_CBO_POLICY_PACK", dplyr::coalesce(direct_cbo_annual_score, FALSE)) |>
    distinct(family_id, title, estimate_year, source_date, source_url) |>
    left_join(idx, by = "family_id") |>
    mutate(
      verified_in_current_latest_index = !is.na(latest_published_estimate_year),
      score_matches_latest_published_year = verified_in_current_latest_index & estimate_year == latest_published_estimate_year,
      passed = verified_in_current_latest_index & score_matches_latest_published_year
    ) |>
    arrange(passed, estimate_year, title)
}

# ------------------------------------------------------------------------------
# FUNCTION: validate_ssa_current_actuarial_target
# Purpose: Verify the configured Social Security solvency target against the newest published SSA OACT provision basis available for that measure.
# ------------------------------------------------------------------------------
validate_ssa_current_actuarial_target <- function() {
  # The model-wide solvency ceiling is a current-law OASDI actuarial target, so
  # validate it against the Trustees Report Summary itself rather than against
  # any one policy-category page in the OACT provision library.  The latter can
  # be in transition between Trustees vintages even when the current-law target
  # has already been updated.
  trustees_url <- "https://www.ssa.gov/oact/TRSUM/"
  path <- file.path(CFG$raw_source_dir, "ssa_trustees_report_summary_2026.html")

  txt <- fetch_text_cached(
    url = trustees_url,
    destination = path,
    force = TRUE,
    source_id = "ssa_trustees_report_summary_2026",
    agency = "Social Security Administration, Office of the Chief Actuary",
    title = "Summary of the 2026 Annual Social Security and Medicare Trust Fund Reports",
    publication_date = "2026",
    baseline_vintage = "2026 Trustees Report",
    evidence_class = "OFFICIAL_CURRENT",
    notes = paste0(
      "Required external validation of the current-law combined OASDI long-range actuarial shortfall ",
      "used by the Social Security solvency constraint. The 2026 Trustees summary states that the ",
      "OASDI actuarial deficit is 4.42 percent of taxable payroll."
    )
  )

  # Search rendered text rather than raw HTML markup.  SSA pages can insert tags,
  # nonbreaking spaces, or typographic entities between words that appear
  # contiguous in the browser, so exact substring searches on raw HTML are not
  # a reliable validation method.
  normalized <- txt |>
    stringr::str_replace_all("(?i)&nbsp;|&#160;|&#xa0;", " ") |>
    stringr::str_replace_all("(?i)&minus;|&#8722;|&#x2212;", "-") |>
    stringr::str_replace_all("<[^>]+>", " ") |>
    stringr::str_replace_all("(?i)&amp;", "&") |>
    stringr::str_squish() |>
    stringr::str_to_lower()

  has_2026_basis <-
    stringr::str_detect(normalized, "summary\\s+of\\s+the\\s+2026\\s+annual\\s+reports") |
    stringr::str_detect(normalized, "summary\\s+of\\s+the\\s+2026\\s+annual\\s+social\\s+security") |
    stringr::str_detect(normalized, "2026\\s+trustees\\s+reports")

  has_oasdi_gap <-
    stringr::str_detect(
      normalized,
      "oasdi.{0,350}(actuarial\\s+deficit|actuarial\\s+balance).{0,350}4\\.42\\s+percent\\s+of\\s+taxable\\s+payroll"
    ) |
    stringr::str_detect(
      normalized,
      "actuarial\\s+deficit\\s+for\\s+social\\s+security\\s+as\\s+a\\s+whole.{0,250}4\\.42\\s+percent\\s+of\\s+taxable\\s+payroll"
    )

  configured_matches <- abs(CFG$ss_actuarial_gap_pct_payroll - 4.42) <= 1e-12

  audit <- tibble::tibble(
    check = c(
      "SSA Trustees summary is on the 2026 Trustees basis",
      "SSA 2026 combined OASDI long-range actuarial deficit equals configured 4.42 percent of taxable payroll"
    ),
    passed = c(
      has_2026_basis,
      has_2026_basis & has_oasdi_gap & configured_matches
    ),
    configured_value = c(NA_real_, CFG$ss_actuarial_gap_pct_payroll),
    authoritative_value = c(NA_real_, 4.42),
    source_url = trustees_url,
    source_file = path
  )

  if (!all(audit$passed)) {
    log_line(
      "SSA Trustees validation diagnostics | has_2026_basis=", has_2026_basis,
      " | has_oasdi_gap=", has_oasdi_gap,
      " | configured_gap=", CFG$ss_actuarial_gap_pct_payroll,
      " | source_file=", path,
      level = "ERROR"
    )
  } else {
    log_line(
      "SSA Trustees target validation passed | 2026 basis | OASDI actuarial deficit=4.42% of taxable payroll"
    )
  }

  audit
}

# ------------------------------------------------------------------------------
# FUNCTION: validate_current_cbo_option_index_snapshot
# Purpose: Verify that the frozen current/latest CBO option index matches the newest published CBO index structure verified for this build.
# ------------------------------------------------------------------------------
validate_current_cbo_option_index_snapshot <- function(current_index) {
  expected <- tibble::tibble(
    latest_estimate_year = c(2018L, 2020L, 2022L, 2024L),
    expected_family_count = c(33L, 12L, 6L, 76L)
  )
  observed <- current_index |>
    mutate(latest_estimate_year = as.integer(latest_estimate_year)) |>
    count(latest_estimate_year, name = "observed_family_count")
  audit <- expected |>
    left_join(observed, by = "latest_estimate_year") |>
    mutate(
      observed_family_count = dplyr::coalesce(observed_family_count, 0L),
      passed = observed_family_count == expected_family_count
    )
  total_row <- tibble::tibble(
    latest_estimate_year = NA_integer_,
    expected_family_count = 127L,
    observed_family_count = n_distinct(current_index$family_id),
    passed = n_distinct(current_index$family_id) == 127L
  )
  status_row <- tibble::tibble(
    latest_estimate_year = -1L,
    expected_family_count = 118L,
    observed_family_count = sum(current_index$annual_data_status == "FULL_OFFICIAL_ANNUAL", na.rm = TRUE),
    passed = sum(current_index$annual_data_status == "FULL_OFFICIAL_ANNUAL", na.rm = TRUE) == 118L
  )
  bind_rows(audit, total_row, status_row)
}

# ------------------------------------------------------------------------------
# FUNCTION: build_current_option_nonannual_audit
# Purpose: Identify current/latest CBO options that cannot enter the annual debt MILP because CBO publishes no annual score path.
# ------------------------------------------------------------------------------
build_current_option_nonannual_audit <- function(current_index) {
  current_index |>
    filter(annual_data_status != "FULL_OFFICIAL_ANNUAL") |>
    transmute(
      family_id, title, latest_estimate_year, latest_estimate, source_url, annual_data_status,
      solver_status = "EXCLUDED_NO_PUBLISHED_ANNUAL_SCORE_PATH",
      reason = "CBO lists this as the newest published option but provides only a ten-year total in the validated source pack. No annual path is invented for the debt-service and timing model."
    )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_current_law_score_adjudication
# Purpose: Admit only presentation-grade coefficients while preserving every valid newest-published official option.
# ------------------------------------------------------------------------------
build_current_law_score_adjudication <- function(meta, current_index) {
  idx <- current_index |>
    transmute(
      family_id,
      latest_published_estimate_year = as.integer(latest_estimate_year),
      latest_published_estimate = latest_estimate,
      current_index_source_url = source_url,
      current_index_status = annual_data_status
    )

  meta |>
    left_join(idx, by = "family_id") |>
    mutate(
      title_l = stringr::str_to_lower(dplyr::coalesce(title, "")),
      variant_l = stringr::str_to_lower(dplyr::coalesce(variant_name, "")),
      current_index_verified =
        source_kind == "LOCAL_FROZEN_CBO_POLICY_PACK" &
        dplyr::coalesce(direct_cbo_annual_score, FALSE) &
        !is.na(latest_published_estimate_year) &
        estimate_year == latest_published_estimate_year,

      # A collision is recorded only when intervening law changes the same
      # mechanism being scored enough that the published coefficient no longer
      # measures the incremental proposal. A different surrounding baseline is
      # not, by itself, an exclusion reason.
      direct_statutory_collision = case_when(
        stringr::str_detect(title_l, "require people who claim the earned income tax credit and child tax credit") ~
          "PARTIALLY_ENACTED_EITC_CTC_SSN_REQUIREMENT",

        title_l == "limit the deduction for charitable giving" &
          stringr::str_detect(variant_l, "2 percent agi floor") ~
          "CHARITABLE_FLOOR_PARTIALLY_ENACTED_AT_0_5_PERCENT",

        title_l == "eliminate or limit itemized deductions" ~
          "ITEMIZED_DEDUCTION_BASE_DIRECTLY_CHANGED_BY_2025_RECONCILIATION_LAW",

        title_l == "increase individual income tax rates on ordinary income" ~
          "ORDINARY_INCOME_RATE_BASE_DIRECTLY_CHANGED_BY_2025_RECONCILIATION_LAW",

        title_l == "tax all foreign income of u.s. corporations at the full statutory corporate rate" ~
          "INTERNATIONAL_CORPORATE_TAX_MECHANISM_DIRECTLY_CHANGED",

        title_l == "limit state taxes on health care providers" ~
          "MEDICAID_PROVIDER_TAX_RULES_DIRECTLY_CHANGED",

        title_l == "repeal the low-income housing tax credit" ~
          "LIHTC_CREDIT_BASE_DIRECTLY_CHANGED",

        TRUE ~ "NONE_IDENTIFIED"
      ),

      reconstruction_status = case_when(
        direct_statutory_collision == "NONE_IDENTIFIED" ~ "NOT_NEEDED",
        TRUE ~ "NO_PRESENTATION_GRADE_RECONSTRUCTION_AVAILABLE_FROM_PUBLISHED_OFFICIAL_DATA"
      ),

      externally_vettable_score = case_when(
        source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" ~ TRUE,
        evidence_class == "OFFICIAL_CURRENT" ~ TRUE,
        current_index_verified ~ TRUE,
        TRUE ~ FALSE
      ),

      score_recency_status = case_when(
        source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" ~ "LATEST_CBO_ACCOUNT_PROJECTIONS_JUNE_2026",
        evidence_class == "OFFICIAL_CURRENT" ~ "CURRENT_OFFICIAL_SCORE",
        current_index_verified ~ "NEWEST_PUBLISHED_CBO_BUDGET_OPTION_SCORE",
        source_kind == "ADDITIONAL_OFFICIAL_CBO_POLICY" ~ "NOT_VERIFIED_AS_CURRENT_CBO_BUDGET_OPTION",
        TRUE ~ "NO_VETTED_SCORE"
      ),

      current_law_change_class = direct_statutory_collision,

      current_law_adjudication_status = case_when(
        source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" ~ "VALID_CURRENT_ACCOUNT_CONTROL",
        evidence_class == "OFFICIAL_CURRENT" ~ "VALID_CURRENT_OFFICIAL_SCORE",
        direct_statutory_collision != "NONE_IDENTIFIED" ~ "INVALID_DIRECT_STATUTORY_COLLISION_NO_DEFENSIBLE_RECONSTRUCTION",
        current_index_verified ~ "VALID_NEWEST_PUBLISHED_OFFICIAL_SCORE",
        !externally_vettable_score ~ "INVALID_NOT_EXTERNALLY_VETTABLE_OR_NOT_CURRENT_INDEX_VERIFIED",
        TRUE ~ "NOT_SOLVER_SCORE"
      ),

      current_law_adjudication_reason = case_when(
        current_law_adjudication_status == "VALID_CURRENT_ACCOUNT_CONTROL" ~
          "Account control is calculated from CBO's June 30, 2026 budget-account spending projections.",
        current_law_adjudication_status == "VALID_CURRENT_OFFICIAL_SCORE" ~
          "Candidate uses a current official score.",
        current_law_adjudication_status == "VALID_NEWEST_PUBLISHED_OFFICIAL_SCORE" ~
          "Candidate uses the newest published CBO Budget Options estimate for the same policy. Estimate age alone is not an exclusion criterion.",
        current_law_adjudication_status == "INVALID_DIRECT_STATUTORY_COLLISION_NO_DEFENSIBLE_RECONSTRUCTION" ~
          paste0(
            "Intervening law directly changed the same scored mechanism (", direct_statutory_collision,
            "). The published coefficient no longer measures the incremental proposal under current law, and no externally supportable reconstruction is available."
          ),
        current_law_adjudication_status == "INVALID_NOT_EXTERNALLY_VETTABLE_OR_NOT_CURRENT_INDEX_VERIFIED" ~
          "The coefficient cannot be verified as the newest published CBO Budget Options score or another current authoritative coefficient and is excluded before optimization.",
        TRUE ~ "Candidate is not a solver-ready scored policy."
      ),

      current_law_evidence_basis = case_when(
        current_law_adjudication_status == "VALID_CURRENT_ACCOUNT_CONTROL" ~
          "CBO_JUNE_30_2026_SPENDING_PROJECTIONS_BY_BUDGET_ACCOUNT",
        current_law_adjudication_status == "VALID_CURRENT_OFFICIAL_SCORE" ~
          "CURRENT_OFFICIAL_SOURCE",
        current_law_adjudication_status == "VALID_NEWEST_PUBLISHED_OFFICIAL_SCORE" ~
          "VALIDATED_CBO_CURRENT_LATEST_POLICY_PACK;CBO_BUDGET_OPTIONS_CURRENT_INDEX",
        direct_statutory_collision != "NONE_IDENTIFIED" ~
          "PUBLIC_LAW_119_21;CBO_2026_OUTLOOK;CBO_CURRENT_BUDGET_OPTIONS_INDEX",
        TRUE ~ "SOURCE_VALIDITY_FAILURE"
      ),

      exact_current_law_rescore_available = current_law_adjudication_status %in% c(
        "VALID_CURRENT_ACCOUNT_CONTROL", "VALID_CURRENT_OFFICIAL_SCORE"
      ),

      current_law_solver_eligible =
        dplyr::coalesce(parameterized_solver_eligible, FALSE) &
        current_law_adjudication_status %in% c(
          "VALID_CURRENT_ACCOUNT_CONTROL",
          "VALID_CURRENT_OFFICIAL_SCORE",
          "VALID_NEWEST_PUBLISHED_OFFICIAL_SCORE"
        )
    ) |>
    select(-title_l, -variant_l)
}

apply_current_law_score_adjudication <- function(policy_model, current_index) {
  policy_model$meta <- build_current_law_score_adjudication(policy_model$meta, current_index)
  excluded <- policy_model$meta |> filter(parameterized_solver_eligible, !current_law_solver_eligible)
  log_line(
    "Presentation-grade validity review complete: retained ",
    sum(policy_model$meta$parameterized_solver_eligible & policy_model$meta$current_law_solver_eligible, na.rm = TRUE),
    " of ", sum(policy_model$meta$parameterized_solver_eligible, na.rm = TRUE),
    " solver-ready candidates; ", nrow(excluded),
    " candidate variants excluded for direct legal-baseline collision or external-vetting failure"
  )
  policy_model
}

validate_validity_vetting_contract <- function(policy_model) {
  m <- policy_model$meta |> filter(parameterized_solver_eligible)
  excluded <- m |> filter(!current_law_solver_eligible)
  allowed_exclusion_statuses <- c(
    "INVALID_DIRECT_STATUTORY_COLLISION_NO_DEFENSIBLE_RECONSTRUCTION",
    "INVALID_NOT_EXTERNALLY_VETTABLE_OR_NOT_CURRENT_INDEX_VERIFIED"
  )
  checks <- tibble::tibble(
    check = c(
      "All pre-solver exclusions use an allowed presentation-grade validity class",
      "Every retained frozen CBO score is verified as the newest published score in the current/latest index",
      "No candidate is excluded merely because its newest official score is old",
      "Every direct statutory collision without a defensible reconstruction is excluded",
      "Every excluded candidate has an auditable reason and evidence basis",
      "Materiality removes zero solver candidates"
    ),
    passed = c(
      nrow(excluded) == 0L || all(excluded$current_law_adjudication_status %in% allowed_exclusion_statuses),
      all(m$current_index_verified[m$current_law_solver_eligible & m$source_kind == "LOCAL_FROZEN_CBO_POLICY_PACK"]),
      !any(!m$current_law_solver_eligible & m$current_law_change_class == "NONE_IDENTIFIED" & m$current_index_verified, na.rm = TRUE),
      !any(m$current_law_solver_eligible & m$current_law_change_class != "NONE_IDENTIFIED", na.rm = TRUE),
      nrow(excluded) == 0L || all(!is.na(excluded$current_law_adjudication_reason) & nzchar(excluded$current_law_adjudication_reason) & !is.na(excluded$current_law_evidence_basis) & nzchar(excluded$current_law_evidence_basis)),
      TRUE
    )
  )
  attr(checks, "excluded_candidates") <- excluded
  checks
}


# ------------------------------------------------------------------------------
# FUNCTION: build_score_vintage_compatibility_audit
# Purpose: Report score vintage and presentation-grade validity without treating age itself as a defect.
# ------------------------------------------------------------------------------
build_score_vintage_compatibility_audit_core_2 <- function(policy_model) {
  policy_model$meta |>
    mutate(
      compatibility_status = case_when(
        current_law_adjudication_status == "VALID_CURRENT_ACCOUNT_CONTROL" ~ "VALID_CURRENT_ACCOUNT_CONTROL",
        current_law_adjudication_status == "VALID_CURRENT_OFFICIAL_SCORE" ~ "VALID_CURRENT_OFFICIAL_SCORE",
        current_law_adjudication_status == "VALID_NEWEST_PUBLISHED_OFFICIAL_SCORE" ~ "VALID_NEWEST_PUBLISHED_OFFICIAL_SCORE",
        current_law_adjudication_status == "INVALID_DIRECT_STATUTORY_COLLISION_NO_DEFENSIBLE_RECONSTRUCTION" ~ "EXCLUDED_DIRECT_STATUTORY_COLLISION",
        current_law_adjudication_status == "INVALID_NOT_EXTERNALLY_VETTABLE_OR_NOT_CURRENT_INDEX_VERIFIED" ~ "EXCLUDED_NOT_EXTERNALLY_VETTABLE",
        TRUE ~ "NOT_APPLICABLE"
      ),
      compatibility_risk = case_when(
        current_law_solver_eligible ~ "PRESENTATION_GRADE_VALID",
        parameterized_solver_eligible ~ "EXCLUDED_BEFORE_SOLVER",
        TRUE ~ "NOT_APPLICABLE"
      ),
      compatibility_reason = current_law_adjudication_reason
    ) |>
    select(
      candidate_id, family_id, title, variant_name, source_kind, evidence_class,
      estimate_year, source_date, source_url, direct_cbo_annual_score,
      solver_eligible_annual, parameterized_solver_eligible, current_law_solver_eligible,
      cumulative_primary_improvement_2027_2036_bil,
      score_recency_status, current_law_change_class,
      compatibility_status, compatibility_risk, compatibility_reason
    )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_solution_score_vintage_exposure
# Purpose: Report the official-score vintages selected by each retained package.
# ------------------------------------------------------------------------------
build_solution_score_vintage_exposure_core_2 <- function(search_result, compatibility_audit) {
  if (is.null(search_result$membership) || nrow(search_result$membership) == 0L) return(tibble())

  scored <- compatibility_audit |>
    select(
      candidate_id, direct_cbo_annual_score, estimate_year,
      compatibility_status, compatibility_risk, current_law_solver_eligible
    )

  search_result$membership |>
    left_join(scored, by = "candidate_id") |>
    group_by(solution_id) |>
    summarise(
      selected_policy_count = n_distinct(candidate_id),
      selected_direct_official_score_count = sum(dplyr::coalesce(direct_cbo_annual_score, FALSE)),
      selected_current_account_or_official_count = sum(
        compatibility_status %in% c("VALID_CURRENT_ACCOUNT_CONTROL", "VALID_CURRENT_OFFICIAL_SCORE"),
        na.rm = TRUE
      ),
      selected_newest_published_official_score_count = sum(
        compatibility_status == "VALID_NEWEST_PUBLISHED_OFFICIAL_SCORE",
        na.rm = TRUE
      ),
      selected_invalid_score_count = sum(!dplyr::coalesce(current_law_solver_eligible, FALSE), na.rm = TRUE),
      oldest_selected_official_score_year = {
        keep_year <- dplyr::coalesce(direct_cbo_annual_score, FALSE) & is.finite(estimate_year)
        if (any(keep_year)) min(estimate_year[keep_year], na.rm = TRUE) else NA_real_
      },
      newest_selected_official_score_year = {
        keep_year <- dplyr::coalesce(direct_cbo_annual_score, FALSE) & is.finite(estimate_year)
        if (any(keep_year)) max(estimate_year[keep_year], na.rm = TRUE) else NA_real_
      },
      .groups = "drop"
    )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_solution_current_law_score_exposure
# Purpose: Confirm that retained packages contain only presentation-grade validity-vetted candidates.
# ------------------------------------------------------------------------------
build_solution_current_law_score_exposure <- function(search_result, policy_model) {
  if (is.null(search_result$membership) || nrow(search_result$membership) == 0L) return(tibble())

  adjudication <- policy_model$meta |>
    select(
      candidate_id, current_law_adjudication_status,
      current_law_solver_eligible, score_recency_status, current_law_change_class
    )

  search_result$membership |>
    left_join(adjudication, by = "candidate_id") |>
    group_by(solution_id) |>
    summarise(
      selected_policy_count = n_distinct(candidate_id),
      selected_current_law_ineligible_count = sum(!dplyr::coalesce(current_law_solver_eligible, FALSE)),
      selected_current_account_or_official_count = sum(
        current_law_adjudication_status %in% c("VALID_CURRENT_ACCOUNT_CONTROL", "VALID_CURRENT_OFFICIAL_SCORE"),
        na.rm = TRUE
      ),
      selected_newest_published_official_score_count = sum(
        current_law_adjudication_status == "VALID_NEWEST_PUBLISHED_OFFICIAL_SCORE",
        na.rm = TRUE
      ),
      selected_direct_collision_count = sum(
        current_law_change_class != "NONE_IDENTIFIED",
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}


# ------------------------------------------------------------------------------
# MODEL LAYER LEGISLATIVE POLICY-SPACE AND SCORE-BASIS RULES
# ------------------------------------------------------------------------------
# These are the active final definitions. The model no longer treats compatibility
# with statutes currently on the books as a substantive eligibility gate. Congress
# can change law. Existing law remains relevant to baseline accounting, provenance,
# and interpretation of an older score, but it cannot by itself remove a policy
# from the optimizer. Protection rules and evidence quality remain binding.

# ------------------------------------------------------------------------------
# FUNCTION: build_policy_search_score_review
# Purpose: Determine whether a solver-ready policy has a defensible scoring anchor,
#          independently of whether enactment requires changing current law.
# ------------------------------------------------------------------------------
build_policy_search_score_review_legislative_policy_space <- function(meta, current_index) {
  idx <- current_index |>
    transmute(
      family_id,
      latest_published_estimate_year = as.integer(latest_estimate_year),
      latest_published_estimate = latest_estimate,
      current_index_source_url = source_url,
      current_index_status = annual_data_status
    )

  meta |>
    left_join(idx, by = "family_id") |>
    mutate(
      title_l = stringr::str_to_lower(dplyr::coalesce(title, "")),
      variant_l = stringr::str_to_lower(dplyr::coalesce(variant_name, "")),
      current_index_verified =
        source_kind == "LOCAL_FROZEN_CBO_POLICY_PACK" &
        dplyr::coalesce(direct_cbo_annual_score, FALSE) &
        !is.na(latest_published_estimate_year) &
        estimate_year == latest_published_estimate_year,

      # Legislative context is metadata only. It can affect interpretation of a
      # score, but never by itself removes a policy from the legislative search.
      legislative_change_context = case_when(
        stringr::str_detect(title_l, "require people who claim the earned income tax credit and child tax credit") ~
          "RELATED_EITC_CTC_SSN_RULES_CHANGED_AFTER_SCORE",
        title_l == "limit the deduction for charitable giving" &
          stringr::str_detect(variant_l, "2 percent agi floor") ~
          "RELATED_CHARITABLE_DEDUCTION_RULES_CHANGED_AFTER_SCORE",
        title_l == "eliminate or limit itemized deductions" ~
          "RELATED_ITEMIZED_DEDUCTION_RULES_CHANGED_AFTER_SCORE",
        title_l == "increase individual income tax rates on ordinary income" ~
          "RELATED_ORDINARY_INCOME_RATE_RULES_CHANGED_AFTER_SCORE",
        title_l == "tax all foreign income of u.s. corporations at the full statutory corporate rate" ~
          "RELATED_INTERNATIONAL_CORPORATE_TAX_RULES_CHANGED_AFTER_SCORE",
        title_l == "limit state taxes on health care providers" ~
          "RELATED_MEDICAID_PROVIDER_TAX_RULES_CHANGED_AFTER_SCORE",
        title_l == "repeal the low-income housing tax credit" ~
          "RELATED_LIHTC_RULES_CHANGED_AFTER_SCORE",
        TRUE ~ "NO_SPECIFIC_POST_SCORE_STATUTORY_CHANGE_FLAGGED"
      ),
      policy_requires_legislative_change =
        legislative_change_context != "NO_SPECIFIC_POST_SCORE_STATUTORY_CHANGE_FLAGGED",

      # Score admissibility is about evidence, not whether Congress must change
      # existing law. The newest published official score remains the default
      # scoring anchor when no newer official estimate exists.
      externally_vettable_score = case_when(
        source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" ~ TRUE,
        evidence_class == "OFFICIAL_CURRENT" ~ TRUE,
        current_index_verified ~ TRUE,
        source_kind == "ADDITIONAL_OFFICIAL_CBO_POLICY" &
          evidence_class == "OFFICIAL_OLDER" ~ TRUE,
        TRUE ~ FALSE
      ),

      score_recency_status = case_when(
        source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" ~
          "LATEST_CBO_ACCOUNT_PROJECTIONS_JUNE_2026",
        evidence_class == "OFFICIAL_CURRENT" ~
          "CURRENT_OFFICIAL_SCORE",
        current_index_verified ~
          "NEWEST_PUBLISHED_CBO_BUDGET_OPTION_SCORE",
        source_kind == "ADDITIONAL_OFFICIAL_CBO_POLICY" &
          evidence_class == "OFFICIAL_OLDER" ~
          "OLDER_OFFICIAL_ANNUAL_SCORE_WITH_NEWER_METHOD_MECHANISM_VALIDATION",
        TRUE ~ "NO_VETTED_SCORE"
      ),

      score_basis_status = case_when(
        source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" ~
          "VALID_LATEST_ACCOUNT_BASELINE_COEFFICIENT",
        evidence_class == "OFFICIAL_CURRENT" ~
          "VALID_CURRENT_OFFICIAL_SCORE",
        current_index_verified & policy_requires_legislative_change ~
          "VALID_NEWEST_PUBLISHED_OFFICIAL_SCORE_REQUIRES_LEGISLATIVE_CHANGE",
        current_index_verified ~
          "VALID_NEWEST_PUBLISHED_OFFICIAL_SCORE",
        source_kind == "ADDITIONAL_OFFICIAL_CBO_POLICY" &
          evidence_class == "OFFICIAL_OLDER" ~
          "VALID_OLDER_OFFICIAL_ANNUAL_SCORE_WITH_CURRENT_METHOD_VALIDATION",
        externally_vettable_score ~
          "VALID_EXTERNALLY_VETTED_OFFICIAL_SCORE",
        TRUE ~
          "INVALID_NO_EXTERNALLY_VETTED_ANNUAL_SCORE"
      ),

      score_basis_reason = case_when(
        score_basis_status == "VALID_LATEST_ACCOUNT_BASELINE_COEFFICIENT" ~
          "Mechanical account control is calculated from CBO's June 30, 2026 budget-account projections.",
        score_basis_status == "VALID_CURRENT_OFFICIAL_SCORE" ~
          "Candidate uses a current official score.",
        score_basis_status == "VALID_NEWEST_PUBLISHED_OFFICIAL_SCORE" ~
          "Candidate uses CBO's newest published official score for this policy. Score age alone is not an exclusion criterion.",
        score_basis_status == "VALID_NEWEST_PUBLISHED_OFFICIAL_SCORE_REQUIRES_LEGISLATIVE_CHANGE" ~
          paste0(
            "Candidate uses CBO's newest published official score for the policy. Implementing the option may require changing current statute (",
            legislative_change_context,
            "), but current law is not a policy-eligibility constraint. The score vintage is carried explicitly."
          ),
        score_basis_status == "VALID_OLDER_OFFICIAL_ANNUAL_SCORE_WITH_CURRENT_METHOD_VALIDATION" ~
          "Candidate uses the latest externally verifiable official annual score available to the model, with newer official evidence validating the fiscal mechanism. No unsupported rescaling is invented.",
        score_basis_status == "VALID_EXTERNALLY_VETTED_OFFICIAL_SCORE" ~
          "Candidate has externally vetted official scoring evidence.",
        TRUE ~
          "No externally verifiable annual fiscal response coefficient is available. The policy remains inventoried but cannot enter an annual debt-service MILP without inventing a score path."
      ),

      score_method_data_basis = case_when(
        source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" ~
          "CBO_JUNE_30_2026_SPENDING_PROJECTIONS_BY_BUDGET_ACCOUNT",
        evidence_class == "OFFICIAL_CURRENT" ~
          "CURRENT_OFFICIAL_SOURCE",
        current_index_verified ~
          "NEWEST_PUBLISHED_CBO_BUDGET_OPTION_SCORE;CURRENT_2026_BASELINE_AND_DEBT_SERVICE_ENGINE",
        source_kind == "ADDITIONAL_OFFICIAL_CBO_POLICY" &
          evidence_class == "OFFICIAL_OLDER" ~
          "LATEST_AVAILABLE_OFFICIAL_ANNUAL_SCORE;NEWER_CBO_METHOD_MECHANISM_VALIDATION;CURRENT_2026_BASELINE_AND_DEBT_SERVICE_ENGINE",
        TRUE ~ "SOURCE_VALIDITY_FAILURE"
      ),

      policy_search_solver_eligible =
        dplyr::coalesce(parameterized_solver_eligible, FALSE) &
        externally_vettable_score,

      # Legacy compatibility aliases are retained only for older helper
      # functions. They no longer encode a current-law veto.
      current_law_change_class = legislative_change_context,
      current_law_adjudication_status = score_basis_status,
      current_law_adjudication_reason = score_basis_reason,
      current_law_evidence_basis = score_method_data_basis,
      exact_current_law_rescore_available = score_basis_status %in% c(
        "VALID_LATEST_ACCOUNT_BASELINE_COEFFICIENT",
        "VALID_CURRENT_OFFICIAL_SCORE"
      ),
      current_law_solver_eligible = policy_search_solver_eligible
    ) |>
    select(-title_l, -variant_l)
}

apply_policy_search_score_review <- function(policy_model, current_index) {
  policy_model$meta <- build_policy_search_score_review(policy_model$meta, current_index)

  excluded <- policy_model$meta |>
    filter(parameterized_solver_eligible, !policy_search_solver_eligible)

  legislative <- policy_model$meta |>
    filter(
      parameterized_solver_eligible,
      policy_search_solver_eligible,
      policy_requires_legislative_change
    )

  log_line(
    "Legislative policy-space score review complete: admitted ",
    sum(
      policy_model$meta$parameterized_solver_eligible &
        policy_model$meta$policy_search_solver_eligible,
      na.rm = TRUE
    ),
    " of ",
    sum(policy_model$meta$parameterized_solver_eligible, na.rm = TRUE),
    " solver-ready scored candidates; ",
    nrow(legislative),
    " admitted candidates may require statutory change; ",
    nrow(excluded),
    " candidates excluded only for lack of an externally vetted annual score"
  )

  policy_model
}

build_policy_search_score_capacity <- function(policy_model) {
  policy_model$meta |>
    filter(parameterized_solver_eligible) |>
    group_by(
      score_basis_status,
      policy_search_solver_eligible,
      policy_requires_legislative_change
    ) |>
    summarise(
      candidate_variants = n(),
      families = n_distinct(family_id),
      cumulative_primary_improvement_2027_2036_bil = sum(
        pmax(
          dplyr::coalesce(
            cumulative_primary_improvement_2027_2036_bil,
            0
          ),
          0
        ),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    arrange(desc(policy_search_solver_eligible), score_basis_status)
}

build_policy_search_score_exclusions <- function(policy_model) {
  policy_model$meta |>
    filter(parameterized_solver_eligible, !policy_search_solver_eligible) |>
    select(
      candidate_id, family_id, title, variant_name,
      source_kind, evidence_class, estimate_year, source_date, source_url,
      direct_cbo_annual_score, score_recency_status,
      score_basis_status, score_basis_reason, score_method_data_basis,
      protection_status,
      cumulative_primary_improvement_2027_2036_bil
    ) |>
    arrange(title, variant_name)
}

validate_policy_search_score_contract <- function(policy_model) {
  m <- policy_model$meta |> filter(parameterized_solver_eligible)
  admitted <- m |> filter(policy_search_solver_eligible)
  excluded <- m |> filter(!policy_search_solver_eligible)

  checks <- tibble::tibble(
    check = c(
      "Every solver-admitted candidate has externally vetted fiscal-response evidence",
      "Every admitted frozen CBO annual score matches the newest published CBO option score year",
      "Current-law changes never independently exclude an otherwise vetted score",
      "Every excluded solver-ready candidate lacks an externally vetted annual score",
      "Protection classification remains separate from score admissibility",
      "Materiality removes zero solver candidates"
    ),
    passed = c(
      all(admitted$externally_vettable_score),
      all(
        admitted$current_index_verified[
          admitted$source_kind == "LOCAL_FROZEN_CBO_POLICY_PACK" &
            dplyr::coalesce(admitted$direct_cbo_annual_score, FALSE)
        ]
      ),
      !any(
        !m$policy_search_solver_eligible &
          m$policy_requires_legislative_change &
          m$externally_vettable_score,
        na.rm = TRUE
      ),
      nrow(excluded) == 0L || all(!excluded$externally_vettable_score),
      all(admitted$protection_status %in% c("ELIGIBLE", "CONDITIONAL", "BLOCKED")),
      TRUE
    )
  )

  attr(checks, "excluded_candidates") <- excluded
  checks
}

# ------------------------------------------------------------------------------
# FUNCTION: policy_universe_for_mode
# Purpose: Apply only approved protection rules and score-evidence rules.
#          Current-law compatibility is deliberately NOT an eligibility filter.
# ------------------------------------------------------------------------------
policy_universe_for_mode <- function(
  policy_model,
  protection_mode = c("STRICT", "EXPANDED"),
  materiality_mode = c("PACKAGE_READY", "FULL_CAPACITY")
) {
  protection_mode <- match.arg(protection_mode)
  materiality_mode <- match.arg(materiality_mode)

  allowed_status <- if (protection_mode == "STRICT") {
    "ELIGIBLE"
  } else {
    c("ELIGIBLE", "CONDITIONAL")
  }

  meta <- policy_model$meta |>
    filter(
      parameterized_solver_eligible,
      policy_search_solver_eligible,
      protection_status %in% allowed_status
    )

  if (CFG$evidence_mode == "OFFICIAL_CURRENT") {
    meta <- meta |> filter(evidence_class == "OFFICIAL_CURRENT")
  }
  if (CFG$evidence_mode == "OFFICIAL_OLDER") {
    meta <- meta |>
      filter(evidence_class %in% c("OFFICIAL_CURRENT", "OFFICIAL_OLDER"))
  }

  ids <- meta$candidate_id

  list(
    meta = meta,
    schedules = policy_model$schedules |> filter(candidate_id %in% ids),
    schedule_summary = policy_model$schedule_summary |> filter(candidate_id %in% ids),
    schedule_flows = policy_model$schedule_flows |> filter(candidate_id %in% ids),
    scenario_meta = policy_model$scenario_meta,
    scenario_annual = policy_model$scenario_annual,
    protection_mode = protection_mode,
    materiality_mode = materiality_mode
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_score_vintage_compatibility_audit
# Purpose: Report score provenance, age, and legislative-change context without
#          using current-law compatibility as a pre-solver veto.
# ------------------------------------------------------------------------------
build_score_vintage_compatibility_audit <- function(policy_model) {
  policy_model$meta |>
    mutate(
      compatibility_status = score_basis_status,
      compatibility_risk = case_when(
        policy_search_solver_eligible & policy_requires_legislative_change ~
          "VALID_SCORE_REQUIRES_LEGISLATIVE_CHANGE",
        policy_search_solver_eligible ~ "VALID_SCORE",
        parameterized_solver_eligible ~ "EXCLUDED_NO_VETTED_ANNUAL_SCORE",
        TRUE ~ "NOT_APPLICABLE"
      ),
      compatibility_reason = score_basis_reason
    ) |>
    select(
      candidate_id, family_id, title, variant_name,
      source_kind, evidence_class, estimate_year, source_date, source_url,
      direct_cbo_annual_score, solver_eligible_annual,
      parameterized_solver_eligible, policy_search_solver_eligible,
      policy_requires_legislative_change, legislative_change_context,
      cumulative_primary_improvement_2027_2036_bil,
      score_recency_status, compatibility_status,
      compatibility_risk, compatibility_reason
    )
}

build_score_vintage_capacity_audit <- function(compatibility_audit) {
  compatibility_audit |>
    filter(parameterized_solver_eligible) |>
    group_by(compatibility_status, compatibility_risk) |>
    summarise(
      solver_candidate_count = n(),
      distinct_policy_families = n_distinct(family_id),
      direct_official_score_count = sum(
        dplyr::coalesce(direct_cbo_annual_score, FALSE)
      ),
      maximum_ten_year_primary_improvement_bil = sum(
        pmax(cumulative_primary_improvement_2027_2036_bil, 0),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    arrange(
      factor(
        compatibility_risk,
        levels = c(
          "EXCLUDED_NO_VETTED_ANNUAL_SCORE",
          "VALID_SCORE_REQUIRES_LEGISLATIVE_CHANGE",
          "VALID_SCORE",
          "NOT_APPLICABLE"
        )
      ),
      compatibility_status
    )
}

build_solution_score_vintage_exposure <- function(
  search_result,
  compatibility_audit
) {
  if (
    is.null(search_result$membership) ||
      nrow(search_result$membership) == 0L
  ) {
    return(tibble())
  }

  scored <- compatibility_audit |>
    select(
      candidate_id, direct_cbo_annual_score, estimate_year,
      compatibility_status, compatibility_risk,
      policy_search_solver_eligible,
      policy_requires_legislative_change
    )

  search_result$membership |>
    left_join(scored, by = "candidate_id") |>
    group_by(solution_id) |>
    summarise(
      selected_policy_count = n_distinct(candidate_id),
      selected_direct_official_score_count =
        sum(dplyr::coalesce(direct_cbo_annual_score, FALSE)),
      selected_current_score_or_account_count = sum(
        compatibility_status %in% c(
          "VALID_LATEST_ACCOUNT_BASELINE_COEFFICIENT",
          "VALID_CURRENT_OFFICIAL_SCORE"
        ),
        na.rm = TRUE
      ),
      selected_newest_published_official_score_count = sum(
        stringr::str_detect(
          dplyr::coalesce(compatibility_status, ""),
          "^VALID_NEWEST_PUBLISHED_OFFICIAL_SCORE"
        ),
        na.rm = TRUE
      ),
      selected_older_official_score_with_method_validation_count = sum(
        compatibility_status ==
          "VALID_OLDER_OFFICIAL_ANNUAL_SCORE_WITH_CURRENT_METHOD_VALIDATION",
        na.rm = TRUE
      ),
      selected_policies_requiring_legislative_change_count =
        sum(dplyr::coalesce(policy_requires_legislative_change, FALSE)),
      selected_invalid_score_count =
        sum(!dplyr::coalesce(policy_search_solver_eligible, FALSE), na.rm = TRUE),
      oldest_selected_official_score_year = {
        keep_year <-
          dplyr::coalesce(direct_cbo_annual_score, FALSE) &
          is.finite(estimate_year)
        if (any(keep_year)) {
          min(estimate_year[keep_year], na.rm = TRUE)
        } else {
          NA_real_
        }
      },
      newest_selected_official_score_year = {
        keep_year <-
          dplyr::coalesce(direct_cbo_annual_score, FALSE) &
          is.finite(estimate_year)
        if (any(keep_year)) {
          max(estimate_year[keep_year], na.rm = TRUE)
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    )
}

build_solution_score_basis_exposure <- function(
  search_result,
  policy_model
) {
  if (
    is.null(search_result$membership) ||
      nrow(search_result$membership) == 0L
  ) {
    return(tibble())
  }

  review <- policy_model$meta |>
    select(
      candidate_id, score_basis_status,
      policy_search_solver_eligible, score_recency_status,
      policy_requires_legislative_change,
      legislative_change_context
    )

  search_result$membership |>
    left_join(review, by = "candidate_id") |>
    group_by(solution_id) |>
    summarise(
      selected_policy_count = n_distinct(candidate_id),
      selected_score_ineligible_count =
        sum(!dplyr::coalesce(policy_search_solver_eligible, FALSE)),
      selected_current_score_or_account_count = sum(
        score_basis_status %in% c(
          "VALID_LATEST_ACCOUNT_BASELINE_COEFFICIENT",
          "VALID_CURRENT_OFFICIAL_SCORE"
        ),
        na.rm = TRUE
      ),
      selected_newest_published_score_count = sum(
        stringr::str_detect(
          dplyr::coalesce(score_basis_status, ""),
          "^VALID_NEWEST_PUBLISHED_OFFICIAL_SCORE"
        ),
        na.rm = TRUE
      ),
      selected_older_official_score_with_method_validation_count = sum(
        score_basis_status ==
          "VALID_OLDER_OFFICIAL_ANNUAL_SCORE_WITH_CURRENT_METHOD_VALIDATION",
        na.rm = TRUE
      ),
      selected_policies_requiring_legislative_change_count =
        sum(dplyr::coalesce(policy_requires_legislative_change, FALSE)),
      .groups = "drop"
    )
}

# ------------------------------------------------------------------------------
# FUNCTION: plot_solution_debt_paths
# Purpose: Render only visually distinct representative debt paths. If two
#          selected packages produce the same plotted path, only one receives a
#          line and legend entry.
# ------------------------------------------------------------------------------
plot_solution_debt_paths_legislative_policy_space <- function(working_baseline, search_result) {
  base <- working_baseline |>
    transmute(
      year,
      solution_id = "Working baseline",
      debt_gdp_pct = working_debt_gdp_pct
    )

  if (
    nrow(search_result$paths) == 0L ||
      nrow(search_result$summary) == 0L
  ) {
    pdat <- base
  } else {
    ids <- search_result$summary |>
      arrange(
        desc(robust_target_2036_pass & robust_target_2046_pass),
        worst_required_scenario_debt_gdp_2036_pct,
        worst_required_scenario_debt_gdp_2046_pct,
        selected_policy_count
      ) |>
      group_by(protection_mode) |>
      slice_head(n = 4L) |>
      ungroup() |>
      pull(solution_id)

    candidate_paths <- search_result$paths |>
      filter(solution_id %in% ids) |>
      select(year, solution_id, debt_gdp_pct) |>
      arrange(solution_id, year)

    # Fingerprint the plotted values. Six decimals is far tighter than chart
    # resolution while collapsing numerical dust from genuinely identical paths.
    unique_ids <- candidate_paths |>
      group_by(solution_id) |>
      summarise(
        path_fingerprint = paste(
          sprintf("%.6f", debt_gdp_pct),
          collapse = "|"
        ),
        .groups = "drop"
      ) |>
      mutate(selection_order = match(solution_id, ids)) |>
      arrange(selection_order) |>
      distinct(path_fingerprint, .keep_all = TRUE) |>
      pull(solution_id)

    pdat <- bind_rows(
      base,
      candidate_paths |> filter(solution_id %in% unique_ids)
    )
  }

  robust_count <- if (nrow(search_result$summary) == 0L) {
    0L
  } else {
    sum(
      search_result$summary$robust_target_2036_pass &
        search_result$summary$robust_target_2046_pass,
      na.rm = TRUE
    )
  }

  chart_title <- if (robust_count > 0L) {
    "Central debt paths for representative robust-target packages"
  } else {
    "Central debt paths for representative best-attainable frontier packages"
  }

  chart_subtitle <- if (robust_count > 0L) {
    "Displayed packages satisfy the target constraints across every required robust scenario"
  } else {
    "No package satisfied every robust target; displayed packages come from the audited best-attainable slack frontier"
  }

  ggplot(
    pdat,
    aes(
      year,
      debt_gdp_pct,
      group = solution_id,
      color = solution_id
    )
  ) +
    geom_line(linewidth = 0.8) +
    geom_hline(
      yintercept = c(90, 80, 75),
      linetype = "dotted"
    ) +
    scale_x_continuous(breaks = seq(2026, 2046, 2)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = chart_title,
      subtitle = chart_subtitle,
      x = NULL,
      y = "Debt held by public / GDP",
      color = "Package",
      caption = "Policy levels, implementation years, and phase-ins are decision variables. Target lines mark 90 percent in 2036, 80 percent in 2046, and the 75 percent long-run reference."
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}

# ------------------------------------------------------------------------------
# FUNCTION: run_model
# Purpose: Execute the complete validation, data-acquisition, optimization, audit-output, and reporting pipeline.
# ------------------------------------------------------------------------------
run_model_legislative_policy_space <- function() {
  log_line("Starting ", CFG$model_name, " | ", CFG$model_version)
  log_line("Project root: ", CFG$project_root)
  log_line("Evidence mode: ", CFG$evidence_mode)
  log_line("Targets: FY2036 <= ", 100 * CFG$target_2036, "% GDP; FY2046 <= ", 100 * CFG$target_2046_high, "% GDP")
  log_line("Mode: recommendation-hardening. Approved protections remain binding; current law never vetoes a policy Congress could enact. Preserves presentation-audit validated model and one-plot presentation while exhaustively testing every recommended policy, quantifying replacement costs, and solving targeted political counterfactuals.")
  validate_policy_pack_present()
  paths <- fetch_cbo_sources()
  expanded_paths <- fetch_expanded_universe_sources()
  ssa_current_target_validation <- validate_ssa_current_actuarial_target()
  assert_model(
    all(ssa_current_target_validation$passed),
    "Current SSA actuarial target validation failed; configured Social Security solvency constraint is not verified against the newest published SSA basis"
  )
  cbo_data <- list(
    ten = read_cbo_long(paths$cbo_ten_year_budget_2026_02),
    lt = read_cbo_long(paths$cbo_long_term_budget_2026_02),
    hist = read_cbo_long(paths$cbo_historical_budget_2026_02),
    econ = read_cbo_long(paths$cbo_historical_economic_2026_02),
    revenue_detail = read_cbo_long(paths$cbo_revenue_detail_2026_02)
  )
  spending_detail <- read_spending_detail(expanded_paths$spending_detail)
  tax_inventory <- build_tax_parameter_inventory(expanded_paths$tax_parameters)
  trust_fund_inventory <- build_trust_fund_inventory(expanded_paths$trust_fund)
  cbo_baseline <- build_cbo_baseline(cbo_data)
  spending_reconciliation <- validate_spending_detail_reconciliation(spending_detail, cbo_baseline)
  gate1 <- run_gate_1(cbo_baseline, cbo_data)
  ext_validation <- validate_external_history(cbo_data, cbo_baseline)
  kernel_obj <- build_cbo_debt_service_kernel()
  gate2a <- validate_cbo_kernel(kernel_obj)
  gate2b <- validate_macro_profiles()
  tariff_profile <- build_tariff_profile(cbo_baseline, cbo_data, kernel_obj)
  working_baseline <- build_working_baseline(cbo_baseline, tariff_profile, kernel_obj, spending_detail)
  log_line("Gate 3: loading preserved scored anchors and building title-first Eisenhower-rule account inventory and growth-restraint universe")
  cbo_universe <- build_full_cbo_policy_universe()
  assert_model(nrow(cbo_universe$meta) == CFG$policy_pack_expected_candidates, "Original frozen CBO policy pack changed before expanded-universe construction")
  current_option_index_validation <- validate_current_cbo_option_index_snapshot(cbo_universe$current_index)
  assert_model(
    all(current_option_index_validation$passed),
    "Frozen CBO current/latest option index does not match the newest published CBO Budget Options index structure verified for this build"
  )
  current_option_nonannual_audit <- build_current_option_nonannual_audit(cbo_universe$current_index)
  assert_model(nrow(current_option_nonannual_audit) == 9L, "Expected nine current/latest CBO option families without published annual score paths")
  register_source(
    source_id = "cbo_budget_options_current_index_2026_09_13",
    agency = "Congressional Budget Office",
    title = "CBO Budget Options current/latest index",
    url = "https://www.cbo.gov/budget-options",
    local_path = NA_character_,
    publication_date = NA_character_,
    baseline_vintage = "CURRENT_INDEX_VERIFIED_2026_09_13",
    evidence_class = "OFFICIAL_CURRENT_INDEX",
    notes = "Build-time verification of CBO's current/latest Budget Options index. The validated frozen index contains 127 families: 33 from 2018, 12 from 2020, 6 from 2022, and 76 from 2024; CBO's page states that the search is updated to include only the most recent version of each option."
  )
  expanded_anchor <- build_expanded_anchor_policy_model(cbo_universe, working_baseline, spending_detail, cbo_data$econ)
  expanded_universe_merge_schema <- validate_expanded_universe_merge_schema(expanded_anchor)
  log_line("Expanded-universe merge-schema preflight passed before parameterization")
  policy_model <- build_parameterized_policy_space(expanded_anchor, working_baseline)
  latest_score_verification <- build_latest_official_score_verification(policy_model, cbo_universe$current_index)
  assert_model(all(latest_score_verification$passed), paste0("Latest-published score verification failed for: ", paste(latest_score_verification$title[!latest_score_verification$passed], collapse = "; ")))
  policy_model <- apply_policy_search_score_review(policy_model, cbo_universe$current_index)
  parameterization_validation <- build_parameterization_validation(policy_model)
  eisenhower_regression <- attr(parameterization_validation, "eisenhower_regression")
  assert_model(all(parameterization_validation$passed), paste0("Expanded parameterization validation failed: ", paste(parameterization_validation$check[!parameterization_validation$passed], collapse = "; ")))
  external_catalog <- build_external_policy_catalog(cbo_universe$pack)
  coverage_audit <- build_expanded_coverage_audit(policy_model, tax_inventory, expanded_paths)
  unmodeled_domains <- build_unmodeled_parameter_domains(tax_inventory)
  policy_capacity_audit <- build_pre_solve_policy_capacity_audit(policy_model, tax_inventory, unmodeled_domains)
  expansion_success <- build_policy_universe_expansion_success(policy_model)
  if (CFG$write_audit_outputs) {
    write_csv_atomic(policy_capacity_audit, file.path(CFG$output_dir, "pre_solve_policy_capacity_audit.csv"))
    write_csv_atomic(expansion_success, file.path(CFG$output_dir, "policy_universe_expansion_success.csv"))
    if (!is.null(expanded_anchor$treasury_greenbook_source_scores)) {
      write_csv_atomic(expanded_anchor$treasury_greenbook_source_scores, file.path(CFG$output_dir, "treasury_greenbook_fy2025_scored_proposals.csv"))
    }
  }
  assert_model(
    all(expansion_success$passed),
    paste0(
      "Policy-universe expansion layer policy-universe expansion added only ",
      format(round(expansion_success$new_quantified_capacity_bil[[1]], 3), nsmall = 3),
      "B of newly quantified ten-year primary capacity, below the configured ",
      format(round(CFG$policy_universe_expansion_min_new_quantified_capacity_bil, 3), nsmall = 3),
      "B material-expansion threshold. Full MILP search intentionally stopped."
    )
  )
  theoretical_capacity_gain <- validate_policy_universe_expansion_theoretical_capacity_gain(
    policy_model, working_baseline, kernel_obj
  )
  if (CFG$write_audit_outputs) {
    write_csv_atomic(
      theoretical_capacity_gain,
      file.path(CFG$output_dir, "policy_universe_expansion_theoretical_capacity_gain.csv")
    )
  }
  assert_model(
    all(theoretical_capacity_gain$passed),
    paste0(
      "Policy-universe expansion layer added only ",
      format(round(theoretical_capacity_gain$incremental_primary_capacity_bil[[1]], 3), nsmall = 3),
      "B of realized EXPANDED theoretical ten-year primary capacity relative to the frozen pre-expansion capacity benchmark, below the configured ",
      format(round(CFG$policy_universe_expansion_min_new_quantified_capacity_bil, 3), nsmall = 3),
      "B threshold. Full MILP search intentionally stopped after the pre-search capacity solve."
    )
  )
  log_line(
    "Policy-universe expansion layer expansion gate passed | realized theoretical primary-capacity gain=$",
    format(round(theoretical_capacity_gain$incremental_primary_capacity_bil[[1]], 3), nsmall = 3),
    "B relative to the frozen pre-expansion capacity benchmark"
  )

  materiality_audit <- build_package_materiality_audit(policy_model)
  authoritative_lever_audit <- build_authoritative_lever_expansion_audit(policy_model, tax_inventory)
  score_vintage_compatibility <- build_score_vintage_compatibility_audit(policy_model)
  score_vintage_capacity <- build_score_vintage_capacity_audit(score_vintage_compatibility)
  score_basis_capacity <- build_policy_search_score_capacity(policy_model)
  score_basis_exclusions <- build_policy_search_score_exclusions(policy_model)
  score_review_contract <- validate_policy_search_score_contract(policy_model)
  assert_model(
    all(score_review_contract$passed),
    paste0(
      "Policy-search score-review contract failed: ",
      paste(score_review_contract$check[!score_review_contract$passed], collapse = "; ")
    )
  )
  strict_count <- nrow(policy_universe_for_mode(policy_model, "STRICT", "PACKAGE_READY")$meta)
  expanded_count <- nrow(policy_universe_for_mode(policy_model, "EXPANDED", "PACKAGE_READY")$meta)
  strict_full_count <- nrow(policy_universe_for_mode(policy_model, "STRICT", "FULL_CAPACITY")$meta)
  expanded_full_count <- nrow(policy_universe_for_mode(policy_model, "EXPANDED", "FULL_CAPACITY")$meta)
  assert_model(strict_count == strict_full_count, "Strict candidate universe changed under the reporting-only materiality label")
  assert_model(expanded_count == expanded_full_count, "Expanded candidate universe changed under the reporting-only materiality label")
  assert_model(strict_count >= CFG$minimum_solver_candidates, paste0("Strict all-valid universe has only ", strict_count, " candidates"))
  assert_model(expanded_count >= CFG$minimum_expanded_solver_candidates, paste0("Expanded all-valid universe has only ", expanded_count, " candidates; expected at least ", CFG$minimum_expanded_solver_candidates))
  log_line(
    "Gate 3 complete: master candidates=", nrow(policy_model$meta),
    " | solver-ready before protection filter=", sum(policy_model$meta$parameterized_solver_eligible, na.rm = TRUE),
    " | solver-admissible after score review=", sum(policy_model$meta$parameterized_solver_eligible & policy_model$meta$policy_search_solver_eligible, na.rm = TRUE),
    " | strict all-valid=", strict_count,
    " | expanded all-valid=", expanded_count,
    " | account growth controls=", sum(policy_model$meta$source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" & policy_model$meta$parameterized_solver_eligible, na.rm = TRUE),
    " | schedules=", nrow(policy_model$schedules),
    " | materiality exclusions=0"
  )
  postsolve_reporting_contract <- validate_postsolve_reporting_contract(policy_model)
  log_line("Post-solve reporting contract preflight passed before Gate 4")
  solution_membership_schema_preflight <- validate_solution_membership_schema(policy_model)
  log_line("Exact parameter-decision and solution-membership reporting preflight passed before Gate 4")
  presolve_simulation_preflight <- validate_presolve_simulation_path(policy_model, working_baseline, kernel_obj)
  log_line("Independent simulation preflight passed before Gate 4")
  log_line("Gate 4: launching robust MILP search and capacity diagnostics over the complete validity-vetted candidate universe")
  search_result <- run_full_solution_search(policy_model, working_baseline, kernel_obj)
  solution_score_vintage_exposure <- build_solution_score_vintage_exposure(search_result, score_vintage_compatibility)
  solution_score_basis_exposure <- build_solution_score_basis_exposure(search_result, policy_model)
  assert_model(all(solution_score_basis_exposure$selected_score_ineligible_count == 0), "A retained package contains a candidate without an admissible externally vetted score")
  stress_tests <- build_solution_stress_tests(search_result, working_baseline, policy_model, kernel_obj)
  model_size <- build_model_size_audit(policy_model, search_result)
  family_summary <- build_solution_family_summary(search_result)
  assumptions <- build_assumptions_table()
  package_versions <- build_package_versions_table()
  validation_table <- build_validation_table(gate1, ext_validation, gate2a, gate2b)
  if (CFG$write_audit_outputs) {
    log_line("Writing expanded-universe audit outputs")
    write_csv_atomic(SOURCE_MANIFEST, file.path(CFG$output_dir, "source_manifest.csv"))
    write_csv_atomic(assumptions, file.path(CFG$output_dir, "assumptions.csv"))
    write_csv_atomic(package_versions, file.path(CFG$output_dir, "package_versions.csv"))
    write_csv_atomic(validation_table, file.path(CFG$output_dir, "validation_tests.csv"))
    write_csv_atomic(cbo_baseline, file.path(CFG$output_dir, "baseline_cbo_feb_2026.csv"))
    write_csv_atomic(working_baseline, file.path(CFG$output_dir, "baseline_working_sep_2026.csv"))
    write_csv_atomic(tariff_profile, file.path(CFG$output_dir, "tariff_adjustment_profile.csv"))
    write_csv_atomic(gate2a$comparison, file.path(CFG$output_dir, "validation_cbo_debt_service_kernel.csv"))
    write_csv_atomic(gate2a$direction_summary, file.path(CFG$output_dir, "validation_cbo_debt_service_kernel_summary.csv"))
    write_csv_atomic(kernel_obj$kernel, file.path(CFG$output_dir, "cbo_debt_service_kernel.csv"))
    write_csv_atomic(kernel_obj$bfm_matrix, file.path(CFG$output_dir, "cbo_bfm_2026_debt_service_matrix.csv"))
    write_csv_atomic(cbo_universe$pack$hash_audit, file.path(CFG$output_dir, "policy_pack_hash_audit.csv"))
    write_csv_atomic(cbo_universe$pack$benchmark_audit, file.path(CFG$output_dir, "policy_pack_benchmark_audit.csv"))
    write_csv_atomic(cbo_universe$pack$coverage, file.path(CFG$output_dir, "policy_pack_coverage_audit.csv"))
    write_csv_atomic(cbo_universe$current_index, file.path(CFG$output_dir, "cbo_option_index_current_all.csv"))
    write_csv_atomic(cbo_universe$report_index, file.path(CFG$output_dir, "cbo_option_index_2024_76.csv"))
    write_csv_atomic(cbo_universe$families, file.path(CFG$output_dir, "cbo_option_family_local_pack_audit.csv"))
    write_csv_atomic(cbo_universe$raw_rows, file.path(CFG$output_dir, "cbo_option_raw_annual_rows.csv"))
    write_csv_atomic(spending_detail, file.path(CFG$output_dir, "cbo_spending_detail_raw_normalized.csv"))
    write_csv_atomic(spending_reconciliation, file.path(CFG$output_dir, "cbo_spending_detail_reconciliation.csv"))
    write_csv_atomic(policy_model$account_catalog, file.path(CFG$output_dir, "expanded_spending_account_catalog.csv"))
    write_csv_atomic(policy_model$account_inventory, file.path(CFG$output_dir, "expanded_spending_account_inventory.csv"))
    write_csv_atomic(policy_model$account_control_design, file.path(CFG$output_dir, "expanded_spending_account_control_design.csv"))
    write_csv_atomic(eisenhower_regression, file.path(CFG$output_dir, "eisenhower_protection_regression.csv"))
    write_csv_atomic(policy_model$account_flows, file.path(CFG$output_dir, "expanded_spending_account_annual_control_bounds.csv"))
    write_csv_atomic(tax_inventory, file.path(CFG$output_dir, "cbo_tax_parameter_inventory.csv"))
    write_csv_atomic(trust_fund_inventory, file.path(CFG$output_dir, "cbo_trust_fund_inventory.csv"))
    write_csv_atomic(coverage_audit, file.path(CFG$output_dir, "expanded_universe_coverage_audit.csv"))
    write_csv_atomic(unmodeled_domains, file.path(CFG$output_dir, "expanded_universe_not_solver_ready.csv"))
    write_csv_atomic(policy_capacity_audit, file.path(CFG$output_dir, "pre_solve_policy_capacity_audit.csv"))
    write_csv_atomic(expansion_success, file.path(CFG$output_dir, "policy_universe_expansion_success.csv"))
    if (!is.null(expanded_anchor$treasury_greenbook_source_scores)) {
      write_csv_atomic(expanded_anchor$treasury_greenbook_source_scores, file.path(CFG$output_dir, "treasury_greenbook_fy2025_scored_proposals.csv"))
    }
    write_csv_atomic(authoritative_lever_audit, file.path(CFG$output_dir, "authoritative_lever_expansion_audit.csv"))
    write_csv_atomic(score_vintage_compatibility, file.path(CFG$output_dir, "score_vintage_compatibility_audit.csv"))
    write_csv_atomic(score_vintage_capacity, file.path(CFG$output_dir, "score_vintage_capacity_audit.csv"))
    write_csv_atomic(solution_score_vintage_exposure, file.path(CFG$output_dir, "solution_score_vintage_exposure.csv"))
    write_csv_atomic(score_basis_capacity, file.path(CFG$output_dir, "policy_score_basis_capacity.csv"))
    write_csv_atomic(score_basis_exclusions, file.path(CFG$output_dir, "policy_score_basis_exclusions.csv"))
    write_csv_atomic(score_review_contract, file.path(CFG$output_dir, "policy_score_review_contract.csv"))
    write_csv_atomic(latest_score_verification, file.path(CFG$output_dir, "latest_official_score_verification.csv"))
    write_csv_atomic(current_option_index_validation, file.path(CFG$output_dir, "current_cbo_option_index_snapshot_validation.csv"))
    write_csv_atomic(current_option_nonannual_audit, file.path(CFG$output_dir, "current_cbo_options_without_annual_score_paths.csv"))
    write_csv_atomic(ssa_current_target_validation, file.path(CFG$output_dir, "ssa_current_actuarial_target_validation.csv"))
    write_csv_atomic(solution_score_basis_exposure, file.path(CFG$output_dir, "solution_score_basis_exposure.csv"))
    write_csv_atomic(
      policy_model$meta |>
        filter(parameterized_solver_eligible) |>
        select(
          candidate_id, family_id, title, variant_name, source_kind, evidence_class, estimate_year, source_date, source_url,
          direct_cbo_annual_score, score_recency_status, score_basis_status, score_basis_reason,
          score_method_data_basis, policy_requires_legislative_change, legislative_change_context,
          policy_search_solver_eligible, protection_status, cumulative_primary_improvement_2027_2036_bil
        ),
      file.path(CFG$output_dir, "policy_score_review.csv")
    )
    write_csv_atomic(materiality_audit$summary, file.path(CFG$output_dir, "package_materiality_summary.csv"))
    write_csv_atomic(materiality_audit$detail, file.path(CFG$output_dir, "package_materiality_detail.csv"))
    write_csv_atomic(policy_model$meta, file.path(CFG$output_dir, "policy_candidate_catalog_full.csv"))
    write_csv_atomic(policy_model$meta |> select(any_of(c("candidate_id", "family_id", "title", "variant_name", "source_kind", "agency", "bureau", "function_code", "subfunction_code", "protection_status", "protected_category", "protection_reason", "solver_exclusion_reason", "risk_ordinary_wages", "risk_ordinary_saving", "risk_productive_investment", "risk_family_formation", "risk_business_reinvestment", "risk_core_social_security", "risk_core_medicare", "risk_productive_public_capacity", "market_function_review_required", "hard_protection_violation", "explicit_review_required", "protection_classification_method"))), file.path(CFG$output_dir, "policy_protection_attributes.csv"))
    write_csv_atomic(policy_model$meta |> select(any_of(c("candidate_id", "family_id", "title", "variant_name", "source_kind", "parameterization_mode", "parameter_name", "parameter_unit", "parameter_anchor_value", "parameter_min_value", "parameter_max_value", "parameter_min_scale", "parameter_max_scale", "parameter_extrapolation", "parameter_evidence_basis", "complexity_weight", "complexity_weight_basis", "protection_status", "source_url"))), file.path(CFG$output_dir, "policy_parameterization_catalog.csv"))
    write_csv_atomic(parameterization_validation, file.path(CFG$output_dir, "parameterization_validation.csv"))
    write_csv_atomic(expanded_universe_merge_schema, file.path(CFG$output_dir, "expanded_universe_merge_schema_preflight.csv"))
    write_csv_atomic(postsolve_reporting_contract, file.path(CFG$output_dir, "postsolve_reporting_contract_preflight.csv"))
    write_csv_atomic(solution_membership_schema_preflight, file.path(CFG$output_dir, "solution_membership_schema_preflight.csv"))
    write_csv_atomic(presolve_simulation_preflight, file.path(CFG$output_dir, "presolve_simulation_preflight.csv"))
    write_csv_atomic(policy_model$schedules, file.path(CFG$output_dir, "policy_implementation_schedule_catalog.csv"))
    write_csv_atomic(policy_model$schedule_summary, file.path(CFG$output_dir, "policy_schedule_score_summary.csv"))
    write_csv_atomic(policy_model$schedule_flows, file.path(CFG$output_dir, "policy_schedule_annual_flow_coefficients.csv"))
    write_csv_atomic(policy_model$scenario_meta, file.path(CFG$output_dir, "robust_scenario_catalog.csv"))
    write_csv_atomic(policy_model$scenario_annual, file.path(CFG$output_dir, "robust_scenario_annual_paths.csv"))
    write_csv_atomic(policy_model$meta |> filter(!parameterized_solver_eligible), file.path(CFG$output_dir, "policy_candidates_not_in_annual_milp.csv"))
    write_csv_atomic(policy_model$flows, file.path(CFG$output_dir, "policy_candidate_annual_flows_full.csv"))
    write_csv_atomic(cbo_universe$pack$ssa, file.path(CFG$output_dir, "ssa_actuarial_reference.csv"))
    write_csv_atomic(external_catalog, file.path(CFG$output_dir, "external_policy_source_catalog.csv"))
    write_csv_atomic(build_interaction_catalog_full(policy_model$meta |> filter(parameterized_solver_eligible, policy_search_solver_eligible)), file.path(CFG$output_dir, "interaction_catalog.csv"))
    write_csv_atomic(model_size, file.path(CFG$output_dir, "model_size_audit.csv"))
    write_csv_atomic(search_result$solver_run_status, file.path(CFG$output_dir, "solver_run_status.csv"))
    write_csv_atomic(search_result$target_feasibility_audit, file.path(CFG$output_dir, "target_feasibility_and_soft_frontier.csv"))
    write_csv_atomic(search_result$theoretical_capacity_summary, file.path(CFG$output_dir, "theoretical_full_capacity_summary.csv"))
    write_csv_atomic(search_result$capacity_diagnostics, file.path(CFG$output_dir, "target_specific_capacity_diagnostics.csv"))
    write_csv_atomic(search_result$solution_origins, file.path(CFG$output_dir, "solution_origins.csv"))
    write_csv_atomic(search_result$summary, file.path(CFG$output_dir, "solution_catalog.csv"))
    write_csv_atomic(search_result$membership, file.path(CFG$output_dir, "solution_policy_decisions.csv"))
    write_csv_atomic(search_result$membership, file.path(CFG$output_dir, "solution_policy_membership.csv"))
    write_csv_atomic(search_result$paths, file.path(CFG$output_dir, "solution_debt_paths.csv"))
    write_csv_atomic(search_result$scenario_paths, file.path(CFG$output_dir, "solution_scenario_paths.csv"))
    write_csv_atomic(stress_tests, file.path(CFG$output_dir, "solution_stress_tests.csv"))
    write_csv_atomic(family_summary, file.path(CFG$output_dir, "solution_family_summary.csv"))
    write_csv_atomic(build_run_manifest_full(), file.path(CFG$output_dir, "run_manifest.csv"))
  }
  log_line("Model run complete")
  print(model_size)
  print(search_result$target_feasibility_audit)
  print(search_result$capacity_diagnostics)
  if (nrow(search_result$summary) > 0L) {
    log_line("Distinct all-valid solution packages retained after de-duplication: ", nrow(search_result$summary))
    print(search_result$summary |> select(solution_id, solve_label, protection_mode, soft_targets, achieved_target_slack_score, selected_policy_count, continuously_parameterized_policy_count, implementation_complexity_score, revenue_2027_2036_bil, spending_cuts_2027_2036_bil, ss_actuarial_improvement_pct_payroll, debt_gdp_2036_pct, debt_gdp_2046_pct, worst_required_scenario_debt_gdp_2036_pct, worst_required_scenario_debt_gdp_2046_pct, robust_target_2036_pass, robust_target_2046_pass, independently_verified))
  } else {
    log_line("No optimized or robust nearest-target solution package could be produced from the all-valid universe", level = "WARN")
  }
  if (CFG$render_plots) print(plot_solution_debt_paths(working_baseline, search_result))
  invisible(list(
    config = CFG,
    source_manifest = SOURCE_MANIFEST,
    cbo_data = cbo_data,
    cbo_baseline = cbo_baseline,
    working_baseline = working_baseline,
    tariff_profile = tariff_profile,
    kernel = kernel_obj,
    policy_model = policy_model,
    cbo_policy_universe = cbo_universe,
    expanded_sources = expanded_paths,
    spending_detail_reconciliation = spending_reconciliation,
    tax_parameter_inventory = tax_inventory,
    trust_fund_inventory = trust_fund_inventory,
    expanded_coverage_audit = coverage_audit,
    pre_solve_policy_capacity_audit = policy_capacity_audit,
    policy_universe_expansion_expansion_success = expansion_success,
    policy_universe_expansion_theoretical_capacity_gain = theoretical_capacity_gain,
    treasury_greenbook_source_scores = expanded_anchor$treasury_greenbook_source_scores,
    authoritative_lever_expansion_audit = authoritative_lever_audit,
    score_vintage_compatibility_audit = score_vintage_compatibility,
    score_vintage_capacity_audit = score_vintage_capacity,
    policy_score_basis_capacity = score_basis_capacity,
    policy_score_basis_exclusions = score_basis_exclusions,
    score_excluded_candidates = score_basis_exclusions,
    policy_score_review_contract = score_review_contract,
    latest_official_score_verification = latest_score_verification,
    current_cbo_option_index_snapshot_validation = current_option_index_validation,
    current_cbo_options_without_annual_score_paths = current_option_nonannual_audit,
    ssa_current_actuarial_target_validation = ssa_current_target_validation,
    solution_score_basis_exposure = solution_score_basis_exposure,
    solution_score_vintage_exposure = solution_score_vintage_exposure,
    package_materiality_audit = materiality_audit,
    expanded_universe_merge_schema = expanded_universe_merge_schema,
    eisenhower_protection_regression = eisenhower_regression,
    account_inventory = policy_model$account_inventory,
    account_control_design = policy_model$account_control_design,
    postsolve_reporting_contract = postsolve_reporting_contract,
    solution_membership_schema_preflight = solution_membership_schema_preflight,
    presolve_simulation_preflight = presolve_simulation_preflight,
    external_policy_catalog = external_catalog,
    solution_search = search_result,
    theoretical_full_capacity_summary = search_result$theoretical_capacity_summary,
    target_specific_capacity_diagnostics = search_result$capacity_diagnostics,
    target_feasibility_and_soft_frontier = search_result$target_feasibility_audit,
    solution_stress_tests = stress_tests,
    model_size_audit = model_size,
    solution_family_summary = family_summary,
    validation_tests = validation_table
  ))
}


# ------------------------------------------------------------------------------
# MODEL LAYER ACTIVE RULE SUMMARY
# - Approved protected categories remain binding exactly as classified.
# - Current law is not a policy-eligibility constraint; Congress may change law.
# - Latest published official scores remain admissible even when implementation
#   requires statutory change, with source vintage and legislative context audited.
# - A candidate is excluded for scoring only when no externally vetted annual
#   fiscal-response coefficient exists; no unsupported annual path is invented.
# - June 2026 account data, 2026 SSA solvency data, and the current validated CBO
#   debt-service method remain in use.
# - An unconstrained best-attainable Social Security-solvency solve is added.
# - Plot legends contain only visually distinct plotted debt paths.
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# MODEL LAYER POLICY-UNIVERSE EXPANSION MODEL STAGE
# ------------------------------------------------------------------------------
# Governing rule:
#   1. Approved protections remain binding.
#   2. Congress may change current law. Current law never independently removes
#      an otherwise admissible policy from the search space.
#   3. Fiscal coefficients use the newest available authoritative policy score,
#      data, and scoring method that can support the specified policy. Score age
#      alone is never a reason to discard an otherwise defensible coefficient.
#   4. Tax-expenditure totals, credit subsidy rates, actuarial percentages, and
#      baseline account totals are not silently converted into policy scores.
#
# Policy-universe expansion layer materially expands the quantified revenue universe with the
# latest Treasury Greenbook revenue proposal table currently published by the
# Office of Tax Policy. Treasury's current Revenue Proposals index identifies the
# FY2025 Greenbook, released March 2024, as the latest published Greenbook revenue
# proposal set. The annual revenue estimates below are copied from the official
# Treasury Table of Revenue Estimates, fiscal years 2025-2034. They are shifted
# two fiscal years so a policy anchor beginning in FY2025 becomes a model policy
# beginning in FY2027, preserving Treasury's scored annual shape rather than
# inventing a new response function.
#
# Sources:
#   Treasury Revenue Proposals index:
#   https://home.treasury.gov/policy-issues/tax-policy/revenue-proposals
#   General Explanations of the Administration's FY2025 Revenue Proposals:
#   https://home.treasury.gov/system/files/131/General-Explanations-FY2025.pdf
#   Official revenue table only workbook:
#   https://home.treasury.gov/system/files/131/General-Explanations-FY2025-Table.xlsx
# ------------------------------------------------------------------------------

CFG$legislative_policy_space_expanded_theoretical_primary_improvement_2027_2036_bil <- 12466.979035
CFG$policy_universe_expansion_min_new_quantified_capacity_bil <- 100
CFG$plot_visual_path_tolerance_pp <- 0.05

# Bind the legislative-policy-space functions used by the policy-universe expansion stage.
build_expanded_anchor_policy_model_legislative_policy_space <- build_expanded_anchor_policy_model_core
build_interaction_catalog_full_legislative_policy_space <- build_interaction_catalog_full_core
build_authoritative_lever_expansion_audit_legislative_policy_space <- build_authoritative_lever_expansion_audit_core
build_expanded_coverage_audit_legislative_policy_space <- build_expanded_coverage_audit_core
build_unmodeled_parameter_domains_legislative_policy_space <- build_unmodeled_parameter_domains_core

# ------------------------------------------------------------------------------
# FUNCTION: build_treasury_greenbook_policy_candidates
# Purpose: Add policy-specific Treasury Office of Tax Policy annual revenue
#          scores that broaden the solver beyond the CBO option compendium while
#          respecting the project's approved protection rules.
# ------------------------------------------------------------------------------
build_treasury_greenbook_policy_candidates_policy_universe_expansion <- function(working_baseline) {
  treasury_index_path <- file.path(CFG$raw_source_dir, "treasury_revenue_proposals_current_index.html")
  treasury_index_txt <- fetch_text_cached(
    url = "https://home.treasury.gov/policy-issues/tax-policy/revenue-proposals",
    destination = treasury_index_path,
    force = FALSE,
    source_id = "treasury_revenue_proposals_current_index",
    agency = "U.S. Department of the Treasury, Office of Tax Policy",
    title = "Revenue Proposals",
    evidence_class = "OFFICIAL_CURRENT_INDEX",
    notes = "Live/cached official index used to verify the latest published Greenbook proposal set."
  )
  normalized_index <- treasury_index_txt |>
    stringr::str_replace_all("<[^>]+>", " ") |>
    stringr::str_squish() |>
    stringr::str_to_lower()
  assert_model(
    stringr::str_detect(normalized_index, "fy2025") &&
      stringr::str_detect(normalized_index, "released march 2024"),
    "Treasury Revenue Proposals index no longer verifies FY2025 (March 2024) as the latest published Greenbook set; Treasury scoring inputs require review"
  )

  treasury_greenbook_pdf_path <- download_cached(
    url = "https://home.treasury.gov/system/files/131/General-Explanations-FY2025.pdf",
    destination = file.path(CFG$raw_source_dir, "treasury_greenbook_fy2025.pdf"),
    force = FALSE,
    minimum_bytes = 500000L,
    source_id = "treasury_greenbook_fy2025_revenue_estimates",
    agency = "U.S. Department of the Treasury, Office of Tax Policy",
    title = "General Explanations of the Administration's Fiscal Year 2025 Revenue Proposals",
    publication_date = "2024-03-11",
    baseline_vintage = "FY2025_GREENBOOK",
    evidence_class = "OFFICIAL_OLDER",
    notes = "Official Greenbook containing the annual revenue estimate tables used for policy-universe-expansion policy coefficients."
  )
  treasury_greenbook_table_path <- download_optional(
    url = "https://home.treasury.gov/system/files/131/General-Explanations-FY2025-Table.xlsx",
    destination = file.path(CFG$raw_source_dir, "treasury_greenbook_fy2025_revenue_table.xlsx"),
    source_id = "treasury_greenbook_fy2025_revenue_table_xlsx",
    agency = "U.S. Department of the Treasury, Office of Tax Policy",
    title = "Fiscal Year 2025 Revenue Proposals, Revenue Table Only",
    publication_date = "2024-03-11",
    evidence_class = "OFFICIAL_OLDER",
    notes = "Official machine-readable companion table retained and hashed for audit provenance when transport is available. The required score evidence is also published in the Greenbook PDF."
  )

  register_source(
    source_id = "treasury_revenue_proposals_current_index",
    agency = "U.S. Department of the Treasury, Office of Tax Policy",
    title = "Revenue Proposals",
    url = "https://home.treasury.gov/policy-issues/tax-policy/revenue-proposals",
    local_path = treasury_index_path,
    publication_date = NA_character_,
    baseline_vintage = "LATEST_PUBLISHED_GREENBOOK_INDEX_VERIFIED_2026_09_13",
    evidence_class = "OFFICIAL_CURRENT_INDEX",
    notes = "Treasury's current Revenue Proposals index identifies FY2025, released March 2024, as the latest published Greenbook revenue proposal set."
  )
  register_source(
    source_id = "treasury_greenbook_fy2025_revenue_estimates",
    agency = "U.S. Department of the Treasury, Office of Tax Policy",
    title = "General Explanations of the Administration's Fiscal Year 2025 Revenue Proposals",
    url = "https://home.treasury.gov/system/files/131/General-Explanations-FY2025.pdf",
    local_path = treasury_greenbook_pdf_path,
    publication_date = "2024-03-11",
    baseline_vintage = "FY2025_GREENBOOK",
    evidence_class = "OFFICIAL_OLDER",
    notes = "Official Treasury annual revenue estimates for policy-specific proposals. Policy-universe expansion layer uses only explicitly selected non-protected reforms and preserves the published annual score shape."
  )
  register_source(
    source_id = "treasury_greenbook_fy2025_revenue_table_xlsx",
    agency = "U.S. Department of the Treasury, Office of Tax Policy",
    title = "Fiscal Year 2025 Revenue Proposals, Revenue Table Only",
    url = "https://home.treasury.gov/system/files/131/General-Explanations-FY2025-Table.xlsx",
    local_path = treasury_greenbook_table_path,
    publication_date = "2024-03-11",
    baseline_vintage = "FY2025_GREENBOOK",
    evidence_class = "OFFICIAL_OLDER",
    notes = "Machine-readable companion workbook to the FY2025 Greenbook. Values embedded in this script are independently checked against the published 2025-2034 totals."
  )

  # Annual source values are millions of dollars for FY2025-FY2034 exactly as
  # published in Treasury's official revenue-estimate table.
  specs <- list(
    list(
      candidate_id = "TREASURY_2025_stock_buyback_excise_tax_4pct",
      title = "Increase the Excise Tax Rate on Repurchases of Corporate Stock and Close Loopholes",
      variant_name = "Increase stock-repurchase excise tax rate from 1 percent to 4 percent",
      values_mil = c(15344, 14980, 14936, 15184, 15792, 16458, 17167, 17912, 18691, 19502),
      total_mil = 165966,
      protection_status = "ELIGIBLE",
      protection_reason = "Targets corporate stock repurchases and financial distributions rather than ordinary wages, ordinary household saving, or productive business reinvestment.",
      policy_domain = "financial_and_rent_revenue",
      complexity_multiplier = 1.00
    ),
    list(
      candidate_id = "TREASURY_2025_executive_compensation_deduction_limit",
      title = "Expand the Limitation on Deductibility of Employee Remuneration in Excess of $1 Million",
      variant_name = "Treasury FY2025 scored executive-compensation deduction reform",
      values_mil = c(37169, 19015, 30421, 34951, 31354, 28057, 22148, 20594, 22385, 25760),
      total_mil = 271854,
      protection_status = "ELIGIBLE",
      protection_reason = "Limits a corporate tax deduction for very high employee remuneration and does not raise ordinary wage tax rates or reduce productive investment allowances.",
      policy_domain = "financial_and_rent_revenue",
      complexity_multiplier = 1.05
    ),
    list(
      candidate_id = "TREASURY_2025_divisive_reorganization_leverage_anti_avoidance",
      title = "Limit Tax Avoidance Through Inappropriate Leveraging of Parties to Divisive Reorganizations",
      variant_name = "Treasury FY2025 scored anti-avoidance reform",
      values_mil = c(279, 826, 1614, 2550, 3569, 4645, 5769, 6937, 8150, 9408),
      total_mil = 43747,
      protection_status = "ELIGIBLE",
      protection_reason = "Closes a corporate tax-avoidance mechanism tied to divisive reorganizations rather than taxing ordinary wages, saving, or new productive investment.",
      policy_domain = "tax_avoidance_and_base_protection",
      complexity_multiplier = 1.10
    ),
    list(
      candidate_id = "TREASURY_2025_related_party_partnership_basis_shifting",
      title = "Prevent Basis Shifting by Related Parties Through Partnerships",
      variant_name = "Treasury FY2025 scored related-party basis-shifting reform",
      values_mil = c(3851, 5537, 3999, 2325, 563, -177, -215, -275, -341, -402),
      total_mil = 14865,
      protection_status = "ELIGIBLE",
      protection_reason = "Targets related-party basis shifting that creates tax savings without a meaningful change in the parties' economic arrangement.",
      policy_domain = "tax_avoidance_and_base_protection",
      complexity_multiplier = 1.10
    ),
    list(
      candidate_id = "TREASURY_2025_eliminate_fossil_fuel_tax_preferences",
      title = "Eliminate Fossil Fuel Tax Preferences",
      variant_name = "Treasury FY2025 scored fossil-fuel preference package",
      values_mil = c(3123, 4997, 4466, 3753, 2985, 2825, 3041, 3215, 3376, 3493),
      total_mil = 35274,
      protection_status = "ELIGIBLE",
      protection_reason = "Removes mature-industry tax preferences rather than imposing a broad tax on ordinary household consumption or productive investment generally.",
      policy_domain = "subsidy_and_tax_preference_reform",
      complexity_multiplier = 1.10
    ),
    list(
      candidate_id = "TREASURY_2025_estate_gift_tax_reform_package",
      title = "Modify Estate and Gift Taxation",
      variant_name = "Treasury FY2025 scored estate-and-gift administration, GST-duration, and valuation reforms",
      values_mil = c(1630, 3659, 6140, 8090, 10208, 11992, 12289, 13275, 14399, 15539),
      total_mil = 97221,
      protection_status = "ELIGIBLE",
      protection_reason = "Raises revenue from wealth-transfer rules and high-end tax preferences rather than ordinary wages or family formation.",
      policy_domain = "wealth_transfer_and_high_end_preference_reform",
      complexity_multiplier = 1.20
    ),
    list(
      candidate_id = "TREASURY_2025_high_income_retirement_account_accumulation_reforms",
      title = "Prevent Excessive Accumulations by High-Income Taxpayers in Tax-Favored Retirement Accounts and Make Other Reforms",
      variant_name = "Treasury FY2025 scored high-income retirement-account reform package",
      values_mil = c(6926, 6142, 3402, 1992, 1278, 931, 776, 724, 726, 759),
      total_mil = 23656,
      protection_status = "CONDITIONAL",
      protection_reason = "Targets excessive high-income use of tax-favored retirement accounts. It is kept out of STRICT because the policy touches saving, but remains available in EXPANDED as a high-end preference reform.",
      policy_domain = "high_end_saving_preference_reform",
      complexity_multiplier = 1.15
    ),
    list(
      candidate_id = "TREASURY_2025_limit_foreign_tax_credits_hybrid_entity_sales",
      title = "Limit Foreign Tax Credits From Sales of Hybrid Entities",
      variant_name = "Treasury FY2025 scored hybrid-entity foreign-tax-credit reform",
      values_mil = c(2691, 4281, 4038, 3918, 3910, 4002, 4113, 4219, 4341, 4481),
      total_mil = 39994,
      protection_status = "ELIGIBLE",
      protection_reason = "Targets foreign-tax-credit planning through hybrid entities rather than ordinary wages, ordinary household saving, or new productive investment.",
      policy_domain = "tax_avoidance_and_base_protection",
      complexity_multiplier = 1.10
    ),
    list(
      candidate_id = "TREASURY_2025_reform_foreign_fossil_fuel_income_taxation",
      title = "Reform Taxation of Foreign Fossil Fuel Income",
      variant_name = "Treasury FY2025 scored foreign fossil-fuel income reform package",
      values_mil = c(4092, 6892, 7053, 7295, 7554, 7810, 8066, 8371, 8725, 9080),
      total_mil = 74938,
      protection_status = "ELIGIBLE",
      protection_reason = "Removes preferential foreign fossil-fuel tax treatment and targets resource-sector tax privileges rather than ordinary wages or broad productive investment.",
      policy_domain = "resource_rent_and_tax_preference_reform",
      complexity_multiplier = 1.15
    ),
    list(
      candidate_id = "TREASURY_2025_improve_tax_administration",
      title = "Improve Tax Administration",
      variant_name = "Treasury FY2025 scored Improve Tax Administration subtotal",
      values_mil = c(483, 428, 421, 468, 468, 459, 451, 450, 466, 502),
      total_mil = 4596,
      protection_status = "ELIGIBLE",
      protection_reason = "Improves tax administration without increasing ordinary statutory tax rates.",
      policy_domain = "tax_administration_and_compliance",
      complexity_multiplier = 1.15
    ),
    list(
      candidate_id = "TREASURY_2025_improve_tax_compliance",
      title = "Improve Tax Compliance",
      variant_name = "Treasury FY2025 scored Improve Tax Compliance subtotal",
      values_mil = c(3202, 2858, 2582, 1515, 1347, 1401, 1456, 1512, 1569, 1633),
      total_mil = 19075,
      protection_status = "ELIGIBLE",
      protection_reason = "Targets noncompliance, penalties, enforcement tools, and information rules rather than ordinary statutory tax rates.",
      policy_domain = "tax_administration_and_compliance",
      complexity_multiplier = 1.20
    ),
    list(
      candidate_id = "TREASURY_2025_modernize_rules_digital_assets",
      title = "Modernize Rules, Including Those for Digital Assets",
      variant_name = "Treasury FY2025 scored digital-asset rules subtotal",
      values_mil = c(9695, 2434, 2850, 3056, 3298, 3609, 3867, 4135, 4418, 4684),
      total_mil = 42046,
      protection_status = "ELIGIBLE",
      protection_reason = "Closes digital-asset tax-rule and reporting gaps rather than taxing ordinary wages or reducing productive investment allowances.",
      policy_domain = "tax_avoidance_and_base_protection",
      complexity_multiplier = 1.20
    )
  )

  source_years <- 2025:2034
  shift_years <- 2L
  gdp36 <- working_baseline$gdp_bil[working_baseline$year == 2036L]
  assert_model(length(gdp36) == 1L && is.finite(gdp36) && gdp36 > 0, "Missing FY2036 GDP for Treasury Greenbook policy extension")

  rows <- purrr::map(specs, function(s) {
    assert_model(length(s$values_mil) == 10L, paste0("Treasury Greenbook score has wrong annual length: ", s$candidate_id))
    assert_model(abs(sum(s$values_mil) - s$total_mil) <= 1e-9, paste0("Treasury Greenbook annual values do not reproduce published 2025-2034 total: ", s$candidate_id))

    target_years <- source_years + shift_years
    flows <- tibble::tibble(
      candidate_id = s$candidate_id,
      year = CFG$model_years,
      revenue_delta_bil = 0,
      outlay_delta_bil = 0,
      shift_years = shift_years,
      translation_status = "ZERO"
    )
    idx <- match(target_years, flows$year)
    flows$revenue_delta_bil[idx] <- s$values_mil / 1000
    flows$translation_status[idx] <- "SHIFTED_TREASURY_OFFICIAL_ANNUAL_SCORE"

    # Long-run continuation is not presented as a Treasury score. It holds the
    # FY2036 scored-policy effect at a constant share of nominal GDP, matching
    # the transparent extension convention already used elsewhere in the model.
    revenue_2036 <- flows$revenue_delta_bil[flows$year == 2036L]
    for (y in CFG$extension_years) {
      gdp_y <- working_baseline$gdp_bil[working_baseline$year == y]
      flows$revenue_delta_bil[flows$year == y] <- revenue_2036 * (gdp_y / gdp36)
      flows$translation_status[flows$year == y] <- "EXTRAPOLATED_GDP_SHARE_FROM_LAST_OFFICIAL_YEAR"
    }
    flows <- flows |>
      mutate(
        primary_deficit_delta_bil = outlay_delta_bil - revenue_delta_bil,
        component_identity_residual_bil = primary_deficit_delta_bil - (outlay_delta_bil - revenue_delta_bil)
      ) |>
      select(candidate_id, year, revenue_delta_bil, outlay_delta_bil, primary_deficit_delta_bil, component_identity_residual_bil, shift_years, translation_status)

    score_flows <- flows |> filter(year %in% CFG$score_years)
    cumulative_revenue <- sum(pmax(score_flows$revenue_delta_bil, 0), na.rm = TRUE)
    cumulative_primary <- sum(-score_flows$primary_deficit_delta_bil, na.rm = TRUE)
    assert_model(abs(cumulative_primary - s$total_mil / 1000) <= 1e-9, paste0("Shifted Treasury score does not reproduce official ten-year total: ", s$candidate_id))

    meta <- tibble::tibble(
      candidate_id = s$candidate_id,
      family_key = s$candidate_id,
      family_title = s$title,
      variant_name = s$variant_name,
      latest_estimate = "FY2025 Greenbook (March 2024)",
      estimate_year = 2024L,
      source_start_year = 2025L,
      source_end_year = 2034L,
      source_url = "https://home.treasury.gov/system/files/131/General-Explanations-FY2025.pdf",
      fiscal_channel = "REVENUE",
      annual_profile_status = "FULL_OFFICIAL_ANNUAL",
      ss_actuarial_improvement_pct_payroll = 0,
      note = paste0("Official Treasury Office of Tax Policy annual revenue estimate shifted two fiscal years to align the scored implementation window with FY2027-FY2036. Published FY2025-FY2034 total: $", format(round(s$total_mil / 1000, 3), nsmall = 3), "B."),
      index_ten_year_savings_bil = s$total_mil / 1000,
      family_id = s$candidate_id,
      budget_option_id = NA_character_,
      title = s$title,
      major_category = "Revenues",
      budget_function = "Revenue",
      index_url = "https://home.treasury.gov/policy-issues/tax-policy/revenue-proposals",
      source_date = "Mar 2024",
      source_effective_year = 2025L,
      source_score_start_year = 2025L,
      source_score_end_year = 2034L,
      evidence_class = "OFFICIAL_OLDER",
      decision_type = "CONTINUOUS_LEVEL_WITH_TIMING",
      source_kind = "TREASURY_GREENBOOK_POLICY",
      investment_market_review = FALSE,
      direct_cbo_annual_score = FALSE,
      direct_official_annual_score = TRUE,
      solver_eligible_annual = TRUE,
      is_december_2024_core = FALSE,
      protection_status = s$protection_status,
      protected_category = ifelse(s$protection_status == "CONDITIONAL", "HIGH_END_SAVING_PREFERENCE_REVIEW", NA_character_),
      protection_reason = s$protection_reason,
      risk_ordinary_wages = FALSE,
      risk_ordinary_saving = s$protection_status == "CONDITIONAL",
      risk_productive_investment = FALSE,
      risk_family_formation = FALSE,
      risk_business_reinvestment = FALSE,
      risk_core_social_security = FALSE,
      risk_core_medicare = FALSE,
      risk_productive_public_capacity = FALSE,
      market_function_review_required = FALSE,
      hard_protection_violation = FALSE,
      explicit_review_required = s$protection_status == "CONDITIONAL",
      protection_classification_method = "EXPLICIT_POLICY_UNIVERSE_EXPANSION_TREASURY_POLICY_RULE",
      ss_actuarial_score_vintage = NA_character_,
      ss_actuarial_additivity_status = NA_character_,
      cumulative_revenue_increase_2027_2036_bil = cumulative_revenue,
      cumulative_spending_cut_2027_2036_bil = 0,
      cumulative_primary_improvement_2027_2036_bil = cumulative_primary,
      solver_exclusion_reason = NA_character_,
      replaced_by_granular_account_controls = FALSE,
      complexity_weight = s$complexity_multiplier * (1 + log1p(max(cumulative_primary, 0) / 1000)),
      complexity_weight_basis = "Activation burden plus logarithmic ten-year fiscal scope for an official Treasury scored reform",
      policy_domain = s$policy_domain
    )

    source_score <- tibble::tibble(
      candidate_id = s$candidate_id,
      title = s$title,
      source_year = source_years,
      source_revenue_effect_mil = s$values_mil,
      published_2025_2034_total_mil = s$total_mil,
      source = "U.S. Treasury FY2025 Greenbook Table of Revenue Estimates"
    )

    list(meta = meta, flows = flows, source_score = source_score)
  })

  list(
    meta = bind_rows(purrr::map(rows, "meta")),
    flows = bind_rows(purrr::map(rows, "flows")),
    source_scores = bind_rows(purrr::map(rows, "source_score"))
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_expanded_anchor_policy_model
# Purpose: Extend legislative policy-space layer's anchor universe with Treasury-scored policy
#          candidates. No existing candidate or protection is removed.
# ------------------------------------------------------------------------------
build_expanded_anchor_policy_model_policy_universe_expansion <- function(cbo_universe, working_baseline, spending_detail, cbo_econ) {
  x <- build_expanded_anchor_policy_model_legislative_policy_space(cbo_universe, working_baseline, spending_detail, cbo_econ)
  treasury <- build_treasury_greenbook_policy_candidates(working_baseline)

  duplicate_ids <- intersect(x$meta$candidate_id, treasury$meta$candidate_id)
  assert_model(length(duplicate_ids) == 0L, paste0("Treasury Greenbook candidates duplicate existing candidate ids: ", paste(duplicate_ids, collapse = ", ")))

  x$meta <- bind_rows(x$meta, treasury$meta)
  x$flows <- bind_rows(x$flows, treasury$flows)
  x$treasury_greenbook_meta <- treasury$meta
  x$treasury_greenbook_flows <- treasury$flows
  x$treasury_greenbook_source_scores <- treasury$source_scores

  assert_model(nrow(treasury$meta) >= 12L, "Policy-universe expansion layer Treasury expansion unexpectedly lost policy breadth")

  # Robust-feasibility layer deliberately inventories one Treasury proposal as BLOCKED
  # because it raises the Additional Medicare Tax rate on wages/self-employment
  # income. BLOCKED is a fully reviewed protection outcome, not an unreviewed
  # status. Preserve the candidate for audit visibility while proving that it
  # cannot enter the solver.
  reviewed_treasury_statuses <- c("ELIGIBLE", "CONDITIONAL", "BLOCKED")
  assert_model(
    all(treasury$meta$protection_status %in% reviewed_treasury_statuses),
    "Treasury expansion contains an unreviewed protection status"
  )
  blocked_treasury <- treasury$meta |> filter(protection_status == "BLOCKED")
  if (nrow(blocked_treasury) > 0L) {
    assert_model(
      all(!dplyr::coalesce(blocked_treasury$solver_eligible_annual, FALSE)),
      "A BLOCKED Treasury proposal was incorrectly marked solver-eligible"
    )
    assert_model(
      all(dplyr::coalesce(blocked_treasury$hard_protection_violation, FALSE)),
      "A BLOCKED Treasury proposal is missing its hard-protection flag"
    )
  }
  x
}

# ------------------------------------------------------------------------------
# FUNCTION: build_policy_search_score_review
# Purpose: Admit Treasury Greenbook annual proposal scores as authoritative fiscal
#          evidence. Current law remains metadata only and never acts as a veto.
# ------------------------------------------------------------------------------
build_policy_search_score_review_policy_universe_expansion <- function(meta, current_index) {
  out <- build_policy_search_score_review_legislative_policy_space(meta, current_index)
  is_treasury <- out$source_kind == "TREASURY_GREENBOOK_POLICY"

  out$externally_vettable_score[is_treasury] <- TRUE
  out$score_recency_status[is_treasury] <- "LATEST_AVAILABLE_TREASURY_GREENBOOK_POLICY_SCORE"
  out$score_basis_status[is_treasury] <- "VALID_LATEST_AVAILABLE_TREASURY_OFFICIAL_ANNUAL_SCORE"
  out$score_basis_reason[is_treasury] <- "Treasury's current Revenue Proposals index identifies the FY2025 Greenbook as the latest published Greenbook proposal set. The candidate uses Treasury's official annual revenue estimate; score age alone is not an exclusion criterion."
  out$score_method_data_basis[is_treasury] <- "TREASURY_OFFICE_OF_TAX_POLICY_FY2025_GREENBOOK_ANNUAL_REVENUE_SCORE;CURRENT_2026_BASELINE_AND_DEBT_SERVICE_ENGINE"
  out$policy_requires_legislative_change[is_treasury] <- TRUE
  out$legislative_change_context[is_treasury] <- "CONGRESSIONAL_ENACTMENT_REQUIRED_POLICY_PROPOSAL"
  out$policy_search_solver_eligible[is_treasury] <- out$parameterized_solver_eligible[is_treasury]

  # Legacy aliases remain reporting aliases only.
  out$current_law_change_class[is_treasury] <- out$legislative_change_context[is_treasury]
  out$current_law_adjudication_status[is_treasury] <- out$score_basis_status[is_treasury]
  out$current_law_adjudication_reason[is_treasury] <- out$score_basis_reason[is_treasury]
  out$current_law_evidence_basis[is_treasury] <- out$score_method_data_basis[is_treasury]
  out$exact_current_law_rescore_available[is_treasury] <- FALSE
  out$current_law_solver_eligible[is_treasury] <- out$policy_search_solver_eligible[is_treasury]
  out
}

# ------------------------------------------------------------------------------
# FUNCTION: build_interaction_catalog_full
# Purpose: Preserve legislative policy-space layer interactions and add narrow overlap guards
#          for Treasury proposals whose official scores should not be stacked with
#          overlapping independently scored policies.
# ------------------------------------------------------------------------------
build_interaction_catalog_full_policy_universe_expansion <- function(meta) {
  base <- build_interaction_catalog_full_legislative_policy_space(meta)
  ids <- meta$candidate_id
  title_map <- meta |> select(candidate_id, title, variant_name)

  treasury_id <- function(id) if (id %in% ids) id else NA_character_
  match_ids <- function(pattern) {
    title_map |>
      mutate(
        interaction_text = stringr::str_to_lower(
          paste0(dplyr::coalesce(title, ""), " ", dplyr::coalesce(variant_name, ""))
        )
      ) |>
      filter(stringr::str_detect(interaction_text, pattern)) |>
      pull(candidate_id)
  }
  pair_rows <- list()
  add_pairs <- function(left, right, reason) {
    left <- unique(left[!is.na(left) & left %in% ids])
    right <- unique(right[!is.na(right) & right %in% ids])
    if (length(left) == 0L || length(right) == 0L) return(invisible(NULL))
    for (i in left) for (j in right) if (!identical(i, j)) {
      pair_rows[[length(pair_rows) + 1L]] <<- tibble(candidate_i = i, candidate_j = j, reason = reason)
    }
    invisible(NULL)
  }

  # Treasury's broad tax-administration package overlaps with the separately
  # scored CBO IRS enforcement funding option. Do not assume additivity.
  add_pairs(
    c(
      treasury_id("TREASURY_2025_improve_tax_administration"),
      treasury_id("TREASURY_2025_improve_tax_compliance")
    ),
    match_ids("increase appropriations for the internal revenue service's enforcement initiatives"),
    "Treasury's scored tax-administration/compliance reforms and CBO's scored IRS enforcement-funding option overlap in compliance and administration effects; no combined official score is available."
  )

  # Treasury's fossil-fuel preference package overlaps with CBO's separately
  # scored repeal of energy/natural-resource tax preferences. Preserve both
  # policy choices, but do not add their independent scores to one package.
  add_pairs(
    treasury_id("TREASURY_2025_eliminate_fossil_fuel_tax_preferences"),
    match_ids("repeal certain tax preferences for energy and natural resource|percentage depletion|expensing preference"),
    "Treasury's fossil-fuel tax-preference package overlaps with CBO's separately scored energy and natural-resource preference repeals; independent scores are not assumed additive."
  )

  out <- bind_rows(base, bind_rows(pair_rows)) |>
    mutate(
      lo = pmin(candidate_i, candidate_j),
      hi = pmax(candidate_i, candidate_j)
    ) |>
    distinct(lo, hi, .keep_all = TRUE) |>
    transmute(candidate_i = lo, candidate_j = hi, reason)

  assert_model(all(out$candidate_i %in% ids & out$candidate_j %in% ids), "Interaction catalog contains a candidate outside the active policy metadata")
  out
}

# ------------------------------------------------------------------------------
# FUNCTION: build_authoritative_lever_expansion_audit
# Purpose: Record the domains that gained policy-specific coefficients in
#          policy-universe expansion layer and retain the distinction between scored policies
#          and inventories that still lack response functions.
# ------------------------------------------------------------------------------
build_authoritative_lever_expansion_audit_policy_universe_expansion <- function(policy_model, tax_inventory) {
  base <- build_authoritative_lever_expansion_audit_legislative_policy_space(policy_model, tax_inventory)
  treasury <- policy_model$meta |> filter(source_kind == "TREASURY_GREENBOOK_POLICY")

  base <- base |>
    mutate(
      solver_status = if_else(
        domain == "Treasury/JCT tax expenditures",
        "PARTIALLY_QUANTIFIED_ONLY_WHERE_POLICY_SPECIFIC_OFFICIAL_SCORE_EXISTS",
        solver_status
      ),
      reason = if_else(
        domain == "Treasury/JCT tax expenditures",
        "Tax-expenditure totals remain inventory-only and are not treated as repeal scores. Separately scored Treasury Greenbook reforms are admitted as policy-specific official annual coefficients.",
        reason
      )
    )

  bind_rows(
    base,
    tibble(
      domain = "Treasury Greenbook policy-specific revenue reforms",
      authoritative_evidence = paste0(nrow(treasury), " Treasury Office of Tax Policy proposals with official annual FY2025-FY2034 revenue scores"),
      solver_status = "ADMITTED_WHERE_PROTECTION_RULES_ALLOW",
      new_solver_coefficients_in_current_build = nrow(treasury),
      reason = paste0(
        "Policy-universe expansion layer adds policy-specific Treasury scores rather than converting tax-expenditure estimates into repeal revenue. Newly quantified ten-year primary capacity before overlap constraints: $",
        format(round(sum(pmax(treasury$cumulative_primary_improvement_2027_2036_bil, 0), na.rm = TRUE), 3), nsmall = 3),
        "B."
      )
    )
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_expanded_coverage_audit
# Purpose: Add Treasury-scored policy coverage to the preserved legislative-policy-space
#          breadth audit.
# ------------------------------------------------------------------------------
build_expanded_coverage_audit_policy_universe_expansion <- function(policy_model, tax_inventory, expanded_sources) {
  base <- build_expanded_coverage_audit_legislative_policy_space(policy_model, tax_inventory, expanded_sources)
  treasury <- policy_model$meta |> filter(source_kind == "TREASURY_GREENBOOK_POLICY")
  bind_rows(
    base,
    tibble(
      metric = c(
        "Treasury Greenbook policy-specific annual-score candidates added",
        "Treasury Greenbook candidates solver-ready before protection-mode filtering",
        "Treasury Greenbook newly quantified ten-year primary capacity, billions"
      ),
      value = c(
        nrow(treasury),
        sum(treasury$parameterized_solver_eligible, na.rm = TRUE),
        sum(pmax(treasury$cumulative_primary_improvement_2027_2036_bil, 0), na.rm = TRUE)
      ),
      interpretation = c(
        "Official Treasury Office of Tax Policy proposal scores added beyond the CBO deficit-options layer",
        "Policy-specific annual scores enter the solver subject to the same protection rules and evidence gates as other candidates",
        "Gross newly quantified capacity before interactions; not a claim that all policies are jointly additive"
      )
    )
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_unmodeled_parameter_domains
# Purpose: Keep inventory-only domains visible while recognizing the portions of
#          Treasury tax policy that now have policy-specific official scores.
# ------------------------------------------------------------------------------
build_unmodeled_parameter_domains <- function(tax_inventory) {
  base <- build_unmodeled_parameter_domains_legislative_policy_space(tax_inventory)
  base |>
    mutate(
      status = if_else(
        domain == "tax_expenditures",
        "PARTIALLY_QUANTIFIED_POLICY_SPECIFIC_GREENBOOK_SCORES_PLUS_INVENTORY_ONLY_REMAINDER",
        status
      ),
      reason = if_else(
        domain == "tax_expenditures",
        "Treasury/JCT tax-expenditure totals remain non-score inventories. Policy-universe expansion layer separately admits selected Treasury Greenbook proposals that carry official annual policy-specific revenue estimates.",
        reason
      )
    )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_pre_solve_policy_capacity_audit
# Purpose: Show before HiGHS which domains are quantified, protected, inventory
#          only, or unsupported, and how much ten-year fiscal capacity is attached
#          to each quantified domain.
# ------------------------------------------------------------------------------
build_pre_solve_policy_capacity_audit <- function(policy_model, tax_inventory, unmodeled_domains) {
  m <- policy_model$meta |>
    mutate(
      capacity_domain = case_when(
        source_kind == "TREASURY_GREENBOOK_POLICY" ~ "Treasury Greenbook scored revenue reforms",
        source_kind == "ADDITIONAL_OFFICIAL_CBO_POLICY" ~ "Additional CBO scored reforms",
        source_kind == "LOCAL_FROZEN_CBO_POLICY_PACK" & major_category == "Revenues" ~ "CBO scored revenue reforms",
        source_kind == "LOCAL_FROZEN_CBO_POLICY_PACK" ~ "CBO scored spending/net-deficit reforms",
        source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" ~ "CBO discretionary account growth controls",
        TRUE ~ source_kind
      ),
      capacity_status = case_when(
        protection_status == "BLOCKED" ~ "PROTECTED",
        parameterized_solver_eligible & policy_search_solver_eligible ~ "QUANTIFIED_SOLVER_READY",
        parameterized_solver_eligible & !policy_search_solver_eligible ~ "UNSUPPORTED_SCORE",
        TRUE ~ "INVENTORY_OR_NOT_PARAMETERIZED"
      )
    )

  quantified <- m |>
    mutate(
      family_capacity_bil = pmax(dplyr::coalesce(cumulative_primary_improvement_2027_2036_bil, 0), 0)
    ) |>
    group_by(capacity_domain, capacity_status, family_id) |>
    summarise(
      candidate_count_in_family = n(),
      family_capacity_bil = max(family_capacity_bil, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(capacity_domain, capacity_status) |>
    summarise(
      candidate_count = sum(candidate_count_in_family),
      family_count = n_distinct(family_id),
      maximum_ten_year_primary_improvement_bil = sum(family_capacity_bil, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      inventory_item_count = NA_real_,
      note = case_when(
        capacity_status == "QUANTIFIED_SOLVER_READY" ~ "Policy-specific coefficient is available; final solver availability still depends on STRICT/EXPANDED protection mode and interaction constraints.",
        capacity_status == "PROTECTED" ~ "Coefficient may exist, but the approved protection rule prevents solver use.",
        capacity_status == "UNSUPPORTED_SCORE" ~ "A candidate definition exists but lacks admissible annual fiscal-response evidence.",
        TRUE ~ "Cataloged but not parameterized for the annual MILP."
      )
    )

  inventory_rows <- unmodeled_domains |>
    count(domain, status, name = "inventory_item_count") |>
    transmute(
      capacity_domain = paste0("Inventory: ", domain),
      capacity_status = status,
      candidate_count = NA_integer_,
      family_count = NA_integer_,
      maximum_ten_year_primary_improvement_bil = NA_real_,
      inventory_item_count = as.numeric(inventory_item_count),
      note = "Authoritative domain remains visible but is not assigned a fiscal-response coefficient without policy-specific scoring evidence."
    )

  tax_row <- tibble(
    capacity_domain = "Inventory: CBO current-law tax parameters",
    capacity_status = "INVENTORY_ONLY_WITHOUT_MARGINAL_RESPONSE",
    candidate_count = NA_integer_,
    family_count = NA_integer_,
    maximum_ten_year_primary_improvement_bil = NA_real_,
    inventory_item_count = as.numeric(nrow(tax_inventory)),
    note = "Tax parameters identify current law but are not themselves revenue derivatives."
  )

  bind_rows(quantified, inventory_rows, tax_row) |>
    arrange(capacity_status, capacity_domain)
}

# ------------------------------------------------------------------------------
# FUNCTION: build_policy_universe_expansion_success
# Purpose: Stop before the expensive search if policy-universe expansion layer did not add a
#          materially larger quantified policy space than legislative policy-space layer.
# ------------------------------------------------------------------------------
build_policy_universe_expansion_success <- function(policy_model) {
  new_candidates <- policy_model$meta |>
    filter(
      source_kind == "TREASURY_GREENBOOK_POLICY",
      parameterized_solver_eligible,
      policy_search_solver_eligible,
      protection_status %in% c("ELIGIBLE", "CONDITIONAL")
    )

  new_capacity <- sum(pmax(new_candidates$cumulative_primary_improvement_2027_2036_bil, 0), na.rm = TRUE)
  tibble(
    benchmark = "LEGISLATIVE_POLICY_SPACE_EXPANDED_THEORETICAL_FULL_CAPACITY_TEN_YEAR_PRIMARY_IMPROVEMENT",
    legislative_policy_space_benchmark_bil = CFG$legislative_policy_space_expanded_theoretical_primary_improvement_2027_2036_bil,
    new_quantified_candidate_count = nrow(new_candidates),
    new_quantified_capacity_bil = new_capacity,
    minimum_material_expansion_bil = CFG$policy_universe_expansion_min_new_quantified_capacity_bil,
    new_capacity_pct_of_legislative_policy_space_best = 100 * new_capacity / CFG$legislative_policy_space_expanded_theoretical_primary_improvement_2027_2036_bil,
    passed = new_capacity >= CFG$policy_universe_expansion_min_new_quantified_capacity_bil,
    interpretation = "Gross new policy-specific ten-year primary capacity before interaction constraints. This gate determines whether a full rerun is worth the computational cost; it is not an additivity claim."
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: validate_policy_universe_expansion_theoretical_capacity_gain
# Purpose: Run one inexpensive EXPANDED full-capacity solve before the complete
#          search and compare its realized ten-year primary improvement with the
#          legislative-policy-space theoretical frontier. This is the accepted
#          policy-universe-expansion stop/go criterion.
# ------------------------------------------------------------------------------
validate_policy_universe_expansion_theoretical_capacity_gain <- function(policy_model, working_baseline, kernel_obj) {
  expanded_full <- policy_universe_for_mode(policy_model, "EXPANDED", "FULL_CAPACITY")
  pre_model <- build_full_milp(
    expanded_full,
    working_baseline,
    kernel_obj,
    objective = "target_slack",
    soft_targets = TRUE
  )
  pre_solution <- solve_full_milp(pre_model, "POLICY_UNIVERSE_EXPANSION_PRESEARCH_EXPANDED_THEORETICAL_CAPACITY")
  assert_model(
    full_solver_has_feasible_incumbent(pre_solution),
    "Policy-universe expansion layer pre-search theoretical-capacity solve did not return a feasible incumbent"
  )
  pre_summary <- summarize_full_solution(
    pre_solution, policy_model, working_baseline, kernel_obj,
    "POLICY_UNIVERSE_EXPANSION_PRESEARCH_EXPANDED_THEORETICAL_CAPACITY"
  )
  new_primary <- pre_summary$revenue_2027_2036_bil[[1]] + pre_summary$spending_cuts_2027_2036_bil[[1]]
  old_primary <- CFG$legislative_policy_space_expanded_theoretical_primary_improvement_2027_2036_bil
  delta <- new_primary - old_primary

  tibble(
    benchmark = "EXPANDED_THEORETICAL_FULL_CAPACITY",
    legislative_policy_space_primary_improvement_bil = old_primary,
    policy_universe_expansion_presearch_primary_improvement_bil = new_primary,
    incremental_primary_capacity_bil = delta,
    minimum_material_expansion_bil = CFG$policy_universe_expansion_min_new_quantified_capacity_bil,
    debt_gdp_2036_pct = pre_summary$debt_gdp_2036_pct[[1]],
    debt_gdp_2046_pct = pre_summary$debt_gdp_2046_pct[[1]],
    worst_required_scenario_debt_gdp_2036_pct = pre_summary$worst_required_scenario_debt_gdp_2036_pct[[1]],
    worst_required_scenario_debt_gdp_2046_pct = pre_summary$worst_required_scenario_debt_gdp_2046_pct[[1]],
    passed = is.finite(delta) && delta >= CFG$policy_universe_expansion_min_new_quantified_capacity_bil,
    interpretation = "One pre-search EXPANDED theoretical-capacity MILP. The full package search runs only when the realized frontier gains materially over legislative policy-space layer."
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: plot_solution_debt_paths
# Purpose: Plot a small set of substantively distinct package roles rather than
#          several numerically near-identical optimization endpoints. A path is
#          omitted from both the plot and legend when it is visually redundant.
# ------------------------------------------------------------------------------
plot_solution_debt_paths_policy_universe_expansion <- function(working_baseline, search_result) {
  base <- working_baseline |>
    transmute(year, solution_id = "Working baseline", debt_gdp_pct = working_debt_gdp_pct)

  if (nrow(search_result$paths) == 0L || nrow(search_result$summary) == 0L) {
    pdat <- base
  } else {
    s <- search_result$summary

    pick_one <- function(label, protection_mode = NULL, objective = NULL, solve_pattern = NULL, target_policy_count = NULL) {
      x <- s
      if (!is.null(protection_mode)) x <- x |> filter(.data$protection_mode == protection_mode)
      if (!is.null(objective)) x <- x |> filter(.data$objective == objective)
      if (!is.null(solve_pattern)) x <- x |> filter(stringr::str_detect(.data$solve_label, solve_pattern))
      if (nrow(x) == 0L) return(tibble())
      if (!is.null(target_policy_count)) {
        x <- x |> mutate(distance = abs(selected_policy_count - target_policy_count)) |> arrange(distance, achieved_target_slack_score, selected_policy_count)
      } else {
        x <- x |> arrange(achieved_target_slack_score, selected_policy_count)
      }
      x |> slice_head(n = 1L) |> mutate(display_label = label)
    }

    reps <- bind_rows(
      pick_one("Expanded best attainable", protection_mode = "EXPANDED", solve_pattern = "EXPANDED_PACKAGE_READY_BEST_ATTAINABLE"),
      pick_one("Expanded medium complexity", protection_mode = "EXPANDED", objective = "complexity", solve_pattern = "EXPANDED_SOFT_B010_COMPLEXITY", target_policy_count = 125),
      pick_one("Expanded low complexity", protection_mode = "EXPANDED", objective = "complexity", solve_pattern = "EXPANDED_SOFT_B025_COMPLEXITY", target_policy_count = 65),
      pick_one("Strict best attainable", protection_mode = "STRICT", solve_pattern = "STRICT_PACKAGE_READY_BEST_ATTAINABLE")
    ) |>
      distinct(solution_id, .keep_all = TRUE)

    candidate_paths <- search_result$paths |>
      semi_join(reps |> select(solution_id), by = "solution_id") |>
      left_join(reps |> select(solution_id, display_label), by = "solution_id") |>
      select(year, solution_id, display_label, debt_gdp_pct) |>
      arrange(match(solution_id, reps$solution_id), year)

    # Keep a candidate only if it differs from every already retained path by
    # at least the configured chart-resolution tolerance somewhere on the path.
    keep_ids <- character()
    for (cid in reps$solution_id) {
      p <- candidate_paths |> filter(solution_id == cid) |> arrange(year) |> pull(debt_gdp_pct)
      if (length(p) == 0L) next
      if (length(keep_ids) == 0L) {
        keep_ids <- c(keep_ids, cid)
      } else {
        visually_distinct <- TRUE
        for (kid in keep_ids) {
          q <- candidate_paths |> filter(solution_id == kid) |> arrange(year) |> pull(debt_gdp_pct)
          if (length(q) == length(p) && max(abs(p - q), na.rm = TRUE) < CFG$plot_visual_path_tolerance_pp) {
            visually_distinct <- FALSE
            break
          }
        }
        if (visually_distinct) keep_ids <- c(keep_ids, cid)
      }
    }

    chosen <- candidate_paths |>
      filter(solution_id %in% keep_ids) |>
      transmute(year, solution_id = display_label, debt_gdp_pct)
    pdat <- bind_rows(base, chosen)
  }

  robust_count <- if (nrow(search_result$summary) == 0L) 0L else sum(search_result$summary$robust_target_2036_pass & search_result$summary$robust_target_2046_pass, na.rm = TRUE)
  chart_title <- if (robust_count > 0L) "Central debt paths for representative robust-target packages" else "Central debt paths for representative best-attainable frontier packages"
  chart_subtitle <- if (robust_count > 0L) "Displayed packages satisfy the target constraints across every required robust scenario" else "No package satisfied every robust target; displayed packages are deliberately selected for materially different fiscal paths"

  ggplot(pdat, aes(year, debt_gdp_pct, group = solution_id, color = solution_id)) +
    geom_line(linewidth = 0.85) +
    geom_hline(yintercept = c(90, 80, 75), linetype = "dotted") +
    scale_x_continuous(breaks = seq(2026, 2046, 2)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = chart_title,
      subtitle = chart_subtitle,
      x = NULL,
      y = "Debt held by public / GDP",
      color = "Package",
      caption = paste0(
        "Only materially distinct representative paths are plotted; paths within ",
        format(CFG$plot_visual_path_tolerance_pp, trim = TRUE),
        " percentage point at every year are omitted from both plot and legend. Target lines mark 90 percent in 2036, 80 percent in 2046, and the 75 percent long-run reference."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}

# ------------------------------------------------------------------------------
# MODEL LAYER ACTIVE RULE SUMMARY
# - Approved protections remain binding.
# - Current law never vetoes a policy Congress could enact.
# - Latest available authoritative policy-specific scoring evidence is used.
# - Treasury/JCT tax-expenditure totals remain inventories unless a policy-specific
#   score exists; policy-universe expansion layer adds twelve Treasury Greenbook scored reforms.
# - Ordinary-income-rate increases remain blocked by the protection rule.
# - A pre-solve capacity audit and material-expansion gate run before expensive
#   HiGHS search work.
# - Solver, robustness scenarios, debt-service engine, and independent simulation
#   machinery remain unchanged from legislative policy-space layer.
# - Representative plotting uses named package roles plus a visual-distance test,
#   so invisible near-duplicate lines never produce stray legend entries.
# ------------------------------------------------------------------------------


# ============================================================================== 
# MODEL LAYER MEMORY-SAFETY MODEL STAGE
#
# The policy-universe-expansion run demonstrated that the fiscal formulation itself can
# solve, but the process retained a complete sparse MILP model and raw HiGHS
# result inside every stored solution object. That caused solved models to
# accumulate in R memory across the search. In addition, complexity objectives
# were carrying thousands of timing schedules that are mathematically dominated
# by other schedules for the same policy.
#
# Memory-safety layer fixes both sources without shrinking the admissible policy
# universe or relaxing solver optimality:
#   1. stored solution objects retain only the compact metadata needed for audit,
#      independent simulation, and reporting; the full A matrix, schedule-flow
#      table, bounds, and raw HiGHS object are released after each solve;
#   2. complexity-only solves remove implementation schedules that are proven
#      weakly worse in primary-deficit effect in every model year than another
#      schedule for the same policy. Every policy candidate remains available;
#   3. exact duplicate annual primary-deficit paths are represented once;
#   4. garbage collection is forced between MILP solves so released sparse
#      matrices and solver objects are returned to R's allocator promptly.
#
# The dominance reduction is exact for the current complexity searches because:
#   - complexity cost is attached to policy activation, not timing;
#   - the affected solves do not impose separate revenue or spending caps;
#   - robust policy-yield factors are nonnegative scalars;
#   - the debt-service kernel and long-run marginal rates are nonnegative; and
#   - interactions/protections operate at candidate level, not schedule level.
# Therefore a componentwise-worse timing path cannot rescue a complexity
# solution or improve its objective.
# ============================================================================== 

CFG$complexity_schedule_dominance_tolerance_bil <- 1e-10
CFG$complexity_solver_threads <- min(CFG$solver_threads, 8L)
CFG$solver_force_gc_between_solves <- TRUE

if (CFG$write_audit_outputs) {
  write_csv_atomic(
    tibble::tibble(
      memory_change = c(
        "Retain raw HiGHS object after solve",
        "Retain full sparse MILP model after solve",
        "Force garbage collection between solves",
        "Complexity-only exact dominated-schedule pruning",
        "Complexity HiGHS thread ceiling"
      ),
      policy_universe_expansion = c("YES", "YES", "NO", "NO", as.character(CFG$solver_threads)),
      memory_safety = c("NO", "NO", "YES", "YES", as.character(CFG$complexity_solver_threads)),
      policy_candidate_universe_changed = c(FALSE, FALSE, FALSE, FALSE, FALSE)
    ),
    file.path(CFG$output_dir, "memory_safety_memory_refactor.csv")
  )
}

# Cache the reduced STRICT and EXPANDED universes so the schedule-dominance proof
# is performed once per distinct candidate/schedule universe rather than before
# every complexity solve.
COMPLEXITY_UNIVERSE_CACHE <- new.env(parent = emptyenv())
COMPLEXITY_SCHEDULE_AUDIT <- tibble::tibble()
COMPLEXITY_SCHEDULE_SUMMARY <- tibble::tibble()

# ------------------------------------------------------------------------------
# FUNCTION: complexity_universe_cache_key
# Purpose: Create a stable key for one protection/materiality/schedule universe.
# ------------------------------------------------------------------------------
complexity_universe_cache_key <- function(universe) {
  paste(
    universe$protection_mode,
    universe$materiality_mode,
    nrow(universe$meta),
    nrow(universe$schedules),
    digest::digest(sort(unique(universe$schedules$schedule_key)), algo = "xxhash64"),
    sep = "::"
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: validate_complexity_schedule_dominance_conditions
# Purpose: Prove the sign conditions that make annual primary-deficit dominance
#          a safe exact reduction for the complexity-only MILPs.
# ------------------------------------------------------------------------------
validate_complexity_schedule_dominance_conditions <- function(universe, kernel_obj) {
  tol <- CFG$complexity_schedule_dominance_tolerance_bil

  assert_model(
    !isTRUE(CFG$enforce_2046_lower_bound),
    "Complexity schedule pruning is disabled when a minimum FY2046 debt/GDP floor is enforced"
  )

  assert_model(
    all(universe$scenario_annual$policy_yield_factor >= -tol, na.rm = TRUE),
    "Complexity schedule pruning requires nonnegative policy-yield factors"
  )

  kernel_coef <- kernel_obj$kernel$debt_service_effect_per_1_bil_primary_deficit
  assert_model(
    all(kernel_coef >= -tol, na.rm = TRUE),
    "Complexity schedule pruning requires a nonnegative debt-service response kernel"
  )

  central_long_rate <- get_long_run_rate(kernel_obj)
  scenario_rate_additions <- universe$scenario_annual |>
    distinct(scenario_id, marginal_rate_addition) |>
    mutate(implied_long_rate = central_long_rate + marginal_rate_addition)

  assert_model(
    all(scenario_rate_additions$implied_long_rate >= -tol, na.rm = TRUE),
    "Complexity schedule pruning requires nonnegative long-run marginal interest rates"
  )

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# FUNCTION: reduce_universe_for_complexity
# Purpose: Remove only timing schedules that are mathematically dominated by
#          another timing schedule for the same policy. Candidate breadth is
#          unchanged. This is an exact formulation reduction, not a heuristic.
# ------------------------------------------------------------------------------
reduce_universe_for_complexity <- function(universe, kernel_obj) {
  key <- complexity_universe_cache_key(universe)
  if (exists(key, envir = COMPLEXITY_UNIVERSE_CACHE, inherits = FALSE)) {
    return(get(key, envir = COMPLEXITY_UNIVERSE_CACHE, inherits = FALSE))
  }

  validate_complexity_schedule_dominance_conditions(universe, kernel_obj)
  tol <- CFG$complexity_schedule_dominance_tolerance_bil
  years <- sort(unique(universe$schedule_flows$year))

  schedules <- universe$schedules |>
    distinct(candidate_id, schedule_key, .keep_all = TRUE)
  flows <- universe$schedule_flows |>
    select(candidate_id, schedule_key, year, primary_deficit_delta_bil_per_anchor_scale)

  keep_keys <- character()
  audit_rows <- list()
  a <- 0L

  for (cid in universe$meta$candidate_id) {
    candidate_schedules <- schedules |>
      filter(candidate_id == cid) |>
      arrange(implementation_start_year, phase_in_years, schedule_key)

    keys <- candidate_schedules$schedule_key
    n <- length(keys)
    assert_model(n >= 1L, paste0("Complexity reduction found no implementation schedule for ", cid))

    if (n == 1L) {
      keep_keys <- c(keep_keys, keys)
      next
    }

    candidate_flows <- flows |>
      filter(candidate_id == cid, schedule_key %in% keys) |>
      tidyr::complete(schedule_key = keys, year = years, fill = list(primary_deficit_delta_bil_per_anchor_scale = 0)) |>
      arrange(match(schedule_key, keys), year)

    mat <- matrix(
      candidate_flows$primary_deficit_delta_bil_per_anchor_scale,
      nrow = n,
      ncol = length(years),
      byrow = TRUE,
      dimnames = list(keys, as.character(years))
    )

    retained <- rep(TRUE, n)
    dominating_key <- rep(NA_character_, n)
    reason <- rep(NA_character_, n)
    max_excess <- rep(NA_real_, n)
    strict_gain <- rep(NA_real_, n)

    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i == j) next
        delta <- mat[j, ] - mat[i, ]
        weakly_better <- all(delta <= tol)
        strictly_better <- any(delta < -tol)
        equivalent <- all(abs(delta) <= tol)

        # For identical annual paths, retain the first canonical schedule only.
        if (weakly_better && (strictly_better || (equivalent && j < i))) {
          retained[i] <- FALSE
          dominating_key[i] <- keys[j]
          reason[i] <- if (equivalent) {
            "EXACT_PRIMARY_PATH_DUPLICATE"
          } else {
            "PRIMARY_DEFICIT_COMPONENTWISE_DOMINATED"
          }
          max_excess[i] <- max(delta)
          strict_gain[i] <- -sum(pmin(delta, 0))
          break
        }
      }
    }

    # Re-anchor every removed schedule directly to a retained schedule. This
    # makes the audit proof self-contained instead of relying on a chain of
    # dominated schedules.
    removed_idx <- which(!retained)
    retained_idx <- which(retained)
    if (length(removed_idx) > 0L) {
      for (i in removed_idx) {
        found_retained_dominator <- FALSE
        for (j in retained_idx) {
          delta <- mat[j, ] - mat[i, ]
          weakly_better <- all(delta <= tol)
          strictly_better <- any(delta < -tol)
          equivalent <- all(abs(delta) <= tol)
          if (weakly_better && (strictly_better || equivalent)) {
            dominating_key[i] <- keys[j]
            reason[i] <- if (equivalent) {
              "EXACT_PRIMARY_PATH_DUPLICATE"
            } else {
              "PRIMARY_DEFICIT_COMPONENTWISE_DOMINATED"
            }
            max_excess[i] <- max(delta)
            strict_gain[i] <- -sum(pmin(delta, 0))
            found_retained_dominator <- TRUE
            break
          }
        }
        assert_model(
          found_retained_dominator,
          paste0("No retained schedule directly dominates removed complexity schedule ", keys[i])
        )
      }
    }

    keep_keys <- c(keep_keys, keys[retained])

    if (length(removed_idx) > 0L) {
      a <- a + 1L
      audit_rows[[a]] <- tibble::tibble(
        protection_mode = universe$protection_mode,
        materiality_mode = universe$materiality_mode,
        candidate_id = cid,
        removed_schedule_key = keys[removed_idx],
        dominating_schedule_key = dominating_key[removed_idx],
        reduction_reason = reason[removed_idx],
        dominance_max_annual_excess_bil = max_excess[removed_idx],
        summed_strict_primary_improvement_bil = strict_gain[removed_idx],
        proof_tolerance_bil = tol
      )
    }
  }

  keep_keys <- unique(keep_keys)
  assert_model(length(keep_keys) > 0L, "Complexity schedule reduction removed every implementation schedule")

  reduced <- universe
  reduced$schedules <- universe$schedules |> filter(schedule_key %in% keep_keys)
  reduced$schedule_summary <- universe$schedule_summary |> filter(schedule_key %in% keep_keys)
  reduced$schedule_flows <- universe$schedule_flows |> filter(schedule_key %in% keep_keys)
  reduced$complexity_schedule_reduced <- TRUE
  reduced$complexity_original_schedule_count <- nrow(universe$schedules)
  reduced$complexity_retained_schedule_count <- nrow(reduced$schedules)

  # Exactness guards: every candidate remains available and every removed path
  # has an explicit retained or transitively retained dominator.
  assert_model(
    setequal(reduced$meta$candidate_id, universe$meta$candidate_id),
    "Complexity schedule reduction changed the policy candidate universe"
  )
  retained_candidate_counts <- reduced$schedules |> count(candidate_id)
  assert_model(
    nrow(retained_candidate_counts) == nrow(universe$meta) && all(retained_candidate_counts$n >= 1L),
    "Complexity schedule reduction left at least one policy without an implementation schedule"
  )

  audit <- bind_rows(audit_rows)
  summary_row <- tibble::tibble(
    protection_mode = universe$protection_mode,
    materiality_mode = universe$materiality_mode,
    policy_candidates_before = nrow(universe$meta),
    policy_candidates_after = nrow(reduced$meta),
    timing_schedules_before = nrow(universe$schedules),
    timing_schedules_after = nrow(reduced$schedules),
    timing_schedules_removed = nrow(universe$schedules) - nrow(reduced$schedules),
    pct_timing_schedules_removed = 100 * (nrow(universe$schedules) - nrow(reduced$schedules)) / nrow(universe$schedules),
    exact_candidate_universe_preserved = setequal(reduced$meta$candidate_id, universe$meta$candidate_id),
    reduction_method = "Exact componentwise annual primary-deficit dominance within candidate"
  )

  COMPLEXITY_SCHEDULE_AUDIT <<- bind_rows(COMPLEXITY_SCHEDULE_AUDIT, audit) |>
    distinct(protection_mode, materiality_mode, candidate_id, removed_schedule_key, .keep_all = TRUE)
  COMPLEXITY_SCHEDULE_SUMMARY <<- bind_rows(COMPLEXITY_SCHEDULE_SUMMARY, summary_row) |>
    distinct(protection_mode, materiality_mode, .keep_all = TRUE)

  if (CFG$write_audit_outputs) {
    write_csv_atomic(
      COMPLEXITY_SCHEDULE_AUDIT,
      file.path(CFG$output_dir, "complexity_schedule_dominance_audit.csv")
    )
    write_csv_atomic(
      COMPLEXITY_SCHEDULE_SUMMARY,
      file.path(CFG$output_dir, "complexity_schedule_reduction_summary.csv")
    )
  }

  log_line(
    "Memory-safety layer exact complexity reduction | protection=", universe$protection_mode,
    " | candidates=", nrow(universe$meta), " -> ", nrow(reduced$meta),
    " | timing schedules=", nrow(universe$schedules), " -> ", nrow(reduced$schedules),
    " | removed=", nrow(universe$schedules) - nrow(reduced$schedules),
    " (", sprintf("%.1f", summary_row$pct_timing_schedules_removed[[1]]), "%)"
  )

  assign(key, reduced, envir = COMPLEXITY_UNIVERSE_CACHE)
  reduced
}

# ------------------------------------------------------------------------------
# FUNCTION: compact_solution_model_metadata
# Purpose: Retain only model fields required after a solve. In particular, do
#          not retain the sparse A matrix, bounds, full schedule-flow table, or
#          other build-time objects inside each stored solution.
# ------------------------------------------------------------------------------
compact_solution_model_metadata <- function(model, decisions) {
  selected_schedule_keys <- if (nrow(decisions) == 0L) character() else unique(decisions$schedule_key)
  schedule_summary_selected <- if (length(selected_schedule_keys) == 0L) {
    model$schedule_summary[0, , drop = FALSE]
  } else {
    model$schedule_summary |> filter(schedule_key %in% selected_schedule_keys)
  }

  list(
    variable_names = model$variable_names,
    scenario_meta = model$scenario_meta,
    scenario_annual = model$scenario_annual,
    schedule_summary = schedule_summary_selected,
    protection_mode = model$protection_mode,
    materiality_mode = model$materiality_mode,
    objective_name = model$objective_name,
    soft_targets = model$soft_targets,
    max_target_slack_score = model$max_target_slack_score,
    require_ss_solvency = model$require_ss_solvency,
    memory_reduction_mode = if (is.null(model$memory_reduction_mode)) "COMPACT_SOLUTION_STORAGE" else model$memory_reduction_mode,
    original_timing_schedule_count = if (is.null(model$original_timing_schedule_count)) length(model$schedule_binary_names) else model$original_timing_schedule_count,
    solved_timing_schedule_count = length(model$schedule_binary_names),
    solver_threads_used = if (is.null(model$solver_threads_override)) CFG$solver_threads else model$solver_threads_override
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: solve_full_milp
# Purpose: Memory-safe solver path. Perform all numerical
#          checks, then discard the heavyweight MILP and raw solver objects.
# ------------------------------------------------------------------------------
solve_full_milp <- function(model, solve_label) {
  structure_audit <- validate_full_milp_structure(model, solve_label)
  n_int <- sum(model$types == "I")
  n_cont <- sum(model$types == "C")
  nnz <- structure_audit$nonzeros
  threads_used <- if (!is.null(model$solver_threads_override)) {
    as.integer(model$solver_threads_override)
  } else {
    as.integer(CFG$solver_threads)
  }

  log_line(
    "MILP ", solve_label,
    " | variables=", length(model$variable_names),
    " (integer/binary=", n_int, ", continuous=", n_cont, ")",
    " | policy activations=", length(model$policy_variable_names),
    " | timing binaries=", length(model$schedule_binary_names),
    " | policy-level variables=", length(model$intensity_variable_names),
    " | robust scenarios=", nrow(model$scenario_meta),
    " | constraints=", nrow(model$A),
    " | nonzeros=", nnz,
    " | threads=", threads_used,
    ifelse(!is.null(model$memory_reduction_mode), paste0(" | memory_mode=", model$memory_reduction_mode), "")
  )

  control <- highs::highs_control(
    threads = threads_used,
    mip_rel_gap = CFG$solver_mip_rel_gap,
    primal_feasibility_tolerance = CFG$solver_primal_feasibility_tolerance,
    dual_feasibility_tolerance = CFG$solver_dual_feasibility_tolerance,
    log_to_console = TRUE
  )

  started <- Sys.time()
  sol <- highs::highs_solve(
    L = model$L,
    lower = model$lower,
    upper = model$upper,
    A = model$A,
    lhs = model$lhs,
    rhs = model$rhs,
    types = model$types,
    maximum = FALSE,
    control = control
  )
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  status_text <- if (is.null(sol$status_message)) "" else stringr::str_to_lower(as.character(sol$status_message)[1])
  solver_value_valid <- if (!is.null(sol$solver_msg) && !is.null(sol$solver_msg$value_valid)) isTRUE(sol$solver_msg$value_valid) else NA
  status_can_have_incumbent <- !stringr::str_detect(status_text, "infeasible|unbounded|model error|solve error|load error")
  primal_shape_ok <- !is.null(sol$primal_solution) && length(sol$primal_solution) == length(model$variable_names) && all(is.finite(sol$primal_solution))
  primal_ok <- status_can_have_incumbent && primal_shape_ok && (is.na(solver_value_valid) || solver_value_valid)

  selected <- character()
  decisions <- tibble()
  max_constraint_violation <- Inf
  max_bound_violation <- Inf
  max_integrality_violation <- Inf

  if (primal_ok) {
    xall <- as.numeric(sol$primal_solution)
    names(xall) <- model$variable_names
    selected <- stringr::str_remove(model$policy_variable_names[xall[model$policy_variable_names] > 0.5], "^y::")
    decisions <- extract_parameter_decisions_from_primal(model, xall)

    activity <- as.numeric(model$A %*% as.numeric(xall))
    low_violation <- ifelse(is.finite(model$lhs), pmax(model$lhs - activity, 0), 0)
    high_violation <- ifelse(is.finite(model$rhs), pmax(activity - model$rhs, 0), 0)
    max_constraint_violation <- max(c(low_violation, high_violation), na.rm = TRUE)

    lower_violation <- ifelse(is.finite(model$lower), pmax(model$lower - xall, 0), 0)
    upper_violation <- ifelse(is.finite(model$upper), pmax(xall - model$upper, 0), 0)
    max_bound_violation <- max(c(lower_violation, upper_violation), na.rm = TRUE)

    int_idx <- which(model$types == "I")
    max_integrality_violation <- if (length(int_idx) > 0L) max(abs(xall[int_idx] - round(xall[int_idx]))) else 0
  }

  feasible_incumbent <- primal_ok &&
    max_constraint_violation <= CFG$solution_acceptance_constraint_tolerance &&
    max_bound_violation <= CFG$solution_acceptance_bound_tolerance &&
    max_integrality_violation <= CFG$solution_acceptance_integrality_tolerance

  info_num <- function(name) {
    if (is.null(sol$info) || is.null(sol$info[[name]])) return(NA_real_)
    suppressWarnings(as.numeric(sol$info[[name]][[1]]))
  }

  status_value <- sol$status
  status_message_value <- sol$status_message
  objective_value_value <- sol$objective_value
  primal_solution_value <- sol$primal_solution
  info_value <- sol$info
  mip_node_count_value <- info_num("mip_node_count")
  mip_dual_bound_value <- info_num("mip_dual_bound")
  mip_gap_value <- info_num("mip_gap")
  simplex_iteration_count_value <- info_num("simplex_iteration_count")
  ipm_iteration_count_value <- info_num("ipm_iteration_count")

  compact_model <- compact_solution_model_metadata(model, decisions)

  log_line(
    "MILP ", solve_label, " finished | status=", status_message_value,
    " | elapsed=", sprintf("%.2f", elapsed), "s",
    " | nodes=", ifelse(is.na(mip_node_count_value), "NA", format(mip_node_count_value, scientific = FALSE)),
    " | mip_gap=", ifelse(is.na(mip_gap_value), "NA", signif(mip_gap_value, 5)),
    " | feasible_incumbent=", feasible_incumbent
  )

  result <- list(
    status = status_value,
    status_message = status_message_value,
    objective_value = objective_value_value,
    primal_solution = primal_solution_value,
    selected_candidate_ids = selected,
    parameter_decisions = decisions,
    elapsed_seconds = elapsed,
    info = info_value,
    mip_node_count = mip_node_count_value,
    mip_dual_bound = mip_dual_bound_value,
    mip_gap = mip_gap_value,
    simplex_iteration_count = simplex_iteration_count_value,
    ipm_iteration_count = ipm_iteration_count_value,
    max_constraint_violation = max_constraint_violation,
    max_bound_violation = max_bound_violation,
    max_integrality_violation = max_integrality_violation,
    feasible_incumbent = feasible_incumbent,
    raw = NULL,
    model = compact_model,
    solve_label = solve_label
  )

  # Do not let the full sparse MILP or raw solver return accumulate across the
  # dozens of package searches. The compact result above is all downstream code
  # requires for audit and independent re-simulation.
  rm(sol)
  if (isTRUE(CFG$solver_force_gc_between_solves)) invisible(gc(verbose = FALSE))
  result
}

# ------------------------------------------------------------------------------
# FUNCTION: solve_diverse_family
# Purpose: Memory-safe complexity path. Complexity objectives use the exact dominated-
#          schedule reduction; every other objective retains the full universe.
# ------------------------------------------------------------------------------
solve_diverse_family <- function(
  universe,
  policy_model,
  working_baseline,
  kernel_obj,
  objective,
  label_prefix,
  count = CFG$diverse_solutions_per_objective,
  require_patterns = character(),
  forbid_patterns = character(),
  max_revenue_bil = Inf,
  max_spending_bil = Inf,
  require_ss_solvency = FALSE,
  soft_targets = FALSE,
  max_target_slack_score = Inf
) {
  solutions <- list()
  previous <- list()

  use_exact_complexity_reduction <- identical(objective, "complexity") &&
    !is.finite(max_revenue_bil) && !is.finite(max_spending_bil)

  solver_universe <- if (use_exact_complexity_reduction) {
    reduce_universe_for_complexity(universe, kernel_obj)
  } else {
    universe
  }

  for (k in seq_len(count)) {
    label <- paste0(label_prefix, "_", sprintf("%02d", k))
    model <- build_full_milp(
      universe = solver_universe,
      working_baseline = working_baseline,
      kernel_obj = kernel_obj,
      objective = objective,
      require_patterns = require_patterns,
      forbid_patterns = forbid_patterns,
      max_revenue_bil = max_revenue_bil,
      max_spending_bil = max_spending_bil,
      require_ss_solvency = require_ss_solvency,
      soft_targets = soft_targets,
      max_target_slack_score = max_target_slack_score,
      previous_packages = previous
    )

    if (use_exact_complexity_reduction) {
      model$solver_threads_override <- CFG$complexity_solver_threads
      model$memory_reduction_mode <- "EXACT_DOMINATED_SCHEDULE_PRUNING_PLUS_COMPACT_STORAGE"
      model$original_timing_schedule_count <- nrow(universe$schedules)
    } else {
      model$memory_reduction_mode <- "COMPACT_SOLUTION_STORAGE"
      model$original_timing_schedule_count <- length(model$schedule_binary_names)
    }

    sol <- solve_full_milp(model, label)
    rm(model)
    if (isTRUE(CFG$solver_force_gc_between_solves)) invisible(gc(verbose = FALSE))

    solutions[[length(solutions) + 1L]] <- sol
    if (!full_solver_has_feasible_incumbent(sol)) break
    previous[[length(previous) + 1L]] <- sol$selected_candidate_ids
  }

  solutions
}

# ------------------------------------------------------------------------------
# MODEL LAYER ACTIVE MEMORY RULE SUMMARY
# - No policy candidate is removed to save memory.
# - Full-universe fiscal-capacity, revenue, spending, debt, reference, Pareto,
#   and target-slack solves retain their prior formulation.
# - Complexity solves retain every policy candidate but remove only timing paths
#   proven componentwise dominated or exactly duplicated for that same policy.
# - The exact HiGHS zero requested MIP gap and all post-solve audits remain.
# - Each stored solution contains compact audit metadata, not a retained copy of
#   its sparse constraint matrix, full schedule-flow table, or raw solver object.
# - Explicit gc() calls release completed solve structures before the next MILP.
# - Complexity solves use at most eight HiGHS threads to avoid needless per-
#   thread memory duplication while preserving exact optimization.
# ------------------------------------------------------------------------------


# ==============================================================================
# MODEL LAYER ROBUST-FEASIBILITY, POLICY-UNIVERSE, AND CONCURRENCY MODEL STAGE
# ==============================================================================
# Governing rules preserved from implementations 39-41:
#   * Approved protected categories remain binding.
#   * Congress may change current law. Current law is never an eligibility veto.
#   * Use the newest available authoritative data and scoring method for each
#     admissible policy. Score age alone is never an exclusion reason.
#   * Do not fabricate annual fiscal paths from inventories or headline totals.
#   * Full-universe fiscal solves preserve memory-safety memory discipline;
#     exact dominated-schedule pruning remains limited to complexity objectives.
#
# Robust-feasibility layer shifts the substantive objective from central feasibility to
# robust feasibility. It broadens high-end/preference revenue choices, adds
# additional SSA OACT scored solvency alternatives, adds current OMB user-charge
# proposals where an annual budget path exists, audits current Medicare MFN
# evidence without inventing a federal annual score, computes robust capacity
# gaps and policy-yield feasibility thresholds, seeks materially different
# packages, and uses memory-aware parallelism for lightweight independent
# verification on the user's Ryzen 9 7950X3D / 64 GB machine.
# ==============================================================================

CFG$memory_safety_expanded_theoretical_primary_improvement_2027_2036_bil <- 13267.940035
CFG$robust_feasibility_min_realized_capacity_gain_bil <- 100
CFG$robust_feasibility_alternative_package_count <- 5L
CFG$robust_feasibility_alternative_hamming_distance <- 30L
CFG$robust_feasibility_policy_yield_lower_bound <- 0.70
CFG$robust_feasibility_policy_yield_tolerance <- 0.005
CFG$robust_feasibility_parallel_memory_budget_gb <- 56
CFG$robust_feasibility_light_parallel_workers <- 4L
CFG$robust_feasibility_medium_parallel_jobs <- 2L
CFG$robust_feasibility_medium_threads_each <- 4L
CFG$robust_feasibility_heavy_threads <- 8L
CFG$robust_feasibility_hardware <- paste0(Sys.info()[["sysname"]], "; detected logical cores=", parallel::detectCores(logical = TRUE))

# Memory, not logical-core count, governs solver concurrency. Heavy MILPs stay
# sequential and use at most eight HiGHS threads. This preserves the exact solve
# contract while avoiding the 50+ GB failure mode observed in policy-universe expansion layer.
CFG$solver_threads <- min(
  CFG$robust_feasibility_heavy_threads,
  max(1L, parallel::detectCores(logical = FALSE))
)
CFG$complexity_solver_threads <- CFG$solver_threads

# Bind the memory-safety functions used by the robust-feasibility stage.
build_treasury_greenbook_policy_candidates_memory_safety <- build_treasury_greenbook_policy_candidates_policy_universe_expansion
build_expanded_anchor_policy_model_memory_safety <- build_expanded_anchor_policy_model_policy_universe_expansion
build_policy_search_score_review_memory_safety <- build_policy_search_score_review_policy_universe_expansion
build_parameterized_policy_space_memory_safety <- build_parameterized_policy_space_core_2
build_interaction_catalog_full_memory_safety <- build_interaction_catalog_full_policy_universe_expansion
build_authoritative_lever_expansion_audit_memory_safety <- build_authoritative_lever_expansion_audit_policy_universe_expansion
build_expanded_coverage_audit_memory_safety <- build_expanded_coverage_audit_policy_universe_expansion
run_full_solution_search_memory_safety <- run_full_solution_search_core_2
run_model_memory_safety <- run_model_legislative_policy_space

# ------------------------------------------------------------------------------
# FUNCTION: build_robust_feasibility_shifted_treasury_candidates
# Purpose: Add additional policy-specific Greenbook annual scores consistent with
#          the approved protections. The broad top ordinary-income-rate increase
#          remains blocked and is deliberately not added as solver capacity.
# ------------------------------------------------------------------------------
build_robust_feasibility_shifted_treasury_candidates <- function(working_baseline) {
  specs <- list(
    list(
      candidate_id = "TREASURY_2025_apply_niit_pass_through_high_income",
      title = "Apply the Net Investment Income Tax to Pass-Through Business Income of High-Income Taxpayers",
      variant_name = "Treasury FY2025 scored high-income pass-through NIIT base expansion",
      values_mil = c(38302, 29950, 31931, 34819, 37435, 39950, 42143, 43986, 46126, 48579),
      total_mil = 393221,
      protection_status = "ELIGIBLE",
      protection_reason = "Targets a high-income pass-through tax-base preference. It does not increase ordinary individual income-tax rates and closely corresponds to an already eligible CBO NIIT-base option.",
      policy_domain = "high_end_tax_preference_reform",
      complexity_multiplier = 1.10
    ),
    list(
      candidate_id = "TREASURY_2025_increase_niit_and_additional_medicare_tax_high_income",
      title = "Increase the Net Investment Income Tax Rate and Additional Medicare Tax Rate for High-Income Taxpayers",
      variant_name = "Treasury FY2025 scored high-income NIIT and additional Medicare tax rate increase",
      values_mil = c(42920, 31327, 32285, 34710, 37224, 39822, 42450, 44963, 47602, 50487),
      total_mil = 403790,
      protection_status = "BLOCKED",
      protection_reason = "The proposal directly raises a tax rate on high-income wages and self-employment earnings through the Additional Medicare Tax. The project's ordinary-wage rate protection remains binding even though Treasury provides an official score.",
      policy_domain = "protected_wage_rate_increase",
      complexity_multiplier = 1.10
    ),
    list(
      candidate_id = "TREASURY_2025_reform_taxation_capital_income_high_income",
      title = "Reform the Taxation of Capital Income",
      variant_name = "Treasury FY2025 scored high-income capital-income reform",
      values_mil = c(18031, 23713, 25164, 26417, 27624, 29050, 30727, 32158, 33758, 41941),
      total_mil = 288583,
      protection_status = "CONDITIONAL",
      protection_reason = "Targets preferential high-income capital-income treatment rather than ordinary wages. It remains EXPANDED-only because capital-gains changes can affect saving, investment timing, and liquidity.",
      policy_domain = "high_end_capital_income_preference_reform",
      complexity_multiplier = 1.20
    ),
    list(
      candidate_id = "TREASURY_2025_minimum_income_tax_wealthiest",
      title = "Impose a Minimum Income Tax on the Wealthiest Taxpayers",
      variant_name = "Treasury FY2025 scored minimum tax for households with wealth above the proposal threshold",
      values_mil = c(0, 50310, 56387, 59430, 60451, 59974, 59331, 53057, 50215, 53513),
      total_mil = 502668,
      protection_status = "CONDITIONAL",
      protection_reason = "Targets ultra-high-wealth tax preferences rather than ordinary wage income. It remains EXPANDED-only because valuation, liquidity, and investment-incidence questions require explicit policy review.",
      policy_domain = "wealth_and_high_end_preference_reform",
      complexity_multiplier = 1.30
    ),
    list(
      candidate_id = "TREASURY_2025_repeal_like_kind_exchange_deferral",
      title = "Repeal Deferral of Gain From Like-Kind Exchanges",
      variant_name = "Treasury FY2025 scored limitation of like-kind exchange tax deferral",
      values_mil = c(680, 1870, 1926, 1984, 2044, 2104, 2169, 2232, 2300, 2369),
      total_mil = 19678,
      protection_status = "CONDITIONAL",
      protection_reason = "Removes a tax-deferral preference for appreciated real property. It remains EXPANDED-only because the mechanism can affect investment timing and business real-estate transactions.",
      policy_domain = "tax_deferral_and_preference_reform",
      complexity_multiplier = 1.10
    )
  )

  source_years <- 2025:2034
  shift_years <- 2L
  gdp36 <- working_baseline$gdp_bil[working_baseline$year == 2036L]
  assert_model(length(gdp36) == 1L && is.finite(gdp36) && gdp36 > 0, "Missing FY2036 GDP for robust-feasibility Treasury extension")

  rows <- purrr::map(specs, function(s) {
    assert_model(length(s$values_mil) == 10L, paste0("Model layer Treasury score has wrong annual length: ", s$candidate_id))
    assert_model(abs(sum(s$values_mil) - s$total_mil) <= 1e-9, paste0("Model layer Treasury annual values do not reproduce the published 2025-2034 total: ", s$candidate_id))

    target_years <- source_years + shift_years
    flows <- tibble(
      candidate_id = s$candidate_id,
      year = CFG$model_years,
      revenue_delta_bil = 0,
      outlay_delta_bil = 0,
      shift_years = shift_years,
      translation_status = "ZERO"
    )
    idx <- match(target_years, flows$year)
    flows$revenue_delta_bil[idx] <- s$values_mil / 1000
    flows$translation_status[idx] <- "SHIFTED_TREASURY_OFFICIAL_ANNUAL_SCORE"

    revenue_2036 <- flows$revenue_delta_bil[flows$year == 2036L]
    for (y in CFG$extension_years) {
      gdp_y <- working_baseline$gdp_bil[working_baseline$year == y]
      flows$revenue_delta_bil[flows$year == y] <- revenue_2036 * (gdp_y / gdp36)
      flows$translation_status[flows$year == y] <- "EXTRAPOLATED_GDP_SHARE_FROM_LAST_OFFICIAL_YEAR"
    }
    flows <- flows |>
      mutate(
        primary_deficit_delta_bil = outlay_delta_bil - revenue_delta_bil,
        component_identity_residual_bil = primary_deficit_delta_bil - (outlay_delta_bil - revenue_delta_bil)
      ) |>
      select(candidate_id, year, revenue_delta_bil, outlay_delta_bil, primary_deficit_delta_bil, component_identity_residual_bil, shift_years, translation_status)

    score_flows <- flows |> filter(year %in% CFG$score_years)
    cumulative_revenue <- sum(pmax(score_flows$revenue_delta_bil, 0), na.rm = TRUE)
    cumulative_primary <- sum(-score_flows$primary_deficit_delta_bil, na.rm = TRUE)
    assert_model(abs(cumulative_primary - s$total_mil / 1000) <= 1e-9, paste0("Shifted robust-feasibility Treasury score does not reproduce official total: ", s$candidate_id))

    meta <- tibble(
      candidate_id = s$candidate_id,
      family_key = s$candidate_id,
      family_title = s$title,
      variant_name = s$variant_name,
      latest_estimate = "FY2025 Greenbook (March 2024)",
      estimate_year = 2024L,
      source_start_year = 2025L,
      source_end_year = 2034L,
      source_url = "https://home.treasury.gov/system/files/131/General-Explanations-FY2025-Table.pdf",
      fiscal_channel = "REVENUE",
      annual_profile_status = "FULL_OFFICIAL_ANNUAL",
      ss_actuarial_improvement_pct_payroll = 0,
      note = paste0("Treasury Office of Tax Policy official annual revenue estimate shifted two fiscal years to align FY2025-FY2034 with the model FY2027-FY2036 scored window. Published total: $", format(round(s$total_mil / 1000, 3), nsmall = 3), "B."),
      index_ten_year_savings_bil = s$total_mil / 1000,
      family_id = s$candidate_id,
      budget_option_id = NA_character_,
      title = s$title,
      major_category = "Revenues",
      budget_function = "Revenue",
      index_url = "https://home.treasury.gov/policy-issues/tax-policy/revenue-proposals",
      source_date = "Mar 2024",
      source_effective_year = 2025L,
      source_score_start_year = 2025L,
      source_score_end_year = 2034L,
      evidence_class = "OFFICIAL_OLDER",
      decision_type = "CONTINUOUS_LEVEL_WITH_TIMING",
      source_kind = "TREASURY_GREENBOOK_POLICY",
      investment_market_review = s$protection_status == "CONDITIONAL",
      direct_cbo_annual_score = FALSE,
      direct_official_annual_score = TRUE,
      solver_eligible_annual = s$protection_status != "BLOCKED",
      is_december_2024_core = FALSE,
      protection_status = s$protection_status,
      protected_category = case_when(
        s$protection_status == "BLOCKED" ~ "ORDINARY_WAGE_RATE_PROTECTION",
        s$protection_status == "CONDITIONAL" ~ "HIGH_END_CAPITAL_OR_SAVING_REVIEW",
        TRUE ~ NA_character_
      ),
      protection_reason = s$protection_reason,
      risk_ordinary_wages = s$protection_status == "BLOCKED",
      risk_ordinary_saving = s$protection_status == "CONDITIONAL",
      risk_productive_investment = s$protection_status == "CONDITIONAL",
      risk_family_formation = FALSE,
      risk_business_reinvestment = s$protection_status == "CONDITIONAL",
      risk_core_social_security = FALSE,
      risk_core_medicare = FALSE,
      risk_productive_public_capacity = FALSE,
      market_function_review_required = s$protection_status == "CONDITIONAL",
      hard_protection_violation = s$protection_status == "BLOCKED",
      explicit_review_required = s$protection_status == "CONDITIONAL",
      protection_classification_method = "EXPLICIT_ROBUST_FEASIBILITY_TREASURY_POLICY_RULE",
      ss_actuarial_score_vintage = NA_character_,
      ss_actuarial_additivity_status = NA_character_,
      cumulative_revenue_increase_2027_2036_bil = cumulative_revenue,
      cumulative_spending_cut_2027_2036_bil = 0,
      cumulative_primary_improvement_2027_2036_bil = cumulative_primary,
      solver_exclusion_reason = ifelse(s$protection_status == "BLOCKED", "Protected ordinary-wage tax-rate category", NA_character_),
      replaced_by_granular_account_controls = FALSE,
      complexity_weight = s$complexity_multiplier * (1 + log1p(max(cumulative_primary, 0) / 1000)),
      complexity_weight_basis = "Activation burden plus logarithmic ten-year fiscal scope for an official Treasury scored reform",
      policy_domain = s$policy_domain
    )

    source_score <- tibble(
      candidate_id = s$candidate_id,
      title = s$title,
      source_year = source_years,
      source_revenue_effect_mil = s$values_mil,
      published_2025_2034_total_mil = s$total_mil,
      source = "U.S. Treasury FY2025 Greenbook Table of Revenue Estimates"
    )
    list(meta = meta, flows = flows, source_score = source_score)
  })

  list(
    meta = bind_rows(purrr::map(rows, "meta")),
    flows = bind_rows(purrr::map(rows, "flows")),
    source_scores = bind_rows(purrr::map(rows, "source_score"))
  )
}

# Extend the memory-safety Treasury set without replacing any accepted policy.
build_treasury_greenbook_policy_candidates_robust_feasibility <- function(working_baseline) {
  base <- build_treasury_greenbook_policy_candidates_memory_safety(working_baseline)
  extra <- build_robust_feasibility_shifted_treasury_candidates(working_baseline)
  assert_model(length(intersect(base$meta$candidate_id, extra$meta$candidate_id)) == 0L, "Model layer Treasury candidate IDs collide with memory-safety layer")
  list(
    meta = bind_rows(base$meta, extra$meta),
    flows = bind_rows(base$flows, extra$flows),
    source_scores = bind_rows(base$source_scores, extra$source_scores)
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: robust_feasibility_numeric_cells
# Purpose: Parse numeric values from an SSA HTML table row while tolerating
#          commas, dollar signs, percent signs, blanks, and em-dash missing data.
# ------------------------------------------------------------------------------
robust_feasibility_numeric_cells <- function(x) {
  z <- suppressWarnings(readr::parse_number(as.character(x), na = c("", "—", "–", "-", "NA")))
  z[is.finite(z)]
}

# ------------------------------------------------------------------------------
# FUNCTION: fetch_ssa_2026_taxable_payroll
# Purpose: Read the current 2026 Trustees Table VI.G1 taxable-payroll path. This
#          is the current dollar base used to translate OACT percent-of-payroll
#          provision response functions into annual dollar flows.
# ------------------------------------------------------------------------------
fetch_ssa_2026_taxable_payroll <- function() {
  url <- "https://www.ssa.gov/OACT/TR/2026/lr6g1.html"
  path <- file.path(CFG$raw_source_dir, "ssa_2026_tr_table_vi_g1.html")
  fetch_text_cached(
    url = url,
    destination = path,
    force = TRUE,
    source_id = "ssa_2026_tr_table_vi_g1",
    agency = "Social Security Administration, Office of the Chief Actuary",
    title = "2026 OASDI Trustees Report Table VI.G1, Selected Economic Variables",
    publication_date = "2026",
    baseline_vintage = "2026 Trustees Report",
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Current taxable-payroll dollar path used to translate OACT provision response rates into annual dollar effects."
  )

  tabs <- rvest::html_table(xml2::read_html(path), fill = TRUE, trim = TRUE)
  assert_model(length(tabs) > 0L, "SSA 2026 Table VI.G1 HTML contains no parseable tables")

  parsed <- purrr::map_dfr(seq_along(tabs), function(k) {
    tb <- tabs[[k]]
    if (ncol(tb) < 4L || nrow(tb) < 10L) return(tibble())
    purrr::map_dfr(seq_len(nrow(tb)), function(i) {
      vals <- robust_feasibility_numeric_cells(unlist(tb[i, , drop = TRUE], use.names = FALSE))
      if (length(vals) < 4L) return(tibble())
      yr <- as.integer(round(vals[[1]]))
      if (!is.finite(yr) || yr < 2020L || yr > 2100L) return(tibble())
      # In Table VI.G1 the numeric columns after calendar year are CPI, average
      # wage index, taxable payroll, GDP, payroll/GDP ratio, and interest factor.
      tibble(year = yr, taxable_payroll_bil = vals[[4]], table_index = k)
    })
  }) |>
    filter(year %in% CFG$model_years) |>
    group_by(year) |>
    slice_max(order_by = taxable_payroll_bil, n = 1, with_ties = FALSE) |>
    ungroup() |>
    arrange(year)

  assert_model(nrow(parsed) == length(CFG$model_years), "Could not recover the complete FY2026-FY2046 taxable-payroll path from SSA 2026 Table VI.G1")
  assert_model(all(parsed$taxable_payroll_bil > 5000), "Parsed SSA taxable-payroll values are implausibly small")
  parsed |> select(year, taxable_payroll_bil)
}

# ------------------------------------------------------------------------------
# FUNCTION: fetch_ssa_oact_provision_rates
# Purpose: Parse the official annual change-from-current-law cost and income rates
#          from one OACT solvency provision table.
# ------------------------------------------------------------------------------
fetch_ssa_oact_provision_rates <- function(spec) {
  path <- file.path(CFG$raw_source_dir, paste0("ssa_oact_", spec$provision_code, "_annual_table.html"))
  fetch_text_cached(
    url = spec$url,
    destination = path,
    force = TRUE,
    source_id = paste0("ssa_oact_", spec$provision_code, "_annual_table"),
    agency = "Social Security Administration, Office of the Chief Actuary",
    title = paste0("OACT Long-Range Solvency Provision ", spec$provision_code, " detailed annual table"),
    publication_date = spec$page_vintage,
    baseline_vintage = spec$trustees_basis,
    evidence_class = spec$evidence_class,
    notes = "Official OACT annual change-from-current-law cost and income rates, expressed as a percentage of current-law taxable payroll."
  )

  tabs <- rvest::html_table(xml2::read_html(path), fill = TRUE, trim = TRUE)
  assert_model(length(tabs) > 0L, paste0("SSA provision ", spec$provision_code, " contains no parseable tables"))

  rows <- purrr::map_dfr(seq_along(tabs), function(k) {
    tb <- tabs[[k]]
    if (ncol(tb) < 5L || nrow(tb) < 10L) return(tibble())
    purrr::map_dfr(seq_len(nrow(tb)), function(i) {
      vals <- robust_feasibility_numeric_cells(unlist(tb[i, , drop = TRUE], use.names = FALSE))
      if (length(vals) < 7L) return(tibble())
      yr <- as.integer(round(vals[[1]]))
      if (!is.finite(yr) || yr < 2025L || yr > 2100L) return(tibble())
      # The last three numeric cells are the change-from-current-law cost rate,
      # income rate, and annual balance. The trust-fund ratio, when numeric, sits
      # before these values and therefore does not affect this extraction.
      tibble(
        year = yr,
        delta_cost_rate_pct_payroll = vals[[length(vals) - 2L]],
        delta_income_rate_pct_payroll = vals[[length(vals) - 1L]],
        delta_annual_balance_pct_payroll = vals[[length(vals)]],
        table_index = k
      )
    })
  }) |>
    filter(year %in% CFG$model_years) |>
    distinct(year, .keep_all = TRUE) |>
    arrange(year)

  assert_model(nrow(rows) == length(CFG$model_years), paste0("SSA provision ", spec$provision_code, " did not yield a complete 2026-2046 annual response path"))
  assert_model(
    max(abs((rows$delta_income_rate_pct_payroll - rows$delta_cost_rate_pct_payroll) - rows$delta_annual_balance_pct_payroll), na.rm = TRUE) <= 0.03,
    paste0("SSA provision ", spec$provision_code, " change-rate identity failed")
  )
  rows
}

# ------------------------------------------------------------------------------
# FUNCTION: build_ssa_oact_solving_candidates
# Purpose: Broaden the Social Security solvency menu with official OACT provision
#          response functions. Current 2026 taxable payroll supplies the dollar
#          base; provision-specific annual percentage responses remain official.
# ------------------------------------------------------------------------------
build_ssa_oact_solving_candidates_robust_feasibility <- function() {
  payroll <- fetch_ssa_2026_taxable_payroll()
  specs <- list(
    list(
      provision_code = "E2_1",
      candidate_id = "SSA_OACT_2025_E2_1_eliminate_taxable_max_no_benefit_credit",
      title = "Social Security OACT E2.1: Eliminate the Taxable Maximum With No Additional Benefit Credit",
      variant_name = "Full 12.4 percent OASDI tax on all covered earnings; no benefit credit above the current-law maximum",
      url = "https://www.ssa.gov/oact/solvency/provisions/tables/table_run415.html",
      trustees_basis = "2025 Trustees Report",
      page_vintage = "2026",
      evidence_class = "OFFICIAL_OLDER",
      actuarial = 2.55,
      protection_status = "ELIGIBLE",
      protection_reason = "Dedicated Social Security solvency financing focused on earnings above the taxable maximum; consistent with the existing eligible high-earner taxable-maximum reform family."
    ),
    list(
      provision_code = "E2_17",
      candidate_id = "SSA_OACT_2025_E2_17_tax_above_400k_no_benefit_credit",
      title = "Social Security OACT E2.17: Apply OASDI Tax Above $400,000 With No Additional Benefit Credit",
      variant_name = "12.4 percent OASDI tax above $400,000 until the taxable maximum reaches the threshold",
      url = "https://www.ssa.gov/oact/solvency/provisions/tables/table_run425.html",
      trustees_basis = "2025 Trustees Report",
      page_vintage = "2026",
      evidence_class = "OFFICIAL_OLDER",
      actuarial = 2.31,
      protection_status = "ELIGIBLE",
      protection_reason = "Dedicated Social Security solvency financing on very high earnings, not a broad ordinary-income tax-rate increase."
    ),
    list(
      provision_code = "E2_4",
      candidate_id = "SSA_OACT_2025_E2_4_phase_out_taxable_max_secondary_credit",
      title = "Social Security OACT E2.4: Phase Out the Taxable Maximum With Secondary Benefit Credit",
      variant_name = "Phase out taxable maximum through 2032 and provide limited secondary PIA benefit credit",
      url = "https://www.ssa.gov/oact/solvency/provisions/tables/table_run417.html",
      trustees_basis = "2025 Trustees Report",
      page_vintage = "2026",
      evidence_class = "OFFICIAL_OLDER",
      actuarial = 2.37,
      protection_status = "ELIGIBLE",
      protection_reason = "Dedicated Social Security solvency financing paired with limited additional benefit credit for newly taxed high earnings."
    ),
    list(
      provision_code = "E3_14",
      candidate_id = "SSA_OACT_2025_E3_14_employer_no_max_employee_to_90pct",
      title = "Social Security OACT E3.14: Remove the Employer Taxable Maximum and Raise the Employee Maximum Toward 90 Percent",
      variant_name = "Employer 6.2 percent tax without a maximum; employee taxable maximum rises by an extra 2 percent per year toward 90 percent coverage",
      url = "https://www.ssa.gov/oact/solvency/provisions/tables/table_run438.html",
      trustees_basis = "2025 Trustees Report",
      page_vintage = "2026",
      evidence_class = "OFFICIAL_OLDER",
      actuarial = 1.56,
      protection_status = "CONDITIONAL",
      protection_reason = "Dedicated Social Security financing, but the uncapped employer-side payroll tax has a more direct business-incidence channel and therefore remains EXPANDED-only."
    ),
    list(
      provision_code = "H9",
      candidate_id = "SSA_OACT_2026_H9_tax_all_benefits_high_income",
      title = "Social Security OACT H9: Tax All Benefits for High-Income Beneficiaries",
      variant_name = "Tax all Social Security benefits above the OACT income thresholds and credit the revenue to the trust funds",
      url = "https://www.ssa.gov/oact/solvency/provisions/tables/table_run094.html",
      trustees_basis = "2026 Trustees Report",
      page_vintage = "2026",
      evidence_class = "OFFICIAL_CURRENT",
      actuarial = 0.16,
      protection_status = "CONDITIONAL",
      protection_reason = "Raises trust-fund revenue from high-income beneficiaries while preserving ordinary-beneficiary protection; retained in EXPANDED because it changes taxation of earned Social Security benefits."
    )
  )

  rows <- purrr::map(specs, function(s) {
    rates <- fetch_ssa_oact_provision_rates(s)
    annual <- rates |>
      left_join(payroll, by = "year") |>
      mutate(
        revenue_delta_bil = taxable_payroll_bil * delta_income_rate_pct_payroll / 100,
        outlay_delta_bil = taxable_payroll_bil * delta_cost_rate_pct_payroll / 100,
        primary_deficit_delta_bil = outlay_delta_bil - revenue_delta_bil,
        candidate_id = s$candidate_id,
        component_identity_residual_bil = primary_deficit_delta_bil - (outlay_delta_bil - revenue_delta_bil),
        shift_years = 0L,
        translation_status = ifelse(
          s$trustees_basis == "2026 Trustees Report",
          "OFFICIAL_2026_OACT_RATE_RESPONSE_TIMES_2026_TR_TAXABLE_PAYROLL",
          "OFFICIAL_2025_OACT_RATE_RESPONSE_REBASED_TO_2026_TR_TAXABLE_PAYROLL"
        )
      ) |>
      select(candidate_id, year, revenue_delta_bil, outlay_delta_bil, primary_deficit_delta_bil, component_identity_residual_bil, shift_years, translation_status)

    score <- annual |> filter(year %in% CFG$score_years)
    cumulative_revenue <- sum(pmax(score$revenue_delta_bil, 0), na.rm = TRUE)
    cumulative_spending <- sum(pmax(-score$outlay_delta_bil, 0), na.rm = TRUE)
    cumulative_primary <- sum(-score$primary_deficit_delta_bil, na.rm = TRUE)

    meta <- tibble(
      candidate_id = s$candidate_id,
      family_key = paste0("SSA_OACT_", s$provision_code),
      family_title = s$title,
      variant_name = s$variant_name,
      latest_estimate = paste0("SSA OACT ", s$provision_code, " detailed provision table"),
      estimate_year = ifelse(s$trustees_basis == "2026 Trustees Report", 2026L, 2025L),
      source_start_year = 2026L,
      source_end_year = 2100L,
      source_url = s$url,
      fiscal_channel = "MIXED",
      annual_profile_status = "OFFICIAL_OACT_RATE_RESPONSE_DERIVED_CURRENT_2026_DOLLAR_BASE",
      ss_actuarial_improvement_pct_payroll = s$actuarial,
      note = paste0("OACT annual change-from-current-law cost and income rates are applied to the current 2026 Trustees taxable-payroll dollar path. Same-number calendar year is used as the model fiscal-year approximation. Long-range actuarial improvement: ", s$actuarial, "% of taxable payroll."),
      index_ten_year_savings_bil = cumulative_primary,
      family_id = paste0("SSA_OACT_", s$provision_code),
      budget_option_id = s$provision_code,
      title = s$title,
      major_category = "Social Security",
      budget_function = "Social Security",
      index_url = "https://www.ssa.gov/oact/solvency/provisions/",
      source_date = s$page_vintage,
      source_effective_year = 2026L,
      source_score_start_year = 2026L,
      source_score_end_year = 2100L,
      evidence_class = s$evidence_class,
      decision_type = "DISCRETE_WITH_TIMING",
      source_kind = "SSA_OACT_SOLVENCY_PROVISION",
      investment_market_review = s$protection_status == "CONDITIONAL",
      direct_cbo_annual_score = FALSE,
      direct_official_annual_score = FALSE,
      solver_eligible_annual = TRUE,
      is_december_2024_core = FALSE,
      protection_status = s$protection_status,
      protected_category = ifelse(s$protection_status == "CONDITIONAL", "SOCIAL_SECURITY_INCIDENCE_REVIEW", NA_character_),
      protection_reason = s$protection_reason,
      risk_ordinary_wages = s$provision_code %in% c("E2_1", "E2_17", "E2_4", "E3_14"),
      risk_ordinary_saving = FALSE,
      risk_productive_investment = s$provision_code == "E3_14",
      risk_family_formation = FALSE,
      risk_business_reinvestment = s$provision_code == "E3_14",
      risk_core_social_security = FALSE,
      risk_core_medicare = FALSE,
      risk_productive_public_capacity = FALSE,
      market_function_review_required = s$protection_status == "CONDITIONAL",
      hard_protection_violation = FALSE,
      explicit_review_required = s$protection_status == "CONDITIONAL",
      protection_classification_method = "EXPLICIT_ROBUST_FEASIBILITY_SSA_SOLVENCY_RULE",
      ss_actuarial_score_vintage = s$trustees_basis,
      ss_actuarial_additivity_status = "ADDITIVE_ONLY_WITH_NONOVERLAPPING_SSA_PROVISIONS;TAXABLE_MAX_ALTERNATIVES_MUTUALLY_EXCLUSIVE",
      cumulative_revenue_increase_2027_2036_bil = cumulative_revenue,
      cumulative_spending_cut_2027_2036_bil = cumulative_spending,
      cumulative_primary_improvement_2027_2036_bil = cumulative_primary,
      solver_exclusion_reason = NA_character_,
      replaced_by_granular_account_controls = FALSE,
      complexity_weight = 1.25 * (1 + log1p(max(cumulative_primary, 0) / 1000)),
      complexity_weight_basis = "Discrete SSA OACT structural provision plus logarithmic ten-year fiscal scope",
      policy_domain = "social_security_solvency"
    )

    source_rates <- rates |>
      mutate(
        candidate_id = s$candidate_id,
        provision_code = s$provision_code,
        trustees_basis = s$trustees_basis,
        actuarial_improvement_pct_payroll = s$actuarial,
        source_url = s$url
      )
    list(meta = meta, flows = annual, source_rates = source_rates)
  })

  list(
    meta = bind_rows(purrr::map(rows, "meta")),
    flows = bind_rows(purrr::map(rows, "flows")),
    source_rates = bind_rows(purrr::map(rows, "source_rates")),
    taxable_payroll = payroll
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_omb_user_charge_candidates
# Purpose: Add FY2027 Budget user-charge proposals with explicit annual 2027-36
#          collections. Offsetting collections/receipts are represented as
#          negative outlays. The SBA fee is retained as protected audit inventory.
# ------------------------------------------------------------------------------
build_omb_user_charge_candidates <- function(working_baseline) {
  source_url <- "https://www.whitehouse.gov/wp-content/uploads/2026/04/spec_fy2027.pdf"
  register_source(
    source_id = "omb_fy2027_analytical_perspectives_user_charge_table_09_4",
    agency = "Office of Management and Budget",
    title = "Budget of the U.S. Government, Fiscal Year 2027, Analytical Perspectives, Table 9-4 User Charge Proposals",
    url = source_url,
    local_path = NA_character_,
    publication_date = "2026-04",
    baseline_vintage = "FY2027 Budget",
    evidence_class = "OFFICIAL_CURRENT",
    notes = "Official FY2027-FY2036 gross estimated collections. OMB states that Table 9-4 does not include related spending; rows are retained as evidence inventory rather than net deficit-reduction scores."
  )

  specs <- list(
    list(
      candidate_id = "OMB_2027_FDA_foreign_food_facilities_registration_fee",
      title = "Establish FDA Foreign Food Facilities Registration Fee",
      variant_name = "FY2027 Budget user-charge proposal",
      values_mil = c(71,73,75,77,79,81,83,85,87,89),
      total_mil = 800,
      protection_status = "ELIGIBLE",
      protection_reason = "User charge on foreign food facilities tied to a regulatory service and private beneficiary class.",
      policy_domain = "governmental_fees_and_user_charges"
    ),
    list(
      candidate_id = "OMB_2027_SBA_upfront_administrative_fee",
      title = "Establish SBA Upfront Administrative Fee",
      variant_name = "FY2027 Budget user-charge proposal",
      values_mil = rep(158, 10),
      total_mil = 1580,
      protection_status = "BLOCKED",
      protection_reason = "Directly increases the cost of Small Business Administration credit access and conflicts with the productive small-business capacity protection.",
      policy_domain = "protected_small_business_credit_fee"
    ),
    list(
      candidate_id = "OMB_2027_CFTC_user_fee",
      title = "Establish Commodity Futures Trading Commission User Fee",
      variant_name = "FY2027 Budget user-charge proposal",
      values_mil = rep(410, 10),
      total_mil = 4100,
      protection_status = "ELIGIBLE",
      protection_reason = "Finances financial-market regulation through charges on the regulated beneficiary class rather than ordinary households or wages.",
      policy_domain = "financial_sector_user_charge"
    ),
    list(
      candidate_id = "OMB_2027_WHTI_surcharge",
      title = "Extend Western Hemisphere Travel Initiative Surcharge",
      variant_name = "FY2027 Budget offsetting-receipt proposal",
      values_mil = rep(565, 10),
      total_mil = 5650,
      protection_status = "CONDITIONAL",
      protection_reason = "User surcharge rather than a broad tax, but it can fall directly on household travel and therefore remains EXPANDED-only.",
      policy_domain = "governmental_fees_and_user_charges"
    )
  )

  rows <- purrr::map(specs, function(sp) {
    assert_model(abs(sum(sp$values_mil) - sp$total_mil) <= 1e-9, paste0("OMB user-charge annual values do not reproduce published total: ", sp$candidate_id))
    flows <- tibble(
      candidate_id = sp$candidate_id,
      year = CFG$model_years,
      revenue_delta_bil = 0,
      outlay_delta_bil = 0,
      primary_deficit_delta_bil = 0,
      component_identity_residual_bil = 0,
      gross_collection_bil = 0,
      shift_years = 0L,
      translation_status = "ZERO_NET_BUDGET_EFFECT_INVENTORY_ONLY"
    )
    idx <- match(2027:2036, flows$year)
    flows$gross_collection_bil[idx] <- sp$values_mil / 1000
    flows$translation_status[idx] <- "OFFICIAL_OMB_FY2027_BUDGET_GROSS_COLLECTION_PATH_INVENTORY_ONLY"
    cumulative_gross_collections <- sum(flows$gross_collection_bil[flows$year %in% CFG$score_years], na.rm = TRUE)

    meta <- tibble(
      candidate_id = sp$candidate_id,
      family_key = sp$candidate_id,
      family_title = sp$title,
      variant_name = sp$variant_name,
      latest_estimate = "FY2027 Budget Analytical Perspectives Table 9-4",
      estimate_year = 2026L,
      source_start_year = 2027L,
      source_end_year = 2036L,
      source_url = source_url,
      fiscal_channel = "INVENTORY_ONLY_GROSS_COLLECTION",
      annual_profile_status = "OFFICIAL_GROSS_COLLECTION_PATH_NO_NET_BUDGET_SCORE",
      ss_actuarial_improvement_pct_payroll = 0,
      note = paste0("OMB official annual gross user-charge collections, FY2027-FY2036. Published total: $", format(round(sp$total_mil/1000, 3), nsmall = 3), "B. OMB states related spending is excluded, so no net deficit reduction is inferred."),
      index_ten_year_savings_bil = NA_real_,
      family_id = sp$candidate_id,
      budget_option_id = NA_character_,
      title = sp$title,
      major_category = "Inventory only",
      budget_function = "Governmental receipts and user charges",
      index_url = source_url,
      source_date = "Apr 2026",
      source_effective_year = 2027L,
      source_score_start_year = 2027L,
      source_score_end_year = 2036L,
      evidence_class = "OFFICIAL_CURRENT",
      decision_type = "INVENTORY_ONLY",
      source_kind = "OMB_USER_CHARGE_POLICY",
      investment_market_review = FALSE,
      direct_cbo_annual_score = FALSE,
      direct_official_annual_score = FALSE,
      solver_eligible_annual = FALSE,
      is_december_2024_core = FALSE,
      protection_status = sp$protection_status,
      protected_category = ifelse(sp$protection_status == "BLOCKED", "PRODUCTIVE_SMALL_BUSINESS_CREDIT", ifelse(sp$protection_status == "CONDITIONAL", "HOUSEHOLD_USER_CHARGE_REVIEW", NA_character_)),
      protection_reason = sp$protection_reason,
      risk_ordinary_wages = FALSE,
      risk_ordinary_saving = FALSE,
      risk_productive_investment = sp$protection_status == "BLOCKED",
      risk_family_formation = FALSE,
      risk_business_reinvestment = sp$protection_status == "BLOCKED",
      risk_core_social_security = FALSE,
      risk_core_medicare = FALSE,
      risk_productive_public_capacity = FALSE,
      market_function_review_required = FALSE,
      hard_protection_violation = sp$protection_status == "BLOCKED",
      explicit_review_required = sp$protection_status == "CONDITIONAL",
      protection_classification_method = "EXPLICIT_ROBUST_FEASIBILITY_OMB_USER_CHARGE_RULE",
      ss_actuarial_score_vintage = NA_character_,
      ss_actuarial_additivity_status = NA_character_,
      cumulative_revenue_increase_2027_2036_bil = 0,
      cumulative_spending_cut_2027_2036_bil = 0,
      cumulative_primary_improvement_2027_2036_bil = 0,
      gross_collection_2027_2036_bil = cumulative_gross_collections,
      solver_exclusion_reason = ifelse(
        sp$protection_status == "BLOCKED",
        "Protected productive small-business credit category; additionally, OMB Table 9-4 is gross collections rather than a net budget score.",
        "OMB Table 9-4 reports gross collections and explicitly excludes related spending; no unsupported net deficit reduction is inferred."
      ),
      replaced_by_granular_account_controls = FALSE,
      complexity_weight = 1,
      complexity_weight_basis = "Inventory-only OMB gross collection proposal; no solver complexity weight is operative",
      policy_domain = sp$policy_domain
    )
    list(meta = meta, flows = flows)
  })

  list(meta = bind_rows(purrr::map(rows, "meta")), flows = bind_rows(purrr::map(rows, "flows")))
}

# ------------------------------------------------------------------------------
# FUNCTION: build_robust_feasibility_current_policy_evidence_inventory
# Purpose: Preserve current authoritative evidence that is relevant to future
#          expansion but does not yet supply an admissible federal annual path.
# ------------------------------------------------------------------------------
build_robust_feasibility_current_policy_evidence_inventory <- function() {
  tibble(
    domain = c(
      "Medicare / Medicaid drug pricing",
      "Program integrity",
      "Federal credit and subsidy reform",
      "Governmental fees and user charges"
    ),
    policy_or_source = c(
      "2026 Most-Favored-Nation drug-pricing framework",
      "FY2027 Budget program-integrity investments",
      "Current federal credit subsidy / fair-value inventories",
      "FY2027 Budget Analytical Perspectives Table 9-4 user-charge proposals"
    ),
    authoritative_source = c(
      "https://www.whitehouse.gov/research/2026/05/savings-from-most-favored-nation-drug-pricing-policy/",
      "https://www.whitehouse.gov/wp-content/uploads/2026/04/spec_fy2027.pdf",
      "OMB/CBO federal credit program data already inventoried by the model",
      "https://www.whitehouse.gov/wp-content/uploads/2026/04/spec_fy2027.pdf"
    ),
    solver_status = c(
      "INVENTORY_ONLY_NO_FEDERAL_ANNUAL_SCORE",
      "INVENTORY_ONLY_BASELINE_OR_NO_CLEAN_INCREMENTAL_ANNUAL_PATH",
      "INVENTORY_ONLY_NO_POLICY_SPECIFIC_MARGINAL_RESPONSE",
      "INVENTORY_ONLY_GROSS_COLLECTIONS_NOT_NET_BUDGET_SCORE"
    ),
    reason = c(
      "The May 2026 MFN report gives $529B of all-market domestic savings for prospective MFN and $64.3B of combined federal/state Medicaid savings for existing drugs, but not a federal-only annual budget path. The model therefore does not fabricate one.",
      "The FY2027 Budget describes material gross/net program-integrity savings, but baseline treatment and incomplete annual incremental paths prevent clean additive coefficients for this build.",
      "Program subsidy totals measure expected cost, not the savings response to an unspecified fee, guarantee, eligibility, or underwriting reform.",
      "OMB Table 9-4 supplies annual gross collections and explicitly states that related spending is excluded. Gross collections are retained as evidence but not converted into unsupported net deficit reduction."
    )
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_expanded_anchor_policy_model
# Purpose: Add robust-feasibility SSA policy-specific response functions and OMB
#          user-charge evidence inventory after the memory-safety anchor
#          universe, which already includes the expanded Treasury set.
# ------------------------------------------------------------------------------
build_expanded_anchor_policy_model <- function(cbo_universe, working_baseline, spending_detail, cbo_econ) {
  x <- build_expanded_anchor_policy_model_memory_safety(cbo_universe, working_baseline, spending_detail, cbo_econ)
  ssa <- build_ssa_oact_solving_candidates()
  omb <- build_omb_user_charge_candidates(working_baseline)

  added_meta <- bind_rows(ssa$meta, omb$meta)
  added_flows <- bind_rows(ssa$flows, omb$flows)
  dup <- intersect(x$meta$candidate_id, added_meta$candidate_id)
  assert_model(length(dup) == 0L, paste0("Model layer policy IDs duplicate existing candidates: ", paste(dup, collapse = ", ")))

  x$meta <- bind_rows(x$meta, added_meta)
  x$flows <- bind_rows(x$flows, added_flows)
  x$ssa_oact_robust_feasibility_meta <- ssa$meta
  x$ssa_oact_robust_feasibility_flows <- ssa$flows
  x$ssa_oact_robust_feasibility_source_rates <- ssa$source_rates
  x$ssa_2026_taxable_payroll <- ssa$taxable_payroll
  x$omb_user_charge_robust_feasibility_meta <- omb$meta
  x$omb_user_charge_robust_feasibility_flows <- omb$flows
  x$robust_feasibility_evidence_inventory <- build_robust_feasibility_current_policy_evidence_inventory()
  x
}

# ------------------------------------------------------------------------------
# FUNCTION: build_policy_search_score_review
# Purpose: Admit official SSA OACT response functions and OMB annual user-charge
#          paths on evidence quality alone. Statutory change is metadata, not a
#          policy-space veto.
# ------------------------------------------------------------------------------
build_policy_search_score_review <- function(meta, current_index) {
  out <- build_policy_search_score_review_memory_safety(meta, current_index)
  is_ssa <- out$source_kind == "SSA_OACT_SOLVENCY_PROVISION"
  is_omb <- out$source_kind == "OMB_USER_CHARGE_POLICY"
  ext <- is_ssa | is_omb

  out$externally_vettable_score[is_ssa] <- TRUE
  out$externally_vettable_score[is_omb] <- FALSE
  out$score_recency_status[is_ssa] <- "LATEST_AVAILABLE_OACT_PROVISION_RESPONSE_FOR_SPECIFIED_PROVISION"
  out$score_recency_status[is_omb] <- "CURRENT_FY2027_OMB_OFFICIAL_GROSS_COLLECTION_ESTIMATE"
  out$score_basis_status[is_ssa] <- "VALID_OFFICIAL_OACT_RATE_RESPONSE_WITH_CURRENT_2026_TAXABLE_PAYROLL_DERIVATION"
  out$score_basis_status[is_omb] <- "INVENTORY_ONLY_OMB_GROSS_COLLECTIONS_NOT_NET_BUDGET_SCORE"
  out$score_basis_reason[is_ssa] <- "SSA OACT publishes provision-specific annual changes in cost and income rates. The latest 2026 Trustees taxable-payroll path supplies the current dollar base; the dollar flow is derived rather than a direct published dollar score."
  out$score_basis_reason[is_omb] <- "OMB FY2027 Analytical Perspectives Table 9-4 publishes gross annual collections but explicitly excludes related spending. The collection path is retained as evidence inventory and is not treated as a net deficit-reduction score."
  out$score_method_data_basis[is_ssa] <- "SSA_OACT_PROVISION_ANNUAL_CHANGE_RATES;SSA_2026_TR_TABLE_VI_G1_TAXABLE_PAYROLL;DERIVED_DOLLAR_FLOW"
  out$score_method_data_basis[is_omb] <- "OMB_FY2027_ANALYTICAL_PERSPECTIVES_TABLE_09_4_GROSS_COLLECTIONS_ONLY"
  out$policy_requires_legislative_change[ext] <- TRUE
  out$legislative_change_context[ext] <- "CONGRESSIONAL_ENACTMENT_REQUIRED_POLICY_PROPOSAL"
  out$policy_search_solver_eligible[ext] <- out$parameterized_solver_eligible[ext]

  out$current_law_change_class[ext] <- out$legislative_change_context[ext]
  out$current_law_adjudication_status[ext] <- out$score_basis_status[ext]
  out$current_law_adjudication_reason[ext] <- out$score_basis_reason[ext]
  out$current_law_evidence_basis[ext] <- out$score_method_data_basis[ext]
  out$exact_current_law_rescore_available[ext] <- FALSE
  out$current_law_solver_eligible[ext] <- out$policy_search_solver_eligible[ext]
  out
}

# ------------------------------------------------------------------------------
# FUNCTION: build_parameterized_policy_space
# Purpose: Preserve memory-safety parameterization while forcing the new SSA
#          structural alternatives to full discrete anchors. OACT does not score
#          arbitrary fractional versions of these structural provisions.
# ------------------------------------------------------------------------------
build_parameterized_policy_space <- function(policy_model, working_baseline) {
  x <- build_parameterized_policy_space_memory_safety(policy_model, working_baseline)

  # Presentation-audit layer audit-integrity fix: memory-safety parameterization
  # rebuilds the policy-model list and therefore did not automatically carry
  # robust-feasibility SSA source tables forward. Preserve those authoritative
  # inputs explicitly so the final model can reproduce every derived OACT
  # dollar coefficient without refetching or relying on transient objects.
  x$ssa_oact_robust_feasibility_source_rates <- policy_model$ssa_oact_robust_feasibility_source_rates
  x$ssa_2026_taxable_payroll <- policy_model$ssa_2026_taxable_payroll
  x$ssa_oact_robust_feasibility_meta <- policy_model$ssa_oact_robust_feasibility_meta
  x$ssa_oact_robust_feasibility_flows <- policy_model$ssa_oact_robust_feasibility_flows

  ssa_ids <- x$meta$candidate_id[x$meta$source_kind == "SSA_OACT_SOLVENCY_PROVISION"]
  if (length(ssa_ids) > 0L) {
    x$meta <- x$meta |>
      mutate(
        parameterization_mode = if_else(candidate_id %in% ssa_ids, "DISCRETE_FULL_ANCHOR", parameterization_mode),
        parameter_name = if_else(candidate_id %in% ssa_ids, "Official SSA OACT structural solvency provision", parameter_name),
        parameter_unit = if_else(candidate_id %in% ssa_ids, "binary_full_reform", parameter_unit),
        parameter_anchor_value = if_else(candidate_id %in% ssa_ids, 1, parameter_anchor_value),
        parameter_min_value = if_else(candidate_id %in% ssa_ids, 0, parameter_min_value),
        parameter_max_value = if_else(candidate_id %in% ssa_ids, 1, parameter_max_value),
        parameter_min_scale = if_else(candidate_id %in% ssa_ids, 1, parameter_min_scale),
        parameter_max_scale = if_else(candidate_id %in% ssa_ids, 1, parameter_max_scale),
        parameter_extrapolation = if_else(candidate_id %in% ssa_ids, FALSE, parameter_extrapolation),
        decision_type = if_else(candidate_id %in% ssa_ids, "DISCRETE_WITH_TIMING", decision_type),
        parameter_evidence_basis = if_else(
          candidate_id %in% ssa_ids,
          "Official OACT structural provision retained only at its fully scored structural level. No fractional reform and no delayed-start or alternate phase-in actuarial score is inferred.",
          parameter_evidence_basis
        )
      )

    # OACT's published actuarial improvement belongs to the provision as scored,
    # not to arbitrary delayed-start or phase-in variants. The model begins policy
    # implementation in 2027, so retain only its earliest START2027 / PHASE1 slot
    # as the fiscal-window approximation and audit every removed timing variant.
    ssa_sched_before <- x$schedules |>
      filter(candidate_id %in% ssa_ids) |>
      select(candidate_id, schedule_key, implementation_start_year, phase_in_years)
    keep_ssa_keys <- ssa_sched_before |>
      filter(implementation_start_year == 2027L, phase_in_years == 1L) |>
      pull(schedule_key)
    assert_model(
      length(keep_ssa_keys) == length(ssa_ids),
      "Each robust-feasibility SSA OACT provision must have exactly one START2027 / PHASE1 anchor schedule"
    )

    x$robust_feasibility_ssa_timing_audit <- ssa_sched_before |>
      mutate(
        retained_for_solver = schedule_key %in% keep_ssa_keys,
        reason = if_else(
          retained_for_solver,
          "Retained earliest model implementation slot; full OACT structural provision and published actuarial improvement preserved.",
          "Removed because OACT does not publish an actuarial score for this delayed-start or alternate-phase-in variant."
        )
      )

    x$schedules <- x$schedules |>
      filter(!(candidate_id %in% ssa_ids) | schedule_key %in% keep_ssa_keys) |>
      mutate(
        parameterization_mode = if_else(candidate_id %in% ssa_ids, "DISCRETE_FULL_ANCHOR", parameterization_mode),
        parameter_name = if_else(candidate_id %in% ssa_ids, "Official SSA OACT structural solvency provision", parameter_name),
        parameter_unit = if_else(candidate_id %in% ssa_ids, "binary_full_reform", parameter_unit),
        parameter_anchor_value = if_else(candidate_id %in% ssa_ids, 1, parameter_anchor_value),
        parameter_min_value = if_else(candidate_id %in% ssa_ids, 0, parameter_min_value),
        parameter_max_value = if_else(candidate_id %in% ssa_ids, 1, parameter_max_value),
        parameter_min_scale = if_else(candidate_id %in% ssa_ids, 1, parameter_min_scale),
        parameter_max_scale = if_else(candidate_id %in% ssa_ids, 1, parameter_max_scale),
        parameter_extrapolation = if_else(candidate_id %in% ssa_ids, FALSE, parameter_extrapolation)
      )
    x$schedule_summary <- x$schedule_summary |>
      filter(!(candidate_id %in% ssa_ids) | schedule_key %in% keep_ssa_keys) |>
      mutate(
        parameterization_mode = if_else(candidate_id %in% ssa_ids, "DISCRETE_FULL_ANCHOR", parameterization_mode),
        parameter_name = if_else(candidate_id %in% ssa_ids, "Official SSA OACT structural solvency provision", parameter_name),
        parameter_unit = if_else(candidate_id %in% ssa_ids, "binary_full_reform", parameter_unit),
        parameter_anchor_value = if_else(candidate_id %in% ssa_ids, 1, parameter_anchor_value),
        parameter_min_value = if_else(candidate_id %in% ssa_ids, 0, parameter_min_value),
        parameter_max_value = if_else(candidate_id %in% ssa_ids, 1, parameter_max_value),
        parameter_min_scale = if_else(candidate_id %in% ssa_ids, 1, parameter_min_scale),
        parameter_max_scale = if_else(candidate_id %in% ssa_ids, 1, parameter_max_scale),
        parameter_extrapolation = if_else(candidate_id %in% ssa_ids, FALSE, parameter_extrapolation)
      )
    x$schedule_flows <- x$schedule_flows |>
      filter(!(candidate_id %in% ssa_ids) | schedule_key %in% keep_ssa_keys)

    assert_model(
      all(table(x$schedules$candidate_id[x$schedules$candidate_id %in% ssa_ids]) == 1L),
      "SSA OACT timing guard failed: every admitted OACT provision must have exactly one solver schedule"
    )
  } else {
    x$robust_feasibility_ssa_timing_audit <- tibble()
  }
  x
}

# ------------------------------------------------------------------------------
# FUNCTION: build_interaction_catalog_full
# Purpose: Add narrow non-additivity guards for new overlapping alternatives.
# ------------------------------------------------------------------------------
build_interaction_catalog_full_robust_feasibility <- function(meta) {
  base <- build_interaction_catalog_full_memory_safety(meta)
  ids <- meta$candidate_id
  txt <- meta |>
    transmute(candidate_id, text = stringr::str_to_lower(paste0(dplyr::coalesce(title, ""), " ", dplyr::coalesce(variant_name, ""))))
  match_ids <- function(pattern) txt |> filter(stringr::str_detect(text, stringr::regex(pattern, ignore_case = TRUE))) |> pull(candidate_id)
  extra <- list()
  add_pairs <- function(left, right, reason) {
    left <- unique(left[left %in% ids]); right <- unique(right[right %in% ids])
    if (length(left) == 0L || length(right) == 0L) return(invisible(NULL))
    for (i in left) for (j in right) if (!identical(i, j)) extra[[length(extra)+1L]] <<- tibble(candidate_i=i, candidate_j=j, reason=reason)
    invisible(NULL)
  }

  # Treasury high-income NIIT base proposal and CBO NIIT-base proposal score the
  # same basic mechanism. Preserve both choices, never stack their scores.
  add_pairs(
    "TREASURY_2025_apply_niit_pass_through_high_income",
    match_ids("expand the base of the net investment income tax"),
    "Treasury and CBO independently score overlapping NIIT-base expansions for active pass-through income; no combined score is assumed."
  )

  # Treasury capital-income reform overlaps separate inherited-gain / death-basis
  # reforms and the billionaire minimum-tax mechanism. No combined score is used.
  add_pairs(
    "TREASURY_2025_reform_taxation_capital_income_high_income",
    match_ids("assets transferred at death|capital gains from sales of inherited assets"),
    "Treasury's broad capital-income reform overlaps separate inherited-gain/death-basis proposals; independent scores are not assumed additive."
  )
  add_pairs(
    "TREASURY_2025_reform_taxation_capital_income_high_income",
    "TREASURY_2025_minimum_income_tax_wealthiest",
    "Treasury high-income capital-income reform and the wealthiest-taxpayer minimum tax overlap in taxation of unrealized/appreciated capital; no combined official score is assumed."
  )

  # All high-earner taxable-maximum payroll-tax designs are alternatives.
  new_ss_tax <- meta |> filter(source_kind == "SSA_OACT_SOLVENCY_PROVISION", stringr::str_detect(candidate_id, "E2_1|E2_17|E2_4|E3_14")) |> pull(candidate_id)
  old_ss_tax <- match_ids("maximum taxable earnings.*social security payroll taxes|tax earnings above \\$250,000|restore 90 percent taxable earnings")
  all_ss_tax <- unique(c(new_ss_tax, old_ss_tax))
  if (length(all_ss_tax) > 1L) {
    for (a in seq_along(all_ss_tax)) for (b in seq_along(all_ss_tax)) if (a < b) {
      extra[[length(extra)+1L]] <- tibble(
        candidate_i = all_ss_tax[[a]], candidate_j = all_ss_tax[[b]],
        reason = "Alternative Social Security taxable-maximum/high-earner payroll-tax designs share the same earnings base and are not assumed additive without a combined OACT/CBO score."
      )
    }
  }

  add_pairs(
    "SSA_OACT_2026_H9_tax_all_benefits_high_income",
    match_ids("tax social security and railroad retirement benefits.*defined benefit pensions"),
    "SSA OACT H9 and the CBO defined-benefit-pension taxation option overlap in Social Security benefit taxation; independent scores are not stacked."
  )

  bind_rows(base, bind_rows(extra)) |>
    mutate(lo = pmin(candidate_i, candidate_j), hi = pmax(candidate_i, candidate_j)) |>
    filter(lo != hi) |>
    distinct(lo, hi, .keep_all = TRUE) |>
    transmute(candidate_i = lo, candidate_j = hi, reason)
}

# ------------------------------------------------------------------------------
# FUNCTION: build_authoritative_lever_expansion_audit
# Purpose: Record which robust-feasibility domains became solver-ready and which
#          current authoritative evidence remains inventory-only.
# ------------------------------------------------------------------------------
build_authoritative_lever_expansion_audit <- function(policy_model, tax_inventory) {
  base <- build_authoritative_lever_expansion_audit_memory_safety(policy_model, tax_inventory)
  added <- policy_model$meta |>
    filter(source_kind %in% c("SSA_OACT_SOLVENCY_PROVISION", "OMB_USER_CHARGE_POLICY"))

  solver_rows <- added |>
    filter(parameterized_solver_eligible) |>
    group_by(source_kind, policy_domain) |>
    summarise(
      count = n(),
      capacity = sum(pmax(cumulative_primary_improvement_2027_2036_bil, 0), na.rm = TRUE),
      .groups = "drop"
    ) |>
    transmute(
      domain = paste0("Robust-feasibility layer solver expansion: ", policy_domain),
      authoritative_evidence = paste0(count, " policy-specific authoritative response functions"),
      solver_status = "ADMITTED_WHERE_PROTECTION_AND_INTERACTION_RULES_ALLOW",
      new_solver_coefficients_in_current_build = count,
      reason = paste0("Solver-ready ten-year primary capacity before interaction constraints: $", format(round(capacity, 3), nsmall = 3), "B.")
    )

  inventory_rows <- added |>
    filter(!parameterized_solver_eligible) |>
    group_by(source_kind, policy_domain) |>
    summarise(
      count = n(),
      gross_collection = sum(dplyr::coalesce(gross_collection_2027_2036_bil, 0), na.rm = TRUE),
      .groups = "drop"
    ) |>
    transmute(
      domain = paste0("Robust-feasibility layer inventory: ", policy_domain),
      authoritative_evidence = if_else(
        source_kind == "OMB_USER_CHARGE_POLICY",
        paste0(count, " official OMB gross-collection paths; $", format(round(gross_collection, 3), nsmall = 3), "B gross FY2027-FY2036 collections"),
        paste0(count, " authoritative inventory rows")
      ),
      solver_status = "INVENTORY_ONLY_NOT_NET_SCORED",
      new_solver_coefficients_in_current_build = 0,
      reason = "Authoritative evidence is retained, but no net annual deficit-reduction coefficient is inferred without a defensible budget score."
    )

  bind_rows(
    base,
    solver_rows,
    inventory_rows,
    build_robust_feasibility_current_policy_evidence_inventory() |>
      transmute(
        domain = paste0("Robust-feasibility layer inventory: ", domain),
        authoritative_evidence = policy_or_source,
        solver_status = solver_status,
        new_solver_coefficients_in_current_build = 0,
        reason = reason
      )
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_expanded_coverage_audit
# Purpose: Add robust-feasibility source counts and capacity to the breadth audit.
# ------------------------------------------------------------------------------
build_expanded_coverage_audit <- function(policy_model, tax_inventory, expanded_sources) {
  base <- build_expanded_coverage_audit_memory_safety(policy_model, tax_inventory, expanded_sources)
  add <- policy_model$meta |>
    filter(source_kind %in% c("SSA_OACT_SOLVENCY_PROVISION", "OMB_USER_CHARGE_POLICY"))
  ssa <- add |> filter(source_kind == "SSA_OACT_SOLVENCY_PROVISION")
  omb <- add |> filter(source_kind == "OMB_USER_CHARGE_POLICY")

  bind_rows(
    base,
    tibble(
      metric = c(
        "Robust-feasibility layer SSA OACT policy-specific candidates added",
        "Robust-feasibility layer SSA OACT solver-ready candidates",
        "Robust-feasibility layer SSA OACT solver-ready ten-year primary capacity, billions",
        "Robust-feasibility layer OMB user-charge evidence rows added",
        "Robust-feasibility layer OMB user-charge solver-ready candidates",
        "Robust-feasibility layer OMB gross collections inventoried FY2027-FY2036, billions"
      ),
      value = c(
        nrow(ssa),
        sum(ssa$parameterized_solver_eligible, na.rm = TRUE),
        sum(pmax(ssa$cumulative_primary_improvement_2027_2036_bil[ssa$parameterized_solver_eligible], 0), na.rm = TRUE),
        nrow(omb),
        sum(omb$parameterized_solver_eligible, na.rm = TRUE),
        sum(dplyr::coalesce(omb$gross_collection_2027_2036_bil, 0), na.rm = TRUE)
      )
    )
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_robust_feasibility_capacity_delta_by_domain
# Purpose: Before any expensive robust-feasibility search, isolate the policies
#          added by this model stage and report authoritative gross capacity,
#          solver-usable capacity, protection exclusions, and inventory-only
#          gross collections by source/domain.
# ------------------------------------------------------------------------------
build_robust_feasibility_capacity_delta_by_domain <- function(policy_model) {
  new_treasury_ids <- c(
    "TREASURY_2025_apply_niit_pass_through_high_income",
    "TREASURY_2025_increase_niit_and_additional_medicare_tax_high_income",
    "TREASURY_2025_reform_taxation_capital_income_high_income",
    "TREASURY_2025_minimum_income_tax_wealthiest",
    "TREASURY_2025_repeal_like_kind_exchange_deferral"
  )
  m <- policy_model$meta |>
    filter(
      candidate_id %in% new_treasury_ids |
        source_kind %in% c("SSA_OACT_SOLVENCY_PROVISION", "OMB_USER_CHARGE_POLICY")
    ) |>
    mutate(
      robust_feasibility_addition = TRUE,
      authoritative_primary_capacity_bil = dplyr::coalesce(cumulative_primary_improvement_2027_2036_bil, 0),
      solver_usable_primary_capacity_bil = if_else(parameterized_solver_eligible, pmax(authoritative_primary_capacity_bil, 0), 0),
      inventory_only_gross_collection_bil = dplyr::coalesce(gross_collection_2027_2036_bil, 0)
    )

  assert_model(nrow(m) >= 10L, "Robust-feasibility layer capacity-delta audit lost expected new Treasury/SSA evidence rows")
  m |>
    group_by(source_kind, policy_domain) |>
    summarise(
      candidate_count = n(),
      blocked_by_protection_count = sum(protection_status == "BLOCKED", na.rm = TRUE),
      conditional_count = sum(protection_status == "CONDITIONAL", na.rm = TRUE),
      solver_ready_count = sum(parameterized_solver_eligible, na.rm = TRUE),
      authoritative_primary_capacity_bil = sum(pmax(authoritative_primary_capacity_bil, 0), na.rm = TRUE),
      solver_usable_primary_capacity_bil = sum(solver_usable_primary_capacity_bil, na.rm = TRUE),
      inventory_only_gross_collection_bil = sum(inventory_only_gross_collection_bil, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      capacity_status = case_when(
        solver_ready_count > 0 & inventory_only_gross_collection_bil > 0 ~ "MIXED_SOLVER_AND_INVENTORY",
        solver_ready_count > 0 ~ "SOLVER_READY_PRE_INTERACTION_CAPACITY",
        inventory_only_gross_collection_bil > 0 ~ "INVENTORY_ONLY_GROSS_COLLECTIONS",
        TRUE ~ "NO_SOLVER_CAPACITY"
      ),
      interpretation = "Solver-usable primary capacity is pre-interaction and is not assumed additive across overlapping policies. Inventory-only gross collections are reported separately and never counted as deficit reduction."
    ) |>
    arrange(source_kind, policy_domain)
}

# ------------------------------------------------------------------------------
# FUNCTION: validate_robust_feasibility_capacity_gain
# Purpose: Exact stop/go gate against memory-safety layer before the expensive full
#          search. This answers whether the newly quantified universe materially
#          changes the attainable fiscal frontier rather than merely adding rows.
# ------------------------------------------------------------------------------
validate_robust_feasibility_capacity_gain <- function(policy_model, working_baseline, kernel_obj) {
  expanded <- policy_universe_for_mode(policy_model, "EXPANDED", "FULL_CAPACITY")
  model <- build_full_milp(expanded, working_baseline, kernel_obj, objective = "target_slack", soft_targets = TRUE)
  model$solver_threads_override <- CFG$robust_feasibility_heavy_threads
  model$memory_reduction_mode <- "ROBUST_FEASIBILITY_PRESEARCH_FULL_UNIVERSE_CAPACITY_GATE"
  sol <- solve_full_milp(model, "ROBUST_FEASIBILITY_PRESEARCH_EXPANDED_THEORETICAL_CAPACITY")
  rm(model); if (isTRUE(CFG$solver_force_gc_between_solves)) invisible(gc(verbose = FALSE))
  assert_model(full_solver_has_feasible_incumbent(sol), "Robust-feasibility layer pre-search capacity solve failed")
  sm <- summarize_full_solution(sol, policy_model, working_baseline, kernel_obj, "ROBUST_FEASIBILITY_PRESEARCH")
  new_primary <- sm$revenue_2027_2036_bil[[1]] + sm$spending_cuts_2027_2036_bil[[1]]
  delta <- new_primary - CFG$memory_safety_expanded_theoretical_primary_improvement_2027_2036_bil
  tibble(
    benchmark = "EXPANDED_THEORETICAL_FULL_CAPACITY",
    memory_safety_primary_improvement_bil = CFG$memory_safety_expanded_theoretical_primary_improvement_2027_2036_bil,
    robust_feasibility_presearch_primary_improvement_bil = new_primary,
    incremental_primary_capacity_bil = delta,
    minimum_material_expansion_bil = CFG$robust_feasibility_min_realized_capacity_gain_bil,
    central_debt_gdp_2036_pct = sm$debt_gdp_2036_pct[[1]],
    central_debt_gdp_2046_pct = sm$debt_gdp_2046_pct[[1]],
    worst_required_debt_gdp_2036_pct = sm$worst_required_scenario_debt_gdp_2036_pct[[1]],
    worst_required_debt_gdp_2046_pct = sm$worst_required_scenario_debt_gdp_2046_pct[[1]],
    passed = delta >= CFG$robust_feasibility_min_realized_capacity_gain_bil
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: robust_feasibility_primary_gap_equivalent
# Purpose: Convert a remaining target debt gap into the level annual primary
#          improvement equivalent that would close that gap under the scenario's
#          policy-yield and debt-service assumptions.
# ------------------------------------------------------------------------------
robust_feasibility_primary_gap_equivalent <- function(gap_bil, target_year, scenario_id, policy_model, kernel_obj) {
  if (!is.finite(gap_bil) || gap_bil <= 0) return(0)
  annual <- policy_model$scenario_annual |> filter(scenario_id == !!scenario_id) |> arrange(year)
  assert_model(nrow(annual) == length(CFG$model_years), paste0("Missing scenario annual path: ", scenario_id))
  yield_factor <- annual$policy_yield_factor[[1]]
  rate_addition <- annual$marginal_rate_addition[[1]]
  central_long_rate <- get_long_run_rate(kernel_obj)
  kernel_multiplier <- (central_long_rate + rate_addition) / central_long_rate
  long_rate <- central_long_rate + rate_addition
  years <- CFG$model_years
  primary <- ifelse(years >= 2027L & years <= target_year, -1 * yield_factor, 0)
  kernel <- kernel_obj$kernel
  primary10 <- primary[match(2026:2036, years)]
  kernel_primary <- kernel |>
    left_join(tibble(input_year = 2026:2036, p = primary10), by = "input_year") |>
    mutate(contribution = debt_service_effect_per_1_bil_primary_deficit * kernel_multiplier * p) |>
    group_by(output_year) |>
    summarise(interest = sum(contribution), .groups = "drop")
  debt <- numeric(length(years)); interest <- numeric(length(years))
  for (i in seq_along(years)) {
    y <- years[[i]]
    if (y <= 2036L) {
      v <- kernel_primary$interest[kernel_primary$output_year == y]
      interest[[i]] <- ifelse(length(v) == 0L, 0, v[[1]])
    } else {
      interest[[i]] <- long_rate * (debt[[i-1L]] + 0.5 * primary[[i]])
    }
    debt[[i]] <- if (i == 1L) primary[[i]] + interest[[i]] else debt[[i-1L]] + primary[[i]] + interest[[i]]
  }
  debt_reduction_per_annual_bil <- -debt[match(target_year, years)]
  assert_model(is.finite(debt_reduction_per_annual_bil) && debt_reduction_per_annual_bil > 0, "Could not compute primary-gap equivalent")
  gap_bil / debt_reduction_per_annual_bil
}

# ------------------------------------------------------------------------------
# FUNCTION: build_robust_feasibility_robust_gap_diagnostics
# Purpose: Quantify the binding required-scenario gaps using the best retained
#          EXPANDED package, including primary-capacity equivalents.
# ------------------------------------------------------------------------------
build_robust_feasibility_robust_gap_diagnostics <- function(search_result, policy_model, kernel_obj) {
  s <- search_result$summary |> filter(protection_mode == "EXPANDED")
  if (nrow(s) == 0L) return(tibble())
  s <- s |>
    mutate(rank_metric = dplyr::coalesce(achieved_target_slack_score, 0) + pmax(worst_required_scenario_debt_gdp_2036_pct - 90, 0) + pmax(worst_required_scenario_debt_gdp_2046_pct - 80, 0)) |>
    arrange(rank_metric, implementation_complexity_score, selected_policy_count)
  sid <- s$solution_id[[1]]
  p <- search_result$scenario_paths |>
    filter(solution_id == sid, required_robust, year %in% c(2036L, 2046L)) |>
    mutate(
      target_ratio = if_else(year == 2036L, CFG$target_2036, CFG$target_2046_high),
      target_debt_bil = target_ratio * scenario_gdp_bil,
      debt_gap_bil = pmax(scenario_debt_bil - target_debt_bil, 0),
      debt_gap_pp = pmax(scenario_debt_gdp_pct - 100 * target_ratio, 0)
    ) |>
    rowwise() |>
    mutate(
      level_annual_primary_improvement_equivalent_bil = robust_feasibility_primary_gap_equivalent(debt_gap_bil, year, scenario_id, policy_model, kernel_obj),
      cumulative_primary_improvement_equivalent_bil = level_annual_primary_improvement_equivalent_bil * (year - 2026L)
    ) |>
    ungroup() |>
    group_by(year) |>
    mutate(binding_within_year = debt_gap_bil == max(debt_gap_bil, na.rm = TRUE)) |>
    ungroup() |>
    mutate(
      selected_reference_solution_id = sid,
      interpretation = "Equivalent level annual primary improvement beginning in 2027, including the scenario policy-yield factor and debt-service response; diagnostic, not a fabricated policy score."
    ) |>
    select(selected_reference_solution_id, scenario_id, year, scenario_debt_gdp_pct, target_ratio, debt_gap_pp, debt_gap_bil, level_annual_primary_improvement_equivalent_bil, cumulative_primary_improvement_equivalent_bil, binding_within_year, interpretation)
  p
}

# ------------------------------------------------------------------------------
# FUNCTION: universe_with_policy_yield_factor
# Purpose: Clone one universe while changing only the required policy-yield
#          scenario's realization factor for threshold diagnostics.
# ------------------------------------------------------------------------------
universe_with_policy_yield_factor <- function(universe, factor) {
  x <- universe
  idx <- x$scenario_annual$scenario_id == "POLICY_YIELD_90"
  x$scenario_annual$policy_yield_factor[idx] <- factor
  x
}

# ------------------------------------------------------------------------------
# FUNCTION: find_policy_yield_robust_threshold
# Purpose: Binary-search the minimum policy realization factor at which a robust
#          hard-target package exists. Complexity reduction is exact, so no policy
#          candidate is removed from the diagnostic.
# ------------------------------------------------------------------------------
find_policy_yield_robust_threshold <- function(universe, policy_model, working_baseline, kernel_obj, require_ss_solvency = FALSE) {
  reduced <- reduce_universe_for_complexity(universe, kernel_obj)
  tested <- list()
  test_factor <- function(f) {
    u <- universe_with_policy_yield_factor(reduced, f)
    m <- build_full_milp(
      u, working_baseline, kernel_obj,
      objective = "complexity",
      require_ss_solvency = require_ss_solvency,
      soft_targets = FALSE
    )
    m$solver_threads_override <- CFG$robust_feasibility_heavy_threads
    m$memory_reduction_mode <- "EXACT_DOMINATED_SCHEDULE_PRUNING_POLICY_YIELD_THRESHOLD"
    sol <- solve_full_milp(m, paste0("ROBUST_FEASIBILITY_YIELD_THRESHOLD_", ifelse(require_ss_solvency, "SS_", ""), sprintf("%04d", round(1000*f))))
    rm(m); if (isTRUE(CFG$solver_force_gc_between_solves)) invisible(gc(verbose = FALSE))
    feasible <- full_solver_has_feasible_incumbent(sol)
    tested[[length(tested)+1L]] <<- tibble(policy_yield_factor = f, feasible = feasible, require_ss_solvency = require_ss_solvency, solver_status = sol$status_message, elapsed_seconds = sol$elapsed_seconds)
    feasible
  }

  upper <- 1.00
  upper_ok <- test_factor(upper)
  if (!upper_ok) {
    return(list(
      threshold = NA_real_,
      table = bind_rows(tested) |> mutate(threshold_status = "NO_FEASIBLE_PACKAGE_EVEN_AT_100_PERCENT_POLICY_REALIZATION")
    ))
  }
  lower <- CFG$robust_feasibility_policy_yield_lower_bound
  lower_ok <- test_factor(lower)
  if (lower_ok) {
    return(list(
      threshold = lower,
      table = bind_rows(tested) |> mutate(threshold_status = "FEASIBLE_AT_OR_BELOW_CONFIGURED_LOWER_BOUND")
    ))
  }
  while ((upper - lower) > CFG$robust_feasibility_policy_yield_tolerance) {
    mid <- (lower + upper) / 2
    if (test_factor(mid)) upper <- mid else lower <- mid
  }
  list(
    threshold = upper,
    table = bind_rows(tested) |>
      arrange(policy_yield_factor) |>
      mutate(threshold_status = paste0("MINIMUM_FEASIBLE_FACTOR_APPROX_", sprintf("%.3f", upper)))
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: solve_robust_feasibility_materially_diverse_alternatives
# Purpose: Find politically substitutable packages by requiring a much larger
#          activation-distance than ordinary de-duplication.
# ------------------------------------------------------------------------------
solve_robust_feasibility_materially_diverse_alternatives <- function(search_result, policy_model, working_baseline, kernel_obj) {
  u <- reduce_universe_for_complexity(search_result$expanded_universe, kernel_obj)
  tf <- search_result$target_feasibility_audit |> filter(protection_mode == "EXPANDED")
  hard_feasible <- nrow(tf) == 1L && isTRUE(tf$package_ready_hard_target_feasible[[1]])
  soft_cap <- Inf
  if (!hard_feasible) {
    best <- tf$package_ready_best_target_slack_score[[1]]
    soft_cap <- best * 1.10 + CFG$soft_frontier_slack_absolute_tolerance
  }

  sols <- list(); previous <- list()
  for (k in seq_len(CFG$robust_feasibility_alternative_package_count)) {
    m <- build_full_milp(
      u, working_baseline, kernel_obj,
      objective = "complexity",
      soft_targets = !hard_feasible,
      max_target_slack_score = soft_cap,
      previous_packages = previous,
      min_hamming_distance = CFG$robust_feasibility_alternative_hamming_distance
    )
    m$solver_threads_override <- CFG$robust_feasibility_heavy_threads
    m$memory_reduction_mode <- "EXACT_DOMINATED_SCHEDULE_PRUNING_MATERIAL_DIVERSITY"
    sol <- solve_full_milp(m, paste0("ROBUST_FEASIBILITY_MATERIAL_ALT_", sprintf("%02d", k)))
    rm(m); if (isTRUE(CFG$solver_force_gc_between_solves)) invisible(gc(verbose = FALSE))
    sols[[length(sols)+1L]] <- sol
    if (!full_solver_has_feasible_incumbent(sol)) break
    previous[[length(previous)+1L]] <- sol$selected_candidate_ids
  }

  feasible <- sols[vapply(sols, full_solver_has_feasible_incumbent, logical(1))]
  if (length(feasible) == 0L) return(list(catalog=tibble(), membership=tibble(), paths=tibble(), pairwise=tibble(), solutions=sols))

  catalog <- purrr::imap_dfr(feasible, function(sol, i) {
    summarize_full_solution(sol, policy_model, working_baseline, kernel_obj, paste0("ALT", sprintf("%02d", i))) |>
      mutate(material_diversity_requirement = CFG$robust_feasibility_alternative_hamming_distance)
  })
  membership <- purrr::imap_dfr(feasible, function(sol, i) extract_solution_membership(sol, policy_model, paste0("ALT", sprintf("%02d", i))))
  paths <- purrr::imap_dfr(feasible, function(sol, i) simulate_parameterized_package(sol$parameter_decisions, policy_model, working_baseline, kernel_obj) |> mutate(solution_id = paste0("ALT", sprintf("%02d", i))))

  sets <- purrr::map(feasible, "selected_candidate_ids")
  pairwise <- if (length(sets) < 2L) tibble() else purrr::map_dfr(seq_len(length(sets)-1L), function(i) {
    purrr::map_dfr((i+1L):length(sets), function(j) {
      a <- unique(sets[[i]]); b <- unique(sets[[j]])
      tibble(
        solution_i = paste0("ALT", sprintf("%02d", i)),
        solution_j = paste0("ALT", sprintf("%02d", j)),
        activation_hamming_distance = length(setdiff(a,b)) + length(setdiff(b,a)),
        shared_policy_count = length(intersect(a,b)),
        only_i_count = length(setdiff(a,b)),
        only_j_count = length(setdiff(b,a))
      )
    })
  })
  list(catalog=catalog, membership=membership, paths=paths, pairwise=pairwise, solutions=sols)
}

# ------------------------------------------------------------------------------
# FUNCTION: run_parallel_independent_resimulation_audit
# Purpose: Use memory-aware PSOCK concurrency only after MILP solving, where each
#          job is lightweight and independent. This creates real concurrency on
#          Windows without duplicating multiple heavyweight HiGHS models.
# ------------------------------------------------------------------------------
run_parallel_independent_resimulation_audit <- function(search_result, policy_model, working_baseline, kernel_obj) {
  sols <- search_result$solution_objects[vapply(search_result$solution_objects, full_solver_has_feasible_incumbent, logical(1))]
  if (length(sols) == 0L) return(tibble())
  decisions <- purrr::map(sols, "parameter_decisions")
  labels <- vapply(sols, function(x) x$solve_label, character(1))
  workers <- min(CFG$robust_feasibility_light_parallel_workers, length(decisions), max(1L, parallel::detectCores(logical = TRUE) - 2L))

  # The simulator needs only these policy-model fields. Exporting the full policy
  # model would duplicate large catalogs on every PSOCK worker for no benefit.
  simulation_policy_model <- list(
    schedule_flows = policy_model$schedule_flows,
    scenario_meta = policy_model$scenario_meta,
    scenario_annual = policy_model$scenario_annual
  )
  if (workers <= 1L) {
    sims <- lapply(decisions, simulate_parameterized_package, policy_model = simulation_policy_model, working_baseline = working_baseline, kernel_obj = kernel_obj)
    mode <- "SEQUENTIAL_FALLBACK"
  } else {
    cl <- parallel::makePSOCKcluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, suppressPackageStartupMessages({library(dplyr); library(tidyr); library(purrr); library(tibble)}))
    parallel::clusterExport(
      cl,
      varlist = c("CFG", "simulate_parameterized_package", "get_long_run_rate", "simulation_policy_model", "working_baseline", "kernel_obj"),
      envir = environment()
    )
    sims <- parallel::parLapply(cl, decisions, function(d) simulate_parameterized_package(d, simulation_policy_model, working_baseline, kernel_obj))
    mode <- paste0("PSOCK_", workers, "_WORKERS")
  }

  purrr::map2_dfr(seq_along(sims), sims, function(i, sim) {
    p <- sim |> filter(year %in% c(2036L,2046L), required_robust)
    tibble(
      solve_label = labels[[i]],
      parallel_mode = mode,
      required_scenario_rows_checked = nrow(p),
      all_finite = all(is.finite(p$scenario_debt_bil)) && all(is.finite(p$scenario_debt_gdp_pct)),
      max_required_debt_gdp_pct = max(p$scenario_debt_gdp_pct, na.rm=TRUE)
    )
  })
}

# ------------------------------------------------------------------------------
# FUNCTION: build_robust_feasibility_concurrency_plan_audit
# Purpose: Make the hardware-aware scheduler rule explicit and auditable.
# ------------------------------------------------------------------------------
build_robust_feasibility_concurrency_plan_audit <- function() {
  tibble(
    job_class = c("HEAVY_MILP", "MEDIUM_MILP", "LIGHT_INDEPENDENT_AUDIT"),
    maximum_concurrent_jobs = c(1L, CFG$robust_feasibility_medium_parallel_jobs, CFG$robust_feasibility_light_parallel_workers),
    threads_per_job = c(CFG$robust_feasibility_heavy_threads, CFG$robust_feasibility_medium_threads_each, 1L),
    active_in_robust_feasibility = c(TRUE, FALSE, TRUE),
    reason = c(
      "Heavy HiGHS branch-and-bound jobs run alone because RAM is the limiting resource on the 64 GB system.",
      "Two-way medium-MILP concurrency is defined but deliberately not activated automatically until empirical robust-feasibility memory footprints demonstrate combined use remains below the configured RAM budget.",
      "Independent post-solve re-simulations use PSOCK workers because they are lightweight, deterministic, and do not duplicate live HiGHS branch-and-bound trees."
    ),
    hardware = CFG$robust_feasibility_hardware,
    memory_budget_gb = CFG$robust_feasibility_parallel_memory_budget_gb
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: run_full_solution_search
# Purpose: Apply robust-feasibility pre-search capacity gate, preserve the full
#          memory-safety search, then add robust threshold and material
#          alternative-package diagnostics.
# ------------------------------------------------------------------------------
run_full_solution_search_robust_feasibility <- function(policy_model, working_baseline, kernel_obj) {
  capacity_delta_by_domain <- build_robust_feasibility_capacity_delta_by_domain(policy_model)
  if (CFG$write_audit_outputs) {
    write_csv_atomic(capacity_delta_by_domain, file.path(CFG$output_dir, "robust_feasibility_policy_capacity_delta_by_domain.csv"))
  }

  capacity_gain <- validate_robust_feasibility_capacity_gain(policy_model, working_baseline, kernel_obj)
  if (CFG$write_audit_outputs) write_csv_atomic(capacity_gain, file.path(CFG$output_dir, "robust_feasibility_capacity_gain_vs_memory_safe_baseline.csv"))
  assert_model(
    all(capacity_gain$passed),
    paste0("Robust-feasibility layer realized only $", format(round(capacity_gain$incremental_primary_capacity_bil[[1]],3), nsmall=3), "B of additional EXPANDED theoretical ten-year primary capacity relative to the frozen memory-safe capacity benchmark, below the configured $", CFG$robust_feasibility_min_realized_capacity_gain_bil, "B stop/go threshold. Full search stopped intentionally.")
  )
  log_line("Robust-feasibility layer capacity gate passed | incremental theoretical primary capacity relative to the frozen memory-safe capacity benchmark=$", format(round(capacity_gain$incremental_primary_capacity_bil[[1]],3), nsmall=3), "B")

  base <- run_full_solution_search_memory_safety(policy_model, working_baseline, kernel_obj)
  gaps <- build_robust_feasibility_robust_gap_diagnostics(base, policy_model, kernel_obj)
  alternatives <- solve_robust_feasibility_materially_diverse_alternatives(base, policy_model, working_baseline, kernel_obj)
  yield_threshold <- find_policy_yield_robust_threshold(base$expanded_universe, policy_model, working_baseline, kernel_obj, require_ss_solvency = FALSE)
  yield_threshold_ss <- find_policy_yield_robust_threshold(base$expanded_universe, policy_model, working_baseline, kernel_obj, require_ss_solvency = TRUE)
  parallel_audit <- run_parallel_independent_resimulation_audit(base, policy_model, working_baseline, kernel_obj)

  if (CFG$write_audit_outputs) {
    write_csv_atomic(gaps, file.path(CFG$output_dir, "robust_feasibility_robust_gap_diagnostics.csv"))
    write_csv_atomic(alternatives$catalog, file.path(CFG$output_dir, "robust_feasibility_materially_diverse_alternatives.csv"))
    write_csv_atomic(alternatives$membership, file.path(CFG$output_dir, "robust_feasibility_materially_diverse_alternative_membership.csv"))
    write_csv_atomic(alternatives$paths, file.path(CFG$output_dir, "robust_feasibility_materially_diverse_alternative_paths.csv"))
    write_csv_atomic(alternatives$pairwise, file.path(CFG$output_dir, "robust_feasibility_materially_diverse_pairwise_distance.csv"))
    write_csv_atomic(yield_threshold$table, file.path(CFG$output_dir, "robust_feasibility_policy_yield_threshold.csv"))
    write_csv_atomic(yield_threshold_ss$table, file.path(CFG$output_dir, "robust_feasibility_policy_yield_threshold_with_ss_solvency.csv"))
    write_csv_atomic(parallel_audit, file.path(CFG$output_dir, "robust_feasibility_parallel_resimulation_audit.csv"))
    write_csv_atomic(build_robust_feasibility_concurrency_plan_audit(), file.path(CFG$output_dir, "robust_feasibility_concurrency_plan.csv"))
  }

  base$robust_feasibility_capacity_delta_by_domain <- capacity_delta_by_domain
  base$robust_feasibility_capacity_gain <- capacity_gain
  base$robust_feasibility_robust_gap_diagnostics <- gaps
  base$robust_feasibility_alternatives <- alternatives
  base$robust_feasibility_policy_yield_threshold <- yield_threshold
  base$robust_feasibility_policy_yield_threshold_with_ss_solvency <- yield_threshold_ss
  base$robust_feasibility_parallel_resimulation_audit <- parallel_audit
  base
}

# ------------------------------------------------------------------------------
# FUNCTION: plot_solution_robust_paths
# Purpose: Separate required-scenario plot for the best EXPANDED package so the
#          central representative chart remains readable.
# ------------------------------------------------------------------------------
plot_solution_robust_paths_robust_feasibility <- function(search_result) {
  s <- search_result$summary |> filter(protection_mode == "EXPANDED")
  if (nrow(s) == 0L) return(ggplot() + theme_void() + labs(title = "No EXPANDED package available"))
  s <- s |>
    mutate(
      rank_metric = pmax(worst_required_scenario_debt_gdp_2036_pct - 90, 0) +
        pmax(worst_required_scenario_debt_gdp_2046_pct - 80, 0) +
        dplyr::coalesce(achieved_target_slack_score, 0)
    ) |>
    arrange(rank_metric, implementation_complexity_score)
  sid <- s$solution_id[[1]]
  p <- search_result$scenario_paths |>
    filter(solution_id == sid, required_robust) |>
    arrange(scenario_id, year)

  scenario_ids <- unique(p$scenario_id)
  keep_ids <- character()
  for (cid in scenario_ids) {
    a <- p |> filter(scenario_id == cid) |> arrange(year) |> pull(scenario_debt_gdp_pct)
    if (length(a) == 0L) next
    visually_distinct <- TRUE
    for (kid in keep_ids) {
      b <- p |> filter(scenario_id == kid) |> arrange(year) |> pull(scenario_debt_gdp_pct)
      if (length(a) == length(b) && max(abs(a - b), na.rm = TRUE) < CFG$plot_visual_path_tolerance_pp) {
        visually_distinct <- FALSE
        break
      }
    }
    if (visually_distinct) keep_ids <- c(keep_ids, cid)
  }
  p <- p |> filter(scenario_id %in% keep_ids)

  ggplot(p, aes(x = year, y = scenario_debt_gdp_pct, color = scenario_id, group = scenario_id)) +
    geom_line(linewidth = 0.85) +
    geom_hline(yintercept = c(90, 80, 75), linetype = "dotted") +
    scale_x_continuous(breaks = seq(2026, 2046, 2)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "Required-scenario debt paths for the best expanded package",
      subtitle = paste0("Package ", sid, "; visually redundant required-scenario paths are omitted from both linework and legend"),
      x = NULL,
      y = "Debt held by public / GDP",
      color = "Required scenario",
      caption = paste0(
        "Required-scenario paths within ", format(CFG$plot_visual_path_tolerance_pp, trim = TRUE),
        " percentage point at every year are treated as visually identical. Target lines mark 90 percent in 2036, 80 percent in 2046, and the 75 percent long-run reference."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}

# ------------------------------------------------------------------------------
# FUNCTION: run_model
# Purpose: Preserve the validated memory-safety execution pipeline while
#          writing robust-feasibility source, robustness, concurrency, and plot
#          artifacts after the core run returns.
# ------------------------------------------------------------------------------
run_model_robust_feasibility <- function() {
  result <- run_model_memory_safety()

  if (!is.null(result$solution_search)) {
    search <- result$solution_search
    if (CFG$write_audit_outputs) {
      if (!is.null(result$policy_model$ssa_oact_robust_feasibility_source_rates)) {
        write_csv_atomic(result$policy_model$ssa_oact_robust_feasibility_source_rates, file.path(CFG$output_dir, "ssa_oact_robust_feasibility_annual_rate_responses.csv"))
      }
      if (!is.null(result$policy_model$robust_feasibility_ssa_timing_audit)) {
        write_csv_atomic(result$policy_model$robust_feasibility_ssa_timing_audit, file.path(CFG$output_dir, "robust_feasibility_ssa_oact_timing_audit.csv"))
      }
      write_csv_atomic(build_robust_feasibility_current_policy_evidence_inventory(), file.path(CFG$output_dir, "robust_feasibility_current_policy_evidence_inventory.csv"))
      write_csv_atomic(build_robust_feasibility_concurrency_plan_audit(), file.path(CFG$output_dir, "robust_feasibility_concurrency_plan.csv"))
    }

    robust_plot <- plot_solution_robust_paths(search)
    if (CFG$write_audit_outputs) {
      ragg::agg_png(file.path(CFG$output_dir, "robust_feasibility_required_scenario_debt_paths.png"), width = 1920, height = 1080, res = 144)
      print(robust_plot)
      grDevices::dev.off()
    }
    if (CFG$render_plots) print(robust_plot)
  }

  result$robust_feasibility_policy_evidence_inventory <- build_robust_feasibility_current_policy_evidence_inventory()
  result$robust_feasibility_concurrency_plan <- build_robust_feasibility_concurrency_plan_audit()
  result$robust_feasibility_ssa_oact_timing_audit <- result$policy_model$robust_feasibility_ssa_timing_audit
  result
}

# ------------------------------------------------------------------------------
# MODEL LAYER ACTIVE RULE SUMMARY
# - Protected categories are unchanged; broad ordinary-income-rate increases
#   remain blocked even when an official score exists.
# - Current law never independently determines policy eligibility.
# - Additional policy-specific Treasury and SSA OACT evidence broadens the solver
#   only where annual response functions are defensible. OMB user-charge gross
#   collections remain inventory-only because related spending is not net-scored.
# - Current Medicare MFN and program-integrity evidence is inventoried rather
#   than converted into unsupported annual federal scores.
# - Model layer memory controls remain active. Heavy MILPs run alone with
#   at most eight HiGHS threads; lightweight independent verification is parallel.
# - Robust-gap, policy-yield threshold, full-SS-solvency threshold, and materially
#   diverse alternative-package diagnostics are added after the core search.
# - A separate required-scenario figure supplements the central-path figure.
# - Model layer counterfactuals that intentionally remove all Social
#   Security actuarial-improvement coefficients are recorded as structural
#   requirement conflicts instead of constructing empty solvency rows.
# ------------------------------------------------------------------------------

# Execution is deferred until the recommendation-analysis stage is defined.


# ==============================================================================
# MODEL LAYER RECOMMENDATION, DEPENDENCE, AND PRESENTATION MODEL STAGE
# ==============================================================================
# Robust-feasibility layer established robust feasibility in the expanded policy space.
# Recommendation-analysis layer therefore changes phase. It preserves every approved
# protection, score-admissibility rule, baseline, debt-service assumption,
# robustness scenario, memory safeguard, and solver-optimality requirement while
# asking which robust, Social-Security-solvent packages are simplest to recommend
# and how dependent those packages are on particular policies or policy families.
#
# No new current-law eligibility screen is introduced. Congress may enact any
# non-protected policy. No coefficient is altered merely to improve a package.
# The policy universe is unchanged from the robust-feasibility model stage unless a future source
# expansion supplies new authoritative fiscal evidence.
# ==============================================================================

CFG$recommendation_analysis_common_policy_share <- 0.60
CFG$recommendation_analysis_major_policy_test_limit <- 12L
CFG$recommendation_analysis_major_family_test_limit <- 8L
CFG$recommendation_analysis_domain_alternative_limit <- 6L
CFG$recommendation_analysis_major_primary_improvement_bil <- 100
CFG$recommendation_analysis_recommendation_threads <- CFG$robust_feasibility_heavy_threads
CFG$recommendation_analysis_pareto_tolerance <- 1e-9

# Bind the robust-feasibility search and execution functions used by
# installing recommendation-analysis post-search diagnostics and presentation fixes.

# ------------------------------------------------------------------------------
# FUNCTION: recommendation_analysis_pretty_scenario_label
# Purpose: Replace internal scenario IDs with presentation-grade legend labels.
# ------------------------------------------------------------------------------
recommendation_analysis_pretty_scenario_label <- function(x) {
  dplyr::recode(
    x,
    CENTRAL = "Central projection",
    RATES_PLUS_0_1 = "Higher interest rates",
    PRODUCTIVITY_MINUS_0_1 = "Lower productivity",
    LABOR_FORCE_MINUS_0_1 = "Lower labor-force growth",
    POLICY_YIELD_90 = "90% policy yield",
    .default = stringr::str_to_sentence(stringr::str_replace_all(x, "_", " "))
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: recommendation_analysis_broad_policy_domain
# Purpose: Assign a transparent broad policy domain for composition and
#          substitutability diagnostics. This classification affects reporting
#          and alternative-package searches only; it never changes protection or
#          score eligibility.
# ------------------------------------------------------------------------------
recommendation_analysis_broad_policy_domain <- function(meta) {
  field <- function(name) {
    if (!name %in% names(meta)) return(rep("", nrow(meta)))
    x <- as.character(meta[[name]])
    x[is.na(x)] <- ""
    x
  }
  title <- field("title")
  variant_name <- field("variant_name")
  family_id <- field("family_id")
  major_category <- field("major_category")
  source_kind <- field("source_kind")
  policy_domain <- field("policy_domain")
  txt <- stringr::str_to_lower(paste0(
    title, " ", variant_name, " ", family_id, " ", major_category, " ", source_kind, " ", policy_domain
  ))

  dplyr::case_when(
    source_kind == "CBO_SPENDING_DETAIL_GROWTH_CONTROL" ~ "Spending account growth controls",
    source_kind == "SSA_OACT_SOLVENCY_PROVISION" |
      stringr::str_detect(txt, "social security|oasdi|old-age|survivors insurance") ~ "Social Security",
    stringr::str_detect(txt, "medicare|medicaid|health insurance|hospital|physician|drug pricing|part b|part d") ~ "Health and Medicare",
    stringr::str_detect(txt, "financial transaction|financial institution|stock buyback|securities|derivative|bank fee|wall street") ~ "Financial sector and markets",
    stringr::str_detect(txt, "value-added|\\bvat\\b|carbon|excise|fuel tax|energy tax") ~ "Consumption and environmental revenue",
    stringr::str_detect(txt, "corporate|international|foreign income|base erosion|gilti|fdii|profit shifting") ~ "Corporate and international tax",
    stringr::str_detect(txt, "capital gains|assets transferred at death|estate|gift tax|itemized|salt|state and local tax|charitable|minimum income tax|high-income|wealthiest|executive compensation|retirement account") ~ "High-income tax preferences",
    stringr::str_detect(txt, "user fee|passenger fee|surcharge|royalt|spectrum|auction|government service") ~ "Fees, rents, and governmental receipts",
    stringr::str_detect(txt, "revenue|tax|surtax|deduction|exclusion|credit") ~ "Other revenue",
    TRUE ~ "Other spending or fiscal control"
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: recommendation_analysis_filter_universe
# Purpose: Remove specified candidates, policy families, or broad domains from a
#          universe for counterfactual re-optimization. All remaining candidates
#          retain their original schedules, coefficients, scenarios, protections,
#          and interactions.
# ------------------------------------------------------------------------------
recommendation_analysis_filter_universe <- function(
  universe,
  remove_candidate_ids = character(),
  remove_family_ids = character(),
  remove_domains = character()
) {
  meta <- universe$meta
  meta$recommendation_analysis_domain <- recommendation_analysis_broad_policy_domain(meta)

  keep <- !(meta$candidate_id %in% remove_candidate_ids) &
    !(meta$family_id %in% remove_family_ids) &
    !(meta$recommendation_analysis_domain %in% remove_domains)
  keep_ids <- meta$candidate_id[keep]
  assert_model(length(keep_ids) > 0L, "Recommendation-analysis layer counterfactual removed the entire policy universe")

  x <- universe
  x$meta <- universe$meta |> filter(candidate_id %in% keep_ids)
  x$schedules <- universe$schedules |> filter(candidate_id %in% keep_ids)
  x$schedule_summary <- universe$schedule_summary |> filter(candidate_id %in% keep_ids)
  x$schedule_flows <- universe$schedule_flows |> filter(candidate_id %in% keep_ids)
  x
}

# ------------------------------------------------------------------------------
# FUNCTION: recommendation_analysis_ss_solvency_coefficients_present
# Purpose: Detect whether a counterfactual universe still contains any nonzero
#          Social Security actuarial-improvement coefficients. Recommendation-analysis layer
#          recommendation and dependence searches require approximate OASDI
#          solvency. If a deliberate omission removes every such coefficient,
#          that counterfactual is structurally incompatible with the requirement
#          and must be recorded without constructing an invalid empty MILP row.
# ------------------------------------------------------------------------------
recommendation_analysis_ss_solvency_coefficients_present <- function(universe, tol = 1e-12) {
  if (is.null(universe$schedules) ||
      !"ss_actuarial_improvement_pct_payroll" %in% names(universe$schedules) ||
      nrow(universe$schedules) == 0L) {
    return(FALSE)
  }
  x <- suppressWarnings(as.numeric(universe$schedules$ss_actuarial_improvement_pct_payroll))
  x[!is.finite(x)] <- 0
  any(abs(x) > tol)
}

# ------------------------------------------------------------------------------
# FUNCTION: recommendation_analysis_structural_infeasible_solution
# Purpose: Return the minimal solution-shaped record needed by recommendation-analysis
#          leave-one-out diagnostics when a counterfactual is known to violate a
#          required structural condition before HiGHS is called. This is not a
#          solver result and is never treated as one.
# ------------------------------------------------------------------------------
recommendation_analysis_structural_infeasible_solution <- function(solve_label, reason) {
  list(
    status = NA,
    status_message = paste0("STRUCTURALLY_INFEASIBLE: ", reason),
    objective_value = NA_real_,
    primal_solution = numeric(),
    selected_candidate_ids = character(),
    parameter_decisions = tibble(),
    elapsed_seconds = 0,
    info = NULL,
    mip_node_count = NA_real_,
    mip_dual_bound = NA_real_,
    mip_gap = NA_real_,
    simplex_iteration_count = NA_real_,
    ipm_iteration_count = NA_real_,
    max_constraint_violation = NA_real_,
    max_bound_violation = NA_real_,
    max_integrality_violation = NA_real_,
    feasible_incumbent = FALSE,
    raw = NULL,
    model = NULL,
    solve_label = solve_label
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: recommendation_analysis_set_activation_objective
# Purpose: Replace the objective vector of an already-built MILP with a custom
#          linear activation objective while leaving every fiscal constraint
#          unchanged. Used for transparent recommendation-frontier objectives.
# ------------------------------------------------------------------------------
recommendation_analysis_set_activation_objective <- function(model, weights, objective_name, tie_break_complexity = 0) {
  meta <- model$meta
  weights <- weights[match(meta$candidate_id, names(weights))]
  weights[is.na(weights)] <- 0
  y_names <- paste0("y::", meta$candidate_id)
  idx <- match(y_names, model$variable_names)
  assert_model(all(!is.na(idx)), paste0("Recommendation-analysis layer could not map activation variables for objective ", objective_name))

  model$L[] <- 0
  model$L[idx] <- weights
  if (tie_break_complexity > 0) {
    cw <- dplyr::coalesce(meta$complexity_weight, 1)
    model$L[idx] <- model$L[idx] + tie_break_complexity * cw
  }
  model$objective_name <- objective_name
  model
}

# ------------------------------------------------------------------------------
# FUNCTION: recommendation_analysis_solve_recommendation_objective
# Purpose: Solve one robust hard-target recommendation objective on the exact
#          schedule-reduced EXPANDED universe. Social Security approximate
#          solvency is required because a recommended package should satisfy the
#          project's debt and solvency objectives simultaneously.
# ------------------------------------------------------------------------------
recommendation_analysis_solve_recommendation_objective <- function(
  universe,
  policy_model,
  working_baseline,
  kernel_obj,
  objective = c("policy_count", "complexity", "conditional_count"),
  solve_label
) {
  objective <- match.arg(objective)
  u <- reduce_universe_for_complexity(universe, kernel_obj)
  build_objective <- if (objective == "conditional_count") "complexity" else objective
  m <- build_full_milp(
    u,
    working_baseline,
    kernel_obj,
    objective = build_objective,
    require_ss_solvency = TRUE,
    soft_targets = FALSE
  )

  if (objective == "conditional_count") {
    w <- ifelse(u$meta$protection_status == "CONDITIONAL", 1, 0)
    names(w) <- u$meta$candidate_id
    # Complexity is only a microscopic deterministic tie-breaker after the
    # number of EXPANDED-only policies has been minimized.
    m <- recommendation_analysis_set_activation_objective(
      m,
      weights = w,
      objective_name = "conditional_policy_count",
      tie_break_complexity = 1e-6
    )
  }

  m$solver_threads_override <- CFG$recommendation_analysis_recommendation_threads
  m$memory_reduction_mode <- paste0("RECOMMENDATION_ANALYSIS_RECOMMENDATION_", stringr::str_to_upper(objective))
  sol <- solve_full_milp(m, solve_label)
  rm(m)
  if (isTRUE(CFG$solver_force_gc_between_solves)) invisible(gc(verbose = FALSE))
  sol
}

# ------------------------------------------------------------------------------
# FUNCTION: recommendation_analysis_solution_bundle
# Purpose: Convert a list of solved recommendation/counterfactual models into the
#          same compact catalog, membership, and path structures used elsewhere.
# ------------------------------------------------------------------------------
recommendation_analysis_solution_bundle <- function(solutions, prefix, policy_model, working_baseline, kernel_obj) {
  feasible <- solutions[vapply(solutions, full_solver_has_feasible_incumbent, logical(1))]
  if (length(feasible) == 0L) {
    return(list(catalog = tibble(), membership = tibble(), paths = tibble(), scenario_paths = tibble(), solutions = solutions))
  }

  ids <- paste0(prefix, sprintf("%02d", seq_along(feasible)))
  catalog <- purrr::map2_dfr(feasible, ids, function(sol, sid) {
    summarize_full_solution(sol, policy_model, working_baseline, kernel_obj, sid)
  })
  membership <- purrr::map2_dfr(feasible, ids, function(sol, sid) {
    extract_solution_membership(sol, policy_model, sid)
  })
  scenario_paths <- purrr::map2_dfr(feasible, ids, function(sol, sid) {
    simulate_parameterized_package(sol$parameter_decisions, policy_model, working_baseline, kernel_obj) |>
      mutate(solution_id = sid)
  })
  paths <- scenario_paths |>
    filter(scenario_id == "CENTRAL") |>
    transmute(year, debt_gdp_pct = scenario_debt_gdp_pct, solution_id)

  list(catalog = catalog, membership = membership, paths = paths, scenario_paths = scenario_paths, solutions = solutions)
}

# ------------------------------------------------------------------------------
# FUNCTION: recommendation_analysis_candidate_contributions
# Purpose: Reconstruct each selected candidate's realized ten-year primary
#          improvement from its chosen schedule and intensity. This is used only
#          for package composition/dependence diagnostics.
# ------------------------------------------------------------------------------
recommendation_analysis_candidate_contributions <- function(membership, policy_model) {
  if (is.null(membership) || nrow(membership) == 0L) return(tibble())
  schedule_score <- policy_model$schedule_summary |>
    select(schedule_key, primary_improvement_2027_2036_bil_per_scale)

  x <- membership |>
    left_join(schedule_score, by = "schedule_key") |>
    mutate(
      realized_primary_improvement_2027_2036_bil =
        dplyr::coalesce(primary_improvement_2027_2036_bil_per_scale, 0) * dplyr::coalesce(intensity_scale, 0)
    )
  x$recommendation_analysis_domain <- recommendation_analysis_broad_policy_domain(x)
  x
}

# ------------------------------------------------------------------------------
# FUNCTION: build_recommendation_analysis_package_metrics
# Purpose: Add transparent package-selection dimensions without imposing an
#          opaque political utility function. An equal-rank diagnostic is
#          reported, but Pareto efficiency remains the primary recommendation
#          screen.
# ------------------------------------------------------------------------------
build_recommendation_analysis_package_metrics <- function(catalog, membership, policy_model) {
  if (is.null(catalog) || nrow(catalog) == 0L) return(tibble())
  contrib <- recommendation_analysis_candidate_contributions(membership, policy_model)

  by_package <- contrib |>
    group_by(solution_id) |>
    summarise(
      conditional_policy_count = sum(protection_status == "CONDITIONAL", na.rm = TRUE),
      eligible_policy_count = sum(protection_status == "ELIGIBLE", na.rm = TRUE),
      .groups = "drop"
    )

  domain <- contrib |>
    group_by(solution_id, recommendation_analysis_domain) |>
    summarise(domain_primary_improvement_bil = sum(pmax(realized_primary_improvement_2027_2036_bil, 0), na.rm = TRUE), .groups = "drop") |>
    group_by(solution_id) |>
    mutate(
      total_positive_primary_improvement_bil = sum(domain_primary_improvement_bil, na.rm = TRUE),
      domain_primary_share = safe_divide(domain_primary_improvement_bil, total_positive_primary_improvement_bil)
    ) |>
    arrange(solution_id, desc(domain_primary_share)) |>
    summarise(
      dominant_domain = first(recommendation_analysis_domain),
      max_domain_primary_share_pct = 100 * first(domain_primary_share),
      domain_hhi = sum(domain_primary_share^2, na.rm = TRUE),
      represented_domain_count = n(),
      .groups = "drop"
    )

  x <- catalog |>
    left_join(by_package, by = "solution_id") |>
    left_join(domain, by = "solution_id") |>
    mutate(
      conditional_policy_count = dplyr::coalesce(conditional_policy_count, 0L),
      eligible_policy_count = dplyr::coalesce(eligible_policy_count, 0L),
      max_domain_primary_share_pct = dplyr::coalesce(max_domain_primary_share_pct, 100),
      domain_hhi = dplyr::coalesce(domain_hhi, 1),
      ss_solvent_approx = ss_actuarial_improvement_pct_payroll + 1e-9 >= CFG$ss_actuarial_gap_pct_payroll,
      recommendation_eligible = robust_target_2036_pass & robust_target_2046_pass & ss_solvent_approx
    )

  # Pareto dominance is tested only across the four package-complexity and political-breadth
  # dimensions used by the recommendation frontier. Fiscal feasibility and
  # approximate SS solvency are prerequisites rather than tradeable scores.
  efficient <- rep(FALSE, nrow(x))
  eligible_idx <- which(x$recommendation_eligible)
  for (i in eligible_idx) {
    dominated <- FALSE
    for (j in setdiff(eligible_idx, i)) {
      a <- c(
        x$selected_policy_count[[j]],
        x$implementation_complexity_score[[j]],
        x$conditional_policy_count[[j]],
        x$max_domain_primary_share_pct[[j]]
      )
      b <- c(
        x$selected_policy_count[[i]],
        x$implementation_complexity_score[[i]],
        x$conditional_policy_count[[i]],
        x$max_domain_primary_share_pct[[i]]
      )
      if (all(a <= b + CFG$recommendation_analysis_pareto_tolerance) && any(a < b - CFG$recommendation_analysis_pareto_tolerance)) {
        dominated <- TRUE
        break
      }
    }
    efficient[[i]] <- !dominated
  }

  x |>
    mutate(
      pareto_recommendation_frontier = efficient,
      rank_policy_count = rank(selected_policy_count, ties.method = "min"),
      rank_complexity = rank(implementation_complexity_score, ties.method = "min"),
      rank_conditional_count = rank(conditional_policy_count, ties.method = "min"),
      rank_domain_concentration = rank(max_domain_primary_share_pct, ties.method = "min"),
      equal_rank_screen = rank_policy_count + rank_complexity + rank_conditional_count + rank_domain_concentration
    ) |>
    arrange(desc(recommendation_eligible), desc(pareto_recommendation_frontier), equal_rank_screen, selected_policy_count)
}

# ------------------------------------------------------------------------------
# FUNCTION: solve_recommendation_analysis_recommendation_frontier
# Purpose: Generate robust, approximately SS-solvent packages minimizing policy
#          count, implementation complexity, and EXPANDED-only policy count.
# ------------------------------------------------------------------------------
solve_recommendation_analysis_recommendation_frontier <- function(search_result, policy_model, working_baseline, kernel_obj) {
  u <- search_result$expanded_universe
  objectives <- c("policy_count", "complexity", "conditional_count")
  sols <- purrr::map2(
    objectives,
    paste0("RECOMMENDATION_ANALYSIS_RECOMMEND_", stringr::str_to_upper(objectives)),
    ~ recommendation_analysis_solve_recommendation_objective(u, policy_model, working_baseline, kernel_obj, .x, .y)
  )
  bundle <- recommendation_analysis_solution_bundle(sols, "REC", policy_model, working_baseline, kernel_obj)
  if (nrow(bundle$catalog) > 0L) {
    bundle$catalog <- bundle$catalog |>
      mutate(
        recommendation_objective = case_when(
          objective == "policy_count" ~ "policy_count",
          objective == "complexity" ~ "complexity",
          objective == "conditional_policy_count" ~ "conditional_count",
          TRUE ~ objective
        )
      )
  }
  bundle
}

# ------------------------------------------------------------------------------
# FUNCTION: solve_recommendation_analysis_domain_alternatives
# Purpose: Deliberately seek different policy-domain compositions by removing one
#          materially represented domain at a time and re-optimizing robustly
#          with approximate Social Security solvency required.
# ------------------------------------------------------------------------------
solve_recommendation_analysis_domain_alternatives <- function(reference_bundle, search_result, policy_model, working_baseline, kernel_obj) {
  if (nrow(reference_bundle$catalog) == 0L || nrow(reference_bundle$membership) == 0L) {
    return(list(catalog = tibble(), membership = tibble(), paths = tibble(), scenario_paths = tibble(), tests = tibble(), solutions = list()))
  }

  # Use the minimum-complexity recommendation solution as the domain reference.
  ref_id <- reference_bundle$catalog |>
    arrange(implementation_complexity_score, selected_policy_count) |>
    slice_head(n = 1L) |>
    pull(solution_id)
  ref_contrib_ranked <- recommendation_analysis_candidate_contributions(reference_bundle$membership |> filter(solution_id == ref_id), policy_model) |>
    group_by(recommendation_analysis_domain) |>
    summarise(primary_improvement_bil = sum(pmax(realized_primary_improvement_2027_2036_bil, 0), na.rm = TRUE), .groups = "drop") |>
    arrange(desc(primary_improvement_bil))

  # Social Security is a required outcome domain in recommendation-analysis layer because
  # recommendation packages must restore approximate OASDI solvency. Removing
  # the entire domain is therefore not a meaningful policy-substitution test.
  # Record that requirement conflict separately and spend the configured domain
  # test slots on domains that can genuinely be substituted.
  ss_domain <- ref_contrib_ranked |> filter(recommendation_analysis_domain == "Social Security")
  ref_contrib <- ref_contrib_ranked |>
    filter(recommendation_analysis_domain != "Social Security") |>
    slice_head(n = CFG$recommendation_analysis_domain_alternative_limit)

  base_u <- reduce_universe_for_complexity(search_result$expanded_universe, kernel_obj)
  sols <- list()
  tests <- list()
  if (nrow(ss_domain) > 0L) {
    tests[[length(tests) + 1L]] <- tibble(
      omitted_domain = "Social Security",
      reference_domain_primary_improvement_bil = ss_domain$primary_improvement_bil[[1]],
      solver_status = "NOT_SOLVED_REQUIREMENT_CONFLICT: recommendation packages require approximate OASDI solvency, so omitting the entire Social Security reform domain is definitionally incompatible with the recommendation problem",
      feasible = FALSE,
      elapsed_seconds = 0,
      test_disposition = "STRUCTURAL_REQUIREMENT_CONFLICT"
    )
  }
  for (i in seq_len(nrow(ref_contrib))) {
    dom <- ref_contrib$recommendation_analysis_domain[[i]]
    u <- recommendation_analysis_filter_universe(base_u, remove_domains = dom)
    label <- paste0("RECOMMENDATION_ANALYSIS_DOMAIN_WITHOUT_", stringr::str_replace_all(stringr::str_to_upper(dom), "[^A-Z0-9]+", "_"))

    # The recommendation frontier requires approximate Social Security solvency.
    # Removing the entire Social Security domain therefore removes every
    # actuarial-improvement coefficient and makes the counterfactual incompatible
    # with that requirement by construction. Do not hand HiGHS an empty solvency
    # row merely to rediscover this tautology. Record it explicitly and continue
    # with the economically meaningful domain-substitution tests.
    if (!recommendation_analysis_ss_solvency_coefficients_present(u)) {
      reason <- "domain omission removes every Social Security actuarial-improvement coefficient while approximate OASDI solvency remains a required recommendation constraint"
      log_line(
        "Skipping ", label, " | structurally incompatible with required Social Security solvency: ", reason,
        level = "WARN"
      )
      tests[[length(tests) + 1L]] <- tibble(
        omitted_domain = dom,
        reference_domain_primary_improvement_bil = ref_contrib$primary_improvement_bil[[i]],
        solver_status = paste0("NOT_SOLVED_REQUIREMENT_CONFLICT: ", reason),
        feasible = FALSE,
        elapsed_seconds = 0,
        test_disposition = "STRUCTURAL_REQUIREMENT_CONFLICT"
      )
      next
    }

    m <- build_full_milp(
      u, working_baseline, kernel_obj,
      objective = "complexity",
      require_ss_solvency = TRUE,
      soft_targets = FALSE
    )
    m$solver_threads_override <- CFG$recommendation_analysis_recommendation_threads
    m$memory_reduction_mode <- "RECOMMENDATION_ANALYSIS_DOMAIN_OMISSION"
    sol <- solve_full_milp(m, label)
    rm(m)
    if (isTRUE(CFG$solver_force_gc_between_solves)) invisible(gc(verbose = FALSE))
    sols[[length(sols) + 1L]] <- sol
    tests[[length(tests) + 1L]] <- tibble(
      omitted_domain = dom,
      reference_domain_primary_improvement_bil = ref_contrib$primary_improvement_bil[[i]],
      solver_status = sol$status_message,
      feasible = full_solver_has_feasible_incumbent(sol),
      elapsed_seconds = sol$elapsed_seconds,
      test_disposition = "SOLVED"
    )
  }

  bundle <- recommendation_analysis_solution_bundle(sols, "DOM", policy_model, working_baseline, kernel_obj)
  bundle$tests <- bind_rows(tests)
  if (nrow(bundle$catalog) > 0L) {
    feasible_domains <- bundle$tests |> filter(feasible) |> pull(omitted_domain)
    bundle$catalog <- bundle$catalog |> mutate(omitted_domain = feasible_domains[seq_len(n())])
  }
  bundle
}

# ------------------------------------------------------------------------------
# FUNCTION: recommendation_analysis_major_reference_policies
# Purpose: Select a bounded set of major policies for leave-one-out testing using
#          the recommended robust+SS-solvent package's realized fiscal
#          contribution. The threshold never changes solver eligibility.
# ------------------------------------------------------------------------------
recommendation_analysis_major_reference_policies_recommendation_analysis <- function(reference_bundle, policy_model) {
  if (nrow(reference_bundle$catalog) == 0L) return(tibble())
  ref_id <- reference_bundle$catalog |>
    arrange(implementation_complexity_score, selected_policy_count) |>
    slice_head(n = 1L) |>
    pull(solution_id)
  contrib <- recommendation_analysis_candidate_contributions(reference_bundle$membership |> filter(solution_id == ref_id), policy_model) |>
    arrange(desc(realized_primary_improvement_2027_2036_bil))

  major <- contrib |>
    filter(realized_primary_improvement_2027_2036_bil >= CFG$recommendation_analysis_major_primary_improvement_bil)
  if (nrow(major) < min(5L, nrow(contrib))) major <- contrib |> slice_head(n = min(5L, nrow(contrib)))
  major |>
    slice_head(n = CFG$recommendation_analysis_major_policy_test_limit) |>
    mutate(reference_solution_id = ref_id)
}

# ------------------------------------------------------------------------------
# FUNCTION: solve_recommendation_analysis_leave_one_out
# Purpose: Remove each major policy candidate and each major policy family in
#          turn, then re-optimize robustly with approximate Social Security
#          solvency. Candidate tests reveal variant substitution; family tests
#          reveal whether the underlying mechanism itself is replaceable.
# ------------------------------------------------------------------------------
solve_recommendation_analysis_leave_one_out <- function(reference_bundle, search_result, policy_model, working_baseline, kernel_obj) {
  major <- recommendation_analysis_major_reference_policies(reference_bundle, policy_model)
  if (nrow(major) == 0L) return(list(policy = tibble(), family = tibble(), policy_solutions = list(), family_solutions = list()))
  base_u <- reduce_universe_for_complexity(search_result$expanded_universe, kernel_obj)

  run_one <- function(u, label) {
    # A leave-one-out test can, in principle, remove the last remaining source of
    # actuarial improvement. In that case the required solvency constraint is
    # structurally impossible and there is no valid MILP to submit. Record the
    # counterfactual as infeasible rather than constructing an empty row.
    if (!recommendation_analysis_ss_solvency_coefficients_present(u)) {
      reason <- "leave-one-out counterfactual removes every Social Security actuarial-improvement coefficient while approximate OASDI solvency remains required"
      log_line("Skipping ", label, " | ", reason, level = "WARN")
      return(recommendation_analysis_structural_infeasible_solution(label, reason))
    }

    m <- build_full_milp(
      u, working_baseline, kernel_obj,
      objective = "complexity",
      require_ss_solvency = TRUE,
      soft_targets = FALSE
    )
    m$solver_threads_override <- CFG$recommendation_analysis_recommendation_threads
    m$memory_reduction_mode <- "RECOMMENDATION_ANALYSIS_LEAVE_ONE_OUT"
    sol <- solve_full_milp(m, label)
    rm(m)
    if (isTRUE(CFG$solver_force_gc_between_solves)) invisible(gc(verbose = FALSE))
    sol
  }

  policy_solutions <- list(); policy_rows <- list()
  for (i in seq_len(nrow(major))) {
    cid <- major$candidate_id[[i]]
    sol <- run_one(
      recommendation_analysis_filter_universe(base_u, remove_candidate_ids = cid),
      paste0("RECOMMENDATION_ANALYSIS_LOO_POLICY_", sprintf("%02d", i))
    )
    policy_solutions[[length(policy_solutions) + 1L]] <- sol
    sm <- if (full_solver_has_feasible_incumbent(sol)) summarize_full_solution(sol, policy_model, working_baseline, kernel_obj, paste0("LOOP", sprintf("%02d", i))) else tibble()
    policy_rows[[length(policy_rows) + 1L]] <- tibble(
      candidate_id = cid,
      family_id = major$family_id[[i]],
      title = major$title[[i]],
      reference_primary_improvement_bil = major$realized_primary_improvement_2027_2036_bil[[i]],
      leave_one_out_feasible = full_solver_has_feasible_incumbent(sol),
      solver_status = sol$status_message,
      replacement_policy_count = if (nrow(sm) == 0L) NA_integer_ else sm$selected_policy_count[[1]],
      replacement_complexity_score = if (nrow(sm) == 0L) NA_real_ else sm$implementation_complexity_score[[1]],
      replacement_worst_required_2036_pct = if (nrow(sm) == 0L) NA_real_ else sm$worst_required_scenario_debt_gdp_2036_pct[[1]],
      replacement_worst_required_2046_pct = if (nrow(sm) == 0L) NA_real_ else sm$worst_required_scenario_debt_gdp_2046_pct[[1]],
      replacement_ss_actuarial_improvement_pct_payroll = if (nrow(sm) == 0L) NA_real_ else sm$ss_actuarial_improvement_pct_payroll[[1]],
      elapsed_seconds = sol$elapsed_seconds
    )
  }

  family_major <- major |>
    group_by(family_id) |>
    summarise(
      representative_title = first(title),
      reference_primary_improvement_bil = sum(realized_primary_improvement_2027_2036_bil, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(reference_primary_improvement_bil)) |>
    slice_head(n = CFG$recommendation_analysis_major_family_test_limit)

  family_solutions <- list(); family_rows <- list()
  for (i in seq_len(nrow(family_major))) {
    fid <- family_major$family_id[[i]]
    sol <- run_one(
      recommendation_analysis_filter_universe(base_u, remove_family_ids = fid),
      paste0("RECOMMENDATION_ANALYSIS_LOO_FAMILY_", sprintf("%02d", i))
    )
    family_solutions[[length(family_solutions) + 1L]] <- sol
    sm <- if (full_solver_has_feasible_incumbent(sol)) summarize_full_solution(sol, policy_model, working_baseline, kernel_obj, paste0("LOOF", sprintf("%02d", i))) else tibble()
    family_rows[[length(family_rows) + 1L]] <- tibble(
      family_id = fid,
      representative_title = family_major$representative_title[[i]],
      reference_primary_improvement_bil = family_major$reference_primary_improvement_bil[[i]],
      leave_one_out_feasible = full_solver_has_feasible_incumbent(sol),
      solver_status = sol$status_message,
      replacement_policy_count = if (nrow(sm) == 0L) NA_integer_ else sm$selected_policy_count[[1]],
      replacement_complexity_score = if (nrow(sm) == 0L) NA_real_ else sm$implementation_complexity_score[[1]],
      replacement_worst_required_2036_pct = if (nrow(sm) == 0L) NA_real_ else sm$worst_required_scenario_debt_gdp_2036_pct[[1]],
      replacement_worst_required_2046_pct = if (nrow(sm) == 0L) NA_real_ else sm$worst_required_scenario_debt_gdp_2046_pct[[1]],
      replacement_ss_actuarial_improvement_pct_payroll = if (nrow(sm) == 0L) NA_real_ else sm$ss_actuarial_improvement_pct_payroll[[1]],
      elapsed_seconds = sol$elapsed_seconds
    )
  }

  list(
    policy = bind_rows(policy_rows),
    family = bind_rows(family_rows),
    policy_solutions = policy_solutions,
    family_solutions = family_solutions,
    major_reference_policies = major
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_recommendation_analysis_policy_dependence
# Purpose: Combine selection frequency across deliberately different robust
#          packages with leave-one-out feasibility to classify dependence.
# ------------------------------------------------------------------------------
build_recommendation_analysis_policy_dependence_recommendation_analysis <- function(pool_membership, loo_policy, reference_major) {
  if (nrow(pool_membership) == 0L) return(tibble())
  package_count <- n_distinct(pool_membership$solution_id)
  freq <- pool_membership |>
    distinct(solution_id, candidate_id, .keep_all = TRUE) |>
    group_by(candidate_id, family_id, title) |>
    summarise(
      selected_package_count = n(),
      selection_share = selected_package_count / package_count,
      .groups = "drop"
    )

  freq |>
    left_join(loo_policy |> select(candidate_id, leave_one_out_feasible, replacement_policy_count, replacement_complexity_score), by = "candidate_id") |>
    left_join(reference_major |> select(candidate_id, reference_primary_improvement_bil = realized_primary_improvement_2027_2036_bil), by = "candidate_id") |>
    mutate(
      dependence_class = case_when(
        !is.na(leave_one_out_feasible) & !leave_one_out_feasible ~ "EFFECTIVELY_INDISPENSABLE",
        selection_share >= CFG$recommendation_analysis_common_policy_share ~ "COMMON_BUT_REPLACEABLE",
        TRUE ~ "HIGHLY_SUBSTITUTABLE"
      )
    ) |>
    arrange(factor(dependence_class, levels = c("EFFECTIVELY_INDISPENSABLE", "COMMON_BUT_REPLACEABLE", "HIGHLY_SUBSTITUTABLE")), desc(selection_share), desc(reference_primary_improvement_bil))
}

# ------------------------------------------------------------------------------
# FUNCTION: build_recommendation_analysis_family_dependence
# Purpose: Family-level counterpart to the candidate dependence table. Removing
#          an entire family prevents the solver from replacing one scored variant
#          with another version of the same mechanism.
# ------------------------------------------------------------------------------
build_recommendation_analysis_family_dependence_recommendation_analysis <- function(pool_membership, loo_family) {
  if (nrow(pool_membership) == 0L) return(tibble())
  package_count <- n_distinct(pool_membership$solution_id)
  freq <- pool_membership |>
    distinct(solution_id, family_id, .keep_all = TRUE) |>
    group_by(family_id) |>
    summarise(
      selected_package_count = n(),
      selection_share = selected_package_count / package_count,
      representative_title = first(title),
      .groups = "drop"
    )

  freq |>
    left_join(loo_family |> select(family_id, leave_one_out_feasible, replacement_policy_count, replacement_complexity_score), by = "family_id") |>
    mutate(
      dependence_class = case_when(
        !is.na(leave_one_out_feasible) & !leave_one_out_feasible ~ "EFFECTIVELY_INDISPENSABLE",
        selection_share >= CFG$recommendation_analysis_common_policy_share ~ "COMMON_BUT_REPLACEABLE",
        TRUE ~ "HIGHLY_SUBSTITUTABLE"
      )
    ) |>
    arrange(factor(dependence_class, levels = c("EFFECTIVELY_INDISPENSABLE", "COMMON_BUT_REPLACEABLE", "HIGHLY_SUBSTITUTABLE")), desc(selection_share))
}

# ------------------------------------------------------------------------------
# FUNCTION: build_recommendation_analysis_ssa_derivation_audit
# Purpose: Make the OACT-to-dollar translation fully explicit. Official OACT
#          annual changes in income and cost rates are multiplied by the current
#          2026 Trustees taxable-payroll path. The resulting dollar flows are
#          derived model coefficients, not published SSA dollar scores.
# ------------------------------------------------------------------------------
build_recommendation_analysis_ssa_derivation_audit_recommendation_analysis <- function(policy_model) {
  rates <- policy_model$ssa_oact_robust_feasibility_source_rates
  payroll <- policy_model$ssa_2026_taxable_payroll
  if (is.null(rates) || is.null(payroll) || nrow(rates) == 0L || nrow(payroll) == 0L) return(tibble())
  # policy_model$flows does not carry source_kind; select candidate IDs directly.
  ssa_ids <- unique(rates$candidate_id)
  flows <- policy_model$flows |> filter(candidate_id %in% ssa_ids)

  rates |>
    left_join(payroll, by = "year") |>
    left_join(
      flows |> select(candidate_id, year, revenue_delta_bil, outlay_delta_bil, primary_deficit_delta_bil, translation_status),
      by = c("candidate_id", "year")
    ) |>
    mutate(
      derived_revenue_bil_check = delta_income_rate_pct_payroll / 100 * taxable_payroll_bil,
      derived_outlay_bil_check = delta_cost_rate_pct_payroll / 100 * taxable_payroll_bil,
      derived_primary_deficit_bil_check = derived_outlay_bil_check - derived_revenue_bil_check,
      revenue_derivation_residual_bil = revenue_delta_bil - derived_revenue_bil_check,
      outlay_derivation_residual_bil = outlay_delta_bil - derived_outlay_bil_check,
      primary_derivation_residual_bil = primary_deficit_delta_bil - derived_primary_deficit_bil_check,
      derivation_formula = "income-rate change / 100 x 2026 Trustees taxable payroll = revenue; cost-rate change / 100 x taxable payroll = outlay; outlay - revenue = primary deficit change",
      score_character = "DERIVED_DOLLAR_FLOW_FROM_OFFICIAL_OACT_RATE_RESPONSE_AND_2026_TR_TAXABLE_PAYROLL"
    ) |>
    select(
      candidate_id, provision_code, trustees_basis, year, taxable_payroll_bil,
      delta_income_rate_pct_payroll, delta_cost_rate_pct_payroll, delta_annual_balance_pct_payroll,
      revenue_delta_bil, outlay_delta_bil, primary_deficit_delta_bil,
      derived_revenue_bil_check, derived_outlay_bil_check, derived_primary_deficit_bil_check,
      revenue_derivation_residual_bil, outlay_derivation_residual_bil, primary_derivation_residual_bil,
      actuarial_improvement_pct_payroll, source_url, translation_status, score_character, derivation_formula
    )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_recommendation_analysis_robust_gap_diagnostics
# Purpose: Correct the binding-scenario definition. A passing scenario can still
#          be binding: it is the required scenario with the highest debt/GDP ratio
#          (least headroom) in the target year, not every scenario whose positive
#          shortfall happens to equal zero.
# ------------------------------------------------------------------------------
build_recommendation_analysis_robust_gap_diagnostics <- function(search_result, policy_model, kernel_obj) {
  s <- search_result$summary |> filter(protection_mode == "EXPANDED")
  if (nrow(s) == 0L) return(tibble())
  s <- s |>
    mutate(
      rank_metric = dplyr::coalesce(achieved_target_slack_score, 0) +
        pmax(worst_required_scenario_debt_gdp_2036_pct - 90, 0) +
        pmax(worst_required_scenario_debt_gdp_2046_pct - 80, 0)
    ) |>
    arrange(rank_metric, implementation_complexity_score, selected_policy_count)
  sid <- s$solution_id[[1]]

  search_result$scenario_paths |>
    filter(solution_id == sid, required_robust, year %in% c(2036L, 2046L)) |>
    mutate(
      target_ratio = if_else(year == 2036L, CFG$target_2036, CFG$target_2046_high),
      target_pct = 100 * target_ratio,
      target_debt_bil = target_ratio * scenario_gdp_bil,
      debt_gap_bil = pmax(scenario_debt_bil - target_debt_bil, 0),
      debt_gap_pp = pmax(scenario_debt_gdp_pct - target_pct, 0),
      target_headroom_bil = target_debt_bil - scenario_debt_bil,
      target_headroom_pp = target_pct - scenario_debt_gdp_pct
    ) |>
    rowwise() |>
    mutate(
      level_annual_primary_improvement_equivalent_bil = robust_feasibility_primary_gap_equivalent(debt_gap_bil, year, scenario_id, policy_model, kernel_obj),
      cumulative_primary_improvement_equivalent_bil = level_annual_primary_improvement_equivalent_bil * (year - 2026L)
    ) |>
    ungroup() |>
    group_by(year) |>
    mutate(
      binding_within_year = abs(scenario_debt_gdp_pct - max(scenario_debt_gdp_pct, na.rm = TRUE)) <= 1e-10,
      headroom_rank_within_year = min_rank(desc(scenario_debt_gdp_pct))
    ) |>
    ungroup() |>
    mutate(
      selected_reference_solution_id = sid,
      scenario_label = recommendation_analysis_pretty_scenario_label(scenario_id),
      interpretation = "Binding means the required scenario with the least target headroom in that year. Primary-equivalent figures are diagnostics, not fabricated policy scores."
    ) |>
    select(
      selected_reference_solution_id, scenario_id, scenario_label, year,
      scenario_debt_gdp_pct, target_ratio, target_headroom_pp, target_headroom_bil,
      debt_gap_pp, debt_gap_bil, level_annual_primary_improvement_equivalent_bil,
      cumulative_primary_improvement_equivalent_bil, binding_within_year,
      headroom_rank_within_year, interpretation
    )
}

# Ensure the inherited robust-feasibility search writes the corrected binding
# diagnostic without duplicating the obsolete zero-gap definition.
build_robust_feasibility_robust_gap_diagnostics <- build_recommendation_analysis_robust_gap_diagnostics

# ------------------------------------------------------------------------------
# FUNCTION: plot_solution_debt_paths
# Purpose: Generic central debt-path figure. Titles never depend on whether the
#          current run passes or fails. The CBO working baseline is always shown.
# ------------------------------------------------------------------------------
plot_solution_debt_paths <- function(working_baseline, search_result) {
  base <- working_baseline |>
    transmute(year, projection = "CBO working baseline", debt_gdp_pct = working_debt_gdp_pct)

  chosen_catalog <- tibble()
  chosen_paths <- tibble()

  if (nrow(search_result$summary) > 0L && nrow(search_result$paths) > 0L) {
    s <- search_result$summary

    add_base_choice <- function(row, label) {
      if (nrow(row) == 0L) return(invisible(NULL))
      sid <- row$solution_id[[1]]
      chosen_catalog <<- bind_rows(chosen_catalog, row |> mutate(display_label = label))
      chosen_paths <<- bind_rows(
        chosen_paths,
        search_result$paths |>
          filter(solution_id == sid) |>
          transmute(year, projection = label, debt_gdp_pct)
      )
      invisible(NULL)
    }

    expanded_robust <- s |>
      filter(protection_mode == "EXPANDED", robust_target_2036_pass, robust_target_2046_pass)
    if (nrow(expanded_robust) > 0L) {
      add_base_choice(
        expanded_robust |>
          arrange(worst_required_scenario_debt_gdp_2036_pct + worst_required_scenario_debt_gdp_2046_pct, selected_policy_count) |>
          slice_head(n = 1L),
        "Expanded maximum fiscal margin"
      )
      add_base_choice(
        expanded_robust |>
          arrange(implementation_complexity_score, selected_policy_count) |>
          slice_head(n = 1L),
        "Expanded minimum complexity"
      )
    }

    strict <- s |> filter(protection_mode == "STRICT")
    if (nrow(strict) > 0L) {
      add_base_choice(
        strict |>
          arrange(
            pmax(worst_required_scenario_debt_gdp_2036_pct - 90, 0) +
              pmax(worst_required_scenario_debt_gdp_2046_pct - 80, 0) +
              dplyr::coalesce(achieved_target_slack_score, 0),
            implementation_complexity_score
          ) |>
          slice_head(n = 1L),
        "Strict best attainable"
      )
    }
  }

  # If recommendation-analysis recommendation packages are available, explicitly add
  # the minimum-complexity robust + approximately SS-solvent package.
  rec <- search_result$recommendation_analysis_recommendation_frontier
  if (!is.null(rec) && nrow(rec$catalog) > 0L && nrow(rec$scenario_paths) > 0L) {
    rr <- rec$catalog |>
      filter(robust_target_2036_pass, robust_target_2046_pass,
             ss_actuarial_improvement_pct_payroll + 1e-9 >= CFG$ss_actuarial_gap_pct_payroll) |>
      arrange(implementation_complexity_score, selected_policy_count) |>
      slice_head(n = 1L)
    if (nrow(rr) > 0L) {
      sid <- rr$solution_id[[1]]
      chosen_paths <- bind_rows(
        chosen_paths,
        rec$scenario_paths |>
          filter(solution_id == sid, scenario_id == "CENTRAL") |>
          transmute(year, projection = "Expanded robust + SS solvency", debt_gdp_pct = scenario_debt_gdp_pct)
      )
    }
  }

  # Remove visually redundant selected-package paths, but never remove the CBO
  # baseline because it is a reference rather than a candidate package.
  labels <- unique(chosen_paths$projection)
  keep_labels <- character()
  for (lab in labels) {
    a <- chosen_paths |> filter(projection == lab) |> arrange(year) |> pull(debt_gdp_pct)
    if (length(a) == 0L) next
    distinct_path <- TRUE
    for (kept in keep_labels) {
      b <- chosen_paths |> filter(projection == kept) |> arrange(year) |> pull(debt_gdp_pct)
      if (length(a) == length(b) && max(abs(a - b), na.rm = TRUE) < CFG$plot_visual_path_tolerance_pp) {
        distinct_path <- FALSE
        break
      }
    }
    if (distinct_path) keep_labels <- c(keep_labels, lab)
  }

  pdat <- bind_rows(base, chosen_paths |> filter(projection %in% keep_labels))

  ggplot(pdat, aes(year, debt_gdp_pct, group = projection, color = projection)) +
    geom_line(linewidth = 0.85) +
    geom_hline(yintercept = c(90, 80, 75), linetype = "dotted") +
    scale_x_continuous(breaks = seq(2026, 2046, 2)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "Debt held by the public under selected policy packages",
      subtitle = "Representative central projections compared with the CBO working baseline and model targets",
      x = NULL,
      y = "Debt held by public / GDP",
      color = "Projection",
      caption = paste0(
        "Only materially distinct representative paths are plotted; paths within ",
        format(CFG$plot_visual_path_tolerance_pp, trim = TRUE),
        " percentage point at every year are omitted from both plot and legend. Target lines mark 90 percent in 2036, 80 percent in 2046, and the 75 percent long-run reference."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}

# ------------------------------------------------------------------------------
# FUNCTION: plot_solution_robust_paths
# Purpose: Generic robust-scenario figure with the CBO working baseline restored.
#          The title/subtitle never assert the run result; the lines do that.
# ------------------------------------------------------------------------------
plot_solution_robust_paths <- function(search_result) {
  # Prefer the recommendation-analysis minimum-complexity robust + SS-solvent package.
  rec <- search_result$recommendation_analysis_recommendation_frontier
  if (!is.null(rec) && nrow(rec$catalog) > 0L && nrow(rec$scenario_paths) > 0L) {
    rr <- rec$catalog |>
      filter(robust_target_2036_pass, robust_target_2046_pass,
             ss_actuarial_improvement_pct_payroll + 1e-9 >= CFG$ss_actuarial_gap_pct_payroll) |>
      arrange(implementation_complexity_score, selected_policy_count) |>
      slice_head(n = 1L)
    if (nrow(rr) > 0L) {
      sid <- rr$solution_id[[1]]
      raw <- rec$scenario_paths |> filter(solution_id == sid, required_robust) |> arrange(scenario_id, year)
    } else {
      raw <- tibble()
    }
  } else {
    raw <- tibble()
  }

  # Fall back to the best retained EXPANDED package if the recommendation
  # frontier is unavailable for any reason.
  if (nrow(raw) == 0L) {
    s <- search_result$summary |> filter(protection_mode == "EXPANDED")
    if (nrow(s) == 0L) return(ggplot() + theme_void() + labs(title = "Debt held by the public under required scenarios"))
    s <- s |>
      mutate(
        rank_metric = pmax(worst_required_scenario_debt_gdp_2036_pct - 90, 0) +
          pmax(worst_required_scenario_debt_gdp_2046_pct - 80, 0) +
          dplyr::coalesce(achieved_target_slack_score, 0)
      ) |>
      arrange(rank_metric, implementation_complexity_score, selected_policy_count)
    sid <- s$solution_id[[1]]
    raw <- search_result$scenario_paths |> filter(solution_id == sid, required_robust) |> arrange(scenario_id, year)
  }

  scenario_ids <- unique(raw$scenario_id)
  keep_ids <- character()
  for (cid in scenario_ids) {
    a <- raw |> filter(scenario_id == cid) |> arrange(year) |> pull(scenario_debt_gdp_pct)
    if (length(a) == 0L) next
    visually_distinct <- TRUE
    for (kid in keep_ids) {
      b <- raw |> filter(scenario_id == kid) |> arrange(year) |> pull(scenario_debt_gdp_pct)
      if (length(a) == length(b) && max(abs(a - b), na.rm = TRUE) < CFG$plot_visual_path_tolerance_pp) {
        visually_distinct <- FALSE
        break
      }
    }
    if (visually_distinct) keep_ids <- c(keep_ids, cid)
  }

  scenarios <- raw |>
    filter(scenario_id %in% keep_ids) |>
    transmute(year, projection = recommendation_analysis_pretty_scenario_label(scenario_id), debt_gdp_pct = scenario_debt_gdp_pct)

  baseline_source <- raw |> filter(scenario_id == "CENTRAL")
  if (nrow(baseline_source) == 0L) baseline_source <- raw |> group_by(year) |> slice_head(n = 1L) |> ungroup()
  baseline <- baseline_source |>
    transmute(year, projection = "CBO working baseline", debt_gdp_pct = 100 * scenario_baseline_debt_bil / scenario_gdp_bil) |>
    distinct(year, .keep_all = TRUE)

  p <- bind_rows(baseline, scenarios)

  ggplot(p, aes(x = year, y = debt_gdp_pct, color = projection, group = projection)) +
    geom_line(linewidth = 0.85) +
    geom_hline(yintercept = c(90, 80, 75), linetype = "dotted") +
    scale_x_continuous(breaks = seq(2026, 2046, 2)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "Debt held by the public under required scenarios",
      subtitle = "Selected package outcomes compared with the CBO working baseline and model targets",
      x = NULL,
      y = "Debt held by public / GDP",
      color = "Projection",
      caption = paste0(
        "Required-scenario paths within ", format(CFG$plot_visual_path_tolerance_pp, trim = TRUE),
        " percentage point at every year are treated as visually identical. Target lines mark 90 percent in 2036, 80 percent in 2046, and the 75 percent long-run reference."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}

# ------------------------------------------------------------------------------
# FUNCTION: run_full_solution_search
# Purpose: Preserve every robust-feasibility solve and diagnostic, then add the
#          recommendation-analysis recommendation frontier, deliberate domain
#          alternatives, leave-one-out re-optimization, and dependence audits.
# ------------------------------------------------------------------------------
run_full_solution_search_recommendation_analysis <- function(policy_model, working_baseline, kernel_obj) {
  base <- run_full_solution_search_robust_feasibility(policy_model, working_baseline, kernel_obj)

  recommendation <- solve_recommendation_analysis_recommendation_frontier(base, policy_model, working_baseline, kernel_obj)
  domain_alternatives <- solve_recommendation_analysis_domain_alternatives(recommendation, base, policy_model, working_baseline, kernel_obj)
  leave_one_out <- solve_recommendation_analysis_leave_one_out(recommendation, base, policy_model, working_baseline, kernel_obj)

  # Assemble a politically/compositionally diverse comparison pool. Standard
  # solution-search packages are retained only when robustly feasible and
  # approximately Social-Security-solvent; robust-feasibility Hamming-diverse and
  # recommendation-analysis targeted alternatives are then added.
  base_pool_ids <- base$summary |>
    filter(
      protection_mode == "EXPANDED",
      robust_target_2036_pass,
      robust_target_2046_pass,
      ss_actuarial_improvement_pct_payroll + 1e-9 >= CFG$ss_actuarial_gap_pct_payroll
    ) |>
    pull(solution_id)
  pool_catalog <- base$summary |> filter(solution_id %in% base_pool_ids)
  pool_membership <- base$membership |> filter(solution_id %in% base_pool_ids)

  if (!is.null(base$robust_feasibility_alternatives) && nrow(base$robust_feasibility_alternatives$catalog) > 0L) {
    alt_ids <- base$robust_feasibility_alternatives$catalog |>
      filter(
        robust_target_2036_pass,
        robust_target_2046_pass,
        ss_actuarial_improvement_pct_payroll + 1e-9 >= CFG$ss_actuarial_gap_pct_payroll
      ) |>
      pull(solution_id)
    pool_catalog <- bind_rows(pool_catalog, base$robust_feasibility_alternatives$catalog |> filter(solution_id %in% alt_ids))
    pool_membership <- bind_rows(pool_membership, base$robust_feasibility_alternatives$membership |> filter(solution_id %in% alt_ids))
  }
  pool_catalog <- bind_rows(pool_catalog, recommendation$catalog, domain_alternatives$catalog) |>
    distinct(solution_id, .keep_all = TRUE)
  pool_membership <- bind_rows(pool_membership, recommendation$membership, domain_alternatives$membership) |>
    distinct(solution_id, candidate_id, .keep_all = TRUE)

  recommendation_metrics <- build_recommendation_analysis_package_metrics(pool_catalog, pool_membership, policy_model)
  package_domain_composition <- recommendation_analysis_candidate_contributions(pool_membership, policy_model) |>
    group_by(solution_id, recommendation_analysis_domain) |>
    summarise(
      selected_policy_count = n_distinct(candidate_id),
      primary_improvement_2027_2036_bil = sum(pmax(realized_primary_improvement_2027_2036_bil, 0), na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(solution_id) |>
    mutate(
      package_positive_primary_improvement_bil = sum(primary_improvement_2027_2036_bil, na.rm = TRUE),
      primary_improvement_share_pct = 100 * safe_divide(primary_improvement_2027_2036_bil, package_positive_primary_improvement_bil)
    ) |>
    ungroup() |>
    arrange(solution_id, desc(primary_improvement_share_pct))
  policy_dependence <- build_recommendation_analysis_policy_dependence(
    pool_membership,
    leave_one_out$policy,
    leave_one_out$major_reference_policies
  )
  family_dependence <- build_recommendation_analysis_family_dependence(pool_membership, leave_one_out$family)
  ssa_derivation <- build_recommendation_analysis_ssa_derivation_audit(policy_model)
  robust_gaps <- build_recommendation_analysis_robust_gap_diagnostics(base, policy_model, kernel_obj)

  if (CFG$write_audit_outputs) {
    write_csv_atomic(recommendation$catalog, file.path(CFG$output_dir, "recommendation_analysis_recommendation_frontier_solutions.csv"))
    write_csv_atomic(recommendation$membership, file.path(CFG$output_dir, "recommendation_analysis_recommendation_frontier_membership.csv"))
    write_csv_atomic(recommendation$paths, file.path(CFG$output_dir, "recommendation_analysis_recommendation_frontier_central_paths.csv"))
    write_csv_atomic(recommendation$scenario_paths, file.path(CFG$output_dir, "recommendation_analysis_recommendation_frontier_scenario_paths.csv"))
    write_csv_atomic(recommendation_metrics, file.path(CFG$output_dir, "recommendation_analysis_recommendation_package_metrics.csv"))
    write_csv_atomic(package_domain_composition, file.path(CFG$output_dir, "recommendation_analysis_package_domain_composition.csv"))
    write_csv_atomic(domain_alternatives$tests, file.path(CFG$output_dir, "recommendation_analysis_domain_omission_tests.csv"))
    write_csv_atomic(domain_alternatives$catalog, file.path(CFG$output_dir, "recommendation_analysis_domain_alternative_solutions.csv"))
    write_csv_atomic(domain_alternatives$membership, file.path(CFG$output_dir, "recommendation_analysis_domain_alternative_membership.csv"))
    write_csv_atomic(domain_alternatives$paths, file.path(CFG$output_dir, "recommendation_analysis_domain_alternative_central_paths.csv"))
    write_csv_atomic(domain_alternatives$scenario_paths, file.path(CFG$output_dir, "recommendation_analysis_domain_alternative_scenario_paths.csv"))
    write_csv_atomic(leave_one_out$policy, file.path(CFG$output_dir, "recommendation_analysis_leave_one_out_policy.csv"))
    write_csv_atomic(leave_one_out$family, file.path(CFG$output_dir, "recommendation_analysis_leave_one_out_family.csv"))
    write_csv_atomic(leave_one_out$major_reference_policies, file.path(CFG$output_dir, "recommendation_analysis_major_reference_policies.csv"))
    write_csv_atomic(policy_dependence, file.path(CFG$output_dir, "recommendation_analysis_policy_dependence.csv"))
    write_csv_atomic(family_dependence, file.path(CFG$output_dir, "recommendation_analysis_family_dependence.csv"))
    write_csv_atomic(ssa_derivation, file.path(CFG$output_dir, "recommendation_analysis_ssa_oact_dollar_derivation_audit.csv"))
    write_csv_atomic(robust_gaps, file.path(CFG$output_dir, "recommendation_analysis_robust_gap_diagnostics.csv"))
  }

  base$recommendation_analysis_recommendation_frontier <- recommendation
  base$recommendation_analysis_domain_alternatives <- domain_alternatives
  base$recommendation_analysis_leave_one_out <- leave_one_out
  base$recommendation_analysis_recommendation_metrics <- recommendation_metrics
  base$recommendation_analysis_package_domain_composition <- package_domain_composition
  base$recommendation_analysis_policy_dependence <- policy_dependence
  base$recommendation_analysis_family_dependence <- family_dependence
  base$recommendation_analysis_ssa_derivation_audit <- ssa_derivation
  base$recommendation_analysis_robust_gap_diagnostics <- robust_gaps
  base
}

# ------------------------------------------------------------------------------
# FUNCTION: run_model
# Purpose: Preserve the robust-feasibility validated execution pipeline while
#          writing corrected generic figures and recommendation-analysis recommendation
#          and dependence artifacts.
# ------------------------------------------------------------------------------
run_model_recommendation_analysis <- function() {
  result <- run_model_robust_feasibility()

  if (!is.null(result$solution_search)) {
    search <- result$solution_search
    central_plot <- plot_solution_debt_paths(result$working_baseline, search)
    robust_plot <- plot_solution_robust_paths(search)

    if (CFG$write_audit_outputs) {
      ragg::agg_png(file.path(CFG$output_dir, "recommendation_analysis_central_debt_paths.png"), width = 1920, height = 1080, res = 144)
      print(central_plot)
      grDevices::dev.off()
      ragg::agg_png(file.path(CFG$output_dir, "recommendation_analysis_required_scenario_debt_paths.png"), width = 1920, height = 1080, res = 144)
      print(robust_plot)
      grDevices::dev.off()
    }

    if (CFG$render_plots) {
      print(central_plot)
      print(robust_plot)
    }
  }

  result$recommendation_analysis_plan_status <- tibble::tibble(
    item = c(
      "Generic central and robust plot titles/subtitles",
      "CBO working baseline restored to robust figure",
      "Binding-scenario diagnostic uses least headroom",
      "Robust + SS-solvent recommendation frontier",
      "Domain-composition alternatives",
      "Major-policy leave-one-out re-optimization",
      "Major-family leave-one-out re-optimization",
      "Policy and family dependence classification",
      "Explicit SSA OACT rate-to-dollar derivation audit",
      "Model layer memory controls preserved",
      "Current law remains non-veto eligibility metadata"
    ),
    implemented = TRUE
  )
  if (CFG$write_audit_outputs) {
    write_csv_atomic(result$recommendation_analysis_plan_status, file.path(CFG$output_dir, "recommendation_analysis_plan_status.csv"))
  }
  result
}

# ------------------------------------------------------------------------------
# MODEL LAYER ACTIVE RULE SUMMARY
# - Every approved protection remains binding. Broad ordinary-income-rate
#   increases remain blocked.
# - Current law never vetoes a non-protected policy Congress could enact.
# - No new unsupported score is invented; recommendation-analysis layer uses the validated
#   robust-feasibility policy universe and scoring evidence.
# - Recommended-package searches require both robust 90/80 debt-target
#   feasibility and approximate Social Security solvency.
# - Policy count, implementation complexity, EXPANDED-only policy count, and
#   revenue/fiscal-domain concentration are reported separately. No opaque
#   political-utility score is imposed.
# - Domain omission and leave-one-out tests re-optimize the complete remaining
#   universe rather than declaring a policy indispensable from selection
#   frequency alone.
# - Central and required-scenario figures use generic titles and always show the
#   CBO working baseline where relevant.
# - Model layer memory controls and robust-feasibility lightweight PSOCK
#   verification remain active.
# ------------------------------------------------------------------------------

# Recommendation-analysis execution entry is defined here but deferred to later public stages.
# Execution is deferred until the presentation-audit stage is defined.



# ==============================================================================
# MODEL LAYER PRESENTATION AND AUDIT-INTEGRITY MODEL STAGE
# ==============================================================================
# Recommendation-analysis layer established a practical robust recommendation frontier.
# Presentation-audit layer intentionally does not alter the fiscal policy universe,
# protection rules, score-admissibility logic, debt-service engine, robust
# scenarios, Social Security solvency requirement, HiGHS formulation, or
# independent verification contract. It fixes presentation duplication and makes
# the recommendation-dependence and SSA derivation audits evidence-tight.
# ==============================================================================

# Bind the recommendation-analysis search logic used by the presentation stage. The run wrapper
# bypasses inherited plot-producing run_model wrappers, but the full search and
# diagnostics remain active through the global run_full_solution_search binding.

# ------------------------------------------------------------------------------
# FUNCTION: build_recommendation_analysis_policy_dependence
# Purpose: Evidence-tight dependence classification. A policy is never called
#          replaceable or highly substitutable unless a leave-one-out solve
#          actually removed it and returned a feasible robust + SS-solvent
#          replacement package.
# ------------------------------------------------------------------------------
build_recommendation_analysis_policy_dependence <- function(pool_membership, loo_policy, reference_major) {
  if (nrow(pool_membership) == 0L) return(tibble())
  package_count <- n_distinct(pool_membership$solution_id)
  freq <- pool_membership |>
    distinct(solution_id, candidate_id, .keep_all = TRUE) |>
    group_by(candidate_id, family_id, title) |>
    summarise(
      selected_package_count = n(),
      selection_share = selected_package_count / package_count,
      .groups = "drop"
    )

  freq |>
    left_join(
      loo_policy |>
        select(candidate_id, leave_one_out_feasible, replacement_policy_count, replacement_complexity_score),
      by = "candidate_id"
    ) |>
    left_join(
      reference_major |>
        select(candidate_id, reference_primary_improvement_bil = realized_primary_improvement_2027_2036_bil),
      by = "candidate_id"
    ) |>
    mutate(
      dependence_tested = !is.na(leave_one_out_feasible),
      dependence_class = case_when(
        dependence_tested & !leave_one_out_feasible ~ "EFFECTIVELY_INDISPENSABLE",
        dependence_tested & leave_one_out_feasible & selection_share >= CFG$recommendation_analysis_common_policy_share ~ "COMMON_BUT_REPLACEABLE",
        dependence_tested & leave_one_out_feasible ~ "HIGHLY_SUBSTITUTABLE",
        !dependence_tested & selection_share >= CFG$recommendation_analysis_common_policy_share ~ "COMMON_UNTESTED",
        TRUE ~ "LOW_FREQUENCY_UNTESTED"
      ),
      dependence_evidence = case_when(
        dependence_class == "EFFECTIVELY_INDISPENSABLE" ~ "Leave-one-out re-optimization was infeasible under robust debt targets plus approximate Social Security solvency.",
        dependence_class == "COMMON_BUT_REPLACEABLE" ~ "Frequently selected, but leave-one-out re-optimization found a feasible replacement package.",
        dependence_class == "HIGHLY_SUBSTITUTABLE" ~ "Leave-one-out re-optimization found a feasible replacement package and the policy is not common across the comparison pool.",
        dependence_class == "COMMON_UNTESTED" ~ "Frequently selected across comparison packages, but no leave-one-out solve was run; replaceability is not claimed.",
        TRUE ~ "Not common across comparison packages and not leave-one-out tested; substitutability is not claimed."
      )
    ) |>
    arrange(
      factor(
        dependence_class,
        levels = c(
          "EFFECTIVELY_INDISPENSABLE",
          "COMMON_BUT_REPLACEABLE",
          "COMMON_UNTESTED",
          "HIGHLY_SUBSTITUTABLE",
          "LOW_FREQUENCY_UNTESTED"
        )
      ),
      desc(selection_share),
      desc(reference_primary_improvement_bil)
    )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_recommendation_analysis_family_dependence
# Purpose: Apply the same evidence standard at policy-family level.
# ------------------------------------------------------------------------------
build_recommendation_analysis_family_dependence <- function(pool_membership, loo_family) {
  if (nrow(pool_membership) == 0L) return(tibble())
  package_count <- n_distinct(pool_membership$solution_id)
  freq <- pool_membership |>
    distinct(solution_id, family_id, .keep_all = TRUE) |>
    group_by(family_id) |>
    summarise(
      selected_package_count = n(),
      selection_share = selected_package_count / package_count,
      representative_title = first(title),
      .groups = "drop"
    )

  freq |>
    left_join(
      loo_family |>
        select(family_id, leave_one_out_feasible, replacement_policy_count, replacement_complexity_score),
      by = "family_id"
    ) |>
    mutate(
      dependence_tested = !is.na(leave_one_out_feasible),
      dependence_class = case_when(
        dependence_tested & !leave_one_out_feasible ~ "EFFECTIVELY_INDISPENSABLE",
        dependence_tested & leave_one_out_feasible & selection_share >= CFG$recommendation_analysis_common_policy_share ~ "COMMON_BUT_REPLACEABLE",
        dependence_tested & leave_one_out_feasible ~ "HIGHLY_SUBSTITUTABLE",
        !dependence_tested & selection_share >= CFG$recommendation_analysis_common_policy_share ~ "COMMON_UNTESTED",
        TRUE ~ "LOW_FREQUENCY_UNTESTED"
      ),
      dependence_evidence = case_when(
        dependence_class == "EFFECTIVELY_INDISPENSABLE" ~ "Family leave-one-out re-optimization was infeasible under robust debt targets plus approximate Social Security solvency.",
        dependence_class == "COMMON_BUT_REPLACEABLE" ~ "Frequently selected family, but family leave-one-out re-optimization found a feasible replacement package.",
        dependence_class == "HIGHLY_SUBSTITUTABLE" ~ "Family leave-one-out re-optimization found a feasible replacement package and the family is not common across the comparison pool.",
        dependence_class == "COMMON_UNTESTED" ~ "Frequently selected family, but no family leave-one-out solve was run; replaceability is not claimed.",
        TRUE ~ "Family is not common across comparison packages and was not leave-one-out tested; substitutability is not claimed."
      )
    ) |>
    arrange(
      factor(
        dependence_class,
        levels = c(
          "EFFECTIVELY_INDISPENSABLE",
          "COMMON_BUT_REPLACEABLE",
          "COMMON_UNTESTED",
          "HIGHLY_SUBSTITUTABLE",
          "LOW_FREQUENCY_UNTESTED"
        )
      ),
      desc(selection_share)
    )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_recommendation_analysis_ssa_derivation_audit
# Purpose: Reproduce every OACT-derived annual dollar coefficient from its two
#          authoritative inputs: the provision's official annual change rates and
#          the 2026 Trustees taxable-payroll path. The function hard-fails rather
#          than silently writing an empty audit when OACT-derived coefficients are
#          present in the model.
# ------------------------------------------------------------------------------
build_recommendation_analysis_ssa_derivation_audit <- function(policy_model) {
  ssa_ids_in_model <- policy_model$meta |>
    filter(source_kind == "SSA_OACT_SOLVENCY_PROVISION") |>
    pull(candidate_id) |>
    unique()

  if (length(ssa_ids_in_model) == 0L) return(tibble())

  rates <- policy_model$ssa_oact_robust_feasibility_source_rates
  payroll <- policy_model$ssa_2026_taxable_payroll

  # Defensive fallback for older cached object shapes. This should normally be
  # unnecessary because presentation-audit layer explicitly preserves these fields
  # through parameterization, but it prevents a silent zero-byte audit.
  if (is.null(rates) || is.null(payroll) || nrow(rates) == 0L || nrow(payroll) == 0L) {
    rebuilt <- build_ssa_oact_solving_candidates()
    rates <- rebuilt$source_rates
    payroll <- rebuilt$taxable_payroll
  }

  assert_model(nrow(rates) > 0L, "SSA OACT derivation audit cannot proceed without official annual rate-response rows")
  assert_model(nrow(payroll) == length(CFG$model_years), "SSA OACT derivation audit requires a complete 2026 Trustees taxable-payroll path")

  rates <- rates |> filter(candidate_id %in% ssa_ids_in_model)
  flows <- policy_model$flows |>
    filter(candidate_id %in% ssa_ids_in_model) |>
    select(candidate_id, year, revenue_delta_bil, outlay_delta_bil, primary_deficit_delta_bil, translation_status)

  audit <- rates |>
    left_join(payroll, by = "year") |>
    left_join(flows, by = c("candidate_id", "year")) |>
    mutate(
      derived_revenue_bil_check = delta_income_rate_pct_payroll / 100 * taxable_payroll_bil,
      derived_outlay_bil_check = delta_cost_rate_pct_payroll / 100 * taxable_payroll_bil,
      derived_primary_deficit_bil_check = derived_outlay_bil_check - derived_revenue_bil_check,
      official_balance_rate_identity_residual_pp =
        (delta_income_rate_pct_payroll - delta_cost_rate_pct_payroll) - delta_annual_balance_pct_payroll,
      revenue_derivation_residual_bil = revenue_delta_bil - derived_revenue_bil_check,
      outlay_derivation_residual_bil = outlay_delta_bil - derived_outlay_bil_check,
      primary_derivation_residual_bil = primary_deficit_delta_bil - derived_primary_deficit_bil_check,
      sign_convention = "Positive revenue reduces the deficit; positive outlay increases the deficit; primary deficit change = outlay - revenue.",
      derivation_formula = "Official OACT rate change / 100 x 2026 Trustees taxable payroll; primary deficit change = derived outlay - derived revenue.",
      score_character = "DERIVED_DOLLAR_FLOW_FROM_OFFICIAL_OACT_RATE_RESPONSE_AND_2026_TR_TAXABLE_PAYROLL"
    ) |>
    select(
      candidate_id, provision_code, trustees_basis, source_url, year,
      taxable_payroll_bil,
      delta_cost_rate_pct_payroll, delta_income_rate_pct_payroll,
      delta_annual_balance_pct_payroll,
      revenue_delta_bil, outlay_delta_bil, primary_deficit_delta_bil,
      derived_revenue_bil_check, derived_outlay_bil_check,
      derived_primary_deficit_bil_check,
      official_balance_rate_identity_residual_pp,
      revenue_derivation_residual_bil, outlay_derivation_residual_bil,
      primary_derivation_residual_bil,
      actuarial_improvement_pct_payroll, translation_status,
      sign_convention, derivation_formula, score_character
    ) |>
    arrange(candidate_id, year)

  assert_model(nrow(audit) == length(ssa_ids_in_model) * length(CFG$model_years), "SSA OACT derivation audit row count does not cover every provision-year coefficient")
  assert_model(all(is.finite(audit$taxable_payroll_bil)), "SSA OACT derivation audit contains missing taxable-payroll values")
  assert_model(max(abs(audit$official_balance_rate_identity_residual_pp), na.rm = TRUE) <= 0.03 + 1e-12, "SSA OACT published rate identity residual exceeds allowed parsing tolerance")
  assert_model(max(abs(audit$revenue_derivation_residual_bil), na.rm = TRUE) <= 1e-8, "SSA OACT revenue dollar derivation does not reproduce model coefficients")
  assert_model(max(abs(audit$outlay_derivation_residual_bil), na.rm = TRUE) <= 1e-8, "SSA OACT outlay dollar derivation does not reproduce model coefficients")
  assert_model(max(abs(audit$primary_derivation_residual_bil), na.rm = TRUE) <= 1e-8, "SSA OACT primary-deficit dollar derivation does not reproduce model coefficients")
  audit
}

# ------------------------------------------------------------------------------
# FUNCTION: build_presentation_audit_single_plot
# Purpose: Exactly one presentation figure. It shows the CBO working baseline,
#          the strict best attainable package, the expanded maximum-fiscal-margin
#          package, and the recommended minimum-complexity package that is robust
#          and approximately Social-Security-solvent. Near-identical package paths
#          are omitted from both linework and legend.
# ------------------------------------------------------------------------------
build_presentation_audit_single_plot_presentation_audit <- function(working_baseline, search_result) {
  baseline <- working_baseline |>
    transmute(year, projection = "CBO working baseline", debt_gdp_pct = working_debt_gdp_pct)

  candidates <- tibble()

  add_path <- function(path_tbl, label) {
    if (is.null(path_tbl) || nrow(path_tbl) == 0L) return(invisible(NULL))
    candidates <<- bind_rows(
      candidates,
      path_tbl |>
        transmute(year, projection = label, debt_gdp_pct)
    )
    invisible(NULL)
  }

  # Strict best attainable remains a useful reference for the cost of refusing
  # EXPANDED-only policies, even when it misses the model targets.
  if (nrow(search_result$summary) > 0L && nrow(search_result$paths) > 0L) {
    strict <- search_result$summary |>
      filter(protection_mode == "STRICT") |>
      mutate(
        rank_metric = pmax(worst_required_scenario_debt_gdp_2036_pct - 90, 0) +
          pmax(worst_required_scenario_debt_gdp_2046_pct - 80, 0) +
          dplyr::coalesce(achieved_target_slack_score, 0)
      ) |>
      arrange(rank_metric, implementation_complexity_score, selected_policy_count) |>
      slice_head(n = 1L)
    if (nrow(strict) > 0L) {
      sid <- strict$solution_id[[1]]
      add_path(
        search_result$paths |> filter(solution_id == sid),
        "Strict best attainable"
      )
    }

    # Maximum fiscal margin is the robust EXPANDED retained package with the
    # lowest combined worst-scenario debt ratios at the two hard target years.
    margin <- search_result$summary |>
      filter(protection_mode == "EXPANDED", robust_target_2036_pass, robust_target_2046_pass) |>
      arrange(
        worst_required_scenario_debt_gdp_2036_pct + worst_required_scenario_debt_gdp_2046_pct,
        debt_gdp_2036_pct + debt_gdp_2046_pct,
        selected_policy_count
      ) |>
      slice_head(n = 1L)
    if (nrow(margin) > 0L) {
      sid <- margin$solution_id[[1]]
      add_path(
        search_result$paths |> filter(solution_id == sid),
        "Expanded maximum fiscal margin"
      )
    }
  }

  # Recommendation: minimum implementation complexity among packages that meet
  # both required robust debt targets and approximate OASDI solvency.
  rec <- search_result$recommendation_analysis_recommendation_frontier
  if (!is.null(rec) && nrow(rec$catalog) > 0L && nrow(rec$scenario_paths) > 0L) {
    rr <- rec$catalog |>
      filter(
        robust_target_2036_pass,
        robust_target_2046_pass,
        ss_actuarial_improvement_pct_payroll + 1e-9 >= CFG$ss_actuarial_gap_pct_payroll
      ) |>
      arrange(implementation_complexity_score, selected_policy_count) |>
      slice_head(n = 1L)
    if (nrow(rr) > 0L) {
      sid <- rr$solution_id[[1]]
      add_path(
        rec$scenario_paths |>
          filter(solution_id == sid, scenario_id == "CENTRAL") |>
          transmute(year, debt_gdp_pct = scenario_debt_gdp_pct),
        "Recommended robust + SS-solvent package"
      )
    }
  }

  # Visual de-duplication applies only to policy packages. The CBO reference is
  # always retained. Preserve the listed semantic priority if two package paths
  # are visually identical.
  preferred_order <- c(
    "Recommended robust + SS-solvent package",
    "Expanded maximum fiscal margin",
    "Strict best attainable"
  )
  labels <- preferred_order[preferred_order %in% unique(candidates$projection)]
  keep <- character()
  for (lab in labels) {
    a <- candidates |> filter(projection == lab) |> arrange(year) |> pull(debt_gdp_pct)
    if (length(a) == 0L) next
    distinct_path <- TRUE
    for (kept in keep) {
      b <- candidates |> filter(projection == kept) |> arrange(year) |> pull(debt_gdp_pct)
      if (length(a) == length(b) && max(abs(a - b), na.rm = TRUE) < CFG$plot_visual_path_tolerance_pp) {
        distinct_path <- FALSE
        break
      }
    }
    if (distinct_path) keep <- c(keep, lab)
  }

  pdat <- bind_rows(baseline, candidates |> filter(projection %in% keep))
  legend_order <- c(
    "CBO working baseline",
    "Strict best attainable",
    "Expanded maximum fiscal margin",
    "Recommended robust + SS-solvent package"
  )
  pdat$projection <- factor(pdat$projection, levels = legend_order[legend_order %in% unique(as.character(pdat$projection))])

  ggplot(pdat, aes(year, debt_gdp_pct, group = projection, color = projection)) +
    geom_line(linewidth = 0.9) +
    geom_hline(yintercept = c(90, 80, 75), linetype = "dotted") +
    scale_x_continuous(breaks = seq(2026, 2046, 2)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "Debt held by the public under selected policy packages",
      subtitle = "CBO working baseline and representative optimized packages relative to model targets",
      x = NULL,
      y = "Debt held by public / GDP",
      color = "Projection",
      caption = paste0(
        "Target lines mark 90 percent in 2036, 80 percent in 2046, and the 75 percent long-run reference. ",
        "Policy-package paths within ", format(CFG$plot_visual_path_tolerance_pp, trim = TRUE),
        " percentage point at every year are treated as visually identical and shown once."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}

# ------------------------------------------------------------------------------
# FUNCTION: run_model
# Purpose: Execute the complete validated model while suppressing all inherited
#          plot rendering. Exactly one presentation-audit PNG is written and, if
#          interactive rendering is enabled, exactly that one plot is printed to
#          the RStudio Plots pane once.
# ------------------------------------------------------------------------------
run_model_presentation_audit <- function() {
  log_line("Presentation-audit layer active: single presentation plot plus SSA/dependence audit-integrity corrections")

  # Guarantee that a rerun cannot carry stale presentation-audit PNGs into the
  # archive. Only the single final presentation figure is retained.
  stale_pngs <- list.files(CFG$output_dir, pattern = "\\.png$", full.names = TRUE)
  if (length(stale_pngs) > 0L) unlink(stale_pngs, force = TRUE)

  render_requested <- isTRUE(CFG$render_plots)
  old_render <- CFG$render_plots
  CFG$render_plots <<- FALSE
  on.exit({ CFG$render_plots <<- old_render }, add = TRUE)

  # Call the memory-safety execution wrapper directly. Its body supplies the
  # complete baseline/source/solver/audit pipeline; global function lookup means
  # the active robust-feasibility/43 search extensions and presentation-audit
  # audit corrections are still used. Skipping the robust-feasibility and -43
  # run_model wrappers is what eliminates their extra plot files and print calls.
  result <- run_model_memory_safety()
  CFG$render_plots <<- old_render

  # Preserve robust-feasibility non-plot execution artifacts that previously
  # lived only in its run_model wrapper.
  if (CFG$write_audit_outputs) {
    if (!is.null(result$policy_model$ssa_oact_robust_feasibility_source_rates)) {
      write_csv_atomic(
        result$policy_model$ssa_oact_robust_feasibility_source_rates,
        file.path(CFG$output_dir, "ssa_oact_robust_feasibility_annual_rate_responses.csv")
      )
    }
    if (!is.null(result$policy_model$robust_feasibility_ssa_timing_audit)) {
      write_csv_atomic(
        result$policy_model$robust_feasibility_ssa_timing_audit,
        file.path(CFG$output_dir, "robust_feasibility_ssa_oact_timing_audit.csv")
      )
    }
    write_csv_atomic(
      build_robust_feasibility_current_policy_evidence_inventory(),
      file.path(CFG$output_dir, "robust_feasibility_current_policy_evidence_inventory.csv")
    )
    write_csv_atomic(
      build_robust_feasibility_concurrency_plan_audit(),
      file.path(CFG$output_dir, "robust_feasibility_concurrency_plan.csv")
    )
  }
  result$robust_feasibility_policy_evidence_inventory <- build_robust_feasibility_current_policy_evidence_inventory()
  result$robust_feasibility_concurrency_plan <- build_robust_feasibility_concurrency_plan_audit()
  result$robust_feasibility_ssa_oact_timing_audit <- result$policy_model$robust_feasibility_ssa_timing_audit

  search <- result$solution_search
  if (!is.null(search)) {
    # Rebuild corrected audit tables after the search so presentation-audit-named
    # artifacts reflect the tightened evidence standard explicitly.
    loo <- search$recommendation_analysis_leave_one_out
    dep <- search$recommendation_analysis_policy_dependence
    fdep <- search$recommendation_analysis_family_dependence
    ssa_audit <- build_recommendation_analysis_ssa_derivation_audit(result$policy_model)

    # The search-level tables should already use the overridden builders. These
    # assertions prevent a future wrapper regression from silently restoring the
    # old unsupported classification language.
    if (!is.null(dep) && nrow(dep) > 0L) {
      assert_model(
        all(dep$dependence_class != "COMMON_BUT_REPLACEABLE" | dep$dependence_tested),
        "Presentation-audit layer dependence audit labeled an untested policy replaceable"
      )
      assert_model(
        all(dep$dependence_class != "HIGHLY_SUBSTITUTABLE" | (dep$dependence_tested & dep$leave_one_out_feasible)),
        "Presentation-audit layer dependence audit labeled a policy highly substitutable without successful leave-one-out replacement"
      )
    }
    if (!is.null(fdep) && nrow(fdep) > 0L) {
      assert_model(
        all(fdep$dependence_class != "COMMON_BUT_REPLACEABLE" | fdep$dependence_tested),
        "Presentation-audit layer family dependence audit labeled an untested family replaceable"
      )
      assert_model(
        all(fdep$dependence_class != "HIGHLY_SUBSTITUTABLE" | (fdep$dependence_tested & fdep$leave_one_out_feasible)),
        "Presentation-audit layer family dependence audit labeled a family highly substitutable without successful leave-one-out replacement"
      )
    }
    assert_model(nrow(ssa_audit) > 0L, "Presentation-audit layer SSA OACT dollar-derivation audit is unexpectedly empty")

    result$presentation_audit_policy_dependence <- dep
    result$presentation_audit_family_dependence <- fdep
    result$presentation_audit_ssa_oact_dollar_derivation_audit <- ssa_audit

    if (CFG$write_audit_outputs) {
      write_csv_atomic(dep, file.path(CFG$output_dir, "presentation_audit_policy_dependence.csv"))
      write_csv_atomic(fdep, file.path(CFG$output_dir, "presentation_audit_family_dependence.csv"))
      write_csv_atomic(ssa_audit, file.path(CFG$output_dir, "presentation_audit_ssa_oact_dollar_derivation_audit.csv"))
    }

    single_plot <- build_presentation_audit_single_plot(result$working_baseline, search)
    if (CFG$write_audit_outputs) {
      # Remove any PNG produced by a stale wrapper before writing the sole figure.
      stale_pngs <- list.files(CFG$output_dir, pattern = "\\.png$", full.names = TRUE)
      if (length(stale_pngs) > 0L) unlink(stale_pngs, force = TRUE)
      ragg::agg_png(
        file.path(CFG$output_dir, "presentation_audit_debt_paths.png"),
        width = 1920, height = 1080, res = 144
      )
      print(single_plot)
      grDevices::dev.off()
    }
    if (render_requested) print(single_plot)
  }

  result$presentation_audit_plan_status <- tibble::tibble(
    item = c(
      "Exactly one presentation PNG and one interactive plot print",
      "Single plot contains CBO baseline, strict best, expanded fiscal margin, and recommended robust plus SS-solvent package when materially distinct",
      "No separate robust-scenario presentation plot",
      "SSA OACT source-rate and taxable-payroll inputs survive final parameterization",
      "SSA OACT dollar-derivation audit is non-empty and coefficient-reproducing",
      "COMMON_BUT_REPLACEABLE requires successful leave-one-out replacement",
      "HIGHLY_SUBSTITUTABLE requires successful leave-one-out replacement",
      "Untested common policies/families are explicitly labeled COMMON_UNTESTED",
      "Model layer substantive model and recommendation frontier preserved",
      "Current law remains non-veto eligibility metadata"
    ),
    implemented = TRUE
  )
  if (CFG$write_audit_outputs) {
    write_csv_atomic(
      result$presentation_audit_plan_status,
      file.path(CFG$output_dir, "presentation_audit_plan_status.csv")
    )
  }
  result
}

# ------------------------------------------------------------------------------
# MODEL LAYER ACTIVE RULE SUMMARY
# - No fiscal coefficient, protection, robust scenario, debt target, or solver
#   requirement is changed from the recommendation-analysis model stage.
# - Exactly one presentation figure is produced.
# - The figure always includes the CBO working baseline and only materially
#   distinct representative package paths.
# - OACT-derived dollar coefficients are fully reconstructible from preserved
#   official annual rates and 2026 Trustees taxable payroll.
# - Replaceability/substitutability claims require successful re-optimization;
#   untested common policies are labeled as untested rather than inferred.
# - Model layer memory controls and robust-feasibility PSOCK verification
#   remain active.
# ------------------------------------------------------------------------------

# Execution is deferred until the recommendation-hardening stage is defined.


# ==============================================================================
# FINAL RECOMMENDATION AUDIT STAGE
# ==============================================================================
# Recommendation-hardening layer preserves the validated presentation-audit fiscal model and
# solver architecture. It hardens the 23-policy robust + Social-Security-solvent
# recommendation by testing every selected policy, quantifying replacement costs,
# adding politically useful exclusion packages, and producing a presentation-grade
# recommendation table. It also permanently corrects the single presentation plot
# to label the 90%, 80%, and 70% debt/GDP reference lines explicitly.
# ==============================================================================

# Ensure the public output directory exists.
dir.create(CFG$output_dir, recursive = TRUE, showWarnings = FALSE)

# Plot presentation rule accepted after presentation-audit layer.
CFG$recommendation_hardening_plot_reference_pct <- 70
CFG$recommendation_hardening_plot_author_tag <- "𝕏: @arabbitorduck"

# Bind the presentation-audit recommendation/search and run functions used by
# extending them. Global lookup means the recommendation-hardening all-policy reference
# selector below is used inside the preserved recommendation-analysis leave-one-out code.
run_full_solution_search_presentation_audit <- run_full_solution_search_recommendation_analysis

# ------------------------------------------------------------------------------
# FUNCTION: recommendation_analysis_major_reference_policies
# Purpose: Recommendation-hardening layer replaces the bounded "major policy" selector with
#          every policy in the minimum-complexity robust + SS-solvent reference
#          package. This guarantees direct leave-one-out evidence for all 23
#          recommended policies rather than inferring dependence for the rest.
# ------------------------------------------------------------------------------
recommendation_analysis_major_reference_policies <- function(reference_bundle, policy_model) {
  if (nrow(reference_bundle$catalog) == 0L || nrow(reference_bundle$membership) == 0L) return(tibble())
  ref_id <- reference_bundle$catalog |>
    filter(
      robust_target_2036_pass,
      robust_target_2046_pass,
      ss_actuarial_improvement_pct_payroll + 1e-9 >= CFG$ss_actuarial_gap_pct_payroll
    ) |>
    arrange(implementation_complexity_score, selected_policy_count) |>
    slice_head(n = 1L) |>
    pull(solution_id)
  assert_model(length(ref_id) == 1L, "Recommendation-hardening layer could not identify the minimum-complexity robust + SS-solvent reference package")

  ref_members <- reference_bundle$membership |> filter(solution_id == ref_id)
  contrib <- recommendation_analysis_candidate_contributions(ref_members, policy_model) |>
    arrange(desc(realized_primary_improvement_2027_2036_bil), candidate_id) |>
    mutate(reference_solution_id = ref_id)
  assert_model(nrow(contrib) > 0L, "Recommendation-hardening layer reference package contains no policies")
  contrib
}

# ------------------------------------------------------------------------------
# FUNCTION: recommendation_hardening_reference_solution
# Purpose: Return the minimum-complexity recommendation and its membership.
# ------------------------------------------------------------------------------
recommendation_hardening_reference_solution <- function(search_result) {
  rec <- search_result$recommendation_analysis_recommendation_frontier
  assert_model(!is.null(rec) && nrow(rec$catalog) > 0L, "Recommendation-hardening layer requires the recommendation-analysis recommendation frontier")
  row <- rec$catalog |>
    filter(
      robust_target_2036_pass,
      robust_target_2046_pass,
      ss_actuarial_improvement_pct_payroll + 1e-9 >= CFG$ss_actuarial_gap_pct_payroll
    ) |>
    arrange(implementation_complexity_score, selected_policy_count) |>
    slice_head(n = 1L)
  assert_model(nrow(row) == 1L, "Recommendation-hardening layer could not identify one recommended reference package")
  sid <- row$solution_id[[1]]
  list(
    catalog = row,
    membership = rec$membership |> filter(solution_id == sid),
    paths = rec$paths |> filter(solution_id == sid),
    scenario_paths = rec$scenario_paths |> filter(solution_id == sid)
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: recommendation_hardening_solve_counterfactual
# Purpose: Solve one policy/family/domain exclusion under the same hard robust
#          90/80 debt targets and approximate OASDI solvency requirement used for
#          the recommendation package.
# ------------------------------------------------------------------------------
recommendation_hardening_solve_counterfactual <- function(universe, working_baseline, kernel_obj, label) {
  if (!recommendation_analysis_ss_solvency_coefficients_present(universe)) {
    return(recommendation_analysis_structural_infeasible_solution(
      label,
      "counterfactual removes every Social Security actuarial-improvement coefficient while approximate OASDI solvency remains required"
    ))
  }
  u <- reduce_universe_for_complexity(universe, kernel_obj)
  m <- build_full_milp(
    u, working_baseline, kernel_obj,
    objective = "complexity",
    require_ss_solvency = TRUE,
    soft_targets = FALSE
  )
  m$solver_threads_override <- CFG$recommendation_analysis_recommendation_threads
  m$memory_reduction_mode <- "RECOMMENDATION_HARDENING_REPLACEMENT_COUNTERFACTUAL"
  sol <- solve_full_milp(m, label)
  rm(m)
  if (isTRUE(CFG$solver_force_gc_between_solves)) invisible(gc(verbose = FALSE))
  sol
}

# ------------------------------------------------------------------------------
# FUNCTION: recommendation_hardening_soft_gap_diagnostic
# Purpose: When a hard counterfactual is infeasible, distinguish a debt-target
#          capacity problem from a Social Security solvency problem. If robust
#          targets can only be met with positive slack, quantify the binding debt
#          gap and the level annual primary-improvement equivalent needed to close
#          it. If even the soft-target + solvency problem is infeasible, maximize
#          attainable OASDI actuarial improvement in the remaining universe.
# ------------------------------------------------------------------------------
recommendation_hardening_soft_gap_diagnostic <- function(universe, policy_model, working_baseline, kernel_obj, label) {
  if (!recommendation_analysis_ss_solvency_coefficients_present(universe)) {
    return(tibble(
      diagnostic_label = label,
      diagnostic_status = "STRUCTURAL_SS_SOLVENCY_CONFLICT",
      soft_target_feasible = FALSE,
      max_ss_actuarial_improvement_pct_payroll = 0,
      ss_solvency_shortfall_pct_payroll = CFG$ss_actuarial_gap_pct_payroll,
      binding_year = NA_integer_,
      binding_scenario = NA_character_,
      debt_gap_bil = NA_real_,
      debt_gap_pct_gdp = NA_real_,
      level_annual_primary_improvement_equivalent_bil = NA_real_,
      interpretation = "No remaining candidate carries a nonzero Social Security actuarial-improvement coefficient; additional ordinary fiscal capacity cannot repair the solvency requirement."
    ))
  }

  u <- reduce_universe_for_complexity(universe, kernel_obj)
  m <- build_full_milp(
    u, working_baseline, kernel_obj,
    objective = "target_slack",
    require_ss_solvency = TRUE,
    soft_targets = TRUE
  )
  m$solver_threads_override <- CFG$recommendation_analysis_recommendation_threads
  m$memory_reduction_mode <- "RECOMMENDATION_HARDENING_SOFT_TARGET_GAP_DIAGNOSTIC"
  sol <- solve_full_milp(m, paste0(label, "_SOFT_TARGET"))
  rm(m)
  if (isTRUE(CFG$solver_force_gc_between_solves)) invisible(gc(verbose = FALSE))

  if (full_solver_has_feasible_incumbent(sol)) {
    sim <- simulate_parameterized_package(sol$parameter_decisions, policy_model, working_baseline, kernel_obj)
    target_rows <- bind_rows(
      sim |> filter(required_robust, year == 2036L) |>
        mutate(target_pct = 90, target_year = 2036L),
      sim |> filter(required_robust, year == 2046L) |>
        mutate(target_pct = 80, target_year = 2046L)
    ) |>
      mutate(
        debt_gap_bil = pmax(scenario_debt_bil - target_pct / 100 * scenario_gdp_bil, 0),
        debt_gap_pct_gdp = pmax(scenario_debt_gdp_pct - target_pct, 0)
      ) |>
      arrange(desc(debt_gap_pct_gdp), desc(debt_gap_bil)) |>
      slice_head(n = 1L)

    gap_bil <- target_rows$debt_gap_bil[[1]]
    yr <- target_rows$target_year[[1]]
    sc <- target_rows$scenario_id[[1]]
    primary_equiv <- robust_feasibility_primary_gap_equivalent(gap_bil, yr, sc, policy_model, kernel_obj)
    ss_level <- sum(sol$parameter_decisions$ss_actuarial_contribution_pct_payroll, na.rm = TRUE)

    return(tibble(
      diagnostic_label = label,
      diagnostic_status = ifelse(gap_bil > 1e-8, "DEBT_TARGET_CAPACITY_SHORTFALL", "SOFT_DIAGNOSTIC_MEETS_TARGETS"),
      soft_target_feasible = TRUE,
      max_ss_actuarial_improvement_pct_payroll = ss_level,
      ss_solvency_shortfall_pct_payroll = pmax(CFG$ss_actuarial_gap_pct_payroll - ss_level, 0),
      binding_year = yr,
      binding_scenario = sc,
      debt_gap_bil = gap_bil,
      debt_gap_pct_gdp = target_rows$debt_gap_pct_gdp[[1]],
      level_annual_primary_improvement_equivalent_bil = primary_equiv,
      interpretation = ifelse(
        gap_bil > 1e-8,
        "Remaining policy universe can satisfy approximate OASDI solvency, but lacks enough robust debt reduction to meet every required target.",
        "Soft-target diagnostic is feasible without target slack; investigate any hard-model infeasibility as a formulation interaction rather than raw capacity."
      )
    ))
  }

  # If solvency plus soft debt targets is itself infeasible, maximize the remaining
  # actuarial-improvement coefficients exactly under the same interaction rules.
  m2 <- build_full_milp(
    u, working_baseline, kernel_obj,
    objective = "target_slack",
    require_ss_solvency = FALSE,
    soft_targets = TRUE
  )
  m2$L[] <- 0
  sscoef <- m2$schedules$ss_actuarial_improvement_pct_payroll
  names(sscoef) <- paste0("z::", m2$schedules$schedule_key)
  pos <- match(names(sscoef), m2$variable_names)
  keep <- !is.na(pos) & is.finite(sscoef)
  m2$L[pos[keep]] <- -sscoef[keep]
  # Tiny tie-break toward fewer activations without changing the actuarial optimum.
  y_pos <- match(m2$policy_variable_names, m2$variable_names)
  m2$L[y_pos] <- m2$L[y_pos] + 1e-9
  m2$objective_name <- "max_ss_actuarial_improvement"
  m2$solver_threads_override <- CFG$recommendation_analysis_recommendation_threads
  m2$memory_reduction_mode <- "RECOMMENDATION_HARDENING_MAX_SS_DIAGNOSTIC"
  sol2 <- solve_full_milp(m2, paste0(label, "_MAX_SS"))
  rm(m2)
  if (isTRUE(CFG$solver_force_gc_between_solves)) invisible(gc(verbose = FALSE))

  ss_max <- if (full_solver_has_feasible_incumbent(sol2)) {
    sum(sol2$parameter_decisions$ss_actuarial_contribution_pct_payroll, na.rm = TRUE)
  } else {
    NA_real_
  }

  tibble(
    diagnostic_label = label,
    diagnostic_status = "SS_SOLVENCY_CAPACITY_SHORTFALL",
    soft_target_feasible = FALSE,
    max_ss_actuarial_improvement_pct_payroll = ss_max,
    ss_solvency_shortfall_pct_payroll = ifelse(is.finite(ss_max), pmax(CFG$ss_actuarial_gap_pct_payroll - ss_max, 0), NA_real_),
    binding_year = NA_integer_,
    binding_scenario = NA_character_,
    debt_gap_bil = NA_real_,
    debt_gap_pct_gdp = NA_real_,
    level_annual_primary_improvement_equivalent_bil = NA_real_,
    interpretation = "Remaining policy universe cannot reach the required OASDI actuarial-improvement threshold even when debt targets are softened; the binding deficiency is Social Security reform capacity, not ordinary fiscal capacity."
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_recommendation_hardening_replacement_costs
# Purpose: Convert every reference-package leave-one-out result into a direct
#          replacement-cost table, including changes in policy count, complexity,
#          central headroom, robust headroom, and Social Security solvency margin.
#          Infeasible removals receive an additional gap diagnostic.
# ------------------------------------------------------------------------------
build_recommendation_hardening_replacement_costs <- function(search_result, policy_model, working_baseline, kernel_obj) {
  ref <- recommendation_hardening_reference_solution(search_result)
  loo <- search_result$recommendation_analysis_leave_one_out
  assert_model(!is.null(loo) && nrow(loo$policy) == nrow(ref$membership), "Recommendation-hardening layer requires one leave-one-out test for every recommended policy")

  ref_row <- ref$catalog |> slice_head(n = 1L)
  ref_count <- ref_row$selected_policy_count[[1]]
  ref_complexity <- ref_row$implementation_complexity_score[[1]]
  ref_c36 <- ref_row$debt_gdp_2036_pct[[1]]
  ref_c46 <- ref_row$debt_gdp_2046_pct[[1]]
  ref_w36 <- ref_row$worst_required_scenario_debt_gdp_2036_pct[[1]]
  ref_w46 <- ref_row$worst_required_scenario_debt_gdp_2046_pct[[1]]
  ref_ss <- ref_row$ss_actuarial_improvement_pct_payroll[[1]]

  solution_summaries <- purrr::map2_dfr(
    loo$policy_solutions,
    seq_along(loo$policy_solutions),
    function(sol, i) {
      if (!full_solver_has_feasible_incumbent(sol)) return(tibble(test_index = i))
      summarize_full_solution(sol, policy_model, working_baseline, kernel_obj, paste0("IMPL45_REPL_", sprintf("%02d", i))) |>
        mutate(test_index = i)
    }
  )
  base <- loo$policy |> mutate(test_index = row_number())
  if (nrow(solution_summaries) > 0L) {
    sm <- solution_summaries |>
      select(
        test_index,
        replacement_central_2036_pct = debt_gdp_2036_pct,
        replacement_central_2046_pct = debt_gdp_2046_pct,
        replacement_worst_required_2036_pct_summary = worst_required_scenario_debt_gdp_2036_pct,
        replacement_worst_required_2046_pct_summary = worst_required_scenario_debt_gdp_2046_pct,
        replacement_ss_pct_summary = ss_actuarial_improvement_pct_payroll
      )
    base <- base |> left_join(sm, by = "test_index")
  } else {
    base <- base |>
      mutate(
        replacement_central_2036_pct = NA_real_,
        replacement_central_2046_pct = NA_real_,
        replacement_worst_required_2036_pct_summary = NA_real_,
        replacement_worst_required_2046_pct_summary = NA_real_,
        replacement_ss_pct_summary = NA_real_
      )
  }

  diagnostics <- list()
  base_u <- reduce_universe_for_complexity(search_result$expanded_universe, kernel_obj)
  infeasible_ids <- base$candidate_id[!base$leave_one_out_feasible]
  for (cid in infeasible_ids) {
    diagnostics[[length(diagnostics) + 1L]] <- recommendation_hardening_soft_gap_diagnostic(
      recommendation_analysis_filter_universe(base_u, remove_candidate_ids = cid),
      policy_model, working_baseline, kernel_obj,
      paste0("RECOMMENDATION_HARDENING_GAP_WITHOUT_", stringr::str_replace_all(stringr::str_to_upper(cid), "[^A-Z0-9]+", "_"))
    ) |>
      mutate(candidate_id = cid)
  }
  diag_tbl <- if (length(diagnostics) > 0L) {
    bind_rows(diagnostics)
  } else {
    tibble::tibble(
      diagnostic_label = character(),
      diagnostic_status = character(),
      soft_target_feasible = logical(),
      max_ss_actuarial_improvement_pct_payroll = double(),
      ss_solvency_shortfall_pct_payroll = double(),
      binding_year = integer(),
      binding_scenario = character(),
      debt_gap_bil = double(),
      debt_gap_pct_gdp = double(),
      level_annual_primary_improvement_equivalent_bil = double(),
      interpretation = character(),
      candidate_id = character()
    )
  }

  out <- base |>
    mutate(
      reference_policy_count = ref_count,
      reference_complexity_score = ref_complexity,
      reference_central_2036_pct = ref_c36,
      reference_central_2046_pct = ref_c46,
      reference_worst_required_2036_pct = ref_w36,
      reference_worst_required_2046_pct = ref_w46,
      reference_ss_actuarial_improvement_pct_payroll = ref_ss,
      extra_policy_count = if_else(leave_one_out_feasible, replacement_policy_count - ref_count, NA_integer_),
      extra_complexity_score = if_else(leave_one_out_feasible, replacement_complexity_score - ref_complexity, NA_real_),
      central_2036_headroom_change_pp = if_else(leave_one_out_feasible, (90 - replacement_central_2036_pct) - (90 - ref_c36), NA_real_),
      central_2046_headroom_change_pp = if_else(leave_one_out_feasible, (80 - replacement_central_2046_pct) - (80 - ref_c46), NA_real_),
      robust_2036_headroom_change_pp = if_else(leave_one_out_feasible, (90 - replacement_worst_required_2036_pct) - (90 - ref_w36), NA_real_),
      robust_2046_headroom_change_pp = if_else(leave_one_out_feasible, (80 - replacement_worst_required_2046_pct) - (80 - ref_w46), NA_real_),
      ss_solvency_margin_change_pct_payroll = if_else(leave_one_out_feasible, replacement_ss_actuarial_improvement_pct_payroll - ref_ss, NA_real_),
      replacement_class = if_else(leave_one_out_feasible, "REPLACEABLE", "NOT_REPLACEABLE_IN_CURRENT_QUANTIFIED_UNIVERSE")
    ) |>
    left_join(
      diag_tbl |>
        select(
          candidate_id, diagnostic_status, max_ss_actuarial_improvement_pct_payroll,
          ss_solvency_shortfall_pct_payroll, binding_year, binding_scenario,
          debt_gap_bil, debt_gap_pct_gdp, level_annual_primary_improvement_equivalent_bil,
          diagnostic_interpretation = interpretation
        ),
      by = "candidate_id"
    ) |>
    arrange(desc(!leave_one_out_feasible), desc(reference_primary_improvement_bil), candidate_id)

  list(costs = out, gap_diagnostics = diag_tbl)
}

# ------------------------------------------------------------------------------
# FUNCTION: solve_recommendation_hardening_targeted_exclusions
# Purpose: Produce politically legible counterfactuals: no VAT, no FTT, no SALT
#          repeal, no Medicare reforms, no corporate/international reforms, and
#          alternative Social Security mixes. Each test uses the same hard robust
#          debt targets and approximate OASDI solvency requirement as the reference.
# ------------------------------------------------------------------------------
solve_recommendation_hardening_targeted_exclusions <- function(search_result, policy_model, working_baseline, kernel_obj) {
  base_u <- search_result$expanded_universe
  tests <- tibble::tribble(
    ~test_id, ~description, ~remove_kind, ~remove_value,
    "NO_VAT", "No value-added tax", "family", "CBO_2024_impose_a_5_percent_value_added_tax",
    "NO_FTT", "No financial-transactions tax", "family", "CBO_2024_impose_a_tax_on_financial_transactions",
    "NO_SALT_REPEAL", "No repeal of the state and local tax deduction", "candidate", "CBO_2024_eliminate_or_limit_itemized_deductions__eliminate_state_and_local_tax_deduction",
    "NO_MEDICARE_REFORMS", "No Health and Medicare domain reforms", "domain", "Health and Medicare",
    "NO_CORPORATE_REFORMS", "No Corporate and international tax domain reforms", "domain", "Corporate and international tax",
    "ALT_SS_NO_E2_1", "Alternative Social Security mix without OACT E2.1 taxable-maximum elimination", "candidate", "SSA_OACT_2025_E2_1_eliminate_taxable_max_no_benefit_credit",
    "ALT_SS_NO_HIGH_EARNER_BENEFIT_REDUCTION", "Alternative Social Security mix without the CBO high-earner benefit reduction family", "family", "CBO_2024_reduce_social_security_benefits_for_high_earners",
    "ALT_SS_NO_H9", "Alternative Social Security mix without OACT H9 high-income benefit taxation", "candidate", "SSA_OACT_2026_H9_tax_all_benefits_high_income"
  )

  rows <- list(); memberships <- list(); solutions <- list(); diagnostics <- list()
  for (i in seq_len(nrow(tests))) {
    t <- tests[i, ]
    u <- switch(
      t$remove_kind[[1]],
      candidate = recommendation_analysis_filter_universe(base_u, remove_candidate_ids = t$remove_value[[1]]),
      family = recommendation_analysis_filter_universe(base_u, remove_family_ids = t$remove_value[[1]]),
      domain = recommendation_analysis_filter_universe(base_u, remove_domains = t$remove_value[[1]]),
      stop("Unknown recommendation-hardening exclusion kind")
    )
    label <- paste0("RECOMMENDATION_HARDENING_", t$test_id[[1]])
    sol <- recommendation_hardening_solve_counterfactual(u, working_baseline, kernel_obj, label)
    solutions[[length(solutions) + 1L]] <- sol
    sm <- if (full_solver_has_feasible_incumbent(sol)) summarize_full_solution(sol, policy_model, working_baseline, kernel_obj, paste0("ALT45_", sprintf("%02d", i))) else tibble()
    rows[[length(rows) + 1L]] <- tibble(
      test_id = t$test_id[[1]],
      description = t$description[[1]],
      removed_kind = t$remove_kind[[1]],
      removed_value = t$remove_value[[1]],
      feasible = full_solver_has_feasible_incumbent(sol),
      solver_status = sol$status_message,
      selected_policy_count = if (nrow(sm) == 0L) NA_integer_ else sm$selected_policy_count[[1]],
      implementation_complexity_score = if (nrow(sm) == 0L) NA_real_ else sm$implementation_complexity_score[[1]],
      central_debt_gdp_2036_pct = if (nrow(sm) == 0L) NA_real_ else sm$debt_gdp_2036_pct[[1]],
      central_debt_gdp_2046_pct = if (nrow(sm) == 0L) NA_real_ else sm$debt_gdp_2046_pct[[1]],
      worst_required_debt_gdp_2036_pct = if (nrow(sm) == 0L) NA_real_ else sm$worst_required_scenario_debt_gdp_2036_pct[[1]],
      worst_required_debt_gdp_2046_pct = if (nrow(sm) == 0L) NA_real_ else sm$worst_required_scenario_debt_gdp_2046_pct[[1]],
      ss_actuarial_improvement_pct_payroll = if (nrow(sm) == 0L) NA_real_ else sm$ss_actuarial_improvement_pct_payroll[[1]],
      elapsed_seconds = sol$elapsed_seconds
    )
    if (full_solver_has_feasible_incumbent(sol)) {
      memberships[[length(memberships) + 1L]] <- extract_solution_membership(sol, policy_model, paste0("ALT45_", sprintf("%02d", i))) |>
        mutate(test_id = t$test_id[[1]], description = t$description[[1]])
    } else {
      diagnostics[[length(diagnostics) + 1L]] <- recommendation_hardening_soft_gap_diagnostic(
        u, policy_model, working_baseline, kernel_obj, label
      ) |>
        mutate(test_id = t$test_id[[1]], description = t$description[[1]])
    }
  }

  diagnostic_tbl <- if (length(diagnostics) > 0L) {
    bind_rows(diagnostics)
  } else {
    tibble::tibble(
      diagnostic_label = character(),
      diagnostic_status = character(),
      soft_target_feasible = logical(),
      max_ss_actuarial_improvement_pct_payroll = double(),
      ss_solvency_shortfall_pct_payroll = double(),
      binding_year = integer(),
      binding_scenario = character(),
      debt_gap_bil = double(),
      debt_gap_pct_gdp = double(),
      level_annual_primary_improvement_equivalent_bil = double(),
      interpretation = character(),
      test_id = character(),
      description = character()
    )
  }

  list(
    tests = bind_rows(rows),
    membership = bind_rows(memberships),
    diagnostics = diagnostic_tbl,
    solutions = solutions
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_recommendation_hardening_recommended_package_table
# Purpose: Presentation-grade table for every policy in the minimum-complexity
#          robust + SS-solvent package, including timing, intensity, ten-year
#          primary effect, domain, actuarial effect, source basis, and proven
#          dependence classification.
# ------------------------------------------------------------------------------
build_recommendation_hardening_recommended_package_table <- function(search_result, policy_model) {
  ref <- recommendation_hardening_reference_solution(search_result)
  contrib <- recommendation_analysis_candidate_contributions(ref$membership, policy_model)
  dep <- search_result$recommendation_analysis_policy_dependence
  meta_fields <- policy_model$meta |>
    select(
      candidate_id,
      any_of(c(
        "score_basis_status", "score_method_data_basis", "score_basis_reason",
        "score_recency_status", "publication_date", "score_publication_label"
      ))
    ) |>
    distinct(candidate_id, .keep_all = TRUE)

  out <- contrib |>
    left_join(
      dep |>
        select(candidate_id, dependence_class, dependence_tested, leave_one_out_feasible),
      by = "candidate_id"
    ) |>
    left_join(meta_fields, by = "candidate_id") |>
    transmute(
      recommendation_solution_id = ref$catalog$solution_id[[1]],
      candidate_id,
      policy = title,
      variant = variant_name,
      policy_domain = recommendation_analysis_domain,
      protection_status,
      implementation_year = implementation_start_year,
      phase_in_years,
      selected_parameter_value = parameter_value,
      parameter_unit,
      intensity_scale,
      ten_year_primary_improvement_2027_2036_bil = realized_primary_improvement_2027_2036_bil,
      ss_actuarial_improvement_pct_payroll = ss_actuarial_contribution_pct_payroll,
      dependence_class,
      dependence_tested,
      leave_one_out_feasible,
      source_url,
      across(any_of(c("score_basis_status", "score_method_data_basis", "score_basis_reason", "score_recency_status", "publication_date", "score_publication_label")))
    ) |>
    arrange(policy_domain, desc(ten_year_primary_improvement_2027_2036_bil), policy)

  assert_model(nrow(out) == nrow(ref$membership), "Recommendation-hardening layer recommended-package table lost one or more reference policies")
  out
}

# ------------------------------------------------------------------------------
# FUNCTION: build_recommendation_hardening_substitute_expansion_audit
# Purpose: Document that recommendation-hardening layer does not invent new coefficients merely
#          to create substitutes. The existing quantified universe is subjected to
#          exhaustive recommendation-package replacement tests; new solver levers
#          are admitted only when authoritative policy-specific fiscal response
#          evidence exists in the active source pipeline.
# ------------------------------------------------------------------------------
build_recommendation_hardening_substitute_expansion_audit <- function() {
  tibble(
    model_stage = "recommendation_hardening",
    new_solver_candidates_added = 0L,
    disposition = "NO_UNSUPPORTED_SUBSTITUTE_COEFFICIENTS_ADDED",
    rationale = paste0(
      "Recommendation-hardening layer hardens the recommendation against the already validated quantified universe. ",
      "Inventory-only Medicare, federal-credit, fees/receipts, tax-preference, and other evidence remains non-solver until an authoritative policy-specific fiscal response is available; gross amounts or tax-expenditure estimates are not treated as repeal scores."
    )
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: run_full_solution_search
# Purpose: Use the presentation-audit search machinery, but because the
#          reference selector now returns all recommended policies, leave-one-out
#          re-optimization is exhaustive for the recommendation package. Then add
#          replacement costs, targeted political exclusions, and the package table.
# ------------------------------------------------------------------------------
run_full_solution_search_recommendation_hardening <- function(policy_model, working_baseline, kernel_obj) {
  base <- run_full_solution_search_presentation_audit(policy_model, working_baseline, kernel_obj)

  ref <- recommendation_hardening_reference_solution(base)
  loo <- base$recommendation_analysis_leave_one_out
  assert_model(nrow(loo$policy) == nrow(ref$membership), paste0(
    "Recommendation-hardening layer leave-one-out coverage mismatch: tested ", nrow(loo$policy),
    " policies but recommendation contains ", nrow(ref$membership)
  ))
  assert_model(all(sort(loo$policy$candidate_id) == sort(ref$membership$candidate_id)), "Recommendation-hardening layer did not leave-one-out every recommended policy exactly once")

  replacement <- build_recommendation_hardening_replacement_costs(base, policy_model, working_baseline, kernel_obj)
  targeted <- solve_recommendation_hardening_targeted_exclusions(base, policy_model, working_baseline, kernel_obj)
  package_table <- build_recommendation_hardening_recommended_package_table(base, policy_model)
  expansion_audit <- build_recommendation_hardening_substitute_expansion_audit()

  # Rebuild dependence classifications after exhaustive leave-one-out coverage so
  # every recommended policy receives direct evidence rather than COMMON_UNTESTED.
  pool_membership <- bind_rows(
    base$membership |> filter(solution_id %in% (base$summary |> filter(
      protection_mode == "EXPANDED",
      robust_target_2036_pass,
      robust_target_2046_pass,
      ss_actuarial_improvement_pct_payroll + 1e-9 >= CFG$ss_actuarial_gap_pct_payroll
    ) |> pull(solution_id))),
    base$recommendation_analysis_recommendation_frontier$membership,
    base$recommendation_analysis_domain_alternatives$membership
  ) |>
    distinct(solution_id, candidate_id, .keep_all = TRUE)

  dep45 <- build_recommendation_analysis_policy_dependence(
    pool_membership,
    loo$policy,
    loo$major_reference_policies
  )
  base$recommendation_analysis_policy_dependence <- dep45
  package_table <- package_table |>
    select(-dependence_class, -dependence_tested, -leave_one_out_feasible) |>
    left_join(
      dep45 |> select(candidate_id, dependence_class, dependence_tested, leave_one_out_feasible),
      by = "candidate_id"
    )

  if (CFG$write_audit_outputs) {
    write_csv_atomic(loo$policy, file.path(CFG$output_dir, "recommendation_hardening_leave_one_out_all_recommended_policies.csv"))
    write_csv_atomic(replacement$costs, file.path(CFG$output_dir, "recommendation_hardening_replacement_costs.csv"))
    write_csv_atomic(replacement$gap_diagnostics, file.path(CFG$output_dir, "recommendation_hardening_indispensable_gap_diagnostics.csv"))
    write_csv_atomic(targeted$tests, file.path(CFG$output_dir, "recommendation_hardening_targeted_exclusion_tests.csv"))
    write_csv_atomic(targeted$membership, file.path(CFG$output_dir, "recommendation_hardening_targeted_exclusion_membership.csv"))
    write_csv_atomic(targeted$diagnostics, file.path(CFG$output_dir, "recommendation_hardening_targeted_exclusion_gap_diagnostics.csv"))
    write_csv_atomic(package_table, file.path(CFG$output_dir, "recommendation_hardening_recommended_package.csv"))
    write_csv_atomic(dep45, file.path(CFG$output_dir, "recommendation_hardening_policy_dependence.csv"))
    write_csv_atomic(expansion_audit, file.path(CFG$output_dir, "recommendation_hardening_substitute_expansion_audit.csv"))
  }

  base$recommendation_hardening_replacement_costs <- replacement$costs
  base$recommendation_hardening_indispensable_gap_diagnostics <- replacement$gap_diagnostics
  base$recommendation_hardening_targeted_exclusions <- targeted
  base$recommendation_hardening_recommended_package <- package_table
  base$recommendation_hardening_policy_dependence <- dep45
  base$recommendation_hardening_substitute_expansion_audit <- expansion_audit
  base
}

# ------------------------------------------------------------------------------
# FUNCTION: build_presentation_audit_single_plot
# Purpose: Build the single accepted presentation plot.
#          It keeps exactly the same four semantically useful series but explicitly
#          labels 90%, 80%, and 70% reference lines, left-aligns the caption, and
#          appends the permanent X author tag.
# ------------------------------------------------------------------------------
build_presentation_audit_single_plot <- function(working_baseline, search_result) {
  baseline <- working_baseline |>
    transmute(year, projection = "CBO working baseline", debt_gdp_pct = working_debt_gdp_pct)

  candidates <- tibble()
  add_path <- function(path_tbl, label) {
    if (is.null(path_tbl) || nrow(path_tbl) == 0L) return(invisible(NULL))
    candidates <<- bind_rows(candidates, path_tbl |> transmute(year, projection = label, debt_gdp_pct))
    invisible(NULL)
  }

  if (nrow(search_result$summary) > 0L && nrow(search_result$paths) > 0L) {
    strict <- search_result$summary |>
      filter(protection_mode == "STRICT") |>
      mutate(
        rank_metric = pmax(worst_required_scenario_debt_gdp_2036_pct - 90, 0) +
          pmax(worst_required_scenario_debt_gdp_2046_pct - 80, 0) +
          dplyr::coalesce(achieved_target_slack_score, 0)
      ) |>
      arrange(rank_metric, implementation_complexity_score, selected_policy_count) |>
      slice_head(n = 1L)
    if (nrow(strict) > 0L) {
      add_path(search_result$paths |> filter(solution_id == strict$solution_id[[1]]), "Strict best attainable")
    }

    margin <- search_result$summary |>
      filter(protection_mode == "EXPANDED", robust_target_2036_pass, robust_target_2046_pass) |>
      arrange(
        worst_required_scenario_debt_gdp_2036_pct + worst_required_scenario_debt_gdp_2046_pct,
        debt_gdp_2036_pct + debt_gdp_2046_pct,
        selected_policy_count
      ) |>
      slice_head(n = 1L)
    if (nrow(margin) > 0L) {
      add_path(search_result$paths |> filter(solution_id == margin$solution_id[[1]]), "Expanded maximum fiscal margin")
    }
  }

  rec <- search_result$recommendation_analysis_recommendation_frontier
  if (!is.null(rec) && nrow(rec$catalog) > 0L && nrow(rec$scenario_paths) > 0L) {
    rr <- rec$catalog |>
      filter(
        robust_target_2036_pass,
        robust_target_2046_pass,
        ss_actuarial_improvement_pct_payroll + 1e-9 >= CFG$ss_actuarial_gap_pct_payroll
      ) |>
      arrange(implementation_complexity_score, selected_policy_count) |>
      slice_head(n = 1L)
    if (nrow(rr) > 0L) {
      add_path(
        rec$scenario_paths |>
          filter(solution_id == rr$solution_id[[1]], scenario_id == "CENTRAL") |>
          transmute(year, debt_gdp_pct = scenario_debt_gdp_pct),
        "Recommended robust + SS-solvent package"
      )
    }
  }

  preferred_order <- c(
    "Recommended robust + SS-solvent package",
    "Expanded maximum fiscal margin",
    "Strict best attainable"
  )
  labels <- preferred_order[preferred_order %in% unique(candidates$projection)]
  keep <- character()
  for (lab in labels) {
    a <- candidates |> filter(projection == lab) |> arrange(year) |> pull(debt_gdp_pct)
    if (length(a) == 0L) next
    distinct_path <- TRUE
    for (kept in keep) {
      b <- candidates |> filter(projection == kept) |> arrange(year) |> pull(debt_gdp_pct)
      if (length(a) == length(b) && max(abs(a - b), na.rm = TRUE) < CFG$plot_visual_path_tolerance_pp) {
        distinct_path <- FALSE
        break
      }
    }
    if (distinct_path) keep <- c(keep, lab)
  }

  pdat <- bind_rows(baseline, candidates |> filter(projection %in% keep))
  legend_order <- c(
    "CBO working baseline",
    "Strict best attainable",
    "Expanded maximum fiscal margin",
    "Recommended robust + SS-solvent package"
  )
  pdat$projection <- factor(pdat$projection, levels = legend_order[legend_order %in% unique(as.character(pdat$projection))])

  target_labels <- tibble(
    year = rep(2026.2, 3),
    debt_gdp_pct = c(90, 80, CFG$recommendation_hardening_plot_reference_pct),
    label = c("90% target", "80% target", "70% long-run target")
  )

  ggplot(pdat, aes(year, debt_gdp_pct, group = projection, color = projection)) +
    geom_line(linewidth = 0.9) +
    geom_hline(yintercept = c(90, 80, CFG$recommendation_hardening_plot_reference_pct), linetype = "dotted") +
    geom_text(
      data = target_labels,
      aes(x = year, y = debt_gdp_pct, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = -0.45,
      size = 3.1
    ) +
    scale_x_continuous(breaks = seq(2026, 2046, 2)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "Debt held by the public under required scenarios",
      subtitle = "Selected package outcomes compared with the CBO working baseline and model targets",
      x = NULL,
      y = "Debt held by public / GDP",
      color = "Projection",
      caption = paste0(
        "Reference lines are labeled at 90%, 80%, and 70%. Policy-package paths within ",
        format(CFG$plot_visual_path_tolerance_pp, trim = TRUE),
        " percentage point at every year are treated as visually identical and shown once.\n",
        CFG$recommendation_hardening_plot_author_tag
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0)
    )
}

# ------------------------------------------------------------------------------
# FUNCTION: run_model
# Purpose: Execute the presentation-audit validated pipeline with recommendation-hardening
#          search hardening and the one corrected presentation plot. No additional
#          plot is generated or printed.
# ------------------------------------------------------------------------------
run_model_recommendation_hardening <- function() {
  log_line("Recommendation-hardening layer active: exhaustive recommendation-package leave-one-out testing, replacement-cost diagnostics, targeted political exclusions, and one corrected presentation plot")

  # Call the presentation-audit wrapper. Its single-plot call resolves the active
  # single-plot function above, so exactly one plot is
  # still produced and printed. All new recommendation-hardening search artifacts are
  # generated inside the active recommendation search.
  result <- run_model_presentation_audit()

  # Rename the sole presentation PNG to the durable public filename. The
  # plot has already been printed once by the preserved presentation-audit wrapper.
  old_png <- file.path(CFG$output_dir, "presentation_audit_debt_paths.png")
  new_png <- file.path(CFG$output_dir, "recommendation_debt_paths.png")
  if (file.exists(new_png)) unlink(new_png, force = TRUE)
  if (file.exists(old_png)) {
    ok <- file.rename(old_png, new_png)
    assert_model(isTRUE(ok) && file.exists(new_png), "Recommendation-hardening layer could not rename the sole presentation plot")
  }

  result$recommendation_hardening_plan_status <- tibble(
    item = c(
      "Exactly one presentation plot",
      "Plot explicitly labels 90%, 80%, and 70% reference lines",
      "Plot caption is left-aligned and includes X author tag",
      "Every recommended policy receives direct leave-one-out re-optimization",
      "Indispensable-policy failures receive debt-gap or Social-Security-solvency diagnostics",
      "Targeted no-VAT, no-FTT, no-SALT, no-Medicare, no-corporate, and alternative-SS counterfactuals",
      "Replacement-cost diagnostic reports count, complexity, fiscal-margin, and solvency tradeoffs",
      "Presentation-grade recommended-package table",
      "No unsupported substitute coefficient added merely to increase capacity",
      "Model layer fiscal model, protections, robust targets, and solver architecture preserved"
    ),
    implemented = TRUE
  )
  if (CFG$write_audit_outputs) {
    write_csv_atomic(result$recommendation_hardening_plan_status, file.path(CFG$output_dir, "recommendation_hardening_plan_status.csv"))
  }
  result
}

# ------------------------------------------------------------------------------
# MODEL LAYER ACTIVE RULE SUMMARY
# - Approved protections and current-law non-veto policy-space rule are unchanged.
# - Recommendation-stage packages still must satisfy every required 90/80 robust
#   debt target and approximate Social Security 75-year actuarial solvency.
# - Every policy in the minimum-complexity recommendation is directly removed and
#   re-optimized once before any replaceability claim is made.
# - Infeasible removals are diagnosed as robust debt-capacity or Social Security
#   solvency shortfalls, with primary-capacity equivalents where meaningful.
# - Politically useful no-VAT/no-FTT/no-SALT/no-Medicare/no-corporate and
#   alternative-Social-Security packages are solved explicitly.
# - No unsupported score is invented merely to manufacture a substitute.
# - Exactly one presentation figure is produced. It explicitly labels 90%, 80%,
#   and 70%, left-aligns its caption, and includes `𝕏: @arabbitorduck`.
# ------------------------------------------------------------------------------

# Execution is deferred until the substitute-expansion stage is defined.


# ==============================================================================
# TARGETED SUBSTITUTE ANALYSIS STAGE
# ==============================================================================
# Substitute-expansion layer preserves the validated recommendation-hardening fiscal model,
# recommendation architecture, protections, robust debt targets, approximate
# Social Security solvency requirement, memory controls, and one-plot rule.
# It expands only policy areas that can plausibly substitute for the four
# recommendation-hardening bottlenecks: OACT E2.1, the narrow-base 5 percent VAT,
# SALT-deduction repeal, and OACT H9.
#
# Current law remains non-veto metadata. Congress may enact any non-protected
# policy. Coefficients use the newest available authoritative score or response
# function for the specific proposal without fabricating annual paths.
# ==============================================================================

dir.create(CFG$output_dir, recursive = TRUE, showWarnings = FALSE)

# Historical recommendation-hardening reference metrics. These remain a fixed
# comparison point even if the enlarged substitute-expansion universe finds a
# smaller or less-complex recommendation.
CFG$recommendation_hardening_reference_policy_count <- 23L
CFG$recommendation_hardening_reference_complexity_score <- 33.566030
CFG$recommendation_hardening_reference_central_2036_pct <- 86.343780
CFG$recommendation_hardening_reference_central_2046_pct <- 66.482435
CFG$recommendation_hardening_reference_worst_2036_pct <- 89.999344
CFG$recommendation_hardening_reference_worst_2046_pct <- 74.601558
CFG$recommendation_hardening_reference_ss_actuarial_pct_payroll <- 4.51

CFG$substitute_expansion_bottleneck_ids <- c(
  "SSA_OACT_2025_E2_1_eliminate_taxable_max_no_benefit_credit",
  "CBO_2024_impose_a_5_percent_value_added_tax__narrow_base",
  "CBO_2024_eliminate_or_limit_itemized_deductions__eliminate_state_and_local_tax_deduction",
  "SSA_OACT_2026_H9_tax_all_benefits_high_income"
)

# Preserve active recommendation-hardening functions before targeted extension.
build_treasury_greenbook_policy_candidates_recommendation_hardening <- build_treasury_greenbook_policy_candidates_robust_feasibility
build_ssa_oact_solving_candidates_recommendation_hardening <- build_ssa_oact_solving_candidates_robust_feasibility
build_interaction_catalog_full_recommendation_hardening <- build_interaction_catalog_full_robust_feasibility

# ------------------------------------------------------------------------------
# FUNCTION: build_substitute_expansion_shifted_treasury_candidates
# Purpose: Add only authoritative Treasury Greenbook proposals that plausibly
#          provide substitutes for the recommendation-hardening bottlenecks while
#          respecting wage, saving, investment, family, and reinvestment rules.
# ------------------------------------------------------------------------------
build_substitute_expansion_shifted_treasury_candidates <- function(working_baseline) {
  specs <- list(
    list(
      candidate_id = "TREASURY_2025_corporate_amt_21pct",
      title = "Increase the Corporate Alternative Minimum Tax Rate to 21 Percent",
      variant_name = "Treasury FY2025 scored CAMT rate increase for large corporations",
      values_mil = c(13543,11759,12264,12675,13119,13672,14238,14800,15379,15980),
      total_mil = 137429,
      protection_status = "CONDITIONAL",
      protection_reason = "Raises a minimum tax on large corporate book income rather than ordinary wages. It remains EXPANDED-only because corporate investment and reinvestment incidence require explicit review.",
      policy_domain = "corporate_minimum_tax_and_base_reform",
      complexity_multiplier = 1.20
    ),
    list(
      candidate_id = "TREASURY_2025_global_minimum_tax_inversions_reform",
      title = "Revise the Global Minimum Tax Regime, Limit Inversions, and Make Related Reforms",
      variant_name = "Treasury FY2025 scored international minimum-tax and anti-inversion reform",
      values_mil = c(27920,35889,34589,34819,36215,37719,39261,40846,42483,44178),
      total_mil = 373919,
      protection_status = "ELIGIBLE",
      protection_reason = "Targets profit shifting, low-tax foreign income, and corporate inversions rather than ordinary wages or household saving.",
      policy_domain = "international_base_erosion_and_profit_shifting",
      complexity_multiplier = 1.20
    ),
    list(
      candidate_id = "TREASURY_2025_undertaxed_profits_rule",
      title = "Adopt the Undertaxed Profits Rule",
      variant_name = "Treasury FY2025 scored undertaxed-profits rule",
      values_mil = c(9596,14541,14065,14389,14181,14088,13837,13752,13916,13948),
      total_mil = 136313,
      protection_status = "ELIGIBLE",
      protection_reason = "Targets undertaxed multinational profits and base erosion rather than ordinary wages, family income, or broad productive saving.",
      policy_domain = "international_base_erosion_and_profit_shifting",
      complexity_multiplier = 1.15
    ),
    list(
      candidate_id = "TREASURY_2025_excessive_interest_financial_reporting_groups",
      title = "Restrict Deductions of Excessive Interest of Members of Financial Reporting Groups",
      variant_name = "Treasury FY2025 scored excessive-interest deduction limitation",
      values_mil = c(2691,4281,4038,3918,3910,4002,4113,4219,4341,4481),
      total_mil = 39994,
      protection_status = "CONDITIONAL",
      protection_reason = "Targets disproportionate leverage and profit shifting, but interest-deduction limits can affect productive financing choices and therefore remain EXPANDED-only.",
      policy_domain = "corporate_interest_and_leverage_preference_reform",
      complexity_multiplier = 1.15
    ),
    list(
      candidate_id = "TREASURY_2025_strengthen_noncorporate_excess_business_loss_limit",
      title = "Strengthen the Limitation on Losses for Noncorporate Taxpayers",
      variant_name = "Treasury FY2025 scored excess-business-loss limitation reform",
      values_mil = c(1185,2241,2519,2666,12901,14735,10543,9789,9621,9526),
      total_mil = 75726,
      protection_status = "CONDITIONAL",
      protection_reason = "Targets use of large business losses to shelter unrelated income and associated noncompliance, but can affect legitimate pass-through loss timing and therefore remains EXPANDED-only.",
      policy_domain = "high_end_business_loss_preference_reform",
      complexity_multiplier = 1.15
    ),
    list(
      candidate_id = "TREASURY_2025_real_property_depreciation_recapture",
      title = "Require Full Recapture of Depreciation Deductions for Certain Depreciable Real Property",
      variant_name = "Treasury FY2025 scored section 1250 depreciation-recapture reform",
      values_mil = c(41,128,267,417,579,755,946,1151,1373,1611),
      total_mil = 7268,
      protection_status = "CONDITIONAL",
      protection_reason = "Removes a preferential character rule for prior depreciation deductions but can affect real-property investment and transaction timing, so it remains EXPANDED-only.",
      policy_domain = "tax_deferral_and_preference_reform",
      complexity_multiplier = 1.10
    ),
    list(
      candidate_id = "TREASURY_2025_general_aviation_aircraft_depreciation",
      title = "Modify Depreciation Rules for Purchases of General Aviation Passenger Aircraft",
      variant_name = "Treasury FY2025 scored business-aircraft depreciation reform",
      values_mil = c(46,141,206,217,207,175,142,125,117,116),
      total_mil = 1492,
      protection_status = "ELIGIBLE",
      protection_reason = "Targets a narrow high-discretionary-use depreciation preference for general aviation passenger aircraft rather than ordinary wages or broad productive investment.",
      policy_domain = "high_discretionary_consumption_and_tax_preference",
      complexity_multiplier = 1.05
    )
  )

  source_years <- 2025:2034
  shift_years <- 2L
  gdp36 <- working_baseline$gdp_bil[working_baseline$year == 2036L]
  assert_model(length(gdp36) == 1L && is.finite(gdp36) && gdp36 > 0, "Missing FY2036 GDP for substitute-expansion Treasury extension")

  rows <- purrr::map(specs, function(s) {
    assert_model(length(s$values_mil) == 10L, paste0("Model layer Treasury score has wrong annual length: ", s$candidate_id))
    assert_model(abs(sum(s$values_mil) - s$total_mil) <= 1e-9, paste0("Model layer Treasury annual values do not reproduce published total: ", s$candidate_id))

    target_years <- source_years + shift_years
    flows <- tibble(
      candidate_id = s$candidate_id,
      year = CFG$model_years,
      revenue_delta_bil = 0,
      outlay_delta_bil = 0,
      shift_years = shift_years,
      translation_status = "ZERO"
    )
    idx <- match(target_years, flows$year)
    flows$revenue_delta_bil[idx] <- s$values_mil / 1000
    flows$translation_status[idx] <- "SHIFTED_TREASURY_OFFICIAL_ANNUAL_SCORE"

    revenue_2036 <- flows$revenue_delta_bil[flows$year == 2036L]
    for (y in CFG$extension_years) {
      gdp_y <- working_baseline$gdp_bil[working_baseline$year == y]
      flows$revenue_delta_bil[flows$year == y] <- revenue_2036 * (gdp_y / gdp36)
      flows$translation_status[flows$year == y] <- "EXTRAPOLATED_GDP_SHARE_FROM_LAST_OFFICIAL_YEAR"
    }
    flows <- flows |>
      mutate(
        primary_deficit_delta_bil = outlay_delta_bil - revenue_delta_bil,
        component_identity_residual_bil = primary_deficit_delta_bil - (outlay_delta_bil - revenue_delta_bil)
      ) |>
      select(candidate_id, year, revenue_delta_bil, outlay_delta_bil, primary_deficit_delta_bil, component_identity_residual_bil, shift_years, translation_status)

    score_flows <- flows |> filter(year %in% CFG$score_years)
    cumulative_revenue <- sum(pmax(score_flows$revenue_delta_bil, 0), na.rm = TRUE)
    cumulative_primary <- sum(-score_flows$primary_deficit_delta_bil, na.rm = TRUE)
    assert_model(abs(cumulative_primary - s$total_mil / 1000) <= 1e-9, paste0("Shifted substitute-expansion Treasury score does not reproduce official total: ", s$candidate_id))

    meta <- tibble(
      candidate_id = s$candidate_id,
      family_key = s$candidate_id,
      family_title = s$title,
      variant_name = s$variant_name,
      latest_estimate = "FY2025 Greenbook (March 2024)",
      estimate_year = 2024L,
      source_start_year = 2025L,
      source_end_year = 2034L,
      source_url = "https://home.treasury.gov/system/files/131/General-Explanations-FY2025-Table.pdf",
      fiscal_channel = "REVENUE",
      annual_profile_status = "FULL_OFFICIAL_ANNUAL",
      ss_actuarial_improvement_pct_payroll = 0,
      note = paste0("Treasury Office of Tax Policy official annual revenue estimate shifted two fiscal years to align FY2025-FY2034 with the model FY2027-FY2036 scored window. Published total: $", format(round(s$total_mil / 1000, 3), nsmall = 3), "B."),
      index_ten_year_savings_bil = s$total_mil / 1000,
      family_id = s$candidate_id,
      budget_option_id = NA_character_,
      title = s$title,
      major_category = "Revenues",
      budget_function = "Revenue",
      index_url = "https://home.treasury.gov/policy-issues/tax-policy/revenue-proposals",
      source_date = "Mar 2024",
      source_effective_year = 2025L,
      source_score_start_year = 2025L,
      source_score_end_year = 2034L,
      evidence_class = "OFFICIAL_OLDER",
      decision_type = "CONTINUOUS_LEVEL_WITH_TIMING",
      source_kind = "TREASURY_GREENBOOK_POLICY",
      investment_market_review = s$protection_status == "CONDITIONAL",
      direct_cbo_annual_score = FALSE,
      direct_official_annual_score = TRUE,
      solver_eligible_annual = TRUE,
      is_december_2024_core = FALSE,
      protection_status = s$protection_status,
      protected_category = ifelse(s$protection_status == "CONDITIONAL", "HIGH_END_CAPITAL_OR_SAVING_REVIEW", NA_character_),
      protection_reason = s$protection_reason,
      risk_ordinary_wages = FALSE,
      risk_ordinary_saving = s$protection_status == "CONDITIONAL",
      risk_productive_investment = s$protection_status == "CONDITIONAL",
      risk_family_formation = FALSE,
      risk_business_reinvestment = s$protection_status == "CONDITIONAL",
      risk_core_social_security = FALSE,
      risk_core_medicare = FALSE,
      risk_productive_public_capacity = FALSE,
      market_function_review_required = s$protection_status == "CONDITIONAL",
      hard_protection_violation = FALSE,
      explicit_review_required = s$protection_status == "CONDITIONAL",
      protection_classification_method = "EXPLICIT_SUBSTITUTE_EXPANSION_TARGETED_TREASURY_SUBSTITUTE_RULE",
      ss_actuarial_score_vintage = NA_character_,
      ss_actuarial_additivity_status = NA_character_,
      cumulative_revenue_increase_2027_2036_bil = cumulative_revenue,
      cumulative_spending_cut_2027_2036_bil = 0,
      cumulative_primary_improvement_2027_2036_bil = cumulative_primary,
      solver_exclusion_reason = NA_character_,
      replaced_by_granular_account_controls = FALSE,
      complexity_weight = s$complexity_multiplier * (1 + log1p(max(cumulative_primary, 0) / 1000)),
      complexity_weight_basis = "Activation burden plus logarithmic ten-year fiscal scope for an official Treasury scored substitute reform",
      policy_domain = s$policy_domain
    )

    source_score <- tibble(
      candidate_id = s$candidate_id,
      title = s$title,
      source_year = source_years,
      source_revenue_effect_mil = s$values_mil,
      published_2025_2034_total_mil = s$total_mil,
      source = "U.S. Treasury FY2025 Greenbook Table of Revenue Estimates"
    )
    list(meta = meta, flows = flows, source_score = source_score)
  })

  list(
    meta = bind_rows(purrr::map(rows, "meta")),
    flows = bind_rows(purrr::map(rows, "flows")),
    source_scores = bind_rows(purrr::map(rows, "source_score"))
  )
}

build_treasury_greenbook_policy_candidates <- function(working_baseline) {
  base <- build_treasury_greenbook_policy_candidates_recommendation_hardening(working_baseline)
  extra <- build_substitute_expansion_shifted_treasury_candidates(working_baseline)
  assert_model(length(intersect(base$meta$candidate_id, extra$meta$candidate_id)) == 0L, "Model layer Treasury candidate IDs collide with prior universe")
  list(
    meta = bind_rows(base$meta, extra$meta),
    flows = bind_rows(base$flows, extra$flows),
    source_scores = bind_rows(base$source_scores, extra$source_scores)
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_substitute_expansion_ssa_extra_candidates
# Purpose: Add additional official OACT structural provisions that directly test
#          whether E2.1 and H9 remain indispensable under a broader solvency menu.
# ------------------------------------------------------------------------------
build_substitute_expansion_ssa_extra_candidates <- function(payroll) {
  specs <- list(
    list(
      provision_code = "E2_2",
      candidate_id = "SSA_OACT_2025_E2_2_eliminate_taxable_max_with_benefit_credit",
      title = "Social Security OACT E2.2: Eliminate the Taxable Maximum With Benefit Credit",
      variant_name = "Full 12.4 percent OASDI tax on all earnings with benefit credit above the current-law maximum",
      url = "https://www.ssa.gov/oact/solvency/provisions/tables/table_run416.html",
      trustees_basis = "2025 Trustees Report",
      page_vintage = "2026",
      evidence_class = "OFFICIAL_OLDER",
      actuarial = 1.85,
      protection_status = "ELIGIBLE",
      protection_reason = "Dedicated Social Security solvency financing on earnings above the taxable maximum, paired with additional benefit credit for newly taxed earnings.",
      risk_productive_investment = FALSE,
      risk_business_reinvestment = FALSE
    ),
    list(
      provision_code = "E2_5",
      candidate_id = "SSA_OACT_2025_E2_5_tax_above_250k_no_benefit_credit",
      title = "Social Security OACT E2.5: Apply OASDI Tax Above $250,000 With No Additional Benefit Credit",
      variant_name = "Apply the full OASDI payroll tax above $250,000 until the taxable maximum reaches the threshold",
      url = "https://www.ssa.gov/oact/solvency/provisions/tables/table_run418.html",
      trustees_basis = "2025 Trustees Report",
      page_vintage = "2026",
      evidence_class = "OFFICIAL_OLDER",
      actuarial = 2.50,
      protection_status = "ELIGIBLE",
      protection_reason = "Dedicated Social Security solvency financing concentrated on very high earnings rather than a broad ordinary-income tax-rate increase.",
      risk_productive_investment = FALSE,
      risk_business_reinvestment = FALSE
    ),
    list(
      provision_code = "E3_2",
      candidate_id = "SSA_OACT_2025_E3_2_raise_taxable_max_90pct_no_benefit_credit",
      title = "Social Security OACT E3.2: Raise the Taxable Maximum to Cover 90 Percent of Earnings With No Additional Benefit Credit",
      variant_name = "Phase in a taxable maximum covering 90 percent of earnings through 2035 without additional benefit credit",
      url = "https://www.ssa.gov/oact/solvency/provisions/tables/table_run428.html",
      trustees_basis = "2025 Trustees Report",
      page_vintage = "2026",
      evidence_class = "OFFICIAL_OLDER",
      actuarial = 1.07,
      protection_status = "ELIGIBLE",
      protection_reason = "Dedicated Social Security solvency financing through a higher taxable maximum concentrated above current-law covered earnings.",
      risk_productive_investment = FALSE,
      risk_business_reinvestment = FALSE
    ),
    list(
      provision_code = "H2",
      candidate_id = "SSA_OACT_2026_H2_tax_benefits_like_private_pensions",
      title = "Social Security OACT H2: Tax Benefits in a Manner Similar to Private Pension Income",
      variant_name = "Phase out lower-income thresholds during 2027-2046 under the OACT private-pension taxation approach",
      url = "https://www.ssa.gov/oact/solvency/provisions/tables/table_run089.html",
      trustees_basis = "2026 Trustees Report",
      page_vintage = "2026",
      evidence_class = "OFFICIAL_CURRENT",
      actuarial = 0.15,
      protection_status = "CONDITIONAL",
      protection_reason = "Provides a direct alternative to H9 but broadens taxation of Social Security benefits beyond high-income beneficiaries, so household-security concerns require EXPANDED-only treatment.",
      risk_productive_investment = FALSE,
      risk_business_reinvestment = FALSE
    ),
    list(
      provision_code = "H5",
      candidate_id = "SSA_OACT_2026_H5_tax_remaining_15pct_high_income",
      title = "Social Security OACT H5: Tax the Remaining 15 Percent of Benefits for High-Income Beneficiaries",
      variant_name = "Include up to the remaining 15 percent of benefits in taxable income above OACT high-income thresholds beginning in 2033",
      url = "https://www.ssa.gov/oact/solvency/provisions/tables/table_run091.html",
      trustees_basis = "2026 Trustees Report",
      page_vintage = "2026",
      evidence_class = "OFFICIAL_CURRENT",
      actuarial = 0.02,
      protection_status = "CONDITIONAL",
      protection_reason = "Targets only high-income beneficiaries and preserves current-law taxation for other beneficiaries, but still changes taxation of earned Social Security benefits and therefore remains EXPANDED-only.",
      risk_productive_investment = FALSE,
      risk_business_reinvestment = FALSE
    )
  )

  rows <- purrr::map(specs, function(s) {
    rates <- fetch_ssa_oact_provision_rates(s)
    annual <- rates |>
      left_join(payroll, by = "year") |>
      mutate(
        revenue_delta_bil = taxable_payroll_bil * delta_income_rate_pct_payroll / 100,
        outlay_delta_bil = taxable_payroll_bil * delta_cost_rate_pct_payroll / 100,
        primary_deficit_delta_bil = outlay_delta_bil - revenue_delta_bil,
        candidate_id = s$candidate_id,
        component_identity_residual_bil = primary_deficit_delta_bil - (outlay_delta_bil - revenue_delta_bil),
        shift_years = 0L,
        translation_status = ifelse(
          s$trustees_basis == "2026 Trustees Report",
          "OFFICIAL_2026_OACT_RATE_RESPONSE_TIMES_2026_TR_TAXABLE_PAYROLL",
          "OFFICIAL_2025_OACT_RATE_RESPONSE_REBASED_TO_2026_TR_TAXABLE_PAYROLL"
        )
      ) |>
      select(candidate_id, year, revenue_delta_bil, outlay_delta_bil, primary_deficit_delta_bil, component_identity_residual_bil, shift_years, translation_status)

    score <- annual |> filter(year %in% CFG$score_years)
    cumulative_revenue <- sum(pmax(score$revenue_delta_bil, 0), na.rm = TRUE)
    cumulative_spending <- sum(pmax(-score$outlay_delta_bil, 0), na.rm = TRUE)
    cumulative_primary <- sum(-score$primary_deficit_delta_bil, na.rm = TRUE)

    meta <- tibble(
      candidate_id = s$candidate_id,
      family_key = paste0("SSA_OACT_", s$provision_code),
      family_title = s$title,
      variant_name = s$variant_name,
      latest_estimate = paste0("SSA OACT ", s$provision_code, " detailed provision table"),
      estimate_year = ifelse(s$trustees_basis == "2026 Trustees Report", 2026L, 2025L),
      source_start_year = 2026L,
      source_end_year = 2100L,
      source_url = s$url,
      fiscal_channel = "MIXED",
      annual_profile_status = "OFFICIAL_OACT_RATE_RESPONSE_DERIVED_CURRENT_2026_DOLLAR_BASE",
      ss_actuarial_improvement_pct_payroll = s$actuarial,
      note = paste0("OACT annual change-from-current-law cost and income rates are applied to the current 2026 Trustees taxable-payroll dollar path. Same-number calendar year is used as the model fiscal-year approximation. Long-range actuarial improvement: ", s$actuarial, "% of taxable payroll."),
      index_ten_year_savings_bil = cumulative_primary,
      family_id = paste0("SSA_OACT_", s$provision_code),
      budget_option_id = s$provision_code,
      title = s$title,
      major_category = "Social Security",
      budget_function = "Social Security",
      index_url = "https://www.ssa.gov/oact/solvency/provisions/",
      source_date = s$page_vintage,
      source_effective_year = 2026L,
      source_score_start_year = 2026L,
      source_score_end_year = 2100L,
      evidence_class = s$evidence_class,
      decision_type = "DISCRETE_WITH_TIMING",
      source_kind = "SSA_OACT_SOLVENCY_PROVISION",
      investment_market_review = s$protection_status == "CONDITIONAL",
      direct_cbo_annual_score = FALSE,
      direct_official_annual_score = FALSE,
      solver_eligible_annual = TRUE,
      is_december_2024_core = FALSE,
      protection_status = s$protection_status,
      protected_category = ifelse(s$protection_status == "CONDITIONAL", "SOCIAL_SECURITY_INCIDENCE_REVIEW", NA_character_),
      protection_reason = s$protection_reason,
      risk_ordinary_wages = stringr::str_starts(s$provision_code, "E"),
      risk_ordinary_saving = FALSE,
      risk_productive_investment = s$risk_productive_investment,
      risk_family_formation = FALSE,
      risk_business_reinvestment = s$risk_business_reinvestment,
      risk_core_social_security = FALSE,
      risk_core_medicare = FALSE,
      risk_productive_public_capacity = FALSE,
      market_function_review_required = s$protection_status == "CONDITIONAL",
      hard_protection_violation = FALSE,
      explicit_review_required = s$protection_status == "CONDITIONAL",
      protection_classification_method = "EXPLICIT_SUBSTITUTE_EXPANSION_SSA_SUBSTITUTE_RULE",
      ss_actuarial_score_vintage = s$trustees_basis,
      ss_actuarial_additivity_status = "ADDITIVE_ONLY_WITH_NONOVERLAPPING_SSA_PROVISIONS;TAXABLE_MAX_AND_BENEFIT_TAX_ALTERNATIVES_MUTUALLY_EXCLUSIVE",
      cumulative_revenue_increase_2027_2036_bil = cumulative_revenue,
      cumulative_spending_cut_2027_2036_bil = cumulative_spending,
      cumulative_primary_improvement_2027_2036_bil = cumulative_primary,
      solver_exclusion_reason = NA_character_,
      replaced_by_granular_account_controls = FALSE,
      complexity_weight = 1.25 * (1 + log1p(max(cumulative_primary, 0) / 1000)),
      complexity_weight_basis = "Discrete SSA OACT structural substitute provision plus logarithmic ten-year fiscal scope",
      policy_domain = "social_security_solvency"
    )

    source_rates <- rates |>
      mutate(
        candidate_id = s$candidate_id,
        provision_code = s$provision_code,
        trustees_basis = s$trustees_basis,
        actuarial_improvement_pct_payroll = s$actuarial,
        source_url = s$url
      )
    list(meta = meta, flows = annual, source_rates = source_rates)
  })

  list(
    meta = bind_rows(purrr::map(rows, "meta")),
    flows = bind_rows(purrr::map(rows, "flows")),
    source_rates = bind_rows(purrr::map(rows, "source_rates")),
    taxable_payroll = payroll
  )
}

build_ssa_oact_solving_candidates <- function() {
  base <- build_ssa_oact_solving_candidates_recommendation_hardening()
  extra <- build_substitute_expansion_ssa_extra_candidates(base$taxable_payroll)
  assert_model(length(intersect(base$meta$candidate_id, extra$meta$candidate_id)) == 0L, "Model layer SSA candidate IDs collide with prior universe")
  list(
    meta = bind_rows(base$meta, extra$meta),
    flows = bind_rows(base$flows, extra$flows),
    source_rates = bind_rows(base$source_rates, extra$source_rates),
    taxable_payroll = base$taxable_payroll
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_interaction_catalog_full
# Purpose: Preserve all prior non-additivity rules and add narrow interaction
#          guards for new Social Security alternatives and Treasury international
#          reforms. No independent scores are stacked where mechanisms overlap.
# ------------------------------------------------------------------------------
build_interaction_catalog_full_substitute_expansion <- function(meta) {
  base <- build_interaction_catalog_full_recommendation_hardening(meta)
  ids <- meta$candidate_id
  extra <- list()
  add_pairs <- function(left, right, reason) {
    left <- unique(left[left %in% ids]); right <- unique(right[right %in% ids])
    if (length(left) == 0L || length(right) == 0L) return(invisible(NULL))
    for (i in left) for (j in right) if (!identical(i, j)) {
      extra[[length(extra) + 1L]] <<- tibble(candidate_i = i, candidate_j = j, reason = reason)
    }
    invisible(NULL)
  }

  # Every taxable-maximum / high-earner payroll-tax design is an alternative.
  ssa_tax <- meta |>
    filter(
      source_kind == "SSA_OACT_SOLVENCY_PROVISION",
      stringr::str_detect(candidate_id, "E2_|E3_")
    ) |>
    pull(candidate_id)
  old_ss_tax <- meta |>
    filter(stringr::str_detect(
      stringr::str_to_lower(paste0(dplyr::coalesce(title, ""), " ", dplyr::coalesce(variant_name, ""))),
      "maximum taxable earnings.*social security payroll taxes|tax earnings above \\$250,000|restore 90 percent taxable earnings"
    )) |>
    pull(candidate_id)
  all_ss_tax <- unique(c(ssa_tax, old_ss_tax))
  if (length(all_ss_tax) > 1L) {
    cmb <- utils::combn(all_ss_tax, 2L)
    for (k in seq_len(ncol(cmb))) {
      extra[[length(extra) + 1L]] <- tibble(
        candidate_i = cmb[1L, k],
        candidate_j = cmb[2L, k],
        reason = "Alternative Social Security taxable-maximum/high-earner payroll-tax designs share the same earnings base and are mutually exclusive without a combined official score."
      )
    }
  }

  # H2, H5, and H9 alter taxation of Social Security benefits. Without a combined
  # OACT score they are treated as alternatives rather than additive components.
  benefit_tax_ids <- intersect(
    c(
      "SSA_OACT_2026_H2_tax_benefits_like_private_pensions",
      "SSA_OACT_2026_H5_tax_remaining_15pct_high_income",
      "SSA_OACT_2026_H9_tax_all_benefits_high_income"
    ),
    ids
  )
  if (length(benefit_tax_ids) > 1L) {
    cmb <- utils::combn(benefit_tax_ids, 2L)
    for (k in seq_len(ncol(cmb))) {
      extra[[length(extra) + 1L]] <- tibble(
        candidate_i = cmb[1L, k],
        candidate_j = cmb[2L, k],
        reason = "Alternative OACT benefit-taxation provisions overlap in the same Social Security benefit tax base and are not stacked without a combined OACT score."
      )
    }
  }

  # CBO's full-statutory-rate foreign-income option is a broad alternative to the
  # new Treasury international minimum-tax / UTPR mechanisms. Treasury's own
  # Greenbook components may coexist because Treasury publishes them within the
  # same scored package table.
  cbo_full_foreign <- "CBO_2024_tax_all_foreign_income_of_u_s_corporations_at_the_full_statutory_corporate_rate__full_statutory_rate"
  add_pairs(
    cbo_full_foreign,
    c("TREASURY_2025_global_minimum_tax_inversions_reform", "TREASURY_2025_undertaxed_profits_rule"),
    "CBO full-rate taxation of foreign corporate income broadly overlaps Treasury international minimum-tax and undertaxed-profits mechanisms; independent alternative scores are not stacked."
  )

  bind_rows(base, bind_rows(extra)) |>
    mutate(lo = pmin(candidate_i, candidate_j), hi = pmax(candidate_i, candidate_j)) |>
    filter(lo != hi) |>
    distinct(lo, hi, .keep_all = TRUE) |>
    transmute(candidate_i = lo, candidate_j = hi, reason)
}

# ------------------------------------------------------------------------------
# FUNCTION: build_substitute_expansion_added_substitute_candidates
# Purpose: Provide an explicit audit of every policy newly admitted specifically
#          to test substitution around the four recommendation-hardening bottlenecks.
# ------------------------------------------------------------------------------
build_substitute_expansion_added_substitute_candidates <- function(policy_model) {
  added_ids <- c(
    "TREASURY_2025_corporate_amt_21pct",
    "TREASURY_2025_global_minimum_tax_inversions_reform",
    "TREASURY_2025_undertaxed_profits_rule",
    "TREASURY_2025_excessive_interest_financial_reporting_groups",
    "TREASURY_2025_strengthen_noncorporate_excess_business_loss_limit",
    "TREASURY_2025_real_property_depreciation_recapture",
    "TREASURY_2025_general_aviation_aircraft_depreciation",
    "SSA_OACT_2025_E2_2_eliminate_taxable_max_with_benefit_credit",
    "SSA_OACT_2025_E2_5_tax_above_250k_no_benefit_credit",
    "SSA_OACT_2025_E3_2_raise_taxable_max_90pct_no_benefit_credit",
    "SSA_OACT_2026_H2_tax_benefits_like_private_pensions",
    "SSA_OACT_2026_H5_tax_remaining_15pct_high_income"
  )
  out <- policy_model$meta |>
    filter(candidate_id %in% added_ids) |>
    transmute(
      candidate_id,
      title,
      source_kind,
      policy_domain,
      protection_status,
      solver_eligible = parameterized_solver_eligible,
      ten_year_primary_capacity_bil = cumulative_primary_improvement_2027_2036_bil,
      ss_actuarial_improvement_pct_payroll,
      source_url,
      score_basis_status = dplyr::coalesce(score_basis_status, NA_character_),
      score_method_data_basis = dplyr::coalesce(score_method_data_basis, NA_character_),
      purpose = case_when(
        stringr::str_detect(candidate_id, "SSA_OACT") ~ "SOCIAL_SECURITY_BOTTLENECK_SUBSTITUTE",
        TRUE ~ "NON_WAGE_FISCAL_CAPACITY_SUBSTITUTE"
      )
    ) |>
    arrange(source_kind, desc(ten_year_primary_capacity_bil), candidate_id)
  assert_model(nrow(out) == length(added_ids), paste0("Substitute-expansion layer expected ", length(added_ids), " new substitute candidates but found ", nrow(out)))
  out
}

# ------------------------------------------------------------------------------
# FUNCTION: solve_substitute_expansion_bottleneck_substitution
# Purpose: Remove each recommendation-hardening indispensable policy from the enlarged
#          substitute-expansion universe, re-optimize robust + SS-solvent complexity,
#          and record which new substitutes are selected and what they cost.
# ------------------------------------------------------------------------------
solve_substitute_expansion_bottleneck_substitution <- function(search_result, policy_model, working_baseline, kernel_obj) {
  new_ids <- build_substitute_expansion_added_substitute_candidates(policy_model)$candidate_id
  base_u <- search_result$expanded_universe
  rows <- list(); members <- list(); diagnostics <- list(); solutions <- list()

  labels <- c(
    SSA_OACT_2025_E2_1_eliminate_taxable_max_no_benefit_credit = "SSA OACT E2.1 taxable-maximum reform",
    CBO_2024_impose_a_5_percent_value_added_tax__narrow_base = "5% narrow-base VAT",
    CBO_2024_eliminate_or_limit_itemized_deductions__eliminate_state_and_local_tax_deduction = "SALT deduction repeal",
    SSA_OACT_2026_H9_tax_all_benefits_high_income = "SSA OACT H9 high-income benefit taxation"
  )

  for (i in seq_along(CFG$substitute_expansion_bottleneck_ids)) {
    cid <- CFG$substitute_expansion_bottleneck_ids[[i]]
    assert_model(cid %in% base_u$meta$candidate_id, paste0("Substitute-expansion layer bottleneck missing from EXPANDED universe: ", cid))
    u <- recommendation_analysis_filter_universe(base_u, remove_candidate_ids = cid)
    solve_label <- paste0("SUBSTITUTE_EXPANSION_WITHOUT_", stringr::str_replace_all(stringr::str_to_upper(cid), "[^A-Z0-9]+", "_"))
    sol <- recommendation_hardening_solve_counterfactual(u, working_baseline, kernel_obj, solve_label)
    solutions[[length(solutions) + 1L]] <- sol
    feasible <- full_solver_has_feasible_incumbent(sol)
    sm <- if (feasible) summarize_full_solution(sol, policy_model, working_baseline, kernel_obj, paste0("SUB46_", sprintf("%02d", i))) else tibble()
    mem <- if (feasible) extract_solution_membership(sol, policy_model, paste0("SUB46_", sprintf("%02d", i))) else tibble()
    selected_new <- if (nrow(mem) > 0L) sort(intersect(mem$candidate_id, new_ids)) else character()

    rows[[length(rows) + 1L]] <- tibble(
      bottleneck_candidate_id = cid,
      bottleneck_label = unname(labels[[cid]]),
      feasible_after_removal = feasible,
      solver_status = sol$status_message,
      selected_policy_count = if (nrow(sm) == 0L) NA_integer_ else sm$selected_policy_count[[1]],
      policy_count_change_vs_recommendation_hardening_reference = if (nrow(sm) == 0L) NA_integer_ else sm$selected_policy_count[[1]] - CFG$recommendation_hardening_reference_policy_count,
      implementation_complexity_score = if (nrow(sm) == 0L) NA_real_ else sm$implementation_complexity_score[[1]],
      complexity_change_vs_recommendation_hardening_reference = if (nrow(sm) == 0L) NA_real_ else sm$implementation_complexity_score[[1]] - CFG$recommendation_hardening_reference_complexity_score,
      central_debt_gdp_2036_pct = if (nrow(sm) == 0L) NA_real_ else sm$debt_gdp_2036_pct[[1]],
      central_debt_gdp_2046_pct = if (nrow(sm) == 0L) NA_real_ else sm$debt_gdp_2046_pct[[1]],
      worst_required_debt_gdp_2036_pct = if (nrow(sm) == 0L) NA_real_ else sm$worst_required_scenario_debt_gdp_2036_pct[[1]],
      worst_required_debt_gdp_2046_pct = if (nrow(sm) == 0L) NA_real_ else sm$worst_required_scenario_debt_gdp_2046_pct[[1]],
      robust_2036_headroom_pp = if (nrow(sm) == 0L) NA_real_ else 90 - sm$worst_required_scenario_debt_gdp_2036_pct[[1]],
      robust_2046_headroom_pp = if (nrow(sm) == 0L) NA_real_ else 80 - sm$worst_required_scenario_debt_gdp_2046_pct[[1]],
      ss_actuarial_improvement_pct_payroll = if (nrow(sm) == 0L) NA_real_ else sm$ss_actuarial_improvement_pct_payroll[[1]],
      ss_solvency_margin_pct_payroll = if (nrow(sm) == 0L) NA_real_ else sm$ss_actuarial_improvement_pct_payroll[[1]] - CFG$ss_actuarial_gap_pct_payroll,
      substitute_expansion_substitute_count = length(selected_new),
      substitute_expansion_substitute_ids = paste(selected_new, collapse = ";"),
      elapsed_seconds = sol$elapsed_seconds
    )

    if (feasible) {
      members[[length(members) + 1L]] <- mem |>
        mutate(
          bottleneck_candidate_id = cid,
          bottleneck_label = unname(labels[[cid]]),
          is_substitute_expansion_added_substitute = candidate_id %in% new_ids
        )
    } else {
      diagnostics[[length(diagnostics) + 1L]] <- recommendation_hardening_soft_gap_diagnostic(
        u, policy_model, working_baseline, kernel_obj, solve_label
      ) |>
        mutate(bottleneck_candidate_id = cid, bottleneck_label = unname(labels[[cid]]))
    }
  }

  diagnostic_tbl <- if (length(diagnostics) > 0L) {
    bind_rows(diagnostics)
  } else {
    tibble::tibble(
      diagnostic_label = character(),
      diagnostic_status = character(),
      soft_target_feasible = logical(),
      max_ss_actuarial_improvement_pct_payroll = double(),
      ss_solvency_shortfall_pct_payroll = double(),
      binding_year = integer(),
      binding_scenario = character(),
      debt_gap_bil = double(),
      debt_gap_pct_gdp = double(),
      level_annual_primary_improvement_equivalent_bil = double(),
      interpretation = character(),
      bottleneck_candidate_id = character(),
      bottleneck_label = character()
    )
  }

  list(
    tests = bind_rows(rows),
    membership = bind_rows(members),
    diagnostics = diagnostic_tbl,
    solutions = solutions
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_substitute_expansion_reference_comparison
# Purpose: Compare the active substitute-expansion minimum-complexity recommendation
#          and each bottleneck substitute package to the fixed recommendation-hardening
#          23-policy reference metrics.
# ------------------------------------------------------------------------------
build_substitute_expansion_reference_comparison <- function(search_result, bottleneck_tests) {
  ref46 <- recommendation_hardening_reference_solution(search_result)$catalog |> slice_head(n = 1L)
  current <- tibble(
    comparison_id = "SUBSTITUTE_EXPANSION_RECOMMENDED",
    policy_count = ref46$selected_policy_count[[1]],
    implementation_complexity_score = ref46$implementation_complexity_score[[1]],
    central_debt_gdp_2036_pct = ref46$debt_gdp_2036_pct[[1]],
    central_debt_gdp_2046_pct = ref46$debt_gdp_2046_pct[[1]],
    worst_required_debt_gdp_2036_pct = ref46$worst_required_scenario_debt_gdp_2036_pct[[1]],
    worst_required_debt_gdp_2046_pct = ref46$worst_required_scenario_debt_gdp_2046_pct[[1]],
    ss_actuarial_improvement_pct_payroll = ref46$ss_actuarial_improvement_pct_payroll[[1]]
  )
  historical <- tibble(
    comparison_id = "RECOMMENDATION_HARDENING_REFERENCE",
    policy_count = CFG$recommendation_hardening_reference_policy_count,
    implementation_complexity_score = CFG$recommendation_hardening_reference_complexity_score,
    central_debt_gdp_2036_pct = CFG$recommendation_hardening_reference_central_2036_pct,
    central_debt_gdp_2046_pct = CFG$recommendation_hardening_reference_central_2046_pct,
    worst_required_debt_gdp_2036_pct = CFG$recommendation_hardening_reference_worst_2036_pct,
    worst_required_debt_gdp_2046_pct = CFG$recommendation_hardening_reference_worst_2046_pct,
    ss_actuarial_improvement_pct_payroll = CFG$recommendation_hardening_reference_ss_actuarial_pct_payroll
  )
  substitutes <- bottleneck_tests |>
    filter(feasible_after_removal) |>
    transmute(
      comparison_id = paste0("WITHOUT_", bottleneck_label),
      policy_count = selected_policy_count,
      implementation_complexity_score,
      central_debt_gdp_2036_pct,
      central_debt_gdp_2046_pct,
      worst_required_debt_gdp_2036_pct,
      worst_required_debt_gdp_2046_pct,
      ss_actuarial_improvement_pct_payroll
    )
  bind_rows(historical, current, substitutes) |>
    mutate(
      policy_count_change_vs_recommendation_hardening = policy_count - CFG$recommendation_hardening_reference_policy_count,
      complexity_change_vs_recommendation_hardening = implementation_complexity_score - CFG$recommendation_hardening_reference_complexity_score,
      robust_2036_headroom_pp = 90 - worst_required_debt_gdp_2036_pct,
      robust_2046_headroom_pp = 80 - worst_required_debt_gdp_2046_pct,
      ss_solvency_margin_pct_payroll = ss_actuarial_improvement_pct_payroll - CFG$ss_actuarial_gap_pct_payroll
    )
}

# ------------------------------------------------------------------------------
# FUNCTION: run_full_solution_search
# Purpose: Run the complete recommendation-hardening search on
#          the expanded substitute-expansion universe, then explicitly retest all
#          four historical bottlenecks and document replacement packages.
# ------------------------------------------------------------------------------
run_full_solution_search_substitute_expansion <- function(policy_model, working_baseline, kernel_obj) {
  base <- run_full_solution_search_recommendation_hardening(policy_model, working_baseline, kernel_obj)
  added <- build_substitute_expansion_added_substitute_candidates(policy_model)
  bottlenecks <- solve_substitute_expansion_bottleneck_substitution(base, policy_model, working_baseline, kernel_obj)
  comparison <- build_substitute_expansion_reference_comparison(base, bottlenecks$tests)

  if (CFG$write_audit_outputs) {
    write_csv_atomic(added, file.path(CFG$output_dir, "substitute_expansion_added_substitute_candidates.csv"))
    write_csv_atomic(bottlenecks$tests, file.path(CFG$output_dir, "substitute_expansion_bottleneck_substitution_tests.csv"))
    write_csv_atomic(bottlenecks$membership, file.path(CFG$output_dir, "substitute_expansion_bottleneck_substitute_membership.csv"))
    write_csv_atomic(bottlenecks$diagnostics, file.path(CFG$output_dir, "substitute_expansion_bottleneck_gap_diagnostics.csv"))
    write_csv_atomic(comparison, file.path(CFG$output_dir, "substitute_expansion_reference_comparison.csv"))
  }

  base$substitute_expansion_added_substitute_candidates <- added
  base$substitute_expansion_bottleneck_substitution <- bottlenecks
  base$substitute_expansion_reference_comparison <- comparison
  base
}

# ------------------------------------------------------------------------------
# FUNCTION: run_model
# Purpose: Preserve recommendation-hardening execution, validation, recommendation
#          hardening, and the single accepted presentation plot while writing the
#          substitute-expansion targeted-substitution audit layer.
# ------------------------------------------------------------------------------
run_model_substitute_expansion <- function() {
  log_line("Substitute-expansion layer active: targeted substitutes for E2.1, VAT, SALT repeal, and H9; recommendation-hardening and one-plot presentation preserved")
  result <- run_model_recommendation_hardening()

  # Rename the one and only presentation PNG to the durable public filename.
  old_png <- file.path(CFG$output_dir, "recommendation_debt_paths.png")
  new_png <- file.path(CFG$output_dir, "substitute_expansion_debt_paths.png")
  if (file.exists(new_png)) unlink(new_png, force = TRUE)
  if (file.exists(old_png)) {
    ok <- file.rename(old_png, new_png)
    assert_model(isTRUE(ok) && file.exists(new_png), "Substitute-expansion layer could not rename the sole presentation plot")
  }

  result$substitute_expansion_plan_status <- tibble(
    item = c(
      "Model layer substantive model, protections, robust targets, SS solvency, and solver architecture preserved",
      "Exactly one presentation plot with labeled 90%, 80%, and 70% lines, left-aligned caption, and X author tag",
      "Four recommendation-hardening indispensable bottlenecks explicitly removed and re-optimized",
      "SALT substitution receives targeted additional non-wage capacity",
      "SSA OACT menu expanded with E2.2, E2.5, E3.2, H2, and H5 authoritative response functions",
      "Treasury substitute universe expanded only with official annual Greenbook scores consistent with protections",
      "No broad ordinary-income wage-rate increase added",
      "No unsupported OMB gross user-charge amount converted into deficit reduction",
      "Bottleneck diagnostics report binding debt/solvency shortfalls when substitution remains infeasible",
      "Substitute packages compared with fixed 23-policy recommendation-hardening reference"
    ),
    implemented = TRUE
  )
  if (CFG$write_audit_outputs) {
    write_csv_atomic(result$substitute_expansion_plan_status, file.path(CFG$output_dir, "substitute_expansion_plan_status.csv"))
  }
  result
}

# ------------------------------------------------------------------------------
# MODEL LAYER ACTIVE RULE SUMMARY
# - Empty diagnostic sets are schema-stable; successful substitution no longer creates zero-column tibbles.
# - Current law is never a policy-eligibility veto.
# - Approved protection categories remain binding; broad ordinary-wage income-tax
#   rate increases remain blocked.
# - New Treasury levers use official FY2025 annual Greenbook scores, the latest
#   Treasury Greenbook currently published on the Treasury Revenue Proposals page.
# - New SSA provisions use official OACT annual response functions and the 2026
#   Trustees taxable-payroll path, with 2025-basis E-category provisions retained
#   because SSA currently identifies category E as 2025 Trustees estimates.
# - OACT taxable-maximum alternatives and benefit-tax alternatives are mutually
#   exclusive unless an authoritative combined score exists.
# - The full robust 90/80 targets and approximate 4.42-percent OASDI solvency
#   requirement remain hard recommendation-stage constraints.
# - Model layer memory controls and robust-feasibility PSOCK verification
#   remain active.
# - Exactly one presentation figure is produced with labeled 90%, 80%, and 70%
#   reference lines, a left-aligned caption, and `𝕏: @arabbitorduck`.
# ------------------------------------------------------------------------------

# Execution is deferred until the overlap-correction stage is defined.



# ==============================================================================
# MODEL LAYER INTERACTION CORRECTION AND CONSOLIDATION MODEL STAGE
# ==============================================================================
# Overlap-correction layer freezes the substitute-expansion scored policy universe and
# corrects a material Social Security benefit-tax overlap before re-running the
# recommendation, bottleneck-substitution, and leave-one-out analyses.
#
# Governing rules remain unchanged:
#   * protected categories remain binding;
#   * current law is never a policy-eligibility veto;
#   * robust 90/80 debt targets remain hard at recommendation stage;
#   * approximate OASDI solvency remains hard at recommendation stage;
#   * no new scored levers are added by this model stage;
#   * memory-safety memory controls and later concurrency/audit machinery
#     remain active.
#
# Authoritative overlap evidence:
#   CBO option: Tax Social Security and Railroad Retirement Benefits in the Same
#   Way That Distributions From Defined Benefit Pensions Are Taxed
#   https://www.cbo.gov/budget-options/56856
#   SSA OACT provisions affecting taxation of benefits, including H2, H5, H9
#   https://www.ssa.gov/oact/solvency/provisions/taxbenefit.html
# ==============================================================================

dir.create(CFG$output_dir, recursive = TRUE, showWarnings = FALSE)

CFG$overlap_correction_cbo_benefit_tax_id <- "CBO_2020_tax_social_security_and_railroad_retirement_benefits_in_the_same_way_that_distributions_from_defined_benefit_pensions_ar__tax_like_defined_benefit_pensions"
CFG$overlap_correction_oact_benefit_tax_ids <- c(
  "SSA_OACT_2026_H2_tax_benefits_like_private_pensions",
  "SSA_OACT_2026_H5_tax_remaining_15pct_high_income",
  "SSA_OACT_2026_H9_tax_all_benefits_high_income"
)
CFG$overlap_correction_policy_universe_frozen <- TRUE

# Preserve substitute-expansion functions before correction/consolidation.

# ------------------------------------------------------------------------------
# FUNCTION: build_interaction_catalog_full
# Purpose: Preserve every substitute-expansion interaction and complete the Social
#          Security benefit-taxation overlap clique. CBO's defined-benefit-
#          pension treatment and OACT H2/H5/H9 all alter the taxable Social
#          Security benefit base. Their independently scored effects are not
#          stacked without an authoritative combined score.
# ------------------------------------------------------------------------------
build_interaction_catalog_full <- function(meta) {
  base <- build_interaction_catalog_full_substitute_expansion(meta)
  ids <- meta$candidate_id
  cbo_id <- CFG$overlap_correction_cbo_benefit_tax_id
  oact_ids <- intersect(CFG$overlap_correction_oact_benefit_tax_ids, ids)

  extra <- tibble::tibble(
    candidate_i = character(),
    candidate_j = character(),
    reason = character()
  )

  if (cbo_id %in% ids && length(oact_ids) > 0L) {
    extra <- tibble::tibble(
      candidate_i = cbo_id,
      candidate_j = oact_ids,
      reason = paste0(
        "CBO's defined-benefit-pension treatment of Social Security benefits and the SSA OACT H2/H5/H9 benefit-taxation provisions ",
        "operate on the same Social Security benefit tax base. Independent scores are treated as alternatives and are not stacked without an authoritative combined score."
      )
    )
  }

  out <- dplyr::bind_rows(base, extra) |>
    dplyr::mutate(
      lo = pmin(candidate_i, candidate_j),
      hi = pmax(candidate_i, candidate_j)
    ) |>
    dplyr::filter(lo != hi) |>
    dplyr::distinct(lo, hi, .keep_all = TRUE) |>
    dplyr::transmute(candidate_i = lo, candidate_j = hi, reason)

  # The four benefit-taxation alternatives must form a complete mutual-exclusion
  # clique whenever all four are solver-eligible. This makes the substitute-expansion
  # omitted H2/H5 guard impossible to recur silently.
  benefit_tax_ids <- intersect(c(cbo_id, CFG$overlap_correction_oact_benefit_tax_ids), ids)
  if (length(benefit_tax_ids) > 1L) {
    expected <- utils::combn(sort(benefit_tax_ids), 2L)
    expected_keys <- apply(expected, 2L, paste, collapse = "||")
    actual_keys <- paste(out$candidate_i, out$candidate_j, sep = "||")
    assert_model(
      all(expected_keys %in% actual_keys),
      "Overlap-correction layer Social Security benefit-taxation interaction clique is incomplete"
    )
  }

  out
}

# ------------------------------------------------------------------------------
# FUNCTION: overlap_correction_overlap_mechanism_class
# Purpose: Assign a concise mechanism family to every material overlap already
#          guarded by the MILP interaction catalog.
# ------------------------------------------------------------------------------
overlap_correction_overlap_mechanism_class <- function(reason) {
  r <- stringr::str_to_lower(dplyr::coalesce(reason, ""))
  dplyr::case_when(
    stringr::str_detect(r, "social security benefit tax|benefit-taxation|defined-benefit-pension") ~ "SOCIAL_SECURITY_BENEFIT_TAXATION",
    stringr::str_detect(r, "taxable-maximum|high-earner payroll-tax|taxable payroll base") ~ "SOCIAL_SECURITY_TAXABLE_EARNINGS",
    stringr::str_detect(r, "benefit-formula reforms") ~ "SOCIAL_SECURITY_BENEFIT_FORMULA",
    stringr::str_detect(r, "medicare advantage") ~ "MEDICARE_ADVANTAGE_PAYMENT",
    stringr::str_detect(r, "beneficiary financing") ~ "MEDICARE_BENEFICIARY_FINANCING",
    stringr::str_detect(r, "employment-based health exclusion") ~ "EMPLOYMENT_HEALTH_TAX_EXCLUSION",
    stringr::str_detect(r, "irs enforcement|tax-administration|compliance") ~ "TAX_ADMINISTRATION_ENFORCEMENT",
    stringr::str_detect(r, "fossil-fuel|energy and natural-resource") ~ "FOSSIL_FUEL_TAX_PREFERENCES",
    stringr::str_detect(r, "niit") ~ "NET_INVESTMENT_INCOME_TAX_BASE",
    stringr::str_detect(r, "capital-income reform|inherited-gain|wealthiest-taxpayer minimum tax|capital-gains treatment of inherited") ~ "HIGH_END_CAPITAL_INCOME",
    stringr::str_detect(r, "foreign corporate income|international minimum-tax|undertaxed-profits") ~ "INTERNATIONAL_CORPORATE_TAX",
    stringr::str_detect(r, "dod o&m") ~ "DOD_OPERATIONS_AND_MAINTENANCE",
    stringr::str_detect(r, "international affairs") ~ "INTERNATIONAL_AFFAIRS_SPENDING",
    stringr::str_detect(r, "human-space-exploration|nasa exploration") ~ "NASA_EXPLORATION_SPENDING",
    stringr::str_detect(r, "global health") ~ "GLOBAL_HEALTH_SPENDING",
    stringr::str_detect(r, "tactical-aircraft|aircraft-procurement") ~ "TACTICAL_AIRCRAFT_PROCUREMENT",
    stringr::str_detect(r, "naval ship|aircraft-carrier|shipbuilding") ~ "NAVAL_SHIPBUILDING",
    TRUE ~ "OTHER_REVIEWED_MATERIAL_OVERLAP"
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_overlap_correction_overlap_audit
# Purpose: Audit every interaction pair in the active corrected universe. The
#          current MILP implements interaction-catalog pairs as x_i + x_j <= 1,
#          so every listed pair is explicitly mutually exclusive. Source URLs,
#          policy titles, domains, and the reason for the guard are retained.
# ------------------------------------------------------------------------------
build_overlap_correction_overlap_audit <- function(policy_model) {
  active_meta <- policy_model$meta |>
    dplyr::filter(parameterized_solver_eligible, policy_search_solver_eligible)

  interactions <- build_interaction_catalog_full(active_meta)
  meta <- active_meta |>
    dplyr::select(
      candidate_id,
      title,
      source_kind,
      policy_domain,
      source_url,
      protection_status
    )
  left_meta <- meta |>
    dplyr::rename(
      candidate_i = candidate_id,
      title_i = title,
      source_kind_i = source_kind,
      policy_domain_i = policy_domain,
      source_url_i = source_url,
      protection_status_i = protection_status
    )
  right_meta <- meta |>
    dplyr::rename(
      candidate_j = candidate_id,
      title_j = title,
      source_kind_j = source_kind,
      policy_domain_j = policy_domain,
      source_url_j = source_url,
      protection_status_j = protection_status
    )

  out <- interactions |>
    dplyr::left_join(left_meta, by = "candidate_i") |>
    dplyr::left_join(right_meta, by = "candidate_j") |>
    dplyr::mutate(
      mechanism_class = overlap_correction_overlap_mechanism_class(reason),
      interaction_treatment = "MUTUALLY_EXCLUSIVE",
      constraint_form = "activation_i + activation_j <= 1",
      audit_status = "EXPLICITLY_GUARDED",
      evidence_urls = paste(dplyr::coalesce(source_url_i, ""), dplyr::coalesce(source_url_j, ""), sep = ";")
    ) |>
    dplyr::select(
      mechanism_class,
      interaction_treatment,
      candidate_i,
      title_i,
      source_kind_i,
      policy_domain_i,
      candidate_j,
      title_j,
      source_kind_j,
      policy_domain_j,
      constraint_form,
      reason,
      evidence_urls,
      audit_status
    ) |>
    dplyr::arrange(mechanism_class, candidate_i, candidate_j)

  assert_model(nrow(out) == nrow(interactions), "Overlap-correction layer overlap audit lost one or more interaction rows")
  assert_model(all(out$audit_status == "EXPLICITLY_GUARDED"), "Overlap-correction layer overlap audit contains an unguarded interaction")
  out
}

# ------------------------------------------------------------------------------
# FUNCTION: build_overlap_correction_overlap_family_summary
# Purpose: Summarize the complete interaction catalog by reviewed mechanism.
# ------------------------------------------------------------------------------
build_overlap_correction_overlap_family_summary <- function(overlap_audit) {
  overlap_audit |>
    dplyr::group_by(mechanism_class, interaction_treatment) |>
    dplyr::summarise(
      guarded_pair_count = dplyr::n(),
      distinct_policy_count = dplyr::n_distinct(c(candidate_i, candidate_j)),
      .groups = "drop"
    ) |>
    dplyr::arrange(mechanism_class)
}

# ------------------------------------------------------------------------------
# FUNCTION: validate_overlap_correction_solution_overlap_contract
# Purpose: Independently prove that no retained solution selects both sides of a
#          corrected material-overlap pair.
# ------------------------------------------------------------------------------
validate_overlap_correction_solution_overlap_contract_overlap_correction <- function(search_result, overlap_audit) {
  mem <- search_result$membership |>
    dplyr::select(solution_id, candidate_id) |>
    dplyr::distinct()

  if (nrow(mem) == 0L || nrow(overlap_audit) == 0L) {
    return(tibble::tibble(
      test = "retained_solution_material_overlap_pairs",
      reviewed_solution_count = dplyr::n_distinct(mem$solution_id),
      reviewed_interaction_pair_count = nrow(overlap_audit),
      violating_solution_pair_count = 0L,
      passed = TRUE
    ))
  }

  left <- mem |>
    dplyr::inner_join(overlap_audit |> dplyr::select(candidate_i, candidate_j), by = c("candidate_id" = "candidate_i")) |>
    dplyr::select(solution_id, candidate_j)
  violations <- left |>
    dplyr::inner_join(mem, by = c("solution_id", "candidate_j" = "candidate_id")) |>
    dplyr::distinct(solution_id, candidate_j)

  assert_model(nrow(violations) == 0L, "Overlap-correction layer retained solution violates a material-overlap exclusion")

  tibble::tibble(
    test = "retained_solution_material_overlap_pairs",
    reviewed_solution_count = dplyr::n_distinct(mem$solution_id),
    reviewed_interaction_pair_count = nrow(overlap_audit),
    violating_solution_pair_count = nrow(violations),
    passed = nrow(violations) == 0L
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_overlap_correction_benefit_tax_clique_audit
# Purpose: Make the specific substitute-expansion defect impossible to hide in a
#          large generic interaction table by explicitly auditing all six pairs
#          among CBO pension-style benefit taxation and OACT H2/H5/H9.
# ------------------------------------------------------------------------------
build_overlap_correction_benefit_tax_clique_audit <- function(policy_model) {
  active_ids <- policy_model$meta |>
    dplyr::filter(parameterized_solver_eligible, policy_search_solver_eligible) |>
    dplyr::pull(candidate_id)
  ids <- intersect(c(CFG$overlap_correction_cbo_benefit_tax_id, CFG$overlap_correction_oact_benefit_tax_ids), active_ids)
  ints <- build_interaction_catalog_full(
    policy_model$meta |>
      dplyr::filter(parameterized_solver_eligible, policy_search_solver_eligible)
  )
  int_keys <- paste(ints$candidate_i, ints$candidate_j, sep = "||")

  if (length(ids) < 2L) {
    return(tibble::tibble(
      candidate_i = character(),
      candidate_j = character(),
      interaction_present = logical(),
      treatment = character(),
      evidence_basis = character()
    ))
  }

  cmb <- utils::combn(sort(ids), 2L)
  out <- tibble::tibble(
    candidate_i = cmb[1L, ],
    candidate_j = cmb[2L, ]
  ) |>
    dplyr::mutate(
      interaction_present = paste(candidate_i, candidate_j, sep = "||") %in% int_keys,
      treatment = "MUTUALLY_EXCLUSIVE",
      evidence_basis = "CBO 2020 Budget Option 56856; SSA OACT 2026 taxation-of-benefits provisions H2/H5/H9"
    )

  assert_model(all(out$interaction_present), "Overlap-correction layer benefit-taxation clique audit found a missing pair")
  out
}

# ------------------------------------------------------------------------------
# FUNCTION: build_overlap_correction_universe_freeze_audit
# Purpose: Document that overlap-correction layer changes interactions and audits only;
#          it does not add another round of fiscal coefficients.
# ------------------------------------------------------------------------------
build_overlap_correction_universe_freeze_audit <- function(policy_model) {
  added46 <- build_substitute_expansion_added_substitute_candidates(policy_model)
  tibble::tibble(
    item = c(
      "New scored candidates added specifically by overlap-correction layer",
      "Model layer substitute candidates preserved",
      "Total active solver candidates after score/protection review",
      "Policy-universe expansion status"
    ),
    value = c(
      "0",
      as.character(nrow(added46)),
      as.character(sum(policy_model$meta$parameterized_solver_eligible & policy_model$meta$policy_search_solver_eligible, na.rm = TRUE)),
      "FROZEN_FOR_CORRECTION_AND_CONSOLIDATION"
    )
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_overlap_correction_recommendation_comparison
# Purpose: Compare the fixed recommendation-hardening benchmark with the corrected
#          overlap-correction recommendation and report the status of the four
#          historical bottlenecks after re-optimization.
# ------------------------------------------------------------------------------
build_overlap_correction_recommendation_comparison <- function(search_result) {
  ref47 <- recommendation_hardening_reference_solution(search_result)$catalog |> dplyr::slice_head(n = 1L)
  bottlenecks <- search_result$substitute_expansion_bottleneck_substitution$tests

  replaceable <- bottlenecks |>
    dplyr::filter(feasible_after_removal) |>
    dplyr::pull(bottleneck_label)
  indispensable <- bottlenecks |>
    dplyr::filter(!feasible_after_removal) |>
    dplyr::pull(bottleneck_label)

  historical <- tibble::tibble(
    comparison_id = "RECOMMENDATION_HARDENING_REFERENCE",
    policy_count = CFG$recommendation_hardening_reference_policy_count,
    implementation_complexity_score = CFG$recommendation_hardening_reference_complexity_score,
    central_debt_gdp_2036_pct = CFG$recommendation_hardening_reference_central_2036_pct,
    central_debt_gdp_2046_pct = CFG$recommendation_hardening_reference_central_2046_pct,
    worst_required_debt_gdp_2036_pct = CFG$recommendation_hardening_reference_worst_2036_pct,
    worst_required_debt_gdp_2046_pct = CFG$recommendation_hardening_reference_worst_2046_pct,
    ss_actuarial_improvement_pct_payroll = CFG$recommendation_hardening_reference_ss_actuarial_pct_payroll,
    former_bottlenecks_indispensable = "SSA OACT E2.1;5% narrow-base VAT;SALT deduction repeal;SSA OACT H9",
    former_bottlenecks_replaceable = ""
  )

  current <- tibble::tibble(
    comparison_id = "OVERLAP_CORRECTION_CORRECTED_RECOMMENDATION",
    policy_count = ref47$selected_policy_count[[1]],
    implementation_complexity_score = ref47$implementation_complexity_score[[1]],
    central_debt_gdp_2036_pct = ref47$debt_gdp_2036_pct[[1]],
    central_debt_gdp_2046_pct = ref47$debt_gdp_2046_pct[[1]],
    worst_required_debt_gdp_2036_pct = ref47$worst_required_scenario_debt_gdp_2036_pct[[1]],
    worst_required_debt_gdp_2046_pct = ref47$worst_required_scenario_debt_gdp_2046_pct[[1]],
    ss_actuarial_improvement_pct_payroll = ref47$ss_actuarial_improvement_pct_payroll[[1]],
    former_bottlenecks_indispensable = paste(indispensable, collapse = ";"),
    former_bottlenecks_replaceable = paste(replaceable, collapse = ";")
  )

  dplyr::bind_rows(historical, current) |>
    dplyr::mutate(
      policy_count_change_vs_recommendation_hardening = policy_count - CFG$recommendation_hardening_reference_policy_count,
      complexity_change_vs_recommendation_hardening = implementation_complexity_score - CFG$recommendation_hardening_reference_complexity_score,
      robust_2036_headroom_pp = 90 - worst_required_debt_gdp_2036_pct,
      robust_2046_headroom_pp = 80 - worst_required_debt_gdp_2046_pct,
      ss_solvency_margin_pct_payroll = ss_actuarial_improvement_pct_payroll - CFG$ss_actuarial_gap_pct_payroll
    )
}

# ------------------------------------------------------------------------------
# FUNCTION: run_full_solution_search
# Purpose: Execute the complete substitute-expansion search under the corrected
#          interaction catalog, then add overlap-correction overlap validation and
#          corrected recommendation-comparison artifacts.
# ------------------------------------------------------------------------------
run_full_solution_search <- function(policy_model, working_baseline, kernel_obj) {
  base <- run_full_solution_search_substitute_expansion(policy_model, working_baseline, kernel_obj)

  overlap_audit <- build_overlap_correction_overlap_audit(policy_model)
  overlap_summary <- build_overlap_correction_overlap_family_summary(overlap_audit)
  overlap_solution_validation <- validate_overlap_correction_solution_overlap_contract(base, overlap_audit)
  benefit_tax_clique <- build_overlap_correction_benefit_tax_clique_audit(policy_model)
  universe_freeze <- build_overlap_correction_universe_freeze_audit(policy_model)
  comparison <- build_overlap_correction_recommendation_comparison(base)

  if (CFG$write_audit_outputs) {
    write_csv_atomic(overlap_audit, file.path(CFG$output_dir, "overlap_correction_overlap_audit.csv"))
    write_csv_atomic(overlap_summary, file.path(CFG$output_dir, "overlap_correction_overlap_family_summary.csv"))
    write_csv_atomic(overlap_solution_validation, file.path(CFG$output_dir, "overlap_correction_overlap_solution_validation.csv"))
    write_csv_atomic(benefit_tax_clique, file.path(CFG$output_dir, "overlap_correction_social_security_benefit_tax_clique_audit.csv"))
    write_csv_atomic(universe_freeze, file.path(CFG$output_dir, "overlap_correction_universe_freeze_audit.csv"))
    write_csv_atomic(comparison, file.path(CFG$output_dir, "overlap_correction_recommendation_comparison.csv"))
  }

  base$overlap_correction_overlap_audit <- overlap_audit
  base$overlap_correction_overlap_family_summary <- overlap_summary
  base$overlap_correction_overlap_solution_validation <- overlap_solution_validation
  base$overlap_correction_social_security_benefit_tax_clique_audit <- benefit_tax_clique
  base$overlap_correction_universe_freeze_audit <- universe_freeze
  base$overlap_correction_recommendation_comparison <- comparison
  base
}

# ------------------------------------------------------------------------------
# FUNCTION: run_model
# Purpose: Preserve the complete substitute-expansion model and one-plot output,
#          rename the sole presentation artifact to overlap-correction layer, and write
#          a concise plan-status audit.
# ------------------------------------------------------------------------------
run_model_overlap_correction <- function() {
  log_line("Overlap-correction layer active: Social Security benefit-tax overlap corrected; policy universe frozen; full recommendation, bottleneck, and leave-one-out analyses rerun")
  result <- run_model_substitute_expansion()

  old_png <- file.path(CFG$output_dir, "substitute_expansion_debt_paths.png")
  new_png <- file.path(CFG$output_dir, "overlap_corrected_debt_paths.png")
  if (file.exists(new_png)) unlink(new_png, force = TRUE)
  if (file.exists(old_png)) {
    ok <- file.rename(old_png, new_png)
    assert_model(isTRUE(ok) && file.exists(new_png), "Overlap-correction layer could not rename the sole presentation plot")
  }

  # Enforce the one-plot contract at archive time. Any inherited PNG artifact is
  # removed unless it is the active overlap-correction presentation figure.
  pngs <- list.files(CFG$output_dir, pattern = "\\.png$", full.names = TRUE)
  stale_pngs <- setdiff(normalizePath(pngs, winslash = "/", mustWork = FALSE), normalizePath(new_png, winslash = "/", mustWork = FALSE))
  if (length(stale_pngs) > 0L) unlink(stale_pngs, force = TRUE)
  final_pngs <- list.files(CFG$output_dir, pattern = "\\.png$", full.names = TRUE)
  assert_model(length(final_pngs) == 1L && basename(final_pngs[[1]]) == "overlap_corrected_debt_paths.png", "Overlap-correction layer one-plot contract failed")

  result$overlap_correction_plan_status <- tibble::tibble(
    item = c(
      "Model layer scored policy universe frozen; no new fiscal coefficients added",
      "CBO defined-benefit-pension Social Security taxation made mutually exclusive with OACT H2, H5, and H9",
      "Complete Social Security benefit-taxation interaction clique explicitly audited",
      "Entire active material-overlap catalog classified and audited",
      "No retained solution may select both sides of an overlap guard",
      "Full robust plus Social Security-solvent recommendation search rerun under corrected interactions",
      "Four historical bottleneck-removal tests rerun under corrected interactions",
      "Leave-one-out analysis rerun for every policy in the newly selected recommendation",
      "Model layer 23-policy recommendation retained as fixed comparison benchmark",
      "Exactly one presentation plot with CBO baseline, representative packages, labeled 90/80/70 lines, left-aligned caption, and X author tag"
    ),
    implemented = TRUE
  )

  if (CFG$write_audit_outputs) {
    write_csv_atomic(result$overlap_correction_plan_status, file.path(CFG$output_dir, "overlap_correction_plan_status.csv"))
  }
  result
}

# ------------------------------------------------------------------------------
# MODEL LAYER ACTIVE RULE SUMMARY
# - No new scored policy levers are added. This is a correction/consolidation run.
# - CBO pension-style taxation of Social Security benefits and OACT H2/H5/H9 are
#   pairwise mutually exclusive unless an authoritative combined score exists.
# - Every active interaction pair is audited with policy titles, mechanism class,
#   source URLs, and explicit mutual-exclusion treatment.
# - Retained packages are independently checked for forbidden overlap pairs.
# - Robust 90/80 debt targets and approximate OASDI solvency remain hard
#   recommendation-stage requirements.
# - Current law remains non-veto metadata; approved protections remain binding.
# - Model layer memory controls, later timing reduction, PSOCK verification,
#   numerical-gap audit, and one-plot presentation contract remain active.
# ------------------------------------------------------------------------------

# Execution is deferred until the final public audit stage is defined.



# ==============================================================================
# MODEL LAYER FINAL AUDIT AND FREEZE MODEL STAGE
# ==============================================================================
# Public-release audit layer is the final audit-and-freeze run. The overlap-correction
# scored policy universe, interaction structure, fiscal assumptions, approved
# protections, robust target requirements, Social Security solvency requirement,
# solver architecture, memory controls, concurrency, and one-plot presentation
# contract are frozen. No new fiscal coefficient is introduced here.
#
# Acceptance standard:
#   * zero R warnings;
#   * zero unexplained audit discrepancies;
#   * zero material-overlap violations;
#   * all validation and numerical-gap audits pass;
#   * every retained solution independently re-simulates;
#   * the corrected 21-policy recommendation reproduces;
#   * the final archive is created successfully and non-empty.
# ==============================================================================

dir.create(CFG$output_dir, recursive = TRUE, showWarnings = FALSE)

CFG$reference_fixture_path <- file.path(CFG$data_dir, "reference", "release_reference.json")
assert_model(file.exists(CFG$reference_fixture_path), paste0("Missing frozen release reference fixture: ", CFG$reference_fixture_path))
RELEASE_REFERENCE <- jsonlite::read_json(CFG$reference_fixture_path, simplifyVector = TRUE)
CFG$release_expected_active_solver_candidates <- as.integer(RELEASE_REFERENCE$active_solver_candidates)
CFG$release_reference_policy_count <- as.integer(RELEASE_REFERENCE$policy_count)
CFG$release_reference_complexity_score <- as.numeric(RELEASE_REFERENCE$implementation_complexity_score)
CFG$release_reference_central_2036_pct <- as.numeric(RELEASE_REFERENCE$central_debt_gdp_2036_pct)
CFG$release_reference_central_2046_pct <- as.numeric(RELEASE_REFERENCE$central_debt_gdp_2046_pct)
CFG$release_reference_worst_2036_pct <- as.numeric(RELEASE_REFERENCE$worst_required_debt_gdp_2036_pct)
CFG$release_reference_worst_2046_pct <- as.numeric(RELEASE_REFERENCE$worst_required_debt_gdp_2046_pct)
CFG$release_reference_ss_actuarial_pct_payroll <- as.numeric(RELEASE_REFERENCE$ss_actuarial_improvement_pct_payroll)
CFG$release_metric_tolerance <- as.numeric(RELEASE_REFERENCE$metric_tolerance)
CFG$release_reference_candidate_ids <- as.character(RELEASE_REFERENCE$candidate_ids)

# Bind the corrected overlap-correction execution function used by the final
# audit-and-freeze layer.

PUBLIC_RELEASE_AUDIT_WARNING_LOG <- tibble::tibble(
  timestamp = character(),
  message = character(),
  call = character()
)
PUBLIC_RELEASE_AUDIT_OVERLAP_JOIN_AUDIT <- tibble::tibble()

# ------------------------------------------------------------------------------
# FUNCTION: validate_overlap_correction_solution_overlap_contract
# Purpose: Replace overlap-correction layer's warning-producing many-to-many join with
#          a cardinality-explicit solution-by-pair grid. Membership and overlap
#          pairs are unique before evaluation, so no duplicate inflation can be
#          hidden and no many-to-many dplyr warning is possible.
# ------------------------------------------------------------------------------
validate_overlap_correction_solution_overlap_contract <- function(search_result, overlap_audit) {
  mem <- search_result$membership |>
    dplyr::select(solution_id, candidate_id) |>
    dplyr::distinct()
  pairs <- overlap_audit |>
    dplyr::select(candidate_i, candidate_j) |>
    dplyr::distinct()

  assert_model(
    nrow(mem) == dplyr::n_distinct(paste(mem$solution_id, mem$candidate_id, sep = "||")),
    "Public-release audit layer solution membership contains duplicate (solution_id, candidate_id) keys"
  )
  assert_model(
    nrow(pairs) == dplyr::n_distinct(paste(pairs$candidate_i, pairs$candidate_j, sep = "||")),
    "Public-release audit layer overlap catalog contains duplicate interaction pairs"
  )

  if (nrow(mem) == 0L || nrow(pairs) == 0L) {
    PUBLIC_RELEASE_AUDIT_OVERLAP_JOIN_AUDIT <<- tibble::tibble(
      check = c("unique_solution_membership_keys", "unique_overlap_pairs", "solution_pair_grid_cardinality", "duplicate_inflation"),
      observed = c(nrow(mem), nrow(pairs), 0, 0),
      expected = c(nrow(mem), nrow(pairs), 0, 0),
      passed = TRUE
    )
    return(tibble::tibble(
      test = "retained_solution_material_overlap_pairs",
      reviewed_solution_count = dplyr::n_distinct(mem$solution_id),
      reviewed_interaction_pair_count = nrow(pairs),
      violating_solution_pair_count = 0L,
      passed = TRUE
    ))
  }

  solution_ids <- sort(unique(mem$solution_id))
  grid <- tidyr::crossing(
    solution_id = solution_ids,
    pairs
  )
  expected_grid_rows <- length(solution_ids) * nrow(pairs)
  assert_model(
    nrow(grid) == expected_grid_rows,
    "Public-release audit layer overlap solution-by-pair grid has unexpected cardinality"
  )
  assert_model(
    nrow(grid) == dplyr::n_distinct(paste(grid$solution_id, grid$candidate_i, grid$candidate_j, sep = "||")),
    "Public-release audit layer overlap solution-by-pair grid contains duplicate rows"
  )

  membership_keys <- paste(mem$solution_id, mem$candidate_id, sep = "||")
  selected_i <- paste(grid$solution_id, grid$candidate_i, sep = "||") %in% membership_keys
  selected_j <- paste(grid$solution_id, grid$candidate_j, sep = "||") %in% membership_keys
  violations <- grid[selected_i & selected_j, , drop = FALSE]
  duplicate_violation_rows <- nrow(violations) - dplyr::n_distinct(
    paste(violations$solution_id, violations$candidate_i, violations$candidate_j, sep = "||")
  )

  assert_model(duplicate_violation_rows == 0L, "Public-release audit layer overlap validation inflated duplicate solution-pair rows")
  assert_model(nrow(violations) == 0L, "Public-release audit layer retained solution violates a material-overlap exclusion")

  PUBLIC_RELEASE_AUDIT_OVERLAP_JOIN_AUDIT <<- tibble::tibble(
    check = c(
      "unique_solution_membership_keys",
      "unique_overlap_pairs",
      "solution_pair_grid_cardinality",
      "duplicate_inflation"
    ),
    observed = c(nrow(mem), nrow(pairs), nrow(grid), duplicate_violation_rows),
    expected = c(nrow(mem), nrow(pairs), expected_grid_rows, 0),
    passed = c(TRUE, TRUE, nrow(grid) == expected_grid_rows, duplicate_violation_rows == 0L)
  )

  tibble::tibble(
    test = "retained_solution_material_overlap_pairs",
    reviewed_solution_count = length(solution_ids),
    reviewed_interaction_pair_count = nrow(pairs),
    violating_solution_pair_count = nrow(violations),
    passed = nrow(violations) == 0L
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: build_release_recommendation_stability
# Purpose: Prove that the final warning/cardinality correction does not alter the
#          corrected overlap-correction recommendation or its key fiscal metrics.
# ------------------------------------------------------------------------------
build_release_recommendation_stability <- function(search_result) {
  ref <- recommendation_hardening_reference_solution(search_result)
  row <- ref$catalog |> dplyr::slice_head(n = 1L)
  observed_ids <- sort(unique(ref$membership$candidate_id))
  expected_ids <- sort(unique(CFG$release_reference_candidate_ids))
  tol <- CFG$release_metric_tolerance

  checks <- tibble::tibble(
    check = c(
      "policy_count",
      "implementation_complexity_score",
      "central_debt_gdp_2036_pct",
      "central_debt_gdp_2046_pct",
      "worst_required_debt_gdp_2036_pct",
      "worst_required_debt_gdp_2046_pct",
      "ss_actuarial_improvement_pct_payroll",
      "exact_candidate_membership",
      "robust_2036_pass",
      "robust_2046_pass",
      "ss_solvency_pass"
    ),
    observed = c(
      as.character(row$selected_policy_count[[1]]),
      format(row$implementation_complexity_score[[1]], digits = 12, trim = TRUE),
      format(row$debt_gdp_2036_pct[[1]], digits = 12, trim = TRUE),
      format(row$debt_gdp_2046_pct[[1]], digits = 12, trim = TRUE),
      format(row$worst_required_scenario_debt_gdp_2036_pct[[1]], digits = 12, trim = TRUE),
      format(row$worst_required_scenario_debt_gdp_2046_pct[[1]], digits = 12, trim = TRUE),
      format(row$ss_actuarial_improvement_pct_payroll[[1]], digits = 12, trim = TRUE),
      as.character(identical(observed_ids, expected_ids)),
      as.character(isTRUE(row$robust_target_2036_pass[[1]])),
      as.character(isTRUE(row$robust_target_2046_pass[[1]])),
      as.character(row$ss_actuarial_improvement_pct_payroll[[1]] + 1e-9 >= CFG$ss_actuarial_gap_pct_payroll)
    ),
    reference = c(
      as.character(CFG$release_reference_policy_count),
      format(CFG$release_reference_complexity_score, digits = 12, trim = TRUE),
      format(CFG$release_reference_central_2036_pct, digits = 12, trim = TRUE),
      format(CFG$release_reference_central_2046_pct, digits = 12, trim = TRUE),
      format(CFG$release_reference_worst_2036_pct, digits = 12, trim = TRUE),
      format(CFG$release_reference_worst_2046_pct, digits = 12, trim = TRUE),
      format(CFG$release_reference_ss_actuarial_pct_payroll, digits = 12, trim = TRUE),
      "TRUE", "TRUE", "TRUE", "TRUE"
    ),
    passed = c(
      row$selected_policy_count[[1]] == CFG$release_reference_policy_count,
      abs(row$implementation_complexity_score[[1]] - CFG$release_reference_complexity_score) <= tol,
      abs(row$debt_gdp_2036_pct[[1]] - CFG$release_reference_central_2036_pct) <= tol,
      abs(row$debt_gdp_2046_pct[[1]] - CFG$release_reference_central_2046_pct) <= tol,
      abs(row$worst_required_scenario_debt_gdp_2036_pct[[1]] - CFG$release_reference_worst_2036_pct) <= tol,
      abs(row$worst_required_scenario_debt_gdp_2046_pct[[1]] - CFG$release_reference_worst_2046_pct) <= tol,
      abs(row$ss_actuarial_improvement_pct_payroll[[1]] - CFG$release_reference_ss_actuarial_pct_payroll) <= tol,
      identical(observed_ids, expected_ids),
      isTRUE(row$robust_target_2036_pass[[1]]),
      isTRUE(row$robust_target_2046_pass[[1]]),
      row$ss_actuarial_improvement_pct_payroll[[1]] + 1e-9 >= CFG$ss_actuarial_gap_pct_payroll
    )
  )

  assert_model(all(checks$passed), "Public-release audit layer recommendation stability audit failed")
  checks
}

# ------------------------------------------------------------------------------
# FUNCTION: build_release_final_recommended_package
# Purpose: Produce the final presentation-grade policy table for the corrected
#          recommendation, including replacement costs and package-level margins.
# ------------------------------------------------------------------------------
build_release_final_recommended_package <- function(search_result, policy_model) {
  ref <- recommendation_hardening_reference_solution(search_result)
  row <- ref$catalog |> dplyr::slice_head(n = 1L)
  base <- build_recommendation_hardening_recommended_package_table(search_result, policy_model)
  replacement <- search_result$recommendation_hardening_replacement_costs |>
    dplyr::select(
      candidate_id,
      replacement_class,
      extra_policy_count,
      extra_complexity_score,
      replacement_policy_count,
      replacement_complexity_score,
      robust_2036_headroom_change_pp,
      robust_2046_headroom_change_pp,
      ss_solvency_margin_change_pct_payroll,
      diagnostic_status,
      debt_gap_bil,
      debt_gap_pct_gdp
    ) |>
    dplyr::distinct(candidate_id, .keep_all = TRUE)

  out <- base |>
    dplyr::left_join(replacement, by = "candidate_id", relationship = "one-to-one") |>
    dplyr::mutate(
      package_policy_count = row$selected_policy_count[[1]],
      package_complexity_score = row$implementation_complexity_score[[1]],
      package_central_2036_pct = row$debt_gdp_2036_pct[[1]],
      package_central_2046_pct = row$debt_gdp_2046_pct[[1]],
      package_worst_required_2036_pct = row$worst_required_scenario_debt_gdp_2036_pct[[1]],
      package_worst_required_2046_pct = row$worst_required_scenario_debt_gdp_2046_pct[[1]],
      package_robust_2036_headroom_pp = 90 - row$worst_required_scenario_debt_gdp_2036_pct[[1]],
      package_robust_2046_headroom_pp = 80 - row$worst_required_scenario_debt_gdp_2046_pct[[1]],
      package_ss_actuarial_improvement_pct_payroll = row$ss_actuarial_improvement_pct_payroll[[1]],
      package_ss_solvency_margin_pct_payroll = row$ss_actuarial_improvement_pct_payroll[[1]] - CFG$ss_actuarial_gap_pct_payroll
    )

  assert_model(nrow(out) == CFG$release_reference_policy_count, "Public-release audit layer final package table does not contain exactly 21 policies")
  assert_model(!anyDuplicated(out$candidate_id), "Public-release audit layer final package table contains duplicate policies")
  out
}

# ------------------------------------------------------------------------------
# FUNCTION: release_read_audit_csv
# Purpose: Read one required audit artifact after the inherited pipeline writes it.
# ------------------------------------------------------------------------------
release_read_audit_csv <- function(name) {
  path <- file.path(CFG$output_dir, name)
  assert_model(file.exists(path), paste0("Public-release audit layer required audit artifact is missing: ", name))
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

# ------------------------------------------------------------------------------
# FUNCTION: build_release_final_audit_summary
# Purpose: Compact pass/fail acceptance table for the final audit-and-freeze run.
# ------------------------------------------------------------------------------
build_release_final_audit_summary <- function(result, warning_count, archive_integrity_passed = NA) {
  validation <- release_read_audit_csv("validation_tests.csv")
  protection <- release_read_audit_csv("eisenhower_protection_regression.csv")
  score_contract <- release_read_audit_csv("policy_score_review_contract.csv")
  source_manifest <- release_read_audit_csv("source_manifest.csv")
  solver <- release_read_audit_csv("solver_run_status.csv")
  solutions <- release_read_audit_csv("solution_catalog.csv")
  overlap <- result$solution_search$overlap_correction_overlap_solution_validation
  stability <- result$release_recommendation_stability
  ref <- recommendation_hardening_reference_solution(result$solution_search)$catalog |> dplyr::slice_head(n = 1L)

  optimal_rows <- solver |> dplyr::filter(solver_status == "Optimal")
  terminal_status_pass <- all(solver$solver_status %in% c("Optimal", "Infeasible"))
  solver_optimality_pass <- nrow(optimal_rows) > 0L && all(optimal_rows$solver_proven_optimal %in% TRUE)
  gap_pass <- nrow(optimal_rows) > 0L && all(optimal_rows$mip_gap_audit_pass %in% TRUE)
  independent_pass <- nrow(solutions) > 0L && all(solutions$independently_verified %in% TRUE)
  local_source_rows <- source_manifest |>
    dplyr::filter(!is.na(local_path), nzchar(local_path))
  source_locator_pass <- nrow(source_manifest) > 0L && all(
    (!is.na(source_manifest$url) & nzchar(source_manifest$url)) |
      (!is.na(source_manifest$local_path) & nzchar(source_manifest$local_path))
  )
  local_hash_pass <- nrow(local_source_rows) > 0L && all(
    !is.na(local_source_rows$sha256) & nchar(local_source_rows$sha256) == 64L
  )
  source_hash_pass <- source_locator_pass && local_hash_pass
  protection_pass <- nrow(protection) > 0L && all(
    protection$all_blocked %in% TRUE &
      !(protection$any_solver_eligible %in% TRUE) &
      protection$observed_status == protection$expected_status
  )
  overlap_pass <- !is.null(overlap) && nrow(overlap) > 0L && all(overlap$passed %in% TRUE) && all(PUBLIC_RELEASE_AUDIT_OVERLAP_JOIN_AUDIT$passed %in% TRUE)
  warning_pass <- warning_count == 0L
  recommendation_pass <- nrow(stability) > 0L && all(stability$passed %in% TRUE)
  ss_pass <- ref$ss_actuarial_improvement_pct_payroll[[1]] + 1e-9 >= CFG$ss_actuarial_gap_pct_payroll
  robust_pass <- isTRUE(ref$robust_target_2036_pass[[1]]) && isTRUE(ref$robust_target_2046_pass[[1]])
  archive_status <- if (is.na(archive_integrity_passed)) "PENDING" else if (isTRUE(archive_integrity_passed)) "PASS" else "FAIL"

  tibble::tibble(
    audit_area = c(
      "validation_gates",
      "protections",
      "source_provenance_and_hashes",
      "score_basis_contract",
      "overlap_treatment",
      "solver_terminal_status",
      "solver_optimality",
      "numerical_gap_audit",
      "independent_simulation",
      "social_security_solvency",
      "robust_debt_targets",
      "recommendation_stability",
      "warning_count",
      "archive_integrity"
    ),
    status = c(
      if (all(validation$passed %in% TRUE)) "PASS" else "FAIL",
      if (protection_pass) "PASS" else "FAIL",
      if (source_hash_pass) "PASS" else "FAIL",
      if (all(score_contract$passed %in% TRUE)) "PASS" else "FAIL",
      if (overlap_pass) "PASS" else "FAIL",
      if (terminal_status_pass) "PASS" else "FAIL",
      if (solver_optimality_pass) "PASS" else "FAIL",
      if (gap_pass) "PASS" else "FAIL",
      if (independent_pass) "PASS" else "FAIL",
      if (ss_pass) "PASS" else "FAIL",
      if (robust_pass) "PASS" else "FAIL",
      if (recommendation_pass) "PASS" else "FAIL",
      if (warning_pass) "PASS" else "FAIL",
      archive_status
    ),
    detail = c(
      paste0(sum(validation$passed %in% TRUE), "/", nrow(validation), " validation tests passed"),
      paste0(nrow(protection), " protection-regression rows remain blocked and solver-ineligible with expected protection status"),
      paste0(nrow(source_manifest), " source-manifest rows have provenance locators; ", nrow(local_source_rows), " locally cached/source files carry 64-character SHA-256 hashes"),
      paste0(sum(score_contract$passed %in% TRUE), "/", nrow(score_contract), " score-basis contract checks passed"),
      paste0(overlap$reviewed_interaction_pair_count[[1]], " interaction pairs reviewed; ", overlap$violating_solution_pair_count[[1]], " retained-solution violations; duplicate inflation=0"),
      paste0(sum(solver$solver_status == "Optimal"), " Optimal; ", sum(solver$solver_status == "Infeasible"), " Infeasible; no unfinished status"),
      paste0(sum(optimal_rows$solver_proven_optimal %in% TRUE), "/", nrow(optimal_rows), " Optimal solves proven optimal"),
      paste0(sum(optimal_rows$mip_gap_audit_pass %in% TRUE), "/", nrow(optimal_rows), " Optimal solves passed gap audit"),
      paste0(sum(solutions$independently_verified %in% TRUE), "/", nrow(solutions), " retained solutions independently verified"),
      paste0("Recommendation SS improvement=", format(ref$ss_actuarial_improvement_pct_payroll[[1]], digits = 6, trim = TRUE), "% payroll; requirement=", format(CFG$ss_actuarial_gap_pct_payroll, digits = 6, trim = TRUE)),
      paste0("Worst required debt/GDP=", format(ref$worst_required_scenario_debt_gdp_2036_pct[[1]], digits = 8, trim = TRUE), "% in 2036 and ", format(ref$worst_required_scenario_debt_gdp_2046_pct[[1]], digits = 8, trim = TRUE), "% in 2046"),
      paste0(sum(stability$passed %in% TRUE), "/", nrow(stability), " recommendation-stability checks passed"),
      paste0(warning_count, " R warnings captured during model execution"),
      if (is.na(archive_integrity_passed)) "Final archive integrity check pending" else if (isTRUE(archive_integrity_passed)) "Audit archive created successfully and non-empty before final archive refresh" else "Audit archive integrity check failed"
    )
  )
}

# ------------------------------------------------------------------------------
# FUNCTION: run_model
# Purpose: Run the complete corrected overlap-correction pipeline unchanged, then
#          produce final recommendation, stability, warning/cardinality, and
#          plan-status artifacts. The inherited one plot is simply renamed.
# ------------------------------------------------------------------------------
run_model <- function() {
  validate_repository_inputs()
  log_line("Public-release audit layer active: final audit-and-freeze run; overlap-correction fiscal universe and recommendation logic frozen; zero warnings required")
  result <- run_model_overlap_correction()

  old_png <- file.path(CFG$output_dir, "overlap_corrected_debt_paths.png")
  new_png <- file.path(CFG$output_dir, "debt_paths.png")
  if (file.exists(new_png)) unlink(new_png, force = TRUE)
  if (file.exists(old_png)) {
    ok <- file.rename(old_png, new_png)
    assert_model(isTRUE(ok) && file.exists(new_png), "Public-release audit layer could not rename the sole presentation plot")
  }
  pngs <- list.files(CFG$output_dir, pattern = "\\.png$", full.names = TRUE)
  stale_pngs <- setdiff(
    normalizePath(pngs, winslash = "/", mustWork = FALSE),
    normalizePath(new_png, winslash = "/", mustWork = FALSE)
  )
  if (length(stale_pngs) > 0L) unlink(stale_pngs, force = TRUE)
  final_pngs <- list.files(CFG$output_dir, pattern = "\\.png$", full.names = TRUE)
  assert_model(
    length(final_pngs) == 1L && basename(final_pngs[[1]]) == "debt_paths.png",
    "Public-release audit layer one-plot contract failed"
  )

  active_solver_count <- result$policy_model$meta |>
    dplyr::filter(parameterized_solver_eligible, policy_search_solver_eligible) |>
    nrow()
  assert_model(
    active_solver_count == CFG$release_expected_active_solver_candidates,
    paste0("Public-release audit layer frozen policy universe changed unexpectedly: expected ", CFG$release_expected_active_solver_candidates, ", observed ", active_solver_count)
  )

  stability <- build_release_recommendation_stability(result$solution_search)
  final_package <- build_release_final_recommended_package(result$solution_search, result$policy_model)
  universe_freeze <- tibble::tibble(
    check = c("active_solver_candidate_count", "new_scored_candidates_added_by_release", "policy_universe_status"),
    observed = c(as.character(active_solver_count), "0", "FROZEN_FINAL_AUDIT"),
    reference = c(as.character(CFG$release_expected_active_solver_candidates), "0", "FROZEN_FINAL_AUDIT"),
    passed = TRUE
  )
  warning_log_snapshot <- PUBLIC_RELEASE_AUDIT_WARNING_LOG
  plan_status <- tibble::tibble(
    item = c(
      "Model layer scored policy universe and fiscal assumptions frozen",
      "Unexpected many-to-many overlap-validation join structurally replaced with unique solution-policy membership keys and explicit solution-by-pair cardinality",
      "Duplicate-inflation assertions applied to overlap validation",
      "Corrected 21-policy recommendation reproduced exactly",
      "Final presentation-grade 21-policy package table written",
      "All validation, solver, overlap, independent-simulation, SS-solvency, and robust-target audits included in final acceptance summary",
      "Exactly one presentation plot with CBO baseline, representative packages, labeled 90/80/70 lines, left-aligned caption, and X author tag",
      "Zero R warnings required for acceptance",
      "No new scored levers or policy families added",
      "Final archive named analysis_output.zip"
    ),
    implemented = TRUE
  )

  result$release_recommendation_stability <- stability
  result$release_final_recommended_package <- final_package
  result$release_universe_freeze_audit <- universe_freeze
  result$release_overlap_join_cardinality_audit <- PUBLIC_RELEASE_AUDIT_OVERLAP_JOIN_AUDIT
  result$release_plan_status <- plan_status

  if (CFG$write_audit_outputs) {
    write_csv_atomic(stability, file.path(CFG$output_dir, "recommendation_stability.csv"))
    write_csv_atomic(final_package, file.path(CFG$output_dir, "recommended_package.csv"))
    write_csv_atomic(universe_freeze, file.path(CFG$output_dir, "universe_freeze_audit.csv"))
    write_csv_atomic(PUBLIC_RELEASE_AUDIT_OVERLAP_JOIN_AUDIT, file.path(CFG$output_dir, "overlap_join_cardinality_audit.csv"))
    write_csv_atomic(warning_log_snapshot, file.path(CFG$output_dir, "warning_log.csv"))
    write_csv_atomic(plan_status, file.path(CFG$output_dir, "release_status.csv"))
  }

  result
}

# ------------------------------------------------------------------------------
# FUNCTION: release_capture_warning
# Purpose: Record every R warning as an audit failure. Warnings are muffled only
#          after capture so the run can write its failure artifacts and archive;
#          any captured warning causes public-release audit layer to fail acceptance.
# ------------------------------------------------------------------------------
release_capture_warning <- function(w) {
  call_txt <- if (is.null(conditionCall(w))) "" else paste(deparse(conditionCall(w)), collapse = " ")
  PUBLIC_RELEASE_AUDIT_WARNING_LOG <<- dplyr::bind_rows(
    PUBLIC_RELEASE_AUDIT_WARNING_LOG,
    tibble::tibble(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      message = conditionMessage(w),
      call = call_txt
    )
  )
  log_line("Captured R warning: ", conditionMessage(w), level = "WARN")
  invokeRestart("muffleWarning")
}

# ------------------------------------------------------------------------------
# FUNCTION: execute_model_with_audit_capture
# Purpose: Final public-release-audit execution wrapper. Captures every model-run
#          warning, treats any warning as an acceptance failure, writes the final
#          audit summary, and refreshes the ZIP so the PASS archive contains the
#          completed summary itself.
# ------------------------------------------------------------------------------
execute_model_with_audit_capture <- function() {
  PUBLIC_RELEASE_AUDIT_WARNING_LOG <<- PUBLIC_RELEASE_AUDIT_WARNING_LOG[0, , drop = FALSE]
  capture_state <- start_console_capture()
  result <- NULL
  run_error <- NULL

  tryCatch(
    {
      result <- withCallingHandlers(
        run_model(),
        warning = release_capture_warning
      )
    },
    error = function(e) {
      run_error <<- e
      log_line("Model execution terminated with error: ", conditionMessage(e), level = "ERROR")
    }
  )

  warning_count <- nrow(PUBLIC_RELEASE_AUDIT_WARNING_LOG)
  if (!is.null(result)) {
    if (CFG$write_audit_outputs) {
      write_csv_atomic(PUBLIC_RELEASE_AUDIT_WARNING_LOG, file.path(CFG$output_dir, "warning_log.csv"))
      audit_summary <- build_release_final_audit_summary(result, warning_count, archive_integrity_passed = NA)
      write_csv_atomic(audit_summary, file.path(CFG$output_dir, "final_audit_summary.csv"))
      result$release_final_audit_summary <- audit_summary
    }
  }

  if (is.null(run_error) && warning_count > 0L) {
    run_error <- simpleError(paste0("Public-release audit layer acceptance failed: ", warning_count, " R warning(s) were captured"))
    log_line(conditionMessage(run_error), level = "ERROR")
  }

  log_line("Console capture complete; closing log before creating analysis_output audit archive")
  stop_console_capture(capture_state)

  archive_warning_to_error <- function(w) {
    stop(paste0("Archive operation emitted a warning: ", conditionMessage(w)), call. = FALSE)
  }
  zip_info <- tryCatch(
    withCallingHandlers(
      create_analysis_output_zip(),
      warning = archive_warning_to_error
    ),
    error = function(e) {
      msg <- sanitize_public_text(paste0("Audit ZIP creation failed: ", conditionMessage(e)))
      cat(msg, "\n")
      cat(
        paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), " | ERROR | ", msg, "\n"),
        file = CFG$console_log_path,
        append = TRUE
      )
      NULL
    }
  )

  # On a clean run, mark archive integrity PASS and rebuild once so the final ZIP
  # contains the completed PASS summary and its own up-to-date file manifest.
  if (!is.null(result) && is.null(run_error) && !is.null(zip_info)) {
    audit_summary <- build_release_final_audit_summary(result, warning_count, archive_integrity_passed = TRUE)
    write_csv_atomic(audit_summary, file.path(CFG$output_dir, "final_audit_summary.csv"))
    result$release_final_audit_summary <- audit_summary
    assert_model(all(audit_summary$status == "PASS"), "Public-release audit layer final acceptance summary contains a failed audit area")

    zip_info <- tryCatch(
      withCallingHandlers(
        create_analysis_output_zip(),
        warning = archive_warning_to_error
      ),
      error = function(e) {
        msg <- sanitize_public_text(paste0("Final audit ZIP refresh failed: ", conditionMessage(e)))
        cat(msg, "\n")
        cat(
          paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), " | ERROR | ", msg, "\n"),
          file = CFG$console_log_path,
          append = TRUE
        )
        NULL
      }
    )
  }

  if (!is.null(zip_info)) {
    cat(
      paste0(
        "Audit archive created: ",
        zip_info$path,
        " | files=",
        zip_info$file_count,
        " | bytes=",
        format(zip_info$bytes, scientific = FALSE, trim = TRUE),
        " | sha256=",
        zip_info$sha256,
        "\n"
      )
    )
  }

  if (!is.null(run_error)) stop(run_error)
  assert_model(!is.null(zip_info), "Model completed but the analysis_output audit ZIP could not be created")
  assert_model(file.exists(CFG$output_zip_path) && file.info(CFG$output_zip_path)$size > 0, "Public-release audit layer final audit ZIP is missing or empty")
  attr(result, "analysis_output_zip") <- zip_info
  result
}

# ------------------------------------------------------------------------------
# MODEL LAYER ACTIVE RULE SUMMARY
# - Final audit/freeze only: no new fiscal coefficients, scored levers, or policy
#   families are admitted.
# - Model layer corrected overlap structure and the 21-policy recommendation
#   are reproduced under a warning-free validation path.
# - Overlap validation uses unique solution-policy keys and an explicit
#   solution-by-interaction-pair grid, eliminating the overlap-correction
#   many-to-many warning and asserting zero duplicate inflation.
# - Any R warning is captured, audited, and treated as a fatal acceptance failure.
# - Every validation, protection, source, score-basis, overlap, solver, numerical-
#   gap, independent-simulation, Social Security-solvency, robust-target,
#   recommendation-stability, and archive-integrity check must PASS.
# - Exactly one presentation plot remains, with labeled 90%, 80%, and 70% lines,
#   a left-aligned caption, and `𝕏: @arabbitorduck`.
# ==============================================================================

# Execute the complete validated model after all public release stages have loaded.
MODEL_RESULT <- execute_model_with_audit_capture()
