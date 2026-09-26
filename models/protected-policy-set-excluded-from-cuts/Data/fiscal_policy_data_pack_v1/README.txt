Federal Fiscal Optimizer policy source pack v1
Built: 2026-09-11

Contents
- cbo_current_families.csv: 127 CBO current/latest option families.
- policy_candidates.csv: 190 candidate variants.
- policy_annual_flows.csv: 1810 official annual fiscal-flow rows.
- ssa_actuarial_reference.csv: current OASDI target plus selected actuarial crosswalk rows.
- source_manifest.csv: source provenance.
- coverage_audit.csv: coverage by source vintage.

Conventions
- outlay_delta_bil: positive increases outlays; negative reduces outlays.
- revenue_delta_bil: positive increases revenue.
- primary_deficit_delta_bil: outlay change minus revenue change. Negative improves the primary balance.
- annual_profile_status=FULL_OFFICIAL_ANNUAL means the annual path is directly transcribed from an official CBO table, with explicitly noted rounding of source entries marked '*' or similar.
- annual_profile_status=OFFICIAL_TEN_YEAR_TOTAL_ONLY means CBO's current/latest index or option page publishes a ten-year total but not a usable annual table. No annual path has been invented.
- Older official options are retained only because CBO's current/latest Budget Options index continues to list them as the latest version of that option family.
- This pack contains no runtime dependency on cbo.gov.

Known supplemental-source status
- SSA: current 2026 OASDI target is included. Some individual provision categories remain officially on a 2025 Trustees basis because SSA says its 2026 update is still in progress.
- GAO: source is registered, but its downloadable CSV/XLSX cannot be imported through the available browser retrieval layer in this environment. No GAO savings have been fabricated.
- JCT tax expenditures: registered for screening only, not treated as repeal scores.
- MedPAC: registered for Medicare policy screening; unscored savings are not fabricated.
