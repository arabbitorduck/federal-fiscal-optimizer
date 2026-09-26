# Methodology

## 1. Scope and objective

The model evaluates combinations of federal fiscal policies over fiscal years 2026-2046 using mixed-integer linear programming (MILP) and independent post-solve simulation.

A feasible selected package must keep debt held by the public at or below **90% of GDP in 2036** and **80% of GDP in 2046** under every required robustness scenario. It must also provide an approximate 75-year Social Security actuarial improvement of at least **4.42% of taxable payroll**.

The final recommendation is selected from the robust and Social Security-solvent recommendation frontier by minimizing the model's implementation-complexity score, then policy count. The objective is therefore not to maximize fiscal consolidation after the required constraints have been met.

## 2. Baseline construction

The latest complete aggregate CBO baseline used by the model is the **February 2026** budget and economic baseline.

The working baseline then incorporates CBO's **August 20, 2026** aggregate update on the budgetary effects of tariffs. CBO published aggregate effects for that update rather than a complete annual replacement baseline. The model therefore preserves the official aggregate information and reconstructs the annual tariff profile explicitly:

- the FY2026 adjustment uses CBO's reported aggregate;
- the 2027-2036 primary effect is allocated across years and calibrated so its modeled debt-service effect tracks CBO's published aggregate debt-service effect within the configured tolerance;
- the 2037-2046 extension holds the modeled FY2036 primary effect constant as a share of GDP.

Those reconstructed portions are identified in the source records as modeled extensions rather than official annual CBO projections.

CBO's **June 30, 2026 Spending Projections by Budget Account** workbook supplies newer account-level evidence. It is used to construct and validate account-level spending controls, but it is not substituted for CBO's complete aggregate budget baseline.

## 3. Accounting identities and debt service

The model uses the following sign conventions:

- positive `revenue_delta_bil` increases federal revenue;
- positive `outlay_delta_bil` increases federal outlays;
- negative outlay changes are spending reductions;
- `primary_deficit_delta_bil = outlay_delta_bil - revenue_delta_bil`.

Policy-induced debt is accumulated annually from the modeled primary-deficit change and induced interest effect.

For each required scenario and target year, the MILP constrains:

`scenario baseline debt + policy-induced debt change <= target debt/GDP ratio × scenario GDP`.

For 2026-2036, debt-service responses use the calibrated CBO debt-service kernel. Beyond 2036, the model extends marginal debt service using the configured long-run marginal interest rate treatment.

The model checks the baseline debt stock-flow identity, debt/GDP reconstruction, revenue/outlay deficit identity, primary-deficit plus interest identity, and consistency between overlapping CBO baseline files before optimization proceeds.

## 4. Debt-service validation

The debt-service kernel is validated against two current CBO Budget and Fiscal Model pulse exports from March 2026.

Each pulse is withheld in turn and reconstructed from the other calibration information. The reciprocal leave-one-out test evaluates 20 annual comparisons and requires the maximum annual error to remain within the configured **$3 billion** tolerance.

This validation is performed before the policy optimization is accepted.

## 5. External historical validation

The model independently cross-checks key historical inputs against other official data sources:

- Treasury Debt to the Penny for FY2025 debt held by the public;
- FRED fiscal-year debt held by the public;
- BEA NIPA data for an independently constructed FY2025 nominal GDP approximation;
- BLS monthly unemployment data averaged over FY2025.

These checks are validation tests, not substitute baseline inputs.

## 6. Policy evidence and score treatment

Candidate policies are assembled from authoritative published federal sources, including CBO budget options, Treasury Greenbook proposals, Social Security Office of the Chief Actuary provisions, Joint Committee on Taxation materials, and other official sources used for specific policy domains.

The model applies the following score rules:

1. Use the newest available published authoritative information.
2. Retain an older official score when it remains the newest published official score for the same proposal.
3. Do not rescale, discount, or discard an official score solely because of its age.
4. Reconstruct a score only when intervening law has changed the proposal enough that the published estimate no longer measures the same incremental policy, and only when the reconstruction can be supported from authoritative evidence.
5. Exclude a coefficient when it cannot be supported to presentation-grade standards.
6. Keep policy eligibility separate from fiscal-score admissibility.
7. Treat current law as the scoring baseline, not as an independent veto on a proposal Congress could otherwise enact.

The validated release uses `OFFICIAL_OLDER` evidence mode. That mode admits both current official scores and older official scores that remain valid under the rules above. The script also supports `OFFICIAL_CURRENT` and `EXPLORATORY` modes, but they are not the evidence setting used for the validated release.

Materiality thresholds used for reporting do not remove otherwise valid candidates from the solver.

## 7. Policy-data pack and candidate universe

The fixed policy-data pack contains:

- **127** current/latest CBO option families;
- **190** scored CBO candidate variants;
- **181** candidates with direct official annual score paths;
- **9** total-only records retained for audit rather than assigned invented annual paths;
- **1,810** official annual fiscal-flow rows.

The model supplements that pack with separately sourced official scored policies and mechanical account-control candidates.

The validated release reports:

- **1,784** mechanical CBO spending-account growth-control variants in the master catalog;
- **901** of those account-control variants solver-ready before protection-mode treatment;
- **1,068** annual-score candidates with continuous policy-level variables;
- **11,809** parameterized implementation and phase-in schedules;
- **1,116** active solver candidates before protection-mode-specific package construction;
- **424** package-ready Strict candidates;
- **1,012** package-ready Expanded candidates.

A total-only official score is not converted into an annual solver path unless the model has an externally defensible annualization method. The model does not invent annual fiscal paths solely to enlarge the policy universe.

## 8. Protected policy set

Policy eligibility is evaluated separately from fiscal scoring.

The protected set covers ordinary wages, saving and investment, family formation, business reinvestment, and protected Social Security and Medicare functions.

The model uses two policy universes.

**Strict** applies the Eisenhower Rule without discretionary exceptions.

**Expanded** retains the same protected categories while allowing reviewed discretion at the margins where a policy does not violate the protected set.

The Strict universe establishes the fiscal-capacity frontier under the more restrictive rule. The Expanded universe establishes the larger admissible policy space from which a feasible package can be selected.

Policies classified as blocked remain outside the solver under both modes. Policies classified as conditional may enter the Expanded universe only after the model's explicit review rules are satisfied.

## 9. Decision variables and parameterization

For candidate policy `i`, the MILP uses:

- a binary activation variable indicating whether the policy is selected;
- binary schedule variables for admissible implementation-year and phase-in combinations;
- a continuous schedule-level intensity variable when the policy can be scaled.

If a policy is active, exactly one admissible timing schedule is selected. The continuous intensity is linked to that schedule's permitted minimum and maximum scale.

Depending on the policy, the model can vary:

- implementation year;
- phase-in duration;
- policy intensity;
- tax rates or rate changes;
- account-level control magnitudes;
- other policy-specific continuous levels supported by the source score.

A projected account outlay path is not treated as evidence that an arbitrary fraction of the account can be cut dollar-for-dollar. Account-level controls enter the solver only through explicit control definitions tied to the official account path.

## 10. Annual fiscal-flow construction

Each solver-admitted candidate has an annual fiscal-flow representation over the relevant model years.

The model carries annual changes in revenue, outlays, and the primary deficit into the debt calculation. Policies with direct official annual score tables use those published annual values subject to documented timing and scaling rules.

Where a valid official score ends before 2046, the model applies explicit extension rules rather than treating a ten-year total as a permanent annual amount. Extension methods and evidence classes are retained in the audit records.

## 11. Policy families, interactions, and overlap

Alternative variants from the same policy family are mutually exclusive.

The model also maintains a material-interaction catalog for policies that affect the same tax base, benefit base, spending mechanism, or other fiscal channel. When no authoritative combined score exists for a material overlap, the relevant pair is constrained so both policies cannot be selected simultaneously.

The final interaction layer includes explicit treatment of overlapping Social Security benefit-tax provisions. Retained solutions are checked against the interaction catalog after optimization.

## 12. Required robustness scenarios

The selected package must satisfy the debt targets under each required scenario:

- **CENTRAL:** working baseline with full modeled policy realization.
- **RATES_PLUS_0_1:** CBO 0.1 percentage-point higher-rate budget sensitivity with a marginal policy debt-service adjustment.
- **PRODUCTIVITY_MINUS_0_1:** CBO 0.1 percentage-point slower-productivity budget sensitivity plus a modeled GDP denominator path.
- **LABOR_FORCE_MINUS_0_1:** CBO 0.1 percentage-point slower labor-force-growth budget sensitivity plus a modeled GDP denominator path.
- **POLICY_YIELD_90:** central macroeconomic path with only 90% of modeled policy fiscal effects realized.

The CBO macro sensitivity profiles are reconstructed from CBO's April 21, 2026 published benchmark effects and validated against the published cumulative and FY2036 values.

The model also evaluates three additional audit-only stress cases:

- `RATES_PLUS_0_5`;
- `PRODUCTIVITY_MINUS_0_3`;
- `COMBINED_ADVERSE`.

These are sensitivity diagnostics. They are not binding constraints in the selected-package MILP. The combined adverse case is an exploratory additive stress case and is not represented as an official CBO combined forecast.

## 13. Social Security solvency

Social Security reform options carry actuarial-improvement coefficients expressed as a percentage of taxable payroll.

The selected-package optimization requires aggregate actuarial improvement of at least **4.42 percentage points of taxable payroll**, matching the model's validated 2026 Trustees basis.

The MILP also limits aggregate actuarial improvement to **4.92 percentage points**, equal to the modeled shortfall plus a configured 0.50 percentage-point excess margin.

Social Security and Medicare are not modeled as eliminable programs.

## 14. MILP constraints

The principal constraint classes are:

- exactly one timing schedule for each active policy;
- intensity bounds linked to the selected schedule;
- mutual exclusivity among alternative variants in the same policy family;
- material-overlap exclusions when combined scoring is unavailable;
- policy-specific required or forbidden conditions used in counterfactual analyses;
- Social Security actuarial lower and upper bounds;
- optional cumulative revenue or spending bounds for diagnostic solves;
- annual revenue, outlay, and primary-deficit accounting identities;
- scenario-specific interest identities;
- annual policy-induced debt accumulation;
- 2036 and 2046 debt/GDP ceilings under every required scenario;
- diversity constraints used to obtain materially different alternative packages.

The 70% long-run reference is not a hard lower-bound constraint in the validated release.

## 15. Objective functions and recommendation selection

The model solves several objective families to establish feasibility, fiscal capacity, alternative packages, and counterfactuals. These include revenue, spending reduction, policy count, implementation complexity, debt, distance from the 75% long-run reference, and target slack.

For the recommendation frontier, the model solves three hard-target Expanded problems that all require Social Security solvency:

- minimize policy count;
- minimize implementation complexity;
- minimize the number of Expanded-only conditional policies, using complexity only as a deterministic tie-breaker.

The final selected package is the robust and Social Security-solvent frontier solution with the lowest implementation-complexity score, with policy count as the secondary ordering criterion.

For ordinary scored reforms, the base complexity weight is:

`1 + log(1 + ten-year primary improvement / 1000)`.

Account-control candidates use separate formulas tied to account fiscal scope and the severity of the control. Certain supplemental Treasury candidates apply documented multipliers to the base scored-reform complexity weight. Every solver-ready candidate carries both its numerical complexity weight and a text field identifying the basis for that weight.

Complexity is therefore an explicit optimization coefficient, not an informal post-solve characterization.

## 16. Solver and numerical acceptance

The model uses the R `highs` interface.

The requested relative MIP gap is **0**.

A solve is treated as proven optimal only when HiGHS returns terminal `Optimal` status and the model's post-solve numerical audit also passes. The configured audit accepts either:

- relative recorded gap at or below `1e-8`; or
- absolute primal-dual objective difference at or below `1e-6` objective units.

These numerical audit tolerances do not convert time-limited, node-limited, interrupted, or otherwise unfinished solves into accepted optimal solutions.

Additional configured tolerances include:

- primal feasibility: `1e-7`;
- dual feasibility: `1e-7`;
- accepted constraint residual: `1e-5`;
- accepted variable-bound residual: `1e-6`;
- accepted integrality residual: `1e-6`;
- independent simulation dollar tolerance: **$2 billion**.

## 17. Independent post-solve simulation

MILP feasibility is not accepted by itself.

Every retained package is re-simulated outside the MILP matrix from its selected policy, timing, and intensity decisions. The independent simulator reconstructs annual fiscal effects and debt paths and verifies that the retained solution remains consistent with the solver result within the configured tolerances.

The selected package must reproduce its required debt paths and Social Security result in this independent simulation.

## 18. Release validation

The validated release applies multiple independent checks before the selected package is accepted:

- baseline accounting identities;
- independent Treasury, FRED, BEA, and BLS historical cross-checks;
- reciprocal CBO debt-service-kernel validation;
- CBO macro-sensitivity reconstruction checks;
- current CBO option-index validation;
- current Social Security actuarial-target validation;
- parameterization checks;
- source provenance and file-hash checks;
- score-basis contract checks;
- protection-regression checks;
- material-overlap checks;
- solver terminal-status and numerical-gap checks;
- independent post-solve simulation;
- exact release-reference membership and metric checks.

The release reference verifies the selected policy membership and principal numerical results against the previously validated run.

## 19. Validated model scale

The Expanded package-ready MILP contains:

- **1,012** candidate-activation binaries;
- **9,937** timing-schedule binaries;
- **9,937** continuous policy-level variables;
- **525** accounting and slack continuous variables;
- **21,411** total variables;
- **21,922** constraints;
- **1,568,494** nonzero coefficients;
- **5** required robustness scenarios.

The Strict package-ready MILP contains **9,547 total variables**, **9,617 constraints**, and **697,778 nonzero coefficients**.

The validated run attempted **121 MILP solves** across the complete analysis. **106** terminated `Optimal`; **15** terminated `Infeasible` in feasibility or counterfactual diagnostics; no solve ended with an unfinished terminal status.

## 20. Interpretation of reported solution families

The **Strict best attainable** result is the fiscal-capacity frontier under the Strict policy rule.

The **Expanded maximum fiscal margin** result is the fiscal-capacity frontier under the larger admissible policy universe.

Neither capacity frontier is the selected package. The selected package is chosen separately by minimizing implementation complexity among solutions that satisfy the required debt, robustness, Social Security, protection, family-exclusivity, and interaction constraints.
