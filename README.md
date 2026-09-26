Federal Fiscal Optimizer
The Federal Fiscal Optimizer is an R-based mixed-integer linear programming model for constructing federal fiscal packages subject to long-run debt, Social Security solvency, policy-eligibility, interaction, and robustness constraints.
The model combines official federal budget and economic baselines with published policy scores, parameterizes implementation timing and policy intensity where the underlying evidence supports doing so, incorporates endogenous debt-service effects, prevents unscored material overlap, and solves the resulting optimization problem with HiGHS. Retained packages are independently re-simulated outside the MILP before they are accepted.
Published models
[Protected Policy Set Excluded from Cuts](models/protected-policy-set-excluded-from-cuts/README.md)
This model searches the admissible federal fiscal policy space while excluding the defined protected policy set from cuts and other prohibited burdens. The protected categories include ordinary wages, saving and investment, family formation, business reinvestment, and protected Social Security and Medicare functions.
The model evaluates fiscal policy over 2026-2046. The validated release contains 1,116 active solver candidates before protection-mode-specific package construction. Its package-ready MILPs contain 424 Strict candidates and 1,012 Expanded candidates.
A feasible selected package must satisfy:
- debt held by the public at or below 90% of GDP in 2036;
- debt held by the public at or below 80% of GDP in 2046;
- an approximate 75-year Social Security actuarial improvement of at least 4.42% of taxable payroll;
- the required interest-rate, productivity, labor-force, and policy-realization stress scenarios;
- the model's policy-protection, score-basis, family-exclusivity, and material-overlap constraints.
The validated selected package contains 21 policies. Under the central scenario, debt held by the public reaches 86.34% of GDP in 2036 and 66.55% in 2046. Under the worst required stress scenario, it reaches 89.99% in 2036 and 74.66% in 2046. The package produces an estimated Social Security actuarial improvement of 4.50% of taxable payroll.
Model design
The optimizer includes:
- binary policy-activation decisions;
- binary implementation-timing and phase-in decisions;
- continuous policy rates, levels, and scalable implementation parameters where supported by the underlying score;
- annual revenue, outlay, primary-deficit, interest, and debt accounting through 2046;
- endogenous debt-service effects;
- five required robustness scenarios;
- explicit Social Security actuarial constraints;
- mutually exclusive policy-family alternatives;
- explicit material-overlap exclusions;
- Strict and Expanded protection modes;
- HiGHS MILP optimization with a requested relative MIP gap of zero;
- independent post-solve simulation and numerical-optimality checks.
The selected package is chosen from robust, Social Security-solvent candidates by minimizing the model's explicit implementation-complexity measure, with policy count used as a secondary ordering criterion.
Baseline and evidence
The aggregate fiscal baseline is based on CBO's February 2026 budget and economic projections. The working baseline incorporates CBO's August 20, 2026 aggregate tariff update. CBO's June 30, 2026 spending-by-budget-account workbook is used for account-level policy controls but is not substituted for the complete aggregate baseline.
Policy coefficients are based on published authoritative federal estimates. Sources include the Congressional Budget Office, the Department of the Treasury, the Social Security Office of the Chief Actuary, the Joint Committee on Taxation, and other official sources identified in the model's provenance records.
The model uses the newest available authoritative score for a proposal when one exists. An older official score remains admissible when it is still the newest published official score for the same proposal. A score is reconstructed only when intervening law has changed the proposal enough that the published estimate no longer measures the same incremental policy and the reconstruction can be supported from authoritative evidence.
Current law supplies the scoring baseline. It is not treated as an independent reason to exclude an otherwise admissible policy that Congress could enact.
The combined policy packages, modeled debt paths, stress scenarios, optimization results, and conclusions are independent model outputs and should not be attributed to the agencies supplying the underlying data or policy scores.
Documentation
- [Model overview and execution](models/protected-policy-set-excluded-from-cuts/README.md)
- [Methodology](models/protected-policy-set-excluded-from-cuts/METHODOLOGY.md)
- [Reproducibility and technical requirements](models/protected-policy-set-excluded-from-cuts/REPRODUCIBILITY.md)
- [R source](models/protected-policy-set-excluded-from-cuts/federal_fiscal_optimizer.R)
License
Original code and documentation are released under the MIT License. Government source materials included for reproducibility remain identified by their originating agencies and provenance records.
