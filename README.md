# Federal Fiscal Optimizer

The Federal Fiscal Optimizer is an R-based mixed-integer linear programming model for constructing federal fiscal packages subject to long-run debt, Social Security solvency, policy-eligibility, and robustness constraints.

The model combines official federal fiscal baselines with published policy scores, parameterizes implementation timing and policy intensity where the evidence supports doing so, accounts for material interactions and overlapping fiscal effects, and searches the resulting policy space with the HiGHS MILP solver. Candidate packages are then independently re-simulated outside the optimization matrix before they are accepted.

The current model evaluates federal fiscal policy over 2026-2046.

## Current published model

### [Protected Policy Set Excluded from Cuts](models/protected-policy-set-excluded-from-cuts/README.md)

This model searches for a fiscal package while excluding the defined protected policy set from cuts and other prohibited burdens. Protected categories include ordinary wages, saving and investment, family formation, business reinvestment, and protected Social Security and Medicare functions.

The active solver universe contains **1,116 policy candidates and parameterized variants** drawn from authoritative federal sources, including the Congressional Budget Office, Treasury, the Social Security Office of the Chief Actuary, and the Joint Committee on Taxation.

The optimization requires the selected package to satisfy:

- debt held by the public at or below **90% of GDP in 2036**;
- debt held by the public at or below **80% of GDP in 2046**;
- an approximate 75-year Social Security actuarial improvement of at least **4.42% of taxable payroll**;
- the required robustness scenarios for interest rates, productivity, labor-force growth, and partial policy realization;
- policy-protection, score-basis, and interaction constraints.

The published solution contains **21 policies**. Under the model's central scenario, debt held by the public reaches **86.34% of GDP in 2036** and **66.55% in 2046**. Under the worst required robustness scenario, it reaches **89.99% in 2036** and **74.66% in 2046**. The package produces an estimated Social Security actuarial improvement of **4.50% of taxable payroll**.

## Model design

The optimizer includes:

- binary policy-activation and implementation-timing decisions;
- continuous policy rates, levels, and scalable implementation parameters where supported by the underlying score;
- annual fiscal-flow accounting through 2046;
- endogenous debt-service effects;
- required macroeconomic and policy-realization stress scenarios;
- Social Security actuarial-solvency constraints;
- explicit policy-interaction and overlap controls;
- protected-policy eligibility rules;
- zero-gap HiGHS optimization where optimality is reported;
- independent post-solve debt re-simulation;
- numerical-gap, provenance, source-hash, overlap, protection, and warning audits;
- a frozen release-parity test against the validated published model.

The optimizer distinguishes between a conservative **Strict** policy universe and a larger **Expanded** universe that allows reviewed discretion at the margins while retaining the same protected categories. The strict universe provides a fiscal-capacity bound under the more restrictive interpretation; the published recommendation is selected from the admissible expanded universe subject to the full robust debt and Social Security constraints.

## Source and scoring approach

Policy coefficients are based on published authoritative federal estimates. The model uses the newest available authoritative score for a proposal when one exists. An older official score is not discarded simply because of age if it remains the newest valid score for the same proposal.

Reconstruction is used only when intervening law has changed a proposal enough that the published estimate no longer measures the same incremental policy and a defensible update can be supported from authoritative evidence.

Current law supplies the scoring baseline. It is not treated as an independent reason to exclude a policy that Congress could otherwise enact.

## Documentation

The complete executable model, fixed reproduction inputs, and model-specific documentation are available here:

- [Model overview and execution](models/protected-policy-set-excluded-from-cuts/README.md)
- [Methodology](models/protected-policy-set-excluded-from-cuts/METHODOLOGY.md)
- [Reproducibility](models/protected-policy-set-excluded-from-cuts/REPRODUCIBILITY.md)
- [R source](models/protected-policy-set-excluded-from-cuts/federal_fiscal_optimizer.R)

## License

Original code and documentation are released under the MIT License. Government source materials included for reproducibility remain identified by their originating agencies and provenance records.
