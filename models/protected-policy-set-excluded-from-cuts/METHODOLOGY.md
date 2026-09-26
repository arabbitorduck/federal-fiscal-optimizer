# Methodology

## Purpose

The model evaluates combinations of federal fiscal policies over 2026 through 2046 using mixed-integer linear programming and independent post-solve simulation.

The binding debt requirements are debt held by the public at or below 90% of GDP in 2036 and at or below 80% of GDP in 2046. The model also requires an approximate 75-year Social Security actuarial improvement of at least 4.42% of taxable payroll.

## Baseline and debt service

The core fiscal baseline is built from official Congressional Budget Office budget and economic data. The model uses the February 2026 CBO data vintage for the principal budget and economic series and the audited June 2026 CBO spending-detail workbook for account-level controls.

The working baseline combines those official series with documented model adjustments. Debt service is calculated through a validated kernel calibrated against CBO debt-service materials and audited against benchmark cases.

## Policy evidence and score treatment

Candidate policies are assembled from authoritative published sources, including CBO budget options, Treasury Greenbook proposals, Social Security Office of the Chief Actuary provisions, Joint Committee on Taxation materials, and other official federal sources used for specific policy domains.

The score rule is:

1. Use the newest available published authoritative information.
2. An older official score remains admissible when it is still the newest published official score for the same proposal.
3. Age alone does not justify rescaling or discarding an official score.
4. Reconstruct a score only when intervening law has changed the proposal enough that the published score no longer measures the same incremental policy, and only when the reconstruction is externally supportable.
5. Exclude a coefficient when it cannot be defended to presentation-grade standards.

Current law is the scoring baseline. It is not an independent veto on a policy Congress could enact.

## Protected policy set

Policy eligibility is kept separate from fiscal scoring.

The protected set covers ordinary wages, saving and investment, family formation, business reinvestment, and protected Social Security and Medicare functions.

The model uses two internal protection modes:

**Strict** applies the Eisenhower Rule conservatively.

**Expanded** preserves the same protected categories while admitting specifically reviewed policies that involve discretion at the margins without violating the protected set.

The strict mode answers how much fiscal capacity exists under the most conservative application of the protection rule. The expanded mode tests the larger admissible policy space when reviewed marginal discretion is allowed.

## Parameterization

Scored policy anchors are converted into implementable decision variables where the evidence supports parameterization.

Depending on the policy, the model can vary implementation year, phase-in duration, policy intensity, tax rates, account-level control magnitudes, and other policy-specific continuous levels.

Implementation timing and activation choices use integer variables. Rates, levels, and scalable policy decisions use continuous variables where appropriate.

Account outlay paths are not treated as proof that arbitrary fractions can be cut dollar-for-dollar. Account-level controls must pass the explicit control-design and validation rules.

## Interaction and overlap treatment

Policies that affect the same tax base, benefit base, spending mechanism, or other material fiscal channel can overlap. The model maintains an interaction catalog and applies explicit constraints or consolidation rules where simultaneous use would double-count fiscal effects.

The final overlap-correction layer includes a specific Social Security benefit-tax overlap audit and an explicit solution-by-interaction-pair validation.

## Robust required scenarios

A published recommendation must satisfy every required scenario:

- `CENTRAL`
- `RATES_PLUS_0_1`
- `PRODUCTIVITY_MINUS_0_1`
- `LABOR_FORCE_MINUS_0_1`
- `POLICY_YIELD_90`

`POLICY_YIELD_90` assumes only 90% of modeled policy fiscal effects are realized.

The model also retains harsher audit-only stress cases. Those cases are diagnostics and are not binding optimization constraints.

## Social Security solvency

Social Security reform options carry actuarial-improvement coefficients expressed as a percentage of taxable payroll. When the solvency constraint is active, the selected package must provide at least 4.42 percentage points of actuarial improvement.

Social Security and Medicare are not modeled as eliminable programs.

## Optimization

The model is solved with HiGHS.

The formulation includes policy-activation binaries, implementation-timing binaries, continuous policy-level variables, annual fiscal-flow constraints, robust-scenario debt constraints, Social Security actuarial constraints, overlap constraints, and protection eligibility rules.

The requested MIP relative gap is zero. A solution is accepted as optimal only when HiGHS reports terminal optimality and the model's independent numerical-gap checks pass.

The model explores multiple objective families for capacity, feasibility, diversity, and recommendation analysis. The published recommendation is the minimum-complexity package satisfying the frozen robust fiscal and Social Security requirements under the defined policy-space rules.

## Independent simulation and acceptance

MILP feasibility is not accepted by itself. Retained packages are independently re-simulated outside the optimization matrix.

Final acceptance requires:

- validation gates to pass;
- protected categories to remain excluded where required;
- source provenance and fixed input hashes to validate;
- score-basis rules to pass;
- zero material-overlap violations;
- complete terminal solver status;
- numerical-gap audits to pass;
- every retained solution to independently re-simulate;
- the Social Security requirement to pass;
- all required robust debt targets to pass;
- frozen release membership and metrics to reproduce;
- zero R warnings.

## Capacity frontiers and recommendation

The strict best-attainable result and expanded maximum-fiscal-margin result are diagnostic capacity frontiers.

They are not recommendations to enact every available policy.

The recommended package is selected separately under the model's robust fiscal, Social Security, protection, and complexity requirements.
