# Protected Policy Set Excluded from Cuts

This model searches for federal fiscal packages capable of meeting the required debt and Social Security solvency constraints while excluding the defined protected policy set from cuts and other prohibited burdens.

It is implemented in R as a mixed-integer linear programming model and solved with HiGHS. The model evaluates annual fiscal effects through 2046, permits continuous policy rates or levels where the source evidence supports parameterization, incorporates debt-service effects and material policy interactions, imposes required stress scenarios, and independently re-simulates retained solutions outside the optimization matrix.

## Policy-space rule

The protected categories include ordinary wages, saving and investment, family formation, business reinvestment, and protected Social Security and Medicare functions. Social Security and Medicare are not modeled as eliminable programs.

The model's protection screen is organized around what this project calls the **Eisenhower Rule**. The term is drawn from President Dwight D. Eisenhower's *Remarks at the Lincoln Day Box Supper* in Washington, D.C., on February 5, 1954. Eisenhower stated:

> "In all those things which deal with people, be liberal, be human. In all those things which deal with people's money, or their economy, or their form of government, be conservative."

Source: Dwight D. Eisenhower, *Remarks at the Lincoln Day Box Supper*, February 5, 1954, Public Papers of the Presidents; available through the American Presidency Project:  
`https://www.presidency.ucsb.edu/documents/remarks-the-lincoln-day-box-supper`

**Eisenhower Rule** is a model-specific name for the protection principle derived from that statement; it is not a statutory or regulatory term. Operationally, the classifier protects person-facing benefits, earned compensation, household security, productive capacity, and core state capacity from generic account-level cuts. Program identity is primary. Agency or bureau identity is used only where institutional function is necessary to classify an otherwise ambiguous account.

The model evaluates two policy universes:

- **Strict:** applies that protection rule without discretionary exceptions. A policy or account control that crosses a protected category is excluded.
- **Expanded:** retains the same protected categories while permitting reviewed discretion at the margins where a proposal can be bounded without violating the protected interest itself.

The Strict universe measures fiscal capacity under the more restrictive application. The Expanded universe tests the larger admissible policy space while retaining the same underlying protection principle.

In the validated release, the package-ready Strict MILP contains **424 candidate activation variables**. The package-ready Expanded MILP contains **1,012 candidate activation variables**.

## Required constraints

A feasible selected package must satisfy debt held by the public at or below **90% of GDP in 2036** and **80% of GDP in 2046** under each of the five required robustness scenarios. It must also provide an approximate 75-year Social Security actuarial improvement of at least **4.42% of taxable payroll** and satisfy the model's protection, score-basis, family-exclusivity, and material-overlap constraints.

The model limits aggregate Social Security actuarial improvement to **4.92% of taxable payroll**, which is the 4.42% modeled shortfall plus a 0.50 percentage-point configured excess margin.

The model's 75% debt-to-GDP value is the center of its long-run reference range. The 70% value is a lower presentation reference. Neither is the binding 2046 ceiling; the hard upper limit is 80%.

## Validated reference result

The validated release contains **1,116 active solver candidates** before protection-mode-specific package construction and selects a **21-policy** package from the admissible Expanded universe.

The selected package produces:

- central debt/GDP of **86.34% in 2036** and **66.55% in 2046**;
- worst required-scenario debt/GDP of **89.99% in 2036** and **74.66% in 2046**;
- Social Security actuarial improvement of **4.50% of taxable payroll**.

The selected package is the lowest-complexity robust and Social Security-solvent package on the model's recommendation frontier, with policy count used as a secondary ordering criterion.

These are independent model results, not official projections by the agencies supplying the underlying data or policy scores.

## Validated model scale

The Expanded package-ready MILP used for the principal recommendation analysis contains:

- **21,411 total variables**;
- **10,949 integer variables**;
- **10,462 continuous variables**;
- **21,922 constraints**;
- **1,568,494 nonzero matrix coefficients**;
- **5 required robustness scenarios**.

The validated run evaluated **11,809 parameterized implementation and phase-in schedules** and executed **121 MILP solves** across feasibility, capacity, recommendation, diversity, and counterfactual analyses.

## Running the model

The main executable is `federal_fiscal_optimizer.R`.

In RStudio:

```r
source("federal_fiscal_optimizer.R", echo = TRUE)
```

From a shell with R available:

```text
Rscript federal_fiscal_optimizer.R
```

The script resolves its own directory, checks and installs required R packages if necessary, validates fixed repository inputs, obtains required public validation data, runs the optimization and independent validation stages, and writes the resulting audit files locally.

## Technical requirements

The validated release uses R 4.6.1 and HiGHS 1.14.0.2. The script declares and checks 22 direct R package dependencies before execution.

Independent historical validation requires `BEA_API_KEY`, `BLS_API_KEY`, and `FRED_API_KEY`. The repository supplies the fixed CBO workbook, JCT PDF, fiscal-policy data pack, input manifest, and release-reference file. Other official baseline and validation data are acquired or queried during execution.

See [METHODOLOGY.md](METHODOLOGY.md) for the analytical design and [REPRODUCIBILITY.md](REPRODUCIBILITY.md) for the complete package list, API-key registration and configuration, supplied-versus-runtime input inventory, numerical tolerances, and reproduction standard.
