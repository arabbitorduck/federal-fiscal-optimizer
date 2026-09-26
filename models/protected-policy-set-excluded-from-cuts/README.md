# Protected Policy Set Excluded from Cuts

This model searches for federal fiscal packages capable of meeting the required debt and Social Security solvency constraints while excluding the defined protected policy set from cuts and other prohibited burdens.

It is implemented in R as a mixed-integer linear programming model and solved with HiGHS. The model evaluates annual fiscal effects through 2046, permits continuous policy rates or levels where the source evidence supports parameterization, incorporates debt-service effects and material policy interactions, imposes required stress scenarios, and independently re-simulates retained solutions outside the optimization matrix.

## Policy-space rule

The protected categories include ordinary wages, saving and investment, family formation, business reinvestment, and protected Social Security and Medicare functions. Social Security and Medicare are not modeled as eliminable programs.

The model evaluates two policy universes:

- **Strict:** applies the Eisenhower Rule without discretionary exceptions.
- **Expanded:** retains the same protected categories while permitting reviewed discretion at the margins where a policy does not violate the protected set.

The Strict universe measures the fiscal capacity available under the more restrictive rule. The Expanded universe tests the larger admissible policy space.

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

See [METHODOLOGY.md](METHODOLOGY.md) for the analytical design and [REPRODUCIBILITY.md](REPRODUCIBILITY.md) for software, package, input, credential, and execution requirements.
