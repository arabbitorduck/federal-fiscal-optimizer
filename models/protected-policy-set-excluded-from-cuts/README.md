# Protected Policy Set Excluded from Cuts

This model searches for federal fiscal-policy combinations capable of meeting the required debt and Social Security solvency constraints while excluding the defined protected policy set from cuts and other prohibited burdens.

The protected categories include ordinary wages, saving and investment, family formation, business reinvestment, and protected Social Security and Medicare functions. Social Security and Medicare are not eliminable programs.

The model uses mixed-integer linear programming with HiGHS, robust required scenarios, independent post-solve simulation, overlap controls, numerical-gap audits, source hashing, and a frozen release-parity check.

## Required fiscal targets

The published model requires:

- debt held by the public at or below 90% of GDP in 2036;
- debt held by the public at or below 80% of GDP in 2046;
- Social Security actuarial improvement of at least 4.42% of taxable payroll.

The 75% debt-to-GDP value is the long-run reference center used by the model. The 70% line is a lower presentation reference, not the binding 2046 ceiling.

## Policy-space treatment

The model evaluates two protection modes internally.

**Strict** applies the Eisenhower Rule conservatively.

**Expanded** applies the same protected-category rule while permitting reviewed discretion at the margins where a policy does not violate the protected set.

The optimizer uses these modes to distinguish the strict capacity frontier from the larger admissible policy space used for the published recommendation.

## Run

Open `federal_fiscal_optimizer.R` in RStudio and source it, or run it with `Rscript`.

The script resolves the model directory from its own file location, checks required packages, offers to install missing packages in an interactive session, validates fixed repository inputs, and writes runtime outputs locally.

See:

- [`METHODOLOGY.md`](METHODOLOGY.md)
- [`REPRODUCIBILITY.md`](REPRODUCIBILITY.md)

Runtime results are intentionally not committed to this repository.
