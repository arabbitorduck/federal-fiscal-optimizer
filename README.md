# Federal Fiscal Optimizer

This repository contains reproducible federal fiscal optimization models written in R.

Each published model is stored under `models/` in a directory named for the substantive policy-space condition that distinguishes that model. Each model directory contains its own executable script, fixed required inputs, methodology, and reproduction instructions. Repository-wide material stays at the root only when it applies across models.

## Published models

- [`protected-policy-set-excluded-from-cuts`](models/protected-policy-set-excluded-from-cuts/README.md): searches the admissible federal fiscal policy space while excluding the defined protected policy set from cuts and other prohibited burdens.

## Repository conventions

- Model directories use descriptive names rather than sequence numbers.
- Each model is self-contained for reproduction.
- Runtime outputs are not committed.
- Fixed repository inputs required for reproduction are committed with the model that uses them.
- A new substantive model is added as a separate descriptively named directory rather than overwriting a previously published model.
- Git tags identify publication states of the repository.

## License

Original code and documentation in this repository are released under the MIT License. Included third-party source materials remain identified by their originating agencies and provenance records.
