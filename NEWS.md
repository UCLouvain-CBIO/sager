# sager 0.3

## sager 0.3.2

- fdr param in `sagePSM()` is now `spectrum_q`, in line with sage
  v0.13.0.

## sager 0.3.1

- Update default filename parameter values in `sageQFeatures()`.

## sager 0.3.0

- Rename functions to create and handle keys across data types:
  `addKey()`, `filterKey()` and `getKey()`.

## sager 0.2.1

- Add `sager` vignette.
- Update data: `sagerMzMLData()` now returns the path to 3 separate
  subsetted mzML files.
- New functions to create and handle keys across data types:
  `addKEY()`, `subsetByKEY()` and `getKEY()`.
- Update vignette, with `MsExperiment` workflow.

## sager 0.2.0

- Update test/subset data and add sage configuration file.
- Add a `fdr` variable to `sagePSM()` (requires PSMatch >= 1.3.3).

# sager 0.1

## sager 0.1.0

- First release with `sagePSM()` and `sageQFeatures()` and test data
  management.
