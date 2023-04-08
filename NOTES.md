## Software

- Data should probably get filtered before creating an MsExperiment. A
  sageMsExperiment() constructor is thus arguably not needed. It would
  be better to document how to create an MsExperiment with the
  available test data.
- Write unit tests.

## Data

- Also add the full dataset?
- Re-generate mzML files including noise (see -n argument) and re-run
  sage to have SN ration.
- The current subset only contains good spectra. Would be better to
  also have some decoy and of low score.
- When creating the subset, important to keep the original filename in
  a spectra variable, to match them back to the file/scannr to the
  quant/id results.

## Functionality

- How to link the different slots in an MsExperiment? Could have a KEY
  variable as `spectraData(.)$KEY`, `rowData(.)$KEY` and `.$KEY` to be
  used as a unique consolidated foreign key. To be discussed with Jo.
- We could then have a `ProtGenerics::makeKEY()` generic and different
  methods if `Spectra`, `QFeatures` and `PSM` that add the KEY
  variables.
- Assuming we have it, how to use it? Could be a `subByKEY()` function
  that filter the respective data types. When called on an
  `MsExperiment`, it delegates to its data types.
