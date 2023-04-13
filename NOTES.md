## Software

- Data should probably get filtered before creating an MsExperiment. A
  sageMsExperiment() constructor is thus arguably not needed. It would
  be better to document how to create an MsExperiment with the
  available test data.
- Write unit tests.

## Data

- Also add the full dataset?
- [X] Re-generate mzML files including noise (see -n argument) and
      re-run sage to have SN ration.
- [X] Add the filename corresponding the basename(dataOrigin)), needed
      for KEY.
- [X] The current subset only contains good spectra. Would be better
      to also have some decoy and of low score.
- [ ] Add cell types: Pellets from 11 human cell lines (RKO, A549,
      U87_MG, HCT116, HEK293T, HeLa, MCF7, U2OS, SUM159, PANC1, and
      Jurkat)
- [ ] MS data has only MS2 scans.

## Analysis

- Remove contaminants.

## Functionality

### MsExperiment

- How to link the different slots in an MsExperiment? Could have a KEY
  variable as `spectraData(.)$KEY`, `rowData(.)$KEY` and `.$KEY` to be
  used as a unique consolidated foreign key. To be discussed with Jo.
- We could then have a `ProtGenerics::makeKEY()` generic and different
  methods if `Spectra`, `QFeatures` and `PSM` that add the KEY
  variables.
- Assuming we have it, how to use it? Could be a `subByKEY()` function
  that filter the respective data types. When called on an
  `MsExperiment`, it delegates to its data types.

### sage configs

- R interface to manage config files; provide a set of standard configs.
