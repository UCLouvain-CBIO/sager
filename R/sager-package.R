##' @title Analysing sage results with R
##'
##' @description
##'
##' The `sager` package can be used to import results produced by the
##' sage search engine into R. Sage produces tab-separated output
##' files for identification and quantitation results. In `sager`, we
##' import these result files into established Bioconductor classes:
##'
##' - Identification results are parsed and imported as
##'   [PSMatch::PSM()] objects with [sagePSM()].
##'
##' - Quantitation (and identification) results are parsed, merged and
##'   imported as [QFeatures::QFeatures()] objects with
##'   [sageQFeatures()].
##'
##' These functions are described and demonstrated in their respective
##' manual pages using data generated from Yu *et al.* 'Benchmarking
##' the Orbitrap Tribrid Eclipse for Next Generation Multiplexed
##' Proteomics', downloaded from ProteomeXchange project
##' PXD016766. See [sagerData()] for details.
##'
##' @references
##'
##' - **Sage**: proteomics searching so fast it seems like
##'   magic. [https://github.com/lazear/sage](https://github.com/lazear/sage).
##'
##' - Yu *et al.* 'Benchmarking the Orbitrap Tribrid Eclipse for Next
##'   Generation Multiplexed Proteomics' Anal. Chem. 2020, 92, 9,
##'   6478–6485 Publication Date:April 6, 2020
##'   [DOI:10.1021/acs.analchem.9b05685](https://doi.org/10.1021/acs.analchem.9b05685).
##'
##' @docType package
##'
##' @name sager
NULL
