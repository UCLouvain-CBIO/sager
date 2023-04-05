##' @title Sager Data
##'
##' @description
##'
##' A set of function to install and access test data. Three datasets
##' are available: a sage TMT11 quantitation tsv file, a sage
##' identification tsv file, and an mzML that contains the raw data
##' used as input.
##'
##' The data are downloaded from ProteomeXchange project PXD016766
##' (see reference below) and processed with sage version 0.10.0. See
##' `inst/scripts/make-data.R` for details on how these data were
##' generated and subset.
##'
##' @param cache Object of class `BiocFileCache`. Default for
##'     `sagerMzMLData()`, `sagerQuantData()` and `sagerIdData()` is
##'     [rpx::rpxCache()].
##'
##' @param pattern `character(1)` defining the pattern used to queray
##'     `cache`. These are respectively `"sager_suset_PXD016766"`,
##'     `"sager_subset_quant"` and `"sager_subset_id"` for
##'     `sagerMzMLData()`, `sagerQuantData()` and `sagerIdData()`.
##'
##' @importFrom rpx rpxCache
##'
##' @importFrom BiocFileCache bfcquery
##'
##' @importFrom utils packageDescription
##'
##' @export
##'
##' @rdname sagerData
##'
##' @references
##'
##' Yu *et al.* 'Benchmarking the Orbitrap Tribrid Eclipse for Next
##' Generation Multiplexed Proteomics' Anal. Chem. 2020, 92, 9,
##' 6478–6485 Publication Date:April 6, 2020
##' [DOI:10.1021/acs.analchem.9b05685](https://doi.org/10.1021/acs.analchem.9b05685).
##'
##' @examples
##'
##' sagerQuantData()
##'
##' sagerIdData()
##'
##' sagerMzMLData()
sagerData <- function(cache, pattern) {
    if (missing(pattern))
        stop("Please provide a pattern to query the cache.")
    if (missing(cache))
        stop("Please provide BiocFile cache.")
    x <- bfcquery(cache, pattern)
    if (!nrow(x))
        stop("Data not found. See ?sagerData for details.")
    if (nrow(x) > 1)
        stop("Found > 1 resource found. Please open an issue in",
             packageDescription("Spectra")$BugReports,
             "with the exact code you ran and your session information.")
    x$rpath
}


##' @export
##'
##' @rdname sagerData
sagerQuantData <- function()
    sagerData(cache = rpx::rpxCache(),
              pattern = "sager_subset_quant")

##' @export
##'
##' @rdname sagerData
sagerIdData <- function()
    sagerData(cache = rpx::rpxCache(),
              pattern = "sager_subset_id")

##' @export
##'
##' @rdname sagerData
sagerMzMLData <- function()
    sagerData(cache = rpx::rpxCache(),
              pattern = "sager_subset_PXD016766")

##' @export
##'
##' @rdname sagerData
sagerGetData <- function(cache = rpx::rpxCache) {
    ## add resources to cache
}
