##' @title Sager Data
##'
##' @description
##'
##' A set of function to install and access test data. Three datasets
##' are available: a sage TMT11 quantitation tsv file, a sage
##' identification tsv file, and an mzML file that contains the raw
##' data used as input.
##'
##' The data were downloaded from the ProteomeXchange project
##' PXD016766 (see reference below) and processed with sage version
##' 0.10.0. See `inst/scripts/make-data.R` for details on how these
##' data were generated and subset.
##'
##' @section Functions:
##'
##' - `sagerMzMLData()` returns the path to the cached mzML file.
##'
##' - `sagerQuantData()` returns the path to the cached quantitation
##'    tab-separated file.
##'
##' - `sagerIdData()` returns the path to the cached identifcation
##'    tab-separated file.
##'
##' - `sagerAddData(which, cache)` checks if the resources are already available
##'    in the cache (default is [BiocFileCache::BiocFileCache()]). If not, data are
##'    downloaded and added to the cache.
##'
##' - `sagerRemoveData(which, cache)` removes the cached resource.
##'
##' @details
##'
##' The `sagerMzMLData()`, `sagerQuantData()` and `sagerIdData()` use
##' the `sager::sagerData()` function under the hood, that defined the
##' resource name and the cache.
##'
##' @param cache Object of class `BiocFileCache`. Default for is
##'     [BiocFileCache::BiocFileCache()].
##'
##' @param rname `character(1)` defining the resource name used to
##'     query `cache`.
##'
##' @importFrom BiocFileCache bfcquery BiocFileCache
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
sagerData <- function(cache, rname) {
    if (missing(rname))
        stop("Please provide a resource name to query the cache.")
    if (missing(cache))
        stop("Please provide BiocFile cache.")
    x <- bfcquery(cache, rname, field = "rname")
    if (!nrow(x))
        stop("Data not found. See ?sagerData for details.")
    if (nrow(x) > 1)
        stop("Found > 1 resource found. Please open an issue in",
             packageDescription("Spectra")$BugReports,
             "with the exact code you ran and your session information.")
    x$rpath
}


## sager cache resource ids
sager_rids <- function(which = c("quant", "id", "mzml")) {
    rids <- c(quant = "sager_subset_quant",
              id = "sager_subset_id",
              mzml = "sager_subset_PXD016766")
    which <- match.arg(which, several.ok = TRUE)
    rids[which]
}

##' @export
##'
##' @rdname sagerData
sagerQuantData <- function()
    sagerData(cache = BiocFileCache::BiocFileCache(),
              rname = sager_rids("quant"))

##' @export
##'
##' @rdname sagerData
sagerIdData <- function()
    sagerData(cache = BiocFileCache::BiocFileCache(),
              rname = sager_rids("id"))

##' @export
##'
##' @rdname sagerData
sagerMzMLData <- function()
    sagerData(cache = BiocFileCache::BiocFileCache(),
              rname = sager_rids("mzml"))

##' @export
##'
##' @rdname sagerData
##'
##' @param which One or several of `"quant"`, `"id"` or `"mzml"`
##'     specifying what data to download.
sagerAddData <- function(which = c("quant", "id", "mzml"),
                         cache = BiocFileCache::BiocFileCache()) {
    which <- match.arg(which, several.ok = TRUE)
    ## add resources to cache
}

##' @export
##'
##' @rdname sagerData
##'
##' @importFrom BiocFileCache bfcremove bfcquery
sagerRemoveData <- function(which = c("quant", "id", "mzml"),
                            cache = BiocFileCache::BiocFileCache()) {
    which <- match.arg(which, several.ok = TRUE)
    rids <- sager_rids(which)
    x <- bfcquery(cache, rids, field = "rname")
    if (!nrow(x)) {
        message("Resource(s) not found, none removed.")
        return(invisible(cache))
    }
    bfcremove(cache, x$rid)
}
