##' @title Import sage results
##'
##' @description
##'
##' This function imports sage TMT quantitation and identification
##' results and combines them into a
##' [QFeatures::QFeatures()] object.
##'
##' @param quantFile `character(1)` containing the quantification
##'     results, typically "quant.tsv".
##'
##' @param idFile `character(1)` containing the identification
##'     results, typically "results.sage.tsv".
##'
##' @param byQuant `character()` containing the specifications of the
##'     quantification columns used for merging. The first element
##'     should correspond the the raw files. Passed the [merge()] as
##'     `by.x`. Default is `c("file", "scannr")`.
##'
##' @param byId `character()` containing the specifications of the
##'     identification columns used for merging. Passed the [merge()]
##'     as `by.y`. Default is `c("filename", "scannr")`.
##'
##' @param splitBy A character refering the a `quantFile` or `idFile`
##'     column that will be used to split the merged table into
##'     different assays, i.e. acquisitions corresponding to different
##'     sets of samples. Default is to use `byQuant[1]`. Set to `NULL`
##'     to not split and return a result with a single assay, for
##'     instance if several fractions from one sample were used.
##'
##' @param quantPattern `character(1)` defining the pattern passed to
##'     [grep()] to extract the columns containing quantitative data.
##'
##' @param class `character(1)` with pne of `"SummarizedExperiment"`
##'     or `"SingleCellExperiment"` defining the assay's
##'     class. Default is the former.
##'
##' @param ... Additional parameters passed to [read.delim()].
##'
##' @return An instance of class [QFeatures()] with as many assays as
##'     defined by `splitBy`.
##'
##' @author Laurent Gatto
##'
##' @export
##'
##' @importFrom utils read.delim
##'
##' @importFrom QFeatures readSummarizedExperiment QFeatures
##'
##' @examples
##'
##' idFile <- BiocFileCache::bfcquery(
##'                              BiocFileCache::BiocFileCache(),
##'                              "sageRes")$fpath
##' quantFile <- BiocFileCache::bfcquery(
##'                                 BiocFileCache::BiocFileCache(),
##'                                 "sageQuant")$fpath
##'
##' sageQFeatures(quantFile, idFile)
sageQFeatures <- function(quantFile, idFile,
                          byQuant = c("file", "scannr"),
                          byId = c("filename", "scannr"),
                          splitBy = byQuant[1],
                          quantPattern = "tmt_",
                          class = c("SummarizedExperiment", "SingleCellExperiment"),
                          ...) {
    stopifnot(suppressPackageStartupMessages(require("QFeatures")))
    class <- match.arg(class)
    quant <- read.delim(quantFile, ...)
    id <- read.delim(idFile, ...)
    x <- merge(quant, id, by.x = byQuant, by.y = byId)
    ecol <- grep(quantPattern, names(x))
    if (!is.null(splitBy)) {
        x <- split(x, x[, splitBy])
    } else {
        x <- list(sage = x)
    }
    if (class == "SingleCellExperiment") {
        requireNamespace("scp")
        return(QFeatures(lapply(x, scp::readSingleCellExperiment, ecol = ecol)))
    }
    QFeatures(lapply(x, QFeatures::readSummarizedExperiment, ecol = ecol))
}
