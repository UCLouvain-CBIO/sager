##' @title Import sage results
##'
##' @description
##'
##' This function imports sage TMT quantitation and identification
##' results and combines them into a
##' [SummarizedExperiment::SummarizedExperiment()] object.
##'
##' @param quantFile `character(1)` containing the quantification
##'     results, typically "quant.tsv".
##'
##' @param idFile `character(1)` containing the identification
##'     results, typically "results.sage.tsv".
##'
##' @param byQuant `character()` containing the specifications of the
##'     quantification columns used for merging. Passed the [merge()]
##'     as `by.x`. Default is `c("file", "scannr")`.
##'
##' @param byId `character()` containing the specifications of the
##'     identification columns used for merging. Passed the [merge()]
##'     as `by.y`. Default is `c("filename", "scannr")`.
##'
##' @param quantPattern `character(1)` defining the pattern passed to
##'     [grep()] to extract the columns containing quantitative data.
##'
##' @param ... Additional parameters passed to [read.delim()].
##'
##' @return An instance of class [SummarizedExperiment()].
##'
##' @author Laurent Gatto
##'
##' @export
##'
##' @importFrom utils read.delim
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
##' sageSummarizedExperiment(quantFile, idFile)
sageSummarizedExperiment <- function(quantFile, idFile,
                                     byQuant = c("file", "scannr"),
                                     byId = c("filename", "scannr"),
                                     quantPattern = "tmt_",
                                     ...) {
    quant <- read.delim(quantFile)
    id <- read.delim(idFile, ...)
    x <- merge(quant, id, by.x = byQuant, by.y = byId)
    QFeatures::readSummarizedExperiment(
                   x,
                   ecol = grep(quantPattern, names(x)))
}
