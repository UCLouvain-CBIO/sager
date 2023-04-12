##' @title Import sage quantitative results
##'
##' @description
##'
##' This function imports sage TMT quantitation and identification
##' results and combines them into a
##' [QFeatures::QFeatures()] object.
##'
##' @param quantTable `character(1)` containing the path to the
##'     quantification result file, typically "quant.tsv", or a
##'     `data.frame` containing the quantitation results.
##'
##' @param idTable `character(1)` containing the path to the
##'     identification results, typically "results.sage.tsv", or a
##'     `data.frame` containing the identification results.
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
##'     instance if several fractions from the same set of samples
##'     were acquired.
##'
##' @param quantPattern `character(1)` defining the pattern passed to
##'     [grep()] to extract the columns containing quantitative
##'     data. Default is `"tmt_"`.
##'
##' @param class `character(1)` with one of `"SummarizedExperiment"`
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
##' ## Add the data to the package's cache if they
##' ## aren't already available
##'
##' if (!sagerAvailableData()["id"])
##'   sagerAddData("id")
##' basename(idf <- sagerIdData())
##'
##' if (!sagerAvailableData()["quant"])
##'   sagerAddData("quant")
##' basename(qf <- sagerQuantData())
##'
##' ## Assays are split by filename
##' sageQFeatures(qf, idf)
##'
##' ## One single assay
##' sageQFeatures(qf, idf, splitBy = NULL)
sageQFeatures <- function(quantTable, idTable,
                          byQuant = c("file", "scannr"),
                          byId = c("filename", "scannr"),
                          splitBy = byQuant[1],
                          quantPattern = "tmt_",
                          class = c("SummarizedExperiment", "SingleCellExperiment"),
                          ...) {
    class <- match.arg(class)
    ## Get the quantitation data from input or from file
    if (is.data.frame(quantTable)) {
        quant <- quantTable
    } else {
        if (!is.character(quantTable) | length(quantTable) != 1 | !file.exists(quantTable))
            stop("Please provide one sage quant.tsv file/data.")
        quant <- read.delim(quantTable, ...)
    }
    ## Get the identification data from input or from file
    if (is.data.frame(idTable)) {
        id <- idTable
    } else {
        if (!is.character(idTable) | length(idTable) != 1 | !file.exists(idTable))
            stop("Please provide one sage results.sage.tsv file/data.")
        id <- read.delim(idTable, ...)
    }
    x <- merge(quant, id, by.x = byQuant, by.y = byId)
    ecol <- grep(quantPattern, names(x))
    if (!is.null(splitBy)) {
        x <- split(x, x[, splitBy])
    } else {
        x <- list(sage = x)
    }
    if (class == "SingleCellExperiment") {
        requireNamespace("scp")
        ans <- QFeatures(lapply(x, scp::readSingleCellExperiment, ecol = ecol))
    } else {
        ans <- QFeatures(lapply(x, QFeatures::readSummarizedExperiment, ecol = ecol))
    }
    ## Differentiate assays' colnames using the assay names
    for (i in seq_along(ans))
        colnames(ans[[i]]) <-
            paste(names(ans)[i], colnames(ans[[i]]), sep = ".")
    ans
}
