##' @importFrom Spectra Spectra
readOrLoadSpectra <- function(x, class = "Spectra") {
    if (inherits(x, class))
        return(x)
    if (is.character(x) & all(file.exists(x)))
        return(Spectra::Spectra(x))
    stop(paste0("Expected a character pointing to existing file(s) ",
                "or an instance of class ", class, "."))
}

readOrLoadPSM <- function(x, class = "Spectra") {
    if (inherits(x, class))
        return(x)
    if (is.character(x) & length(x) == 1 & file.exists(x))
        return(sagePSM(x))
    stop(paste0("Expected a character pointing to existing file ",
                "or an instance of class ", class, "."))
}

readOrLoadQFeatures <- function(x, y,
                                class = "QFeatures") {
    if (inherits(x, class))
        return(x)
    if ((is.character(x) & length(x) == 1 & file.exists(x)) &
        (is.character(y) & length(y) == 1 & file.exists(y)))
        return(sageQFeatures(x, y))
    stop(paste0("Expected a character pointing to existing file ",
                "or an instance of class ", class, "."))
}


##' @title Import raw and sage data
##'
##' @description
##'
##' This function imports sage quantitation and identification results
##' and the raw data that produced them and combines all into a
##' [MsExperiment::MsExperiment()] object.
##'
##' @param rawData `character(1)` containing the path to the
##'     quantification raw MS data files or a [Spectra::Spectra()]
##'     object containing the raw MS data.
##'
##' @param quantData `character(1)` containing the path to the
##'     quantification result file (typically "quant.tsv"), or an
##'     object or class [SummarizedExperiment::SummarizedExperiment()]
##'     or [QFeatures::QFeatures()].
##'
##' @param idData `character(1)` containing the path to the
##'     identification results (typically "results.sage.tsv"), or an
##'     instance of class [PSMatch::PSM()].
##'
##' @return An instance of class [MsExperiment::MsExperiment()].
##'
##' @author Laurent Gatto
##'
##' @export
##'
##' @importFrom MsExperiment MsExperiment
sageMsExperiment <- function(rawData,
                             quantData,
                             idData) {
    sp <- readOrLoadSpectra(rawData)
    quant <- readOrLoadQFeatures(quantData, idData)
    id <- readOrLoadPSM(idData)
    message("Under development")
    MsExperiment::MsExperiment()
}
