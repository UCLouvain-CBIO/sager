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

readOrLoadSummarizedExperiment <- function(x, y,
                                           class = "SummarizedExperiment") {
    if (inherits(x, class))
        return(x)
    if ((is.character(x) & length(x) == 1 & file.exists(x)) &
        (is.character(y) & length(y) == 1 & file.exists(y)))
        return(sageSummarizedExperiment(x, y))
    stop(paste0("Expected a character pointing to existing file ",
                "or an instance of class ", class, "."))
}


sageMsExperiment <- function(rawFile,
                             quantFile,
                             idFile) {
    sp <- readOrLoadSpectra(rawFile)
    quant <- readOrLoadSummarizedExperiment(quantFile, idFile)
    id <- readOrLoadPSM(idFile)
    MsExperiment::MsExperiment()
}
