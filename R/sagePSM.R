##' @title Import sage identification results
##'
##' @description
##'
##' This function imports sage identification results as a
##' [PSMatch::PSM()] object.
##'
##' @param idTable `character(1)` containing the identification
##'     results, typically "results.sage.tsv" or a `data.frame`
##'     containing the identification results.
##'
##' @param spectrum `character(1)` variable name that defines a
##'     spectrum in the PSM data. Default are `"scannr"`.
##'
##' @param peptide `character(1)` variable name that defines a peptide
##'     in the PSM data. Detaults are `"peptide"`.
##'
##' @param protein protein `character(1)` variable name that defines a
##'     protein in the PSM data. Detaults are `"proteins"`.
##'
##' @param decoy `character(1)` variable name that defines a decoy hit
##'     in the PSM data. Detaults are `"label"`.
##'
##' @param rank rank `character(1)` variable name that defines the
##'     rank of the peptide spectrum match in the PSM data. Default is
##'     `"rank"`.
##'
##' @param score score `character(1)` variable name that defines the
##'     PSM score. default is `"hyperscore"`.
##'
##' @param ... Additional arguments passed to [read.delim()].
##'
##' @return An instance of class [PSM()].
##'
##' @author Laurent Gatto
##'
##' @export
##'
##' @importFrom PSMatch PSM
##'
##' @examples
##'
##' basename(f <- sagerIdData())
##' sagePSM(f)
sagePSM <- function(idTable,
                    spectrum = "scannr",
                    peptide = "peptide",
                    protein = "proteins",
                    decoy = "label",
                    rank = "rank",
                    score = "hyperscore",
                    ...) {
    ## Get the identification data from input or from file
    if (is.data.frame(idTable)) {
        psmdf <- idTable
    } else {
        if (!is.character(idTable) | length(idTable) != 1 | !file.exists(idTable))
            stop("Please provide one sage results.sage.tsv file/data.")
        psmdf <- read.delim(idTable, ...)
    }
    psmdf[, decoy] <- psmdf[, decoy] < 0
    PSMatch::PSM(psmdf, spectrum = spectrum, peptide = peptide,
                 protein = protein, decoy = decoy,
                 rank = rank, score = score)
}
