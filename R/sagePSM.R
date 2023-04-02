##' @title Import sage identification results
##'
##' @description
##'
##' This function imports sage identification results as a
##' [PSMatch::PSM()] object.
##'
##' @param idFile `character(1)` containing the identification
##'     results, typically "results.sage.tsv".
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
##' idFile <- BiocFileCache::bfcquery(
##'                              BiocFileCache::BiocFileCache(),
##'                              "sageRes")$fpath
##' sagePSM(idFile)
sagePSM <- function(idFile,
                    spectrum = "scannr",
                    peptide = "peptide",
                    protein = "proteins",
                    decoy = "label",
                    rank = "rank",
                    score = "hyperscore",
                    ...) {
    psmdf <- read.delim(idFile, ...)
    psmdf[, decoy] <- psmdf[, decoy] < 0
    PSMatch::PSM(psmdf, spectrum = spectrum, peptide = peptide,
                 protein = protein, decoy = decoy,
                 rank = rank, score = score)
}
