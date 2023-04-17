setGeneric("addKEY", function(object, ...) standardGeneric("addKEY"))
setGeneric("subsetByKEY", function(object, ...) standardGeneric("subsetByKEY"))


##' @title Add and subsetting keys
##'
##' @description
##'
##' A key is a variable that is used to identify features within an
##' object. These features can refer to spectra in a `Spectra` object,
##' peptide-spectrum matches in a `PSM` object, or PSMs, peptides of
##' proteins in a `SummarizedExperiment` or `QFeatures` object. They
##' do not need to be unique.
##'
##' The goal of a key is to identify matching features across data
##' types, such as for example to match de MS spectra to the PSMs and
##' peptides. These different data types can be stored together in an
##' `MsExperiment` object, and matched through one or multiple keys.
##'
##' There are two general functions to work with keys across objects:
##'
##' - `addKEY()` will add a single key to an object.
##'
##' - `subsetByKEY()` will subset the object based on the presence of
##'   one or multiple values in one key..
##'
##' @param object An instance of class `SummarizedExperiment`,
##'     `QFeatures`, `PSM` or `Spectra`.
##'
##' @param vars `character()` referencing the variables to use to
##'     create the key. These are to be found in the object's
##'     `rowData` (for `SummarizedExperiment` and `QFeatures` objects)
##'     or `spectraData` (for `Spectra` objects) or in the object
##'     itself (for `PSM` objects). Throws an error if the variables
##'     aren't found. Default is `NULL`.
##'
##' @param key `character(1)` defining the key's name. Default is
##'     `".KEY"`.
##'
##' @param force `logical(1)` that defines is an existing key should
##'     be overwritten. Default is `FALSE` and an error is thrown if
##'     the key is already present.
##'
##' @param sep `character(1)` specifying the separator when multiple
##'     variables are pasted together to generate the key. Default is
##'     `"."`.
##'
##' @export
##'
##' @aliases addKEY subsetByKEY
##'
##' @rdname keys
##'
##' @importFrom SummarizedExperiment rowData rowData<-
##'
##' @importFrom methods setMethod setGeneric
setMethod("addKEY", "SummarizedExperiment",
          function(object, vars = NULL, key = ".KEY", force = FALSE, sep = ".") {
              if (key %in% names(rowData(object)) & !force)
                  stop(key, " already in names(rowData()). Use 'force = TRUE' to overwrite.")
              if (is.null(vars) | !all(vars %in% names(rowData(object))))
                  stop("vars to generate KEY must all be in names(rowData()).")
              if (length(vars) == 1) {
                  KEY <- rowData(object)[[vars]]
              } else {
                  rd <- rowData(object)[, vars]
                  KEY <- apply(rd, 1, paste, collapse = sep)

              }
              rowData(object)[, key] <- KEY
              object
          })


##' @importFrom Spectra spectraData spectraVariables spectraData<-
##'
##' @export
##'
##' @rdname keys
setMethod("addKEY", "Spectra",
          function(object, vars = NULL, key = ".KEY", force = FALSE, sep = ".") {
              if (key %in% spectraVariables(object) & !force)
                  stop(key, " already in spectraData(). Use 'force = TRUE' to overwrite.")
              if (is.null(vars) | !all(vars %in% spectraVariables(object)))
                  stop("vars to generate KEY must all be in spectraData().")
              if (length(vars) == 1) {
                  KEY <- spectraData(object)[[vars]]
              } else {
                  rd <- spectraData(object)[, vars]
                  KEY <- apply(rd, 1, paste, collapse = sep)

              }
              suppressWarnings(spectraData(object)[, key] <- KEY)
              object
          })

##' @export
##'
##' @rdname keys
setMethod("addKEY", "PSM",
          function(object, vars = NULL, key = ".KEY", force = FALSE, sep = ".") {
              if (key %in% names(object) & !force)
                  stop(key, " already in names(). Use 'force = TRUE' to overwrite.")
              if (is.null(vars) | !all(vars %in% names(object)))
                  stop("vars to generate KEY must all be in names().")
              if (length(vars) == 1) {
                  KEY <- object[[vars]]
              } else {
                  KEY <- apply(object[, vars], 1, paste, collapse = sep)

              }
              object[, key] <- KEY
              object
          })

##' @importFrom QFeatures replaceAssay
##'
##' @export
##'
##' @rdname keys
setMethod("addKEY", "QFeatures",
          function(object, vars = NULL, key = ".KEY", force = FALSE, sep = ".") {
              if (is.null(vars))
                  return(object)
              for (i in seq_along(object)) {
                  x <- object[[i]]
                  if (!all(vars %in% names(rowData(x))))
                      next()
                  x <- addKEY(x, vars, key, force, sep)
                  object <- replaceAssay(object, x, i)
              }
              object
          })

## ============================================================================


##' @importFrom Spectra spectraData spectraVariables
##'
##' @export
##'
##' @param value `character()` containing the value of key of the
##'     feature(s) of interest.
##'
##' @rdname keys
setMethod("subsetByKEY", "Spectra",
          function(object, value, key = ".KEY") {
              if (!key %in% spectraVariables(object)) {
                  message("Key '", key,
                          "' not found. Returning empty object.")
                  return(object[integer()])
              }
              object[spectraData(object)[[key]] %in% value, ]
          })

##' @importFrom PSMatch PSM
##'
##' @export
##'
##' @rdname keys
setMethod("subsetByKEY", "PSM",
          function(object, value, key = ".KEY") {
              if (!key %in% names(object)) {
                  message("Key '", key,
                          "' not found. Returning empty object.")
                  return(object[integer(), ])
              }
              object[object[[key]] %in% value, ]
          })

##' @importFrom SummarizedExperiment rowData
##'
##' @export
##'
##' @rdname keys
setMethod("subsetByKEY", "SummarizedExperiment",
          function(object, value, key = ".KEY") {
              if (!key %in% names(rowData(object))) {
                  message("Key '", key,
                          "' not found. Returning empty object.")
                  return(object[integer(), ])
              }
              object[rowData(object)[[key]] %in% value, ]
          })

##' @importFrom QFeatures VariableFilter filterFeatures nrows
##'
##' @export
##'
##' @rdname keys
##'
##' @param keep `logical(1)` that defines whether to keep assays that
##'     didn't contain the key of interest. If `TRUE`, these are
##'     returned as empty assays (i.e. with zero
##'     rows/features)`. Default is `FALSE`; only assays containing
##'     matching keys are returned.
setMethod("subsetByKEY", "QFeatures",
          function(object, value, key = ".KEY", keep = FALSE) {
              for (i in seq_along(object)) {
                  suppressMessages(
                      x <- subsetByKEY(object[[i]], value, key))
                  object <- replaceAssay(object, x, i)
              }
              if (!keep)
                  object <- object[, , nrows(object) > 0]
              object
          })


##' @importFrom MsExperiment MsExperiment
##'
##' @export
##'
##' @rdname keys
setMethod("subsetByKEY", "MsExperiment",
          function(object, value, key = ".KEY") {
          ## function(object, value, key = ".KEY", keep = FALSE, data) {
              stop("Not yet implemented")
          })


## TODO:
## - findKEY to get indices
