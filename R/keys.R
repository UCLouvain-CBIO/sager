setGeneric("addKEY", function(object, ...) standardGeneric("addKEY"))
setGeneric("subsetByKEY", function(object, ...) standardGeneric("subsetByKEY"))


##' @title Add a key to an object
##'
##' @description
##'
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
##' @rdname addKEY
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
##' @rdname addKEY
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

##' @importFrom QFeatures replaceAssay
##'
##' @export
##'
##' @rdname addKEY
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


setMethod("subsetByKEY", "Spectra",
          function(object, value, key = ".KEY") {
              stopifnot(key %in% spectraVariables(object))
              object[spectraData(object)[[key]] == value]
          })

setMethod("subsetByKEY", "SummarizedExperiment",
          function(object, value, key = ".KEY") {
              stopifnot(key %in% names(rowData(object)))
              object[rowData(object)[[key]] == value, ]
          })


setMethod("subsetByKEY", "QFeatures",
          function(object, value, key = ".KEY") {
          })


setMethod("subsetByKEY", "MsExperiment",
          function(object, value, key = ".KEY") {
              ## call subsetByKEY on spectra(object), qdata(object)
              ## and possible other in otherData(object)
          })



## TODO:
## - vectorise subsetByKEY
## - findKEY to get indices
