setGeneric("makeKEY", function(object, ...) standardGeneric("makeKEY"))

setMethod("makeKEY", "SummarizedExperiment",
          function(object, vars = NULL, key = ".KEY", force = FALSE, sep = ".") {
              if (key %in% names(rowData(object)) & !force)
                  stop(key, " already in names(rowData()). Use 'force = TRUE to overwrite.")
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

setMethod("makeKEY", "Spectra",
          function(object, vars = NULL, key = ".KEY", force = FALSE, sep = ".") {
              if (key %in% spectraVariables(object) & !force)
                  stop(key, " already in spectraData(). Use 'force = TRUE to overwrite.")
              if (is.null(vars) | !all(vars %in% spectraVariables(object)))
                  stop("vars to generate KEY must all be in spectraData().")
              if (length(vars) == 1) {
                  KEY <- spectraData(object)[[var]]
              } else {
                  rd <- spectraData(object)[, vars]
                  KEY <- apply(rd, 1, paste, collapse = sep)

              }
              suppressWarnings(spectraData(object)[, key] <- KEY)
              object
          })

setMethod("makeKEY", "QFeatures",
          function(object, vars = NULL, key = ".KEY", force = FALSE, sep = ".") {
              if (is.null(vars))
                  return(object)
              for (i in seq_along(object)) {
                  x <- object[[i]]
                  if (!all(vars %in% names(rowData(x))))
                      next()
                  x <- makeKEY(x, vars, key, force, sep)
                  object <- replaceAssay(object, x, i)
              }
              object
          })
