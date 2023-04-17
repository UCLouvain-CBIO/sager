library(sager)
library(Spectra)
library(QFeatures)
library(SummarizedExperiment)

## ## ---------------------------
## ## With Spectra
## sp <- Spectra(sagerMzMLData())

## sp$filename <- basename(dataOrigin(sp))
## sp <- makeKEY(sp, var = c("filename", "spectrumId"))
## sp$.KEY

## sp <- makeKEY(sp, var = c("dataOrigin", "spectrumId"), key = "KEY2")

## qf <- sageQFeatures(sagerQuantData(), sagerIdData())

## ## ---------------------------
## ## With SE
## se <- makeKEY(qf[[1]], vars = c("file", "scannr"))
## rowData(se)$.KEY

## ## ---------------------------
## ## With QFreature

## qf <- makeKEY(qf, vars = c("file", "scannr"))
## head(rowData(qf[[1]])$.KEY)
## head(rowData(qf[[2]])$.KEY)
## rowData(qf[[3]])$.KEY

## Run vignette up to creation of mse

k <- c("subset_dq_00084_11cell_90min_hrMS2_A5.mzML.controllerType=0 controllerNumber=1 scan=11785",
       "subset_dq_00084_11cell_90min_hrMS2_A5.mzML.controllerType=0 controllerNumber=1 scan=17928")

subsetByKEY(psm, k)

subsetByKEY(qf[[1]], k)

subsetByKEY(qf, k)

subsetByKEY(qf, k, keep = TRUE)

subsetByKEY(qf, k) |> nrows()

subsetByKEY(sp, k)

x <- qf["sp|P49916|DNLI3_HUMAN", , ]
subsetByKEY(qf, k)

## ==============================================================
##
## Add/update in MsExperiment
otherData <- function(object) {
    stopifnot(inherits(object, "MsExperiment"))
    object@otherData

}

`otherData<-` <- function(object, value) {
    object@otherData <- value
    object
}


setMethod("show", "MsExperiment",
          function (object) {
              mess <- "Object of class"
              if (MsExperiment:::.ms_experiment_is_empty(object))
                  mess <- "Empty object of class"
              cat(mess, class(object), "\n")
              if (length(experimentFiles(object))) ## FIX: print based on length
                  cat(" Files:", paste(names(experimentFiles(object)),
                                       collapse = ", "), "\n")
              if (!is.null(object@spectra)) {
                  mstab <- table(msLevel(object@spectra))
                  cat(" Spectra:", paste0("MS", names(mstab), " (", mstab, ")"), "\n")
              }
              ## Show quantitative data
              if (!is.null(object@qdata)) {
                  if (inherits(object@qdata, "SummarizedExperiment"))
                      cat( " SummarizedExperiment:",
                          nrow(object@qdata), "feature(s)\n")
                  else ## QFeatures
                      cat( " QFeatures:",
                          length(object@qdata), "assay(s)\n")

              }
              ## Show other data
              if (length(object@otherData)) {
                  cat(" Other data:",
                      paste(names(object@otherData), collapse = ", "),
                      "\n")
              }
              if (nrow(object@sampleData)) {
                  cat(" Experiment data:", nrow(object@sampleData),
                      "sample(s)\n")
              }
              lnks <- object@sampleDataLinks
              if (length(lnks)) {
                  cat(" Sample data links:\n")
                  for (i in seq_along(lnks)) {
                      if (mcols(lnks)$subsetBy[i] == 2)
                          cols <- " column(s).\n"
                      else cols <- " element(s).\n"
                      cat("  - ", names(lnks)[i], ": ",
                          length(unique(lnks[[i]][, 1L])),
                          " sample(s) to ",
                          length(unique(lnks[[i]][, 2L])), cols, sep = "")
                  }
              }
          })

## ==============================================================


subsetByKEY(mse, k[1], otherdata = "PSM")

msek <- subsetByKEY(mse, k, otherdata = "PSM")
msek
