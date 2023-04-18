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

## See https://github.com/rformassspectrometry/QFeatures/issues/184



## ==============================================================


subsetByKEY(mse, k[1], otherdata = "PSM")

msek <- subsetByKEY(mse, k, otherdata = "PSM")
msek


x <- qf["sp|P49916|DNLI3_HUMAN", , ] |> dropEmptyAssays()
