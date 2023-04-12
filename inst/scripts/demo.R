library(sager)
library(Spectra)
library(QFeatures)
library(SummarizedExperiment)

## ---------------------------
## With Spectra
sp <- Spectra(sagerMzMLData())

sp$filename <- basename(dataOrigin(sp))
sp <- makeKEY(sp, var = c("filename", "spectrumId"))
sp$.KEY


sp <- makeKEY(sp, var = c("dataOrigin", "spectrumId"), key = "KEY2")

subsetByKEY(sp, "5779224eb8e7_sager_subset_PXD016766.mzML.controllerType=0 controllerNumber=1 scan=5869")

qf <- sageQFeatures(sagerQuantData(), sagerIdData())

## ---------------------------
## With SE
se <- makeKEY(qf[[1]], vars = c("file", "scannr"))
rowData(se)$.KEY

## ---------------------------
## With QFreature

qf <- makeKEY(qf, vars = c("file", "scannr"))
head(rowData(qf[[1]])$.KEY)
head(rowData(qf[[2]])$.KEY)
rowData(qf[[3]])$.KEY
