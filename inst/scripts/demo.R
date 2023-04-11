library(sager)
library(Spectra)
library(QFeatures)

## ---------------------------
## With Spectra
sp <- Spectra(sagerMzMLData())

sp$filename <- basename(dataOrigin(sp))
sp <- makeKEY(sp, var = c("filename", "spectrumId"))
sp$.KEY


qf <- sageQFeatures(sagerQuantData(), sagerIdData())

## ---------------------------
## With SE
se <- makeKEY(qf[[1]], vars = c("file", "scannr"))
rowData(se)$.KEY

## ---------------------------
## With QFreature

qf <- makeKEY(qf, vars = c("file", "scannr"))
rowData(qf[[1]])$.KEY
rowData(qf[[2]])$.KEY
rowData(qf[[3]])$.KEY
