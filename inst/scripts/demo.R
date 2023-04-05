## --------------------------------------------------------------------
## Get sage results from the rpx cache



## --------------------------------------------------------------------
## Import with sager
library(sager)

library(QFeatures)
x <- sageQFeatures(sager_rpath["quant"], sager_rpath["id"])
x

library(PSMatch)
psm <- sagePSM(sager_rpath["id"])
psm

library(Spectra)
sp <- Spectra(mzml_rpath)
sp
