## --------------------------------------------------------------------
## Get sage results from the rpx cache

library(BiocFileCache)
rpx_cache <- rpx::rpxCache()

## Quantitation and identification results
sager_rpath <- c(quant = bfcquery(rpx_cache, "sager_quant")$rpath,
                 id = bfcquery(rpx_cache, "sager_results.sage.tsv")$rpath)

## Raw data
mzml_rpath <- bfcquery(rpx_cache, "11cell_90min_hrMS2.+\\.mzML", exact = FALSE)$rpath


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
