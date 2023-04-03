library("BiocFileCache")

quantFile <- "~/dev/sager/inst/extdata/quant.tsv"
bfcadd(BiocFileCache(), "sageQuantExample", quantFile)

sageFile <- "~/dev/sager/inst/extdata/results.sage.tsv"
bfcadd(BiocFileCache(), "sageResultExample", sageFile)


rawFiles <- c("/home/lgatto/dev/sager/inst/extdata/CBIO944_1.mzML",
              "/home/lgatto/dev/sager/inst/extdata/CBIO944_2.mzML",
              "/home/lgatto/dev/sager/inst/extdata/CBIO944_3.mzML",
              "/home/lgatto/dev/sager/inst/extdata/CBIO944_4.mzML")
bfcadd(BiocFileCache(), "sageRawExample", rawFiles)

library(Spectra)
sageMzML <- Spectra(rawFiles)
bfc <- BiocFileCache()
savepath <- bfcnew(bfc, "sageMzML", ext=".RData")
save(sageMzML, file = savepath)
