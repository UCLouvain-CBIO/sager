library("BiocFileCache")

quantFile <- "~/wrk/sager/inst/extdata/quant.tsv"
bfcadd(BiocFileCache(), "sageQuantExample", quantFile)

sageFile <- "~/wrk/sager/inst/extdata/results.sage.tsv"
bfcadd(BiocFileCache(), "sageResultExample", sageFile)
