## library("BiocFileCache")

## quantFile <- "~/dev/sager/inst/extdata/quant.tsv"
## bfcadd(BiocFileCache(), "sageQuantExample", quantFile)

## sageFile <- "~/dev/sager/inst/extdata/results.sage.tsv"
## bfcadd(BiocFileCache(), "sageResultExample", sageFile)


## rawFiles <- c("/home/lgatto/dev/sager/inst/extdata/CBIO944_1.mzML",
##               "/home/lgatto/dev/sager/inst/extdata/CBIO944_2.mzML",
##               "/home/lgatto/dev/sager/inst/extdata/CBIO944_3.mzML",
##               "/home/lgatto/dev/sager/inst/extdata/CBIO944_4.mzML")
## bfcadd(BiocFileCache(), "sageRawExample", rawFiles)

## library(Spectra)
## sageMzML <- Spectra(rawFiles)
## bfc <- BiocFileCache()
## savepath <- bfcnew(bfc, "sageMzML", ext=".RData")
## save(sageMzML, file = savepath)


## =========================================


## --------------------------------------------------------------------
## Get the sage parameter file to run the seach (see below) and
## extract the mzML files used for that benchmark

library(jsonlite)
download.file("https://raw.githubusercontent.com/lazear/sage/master/figures/benchmark_params/tmt.json",
              dest = "../extdata/tmt.json")

mzml_files <- fromJSON("../extdata/tmt.json")$mzml_paths

## --------------------------------------------------------------------
## Get the raw files from the ProteomeXchange project
library(rpx)
px <- PXDataset("PXD016766")

raw_files <- sub(".mzML", ".raw", mzml_files)
f <- pxget(px, raw_files)

## --------------------------------------------------------------------
## Convert the raw files to mzML. These mzML files are stored in a
## temporary directory before being cached using rpx.

tmpdir <- tempdir()
mzml_dest <- file.path(tmpdir, sub("raw", "mzML", basename(f)))

convert <- paste0("mono ~/bin/ThermoRawFileParser/1.4.2/ThermoRawFileParser.exe",
                  " -i=", f,
                  " -b=", mzml_dest,
                  " -f=2", ## indexed mzML
                  " -p=1") ## no peak picking for MS level 1

sapply(convert, system)

## --------------------------------------------------------------------
## Add the mzML files to the rpx cache

library(BiocFileCache)

rpx_cache <- rpxCache()

## bfcadd(rpx_cache,
##        rname = basename(mzml_dest),
##        fpath = mzml_dest,
##        action = "copy")

mzml_rpath <- bfcquery(rpx_cache, "11cell_90min_hrMS2.+\\.mzML", exact = FALSE)$rpath

## --------------------------------------------------------------------
## Update the sage config with new mzML filenames and run it

config <- fromJSON("../extdata/tmt.json")
config$mzml_paths <- mzml_rpath
config$database$fasta <- "/mnt/isilon/CBIO/data/SCPCBIO/fasta/UP000005640_9606.fasta"
config$output_directory <- "output2"
config$process_files_parallel <- NULL
config$chimera <- TRUE
config$quant$tmt <- "Tmt11"
config$quant$tmt_level <- 2
config$quant$sn <- TRUE
toJSON(config, auto_unbox = TRUE) |> writeLines("../extdata/tmt2.json")

system("~/bin/sage/sage-0.8.1/target/release/sage ../extdata/tmt2.json")

## system("~/dev/sage/target/release/sage ../extdata/tmt2.json")


## --------------------------------------------------------------------
## Import results into R
library(sager)

x <- sageQFeatures("output2/quant.tsv", "output2/results.sage.tsv")

psm <- sagePSM("output2/results.sage.tsv")
psm

library(Spectra)
sp <- Spectra(mzml_rpath)
