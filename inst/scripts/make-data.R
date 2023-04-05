## --------------------------------------------------------------------
## Get the sage parameter file to run the seach (see below) and
## extract the mzML files used for that benchmark

library(jsonlite)

download.file("https://raw.githubusercontent.com/lazear/sage/master/figures/benchmark_params/tmt.json",
              dest = "tmt.json")
mzml_files <- fromJSON("tmt.json")$mzml_paths

## --------------------------------------------------------------------
## Get the raw files from the ProteomeXchange project
library(rpx)
px <- PXDataset("PXD016766")

raw_files <- sub(".mzML", ".raw", mzml_files)
f <- pxget(px, raw_files)

## --------------------------------------------------------------------
## Convert the raw files to mzML using ThermoRawFileParser. These mzML
## files are stored in a temporary directory before being cached using
## BiocFileCache.

mzml_dest <- file.path(tmpdir, sub("raw", "mzML", basename(f)))
convert <- paste0("mono ~/bin/ThermoRawFileParser/1.4.2/ThermoRawFileParser.exe",
                  " -i=", f,
                  " -b=", mzml_dest,
                  " -f=2", ## indexed mzML
                  " -p=1") ## no peak picking for MS level 1
sapply(convert, system)

## --------------------------------------------------------------------
## Add the mzML files to the rpx cache using the BiocFileCache
## package. The cached locations are then queried and stored in
## mzml_rpath.

library(BiocFileCache)
rpx_cache <- rpx::rpxCache()

bfcadd(rpx_cache,
       rname = basename(mzml_dest),
       fpath = mzml_dest,
       action = "copy")

mzml_rpath <- bfcquery(rpx_cache, "11cell_90min_hrMS2.+\\.mzML", exact = FALSE)$rpath

## --------------------------------------------------------------------
## Update the sage config with new mzML filenames and run it

config <- fromJSON("tmt.json")
config$mzml_paths <- mzml_rpath
config$database$fasta <- "/mnt/isilon/CBIO/data/SCPCBIO/fasta/UP000005640_9606.fasta"
config$database$missed_cleavages <- 2
config$database$fragment_min_mz <- 100
config$database$variable_mods$'$' <- 49.2022
config$database$variable_mods$'[' <- 42.0
config$database$variable_mods$']' <- 111.0
config$database$variable_mods$'M' <- 15.9949
config$database$generate_decoys <- TRUE
config$database$max_variable_mods <- 2
config$database$decoy_tag <- "rev_"
config$output_directory <- "sage_output"
config$process_files_parallel <- NULL
config$chimera <- TRUE
config$quant$tmt <- "Tmt11"
config$quant$tmt_level <- 2
toJSON(config, auto_unbox = TRUE, pretty = TRUE) |>
    writeLines("tmt2.json")

system("~/bin/sage/sage-v0.10.0-x86_64-unknown-linux-gnu/sage tmt2.json")



## --------------------------------------------------------------------
## Import results into R
library(sager)

x <- sageQFeatures("output2/quant.tsv", "output2/results.sage.tsv")
x

psm <- sagePSM("output2/results.sage.tsv")
psm

library(Spectra)
sp <- Spectra(mzml_rpath)
