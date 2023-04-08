## --------------------------------------------------------------------
## Get the sage parameter file to run the seach (see below) and
## extract the mzML files used for that benchmark

tmpdir <- tempdir()

library(jsonlite)
download.file("https://raw.githubusercontent.com/lazear/sage/master/figures/benchmark_params/tmt.json",
              dest = file.path(tmpdir, "tmt.json"))
mzml_files <- fromJSON(file.path(tmpdir, "tmt.json"))$mzml_paths


## --------------------------------------------------------------------
## Get the raw files from the ProteomeXchange project
library(rpx)
px <- PXDataset("PXD016766")

(raw_files <- sub(".mzML", ".raw", mzml_files))
f <- pxget(px, raw_files)

## --------------------------------------------------------------------
## Convert the raw files to mzML using ThermoRawFileParser. These mzML
## files are stored in a temporary directory before being cached using
## BiocFileCache.

mzml_dest <- file.path(tmpdir, sub("raw", "mzML", basename(f)))
convert <- paste0("mono ~/bin/ThermoRawFileParser/1.4.2/ThermoRawFileParser.exe",
                  " -i=", f,
                  " -b=", mzml_dest,
                  " -N",   ## include noise data in mzML output
                  " -f=2", ## indexed mzML
                  " -p=1") ## no peak picking for MS level 1
sapply(convert, system)

## --------------------------------------------------------------------
## Add the mzML files to the BiocFileCache cache. The cached locations
## are then queried and stored in mzml_rpath.

library(BiocFileCache)
(sager_cache <- sager::sagerCache())

bfcadd(sager_cache,
       rname = basename(mzml_dest),
       fpath = mzml_dest,
       action = "copy")

(mzml_rpath <- bfcquery(sager_cache, "11cell_90min_hrMS2.+\\.mzML", exact = FALSE)$rpath)

## --------------------------------------------------------------------
## Update the sage config with new mzML filenames and run it

config <- fromJSON(file.path(tmpdir, "tmt.json"))
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
## Add the sage results to the BiocFileCache cache. The cached
## locations are then queried and stored in sager_rpath.

(sage_results <- dir("sage_output", full.names = TRUE))

bfcadd(sager_cache,
       rname = paste0("sager_", basename(sage_results)),
       fpath = sage_results,
       action = "copy")

sager_rpath <- c(quant = bfcquery(sager_cache, "sager_quant")$rpath,
                 id = bfcquery(sager_cache, "sager_results.sage.tsv")$rpath)

## ====================================================================
## Create smaller data and add them to the cache.

library(tidyverse)

## --------------------------------------------------------------------
## Subset sage identification results results.sage.tsv

set.seed(123)
sager_id2 <- read_tsv(sager_rpath["id"]) |>
    filter(filename %in%
           c("5109a2c76e223_3ff35e72d009e6_dq_00087_11cell_90min_hrMS2_A11.mzML",
             "5109a2c76e223_3ff35ee5d5a37_dq_00084_11cell_90min_hrMS2_A5.mzML",
             "5109a2c76e223_3ff35e790c7b57_dq_00086_11cell_90min_hrMS2_A9.mzML")) |>
    sample_n(500)

write_tsv(sager_id2, file.path(tmpdir, "subset_results.sage.tsv"))

bfcadd(sager_cache,
       rname = "sager_subset_id",
       fpath = file.path(tmpdir, "subset_results.sage.tsv"),
       action = "copy")

## --------------------------------------------------------------------
## Subset corresponding spectra: based on this selection of good MS2
## scans, identify the precursor scans and extract all corresponding
## MS2 scans.
library(Spectra)

sp <- Spectra(mzml_rpath)
sp$filename <- basename(sp$dataOrigin)

prec_scans <-
    left_join(sager_id2, data.frame(spectraData(sp)),
              by = join_by(filename == filename,
                           scannr == spectrumId)) |>
    mutate(scannr = sub("^.+scan=", "", scannr)) |>
    select(dataOrigin, precScanNum)

k <- split(prec_scans$precScanNum, prec_scans$dataOrigin)

sager_subset_PXD016766 <-
    do.call(c, lapply(seq_along(k), function(i)
        filterPrecursorScan(sp, k[[i]], names(k)[i])))
sager_subset_PXD016766@processing <- character()

sager_subset_PXD016766 |>
    export(MsBackendMzR(),
           file = file.path(tmpdir, "sager_subset_PXD016766.mzML"))

bfcadd(sager_cache,
       rname = "sager_subset_PXD016766",
       fpath = file.path(tmpdir, "sager_subset_PXD016766.mzML"),
       action = "copy")


## --------------------------------------------------------------------
## Subset sage quantitation results quant.tsv

sager_quant2 <- read_tsv(sager_rpath["quant"]) |> ## has 14 variables
    right_join(sager_id2, multiple = "all") |>
    select(1:14)

write_tsv(sager_quant2, file.path(tmpdir, "subset_quant.tsv"))

bfcadd(sager_cache,
       rname = "sager_subset_quant",
       fpath = file.path(tmpdir, "subset_quant.tsv"),
       action = "copy")
