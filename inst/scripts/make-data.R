## --------------------------------------------------------------------
## Get the sage parameter file to run the seach (see below) and
## extract the mzML files used for that benchmark

tmpdir <- tempdir()

library(jsonlite)
download.file("https://raw.githubusercontent.com/lazear/sage/master/figures/benchmark_params/tmt.json",
              dest = file.path(tmpdir, "tmt.json"))
(mzml_files <- fromJSON(file.path(tmpdir, "tmt.json"))$mzml_paths)


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

## there might be subsetted mzML files here, so make sure we remove
## these before proceeded
(mzml_rpath <- mzml_rpath[!grepl("subset_", mzml_rpath)])


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
## Create smaller data and add them to the cache. First, a set of
## scans from 3 mzML files are selected. These new subsetted mzML
## files are then saved to the sager cache and re-searched using sage
## and config file above.

library(tidyverse)

## --------------------------------------------------------------------
## Subset sage identification results results.sage.tsv

selected_mzml_files <-
    c("5109a2c76e223_3ff35e72d009e6_dq_00087_11cell_90min_hrMS2_A11.mzML",
      "5109a2c76e223_3ff35ee5d5a37_dq_00084_11cell_90min_hrMS2_A5.mzML",
      "5109a2c76e223_3ff35e790c7b57_dq_00086_11cell_90min_hrMS2_A9.mzML")

set.seed(123)
subset_scannr <- read_tsv(sager_rpath["id"]) |>
    filter(filename %in%
           selected_mzml_files) |>
    sample_n(600) |>
    select(filename, scannr)


## --------------------------------------------------------------------
## Create the subsetted mzML files

library(Spectra)

for (f in selected_mzml_files) {
    i <- match(f, basename(bfcinfo(sager_cache)$rpath))
    fi <- bfcinfo(sager_cache)$rpath[i]
    sp <- Spectra(fi)
    scans <- filter(subset_scannr, filename == f)[[2]]
    sp <- sp[sp$spectrumId %in% scans]
    sp |>
        export(MsBackendMzR(),
               file = file.path(tmpdir, sub("^.+dq", "subset_dq", basename(fi))))
}


(subset_mzmls <- dir(tmpdir, full.names = TRUE, pattern = "subset.+mzML"))

bfcadd(sager_cache,
       rname = basename(subset_mzmls),
       fpath = subset_mzmls,
       action = "copy",
       fname = "exact")

## --------------------------------------------------------------------
## Re-run sage

config$mzml_paths <- subset_mzmls
config$output_directory <- "sage_subset"
toJSON(config, auto_unbox = TRUE, pretty = TRUE) |>
    writeLines("tmt_subset.json")

system("~/bin/sage/sage-v0.10.0-x86_64-unknown-linux-gnu/sage tmt_subset.json")

(sage_subset_results <- dir("sage_subset", full.names = TRUE))

bfcadd(sager_cache,
       rname = "sager_subset_quant",
       fpath = grep("quant.tsv", sage_subset_results, value = TRUE),
       action = "copy")

bfcadd(sager_cache,
       rname = "sager_subset_id",
       fpath = grep("results.sage.tsv", sage_subset_results, value = TRUE),
       action = "copy")

bfcadd(sager_cache,
       rname = "sager_subset_json",
       fpath = grep("results.json", sage_subset_results, value = TRUE),
       action = "copy")
