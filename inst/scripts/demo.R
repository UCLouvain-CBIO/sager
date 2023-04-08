## --------------------------------------------------------------------
## Import with sager
library(sager)

library(QFeatures)
library(scp)

sageQFeatures(sagerQuantData(), sagerIdData(), splitBy = NULL)

qf <- sageQFeatures(sagerQuantData(), sagerIdData())


qf$filename <- rep(names(qf), each = 11)
qf$tmt_tag <- rep(paste0("tmt_", 1:11), 12)
qf <- renamePrimary(qf,
                    sub("\\.mzML", "",
                        sub("^.+hrMS2_", "", rownames(colData(qf)))))

names(qf) <- sub("\\.mzML", "", sub("^.+MS2_", "psm", names(qf)))
qf

colData(qf)


nms <- sub("psm", "", names(qf))
qf |>
    filterFeatures(~ label > 0) |>
    filterFeatures(~ rank == 1) |>
    filterFeatures(~ spectrum_fdr < 0.05) |>
    aggregateFeaturesOverAssays(i = 1:12,
                                fcol = "peptide",
                                name = sub("psm", "peptide", names(qf)),
                                fun = colMedians,
                                na.rm = TRUE) |>
    joinAssays(i = paste0("peptide", nms),
               name = "peptides") |>
    aggregateFeatures(i = "peptides",
                      name = "proteins",
                      fcol = "proteins",
                      fun = colMedians,
                      na.rm = TRUE) -> qf2


logTransform(qf2[["proteins"]], pc = 1) |>
    filterNA() |>
    assay() |>
    t() |>
    prcomp() |>
    fviz_pca_ind()


library(PSMatch)
psm <- sagePSM(sagerIdData())
psm

library(Spectra)
sp <- Spectra(sagerMzMLData())
sp
