library(sager)
library(tidyverse)

## ============================================================

library(QFeatures)
library(scp)

sageQFeatures(sagerQuantData(), sagerIdData(), splitBy = NULL)

qf <- sageQFeatures(sagerQuantData(), sagerIdData())
renameqf <- function(x) sub("\\.mzML", "", sub("^.+hrMS2_", "", x))

qf$filename <- rep(names(qf), each = 11)
qf$tmt_tag <- rep(paste0("tmt_", 1:11), length(qf))
qf$acquisition <- renameqf(qf$filename)
names(qf) <- paste0("psm", renameqf(names(qf)))
qf <- renamePrimary(qf, paste(qf$acquisition, qf$tmt_tag, sep = "."))
colnames(qf) <- CharacterList(lapply(colnames(qf), renameqf))


qf
colData(qf)


qf |>
    filterFeatures(~ label > 0) |>
    filterFeatures(~ rank == 1) |>
    filterFeatures(~ spectrum_fdr < 0.05) |>
    zeroIsNA(1:3) |>
    logTransform(i = 1:3, name = paste0("log_", names(qf))) |>
    aggregateFeaturesOverAssays(i = 4:6,
                                fcol = "peptide",
                                name = sub("psm", "peptide", names(qf)),
                                fun = colMedians,
                                na.rm = TRUE) |>
    joinAssays(i = 7:9,
               name = "peptides") |>
    normalize(i = 10,
              name = "norm_peptides",
              method = "center.mean") |>
    aggregateFeatures(i = "norm_peptides",
                      name = "proteins",
                      fcol = "proteins",
                      fun = MsCoreUtils::robustSummary,
                      na.rm = TRUE) -> qf2


impute(qf2[["proteins"]], method = "zero") |>
    assay() |>
    t() |>
    prcomp() |>
    factoextra::fviz_pca_ind(habillage = qf2$acquisition)

## ============================================================

library(PSMatch)
psm <- sagePSM(sagerIdData())
psm

as_tibble(psm) |>
    ggplot(aes(x = hyperscore,
               colour = label)) +
    geom_density()

as_tibble(psm) |>
    ggplot(aes(x = spectrum_fdr,
               colour = label)) +
    geom_density()



## ============================================================

library(Spectra)
sp <- Spectra(sagerMzMLData())
sp
