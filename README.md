## `sager`: Analysing sage results with R

The `sager` package can be used to import results produced by the
[sage](https://lazear.github.io/sage/) search engine into R. Sage
produces tab-separated output files for identification and
quantitation results. In `sager`, we import these result files into
establised Bioconductor classes:


- Identification results are parsed and imported as
  [PSM](https://rformassspectrometry.github.io/PSMatch/articles/PSM.html)
  objects with
  [sagePSM()](https://uclouvain-cbio.github.io/sager/reference/sagePSM.html).

- Quantitation (and identification) results are parsed, merged and
  imported as
  [QFeatures](https://rformassspectrometry.github.io/QFeatures/articles/QFeatures.html)
  objects with
  [sageQFeatures()](https://uclouvain-cbio.github.io/sager/reference/sageQFeatures.html).

These functions are described and demonstrated in their respective
manual pages using data generated from Yu *et al.* [Benchmarking the
Orbitrap Tribrid Eclipse for Next Generation Multiplexed
Proteomics](https://doi.org/10.1021/acs.analchem.9b05685), downloaded
from ProteomeXchange project
[PXD016766](https://www.ebi.ac.uk/pride/archive/projects/PXD016766). See
[sagerData()](https://uclouvain-cbio.github.io/sager/reference/sagerData.html)
for details.

If you want to install and try the package out:

```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("UCLouvain-CBIO/sager")
```

If you run into issues of have questions, please open [an
issue](https://github.com/UCLouvain-CBIO/sager/issues).
