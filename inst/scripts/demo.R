idFile <- BiocFileCache::bfcquery(
                             BiocFileCache::BiocFileCache(),
                             "sageRes")$fpath


quantFile <- BiocFileCache::bfcquery(
                                BiocFileCache::BiocFileCache(),
                                "sageQuant")$fpath

rawFiles <- BiocFileCache::bfcquery(
                               BiocFileCache::BiocFileCache(),
                               "sageRaw")$fpath


BiocFileCache::bfcrpath(BiocFileCache::BiocFileCache(), "sageMzML")

load(BiocFileCache::bfcrpath(BiocFileCache::BiocFileCache(), "sageMzML"))
