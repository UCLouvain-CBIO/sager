% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sageQFeatures.R
\name{sageQFeatures}
\alias{sageQFeatures}
\title{Import sage quantitative results}
\usage{
sageQFeatures(
  quantTable,
  idTable,
  byQuant = c("filename", "scannr"),
  byId = c("filename", "scannr"),
  splitBy = byQuant[1],
  quantPattern = "tmt_",
  class = c("SummarizedExperiment", "SingleCellExperiment"),
  ...
)
}
\arguments{
\item{quantTable}{\code{character(1)} containing the path to the
quantification result file, typically "quant.tsv", or a
\code{data.frame} containing the quantitation results.}

\item{idTable}{\code{character(1)} containing the path to the
identification results, typically "results.sage.tsv", or a
\code{data.frame} containing the identification results.}

\item{byQuant}{\code{character()} containing the specifications of the
quantification columns used for merging. The first element
should correspond the the raw files. Passed the \code{\link[=merge]{merge()}} as
\code{by.x}. Default is (since 0.3.1) \code{c("filename", "scannr")}.}

\item{byId}{\code{character()} containing the specifications of the
identification columns used for merging. Passed the \code{\link[=merge]{merge()}}
as \code{by.y}. Default is \code{c("filename", "scannr")}.}

\item{splitBy}{A character refering the a \code{quantFile} or \code{idFile}
column that will be used to split the merged table into
different assays, i.e. acquisitions corresponding to different
sets of samples. Default is to use \code{byQuant[1]}. Set to \code{NULL}
to not split and return a result with a single assay, for
instance if several fractions from the same set of samples
were acquired.}

\item{quantPattern}{\code{character(1)} defining the pattern passed to
\code{\link[=grep]{grep()}} to extract the columns containing quantitative
data. Default is \code{"tmt_"}.}

\item{class}{\code{character(1)} with one of \code{"SummarizedExperiment"}
or \code{"SingleCellExperiment"} defining the assay's
class. Default is the former.}

\item{...}{Additional parameters passed to \code{\link[=read.delim]{read.delim()}}.}
}
\value{
An instance of class \code{\link[=QFeatures]{QFeatures()}} with as many assays as
defined by \code{splitBy}.
}
\description{
This function imports sage TMT quantitation and identification
results and combines them into a
\code{\link[QFeatures:QFeatures-class]{QFeatures::QFeatures()}} object.
}
\examples{

## Add the data to the package's cache if they
## aren't already available

if (!sagerAvailableData("id"))
  sagerAddData("id")
basename(idf <- sagerIdData())

if (!sagerAvailableData("quant"))
  sagerAddData("quant")
basename(qf <- sagerQuantData())

## Assays are split by filename - setting byQuant to use the old
## 'file' filename in the quantitation output file. New default
## value (since 0.3.1) is 'filename'.
sageQFeatures(qf, idf, byQuant = c("file", "scannr"))

## One single assay
sageQFeatures(qf, idf, splitBy = NULL, byQuant = c("file", "scannr"))
}
\author{
Laurent Gatto
}
