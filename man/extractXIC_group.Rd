% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_peaks_chromatograms.R
\name{extractXIC_group}
\alias{extractXIC_group}
\title{Extract XICs of all transitions requested in chromIndices.}
\usage{
extractXIC_group(
  mz,
  chromIndices,
  XICfilter = "sgolay",
  SgolayFiltOrd = 4,
  SgolayFiltLen = 9
)
}
\arguments{
\item{mz}{(mzRpwiz object)}

\item{chromIndices}{(vector of Integers) Indices of chromatograms to be extracted.}

\item{XICfilter}{(string) This must be one of the strings "sgolay", "none".}

\item{SgolayFiltOrd}{(integer) It defines the polynomial order of filer.}

\item{SgolayFiltLen}{(integer) Must be an odd number. It defines the length of filter.}
}
\value{
A list of data-frames. Each data frame has elution time and intensity of fragment-ion XIC.
}
\description{
Extracts XICs using mz object. Generally Savitzky–Golay filter is used, however, filter can be turned-off as well.
}
\examples{
dataPath <- system.file("extdata", package = "DIAlignR")
mzmlName<-paste0(dataPath,"/mzml/hroest_K120809_Strep10\%PlasmaBiolRepl2_R04_SW_filt.chrom.mzML")
mz <- mzR::openMSfile(mzmlName, backend = "pwiz")
chromIndices <- c(37L, 38L, 39L, 40L, 41L, 42L)
\dontrun{
XIC_group <- extractXIC_group(mz, chromIndices, SgolayFiltOrd = 4, SgolayFiltLen = 13)
}
}
\author{
Shubham Gupta, \email{shubh.gupta@mail.utoronto.ca}

ORCID: 0000-0003-3500-8152

License: (c) Author (2019) + MIT
Date: 2019-12-13
}
