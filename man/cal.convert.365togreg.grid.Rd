% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cal.convert.365togreg.grid.R
\name{cal.convert.365togreg.grid}
\alias{cal.convert.365togreg.grid}
\title{Convert gridded 365-days field to Gregorian calendar}
\usage{
cal.convert.365togreg.grid(org.series, yearstart, yearend, fill.vector,
  data.type = 0)
}
\arguments{
\item{org.series}{Original time series (complete years only!).}

\item{yearstart}{First year of original 365-day time series.}

\item{yearend}{Last year of original 365-day time series.}

\item{fill.vector}{Vector indicating at which positions NA days
should be introduced; output of function
\code{\link{cal.indices.365togreg.doytoabs()}}. If
\code{data.type}=1: filename of RData file containing the
fill vector.}

\item{data.type}{If 0: fill vector is provided as an
R object with name \code{fill.vector} (default). If 1: fill vector
is provided in an RData file containing the fill vector only
(filename is provided by \code{fill.vector}).}
}
\value{
3D array (nx x ny x ndays): 	original time series but
in Gregorian calendar (original series filled with NAs).
}
\description{
\code{cal.convert.365togreg.grid} converts a 3D field
(nx * ny * time) of a 365-days calendar (complete years) to Gregorian
calendar by filling in NAs.
}
\examples{
\dontrun{
# convert gridded daily series of a 365-days calendar for the years
# 1980 to 2009 to a Gregorian calendar. NA indices provided as vector
# 'indices'
cal.convert.365togreg.grid(series,1980,2009,indices,0)
}
}
\author{
Iris Feigenwinter, Sven Kotlarski (MeteoSwiss)
}
