% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cal.indices.365togreg.R
\name{cal.indices.365togreg}
\alias{cal.indices.365togreg}
\title{Compute indices for 365-days to Gregorian calendar conversion}
\usage{
cal.indices.365togreg(yearend, outfile = NA)
}
\arguments{
\item{yearend}{Last year to consider (implicitly, first year is year 1).}

\item{outfile}{If provided, the resulting vector will be written into
this RData file (optional).}
}
\value{
Vector (1:yearend): Index for each given 365-days
year after which to append an NA days (NA: no NA day to be
introduced in this year).
}
\description{
\code{cal.indices.365togreg} determines the day-of-year (DOY)
indices for each individual year of a 365-days calendar (for a given period)
at which to introduce NAs in order to transfer series to a full Gregorian
calendar.
}
\examples{
\dontrun{
# Calculate DOY indices after which to fill in NAs for all years
of the period 1 to 2200. No outfile written.
cal.indices.365togreg(2200)
}
}
\author{
Sven Kotlarski (MeteoSwiss)
}
