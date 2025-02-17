% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qm.cdf.bulk.R
\name{qm.cdf.bulk}
\alias{qm.cdf.bulk}
\title{Compute CDF for bulk series}
\usage{
qm.cdf.bulk(series, minq, maxq, incq, var, pr.wet)
}
\arguments{
\item{series}{Time series of modelled / observed data.}

\item{minq}{Minimum quantile [0..1].}

\item{maxq}{Maximum quantile [0..1].}

\item{incq}{Quantile increment (bin size).}

\item{var}{Variable name ("pr" for precip).}

\item{pr.wet}{Wet day threshold [mm/day].}
}
\value{
List of 3:
$cdf.vector: CDF of bulk series (vector[nquantiles]).
$f.wet: wet day frequency of bulk series; NA if not
precipitation.
$cdf.wet.vector: wet day CDF for bulk series
(vector[101]; all percentiles); NA if not precipitation.
}
\description{
\code{qm.cdf.bulk} computes a cumulative distribution
function (CDF) for bulk series (no day-of-year dependence). In case
of precipitation the function also computes the wet day frequency
and the wet day CDF.
}
\examples{
\dontrun{
# Compute bulk CDF for min and max quantile of 0.01 and 0.99,
# respectively. Quantile increment: 0.01. Variable: Precipitation.
# Wet day threshold: 0.1 mm/day.
qm.cdf.bulk(time.series,0.01,0.99,0.01,'pr',0.1)
}

}
\author{
Sven Kotlarski (MeteoSwiss)
}
