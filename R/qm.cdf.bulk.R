#' @title Compute CDF for bulk series
#'
#' @description \code{qm.cdf.bulk} computes a cumulative distribution
#' function (CDF) for bulk series (no day-of-year dependence). In case
#' of precipitation the function also computes the wet day frequency
#' and the wet day CDF.
#'
#' @param series Time series of modelled / observed data.
#' @param minq Minimum quantile [0..1].
#' @param maxq Maximum quantile [0..1].
#' @param incq Quantile increment (bin size).
#' @param var Variable name ("pr" for precip).
#' @param pr.wet Wet day threshold [mm/day].
#'
#' @return List of 3:
#' $cdf.vector: CDF of bulk series (vector[nquantiles]).
#' $f.wet: wet day frequency of bulk series; NA if not
#' precipitation.
#' $cdf.wet.vector: wet day CDF for bulk series
#' (vector[101]; all percentiles); NA if not precipitation.

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Compute bulk CDF for min and max quantile of 0.01 and 0.99,
#' # respectively. Quantile increment: 0.01. Variable: Precipitation.
#' # Wet day threshold: 0.1 mm/day.
#' qm.cdf.bulk(time.series,0.01,0.99,0.01,'pr',0.1)
#' }
#'

qm.cdf.bulk <- function(series, minq, maxq, incq, var, pr.wet){

        cdf		<- rep(NA,length(seq(minq,maxq,by=incq)))
        fwet    	<- NA
        cdfwet	        <- rep(NA,101)

        # *** compute CDF ***
        cdf <- quantile(series, probs=seq(minq,maxq,by=incq),na.rm=TRUE)

        # *** compute wet day CDF for precip ***
        if (var == 'pr') {
                # *** total number of days w/o NA ***
                ndays <- length(series[!is.na(series)])
                # *** number of wet days w/o NA ***
                ndays.wet <- length(which(series >= pr.wet))
                # *** wet day fraction for bulk series ***
                fwet <- ndays.wet / ndays
                # *** all values excluding dry days ***
                series.wet <- series[series >= pr.wet]
                # *** wet day CDF ***
                cdfwet <- quantile(series.wet,probs=seq(0,1,0.01),na.rm=TRUE)
        }

        return(list(cdf.vector = cdf, f.wet = fwet, cdf.wet.vector = cdfwet))
}
