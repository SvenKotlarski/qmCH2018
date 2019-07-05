#' @title Compute CDF for each day-of-year with moving window
#' 
#' @description \code{qm.cdf.doy} computes the cumulative distribution
#' function (CDF) for each day-of-year (DOY) with a moving window
#' (+/- half window - 0.5). In case of precipitation also the wet day
#' frequency and wet day CDF are computed. The 29th February is considered
#' for all DOY ranges that include 1st March (DOY=60).
#'
#' @param series Time series of modelled / observed data.
#' @param yearbegin First year of series.
#' @param yearend Last year of series.
#' @param doywindow Total width of moving window [days].
#' @param minq Minimum quantile [0..1].
#' @param maxq Maximum quantile [0..1].
#' @param incq Quantile increment (bin size).
#' @param var Variable name ('pr' for precip).
#' @param pr.wet Wet day threshold [mm/day].
#'
#' @return List of 3:
#' $cdf.matrix: CDF for each DOY (matrix[365,nquantiles]).
#' $f.wet: wet day frequency for each DOY (vector[365]); NA if not
#' precipitation.
#' $cdf.wet.vector: wet day CDF for each DOY
#' (matrix[365,101]; all percentiles); NA if not precipitation.

#' @author Sven Kotlarski (MeteoSwiss), Jan Rajczak (ETH Zurich)
#'
#' @export
#'
#' @examples \dontrun{
#' # Compute DOY-dependent CDF for a daily series ranging from 1981 to 2010 and
#' # for min and max quantile of 0.01 and 0.99, respectively. Quantile
#' # increment: 0.01. Variable: Precipitation. Wet day threshold: 0.1 mm/day.
#' qm.cdf.doy(time.series,1981,2010,91,0.01,0.99,0.01,'pr',0.1)
#' }
qm.cdf.doy <- function(series, yearbegin, yearend, doywindow, minq, maxq, incq, var, pr.wet){

        d		<- qm.doywindowgen(doywindow)
        cdf_doy		<- array(NA,dim=c(365,length(seq(minq,maxq,by=incq))))
        doy.series	<- qm.doystring(yearbegin,yearend)
        fwet_doy	<- rep(NA,365)
        cdfwet_doy	<- array(NA,dim=c(365,101))

        # *** set 29th February to 1st March ***
        doy.series[doy.series > 500] <- 60

        # *** check of input data: length OK? ***
        if(length(series) != length(doy.series)){
                print("ERROR: data series and doy series not equally long")
                cdf_doy <- NULL
        }else{
                for(i in 1:365){
                        #			*** match: returns vector of length(doy.series): Position of matches in d$doy.matrix[i,] or NA if no match
                        #			*** => all positions in doy.series that correspond to the window defined in d$doy.matrix[i,] are marked
                        cdf_doy[i,] <- quantile(series[which(is.na(match(doy.series,d$doy.matrix[i,])) == FALSE)], probs=seq(minq,maxq,by=incq),na.rm=TRUE)

                        #			*** possible (shorter) alternative ***
                        #			cdf_doy[i,] <- quantile(series[which(!is.na(match(doy.series,d$doy.matrix[i,])))],probs=c(seq(minq,maxq,by=incq)),na.rm=TRUE)

                        #			*** for precipitation additionally compute wet day frequency and wet day distribution for each DOY ***
                        if (var == 'pr') {
                                # *** all values for current DOY including window ***
                                series.doywin <- series[which(is.na(match(doy.series,d$doy.matrix[i,])) == FALSE)]
                                # *** total number of days w/o NA ***
                                ndays <- length(series.doywin[!is.na(series.doywin)])
                                # *** number of wet days w/o NA ***
                                ndays.wet <- length(which(series.doywin >= pr.wet))
                                # *** wet day fraction for current DOY ***
                                fwet_doy[i] <- ndays.wet / ndays
                                # *** all values for current DOY including window but excluding dry days ***
                                series.doywin.wet <- series.doywin[series.doywin >= pr.wet]
                                # *** wet day CDF for current DOY including window ***
                                cdfwet_doy[i,] <- quantile(series.doywin.wet,probs=seq(0,1,0.01),na.rm=TRUE)
                        }
                }
        }
        return(list(cdf.matrix = cdf_doy, f.wet = fwet_doy, cdf.wet = cdfwet_doy))
}
