#' @title Construct daily vector of day-of-year for a given period
#'
#' @description \code{qm.doystring} constructs a daily vector with the
#' length of the entire period (defined by first and last year) specifying
#' the day-of-year (DOY; 1:365) of each day. A leap day (Feb 29) is given
#' the value 999.
#'
#' @param yearbegin First year of time series.
#' @param yearend Last year of time series.
#'
#' @return Vector: Day-of-year time series.

#' @author Jan Rajczak (ETH Zurich), Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Construct DOY vector for the period 1981 to 2010
#' qm.doystring(1981,2010)
#' }

qm.doystring <- function(yearbegin,yearend){

        doyreg 		<- c(1:365)
        doyleap		<- c(1:59,999,60:365)
        doyscenario     <- integer()

        for (countyear in yearbegin:yearend) {
                if (helper.is.leapyear(countyear)) doyscenario <- c(doyscenario,doyleap) else doyscenario <- c(doyscenario,doyreg)
        }
        return(doyscenario)
}
