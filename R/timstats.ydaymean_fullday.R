#' @title Multi-year daily means
#'
#' @description \code{timstats.ydaymean_fullday(series,yearbegin,yearend)}
#' computes multi-year daily means (mean annual cycle at daily resolution)
#' based on a daily time series covering full years.
#'
#' @param series Time series (vector) at daily resolution and for full years
#' (full Gregorian calendar).
#' @param yearbegin First year of time series.
#' @param yearend Last year of time series.
#'
#' @return Vector (366) of multi-year daily means.

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Compute multi year daily means for a daily time series
#' # covering the years 2000 to 2005
#' timstats.ydaymean_fullday(series,2000,2005)
#' }
#'

timstats.ydaymean_fullday <- function(series,yearbegin,yearend) {

        doyreg 	<- c(1:59,61:366)
        doyleap	<- c(1:366)
        doyseries   <- integer()

        for (countyear in yearbegin:yearend) {
                if (helper.is.leapyear(countyear)) doyseries <- c(doyseries,doyleap) else doyseries <- c(doyseries,doyreg)
        }

        if(length(doyseries) != length(series)) {
                print('ERROR in ydaymean_fullday: Input data series does not represent complete years!')
                return()
        }

        series.out.temp <- aggregate(series~doyseries,FUN=mean,na.rm=TRUE)
        series.out <- series.out.temp[[2]]
        names(series.out) <- series.out.temp[[1]]

        # *** if only one non-leap year: append 29th February ***
        if (length(doyseries)==365) {
                names.temp <- names(series.out)
                series.out <- append(series.out,NA,59)
                names(series.out) <- append(names.temp,60,59)
        }

        return(series.out)
}
