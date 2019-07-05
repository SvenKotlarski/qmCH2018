#' @title Annual means
#'
#' \code{timstats.yearmean_fullday(series,yearbegin,yearend)}
#' computes a time series of annual means based on a daily time
#' series covering full years.
#'
#' @param series Time series (vector) at daily resolution and for full years
#' (full Gregorian calendar).
#' @param yearbegin First year of time series.
#' @param yearend Last year of time series.
#'
#' @return Vector of annual means

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Compute annual means for a daily time series covering
#' # the years 2000 to 2005
#' timstats.yearmean_fullday(series,2000,2005)
#' }
#'

timstats.yearmean_fullday <- function(series,yearbegin,yearend) {

        yearseries	<- integer()

        for (countyear in yearbegin:yearend) {
                if (helper.is.leapyear(countyear)) yearseries <- c(yearseries,rep(countyear,366)) else yearseries <- c(yearseries,rep(countyear,365))
        }

        if(length(yearseries) != length(series)) {
                print('ERROR in yearmean_fullday: Input data series does not represent complete years!')
                return()
        }

        series.out.temp <- aggregate(series~yearseries,FUN=mean,na.rm=TRUE)
        series.out <- series.out.temp[[2]]
        names(series.out) <- series.out.temp[[1]]

        return(series.out)
}
