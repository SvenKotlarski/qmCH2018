#' @title Multi-year daily quantiles
#'
#' \code{timstats.ydayquant_fullday(series,yearbegin,yearend,quant)}
#' computes multi-year daily quantiles based on a daily time series covering
#' full years
#'
#' @param series Time series (vector) at daily resolution and for full years
#' (full Gregorian calendar).
#' @param yearbegin First year of time series.
#' @param yearend Last year of time series.
#' @param quant Quantile to compute [0,1].
#'
#' @return Vector (366) of multi-year daily quantiles.

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Compute multi year daily 95th quantile for a
#' # daily time series covering the years 2000 to 2005
#' timstats.ydayquant_fullday(series,2000,2005,0.95)
#' }
#'
timstats.ydayquant_fullday <- function(series,yearbegin,yearend,quant) {

        doyreg 	<- c(1:59,61:366)
        doyleap	<- c(1:366)
        doyseries   <- integer()

        for (countyear in yearbegin:yearend) {
                if (helper.is.leapyear(countyear)) doyseries <- c(doyseries,doyleap) else doyseries <- c(doyseries,doyreg)
        }

        if(length(doyseries) != length(series)) {
                print('ERROR in ydayquant_fullday: Input data series does not represent complete years!')
                return()
        }

        series.out.temp <- aggregate(series~doyseries,FUN=quantile,probs=quant, na.rm=TRUE)
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
