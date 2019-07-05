#' @title Multi-year seasonal means
#'
#' \code{timstats.yseasmean_fullday(series,yearbegin,yearend)}
#' computes multi-year seasonal means (mean annual cycle at seasonal resolution)
#' based on a daily time series covering full years.
#'
#' @param series Time series (vector) at daily resolution and for full years
#' (full Gregorian calendar).
#' @param yearbegin First year of time series.
#' @param yearend Last year of time series.
#'
#' @return Vector (4) of multi-year seasonal means

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Compute multi year seasonal means for a daily time series covering
#' # the years 2000 to 2005
#' timstats.yseasmean_fullday(series,2000,2005)
#' }
#'

timstats.yseasmean_fullday <- function(series,yearbegin,yearend) {

        moyreg 	<- c(rep(1,31),rep(1,28),rep(2,31),rep(2,30),rep(2,31),rep(3,30),rep(3,31),rep(3,31),rep(4,30),rep(4,31),rep(4,30),rep(1,31))
        moyleap 	<- c(rep(1,31),rep(1,29),rep(2,31),rep(2,30),rep(2,31),rep(3,30),rep(3,31),rep(3,31),rep(4,30),rep(4,31),rep(4,30),rep(1,31))
        moyseries   <- integer()

        seasons <- c('DJF','MAM','JJA','SON')

        for (countyear in yearbegin:yearend) {
                if (helper.is.leapyear(countyear)) moyseries <- c(moyseries,moyleap) else moyseries <- c(moyseries,moyreg)
        }

        if(length(moyseries) != length(series)) {
                print('ERROR in yseasmean_fullday: Input data series does not represent complete years!')
                return()
        }

        series.out.temp <- aggregate(series~moyseries,FUN=mean,na.rm=TRUE)
        series.out <- series.out.temp[[2]]
        names(series.out) <- seasons[series.out.temp[[1]]]

        return(series.out)
}
