#' @title Multi-year monthly means
#'
#' @description \code{timstats.ymonmean_fullday(series,yearbegin,yearend)}
#' computes multi-year monthly means (mean annual cycle at monthly resolution)
#' based on a daily time series covering full years.
#'
#' @param series Time series (vector) at daily resolution and for full years
#' (full Gregorian calendar).
#' @param yearbegin First year of time series.
#' @param yearend Last year of time series.
#'
#' @return Vector (12) of multi-year monthly means

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Compute multi year monthly means for a daily time series
#' # covering the years 2000 to 2005
#' timstats.ymonmean_fullday(series,2000,2005)
#' }
#'

timstats.ymonmean_fullday <- function(series,yearbegin,yearend) {

        moyreg 	<- c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
        moyleap 	<- c(rep(1,31),rep(2,29),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
        moyseries   <- integer()

        months <- c('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC')

        for (countyear in yearbegin:yearend) {
                if (helper.is.leapyear(countyear)) moyseries <- c(moyseries,moyleap) else moyseries <- c(moyseries,moyreg)
        }

        if(length(moyseries) != length(series)) {
                print('ERROR in ymonmean_fullday: Input data series does not represent complete years!')
                return()
        }

        series.out.temp <- aggregate(series~moyseries,FUN=mean,na.rm=TRUE)
        series.out <- series.out.temp[[2]]
        names(series.out) <- months[series.out.temp[[1]]]

        return(series.out)
}

