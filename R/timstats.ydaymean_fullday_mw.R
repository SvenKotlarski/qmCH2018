#' @title Multi-year daily means with moving window
#'
#' @description \code{timstats.ydaymean_fullday_mw(series,yearbegin,yearend,winsize)}
#' computes multi-year daily means (mean annual cycle at daily resolution) employing
#' a moving window around the center day and based on a daily time series covering full
#' years. The 29th February is discarded (NA in output series).
#'
#' @param series Time series (vector) at daily resolution and for full years
#' (full Gregorian calendar).
#' @param yearbegin First year of time series.
#' @param yearend Last year of time series.
#' @param winsize Moving window length (uneven number of days).
#'
#' @return Vector (366) of multi-year daily means. 29th February is NA.

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Compute multi year daily means for employing a window of 91 days for
#' # a daily time series covering the years 2000 to 2005
#' timstats.ydaymean_fullday_mw(series,2000,2005,91)
#' }
#'

timstats.ydaymean_fullday_mw <- function(series,yearbegin,yearend,winsize) {

        if ((winsize%%2)==0) {
                print('ERROR in ydaymean_fullday_mw: Moving window size is not an uneven number!')
                return()
        }

        if (winsize>365) {
                print('ERROR in ydaymean_fullday_mw: Moving window size is larger than 365 days!')
                return()
        }

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

        series.reg.temp <- aggregate(series~doyseries,FUN=mean,na.rm=TRUE)
        series.reg <- series.reg.temp[[2]]
        #      names(series.reg) <- series.reg.temp[[1]]
        names(series.reg) <- NULL


        # *** remove 29th February if input series contained leap year ***
        if (length(series.reg)==366) {
                series.reg <- c(series.reg[1:59],series.reg[61:366])
        }

        series.out <- rep(NA,365)
        winsize.half <- (winsize/2)-0.5

        for (countday in 1:365){

                index.start <- countday - winsize.half
                index.end <- countday + winsize.half

                if ((index.start>=1)&&(index.end<=365)) {
                        series.out[countday] <- mean(series.reg[index.start:index.end],na.rm=T)
                } else if ((index.start<=0)&&(index.end<=365)) {
                        series.out[countday] <- mean(c(series.reg[(365+index.start):365],series.reg[1:index.end]),na.rm=T)
                } else if ((index.start>=1)&&(index.end>365)) {
                        series.out[countday] <- mean(c(series.reg[index.start:365],series.reg[1:(index.end-365)]),na.rm=T)
                } else
                        series.out[countday] <- NA
        }

        series.out <- append(series.out,NA,59)
        return(series.out)
}
