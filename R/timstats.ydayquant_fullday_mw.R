#' @title Multi-year daily quantiles with moving window
#'
#' \code{timstats.ydayquant_fullday_mw(series,yearbegin,yearend,quant,winsize)}
#' computes multi-year daily quantiles employing a moving window around
#' the center day and based on a daily time series covering full
#' years. Quantiles are computed by taking into account all daily
#' values within the moving window (no simple averaging of daily quantiles).
#'
#' @param series Time series (vector) at daily resolution and for full years
#' (full Gregorian calendar).
#' @param yearbegin First year of time series.
#' @param yearend Last year of time series.
#' @param quant Quantile to compute [0,1].
#' @param winsize Moving window length (uneven number of days).
#'
#' @return Vector (366) of multi-year daily quantiles. 29th February is NA.

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Compute multi year daily 95th quantile employng a moving window of
#' # 91 days for a daily time series covering the years 2000 to 2005
#' timstats.ydayquant_fullday_mw(series,2000,2005,0.95,91)
#' }
#'

timstats.ydayquant_fullday_mw <- function(series,yearbegin,yearend,quant,winsize) {

        if ((winsize%%2)==0) {
                print('ERROR in ydayquant_fullday_mw: Moving window size is not an uneven number!')
                return()
        }

        if (winsize>365) {
                print('ERROR in ydayquant_fullday_mw: Moving window size is larger than 365 days!')
                return()
        }

        doyreg 	<- c(1:365)
        doyleap	<- c(1:59,999,60:365)
        doyseries   <- integer()

        for (countyear in yearbegin:yearend) {
                if (helper.is.leapyear(countyear)) doyseries <- c(doyseries,doyleap) else doyseries <- c(doyseries,doyreg)
        }

        if(length(doyseries) != length(series)) {
                print('ERROR in ydayquant_fullday: Input data series does not represent complete years!')
                return()
        }

        # *** construct DOY matrix ***
        width		<- (winsize/2)-0.5
        doys 		<- c(1:365)
        ndays 	<- length(doys)
        doy.matrix <- array(data=NA,dim=c(ndays,winsize))
        temp.vec <- rep(1:ndays,3)
        for (day in doys) { doy.matrix[day,] <- temp.vec[((ndays+day)-width):((ndays+day)+width)] }

        # *** set 29th February to 1st March ***
        doyseries[doyseries > 500] <- 60

        # *** compute quantile over moving window ***
        p.out	<- rep(NA,365)

        for(i in 1:365){
                p.out[i] <- quantile(series[which(!is.na(match(doyseries,doy.matrix[i,])))],probs=quant,na.rm=TRUE)
        }

        # *** 29th February to NA ***
        p.out <- append(p.out,NA,59)
        names(p.out) <- NULL

        return(p.out)
}
