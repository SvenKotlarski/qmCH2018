#' @title Seasonal means
#'
#' \code{timstats.seasmean_fullday(series,yearbegin,yearend)} computes time series
#' of seasonal means (mean annual cycle at seasonal resolution; last
#' December value is skipped) based on a daily time series covering full
#' years.
#'
#' @param series Time series (vector) at daily resolution and for full years
#' (full Gregorian calendar).
#' @param yearbegin First year of time series.
#' @param yearend Last year of time series.
#'
#' @return List with 4 elements (one for each season) containing vectors of
#' seasonal means. Last DJF mean (December only) is skipped.

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Compute seasonal means for a daily time series covering
#' # the years 2000 to 2005
#' timstats.seasmean_fullday(series,2000,2005)
#' }
#'

timstats.seasmean_fullday <- function(series,yearbegin,yearend) {

        soyreg 		<- c(rep(1,31),rep(1,28),rep(2,31),rep(2,30),rep(2,31),rep(3,30),rep(3,31),rep(3,31),rep(4,30),rep(4,31),rep(4,30),rep(1,31))
        soyleap 	<- c(rep(1,31),rep(1,29),rep(2,31),rep(2,30),rep(2,31),rep(3,30),rep(3,31),rep(3,31),rep(4,30),rep(4,31),rep(4,30),rep(1,31))
        soyseries   	<- integer()
        ndays.JANtoNOV.reg  <- 334
        ndays.JANtoNOV.leap <- 335

        for (countyear in yearbegin:yearend) {

                if (helper.is.leapyear(countyear)) ndays.JANtoNOV <- ndays.JANtoNOV.leap else ndays.JANtoNOV <- ndays.JANtoNOV.reg
                if (helper.is.leapyear(countyear)) {
                        soyseries <- c(soyseries,(countyear*100)+soyleap[1:ndays.JANtoNOV])
                        soyseries <- c(soyseries,((countyear+1)*100)+soyleap[(ndays.JANtoNOV+1):366])
                } else {
                        soyseries <- c(soyseries,(countyear*100)+soyreg[1:ndays.JANtoNOV])
                        soyseries <- c(soyseries,((countyear+1)*100)+soyreg[(ndays.JANtoNOV+1):365])
                }
        }

        if(length(soyseries) != length(series)) {
                print('ERROR in yseasmean_fullday: Input data series does not represent complete years!')
                return()
        }

        series.out.temp <- aggregate(series~soyseries,FUN=mean,na.rm=TRUE)
        series.out <- series.out.temp[[2]]
        names(series.out) <- series.out.temp[[1]]

        DJFtemp <- series.out[substr(names(series.out),5,6) =='01']
        DJF <- DJFtemp[1:(length(DJFtemp)-1)]
        MAM <- series.out[substr(names(series.out),5,6) =='02']
        JJA <- series.out[substr(names(series.out),5,6) =='03']
        SON <- series.out[substr(names(series.out),5,6) =='04']

        names(DJF) <- substr(names(DJF),1,4)
        names(MAM) <- substr(names(MAM),1,4)
        names(JJA) <- substr(names(JJA),1,4)
        names(SON) <- substr(names(SON),1,4)

        list.out <- list(DJF=DJF,MAM=MAM,JJA=JJA,SON=SON)

        return(list.out)
}
