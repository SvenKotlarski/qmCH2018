#' Convert gridded 365-days field to Gregorian calendar
#'
#' \code{cal.convert.365togreg} converts a timeseries of
#' a 365-days calendar (complete years) to Gregorian calendar by
#' filling in NAs.
#'
#' @param org.series Original time series (complete years only!).
#' @param yearstart First year of original 365-day time series.
#' @param yearend Last year of original 365-day time series.
#' @param fill.vector Vector indicating after which days in a
#' given year NA days should introduced; output of function
#' \code{\link{cal.indices.365togreg()}}. If
#' \code{data.type}=1: filename of RData file containing the
#' fill vector only.
#' @param data.type If 0: fill vector is provided as an
#' R object  (default). If 1: fill vector is provided in an
#' RData file containing the fill vector only
#' (filename is provided by \code{fill.matrix}).
#'
#' @return Vector (365/366 x nyears): original time series but in
#' Gregorian calendar (original series filled with NAs).

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # convert daily series of a 365-days calendar for the years
#' # 1980 to 2009 to a Gregorian calendar. NA indices provided as vector
#' # 'indices'
#' cal.convert.365togreg.grid(series,1980,2009,indices,0)
#' }

cal.convert.365togreg <- function(org.series,yearstart,yearend,fill.vector,data.type=0){

        # *** check if data.type <= 1 ***
        if (data.type > 1) {
                print(paste('ERROR in cal.convert.365togreg(): data.type is ', data.type,' but has to be either 0 or 1.',sep=''))
                return()
        }

        if(data.type==1) {
                indices <- get(load(fill.vector))
        }else{
                indices <- fill.vector
        }

        nyears <- (yearend - yearstart) + 1
        ndays <- 365

        # *** check if yearstart <= yearend ***
        if (yearstart > yearend) {
                print(paste('ERROR in cal.convert.365togreg(): Start year (', yearstart,') larger than end year (',yearend,').',sep=''))
                return()
        }

        # *** check if index vector is really a vector ***
        if (!is.vector(indices)) {
                print('ERROR in cal.convert.360togreg(): index vector is not a vector.')
                return()
        }

        # *** check if index vector contains all required years ***
        if (yearend > length(indices)) {
                print(paste('ERROR in cal.convert.365togreg(): Maximum year in index matrix (', length(indices),') is smaller than end year (',yearend,').',sep=''))
                return()
        }

        # *** check if orginal time series is complete ***
        if (length(org.series) != (nyears*ndays)) {
                print(paste('ERROR in cal.convert.365togreg(): Length of original timeseries (', length(org.series),') does not represent complete years (should be ',nyears*360,' according to start end end year.',sep=''))
                return()
        }

        # *** loop over years: fill in NAs ***
        res.series <- NULL
        for (countyear in yearstart:yearend) {
                ind.start <- ((countyear-yearstart)*ndays)+1
                ind.stop <- ((countyear-yearstart)+1)*ndays
                temp.series <- org.series[ind.start:ind.stop]

                # *** append NA after index in case that index is not NA ***
                index.year <- indices[countyear]
                if (!is.na(index.year)) temp.series <- append(temp.series,NA,index.year)

                # *** merge updated series for current year to result series ***
                res.series <- c(res.series,temp.series)

        # *** end loop over years ***
        }

        return(res.series)

# *** end of function ***
}
