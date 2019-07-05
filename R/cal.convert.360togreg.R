#' @title Convert gridded 360-days field to Gregorian calendar
#'
#' @description \code{cal.convert.360togreg} converts a timeseries of
#' a 360-days calendar (complete years) to Gregorian calendar by
#' filling in NAs.
#'
#' @param org.series Original time series (complete years only!).
#' @param yearstart First year of original 360-day time series.
#' @param yearend Last year of original 360-day time series.
#' @param fill.matrix Matrix indicating after which days in a
#' given year NA days should introduced; output of function
#' \code{\link{cal.indices.360togreg()}}. If
#' \code{data.type}=1: filename of RData file containing the
#' fill matrix only.
#' @param data.type If 0: fill matrix is provided as an
#' R object (default). If 1: fill matrix is provided in
#' an RData file containing the fill matrix only
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
#' # convert daily series of a 360-days calendar for the years
#' # 1980 to 2009 to a Gregorian calendar. NA indices provided as matrix
#' #' indices'
#' cal.convert.360togreg.grid(series,1980,2009,indices,0)
#' }

cal.convert.360togreg <- function(org.series,yearstart,yearend,fill.matrix,data.type=0){

        # *** check if data.type <= 1 ***
        if (data.type > 1) {
                print(paste('ERROR in cal.convert.360togreg(): data.type is ', data.type,' but has to be either 0 or 1.',sep=''))
                return()
        }

        if(data.type==1) {
                indices <- get(load(fill.matrix))
        }else{
                indices <- fill.matrix
        }

        nyears <- (yearend - yearstart) + 1
        ndays <- 360

        # *** check if yearstart <= yearend ***
        if (yearstart > yearend) {
                print(paste('ERROR in cal.convert.360togreg(): Start year (', yearstart,') larger than end year (',yearend,').',sep=''))
                return()
        }

        # *** check if index matrix is really a matrix ***
        if (!is.matrix(indices)) {
                print('ERROR in cal.convert.360togreg(): index matrix is not a matrix.')
                return()
        }

        # *** check if index matrix contains all required years ***
        if (yearend > dim(indices)[1]) {
                print(paste('ERROR in cal.convert.360togreg(): Maximum year in index matrix (', dim(indices)[1],') is smaller than end year (',yearend,').',sep=''))
                return()
        }

        # *** check if orginal time series is complete ***
        if (length(org.series) != (nyears*ndays)) {
                print(paste('ERROR in cal.convert.360togreg(): Length of original timeseries (', length(org.series),') does not represent complete years (should be ',nyears*360,' according to start end end year.',sep=''))
                return()
        }

        # *** loop over years: fill in NAs ***
        res.series <- NULL
        for (countyear in yearstart:yearend) {
                ind.start <- ((countyear-yearstart)*ndays)+1
                ind.stop <- ((countyear-yearstart)+1)*ndays
                temp.series <- org.series[ind.start:ind.stop]

                # *** loop over all NA indices in current year ***
                indices.year <- indices[countyear,]
                for (countindex in 1:length(indices.year[!is.na(indices.year)])){
                        # *** append NA after each index (increase index by one after each append operation) ***
                        temp.series <- append(temp.series,NA,indices.year[countindex]+(countindex-1))
                }

                # *** merge updated series for current year to result series ***
                res.series <- c(res.series,temp.series)

        # *** end loop over years ***
        }

        return(res.series)

# *** end of function ***
}
