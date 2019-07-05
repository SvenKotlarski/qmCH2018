#' @title Convert gridded 365-days field to Gregorian calendar
#'
#' @description \code{cal.convert.365togreg.grid} converts a 3D field
#' (nx * ny * time) of a 365-days calendar (complete years) to Gregorian
#' calendar by filling in NAs.
#'
#' @param org.series Original time series (complete years only!).
#' @param yearstart First year of original 365-day time series.
#' @param yearend Last year of original 365-day time series.
#' @param fill.vector Vector indicating at which positions NA days
#' should be introduced; output of function
#' \code{\link{cal.indices.365togreg.doytoabs()}}. If
#' \code{data.type}=1: filename of RData file containing the
#' fill vector.
#' @param data.type If 0: fill vector is provided as an
#' R object with name \code{fill.vector} (default). If 1: fill vector
#' is provided in an RData file containing the fill vector only
#' (filename is provided by \code{fill.vector}).
#'
#' @return 3D array (nx x ny x ndays): 	original time series but
#' in Gregorian calendar (original series filled with NAs).

#' @author Iris Feigenwinter, Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # convert gridded daily series of a 365-days calendar for the years
#' # 1980 to 2009 to a Gregorian calendar. NA indices provided as vector
#' # 'indices'
#' cal.convert.365togreg.grid(series,1980,2009,indices,0)
#' }

cal.convert.365togreg.grid <- function(org.series,yearstart,yearend,fill.vector,data.type=0){

        # *** check if data.type <= 1 ***
        if (data.type > 1) {
                print(paste('ERROR in cal.convert.365togreg.grid(): data.type is ', data.type,' but has to be either 0 or 1.',sep=''))
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
                print(paste('ERROR in cal.convert.365togreg.grid(): Start year (', yearstart,') larger than end year (',yearend,').',sep=''))
                return()
        }

        # *** check if index vector is really a vector ***
        if (!is.vector(indices)) {
                print('ERROR in cal.convert.365togreg.grid(): index vector is not a vector.')
                return()
        }

        # *** check if orginal time series is complete ***
        if (dim(org.series)[3] != (nyears*ndays)) {
                print(paste('ERROR in cal.convert.365togreg.grid(): Length of original timeseries (', dim(org.series)[3],') does not represent complete years (should be ',nyears*360,' according to start end end year.',sep=''))
                return()
        }

       	# *** new NA filling ***
	res.series <- NULL

	# *** create vector with NAs as long as the "real" (gregorian) time series and same dimension as the data series ***
	greg.length <- length(seq(ISOdate(yearstart,1,1),ISOdate(yearend,12,31),'days'))
	NAs <- array(NA,dim=c(nrow(org.series),ncol(org.series),greg.length))

	# *** fill with data so that the NAs remain at the positions (from indices) ***
	NAs[,,-indices] <- org.series[,,]
	res.series <- NAs

        return(res.series)

# *** end of function ***
}
