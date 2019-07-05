#' @title Compute indices for 365-days to Gregorian calendar conversion
#'
#' @description \code{cal.indices.365togreg} determines the day-of-year (DOY)
#' indices for each individual year of a 365-days calendar (for a given period)
#' at which to introduce NAs in order to transfer series to a full Gregorian
#' calendar.
#'
#' @param yearend Last year to consider (implicitly, first year is year 1).
#' @param outfile If provided, the resulting vector will be written into
#' this RData file (optional).
#'
#' @return Vector (1:yearend): Index for each given 365-days
#' year after which to append an NA days (NA: no NA day to be
#' introduced in this year).

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate DOY indices after which to fill in NAs for all years
#' of the period 1 to 2200. No outfile written.
#' cal.indices.365togreg(2200)
#' }

cal.indices.365togreg <- function(yearend,outfile=NA){

	indices <- rep(NA,yearend)

	for (countyear in 1:yearend) if (helper.is.leapyear(countyear)) indices[countyear] <- sample(1:365,1)
	if (!is.na(outfile)) { save(indices,file=outfile) }
	return(indices)

# *** end of function ***
}

