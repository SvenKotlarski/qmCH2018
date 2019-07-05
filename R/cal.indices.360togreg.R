#' @title Compute indices for 360-days to Gregorian calendar conversion
#'
#' @description \code{cal.indices.360togreg} determines the day-of-year (DOY)
#' indices for each individual year of a 360-days calendar (for a given
#' period) at which to introduce NAs in order to transfer series to a full
#' Gregorian calendar.
#'
#' @param yearend Last year to consider (implicitly, first year is year 1).
#' @param outfile If provided, the resulting matrix will be written into
#' this RData file (optional).
#'
#' @return Matrix (1:yearend,6): indices (columns) for a given 360-days
#' year (row) after which to append NA days.

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate DOY indices after which to fill in NAs for all years
#' of the period 1 to 2200. No outfile written.
#' cal.indices.360togreg(2200)
#' }
#'

cal.indices.360togreg <- function(yearend, outfile=NA){

	indices <- matrix(NA,nrow=yearend,ncol=6)

	for (countyear in 1:yearend){
		if (helper.is.leapyear(countyear)) truncs<-rbind(c(0,60),c(61,120),c(121,180),c(181,240),c(241,300),c(301,360)) else truncs<-rbind(c(0,72),c(73,144),c(145,216),c(217,288),c(289,360))
		for (countrow in 1:nrow(truncs)){
			indices[countyear,countrow] <- sample(truncs[countrow,1]:truncs[countrow,2],1)
		}
	}

	if (!is.na(outfile)) { save(indices,file=outfile) }

	return(indices)

# *** end of function ***
}
