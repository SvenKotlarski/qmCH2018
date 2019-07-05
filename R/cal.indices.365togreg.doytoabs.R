#' @title Compute absolute indices for 365-days to Gregorian calendar
#' conversion
#'
#' @description \code{cal.indices.365togreg.doytoabs} determines absolute
#' indices in a full Gregorian calendar (for a given period) at which to
#' introduce NA days corresponding to day-of-year (DOY) indices for individual
#' years with a 365-days calendar. Basis: Vector providing the NA DOY indices
#' for each individual year.
#'
#' @param infile Rdata-file contianing one matrix with DOY positions for each
#' year to fill in NAs (years = rows 1:2200, days = columns, 1:6); output of
#' function \code{\link{cal.indices.365togreg}}.
#' @param yearstart Start year of the desired period.
#' @param yearend End year of the desired period.
#' @param outfile If provided, the resulting vector will be written into this
#' RData file (optional).
#'
#' @return Vector (positions to fill in NAs): absolute indices for the period
#' (in Gregorian calendar) at which to fill NA days.

#' @author Sven Kotlarski, Iris Feigenwinter (MeteoSwiss)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Determine absolute indices to fill in NAs for the period
#' # 1981 to 2010. Matrix with indices for individual years
#' # provided by infile. No outfile written.
#' cal.indices.365togreg.doytoabs(infile,1981,2010)
#' }

cal.indices.365togreg.doytoabs <- function(infile,yearstart,yearend,outfile=NA) {

	# *** get old doy positions ***
	indices <- get(load(infile))

	# *** select time period, plus 1 to place NAs AFTER these indices ***
	indices <- indices[yearstart:yearend]+1

	# *** get only the positions of leap years (Not NAs) ***
	tt=which(!is.na(indices)) # pick only years with a value, the others are set to NA (regular years, no additional day to fill in)

	# *** define vector to write new indices, only gives the non NA positions ***
	absind <- rep(NA,length(tt))

	# *** loop over the doy positions ***
	for (pos in 1:length(tt)) {

		# *** years after yearstart is indicated by the positions of the none NA indices ***
		nyears=tt[pos]-1 # years that have passed since the yearstart to the beginning of current year

		# *** for the first year the indices stay the same ***
		if (nyears==0) absind[pos]=indices[tt[pos]] else {

			# *** for the other years calculate how many days the offset is to startyear, minus 1 to take the year before ***
			time=seq(ISOdate(yearstart,1,1),ISOdate(yearstart+nyears-1,12,31),'days')

			# *** add offset to doy position ***
			absind[pos]=indices[tt[pos]]+length(time)
		}

 	# *** end loop over doy positions ***
	}

	# *** rename ***
	indices <- absind
	if (!is.na(outfile)) { save(indices,file=outfile) }

	return(indices)

# *** end of function ***
}
