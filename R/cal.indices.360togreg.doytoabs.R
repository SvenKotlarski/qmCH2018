#' @title Compute absolute indices for 360-days to Gregorian calendar
#' conversion
#'
#' @description \code{cal.indices.360togreg.doytoabs} determines absolute
#' indices in a full Gregorian calendar (for a given period) at which to
#' introduce NA days corresponding to day-of-year (DOY) indices for individual
#' years with a 360-days calendar. Basis: Vector providing the NA DOY indices
#' for each individual year.
#'
#' @param infile Rdata-file containing one matrix with DOY positions for each
#' year to fill in NAs (years = rows 1:2200, days = columns, 1:6); output of
#' function \code{\link{cal.indices.360togreg}}.
#' @param yearstart Start year of the desired period.
#' @param yearend End year of the desired period.
#' @param outfile If provided, the resulting vector will be written into this
#' RData file (optional).
#'
#' @return Vector (positions to fill in NAs): Absolute indices for the period
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
#' cal.indices.360togreg.doytoabs(infile,1981,2010)
#' }

cal.indices.360togreg.doytoabs <- function(infile,yearstart,yearend,outfile=NA) {

	# *** get old doy positions ***
	indices <- get(load(infile))

	# *** select time period, plus 1 to place NAs AFTER these indices ***
	indices <- indices[yearstart:yearend,]+1

	# **********************************************************************************************
	# *** add offset according to column to make it consistent with old 360togreg filling script ***
	# **********************************************************************************************
	indices[,2] <- indices[,2]+1
	indices[,3] <- indices[,3]+2
	indices[,4] <- indices[,4]+3
	indices[,5] <- indices[,5]+4
	indices[,6] <- indices[,6]+5
	# **************************

	absind=matrix(NA,ncol=ncol(indices),nrow=nrow(indices))

	# *** loop over the years/rows of the indices ***
	for (yy in yearstart:yearend) {

		# *** transfer years into position after yearstart ***
		pos <- yy-yearstart+1

		# *** for the first year the indices stay the same ***
		if (yy==yearstart) absind[pos,]=indices[pos,] else {

			# *** for the other years calculate the offset wrt. startyear ***
			time=seq(ISOdate(yearstart,1,1),ISOdate(yy-1,12,31),'days')

			# *** add offset to doy position ***
			absind[pos,]=indices[pos,]+length(time)
		}

	# *** end loop over years ***
	}

	# *** choose only indices which are not NAs (NAs in indices matrix indicate non leap years), to fill in NAs at these positions, sort by increasing indices ***
	vector <- sort(absind[!is.na(absind)])

	# *** rename ***
	indices <- vector
	if (!is.na(outfile)) { save(indices,file=outfile) }
	return(indices)

# *** end of function ***
}
