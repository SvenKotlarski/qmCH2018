#' @title Construct matrix of all DOYs to be considered for a moving
#' window application
#'
#' @description \code{qm.doywindowgen} constructs a matrix [365,window.width]
#' that indicates for each day-of-year (DOY) the DOYs to be considered in
#' moving window apoplications.
#'
#' @param window.width Length of moving window (number of days). Should be
#' an uneven number.
#'
#' @return List of 3:
#' $half.width: Half window width minus 0.5.
#' $window.width: Total width of moving window.
#' $doymatrix: Matrix indicating the DOYs to account for depending on
#' the actual centered DOY (matrix[365,window.width]).

#' @author Sven Kotlarski (MeteoSwiss), Jan Rajczak (ETH Zurich)
#'
#' @export
#'
#' @examples \dontrun{
#' # Construct DOY-matrix for 91-day moving window
#' qm.doywindowgen(91)
#' }

qm.doywindowgen	<- function(window.width){

        doys 		<- c(1:365)
        ndays 		<- length(doys)
        half.width	<- (window.width/2) - 0.5

        doy_ix <- array(data=NA,dim=c(ndays,window.width))
        temp.vec <- rep(1:ndays,3)

        for (day in doys) { doy_ix[day,] <- temp.vec[((ndays+day)-half.width):((ndays+day)+half.width)] }

        return(list(half.width = half.width,window.width = window.width, doy.matrix = doy_ix))
}
