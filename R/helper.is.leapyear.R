#' @title Check if a given year is a leap year
#'
#' @description Helper function: Check if a given year is a leap year.
#'
#' @param year Year to check.

#' @return Logical. TRUE/1: year is leap year. FALSE/0: year
#' is not a leap year.

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Check if year 2000 is a leap year
#' helper.is.leapyear(2000)
#' }

helper.is.leapyear <- function(year){
        return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}
