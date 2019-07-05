#' @title Sort a matrix by a column
#'
#' @description Helper function: Sort a matrix by a certain column
#' (increasing values).
#'
#' @param mat Matrix to be sorted.
#' @param n Index of column that defines sorting (increasing values).

#' @return Sorted matrix.

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Sort matrix mat by 2nd column
#' helper.mat.sort(mat,2)
#' }

helper.mat.sort <- function(mat,n) {
        mat[rank(mat[,n]),] <- mat[c(1:nrow(mat)),]
        return(mat)
}
