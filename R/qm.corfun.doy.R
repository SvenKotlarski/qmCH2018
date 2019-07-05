#' @title Compute QM correction function for each day-of-year
#'
#' @description \code{qm.corfun.doy} computes the additive QM correction
#' function for each quantile and for each day-of-year (DOY); basically
#' CDF mod [doy] - CDF obs [doy].
#'
#' @param cdf.mod Modelled CDF for each DOY (matrix[365,nquantiles])
#' @param cdf.obs Observed CDF for each DOY (matrix[365,nquantiles])
#'
#' @return List of 3:
#' $correction: An additive correction value for each quantile and each DOY
#' (matrix[365,nquantiles])
#' $cdfmod: The original modelled CDF matrix.
#' $cdfobs: The original observed CDF matrix.

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Compute QM correction function based on modelled and observed CDFs
#' # for each DOY
#' qm.corfun.doy(cdf.mod,cdf.obs)
#' }

qm.corfun.doy <- function(cdf.mod, cdf.obs){

        # *** Check for same dimension of input CDFs ***
        # *** (could be implemented more elegant...) ***
        if((length(cdf.mod[1,]) != length(cdf.obs[1,])) & (length(cdf.mod[,1]) != length(cdf.obs[,1]))){
                print("ERROR: doy-based CDF of model and observations do not have the same dimension")
                dif.cdf	<- NULL
        }else{
                dif.cdf	<- cdf.mod-cdf.obs
        }
        return(list(correction = dif.cdf, cdfmod = cdf.mod, cdfobs = cdf.obs))
}
