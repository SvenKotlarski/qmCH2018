#' @title Compute QM correction function for bulk series
#'
#' @description \code{qm.corfun.bulk} computes the additive QM correction
#' function for each quantile of the bulk series; basically CDF mod - CDF obs.
#'
#' @param cdf.mod Modelled CDF for bulk series (vector[nquantiles])
#' @param cdf.obs Observed CDF for bulk series (vector[nquantiles])
#'
#' @return List of 3:
#' $correction: An additive correction value for each quantile of the bulk
#' series (vector[nquantiles]).
#' $cdfmod: The original modelled CDF vector.
#' $cdfobs: The original observed CDF vector.

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Compute QM correction function based on bulk modelled and observed CDFs
#' qm.corfun.bulk(cdf.mod,cdf.obs)
#' }

qm.corfun.bulk <- function(cdf.mod.bulk, cdf.obs.bulk){

        dif.cdf	<- cdf.mod.bulk - cdf.obs.bulk

        return(list(correction = dif.cdf, cdfmod = cdf.mod.bulk, cdfobs = cdf.obs.bulk))
}
