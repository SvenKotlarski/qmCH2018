#' @title The actual QM function for the bulk series without
#' frequency adaptation (TAKE CARE: NOT YET FULLY TESTED!!)
#'
#' @description \code{qm.empqm.bulk} is the actual QM function for the bulk
#' series. Equivalent to function \link{qm.empqm.doy} but for the bulk series,
#' i.e. disregarding a day-of-year (DOY) dependence of the correction function,
#' and without frequency adaptation.
#'
#' @param series Time series of modelled data, e.g. transient (control+)
#' scenario series.
#' @param cdf.mod.bulk CDF of the modelled time series in the calibration
#' period, estimated with function \link{qm.cdf.bulk} (vector[nquantiles]).
#' @param corr.fun.bulk Additive correction function (cdf A - cdf B), obtained by
#' function \link{qm.corfun.bulk}.
#' @param method QM method: "binary" -> correction for closest quantile is
#' used, "linear" -> linear interpolation of correction function between lower
#' and upper quantile.
#' @param var Variable to correct.
#' @param output.diag Diagnostic QM console output (TRUE) or not (FALSE);
#' default: FALSE.
#' @param minq Minimum quantile for correction function [0.01 .. 0.99];
#' note: should not be 0 (for correct handling of extremes)!
#' @param maxq Maximum quantile for correction function [0.01 .. 0.99];
#' note: should not be 1 (for correct handling of extremes)!
#' @param incq Quantile increment for correction function (bin size).
#'
#' @return List of 3:
#' $qm.input.series: Input series to QM (input argument series).
#' $qm.corrected.series: Corrected series.
#' $quantile.index: A series of quantile indices wrt. simulated series in
#' calibration period.

#' @author Sven Kotlarski (MeteoSwiss), Jan Rajczak (ETH Zurich)
#'
#' @export
#'
#' @examples \dontrun{
#' # Carry out bulk QM without frequency adaptation. Standard quantiles.
#' # Variable: precipitation. Linear QM, diagnostic terminal output.
#' qm.empqm.bulk(series, cdf.modelled.bulk, correction.function.bulk, 'linear', 'pr', TRUE, 0.01, 0.00, 0.01)
#' }

qm.empqm.bulk <- function(series, cdf.mod.bulk, cor.fun.bulk, method=c("linear","bin"), var, output.diag=FALSE, minq, maxq, incq){

        if(output.diag==TRUE){
                print("")
                print("QM WITHOUT frequency adjustment (bulk) ...")
                print("")
        }

        series_q	<- rep(NA,length(series))
        series_c	<- rep(NA,length(series))

        for(i in 1:length(series)){
                if(is.na(series[i]) == "TRUE"){
                        series_q[i] <- NA
                        series_c[i] <- NA
                }else{

                        # using quantile-"bins"
                        if(match.arg(method) == "bin"){
                                # *** Note: this command implicitly selects the first (last) quantile considered for new extremes , i.e.  ***
                                # *** for smaller (larger) values than those occuring in the calbration period (in case of zeros: first   ***
                                # *** occurence). If minq=0.01 and maxq=0.99 nex extremes are hence corrected according to the correction ***
                                # *** function for the 1st and 99th quantile.                                                             ***
                                ix		<- which.min(abs(cdf.mod.bulk-series[i]))
                                # *** Compute quantile index  ***
                                series_q[i]	<- minq + (ix-1)*incq
                                # *** Compute corrected value ***
                                series_c[i]	<- series[i]-cor.fun.bulk[ix]
                        }

                        # using linear interpolation between quantiles
                        if(match.arg(method) == "linear"){
                                # *** number of interpolated sampling points ***
                                npoints <- 1000
                                cor		<- approx(cdf.mod.bulk,n=npoints,method="linear")$y
                                # *** Note: this command implicitly selects the first (last) quantile considered for new extremes , i.e.  ***
                                # *** for smaller (larger) values than those occuring in the calbration period (in case of zeros: first   ***
                                # *** occurence). If minq=0.01 and maxq=0.99 nex extremes are hence corrected according to the correction ***
                                # *** function for the 1st and 99th quantile.                                                             ***
                                ix		<- which.min(abs(cor-series[i]))	# closest value
                                # *** Compute quantile index  ***
                                series_q[i]	<- minq + (ix-1) * ((maxq-minq)/(npoints-1))
                                # *** Compute correction function ***
                                cf		<- approx(cor.fun.bulk,n=npoints,method="linear")$y
                                # *** Compute corrected value ***
                                series_c[i]	<- series[i]-cf[ix]
                        }

                }
        }

        # *** for huss (specific humidity), sfcWind (wind speed), hurs (relative humidity), rsds (global radiation): set negative values to 0 ***
        if ((var == 'huss') || (var == 'sfcWind') || (var == 'hurs') || (var == 'rsds')) series_c[which(series_c<0)] <- 0

        return(list(qm.input.series = series, qm.corrected.series = series_c, quantile.index  = series_q))

}
