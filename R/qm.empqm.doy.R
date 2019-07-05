#' @title Short The actual QM function considering the DOY without
#' frequency adaptation
#'
#' @description \code{qm.empqm.doy} is the actual QM function considering
#' the day-of-year (DOY) and not considering frequency adaptation for
#' precipitation.
#'
#' @details The function estimates the index (quantile) in which an observation
#' falls with respect to the calibration period. Based on this quantile (and
#' the DOY) a correction is applied, the final quantile map. For the 29th
#' February the same correction function as for 1st March (DOY=60) is applied.
#' Old and new extremes beyond the min and max percentile considered are
#' corrected according to the correction of the min and max percentile,
#' respectively (first and last quantile considered therefore have to be 0.01
#' and 0.99 -> check in function \link{qm.doqm}).
#'
#' @param series Time series of modelled data, e.g. transient (control+)
#' scenario series.
#' @param doy.series Corresponding series of DOYs (output of function
#' \link{qm.doystring}).
#' @param cdf.mod CDF of the modelled time series in the calibration period,
#' estimated with function qm.cdf.doy(...) (matrix[365,nquantiles]).
#' @param corr.fun Additive correction function (cdf A - cdf B), obtained by
#' function \link{qm.corfun.doy(...)}.
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
#'
#' @author Jan Rajczak (ETH Zurich), Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Carry out DOY-dependent QM without frequency adaptation. Standard
#' # quantiles. Variable: precipitation. Linear QM, diagnostic terminal output.
#' qm.empqm.doy(series, doy.series, cdf.modelled, correction.function, 'linear', 'pr', TRUE, 0.01, 0.00, 0.01)
#' }

qm.empqm.doy <- function(series, doy.series, cdf.mod, cor.fun, method=c("linear","bin"), var, output.diag=FALSE, minq, maxq, incq){

        if(output.diag==TRUE){
                print("")
                print("QM WITHOUT frequency adjustment (DOY) ...")
                print("")
        }

        if(length(series) != length(doy.series)){ # *** a little check
                print(paste("ERROR: length of time series (",length(series),") and DOY series (",length(doy.series),") do not match.", sep=""))
        }else{
                series_q	<- rep(NA,length(series))
                series_c	<- rep(NA,length(series))
                for(i in 1:length(series)){
                        doy <- doy.series[i]
                        if(doy == 999){ doy <- 60 }
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
                                        ix		<- which.min(abs(cdf.mod[doy,]-series[i]))
                                        # *** Compute quantile index  ***
                                        series_q[i]	<- minq + (ix-1)*incq
                                        # *** Compute corrected value ***
                                        series_c[i]	<- series[i]-cor.fun[doy,ix]
                                }

                                # using linear interpolation between quantiles
                                if(match.arg(method) == "linear"){
                                        # *** number of interpolated sampling points ***
                                        npoints <- 1000
                                        cor		<- approx(cdf.mod[doy,],n=npoints,method="linear")$y
                                        # *** Note: this command implicitly selects the first (last) quantile considered for new extremes , i.e.  ***
                                        # *** for smaller (larger) values than those occuring in the calbration period (in case of zeros: first   ***
                                        # *** occurence). If minq=0.01 and maxq=0.99 nex extremes are hence corrected according to the correction ***
                                        # *** function for the 1st and 99th quantile.                                                             ***
                                        ix		<- which.min(abs(cor-series[i]))
                                        # *** Compute quantile index  ***
                                        series_q[i]	<- minq + (ix-1) * ((maxq-minq)/(npoints-1))
                                        # *** Compute correction function ***
                                        cf		<- approx(cor.fun[doy,],n=npoints,method="linear")$y
                                        # *** Compute corrected value ***
                                        series_c[i]	<- series[i]-cf[ix]
                                }

                        }
                }

                # *** for huss (specific humidity), sfcWind (wind speed), hurs (relative humidity), rsds (global radiation): set negative values to 0 ***
                if ((var == 'huss') || (var == 'sfcWind') || (var == 'hurs') || (var == 'rsds')) series_c[which(series_c<0)] <- 0

                return(list(qm.input.series = series, qm.corrected.series = series_c, quantile.index  = series_q))
        }
}
