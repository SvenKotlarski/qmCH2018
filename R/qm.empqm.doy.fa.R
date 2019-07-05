#' @title Short The actual QM function considering the DOY and frequency
#' adaptation
#'
#' @description \code{qm.empqm.doy.fa} is the actual QM function considering
#' the day-of-year (DOY) and including frequency adaptation for precipitation.
#'
#' @details The function estimates the index (quantile) in which an observation
#' falls with respect to the calibration period. Based on this quantile (and
#' the DOY) a correction is applied, the final quantile map. If the number of
#' dry days in the modelled time series is larger than the number in the
#' reference series, an additional frequency adaptation is carried out: for
#' modelled dry days it is first determined (using a random number) if the
#' corrected value should be zero as well. If not, a random value is drawn from
#' the wet day distribution. Old and new extremes beyond the min and max
#' percentile considered are corrected according to the correction of the min
#' and max percentile, respectively (first and last quantile considered
#' therefore have to be 0.01 and 0.99 -> check in function \link{qm.doqm}).
#'
#' @param series Time series of modelled data, e.g. transient (control+)
#' scenario series.
#' @param doy.series Corresponding series of DOYs (output of function
#' \link{qm.doystring}).
#' @param cdf.mod CDF of the modelled time series in the calibration period,
#' estimated with function qm.cdf.doy(...) (matrix[365,nquantiles]).
#' @param cdf.obs CDF of the observed time series in the calibration period,
#' estimated with function qm.cdf.doy(...) (matrix[365,nquantiles]).
#' @param cdf.obs.wet CDF of the observed time series in the calibration period
#' for wet days only, estimated with function qm.cdf.doy(...)
#' (matrix[365,nquantiles]).
#' @param corr.fun Additive correction function (cdf A - cdf B), obtained by
#' function qm.corfun.doy(...).
#' @param fwet.obs Observed wet day frequency for each DOY (vector[365],
#' output of function qm.cdf.doy).
#' @param fwet.mod Modelled wet day frequency for each DOY (vector[365],
#' output of function qm.cdf.doy).
#' @param pr.wet Internal precipitation wet day threshold [mm/day].
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

#' @author Jan Rajczak (ETH Zurich), Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Carry out DOY-dependent QM with frequency adaptation. Standard quantiles.
#' # Variable: precipitation. Wet-day treshold of 0.1 mm/day. Linear QM,
#' # diagnostic terminal output.
#' qm.empqm.doy.fa(series, doy.series, cdf.modelled, cdf.observed, cdf.observed.wet, correction.function, fwet.obs, fwet.mod, 0.1, 'linear', 'pr', TRUE, 0.01, 0.00, 0.01)
#' }

qm.empqm.doy.fa <- function(series, doy.series, cdf.mod, cdf.obs, cdf.obs.wet, cor.fun, fwet.obs, fwet.mod, pr.wet, method=c("linear","bin"), var, output.diag=FALSE, minq, maxq, incq){

        if(output.diag==TRUE){
                print("")
                print("QM WITH frequency adjustment (DOY) ...")
                print("")
        }

        if(length(series) != length(doy.series)){ # *** a little check
                print(paste("ERROR: length of time series (",length(series),") and DOY series (",length(doy.series),") do not match.", sep=""))
        }else{
                series_q		<- rep(NA,length(series))
                series_c		<- rep(NA,length(series))
                for(i in 1:length(series)){
                        doy <- doy.series[i]
                        if(doy == 999){ doy <- 60 }
                        if(is.na(series[i]) == "TRUE"){
                                series_q[i] <- NA
                                series_c[i] <- NA
                        }else{
                                # *** OPTION A: if dry day in model and if modelled wet day frequency <= observed wet day frequency apply frenquency adjustment ***
                                if ((series[i]==0) & (fwet.mod[doy] <= fwet.obs[doy])) {
                                        # *** generate random number ***
                                        rnum <- runif(1)
                                        # *** determine fraction of dry days in model that needs to become wet in order to represent observed wet day frequency ***
                                        probwet <- 1 - ((1-fwet.obs[doy])/(1-fwet.mod[doy]))
                                        # *** if rnum <= probwet: fill with a precipitation day; else assign original value, i.e. zero precip ***
                                        if (rnum <= probwet) {
                                                # *** randomly draw from observed wet day distribution and assign that value ***
                                                fun <- approxfun(seq(0,1,0.01),cdf.obs.wet[doy,])
                                                random.number <- runif(1)
                                                series_c[i] <- fun(random.number)
                                        }else{
                                                series_c[i] <- series[i]
                                        }
                                        # *** in any case, assign NA to series of modelled quantile numbers: means FA has been carried out! ***
                                        series_q[i]	<- NA
                                }else{
                                        # *** OPTION B: else, don't apply frenquency adjustment; apply regular correction function ***
                                        # *** using quantile-"bins" ***
                                        if(match.arg(method) == "bin"){
                                                # *** Note: this command implicitly selects the first (last) quantile considered for new extremes , i.e.  ***
                                                # *** for smaller (larger) values than those occuring in the calbration period (in case of zeros: first   ***
                                                # *** occurence). If minq=0.01 and maxq=0.99 nex extremes are hence corrected according to the correction ***
                                                # *** function for the 1st and 99th percentile.                                                           ***
                                                ix		<- which.min(abs(cdf.mod[doy,]-series[i]))
                                                # *** Compute quantile index  ***
                                                series_q[i]	<- minq + (ix-1)*incq
                                                # *** Compute corrected value ***
                                                series_c[i]	<- series[i]-cor.fun[doy,ix]
                                        }

                                        # *** using linear interpolation between quantiles ***
                                        if(match.arg(method) == "linear"){
                                                # *** number of interpolated sampling points ***
                                                npoints <- 1000
                                                cor		<- approx(cdf.mod[doy,],n=npoints,method="linear")$y
                                                # *** Note: this command implicitly selects the first (last) quantile considered for new extremes , i.e.  ***
                                                # *** for smaller (larger) values than those occuring in the calbration period (in case of zeros: first   ***
                                                # *** occurence). If minq=0.01 and maxq=0.99 nex extremes are hence corrected according to the correction ***
                                                # *** function for the 1st and 99th percentile.                                                           ***
                                                ix		<- which.min(abs(cor-series[i]))
                                                # *** Compute quantile index ***
                                                series_q[i]	<- minq + (ix-1) * ((maxq-minq)/(npoints-1))
                                                # *** Compute correction function ***
                                                cf		<- approx(cor.fun[doy,],n=npoints,method="linear")$y
                                                # *** Compute corrected value ***
                                                series_c[i]	<- series[i]-cf[ix]
                                        }
                                }

                        }
                }

                # *** for huss (specific humidity), sfcWind (wind speed), hurs (relative humidity), rsds (global radiation): set negative values to 0 ***
                if ((var == 'huss') || (var == 'sfcWind') || (var == 'hurs') || (var == 'rsds')) series_c[which(series_c<0)] <- 0

                return(list(qm.input.series = series, qm.corrected.series = series_c, quantile.index  = series_q))
        }
}
