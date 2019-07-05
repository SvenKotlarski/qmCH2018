#' @title The actual QM function for the bulk series
#' considering frequency adaptation (TAKE CARE: NOT YET FULLY TESTED!!)
#'
#' @description \code{qm.empqm.bulk.fa} is the actual QM function for
#' the bulk series. Equivalent to function \link{qm.empqm.doy.fa} but
#' for the bulk series, i.e. disregarding a day-of-year (DOY) dependence
#' of the correction function.
#'
#' @param series Time series of modelled data, e.g. transient (control+)
#' scenario series.
#' @param cdf.mod.bulk CDF of the modelled time series in the calibration
#' period, estimated with function \link{qm.cdf.bulk} (vector[nquantiles]).
#' @param cdf.obs.bulk CDF of the observed time series in the calibration
#' period, estimated with function \link{qm.cdf.bulk} (vector[nquantiles]).
#' @param cdf.obs.wet.bulk  CDF of the observed time series in the calibration
#' period for wet days only, estimated with function \link{qm.cdf.bulk}
#' (vector[nquantiles]).
#' @param corr.fun.bulk Additive correction function (cdf A - cdf B), obtained by
#' function \link{qm.corfun.bulk}.
#' @param fwet.obs Observed wet day frequency (output of function
#' \link{qm.cdf.doy}).
#' @param fwet.mod Modelled wet day frequency output of function
#' \link{qm.cdf.doy}).
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
#' $quantile.index: A series of quantile indices wrt. simulated series in calibration period.

#' @author Sven Kotlarski (MeteoSwiss), Jan Rajczak (ETH Zurich)
#'
#' @export
#'
#' @examples \dontrun{
#' # Carry out bulk QM with frequency adaptation. Standard quantiles.
#' # Variable: precipitation. Observed wet day frequency of 0.9, modelled
#' # wet-day frequency of 0.95. Wet-day treshold of 0.1 mm/day. Linear QM,
#' # diagnostic terminal output.
#' qm.empqm.bulk.fa(series, cdf.modelled.bulk, cdf.observed.bulk, cdf.observed.wet.bulk, correction.function.bulk, 0.9, 0.95, 0.1, 'linear', 'pr', TRUE, 0.01, 0.00, 0.01)
#' }

qm.empqm.bulk.fa <- function(series, cdf.mod.bulk, cdf.obs.bulk, cdf.obs.wet.bulk, cor.fun.bulk, fwet.obs, fwet.mod, pr.wet, method=c("linear","bin"), var, output.diag=FALSE, minq, maxq, incq){

        if(output.diag==TRUE){
                print("")
                print("QM WITH frequency adjustment (bulk) ...")
                print("")
        }

        series_q		<- rep(NA,length(series))
        series_c		<- rep(NA,length(series))

        for(i in 1:length(series)){

                if(is.na(series[i]) == "TRUE"){
                        series_q[i] <- NA
                        series_c[i] <- NA
                }else{

                        # *** OPTION A: if dry day in model and if modelled wet day frequency <= observed wet day frequency apply frenquency adjustment ***
                        if ((series[i]==0) & (fwet.mod <= fwet.obs)) {
                                # *** generate random number ***
                                rnum <- runif(1)
                                # *** determine fraction of dry days in model that needs to become wet in order to represent observed wet day frequency ***
                                probwet <- 1 - ((1-fwet.obs)/(1-fwet.mod))
                                # *** if rnum <= probwet: fill with a precipitation day; else assign original value, i.e. zero precip ***
                                if (rnum <= probwet) {
                                        # *** randomly draw from observed wet day distribution and assign that value ***
                                        fun <- approxfun(seq(0,1,0.01),cdf.obs.wet.bulk)
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
                                        ix		<- which.min(abs(cdf.mod.bulk-series[i]))
                                        # *** Compute quantile index  ***
                                        series_q[i]	<- minq + (ix-1)*incq
                                        # *** Compute corrected value ***
                                        series_c[i]	<- series[i]-cor.fun.bulk[ix]
                                }

                                # *** using linear interpolation between quantiles ***
                                if(match.arg(method) == "linear"){
                                        # *** number of interpolated sampling points ***
                                        npoints <- 1000
                                        cor		<- approx(cdf.mod.bulk,n=npoints,method="linear")$y
                                        # *** Note: this command implicitly selects the first (last) quantile considered for new extremes , i.e.  ***
                                        # *** for smaller (larger) values than those occuring in the calbration period (in case of zeros: first   ***
                                        # *** occurence). If minq=0.01 and maxq=0.99 nex extremes are hence corrected according to the correction ***
                                        # *** function for the 1st and 99th percentile.                                                           ***
                                        ix		<- which.min(abs(cor-series[i]))
                                        # *** Compute quantile index ***
                                        series_q[i]	<- minq + (ix-1) * ((maxq-minq)/(npoints-1))
                                        # *** Compute correction function ***
                                        cf		<- approx(cor.fun.bulk,n=npoints,method="linear")$y
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
