#' @title Wrapper function for application of QM
#'
#' @description \code{qm.doqm} wraps up all necessary functions to apply
#' QM. For precipitation: internal wet day threshold is applied before
#' and after actual QM (and resulting negative values are set to 0 implicitly
#' in case that FA==TRUE). Day-of-year (DOY) dependency versus bulk QM
#' is controlled by argument \code{w.width} ('none' for bulk QM).
#'
#' @param series.mod.cal Modelled time series (calibration period), vector.
#' @param series.obs.cal Observed (reference) time series (calibration period),
#' vector.
#' @param series.mod.all Modelled time series (entire period for which QM
#' should be applied), vector.
#' @param year.cal.begin First year of calibration period (only required
#' if DOY QM; default: -999).
#' @param year.cal.end Last year of calibration period (only required if
#' DOY QM; default: -999).
#' @param year.scen.begin First year of entire period which should be
#' corrected (start year of series.mod.all; only required if DOY QM;
#' default: -999).
#' @param year.scen.end Last year of entire period which should be corrected
#' (end year of series.mod.all; only required if DOY QM; default: -999).
#' @param w.width Full width of moving window (has to be an uneven number) in
#' [days]; e.g. 91 for moving window of 91 days; if w.width='none': bulk QM
#' (no DOY dependence).
#' @param minq Minimum quantile for correction function [0.01 .. 0.99],
#' default: 0.01; note: should not be 0 (for correct handling of extremes)!
#' @param maxq Maximum quantile for correction function [0.01 .. 0.99],
#' default: 0.99; note: should not be 1 (for correct handling of extremes)!
#' @param incq Quantile increment for correction function (bin size);
#' default: 0.01.
#' @param var Variable that is considered. List of allowed values
#' (to be extended):
#' 	tas: daily mean temperature
#'      tasmin: daily minimum temperature
#'      tasmax: daily maximum temperature
#'      pr: daily precipitation sum
#'      huss: daily mean specific humidity
#'      sfcWind: daily mean 10m wind speed
#'      hurs: daily mean relative humidity
#'      rsds: daily mean global radiation (surface shortwave downwelling).
#' @param pr.wet Internal precipitation wet day threshold [mm/day]; will be
#' applied prior to QM; default: 0.1 mm/day.
#' @param FA Carry out frequency adaptation for precipitation (TRUE, default)
#' or not (FALSE).
#' @param qm.method QM method ("linear" or "bin"); default: linear.
#' @param output.diag Diagnostic QM console output (TRUE) or not (FALSE);
#' default: FALSE.
#'
#' @return List of 6:
#' $obs.series: Series of original reference values (observations) over
#' calibration period (series.obs.cal; no wet day threshold applied yet).
#' $raw.series: Series of original values over entire period  (series.mod.all;
#' no wet day threshold applied yet).
#' $qm.input.series: Input series to QM over entire period (=series.mod.all
#' but possibly wet day threshold applied).
#' $qm.corrected.series: Series of corrected values over entire period
#' (after QM).
#' $quantile.index: Series of quantile indices, wrt calibration period.
#' $corr.fun: Correction function for each quantile and DOY
#' (matrix[365,nquantiles]).

#' @author Jan Rajczak (ETH Zurich), Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Carry out DOY-dependent QM. Calibration period: 1981 to 2010. Entire period
#' # to be quantile-mapped: 1981 to 2099. 91-day moving window. Standard quantiles.
#' # Variable: precipitation. Wet-day treshold of 0.1 mm/day. Frequency
#' # adaptation carried out, linear QM, diagnostic terminal output.
#' qm.doqm(series.mod.cal, series.obs.cal, series.mod.all, 1981, 2010, 1981,
#' 2099, 91, 0.01, 0.99, 0.01, 'pr', 0.01, TRUE, 'linear', TRUE)
#' }

qm.doqm <- function(series.mod.cal, series.obs.cal, series.mod.all, year.cal.begin=-999, year.cal.end=-999, year.scen.begin=-999, year.scen.end=-999, w.width, minq=0.01, maxq=0.99, incq=0.01, var, pr.wet=0.01, FA=FALSE, qm.method='linear',output.diag=FALSE){

        # *** check whether variable name is allowed or not (to be extended...) ***
        varlist <- c("tas","tasmin","tasmax","pr","huss","sfcWind","hurs","rsds")
        if (is.element(var,varlist)) {
                if(output.diag==TRUE) print(paste('Variable: ',var))
        }else{
                print(paste('ERROR in function qm.doqm: Variable',var,'not valid!'))
                return()
        }

        # *** check if moving window width is positive and is an uneven number or if it is 'none'; set bulk logical (QM to bulk series or not) ***
        if (is.numeric(w.width)){
                if (w.width <= 0) {
                        print(paste('ERROR in function qm.doqm: Moving window width (w.width) has to be >= 1 (or none) but is ',w.width,sep=''))
                        return()
                }

                if ((w.width%%2) == 0) {
                        print(paste('ERROR in function qm.doqm: Moving window width (w.width) has to be an uneven number (or none) but is ',w.width,sep=''))
                        return()
                }
                bulk=FALSE
        } else {
                if (w.width=='none') {
                        bulk=TRUE
                } else {
                        print(paste('ERROR in function qm.doqm: Moving window width (w.width) has to be an uneven number or none but is ',w.width,sep=''))
                        return()
                }
        }

        # *** check if qm.method is valid ***
        if ((qm.method=='linear')||(qm.method=='bin')) {
                if(output.diag==TRUE) print(paste('QM method: ',qm.method))
        }else{
                print(paste('ERROR in function qm.doqm: qm.method has to be "linear" or "bin" but is "',qm.method,'".'))
                return()
        }

        # *** check for appropriate setting of minq and maxq ***
        if (minq != 0.01) {
                print(paste('ERROR in function qm.doqm: For a correct handling of extremes minq needs to be set to 0.01, but is ',minq,'.'))
                return()
        }

        if (maxq != 0.99) {
                print(paste('ERROR in function qm.doqm: For a correct handling of extremes maxq needs to be set to 0.99, but is ',maxq,'.'))
                return()
        }


        # *** control output ***
        if(output.diag==TRUE){
                print("")
                print("DIAGNOSTIC QM OUTPUT")
                print("====================")
                print("")
                print("Properties of reference (observed) series (calibration period):")
                print(summary(series.obs.cal))
                print("")
                print("Properties of modelled series (calibration period):")
                print(summary(series.mod.cal))
                print("")
                print("Properties of modelled series (entire period):")
                print(summary(series.mod.all))
        }

        # *** series to be handed over to QM function; for precipitation:         ***
        # *** apply wet day threshold to observed and modelled series if FA==TRUE ***
        series.mod.cal.in <- series.mod.cal
        series.obs.cal.in <- series.obs.cal
        series.mod.all.in <- series.mod.all

        if ((var=='pr') & FA) {
                series.mod.cal.in[series.mod.cal.in < pr.wet] <- 0
                series.obs.cal.in[series.obs.cal.in < pr.wet] <- 0
                series.mod.all.in[series.mod.all.in < pr.wet] <- 0
        }

        # *** (1) generate doy-string for scenario
        if (!bulk) doy.scen <- qm.doystring(year.scen.begin, year.scen.end)

        # *** (2) calculate doy-dependent CDFs for model and obs; for precip additionally compute wet day frequency for each doy and wet day distribution
        if (bulk) {
                c_mod <- qm.cdf.bulk(series.mod.cal.in, minq, maxq, incq, var, pr.wet)
                c_obs <- qm.cdf.bulk(series.obs.cal.in, minq, maxq, incq, var, pr.wet)
        } else {
                c_mod <- qm.cdf.doy(series.mod.cal.in, year.cal.begin, year.cal.end, w.width, minq, maxq, incq, var, pr.wet)
                c_obs <- qm.cdf.doy(series.obs.cal.in, year.cal.begin, year.cal.end, w.width, minq, maxq, incq, var, pr.wet)
        }

        # *** (3) estimate correction function
        if (bulk)
                cf <- qm.corfun.bulk(c_mod$cdf.vector, c_obs$cdf.vector)
        else
                cf <- qm.corfun.doy(c_mod$cdf.matrix, c_obs$cdf.matrix)

        # ***?(4) perform quantile-mapping to data (frequency adaptation and application of ***
        # *** wet day threshold after QM for the case of precipitation and if FA==TRUE)     ***
        if ((var=='pr') & FA) {
                if (bulk) {
                        cc <- qm.empqm.bulk.fa(series.mod.all.in, c_mod$cdf.vector, c_obs$cdf.vector, c_obs$cdf.wet.vector, cf$correction, c_obs$f.wet, c_mod$f.wet, pr.wet, method=qm.method, var, output.diag, minq, maxq, incq)
                        cc$qm.corrected.series[cc$qm.corrected.series < pr.wet] <- 0
                } else {
                        cc <- qm.empqm.doy.fa(series.mod.all.in, doy.scen, c_mod$cdf.matrix, c_obs$cdf.matrix, c_obs$cdf.wet, cf$correction, c_obs$f.wet, c_mod$f.wet, pr.wet, method=qm.method, var, output.diag, minq, maxq, incq)
                        cc$qm.corrected.series[cc$qm.corrected.series < pr.wet] <- 0
                }

        }else{
                if (bulk)
                        cc <- qm.empqm.bulk(series.mod.all.in, c_mod$cdf.vector, cf$correction, method=qm.method, var, output.diag, minq, maxq, incq)
                else
                        cc <- qm.empqm.doy(series.mod.all.in, doy.scen, c_mod$cdf.matrix, cf$correction, method=qm.method, var, output.diag, minq, maxq, incq)
        }

        if(output.diag==TRUE){
                print("Properties of corrected series (entire period):")
                print(summary(cc$qm.corrected.series))
                print("")
                print("Summary of quantile bins used for correction (entire period):")
                print(summary(cc$quantile.index))
        }

        obs_max <- max(series.obs.cal,na.rm=TRUE)
        obs_min <- min(series.obs.cal,na.rm=TRUE)

        if(output.diag==TRUE){
                print("")
                print("Days in corrected series (entire period) > maximum of observed series (calib. period):")
                print(paste( length(which(cc$qm.corrected.series > obs_max)),
                             " [ ",round(length(which(cc$qm.corrected.series > obs_max))/length(cc$qm.corrected.series)*100,5)," %]",sep=""))
                print("Days in corrected series (entire  period) < minimum of observed series (calib. period):")
                print(paste(length(which(cc$qm.corrected.series < obs_min)),
                            " [ ",round(length(which(cc$qm.corrected.series < obs_min))/length(cc$qm.corrected.series)*100,5)," %]",sep=""))
        }

        return(list(obs.series = series.obs.cal, raw.series = series.mod.all, qm.input.series = cc$qm.input.series, qm.corrected.series = cc$qm.corrected.series, quantile.index = cc$quantile.index, corr.fun = cf$correction))

}
