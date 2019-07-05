#' @title Select grid cell series from NetCDF file
#'
#' @description \code{selbox.selindexbox.netcdf.cdo} selects a single grid
#' cell series from a NetCDF file using cdo selindebox. Stores the series
#' as both RData and NetCDF file if desired. No calendar adjustment.
#'
#' @param x X-Index (longitude) of grid cell to select.
#' @param y Y-Index (latitude) of grid cell to select.
#' @param file.org Full filename of NetCDF file (including directory).
#' @param varname NetCDF variable name.
#' @param workdir Path of NetCDF file.
#' @param save.series Logical indicating whether result series should be saved
#' in a file.
#' @param file.netcdf Full filename of result NetCDF file
#' (including directory).
#' @param file.rdata Full filename of result RData file
#' (including directory).
#'
#' @return Extracted time series (vector).
#'
#' @author Iris Feigenwinter (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Select precipitation series for grid cell x=10 and y=20 and save series
#' # as NetCDF and RData
#' selbox.selindexbox.netcdf.cdo(10,20,infile,'pr',workdir,TRUE,out.netcdf,out.rdata)
#' }

selbox.selindexbox.netcdf.cdo <- function(x,y,file.org,varname,workdir,save.series=FALSE,file.netcdf,file.rdata){

        # *** load libraries ***
        #library(ncdf4)

        # *** select grid cell and store resulting time series in temporary file ***
        command <- paste('cdo selindexbox,',x,',',x,',',y,',',y,' ',file.org,' ',workdir,'/temp.nc',sep='')
        system(command)

        # *** read time series temporary file ***
        ncfile <- ncdf4::nc_open(paste(workdir,'/temp.nc',sep=''))
        series <- ncdf4::ncvar_get(ncfile,varid=varname)

        # *** if save.series=TRUE: save resulting RData and NetCDF file ***
        if (save.series==TRUE) {
                save(series,file=file.rdata)
                command <- paste('mv ',workdir,'/temp.nc ',file.netcdf,sep='')
                system(command)
        }else{
                command <- paste('rm -f ',workdir,'/temp.nc',sep='')
                system(command)
        }

        return(series)
}
