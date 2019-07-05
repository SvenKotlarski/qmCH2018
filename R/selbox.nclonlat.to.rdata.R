#' @title Create RData file with grid lattitudes and longitudes
#'
#' @description \code{selbox.nclonlat.to.rdata} reads two fields (real-world
#' longitude and latitude) from a netcdf file and stores them in
#' an RData file. List name coords with two components:
#' $longrid: Matrix(nx,ny) containing real-world longitudes.
#' $latgrid: Matrix(nx,ny) containing real-world latitudes.
#'
#' @param infile Netcdf filename (file has to contain real-word lon
#' and lat fields).
#' @param outfile RData filename for output.
#' @param varlon Variable name for real-world longitude in netcdf file.
#' @param varlat Variable name for real-world latitude in netcdf file.
#'
#' @return Return value of save statement (NULL).
#'
#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Write netcdf longitudes and latitudes to Rdata file
#' selbox.nclonlat.to.rdata(file.netcdf,file.rdata,'longitude','latitude')
#' }

selbox.nclonlat.to.rdata <- function(infile,outfile,varlon,varlat){

        # *** load libraries ***
        #library(ncdf4)

        ncfile <- ncdf4::nc_open(infile)
        longrid <- ncdf4::ncvar_get(ncfile,varid=varlon)
        latgrid <- ncdf4::ncvar_get(ncfile,varid=varlat)
        coords <- list(longrid=longrid,latgrid=latgrid)
        save(coords,file=outfile)

        # *** end of function ***
}
