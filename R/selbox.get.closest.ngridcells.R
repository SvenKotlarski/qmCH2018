#' @title Determine the n closest climate model grid cells
#'
#' @description \code{selbox.get.closest.gridcell} determines the n closest
#' climate model grid cells for a given lon/lat coordinate pair (target).
#' Criterion: smallest Euclidean distance in lon-lat space. Return error if
#' target lies completely out of grid range (as defined by grid cell centers).
#'
#' @param lon.point Real-world longitude of target (decimal system).
#' @param lat.point Real-world latgitude of target (decimal system).
#' @param file.lonlatgrid Filename of Rdata file containing list (coords)
#' of grid lon and lat coordinates (matrices longrid and latgrid). Output
#' of function \link{selbox.nclonlat.to.rdata}.
#' @param n Number of closest grid cells to determine.
#'
#' @return Two-element Matrix [n,2] containing the x-(lon) and the y-indices
#' (lat) of the n closest grid cells.

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Find the 10 closest grid cells for coordinate longitude 10.38 and
#' # latitude 42.25
#' selbox.get.closest.gridcell(10.38,42.25,file.lonlatgrid,10)
#' }

selbox.get.closest.ngridcells <- function(lon.point,lat.point,file.lonlatgrid,n){

        load(file.lonlatgrid)

        # *** get grid dimensions ***
        nx <- dim(coords$longrid)[1]
        ny <- dim(coords$longrid)[2]

        # *** check if target longitude or target latitude is out of range ***
        minlon <- min(coords$longrid)
        maxlon <- max(coords$longrid)
        minlat <- min(coords$latgrid)
        maxlat <- max(coords$latgrid)

        if ((lon.point < minlon) | (lon.point > maxlon)){
                print(paste('ERROR in selbox.get.closest.ngridcells(): Target longitude (',lon.point,') out of grid longitude range.'),sep='')
                return()
        }

        if ((lat.point < minlat) | (lat.point > maxlat)){
                print(paste('ERROR in selbox.get.closest.gridcell(): Target latitude (',lat.point,') out of grid latitude range.'),sep='')
                return()
        }

        # *** Determine indices of the n closest grid cells
        full.matrix <- matrix(NA,nrow=nx*ny,ncol=3)
        res.matrix <- matrix(NA,nrow=n,ncol=2)

        for (county in 1:ny) {
                for (countx in 1:nx){

                        # *** Compute distance in lon and lat direction. If distance in lon direction > 180 replace by 360-distance ***
                        dist.lon <- lon.point-coords$longrid[countx,county]
                        if (dist.lon > 180) dist.lon <- 360 - dist.lon
                        dist.lat <- lat.point-coords$latgrid[countx,county]

                        # *** Compute Euclidean distance ***
                        distance <- sqrt(dist.lon^2 + dist.lat^2)

                        # *** assign distance to matrix ***
                        full.matrix[((county-1)*nx)+countx,] <- c(countx,county,distance)
                }
        }

        # *** sort matrix by third column (distance) ***
        temp.matrix <- helper.mat.sort(full.matrix,3)

        # *** extract indices of the first n rows ***
        res.matrix <- temp.matrix[1:n,1:2]

        # *** return n*2 matrix contiaing box indices ***
        return(res.matrix)

        # *** end of function ***
}
