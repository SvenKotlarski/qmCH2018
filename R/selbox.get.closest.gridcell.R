#' @title Determine closest climate model grid cell
#'
#' @description \code{selbox.get.closest.gridcell} determines the closest
#' climate model grid cell for a given lon/lat coordinate pair (target).
#' Criterion: smallest Euclidean distance in lon-lat space. Returns error
#' if target lies completely out of grid range (as defined by grid cell
#' centers).
#'
#' @param lon.point Real-world longitude of target (decimal system).
#' @param lat.point Real-world latgitude of target (decimal system).
#' @param file.lonlatgrid Filename of Rdata file containing list (coords)
#' of grid lon and lat coordinates (matrices longrid and latgrid). Output
#' of function \link{selbox.nclonlat.to.rdata}.
#'
#' @return Two-element Vector containing the x-(lon) and the y-index (lat)
#' of the closest grid cell.

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Find closest grid cell for coordinate longitude 10.38 and latitude 42.25
#' selbox.get.closest.gridcell(10.38,42.25,file.lonlatgrid)
#' }

selbox.get.closest.gridcell <- function(lon.point,lat.point,file.lonlatgrid){

        load(file.lonlatgrid)

        # *** check if target longitude or target latitude is out of range ***
        minlon <- min(coords$longrid)
        maxlon <- max(coords$longrid)
        minlat <- min(coords$latgrid)
        maxlat <- max(coords$latgrid)

        if ((lon.point < minlon) | (lon.point > maxlon)){
                print(paste('ERROR in selbox.get.closest.gridcell(): Target longitude (',lon.point,') out of grid longitude range.'),sep='')
                return()
        }

        if ((lat.point < minlat) | (lat.point > maxlat)){
                print(paste('ERROR in selbox.get.closest.gridcell(): Target latitude (',lat.point,') out of grid latitude range.'),sep='')
                return()
        }

        # *** Determine index of closest grid cell ***
        res.vector <- c(NA,NA)
        min.distance <- NA
        nx <- dim(coords$longrid)[1]
        ny <- dim(coords$longrid)[2]

        for (countx in 1:nx) {
                for (county in 1:ny){

                        # *** Compute distance in lon and lat direction. If distance in lon direction > 180 replace by 360-distance ***
                        dist.lon <- lon.point-coords$longrid[countx,county]
                        if (dist.lon > 180) dist.lon <- 360 - dist.lon
                        dist.lat <- lat.point-coords$latgrid[countx,county]

                        # *** Compute Euclidean distance ***
                        distance <- sqrt(dist.lon^2 + dist.lat^2)

                        if ((countx==1) & (county==1)) {
                                min.distance <- distance
                                res.vector=c(1,1)
                        }else{
                                if (distance <= min.distance){
                                        min.distance <- distance
                                        res.vector <- c(countx,county)
                                }
                        }

                }
        }

        return(res.vector)

        # *** end of function ***
}
