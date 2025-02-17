% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selbox.get.closest.ngridcells.R
\name{selbox.get.closest.ngridcells}
\alias{selbox.get.closest.ngridcells}
\title{Determine the n closest climate model grid cells}
\usage{
selbox.get.closest.ngridcells(lon.point, lat.point, file.lonlatgrid, n)
}
\arguments{
\item{lon.point}{Real-world longitude of target (decimal system).}

\item{lat.point}{Real-world latgitude of target (decimal system).}

\item{file.lonlatgrid}{Filename of Rdata file containing list (coords)
of grid lon and lat coordinates (matrices longrid and latgrid). Output
of function \link{selbox.nclonlat.to.rdata}.}

\item{n}{Number of closest grid cells to determine.}
}
\value{
Two-element Matrix [n,2] containing the x-(lon) and the y-indices
(lat) of the n closest grid cells.
}
\description{
\code{selbox.get.closest.gridcell} determines the n closest
climate model grid cells for a given lon/lat coordinate pair (target).
Criterion: smallest Euclidean distance in lon-lat space. Return error if
target lies completely out of grid range (as defined by grid cell centers).
}
\examples{
\dontrun{
# Find the 10 closest grid cells for coordinate longitude 10.38 and
# latitude 42.25
selbox.get.closest.gridcell(10.38,42.25,file.lonlatgrid,10)
}
}
\author{
Sven Kotlarski (MeteoSwiss)
}
