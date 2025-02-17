% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selbox.selindexbox.netcdf.cdo.R
\name{selbox.selindexbox.netcdf.cdo}
\alias{selbox.selindexbox.netcdf.cdo}
\title{Select grid cell series from NetCDF file}
\usage{
selbox.selindexbox.netcdf.cdo(x, y, file.org, varname, workdir,
  save.series = FALSE, file.netcdf, file.rdata)
}
\arguments{
\item{x}{X-Index (longitude) of grid cell to select.}

\item{y}{Y-Index (latitude) of grid cell to select.}

\item{file.org}{Full filename of NetCDF file (including directory).}

\item{varname}{NetCDF variable name.}

\item{workdir}{Path of NetCDF file.}

\item{save.series}{Logical indicating whether result series should be saved
in a file.}

\item{file.netcdf}{Full filename of result NetCDF file
(including directory).}

\item{file.rdata}{Full filename of result RData file
(including directory).}
}
\value{
Extracted time series (vector).
}
\description{
\code{selbox.selindexbox.netcdf.cdo} selects a single grid
cell series from a NetCDF file using cdo selindebox. Stores the series
as both RData and NetCDF file if desired. No calendar adjustment.
}
\examples{
\dontrun{
# Select precipitation series for grid cell x=10 and y=20 and save series
# as NetCDF and RData
selbox.selindexbox.netcdf.cdo(10,20,infile,'pr',workdir,TRUE,out.netcdf,out.rdata)
}
}
\author{
Iris Feigenwinter (MeteoSwiss)
}
