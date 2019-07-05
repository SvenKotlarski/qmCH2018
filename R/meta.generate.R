#' @title Generate GCM-RCM meta information
#'
#' @description \code{meta.generate} creates a meta information file
#' containing required GCM and RCM information for use within CH2018.
#'
#' @param filename Name of meta information file to be created.
#'
#' @return List containing meta information (the same object that is
#' saved to filename)

#' @author Sven Kotlarski (MeteoSwiss)
#'
#' @export
#'
#' @examples \dontrun{
#' # Create meta information file metainfo_CH2018.RData
#' generate.meta('metainfo_CH2018.RData')
#' }

meta.generate <- function(filename){

        # *** create base list ***
        meta <- list(GCM=NULL,RCM=NULL)

        # ****************************
        # *** fill GCM information ***
        # ****************************
        # *** MPIESM ***
        meta$GCM$MPIESM<-list(fullname='MPI-M-MPI-ESM-LR')
        # *** ECEARTH ***
        meta$GCM$ECEARTH<-list(fullname='ICHEC-EC-EARTH')
        # *** CNRM ***
        meta$GCM$CNRM<-list(fullname='CNRM-CERFACS-CNRM-CM5')
        # *** IPSL ***
        meta$GCM$IPSL<-list(fullname='IPSL-IPSL-CM5A-MR')
        # *** CCCMA ***
        meta$GCM$CCCMA<-list(fullname='CCCma-CanESM2')
        # *** CSIRO ***
        meta$GCM$CSIRO<-list(fullname='CSIRO-QCCCE-CSIRO-Mk3-6-0')
        # *** MIROC ***
        meta$GCM$MIROC<-list(fullname='MIROC-MIROC5')
        # *** NORESM ***
        meta$GCM$NORESM<-list(fullname='NCC-NorESM1-M')
        # *** GFDL ***
        meta$GCM$GFDL<-list(fullname='NOAA-GFDL-GFDL-ESM2M')
        # *** HADGEM ***
        meta$GCM$HADGEM<-list(fullname='MOHC-HadGEM2-ES')


        # ****************************
        # *** fill RCM information ***
        # ****************************
        # *** CLMCOM-CCLM4 ***
        meta$RCM[['CLMCOM-CCLM4']]<-list(fullname='CLMcom-CCLM4-8-17')
        # *** ETHZ-CCLM5 ***
        meta$RCM[['CLMCOM-CCLM5']]<-list(fullname='CLMcom-CCLM5-0-6')
        # *** CNRM-ALADIN ***
        meta$RCM[['CNRM-ALADIN']]<-list(fullname='CNRM-ALADIN53')
        # *** DMI-HIRHAM ***
        meta$RCM[['DMI-HIRHAM']]<-list(fullname='DMI-HIRHAM5')
        # *** IPSL-WRF ***
        meta$RCM[['IPSL-WRF']]<-list(fullname='IPSL-INERIS-WRF331F')
        # *** KNMI-RACMO ***
        meta$RCM[['KNMI-RACMO']]<-list(fullname='KNMI-RACMO22E')
        # *** MPICSC-REMO1 ***
        meta$RCM[['MPICSC-REMO1']]<-list(fullname='MPI-CSC-REMO2009')
        # *** MPICSC-REMO2 ***
        meta$RCM[['MPICSC-REMO2']]<-list(fullname='MPI-CSC-REMO2009')
        # *** SMHI-RCA ***
        meta$RCM[['SMHI-RCA']]<-list(fullname='SMHI-RCA4')
        # *** BCCR-WRF ***
        meta$RCM[['BCCR-WRF']]<-list(fullname='BCCR-WRF331C')
        # *** HMS-ALADIN ***
        meta$RCM[['HMS-ALADIN']]<-list(fullname='HMS-ALADIN52')
        # *** ICTP-REGCM ***
        meta$RCM[['ICTP-REGCM']]<-list(fullname='ICTP-RegCM4-3')


        # ********************************
        # *** fill RCM-GCM information ***
        # ********************************
        # *** CLMCOM-CCLM4-CNRM ***
        meta$RCMGCM[['CLMCOM-CCLM4-CNRM']]<-list(realization='r1i1p1',cal='366')
        # *** CLMCOM-CCLM4-ECEARTH ***
        meta$RCMGCM[['CLMCOM-CCLM4-ECEARTH']]<-list(realization='r12i1p1',cal='366')
        # *** CLMCOM-CCLM4-HADGEM ***
        meta$RCMGCM[['CLMCOM-CCLM4-HADGEM']]<-list(realization='r1i1p1',cal='360')
        # *** CLMCOM-CCLM4-MPIESM ***
        meta$RCMGCM[['CLMCOM-CCLM4-MPIESM']]<-list(realization='r1i1p1',cal='366')
        # *** CNRM-ALADIN-CNRM ***
        meta$RCMGCM[['CNRM-ALADIN-CNRM']]<-list(realization='r1i1p1',cal='366')
        # *** DMI-HIRHAM-ECEARTH ***
        meta$RCMGCM[['DMI-HIRHAM-ECEARTH']]<-list(realization='r3i1p1',cal='366')
        # *** HMS-ALADIN-CNRM ***
        meta$RCMGCM[['HMS-ALADIN-CNRM']]<-list(realization='r1i1p1',cal='366')
        # *** ICTP-REGCM-HADGEM ***
        meta$RCMGCM[['ICTP-REGCM-HADGEM']]<-list(realization='r1i1p1',cal='360')
        # *** IPSL-WRF-IPSL ***
        meta$RCMGCM[['IPSL-WRF-IPSL']]<-list(realization='r1i1p1',cal='366')
        # *** KNMI-RACMO-ECEARTH ***
        meta$RCMGCM[['KNMI-RACMO-ECEARTH']]<-list(realization='r1i1p1',cal='366')
        # *** KNMI-RACMO-HADGEM ***
        meta$RCMGCM[['KNMI-RACMO-HADGEM']]<-list(realization='r1i1p1',cal='360')
        # *** MPICSC-REMO1-MPIESM ***
        meta$RCMGCM[['MPICSC-REMO1-MPIESM']]<-list(realization='r1i1p1',cal='366')
        # *** MPICSC-REMO2-MPIESM ***
        meta$RCMGCM[['MPICSC-REMO2-MPIESM']]<-list(realization='r2i1p1',cal='366')
        # *** SMHI-RCA-CNRM ***
        meta$RCMGCM[['SMHI-RCA-CNRM']]<-list(realization='r1i1p1',cal='366')
        # *** SMHI-RCA-ECEARTH ***
        meta$RCMGCM[['SMHI-RCA-ECEARTH']]<-list(realization='r12i1p1',cal='366')
        # *** SMHI-RCA-IPSL ***
        meta$RCMGCM[['SMHI-RCA-IPSL']]<-list(realization='r1i1p1',cal='365')
        # *** SMHI-RCA-HADGEM ***
        meta$RCMGCM[['SMHI-RCA-HADGEM']]<-list(realization='r1i1p1',cal='360')
        # *** SMHI-RCA-MPIESM ***
        meta$RCMGCM[['SMHI-RCA-MPIESM']]<-list(realization='r1i1p1',cal='366')
        # *** SMHI-RCA-CCCMA ***
        meta$RCMGCM[['SMHI-RCA-CCCMA']]<-list(realization='r1i1p1',cal='365')
        # *** SMHI-RCA-CSIRO ***
        meta$RCMGCM[['SMHI-RCA-CSIRO']]<-list(realization='r1i1p1',cal='365')
        # *** SMHI-RCA-MIROC ***
        meta$RCMGCM[['SMHI-RCA-MIROC']]<-list(realization='r1i1p1',cal='365')
        # *** SMHI-RCA-NORESM ***
        meta$RCMGCM[['SMHI-RCA-NORESM']]<-list(realization='r1i1p1',cal='365')
        # *** SMHI-RCA-GFDL ***
        meta$RCMGCM[['SMHI-RCA-GFDL']]<-list(realization='r1i1p1',cal='365')
        # *** ETHZ-CCLM5-CNRM ***
        meta$RCMGCM[['CLMCOM-CCLM5-CNRM']]<-list(realization='r1i1p1',cal='366')
        # *** ETHZ-CCLM5-ECEARTH ***
        meta$RCMGCM[['CLMCOM-CCLM5-ECEARTH']]<-list(realization='r12i1p1',cal='366')
        # *** ETHZ-CCLM5-MIROC ***
        meta$RCMGCM[['CLMCOM-CCLM5-MIROC']]<-list(realization='r1i1p1',cal='365')
        # *** ETHZ-CCLM5-MPIESM ***
        meta$RCMGCM[['CLMCOM-CCLM5-MPIESM']]<-list(realization='r1i1p1',cal='366')
        # *** ETHZ-CCLM5-HADGEM ***
        meta$RCMGCM[['CLMCOM-CCLM5-HADGEM']]<-list(realization='r1i1p1',cal='360')
        # *** HMS-ALADIN-CNRM ***
        meta$RCMGCM[['HMS-ALADIN-CNRM']]<-list(realization='r1i1p1',cal='366')
        # *** BCCR-WRF-NORESM ***
        meta$RCMGCM[['BCCR-WRF-NORESM']]<-list(realization='r1i1p1',cal='366')


        # *************************
        # *** save results file ***
        # *************************
        save(meta,file=filename)

        return(meta)

# *** end of function ***
}





