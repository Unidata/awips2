/*******************************************************************************
* FILENAME:    mpe_params.h
*
* DESCRIPTION: This file contains static parameters for the 
*              mpe_fieldgen.
*
* ORIGINAL AUTHOR:  Guoxian Zhou
* CREATION DATE:    January 7, 2005
* ORGANIZATION:     HSEB / OHD
* MACHINE:          HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
* May 2011       S Naples          Added process_PC token
* Sep 2013         				   Added dual-pol changes
********************************************************************************
*/

#ifndef MPE_PARAMS_H
#define MPE_PARAMS_H

#include "mpe_db_tables.h"
#include "mpe_constants.h"
#include "mpe_field_names.h"
/*-----------------------------------------------------------------*/
/* polarization type (SinglePol or DualPol) for multisensor fields */
/*-----------------------------------------------------------------*/

typedef enum MultisensorPolarizationType
{
  SinglePol,
  DualPol
} MultisensorPolarizationType;

/*--------------------------------*/
/*  definition of variables       */
/*--------------------------------*/

typedef struct _mpe_params_struct
{
    char rfc_name[RFC_NAME_LEN + 1];
    char db_name[DB_DESCR_LEN + 1];

    /** os = operating system (HP or LX) for header of xmrg.
      *  = "HP"  OR  "LX".
      **/
    char os[3];

    /** user = user name.
      *  = OS + "SAN".
      **/
    char user[6];

    /** xmrgdtform = format of time stamp in xmrg filename.
      *  = mdY (= mmddyyyyhh)  OR  Ymd (= yyyymmddhh).
      *  default = mdY
      **/
    char xmrgdtform[XMRGDTFORM_LEN+1];

    /** number of minutes around top of hour in which to search
      * for a top-of-hour DPA product. 
      * determined by token "dpa_wind".
      * default = 10 minutes.
      **/
    int dpa_wind;
    /** number of minutes around top of hour in which to search
      * for a top-of-hour DAA product. 
      * determined by token "daa_wind".
      * default = 5 minutes.
      **/
    int daa_wind;

    /** 
     * minimum coverage of an hour
     * for a radar product to be considered good
     * units = minutes                        
     * default = 60 
     **/
    int daa_min_coverage_dur;    

    /** check if need delete zeros in gage data
      * determined by token "mpe_del_gage_zeros".
      **/
    int del_gage_zeros;

    /** check if need to process PC data
      * determined by token "mpe_process_PC".
      **/
    int process_PC;
    
    /** 
      * Parameters from tables RWParams and RWBiasStat.
      **/
    RWParams * ptrRWParams;
    
    RWBiasStat * ptrRWBiasStat;

    float memory_spans [ NUM_MEMORY_SPANS ];

    /** gage_qc = integer value of Gauge QC token.
      *  = 0 -- do not run Gauge QC 
      *  = 1 -- run Gauge QC
      * determined by token "mpe_gage_qc".
      * default = 0.
      **/
    int gage_qc;

    char qpe_fieldtype[MOSAIC_TYPE_LEN];
    enum DisplayFieldData mosaic_type;
    
    /** blnMeanFieldBias = the mean field bias flag for single-pol radars
      *  = 0 -- don't compute mean field bias
      *  = 1 -- compute mean field bias
      **/
    int blnMeanFieldBias;

    /** blnMeanFieldBiasDP = the mean field bias flag for dual-pol radars
      *  = 0 -- don't compute mean field bias
      *  = 1 -- compute mean field bias
      **/
    int blnMeanFieldBiasDP;

    /** locbias_1hr_rerun = the local bias rerun.
      *  = 0 -- do not recalc local bias on rerun
      *  = 1 -- recalc local bias on rerun
      * determined by token "mpe_locbias_1hr_rerun".
      * default = 0.
      **/
    int locbias_1hr_rerun ;

    int irc_load_stat ;

    /* sat_avail = satellite availability flag
	   0 = satellite unavailable, 1 = available
     */
    int sat_avail;
    int radar_avail_num ;
/*----------------------------------------------------------------------*/
/*  neighbor list generation flag                                       */
/*  build_neighbor_list_SP = 0 -- neighbor list not generated           */
/*                         = 1 -- neighbor list generated for this hour for SP multisensor fields*/
/*  (same definitionsfor build_neighbor_list_DP)                        */
/*----------------------------------------------------------------------*/
    int build_neighbor_list_SP ;
    int build_neighbor_list_DP ;

/*-----------------------------------------------------------------*/
/* polarization type (SinglePol or DualPol) for multisensor fields */
/*-----------------------------------------------------------------*/

    enum MultisensorPolarizationType  polarizationType;

    /** mpe_save_netcdf = integer value of mpe_save_netcdf token.
      * if token is found as "ON",
      * then create and save netCDF image.
      *  = 0 -- do not save 
      *  = 1 -- create and save netCDF image.
      * determined by token "mpe_save_netcdf".
      * default = 0.
      **/
    int mpe_save_netcdf;

    /** mpe_save_gif = integer value of mpe_save_netcdf token.
      * if token is found as "ON",
      * then create and save GIF image.
      *  = 0 -- do not save 
      *  = 1 -- create and save netCDF image.
      * determined by token "mpe_save_gif".
      * default = 0.
      **/
    int mpe_save_gif;

    /** mpe_save_jpeg = integer value of mpe_save_netcdf token.
      * if token is found as "ON",
      * then create and save JPEG image.
      *  = 0 -- do not save 
      *  = 1 -- create and save netCDF image.
      * determined by token "mpe_save_jpeg".
      * default = 0.
      **/
    int mpe_save_jpeg;

    /** mpe_save_grib = integer value of mpe_save_netcdf token.
      * if token is found as "ON",
      * then create and save GRIB image.
      *  = 0 -- do not save 
      *  = 1 -- create and save netCDF image.
      * determined by token "mpe_save_grib".
      * default = 0.
      **/
    int mpe_save_grib;

    char fxa_local_site [ RFC_LEN + 1 ];

} mpe_params_struct; 


#endif /* #ifndef MPE_PARAMS_H */
