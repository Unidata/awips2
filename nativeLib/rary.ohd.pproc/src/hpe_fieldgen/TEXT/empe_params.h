/*******************************************************************************
* FILENAME:    empe_params.h
*
* DESCRIPTION: This file contains static parameters for the
*              mpe_fieldgen.
*
* ORIGINAL AUTHOR:  Guoxian Zhou
* CREATION DATE:    January  2007
* ORGANIZATION:     HSEB / OHD
* MACHINE:          HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#ifndef EMPE_PARAMS_H
#define EMPE_PARAMS_H

#include "empe_constants.h"
#include "empe_db_tables.h"
#include "empe_field_names.h"
#include "get_empe_product_state.h"

/*--------------------------------*/
/*  definition of variables       */
/*--------------------------------*/

typedef struct _empe_params_struct
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

    /*
     * number of minutes around top of hour in which to search
     * for a top-of-hour product.
     * determined by token "dpa_window".
     * default = 10 minutes.
     */

    int dpa_window;

    /*
     * number of minutes around the end time in which to search
     * for an product within the range.
     * determined by token "dhr_window".
     * default = 10 minutes.
     */

    int dhr_window;

    /*
     * number of minutes around the end time in which to search
     * for an product within the range.
     * determined by token "dsp_window".
     * default = 10 minutes.
     */

    int dsp_window;

    /*
     * duration minutes for dsp product.
     * determined by token "hpe_dsp_duration".
     * default = 60 minutes.
     */

    int dsp_duration;

    /*
     * category name based on the dsp_duration.
     */

    char category_name[4];

    /* check if need delete zeros in gage data
     * determined by token "mpe_del_gage_zeros".
     */

    int del_gage_zeros;

    /*
     * check if running for quarter hrap grid
     * determined by token "hpe_quarter_hrap_grid".
     */

    int hrap_grid_factor;

    /*
     * check if need load the misbin file
     * determined by token "hpe_load_misbin".
     */

    int load_misbin;

    /*
     * Parameters from tables RWParams and RWBiasStat.
     */

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

    /* blnMeanFieldBias = the mean field bias flag for DSP radar.
     *  = 0 -- don't compute mean field bias
     *  = 1 -- compute mean field bias
     */

    int blnMeanFieldBias;

    /* blnMeanFieldBias = the mean field bias flag for DHR radar.
     *  = 0 -- don't compute mean field bias
     *  = 1 -- compute mean field bias
     */

    int blnDHRMeanFieldBias;

    /** locbias_1hr_rerun = the local bias rerun.
      *  = 0 -- do not recalc local bias on rerun
      *  = 1 -- recalc local bias on rerun
      * determined by token "mpe_locbias_1hr_rerun".
      * default = 0.
      **/

    int locbias_1hr_rerun ;

    char fxa_local_site[WFO_LEN + 1];

    int irc_load_stat ;

    /* sat_avail = satellite availability flag
     * 0 = satellite unavailable, 1 = available
     */

    int sat_avail;
    int radar_avail_num ;
    int build_neighbor_list ;

    /** save_netcdf = integer value of save_netcdf token.
      * if token is found as "ON",
      * then create and save netCDF image.
      *  = 0 -- do not save
      *  = 1 -- create and save netCDF image.
      * determined by token "save_netcdf".
      * default = 0.
      **/

    int save_netcdf;

    /** save_gif = integer value of save_netcdf token.
      * if token is found as "ON",
      * then create and save GIF image.
      *  = 0 -- do not save
      *  = 1 -- create and save netCDF image.
      * determined by token "save_gif".
      * default = 0.
      **/

    int save_gif;

    /** save_jpeg = integer value of save_netcdf token.
      * if token is found as "ON",
      * then create and save JPEG image.
      *  = 0 -- do not save
      *  = 1 -- create and save netCDF image.
      * determined by token "save_jpeg".
      * default = 0.
      **/

    int save_jpeg;

    /** save_grib = integer value of save_netcdf token.
      * if token is found as "ON",
      * then create and save GRIB image.
      *  = 0 -- do not save
      *  = 1 -- create and save netCDF image.
      * determined by token "save_grib".
      * default = 0.
      **/

    int save_grib;

    char base_radar_mosaic [ BESTFIELD_LEN ];

    /* blnUseLocalBias = indicator for using the local bias.
     *                 = 0 -- use mean field bias
     *                 = 1 -- use local bias
     */

    int blnUseLocalBias;

    /* blnRunNowcast = indicator for running the nowcast.
     *                 = 0 -- not run nowcast
     *                 = 1 -- run nowcast
     */

     int blnRunNowcast;

} empe_params_struct;


#endif /* #ifndef EMPE_PARAMS_H */
