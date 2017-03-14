/*******************************************************************************
* FILENAME:     get_mpe_product_state.h
* DESCRIPTION:  Contains the prototype for the get_mpe_product_state,
*               get_mpe_qpe_fieldtype, and get_qpe_fields_array routines.
*
* ORIGINAL AUTHOR:  Bryon Lawrence
* CREATION DATE:    September 28, 2004
* ORGANIZATION:     OHD-11, HSEB
* MACHINE:
* MODIFICATION HISTORY:
*    DATE         PROGRAMMER        DESCRIPTION/REASON
*    9/28/2004    Bryon Lawrence    Original Coding
*   11/12/2004    Bryon Lawrence    Added get_qpe_fields_array routine
*                                   prototype.
*    March 2007   P Tilles          SRG field changes
*   08/2008       Jingtao Deng      Q2 data
********************************************************************************
*/

#ifndef GET_MPE_PRODUCT_STATE_H
#define GET_MPE_PRODUCT_STATE_H

#include "mpe_field_names.h"

#define MPE_PRODUCT_REPLY_LEN 128

enum MpeBaseRadarList { RMOSAIC_BASE, AVGRMOSAIC_BASE, MAXRMOSAIC_BASE,
                        NUM_BASE_RADAR_MOSAICS };

enum MpeBestProdList  { RMOSAIC,
                        AVGRMOSAIC,
                        MAXRMOSAIC,
                        BMOSAIC,
                        LMOSAIC,
                        GAGEONLY,
                        MMOSAIC,
                        MLMOSAIC ,
                        SATPRE,
                        LSATPRE,
                        SRMOSAIC,
                        SGMOSAIC,
                        SRGMOSAIC,
                        P3LMOSAIC,
                        RFCMOSAIC,
                        RFCBMOSAIC,
                        RFCMMOSAIC,
                        QMOSAIC,
                        LQMOSAIC,
                        MLQMOSAIC,
			
                        RDMOSAIC,
                        AVGRDMOSAIC,
                        MAXRDMOSAIC,
                        BDMOSAIC,
                        LDMOSAIC,
                        MDMOSAIC,
                        MLDMOSAIC,
                        SRDMOSAIC, 
                        SRDGMOSAIC,
            LOCALFIELD1,
			LOCALFIELD2,
			LOCALFIELD3,
                        NUM_BEST_PRODUCTS } ;

/* Added April 2, 2008.  These names correspond to the
   best QPE products above. */
static const char * MpeBestProdNames [ NUM_BEST_PRODUCTS ] =
             { "Radar Mosaic",
                     "Average Radar Mosaic",
                     "Maximum Radar Mosaic",
                     "Field Bias Radar Mosaic",
                     "Local Bias Radar Mosaic",
                     "Gage Only Analysis",
                     "Multi-sensor Mosaic",
                     "Local Bias Multi-sensor Mosaic",
                     "Satellite Precipitation Field",
                     "Local Bias Satellite Precip Field",
                     "Satellite Radar Mosaic",
                     "Satellite Gage Mosaic",
                     "Satellite Radar Gage Mosaic",
                     "Triangulated Local Bias Mosaic",
                     "RFC Mosaic",
                     "RFC Field Bias Mosaic",
                     "RFC Multi-sensor Mosaic",
                     "Raw Q2 Mosaic",
                     "Local Bias Q2 Mosaic",
		     "Multi-sensor Q2 Mosaic", 		   
                     "DP Radar Mosaic",
                     "DP Average Radar Mosaic",
                     "DP Maximum Radar Mosaic", 
                     "DP Field Bias Radar Mosaic",
                     "DP Local Bias Radar Mosaic",
                     "DP Multi-sensor Mosaic",
                     "DP Local Bias Multi-sensor Mosaic",
                     "DP Satellite Radar Mosaic",
                     "DP Satellite Radar Gage Mosaic",
             "Local Field 1", 
		     "Local Field 2", 
		     "Local Field 3"};


/* Added April 3, 2008. These display field types correspond to the
   best QPE products above. */

/* Added April 3, 2008. These display field types correspond to the
   best QPE products above. */
static const enum DisplayFieldData MPEBestDisplayTypes [ NUM_BEST_PRODUCTS ] =
                   {  display_rMosaic,
                      display_avgrMosaic,
                      display_maxrMosaic,
                      display_bMosaic,
                      display_lMosaic,
                      display_gageOnly,
                      display_mMosaic,
                      display_mlMosaic,
                      display_satPrecip,
                      display_lsatPrecip,
                      display_srMosaic,
                      display_sgMosaic,
                      display_srgMosaic,
                      display_p3Mosaic,
                      display_rfcMosaic,
                      display_rfcbMosaic,
                      display_rfcmMosaic,
                      display_qMosaic,
                      display_lqMosaic,
		      display_mlqMosaic,  

	 	      display_rdMosaic,
		      display_avgrdMosaic,
		      display_maxrdMosaic,
		      display_bdMosaic,
		      display_ldMosaic,
		      display_mdMosaic,
		      display_mldMosaic,
              display_srdMosaic,
              display_srdgMosaic,

              display_localField1,
		      display_localField2,
		      display_localField3 };

/* The token names to for the list of mpe products to generate and the
   best qpe. */
#define BESTFIELD_LEN 15
#define MPE_DEL_GAGE_ZEROS_TOKEN "mpe_del_gage_zeros"
#define MPE_GENERATE_LIST_TOKEN  "mpe_generate_list"
#define MPE_QPE_FIELDTYPE_TOKEN  "mpe_qpe_fieldtype"

/* Token which contains which of the radar mosaics to use
   as the base for the MPE products. */
#define MPE_BASE_RADAR_MOSAIC_TOKEN "mpe_base_radar_mosaic"
#define MPE_BASE_RADARDP_MOSAIC_TOKEN "mpe_base_radardp_mosaic"

void get_mpe_product_state ( const char * product , const int * product_len ,
			     const int * verbose , int * state ,
			     int * exit_status ) ;

int isInGenerateList(const char * qpeFieldName);

void get_mpe_qpe_fieldtype (  const int * verbose , char * bestfield ,
		              int * bestfield_len , int * exit_status ) ;

void get_mpe_base_radar ( const int * verbose , char * base_radar_field ,
                          int * baseradar_len , int * exit_status );

void get_mpe_base_radardp ( const int * verbose , char * base_radardp_field ,
                          int * baseradar_len , int * exit_status );
const char ** get_qpe_fields_array (  ) ;

#endif /* #ifndef GET_MPE_PRODUCT_STATE_H */
