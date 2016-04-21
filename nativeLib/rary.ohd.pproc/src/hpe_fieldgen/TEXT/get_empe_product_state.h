/*******************************************************************************
* FILENAME:     get_empe_product_state.h
* DESCRIPTION:  Contains the prototype for the get_empe_product_state,
*               get_empe_qpe_fieldtype, and get_qpe_fields_array routines.  
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
********************************************************************************
*/

#ifndef GET_EMPE_PRODUCT_STATE_H 
#define GET_EMPE_PRODUCT_STATE_H

#define EMPE_PRODUCT_REPLY_LEN 128

enum EmpeBaseRadarList { ERMOSAIC_BASE, AVGERMOSAIC_BASE,
                         MAXERMOSAIC_BASE, NUM_EMPE_BASE_RADAR_MOSAICS };
enum EmpeBestProdList  { DHRMOSAIC, BDHRMOSAIC, 
                         ERMOSAIC, AVGERMOSAIC, MAXERMOSAIC, P3LMOSAIC, MMOSAIC,
                         EBMOSAIC, LMOSAIC, MLMOSAIC, GAGEONLY, SATPRE,
                         LSATPRE, NUM_EMPE_BEST_PRODUCTS } ; 

/* The token names to for the list of mpe products to generate and the
   best qpe. */
#define BESTFIELD_LEN 12
#define EMPE_DEL_GAGE_ZEROS_TOKEN "hpe_del_gage_zeros"
#define EMPE_GENERATE_LIST_TOKEN  "hpe_generate_list"
#define EMPE_QPE_FIELDTYPE_TOKEN  "hpe_qpe_fieldtype"

/* Token which contains which of the radar mosaics to use 
   as the base for the MPE products. */
#define EMPE_BASE_RADAR_MOSAIC_TOKEN "hpe_base_radar_mosaic"

void get_empe_product_state ( const char * product , const int * product_len ,
			     const int * verbose , int * state , 
			     int * exit_status ) ;

void get_empe_qpe_fieldtype (  const int * verbose , char * bestfield , 
		              int * bestfield_len , int * exit_status ) ;  

void get_empe_base_radar ( const int * verbose , char * base_radar_field ,
                          int * baseradar_len , int * exit_status );

const char ** get_empe_qpe_fields (  ) ;

#endif /* #ifndef GET_EMPE_PRODUCT_STATE_H */
