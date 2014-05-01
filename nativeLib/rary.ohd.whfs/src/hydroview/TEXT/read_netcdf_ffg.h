/*******************************************************************************
* FILENAME:              read_netcdf_ffg.h
*
* GENERAL INFORMATION:   Contains the function declarations for the
*                        "read_netcdf_ffg" and "ffconv" routines.
*                        Also contains the definitions of constants
*                        required by these routines. 
*
* ORIGINAL AUTHOR:       Bryon Lawrence
* CREATION DATE:         April 30, 2002
* ORGANIZATION:          HSEB / OHD
* MACHINE:               HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#ifndef READ_NETCDF_FFG_H
#define READ_NETCDF_FFG_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <Xm/Xm.h> /* For Boolean definition. */

#include "HvDisplayControlDefs.h"
#include "HydroStatus.h"

/*--------------------------------*/
/*  definition of variables       */
/*--------------------------------*/

/*   variables used in function ffconv  */

#define lower_thres  201
#define upper_thres  253
#define mfactor 6
#define miss_value_float -99.
#define fill_value_float -98.
#define DEFAULT_FFG_COVERAGE 0.80
#define AREAL_VAL_TOGGLE_BUTTON 0
#define AREAL_ID_TOGGLE_BUTTON 1

/*   variables used in function mosaic_qc  */

#define miss_value_uchar 0 
#define fill_value_uchar 255 

enum FfgMode { GRID_MODE , AREAL_MODE , NUM_FFG_MODES } ;

enum FfgGridArea { WFO_FFG , RFC_FFG , NUM_FFG_SOURCES } ;

enum FfgRfcIds { ABRFC , AKRFC , CBRFC , CNRFC , LMRFC , MARFC ,
                 MBRFC , NCRFC , NERFC , NWRFC , OHRFC , SERFC ,
                 WGRFC , ALL_RFCS , NUM_FFG_RFCS } ;

enum FfgDurationIds { FFG_1HR , FFG_3HR , FFG_6HR , FFG_12HR, FFG_24HR ,
                      ALL_FFG_DURATIONS , NUM_FFG_DURATIONS } ;

enum FfgArealTypes { FFG_ALL , FFG_GRID , FFG_BASIN , FFG_COUNTY , FFG_ZONE ,
                     NUM_FFG_DISPLAY_AREAS } ;

struct MeanArealFFG
{
   Node node ;  /* The node. */
   char area_id [ LOC_ID_LEN + 1 ] ; /* The id of the area being processed. */
   char name [ LOC_AREANAME_LEN + 1 ] ; /* The name of the area being
                                           processed. */
   int numrows ; /* The number of rows in the geoarea being processed. */
   long * rows ; /* The numbers of the HRAP rows in the area being
                    processed. */
   long * beg_cols ; /* The beginning HRAP column corresponding to each row. */
   long * end_cols ; /* The ending HRAP column corresponding to each row. */
   List list ;       /* The linked list. */
   float avg_val ; /* The average precipitation value in the area being
                      processed. */
   float max_val ; /* The maximum precipitation value in the area being
                      processed. */
   float min_val ; /* The minimum precipitation value in the area being
                      processed. */
   float area_covered ; /* The percentage of the area covered by valid
                           precipitation values. */
} ;

/*-----------------------------*/
/*  function prototypes        */
/*-----------------------------*/

void free_ffg_grids ( ) ;
void free_ffg_linesegs ( ) ;
Boolean getFfgIdState ( ) ;
Boolean getFfgValueState ( ) ;
int isThereFfgDataToDraw ( ) ;
void drawFfgHrapGrid ( HvDisplayControl * hdc ) ;
HydroStatus read_netcdf_ffg ( char * filename , 
                              enum FfgArealTypes basin_flag , 
                              int duration ) ;
void turnOffFfgData ( ) ;
void SetFfgMode ( enum FfgMode mode ) ;
void read_areal_ffg ( char * boundary_type ,
                      int ffg_hours ,
                      time_t ticks ,
                      int since_flag ) ;

void toggle_areal_annotations ( int toggle_button_id , Boolean flag ) ;

#endif /* #ifndef READ_NETCDF_FFG_H */
