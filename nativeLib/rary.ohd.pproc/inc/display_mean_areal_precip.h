/*******************************************************************************
* FILENAME:            display_mean_areal_precip.h
* DESCRIPTION:         Contains the prototypes and structure definitions
*                      for the routines used to compute the mean
*                      areal precipitation for the different types of areas
*                      such as basins, zones, counties.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       July 13, 2002
* ORGANIZATION:        OHD / HSEB  
* MACHINE:             HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
* DATE             PROGRAMMER             DESCRIPTION/REASON
* 7/13/2002        Bryon Lawrence         Original Coding
********************************************************************************
*/

#ifndef DISPLAY_MEAN_AREAL_PRECIP_H
#define DISPLAY_MEAN_AREAL_PRECIP_H

#include "DbmsDefs.h"
#include "List.h"
#include "get_geoareainfo.h"

#define DEFAULT_COVERAGE 0.80
#define DEBUG 1

struct MeanArealPrecip
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

int display_mean_areal_precip ( int ** data_array , int xor ,
                                int yor , int xsize , int ysize ,
                                const char * area_type ) ; 

void drawMultiHourGeoAccumLabels ( ) ;
void mean_precip_showCB ( Widget w, XtPointer ptr, XtPointer cbs) ;
void turnOnMultiHourValues ( ) ;
void turnOffMultiHourValues ( ) ;
void turnOnMultiHourIds ( ) ;
void turnOffMultiHourIds ( ) ;
int isThereMultiHourIds ( ) ;
int isThereMultiHourValues ( ) ;
void free_mean_areal_precip ( ) ;

#endif /* #ifndef DISPLAY_MEAN_AREAL_PRECIP_H */
