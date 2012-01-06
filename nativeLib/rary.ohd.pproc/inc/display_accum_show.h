/*******************************************************************************
* FILENAME:             display_accum_show.h
* GENERAL INFORMATION:  
* DESCRIPTION:          Contains prototypes and definitions for the routines
*                       that implement the Multi-Hour Precipitation
*                       Accumulation GUI callbacks.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        July 10, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   7/10/2002    Bryon Lawrence    Original Coding
********************************************************************************
*/

#ifndef DISPLAY_ACCUM_SHOW_H
#define DISPLAY_ACCUM_SHOW_H

#include <Xm/Xm.h>

#include "GeoArea.h"
#include "stage3.h"  /* For date_struct structure information. */

#define DEBUG 1

enum PrecipAccumArea { PrecipGrid , PrecipBasin , PrecipCounty , PrecipZone , NumPrecipAreaItems } ;

enum PrecipAccumInterval { Accum1 = 1 , Accum3 = 3 , Accum6 = 6 , 
                           Accum12 = 12 , Accum24 = 24 , Accum36 = 36 , 
                           Accum48 = 48 , Accum72 = 72 ,  AccumOther } ;

enum PrecipEndTimeOper { EndTimeDayIncrement , EndTimeDayDecrement ,
                         EndTimeHourIncrement , EndTimeHourDecrement ,
                         EndTimeNoOperation } ; 

extern char * AccumAreaTypes [ NumPrecipAreaItems ] ;

date_struct get_multi_hour_date_info ( ) ;
int get_multi_hour_interval_val ( ) ;
void precip_accum_show ( Widget w ) ;
void set_multi_hour_interval_val ( int value ) ;
void set_multi_hour_endtime_val ( int value ) ;
void mean_precip_showCB(Widget w, XtPointer ptr, XtPointer cbs) ;
void set_base_time ( const date_struct * pDate );

#endif  /* #ifndef DISPLAY_ACCUM_SHOW_H */
