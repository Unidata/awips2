/*******************************************************************************
* FILENAME:            read_precip_data.h
* GENERAL INFORMATION:
* DESCRIPTION:         This file contains prototype information and
*                      user-defined types for the initialize_data_RFCW,
*                      ReadParameters_RFCW, ReadGageData_RFCW, and
*                      ReadRadarData functions.
*
* ORIGINAL AUTHOR:     Hmap_mpe Team
* CREATION DATE:       February 6, 2002
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE                PROGRAMMER(S)        DESCRIPTION/REASON
* February 6, 2002    Moria S, Bryon L.    Original Coding 
* May, 2003           MarkG		   Rewrote ReadGageData_RFCW() operations
********************************************************************************
*/

#ifndef READ_PRECIP_DATA_H
#define READ_PRECIP_DATA_H

#include "DbmsDefs.h"   /* for LOC_ID_LEN */

/* this structure is used to get the lat-lon of the precip stations */

/*typedef struct
{
   char id[LOC_ID_LEN + 1];
   double lat;
   double lon;
} latlon_list_struct; */


/* protos */

void initialize_data_RFCW ( ) ;
void free_loc_latlon_list ( ) ;
void ReadParameters_RFCW ( ) ;
void ReadGageData_RFCW (  ) ;
void ReadRadarData ( ) ;

#endif /* #ifndef READ_PRECIP_DATA_H */
