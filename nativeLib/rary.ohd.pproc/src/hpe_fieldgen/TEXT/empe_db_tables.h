/*******************************************************************************
* FILENAME:            empe_db_tables.h
*
* DESCRIPTION:         This file contains struct data type for field list
*                      of tables.
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         January  2007
* ORGANIZATION:          HSEB / OHD
* MACHINE:               HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#ifndef EMPE_DB_TABLES_H
#define EMPE_DB_TABLES_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "DbmsDefs.h"   /* for LOC_ID_LEN */

#include "RWBiasStat.h"
#include "RWParams.h"

/*--------------------------------*/
/*  definition of variables       */
/*--------------------------------*/

typedef struct _gage_record_struct
{
    
    char gageID[ LOC_ID_LEN + 1];  /* The id of the gage */
    
    char gageTS[ SHEF_TS_LEN + 1];    
    
    double gageValue;
    
    /* The HRAP coordinate */

    int hrap_x;
    int hrap_y;

    double latitude;
    double longitude;

} gage_record_struct;


typedef struct _gage_table_struct
{
    gage_record_struct * ptrGageRecords;
    
    int pseudoGageNum;
    int totalGageNum;
    
} gage_table_struct;

typedef struct _radarLoc_record_struct
{
    
    char radarID[RADAR_ID_LEN + 1]; /* The radar id id */

    double latitude;

    double longitude;

    double elevation;

} radarLoc_record_struct;
 
typedef struct _radarLoc_table_struct
{

   radarLoc_record_struct * ptrRadarLocRecords;

   int radarNum;
   
} radarLoc_table_struct; 

#endif /* #ifndef EMPE_DB_TABLES_H */
