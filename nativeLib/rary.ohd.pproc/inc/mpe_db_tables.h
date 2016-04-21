/*******************************************************************************
* FILENAME:              mpe_db_tables.h
*
* DESCRIPTION:         This file contains struct data type for field list
*                      of tables.
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         January 11, 2005
* ORGANIZATION:          HSEB / OHD
* MACHINE:               HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#ifndef MPE_DB_TABLES_H
#define MPE_DB_TABLES_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "DbmsDefs.h"   /* for LOC_ID_LEN */

#include "RWParams.h"
#include "RWBiasStat.h"

/*--------------------------------*/
/*  definition of variables       */
/*--------------------------------*/

typedef struct _gage_record_struct
{
    /* The id of the gage */
    char gageID[ LOC_ID_LEN + 1];
    
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
    /* The id of the radarLoc */
    char radarID[ RADAR_ID_LEN + 1];

    double latitude;

    double longitude;

    double elevation;

    char officeID[ RFC_LEN + 1];

} radarLoc_record_struct;
 
typedef struct _radarLoc_table_struct
{

   radarLoc_record_struct * ptrRadarLocRecords;

   int radarNum;
   
} radarLoc_table_struct; 


/*-----------------------------*/
/*  function prototypes        */
/*-----------------------------*/

#endif /* #ifndef MPE_DB_TABLES_H */
