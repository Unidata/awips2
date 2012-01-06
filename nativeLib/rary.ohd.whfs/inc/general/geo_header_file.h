/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#ifndef GEO_HEADER_H
#define GEO_HEADER_H

#include <stdio.h>
#include "List.h"

#define MAX_RECORD_LEN 240
#define MAX_PTS 12000


/* arbitrary identifiers for tables; used for the geo tables */

#define GEOLINE 14
#define GEOAREA 15

#define UNASSIGNED -1.

typedef struct GeoAreaData {
        Node            node ;
        char            area_id [LOC_ID_LEN + 1] ;
        char            name[ LOC_AREANAME_LEN ] ;
        char            boundary_type[ LONG_CODE_LEN ] ;
        double          interior_lat ;
        double          interior_lon ;
        double *        lon ;
        double *        lat ;
        int             npts ;
        int		save_data_block ;
        List            list ;
} GeoAreaData ;

void check_for_int_latlon ( FILE * log_file ,
                            FILE * infile ,
                            char * str ,
                            int linenum ,
                            double * intlat ,
                            double * intlon ,
                            int * save_data_block ) ;

void parse_geo_header ( char buf [ MAX_RECORD_LEN ] ,
                        FILE * infile ,
                        FILE * log_file ,
                        int linenum ,
                        int geotable ,
                        int * save_data_block ,
                        char id [ LOC_ID_LEN + 1 ] ,
                        char name [ LOC_NAME_LEN + 1 ] ,
                        int * npts ,
                        int * order ,
                        double * intlat ,
                        double * intlon ) ;

GeoAreaData * read_geodata(FILE   * infile ,
                 char   * geotype ,
                 int    rank ,
                 int    geotable ) ;

void freeGeoAreaData (GeoAreaData  ** pGeoAreaDataHead ) ;

void exit_geoutil_file(FILE *filePtr) ;
#endif /* #ifndef GEO_HEADER_H */
