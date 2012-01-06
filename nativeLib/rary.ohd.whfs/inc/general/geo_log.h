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

#ifndef GEO_LOG_H
#define GEO_LOG_H

#include <stdio.h>

void log_geomsg ( char * msgstr ,
                  FILE * log_file ) ;

void open_geolog ( int    geotable ,
                  char   * geotype ,
                  int    rank ,
                  FILE   ** log_file ) ;

#endif /* #ifndef GEO_LOG_H */
