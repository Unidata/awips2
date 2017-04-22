/*******************************************************************************
* FILENAME:              dqc_preproc_setup.h
*
* DESCRIPTION:         This file contains parameters and
*                      user-defined types for the mpe_editor_setup program.
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         Feburary 15, 2006
* ORGANIZATION:          HSEB / OHD
* MACHINE:               Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#ifndef MPE_EDITOR_SETUP_H
#define MPE_EDITOR_SETUP_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <ctype.h>

#include "DbmsDefs.h"
#include "List.h"
#include "IngestFilter.h"
#include "convert_hrap.h" 
#include "read_xmrg.h"
#include "Swap2Bytes.h"

#include "rfcwide.h"
#include "HydroStatus.h"

/*--------------------------------*/
/*  definition of constants       */
/*--------------------------------*/

#define PATH_LEN	256
#define FILE_LEN	512
#define FNAME_LEN	24
#define GAGE_ID_LEN	8
#define SITENAME_LEN	8
#define DEFAULT_GAGETYPE 9
#define TOKEN_LEN	128
#define DATA_STRING_LEN	256
#define CLIMO_DEFAULT	-99.0

static const char * prismTypeSource="PB";

/*--------------------------------*/
/*  definition of struct types    */
/*--------------------------------*/
typedef struct
{
	int hrap_x;
	int hrap_y;
	int num_cols;
    int num_rows;
} geo_data_struct; 

typedef struct 
{
    char lid [ GAGE_ID_LEN + 1];
    char pe [ SHEF_PE_LEN+1 ];
    char ts[3];
    double lat;
    double lon;
} station_info;

/*--------------------------------*/
/*  definition of variables       */
/*--------------------------------*/

extern const char * mon[];

extern geo_data_struct * ptrGeoData ;

extern station_info * ptrPrecipInfo ;
extern station_info * ptrTempInfo ;
extern int dqc_preproc_setup_precip_count ;
extern int dqc_preproc_setup_temperature_count ;

extern char  dqc_preproc_setup_mpe_station_list_dir[PATH_LEN];
extern char  mpe_climo_dir[PATH_LEN];
extern char  mpe_prism_dir[PATH_LEN] ;
extern char  dqc_preproc_setup_mpe_site_id[SITENAME_LEN] ;

extern FILE * ptrClimoFile ;

/*-----------------------------*/
/*  function prototypes        */
/*-----------------------------*/

int getAppsDefaults(const char* strToken, char* strTokenValue);
void loadAppsDefaults();

int buildLocalHrap(const geo_data_struct * pGeoData,
                    double lat, double lon,
                    int * irow, int * icol) ;

void qdc_preproc_setup_readGeoData();
int read_station_info (const char *path, const char *areaName) ;
void readClimoData() ;

void openClimoOutputFile (const char * path, const char * areaName ) ;

void processClimoData ();

void memoryRelease();

#endif /* #ifndef MPE_EDITOR_SETUP_H */

