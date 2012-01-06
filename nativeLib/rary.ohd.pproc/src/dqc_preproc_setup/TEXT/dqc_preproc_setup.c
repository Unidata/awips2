
/*******************************************************************************
* FILENAME:  dqc_preproc_setup.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*	MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR: 	    Guoxian ZHou
* CREATION DATE:		Feb 24, 2006
* ORGANIZATION: 		OHD11/HSEB
* MACHINE:				Linux
*
* MODIFICATION HISTORY:
*	MODULE #  		DATE			PROGRAMMER  		DESCRIPTION/REASON
*  	1               Feb 24 2006     Guoxian Zhou        First version
*   1               Mar 08 2006     Guoxian Zhou        rewrite to use master
*                                                       station list.
********************************************************************************
*/
#include "dqc_preproc_setup.h"

#define USAGE printf ( "Usage:\n"  				\
  						" dqc_preproc_setup  "	\

const char *mon[] = {"jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"};

char  dqc_preproc_setup_mpe_station_list_dir[PATH_LEN] = {'\0'} ;
char  mpe_climo_dir[PATH_LEN] = {'\0'} ;
char  mpe_prism_dir[PATH_LEN] = {'\0'} ;
char  dqc_preproc_setup_mpe_site_id[SITENAME_LEN] = {'\0'} ;

FILE * ptrClimoFile = NULL;

geo_data_struct * ptrGeoData = NULL ;

station_info * ptrPrecipInfo = NULL ;
station_info * ptrTempInfo = NULL ;

int dqc_preproc_setup_precip_count = 0;
int dqc_preproc_setup_temperature_count = 0;

int dqc_preproc_setup_main ( int argc, const char ** argv)
{
	/*
	 * Load the apps default tokens.
	 */
	loadDqcAppsDefaults();

	/*
	 * Load the geographic data.
	 */
	qdc_preproc_setup_readGeoData() ;

	/*
	 * Load the station info list for PPD and TAI.
	 */
	read_station_info (dqc_preproc_setup_mpe_station_list_dir, dqc_preproc_setup_mpe_site_id) ;

	/*
	 * Process the Precip, Max/Min Temperature data.
	 */
	processClimoData (mpe_climo_dir, dqc_preproc_setup_mpe_site_id );

	/*
	 * Free dynamically allocated memory and close files.
	 */
	memoryRelease();

	printf("\nEnd running dqc_preproc_setup.\n");
	printf("Output file directory for climo list:\n%s\n\n", mpe_climo_dir);

	return 0;
}
