/*.......................................
* File: get_dhm_settings.c
* Author(s): A. Vo, A. Voellmy, L. Cajina, W. Kwock
* Date Created: 5/5/06
* Development group: OHD HSEB
* Purpose: This subroutine get token values for DHM-OP operation for fcst program
* Module(s): get_dhm_settings
*.......................................
*  input  - None                                                                          *
*  output - char *precipDataPath - directory of gridded precipitation data                *
*  output - char *modelDataPath - directory of DHM model data                             *
*  output - char *d2dDataPath       - directory for output D2D data                       *
*  output - char *dhmNotifyDataPath       - directory of dhmNotify exe (ping D2D)         *
*  output - char *geoCoordDataPathAndFileName - directory with coord file (rfc boundaries *
*  output - char *precipXmrgSrcPath - source directory with precip xmrg files             *
*  output - char *modelDataSrcPath - source directory with dhm data files                 *
*
*
* Date Modified: 
* 10/02/06  added a string argument for goecoordinate path                 
*                          and filename. Also removed the all length       
*                          arguments                                       
* 03/07     added a string argument for precip xmrg src dir; directory with precip 
*            xmrg files; when running DHM througth OFS/FCST precip xmrg src dir =
*	    precipDataPath  no need to copy xmrg data
*
* 04/07     added a string argument for dhm model data src dir; directory with dhm 
*           data files (parameters, states, pet, Connectivity); when running DHM througth OFS/FCST 
*	    dhm data src dir = modelDataPath (no need to copy data)
* 
* 04/07     added gridOutputConfigurationPath (not used when running DHM through fcst)
* 09/07     added use rain+melt flag: 1 use snow-17 rain+melt precip grid to ingest into SAC-SMA model; 0 use ofs_grid
*           (MPE precip grid)
*/

#include "dhm.h"

void get_dhm_settings(
                   char *precipDataPath, 
                   char *modelDataPath, 
                   char *d2dDataPath, 
                   char *dhmNotifyDataPath,
                   char *geoCoordDataPathAndFileName,
		   char *precipXmrgSrcPath,
		   char *modelDataSrcPath,
		   char *gridOutputConfigurationPath,
		   int *useRainPlusMelt)
{
    static const char *ascii = "ascii";
    static const char *coord = "coord";
    static char *token_griddb = "ofs_griddb_dir";
    static char *token_dhm = "dhm_data_dir";
    static char *token_d2d = "dhm_d2d_data_dir";
    static char *token_dhmnotify = "dhm_d2d_notify_bin_dir";
    static char *token_ifprfc = "ifp_rfc";
    static char *token_geodata = "geo_data";
    static char *token_rdhm_grid = "dhm_rain_plus_melt_data_dir";
    char ifprfcName[20];
    char geodataPath[MAX_DIRECTORY_NAME_LENGTH];
    int tokenlen;
    int returnValueLength;
    if( *useRainPlusMelt ){
       tokenlen=strlen(token_rdhm_grid);
       get_apps_defaults(token_rdhm_grid, &tokenlen, precipDataPath,
       &returnValueLength);           
    }
    else{    
       tokenlen=strlen(token_griddb);
        get_apps_defaults(token_griddb, &tokenlen, precipDataPath,
        &returnValueLength); 
    }
    
    precipDataPath[returnValueLength]='\0';
    /* precip xmrg src dir is equal to xmrg precip path when running through ofs/fcst */    
    strcpy(precipXmrgSrcPath,precipDataPath);
	
    tokenlen=strlen(token_dhm);
    get_apps_defaults(token_dhm, &tokenlen, modelDataPath,
        &returnValueLength);
    
    modelDataPath[returnValueLength]='\0';
    /* dhm data src dir is equal to modelDataPath when running through ofs/fcst */
    strcpy(modelDataSrcPath,modelDataPath);
   
    tokenlen=strlen(token_d2d);
    get_apps_defaults(token_d2d, &tokenlen, d2dDataPath,
        &returnValueLength);
    d2dDataPath[returnValueLength]='\0';
   
    tokenlen=strlen(token_dhmnotify);
    get_apps_defaults(token_dhmnotify, &tokenlen, dhmNotifyDataPath,
        &returnValueLength);
    dhmNotifyDataPath[returnValueLength]='\0';
    
    tokenlen = strlen(token_ifprfc);
    get_apps_defaults(token_ifprfc,&tokenlen, ifprfcName, &returnValueLength);
  
    tokenlen = strlen(token_geodata);
    get_apps_defaults(token_geodata, &tokenlen, geodataPath, &returnValueLength);
    
    geoCoordDataPathAndFileName[0] = '\0';    
    sprintf(geoCoordDataPathAndFileName,"%s/%s/%s/%s_%s.dat", geodataPath, ifprfcName, 
                                                          ascii, coord, ifprfcName);
							  
    /* ofs does not use gridOutputConfigurationPath. Set to N/A*/
    gridOutputConfigurationPath[0]='\0';
    strcpy(gridOutputConfigurationPath,"N/A");

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
