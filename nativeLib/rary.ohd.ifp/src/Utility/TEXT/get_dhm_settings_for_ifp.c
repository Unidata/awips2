/*.......................................
* File: getDHMSettingsForIFP.c
* Author(s): A. Vo, A. Voellmy, L. Cajina, W. Kwock
* Date Created: 5/5/06
* Development group: OHD HSEB
* Purpose: This subroutine get token values for DHM-OP operation for ifp_nwsrfs
*          program
* Module(s): get_dhm_settings
*.......................................*
*  input  - None                                                                          *
*  output - char *precipDataPath - directory of gridded precipitation data                *
*  output - char *modelDataPath - directory of DHM model data                             *
*  output - char *d2dDataPath       - directory for output D2D data                       *
*  output - char *dhmNotifyDataPath       - directory of dhmNotify exe (ping D2D)         *
*  output - char *geoCoordDataPathAndFileName - directory with coord file (rfc boundaries *
*  output - char *precipXmrgSrcPath - source directory with precip xmrg files             *
*  output - char *modelDataSrcPath - source directory with dhm data files                 * 
* Date Modified: 
* 10/02/06  added a string argument for goecoordinate path                 
*                          and filename. Also removed the all length       
*                          arguments                                       
* 03/07     added a string argument for precip xmrg src dir; directory with precip 
*           xmrg files; when running DHM througth IFP precip xmrg src dir =
*	    ofs_griddb_dir
* 04/07     changed modelDataPath to be set using the token ifp_dhm_data_dir; this is where
*           the Runner looks for its dhm data
*          
*           added a string argument for dhm model data src dir; directory with dhm 
*           data files (parameters, states, pet, Connectivity); when running DHM througth IFP
*	    dhm data src dir = dhm_data_dir (possibly need to copy data to model data path);
*           
* 04/07     added getGridOutputConfigurationPath; used to tell runner where file with info. on grids to output is located
* 09/07     added use rain+melt flag: 1 use snow-17 rain+melt precip grid to ingest into SAC-SMA model; 0 use ofs_grid
*           (MPE precip grid)
*/
#include "dhm.h"

void get_dhm_settings(char *precipDataPath, 
		      char *modelDataPath, 
                      char *d2dDataPath, 
                      char *dhmNotifyDataPath, 
                      char *geoCoordDataPathAndFileName,
		      char *precipXmrgSrcPath,
		      char *modelDataSrcPath,
		      char *gridOutputConfigurationPath,
		      int *useRainPlusMelt)
{
    int tokenlen;
    static const char *ascii = "ascii";
    static const char *coord = "coord";
    static const char *ifp_files_dir = ".ifp_files";
    static char *token_ifp_griddb="ifp_griddb_dir";
    static char *token_ofs_griddb="ofs_griddb_dir";
    static char *token_dhm="dhm_data_dir";
    static char *token_ifp_dhm="ifp_dhm_data_dir";
    static char *token_ifprfc="ifp_rfc";
    static char *token_geodata="geo_data";
    static char *token_home="HOME";
    static char *token_rdhm_grid = "dhm_rain_plus_melt_data_dir";
    char ifprfcName[20];  
    char geodataPath[MAX_DIRECTORY_NAME_LENGTH];
    char homePath[MAX_DIRECTORY_NAME_LENGTH];
    int returnValueLength;
    
    tokenlen=strlen(token_ifp_griddb);
    get_apps_defaults(token_ifp_griddb,&tokenlen,precipDataPath,
    &returnValueLength);
    precipDataPath[returnValueLength]='\0';
    
    //the block below added to set source directory for precip xmrgs to "dhm_rain_plus_melt_data_dir"
    if(*useRainPlusMelt){
        tokenlen=strlen(token_rdhm_grid);
        get_apps_defaults(token_rdhm_grid,&tokenlen,precipXmrgSrcPath,
        &returnValueLength);	
    }
    else/*use snow-17 input grid from rdhm_grid*/{
       tokenlen=strlen(token_ofs_griddb);
        get_apps_defaults(token_ofs_griddb,&tokenlen,precipXmrgSrcPath,
        &returnValueLength);
    }
    precipXmrgSrcPath[returnValueLength]='\0';
    
    
    
    tokenlen=strlen(token_ifp_dhm);
    get_apps_defaults(token_ifp_dhm,&tokenlen,modelDataPath,
                      &returnValueLength);
    modelDataPath[returnValueLength]='\0';
   
   
    /*the block below added to set source directory for dhm data to "dhm_data_dir" */
    tokenlen=strlen(token_dhm);
    get_apps_defaults(token_dhm,&tokenlen,modelDataSrcPath,
    &returnValueLength);
    modelDataSrcPath[returnValueLength]='\0';
    
    /*the block below added to set path to gridOutputConfiguration fle (in  $HOME/.ifp_files)" */
    tokenlen=strlen(token_home);
    get_apps_defaults(token_home,&tokenlen,homePath,
    &returnValueLength);
    sprintf(gridOutputConfigurationPath,"%s/%s",homePath,ifp_files_dir);
     
   
   
    /* ifp does not use D2D data. Set D2D datapath to N/A*/
    d2dDataPath[0]='\0';
    strcpy(d2dDataPath,"N/A");    
   
    /* ifp does not use DHM notify Set to N/A*/
    dhmNotifyDataPath[0]='\0';
    strcpy(dhmNotifyDataPath,"N/A");    
  
    tokenlen = strlen(token_ifprfc);
    get_apps_defaults(token_ifprfc,&tokenlen,ifprfcName,
    	&returnValueLength);
    	
    tokenlen = strlen(token_geodata);
    get_apps_defaults(token_geodata,&tokenlen,geodataPath,
    	&returnValueLength);
    	
    geoCoordDataPathAndFileName[0] = '\0';    
    sprintf(geoCoordDataPathAndFileName,"%s/%s/%s/%s_%s.dat", 
                           geodataPath, ifprfcName, ascii, coord, ifprfcName);
   	
   	

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

