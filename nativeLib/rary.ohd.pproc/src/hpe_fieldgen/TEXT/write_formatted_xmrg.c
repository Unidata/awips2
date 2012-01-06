/*******************************************************************************
* FILENAME:            write_formatted_xmrg.c
*
* Purpose:
* This function writes out XMRG data in different formates
*
* calling function: run_XXXMosaic
* functions called: save_empe_grib, saveNetCDF, saveGif
*
* input variables
*
* pMPEParams - static parameters
*
* pGeoData - global HRAP lowerleft-corner bin and dimension
*           and dimension of the RFC estimation domain 
*
* strDateTime - date/time 
*
* fname_mosaic - the file name of the mosaic data
* 
* save_grib_token - save grib token string ("save"/"nosave")
*
* save_gif_token - save gif token string ("save"/"nosave")
*
* gif_dir_token - gif output directory token string
*
* gif_id_token - gif id token string
* 
* save_netcdf_token - save netCDF token string ("save"/"nosave")
*
* netcdf_dir_token - netCDF output directory token string
*
* netcdf_id_token - netCDF id token string
*
* save_jpeg_token - save jpeg token string ("save"/"nosave")
*  
* pMosaic - the mosaic of radars.
*
* output variables
*
* none
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   May 2007     Guoxian Zhou      First version 
*
********************************************************************************
*/

#include "empe_fieldgen.h"
#include "save_empe_grib.h"

void writeFormattedXMRG(const empe_params_struct * pEMPEParams,
                        const geo_data_struct * pGeoData,
                        const char * mosaic_dir,
                        const char * fname_mosaic,
                        const char * proc_flag ,
                        const char * save_grib_token,
                        const char * save_gif_token,
                        const char * gif_dir_token,
                        const char * gif_id_token,
                        const char * save_netcdf_token,
                        const char * netcdf_dir_token,
                        const char * netcdf_id_token,
                        const char * save_jpeg_token,
                        double ** pMosaic)
{

    static char fname_xmrg[PATH_LEN] = {'\0'} ; 
    static char strDateTime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'};

    static char netcdf_dir[PATH_LEN] = {'\0'} ; 
    static char netcdf_id[FNAME_LEN] = {'\0'} ;

    static char gif_dir[PATH_LEN] = {'\0'} ; 
    static char gif_id[FNAME_LEN] = {'\0'} ;

    static char fname_grib[FNAME_LEN] = {'\0'} ;

    static char save_file_path[PATH_LEN + FNAME_LEN] = {'\0'} ;

    long int irc ;

    char saveflag[TOKEN_LEN]= {'\0'};    
    char * pGribCommand = NULL;

    int i;
    int size = strlen(fname_mosaic);
    for(i = 0; i < size; i++)
    {
    	if( isdigit(fname_mosaic[i]))
    	{
    		break;
    	}
    }

    strncpy(strDateTime, fname_mosaic + i, size - i - 1);

    /*
     * if save_grib is found as "ON",
     * then create and save grib image.
     */

    hpe_fieldgen_getAppsDefaults(save_grib_token, saveflag) ;

    if(strcmp(hpe_fieldgen_toLowerCase(saveflag), "save") == 0) 
    {
        sprintf(fname_xmrg, "%s", fname_mosaic);
        sprintf(fname_grib, "%s.grib", fname_xmrg);

        pGribCommand = save_hpe_grib(fname_xmrg, fname_grib, proc_flag);

        if ( pGribCommand != NULL )
        {
            sprintf ( message, "STATUS: process_hpe_grib_files script called " 
                             "using command '%s'.", pGribCommand );           
            hpe_fieldgen_printMessage( message );
            free ( pGribCommand );
            pGribCommand = NULL;
        }
    }

    /*
     * if save_netcdf is found as "ON",
     * then create and save netCDF image.
     */

    hpe_fieldgen_getAppsDefaults(save_netcdf_token, saveflag) ;

    if(strcmp(hpe_fieldgen_toLowerCase(saveflag), "save") == 0)
    {

        /*
         * read directory for netCDF file.
         */

        if(hpe_fieldgen_getAppsDefaults(netcdf_dir_token, netcdf_dir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"%s\". netCDF file is skipped.",
                netcdf_dir_token) ;
            printLogMessage( message );
        }
        else
        {
            /*
             * build netcdf file name.
             */

            sprintf(fname_xmrg, "%s/%s", mosaic_dir, fname_mosaic);
            hpe_fieldgen_getAppsDefaults(netcdf_id_token, netcdf_id) ;

            if(strlen(netcdf_id) > 0)
            {
                sprintf(save_file_path, "%s%s.nc", netcdf_id, fname_mosaic);
            }
            else
            {
                sprintf(save_file_path, "%s.nc", fname_mosaic);
            }
                
            saveNetCDF( pGeoData->num_cols, pGeoData->num_rows, strDateTime, 
                 save_file_path, pMosaic, proc_flag, netcdf_dir, &irc) ;
    
            if(irc == 1)
            {
                sprintf ( message , "malloc for precip array failed "
                          " netCDF file not saved" );
                hpe_fieldgen_printMessage( message );
            }
            else if(irc == 2)
            {
                sprintf ( message , "error reading %s "
                         " netCDF file not saved", save_file_path);
                hpe_fieldgen_printMessage( message );
            }
            else if(irc == 3)
            {
                sprintf ( message , "error from nc_create routine "
                          " netCDF file not saved");
                hpe_fieldgen_printMessage( message );
            }
        }
    }

    /**
     * if save_gif is found as "ON",
     * then create and save gif image.
     **/

    hpe_fieldgen_getAppsDefaults(save_gif_token, saveflag) ;

    if(strcmp(hpe_fieldgen_toLowerCase(saveflag), "save") == 0)
    {
        if(pEMPEParams->irc_load_stat == 0)
        {

            /*
             * read directory for gif file.
             */

            if(hpe_fieldgen_getAppsDefaults(gif_dir_token, gif_dir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"%s\". GIF file is skipped.",
                    gif_dir_token) ;
                printLogMessage( message );
            }
            else
            {
                hpe_fieldgen_getAppsDefaults(gif_id_token, gif_id) ;

                if(strlen(gif_id) > 0 )
                {
                    sprintf(save_file_path, "%s/%s%s.gif",
                        gif_dir, gif_id, fname_mosaic);
                }
                else
                {
                    sprintf(save_file_path, "%s/%s.gif",
                        gif_dir, fname_mosaic);
                }
    
                saveGif( pGeoData, strDateTime, save_file_path, pMosaic, &irc) ;
    
                if(irc != 0)
                {
                    sprintf ( message , "ERROR: attempting to write file = %s",
                        save_file_path);
                    hpe_fieldgen_printMessage( message ); 
                }
                else
                {
                    sprintf ( message , "STATUS: file written to: %s",
                        save_file_path);
                    hpe_fieldgen_printMessage( message ); 
                }
            }
        }
    }

    /*
     * if save_jpeg is found as "ON",
     * then create and save jpeg image.
     */

    hpe_fieldgen_getAppsDefaults(save_jpeg_token, saveflag) ;

    if(strcmp(hpe_fieldgen_toLowerCase(saveflag), "save") == 0)
    {
        /*
         * temporary no function call to create jpeg file.
         */

        sprintf ( message , "STATUS: \'%s\' is ON but jpeg image not available"
                            " - use gif", save_jpeg_token );
        hpe_fieldgen_printMessage( message );
    }
}
