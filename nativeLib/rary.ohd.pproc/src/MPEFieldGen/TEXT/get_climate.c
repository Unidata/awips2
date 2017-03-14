/*******************************************************************************
* FILENAME:            get_climate.c
*
* Purpose:
* This function is converted from FORTRAN code: get_climate.f
* This function reads in prism data
*
* calling function: 
* functions called: 
*
* input variables
*
* os      - operation system like "LX".
* rowSize - dimension of site in y direction.
* colSize - dimension of site in x direction.
* mon     - month data.
*
* output variables
*
* umeang  - the PRISM data
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   April 2005   Guoxian Zhou      finish conversion to C Language 
*   May 25 2006  Guoxian Zhou      Change the PRISM file name 
*
*********************************************************************************/

#include "mpe_fieldgen.h"
#include <sys/stat.h>
#include <sys/types.h>

void MPEFieldGen_get_climate(const char * os,
                 const int rowSize, const int colSize,
                 const int mon, double ** umeang)
{
    static char mosaicDir[PATH_LEN] = {'\0'} ;
    static int first = 1 ;
    char mosaicName[FILE_LEN] = {'\0'} ;
    int i, j, lenfn ;
    double factor = 1.0 ;
    int irc = 0 ;

    char * Months[12] = {"jan", "feb", "mar", "apr", "may", "jun",
                         "jul", "aug", "sep", "oct", "nov", "dec"};

    struct stat statInfo;
    int status;

    /**
     * the token mpe_site id may be the same value as the 
     * the token rfcw_rfcname.
     * may use the value passed from the parameter struct late.
     * gzhou 05/25/2006
     */
    static char siteID[RFC_NAME_LEN + 1] = {'\0'} ;

    if(first == 1)
    {
        getAppsDefaults("rfcwide_prism_dir", mosaicDir);
        getAppsDefaults("mpe_site_id", siteID);
        first = 0 ;
    }

    sprintf(mosaicName, "%s/prism_mean_precip_%s_%s",
                       mosaicDir, siteID, Months[mon] );

     /* Check to determine if the PRISM file exists and is readable. */
     status = stat ( mosaicName, & statInfo );

     if ( ( status != 0) || !( statInfo.st_mode & S_IFREG ) )  
     {
        sprintf ( message , "WARNING: PRISM file = \"%s\" not found"
                  " -- array of 1.0 used.", mosaicName) ;
        printMessage( message, logFile );

        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
                umeang[i][j] = 1.0 ;
        }
     }
     else
     {
        lenfn = strlen(mosaicName); 

        MPEFieldGen_readxmrg(os, rowSize, colSize, mosaicName,
            lenfn, factor, umeang, &irc) ;

        if(irc != 0)
        {
            for(i = 0; i < rowSize; i ++)
            {
                for(j = 0; j < colSize; j ++)
                    umeang[i][j] = 1.0 ;
            }
        }
    }
}
