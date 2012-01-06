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
* cem     - month data string like "01".
*
* output variables
*
* umeang  - the PRISM data
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   April 2005   Guoxian Zhou      finish conversion to C Language 
*
*********************************************************************************/

#include <sys/stat.h>
#include <sys/types.h>

#include "empe_fieldgen.h"

void get_climate(const char * os, const int rowSize, const int colSize,
            const char * cem, double ** umeang)
{
    static char    mosaicDir[PATH_LEN] = {'\0'} ;
    static int    first = 1 ;
    char    mosaicName[FILE_LEN] = {'\0'} ;
    int i, j, lenfn ;
    double factor = 1.0 ;
    int irc = 0 ;

    struct stat statInfo;
    int status;

    if(first == 1)
    {
        hpe_fieldgen_getAppsDefaults("rfcwide_prism_dir", mosaicDir);
        first = 0 ;
    }

    sprintf(mosaicName, "%s/PRISM_%s", mosaicDir, cem ); 

     /* Check to determine if the PRISM file exists and is readable. */

     status = stat ( mosaicName, & statInfo );

     if ( ( status != 0) || !( statInfo.st_mode & S_IFREG ) )  
     {
        sprintf ( message , "WARNING: file = \"%s\" not found"
                  "    - array of 1.0 used.", mosaicName) ;
        hpe_fieldgen_printMessage( message );

        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                umeang[i][j] = 1.0 ;
            }
        }
     }
     else
     {
        lenfn = strlen(mosaicName); 

        readxmrg(os, rowSize, colSize, mosaicName,
            lenfn, factor, umeang, &irc) ;

        if(irc != 0)
        {
            for(i = 0; i < rowSize; i ++)
            {
                for(j = 0; j < colSize; j ++)
                {
                    umeang[i][j] = 1.0 ;
                }
            }
        }
    }
}
