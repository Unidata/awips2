
#include "dqc_preproc_setup.h"

const char * STATION_CLIMO_FILE = "station_climo_list";

void openClimoOutputFile (const char * path, const char * areaName )
{
    bool is_slash = false;
    char * pFilePath = NULL;
    int filename_len;
    int path_len;
    int filepath_len;

    char fileName[FNAME_LEN] = {'\0'};

    if(ptrClimoFile != NULL)
    {
        return;
    }

    if ( strlen(areaName) > 0)
    {
        sprintf ( fileName, "%s_%s", areaName , STATION_CLIMO_FILE);
    }
    else
    {
        sprintf ( fileName, "%s", STATION_CLIMO_FILE );
    }

    /* 
     * Build the station file path.
     */
    filename_len = strlen ( fileName );
    path_len = strlen ( path );
 
    if ( path [ path_len - 1 ] != '/' )
    {
        ++ path_len;
    }
    else
    {
        is_slash = true;
    }

    filepath_len = path_len + filename_len + 1;

    pFilePath = ( char * ) malloc ( filepath_len * sizeof ( char ) );

    if ( pFilePath == NULL )
    {
        fprintf ( stderr, "In routine 'openClimoOutputFile': "
                        "dynamic memory allocation failure.\n" );
        exit(-1) ;
    }

    if ( is_slash  && strlen(dqc_preproc_setup_mpe_site_id) > 0)
    {
        sprintf ( pFilePath, "%s%s", path, fileName );
    }
    else
    {
        sprintf ( pFilePath, "%s/%s", path, fileName );
    }

    /* 
     * Attempt to open the file for writing.
     */
    ptrClimoFile = fopen ( pFilePath, "w" );

    if ( ptrClimoFile == NULL )
    {
        if ( pFilePath != NULL )
        {
            free ( pFilePath );
            pFilePath = NULL; 
        }

        fprintf ( stderr, "In routine 'openClimoOutputFile': "
                        "Could not open file %s.\n", pFilePath );
        exit(-1) ;
    }

    if ( pFilePath != NULL )
    {
        free ( pFilePath );
        pFilePath = NULL;
    }

    return ;
}
