
/**********************************************************************
 * read_station_info ( )
 * 
 * This function reads the station info from station file.
 * This file contains the lid, latitude, longitude, etc
 * of each station predetermined by user.
 * The first record in this file is the number of stations in the file.
 *********************************************************************/

#include "dqc_preproc_setup.h"

int read_station_info (const char *path, const char *areaName)
{
    bool is_slash = false;
    char filename[FNAME_LEN] = {'\0'};
    char * pFilePath = NULL;
    int filename_len;
    FILE * fp = NULL;
    int i = 0;
    int item_count;
    int num_records;

    char * strLine = NULL ;
    char tmpLid[GAGE_ID_LEN + 1] = {'\0'};
    char tmpShef[8] = {'\0'};
    char tmpPE[4] = {'\0'};
    struct stat statInfo;
    int status;
    int path_len, filepath_len ;

    sprintf(filename, "%s_station_list", areaName);

    /*
     * Build the file path.
     */
    filename_len = strlen ( filename );
    path_len = strlen ( dqc_preproc_setup_mpe_station_list_dir );

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
        fprintf ( stderr, "In routine 'read_station_file': "
                    "dynamic memory allocation failure.\n" );
        exit(-1);
    }

    if ( is_slash )
    {
        sprintf ( pFilePath, "%s%s", path, filename );
    }
    else
    {
        sprintf ( pFilePath, "%s/%s", path, filename );
    }

     /*
      * Check to determine if the file exists and is readable.
      */
    status = stat ( pFilePath, & statInfo );

    if ( ( status != 0) || !( statInfo.st_mode & S_IFREG ) )
    {
        fprintf ( stderr , "WARNING: file = \"%s\" not found.\n", pFilePath) ;
        return -1 ;
    }

    /*
     * Attempt to open the station data file.
     */
    fp = fopen ( pFilePath, "r" );

    if ( fp == NULL )
    {
        /*
         * Could not open the station file.
         */
        fprintf ( stderr, "WARNING: Could not open file %s. "
                "\nFile may be corrupted."
                "\nProgram exit.\n", pFilePath );
        exit(-1);
    }

    strLine = ( char * ) malloc ( DATA_STRING_LEN * sizeof ( char ) );

    while((strLine = fgets(strLine, DATA_STRING_LEN, fp)) != NULL)
    {
        item_count = sscanf ( strLine, "%d", &num_records );
        if ( item_count == EOF ||    item_count != 1 )
        {
            /*
             * Could not read the record count from the first record
             * of the station file.
             */
            fprintf ( stderr,    "ERROR: Could not read station count from first record of "
                    "file:\n\t%s.\n\tCreate station list file first."
                    "\n\tProgram exit.\n", pFilePath );
            exit(-1);
        }

        strLine = fgets(strLine, DATA_STRING_LEN, fp);
        if(strLine == NULL)
        {
            fprintf ( stderr, "INVALID data records "
                "of file %s. File corrupt.\n"
                "\tProgram exit.\n", pFilePath );
            exit(-1);
        }
    
        sscanf(strLine, "%s %s", tmpLid, tmpShef);
        strncpy(tmpPE, tmpShef, 3);

        if(strcmp(tmpPE, "PPH") == 0)
        {
            /*
             * Skip the PPH records from the station data file.
             * 
             * Read each line from the station file
             * and skip the PPH data.
             */
            for ( i = 1; ( i < num_records ) && ( !feof ( fp ) ); ++i )
            {
                strLine = fgets(strLine, DATA_STRING_LEN, fp);
                if(strLine == NULL)
                {
                    fprintf ( stderr, "INVALID PPH data records "
                        "of file %s. File corrupt.\n"
                        "\tProgram exit.\n", pFilePath );
                    exit(-1);
                }
            }
        }
        else if(strcmp(tmpPE, "PPD") == 0)
        {
            /*
             * Allocate memory and initialize Precip Array.
             */ 
            ptrPrecipInfo = (station_info *)calloc(num_records, sizeof(station_info)); 
            if(ptrPrecipInfo == NULL)
            {
                fprintf ( stderr , "ERROR:\n\tMemory allocation failure"
                    " in initStationInfoList function."
                    "\n\tProgram exit.\n") ;
                exit(-1) ;
            }

            for(i = 0; i < num_records; i++)
            {
                memset(ptrPrecipInfo[i].lid, '\0', GAGE_ID_LEN + 1);
                memset(ptrPrecipInfo[i].pe, '\0', SHEF_PE_LEN + 1);
                memset(ptrPrecipInfo[i].ts, '\0', 3);
            }

            item_count = sscanf ( strLine, "%s %s %lf %lf", 
                                        ptrPrecipInfo[0].lid ,
                                        tmpShef ,
                                        & ptrPrecipInfo[0].lat ,
                                        & ptrPrecipInfo[0].lon );    
    
            if ( item_count != 4 )
            {
                fprintf ( stderr, "ERROR: Could not read lid, shef, lat, and lon from "
                    "record %d of file %s. File corrupt.\n"
                    "\tProgram exit.\n", 1, pFilePath );
                printf("%s\n", strLine);
                exit(-1);
            }

            /*
             * Load the source value from the ts.
             */
            ptrPrecipInfo[0].ts[0] = tmpShef[3] ;
            ptrPrecipInfo[0].ts[1] = tmpShef[4] ;

            /*
             * Read following records from the station file, 
             * storing its contents into the corresponding
             * element in the station data array.
             */
            for ( i = 1; ( i < num_records ) && ( !feof ( fp ) ); ++i )
            {
                strLine = fgets(strLine, DATA_STRING_LEN, fp);
                if(strLine == NULL)
                {
                    fprintf ( stderr, "Could not read lid, lat, and lon from "
                        "record %d of file %s.    File corrupt.\n"
                        "\tProgram exit.\n", i+1, pFilePath );
                    exit(-1);
                }
        
                item_count = sscanf ( strLine, "%s %s %lf %lf", 
                                            ptrPrecipInfo[i].lid ,
                                            tmpShef ,
                                            & ptrPrecipInfo[i].lat ,
                                            & ptrPrecipInfo[i].lon );    
        
                if ( item_count != 4 )
                {
                    fprintf ( stderr, "ERROR: Could not read lid, shef, lat, "
                        "and lon from record %d of file %s. File corrupt.\n"
                        "\tProgram exit.\n", i, pFilePath );
                    exit(-1);
                }

                /*
                 * Load the source value from the ts.
                 */
                ptrPrecipInfo[i].ts[0] = tmpShef[3] ;
                ptrPrecipInfo[i].ts[1] = tmpShef[4] ;
            }

            if ( i < num_records )
            {
                fprintf ( stderr, "File %s is incomplete. The record count "
                        "in the first row of the file is %d. The number\n"
                        "of records read in are %d.\n"
                        "\tProgram exit.\n", pFilePath, num_records, i );
                exit(-1);
            }
            dqc_preproc_setup_precip_count = num_records;
        }
        else if(strcmp(tmpPE, "TAI") == 0)
        {
            /*
             * Allocate memory and initialize temperature Array.
             */ 
            ptrTempInfo = (station_info *)calloc(num_records, sizeof(station_info)); 
            if(ptrTempInfo == NULL)
            {
                fprintf ( stderr , "ERROR:\n\tMemory allocation failure"
                    " in initStationInfoList function."
                    "\n\tProgram exit.\n") ;
                exit(-1) ;
            }

            for(i = 0; i < num_records; i++)
            {
                memset(ptrTempInfo[i].lid, '\0', GAGE_ID_LEN + 1);
                memset(ptrTempInfo[i].pe, '\0', SHEF_PE_LEN + 1);
                memset(ptrTempInfo[i].ts, '\0', 3);
            }

            item_count = sscanf ( strLine, "%s %s %lf %lf", 
                                        ptrTempInfo[0].lid ,
                                        tmpShef ,
                                        & ptrTempInfo[0].lat ,
                                        & ptrTempInfo[0].lon );    

            if ( item_count != 4 )
            {
                fprintf ( stderr, "ERROR: Could not read lid, shef, lat, "
                    "and lon from record %d of file %s. File corrupt.\n"
                    "\tProgram exit.\n", 1, pFilePath );
                exit(-1);
            }

            /*
             * Load the source value from the ts.
             */
            ptrTempInfo[0].ts[0] = tmpShef[3] ;
            ptrTempInfo[0].ts[1] = tmpShef[4] ;

            /*
             * Read following records from the station file, 
             * storing its contents into the corresponding
             * element in the station data array.
             */
            for ( i = 1; ( i < num_records ) && ( !feof ( fp ) ); ++i )
            {
                strLine = fgets(strLine, DATA_STRING_LEN, fp);
                if(strLine == NULL)
                {
                    fprintf ( stderr, "Could not read lid, lat, and lon from "
                        "record %d of file %s. File corrupt.\n"
                        "\tProgram exit.\n", i+1, pFilePath );
                    exit(-1);
                }

                item_count = sscanf ( strLine, "%s %s %lf %lf", 
                                            ptrTempInfo[i].lid ,
                                            tmpShef ,
                                            & ptrTempInfo[i].lat ,
                                            & ptrTempInfo[i].lon );    

                if ( item_count != 4 )
                {
                    fprintf ( stderr, "ERROR: Could not read lid, shef, lat, and lon from "
                        "record %d of file %s. File corrupt.\n"
                        "\tProgram exit.\n", i, pFilePath );
                    exit(-1);
                }

                /*
                 * Load the source value from the ts.
                 */
                ptrTempInfo[i].ts[0] = tmpShef[3] ;
                ptrTempInfo[i].ts[1] = tmpShef[4] ;
            }

            if ( i < num_records )
            {
                fprintf ( stderr, "File %s is incomplete. The record count "
                        "in the first row of the file is %d. The number\n"
                        "of records read in are %d.\n"
                        "\tProgram exit.\n", pFilePath, num_records, i );
                exit(-1);
            }
            dqc_preproc_setup_temperature_count = num_records;
        }
        else if(strcmp(tmpPE, "HZI") == 0)
        {
            /*
             * Skip the HZI records from the station data file.
             * Read each line from the station file
             * and skip the HZI data.
             */
            for ( i = 1; ( i < num_records ) && ( !feof ( fp ) ); ++i )
            {
                strLine = fgets(strLine, DATA_STRING_LEN, fp);
                if(strLine == NULL)
                {
                    fprintf ( stderr, "INVALID HZI data records "
                        "of file %s. File corrupt.\n"
                        "\tProgram exit.\n", pFilePath );
                    exit(-1);
                }
            }
        }
    }

    if ( pFilePath != NULL )
    {
        free ( pFilePath );
        pFilePath = NULL;
    }

    /*
     * Close the station file.
     */
    if(fp != NULL)
    {
        fclose ( fp );
        fp = NULL;
    }

    return 0;
}
