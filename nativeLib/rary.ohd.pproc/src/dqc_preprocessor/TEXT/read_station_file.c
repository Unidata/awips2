
/**********************************************************************
 * read_station_file ( )
 * 
 * This function reads the station info file. This file contains the lid,
 * latitude, longitude, etc of each station for PPH, PPD, HAI and HZI blocks.
 * Previously, the first record for each block is the number of stations.  However,
 * due to requirement changes for station list file format, this was changed
 * so that the number of records for the PPD and TAI stations were counted and
 * then used later in determining the size of the station structure arrays and
 * filling those structures.
 *********************************************************************/

#include "dqc_preproc.h"
#include "gageqc_defs.h"

int read_station_file ( const int numDays, const char *areaName)
{
    bool is_slash = false;
    char filename[FNAME_LEN] = {'\0'};
    char * pFilePath = NULL;
    int filename_len;
    FILE * fp = NULL;
    int i = 0;
    int j = 0;
    int item_count;
    int num_precip_records = 0 ;
    int num_temp_records = 0 ;

    char strLine[ DATA_STRING_LEN +1 ] = {'\0'} ;
    char tmpLid[GAGE_ID_LEN + 1] = {'\0'};
    char tmpShef[8] = {'\0'};
    char tmpPE[4] = {'\0'};
    struct stat statInfo;
    int status;
    int path_len, filepath_len ;

/*    printf(" entering dqc read station list\n");*/

    sprintf(filename, "%s_station_list", areaName);

    /*
     * Build the file path.
     */
    filename_len = strlen ( filename );
    path_len = strlen ( mpe_station_list_dir );

    if ( mpe_station_list_dir [ path_len - 1 ] != '/' )
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
        printf( "In routine 'read_station_file': "
                    "dynamic memory allocation failure.\n" );
        exit(-1);
    }

    if ( is_slash )
    {
        sprintf ( pFilePath, "%s%s", mpe_station_list_dir, filename );
    }
    else
    {
        sprintf ( pFilePath, "%s/%s", mpe_station_list_dir, filename );
    }

     /*
      * Check to determine if the PRISM file exists and is readable.
      */
    status = stat ( pFilePath, & statInfo );

    if ( ( status != 0) || !( statInfo.st_mode & S_IFREG ) )
    {
        printf ( "WARNING: file = \"%s\" not found.\n", pFilePath) ;
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
        printf( "WARNING: Could not open file %s. "
                "\nFile may be corrupted."
                "\nProgram exit.\n", pFilePath );
        exit(-1);
    }

 /*   printf( " about to enter the first loop through the file \n");*/

/*    strLine = ( char * ) malloc ( DATA_STRING_LEN * sizeof ( char ) );*/

    while(!feof(fp))
    {
        memset(strLine,'\0',DATA_STRING_LEN+1); /* null out before reading next line to basically clear the buffer */
        fgets(strLine, DATA_STRING_LEN, fp);

        item_count = sscanf(strLine, "%s %s", tmpLid, tmpShef);
	if (item_count == 2)
        {

           strncpy(tmpPE, tmpShef, 3);
           if(strcmp(tmpPE, "PPD") == 0)
           {
	       num_precip_records++;
/*	       printf(" record number %d tmpLid %s tmpShef %s\n",num_precip_records, tmpLid, tmpShef);*/

	   }
           else if(strcmp(tmpPE, "TAI") == 0)
           {
	       num_temp_records++;
	   }
	}
    }

/*    printf(" about to enter second loop; first initialize arrays\n");*/

    rewind(fp);
/*
    printf(" num_precip_records = %d\n",num_precip_records);

    printf(" num_temp_records = %d\n", num_temp_records);
*/
    if(num_precip_records > 0)
    {

       initPrecipArray ( num_precip_records, numDays ) ;
       precip_count = num_precip_records;
    }

    if(num_temp_records > 0)
    {
       initTempArray ( num_temp_records, numDays ) ;
       temperature_count = num_temp_records;
    }

    while(!feof(fp))
    {
        memset(strLine,'\0',DATA_STRING_LEN+1); /* null out before reading next line to basically clear the buffer */
        fgets(strLine, DATA_STRING_LEN, fp);
    
        item_count = sscanf(strLine, "%s %s", tmpLid, tmpShef);
	if(item_count == 2)
	{
           strncpy(tmpPE, tmpShef, 3);


           if(strcmp(tmpPE, "PPD") == 0 && num_precip_records > 0 && i< num_precip_records )
           {
               item_count = sscanf ( strLine, "%s %s %lf %lf",
                                            pPrecipInfo[i].lid ,
                                            tmpShef ,
                                            & pPrecipInfo[i].lat ,
                                            & pPrecipInfo[i].lon );    

                if ( item_count != 4 )
                {
                    printf( "ERROR: Could not read lid, shef, lat, "
                        "and lon from record %d of file %s. File corrupt.\n"
                        "\tProgram exit.\n", i+1, pFilePath );
                    exit(-1);
                }

                /*
                 * Load the source value from the ts.
                 */
                pPrecipInfo[i].source = tmpShef[4] ;
/*
		printf(" DEBUG: checking values for precip %d %s %c %lf %lf\n", i,
                                            pPrecipInfo[i].lid ,
                                            pPrecipInfo[i].source ,
                                            pPrecipInfo[i].lat ,
                                            pPrecipInfo[i].lon );
*/

	        i++;

	   }
           else if(strcmp(tmpPE, "TAI") == 0 && num_temp_records > 0  && j< num_temp_records )
           {
                item_count = sscanf ( strLine, "%s %s %lf %lf", 
                                      pTempInfo[j].lid ,
                                      tmpShef ,
                                      & pTempInfo[j].lat ,
                                      & pTempInfo[j].lon );

                if ( item_count != 4 )
                {
                    printf( "ERROR: Could not read lid, shef, lat, and lon from "
                            "record %d of file %s. File corrupt.\n"
                            "\tProgram exit.\n", j+1, pFilePath );
                    exit(-1);
                }

                /*
                 * Load the source value from the ts.
                 */
                pTempInfo[j].source   = tmpShef[4] ;
                pTempInfo[j].extremum = tmpShef[5] ;
/*
		printf(" DEBUG: checking values for temp %d %s %c %c %lf %lf\n", j,
                                      pTempInfo[j].lid ,
                                      pTempInfo[j].source,
				      pTempInfo[j].extremum,
                                       pTempInfo[j].lat ,
                                       pTempInfo[j].lon );
*/
		j++;
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
