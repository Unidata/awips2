
/*************************************************************************
 * write_precip_data()
 *
 * This function writes out the precip array into
 * precip level 1 point data file(s).
 ***************************************************************************/

#include "dqc_preproc.h"
#include "gageqc_defs.h"

extern struct precip_info * pPrecipInfo ;
extern int precip_count ;

void write_precip_data (const time_t start_time_t,
                        const int numDays,
                        const char * areaName,
                        const bool blnSetZero )
{
    bool is_slash = false;
    char filename[FNAME_LEN] = {'\0'};
    char * pFilePath = NULL;
    FILE * pFile = NULL;
    int filename_len;
    int path_len;
    int filepath_len;

    char shefMessage[100] = {'\0'} ;
    char tmpValue[10] = {'\0'}; ;

    int i, j, index;
    char a_datetime_shef[9] = {'\0'} ;
    char e_datetime_shef[9] = {'\0'} ;

    struct tm * tm_time = NULL;
    time_t curr_time_t ;

    char strDate[9] = {'\0'};
    static int first = 0;

    if ( pPrecipInfo == NULL ) return;

    /*
     * Compute the date string array.
     */
    curr_time_t = start_time_t + 24 * SECONDS_PER_HOUR;

    for(index = 0; index < numDays; index++)
    {
        tm_time = gmtime(&curr_time_t);
       /* strftime(strDate, sizeof(strDate), "%Y%m%d", tm_time);*/

        sprintf(strDate, "%04d%02d%02d", tm_time->tm_year+1900,
	                                 tm_time->tm_mon+1, tm_time->tm_mday);

        sprintf(filename, "precip_1_%s_point_%s", areaName, strDate);

        /*
         * Build the file path.
         */
        filename_len = strlen ( filename );
        path_len = strlen ( mpe_point_precip_dir );

        if ( mpe_point_precip_dir [ path_len - 1 ] != '/' )
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
            printf( "In routine 'write_precip_data': "
                    "dynamic memory allocation failure.\n" );
            return;
        }

        if ( is_slash )
        {
            sprintf ( pFilePath, "%s%s", mpe_point_precip_dir, filename );
        }
        else
        {
            sprintf ( pFilePath, "%s/%s", mpe_point_precip_dir, filename );
        }

        /*
         * Attempt to open the file for writing.
         */
        pFile = fopen ( pFilePath, "w" );

        if ( pFile == NULL )
        {
            if ( pFilePath != NULL )
            {
                 free ( pFilePath );
                 pFilePath = NULL;
            }

            printf( "In routine 'write_precip_data': "
                    "Could not open file %s.\n", pFilePath );
            return;
        }

        tm_time = gmtime(&curr_time_t);
       /* strftime( a_datetime_shef, sizeof(a_datetime_shef), "%Y%m%d", tm_time );*/

        sprintf(a_datetime_shef, "%04d%02d%02d", tm_time->tm_year+1900,
	                                         tm_time->tm_mon+1, tm_time->tm_mday);

        time_t e_time_t = curr_time_t - 24 * SECONDS_PER_HOUR ;
        tm_time = gmtime(&e_time_t);
/*        strftime( e_datetime_shef, sizeof(e_datetime_shef), "%Y%m%d", tm_time );*/

        sprintf(e_datetime_shef, "%04d%02d%02d", tm_time->tm_year+1900,
	                                         tm_time->tm_mon+1, tm_time->tm_mday);

        for(i = 0; i < precip_count; i ++ )
        {
            /*
             * build the .A record
             */
            memset(shefMessage, '\0', sizeof(shefMessage));
	  /*  memset(tmpValue, '\0', sizeof(tmpValue));*/

            if(blnSetZero == true)  // build all-zero records
            {
                sprintf(shefMessage,".A %-5s %s DH12/PPD1%c/  0.00S\n",
                    pPrecipInfo[i].lid,
                    a_datetime_shef,
                    pPrecipInfo[i].source);
            }
            else
            {
                if(pPrecipInfo[i].pPPD[index] != PRECIP_MISSING)
                {
                    sprintf(shefMessage,".A %-5s %s DH12/PPD1%c/ %5.2fS\n",
                        pPrecipInfo[i].lid,
                        a_datetime_shef,
                        pPrecipInfo[i].source,
                        pPrecipInfo[i].pPPD[index]);
                }
                else
                {
                    sprintf(shefMessage,".A %-5s %s DH12/PPD1%c/ m \n",
                        pPrecipInfo[i].lid,
                        a_datetime_shef,
                        pPrecipInfo[i].source);
                }
            }

            fprintf(pFile, shefMessage);

            /*
             * build the .E record
             */
            memset(shefMessage, '\0', sizeof(shefMessage));
            if(blnSetZero == true)  // build all-zero records
            {
                sprintf(shefMessage,".E %-5s %s DH18/PPQ1%c/DIH+6/"
                    "  0.00S/  0.00S/  0.00S/  0.00S\n",
                    pPrecipInfo[i].lid,
                    e_datetime_shef,
                    pPrecipInfo[i].source);
            }
            else
            {
                sprintf(shefMessage,".E %-5s %s DH18/%sQ1%c/DIH+6/",
                    pPrecipInfo[i].lid,
                    e_datetime_shef,
                    pPrecipInfo[i].pPPQPE[index],
                    pPrecipInfo[i].source);

                for(j = 0; j < 4; j++)
                {
                    if(pPrecipInfo[i].pPPQ[index][j] != PRECIP_MISSING)
		    {
                        sprintf(tmpValue, " %5.2fS ", pPrecipInfo[i].pPPQ[index][j]);
                    }
		    else
		    {
                        sprintf(tmpValue, " m ");
                    }

                    strcat(shefMessage, tmpValue);

		    if(j != 3)
                        strcat(shefMessage, "/");
                }
                strcat(shefMessage, "\n");
            }

            fprintf(pFile, shefMessage);
        }

        if ( pFilePath != NULL )
        {
            free ( pFilePath );
            pFilePath = NULL;
        }

        fclose ( pFile );
        pFile = NULL;

        curr_time_t += 24 * SECONDS_PER_HOUR ;
    }

    if(first == 0)
    {
        printf("STATUS: Output precip data to: %s\n", mpe_point_precip_dir);
        first = 1;
    }


    return ;
}


/*************************************************************************
 * write_temperature_data()
 *
 * This function writes out the temperature data
 * into temperature level 1 point data file(s).
 ***************************************************************************/

void write_temperature_data (const time_t start_time_t,
                             const int numDays,
                             const char * areaName)
{
    bool is_slash = false;
    char filename[FNAME_LEN] = {'\0'};
    char * pFilePath = NULL;
    FILE * pFile = NULL;
    int filename_len;
    int path_len;
    int filepath_len;

    char shefMessage[100] = {'\0'} ;

    int i, j, index;
    char a_datetime_shef[9] = {'\0'} ;
    char e_datetime_shef[9] = {'\0'} ;
    char tmpValue[10] = {'\0'};


    struct tm * tm_time = NULL;
    time_t curr_time_t ;

    char strDate[9] = {'\0'};;
    static int first = 0;

    int dqc_ending_6hour_obstime_flag;

    dqcInitStruct initStruct;

    initStruct = getInitStruct ( );
    dqc_ending_6hour_obstime_flag = initStruct.dqc_ending_6hour_obstime_flag;

    if ( pTempInfo == NULL ) return;

    /*
     * Compute the date string array.
     */
    curr_time_t = start_time_t + 24 * SECONDS_PER_HOUR;

    for(index = 0; index < numDays; index++)
    {
        tm_time = gmtime(&curr_time_t);
       /* strftime(strDate, sizeof(strDate), "%Y%m%d", tm_time);*/
	sprintf(strDate, "%04d%02d%02d", tm_time->tm_year+1900,
	                                 tm_time->tm_mon+1, tm_time->tm_mday);

        sprintf(filename, "temperature_1_%s_point_%s", areaName, strDate);

        /*
         * Build the file path.
         */
        filename_len = strlen ( filename );
        path_len = strlen ( mpe_point_temperature_dir );

        if ( mpe_point_temperature_dir [ path_len - 1 ] != '/' )
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
            printf( "In routine 'write_temperature_data': "
                    "dynamic memory allocation failure.\n" );
            return;
        }

        if ( is_slash )
        {
            sprintf ( pFilePath, "%s%s", mpe_point_temperature_dir, filename );
        }
        else
        {
            sprintf ( pFilePath, "%s/%s", mpe_point_temperature_dir, filename );
        }
        /*
         * Attempt to open the file for writing.
         */
        pFile = fopen ( pFilePath, "w" );

        if ( pFile == NULL )
        {
            if ( pFilePath != NULL )
            {
                 free ( pFilePath );
                 pFilePath = NULL;
            }

            printf( "In routine 'write_temperature_data': "
                    "Could not open file %s.\n", pFilePath );
            return;
        }

        tm_time = gmtime(&curr_time_t);
        /*strftime( a_datetime_shef, sizeof(a_datetime_shef), "%Y%m%d", tm_time );*/
	sprintf(a_datetime_shef, "%04d%02d%02d", tm_time->tm_year+1900,
	                                         tm_time->tm_mon+1, tm_time->tm_mday);

        time_t e_time_t = curr_time_t - 24 * SECONDS_PER_HOUR ;
        tm_time = gmtime(&e_time_t);
/*        strftime( e_datetime_shef, sizeof(e_datetime_shef), "%Y%m%d", tm_time );*/

        sprintf(e_datetime_shef, "%04d%02d%02d", tm_time->tm_year+1900,
	                                         tm_time->tm_mon+1, tm_time->tm_mday);


        for(i = 0; i < temperature_count; i ++ )
        {
            /*
             * build the .A max temperature record
             */
            memset(shefMessage, '\0', sizeof(shefMessage));

            sprintf(shefMessage,".A %-5s %s DH12/TAI1%cXZ",
                    pTempInfo[i].lid,
                    a_datetime_shef,
                    pTempInfo[i].source);

            if(pTempInfo[i].value[index][4] != MISSING_MAX_TEMPERATURE)
                sprintf(tmpValue, " %5dS ", (int)(pTempInfo[i].value[index][4] + 0.5));
            else
                sprintf(tmpValue, " m ");

            strcat(shefMessage, tmpValue);
            strcat(shefMessage, "\n");
            fprintf(pFile, shefMessage);

            /*
             * build the .A min temperature record
             */
            memset(shefMessage, '\0', sizeof(shefMessage));

	    sprintf(shefMessage,".A %-5s %s DH12/TAI1%cNZ",
                    pTempInfo[i].lid,
                    a_datetime_shef,
                    pTempInfo[i].source);

            if(pTempInfo[i].value[index][5] != MISSING_MIN_TEMPERATURE)
                sprintf(tmpValue, " %5dS ", (int)(pTempInfo[i].value[index][5] + 0.5));
            else
                sprintf(tmpValue, " m ");

            strcat(shefMessage, tmpValue);
            strcat(shefMessage, "\n");
            fprintf(pFile, shefMessage);

            /*
             * build the .E temperature record
             */
            memset(shefMessage, '\0', sizeof(shefMessage));

	    if (dqc_ending_6hour_obstime_flag == DQC_PREPROCESSOR_ENDING_OBSTIME_12Z)
	    {
	       sprintf(shefMessage,".E %-5s %s DH18/TAI1%cZZ/DIH+6/",
                       pTempInfo[i].lid,
                       e_datetime_shef,
                       pTempInfo[i].source);
	    }
            else
	    {
	       sprintf(shefMessage,".E %-5s %s DH12/TAI1%cZZ/DIH+6/",
                       pTempInfo[i].lid,
                       e_datetime_shef,
                       pTempInfo[i].source);
	    }

            for(j = 0; j < 4; j++)
            {
                if(pTempInfo[i].value[index][j] != TEMPERATURE_MISSING)
                    sprintf(tmpValue, " %5dS ", (int)(pTempInfo[i].value[index][j] + 0.5));
                else
                    sprintf(tmpValue, " m ");

                strcat(shefMessage, tmpValue);
                if(j != 3)
                    strcat(shefMessage, "/");
            }
            strcat(shefMessage, "\n");

            fprintf(pFile, shefMessage);
         }

        if ( pFilePath != NULL )
        {
            free ( pFilePath );
            pFilePath = NULL;
        }

        fclose ( pFile );
        pFile = NULL;

        curr_time_t += 24 * SECONDS_PER_HOUR ;
    }

    if(first == 0)
    {
        printf("STATUS: Output temperature data to: %s\n\n", mpe_point_temperature_dir);
        first = 1;
    }

    return ;
}

