
/*******************************************************************************
* FILENAME:                 dqc_precip_preproc.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*     MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:     Guoxian Zhou
* CREATION DATE:       Febrary 1, 2006
* ORGANIZATION:        OHD11/HSEB
* MACHINE:             Linux
* MODIFICATION HISTORY:
*     MODULE #            DATE             PROGRAMMER            DESCRIPTION/REASON
*
********************************************************************************
*/

#include "dqc_preproc.h"

#define FREE_MEMORY                                     \
                    if ( pDate != NULL )                \
                    {                                   \
                        free ( pDate );                 \
                        pDate = NULL;                   \
                    }                                   \
                                                        \
                    if ( pPrecipInfo != NULL )          \
                    {                                   \
                        free ( pPrecipInfo );           \
                        pPrecipInfo = NULL;             \
                    }                                   \
                                                        \

#define USAGE printf ( "Usage:\n"                                      \
                        " dqc_preprocessor -d[num_days] -t[end_date]"  \
                        " -a[area1,area2...] -z\n" );                  \

int precip_count = 0 ;
int temperature_count = 0 ;
struct precip_info * pPrecipInfo = NULL ;
struct temperature_info * pTempInfo = NULL ;

char  mpe_station_list_dir[PATH_LEN] = {'\0'} ;
char  mpe_point_precip_dir[PATH_LEN] = {'\0'};
char  mpe_site_id[SITENAME_LEN] = {'\0'} ;
char  mpe_area_names[DATA_STRING_LEN] = {'\0'} ;
char  db_name[DB_LEN] = {'\0'} ;
char  mpe_point_temperature_dir[PATH_LEN] = {'\0'};

int dqc_preproc_main ( int argc, const char ** argv )
{
    char * pDate = NULL ;
    int numDays = DEFAULT_DAY ;
    int len;
    int status ;
    int intYear = -1 ;
    int intMon = -1 ;
    int intDay = -1 ;
    time_t start_time_t, end_time_t, run_time_t;
    struct tm tm_time;
    struct tm * temp_time = NULL;
    long tmpDate ;
    char c;
    char strDate[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'};

    char delims[] = {","};
    char ** areaList = NULL;
    char ** inputAreaList = NULL;
    char * pAreaListString = NULL;
    int areaNumber = 0 ;
    int count ;
    int i, j;
    int * pAreaStatus ;
    bool blnSetZero  = false;

    time(&start_time_t);

    timet_to_yearsec_ansi(start_time_t, strDate);
    printf("\nSTATUS: Start running dailyQC preprocessor -- %s\n", strDate);

    temp_time = gmtime(&start_time_t);

    memset(&tm_time, '\0', sizeof(struct tm)) ;

    tm_time.tm_year = temp_time->tm_year;
    tm_time.tm_mon  = temp_time->tm_mon;


    tm_time.tm_mday = temp_time->tm_mday;

    tm_time.tm_hour = 12;
    tm_time.tm_min = 0;
    tm_time.tm_sec = 0;

    /*
     * Get the command line arguments.
     * These include the number of days
     * and the end date value.
     */
   while (( c = getopt ( argc, argv, ":t:d:a:z" ) ) != -1 )
   {
      switch ( c )
      {
         case 't' :
            len = strlen ( optarg );
            if(len == 8)
            {
                pDate = ( char * ) malloc ( ( len + 1 ) * sizeof ( char ) );

                if ( pDate == NULL )
                {
                    printf( "Dynamic memory allocation failure\n" );
                    FREE_MEMORY;
                    return 1;
                }

                memset ( pDate, '\0', len + 1 );
                strncpy ( pDate, optarg, len );

                tmpDate = atoi(pDate);

                intYear = tmpDate / 10000;
                intMon  = (tmpDate - intYear * 10000) / 100 ;
                intDay  = tmpDate % 100 ;
            }
            else
            {
                printf( "ERROR: Date value \"%s\" INVALID!"
                        " default to current date.\n", optarg) ;
            }
            break;

         case 'd' :
            tmpDate = atol(optarg);

            if(tmpDate < 1 || tmpDate > MAXIMUM_DAY)
            {
                printf( "ERROR: Days value \"%s\" INVALID!"
                        " default to %d days.\n",
                        optarg, DEFAULT_DAY) ;
            }
            else
                numDays = tmpDate ;
            break;

         case 'a' :
            len = strlen ( optarg );
            pAreaListString = ( char * ) malloc ( ( len + 1 ) * sizeof ( char ) );

            if ( pAreaListString == NULL )
            {
                printf( "Dynamic memory allocation failure\n" );
                return 1;
            }

            memset ( pAreaListString, '\0', len + 1 );
            strcpy ( pAreaListString, optarg );
            break;

         case 'z' :
            blnSetZero = true;
            break;

         case ':' :
            printf( "WARNING:\n"
                    "\tThe \"end date\"  must be supplied with the \"-t\" option.\n"
                    "\tThe \"num_days\"  must be supplied with the \"-d\" option.\n"
                    "\tThe \"area list\" must be supplied with the \"-a\" option.\n"
                    "\tThe \"set zero \" must be supplied with the \"-z\" option.\n\n"
                   );

            return 1;

         case '?' :
            printf( "An invalid command line argumane has been "
                    "passed into this program.\n" );
            USAGE;
            FREE_MEMORY;
            return 1;

         default :
             printf( "An invalid command line character, %c, was "
                     "found while parsing the command line options\n"
                     "passed into this routine.\n", c ) ;
             USAGE;
             FREE_MEMORY;
             return 1;
      }
   }

    /*
     * Adjust the date value based on
     * the number of days
     * and the end date value.
     */
    if( ( intYear != -1 ) &&
        ( intMon  != -1 ) &&
        ( intDay  != -1 ) )
    {
        tm_time.tm_year = intYear - 1900 ;
        tm_time.tm_mon  = intMon - 1 ;
        tm_time.tm_mday = intDay - numDays ;
    }
    else
    {
        tm_time.tm_mday -= numDays ;
    }

    /* define the startime date/time */

    run_time_t = gm_mktime( &tm_time );

    /* advance one day from current day in order to get point data at 12~18Z for the current day*/

    numDays = numDays + 1;

    /*
     * Load token values.
     */
    loadAppsDefaults();

    /*
     * build the area token list.
     */
    areaList = stringTokenize(mpe_area_names, delims, &count);
    areaNumber = count;

    pAreaStatus = ( int * ) malloc ( areaNumber * sizeof ( int ) );

    if ( pAreaStatus == NULL )
    {
        printf( "Dynamic memory allocation failure\n" );
        FREE_MEMORY;
        return 1;
    }

    for(i = 0; i < areaNumber; i++)
    {
        pAreaStatus[i] = 0;
    }

    if(pAreaListString != NULL)
    {
        inputAreaList = stringTokenize(pAreaListString, delims, &count);

        for(i = 0; i < count; i++)
        {
            int blnFlag = 0;
            for(j = 0; j < areaNumber; j++)
            {
                if(strcmp(inputAreaList[i], areaList[j]) == 0)
                {
                    blnFlag = 1;
                    break;
                }
            }
            if(blnFlag == 1)
                pAreaStatus[j] = 1;
            else
                printf( "INVALID area value: %s\n", inputAreaList[i]);
        }
    }
    else
    {
        for(i = 0; i < areaNumber; i++)
        {
            pAreaStatus[i] = 1;
        }
    }

    timet_to_yearsec_ansi(run_time_t, strDate);
    printf("STATUS: Setup parameters:\n");
    printf("\tRunning days: %d\n", numDays);
    printf("\tStart time: %s\n", strDate);
    printf("\tDatabase: %s\n", db_name);
    printf("\tmpe_site_id: %s\n", mpe_site_id);
    printf("\tSub area list: ");
    for(i = 0; i < areaNumber; i++)
    {
        if(pAreaStatus[i] == 1)
            printf("%s ", areaList[i]);
    }
    printf("\n");

    if(blnSetZero == true)
        printf("\tSet precip value to zero\n");
    printf("\n");

    /*
     * Add the mpe_site_id as the last item.
     */
    areaList[areaNumber] = (char *)malloc( strlen(mpe_site_id) * sizeof(char ));
    if(areaList[areaNumber] == NULL)
    {
        printf( "ERROR: memory allocation failure"
            " in main function.\n\tProgram exit.\n") ;
        exit(-1) ;
    }
    strcpy(areaList[areaNumber], mpe_site_id);
    pAreaStatus[areaNumber] = 1 ;
    areaNumber ++;

    /*
     * Open the IHFS database.
     */
    status = OpenDbms ( db_name );

    if ( status != Ok )
    {
        printf( "Could not open database %s.\n", db_name );
        FREE_MEMORY;
        return 1;
    }

    /*
     * Build the level 1 data for each sub-area
     * if it's status is ON,
     * and the master list.
     */
    for(i = 0; i < areaNumber ; i++)
    {
        if(pAreaStatus[i] != 1)
            continue;

        /*
         * Get the station data.
         * if the file is not available,
         * then skip it.
         */
        status = read_station_file ( numDays, areaList[i]) ;
        if(status != 0)
            continue;

        /*
         * Write the precip data to a file with all-zero values
         * if there is a -z option from the command line.
         */
        if(blnSetZero == true)
        {
            write_precip_data (run_time_t, numDays, areaList[i], blnSetZero ) ;
        }
        else
        {
            /*
             * Get the daily precip data.
             */
             processDailyPP ( run_time_t, numDays ) ;

            /*
             * Get the HourlyPP/HourlyPC data.
             */
            processHourlyPPPC ( run_time_t, numDays);

            /*
             * Write the precip data to a file.
             */
            write_precip_data (run_time_t, numDays, areaList[i], blnSetZero ) ;
        }

        /*
         * Get the 00z, 06z, 12z, 18z, max/min temperature data.
         */
        processTemperature (run_time_t, numDays ) ;

        /*
         * Write the temperature data to a file.
         */
        write_temperature_data (run_time_t, numDays, areaList[i]);

        /*
         * Release memory for precip and temperature arrays.
         */
        releasePrecipArray (precip_count, numDays );
        releaseTempArray (temperature_count, numDays );

    }

    /*
     * Close the IHFS database.
     */
    CloseDbms ( );

    /*
     * Free dynamically allocated memory.
     */
    FREE_MEMORY;

    time(&end_time_t);
    int elapse = end_time_t - start_time_t;
    timet_to_yearsec_ansi(start_time_t, strDate);
    printf("\nSTATUS: End running dailyQC preprocessor with elaspe time: %d seconds\n",
            elapse);

    return 0;
}
