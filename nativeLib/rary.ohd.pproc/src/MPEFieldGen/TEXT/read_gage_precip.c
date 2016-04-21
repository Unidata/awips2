/*******************************************************************************
* FILENAME:            read_gage_precip.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          readGagePrecip
* DESCRIPTION:         Retrieves 1 hour precipitation totals from the HourlyPP
*                      and HourlyPC tables.  This replaces the ProcPrecip
*                      table as the source of precipitation information.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       September 9, 2004
* ORGANIZATION:        OHD-11, HSEB
* MACHINE:             Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        Sept 9, 2004 Bryon Lawrence    Original Coding
*          1        Nov 17, 2004 Bryon Lawrence    Added call to write_log
*                                                  routine in place of
*                                                  fprintf to stdout.
*                                                  This was causing problems
*                                                  on the cron run.
*          1        01/31/2005   Guoxian Zhou      Using struct data type
*          1        03/21/2005   Guoxian Zhou      retrieve multiple hours data
*          1        10/17/2005   Guoxian Zhou      Added range check for PC data
*          1        10/23/2006   Bryon Lawrence    Changed precip settings to
*                                                  match those used by
*                                                  MPE Fieldgen.
*
* May 2011   S Naples          added check of new option (process_PC field)
*                                  to determine whether or not to
*                                  skip PC to PPH process
*
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "DbmsDefs.h"
#include "get_loc_latlon.h"
#include "get_total_precip.h"
#include "BinarySearch.h"
#include "HourlyPC.h"
#include "HourlyPP.h"
#include "load_PCPP_data.h"
#include "time_convert.h"
#include "time_defs.h"

#include "mpe_fieldgen.h"
#include "polygon_RFCW.h"
#include "delete_polygons_show.h"
#include "draw_precip_poly_RFCW.h"
#include "List.h"

#include "stage3.h"

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    readGagePrecip
* PURPOSE:        Retrieves 1 hour precipitation totals for gages within the
*                 site's MPE area.  1 hour values are used only if:
*                    The 1 hour precipitation total is 0 or greater
*                    The gage location is contained with the site's MPE area
*                    The typesource of the precipitation data is not "PM"
*
*                 In AWIPS builds prior to OB5, the 1 hour precipitation totals
*                 were read from the ProcPrecip table in the IHFS database.
*                 These totals were produced by SIIPP.  Now hourly PC and PP
*                 precipitation amounts are written to the HourlyPC and
*                 HourlyPP tables respectively by GagePP.
*
*                 This routine first loads the hourly rain data from
*                 the HourlyPP and HourlyPC tables.  It then computes
*                 a 1 hour total based on this data for each valid gage.
*
* ARGUMENTS:
*
*   Note that all parameters are passed in as pointers
*   because this routine needs to be callable from Fortran.
*   Fortran passes paramters by reference.
*
*   TYPE   DATA TYPE NAME             DESCRIPTION/UNITS
*   Input  int *     iunit            The unit number of the log file.
*   Input  char *    datetime         The datetime to retrieve the
*                                     data for.
*   Input  int *     max_num_gages    The maximum number of gages
*                                     allowed.
*   Input  int *     hrap_x_orig      The X origin of the local MPE HRAP
*                                     grid in relation to the National grid.
*   Input  int *     hrap_y_orig      The Y origin of the local MPE HRAP
*                                     grid in relation to the National grid.
*   Input  int *     hrap_num_cols    The number of columns in the local
*                                     MPE HRAP grid.
*   Input  int *     hrap_num_rows    The number of rows in the local MPE
*                                     HRAP grid.
*   Input  int       gage_qc         Flag indicating if QC checking should
*                                     be performed.
*   Output char      gage_ids [ ] [ ] The ids of the gages read in.
*   Output float * gage_values        Array of gage values read in.
*   Output int * x_hrap_coords        Array of gage HRAP x coordinates.
*   Output int * y_hrap_coords        Array of gage HRAP y coordinates.
*   Output float * gage_lats          Array of gage latitudes read in.
*   Output float * gage_lons          Array of gage longitudes read in.
*   Output int * num_gages            The number of gages processed.
*
*   Modified by guoxian zhou 01/25/2005
*
*   Input  geo_data_struct *pGeoData        local MPE hrap grid struct data
*
*   Input  FILE *       logFile                    log file
*
*   Output gage_table_struct * pGageTable    The array of pseudo gage struct data
*                                            read from the PseudoGageVal table.
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*   NAME                    HEADER FILE     DESCRIPTION
*   get_loc_latlon
*   get_total_hourly_precip
*   LatLongToHrap
*   load_PC_hourly
*   load_PP_hourly
*   write_log
*   yearsec_ansi_to_timet
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME              DESCRIPTION
*   char [ ]   datetime_cstr     The C-Style datetime strig.
*   char [ ]   message           Contains messages to be written to the log
*                                file.
*   char *     ts                Contains the typesource, "PM", to not process.
*   double     col               The HRAP column a gage is located in.
*   double     lat               The latitude of the gage.
*   double     lon               The longitude of the gage.
*   double     row               The HRAP row a gage is located in.
*   float      min_percent       The amount of the overall duration a computed
*                                precipitation total must cover.
*   HourlyPC * pHourlyPC         The linked list of PC data read from the
*                                HourlyPC table.
*   HourlyPP * pHourlyPP         The linked list of PP data read from the
*                                HourlyPP table.
*   int        icol              The HRAP column a gage is located in
*                                truncated to a whole number.
*   int        irow              The HRAP row a gage is located in
*                                truncated to a whole number.
*   int        pc_rec_cnt        The number of pc data records read from the
*                                database and processed by the precip totaling
*                                routine.
*   int        pp_rec_cnt        The number of pp data records read from the
*                                database and process by the precip totaling
*                                routine.
*   int        status            Generic return code variable for testing
*                                errors.
*   short int  advance           Have the precipitation totaling routine
*                                automatically advance the HourlyPP and
*                                HourlyPC pointers.
*   short int  duration          The number of hours to total data from
*                                the HourlyPC and HourlyPP tables.
*   time_t     end_time_t        The endtime of the interval to retrieve
*                                precip data for in UNIX ticks.
*   time_t     start_time_t      The starttime of the interval to retrieve
*                                precip data for in UNIX ticks.
*   struct total_precip total_precip  Contains the total precipitation for
*                                     one station.
*
*
* DATA FILES AND/OR DATABASE:
* Requires a connection to the IHFS database.  Reads the HourlyPC, HourlyPP
* and Location tables.
*
* ERROR HANDLING:
*   Execution of MPE FieldGen will be aborted if the number of gages read
*   is larger than the maximum allowable number of gages.  All error messages
*   are written to the standard output stream.
*
********************************************************************************
*/
static int compare_gage_lids (  void * string_to_compare,
                                void * string_in_array )
{
   char * gage_id = ( char * ) string_to_compare;
   char * bad_gage_id = * ( char ** ) string_in_array;
   int status;

   status = strcmp ( gage_id, bad_gage_id );
   return status;
} /* end compare_gage_lids */

static int compare_gage_ids ( const void * string_to_compare,
                              const void * string_in_array )
{
   char * gage_id = * ( char ** ) string_to_compare;
   char * bad_gage_id = * ( char ** ) string_in_array;
   int status;

   status = strcmp ( gage_id, bad_gage_id );
   return status;
} /* end compare_gage_ids */

void MPEFieldGen_readGagePrecip ( const int runHours ,
                      char ** datetimes ,
                      const geo_data_struct *pGeoData,
                      const int  gage_qc,
                      gage_table_struct ** pGageTable,
                      gage_table_struct ** pGageTableP3,
                      int * gageNumber,
                      int * gageNumberP3,
                      const run_date_struct * pRunDate )
{
    const static char * ts = "!PM" ;
    double col, lat, lon, row ;
    double min_percent = 0.0 ;
    HourlyPC * pHourlyPC = NULL ;
    HourlyPP * pHourlyPP = NULL ;
    HourlyPC * pOrigHourlyPC = NULL ;
    HourlyPP * pOrigHourlyPP = NULL ;
    int bad_gages_exist = 0;
    int icol  = 0;
    int irow  = 0;
    int pc_rec_cnt ;
    int pp_rec_cnt ;
    int status ;
    short int advance = 1 ;
    short int duration = 1 ; /* The duration in hours. */
    time_t end_time_t ;
    time_t start_time_t ;
    struct total_precip total_precip ;
    int i, total_gages = 0 ;
    int total_gages_p3 = 0;
    int num_bad_gage_records = 0;
    int * num_gages = NULL ;
    int * num_gages_p3 = NULL ;

    double maxPrecip = 0.0 ;
    float gage_value;

	int first = 0;

    FILE *fd = NULL;
    char dirname[120], dirname1[120];
    char bad_gages_file_name[120];
    char snow_list_file_name[120];
    char * pBadTest = NULL;
    char ** bad_gage_array = NULL;
    int len = 0;
    char buf[9];
    int gage_inside_snow_poly = 0;
    struct tm * pRunTime = NULL ;
    FILE *snowList = NULL;


    char file_time_string[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
    List PolyList;
    rubber_poly_data *pPolyNode = NULL;
    int hrap_x, hrap_y, z;
    time_t runTimeInTicks;

    memset(buf, '\0', 9);
    memset(dirname, '\0', 120);
    memset(bad_gages_file_name, '\0', 120);
    len = strlen("mpe_bad_gages_dir");
    get_apps_defaults("mpe_bad_gages_dir",&len,dirname,&len);
    strcpy(bad_gages_file_name, dirname);
    strcat(bad_gages_file_name, "/mpe_bad_gage_list");

    sprintf ( message , "mpe_bad_gage_list=%s.\n", bad_gages_file_name);
    printMessage( message, logFile );

    num_gages = (int *)malloc(runHours * sizeof(int));
    if(num_gages == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
                        " in readGagePrecip function.\n"
                        "\tProgram exit.");
        shutDownMPE( message, logFile );
    }
    num_gages_p3 = (int *)malloc(runHours * sizeof(int));
    if(num_gages_p3 == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
                        " in readGagePrecip function.\n"
                        "\tProgram exit.");
        shutDownMPE( message, logFile );
    }

    /**
     * append the observed precip data
     * after the pseudo precip data.
     **/
    for(i = 0; i < runHours; i++)
    {
        num_gages[i] = pGageTable[i]->pseudoGageNum ;
    }
    for(i = 0; i < runHours; i++)
    {
        num_gages_p3[i] = pGageTableP3[i]->pseudoGageNum ;
    }


    /**
     * Convert the YYYY-MM-DD HH:MM:SS formatted datetime to
     * a time_t (UNIX ticks value).   Note that it is assumed that
     * the minute and seconds portion of the time are 0.
     **/
    status = yearsec_ansi_to_timet ( datetimes[runHours - 1] , & end_time_t ) ;

    if ( status != 0 )
    {
        sprintf( message , "ERROR: in readGagePrecip ... endtime ansi to timet "
                          "conversion failed.\nProgram exit. ") ;
        shutDownMPE( message, logFile );
    }

    /* Compute the ending data retrieval time. */
    start_time_t = end_time_t - runHours * SECONDS_PER_HOUR ;

    /**
     * Optionally load the PC data and load the PP hourly data
     * to use in the hourly precipitation estimates.
     **/
    if(ptrMPEParams->process_PC == 1) {
    	pOrigHourlyPC = load_PC_hourly ( start_time_t, end_time_t, NULL, &ts, 1, &pc_rec_cnt ) ;
    }
    pOrigHourlyPP = load_PP_hourly ( start_time_t, end_time_t, NULL, &ts, 1,
                    & pp_rec_cnt ) ;

    /* Write out message when there is no gage data. */
    if ( ( pOrigHourlyPP == NULL ) && ( pOrigHourlyPC == NULL ) )
    {
        /* Write out the header only once. */
        sprintf( message, "\nSTATUS: There is no precip data "
             "between %s and %s.",
             datetimes[0], datetimes[runHours-1] ) ;
        printMessage( message, logFile );
        return;
    }

    fd = (FILE*) fopen(bad_gages_file_name, "r");
    if(fd == NULL)
    {
         sprintf(message, "Could not open bad gage list file %s ...\n", bad_gages_file_name);
         printMessage(message, logFile);
    }
    else
    {
         sprintf(message, "Opened bad gage list file %s ...\n", bad_gages_file_name);
         printMessage(message, logFile);

         fscanf(fd, "%s", buf);

         /* Count the records. */
         while (!feof(fd))
         {
            ++num_bad_gage_records;
            fscanf(fd, "%s", buf);
         }

         if ( num_bad_gage_records > 0)
         {
             bad_gages_exist = 1;

             /* reset file pointer to beginning of file. */
             rewind ( fd );

             bad_gage_array = ( char ** ) malloc ( sizeof ( char * ) * num_bad_gage_records );

             if ( bad_gage_array == NULL )
             {
                sprintf ( message, "Could not allocate memory for the "
                                   "bad gage array.\n" );
                printMessage ( message, logFile );
                exit ( 1 );
             }

             for ( i = 0; i < num_bad_gage_records; ++i )
             {
                 bad_gage_array [ i ] = NULL;
             }

             /* Read records from the bad gage file. */
             i = 0;
             fscanf (fd, "%s", buf );

             while(( !feof(fd) ) && (i < num_bad_gage_records ) )
             {
                bad_gage_array[i] = (char * ) malloc ( sizeof ( char ) * ( LOC_ID_LEN + 1));

                if ( bad_gage_array[i] == NULL )
                {
                   sprintf ( message, "Could not allocate memory for the "
                                      "bad gage array.\n" );
                   printMessage ( message, logFile );
                   exit ( 1 );
                }
                memset ( bad_gage_array[i], '\0', LOC_ID_LEN + 1 );
                strncpy ( bad_gage_array[i], buf, LOC_ID_LEN );
                ++i;
                fscanf (fd, "%s", buf );
             }

             fclose ( fd );
             fd = NULL;

             qsort ( bad_gage_array, num_bad_gage_records,
                     sizeof ( char * ), compare_gage_ids );
         }
    }


/* Compute the total hourly precipitation. */
    for(i = 0; i < runHours; i++)
    {
        total_gages = 0 ;
        total_gages_p3 = 0;

		first = 0;

        /* Compute the ending data retrieval time. */
        end_time_t = start_time_t + SECONDS_PER_HOUR ;

        runTimeInTicks = pRunDate->tRunTime - ( i * SECONDS_PER_HOUR );
        pRunTime = gmtime ( &runTimeInTicks );
        strftime (file_time_string, ANSI_YEARSEC_TIME_LEN + 1, "%Y%m%d%H", pRunTime);
        get_snow_polygons (&PolyList, file_time_string);

        len = 0;
	len = strlen("rfcwide_drawpre_dir");
        memset(dirname, '\0', 120);
        get_apps_defaults("rfcwide_drawpre_dir",&len,dirname,&len);
    	memset(snow_list_file_name, '\0', 120);
    	strcpy(snow_list_file_name, dirname);
    	strcat(snow_list_file_name, "/snow_gages");
    	strcat(snow_list_file_name, file_time_string);
    	strcat(snow_list_file_name, "z");

    	pHourlyPC = pOrigHourlyPC ;
        pHourlyPP = pOrigHourlyPP ;

        while ( ( pHourlyPP != NULL ) || ( pHourlyPC != NULL ))
        {

           /* Initialize the total_precip structure. */
           memset(total_precip.lid, '\0' , LOC_ID_LEN + 1 ) ;
           memset(total_precip.PE , '\0' , SHEF_PE_LEN + 1 ) ;
           memset(total_precip.TS , '\0' , SHEF_TS_LEN + 1 ) ;

           /* Get the total precip for the station. */
            total_precip =
            get_total_hourly_precip ( & pHourlyPC ,
                                      & pHourlyPP ,
                                      end_time_t ,
                                      duration ,
                                      min_percent ,
                                      PRECIP_TS_RANK | PRECIP_PP ,
                                      advance ,
                                      & pc_rec_cnt ,
                                      & pp_rec_cnt ) ;

           /* Test if the gage is in the bad gage list */
           if ( bad_gages_exist == 1 )
           {
               pBadTest = NULL;
               pBadTest = (char *) binary_search ( bad_gage_array,
                                                   total_precip.lid,
                                                   num_bad_gage_records,
                                                   sizeof ( char * ),
                                                   compare_gage_lids );

               if ( pBadTest != NULL )
               {
                  sprintf ( message, "Gage %s is in bad gage file. Skipped ...\n",
                            total_precip.lid );
                  printMessage ( message, logFile );
               }
           }

            /* For each station with a precipitation total greater than
               or equal to 0.0, process the gage's value. */

            if (pBadTest == NULL &&
                total_precip.value >= 0.0 &&
                total_precip.value != MISSING_PRECIP)
            {


	         /**
                 * Load the max hourly precip data
                 * for range check if the PE type is "PC".
                 **/
                 if(strcmp(total_precip.PE, "PC") == 0)
                 {
                    maxPrecip = MPEFieldGen_readPrecipLimit(total_precip.lid, end_time_t);

                    if(maxPrecip != RANGE_CHECK_DEFAULT)
                    {
                        if(total_precip.value > maxPrecip)
                        {
                            sprintf( message , "WARNING: gage value is greater"
                                " than the max precip value for %s.",
                                total_precip.lid ) ;
                            printMessage( message, logFile );

                            continue ;
                        }
                    }
                }


	    /* Retrieve the stations latitude and longitude and convert to
                obtain the station's HRAP coordinates. */
                status = get_mpe_loc_latlon(total_precip.lid, &lat, &lon) ;

                if ( first == 0 )
                {
                    /* Write out the header only once. */
                    sprintf( message, "\nSTATUS: loading precip for %s\n"
                         "    #        ID      X      Y  VALUE(mm)",
                         datetimes[i] ) ;
                    printMessage( message, logFile );
					first = 1;
                }

                if ( status == LATLONNOTFOUND )
                {
                    sprintf( message , "WARNING: in readGagePrecip ... "
                        "could not retrieve "
                        "lat/lon for %s." , total_precip.lid ) ;
                    printMessage( message, logFile );
                }
                else
                {

                    /* Retrieve the HRAP coordinates for this gage. */
                    LatLongToHrapByReference ( lat , lon , &row , &col ) ;

                    /* Truncate to find integer HRAP coordinates which gage is in.
                       Translate the origin to the lowerleft corner of the MPE
                       estimation domain, that is, convert from the global to the
                       local HRAP grid. */
                    irow = ( int )row ;
                    icol = ( int )col ;

                    irow -= pGeoData->hrap_y ;
                    icol -= pGeoData->hrap_x ;


                       total_precip.value *= 25.4 ;

                    gage_inside_snow_poly = 0;
      	            pPolyNode = (rubber_poly_data *) ListFirst (&PolyList);
                    while (pPolyNode != NULL)
                    {
                       if (pPolyNode->snow_flag == True)
                       {
                          gage_value = total_precip.value;

                         // Convert the gage value to inches.
                         gage_value /= 25.4;

                         if (gage_value <= pPolyNode->draw_precip_value)
                         {
                            hrap_x = icol;
                            hrap_y = irow;

                            if ((hrap_x <= pPolyNode->maxx) &&
                            (hrap_x >= pPolyNode->minx) &&
                            (hrap_y <= pPolyNode->maxy) && (hrap_y >= pPolyNode->miny))
                            {
                               // Determine if this gage is inside or outside the edit polygon.
                               z = InOutPoly (hrap_x, hrap_y, pPolyNode);

                               if (z == 1)
                               {
                                  gage_inside_snow_poly = 1;
                                  break;     //Don't need to loop through any more polygons.
                               }
                            }
                        }
                    }
                    pPolyNode = (rubber_poly_data *) ListNext (&pPolyNode->node);
                 }

			//Added by Ram
			//code added to read in all the pseudo gages which are outside the HRAP area
			//into the p3 gage structure (ABRFC requirement). This is just for the
			//p3lmosaic routine. All other mosaics use gages inside the HRAP area
			//--------------------------------------------------------------------
                        if(gage_inside_snow_poly == 1)
                        {
                           gage_inside_snow_poly = 0;

                           /* Open the snow file the first time a gage is found. */
                           if(snowList == NULL)
                           {
                              snowList = fopen(snow_list_file_name, "a+");

                              if ( snowList == NULL )
                              {
                                 sprintf( message,
                                 "Warning: could not open snow list file name %s", snow_list_file_name);
                                 return;
                              }
                           }

                           fprintf(snowList, "%s\n", total_precip.lid);
                           fflush(snowList);

                           continue;
                        }

                        ++ total_gages_p3;
		/*	sprintf( message , "%5d  %8s  %5d  %5d    %7.2f" ,
                                            ++ total_gages_p3 , total_precip.lid ,
                                            icol , irow , total_precip.value ) ;
                        printMessage( message, logFile );
		*/
                        /**
                          Store the HRAP coordinates and gage values in the
                          arrays which will be passed back to the caller of this
                          routine.

                          Be sure to convert the total precipitation value
                          from inches to millimeters.   When the precip data
                          was read from the ProcPrecip table it was in
                          Now (OB5 and beyond) the precipitation totals are read
                          millimeters. from the HourlyPC and HourlyPP tables.
                          These values are now in inches.
                         **/

                        /**
                         * Test to see if the number of gages has exceeded
                         * the capacity of the gage array.
                         **/

                        if ( num_gages_p3[i] >= gageNumberP3[i] )
                        {
                            sprintf ( message , "ERROR: in get_gage_precip ... "
                                  "number of gages exceeds capacity of gage "
                                  "array.\n" ) ;
                            shutDownMPE( message, logFile );
                        }

		        pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].gageValue
                                 = total_precip.value ;
                        strncpy(pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].gageID,
                                total_precip.lid, LOC_ID_LEN) ;
                        pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].gageID[LOC_ID_LEN]
                                 = '\0' ;
                        strncpy(pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].gageTS,
                                total_precip.TS, SHEF_TS_LEN) ;
                        pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].gageTS[SHEF_TS_LEN]
                                 = '\0' ;
		        pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].hrap_x = icol ;
                        pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].hrap_y = irow ;
                        pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].latitude
                                 = lat ;
                        pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].longitude
                                 = lon ;
                        num_gages_p3[i] ++ ;

		//--------------------------------------------------------------

		/* Test to determine whether or not this gage is inside the
                       office's MPE area. */
                    if ( ( irow >= 0 ) && ( irow < pGeoData->num_rows ) &&
                         ( icol >= 0 ) && ( icol < pGeoData->num_cols ) )
                    {


                        sprintf( message , "%5d  %8s  %5d  %5d    %7.2f" ,
                                            ++ total_gages , total_precip.lid ,
                                            icol , irow , total_precip.value ) ;
                        printMessage( message, logFile );

                        /**
                          Store the HRAP coordinates and gage values in the
                          arrays which will be passed back to the caller of this
                          routine.

                          Be sure to convert the total precipitation value
                          from inches to millimeters.   When the precip data
                          was read from the ProcPrecip table it was in
                          Now (OB5 and beyond) the precipitation totals are read
                          millimeters. from the HourlyPC and HourlyPP tables.
                          These values are now in inches.
                         **/

                        /**
                         * Test to see if the number of gages has exceeded
                         * the capacity of the gage array.
                         **/
                        if ( num_gages[i] >= gageNumber[i] )
                        {
                            sprintf ( message , "ERROR: in get_gage_precip ... "
                                  "number of gages exceeds capacity of gage "
                                  "array.\n" ) ;
                            shutDownMPE( message, logFile );
                        }

                        pGageTable[i]->ptrGageRecords[num_gages[i]].gageValue
                                 = total_precip.value ;
                        strncpy(pGageTable[i]->ptrGageRecords[num_gages[i]].gageID,
                                total_precip.lid, LOC_ID_LEN) ;
                        pGageTable[i]->ptrGageRecords[num_gages[i]].gageID[LOC_ID_LEN]
                                 = '\0' ;
                        strncpy(pGageTable[i]->ptrGageRecords[num_gages[i]].gageTS,
                                total_precip.TS, SHEF_TS_LEN) ;
                        pGageTable[i]->ptrGageRecords[num_gages[i]].gageTS[SHEF_TS_LEN]
                                 = '\0' ;
                        pGageTable[i]->ptrGageRecords[num_gages[i]].hrap_x
                                 = icol ;
                        pGageTable[i]->ptrGageRecords[num_gages[i]].hrap_y
                                 = irow ;
                        pGageTable[i]->ptrGageRecords[num_gages[i]].latitude
                                 = lat ;
                        pGageTable[i]->ptrGageRecords[num_gages[i]].longitude
                                 = lon ;
                        num_gages[i] ++ ;
            }
          }

	  }
       }

        if(snowList != NULL)
	{
	   fclose(snowList);
	   snowList = NULL;
	}

        /* Write out log message if there is no data for a hour. */
        if ( first == 0 )
        {
            sprintf( message, "\nSTATUS: There is no precip data for %s."
                 , datetimes[i] ) ;
            printMessage( message, logFile );
        }

        start_time_t += SECONDS_PER_HOUR ;

}

    for(i = 0; i < runHours; i++)
    {
        if(num_gages[i] > 0)
            pGageTable[i]->totalGageNum = num_gages[i] ;
    }
    for(i = 0; i < runHours; i++)
    {
        if(num_gages_p3[i] > 0)
            pGageTableP3[i]->totalGageNum = num_gages_p3[i] ;
    }

    if(num_gages_p3 != NULL)
    {
        free(num_gages_p3);
        num_gages_p3 = NULL ;
    }

    if(pOrigHourlyPC != NULL)
    {
        FreeHourlyPC(pOrigHourlyPC);
        pOrigHourlyPC = NULL ;
    }

    if(pOrigHourlyPP != NULL)
    {
        FreeHourlyPP(pOrigHourlyPP);
        pOrigHourlyPP = NULL ;
    }



} /* end MPEFieldGen_readGagePrecip */


//---------------------------------------------------------------------------------------------


void copyGage(gage_record_struct *gage_record_ptr,  gage_struct *gagePtr, double lat, double lon)
{

    gage_record_ptr->gageValue = gagePtr->gval;

    strncpy(gage_record_ptr->gageID, gagePtr->id, LOC_ID_LEN) ;
    gage_record_ptr->gageID[LOC_ID_LEN] = '\0' ;

    strncpy(gage_record_ptr->gageTS, gagePtr->ts, SHEF_TS_LEN);
    gage_record_ptr->gageTS[SHEF_TS_LEN] = '\0' ;

    gage_record_ptr->hrap_x  = (int) gagePtr->hrap_loc.x ;
    gage_record_ptr->hrap_y  = (int) gagePtr->hrap_loc.y ;

    gage_record_ptr->latitude  = lat ;
    gage_record_ptr->longitude = lon ;

    return;
} /* end copyGage */


//---------------------------------------------------------------------------------------------

void readGagePrecipFromMpeEditor ( const int runHours ,
                      char ** datetimes ,
                      const geo_data_struct *pGeoData,
                      const int  gage_qc,
                      gage_table_struct ** pGageTable,
                      gage_table_struct ** pGageTableP3,
                      int * gageNumber,
                      int * gageNumberP3,
                      const run_date_struct * pRunDate )
{

    double lat;
    double lon;
    int icol  = 0;
    int irow  = 0;

    int status ; //used once

    time_t start_time_t ; //used once

    int i;
    int k;
    int total_gages = 0 ;
    int total_gages_p3 = 0;
    int gageIndex = 0;

    int first = 0;

    gage_record_struct *gage_record_ptr = NULL;
    gage_struct *gagePtr = NULL;


    printMessage( "readGagePrecipFromMpeEditor() version 10/08/08", logFile );

/* Compute the total hourly precipitation. */
    for(i = 0; i < runHours; i++)
    {
        total_gages = 0 ;
        total_gages_p3 = 0;

                first = 0;


        for(k=0; k < ngages; k++)
        {
            /* For each station with a precipitation total greater than
               or equal to 0.0, process the gage's value. */

           gageIndex = total_gages_p3;
           total_gages_p3 ++;



                   if ( total_gages_p3 >= gageNumberP3[i] )
           {
                  sprintf ( message , "ERROR: in get_gage_precip ... "
                                  "number of gages exceeds capacity of gage "
                                  "array.\n" ) ;
                  shutDownMPE( message, logFile );
           }

           status = get_mpe_loc_latlon(gage[gageIndex].id, &lat, &lon) ;


           //copy gage information
           gage_record_ptr  = &(pGageTableP3[i]->ptrGageRecords[gageIndex]);
           gagePtr =  &(gage[k]);


           copyGage(gage_record_ptr, gagePtr, lat, lon);


        //--------------------------------------------------------------

        /* Test to determine whether or not this gage is inside the
                       office's MPE area. */

           gageIndex = total_gages;

           if ( ( (int) gagePtr->hrap_loc.y >= 0 ) &&
                ( (int) gagePtr->hrap_loc.y < pGeoData->num_rows ) &&
                ( (int) gagePtr->hrap_loc.x >= 0 ) &&
                ( (int) gagePtr->hrap_loc.x < pGeoData->num_cols ) )
           {


               total_gages++;

               sprintf( message , "%5d  %8s  %5d  %5d    %7.2f" ,
                        total_gages , gagePtr->id ,
                        (int) gagePtr->hrap_loc.x,
                        (int) gagePtr->hrap_loc.y,
                        gagePtr->gval ) ;
               printMessage( message, logFile );

               /**
                * Test to see if the number of gages has exceeded
                * the capacity of the gage array.
               **/
               if ( total_gages >= gageNumber[i] )
               {
                   sprintf ( message , "ERROR: in get_gage_precip ... "
                                  "number of gages exceeds capacity of gage "
                                  "array.\n" ) ;
                   shutDownMPE( message, logFile );
               }


               gage_record_ptr  = &(pGageTable[i]->ptrGageRecords[gageIndex]);
               gagePtr =  &(gage[k]);


               //copy gage information
               copyGage(gage_record_ptr, gagePtr, lat, lon);


           }  // end if

        }  //end for k


        /* Write out log message if there is no data for a hour. */
        if ( first == 0 )
        {
            sprintf( message, "\nSTATUS: There is no precip data for %s."
                 , datetimes[i] ) ;
            printMessage( message, logFile );
        }

        start_time_t += SECONDS_PER_HOUR ;


        //set the total number of real pseudo gages for each hour
        pGageTable[i]->totalGageNum = pGageTable[i]->pseudoGageNum + total_gages;
        pGageTableP3[i]->totalGageNum = pGageTableP3[i]->pseudoGageNum + total_gages_p3;


    } //end for i  (runHours)

} /* end readGagePrecipFromMpeEditor */

