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
*          1        11/26/2006   Guoxian Zhou      modified for HRAP/Quarter HRAP
********************************************************************************
*/

#include <stdio.h>
#include <string.h>
#include <time.h>

#include "DbmsDefs.h"
#include "HourlyPC.h"
#include "HourlyPP.h"
#include "empe_fieldgen.h"
#include "get_loc_latlon.h"
#include "get_total_precip.h"
#include "load_PCPP_data.h"
#include "time_convert.h"
#include "time_defs.h"


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

void readGagePrecip ( const int runHours ,
                      char ** datetimes ,
                      const int hrap_grid_factor ,
                      const geo_data_struct *pGeoData,                     
                      const int  gage_qc, 
                      gage_table_struct ** pGageTable,
                      gage_table_struct ** pGageTableP3,
                      int * gageNumber, 
                      int * gageNumberP3 ) 
{
    const static char * ts = "!PM" ;
    double col, lat, lon, row ;
    double min_percent = 0.0 ;
    HourlyPC * pHourlyPC = NULL ;
    HourlyPP * pHourlyPP = NULL ;
    HourlyPC * pOrigHourlyPC = NULL ;
    HourlyPP * pOrigHourlyPP = NULL ;
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
    int * num_gages = NULL ;
    int * num_gages_p3 = NULL ;

    double maxPrecip = 0.0 ;

    int first = 0;

    num_gages = (int *)malloc(runHours * sizeof(int));
    if(num_gages == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
                        " in readGagePrecip function.\n"
                        "\tProgram exit.");
        shutdown( message);
    }
    num_gages_p3 = (int *)malloc(runHours * sizeof(int));
    if(num_gages_p3 == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
                        " in readGagePrecip function.\n"
                        "\tProgram exit.");
        shutdown( message);
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
        shutdown( message);
    }

    /* Compute the ending data retrieval time. */

    start_time_t = end_time_t - runHours * SECONDS_PER_HOUR ;

    /**
     * Load the PC and PP multiple hourly data 
     * to use in the hourly precipitationestimates.
     **/

    pOrigHourlyPC = load_PC_hourly ( start_time_t, end_time_t, NULL, &ts, 1, 
                    & pc_rec_cnt ) ;
    pOrigHourlyPP = load_PP_hourly ( start_time_t, end_time_t, NULL, &ts, 1,
                    & pp_rec_cnt ) ;

    /* Write out message when there is no gage data. */

    if ( ( pOrigHourlyPP == NULL ) && ( pOrigHourlyPC == NULL ) )
    {
        /*
         * Write out the header only once.
         */

        if(runHours > 1)
        { 
            sprintf( message, "\nSTATUS: There is no precip data "
                 "between %s and %s.", 
                 datetimes[0], datetimes[runHours-1] ) ;
            hpe_fieldgen_printMessage( message);
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

        pHourlyPC = pOrigHourlyPC ;
        pHourlyPP = pOrigHourlyPP ;

        while ( ( pHourlyPP != NULL ) || ( pHourlyPC != NULL ) )
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

            /* For each station with a precipitation total greater than 
               or equal to 0.0, process the gage's value. */

            if ( ( total_precip.value >= 0.0 ) &&
               ( total_precip.value != MISSING_PRECIP ) )
            {
                /**
                 * Load the max hourly precip data 
                 * for range check if the PE type is "PC".
                 **/

                 if(strcmp(total_precip.PE, "PC") == 0)
                 {
                    maxPrecip = readPrecipLimit(total_precip.lid, end_time_t);

                    if(maxPrecip != RANGE_CHECK_DEFAULT)
                    {
                        if(total_precip.value > maxPrecip)
                        {
                            sprintf( message , "WARNING: gage value is greater"
                                " than the max precip value for %s.",
                                total_precip.lid ) ; 
                            hpe_fieldgen_printMessage( message);

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
                    hpe_fieldgen_printMessage( message);
                    first = 1;
                }

                if ( status == LATLONNOTFOUND )
                {
                    sprintf( message , "WARNING: in readGagePrecip ... "
                        "could not retrieve "
                        "lat/lon for %s." , total_precip.lid ) ; 
                    hpe_fieldgen_printMessage( message);
                }
                else
                {
			        /*
			         * Retrieve the HRAP coordinates for this gage.
			         *
			         * modified to calculate HRAP/Quarter HRAP grid.
			         * -- gzhou 11/2006 
			         */
		
			        /*
                    LatLongToHrap ( lat , lon , &row , &col ) ;
		            */
		
		            LatLongToScaledHrap(lat, lon,
		                                hrap_grid_factor,
		                                &row , &col );

                    /* Truncate to find integer HRAP coordinates which gage is in.
                       Translate the origin to the lowerleft corner of the MPE 
                       estimation domain, that is, convert from the global to the 
                       local HRAP grid. */

                    irow = ( int )row ;
                    icol = ( int )col ;

                    irow -= pGeoData->hrap_y ;
                    icol -= pGeoData->hrap_x ;


                       total_precip.value *= 25.4 ;

                        
            //Added by Ram
            //code added to read in all the pseudo gages which are outside the HRAP area
            //into the p3 gage structure (ABRFC requirement). This is just for the 
            //p3lmosaic routine. All other mosaics use gages inside the HRAP area
            //--------------------------------------------------------------------
    
        ++ total_gages_p3;
        /*    sprintf( message , "%5d  %8s  %5d  %5d    %7.2f" ,
                                            ++ total_gages_p3 , total_precip.lid ,
                                            icol , irow , total_precip.value ) ; 
                        printMessage( message);
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
                            shutdown( message);
                        }

                        pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].gageValue
                                 = total_precip.value ;
                        pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].hrap_x = icol ;
                        pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].hrap_y = irow ; 
                        strncpy(pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].gageID,
                                total_precip.lid, LOC_ID_LEN) ;
                        pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].gageID[LOC_ID_LEN]
                                 = '\0' ;
                        strncpy(pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].gageTS,
                                total_precip.TS, SHEF_TS_LEN) ;
                        pGageTableP3[i]->ptrGageRecords[num_gages_p3[i]].gageTS[SHEF_TS_LEN]
                                 = '\0' ;
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
                                          icol , irow , total_precip.value ); 
                        hpe_fieldgen_printMessage( message);

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
                            shutdown( message);
                        }

                        pGageTable[i]->ptrGageRecords[num_gages[i]].gageValue
                                 = total_precip.value ;
                        pGageTable[i]->ptrGageRecords[num_gages[i]].hrap_x
                                 = icol ;
                        pGageTable[i]->ptrGageRecords[num_gages[i]].hrap_y
                                 = irow ; 
                        strncpy(pGageTable[i]->ptrGageRecords[num_gages[i]].gageID,
                                total_precip.lid, LOC_ID_LEN) ;
                        pGageTable[i]->ptrGageRecords[num_gages[i]].gageID[LOC_ID_LEN]
                                 = '\0' ;
                        strncpy(pGageTable[i]->ptrGageRecords[num_gages[i]].gageTS,
                                total_precip.TS, SHEF_TS_LEN) ;
                        pGageTable[i]->ptrGageRecords[num_gages[i]].gageTS[SHEF_TS_LEN]
                                 = '\0' ;
                        pGageTable[i]->ptrGageRecords[num_gages[i]].latitude
                                 = lat ;
                        pGageTable[i]->ptrGageRecords[num_gages[i]].longitude
                                 = lon ; 

                        num_gages[i] ++ ;
                    }
                }
            }
        }

        /* Write out log message if there is no data for a hour. */
        if ( first == 0 )
        {
            sprintf( message, "\nSTATUS: There is no precip data for %s."
                 , datetimes[i] ) ; 
            hpe_fieldgen_printMessage( message);
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

    if(num_gages != NULL)
    {
        free(num_gages);
        num_gages = NULL ;
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
}
