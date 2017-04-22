/***********************************************************************
* Filename: mpe_parse_args.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: March 2005
*
* Development Group: HSEB/OHD
*
* Description:
* parse the command line parameter(s)
* 
* Modules:
* parseArgs
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include <unistd.h>
#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: parseArgs
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: March 2005
* 
* Description:
*   This function parses the arguments from the command line
*   and plugs the time data into the run_date_struct variable.
*
* calling function: main_empe_fieldgen
* functions called: 
*
* Calling Arguments:
* Name         Input/Output Type             Description
*
* argc         Input        int              the number of parameters.
* argv         Input        char**           the list of parameters.
* pRunDate     Input        run_date_struct* the run date/time
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* 
*
* Return Value:
* Type          Description
* None
*
* Error Codes/Exceptions:
* 
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type       Description
*
* Modification History:
*   DATE           PROGRAMMER        DESCRIPTION/REASON
*   March   2005   Guoxian Zhou      Finish first version 
*   Aug 03, 2006   Guoxian Zhou      Using getopt function for parsing,
*                                    add parsing for minute and lag time
*
***********************************************************************/

void hpe_fieldgen_parseArgs(const int argc ,
                char **argv ,
                run_date_struct *pRunDate)
{
    time_t tmpTime;
    struct tm * pRunTime = NULL ;
    struct tm time_struct ;

    logFile = NULL ;

    int option;
    int num_run = 1;
    int lag_time = 0;
    int run_time = -1;
    int run_date = -1;

    while ((option = getopt (argc, argv, "n:t:d:l:")) != EOF)
    {
        switch (option)
        {

            /*
             * parse the parameter for the number of run
             */ 

            case 'n':
                if(optarg == NULL)
                {
                    hpe_fieldgen_printUsage();
                    sprintf ( message , "ERROR: Invalid argument."
                                        "\n\tProgram exit." ) ;
                    shutdown( message );
                }

                /**
                 * Check valid argument data,
                 * argument should be digit data.
                 **/

                if( hpe_fieldgen_isDigits( optarg ) == 0)
                {
                    hpe_fieldgen_printUsage();
                    sprintf ( message , "ERROR: Invalid argument data: \"%s\"."
                            "\n\tProgram exit.", optarg ) ;
                    shutdown( message );
                }

                num_run = atoi(optarg);
                break;

            /*
             * parse the parameter for the hour/minute  
             */ 

            case 't':
                if(optarg == NULL)
                {
                    hpe_fieldgen_printUsage();
                    sprintf ( message , "ERROR: Invalid argument."
                                        "\n\tProgram exit." ) ;
                    shutdown( message );
                }

                /*
                 * Check valid argument data
                 * argument should be digit data.
                 */

                if( hpe_fieldgen_isDigits( optarg ) == 0)
                {
                    hpe_fieldgen_printUsage();
                    sprintf ( message , "ERROR: Invalid argument data: \"%s\"."
                            "\n\tProgram exit.", optarg ) ;
                    shutdown( message );
                }

                run_time = atoi(optarg);
                break;

            /*
             * parse the parameter for the date  
             */ 

            case 'd':
                if(optarg == NULL)
                {
                    hpe_fieldgen_printUsage();
                    sprintf ( message , "ERROR: Invalid argument."
                                        "\n\tProgram exit." ) ;
                    shutdown( message );
                }

                /*
                 * Check valid argument data
                 * argument should be digit data.
                 */

                if( hpe_fieldgen_isDigits( optarg ) == 0)
                {
                    hpe_fieldgen_printUsage();
                    sprintf ( message , "ERROR: Invalid argument data: \"%s\"."
                            "\n\tProgram exit.", optarg ) ;
                    shutdown( message );
                }

                run_date = atoi(optarg);
                break;

            /*
             * parse the parameter for the lag time  
             */ 

            case 'l':
                if(optarg == NULL)
                {
                    hpe_fieldgen_printUsage();
                    sprintf ( message , "ERROR: Invalid argument."
                                        "\n\tProgram exit." ) ;
                    shutdown( message );
                }

                /*
                 * Check valid argument data
                 * argument should be digit data.
                 */

                if( hpe_fieldgen_isDigits( optarg ) == 0)
                {
                    hpe_fieldgen_printUsage();
                    sprintf ( message , "ERROR: Invalid argument data: \"%s\"."
                            "\n\tProgram exit.", optarg ) ;
                    shutdown( message );
                }

                lag_time = atoi(optarg);
                break;
        }
    }

    /*
     * Check valid run number
     * run number must be [1,24].
     */

    if(num_run < 1 || num_run > 24)
    {
        hpe_fieldgen_printUsage();
        sprintf ( message , "ERROR: Invalid number of hours: \"%d\"."
                    "\n\tProgram exit.", num_run ) ;
        shutdown( message );
    }
    else
    {
        pRunDate->hourNum = num_run;
    }

    /*
     * Check valid argument data for ending hour and ending date
     * All arguments should be digit data.
     *
     * Initialize the date value with current date.
     * Always set the seconds to 0.
     */

    time(&tmpTime);    
    pRunTime = gmtime(&tmpTime);

    memset(&time_struct, '\0', sizeof(struct tm)) ;

    time_struct.tm_year = pRunTime->tm_year;
    time_struct.tm_mon  = pRunTime->tm_mon; 
    time_struct.tm_mday = pRunTime->tm_mday;
    time_struct.tm_hour = pRunTime->tm_hour;
    time_struct.tm_min  = pRunTime->tm_min;
    time_struct.tm_sec  = 0;

    /*
     * Pick up the user defined hour/minute value.
     */

    if(run_time >= 0)
    {
        int tmpHour = run_time / 100;
        
        if(tmpHour < 0 || tmpHour > 23)
        {
            hpe_fieldgen_printUsage();
            sprintf ( message , "ERROR: Invalid ending hour of runs: \"%d\"."
                        "\n\tProgram exit.", run_time ) ;
            shutdown( message );
        }
        else
        {
            time_struct.tm_hour = tmpHour;
        }

        int tmpMinute = run_time - tmpHour * 100;
        
        if(tmpMinute < 0 || tmpMinute > 59)
        {
            hpe_fieldgen_printUsage();
            sprintf ( message , "ERROR: Invalid ending minute of runs: \"%d\"."
                        "\n\tProgram exit.", run_time ) ;
            shutdown( message );
        }
        else
        {
            time_struct.tm_min = tmpMinute;
        }
    }

    /*
     * Pick up the user defined date value.
     */

    if(run_date > 0)
    {

        /*
         * Pick up the month data.
         */

        int tmpDate = run_date / 1000000;
        if(tmpDate < 1 || tmpDate > 12)
        {
            hpe_fieldgen_printUsage();
            sprintf ( message , "ERROR: Invalid month value of runs: \"%d\"."
                        "\n\tProgram exit.", run_date ) ;
            shutdown( message );
        }
        else
        {
            time_struct.tm_mon = tmpDate - 1;
        }

        /*
         * Pick up the day data.
         */

        tmpDate = (run_date % 1000000) / 10000;
        if(tmpDate < 1 || tmpDate > 31)
        {
            hpe_fieldgen_printUsage();
            sprintf ( message , "ERROR: Invalid day value of runs: \"%d\"."
                        "\n\tProgram exit.", run_date ) ;
            shutdown( message );
        }
        else
        {
            time_struct.tm_mday = tmpDate;
        }

        /*
         * Pick up the year data.
         */

        tmpDate = run_date % 10000;
        if(tmpDate < 1900)
        {
            hpe_fieldgen_printUsage();
            sprintf ( message , "ERROR: Invalid year value of runs: \"%d\"."
                        "\n\tProgram exit.", run_date ) ;
            shutdown( message );
        }
        else
        {
            time_struct.tm_year = tmpDate - 1900;
        }
    }

    /*
     * Save the run time data in time_t format.
     */

    pRunDate->tRunTime = gm_mktime(&time_struct) ;

    /*
     * adjust the run time by diminishing the lag time.
     */

    pRunDate->tRunTime -= lag_time * 60;

}
