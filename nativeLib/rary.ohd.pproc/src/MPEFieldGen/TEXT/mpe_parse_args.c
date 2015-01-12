/*******************************************************************************
* FILENAME:            mpe_parse_args.c
*
* This function parses the arguments from the command line
* and plugs the time data into the run_date_struct variable.
*
* calling function:        main_mpe_fieldgen
* functions called:        none
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         March, 2005
* ORGANIZATION:          HSEB / OHD
* MACHINE:               HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE           PROGRAMMER        DESCRIPTION/REASON
*   March   2005   Guoxian Zhou      Finish first version 
*   Jun 28, 2005   Guoxian Zhou      Finish testing 
*
********************************************************************************
*/

#include "mpe_fieldgen.h"

void parseArgs(const int argc ,
                char **argv ,
                run_date_struct *pRunDate)
{
    int  i, argData;
    char tmpString[ANSI_YEARSEC_TIME_LEN + 1];
    time_t tmpTime;
    struct tm * pRunTime = NULL ;
    struct tm time_struct ;

    logFile = NULL ;

    /************************************************************************
     * Verify the argument list and insert the values into variable runDate.
     **/

    /* Check valid argument number */
    if(argc != 2 && argc != 4)
    {
        printUsage();
        sprintf ( message , "ERROR: Invalid argument number."
                    "\n\tProgram exit." ) ;
        shutDownMPE( message, logFile );
    }

    /**
     * Check valid argument data
     * All arguments should be digit data.
     **/
    for(i = 1; i < argc; i++)
    {
        memset(tmpString, '\0', ANSI_YEARSEC_TIME_LEN + 1);
        strncpy(tmpString, argv[i], 8);
        if( isDigits( tmpString ) == 0)
        {
            printUsage();
            sprintf ( message , "ERROR: Invalid argument data: \"%s\"."
                    "\n\tProgram exit.", tmpString ) ;
            shutDownMPE( message, logFile );
        }
    }

    /**
     * Check valid hour number
     * hour number must be [1,24].
     **/
    argData = atoi(argv[1]);
    if(argData < 1 || argData > 24)
    {
        printUsage();
        sprintf ( message , "ERROR: Invalid number of hours: \"%s\"."
                    "\n\tProgram exit.", argv[1] ) ;
        shutDownMPE( message, logFile );
    }
    else
    {
        pRunDate->hourNum = argData;
    }

    /**
     * Check valid argument data for ending hour and ending date
     * All arguments should be digit data.
     * If argc = 2, use current date.
     *
     * Initialize the date value with current date.
     **/
    time(&tmpTime);    
    pRunTime = gmtime(&tmpTime);

    memset(&time_struct, '\0', sizeof(struct tm)) ;

    /**
     * Always set the minutes and seconds to 0.
     **/
    time_struct.tm_year = pRunTime->tm_year;
    time_struct.tm_mon  = pRunTime->tm_mon; 
    time_struct.tm_mday = pRunTime->tm_mday;
    time_struct.tm_hour = pRunTime->tm_hour;
    time_struct.tm_min = 0;
    time_struct.tm_sec = 0;

    if(argc == 4)
    {
        argData = atoi(argv[2]);
        
        if(argData < 0 || argData > 23)
        {
            printUsage();
            sprintf ( message , "ERROR: Invalid ending hour of runs: \"%s\"."
                        "\n\tProgram exit.", argv[2] ) ;
            shutDownMPE( message, logFile );
        }
        else
        {
            time_struct.tm_hour = argData;
        }

        /**
         * Pick up the user defined date value.
         **/
        argData = atoi(argv[3]);
        
        /**
         * Pick up the month data.
         **/
        int tmpDate = argData / 1000000;
        if(tmpDate < 1 || tmpDate > 12)
        {
            printUsage();
            sprintf ( message , "ERROR: Invalid month value of runs: \"%s\"."
                        "\n\tProgram exit.", argv[3] ) ;
            shutDownMPE( message, logFile );
        }
        else
        {
            time_struct.tm_mon = tmpDate - 1;
        }

        /**
         * Pick up the day data.
         **/
        tmpDate = (argData % 1000000) / 10000;
        if(tmpDate < 1 || tmpDate > 31)
        {
            printUsage();
            sprintf ( message , "ERROR: Invalid day value of runs: \"%s\"."
                        "\n\tProgram exit.", argv[3] ) ;
            shutDownMPE( message, logFile );
        }
        else
        {
            time_struct.tm_mday = tmpDate;
        }

        /* Pick up the year data. */
        tmpDate = argData % 10000;
        if(tmpDate < 1900)
        {
            printUsage();
            sprintf ( message , "ERROR: Invalid year value of runs: \"%s\"."
                        "\n\tProgram exit.", argv[3] ) ;
            shutDownMPE( message, logFile );
        }
        else
        {
            time_struct.tm_year = tmpDate - 1900;
        }
    }

    /**
     * Save the run time data in time_t format.
     **/
    pRunDate->tRunTime = gm_mktime(&time_struct) ;

} /* end parseArgs */
