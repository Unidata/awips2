/********************************************************************************
* FILENAME:             gage_pp_process_file.c
* NUMBER OF MODULES:         1
* GENERAL INFORMATION:
*   MODULE 1:           gage_pp_hour_slot
* DESCRIPTION:          Determines the correct hour slot to place a value in.
*   MODULE 2:           gage_pp_process_file
* DESCRIPTION:          This subroutine processes data from the file found
*                       in the data directory, and creates a record to be 
*                       inserted to the HourlyPP or HourlyPC database table.
*
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        July 20, 2004
* ORGANIZATION:         HSEB / OHD
* MACHINE:              HP9000 / Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        8/20/2004    Bryon Lawrence    Completed Coding/doc 
*          2        7/20/2004    Bryon Lawrence    Began Coding  
*          2        8/20/2004    Bryon Lawrence    Completed coding/doc
*    General        12/29/2005   Bryon Lawrence    Refactored/coded to 
*                                                  support 6 hour and
*                                                  24 gage amounts.
*    General        1/5/2006     Bryon Lawrence    Completed coding to 
*                                                  support 6 and 24 hour
*                                                  amounts
*********************************************************************************/
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>

#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "gage_pp_log.h" 
#include "gage_pp_main.h"
#include "get_precip_settings.h"
#include "HourlyPP.h"
#include "precip_total.h"
#include "gage_pp_process_file.h"
#include "time_convert.h"
#include "time_defs.h"
#include "gage_pp_write_rec.h"
#include "ToolDefs.h"

/*******************************************************************************
* MODULE NUMBER:    1
* MODULE NAME:      gage_pp_hour_slot
* PURPOSE:          This routine determines if a precipitation report can
*                   be considered top of the hour.  If it can be, then
*                   it determines the hour slot the value should be stored in. 
*                   It also determines the minute offset code that
*                   represents the number of minutes the report's obstime
*                   either precedes or follows the top of the hour.
*                   This routine will adjust the obsdate based on any 
*                   necessary adjustments to the hour when mapping to the
*                   1 to 24 hour hour slots.   This needs to be done when
*                   the hour of the report's obstime is 0.
*                  
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                   DESCRIPTION/UNITS
*   Input  char [ ]    ansi_obstime_year_sec  The time string representation
*                                             of the report's obstime 
*                                             (YYYY-MM-DD HH:MM:SS).
*   Input  char [ ]    pPE                    The physical element (PC or PP). 
*   Output char [ ]    ansi_date_year_day     The date (YYYY-MM-DD) of the
*                                             record in the HourlyPC or 
*                                             HourlyPP table to write the
*                                             value in. 
*   Output short *     pHourSlot              The hour slot the value should
*                                             be written in.
*   Output char *      pOffsetCode            The code representing the
*                                             report's obstime offset from the
*                                             top of the hour (minute 0).
* RETURNS:
*    None
*
* APIs UTILIZED:
*   NAME                  HEADER FILE            DESCRIPTION
*   get_precip_window     get_precip_settings.h  Gets the time window in which
*                                                a PC or PP report can be
*                                                considered top of the hour.
*   get_offset_code       gage_pp_write_rec.h    Returns a code reprenting
*                                                the offset of a report's
*                                                obstime from the top of the
*                                                hour.
*   timet_to_yearsec_ansi time_convert.h         Converts a time in Unix ticks
*                                                to a string of format
*                                                YYYY-MM-DD HH:MM:SS.
*   yearsec_ansi_to_timet time_convert.h         Converts a time of format
*                                                YYYY-MM-DD HH:MM:SS to
*                                                Unix ticks.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE     NAME              DESCRIPTION
*   char [ ]      hourstr           The hour portion of the obstime string.
*   char [ ]      minutestr         The minute portion of the obstime string.
*   char [ ]      converted_obstime Creates a local copy of the
*                                   ansi_obstime_year_sec variable for
*                                   modification.
*   int           first_call        Flag indicating if this is the first time
*                                   this function has been called.
*   int           hour              Integer representation of the hourstr.
*   int           minute            Integer representation of the minutestr.
*   int           pc_window         The number of minutes either side of the
*                                   top of the hour in which a PC report is
*                                   considered to be top of hour.
*   int           pp_afterwindow    The number of minutes after the top of the
*                                   hour within which a PP report is 
*                                   considered top of the hour.
*   int           pp_beforewindow   The number of minutes before the top of the
*                                   hour within which a PP report is considered
*                                   top of the hour.
*   time_t        obstimet          The Unix ticks representation of the
*                                   report's obstime.
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   None.
********************************************************************************
*/
static void gage_pp_1hour_slot ( const GagePPoptions * pOptions, 
                                 const char ansi_obstime_year_sec [ ] , 
                                 const char pPE [ ] ,
                                 char ansi_date_year_day [ ] , 
                                 short * pHourSlot , 
                                 char * pOffsetCode )
{
   char hourstr [ HOURSIZ + 1 ] ;
   char minutestr [ MINSIZ + 1 ] ;
   char converted_obstime [ ANSI_TIME_LEN + 1 ] ;
   int hour ;
   int minute ;
   time_t obstimet ;

   memset ( hourstr , '\0' , HOURSIZ + 1 ) ;
   memset ( minutestr , '\0' , MINSIZ + 1 ) ;
   memset ( converted_obstime , '\0' , ANSI_TIME_LEN + 1 ) ;
   
   strncpy ( converted_obstime , ansi_obstime_year_sec , ANSI_TIME_LEN ) ;

   /* Get the hours. */
   strncpy ( hourstr , & converted_obstime [ 11 ] , HOURSIZ ) ; 
   hour = atoi ( hourstr ) ;               

   /* Get the minutes.*/
   strncpy ( minutestr , & converted_obstime [ 14 ] , MINSIZ ) ;
   minute = atoi ( minutestr ) ;
   
  
   /* The PE should be PP with a duration of Q. Not going to test for that
    * here. */
   if ( ( ( pPE [ 1 ] == 'P' ) && 
      ( minute >= MINUTES_PER_HOUR - pOptions->intlppp ) ) ||
      ( ( pPE [ 1 ] == 'C' ) &&
      ( minute >= MINUTES_PER_HOUR - pOptions->intpc ) ) )
   {
      /* If so, the hour needs to be incremented. */
      ++ hour ;
   }
  else if ( hour == 0 )
   {
      /* If the hour is 0, then decrement the date by one day and set 
         the hour to 24. */
      yearsec_ansi_to_timet ( converted_obstime , & obstimet  ) ;
      obstimet -= SECONDS_PER_DAY ;
      timet_to_yearsec_ansi ( obstimet , converted_obstime ) ;
      hour = 24 ;
   }
   
   strncpy ( ansi_date_year_day , converted_obstime , OBSYTD ) ;

   /* Using the minutes, get the offset code. */
   * pOffsetCode = get_offset_code ( minute ) ;
   * pHourSlot = ( short ) hour ;

}

/*******************************************************************************
* MODULE NUMBER:  2
* MODULE NAME:    gage_pp_6hour_slot
* PURPOSE:        Determines into which of the four 6 hour slots of the 
*                 HourlyPP table to place a PPQ precipitation report.
* 
*                 The search window for the the 6 hour reports is given
*                 by the intppq token.  This token specifies the window
*                 in hours.  The offset codes are the same as those
*                 used for the PCI and PPH data.  The difference is that
*                 the offset codes for the 6 hour data represent 
*                 5 minute blocks of time instead of 1 minute blocks of
*                 time.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE                               DESCRIPTION
*   int                                     Returns wether or not
*                                           a slot could be found for
*                                           the 6 hour report.
*
* APIs UTILIZED:
*   NAME                       HEADER FILE            DESCRIPTION
*   get_6hour_precip_window    get_precip_settings.h  Gets the value of the
*                                                     token which defines
*                                                     the 6 hour search
*                                                     window.
* 
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME                  DESCRIPTION
*   char       hourstr [ ]           The hour of the report
*   char       minutestr [ ]         The minute of the report
*   char       converted_obstime [ ] 
*   float      ppq_window            The value of the intppq token
*   int        bottom_6_hour_period  The lower 6 hour period of the 
*                                    two 6 hour periods that the
*                                    report lies between.
*   int        diff1 
*   int        diff2   
*   int        hour                  The numeric representation of the
*                                    hourstr variable.
*   int        minute                The numberic representation of the
*                                    minutestr variable.
*   int        num_periods           The number of 6 hour periods completely
*                                    contained by the obstime of the report
*   int        remainder             The number of seconds the report exceeds
*                                    the closest smallest 6 hour period.
*   int        top_6_hour_period    The upper 6 hour period of the two
*                                   6 hour periods that the report lies
*                                   between.
*   time_t     num_seconds_since_00z The number of seconds elapsed since
*                                    00z.
*   time_t     obstimet			    Used for time conversions when it is
*                                   necessary to decrement the date by
*                                   1 day.
* 
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*    GPP_OK									This worked.
*    GPP_RECORD_SKIPPED					    A slot cannot be found
*                                           for this record.  This is
*                                           probably because this report
*                                           falls outside the intppq time
*                                           window.
*   
********************************************************************************
*/
static int gage_pp_6hour_slot ( const GagePPoptions * pOptions ,
                                const char ansi_obstime_year_sec [ ] , 
                                char ansi_date_year_day [ ] , 
                                short * p6HourSlot , 
                                char * p6HourOffsetCode )
{
   char hourstr [ HOURSIZ + 1 ] ;
   char minutestr [ MINSIZ + 1 ] ;
   char converted_obstime [ ANSI_TIME_LEN + 1 ] ;
   float ppq_window;
   int bottom_6_hour_period;
   int diff1;
   int diff2;   
   int hour ;
   int minute ;
   int num_periods;
   int remainder;
   int top_6_hour_period;
   time_t num_seconds_since_00z;
   time_t obstimet ;

   ppq_window = pOptions->intppq;
   ppq_window *= SECONDS_PER_HOUR;

   /* Initialize the strings to contain the hour and minute portions
    * of the report time. */
   memset ( hourstr , '\0' , HOURSIZ + 1 ) ;
   memset ( minutestr , '\0' , MINSIZ + 1 ) ;
   memset ( converted_obstime , '\0' , ANSI_TIME_LEN + 1 ) ;
   strncpy ( converted_obstime , ansi_obstime_year_sec , ANSI_TIME_LEN ) ;

   /* Get the hour. It starts at character position 11.*/
   strncpy ( hourstr , & converted_obstime [ 11 ] , HOURSIZ ) ; 
   hour = atoi ( hourstr ) ;

   /* Get the number of minutes. It starts at character position 14. */
   strncpy ( minutestr , & converted_obstime [ 14 ] , MINSIZ ) ;
   minute = atoi ( minutestr ) ;
   
   /* We have 4 six hour slots.  The time window is determined by one token.
    * This token is in hours.  The offset codes are the same as those used
    * for the PCI and PPH data.  However, the codes represent 5 minute
    * time increments as opposed to 1 minute time increments. */

   /* Convert the hour and minute to seconds. */
   num_seconds_since_00z = ( hour * SECONDS_PER_HOUR ) +
                           ( minute * SECONDS_PER_MINUTE );

   /* Divide this value by the number of seconds in a 6 hour period. */
   bottom_6_hour_period = num_seconds_since_00z / SECONDS_IN_6HOUR_PERIOD;
   top_6_hour_period = bottom_6_hour_period + 1;
   
    /* Find the synoptic time that this 6 hour report falls closest to. */
   diff1 = num_seconds_since_00z - ( bottom_6_hour_period * 
                                     SECONDS_IN_6HOUR_PERIOD) ;
   diff2 = ( top_6_hour_period * SECONDS_IN_6HOUR_PERIOD)  - 
           num_seconds_since_00z ;
      
   if ( diff1 < diff2 )
   {
   	  /* The report is closest to the bottom 6 hour period. */
   	  /* Check if the the report falls within the acceptable time window. */
   	  if ( diff1 <= ppq_window )
   	  {
   	     /* The report falls within the allotted time window.  Determine
   	      * the offset code. */
   	      num_periods = diff1 / NUM_SECONDS_PER_6HOUR_OFFSET;
   	      remainder = diff1 % NUM_SECONDS_PER_6HOUR_OFFSET;
   	      
   	      if ( remainder != 0 ) num_periods ++;

   	      if ( bottom_6_hour_period == 0 )
   	      {
   	         /* Subtract one day from the date. */
   	         yearsec_ansi_to_timet ( converted_obstime , & obstimet  ) ;
             obstimet -= SECONDS_PER_DAY ;
             timet_to_yearsec_ansi ( obstimet , converted_obstime ) ;
             
   	         /* Set the hour slot to be the fourth period. */
   	         * p6HourSlot = 4;
   	      }
   	      else
   	      {
   	      	 * p6HourSlot = bottom_6_hour_period;
   	      }
   	  }
   	  else
   	  {
   	  	  /* This report cannot be used.  It is outside the window
   	  	   * of acceptable time. Skip this report.*/
   	  	   return GPP_SKIP_RECORD;
   	  }
   	  	   
   }
   else
   {
      /* The report is closest to the top 6 hour period. */
      /* Check if the report falls within the acceptable time window. */
      if ( diff2 <= ppq_window )
      {
         /* It does.  Determine the offet code. */
         num_periods = diff2 / NUM_SECONDS_PER_6HOUR_OFFSET;
         remainder = diff2 % NUM_SECONDS_PER_6HOUR_OFFSET;
         
         if ( remainder != 0 ) num_periods ++;
         
         /* Since using the same routine to determine offset codes
          * as the PPH and PCI, need to convert the number of 6 hour 
          * offset periods to "minutes" before the top of hour. */
         num_periods = MINUTES_PER_HOUR - num_periods;
         * p6HourSlot = top_6_hour_period;         
      } 
      else
      {
        /* This report cannot be used.  It is outside the window
         * of acceptable time. */
         return GPP_SKIP_RECORD;
      }
   }
   
   strncpy ( ansi_date_year_day , converted_obstime , OBSYTD ) ;

   /* Using the number of NUM_MINUTES_PER_6HOUR_OFFSET periods, 
    * get the offset code. */
   * p6HourOffsetCode = get_offset_code ( num_periods );   
   
   return GPP_OK;
}                                

/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   gage_pp_write_info
* PURPOSE:       Formats the information contained within the 
*                write_info for writing to the gage pp log file.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   input  char *      msgstr               The message written to the log
*                                           file.
*   input  WriteInfo * pWriteInfo           Contains information on the 
*                                           number of records updated,
*                                           inserted and deleted.
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   Void 
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*   N/A
* 
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   char       temp                         Temporary array used to build
*                                           the message string.
*   char *     pComma                       Used to remove the last comma
*                                           in the message string.
*   int        num_ignored                  The number of reports ignored.
*   int        num_inserts                  The number of reports inserted.
*   int        num_updates                  The number of reports updated.
*
* DATA FILES AND/OR DATABASE:
* None
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
static void gage_pp_write_info ( char * msgstr , const WriteInfo * pWriteInfo )
{
   char temp [ 100 ] ;
   char * pComma = NULL ;
   int num_ignored = pWriteInfo->num_hr_ignored ;
   int num_inserts = pWriteInfo->num_hr_inserts ;
   int num_updates = pWriteInfo->num_hr_updates ;

   if ( num_inserts != 0 )
   {
      sprintf ( temp , "Inserted %d, " , num_inserts ) ;
      strcat ( msgstr , temp ) ;
   }

   if ( num_updates != 0 )
   {
      sprintf ( temp , "Replaced %d, " , num_updates ) ;
      strcat ( msgstr , temp ) ;
   }

   if ( num_ignored != 0 )
   {
      sprintf ( temp , "Ignored %d" , num_ignored ) ;
      strcat ( msgstr , temp ) ;
   }

   pComma = strrchr ( msgstr , ',' ) ;

   if ( pComma != NULL )
   {
      * pComma = ' ' ;
   }
 
   return ;
}

static int is_near_12z ( const char *  yearsec_ansi,
                         time_t * pTimet12z )
{
	static int first = 1;
	static int ppp_ppd_window;
	struct tm * pStructTm = NULL;
	time_t lower_bound;
	time_t TimetObs;
	time_t upper_bound;
	

	/* Convert the ansi time to timet GMT. */
	yearsec_ansi_to_timet ( (char * ) yearsec_ansi, & TimetObs );
	
	/* Convert the timet to a struct tm and convert this to 12z. */
	pStructTm = gmtime ( & TimetObs );
	
	pStructTm->tm_hour = 12;
	pStructTm->tm_min = 0;
	pStructTm->tm_sec = 0;
	
	* pTimet12z = gm_mktime ( pStructTm );
	
	/* Retrieve the ppp/ppd window. */
	if ( first == 1 )
	{
       first = 0;
       ppp_ppd_window = get_local_7am_search_window ( );
       ppp_ppd_window *= SECONDS_PER_HOUR;
	}

    upper_bound = *pTimet12z + ppp_ppd_window;
    lower_bound = *pTimet12z - ppp_ppd_window;
    
    if ( ( TimetObs >= lower_bound ) &&
         ( TimetObs <= upper_bound ) )
    {
    	return 1;
    }	
    else
    {
    	return 0;
    }
}	                         

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:  parse_precip_record
* PURPOSE:      Tokenizes and parses a single record read from gage pp
*               input precipitation file.
*
* ARGUMENTS:
*   TYPE   DATA TYPE        NAME            DESCRIPTION/UNITS
*   Input   char *          data_rec        The pipe-delimited text record 
*                                           read in from the precpitation file.
*   Input   GagePPoptions * pOptions        Contains the application 
*                                           specific settings
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   int         status                      Indicates if any errors were
*                                           encountered while reading the
*                                           record.
* APIs UTILIZED:
*   NAME                    HEADER FILE      DESCRIPTION
*   gage_pp_1hour_slot      static           Finds the hour slot to place
*                                            a one hour value in.
*   gage_pp_6hour_slot      static           Finds the 6hour slot to place
*                                            a six hour value in.
*   shefdur_to_timet        time_util.h      Converts a shef duration to a 
*                                            number of seconds.
*   yearsec_ansi_to_timet   time_util.h      Converts an ASCII formatted time
*                                            string to a ticks representation.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                DESCRIPTION
*   char *     pToken              Points to an element parsed from the 
*                                  data record.
*   int        i                   A loop index variable.
*   int        status              Contains code indicating success or
*                                  failure to completely parse the record.
*   time_t     ppp_in_seconds      Used in computing the duration of a 5004
*                                  report.
* *
* DATA FILES AND/OR DATABASE:
* 
* ERROR HANDLING:
*    ERROR CODE                    DESCRIPTION
*    GPP_OK                        It worked!
*    GPP_BADRECORD                 The data record could not be tokenized.
*    GPP_SKIP_RECORD               The data record could be parsed, but
*                                  it cannot be used.  Most likely
*                                  a 5004 report does not have a 24 
*                                  duration or a PP report has a duration
*                                  of 0.
********************************************************************************
*/

static int parse_precip_record ( char * data_rec,
                                 const GagePPoptions * pOptions,
                                 struct PrecipRecord * pPrecipRecord,
                                 char * msgstr,
                                 char * obstime_ytd,
                                 const char * fname,
                                 char * offset_code,
                                 short * hour )
{
   enum PrecipRec { PRECIP_LID , PRECIP_PE , PRECIP_DUR , PRECIP_TS , 
                    PRECIP_EX , PRECIP_OBS , PRECIP_VAL , PRECIP_SHEF , 
	  	            PRECIP_QC , PRECIP_REV , PRECIP_ID , PRECIP_TIME , 
		            PRECIP_POST_TIME } ;
   
   /* For each precipitation report read from the input file,
    * initialize the duration to -1.  This variable keeps track
    * of the number of hours in the report's duration. */
   char * pToken = NULL;
   
   int i;
   int near_12z;
   int status = GPP_ERROR;  /* Status is not set to GPP_OK until all of the
                               fields are read in from the precipitation
                               record. */
                              
   time_t ppp_in_seconds;
   time_t timet_12z;
   
   /* Initialize the elements of the PrecipRecord structure. */
   pPrecipRecord->shef_duration = 0; /* Initialize the SHEF duration to instantaneous. */
   pPrecipRecord->shef_qual_code = '\0' ;
   pPrecipRecord->revision = 0 ;
   pPrecipRecord->pLid = NULL ;
   pPrecipRecord->pObstime = NULL;
   pPrecipRecord->pPe = NULL ;
   pPrecipRecord->pPostingTime = NULL;
   pPrecipRecord->pTs = NULL ;
   pPrecipRecord->data_duration = -1;
   pPrecipRecord->value = MISSING_PRECIP ;
       
   /* Tokenize the pipe-delimited precipitation record. */
   pToken = strtok ( data_rec , "|" ) ;

   for ( i = PRECIP_LID ; i <= PRECIP_POST_TIME ; ++ i )
   {
      if ( pToken == NULL )
      {
         sprintf ( msgstr , "Incomplete record in file %s. Skipping to\n"
                            "next record.\n" , fname ) ;
         writelog ( msgstr ) ;
         status = GPP_BADRECORD ;
         break ;
      }

      switch ( i )
      {
         case PRECIP_LID:

            pPrecipRecord->pLid = pToken ;
            break ;

         case PRECIP_PE:

            pPrecipRecord->pPe = pToken ;
            break ;
           
         case PRECIP_DUR:
             
            /* Variable placed here to contain the SHEF duration.
             * This, along with the obstime, will help to determine
             * if a report has a duration of 1 hour (PPH), 6 hours (PPQ)
             * of 24 hours (PPP,PPH). */
            pPrecipRecord->shef_duration = atoi ( pToken ) ;
            break ;

         case PRECIP_TS:

            pPrecipRecord->pTs = pToken ;
            break ;

         case PRECIP_EX:

            break ;

         case PRECIP_OBS:

            /* Logic added here to distinguish between a 1 hour PP report
             * (PPH), a 6-hour PP report (PPQ) and a 24 hour PP report
             * (either PPD or PPP).  This logic does not apply to PC
             * data which always have an instantaneous duration. 
             * December 27, 2005. */
            if ( pPrecipRecord->pPe [ 1 ] == 'C' )
            {
               /* Sometimes PC report non-zero durations which is incorrect.
                * Force the duration to 0. */
               pPrecipRecord->shef_duration = 0;
            }
               
            /* Keep a reference to the obstime. The obstime is needed
             * when storing precipitation reports into the DailyPP table. */
             pPrecipRecord->pObstime = pToken;
                
            /* We only care about durations from PP precipitation
             * types. PC types are always instantaneous. */
            switch ( pPrecipRecord->shef_duration )
            {
               case 0:  /* PPI - PC is always instantaneous. */
                
                  if ( pPrecipRecord->pPe [ 1 ] == 'P' )
                  {
                  	/* A PP report cannot be instantaneous. */
                  	sprintf ( msgstr, "%s %s %d %s %s cannot have an "
                                      "instantaneous duration", 
                                      pPrecipRecord->pLid,
                     	              pPrecipRecord->pPe, 
                     	              pPrecipRecord->shef_duration, 
                     	              pPrecipRecord->pTs, 
                     	              pPrecipRecord->pObstime);
                    status = GPP_SKIP_RECORD;
                     	                   	                  
                  }
                  else
                  {
                     
                     pPrecipRecord->data_duration = 0;
                     gage_pp_1hour_slot ( pOptions ,
                                          pPrecipRecord->pObstime , 
                                          pPrecipRecord->pPe , 
                                          obstime_ytd , 
                                          hour , 
                                          offset_code ) ;
                  }
                     
                  break;
                                          
               case 60:    /* 60 minute precipitation report. */
               case 1001:  /* PPH */
                  /* This is a one hour PP report. */
                  pPrecipRecord->data_duration = 1;
                     
                  /* Determine which 1 hour slot to place the 
                   * precipitation value in. */
                  gage_pp_1hour_slot ( pOptions ,
                                       pPrecipRecord->pObstime , 
                                       pPrecipRecord->pPe , 
                                       obstime_ytd , 
                                       hour , 
                                       offset_code ) ;
                  break;
                        
               case 1006:   /* PPQ */
                  /* This is a 6 hour PP report. */
                  pPrecipRecord->data_duration = 6;
                     
                  /* Determine which 6 hour slot to place the 
                   * data value in. */
                  status = gage_pp_6hour_slot ( pOptions ,
                                                pPrecipRecord->pObstime , 
                                                obstime_ytd ,
                                                hour , 
                                                offset_code );
                  break;
                     
               case 5004:   /* PPP */

                  /* This MAY be a 24 hour PP report. It depends on 
                     how the obstime of the report compares with 7 am
                     local time. */
                  yearsec_ansi_to_timet ( pPrecipRecord->pObstime, 
                                          & ppp_in_seconds );
                  shefdur_to_timet ( pPrecipRecord->shef_duration ,
                                     ppp_in_seconds ,
                                     & pPrecipRecord->data_duration );
                     
                  if ( pPrecipRecord->data_duration != SECONDS_PER_DAY )
                  {
                     sprintf ( msgstr, "%s %s %d %s %s does not have 24 "
                                       "hour duration", pPrecipRecord->pLid, 
                                       pPrecipRecord->pPe, 
                                       pPrecipRecord->shef_duration, 
                                       pPrecipRecord->pTs, 
                                       pPrecipRecord->pObstime );
                     status = GPP_SKIP_RECORD;
                  }
                     
                  pPrecipRecord->data_duration = 24;
                  
                  /* Check to make sure that this report falls within a 
                   * specified time window around 12z. If it doesn't, then
                   * do not use it. Otherwise, force the obstime of this
                   * station to be 12z. */
                   near_12z = is_near_12z ( pPrecipRecord->pObstime,
                                            & timet_12z );
                   
                   if ( near_12z == 1)
                   {
                   	  timet_to_ansi ( timet_12z, pPrecipRecord->pObstime );
                   }
                   else
                   {
                      status = GPP_SKIP_RECORD;
                   }

                  break;
                     
               case 2001:  /* PPD */
               
                  /* This is a 24 hour PP report. */
                  pPrecipRecord->data_duration = 24;
                  
                  /* Check to make sure that this report falls within a 
                   * specified time window around 12z.  If it doesn't, 
                   * then do not use it. Otherwise, force the obstime to 
                   * be 12z. */
                   near_12z = is_near_12z ( pPrecipRecord->pObstime,
                                            & timet_12z );
                   
                   if ( near_12z == 1 )
                   {
                      timet_to_ansi ( timet_12z, pPrecipRecord->pObstime );
                   }
                   else
                   {
                      status = GPP_SKIP_RECORD;
                   }
                   
                  break;
                     
               default :
              
                  /* The duration of this report was not recognized. */
                  sprintf ( msgstr, "%s %s %d %s %s has unsupported duration",
                                     pPrecipRecord->pLid, 
                                     pPrecipRecord->pPe, 
                                     pPrecipRecord->shef_duration, 
                                     pPrecipRecord->pTs, 
                                     pPrecipRecord->pObstime );
                                        
                  writelog ( msgstr );
                  status = GPP_SKIP_RECORD;
             }
               
             break;
                              
          case PRECIP_VAL:

             pPrecipRecord->value = atof ( pToken ) ;
              
             /* 24 hour values are not scaled by 100.  Only 1 and 6 hour
              * values are scaled by 100 because they are stored in
              * smallint hour slots in the HourlyPP table. */
             if ( ( pPrecipRecord->value != MISSING_PRECIP ) && 
                  ( pPrecipRecord->data_duration != 24 ) )
             {
                pPrecipRecord->value *= 100.0 ;

                if ( pPrecipRecord->value > ( float ) SHRT_MAX  )
                {
                   sprintf ( msgstr , "Precip value exceeds capacity "
                                      "of short integer. Setting value "
                                      "to %f5.0." ,
                                      MISSING_PRECIP ) ;
                   writelog ( msgstr ) ;
                   pPrecipRecord->value = MISSING_PRECIP ;
                }
                  
                /* Make certain that the value is properly rounded.  This
                   will prevent problems when downcasting to a short. */
                pPrecipRecord->value = round ( pPrecipRecord->value );
              }


              break ;
         
           case PRECIP_SHEF:

              pPrecipRecord->shef_qual_code = pToken [ 0 ];
              break ;
     
	       case PRECIP_QC :  

              /* Ignore this case. */
              break ;

           case PRECIP_REV:
           
              pPrecipRecord->revision = atoi ( pToken ) ;
              break ;

           case PRECIP_ID :

              break ;

           case PRECIP_TIME :
 
              break ;

           case PRECIP_POST_TIME :
            
              /* With the creation of the DailyPP table, we now need to 
               * store the posting time so that it can be used in the 
               * future. */
              pPrecipRecord->pPostingTime = pToken;
               
              /* The record contained the expected number of fields. */
              status = GPP_OK ;
              break ;

           default:
             
             break ;
         }

         if ( status == GPP_SKIP_RECORD )
         {
            break;
         }
         
         pToken = strtok ( NULL , "|" ) ;
      }
           
      /* End proposed parse precipitation data routine. */

   return status;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:   initialize_hourlypp
* PURPOSE:       Initializes an empty hourlyPP structure.  Also intializes
*                the 1 hour and 6 hour revisions codes.
*
* ARGUMENTS:
*   TYPE   DATA TYPE           NAME            DESCRIPTION/UNITS
*   I/O    HourlyPP*           pHourlyPP       The HourlyPP struct
*                                              to be initialized. 
*   I/O    short               rev_code        The array of 1 hour revision
*                                              codes.
*   I/O    short               rev_6hour_code  The array of 6 hour revision
*                                              codes.
*   I/O    char *              msgstr          Messages are constructed in
*                                              this array to be logged
*                                              to the GagePP log file.
*   I      struct PrecipRecord * pPrecipRecord Contains elements parsed from
*                                              this data record.  
*   I      char *              obstime_ytd     The observation date
*                                              of the report being processed.
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   void
*
* APIs UTILIZED:
*   NAME                        HEADER FILE     DESCRIPTION
*   SetNull                     DbmsUtils.h     Sets a value to NULL.
*   set_hour_slot_value         gage_pp_write_rec.h
*   set_6hour_slot_value        gage_pp_write_rec.h
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
static void initialize_hourlypp ( HourlyPP * pHourlyPP, 
								  short rev_code [ ],
								  short rev_6hour_code [ ],
								  char * msgstr,
                                  const struct PrecipRecord * pPrecipRecord, 
                                  char * obstime_ytd )
{
   date_t obstime_date_t ;
   int i;
   int status;
   short precip_val;
   
   memset ( pHourlyPP , '\0' , sizeof ( HourlyPP ) ) ;

   /* Create HourlyPP or HourlyPC table record  */ 
   strcpy ( pHourlyPP->lid , pPrecipRecord->pLid ) ;
   strcpy ( pHourlyPP->ts , pPrecipRecord->pTs ) ;
   ansi_date_to_date_t ( obstime_ytd , & obstime_date_t ) ;
   pHourlyPP->obsdate = obstime_date_t ; 
   
   status = SetNull ( SHORT , & precip_val ) ;

   if ( status != OK )
   {
       sprintf ( msgstr , "\tCould not set precip value to NULL.\n" ) ;
       writelog ( msgstr ) ;
   }

    /* Initialize the 1 hour slots. */
    for ( i = 0 ; i < HOURS_PER_DAY ; ++ i )
    {
       set_hour_slot_value ( pHourlyPP , i + 1 , precip_val ) ; 
	   rev_code [ i ] = 0 ;
    }
    
    
    /* Initialize the 6 hour slots. */
    for ( i = 0; i < NUM_6HOURLY_SLOTS; ++ i )
    {
    	set_6hour_slot_value ( pHourlyPP , i + 1, precip_val ); 
        rev_6hour_code [ i ] = 0;
    }
    

    memset ( pHourlyPP->hourly_qc , '-' , NUM_HOURLY_SLOTS ) ;
	pHourlyPP->hourly_qc [ NUM_HOURLY_SLOTS ] = '\0' ;
	memset ( pHourlyPP->minute_offset , '-' , NUM_HOURLY_SLOTS ) ;
	pHourlyPP->minute_offset [ NUM_HOURLY_SLOTS ] = '\0' ;
	memset ( pHourlyPP->sixhrqc, '-', NUM_6HOURLY_SLOTS );
	pHourlyPP->sixhrqc [ NUM_6HOURLY_SLOTS ] = '\0';
	memset ( pHourlyPP->sixhroffset, '-', NUM_6HOURLY_SLOTS );
	pHourlyPP->sixhroffset [ NUM_6HOURLY_SLOTS ] = '\0';
	
	return;
}

/*****************1**************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
static void initialize_dailypp ( DailyPP * pDailyPP,
                                 char * msgstr,
                                 const struct PrecipRecord * pPrecipRecord )
{
	double precip_val;
	int status;
	
   /* Initialize the elements of the DailyPP record to
    * be inserted into the DailyPP table. */
   memset ( pDailyPP, '\0', sizeof ( DailyPP ) ); 
      	 	 
   /* Initialize the lid ... */
   strncpy ( pDailyPP->lid, pPrecipRecord->pLid, LOC_ID_LEN );
      	 	 
   /* Initialize the typesource ... */
   strncpy ( pDailyPP->ts, pPrecipRecord->pTs, SHEF_TS_LEN );
      	 	 
   /* Initialize the obstime ... */
   yearsec_ansi_to_dt ( pPrecipRecord->pObstime, & pDailyPP->obstime );
      	 	 
   /* Initialize the value ... */
   status = SetNull ( DOUBLE , & precip_val ) ;

   if ( status != OK )
   {
       sprintf ( msgstr , "\tCould not set precip value to NULL.\n" ) ;
       writelog ( msgstr ) ;
   }
  
   pDailyPP->value = precip_val;
      	 	 
   /* Initialize the qc flag ... */
   memset ( pDailyPP->qc, '\0', CODE_LEN + 1 );
    	 	pDailyPP->qc [ 0 ] = '-';
      	 	 
   /* Initialize the posting time ... */
   yearsec_ansi_to_dt ( pPrecipRecord->pPostingTime, 
                        & pDailyPP->postingtime );
 
   return;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
static int use_1_hour_value ( const GagePPoptions * pOptions,
                              const struct PrecipRecord * pPrecipRecord, 
                              const HourlyPP * pHourlyPP, 
                              char offset_code, 
                              short hour )
{
	int status;
	int use_value = 1;
	short precip_val;
	
    precip_val = get_hour_slot_value ( pHourlyPP , ( short ) hour ) ;
    status = IsNull ( SHORT , & precip_val ) ; 

    if  ( status == NOTNULL ) 
    { 
    	use_value = use_precip_value ( ( short ) pPrecipRecord->value,
    	                               precip_val,
    	                               offset_code,
    	                               pHourlyPP->minute_offset [ hour - 1 ],
    	                               pPrecipRecord->shef_qual_code,
    	                               pHourlyPP->hourly_qc [ hour - 1],
    	                               pOptions->shef_duplicate,
    	                               pPrecipRecord->revision );
    }
    
    return use_value;
}
                                
/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
static int use_6_hour_value ( const GagePPoptions * pOptions,
                              const struct PrecipRecord * pPrecipRecord, 
                              const HourlyPP * pHourlyPP, 
                              char offset_code, 
                              short hour )
{
	int status;
	int use_value = 1;
	short precip_val;
	
    precip_val = get_6hour_slot_value ( pHourlyPP , ( short ) hour ) ;
    status = IsNull ( SHORT , & precip_val ) ; 

    if  ( status == NOTNULL ) 
    { 
  	   use_value = use_precip_value ( ( short ) pPrecipRecord->value,
    	                              precip_val,
    	                              offset_code,
    	                              pHourlyPP->sixhroffset [ hour - 1 ],
    	                              pPrecipRecord->shef_qual_code,
    	                              pHourlyPP->sixhrqc [ hour - 1],
    	                              pOptions->shef_duplicate,
    	                              pPrecipRecord->revision );    	
    }
    
    return use_value;

}

/*******************************************************************************
* MODULE NUMBER:   
* MODULE NAME:    use_24_hour_value
* PURPOSE:        Determines whether or not the 24 hour value should
*                 be stored in the DailyPP table.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
static int use_24_hour_value ( const GagePPoptions * pOptions,
                               const struct PrecipRecord * pPrecipRecord,
                               const DailyPP * pDailyPP )
{
	double precip_val;
	int status;
	int update_action;
	int use_value = 1;
	
	/* Check if there is an existing value in the DailyPP structure
	 * for this hour. */
    status = IsNull ( DOUBLE , & precip_val ) ; 

    if  ( status == NOTNULL ) 
    { 
    	/* There is already a value.  Get the update action which is
    	 * based on the shef_duplicate token. */
        update_action = determine_update_action ( 
                                  pOptions->shef_duplicate,
                                  pPrecipRecord->revision );

        if ( ( update_action == DONT_UPDATE_ACTION ) ||
             ( ( update_action == IF_DIFFERENT_UPDATE_ACTION ) &&
               ( precip_val == pPrecipRecord->value ) ) )
        {
           use_value = 0 ;
        }
    }
    
    return use_value;
}                 

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:     write_precip_data
* PURPOSE:         Writes precipitation data to the Hourly and Daily
*                  precipitation tables.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
static void write_precip_data ( const HourlyPP * pHourlyPP,
                                const DailyPP * pDailyPP,
                                WriteInfo * pWriteInfo,
                                const GagePPoptions * pOptions,
                                const short * rev_code,
                                const short * rev_6hour_code,
                                short rev_24hour_code,
                                const char * obstime,
                                int used_value_count,
                                int ignored_value_count,
                                char msgstr [ ],
                                const char * prev_pe,
                                int duration )
{
   static const char * db_action_strings [ ]  = { "Inserted record" ,
                                                  "Updated record" ,
 	  				                              "Ignored record" };
   int status;
   
   switch ( duration )
   {
      case 0:
      case 60:
      case 1001:
      case 1006:
               	  	 
         status = gage_pp_write_rec ( pHourlyPP , 
                                      pWriteInfo , 
                                      msgstr , prev_pe , 
                                      obstime , 
                                      pOptions , 
                                      rev_code ,
                                      rev_6hour_code, 
                                      used_value_count ) ;

          if ( status == GPP_OK )
          {
             sprintf ( msgstr , "   %s %s %d %s %s: %s; " ,
                                pHourlyPP->lid , prev_pe , 
                                duration,
                                pHourlyPP->ts ,
                                obstime ,
		                        db_action_strings [ pWriteInfo->db_action ]
                     ) ;
             pWriteInfo->num_hr_ignored += ignored_value_count ; 
             gage_pp_write_info ( msgstr , pWriteInfo ) ;
	         writelog ( msgstr ) ;
	      }
          else
          {
             /* An error occurred while trying to write the 
              * precip data to the HourlyPP or HourlyPC table. */
             writelog ( msgstr ) ;
          }
                        
          break;
               	  
       case 2001:
       case 5004:
               	  
          status = gage_pp_write_daily_rec ( pDailyPP,
                                             pOptions,
                                             obstime,
                                             pWriteInfo,
                                             msgstr,
                                             rev_24hour_code );
                                       
          if ( status == GPP_OK )
          {
             sprintf ( msgstr , "   %s %s %d %s %s: %s; " ,
                              pDailyPP->lid , "PP" , 
                              duration , 
                              pDailyPP->ts , obstime ,
                              db_action_strings [ pWriteInfo->db_action ]
                     ) ;
            
             /* Don't need to update the number of values 
              * ignored here. */           
             gage_pp_write_info ( msgstr , pWriteInfo ) ;
	         writelog ( msgstr ) ;
          }
          else
          {
             writelog ( msgstr ) ;
          }
             
          break;
               	        
       default:                                        
       
          break;
                        
    }
}
 
/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:    gage_pp_process_file
* PURPOSE:        Processes each record in a precip file in the
*                 gagepp data directory and stores it into the correct
*                 Hourly table or the DailyPP table.
*
* ARGUMENTS:
*   TYPE   DATA TYPE        NAME                 DESCRIPTION/UNITS
*   input  char *          filename              The name of the file
*                                                containing precipitation
*                                                records.
*   input  GagePPoptions * pOptions              Contains token values 
*                                                and settings.
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   int         status
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
int gage_pp_process_file ( const char * filename , 
                           const GagePPoptions * pOptions )
{
   const char * fname = NULL;
   char * pRecord = NULL;
   char curtime [ ANSI_YEARSEC_TIME_LEN + 1 ] ;
   char data_rec [ BUFSIZ ] ;
   char obstime_ytd [ OBSYTD + 1 ] ;
   char obstime_prev [ ANSI_YEARSEC_TIME_LEN + 1 ];
   char offset_code = '\0' ;
   char prev_pe [ SHEF_PE_LEN + 1 ]  ;
   char  msgstr [ MAX_LOG_LENGTH ] ;
   char prev_key [ KEYSIZ ] ;
   char cur_key [ KEYSIZ ] ;
   DailyPP dailyPP;
   
   FILE * infilePtr = NULL ;
   HourlyPP hourlyPP ;
   double elapsed_time ;
   int group_count ;

   int ignored_value_count ;
   int length ;
   int prev_dur = 0 ;
   int record_count;
   int status ;
   int use_value ;
   int used_value_count ;
   int total_value_count ;
   struct PrecipRecord precip_record;
   short precip_val ;
   short hour = 0 ;
   short rev_code [ HOURS_PER_DAY ] ;
   short rev_6hour_code [ NUM_6HOURLY_SLOTS ];
   short rev_24hour_code;
   struct timeval btime ;
   struct timeval etime ;
   struct tm * pTmStruct = NULL ;
   time_t start_timet ;
   time_t end_timet ;
   WriteInfo write_info ;
    
   memset ( prev_key , '\0' , KEYSIZ ) ;
   memset ( cur_key , '\0' , KEYSIZ ) ;

   /* For logging purposes, separate the filename from the file path. */
   fname = strrchr ( filename , '/' ) ;

   if ( fname != NULL ) 
   {
      ++ fname ; 
   }
   else
   {
      fname = filename ;
   }
   
   /* Start to build the GagePP log entry for this precipitation data
    * file. */   
     
   sprintf ( msgstr , "--------------------" ) ;
   writelog ( msgstr ) ;

   time ( & start_timet ) ;
   pTmStruct = gmtime ( & start_timet ) ;

   strftime ( curtime , ANSI_YEARSEC_TIME_LEN + 1 , "%Y-%m-%d %H:%M:%S" ,
              pTmStruct ) ;
   sprintf ( msgstr , "Processing file %s; at %s" , fname , curtime ) ;
   writelog ( msgstr ) ;

   /* This will enable the program to retrieve timing information to 
      the nearest millisecond. */
   gettimeofday  ( & btime , NULL ) ;

   /* This is the first attempt to open the file. Don't bother continuing
      with this file if there is any problem opening the file. */
   infilePtr = fopen ( filename , "r" ) ;

   if ( infilePtr == NULL )
   {
      sprintf ( msgstr , "Error opening product file: %s" , filename ) ;
      writelog ( msgstr ) ;
      return GPP_ERROR ;
   }

   memset ( obstime_ytd , '\0' , OBSYTD + 1  ) ;
   memset ( obstime_prev , '\0', ANSI_YEARSEC_TIME_LEN + 1 );
   memset ( prev_pe , '\0' , SHEF_PE_LEN + 1 ) ;
   used_value_count = 0 ;
   record_count = 0 ;
   ignored_value_count = 0 ;
   total_value_count = 0 ;
   group_count = 0 ; 
   rev_24hour_code = 0;

   /* Get a line from the file and parse it */
   pRecord = fgets ( data_rec , BUFSIZ , infilePtr ) ;

   while ( pRecord != NULL )
   {
      status = GPP_ERROR ;
  
      length = strlen ( data_rec ) ;

      if ( length == 0 )
      {
         sprintf ( msgstr , "Product file+ %s contains a zero-length data\n"
                            "record. Skipping to next record." , fname ) ;
         writelog ( msgstr ) ;
         continue ; 
      }
      
      
      /* Tokenize and parse the precipitation record. */
      status = parse_precip_record ( data_rec,
                                     pOptions,
                                     & precip_record,
                                     msgstr,
                                     obstime_ytd,
                                     fname,
                                     & offset_code,
                                     & hour );
           
      /* Check if the processing of the record was complete. */
      if ( status == GPP_OK )
      {
     	 /* Define current key which will contain such key values: 
            lid, pe, ts, day */
         if ( precip_record.data_duration != 24 )
         {
            sprintf ( cur_key , "%s%s%02d%s%s" , precip_record.pLid , 
                                precip_record.pPe ,
                                ( int ) precip_record.data_duration, 
                                precip_record.pTs , 
                                obstime_ytd ) ;
         }
         else
         {
            sprintf ( cur_key , "%s%s%02d%s%s" , precip_record.pLid , 
                                precip_record.pPe ,
                                ( int ) precip_record.data_duration, 
                                precip_record.pTs , 
                                precip_record.pObstime ) ;
         	
         }

         if ( * prev_key != '\0' )
         {
         	/* The current and previous keys do not match.  It is time to
         	 * write the previously processed report to the database. */ 
            if ( strcmp ( cur_key , prev_key ) != 0 )
            {
               if ( total_value_count > 0 )
               {
                  write_precip_data ( & hourlyPP,
                                      & dailyPP,
                                      & write_info,
                                      pOptions,
                                      rev_code,
                                      rev_6hour_code,
                                      rev_24hour_code,
                                      obstime_prev,
                                      used_value_count,
                                      ignored_value_count,
                                      msgstr,
                                      prev_pe,
                                      prev_dur );
               }

               /* Increment variables indicating the number of
                * LID,PE,DUR,TS,OBSTIME combinations */
	           ++ group_count ;
               used_value_count = 0 ;
               ignored_value_count = 0 ;
               total_value_count = 0;
            } 

          }
   
          /* Always initialize HourlyPP and DailyPP structures before
           * writing data to them for the first time. */
          if ( total_value_count == 0 )
          {
          	
          	/* Initialization of DailyPP and HourlyPP structures. */
            /* Initialize the structure to contain hourly precipitation
             * amounts. */
            switch ( precip_record.data_duration )
            {
            	case 0:
            	case 1:
            	case 6:
            	
            	   initialize_hourlypp ( & hourlyPP,
            	                         rev_code,
            	                         rev_6hour_code,
            	                         msgstr,
            	                         & precip_record,
            	                         obstime_ytd );

	               strcpy ( obstime_prev , obstime_ytd ) ;
	               break;
	               
                case 24:
                
                   initialize_dailypp ( & dailyPP,
                                        msgstr,
                                        & precip_record );
                                          
                   strcpy ( obstime_prev , precip_record.pObstime ) ;
                   break;
                   
                default:
                   break;
            }
                    
          }

          /* Initialize the use_value flag to indicate that the current
           * precipitation report is to be used.  Then perform tests 
           * to determine if the value should NOT be used. */
          use_value = 1 ;
          
          /* Determine if the value should be used. Must take into 
           * consideration the duration of the report because this 
           * determines where the value needs to be stored. */

          /* MUST check to determine if there is a value already in this 
          hour slot. */
          switch ( precip_record.data_duration )
          {
             case 0:
             case 1:
        
                use_value = use_1_hour_value ( pOptions,
                                                 & precip_record, 
                                                 & hourlyPP, 
                                                 offset_code, 
                                                 hour );
                break;
             
             case 6:
             
                use_value = use_6_hour_value ( pOptions,
                                               & precip_record,
                                               & hourlyPP,
                                               offset_code,
                                               hour );
                break;
                
             case 24:
             
                use_value = use_24_hour_value ( pOptions,
                                                & precip_record,
                                                & dailyPP );
                                                
                break;
                
             default:
                break;
                      
          }  
         
          if ( use_value == 1 )
          {
             /* Increment the count of used values. */
             used_value_count ++;
             
             switch ( precip_record.data_duration )
             {
                case 0:
                case 1:
                  
                   precip_val = ( short ) precip_record.value; 
                   set_hour_slot_value ( & hourlyPP , hour , precip_val ) ;
                   hourlyPP.hourly_qc [ hour - 1 ] = precip_record.shef_qual_code ;
	               hourlyPP.minute_offset [ hour - 1  ] = offset_code ;
                   rev_code [ hour - 1 ] = precip_record.revision ;
                   break;
                   
                case 6:
                
                   precip_val = ( short ) precip_record.value;
                   set_6hour_slot_value (  & hourlyPP, hour , precip_val );
                   hourlyPP.sixhrqc [  hour - 1 ] = 
                                    precip_record.shef_qual_code;                                           
                   hourlyPP.sixhroffset [ hour - 1 ] = offset_code;
                   rev_6hour_code [ hour -1 ] = precip_record.revision ;
                   break;
                   
                case 24:
                   
                   dailyPP.value = precip_record.value;
                   dailyPP.qc[0] = precip_record.shef_qual_code;
                   rev_24hour_code = precip_record.revision;
                   break;
                   
                default:
                   break;
             }
          }
          else
          {
             /* Increment the count of ignored values. */
             ignored_value_count ++;
          }
            
          /* Increment the count of total values.  This should always be 
           * equal to the sum of the ignored value count and the
           * used value count. */
           total_value_count ++;
       
          /* Define prev key which will contain such key values:
           * lid, pe, dur, ts, day */
          strcpy ( prev_key , cur_key ) ;
           
          /* Record the current PE as the previous PE. */
          strcpy ( prev_pe , precip_record.pPe ) ;
            
          /* Record the current duration as the previous duration. */
          prev_dur = precip_record.shef_duration ;
      }
      else if ( status == GPP_BADRECORD )
      {
      	 /* A problem was encountered while tokenizing and parsing the
      	  * record.  Don't use it. */
         sprintf ( msgstr , "   Incomplete record ignored." ) ;
         writelog ( msgstr ) ; 
      }
      else if ( status == GPP_SKIP_RECORD )
      {
      	 /* The record was skipped because a PP record had an instantaneous
      	  * duration or a 5004 report did not have a 24 hour duration. */
         sprintf ( msgstr, "   %s %s %d %s %s:  Record skipped",
                   precip_record.pLid, precip_record.pPe, 
                   precip_record.shef_duration,
                   precip_record.pTs, precip_record.pObstime );
         writelog ( msgstr );

      }
      
      ++ record_count ;
      pRecord = fgets ( data_rec , BUFSIZ , infilePtr ) ;
   }

   ++ group_count ;

   /* Check if there is still a record to be written out to the 
    * HourlyPP or HourlyPC IHFS tables.  This will be the case if
    * the total_value_count > 0.  This does not apply to the 
    * 24 hour data which are written to the DailyPP table immediately
    * upon receipt, so there is no need to buffer them like the 1 hour
    * reports for efficiency. */
   if ( total_value_count > 0 )
   {
  
      write_precip_data ( & hourlyPP,
                           & dailyPP,
                           & write_info,
                           pOptions,
                           rev_code,
                           rev_6hour_code,
                           rev_24hour_code,
                           obstime_prev,
                           used_value_count,
                           ignored_value_count,
                           msgstr,
                           prev_pe,
                           prev_dur );
   	  
   }

   /* Close the input file */
   status = fclose ( infilePtr ) ;

   if ( status != 0 )
   {
      sprintf ( msgstr , "Error closing file: %s" , filename ) ;
      writelog ( msgstr ) ;
   }

   gettimeofday ( & etime , NULL ) ;
   time ( & end_timet ) ;
   pTmStruct = gmtime ( & end_timet ) ;
   strftime ( curtime , ANSI_YEARSEC_TIME_LEN + 1 , "%Y-%m-%d %H:%M:%S" ,
              pTmStruct ) ;
   elapsed_time = ( double ) ( etime.tv_sec - btime.tv_sec ) + 
                  ( double ) ( etime.tv_usec - btime.tv_usec ) / 1000000. ;
   sprintf ( msgstr , "Total vals, keys, secs: %d, %d, %5.3f" , record_count , 
             group_count , elapsed_time ) ;  
   writelog ( msgstr ) ;
   sprintf ( msgstr , "Done processing file at %s" , curtime ) ;
   writelog ( msgstr ) ;

   return status ;
}
