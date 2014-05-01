/*******************************************************************************
* FILENAME:            get_total_PCPP.c 
* NUMBER OF MODULES:   4
* GENERAL INFORMATION:
*   MODULE 1:   get_total_hourly_PC
* DESCRIPTION:  Computes precipitation totals based on PC data retrieved from
*               the HourlyPC IHFS database table.  The data in this table 
*               have been preprocessed by the Gauge Precipitation Processor 
*               (GPP).
*   MODULE 2:   get_total_hourly_PP
* DESCRIPTION:  Computes precipitation totals based on PP data retrieved from
*               the HourlyPP and PPother IHFS database tables.  The data in
*               HourlyPP table have been preprocessed by the GPP.
*   MODULE 3:   get_total_raw_PC
* DESCRIPTION:  Computes precipitation totals based on PC data retrieved from
*               the RawPC table.  The data in the RawPC table have not been
*               preprocessed by the GPP.
*   MODULE 4:   get_total_raw_PP
* DESCRIPTION:  Computes precipitation totals based on PP data retrieved from
*               the RawPP table.  The data in the RawPP table have not been
*               preprocessed by the GPP.
*
* ORIGINAL AUTHOR:  Bryon Lawrence
* CREATION DATE:    July 9, 2004
* ORGANIZATION:     OHD/HSEB
* MACHINE:          i686, Redhat Linux Version 2.4.9 - 31 Enterprise.
* MODIFICATION HISTORY:
*   MODULE #        DATE           PROGRAMMER        DESCRIPTION/REASON
*   1               July 9,  2004  Bryon Lawrence    Original Coding
*   2               July 9,  2004  Bryon Lawrence    Original Coding
*   3               July 12, 2004  Bryon Lawrence    Original Coding
*   4               July 12, 2004  Bryon Lawrence    Original Coding
*   1               Aug  24, 2004  Bryon Lawrence    Modified processing of 
*                                                    HourlyPC.
*   2               Aug  24, 2004  Bryon Lawrence    Modified processing of
*                                                    HourlyPP.
*   1               Aug  23, 2005  Bryon Lawrence    Added logic to catch
*                                                    case where start time
*                                                    is after end time.
********************************************************************************
*/

#include <limits.h>
#include <string.h>
#include <time.h>

#include "DbmsUtils.h"
#include "gage_pp_write_rec.h"
#include "get_total_PCPP.h"
#include "get_total_precip.h"
#include "HourlyPC.h"
#include "HourlyPP.h"
#include "List.h"
#include "precip_total.h"
#include "RawPC.h"
#include "RawPP.h"
#include "time_convert.h"
#include "time_defs.h"

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    get_total_hourly_PC
* PURPOSE:        Returns precipitation totals derived from the HourlyPC table.
*                 Each record in the HourlyPC table contains 24 slots.  These
*                 slots contain top of the hour PC values for each hour of
*                 the day for a unique lid, ts, and obsdate.
*
*                 In order to derive an actual precipitation amount from the
*                 accumulator values, two PC values must be retrieved.
*                 One of these must be as close as possible to the 
*                 beginning of the retrieval interval.  The other must be
*                 as close as possible to the end of the retrieval interval.
*                 The PC value from the beginning of the accumulation 
*                 interval is then subtracted from the PC value at the
*                 end of the accumulation interval to get the amount
*                 of precipitation that fell.
*
*                 Algorithm: 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME             DESCRIPTION/UNITS
*
*   Input  HourlyPC *  pHourlyPC        A linked list of PC data ordered
*                                       by ascending lid, ts, and obsdate. 
*   Input  time_t      ending_time      The ending time of the accumulation 
*                                       interval in UNIX ticks. 
*   Input  int         num_hours        The number of hours in the accumulation
*                                       interval.
*   Input  int         num_pc_records   The number of records that have the
*                                       same lid and ts.
*   Output int *       seconds_covered  The number of seconds in the 
*                                       accumulation interval covered by
*                                       actual PC data.
*   Output char *      pc_qc            The quality code of the ending
*                                       PC value.
*
* RETURNS:
*   DATA TYPE    DESCRIPTION
*   float        The total precipitation.     
*
* APIs UTILIZED:
*   NAME                HEADER FILE     DESCRIPTION
*   gm_mktime           time_convert.h  Works like mktime but asumes the
*                                       input t is in GMT (UTC) instead of
*                                       local time. 
*   IsNull              DbmsUtils.h     Checks a database field for NULL value.
*   ListNext            List.h          Lists the next node in the linked list.
*   ListPrev            List.h          Lists the previous node in the linked
*                                       list
*   date_t_to_timet     time_convert.h  Converts a Postgres year to day time 
*                                       to a ticks value.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME          DESCRIPTION
*  char shef_qc_char         Contains the shef quality code character of the
*                            PC report whose value is used as the ending
*                            value.
*  float total               The total obtained by subtracting a beginning
*                            PC value from an ending PC value.
*  HourlyPC * pEndPC         Used to locate the ending PC value to use
*                            in computing the precipitation total.
*  HourlyPC * pEndPrev       A temporary pointer to the previous PC report 
*                            processed.
*  HourlyPC * pStartPC       Used to locate the beginning PC value to use
*                            in computing the precipitation total.
*  int diff                  Used in the computation of the starting hour. 
*                            The starting hour is obtained by subtracting the
*                            accumulation interval in hours from the endtime
*                            time.   If the start_hour 
*  int end_hour              The ending hour of the accumulation interval.
*  int found                 A flag indicating when an acceptable starting
*                            PC time has been found.
*  int i                     A loop index.
*  int hour_index            The index into the hourly PC data array which
*                            corresponds to the hourly PC slots in a record
*                            from HourlyPC.
*  int num_days              The number of days the user requested 
*                            precipitation accumulation interval spans.
*  int record_count          Tracks the number of records in the pc data
*                            block processed.
*  int start_hour            The starting hour of the accumulation index.
*  int status                Contains return codes from the time conversion
*                            and NULL testing utilities.
*  int value_found           Indicates if an acceptable starting PC value 
*                            could be found.
*  short int end_value       The ending PC value.
*  short int start_value     The starting PC value.
*  struct tm * pEndTm        A tm struct containing the ending time broken
*                            down into its components. 
*  time_t end_date           The ending date in ticks of the PC
*                            accumulation interval.
*  time_t pc_end_timet       The time in UNIX ticks of the ending PC report
*                            used in the precip total. 
*  time_t pc_start_timet     The time in UNIX ticks of the starting PC report
*                            used in the precip total. 
*  time_t pc_timet           The datetime of the current PC report being
*                            procesed is converted to ticks and stored in
*                            this variable.  This is done to test if the
*                            report is within the start and end bounds of the
*                            user's accumulation interval.
*  time_t start_date         The starting time in ticks of the PC accumulation
*                            interval
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
float get_total_hourly_PC ( const HourlyPC * pHourlyPC ,
                            time_t ending_time ,
                            int num_hours ,
                            int num_pc_records ,
                            int * seconds_covered ,
			    char * pc_qc )
{
   char shef_qc_char ;
   float total = MISSING_PRECIP ;
   const HourlyPC * pEndPC = NULL ;
   const HourlyPC * pEndPrev = NULL ;
   const HourlyPC * pStartPC = NULL ;
   int diff ;
   int end_hour ;
   int found ;
   int i ;
   int hour_index ;
   int num_days ;
   int record_count ;
   int start_hour ;
   int status ;
   int value_found ;
   short int end_value ;
   short int start_value ;
   struct tm * pEndTm = NULL ;
   time_t end_date ;
   time_t pc_end_timet ;
   time_t pc_start_timet ;
   time_t pc_timet = 0 ;
   time_t start_date ;

   * pc_qc = 'Z' ;

   /* Initialize the number of seconds covered to 0. */
   * seconds_covered = 0 ;
   
   if ( pHourlyPC == NULL )
   {
      /* No PC data was passed into this routine. */
      return MISSING_PRECIP ;
   }

   /* Determine the end date (YYYY-MM-DD) and hour of the accumulation 
      interval. */
   pEndTm = gmtime ( & ending_time ) ;
   end_hour = pEndTm->tm_hour ;

   /* Check to make sure the end date is top of the hour. If not, then
      adjust the date according to the number of minutes. */
   if ( pEndTm->tm_min > 30 )
   {
      ++ end_hour ;
   }
 
   /* Determine if the end hour is 0.  If it is, then set it to 24 and
      decrease the date by one day. */
   if ( end_hour == 0 )
   {
      end_hour = 24 ;
      ending_time -= SECONDS_PER_DAY ;
      pEndTm = gmtime ( & ending_time ) ;
   }

   pEndTm->tm_sec = 0 ;
   pEndTm->tm_min = 0 ;
   pEndTm->tm_hour = 0 ;

   end_date = gm_mktime ( pEndTm ) ;

   /* Determine the start date (YYYY-MM-DD) of the accumulation interval. */
   start_date = end_date ;
   diff = end_hour - num_hours ;
   start_hour = diff % 24 ;

   if ( start_hour <= 0 )
   {
      start_hour += 24 ;
   }
   
   if ( diff <= 0 )
   {
      num_days = diff / 24 + 1 ; 
      start_date -= SECONDS_PER_DAY * num_days ;
   }

   /* Load the record from the HourlyPC table corresponding to the beginning
      date of the interval.  Since we are dealing with the Hourly tables,
      a value for the exact start date and hour must be available. */
   pStartPC = pHourlyPC ;
   found = 0 ;
   record_count = 0 ;

   for ( i = 0 ; i < num_pc_records ; ++ i )
   {
      record_count ++ ;
      pc_timet = date_t_to_timet ( pStartPC->obsdate ); 

      if ( pc_timet >= start_date )
      {
         found = 1 ;
         break ;
      }

      pStartPC = ( HourlyPC * ) ListNext ( & pStartPC->node ) ;
   }

   if ( found == 0 )
   {
      /* No start date could be retrieved. */
      return MISSING_PRECIP ;
   }

   if ( pc_timet > start_date )
   {
      /* An exact match for the start date could not be found.
         Set the starting hour to 1. */
      start_hour = 1 ;
   }

   pEndPC = pStartPC ;

   if ( pc_timet > end_date )
   {
      /* The starting time is after the ending time.
         Cannot compute a PC-based precipitation total. */
      return MISSING_PRECIP ;
   }

   if ( pc_timet != end_date )
   {
      pEndPrev = pEndPC ;
        
      for ( i = record_count ; i < num_pc_records ; ++ i )
      {
         pEndPC = ( HourlyPC * ) ListNext ( & pEndPC->node ) ;
         ++ record_count ;
         pc_timet = date_t_to_timet ( pEndPC->obsdate ) ;

	 if ( pc_timet == end_date ) 
         {
            break ;
         }
         else if ( pc_timet > end_date )
         {
            pEndPC = pEndPrev ;
            break ;
         }

         pEndPrev = pEndPC ;
         pEndPC = ( HourlyPC * ) ListNext ( & pEndPC->node ) ;

      }
   }

   /* In the case where an exact match for the ending date could not be
      found set the end hour to 24. */ 
   if ( ( pEndPC == NULL ) ||  ( pc_timet < end_date ) )
   {
      end_hour = 24 ;
      pEndPC = pEndPrev ;
   }

   /* The start and end dates have been retrieved. Compute the precipitation
      total. */

   /* Find the hour slot in the start date which is closest to the starting
      time. */
   value_found = 0 ;
   start_value = MISSING_PRECIP ;

   while ( ( pStartPC != NULL ) &&
           ( ( pStartPC != pEndPC ) ||
             ( start_hour < end_hour ) ) ) 
   {
      start_value = get_hour_slot_value ( ( HourlyPP * ) pStartPC , start_hour ) ; 

      hour_index = start_hour - 1 ;
      shef_qc_char = pStartPC->hourly_qc [ hour_index ] ;

      /* Test if the start_value is NULL. */
      status = IsNull ( SHORT , & start_value ) ; 

      if ( ( status == NOTNULL ) && ( start_value != MISSING_PRECIP ) &&
           ( shef_qc_char != 'B' ) && ( shef_qc_char != 'R' ) )
      {
         /* The starting PC value has been found. */
         value_found = 1 ;
         break ;
      }

      ++ start_hour ;

      if ( start_hour > 24 )
      {
         start_hour = 1 ;
         pStartPC = ( HourlyPC * ) ListNext ( & pStartPC->node ) ;
      }
   }

   if ( value_found == 0 )
   {
      /* The starting value could not be found. */
      return MISSING_PRECIP ;
   }

   /* Find the hour slot in the end date which is closest to the starting
      time. */
   value_found = 0 ;
   end_value = MISSING_PRECIP ;

   while ( ( pEndPC > pStartPC ) ||
           ( ( pEndPC == pStartPC ) && ( end_hour > start_hour ) ) )
   {
      hour_index = end_hour - 1 ; 
      end_value = get_hour_slot_value ( ( HourlyPP * ) pEndPC , end_hour ) ; 

      shef_qc_char = pEndPC->hourly_qc [ hour_index ] ;

      /* Test if the end value is NULL. */
      status = IsNull ( SHORT , & end_value ) ;

      if ( ( status == NOTNULL ) && ( end_value != MISSING_PRECIP ) &&
           ( shef_qc_char != 'B' ) && ( shef_qc_char != 'R' ) ) 
      {
         value_found = 1 ;
        
         /* Set the qc value to the end hour qc. */
         * pc_qc = shef_qc_char ;
         break ;
      }
   
      -- end_hour ;
  
      if ( end_hour < 0 )
      {
         end_hour = 24 ;
         pEndPC = ( HourlyPC * ) ListPrev ( & pEndPC->node ) ;
      }
   }

   if ( value_found == 0 )
   {
      /* The ending PC value could not be found. */
      return total ;
   }

   /* Both the starting and the ending PC values have been found. */
   /* Compute the number of seconds in the interval. */
   pc_start_timet = date_t_to_timet ( pStartPC->obsdate );

   pc_start_timet += start_hour * SECONDS_PER_HOUR ;
   pc_end_timet = date_t_to_timet ( pEndPC->obsdate ) ;
   pc_end_timet += end_hour * SECONDS_PER_HOUR ;

   * seconds_covered = pc_end_timet - pc_start_timet ;

   /* The starting and ending values have been found. */
   total = end_value - start_value ;
   total /= 100 ;

   return total ;
}

static int is_near_7am_local ( time_t ending_time, time_t obstime )
{
   int local_7am_window;
   struct tm * pLocalTime = NULL;
   time_t diff ;
   time_t local_timet ;

   pLocalTime = localtime ( & obstime );
   pLocalTime->tm_hour = 07;
   pLocalTime->tm_min = 0;
   pLocalTime->tm_sec = 0;

   local_timet = mktime ( pLocalTime );
   diff = labs ( ending_time - local_timet );

   /* Get the local 7am search hour window. */
   local_7am_window = get_local_7am_search_window ( );

   if ( diff <= ( local_7am_window * SECONDS_PER_HOUR ) )
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
static enum PPdurationMatch find_duration_match ( const RawPP * pRawPP ,
                                                  time_t starting_time ,
						  time_t ending_time ,
                                                  int num_records ,
                                                  float * pTotal ,
                                                  time_t * pMatchTime ,
                                                  short int ending_time_match )
{
   int duration;
   int is_endtime_near_7am;
   int is_obstime_near_7am;
   int pp_count;
   int status;

   long diff;
   long prev_diff = LONG_MAX; 
   
   time_t datadur;
   time_t obstime;

   enum PPdurationMatch PPmatch = PPdurationMatchNotFound;

   duration = ending_time - starting_time;

   for ( pp_count = 0 ; pp_count < num_records ; ++ pp_count )
   {
      /* Only consider the PP value if it is valid. Note that the
         ( void * ) casts away the const. */   
      status = IsNull ( FLOAT , ( void * ) & pRawPP->value ) ;   

      if ( ( pRawPP->value != MISSING_PRECIP ) &&
           ( status == NOTNULL ) &&
           ( pRawPP->shef_qual_code [ 0 ] != 'B' ) &&
           ( pRawPP->shef_qual_code [ 0 ] != 'R' ) )
      {
         /* Convert the SHEF duration code to an interval in seconds.
            Subtract this interval from the report's obstime to get the
            start time. */
         status = yearsec_dt_to_timet ( pRawPP->obstime , & obstime ) ;

         if ( status >= 0  )
         {
            status = shefdur_to_timet ( pRawPP->dur, obstime , & datadur ) ;

            if ( status >= 0 )
	    { 
               /* A duration match has been found. Determine how the
                  user wants to handle a duration match. */
               if ( datadur == duration )
               {
                  switch ( ending_time_match )
                  {
                     case LATEST_ENDINGTIME_MATCH :

                        /* All done.  The duration match with the most recent
                           ending time has been found. */
                        * pTotal = pRawPP->value ;
                        * pMatchTime = obstime ;
                        return PPdurationMatchFound ; 
                        break ;

                     case EXACT_ENDINGTIME_MATCH :

                        /* When accumulating for 24 hours and processing a
                           5004 report with a duration of 24 hours and the
                           ending time of the accumulation interval is 
                           within +/- 3 hours of 7 AM local time, find 
                           the 5004 report whose obstime is closest to 
                           the ending time of the accumulation interval. 
			   Treat it like an exact match. */ 
                        if ( ( pRawPP->dur == 5004 || pRawPP->dur == 2001 ) &&
                             ( duration == SECONDS_PER_DAY ) )
                        {
                           is_endtime_near_7am = 
                                        is_near_7am_local ( ending_time, 
                                                            obstime ) ;
                            
                           if ( is_endtime_near_7am == 1 )
                           {
                              /* If this is a 2001 report, make sure that its
                                 obstime is within the specified number of hours
                                 of 7am local. For a 5004 report we already know
                                 that the obstime is close enough to 7am local
                                 because it has a 24 hour duration. */
                                 is_obstime_near_7am = 1;

                              if ( pRawPP->dur == 2001 )
                              {
                                 is_obstime_near_7am = 
                                        is_near_7am_local ( obstime,
                                               obstime );
                              }

                              if ( is_obstime_near_7am == 1 )
                              {
                                 PPmatch = PPdurationMatchFound ; 
                                 diff = labs ( obstime - ending_time ) ; 

                                 if ( prev_diff >= diff )
                                 {
                                    * pTotal = pRawPP->value ;
                                    * pMatchTime = obstime ;
                                    prev_diff = diff ; 
                                 }
                              }
                           }
                        }
                        else
                        {
                           /* Does the obstime of the report match the 
                              ending time of the accumulation interval? */
                           if ( obstime == ending_time )  
                           {
                              * pTotal = pRawPP->value ;
                              * pMatchTime = obstime ;
                              return PPdurationMatchFound ; 
                           }
                        }

                           break ;

                     case CLOSEST_ENDINGTIME_MATCH :

                        /* Find a report with a matching duration whose
                           obstime is closest to the ending time of the
                           accumulation interval. */
                        PPmatch = PPdurationMatchFound ; 
                        diff = labs ( obstime - ending_time ) ; 

                        if ( prev_diff >= diff )
                        {
                           * pTotal = pRawPP->value ;
                           * pMatchTime = obstime ;
                           prev_diff = diff ; 
                        }
                        else
                        {
                           return PPdurationMatchFound ; 
                        }
                       
                        break ; 

                     default :
   
                        /* Find a report with a matching duration whose
                           obstime is closest to the ending time of the
                           accumulation interval and within the user
                           specified hour window. */
                        diff = labs ( obstime - ending_time ) ; 

                        if ( ( prev_diff >= diff ) &&
                             ( diff <= 
		             ( ending_time_match * SECONDS_PER_HOUR ) ) )
                        {
                           PPmatch = PPdurationMatchFound ;
                           *pTotal = pRawPP->value ;
                           * pMatchTime = obstime ;
                           prev_diff = diff ; 
                        }
                        else if ( PPmatch == PPdurationMatchFound )
                        {
                           return PPdurationMatchFound ;
                        }
                       
                        break ; 
                  } 
               }
            }
         }
      }

      pRawPP = ( RawPP * ) ListNext ( & pRawPP->node ) ;
   }

   return PPmatch ;
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   compute_raw_pp_total   
* PURPOSE:       This routine calculates a raw PP precipitation total.
*
* ARGUMENTS:
*   TYPE         DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input/Output RawPP **    pRawPP               
*   Input        time_t      starting_time
*   Input        time_t      ending_time
*   Input        int         num_records
*   Output       float *     pTotal
*   Output       int *       counter
*   Input        short int   exact_match_only
*   Input        short int   exact_match_window
*   Input        int         num_minutes
*   Input        short *     pMinutes
*		                         
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
static enum PPdurationMatch 
                 compute_raw_PP_total ( const RawPP * pRawPP ,
                                        time_t starting_time ,
                                        time_t ending_time ,
		                        int num_records ,
                                        float * pTotal ,
                                        time_t * pMatchTime ,
			                short int no_accum_flag ,
                                        short int ending_time_match ,
					int num_minutes ,
				        short * pMinutes )
		                         
{
   float total ; 
   int already_used ;
   int end_is_within ;
   int endmin ;
   int j ;
   int pp_count ;
   int start_is_within ;
   int startmin ;
   int status ;
   enum PPdurationMatch duration_match ;
   time_t datadur ;
   time_t obstime ;
   time_t report_start ;

   if ( ( pRawPP == NULL ) || ( pMinutes == NULL ) || ( pTotal == NULL ) )
   {
      return PPdurationMatchNotFound ; 
   } 

   * pMatchTime = 0 ;
   total = * pTotal ;

   /* Check for an exact match first. */
   duration_match = find_duration_match ( pRawPP ,
                                          starting_time ,
					  ending_time ,
                                          num_records ,
                                          & total ,
                                          pMatchTime ,
                                          ending_time_match ) ;
 
   if ( duration_match == PPdurationMatchFound )
   {
      * pTotal = total ;
      return PPdurationMatchFound ;
   }

   total = * pTotal ;
   
   /* A duration match was not found.  Check if the user will allow
      an accumulation. */
   if ( no_accum_flag == 0 )  
   {
      for ( pp_count = 0 ; pp_count < num_records ; ++ pp_count )
      {
         /* Only consider the PP value if it is valid. Note that the
            ( void * ) casts away the const. */   
         status = IsNull ( FLOAT , ( void * ) & pRawPP->value ) ;   

         if ( ( pRawPP->value != MISSING_PRECIP ) &&
              ( status == NOTNULL ) &&
              ( pRawPP->shef_qual_code [ 0 ] != 'B' ) &&
              ( pRawPP->shef_qual_code [ 0 ] != 'R' ) )
         {
            /* Convert the SHEF duration code to an interval in seconds.
               Subtract this interval from the report's obstime to get the
               start time. */
            status = yearsec_dt_to_timet ( pRawPP->obstime , & obstime ) ;

	    /* Write out the string representation of the obstime. */
            if ( status >= 0  )
            {
               status = shefdur_to_timet ( pRawPP->dur, obstime , & datadur ) ;

               if ( status >= 0 )
	       { 
                  /* Test whether the report's obstime is within the
                     interval.  If it is, then set the start flag 
                     to true. */
	          if ( ( obstime > starting_time ) &&
	               ( obstime <= ending_time ) )
                  {
                     end_is_within = 1 ;
                  }
                  else
                  {
                     end_is_within = 0 ;
                  }

	          report_start = obstime - datadur ;

                  /* Test whether the starting time of the report's
                     observation is within the interval.  If it is,
                     then set the end flag to true. */
                  if ( ( report_start >= starting_time ) &&
                       ( report_start < ending_time ) )
                  {
                     start_is_within = 1 ;
                  }
                  else
                  {
                     start_is_within = 0 ;
                  }

                  /* Calculate the indexes in the minutes array 
                     corresponding to the starting and ending times. */
	          startmin =  ( report_start - starting_time ) / 
                                SECONDS_PER_MINUTE ;
	          endmin = ( obstime - starting_time ) / 
                             SECONDS_PER_MINUTE ;

                  /* Check that the report's time period is completetly
                     within the time period being considered. */
                  if ( pRawPP->value >= 0.0 )
	          {
	             if ( ( start_is_within == 1 ) && 
                          ( end_is_within == 1 ) )
	             {
	                /* Check to determine if the portion of the 
	                   accumulation interval covered by the 
                           report's duration has already been covered
                           by a previous report. */
	                already_used = 0 ;

                        for ( j = startmin ; j < endmin ; ++ j )
		        {
		           if ( pMinutes [ j ] == 1 )
		           {
		              already_used = 1 ;
		              break ;
		           }
		        }

                        /* The data being considered has a duration that 
                           fits in the duration being considered, and 
                           the duration 'slot' is available, so apply 
                           the data value to the total and set the array 
                           to indicate the slot is now not available. */
                        if ( already_used == 0 )
	                {
		           if ( total == MISSING_PRECIP )
		           {
		              total = pRawPP->value ;
                           }
		           else
		           {
		              total += pRawPP->value ;
                           }

		           for ( j = startmin ; j < endmin ; ++ j ) 
                           {
                              pMinutes [ j ] = 1 ;
                           }
                        }
                     }
                     else if (pRawPP->value == 0.00)
                     {
                        if ( ( start_is_within == 1 ) || 
		             ( end_is_within == 1 ) )
                        {
                           /* initialize the value as necessary and set
                              what time period the zero value covers. */
                           if (total == MISSING_PRECIP)
                           {
                              total = pRawPP->value;
                           }

                           if (startmin < 0) startmin = 0 ;
                           if (endmin > num_minutes) endmin = num_minutes ;

                           for (j = startmin; j < endmin; j++)
                              pMinutes[j] = 1;
                        }
                     }
                  } 
               }
            }
         }

         pRawPP = ( RawPP * ) ListNext ( & pRawPP->node ) ;
      }

   }

   * pTotal = total ;
   
   return PPdurationMatchNotFound ;
}

/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   get_total_hourly_PP
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

float get_total_hourly_PP ( const HourlyPP * pHourlyPP ,
                            time_t ending_time ,
                            int num_hours ,
                            int num_pp_records ,
                            int * seconds_covered ,
                            char * pp_qc ,
	                    int * reported_missing )
{
   /* Concept of an exact match does not apply here. BAL August 23, 2004.*/ 
   /* Ending time must be top of the hour.  If it is not, then it is 
      forced to be top of the hour. */
   /* Requires a list of PP data ordered by lid ascending, TS ascending,
   duration descending, and obstime descending (only one TS). */

   char shef_qc_char ;
   float total = MISSING_PRECIP ;
   int end_hour_index ;
   int hour ;
   int hour_index ;
   int i ;
   int j ;
   int num_minutes ;
   int pp_count = 0 ;
   int start_hour_index ;
   int status ;
   int value ;

   short int * pMinutes = NULL ;
   struct tm * pStructTm = NULL ;
   time_t date_timet ;
   time_t hour_timet ;
   time_t pp_date_timet ;
   time_t starting_time ;

   /* Initialize the reported missing flag to 0. */
   * reported_missing = 0 ;
   
   /* Initialize the seconds covered to 0. */
   * seconds_covered = 0 ;

   /* Check to make sure that there is data to process. */
   if ( pHourlyPP == NULL )
   {
      return MISSING_PRECIP ;
   }

   /* Check to make sure that the ending time is top of hour. */ 
   /* Create an array which has a number of elements equal to the number
      of minutes between the starting and the ending times. */
   num_minutes = num_hours * MINUTES_PER_HOUR ;

   pMinutes = ( short * ) malloc ( sizeof ( short ) * ( num_minutes + 1 ) ) ;

   if ( pMinutes == NULL )
   {
      /* Return a missing precipitation value. */
      return MISSING_PRECIP ;
   }

   /* Initialize all the elements in the minutes array to 0. */
   memset ( pMinutes , 0 , sizeof ( short ) * ( num_minutes + 1 ) ) ;

   /* Check the ending time.  Is it exactly on the top of the
      hour? */
   pStructTm = gmtime ( & ending_time ) ;

   if ( pStructTm->tm_min > 30 )
   {
      ending_time += SECONDS_PER_MINUTE * ( 60 - pStructTm->tm_min )  ;
      pStructTm = gmtime ( & ending_time ) ;
   }

   /* Compute the starting time. */
   starting_time = ending_time - num_minutes * SECONDS_PER_MINUTE ;

   /* Determine the starting hour and ending hour indices in the
      the minutes array. */
   start_hour_index = MINUTES_PER_HOUR ;              
   end_hour_index = num_minutes + 1 ;

   for ( i = start_hour_index ; ( i <= end_hour_index ) && 
         ( pp_count < num_pp_records )  ; 
         i += MINUTES_PER_HOUR )
   {
      /* Compute the time in ticks of the hour being 
         examined. */
      hour_timet = starting_time + ( i * SECONDS_PER_MINUTE ) ; 

      /* Convert this time into a Year to Day time_t
         value. */
      pStructTm = gmtime ( & hour_timet ) ;
      hour = pStructTm->tm_hour ;
      
      if ( hour == 0 )
      {
         hour = 24 ; 
         hour_timet -= SECONDS_PER_HOUR ;
         pStructTm = gmtime ( & hour_timet ) ;
      }

      pStructTm->tm_hour = 0 ;
      pStructTm->tm_min = 0 ;
      pStructTm->tm_sec = 0 ;

      date_timet = gm_mktime ( pStructTm ) ;

      while ( pp_count < num_pp_records )
      {
         /* Check the corresponding slot in the HourlyPP table
            to for a PP value. */
         pp_date_timet = date_t_to_timet ( pHourlyPP->obsdate );

         if ( pp_date_timet == date_timet )
         {
            hour_index = hour - 1 ;
            /* Retrieve the value from the appropriate hour slot. */
            value = get_hour_slot_value ( pHourlyPP , hour ) ;
	    shef_qc_char = pHourlyPP->hourly_qc [ hour_index ] ;
          
            /* Check the value to determine if it is NULL. */
            status = IsNull ( SHORT , & value ) ;  

            if ( ( status == NOTNULL ) &&
	         ( shef_qc_char != 'B' ) && 
	         ( shef_qc_char != 'R' ) )
            {
                * pp_qc = shef_qc_char ;

                if ( ( total == MISSING_PRECIP ) && 
                     ( ( value == MISSING_PRECIP ) ||
                     ( ( value < 0 ) && ( shef_qc_char == 'M' ) ) ) )
                {
                   * reported_missing = 1 ;
                }
                else if ( value >= 0 )
                {

                   * reported_missing = 0 ;
                   
                   if ( total == MISSING_PRECIP )
                   {
                      total = value ;
                   }
                   else
                   {
                      total += value ;
                   }
                   
                   /* Missing values DO NOT count towards the minutes
                      array. */
                   if ( value >= 0 )
                   {
                      for ( j = 0 ; j < MINUTES_PER_HOUR ; ++ j ) 
                      {
                         pMinutes [ i - j ] = 1 ;
                      }
                   }
                }
            }

            break ;
                          
         }
         else if ( pp_date_timet < date_timet )
         {
            /* The date of the HourlyPP record is earlier than
               the date of the hour being processed. */
            pHourlyPP = ( HourlyPP * )
                        ListNext ( & pHourlyPP->node ) ;
            ++ pp_count ;
         }
         else
         {
            /* The date of the HourlyPP record is later than the 
               date of the hour being processed. Get the next hour
               to process. */ 
            break ;
         }
      }
   }

   /* Return the value and seconds covered to the user. */
   j = 0 ;

   for ( i = 1 ; i <=num_minutes ; ++ i ) 
   {
      if ( pMinutes [ i ] == 1 ) j ++ ;
   }

   * seconds_covered = j * SECONDS_PER_MINUTE ; 

   if ( total != MISSING_PRECIP )
   {
      total /= 100 ;
   }

   if ( pMinutes != NULL )
   {
      free ( pMinutes ) ;
      pMinutes = NULL ;
   }

   return total ;
}

/*******************************************************************************
* MODULE NUMBER:  5
* MODULE NAME:    get_total_by_subtracting_raw_PC
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

static float get_total_by_subtracting_raw_PC ( const RawPC * pRawPC ,
                                               time_t starting_time ,
                                               time_t ending_time ,
			                       int num_records ,
                                               int * seconds_covered )
{
   double       begin_value = MISSING_PRECIP ; 
   double       end_value = MISSING_PRECIP ;
   float	total = MISSING_PRECIP ;
   int		end_found ;
   int          i ;
   int		status ;
   long         absolute_time_diff ;
   long         begin_time_diff = ( long ) LONG_MAX ;  
   long         end_time_diff = ( long ) LONG_MAX ;
   time_t	begin_time = 0 ; 
   time_t       end_time = 0 ;
   time_t	obstime ;
      
   /* find the first record  (the first record is the PC on the point 
      which is closest to the start_time within
      the precipitation duration plus the hour window);
      make sure the type-source also matches.
      the records should be ordered by ts, then by time.
      they are ordered earliest to latest. Assign the value of
      the first record within the time window (not the missing value)
      to the begin_value */
   
   * seconds_covered = 0 ;
   end_found = 0 ;
         
   if ( pRawPC == NULL )
   {
      return MISSING_PRECIP ;
   }
   
   for ( i = 0 ; ( i < num_records ) && ( pRawPC != NULL ) ; ++ i ) 
   {
      status = yearsec_dt_to_timet ( pRawPC->obstime , & obstime ) ;
	 
      if ( obstime > starting_time )
      {
         /* Check to make sure the value is valid. */
         status = IsNull ( FLOAT , ( void * ) & pRawPC->value ) ;

         if ( ( pRawPC->value >= 0.0 ) &&
              ( status == NOTNULL ) &&
              ( pRawPC->shef_qual_code [ 0 ] != 'B' ) &&
              ( pRawPC->shef_qual_code [ 0 ] != 'R' ) )           
         {
            absolute_time_diff = labs ( obstime - ending_time ) ;

 	    if ( absolute_time_diff < end_time_diff )
	    {
	       end_time_diff = absolute_time_diff ;
	       end_value = pRawPC->value;
	       end_time = obstime ;
	       end_found = 1;
	    }
            else
            {
               /* The best ending value has been found. */
               break ;
            }
	 }

         pRawPC = ( RawPC * ) ListNext ( & pRawPC->node ) ;

       }
       else 
       {
          break;
       }
   }
	 
   /* if the ending PC report cannot be found, then return with
    * a missing value */
   if ( ! end_found )
   {
     return MISSING_PRECIP ;
   }
   
   /* starting from the first record found above to look for the last record
   which has value <= the end_value (not missing value) in the precipitation 
   duration plus the hour window */
    
   for ( ; ( i < num_records ) && ( pRawPC != NULL ) ; ++ i ) 
   {
      status =  yearsec_dt_to_timet ( pRawPC->obstime , & obstime ) ;  
        
      if ( obstime >= starting_time )
      {
         status = IsNull ( FLOAT , ( void * ) & pRawPC->value ) ;

         if ( ( pRawPC->value >= 0.0 ) &&
              ( pRawPC->value <= end_value ) &&
              ( status == NOTNULL ) &&
              ( pRawPC->shef_qual_code [ 0 ] != 'B' ) &&
              ( pRawPC->shef_qual_code [ 0 ] != 'R' ) )           
	 {
            absolute_time_diff = abs ( obstime - starting_time ) ;

	    if ( absolute_time_diff < begin_time_diff )
            {   
	       begin_time_diff = absolute_time_diff ;  
	       begin_value = pRawPC->value;
               begin_time = obstime ;
	    }
            else
            {
               /* The best starting value has been found. */
               break ;
            }
	 }
       }      
       else
       {
          break ;
       }
	 
       pRawPC = ( RawPC * ) ListNext ( & pRawPC->node ) ;	
    }
    
    if (begin_value != MISSING_PRECIP && end_value != MISSING_PRECIP)
    {
       total = end_value - begin_value;
       * seconds_covered = end_time - begin_time;
    }
   
   return total ;

}

/*******************************************************************************
* MODULE NUMBER:  7
* MODULE NAME:    get_total_by_summing_raw_PC
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

static float get_total_by_summing_raw_PC ( const RawPC * pRawPC ,
                                           time_t starting_time ,
                                           time_t ending_time ,
			                   int num_records ,
                                           int * seconds_covered )
{
   double       begin_value = MISSING_PRECIP ; 
   double       end_value = MISSING_PRECIP ;
   float	total = MISSING_PRECIP ;
   int		end_valid = 0;
   int          i ;
   int		status ;
   long         absolute_time_diff ;
   long         begin_time_diff = ( long ) LONG_MAX ;  
   long         end_time_diff = ( long ) LONG_MAX ;
   time_t	begin_time = 0 ; 
   time_t       end_time = 0 ;
   time_t	obstime ;
      
   /* find the first record  (the first record is the PC on the point 
      which is closest to the start_time within
      the precipitation duration plus the hour window);
      make sure the type-source also matches.
      the records should be ordered by ts, then by time.
      they are ordered earliest to latest. Assign the value of
      the first record within the time window (not the missing value)
      to the begin_value */
   
   * seconds_covered = 0 ;

   if ( pRawPC == NULL )
   {
      return MISSING_PRECIP ;
   }
   
   for ( i = 0 ; ( i < num_records ) && ( pRawPC != NULL ) ; ++ i ) 
   {
      status = yearsec_dt_to_timet ( pRawPC->obstime , & obstime ) ;
	 
      if ( obstime > starting_time )
      {
         /* Check to make sure the value is valid. */
         status = IsNull ( FLOAT , ( void * ) & pRawPC->value ) ;

         if ( ( pRawPC->value >= 0.0 ) &&
              ( status == NOTNULL ) &&
              ( pRawPC->shef_qual_code [ 0 ] != 'B' ) &&
              ( pRawPC->shef_qual_code [ 0 ] != 'R' ) )           
         {
            absolute_time_diff = labs ( obstime - ending_time ) ;

 	    if ( absolute_time_diff < end_time_diff )
	    {
	       end_time_diff = absolute_time_diff ;
	       end_value = pRawPC->value;
	       end_time = obstime ;
	       end_valid = 1;
	    }
            else
            {
               /* The best ending value has been found. */
               break ;
            }
	 }

         pRawPC = ( RawPC * ) ListNext ( & pRawPC->node ) ;

       }
       else 
       {
          break;
       }
   }
	 
   /* if valid ending PC report cannot be found, then return with
    * a missing value */
   if ( end_valid != 1 )
   {
      return MISSING_PRECIP ;
   }
   
   /* starting from the first record found above 
    * walk through each PC report.  If the PC report is valid, then
    * compute the precipitation total. */
   for ( ; ( i < num_records ) && ( pRawPC != NULL ) ; ++ i ) 
   {
      status =  yearsec_dt_to_timet ( pRawPC->obstime , & obstime ) ;  
        
      if ( obstime >= starting_time )
      {
         status = IsNull ( FLOAT , ( void * ) & pRawPC->value ) ;

         if ( ( pRawPC->value >= 0.0 ) &&
              ( pRawPC->value <= end_value ) &&
              ( status == NOTNULL ) &&
              ( pRawPC->shef_qual_code [ 0 ] != 'B' ) &&
              ( pRawPC->shef_qual_code [ 0 ] != 'R' ) )           
	 {
            absolute_time_diff = abs ( obstime - starting_time ) ;

	    if ( absolute_time_diff < begin_time_diff )
            {   
	       begin_time_diff = absolute_time_diff ;  
	       begin_value = pRawPC->value;
               begin_time = obstime ;

               if ( total != MISSING_PRECIP )
               {
                  total += ( end_value - begin_value ) ;
               }
               else
               {
                  total = end_value - begin_value ;
               }

               /* Figure out the coverage. */
               if ( * seconds_covered > 0 )
               {
                  * seconds_covered += ( end_time - begin_time ) ;
               }
               else
               {
                  * seconds_covered = end_time - begin_time ;
               }

	    }
            else
            {
               /* The best starting value has been found. */
               break ;
            }
	 }

         /* Set the end values to the current values. */
         end_value = pRawPC->value ;
         end_time = obstime ;

       }      
       else
       {
          break ;
       }
	 
       pRawPC = ( RawPC * ) ListNext ( & pRawPC->node ) ;	
    }
    
   return total ;
}

/*******************************************************************************
* MODULE NUMBER:  4
* MODULE NAME:    get_total_raw_PC
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

float get_total_raw_PC ( const RawPC * pRawPC ,
                         time_t starting_time ,
                         time_t ending_time ,
			 int num_records ,
                         int sum_pc_reports ,
                         int * seconds_covered )
{
   float total ;

   /* Determine which algorithm to use in deriving PC precip
    * totals. */
   if ( sum_pc_reports == 1 )
   {
      total = get_total_by_summing_raw_PC ( pRawPC ,
                                            starting_time ,
                                            ending_time ,
                                            num_records ,
                                            seconds_covered ) ; 
   }
   else
   {
      total = get_total_by_subtracting_raw_PC ( pRawPC ,
                                                starting_time ,
                                                ending_time ,
                                                num_records ,
                                                seconds_covered ) ; 
   }
   
   return total ;
}

/*******************************************************************************
* MODULE NUMBER: 6
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

float get_total_raw_PP ( const RawPP * pRawPP ,
                         time_t starting_time ,
                         time_t ending_time ,
			 int num_pp_records ,
			 short int no_accum_flag ,
                         short int ending_time_match ,
                         time_t * pMatchTime ,
                         int * seconds_covered ,
	                 int * summed_flag )
{
   /* Requires a list of PP data ordered by lid ascending, TS ascending,
   duration descending, and obstime descending (only one TS). */

   float total = MISSING_PRECIP ;
   int i ;
   int j ;
   int num_minutes ;
   short int * pMinutes = NULL ;
   enum PPdurationMatch duration_match ;

   /* Initialize the seconds covered to 0. */
   * seconds_covered = 0 ;
   * pMatchTime = 0 ;

   /* Check to make sure that there is data to process. */
   if ( pRawPP == NULL )
   {
      return MISSING_PRECIP ;
   }

   /* Create an array which has a number of elements equal to the number
      of minutes between the starting and the ending times. */
   num_minutes = ( ending_time - starting_time ) / SECONDS_PER_MINUTE ;

   pMinutes = ( short * ) malloc ( sizeof ( short ) * ( num_minutes + 1 ) ) ;

   if ( pMinutes == NULL )
   {
      /* Return a missing precipitation value. */
      return MISSING_PRECIP ;
   }

   /* Initialize all the elements in the minutes array to 0. */
   memset ( pMinutes , 0 , sizeof ( short ) * ( num_minutes + 1 ) ) ;

   duration_match = compute_raw_PP_total ( pRawPP ,
                                           starting_time ,
                                           ending_time ,
                                           num_pp_records ,
                                           & total ,
                                           pMatchTime ,
			                   no_accum_flag ,
                                           ending_time_match ,
				           num_minutes ,
				           pMinutes ) ;

   /* Return the value and seconds covered to the user. */
   if ( duration_match == PPdurationMatchNotFound )
   {
      j = 0 ;

      for ( i = 0 ; i < num_minutes ; ++ i ) if ( pMinutes [ i ] == 1 ) j ++ ;

      * seconds_covered = j * SECONDS_PER_MINUTE ; 
      
      if ( total >= 0. ) 
      {
         * summed_flag = 1 ;
      }
      else
      {
         * summed_flag = 0 ;
      }
   }
   else
   {
      * seconds_covered = ending_time - starting_time ;
      * summed_flag = 0 ;
   } 

   if ( pMinutes != NULL )
   {
      free ( pMinutes ) ;
      pMinutes = NULL ;
   }

   return total ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
