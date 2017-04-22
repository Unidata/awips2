/********************************************************************************
* FILENAME:             gage_pp_write_rec.c
* NUMBER OF MODULES:         2
* GENERAL INFORMATION:
*   MODULE 1:           gage_pp_write_rec 
* DESCRIPTION:          This subroutine checks PE to define to which table -
*                       HourlyPP or HourlyPC, to write a record. 
*
*   MODULE 2:           get_offset_code
* DESCRIPTION:          This subroutine creates a one character code which 
*                       represents the number of minutes before or after the
*                       top of the hour a report's obstime is.
*
*   MODULE 3:           compare_offset_codes
* DESCRIPTION:          This subroutine compares two offset character codes.
*                       to determine which represents the largest offset.   
*
*   MODULE 4:           get_hour_slot_value
* DESCRIPTION:          For a given hour, this function returns the precip
*                       amount from a the HourlyPC structure.
*
*   MODULE 5:           set_hour_slot_value
* DESCRIPTION:          Set a value in a given hour slot in a HourlyPP or 
*                       HourlyPC structure.
*
*
* ORIGINAL AUTHOR:      Moria Shebsovich
* CREATION DATE:        July 21, 2004
* ORGANIZATION:         HSEB / OHD
* MACHINE:              HP9000 / Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        7/21/2004    Moria Shebsovich  Original Coding
*          1        7/31/2004    Bryon Lawrence    Modified.
*          2        8/5/2004     Bryon Lawrence    Original Coding 
*          4        10/20/2004   Bryon Lawrence    Added the 
*                                                  get_hour_slot_value
*                                                  routine.
*********************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dbmserrs.h"
#include "DbmsUtils.h"
#include "gage_pp_write_rec.h"
#include "GeneralUtil.h"
#include "HourlyPP.h"
#include "HourlyPC.h"
#include "precip_total.h"
#include "time_defs.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   gage_pp_write_rec
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
int gage_pp_write_rec ( const HourlyPP * pHour_new , WriteInfo * pWriteInfo ,
                        char msgstr [ ] , const char * pe ,
                        const char * obsdate , 
                        const GagePPoptions * pOptions ,
                        const short * rev_code , 
                        const short * rev_6hour_code ,
                        int num_values )
{
    DbStatus * pDbStatus = NULL;
    HourlyPP * pHour_old = NULL ;
    char value_new_offset ;
    char value_old_offset ;
    char value_new_qc ;
    char value_old_qc ;
    char where [ 100 ] ;
    int i ;
    short int is_pc = 0 ;
    int return_status = GPP_OK ;
    int status ;
    int update_action;
    int use_value ;
    short value_new ;
    short value_old ;

    /* Initialize the write information structure. */
    pWriteInfo->db_action = WriteRecNone ;
    pWriteInfo->num_hr_inserts = 0 ;
    pWriteInfo->num_hr_updates = 0 ;
    pWriteInfo->num_hr_ignored = 0 ;

    /* Prepare the where clause. */
    sprintf ( where , "WHERE lid = '%s' AND ts = '%s' AND "
                      "obsdate = '%s'" , pHour_new->lid ,
		              pHour_new->ts , obsdate ) ;

    /* Determine if this is a PP or a PC report. */
    status = strcmp ( pe , "PC" ) ;

    if ( status == 0 )
    {
       is_pc = 1 ;
    }

    /* Retrieve the existing record for the given lid, ts, and obsdate.*/
    if ( is_pc == 1 )
    {
       pHour_old = ( HourlyPP * ) GetHourlyPC ( where ) ;
    }
    else
    {
       pHour_old = ( HourlyPP * ) GetHourlyPP ( where ) ;
    }

    /* If the record doesn't exist in the HourlyPC table, insert it.
       Do not bother inserting a record which has nothing but missing
       hourly values. */
   if ( pHour_old == NULL )
   {

      if ( is_pc == 1 )
      {
         status = PutHourlyPC ( ( HourlyPC * ) pHour_new ) ;
      }
      else
      {
         status = PutHourlyPP ( pHour_new ) ;
      }

      if ( status != ERR_OK )
      {
         /* Retrieve the SQLSTATE. SQLCODE is no longer a useful
            indicator of database errors with PostGres. */
         pDbStatus = GetHourlyPPDbStatus ( );

         if ( pDbStatus != NULL )
         {
         
            if ( is_pc == 1 )
            {
               sprintf ( msgstr , "Error on insert to HourlyPC, SQLSTATE: %s "
                                  "Key:%s%s%s" , pDbStatus->sql_state , 
                                  pHour_new->lid , pHour_new->ts , obsdate ) ;
            }
            else
            {
               sprintf ( msgstr , "Error on insert to HourlyPP, SQLSTATE: %s "
                                  "Key:%s%s%s" , pDbStatus->sql_state , 
                                  pHour_new->lid , pHour_new->ts , obsdate ) ;
            }
         }

         pWriteInfo->db_action = WriteRecNone ;
         pWriteInfo->num_hr_ignored = num_values ;
         return_status =  GPP_ERROR ;
      }   
      else
      {
         pWriteInfo->db_action = WriteRecInsert ;
         pWriteInfo->num_hr_inserts = num_values ;
      }

   }
   else
   {
      /* If the record exists in the HourlyPC table, then check
         each hour slot to find new values, duplcate values, and
         revisions. */
         
      /* New starting on December 29, 2005.  If processing for the
       * HourlyPP table, MUST also check the 4 six hour slots along
       * with their offsets and QC values. */
       
       /* First process the data in the hourly slots. */
      for ( i = 0 ; i < HOURS_PER_DAY ; ++ i )
      {
	     value_new = get_hour_slot_value ( pHour_new , i + 1 ) ; 
         value_new_offset = pHour_new -> minute_offset [ i ] ; 
         status = IsNull ( SHORT , & value_new ) ; 

         /* Only do this processing if there is actually a new value for
            this hour slot. */
         if ( status == NOTNULL )
         {
            use_value = 1 ;
            value_old = get_hour_slot_value ( pHour_old , i + 1 );
            status = IsNull ( SHORT , & value_old ) ;  

            if ( status == NOTNULL )
            {
                value_old_qc = pHour_old -> hourly_qc [ i ] ;
                value_new_qc = pHour_new -> hourly_qc [ i ] ; 
                value_old_offset = pHour_old -> minute_offset [ i ] ;
            	
            	use_value = use_precip_value ( value_new,
            	                               value_old,
            	                               value_new_qc,
            	                               value_old_qc,
            	                               value_new_offset,
            	                               value_old_offset,
            	                               pOptions->shef_duplicate,
            	                               rev_code [ i ] ) ;

               if ( use_value == 1)
               {
                  pWriteInfo->db_action = WriteRecUpdate ;
                  pWriteInfo->num_hr_updates ++ ; 
               }
            }
            else
            {
               /* There is not a preexisting value. The record
                * already exists in the Hourly table so it will 
                * be an update, but for this particular hour there
                * is no preexisting value, so the insert counter
                * is incremented. */
               pWriteInfo->db_action = WriteRecUpdate ;
               pWriteInfo->num_hr_inserts ++ ;
            }

            if ( use_value == 1 )
            {
               set_hour_slot_value ( pHour_old , i + 1 , value_new ) ; 
               pHour_old->hourly_qc [ i ] = pHour_new->hourly_qc [ i ] ; 
               pHour_old->minute_offset [ i ] = value_new_offset ;
            }
            else
            {
               pWriteInfo->num_hr_ignored ++ ;
            }
         }
      }

      /* Now process the data in the 6 hour slots. */
      for ( i = 0 ; i < NUM_6HOURLY_SLOTS ; ++ i )
      {
	     value_new = get_6hour_slot_value ( pHour_new , i + 1 ) ; 

         value_new_offset = pHour_new->sixhroffset [ i ] ; 

         status = IsNull ( SHORT , & value_new ) ; 

         /* Only do this processing if there is actually a new value for
            this hour slot. */
         if ( status == NOTNULL )
         {
            use_value = 1 ;
            value_old = get_6hour_slot_value ( pHour_old , i + 1 );
            status = IsNull ( SHORT , & value_old ) ;  

            if ( status == NOTNULL )
            { 
               /* There is a value in the new hour slot.  There is also
                  a value in the old hour slot.  If the value in the old
                  hour slot has been manually edited, DO NOT overwrite
                  it unless the new value is a manual edited. */
               value_old_qc = pHour_old->sixhrqc [ i ] ;
               value_new_qc = pHour_new->sixhrqc [ i ] ; 

               if ( ( value_old_qc == 'M' ) && ( value_new_qc != 'M' ) )
               {
                  use_value = 0 ;
               }
               else
               {
                  /* Check to see which is closest to the top of the hour. 
                     Check to see if they
                     are identical. */
                  value_old_offset = pHour_old->sixhroffset [ i ] ;

                  /* Compare the number of minutes each of these reports is 
                  offset from the top of the hour. */
                  status = compare_offset_codes ( value_new_offset , 
                                                  value_old_offset ) ;

                  if ( status > 0 ) 
                  {
                     use_value = 0 ;
                  }
                  else if ( ( status == 0 ) && 
                            ( value_new_offset == value_old_offset ) )
                  {
                     /* This is either a duplicate report or a revision.
                        Check the revision flag to determine how to deal
                        with this report. */ 
                     update_action = determine_update_action (
                                              pOptions->shef_duplicate,
                                              rev_6hour_code [ i ] ); 

                     if ( ( update_action == DONT_UPDATE_ACTION ) ||
                          ( ( update_action == IF_DIFFERENT_UPDATE_ACTION ) &&
                            ( value_new == value_old ) ) )
                     {
                        use_value = 0 ;
                     }
                  }
               } 

               if ( use_value == 1)
               {
                  pWriteInfo->db_action = WriteRecUpdate ;
                  pWriteInfo->num_hr_updates ++ ; 
               }
            }
            else
            {
               /* There is not a preexisting value. */
               pWriteInfo->db_action = WriteRecUpdate ;
               pWriteInfo->num_hr_inserts ++ ;
            }

            if ( use_value == 1 )
            {
               set_6hour_slot_value ( pHour_old , i + 1 , value_new ) ; 
               pHour_old->sixhrqc [ i ] = pHour_new->sixhrqc [ i ] ; 
               pHour_old->sixhroffset [ i ] = value_new_offset ;
            }
            else
            {
               pWriteInfo->num_hr_ignored ++ ;
            }
         }
      }
  
      if ( pWriteInfo->db_action == WriteRecUpdate )
      {
          if ( is_pc == 1 )
          {
             status = UpdateHourlyPC ( ( HourlyPC * ) pHour_old , where ) ;              
          }
          else
          {
             status = UpdateHourlyPP ( pHour_old , where ) ;              
          }

          if ( status != ERR_OK ) 
          {
             /* Retrieve the DbStatus structure which contains the
                SQLSTATE structure. */
             pDbStatus = GetHourlyPPDbStatus ( );

             if ( pDbStatus != NULL )
             {
                if ( is_pc == 1 )
                {
                   sprintf ( msgstr , "Error on update to HourlyPC. " 
                                      "SQLSTATE: %s Key: %s%s%s" , 
                                       pDbStatus->sql_state , pHour_new->lid ,
		      		       pHour_new->ts , obsdate ) ;
                }
                else
                {
                   sprintf ( msgstr , "Error on update to HourlyPP " 
                                      "SQLSTATE: %s Key: %s%s%s" ,
                                      pDbStatus->sql_state , pHour_new->lid ,
		   		      pHour_new->ts , obsdate ) ;
                }
             }

             pWriteInfo->db_action = WriteRecNone ; 
             pWriteInfo->num_hr_ignored = num_values ;
             return_status = GPP_ERROR ;
          }
      }

      /* Free the memory associated with the one record read from 
         the Hourly table. */ 
      if ( is_pc == 1 )
      {
         FreeHourlyPC ( ( HourlyPC * ) pHour_old ) ;
      }
      else
      {
         FreeHourlyPP ( pHour_old  ) ;
      }

      pHour_old = NULL ;

   }

   return return_status ;
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   get_offset_code
* PURPOSE:       Returns the character code which represents the minute 
*                offset from the top of the hour.  The coding scheme works as 
*                follows:
*
*                Char Code     Positive Offset / Negative Offset (minutes)   
*                0    0               0            0
*                A    a               1           -1 
*                B    b               2           -2
*                C    c               3           -3
*                D    d               4           -4
*                E    e               5           -5
*                F    f               6           -6
*                G    g               7           -7
*                H    h               8           -8
*                I    i               9           -9
*                J    j              10           -10
*                K    k              11           -11
*                L    l              12           -12
*                M    m              13           -13
*                N    n              14           -14
*                O    o              15           -15
*                P    p              16           -16
*                Q    q              17           -17
*                R    r              18           -18
*                S    s              19           -19
*                T    t              20           -20
*                U    u              21           -21
*                V    v              22           -22
*                W    w              23           -23
*                X    x              24           -24
*                Y    y              25           -25
*                Z    z              26           -26
*                [    {              27           -27
*                \    |              28           -28
*                ]    }              29           -29
*                ^                   30           
*
*                These are divided into two sets.  One set represents positive
*                offsets.  These are offsets from 0 (the top of the hour) to
*                30 minutes past the hour.
*
*                The other set represents the negative offsets.  These are
*                offsets.  These are offsets from 1 minute before the top of
*                the hour to 29 minutes before the top of the hour.
*
*                If the minute supplied to this routine ranges from 31 to 59,
*                then the negative offset (before the top of the hour) list
*                is used and the appropriate character is used.
*
*                If the minute supplies to this routine ranges from 0 to 30,
*                then the positive offset (after the top of the hour) list is
*                used and the appropriate character is used.
*
*                The characters have been chosen is such a way that they are
*                sequential ASCII characters this makes it easy to convert them
*                back into actual durations. 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME          DESCRIPTION/UNITS
*   Input  integer     minute        The minute to retrieve the duration code
*                                    for.
*
* RETURNS:
*   DATA TYPE                 DESCRIPTION
*   char                      The one character offset code for the 
*                             supplied minute.
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME        DESCRIPTION
*   char       code        Contains the actual offset code.
*   int        offset      Contains the offset in minutes of the
*                          caller-supplied minute from the top of the hour.
*
* DATA FILES AND/OR DATABASE:
* None.
*
* ERROR HANDLING:
* None.
*
********************************************************************************
*/

static char zero_offset = '0' ;
static int positive_offset_base = ( int ) 'A' ;
static int negative_offset_base = ( int ) 'a' ;

char get_offset_code ( int minute )
{
   char code ;
   int offset ;

   /* You should be able to have a base value for positive and negative offsets
      to allow you to calculate the absolute offset when decoding. */
   if ( minute == 0 )
   {
      code = zero_offset ;
   }
   else if ( minute > 30 )
   {
      offset = MINUTES_PER_HOUR - minute - 1  ;
      code = negative_offset_base + offset ; 
   }
   else
   {
      offset = minute - 1  ;
      code = positive_offset_base + offset ;
   }

   return code ; 
}

/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   compare_offset_codes
* PURPOSE:       This routine compares two offset character codes.  It returns
*                a value indicating how the absolute offset of the first 
*                character code compares to the absolute offset of the 
*                second character code.  A return value of 0 means the 
*                durations are equal.  A return value of -1 means the
*                duration of the first code is smaller than the duration of the
*                second code.  A return value of 1 means the duration of the
*                first code is larger than the duration of the second code.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  char        minute_offset1       The first offset code to be
*                                           compared.
*   Input  char        minute_offset2       The second offset code to be
*                                           compared.
*
* RETURNS:
*   DATA TYPE          DESCRIPTION
*   int                A value indicating how the offset represented by the
*                      first character code compares to the offset
*                      represented by the second character code.
*
*                      -1   The duration of the first character code is smaller
*                           than the duration of the second character code.
*                       0   The durations of the two character codes are the
*                           same. 
*                       1   The durations of the second character code is 
*                           greater than the duration of the first character
*                           code.
*
* APIs UTILIZED:
*   None.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME           DESCRIPTION
*   int        offset1        The actual offset represented by the first
*                             character code.
*   int        offset2        The actual offset represented by the second
*                             character code. 
*   int        status         Can have one value from the set {-1,0,1}.
*
* DATA FILES AND/OR DATABASE:
* None.
*
* ERROR HANDLING:
* There is no error handling.
********************************************************************************
*/
int compare_offset_codes ( char minute_offset1 , char minute_offset2 ) 
{
   int offset1 ;
   int offset2 ;
   int status ;

   if ( ( int ) minute_offset1 >= ( int ) negative_offset_base )
   {
      offset1 = ( ( int ) minute_offset1 - ( int ) negative_offset_base ) + 1;
   }
   else if ( ( int ) minute_offset1 >= ( int ) positive_offset_base )
   {
      offset1 = ( ( int ) minute_offset1 - ( int ) positive_offset_base ) + 1;
   }
   else 
   {
      offset1 = 0 ;
   }

   if ( ( int ) minute_offset2 >= ( int ) negative_offset_base )
   {
      offset2 = ( ( int ) minute_offset2 - ( int ) negative_offset_base ) + 1;
   }
   else if ( ( int ) minute_offset2 >= ( int ) positive_offset_base )
   {
      offset2 = ( ( int ) minute_offset2 - ( int ) positive_offset_base ) + 1;
   }
   else
   {
      offset2 = 0 ;
   }

   status = 0 ;

   if ( offset1 < offset2 ) 
   {
      status = -1 ;
   }
   else if ( offset1 > offset2 )
   {
      status = 1 ;
   }

   return status ;
} 

/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   get_hour_slot_value
* PURPOSE:       Give an hour ( 1 through 24 ) and an structure containing
*                hourly data (either a HourlyPC or a HourlyPP structure),
*                this function returns the value for that hour.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  HourlyPP *  pHourlyPP            Contains data which corresponds
*                                           to a record in the HourlyPP table. 
*                                           Since the HourlyPP table has the
*                                           exact same structure as the
*                                           HourlyPC table, it can be passed
*                                           to this routine as well with
*                                           proper casting.
*   Input  int         hour                 The hour slot to retrieve the
*                                           precip amount from. Acceptable
*                                           hours are 1 through 24 inclusive.
*
* RETURNS:
*   DATA TYPE         DESCRIPTION 
*   short int         The precip value retrieved from the hour slot.
*
* APIs UTILIZED:
*   None.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   short int  precip_amount                The amount of precipitation 
*                                           found in the hour slot.
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   None.
********************************************************************************
*/

inline short int get_hour_slot_value ( const HourlyPP * pHourlyPP , 
		                               short int hour )
{
   short int precip_value = MISSING_PRECIP ;

   /* Depending on the hour, select the value in the correct hour slot
      in the HourPC structure. */
   switch ( hour )
   {
      case 1:

         precip_value = pHourlyPP->hour1 ;
         break ;

      case 2:

         precip_value = pHourlyPP->hour2 ;
         break ;
  
      case 3:

         precip_value = pHourlyPP->hour3 ;
         break ;

      case 4:

         precip_value = pHourlyPP->hour4 ;
         break ;

      case 5:

         precip_value = pHourlyPP->hour5 ;
         break ;

      case 6:

         precip_value = pHourlyPP->hour6 ;
         break ;

      case 7:

         precip_value = pHourlyPP->hour7 ;
         break ;

      case 8:

         precip_value = pHourlyPP->hour8 ;
         break ;

      case 9:

         precip_value = pHourlyPP->hour9 ;
         break ;

      case 10:

         precip_value = pHourlyPP->hour10 ;
         break ;

      case 11:

         precip_value = pHourlyPP->hour11 ;
         break ;

      case 12:

         precip_value = pHourlyPP->hour12 ;
         break ;

      case 13:

         precip_value = pHourlyPP->hour13 ;
         break ;

      case 14:

         precip_value = pHourlyPP->hour14 ;
         break ;

      case 15:

         precip_value = pHourlyPP->hour15 ;
         break ;

      case 16:

         precip_value = pHourlyPP->hour16 ;
         break ;

      case 17:

         precip_value = pHourlyPP->hour17 ;
         break ;

      case 18:

         precip_value = pHourlyPP->hour18 ;
         break ;

      case 19:

         precip_value = pHourlyPP->hour19 ;
         break ;

      case 20:

         precip_value = pHourlyPP->hour20 ;
         break ;

      case 21:

         precip_value = pHourlyPP->hour21 ;
         break ;

      case 22:

         precip_value = pHourlyPP->hour22 ;
         break ;

      case 23:

         precip_value = pHourlyPP->hour23 ;
         break ;

      case 24:

         precip_value = pHourlyPP->hour24 ;
         break ;
    
      default:
         break ;
   }

   return precip_value ;
}

/*******************************************************************************
* MODULE NUMBER:   5
* MODULE NAME:     set_hour_slot_value
* PURPOSE:         Sets the value in an hour slot in a HourlyPC or HourlyPP
*                  structure.
*
* ARGUMENTS:
*   TYPE         DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input/Output HourlyPP *  pHourlyPP            Contains the hour slot
*                                                 to place the precip
*                                                 value in.
*   Input        short int   hour                 The hour slot to receive
*                                                 the precip value.
*   Input        short int   precip_value         The precip value to 
*                                                 place into the hour slot. 
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS:
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
inline void set_hour_slot_value ( HourlyPP * pHourlyPP , short int hour ,
                                  short int precip_value )
{
   
   /* Depending on the hour, insert the value into the correct hour slot
      in the HourPC structure. */
   switch ( hour )
   {
      case 1:

         pHourlyPP->hour1 = precip_value ;
         break ;

      case 2:

         pHourlyPP->hour2 = precip_value ;
         break ;
  
      case 3:

         pHourlyPP->hour3 = precip_value ;
         break ;

      case 4:

         pHourlyPP->hour4 = precip_value ;
         break ;

      case 5:

         pHourlyPP->hour5 = precip_value ;
         break ;

      case 6:

         pHourlyPP->hour6 = precip_value ;
         break ;

      case 7:

         pHourlyPP->hour7 = precip_value ;
         break ;

      case 8:

         pHourlyPP->hour8 = precip_value ;
         break ;

      case 9:

         pHourlyPP->hour9 = precip_value ;
         break ;

      case 10:

         pHourlyPP->hour10 = precip_value ;
         break ;

      case 11:

         pHourlyPP->hour11 = precip_value ;
         break ;

      case 12:

          pHourlyPP->hour12 = precip_value ;
         break ;

      case 13:

         pHourlyPP->hour13 = precip_value ;
         break ;

      case 14:

         pHourlyPP->hour14 = precip_value ;
         break ;

      case 15:

         pHourlyPP->hour15 = precip_value ;
         break ;

      case 16:

         pHourlyPP->hour16 = precip_value ;
         break ;

      case 17:

         pHourlyPP->hour17 = precip_value ;
         break ;

      case 18:

         pHourlyPP->hour18 = precip_value ;
         break ;

      case 19:

         pHourlyPP->hour19 = precip_value ;
         break ;

      case 20:

         pHourlyPP->hour20 = precip_value ;
         break ;

      case 21:

         pHourlyPP->hour21 = precip_value ;
         break ;

      case 22:

         pHourlyPP->hour22 = precip_value ;
         break ;

      case 23:

         pHourlyPP->hour23 = precip_value ;
         break ;

      case 24:

         pHourlyPP->hour24 = precip_value ;
         break ;
    
      default:

         break ;
   }

   return ;
}

/*******************************************************************************
* MODULE NUMBER:   5
* MODULE NAME:     gage_pp_write_daily_rec
* PURPOSE:         This routine writes a record to the DailyPP table.  This
*                  record represents a 24 hour precipitation amount either of
*                  type PPP or PPD.
*
* ARGUMENTS:
*   TYPE         DATA TYPE   NAME                 DESCRIPTION/UNITS
*                                                 place into the hour slot. 
*
* RETURNS:
*
* APIs UTILIZED:
*
* LOCAL DATA ELEMENTS:
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*
********************************************************************************
*/

int gage_pp_write_daily_rec ( const DailyPP * pDailyPP,
                              const GagePPoptions * pOptions,
                              const char * obstime,
                              WriteInfo * pWriteInfo,
                              char msgstr [ ],
                              int revision_flag )
{
	bool record_exists;
	DbStatus * pDbStatus = NULL;
	int db_status;
	int status = GPP_OK;
	static char where [ 150 ];
	DailyPP * pRecord = NULL;
	int update_action;
	
	/* Initialize the write information structure. */
    pWriteInfo->db_action = WriteRecNone ;
    pWriteInfo->num_hr_inserts = 0 ;
    pWriteInfo->num_hr_updates = 0 ;
    pWriteInfo->num_hr_ignored = 1 ;
		
	/* This routine writes a record out to the DailyPP table.  It tests the
	 * shef_duplicate token to determine how it should handle duplicate
	 * reports and revisions. */
    update_action = determine_update_action ( pOptions->shef_duplicate,
                                              revision_flag ); 
                                
    /* The possible update actions are: DONT_UPDATE_ACTION, UPDATE_ACTION,
                                        IF_DIFFERENT_UPDATE_ACTION */
                                        
    /* Check if there is already a record in the DailyPP table for this
     * record. */
    /* Construct the where clause. */
    memset ( where, '\0', 150 );
    sprintf ( where, "WHERE lid = '%s' and ts = '%s' and obstime = '%s'",
              pDailyPP->lid, pDailyPP->ts, obstime ); 
    pRecord = GetDailyPP ( where );
     
    if ( pRecord != NULL )
    {
       record_exists = true;
    }
    else
    {
       record_exists = false;
    }
     
    if ( record_exists == false || 
         update_action == UPDATE_ACTION )                                        \
    {
       /* Perform a Insert or Update. */
       db_status = InsertOrUpdateDailyPP ( pDailyPP );
     	
       if ( db_status != ERR_OK )
       {
     	   /* Retrieve the SQLSTATE. SQLCODE is no longer a useful
              indicator of database errors with PostGres. */
           pDbStatus = GetDailyPPDbStatus ( );
          
           sprintf ( msgstr , "Error on insert/update to DailyPP, SQLSTATE: %s "
                              "Key:%s%s%s" , pDbStatus->sql_state , 
                              pDailyPP->lid , pDailyPP->ts , obstime ) ;        
           status = GPP_ERROR;

       }
       else
       {
     	
          if ( record_exists == true )
          {
             pWriteInfo->num_hr_updates = 1;
             pWriteInfo->num_hr_ignored = 0;
             pWriteInfo->db_action = WriteRecUpdate;
          }
          else
          {
     	     pWriteInfo->num_hr_inserts = 1;
     	     pWriteInfo->num_hr_ignored = 0;
     	     pWriteInfo->db_action = WriteRecInsert;
          }
       }
     }
     else
     {
     	/* The record exists and update action is not UPDATE_ACTION */
     	if ( update_action == IF_DIFFERENT_UPDATE_ACTION )
        {
      	   /* Check if the new value is different from the 
     		* value which already exists in the database. */
     		if ( pRecord->value != pDailyPP->value )
     		{
     			/* Update the record. */
     			db_status = UpdateDailyPPByRecord ( pDailyPP, pRecord );
     			
     			if ( db_status != ERR_OK )
     			{
     				pDbStatus = GetDailyPPDbStatus ( );
     				sprintf ( msgstr, "Error on update to DailyPP, " 
     				                   "SQLSTATE: %s Key: %s%s%s",
     				                   pDbStatus->sql_state ,
     				                   pDailyPP->lid, pDailyPP->ts,
     				                   obstime );
     		        status = GPP_ERROR;
     			}
     			else
     			{
     			    pWriteInfo->num_hr_updates = 1;	
     			    pWriteInfo->num_hr_ignored = 0;
     			    pWriteInfo->db_action = WriteRecUpdate;
     			}
     		}
      	}
     }
          
     return status;
}

/*******************************************************************************
* MODULE NUMBER: 6
* MODULE NAME:   get_6hour_slot_value
* PURPOSE:       Give a 6 hour value and an structure containing
*                hourly data (only a HourlyPP structure),
*                this function returns the value for that 6 hour slot.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  HourlyPP *  pHourlyPP            Contains data which corresponds
*                                           to a record in the HourlyPP table. 
*                                           Since the HourlyPP table has the
*                                           exact same structure as the
*                                           HourlyPC table, it can be passed
*                                           to this routine as well with
*                                           proper casting.
*   Input  int         hour                 The hour slot to retrieve the
*                                           precip amount from. Acceptable
*                                           hours are 1 through 4 inclusive.
*                                           1: 00 - 06z
*                                           2: 06 - 12z
*                                           3: 12z - 18z
*                                           4: 18z - 00z
* RETURNS:
*   DATA TYPE         DESCRIPTION 
*   short int         The precip value retrieved from the hour slot.
*
* APIs UTILIZED:
*   None.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   short int  precip_value                 The amount of precipitation 
*                                           found in the hour slot.
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   None.
********************************************************************************
*/

inline short int get_6hour_slot_value ( const HourlyPP * pHourlyPP , 
		                                short int hour )
{
   short int precip_value = MISSING_PRECIP ;

   /* Depending on the hour, select the value in the correct hour slot
      in the HourPC structure. */
   switch ( hour )
   {
      case 1:   /* 00z - 06z */

         precip_value = pHourlyPP->sixhr06 ;
         break ;

      case 2:  /* 06z - 12z */

         precip_value = pHourlyPP->sixhr12 ;
         break ;
  
      case 3:  /* 12z - 18z */

         precip_value = pHourlyPP->sixhr18 ;
         break ;

      case 4:  /* 18z - 00z */

         precip_value = pHourlyPP->sixhr24 ;
         break ;
    
      default:
         break ;
   }

   return precip_value ;
}

/*******************************************************************************
* MODULE NUMBER:   7
* MODULE NAME:     set_6hour_slot_value
* PURPOSE:         Sets the value in an 6 hour slot in a HourlyPP
*                  structure.
*
* ARGUMENTS:
*   TYPE         DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input/Output HourlyPP *  pHourlyPP            Contains the 6 hour slot
*                                                 to place the precip
*                                                 value in.
*   Input        short int   hour                 The 6 hour slot to receive
*                                                 the precip value.
*                                                 1:  00 - 06z
*                                                 2:  06 - 12z
*                                                 3:  12 - 18z
*                                                 4:  18 - 00z
* 
*   Input        short int   precip_value         The precip value to 
*                                                 place into the hour slot. 
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS:
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
inline void set_6hour_slot_value ( HourlyPP * pHourlyPP , short int hour ,
                                   short int precip_value )
{
   
   /* Depending on the hour, insert the value into the correct hour slot
      in the HourPC structure. */
   switch ( hour )
   {
      case 1:

         pHourlyPP->sixhr06 = precip_value ;
         break ;

      case 2:

         pHourlyPP->sixhr12 = precip_value ;
         break ;
  
      case 3:

         pHourlyPP->sixhr18 = precip_value ;
         break ;

      case 4:

         pHourlyPP->sixhr24 = precip_value ;
         break ;
    
      default:

         break ;
   }

   return ;
}

/*******************************************************************************
* MODULE NUMBER:   8
* MODULE NAME:     use_precip_value
* PURPOSE:         Given an old and new precip value, their respective
*                  offsets from top of hour, their respective qc
*                  codes, and the revision flag associated with the
*                  new value, determine if the new precip value
*                  should overwrite the old one.
*
* ARGUMENTS:
*   TYPE         DATA TYPE   NAME            DESCRIPTION/UNITS
*   Input        short       value_new       The new precip value.
*   Input        short       value_old       The old precip value.
*   Input        char        value_new_qc    The shef qc code associated
*                                            with the new precip value.
*   Input        char        value_old_qc    the shef qc code associated
*                                            with the old precip_value.
*   Input        char        value_new_offset  The 1 character offset code
*                                              associated with the new
*                                              value.
*   Input        char        value_old_offset  The 1 character offset code
*                                              associated with the old value.
*   Input        int         shef_duplicate    Contains the value of the 
*                                              SHEF duplicate token.
*   Input        int         revision_codea    Contains the revision flag
*                                              associated with the new
*                                              precipitation value.
*  
* RETURNS:
*   int       0:  Don't overwrite the old precipitation value with the
*                 old precipitation value.
*   int       1:  Overwrite the old precipitation value with the new 
*                 precipitation value.
*
* APIs UTILIZED:
*   determine_update_action
*   compare_offset_codes
*
* LOCAL DATA ELEMENTS:
*   int status			The return status from calls to IsNUll and
*                       compare_offset_codes.	      
*   int update_action   The return value from a call to the 
*                       determine_update_action routine.
*   int use_value       Flag indicating, 0 - don't use the new 
*                       precipitation value; 1 - do use the new 
*                       precipitation value.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/


int use_precip_value ( short value_new,
                       short value_old,
                       char value_new_qc,
                       char value_old_qc,
                       char value_new_offset,
                       char value_old_offset,
                       int shef_duplicate,
                       int revision_code
                     )
{
   int status;
   int update_action;
   int use_value = 1;

   /* There is a value in the new hour slot.  There is also
      a value in the old hour slot.  If the value in the old
      hour slot has been manually edited, DO NOT overwrite
      it unless the new value is a manual edited. */
   if ( ( value_old_qc == 'M' ) && ( value_new_qc != 'M' ) )
   {
      use_value = 0 ;
   }
   else
   {
      /* Check to see which is closest to the top of the hour. 
         Check to see if they are identical. */

      /* Compare the number of minutes each of these reports is 
         offset from the top of the hour. */
      status = compare_offset_codes ( value_new_offset , 
                                      value_old_offset ) ;

      if ( status > 0 ) 
      {
         use_value = 0 ;
      }
      else if ( ( status == 0 ) && 
                ( value_new_offset == value_old_offset ) )
      {
         /* This is either a duplicate report or a revision.
            Check the revision flag to determine how to deal
            with this report. */ 
         update_action = determine_update_action ( shef_duplicate,
                                      revision_code ); 

         if ( ( update_action == DONT_UPDATE_ACTION ) ||
              ( ( update_action == IF_DIFFERENT_UPDATE_ACTION ) &&
                ( value_new == value_old ) ) )
         {
            use_value = 0 ;
         }
     }
  } 

   return use_value;
}
