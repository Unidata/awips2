/*******************************************************************************
* FILENAME:            update_rwr_save.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          update_rwr_save
* DESCRIPTION:         This subroutine updates the mapx_field_type, auto_save, 
*                      draw_precip and last_save_time fields in the RWResult 
*                      table.
*
* ORIGINAL AUTHOR:  Bryon Lawrence
* CREATION DATE:    November 4, 2004
* ORGANIZATION:     HSEB/OHD
* OPERATING SYSTEM: Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/5/2004    Bryon Lawrence    Modified to use DBGen 
*                                                  routines instead of ESQLC.
********************************************************************************
*/
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "DbmsDefs.h"
#include "dbmserrs.h"
#include "mpe_log_utils.h"
#include "RWResult.h"
#include "stage3.h"
#include "time_convert.h"
#include "update_rwr_save.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   update_rwr_save
* PURPOSE:       This routine updates the  mapx_field_type, auto_save,
*                draw_precip and last_save_time fields in the RWResult
*                table. 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  char *      rfc         The id of the office Hydroview/MPE is
*                                  being run at.
*   Input  char *      dt          The date and time of the MPE run.
*   Input  char *      fldtype     The type of the field being saved.
*   Output long *      irc         Indicates whether or not the update
*                                  was successful.
* RETURNS:
*   No return value
*
* APIs UTILIZED:
*   NAME                HEADER FILE     DESCRIPTION
*   FreeRWResult        RWResult.h      Frees memory used by linked list of
*                                       RWResult structures.
*   GetRWResult         RWResult.h      Retrieves rows from RWResult table.
*                                       Builds a linked list where a node
*                                       is a record in the table. 
*   ListFirst           List.h          Lists the first node in the linked
*                                       list.
*   UpdateRWResult      RWResult.h      Updates a record in the RWResult
*                                       table. 
*   yearsec_ansi_to_dt  convert_time.h  Converts a year to second ansi
*                                       formatted time to a dtime_t.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE   NAME               DESCRIPTION
*   char [ ]    asave              The autosave flag.
*   char [ ]    datetime_save_xmrg The time the xmrg product was last saved.
*   char [ ]    drpr               The draw polygon flag.
*   char [ ]    where              The where clause used to query the
*                                  RWResult.
*   int         status             The status of the update of the RWResult
*                                  table. 
*   RWResult  * pRWResultHead      A pointer to the head node in the linked
*                                  list of RWResult structures.
*   RWResult  * pRWResultNode      A pointer to a node in the linked list
*                                  of RWResult structures.
*   struct tm * pStructTm          A pointer to a struct tm.
*   time_t      current_timet      The current time in time_t.
*
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
* CALLING ROUTINE:
*    save_rfcwide
*
********************************************************************************
*/
void update_rwr_save ( const char * rfc, const date_struct * dt, const char * fldtype,
                       long int * irc )
{
   char asave [ BOOL_LEN + 1 ]; 
   char datetime_save_xmrg [ ANSI_TIME_LEN ];
   char datetime_obs_xmrg [ ANSI_TIME_LEN ];
   char drpr [ BOOL_LEN + 1 ];
   char where [ 100 ];
   int status ;
   RWResult * pRWResultHead = NULL;
   RWResult * pRWResultNode = NULL;
   struct tm * pStructTm = NULL;
   time_t current_timet;

   memset ( asave, '\0', BOOL_LEN + 1 );
   memset ( drpr , '\0', BOOL_LEN + 1 );
   strncpy ( asave , "F", BOOL_LEN );
   strncpy ( drpr , "F", BOOL_LEN ); 
   memset ( where , '\0', 100 );

   * irc = ERR_OK;

   if ( applyprecip_flag == 1 )
   {
      strncpy ( drpr, "T", BOOL_LEN );
   }
   /* Build the obstime time string. */
   memset ( datetime_obs_xmrg, '\0', ANSI_TIME_LEN );
   sprintf ( datetime_obs_xmrg , "%04d-%02d-%02d %02d:00:00" , dt->year, 
             dt->month , dt->day , dt->hour ) ;
      
   /* Build the where clause. This can be used for both the select
      and the update. */
   sprintf ( where, "WHERE rfc='%s' AND obstime='%s'", rfc, datetime_obs_xmrg );
                 
   /* Get the record to update from the RWResult table. */
   pRWResultHead = GetRWResult ( where ) ;

   if ( pRWResultHead == NULL )
   {
      flogMessage ( stderr, "In routine 'update_rwr_save':\n"
                        "Could not select a record from the RWResult\n"
                        "table for query '%s'.\n" , where ) ;
      * irc = -1 ; 
   }
   else
   {
      pRWResultNode = ( RWResult * ) ListFirst ( & pRWResultHead->list ) ;

      /* Update the elements in the RWResult node. */
      strcpy ( pRWResultNode->mapx_field_type, fldtype );
      strcpy ( pRWResultNode->auto_save, asave );
      strcpy ( pRWResultNode->draw_precip, drpr );

      current_timet = time ( NULL );
      pStructTm = gmtime ( & current_timet );
      strftime ( datetime_save_xmrg, ANSI_TIME_LEN, "%Y-%m-%d %H:%M:%S",
                 pStructTm );

      yearsec_ansi_to_dt ( datetime_save_xmrg, 
                           & pRWResultNode->last_save_time );

      /* Update the record in the database. */
      status = UpdateRWResult ( pRWResultNode , where ) ;

      if ( status != ERR_OK )
      {
         flogMessage ( stderr, "In routine 'update_rwr_save':\n"
                           "could not update record in RWResult for\n"
                           "query '%s'.\n" , where ) ;
	 * irc = ( long ) status ;
      }


      /* Free the memory used by the linked list of RWResult structures. */
      FreeRWResult ( pRWResultHead ) ;
      pRWResultHead = NULL ;
   }

   return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
