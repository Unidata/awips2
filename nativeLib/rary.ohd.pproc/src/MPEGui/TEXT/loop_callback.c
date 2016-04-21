

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <sys/time.h>
#include <stdlib.h>
#include <X11/cursorfont.h>

#include "drawa.h"
#include "GeneralUtil.h"
#include "keysymbol.h"
#include "loop_callback.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "post_functions.h"
#include "stage3.h"
#include "rfcwide_interface.h"
#include "stage3_globals.h"

/***************************************************************************/
/*  FUNCTION NAME:   loop_callback()                                       */
/*       FUNCTION:   controlling function for time loop                    */
/***************************************************************************

Function type:
   void

Called by function:
   (callback from) XtAppAddTimeOut 

Functions called:
   get_loop_callback_duration
   _get_map_context 
   mUpdateMap
   setPixmapToDraw

******************************************** BEGIN loop_callback ***********/

static XtIntervalId xtid = 0 ;

void loop_callback ( XtPointer clientdata , XtIntervalId * id )
{
   int duration ;
   loop_struct * loop_data = ( loop_struct * ) clientdata ;
   XtAppContext app ;

 /*--------------------------------------------------------------*/
 /*     Set time delay and sleep to slow down loop               */
 /*     if last in sequence, pause is double                     */
 /*--------------------------------------------------------------*/
   loop_data->icnt -- ;

   if ( loop_data->icnt < 0 )
   {
      loop_data->icnt = loop_data->max ;
   }

   /* Set the current date of the data. */
   date_st3 = loop_data->dates [ loop_data->icnt ] ;
 
   if ( loop_data->icnt != loop_data->max )
   {
      date_prev = loop_data->dates [ loop_data->icnt + 1 ] ;
   }
   else
   {
      date_prev = loop_data->dates [ 0 ] ;
   }
 
   setPixmapToDraw ( & ( loop_data->pixmap [ loop_data->icnt ] ) ,
                     & ( loop_data->gc [ 0 ] ) ) ;

   mUpdateMap ( 0 ) ;

   app = _get_map_context ( ) ;
   duration  = get_loop_callback_duration ( ) ;
   xtid = XtAppAddTimeOut ( app , duration ,
                            loop_callback , ( XtPointer ) loop_data ) ;
}

/********************************************* END loop_callback ***********/

/***************************************************************************/
/*  FUNCTION NAME:   get_loop_callback_interval_id ( )                     */
/*       FUNCTION:   Returns the id of the time out interval controlling   */
/*                   the time lapse loop.                                  */  
/***************************************************************************

Function type:
   XtIntervalId 

Called by function:
   (callback from) end_time_lapse

Functions called:
   None
**************************** BEGIN get_loop_callback_interval_id ***********/

XtIntervalId get_loop_callback_interval_id ( )
{
   return xtid ;
}

/*************************** END get_loop_callback_interval_id *************/

/***************************************************************************/
/*  FUNCTION NAME:   set_loop_callback_interval_id ( )                     */
/*       FUNCTION:   Sets the id representing the time out interval.       */ 
/***************************************************************************

Function type:
   void

Called by function:
   (callback from) time_lapse_RFCW, loop_callback

Functions called:
   None

**************************** BEGIN set_loop_callback_interval_id ***********/

void set_loop_callback_interval_id ( XtIntervalId id )
{
   xtid = id ;
}

/*************************** END set_loop_callback_interval_id *************/

/***************************************************************************/
/*  FUNCTION NAME:   get_loop_callback_duration ( )                        */
/*       FUNCTION:   Returns the duration of the loop by reading the       */
/*                   "hmap_mpe_timelapse" token in the .Apps_defaults      */
/*                   file.                                                 */ 
/***************************************************************************

Function type:
   int 

Called by functions:
   time_lapse_RFCW, loop_callback

Functions called:
   get_apps_defaults 

**************************** BEGIN get_loop_callback_duration **************/

int get_loop_callback_duration ( )
{
   static char * hmap_mpe_timelapse_token = "hmap_mpe_timelapse" ;
   char reply [ MAX_TIMELAPSE_TOKEN_LEN ] ;

   static int duration_value = -1 ;
   int reply_len ;
   int request_len ;
   int status ;

   request_len = strlen ( hmap_mpe_timelapse_token ) ;

   if ( duration_value == -1 )
   {
      /* This is the first time that this routine is being called.
         Try to retrieve the value of the time lapse duration. */
      status = get_apps_defaults ( hmap_mpe_timelapse_token ,
                                   & request_len ,
                                   reply ,
                                   & reply_len ) ;

      if ( status != 0 )
      {
         flogMessage ( stderr , "\nIn routine \"get_loop_callback_duration\":\n"
                            "Could not retrieve the value of token\n"
                            "\"hmap_mpe_timelapse\".  Setting the default\n"
                            "duration to %d milliseconds.\n" , 
                            DEFAULT_TIMELAPSE_DURATION ) ; 
         duration_value = DEFAULT_TIMELAPSE_DURATION ; 
      }
      else
      {
         duration_value = atoi ( reply ) ;
      }
   }

   return duration_value ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/loop_callback.c,v $";
 static char rcs_id2[] = "$Id: loop_callback.c,v 1.3 2007/02/14 20:23:24 lawrence Exp $";}
/*  ===================================================  */

}

/**************************** END get_loop_callback_duration ****************/
