/*=========================================================================*/
/*                         FILE NAME:   display_max_temp_prism.c           */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_max_temp_prism_RFCW        */
/*                                      display_max_temp_prism             */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <Xm/Xm.h>
#include <Xm/ToggleB.h>

#include "display_field.h"
#include "display_field_data_RFCW.h"
#include "display_precip_data.h"
#include "drawa.h"
#include "map_library.h"
#include "map_resource.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "clear_data_RFCW.h"
#include "Xtools.h"
#include "post_functions.h"
#include "map.h"

/***************************************************************************/
/*  FUNCTION NAME:   display_max_temp_prism_RFCW ( )                       */
/*       FUNCTION:   This is a wrapper around the display_max_temp_prism   */
/*                   routine which allows it to easily be used as a        */
/*                   callback in a GUI.                                    */
/***************************************************************************

Function type:
   void

Called by function:
   This routine is called from the "Prism" item on the "Fields" menu on
   the hmap_mpe application's menu bar. 

*************************** BEGIN display_max_temp_prism_RFCW *************/

void display_max_temp_prism_RFCW ( Widget w , 
                                   XtPointer clientdata ,
                                   XtPointer calldata ) 
{
   int * map_number = NULL;     
   
   if ( clientdata == NULL ) return;
   
   map_number = ( int * ) clientdata;
   
   if (XmToggleButtonGetState (mpeClimo[ MonthlyMaxTempItem]))
   {
      XmToggleButtonSetState(mpeClimo[ MonthlyPrecipItem ],False,False); 
      XmToggleButtonSetState(mpeClimo[ MonthlyMaxTempItem ],True,False); 
      XmToggleButtonSetState(mpeClimo[ MonthlyMinTempItem ],False,False); 
   
      display_max_temp_prism ( * map_number ) ;
   }
   else
   {
      XmToggleButtonSetState(mpeClimo[ MonthlyMaxTempItem ],False,False); 
      
       turnOffMpeData ( ) ;
      _turn_legend_off ( ) ;
       mUpdateMap ( 0 ) ;
        
   }     
   
   
}

/*********************************** END display_max_temp_prism_RFCW ******/

/***************************************************************************/
/*  FUNCTION NAME:   get_month_name ( )                                    */
/*       FUNCTION:   Retrieves the abbreviated month name for a month      */
/*                   number.                                               */
/***************************************************************************

Function type:
   const char *

Called by function:
   This routine is called from the display_min_temp_prism and
   display_max_temp_prism functions. 

************************************** BEGIN get_month_name  *************/

const char * get_month_name ( int month_number )
{
   const static char * months [ 12 ] = { "jan",
                                         "feb",
                                         "mar",
                                         "apr",
                                         "may",
                                         "jun",
                                         "jul",
                                         "aug",
                                         "sep",
                                         "oct",
                                         "nov",
                                         "dec" };
   return months [ month_number - 1 ];

}

/*********************************** END get_month_name ********************/

/***************************************************************************/
/*  FUNCTION NAME:   display_max_temp_prism()                              */
/*       FUNCTION:   display prism values                                  */
/***************************************************************************

Function type:
   void

Called by function:
   callback from Prism   button (under Display)
   initialize_data_RFCW

************************************** BEGIN display_max_temp_prism *********/

void display_max_temp_prism( int map_number )
{
 extern int           cv_duration ;
 static int           first = 1;
 int                  len, len_fname;
 static char          dirname[97] = {'\0'};
 static char          fname [ 128 ] = {'\0'};
 static char          mpe_site_id [ 30 ] = {'\0'};
 const char *         month_name = NULL;

 /*----------------------------------------------------------------------*/
 /*  set parameters for search in colorvalue table                       */
 /*----------------------------------------------------------------------*/
 mSetCursor ( M_WATCH ) ;
 rad_data[ map_number ].field_type = display_maxtempPrism;

 strcpy(cv_use,"MAX_TEMP_PRISM");
 strcpy(rad_data[map_number].cv_use,"MAX_TEMP_PRISM");
 cv_duration = 0;
 rad_data[map_number].cv_duration = cv_duration;

 cv_duration_mainwin = cv_duration;
 strcpy(cv_use_mainwin,cv_use);

/*----------------------------------*/
/*  turn off save option            */
/*  turn off draw_precip option     */
/*----------------------------------*/

 if (first_display == FALSE)
 {
   XtSetSensitive(savemaintop_widget, FALSE);
   XtSetSensitive(drawpoly_widget, FALSE);
   XtSetSensitive(deletepoly_widget, FALSE);
 }

 /*----------------------------------------------------------------------*/
 /*  construct filename                                                   */
 /*----------------------------------------------------------------------*/

 if ( first == 1 )
 {
    /* Load and store the token values need to build the PRISM file path. */
    len = strlen("mpe_prism_dir");
    get_apps_defaults("mpe_prism_dir",&len,dirname,&len);
    len = strlen("mpe_site_id");
    get_apps_defaults("mpe_site_id",&len,mpe_site_id,&len);
    first = 0;
 }

 /* Reformat the name of the PRISM file to reflect the names used for the
    added DailyQC functionality. */
 month_name = get_month_name ( date_st3.month );
 sprintf ( fname, "%s/prism_max_temp_%s_%s", dirname, mpe_site_id,
                  month_name );
 len_fname = strlen ( fname );

 /*----------------------------------------------------------------------*/
 /*  read field data from file                                           */
 /*  display field on main window                                        */
 /*----------------------------------------------------------------------*/

 display_field(fname, len_fname, map_number );

 mSetCursor ( M_NORMAL ) ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/pproc_lib/src/MPEGui/RCS/display_max_temp_prism.c,v $";
 static char rcs_id2[] = "$Id: display_max_temp_prism.c,v 1.4 2007/07/11 16:53:17 lawrence Exp $";}
/*  ===================================================  */

}
/*********************************** END display_max_temp_prism_RFCW ****/

