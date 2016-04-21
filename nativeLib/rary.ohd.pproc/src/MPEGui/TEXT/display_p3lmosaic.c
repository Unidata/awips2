/*=========================================================================*/
/*                         FILE NAME:   display_p3lmosaic.c                */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_p3lmosaic                  */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <Xm/Xm.h>

#include "display_field.h"
#include "display_field_data_RFCW.h"
#include "display_precip_data.h"
#include "drawa.h"
#include "map_library.h"
#include "map_resource.h"
#include "newhour_RFCW.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"

/***************************************************************************/
/*  FUNCTION NAME:   display_p3lmosaic_RFCW()                               */
/*       FUNCTION:   A X/Motif wrapper around the "display_p3lmosaic"       */
/*                   routine.  This can be called directly from             */
/*                   a X/Motif widget such as a button.                     */ 
/***************************************************************************

Function type:
   void

Called by function:
   This is the callback for the "Local Bias Mosaic" item under the "Fields"
   drop down menu. 

************************************** BEGIN display_lmosaic_RFCW ***********/

void display_p3lmosaic_RFCW ( Widget w , XtPointer clientdata ,
                              XtPointer calldata ) 
{
   int * map_number = NULL;

   if ( clientdata == NULL ) return;

   map_number = ( int * ) clientdata;

   display_p3lmosaic ( * map_number ) ;
}


/***************************************************************************/
/*  FUNCTION NAME:   display_p3lmosaic()                                     */
/*       FUNCTION:   display p3lmosaic value                                 */
/***************************************************************************

Function type:
   void

Called by function:
   callback from P3 LMosaic button (under Display)
   initialize_data_RFCW

************************************** BEGIN display_lmosaic **************/

void display_p3lmosaic( int map_number )
{

 extern int           cv_duration ;
 static int           first = 1;
 int                  len, len_fname;
 static char          dirname [ 97 ] = {'\0'};
 static char          fname [ 128 ] = {'\0'};

 /*----------------------------------------------------------------------*/
 /*  set parameters for search in colorvalue table                       */
 /*----------------------------------------------------------------------*/
 mSetCursor ( M_WATCH ) ;

 rad_data [ map_number ].field_type = display_p3Mosaic ;

 strcpy(cv_use,"P3LMOSAIC");
 strcpy(rad_data[map_number].cv_use,"P3LMOSAIC");
 cv_duration = 3600;
 rad_data[map_number].cv_duration = cv_duration;

 cv_duration_mainwin = cv_duration;
 strcpy(cv_use_mainwin,cv_use);

/*----------------------------------*/
/*  turn on save option             */
/*  turn on draw precip option      */
/*----------------------------------*/

 if (first_display == FALSE && map_number == 0)
 {
   sensitize_save_buttons ( );
   XtSetSensitive(drawpoly_widget, TRUE);
   XtSetSensitive(deletepoly_widget, TRUE);
 }

 /*----------------------------------------------------------------------*/
 /*  construct filename                                                   */
 /*----------------------------------------------------------------------*/

 if ( first == 1 )
 {
    len = strlen("rfcwide_p3_lmosaic_dir");
    get_apps_defaults("rfcwide_p3lmosaic_dir",&len,dirname,&len);
    first = 0;
 }

 sprintf(fname,"%s/%s%sz",dirname,cv_use,date_st3.cdate);
 len_fname = strlen(fname);

 /*----------------------------------------------------------------------*/
 /*  read field data from file                                           */
 /*  display field on main window                                        */
 /*----------------------------------------------------------------------*/

 display_field(fname, len_fname, map_number);
 mSetCursor ( M_NORMAL ) ;
}
/********************************************* END display_lmosaic ************/
