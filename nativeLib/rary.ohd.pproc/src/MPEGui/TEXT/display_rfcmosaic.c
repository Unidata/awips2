/*=========================================================================*/
/*                         FILE NAME:   display_rfcmosaic.c                */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_rfcmosaic                  */
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
/*  FUNCTION NAME:  display_rfcmosaic_RFCW ( )                             */
/*  FUNCTION:       display_rfcmosaic values callback routine.  This is a  */
/*                  simple X/Motif wrapper around the "display_rfcmosaic"  */
/*                  routine.                                               */
/***************************************************************************

Function type:
   void

Called by function:
   This is the callback to the "RFC QPE Mosaic" menu item under the "Fields"
   drop down menu on the MPE Editor application.

************************************** BEGIN display_rmosaic_RFCW ************/

void display_rfcmosaic_RFCW ( Widget w , XtPointer clientdata , 
                              XtPointer call_data )
{
   int * map_number = NULL;

   if ( clientdata == NULL ) return;

   map_number = ( int * ) clientdata;

   display_rfcmosaic ( * map_number ) ;
}

/***************************************************************************/
/*  FUNCTION NAME:   display_rfcmosaic()                                   */
/*       FUNCTION:   display rfcmosaic values                              */
/***************************************************************************

Function type:
   void

Called by function:
   initialize_data_RFCW

************************************** BEGIN display_rmosaic **************/

void display_rfcmosaic( int map_number )
{

 extern int           cv_duration ;
 static int           first = 1;
 int                  len, len_fname ;
 static char          dirname [ 97 ] = {'\0'};
 static char          fname [ 128 ] = {'\0'};

 /*----------------------------------------------------------------------*/
 /*  set parameters for search in colorvalue table                       */
 /*----------------------------------------------------------------------*/

 rad_data [ map_number ].field_type = display_rfcMosaic;

 strcpy(cv_use,"RFCMOSAIC");
 strcpy(rad_data[map_number].cv_use,"RFCMOSAIC");
 cv_duration = 3600;
 rad_data[map_number].cv_duration = cv_duration;

 cv_duration_mainwin = cv_duration ;
 strcpy ( cv_use_mainwin , cv_use ) ;

/*----------------------------------*/
/*  turn on save option             */
/*  turn on draw precip option      */
/*----------------------------------*/

 if (first_display == FALSE && map_number == 0 && savemaintop_widget != NULL &&
     drawpoly_widget != NULL && deletepoly_widget != NULL ) 
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
    len = strlen("gaq_xmrg_1hr_dir");
    get_apps_defaults("gaq_xmrg_1hr_dir",&len,dirname,&len);
    first = 0;
 }

 sprintf(fname,"%s/%s01%sz",dirname,cv_use,date_st3.cdate);
 len_fname = strlen(fname);

 /*----------------------------------------------------------------------*/
 /*  read field data from file                                           */
 /*  display field on main window                                        */
 /*----------------------------------------------------------------------*/

 display_field(fname, len_fname, map_number);
}
/********************************************* END display_rmosaic ************/
