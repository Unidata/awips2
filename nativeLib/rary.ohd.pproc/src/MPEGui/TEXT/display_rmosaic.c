/*=========================================================================*/
/*                         FILE NAME:   display_rmosaic.c                  */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_rmosaic                    */
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
/*  FUNCTION NAME:  display_rmosaic_RFCW ( )                               */
/*  FUNCTION:       display_rmosaic values callback routine.  This is a    */
/*                  simple X/Motif wrapper around the "display_rmosaic"    */
/*                  routine.                                               */
/***************************************************************************

Function type:
   void

Called by function:
   This is the callback to the "Radar Mosaic" menu item under the "Fields"
   drop down menu on the  hmap_mpe application.

************************************** BEGIN display_rmosaic_RFCW ************/

void display_rmosaic_RFCW ( Widget w , XtPointer clientdata , 
                            XtPointer call_data )
{
   int * map_number = NULL;

   if ( clientdata == NULL ) return;

   map_number = ( int * ) clientdata;

   display_rmosaic ( * map_number ) ;
}

/***************************************************************************/
/*  FUNCTION NAME:   display_rmosaic()                                     */
/*       FUNCTION:   display rmosaic values                                */
/***************************************************************************

Function type:
   void

Called by function:
   callback from RMosaic button (under Display)
   initialize_data_RFCW

************************************** BEGIN display_rmosaic **************/

void display_rmosaic( int map_number )
{

 extern int           cv_duration ;
 static int           first = 1;
 int                  len, len_fname ;
 static char          dirname [ 97 ] = {'\0'};
 static char          fname [ 128 ] = {'\0'};

 /*----------------------------------------------------------------------*/
 /*  set parameters for search in colorvalue table                       */
 /*----------------------------------------------------------------------*/

 rad_data [ map_number ].field_type = display_rMosaic;

 strcpy(cv_use,"RMOSAIC");
 strcpy(rad_data[map_number].cv_use,"RMOSAIC");
 cv_duration = 3600;
 rad_data[map_number].cv_duration = cv_duration;

 cv_duration_mainwin = cv_duration ;
 strcpy ( cv_use_mainwin , cv_use ) ;

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
    len = strlen("rfcwide_rmosaic_dir");
    get_apps_defaults("rfcwide_rmosaic_dir",&len,dirname,&len);
    first = 0;
 }

 sprintf(fname,"%s/%s%sz",dirname,cv_use,date_st3.cdate);
 len_fname = strlen(fname);

 /*----------------------------------------------------------------------*/
 /*  read field data from file                                           */
 /*  display field on main window                                        */
 /*----------------------------------------------------------------------*/

 display_field(fname, len_fname, map_number);
}
/********************************************* END display_rmosaic ************/
