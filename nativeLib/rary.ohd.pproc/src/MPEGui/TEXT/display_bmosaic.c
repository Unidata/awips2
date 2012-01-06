/*=========================================================================*/
/*                         FILE NAME:   display_bmosaic.c                  */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_bmosaic                    */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <Xm/Xm.h>

#include "drawa.h"
#include "display_field.h"
#include "display_field_data_RFCW.h"
#include "display_precip_data.h"
#include "map_library.h"
#include "map_resource.h"
#include "newhour_RFCW.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"

/***************************************************************************/
/*  FUNCTION NAME:   display_bmosaic_RFCW()                                */
/*       FUNCTION:   This is a X/Motif wrapper around the "display_bmosaic"*/
/*                   routine.                                              */ 
/***************************************************************************

Function type:
   void

Called by function:
   This is the callback for the "Field Bias Mosaic" menu item under the
   "Fields" drop down menu on the hmap-mpe application.

************************************** BEGIN display_bmosaic_RFCW ************/

void display_bmosaic_RFCW ( Widget w , XtPointer clientdata ,
                            XtPointer calldata )
{
   int * map_number = NULL;

   if ( clientdata == NULL ) return;

   map_number = ( int * ) clientdata;

   display_bmosaic ( * map_number ) ;
}

/***************************************************************************/
/*  FUNCTION NAME:   display_bmosaic()                                     */
/*       FUNCTION:   display bmosaic grid                                  */
/***************************************************************************

Function type:
   void

Called by function:
   callback from BMosaic button (under Display)

************************************** BEGIN display_bmosaic **************/

void display_bmosaic( int map_number )
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

 rad_data [ map_number ].field_type = display_bMosaic ;  

 strcpy( cv_use , "BMOSAIC" ) ;
 strcpy( rad_data[map_number].cv_use , "BMOSAIC" ) ;
 cv_duration = 3600 ;
 rad_data[map_number].cv_duration = cv_duration;
 
 cv_duration_mainwin = cv_duration;
 strcpy(cv_use_mainwin,cv_use);

/*----------------------------------*/
/*  turn on save option             */
/*  turn on draw precip option      */
/*----------------------------------*/

 if (first_display == FALSE && map_number == 0 )
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
    len = strlen("rfcwide_bmosaic_dir");
    get_apps_defaults("rfcwide_bmosaic_dir",&len,dirname,&len);
    first = 0;
 }

 sprintf(fname,"%s/%s%sz",dirname,cv_use,date_st3.cdate);
 len_fname = strlen(fname);

 /*----------------------------------------------------------------------*/
 /*  read field data from file                                           */
 /*  display field on main window                                        */
 /*----------------------------------------------------------------------*/

 display_field(fname, len_fname, map_number );

 mSetCursor ( M_NORMAL ) ;
}
/********************************************* END display_bmosaic ************/
