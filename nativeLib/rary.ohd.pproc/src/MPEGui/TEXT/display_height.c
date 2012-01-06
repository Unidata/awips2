/*=========================================================================*/
/*                         FILE NAME:   display_height.c                   */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_height,                    */
/*                                      display_height_RFCW                */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <Xm/Xm.h>

#include "drawa.h"
#include "display_field.h"
#include "display_field_data_RFCW.h"
#include "display_precip_data.h"
#include "map_library.h"
#include "map_resource.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"

/***************************************************************************/
/*  FUNCTION NAME:   display_height_RFCW ( )                               */
/*       FUNCTION:   A X/Motif wrapper around the "display_height" routine */
/*                   that allows X/Motif widgets to easily use it as a     */
/*                   callback routine.                                     */
/***************************************************************************

Function type:
   void

Called by function:
   Callback for the "Height Field" item under the "Fields" drop down
   menu on the hmap_mpe application.

************************************** BEGIN display_height_RFCW *************/

void display_height_RFCW ( Widget w , XtPointer clientdata ,
                           XtPointer calldata )
{
   int * map_number = NULL;

   if ( clientdata == NULL ) return;

   map_number = ( int * ) clientdata;

   display_height ( * map_number ) ;
}

/***************************************************************************/
/*  FUNCTION NAME:   display_height( )                                     */
/*       FUNCTION:   display height values                                 */
/***************************************************************************

Function type:
   void

Called by function:
   callback from Height button (under Display)

************************************** BEGIN display_height **************/

void display_height( int map_number )
{

 extern int           cv_duration ;
 static int           first = 1;
 int                  len, len_fname;
 static char          dirname[97] = {'\0'};
 static char          fname [ 128 ] = {'\0'} ;

 /*----------------------------------------------------------------------*/
 /*  set parameters for search in colorvalue table                       */
 /*----------------------------------------------------------------------*/
 mSetCursor ( M_WATCH ) ;

 rad_data [ map_number ].field_type = display_Height ;

 strcpy(cv_use,"HEIGHT");
 strcpy(rad_data[map_number].cv_use,"HEIGHT");
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
 /*  construct filename                                                  */
 /*----------------------------------------------------------------------*/

 if ( first == 1 )
 {
    len = strlen("rfcwide_height_dir");
    get_apps_defaults("rfcwide_height_dir",&len,dirname,&len);
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
/********************************************* END display_height ************/

