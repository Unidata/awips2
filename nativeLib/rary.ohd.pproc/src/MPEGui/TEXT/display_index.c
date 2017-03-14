/*=========================================================================*/
/*                         FILE NAME:   display_index.c                    */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_index                      */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <Xm/Xm.h>

#include "display_field.h"
#include "display_field_data_RFCW.h"
#include "display_precip_data.h"
#include "drawa.h"
#include "map_library.h"
#include "map_resource.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"

/***************************************************************************/
/*  FUNCTION NAME:   display_index_RFCW ( )                               */
/*       FUNCTION:   A X/Motif wrapper around the "display_index" routine.*/ 
/*                   This allows X/Motif widgets to use this routine      */
/*                   as a callback.                                       */  
/***************************************************************************

Function type:
   void

Called by function:
   This is the callback routine for the "Index Field" menu item
   under the "Fields" menu on the hmap_mpe application.  

************************************** BEGIN display_index **************/

void display_index_RFCW ( Widget w , XtPointer clientdata ,
                          XtPointer calldata )
{
   int * map_number = NULL;
 
   if ( clientdata == NULL ) return;

   map_number = ( int * ) clientdata;

   display_index ( * map_number ) ;
}

/***************************************************************************/
/*  FUNCTION NAME:   display_index()                                       */
/*       FUNCTION:   display index values                                 */
/***************************************************************************

Function type:
   void

Called by function:
   callback from Index button (under Display)

************************************** BEGIN display_index **************/

void display_index( int map_number )
{

 extern int           cv_duration ;
 static int           first = 1;
 int                  len, len_fname;
 static char          dirname [ 97 ] = {'\0'} ;
 static char          fname [ 128 ] = {'\0'} ;

 /*----------------------------------------------------------------------*/
 /*  set parameters for search in colorvalue table                       */
 /*----------------------------------------------------------------------*/
 mSetCursor ( M_WATCH ) ;
 rad_data [ map_number ].field_type = display_Index;

 strcpy(cv_use,"INDEX");
 strcpy(rad_data[map_number].cv_use,"INDEX");
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
    len = strlen("rfcwide_index_dir");
    get_apps_defaults("rfcwide_index_dir",&len,dirname,&len);
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
/********************************************* END display_index ************/

