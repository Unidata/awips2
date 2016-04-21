/*=========================================================================*/
/*                         FILE NAME:   display_locspan.c                  */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_locspan                    */
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
/*  FUNCTION NAME:   display_locspan_RFCW ( )                              */
/*       FUNCTION:   A X/Motif wrapper around the display_locspan routine  */
/*                   that allows it to be used as a callback routine from  */
/*                   a Motif widget.                                       */
/***************************************************************************

Function type:
   void

Called by function:
   This routine is registered as a callback to the "Local Span" item
   on the "Fields" drop down menu on hmap_mpe.

************************************** BEGIN display_locspan_RFCW ************/

void display_locspan_RFCW ( Widget w , XtPointer clientdata , 
                            XtPointer calldata )
{
   int * map_number = NULL;

   if ( clientdata == NULL ) return;

   map_number = ( int * ) clientdata;

   display_locspan ( * map_number ) ;
}

/********************************************* END display_locspan_RFCW ******/

/***************************************************************************/
/*  FUNCTION NAME:   display_locspan()                                       */
/*       FUNCTION:   display locspan values                                 */
/***************************************************************************

Function type:
   void

Called by function:
   callback from LocSpan button (under Display)

************************************** BEGIN display_locspan **************/

void display_locspan( int map_number )
{

 extern int           cv_duration ;
 static int           first = 1;
 int                  len, len_fname;
 static char          dirname [ 97 ] ;
 static char          fname [ 128 ] ;

 /*----------------------------------------------------------------------*/
 /*  set parameters for search in colorvalue table                       */
 /*----------------------------------------------------------------------*/
 mSetCursor ( M_WATCH ) ;
 rad_data[ map_number ].field_type = display_Locspan;

 strcpy(cv_use,"LOCSPAN");
 strcpy(rad_data[map_number].cv_use,"LOCSPAN");
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
    len = strlen("rfcwide_locspan_dir");
    get_apps_defaults("rfcwide_locspan_dir",&len,dirname,&len);
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
/********************************************* END display_locspan ************/

