/*=========================================================================*/
/*                         FILE NAME:   display_locbias.c                  */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_locbias,                   */
/*                                      display_locbias_RFCW               */
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
/*  FUNCTION NAME:   display_locbias_RFCW()                                */
/*       FUNCTION:   A wrapper around the "display_locbias" routine.       */
/*                   This allows the routine to be easily called from      */
/*                   X/Motif as a callback.                                */ 
/***************************************************************************

Function type:
   void

Called by function:
   Called from the "Local Bias" item on the "Fields" menu on the hmap_mpe
   application menu bar. 

************************************** BEGIN display_locbias_RFCW ************/

void display_locbias_RFCW ( Widget w , XtPointer clientdata ,
                            XtPointer calldata ) 
{
   int * map_number = NULL;

   if ( clientdata == NULL ) return;

   map_number = ( int * ) clientdata;

   display_locbias ( * map_number ) ;
}

/**************************************** END display_locbias_RFCW *********/

/***************************************************************************/
/*  FUNCTION NAME:   display_locbias()                                       */
/*       FUNCTION:   display locbias values                                 */
/***************************************************************************

Function type:
   void

Called by function:
   callback from LocBias button (under Display)

************************************** BEGIN display_locbias **************/

void display_locbias( int map_number )
{

 extern int           cv_duration ;
 static int           first = 1;
 int                  len , len_fname ;
 static char          dirname [ 97 ] = {'\0'};
 static char          fname [ 128 ] = {'\0'};

 /*----------------------------------------------------------------------*/
 /*  set parameters for search in colorvalue table                       */
 /*----------------------------------------------------------------------*/
 mSetCursor ( M_WATCH ) ;

 rad_data [ map_number ].field_type = display_Locbias;

 strcpy(cv_use,"LOCBIAS");
 strcpy(rad_data[map_number].cv_use,"LOCBIAS");
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
   len = strlen("rfcwide_locbias_dir");
   get_apps_defaults("rfcwide_locbias_dir",&len,dirname,&len);
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

/**************************************** END display_locbias *********/
