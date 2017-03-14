/*=========================================================================*/
/*                         FILE NAME:   display_gageonly.c                 */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_gageonly,                  */
/*                                      display_gageonly_RFCW              */
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
/*  FUNCTION NAME:   display_gageonly_RFCW                                 */
/*       FUNCTION:   This is the callback routine for the "Gage Only" item */
/*                   under the "Fields" drop down menu on the "hmap_mpe"   */
/*                   application.                                          */
/***************************************************************************

Function type:
   void

Called by function:
   callback from Gage Only button (under Fields)

************************************** BEGIN display_gageonly_RFCW *********/

void display_gageonly_RFCW ( Widget w , XtPointer clientdata ,
                             XtPointer calldata )
{
   int * map_number = NULL;

   if ( clientdata == NULL ) return;

   map_number = ( int * ) clientdata;

   display_gageonly ( * map_number ) ;
}

/********************************************* END display_gageonly_RFCW ***/

/***************************************************************************/
/*  FUNCTION NAME:   display_gageonly                                      */
/*       FUNCTION:   display gageonly grid                                 */
/***************************************************************************

Function type:
   void

Called by function:
   callback from GageOnly button (under Display)

************************************** BEGIN display_gageonly **************/

void display_gageonly( int map_number )
{

 extern int           cv_duration ;
 static  int          first = 1;
 int                  len, len_fname;
 static char          dirname [ 97 ] = {'\0'};
 static char          fname [ 128 ] = {'\0'};

 /*----------------------------------------------------------------------*/
 /*  set parameters for search in colorvalue table                       */
 /*----------------------------------------------------------------------*/
 mSetCursor ( M_WATCH ) ;

 rad_data [ map_number ].field_type = display_gageOnly ;

 strcpy(cv_use,"GAGEONLY");
 strcpy(rad_data[map_number].cv_use,"GAGEONLY");
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
 /*  construct filename                                                  */
 /*----------------------------------------------------------------------*/

 if ( first == 1 )
 {
    len = strlen("rfcwide_gageonly_dir");
    get_apps_defaults("rfcwide_gageonly_dir",&len,dirname,&len);
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
/********************************************* END display_gageonly ************/

