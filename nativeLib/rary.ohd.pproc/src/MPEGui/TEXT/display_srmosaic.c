/*=========================================================================*/
/*                         FILE NAME:   display_srmosaic.c                 */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_srmosaic ,                 */
/*                                      display_srmosaic_RFCW              */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <Xm/Xm.h>

#include "display_field.h"
#include "display_field_data_RFCW.h"
#include "display_precip_data.h"
#include "drawa.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpe_field_names.h"
#include "newhour_RFCW.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"

/***************************************************************************/
/*  FUNCTION NAME:   display_srmosaic_RFCW ( )                             */
/*       FUNCTION:   A X/Motif wrapper around the display_srmosaic ( )     */
/*                   routine.                                              */
/***************************************************************************

Function type:
   void

Called by function:
   This is called from the "Satellite Radar Mosaic" item under the 
   "Precip Fields" drop down menu on the mpe editor application. 

************************************** BEGIN display_srmosaic_RFCW ***********/

void display_srmosaic_RFCW ( Widget w , XtPointer clientdata , 
                             XtPointer calldata )
{
   int * map_number = NULL;

   if ( clientdata == NULL ) return;

   map_number = ( int * ) clientdata;

   display_srmosaic ( * map_number ) ;
}

/***************************************************************************/
/*  FUNCTION NAME:   display_srmosaic()                                    */
/*       FUNCTION:   display srmosaic grid                                 */
/***************************************************************************

Function type:
   void

Called by function:
   callback from Satellite Radar Mosaic button (under Precip Fields)

************************************** BEGIN display_srmosaic **************/

void display_srmosaic ( int map_number )
{

 extern int           cv_duration ;
 static int           first = 1;
 int                  len, len_fname ;
 static char          dirname [ 97 ] = {'\0'};
 char                 fname [ 128 ] = {'\0'};

 /*----------------------------------------------------------------------*/
 /*  set parameters for search in colorvalue table                       */
 /*----------------------------------------------------------------------*/

 mSetCursor ( M_WATCH ) ;
 rad_data [ map_number ].field_type = display_srMosaic;

 strcpy(cv_use,"SRMOSAIC");
 strcpy(rad_data [map_number].cv_use,"SRMOSAIC");
 cv_duration = 3600;
 rad_data [map_number].cv_duration = cv_duration;

 cv_duration_mainwin = cv_duration;
 strcpy(cv_use_mainwin,cv_use);

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
    len = strlen("mpe_srmosaic_dir");
    get_apps_defaults("mpe_srmosaic_dir",&len,dirname,&len);
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

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
/********************************************* END display_srmosaic ************/

