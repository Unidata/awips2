/*=========================================================================*/
/*                         FILE NAME:   display_rfcmmosaic.c               */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_rfcmmosaic ,               */
/*                                      display_rfcmmosaic_RFCW              */
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
/*  FUNCTION NAME:   display_rfcmmosaic_RFCW ( )                           */
/*       FUNCTION:   A X/Motif wrapper around the display_rfcmmosaic ( )   */
/*                   routine.                                              */
/***************************************************************************

Function type:
   void

Called by function:
   This is called from the "RFC Multisensor Mosaic" item under the "PrecipFields"
   drop down menu on the mpe editor application. 

*********************************** BEGIN display_rfcmmosaic_RFCW **********/

void display_rfcmmosaic_RFCW ( Widget w , XtPointer clientdata , 
                               XtPointer calldata )
{
   int * map_number = NULL;

   if ( clientdata == NULL ) return;

   map_number = ( int * ) clientdata;

   display_rfcmmosaic ( * map_number ) ;
}

/***************************************************************************/
/*  FUNCTION NAME:   display_rfcmmosaic()                                  */
/*       FUNCTION:   display rfc multisensor mosaic                        */
/***************************************************************************

Function type:
   void

Called by function:
   callback from RFC Multisensor Mosaic button (under Display)

************************************** BEGIN display_rfcmmosaic **************/

void display_rfcmmosaic ( int map_number )
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
 rad_data [ map_number ].field_type = display_rfcmMosaic;

 strcpy(cv_use,"RFCMMOSAIC");
 strcpy(rad_data [map_number].cv_use,"RFCMMOSAIC");
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
 /*  construct filename                                                  */
 /*----------------------------------------------------------------------*/

 if ( first == 1 )
 {
    len = strlen("mpe_rfcmmosaic_dir");
    get_apps_defaults("mpe_rfcmmosaic_dir",&len,dirname,&len);
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
/********************************************* END display_rfcmmosaic ************/

