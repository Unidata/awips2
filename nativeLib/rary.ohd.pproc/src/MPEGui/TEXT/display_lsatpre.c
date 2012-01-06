/*=========================================================================*/
/*                         FILE NAME:   display_lsatpre.c                  */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_lsatpre,                   */
/*                                      display_lsatpre_RFCW               */ 
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
#include "ReadSPE.h"

/***************************************************************************/
/*  FUNCTION NAME:   display_lsatpre_RFCW                                  */
/*       FUNCTION:   A X/Motif callback-friendly wrapper around the        */
/*                   "display_lsatpre" routine.                            */
/***************************************************************************

Function type:
   void

Called by function:
   Called from the "Local Bias Satellite Precip" item under the "Fields"
   menu option in hydroview_mpe.

************************************** BEGIN display_satpre_RFCW ***********/

void display_lsatpre_RFCW ( Widget w , XtPointer clientdata ,
                            XtPointer calldata )
{
   int * map_number = NULL;

   if ( clientdata == NULL ) return;

   map_number = ( int * ) clientdata; 

   display_lsatpre ( * map_number ) ;
}

/********************************************* END display_lsatpre_RFCW *****/

/***************************************************************************/
/*  FUNCTION NAME:   display_lsatpre                                       */
/*       FUNCTION:   display local bias satellite precip grid              */
/***************************************************************************

Function type:
   void

Called by function:
   Satellite Precip button (under Display)

************************************** BEGIN display_lsatpre **************/

void display_lsatpre ( int map_number )
{
 extern int           cv_duration ;
 static int           first = 1;
 int                  ida ;
 int                  ihr ;
 int                  im ;
 int                  imo ;
 int                  is ;
 int                  iyr ;
 int                  len, len_fname;
 int                  tdiff ;
 int                  tunit ;
 static char          dirname [ 97 ] ;
 static char          fname [ 128 ] ;

 /*----------------------------------------------------------------------*/
 /*  set parameters for search in colorvalue table                       */
 /*----------------------------------------------------------------------*/
 mSetCursor ( M_WATCH ) ;
 rad_data [ map_number ].field_type = display_lsatPrecip;

 strcpy(cv_use,"LSATPRE");
 strcpy(rad_data[map_number].cv_use,"LSATPRE");
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
    len = strlen("rfcwide_lsatpre_dir");
    get_apps_defaults("rfcwide_lsatpre_dir",&len,dirname,&len);
    first = 0;
 }

 /* Before the filename of the file containing satellite 
    precipitation data can be created, the current time must be 
    adjusted by subtracting one hour from it. */

 iyr = date_st3.year ;
 imo = date_st3.month ;
 ida = date_st3.day ;
 ihr = date_st3.hour ;
 im = 0 ;
 is = 0 ;
 tdiff = -1 ;
 tunit = 2 ;

 /* TADJ ( & iyr , & imo , & ida , & ihr , & im , & is , & tdiff , 
           & tunit ) ; */
 sprintf(fname,"%s/%s%sz",dirname,cv_use,date_st3.cdate);
 len_fname = strlen(fname);

 /*----------------------------------------------------------------------*/
 /*  read field data from file                                           */
 /*  display field on main window                                        */
 /*----------------------------------------------------------------------*/

 display_field(fname, len_fname, map_number);
 /* Call the version of display_field that can read satellite data here. */

 mSetCursor ( M_NORMAL ) ;
}
/********************************************* END display_lsatpre ************/
