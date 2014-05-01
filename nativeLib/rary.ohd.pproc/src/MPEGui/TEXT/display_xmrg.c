/*=========================================================================*/
/*                         FILE NAME:   display_xmrg.c                     */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_xmrg,                      */
/*                                      display_xmrg_RFCW                  */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

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
/*  FUNCTION NAME:   display_xmrg_RFCW ( )                                 */
/*       FUNCTION:   A X/Motif wrapper around the "display_xmrg" routine   */
/*                   that allows it to be used as a callback routine.      */ 
/***************************************************************************

Function type:
   void

Called by function:
   This routine is called from the "Best Estimate QPE" item on the
   "Fields" drop down menu.

************************************** BEGIN display_xmrg **************/

void display_xmrg_RFCW ( Widget w , XtPointer clientdata ,
                         XtPointer calldata )
{
   int * map_number = NULL;

   if ( clientdata == NULL ) return;

   map_number = ( int * ) clientdata;

   display_xmrg ( * map_number ) ;

}

/***************************************************************************/
/*  FUNCTION NAME:   display_xmrg()                                       */
/*       FUNCTION:   display xmrg values                                 */
/***************************************************************************

Function type:
   void

Called by function:
   callback from xmrg button (under Display)
   initialize_data_RFCW

************************************** BEGIN display_xmrg **************/

void display_xmrg ( int map_number )
{

 extern int           cv_duration ;
 static int           first = 1;
 int                  len, len_fname, idate;
 static char          dirname [ 97 ] = {'\0'};
 static char          fname [ 128 ] = {'\0'}; 
 static char          cdate [ 11 ];

 /*----------------------------------------------------------------------*/
 /*  set parameters for search in colorvalue table                       */
 /*----------------------------------------------------------------------*/
 rad_data [ map_number ].field_type = display_Xmrg ;

 strcpy(cv_use,"XMRG");
 strcpy(rad_data[map_number].cv_use,"XMRG");
 cv_duration = 3600;
 rad_data[map_number].cv_duration = cv_duration;

 cv_duration_mainwin = cv_duration;
 strcpy(cv_use_mainwin,cv_use);

/*----------------------------------*/
/*  turn on save option             */
/*  turn on draw precip option      */
/*----------------------------------*/

 
 if ( ( first_display == FALSE ) && ( map_number == 0 )
      && ( savemaintop_widget != NULL )  )
 {
    sensitize_save_buttons ( );
    XtSetSensitive(drawpoly_widget, TRUE);
    XtSetSensitive(deletepoly_widget, TRUE);
 }

 /*--------------------------------------------------*/
 /*  construct filename                              */
 /*--------------------------------------------------*/

 if ( first == 1 ) 
 {
    len = strlen("rfcwide_xmrg_dir");
    get_apps_defaults("rfcwide_xmrg_dir",&len,dirname,&len);
    first = 0;
 }

 if(strcmp(date_form,"mdY") == 0)
 {
    idate = date_st3.month*1000000 + date_st3.day*10000 + date_st3.year;
    sprintf(cdate,"%08d%02d",idate,date_st3.hour);
 }
 else
 {
    strcpy(cdate,date_st3.cdate);
 }

 sprintf(fname,"%s/xmrg%sz",dirname,cdate);
 len_fname = strlen(fname);

 /*----------------------------------------------------------------------*/
 /*  read field data from file                                           */
 /*  display field on main window                                        */
 /*----------------------------------------------------------------------*/

 display_field ( fname , len_fname, map_number ) ;
}

/********************************************* END display_xmrg ************/

