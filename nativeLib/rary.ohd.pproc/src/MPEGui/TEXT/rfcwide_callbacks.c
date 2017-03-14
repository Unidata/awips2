/*=========================================================================*/
/*                         FILE NAME:  rfcwide_callbacks.c                 */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   add_mpe_gage_ids                   */
/*                                      add_mpe_gage_values                */
/*					create_save_data_dialog_RFCW       */
/*                                      display_single_gage_RFCW           */
/*                                      do_time_lapse_6_RFCW               */
/*                                      do_time_lapse_12_RFCW              */
/*                                      do_time_lapse_24_RFCW              */
/*                                      do_time_lapse_RFCW                 */
/*                                      drawMpeGageIds                     */
/*                                      drawMpeGageValues                  */
/*                                      init_single_gage_RFCW              */
/*					isThereMpeGageIds                  */
/*                                      isThereMpeGageValues               */
/*                                      next_callback_RFCW                 */
/*                                      set_gage_ids                       */
/*                                      set_gage_values                    */
/*                                      set_mpe_gage_label_color           */
/*                                      show_gage_table_RFCW               */
/*                                      show_gage_ids                      */
/*                                      show_gage_values                   */
/*                                      store_mpe_gage_label_color         */
/*                                      turnOffMpeGageIds                  */
/*                                      turnOffMpeGageValues               */
/*                                      turnOnMpeGageIds                   */
/*                                      turnOnMpeGageValues                */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <Xm/ToggleB.h>
#include <X11/cursorfont.h>
#include <time.h>

#include "clear_data_RFCW.h"
#include "convert_hrap.h"
#include "display_field.h"
#include "display_field_data_RFCW.h"
#include "display_precip_data.h"
#include "drawa.h"
#include "gage_table_RFCW.h"
#include "libXs.h"
#include "map.h"
#include "map_library.h"  /* to show Mpe gage ids and values on the Hydromap */
#include "map_defines.h"  /* For the enum MapOverlays typedef. */
#include "map_resource.h" /* For the mSetCursor routine prototype. */
#include "math.h"
#include "mpe_log_utils.h"
#include "overlay.h"
#include "post_functions.h"
#include "restore_date.h"
#include "read_precip_data.h"
#include "rfcwide.h"
#include "rfcwide_callbacks.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "time_lapse_RFCW.h"
#include "choose_rfcwide_date.h"
#include "Xtools.h"
#include <errno.h>
#define INTSIZE 10

int ok_call_check = 0;
int dates_struct_count = 0;
int restore_on_cancel_dates_struct_count = 0;
char temp_num_of_days[5] = {'1','0','\0'};
char restore_on_cancel_num_of_days[5];

extern int display_7x7_flag ;  /* Flag indicating whether or not there
                                  was chosen MPEfields->Display7x7 menu item. */

static int mpe_gage_ids_flag = 0 ; /* Flag indicating whether or not there
                                  was chosen GageIdentifiers menu item. */
				  
static int mpe_gage_values_flag = 0 ; /* Flag indicating whether or not there
                                     was chosen GageValues menu item. */				  
//Added by Ram; flag to indicate whether or not there
//was choosen GageTriangles menu item
//--------------------
static int mpe_gage_triangles_flag = 0 ;
//-------------------


int gage_ids_status = M_OFF ;
int gage_values_status = M_OFF ;

//Added by Ram
//-----------
static int gage_triangles_status = M_OFF;

/* The variable which keeps track of the coloring scheme being used
   for the MPE gage id and value labels. */
static enum MPEgageColorOptions mpe_gage_label_color = MPEgageColorSolid ; 
static enum MPEgageMissingOptions mpe_gage_missing_option = MPEgageMissingAll ;
			  
/*********************************************************************/
/*  FUNCTION NAME:   create_save_data_dialog_RFCW                    */
/*       FUNCTION:                                                   */
/**********************************************************************

Function type:
   void

Called by function:
   newhour_RFCW
   quit_rfcwide

Functions called:
   ok_quit
   next_callback
   restore_date

delta = info flag
      = ClearData   -- when called by the "Clear Data" option
      = NextHour    -- when called by next hour option
      = OkHourPopup -- when called by OK option of choose_dates popup
      = PrevHour    -- when called by previous hour option
      = Quit        -- when called by Quit  option

The enumerated values for Delta are defined in the ControlMenuItemInfo
enumeration which is defined in the newhour_RFCW.h file.



*********************************************************************/
//Added by Ram
//a helper function written to convert an integer to a string.
//---------------
char *itoa(int value)
{
	int count, i, sign;
	char *ptr,*string,*temp;
	count = 0;
	if ((sign = value) < 0)
	{
		value = -value;
		count++;
	}
	temp = (char *) malloc(INTSIZE + 2);
	if (temp == NULL)
	{
		return(NULL);
	}
	memset(temp,'\0', INTSIZE + 2);
	string = (char *) malloc(INTSIZE + 2);
	if (string == NULL)
	{
		return(NULL);
	}
	memset(string,'\0', INTSIZE + 2);
	ptr = string;
	do{
		*temp++ = value % 10 + '0';
		count++;
	}while (( value /= 10) >0);
	if (sign < 0)
	{
		*temp++ = '-';
	}
	*temp-- = '\0';
	for (i = 0; i < count; i++, temp--, ptr++)
	{
		memcpy(ptr,temp,sizeof(char));
	}

	return(string);
}
//--------------------

void set_values_for_change_and_cancel()
{
	restore_on_cancel_dates_struct_count = dates_struct_count;
	strcpy(restore_on_cancel_num_of_days, temp_num_of_days);
}

Widget create_save_data_dialog_RFCW ( ControlMenuItemInfo value )
{
  int n;
  Widget save_data_dialog;
  Arg wargs[4];
  Widget ok_button, help_button, cancel_button;
  XmString xstr ;
 
  n = 0;
  xstr  = XmStringCreate ( "Data Not Saved - OK to Proceed?" ,
                           XmSTRING_DEFAULT_CHARSET ) ;  
  XtSetArg ( wargs[n] , XmNmessageString , xstr ) ; n++ ;
  save_data_dialog = XmCreateQuestionDialog(toplevel, "save_dialog",
                                            wargs, n);
  XmStringFree ( xstr ) ;

  ok_button = XmMessageBoxGetChild(save_data_dialog, XmDIALOG_OK_BUTTON);

  XtUnmanageChild(XmMessageBoxGetChild(save_data_dialog, XmDIALOG_HELP_BUTTON));
  help_button = XtCreateManagedWidget("Help",
                                      xmPushButtonWidgetClass,
                                      save_data_dialog,NULL,0);
//  XtAddCallback(help_button, XmNactivateCallback,popup_help_window,"SAVEDATA");

  if (value == Quit )
     XtAddCallback(ok_button, XmNactivateCallback, ok_quit, NULL);
  else if ( value == ClearData )
     XtAddCallback ( ok_button , XmNactivateCallback , clear_data_ok_RFCW , NULL ) ;
  else
  {
     XtAddCallback(ok_button, XmNactivateCallback, next_callback_RFCW, ( XtPointer ) value ) ;
     XtAddCallback(ok_button, XmNactivateCallback, set_values_for_change_and_cancel, NULL);
     XtAddCallback(ok_button, XmNactivateCallback, hour_sensitive_RFCW, NULL);
  }
  if ( value == OkHourPopup )
  {
    XtUnmanageChild(XmMessageBoxGetChild(save_data_dialog, XmDIALOG_CANCEL_BUTTON));
    cancel_button = XtCreateManagedWidget("Cancel",xmPushButtonWidgetClass,save_data_dialog,NULL,0);
    
    XtAddCallback(cancel_button, XmNactivateCallback, restore_date, NULL);
    XtAddCallback(cancel_button, XmNactivateCallback, restore_working_date, NULL);
  }

  return(save_data_dialog);
}
/******************************** END create_save_data_dialog_RFCW********/


/*********************************************************************/
/*  FUNCTION NAME:   next_callback_RFCW                              */
/*       FUNCTION:                                                   */
/**********************************************************************

Function type:
   void

Called by function:
   (callback) OK button on save_data_dialog popup

Functions called:
   newhour_RFCW

data = info flag
      = 0  -- when called by Quit option
      = 1  -- when called by next hour option
      = -1 -- when called by previous hour option
      = 99 -- when called by OK option of choose_dates popup

*********************************************************************/


void next_callback_RFCW ( Widget w , XtPointer clientdata , 
                          XtPointer calldata )
{
  DataSaved = TRUE ;
  XtUnmanageChild(w) ;
  newhour_RFCW ( w , clientdata , calldata ) ;
}
/******************************** END next_callback_RFCW*********/


/***************************************************************************/
/*  FUNCTION NAME:   do_time_lapse_6_RFCW()                                */
/*       FUNCTION:   callback that calls function to display a 6 Hour      */
/*                    time lapse                                           */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) time lapse (6 hr) button

Functions called:
   time_lapse_RFCW

******************************************** BEGIN do_time_lapse_6_RFCW *********/

void do_time_lapse_6_RFCW ( Widget w , XtPointer clientdata , 
                            XtPointer calldata )
{
   time_lapse_struct * data = ( time_lapse_struct * ) clientdata ;
   data->nhrs = 6 ;
   time_lapse_RFCW ( w , data , NULL ) ;
}

/********************************************* END do_time_lapse_6_RFCW *********/


/***************************************************************************/
/*  FUNCTION NAME:   do_time_lapse_12_RFCW()                                    */
/*       FUNCTION:   callback that calls function to display a 12 Hour     */
/*                    time lapse                                           */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) time lapse (12 hr) button

Functions called:
   time_lapse_RFCW

************************************** BEGIN do_time_lapse_12_RFCW *********/

void do_time_lapse_12_RFCW ( Widget w , XtPointer clientdata , 
                             XtPointer calldata )
{
   time_lapse_struct * data = ( time_lapse_struct * ) clientdata ;
   data->nhrs = 12;
   time_lapse_RFCW(w, data, NULL);
}

/****************************************** END do_time_lapse_12_RFCW *********/


/***************************************************************************/
/*  FUNCTION NAME:   do_time_lapse_24_RFCW()                                    */
/*       FUNCTION:   callback that calls function to display a 24 Hour     */
/*                    time lapse                                           */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) time lapse (24 hr) button

Functions called:
   time_lapse_RFCW

************************************** BEGIN do_time_lapse_24_RFCW *********/

void do_time_lapse_24_RFCW ( Widget w , XtPointer clientdata , 
                             XtPointer calldata )
{
   time_lapse_struct * data = ( time_lapse_struct * ) clientdata ;
   data->nhrs = 24 ;
   time_lapse_RFCW ( w , data , NULL ) ;
}

/**************************************** END do_time_lapse_24_RFCW *********/

/***************************************************************************/
/*  FUNCTION NAME:   do_time_lapse_RFCW()                                  */
/*       FUNCTION:   callback that pops up widget with time lapse shell    */
/*                    to select time loop duration                         */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) time lapse (other) button

***************************************** BEGIN do_time_lapse_RFCW ***********/

void do_time_lapse_RFCW ( Widget w , XtPointer clientdata , 
                          XtPointer calldata )
{
   XtPopup ( widget_struct->time_lapse_shell , XtGrabNone ) ;
}

/****************************************** END do_time_lapse_RFCW ***********/

void init_single_gage_RFCW ( Widget w, XtPointer clientdata , 
				XtPointer calldata ) 
{
  Display * dpy = NULL ;
  extern int add_pseudo_flag;
  extern int draw_poly_flag ;
  
  draw_struct * data = (draw_struct *) clientdata ; 

  /* If the draw polygon flag is on, turn it off. */
  draw_poly_flag = 0;
  add_pseudo_flag = 0;
  
  if ( display_7x7_flag == 1 )
  {
     return ;
  }

  dpy = XtDisplay(data->w);
 
   
   /*--------------------------------------------------------------*/
   /*   Display watch cursor and flush the display buffer.         */
   /*--------------------------------------------------------------*/
   mSetCursor ( M_WATCH ) ;
   XFlush(XtDisplay(rad_data[0].w));

   /*----------------------------------------------------------------*/
   /*  define display_7x7_flag                                       */
   /*  = 0 -- mouse click on Hydromap (Zoom option)                  */
   /*  = 1 -- Display 7x7 menu item  chosen from MPEfields menu      */
   /*----------------------------------------------------------------*/
   
  display_7x7_flag = 1;

  if ( gage == NULL )
  {
     ReadGageData_RFCW ( ) ;
  }

  /*if (pscursor == (Cursor) NULL)
  {
     pscursor = XCreateFontCursor(dpy,XC_hand2);
  }*/

  mDisableExposeWatch ( ) ;
  mSetCursor ( M_SELECT ) ;
  /* XDefineCursor(dpy, XtWindow(XtParent(data->w)), pscursor); */
}


/*********************************************************************/
/*  FUNCTION NAME:   display_single_gage_RFCW                        */
/*       FUNCTION:   callback to locate gage nearest to cursor when  */
/*                     left mouse button press occurs in zoom display*/
/*                   if no gages are available, then popup window    */
/*                     with message                                  */
/**********************************************************************

Function type:
   void

Called by function:
   (callback) left mouse button press on zoom canvas

Functions called:
   display_gage_RFCW
   display_no_gage

Local variables:
   xloc - integer;  location of selected point in pixel units
   yloc - integer;
   n - integer; index (incrementor) for wargs array
   xpix - integer; number of x-pixels per hrap unit
   ypix - integer; number of y-pixels per hrap unit
   i - integer; counter
   imin - integer; pointer to nearest gage
   width - Dimension structure; width of drawing area canvas
   height - Dimension structure; height of drawing area canvas
   hrap - HRAP structure; hrap location of selected point
   xdist - double (real); x-distance from gage to selected point (hrap)
   ydist - double (real); y-distance from gage to selected point (hrap)
   dist - double (real);  distance from gage to selected point
   mindist - double (real); distance from nearest gage to selected point
   wargs - stack deref (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned to 5

 History:

 January 21, 2006   Bryon L. - Removed limitation that gages must be in the
                               MPE forecast area.  This was done for P3.

******************************************** BEGIN display_single_gage_RFCW *****/

void display_single_gage_RFCW ( Widget w , XtPointer clientdata , 
                                XEvent * event , 
                                Boolean * continue_to_dispatch_return )
{
   
   int          i, imin=0;
   HRAP         hrap;
   double       xdist, ydist, dist, mindist;
   float        lat, lon ;
   
   draw_struct * data = ( draw_struct * ) clientdata ; 
   clicks * mouse_event = (clicks * ) event ;

 /*--------------------------------------------------------------*/
 /*     if no gages are available display message with that info */
 /*--------------------------------------------------------------*/

  if ( ngages == 0 )
  {
       display_no_gage ( );
       mEnableExposeWatch ( ) ;
       mSetCursor ( M_NORMAL ) ;
       display_7x7_flag = 0 ;
       return;
  }

 /*-------------------------------------------------------------------------*/
 /*    get cursor location in screen coordinates and convert                */
 /*    screen coordinates to HRAP coordinates                               */
 /*-------------------------------------------------------------------------*/

  mConvertXY2LatLon ( mouse_event -> x , mouse_event -> y, &lat , &lon ) ;
  hrap = LatLongToHrapMpe (lat , (-1) * lon ) ;

 /*-------------------------------------------------------------------------*/
 /*     loop through all gages to locate gage nearest to cursor location    */
 /*      that is within the rectangle defined in coord_XXXXX.dat            */
 /*-------------------------------------------------------------------------*/

  mindist = 999.;
  for (i=0;i<ngages;i++)
  {
    xdist = hrap.x - gage[i].hrap.x;
    ydist = hrap.y - gage[i].hrap.y;
    dist = sqrt(xdist*xdist + ydist*ydist);
    if (dist < mindist )
    {
       imin = i;
       mindist = dist;
    }
  }

 /*-------------------------------------------------------------------------*/
 /*     if nearest gage is less than threshold distance display popup       */
 /*     with single gage display                                            */
 /*-------------------------------------------------------------------------*/

  if ( mindist < 900. ) 
  {
     display_gage_RFCW(imin,data);
  }

}

/********************************************* END display_single_gage_RFCW *****/

/***************************************************************************/
/*  FUNCTION NAME:   show_gage_table_RFCW()                                */
/*       FUNCTION:   callback that calls function to create the gage       */
/*                    table display                                        */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) show gage table buttons

Functions called:
   create_gage_table

Local variables:
   w - Widget structure;  Gage table button

******************************************** BEGIN show_gage_table_RFCW  *********/

void show_gage_table_RFCW ( Widget w , XtPointer clientdata ,
                            XtPointer calldata )
{
  draw_struct * data = ( draw_struct * ) clientdata ;
  
  /*--------------------------------------------------------------*/
  /*   Display watch cursor and flush the display buffer.         */
  /*--------------------------------------------------------------*/
   mSetCursor ( M_WATCH ) ;
   XFlush(XtDisplay(rad_data[0].w));

  /* Check to determine if the gage data has already been read.  If not
     then read it. */
  if ( gage == NULL )
  {
     ReadGageData_RFCW ( ) ;
  }  

  create_gage_table_RFCW ( data ) ;
  mSetCursor ( M_NORMAL ) ;
}

/********************************************* END show_gage_table_RFCW *********/


/***************************************************************************/
/*  FUNCTION NAME:   show_gage_ids()                                       */
/*       FUNCTION:   callback to display gage identifiers on zoom display  */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) show precip gages button

Functions called:
   get_pixel_by_name

Local variables:
   n - integer; index (incrementor) for wargs array
   xloc - integer; x location of gage in pixel units
   yloc - integer; y location of gage in pixel units
   width - Dimension structure; width of drawing area canvas
   height - Dimension structure; height of drawing area canvas
   wargs - stack deref (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned to 5
   x, y - integer; number of pixels per hrap unit

******************************************** BEGIN show_gage_ids **************/

void show_gage_ids( Widget w , XtPointer clientdata , XtPointer calldata )
{
   int          i, n, xloc, yloc;
   GC           gc;
   Display     *dpy;
   Dimension    width, height;
   Arg          wargs[5];
   int          x, y;
   int          mask = GCForeground;
   XGCValues    gcv;
   
 draw_struct * data = ( draw_struct * ) clientdata ; 
 
 dpy = XtDisplay(data->w);
 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(data->w, wargs, n);

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;
 if (x > y)
	x = y;
 else if (y > x)
	y = x;

 gcv.foreground = get_pixel_by_name ( data->w , "SandyBrown" ) ;
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 for(i=0;i<ngages;i++)
    {
       xloc = ( int ) ( ( gage[i].hrap.x - ( float ) data->origin.x ) * 
		        ( float ) x ) ;
       yloc = ( int ) ( ( ( float ) data->maximum_rows -
                        ( gage[i].hrap.y - ( float ) data->origin.y)) * 
		        ( float ) y ) ;
       XDrawString(XtDisplay(data->w),data->pixbase,gc,
		  xloc,yloc,gage[i].id,strlen(gage[i].id));
    }

 if (XtIsRealized(data->w))
    XClearArea(dpy,XtWindow(data->w),0,0,0,0,TRUE);
}

/****************************************** END show_gage_ids **************/

/***************************************************************************/
/*  FUNCTION NAME:   show_gage_values()                                    */
/*       FUNCTION:   callback to display gage values on zoom display       */
/*                   time distributed values have "TD" appended to         */
/*                     the value                                           */
/*                   edited values have "E" appended to the value          */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) show gage values button

Functions called:
   get_pixel_by_name

Local variables:
   n - integer; index (incrementor) for wargs array
   xloc - integer; x location of gage in pixel units
   yloc - integer; y location of gage in pixel units
   width - Dimension structure; width of drawing area canvas
   height - Dimension structure; height of drawing area canvas
   wargs - stack deref (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned to 5
   x, y - integer; number of pixels per hrap unit

***************************************** BEGIN show_gage_values **************/

void show_gage_values ( Widget w , draw_struct * data , caddr_t * call_data )
{
   char         cgval [ 9 ] ;
   int          i, n, xloc, yloc;
   GC           gc;
   Display     *dpy;
   Dimension    width, height;
   Arg          wargs[5];
   int          x, y;
   int          mask = GCForeground;
   XGCValues    gcv;

 dpy = XtDisplay(data->w);
 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(data->w, wargs, n);

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;
 if (x > y)
	x = y;
 else if (y > x)
	y = x;

 gcv.foreground = get_pixel_by_name(data->w,"SandyBrown");
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 for(i=0;i<ngages;i++)
    {
       xloc = ( int ) ( ( gage[i].hrap.x - ( float ) data->origin.x) *
                         ( float ) x ) ;
       yloc = ( int ) ( ( ( float ) data->maximum_rows -
                        ( gage[i].hrap.y - ( float ) data->origin.y ) ) *
                          ( float ) y ) ;

       if(gage[i].manedit == 0)
         sprintf(cgval,"%.2f",gage[i].gval);
       else
         sprintf(cgval,"%.2fE",gage[i].gval);

       XDrawString(XtDisplay(data->w),data->pixbase,gc,
		  xloc,yloc,cgval,strlen(cgval));
    }
 

 if (XtIsRealized(data->w))
    XClearArea(dpy,XtWindow(data->w),0,0,0,0,TRUE);



}

/********************************************* END show_gage_values **************/

/***************************************************************************/
/*  FUNCTION NAME:   add_mpe_gage_ids()                                    */
/*       FUNCTION:   This function adds Mpe gage identifiers to the screen */
/*                   in hmap/mpe application                               */	
/***************************************************************************

Function type:
   void

Called by function:
   drawMpeGageIds
Functions called:
   mSetColor
   HrapToLatLongMpe
   mConvertLatLon2XY
******************************** BEGIN add_mpe_gage_ids *********************/

void add_mpe_gage_ids ( int map_index ) 
{
   double lat ;
   double lon ;

   int         	i , x , y ;
   int is_missing ;
   int reported_missing ;
   
   char idLabel[BUFSIZ] ;

   for ( i = 0 ; i < ngages ; i ++ )
   {
        if ( ( ( rad_data [ map_index ].field_type != display_p3Mosaic ) && 
               ( rad_data [ map_index ].field_type != display_avgrMosaic )) || 
	     ( gage [ i ].use_in_p3 == 1 ) )
        {
           if ( gage[ i ].gval == -999. )
           {
              is_missing = 1 ;
           }
           else
           {
               is_missing = 0 ;
           }

           if ( gage [ i ].reported_missing == 1 )
           {
              reported_missing = 1 ;
           }
           else
           {
              reported_missing = 0 ;
           }

           /* Retrieve the gage display missing mode. */
           switch ( mpe_gage_missing_option )
           {
              case MPEgageMissingNone :

                 if ( is_missing == 1 ) continue ;
                 break ;

              case MPEgageMissingReported :

                 if ( ( is_missing == 1 ) && ( reported_missing == 0 ) ) continue;
                 break ;

              case MPEgageMissingAll :

                 break ;

              default :

                 break ;
           }

           /* This function has been "inlined" to reduce the overhead
              of the function call. */
           set_mpe_gage_label_color ( & gage [ i ], map_index ) ; 

           HrapToLatLongByReference ( gage [ i ].hrap.y ,
                           gage [ i ].hrap.x ,
                           & lat , & lon ) ;
		
	   mConvertLatLon2XY ( ( float ) lat , 
                               ( float ) -1.0 * lon , 
                               & x , 
                               & y ) ;
	
  	   sprintf ( idLabel, "%s%s", ".", gage[i].id ) ;
	
      	   mDrawText ( M_EXPOSE , map_index , x , y , idLabel) ;	
        }
   }

   /* Undo any special settings that were needed for coloring
      the gage labels and values. */
   unset_mpe_gage_label_color ( ) ;
}

/******************************** END add_mpe_gage_ids ***********************/

/******************************************************************************/
/*  FUNCTION NAME:   isThereMpeGageIds()                                      */
/*                                                                            */
/*       FUNCTION:   This function provides the user access to the flag       */
/*                   which indicates whether or not Gage->GageIdentificators  */
/*                   menu item was choosen.  A value of '0' means that is not.*/
/*                   A value of '1' means that was choosen. 		      */		      	           
/******************************************************************************

Function type:
   int

Called by function:

Functions called:
 

******************************** BEGIN isThereMpeGageIds *********************/

int isThereMpeGageIds ( )
{
  return mpe_gage_ids_flag ;
}

/******************************** END isThereMpeGageIds **********************/

/*****************************************************************************/
/*  FUNCTION NAME:   turnOffMpeGageIds()                                        */
/*       FUNCTION:   This function sets the mpe_gage_ids_flag to '0' indicating */ 
/*                   that there is no GageIdentifiers to draw.	                */	      	         
/******************************************************************************

Function type:
   void

Called by function:
   set_mpe_gage_ids
Functions called:
   none
******************************** BEGIN turnOffMpeGageIds ********************/

void turnOffMpeGageIds ( )
{
   mpe_gage_ids_flag = 0 ;
}

/******************************** END turnOffMpeGageIds **********************/

/********************************************************************************/
/*  FUNCTION NAME:   turnOnMpeGageIds()                                         */
/*       FUNCTION:   This function sets the mpe_gage_ids_flag to '1' indicating */ 
/*                   that there was choosen GageIdentifiers menu item.          */	      	           
/*****************************************************************************

Function type:
   void

Called by function:
   set_mpe_gage_ids
Functions called:
   none
******************************** BEGIN turnOnMpeGageIds **********************/

void turnOnMpeGageIds ( )
{
   mpe_gage_ids_flag = 1 ;
}

/******************************** END turnOnMpeGageIds **********************/

/***************************************************************************/
/*  FUNCTION NAME:   drawMpeGageIds()                                      */
/*       FUNCTION:   This routine redraws map with Mpe gage identificators */ 
/*                   when GageIdentifiers menu item was choosen.           */   	           
/***************************************************************************

Function type:
   void

Called by function:
   redrawBase
Functions called:
   add_mpe_gage_ids
******************************** BEGIN drawMpeGageIds ********************/

void drawMpeGageIds ( int map_index )
{

   if ( mpe_gage_ids_flag != 0 )
   {
      add_mpe_gage_ids ( map_index ) ;
   }
}

/******************************** END drawMpeGageIds *********************/

/*****************************************************************************
 *
 * Routine: set_gage_ids ()
 *
 * Description: this routine is callback for gage_ids toggle buttons.  It sets
 * the status of the gage identifiers and updates the maps.
 *
 ****************************************************************************/

void set_mpe_gage_ids ( Widget w , XtPointer clientdata , XtPointer calldata)
{
  int status;

  if ( gage_ids_status == M_OFF )
  {
  	gage_ids_status = M_ON ;
	turnOnMpeGageIds ( ) ;

        /* Read in the gage data only if it has not already been done so
           for the gage values. */
        if ( gage == NULL ) 
        {
           ReadGageData_RFCW ( ) ;
        }
  }
  else
  {
  	gage_ids_status = M_OFF ;
	turnOffMpeGageIds ( );	
  }  

  mUpdateMap (0);

  /* Check to see if the second map is active.  If so, then
     draw gages on it, too. */
  status = is_screen_split ( );

  if ( status == 1 )
  {
     mUpdateMap (1);
  }
}

/***************************************************************************/
/*  FUNCTION NAME:   add_mpe_gage_values()                                 */
/*       FUNCTION:   This function adds Mpe gage values to the Hydromap,   */
/*                   in hmap/mpe application,                              */
/*                   edited values have "E" appended to the value          */
/***************************************************************************

Function type:
   void

Called by function:
   drawMpeGageIds
Functions called:
   mSetColor
   HrapToLatLongMpe
   mConvertLatLon2XY
******************************************** BEGIN add_mpe_gage_values **************/

void add_mpe_gage_values ( int map_index )
{
   char cgval [ MAX_GAGE_STRING_SIZE + 1 ] ;
   char gage_val_str [ GAGE_VAL_STR_SIZE + 1 ] ;
   double       lat ;
   double       lon ;
   int         	i , x , y ;
   int is_missing ;
   int reported_missing ;

   for ( i=0 ; i < ngages ; i++ )
   {
        if ( ( ( rad_data [ map_index ].field_type != display_p3Mosaic ) && 
               ( rad_data [ map_index ].field_type != display_avgrMosaic )) || 
	     ( gage [ i ].use_in_p3 == 1 ) )
        {
           if ( gage [ i ].gval == -999. )
           {
              is_missing = 1 ;
           }
           else
           {
               is_missing = 0 ;
           }

           if ( gage [ i ].reported_missing == 1 )
           {
              reported_missing = 1 ;
           }
           else
           {
              reported_missing = 0 ;
           }

           /* Retrieve the gage display missing mode. */
           switch ( mpe_gage_missing_option )
           {
              case MPEgageMissingNone :

                 if ( is_missing == 1 ) continue ;
                 break ;

              case MPEgageMissingReported :

                 if ( ( is_missing == 1 ) && ( reported_missing == 0 ) ) 
                 {
                    continue;
                 }

                 break ;

              case MPEgageMissingAll :

                 break ;

              default :

                 break ;
           }

           /* This function has been "inlined" to reduce the overhead of
              calling multiple times. */
           set_mpe_gage_label_color ( & gage [ i ], map_index ) ; 

           memset ( cgval , '\0' , MAX_GAGE_STRING_SIZE + 1 ) ;
           HrapToLatLongByReference ( gage [ i ].hrap.y , 
                           gage [ i ].hrap.x , 
                           & lat , 
                           & lon ) ;
		
           mConvertLatLon2XY ( ( float ) lat , 
                               ( float ) -1.0 * lon , 
                               & x , 
                               & y ) ;
     
           if ( is_missing == 1 ) 
           {
              strcpy ( gage_val_str , "m" ) ;
           }
           else
           {
              sprintf ( gage_val_str , "%.2f", gage [ i ].gval ) ;
           }

           if ( gage [ i ].manedit == 0 && gage [ i ].td == 0 )
           {
              strcpy ( cgval , gage_val_str ) ; 
           }
           else if ( gage[i].manedit == 1 && gage[i].td == 1)
           {
              sprintf ( cgval , "%sde" , gage_val_str ) ;
           }
           else if ( gage[i].manedit == 1 )
           {
              sprintf ( cgval , "%se" , gage_val_str ) ;
           }
           else /* gage [ i ] . td must == 1 and gage [ i ] . manedit == 0 */
           {
              sprintf ( cgval , "%sd" , gage_val_str ) ;
           }

           /* Conditional logic to draw missing values will go here. */
           mDrawText ( M_EXPOSE , map_index, x + X_VALUES_OFFSET, 
                       y + Y_VALUES_OFFSET, cgval ) ;
        }
   }

   unset_mpe_gage_label_color ( ) ;
}


//Added by Ram
//------------
void add_mpe_gage_triangles ( )
{
	FILE * fd = NULL;
	long int j = 0;
	char pdir[150], filename[150], mystr[150];
	int len = strlen("rfcwide_gagetriangles_dir");
	char gage_id[9];
	long int gage_n_contig = 0;
	double lat_1 = 0.0,lon_1 = 0.0,lat_2 = 0.0,lon_2 = 0.0;
	int lat_11 = 0,lon_11 = 0,lat_22 = 0,lon_22 = 0;
	len = get_apps_defaults("rfcwide_gagetriangles_dir",&len,pdir,&len);
	bzero(mystr,150);
	strcat(mystr,itoa(date_st3.year));
	if(date_st3.month < 10)
	{
		strcat(mystr,"0");	
	}
	strcat(mystr,itoa(date_st3.month));
	if(date_st3.day < 10)
	{
		strcat(mystr,"0");
	}
	strcat(mystr,itoa(date_st3.day));
	if(date_st3.hour < 10)
	{
		strcat(mystr,"0");
	}
	strcat(mystr,itoa(date_st3.hour));
	sprintf(filename, "%s%s%sz",pdir,"/GAGETRIANGLES",mystr);
	logMessage("trying to read gage triangles in %s\n",filename);
	if(mpe_gage_triangles_flag == 0)
	{
		return;
	}
	else
	{
		fd = fopen(filename,"r");
		if(fd != NULL)
		{
			mSetColor ( "yellow" ) ;
			while(!feof(fd))
			{
				fread(&gage_id,(9*sizeof(char)),1,fd);
				fread(&gage_n_contig,sizeof(long),1,fd);
				fread(&lat_1,sizeof(double),1,fd);
				fread(&lon_1,sizeof(double),1,fd);
				if(lon_1 > 0)
				{
					lon_1 *= -1;
				}
				if(lat_1 < 0)
				{
					lat_1 *= -1;
				}
				mConvertLatLon2XY((float)lat_1,(float)lon_1,&lat_11,&lon_11);
				for (j = 0; j < gage_n_contig; j++)
				{
					fread(&lat_2,sizeof(double),1,fd);
					fread(&lon_2,sizeof(double),1,fd);
					if(lon_2 > 0)
					{
						lon_2 *= -1;
					}
					if(lat_2 < 0)
					{
						lat_2 *= -1;
					}
					mConvertLatLon2XY((float)lat_2,(float)lon_2,&lat_22,&lon_22);	
					mDrawLine(M_EXPOSE,0,lat_11,lon_11,lat_22,lon_22);
				}
			}
			fclose(fd);
		}
		else
		{
			logMessage("error opening file %s,so not displaying gage triangles \n",filename);
			return;
		}
	}	
}
//-----------


/********************************************* END add_mpe_gage_values **************/

/******************************************************************************/
/*  FUNCTION NAME:   isThereMpeGageValues()                                   */
/*                                                                            */
/*       FUNCTION:   This function provides the user access to the flag       */
/*                   which indicates whether or not Gage->GageValues          */
/*                   menu item was choosen.  A value of '0' means that is not.*/
/*                   A value of '1' means that was choosen. 		      */		      	           
/******************************************************************************

Function type:
   int

Called by function:

Functions called:
 

******************************** BEGIN isThereMpeGageValues ***********************/

int isThereMpeGageValues ( )
{
  return mpe_gage_values_flag ;
}


//Added by Ram
//-------------
int isThereMpeGageTriangles ( )
{
  return mpe_gage_triangles_flag ;
}
//------------

/******************************** END isThereMpeGageValues *************************/

/*******************************************************************************/
/*  FUNCTION NAME:   turnOffMpeGageValues()                                    */
/*       FUNCTION:   This function sets mpe_gage_values_flag to '0' indicating */ 
/*                   that there is no GageValues to draw.	               */	      	         
/*******************************************************************************

Function type:
   void

Called by function:
   set_mpe_gage_values
Functions called:
   none
******************************** BEGIN turnOffMpeGageValues *******************/

void turnOffMpeGageValues ( )
{
   mpe_gage_values_flag = 0 ;
}

//Added by Ram for the gage triangles
//-----------
void turnOffMpeGageTriangles ( )
{
   mpe_gage_triangles_flag = 0 ;
}
//----------

/******************************** END turnOffMpeGageValues ********************/

/*****************************************************************************/
/*  FUNCTION NAME:   turnOnMpeGageValues()                                   */
/*       FUNCTION:   This function sets the mpe_gage_values_flag to '1'      */ 
/*                   indicating that there was choosen GageIdentifiers       */ 
/*                   menu item.                                              */	      	           
/*****************************************************************************

Function type:
   void

Called by function:
   set_mpe_gage_values
Functions called:
   none
******************************** BEGIN turnOnMpeGageValues ***********/

void turnOnMpeGageValues ( )
{
   mpe_gage_values_flag = 1 ;
}

//Added by Ram for gage triangles
//------------------
void turnOnMpeGageTriangles ( )
{
   mpe_gage_triangles_flag = 1 ;
}
//-----------------

/******************************** END turnOnMpeGageValues **********************/

/***************************************************************************/
/*  FUNCTION NAME:   drawMpeGageValues()                                   */
/*       FUNCTION:   This routine redraws map with Mpe gage values         */ 
/*                   when GageValues menu item was choosen.                */  
/***************************************************************************

Function type:
   void

Called by function:
   redrawBase
Functions called:
   add_mpe_gage_values
******************************** BEGIN drawMpeGageValues ********************/

void drawMpeGageValues ( int map_index )
{

   if ( mpe_gage_values_flag != 0 )
   {
      add_mpe_gage_values ( map_index ) ;
   }
}


//Added by Ram
//------------
void drawMpeGageTriangles ( )
{

   if ( mpe_gage_triangles_flag != 0 )
   {
      add_mpe_gage_triangles ( ) ;
   }
}
//-----------


/******************************** END drawMpeGageValues *********************/

/*****************************************************************************
 *
 * Routine: set_gage_values ()
 *
 * Description: this routine is callback for gage_values toggle buttons.  
 * It sets the status of the gage values and updates the maps.
 *
 ****************************************************************************/

void set_mpe_gage_values ( Widget w , XtPointer clientdata , XtPointer calldata)
{ 
  int status;

  if ( gage_values_status == M_OFF )
  {
  	gage_values_status = M_ON ;
	turnOnMpeGageValues ( );

        /* Read in the gage data only if it has not already been read in
           for the gage values. */
        if ( gage == NULL )
        {
           ReadGageData_RFCW ( ) ;
        }

  }
  else
  {
  	gage_values_status = M_OFF ;
	turnOffMpeGageValues ( );	
  }  

  mUpdateMap ( 0 ) ;

  status = is_screen_split ( );

  if ( status == 1 )
  {
    /* Split screen mode is "ON". Draw the gage values on the second screen. */
    mUpdateMap ( 1 );
  }
}

void set_mpe_gage_triangles ( Widget w, XtPointer clientdata,
                              XtPointer calldata )
{
        int status ;
	if ( gage_triangles_status == M_OFF )
	{
		gage_triangles_status = M_ON ;
		turnOnMpeGageTriangles ( );
	}
	else
	{
		gage_triangles_status = M_OFF ;
		turnOffMpeGageTriangles ( );
	}
	
	mUpdateMap ( 0 ) ;

        status = is_screen_split ( );

        if ( status == 1 )
        {
          /* Split screen mode is "ON". Draw the gage values on the second screen. */
          mUpdateMap ( 1 );
        }
}

/*****************************************************************************
 *
 * Routine: store_mpe_gage_label_color ()
 *
 * Description:  This routine stores the coloring scheme to be used 
 *               when drawing gage ids and value labels.
 *
 *               There are currently four different options for the colors
 *               of the gage id and value labels:
 *
 *               Solid Color - The labels are given a default sandy brown
 *                             color.
 *               Contrast Color - The labels are automatically assigned colors
 *                                which ensure that they constrast with
 *                                their background.
 *               Color By QC    - Colors are assigned to the gage value and id
 *                                labels according to their QC value.
 *               Color By Value - Colors are assigned to the gage id and 
 *                                value labels according to their precipitation
 *                                amount. 
 ****************************************************************************/

void store_mpe_gage_label_color ( enum MPEgageColorOptions gage_color_option )
{

   switch ( gage_color_option )
   {
      case MPEgageColorSolid :
      case MPEgageColorContrast :
      case MPEgageColorQC :
      case MPEgageColorByValue :

         mpe_gage_label_color = gage_color_option ;
         break ;

      default :

         flogMessage ( stderr , "\nIn routine 'store_mpe_gage_label_color':\n"
                            "Reached the default case of the switch\n"
                            "statement.  The value of gage_color_option is\n"
                            "%d.\n" , gage_color_option ) ; 
         break ;
   }
}

/*****************************************************************************
 *
 * Routine: store_mpe_gage_missing ()
 *
 * Description:  This routine stores information about whether to display
 *               all missing gage values, reported missing gage values,
 *               or no missing gage values.
 *
 *               There are currently three different options for displaying
 *               missing gage data:
 *
 *                All Missing - All missing gage data.  This includes gage data
 *                              which is missing because the station did not
 *                              report and data which is missing because the
 *                              station reported a missing value.
 *               Reported Missing - Only display missing gage value if actually
 *                                  reported. 
 *               No Missing Values - Do not display any missing values.
 ****************************************************************************/

void store_mpe_gage_missing ( enum MPEgageMissingOptions gage_missing_option )
{

   switch ( gage_missing_option )
   {
      case MPEgageMissingAll :
      case MPEgageMissingReported :
      case MPEgageMissingNone :

         mpe_gage_missing_option = gage_missing_option ;

         break ;

      default :

         flogMessage ( stderr , "\nIn routine 'store_mpe_gage_missing':\n"
                            "Reached the default case of the switch\n"
                            "statement.  The value of gage_missing_option is\n"
                            "%d.\n" , gage_missing_option ) ; 
         break ;
   }

   return ;
}

/*****************************************************************************
 *
 * Routine: set_mpe_gage_label_color ()
 *
 * Description:  This routine sets the displayed color of the gage id
 *               and value labels.
 *
 ****************************************************************************/

inline void set_mpe_gage_label_color ( const gage_struct * pGage,
                                       int map_index )
{
   enum DisplayFieldData display_field_type ;
   draw_struct * data = NULL ;
   const static float mfactor = 2540. ;
   int vip ;

   display_field_type = rad_data [ map_index ].field_type;

   if ( pGage == NULL )
   {
      return ;
   }

   data = getDrawStruct ( ) ;

   if ( data == NULL )
   {
      return ;
   }

   switch ( mpe_gage_label_color ) 
   {
      case MPEgageColorSolid :

         /* This applies to all gages. */
         mSetColor ( "SandyBrown" ) ;
         break ;

      case MPEgageColorContrast :

         /* This applies to all gages. */
         mSetGC ( M_XOR_GC , "SandyBrown" ) ;
         break ;

      case MPEgageColorQC :

         /* Call the QC gage coloring routine here.  This needs
            to be done on a gage by gage basis. */

         if ( pGage->qc == 3 )
         {
            /* The value passed both SCC_QC and MSC_QC checks. */
            mSetColor ( "SandyBrown" ) ;
         }
         else if ( pGage->qc == 1 )
         {
            /* The gage value does not pass Spatial Consistency Check */
            mSetColor ( "Red" ) ;
         }
         else
         {
            /* The gage value does not pass Multi Sensor Check */
            mSetColor ( "Yellow" ) ;
         }

         break ;

      case MPEgageColorByValue :

         /* Colors the gage labels according to the actual precipitation 
            value. This needs to be done on a gage by gage basis. */
         /* Only retrieve the color information when necessary. */
         /* Determine what type of MPE field is currently being 
            displayed. */
         if ( ( data == NULL ) ||
              ( data->levels == NULL ) )
         {
             /* Can't determine the color to associate with the gage value. */
             return ;
         }

         switch ( display_field_type )
         {
            /* All of these use a color scale which represent
               levels of precipitation. */
            case display_rMosaic :
            case display_avgrMosaic :
            case display_maxrMosaic :
            case display_bMosaic :
            case display_lMosaic :
            case display_mMosaic :
            case display_rfcMosaic :
            case display_mlMosaic :
            case display_p3Mosaic :
            case display_Xmrg :
            case display_multiHour :
            case display_satPrecip :
            case display_lsatPrecip :
            case display_sgMosaic :
            case display_srMosaic :
            case display_srgMosaic :
            case display_rfcbMosaic :
            case display_rfcmMosaic :
            case display_gageOnly :

               /* These all have regular precipitation legends. */
               vip = get_vip_level ( data->num_levels ,
                                     data->levels ,
                                     (int ) ( pGage->gval * mfactor ) ) ;
               mSetColor ( color_list_levels [ vip ] ) ; 
               break ;

            case display_subValue :
            case display_Height :
            case display_Index :
            case display_Locspan :
            case display_Locbias :
            case display_Prism :
            case display_maxtempPrism :
            case display_mintempPrism :
            case display_missing :
               
               /* These have non precipitation legends.  Set the color
                  of the mpe gage identifier and value labels to be
                  monochrome. */
                  
               mSetColor ( "SandyBrown" ) ;
               
               break ;

             default :
            
               /* Shouldn't get in here. */ 
               flogMessage ( stderr , "In routine 'mpe_set_gage_label_color':\n"
                                  "Reached the default case of the\n"
                                  "display_field_type switch statement.\n"
                                  "display_field_type = %d.\n" , 
                                   display_field_type ) ;
               break ;
         }
   
         break ;

      default :

         flogMessage ( stderr , "In routine 'mpe_set_gage_label_color':\n"
                            "Reached the default case of the\n"
                            "mpe_gage_label_color switch statement.\n"
                            "mpe_gage_label_color = %d.\n" , 
                            mpe_gage_label_color ) ;
         break ;
   } 

}

/*****************************************************************************
 *
 * Routine: unset_mpe_gage_label_color ()
 *
 * Description:  This routine sets the displayed colors of the gage id
 *               and value labels.
 *
 ****************************************************************************/

void unset_mpe_gage_label_color ( )
{
   /* Check if the MPEgageColorContrast gage color option was
      selected. If so, the graphics context needs to be reset. */
   if ( mpe_gage_label_color == MPEgageColorContrast )
   {
      mSetGC ( M_DEFAULT_GC , "SandyBrown" ) ;
   }
}

/*****************************************************************************
 *
 * Routine: full_screen_callback ()
 *
 * Description:  This routine toggles on the full screen display mode.
 *
 ****************************************************************************/
static Boolean is_split_screen_on = False;

void full_screen_callback ( Widget w, XtPointer client_data, 
                            XtPointer call_data )
{
   extern struct _Map map [ MAX_NUM_MAPS ];
   int legend_height;
   int screen_height;
   int screen_width;
   int status;

   if ( is_split_screen_on == False ) return;

   is_split_screen_on = False;

   /* Set flag to indicate full screen mode. */
   screen_height = _get_screen_height ( );
   screen_width = _get_screen_width ( );
   legend_height = _get_legend_height ( );

   /* Set flag indicating full screen mode. */
   /* There are two maps: Map 0 and Map 1. Unmanage Map 1. */
   XtUnmanageChild ( map [ 0 ].map );
   XtUnmanageChild ( map [ 0 ].legend );
   XtUnmanageChild ( map [ 1 ].map );
   XtUnmanageChild ( map [ 1 ].legend );

   /* Alter the height of Map 0. */
   map [ 0 ].height = screen_height;

   /* Alter the position of the Map's legend. */
   map [ 0 ].ly = screen_height - legend_height; 
   XtVaSetValues ( map[0].legend, XmNy, map [ 0 ].ly, NULL ); 

   /* Alter the sizes of the drawing area and pixmap associated with the
      map. */ 
   XFreePixmap ( _get_map_display ( ), map [ 0 ].pixmap );
   XFreePixmap ( _get_map_display ( ), map [ 0 ].lmap );
   _create_pixmaps ( 0, screen_width, screen_height );
   XtVaSetValues ( map[0].map, XmNheight, screen_height, NULL );

   XtManageChild ( map [ 0 ].map );
   XtManageChild ( map [ 0 ].legend );

   /* Trigger an expose event. */
   _set_num_of_maps ( 1 );

   /* Recompute the pixel grid. */
   status = createPixelGrid ( );
 
   if ( status != HydroStatus_OK )
   {
      flogMessage ( stderr, "\nIn routine 'full_screen_callback':\n"
                        "An error was encountered modifying the\n"
                        "pixel grid.  MPE data will not be displayed\n"
                        "properly.\n" );
   }

   /* Gray out the save hour's data bottom menu item. */
   DeSensitize ( savemainbottom_widget );

   mUpdateMap ( 0 );
   mUpdateLegend ( 0 );
}

/*****************************************************************************
 *
 * Routine: split_screen_callback ()
 *
 * Description:  This routine toggles on the full screen display mode.
 *
 ****************************************************************************/
/* Reference to maps. */

void split_screen_callback ( Widget w, XtPointer client_data,
                             XtPointer call_data )
{
   static int first = 1;
   int legend_height;
   int screen_height;
   int screen_width;
   int status;
   extern struct _Map map [ MAX_NUM_MAPS ];

   /* Set flag indicating split screen mode. */
   if ( is_split_screen_on == True ) return;

   is_split_screen_on = True;

   XtUnmanageChild ( map [ 0 ].map );
   XtUnmanageChild ( map [ 0 ].legend ); 

   if ( first == 1 )
   {
      first = 0;
      _create_2nd_map ( );
   }
   else
   {
      legend_height = _get_legend_height ( );
      screen_height = _get_screen_height ( );
      screen_width = _get_screen_width ( );

      /* There is one map:  Map 0. */
      /* Alter the height of Map 0. */
      map [ 0 ].height = screen_height/2;

      /* Alter the sizes of the drawing area and pixmap associated with
         map 0. */
      XFreePixmap ( _get_map_display ( ), map[0].pixmap );
      _create_pixmaps ( 0, screen_width, map [ 0 ].height );
      XtVaSetValues ( map[0].map, XmNheight, map[0].height, NULL );

      /* Make sure Map 0's legend is drawn in the correct place. */
      map [ 0 ].ly = map [ 0].height - legend_height; 
      XtVaSetValues ( map[0].legend, XmNy, map [ 0 ].ly, NULL ); 

      /* Manage Map 1. */
      XtManageChild ( map [ 1 ].map ); 
      XtManageChild ( map [ 1 ].legend );
   }

   XtManageChild ( map [ 0 ].map ); 
   XtManageChild ( map [ 0 ].legend );

   _set_num_of_maps ( 2 );

   
   /* Recompute the pixel grid. */
   status = createPixelGrid ( );
 
   if ( status != HydroStatus_OK )
   {
      flogMessage ( stderr, "\nIn routine 'split_screen_callback':\n"
                        "An error was encountered modifying the\n"
                        "pixel grid.  MPE data will not be displayed\n"
                        "properly.\n" );
   }

   /* Sensitize the save hour's data bottom menu item. */
   Sensitize ( savemainbottom_widget );

   /* Trigger an expose event for first map. */
   mUpdateMap ( 0 );
   mUpdateLegend ( 0 );

   /* Update color levels for second map. */
   display_mpe_data ( 1 );
}

int is_screen_split ( )
{
   return ( is_split_screen_on == True ) ? 1 : 0 ;
}

void restore_working_date(Widget w, XtPointer clientdata, XtPointer calldata)
{
        dates_struct_count = restore_on_cancel_dates_struct_count;
        strcpy(temp_num_of_days, restore_on_cancel_num_of_days);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/rfcwide_callbacks.c,v $";
 static char rcs_id2[] = "$Id: rfcwide_callbacks.c,v 1.32 2007/05/24 13:07:42 whfs Exp $";}
/*  ===================================================  */

}
