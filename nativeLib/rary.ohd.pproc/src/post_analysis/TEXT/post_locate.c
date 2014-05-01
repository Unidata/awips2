/*=========================================================================*/
/*                    FILE PATH/NAME:   post_locate.c                      */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   locate_main()                      */
/*                                      locate_merge()                     */
/*                                      close_locate()                     */
/*          Modified by Jingtao Deng Feb. 2006 Change HrapToLatLong()      */
/*                                      to HrapToLatLongPproc()            */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "postX.h"

#include "hrap.h"
#include "xs_create_menu_buttons.h"
#include "set_fields.h"
#include "post_stage3.h"
#include "overlay.h"
#include "postanalysis_functions.h"
#include "post_stage3_globals.h"
#include <stdlib.h>
#include "post_stage3_interface.h"
#include "get_vip_level.h"
#include "post_locate.h"

/*~~~GLOBAL VARIABLES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

HRAP HrapToLatLongPproc();

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/


/*********************************************************************/
/*  FUNCTION NAME:   locate_main()                                   */
/*       FUNCTION:   pop up locator shell from main window           */
/*                   displaying  lat/lon, HRAP                       */
/*                   coord, county name, basin name, summed          */
/*                   hourly MPE precip and gage only precip     */
/**********************************************************************

Function type:
   void

Called by function:
   (callback) post_interface

Functions called:
   HrapToLatLongPproc

******************************************** BEGIN locate_main ******************/

void locate_main(w, data, event)
   Widget       w;
   draw_struct *data;
   XEvent      *event;
{
   int          x, y, n;
   Arg          wargs[3];
   Dimension    width, height;
   int          x_pixels_per_bin, y_pixels_per_bin;
   float        value;
   point        hrap;
   HRAP         ll;
   char         str[80];
   XmString     msg;
   Widget       loc_shell, bb, text1, text2, text3, text4, text5, text6;

 /*-------------------------------------------------------------------------*/
 /*     check to see if right button has been pushed                        */
 /*-------------------------------------------------------------------------*/

 if (event->xbutton.button != 3) return;
 if (dbg) printf("In locate_main:\n");

 /*-------------------------------------------------------------------------*/
 /*     check location of button in screen coordinates                      */
 /*-------------------------------------------------------------------------*/

 x = event->xbutton.x;
 y = event->xbutton.y;
 if (dbg) printf("In locate_main, button location coordinates: x=%d y=%d\n",x,y);

 /*-------------------------------------------------------------------------*/
 /*     find location in HRAP coordinates                                   */
 /*-------------------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(w, wargs, n);

 x_pixels_per_bin = (float)width/(float)data->maximum_columns;
 y_pixels_per_bin = (float)height/(float)data->maximum_rows;
 if (x_pixels_per_bin > y_pixels_per_bin)
	x_pixels_per_bin = y_pixels_per_bin;
 else if (y_pixels_per_bin > x_pixels_per_bin)
	y_pixels_per_bin = x_pixels_per_bin;

 hrap.x = data->origin.x + (x/x_pixels_per_bin);
 hrap.y = data->origin.y + (data->maximum_rows - y/y_pixels_per_bin - 1);

 if (dbg) printf("In locate_main, button location HRAP coordinates: hrap.x=%d hrap.y=%d\n",hrap.x,hrap.y);

 /*-------------------------------------------------------------------------*/
 /*     find locations                                                      */
 /*     if HRAP coordinates are outside of rectangle defined by             */
 /*       coord_XXXX.dat, then map basin name and county name are printed   */
 /*       as "location out of range"                                        */
 /*     if grid_to_county and/or grid_to_basin files do not exist, */
 /*       then map basin name and/or county name are printed as    */
 /*       "not defined"                                            */
 /*-------------------------------------------------------------------------*/

 loc_shell = XtCreatePopupShell("locator_shell",
	     transientShellWidgetClass, toplevel, NULL, 0);

 bb = XtCreateManagedWidget("loc_bb",xmRowColumnWidgetClass,loc_shell,NULL,0);

 n=0;
 sprintf(str,"HRAP: x=%d  y=%d", hrap.x, hrap.y );
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, msg); n++;
 text1 = XtCreateManagedWidget("loc_hrap",xmLabelWidgetClass, bb, wargs, n);

 /*ll=HrapToLatLong(hrap);*/

 ll=HrapToLatLongPproc(hrap);

 n=0;
 sprintf(str,"Latitude: %.2f   Longitude: %.2f",ll.y,ll.x );
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text2 = XtCreateManagedWidget("loc_ll",xmLabelWidgetClass, bb, wargs, n);

 if(overlay_avail.gridtobasin == 0)
   sprintf(str,"MAP Basin: not defined");
 else
 {
   if (hrap.y-YOR >= MAXY || hrap.x-XOR >= MAXX || hrap.y-YOR < 0 || hrap.x-XOR < 0)
     sprintf(str,"MAP Basin: location out of range");
   else
   {
     if(strcmp(loc_basin[hrap.y-YOR][hrap.x-XOR],"\0") == 0)
       sprintf(str,"MAP Basin: not defined");
     else
       sprintf(str,"MAP Basin: %s",loc_basin[hrap.y-YOR][hrap.x-XOR]);
   }
 }

 n=0;
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text3 = XtCreateManagedWidget("loc_bas",xmLabelWidgetClass, bb, wargs, n);

 if(overlay_avail.gridtocounty == 0)
   sprintf(str,"County: not defined");
 else
 {
   if (hrap.y-YOR >= MAXY || hrap.x-XOR >= MAXX || hrap.y-YOR < 0 || hrap.x-XOR < 0)
     sprintf(str,"County: location out of range");
   else
     sprintf(str,"County: %s",loc_cty[hrap.y-YOR][hrap.x-XOR] );
 }

 n=0;
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text4 = XtCreateManagedWidget("loc_cty",xmLabelWidgetClass, bb, wargs, n);

 if (hrap.y-YOR >= MAXY || hrap.x-XOR >= MAXX || hrap.y-YOR < 0 || hrap.x-XOR < 0)
   sprintf(str,"Summed Hourly MPE Precip: location out of range");
 else
 {
   value = (float)(precip24[hrap.y-YOR][hrap.x-XOR]/2540.); /*in inch unit*/
   if (value >= 0)
   {
     sprintf(str,"Summed Hourly MPE Precip: %.3f inch", value);
   }
   else
     sprintf(str,"Summed Hourly MPE Precip: missing");
 }

 n=0;
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text5 = XtCreateManagedWidget("loc_pcp",xmLabelWidgetClass, bb, wargs, n);

 if (hrap.y-YOR >= MAXY || hrap.x-XOR >= MAXX || hrap.y-YOR < 0 || hrap.x-XOR < 0)
   sprintf(str,"Gage Only Precip: location out of range");
 else
 {
   value = (float)(gageonly2[hrap.y-YOR][hrap.x-XOR]/254.); /*in inch unit*/

   if (value >= 0)
     sprintf(str,"Gage Only Precip: %.3f inch", value);
   else
     sprintf(str,"Gage Only Precip: missing");
 }

 n=0;
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text6 = XtCreateManagedWidget("loc_pcp",xmLabelWidgetClass, bb, wargs, n);

 XtPopup(loc_shell,XtGrabNone);
 XtAddEventHandler(w, ButtonReleaseMask, FALSE, close_locate, loc_shell);

}

/********************************************* END locate_main ******************/


/*********************************************************************/
/*  FUNCTION NAME:   locate_merge()                                  */
/*       FUNCTION:   pop up locator shell from merged field window   */
/*                   displaying  lat/lon, HRAP                       */
/*                   coord, county name, basin name, merged          */
/*                   precip and ratio of merged precip to summed     */
/*                   hourly MPE precip                          */
/**********************************************************************

Function type:
   void

Called by function:
   (callback) post_interface

Functions called:
   HrapToLatLongPproc

******************************************** BEGIN locate_main ******************/

void locate_merge(w, data, event)
   Widget       w;
   draw_struct *data;
   XEvent      *event;
{
   int          x, y, n;
   Arg          wargs[3];
   Dimension    width, height;
   int          x_pixels_per_bin, y_pixels_per_bin;
   float        value;
   point        hrap;
   HRAP         ll;
   char         str[80];
   XmString     msg;
   Widget       loc_shell, bb, text1, text2, text3, text4, text5, text6;

 /*-------------------------------------------------------------------------*/
 /*     check to see if right button has been pushed                        */
 /*-------------------------------------------------------------------------*/

 if (event->xbutton.button != 3) return;
 if (dbg) printf("In locate_merge\n");

 /*-------------------------------------------------------------------------*/
 /*     check location of button in screen coordinates                      */
 /*-------------------------------------------------------------------------*/

 x = event->xbutton.x;
 y = event->xbutton.y;
 if (dbg) printf("In locate_merge, button location screen coordinates:x=%d y=%d\n",x,y);

 /*-------------------------------------------------------------------------*/
 /*     find location in HRAP coordinates                                   */
 /*-------------------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(w, wargs, n);

 x_pixels_per_bin = (float)width/(float)data->maximum_columns;
 y_pixels_per_bin = (float)height/(float)data->maximum_rows;
 if (x_pixels_per_bin > y_pixels_per_bin)
	x_pixels_per_bin = y_pixels_per_bin;
 else if (y_pixels_per_bin > x_pixels_per_bin)
	y_pixels_per_bin = x_pixels_per_bin;

 hrap.x = data->origin.x + (x/x_pixels_per_bin);
 hrap.y = data->origin.y + (data->maximum_rows - y/y_pixels_per_bin);

 if (dbg) printf("In locate_merge, button location HRAP coordinates: hrap.x=%d hrap.y=%d\n",hrap.x,hrap.y);

 /*-------------------------------------------------------------------------*/
 /*     find locations                                                      */
 /*     if HRAP coordinates are outside of rectangle defined by             */
 /*       coord_XXXX.dat, then map basin name and county name are printed   */
 /*       as "location out of range"                                        */
 /*     if grid_to_county and/or grid_to_basin files do not exist, */
 /*       then map basin name and/or county name are printed as    */
 /*       "not defined"                                            */
 /*-------------------------------------------------------------------------*/

 loc_shell = XtCreatePopupShell("locator_shell",
	     transientShellWidgetClass, toplevel, NULL, 0);

 bb = XtCreateManagedWidget("loc_bb",xmRowColumnWidgetClass,loc_shell,NULL,0);

 n=0;
 sprintf(str,"HRAP: x=%d  y=%d", hrap.x, hrap.y );
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, msg); n++;
 text1 = XtCreateManagedWidget("loc_hrap",xmLabelWidgetClass, bb, wargs, n);

 /*ll=HrapToLatLong(hrap);*/

 ll=HrapToLatLongPproc(hrap);

 n=0;
 sprintf(str,"Latitude: %.2f   Longitude: %.2f",ll.y,ll.x );
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text2 = XtCreateManagedWidget("loc_ll",xmLabelWidgetClass, bb, wargs, n);

 if(overlay_avail.gridtobasin == 0)
 {
   sprintf(str,"MAP Basin: not defined");
 }
 else
 {
   if (hrap.y-YOR >= MAXY || hrap.x-XOR >= MAXX || hrap.y-YOR < 0 || hrap.x-XOR < 0)
   {
     sprintf(str,"MAP Basin: location out of range");
   }
   else
   {
     if(strcmp(loc_basin[hrap.y-YOR][hrap.x-XOR],"\0") == 0)
     {
     sprintf(str,"MAP Basin: not defined");
     }
     else
     {
     sprintf(str,"MAP Basin: %s",loc_basin[hrap.y-YOR][hrap.x-XOR]);
     }
   }
 }

 n=0;
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text3 = XtCreateManagedWidget("loc_bas",xmLabelWidgetClass, bb, wargs, n);

 if(overlay_avail.gridtocounty == 0)
 {
    sprintf(str,"County: not defined");
 }
 else
 {
   if (hrap.y-YOR >= MAXY || hrap.x-XOR >= MAXX || hrap.y-YOR < 0 || hrap.x-XOR < 0)
   {
     sprintf(str,"County: location out of range");
   }
   else
   {
     sprintf(str,"County: %s",loc_cty[hrap.y-YOR][hrap.x-XOR] );
   }
 }
 n=0;
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text4 = XtCreateManagedWidget("loc_cty",xmLabelWidgetClass, bb, wargs, n);

 if (hrap.y-YOR >= MAXY || hrap.x-XOR >= MAXX || hrap.y-YOR < 0 || hrap.x-XOR < 0)
 {
   sprintf(str,"Merged Precip: location out of range");
 }
 else
 {
   value = (float)merge[hrap.y-YOR][hrap.x-XOR]/254.;
   if (value >= 0.0)
     sprintf(str,"Merged Precip: %.3f inch", value);
   else
     sprintf(str,"Merged Precip: missing");
 }
 n=0;
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text5 = XtCreateManagedWidget("loc_pcp",xmLabelWidgetClass, bb, wargs, n);

 if (hrap.y-YOR >= MAXY || hrap.x-XOR >= MAXX || hrap.y-YOR < 0 || hrap.x-XOR < 0)
 {
   sprintf(str,"ratio: location out of range");
 }
 else
 {
   value = (float)ratio[hrap.y-YOR][hrap.x-XOR];
   if (value >= 0.0)
   {
     sprintf(str,"ratio: %.3f ", value);
   }
   else
   {
     value = value/254.;
     sprintf(str,"ratio: %.3f in.", value);
   }
 }
 n=0;
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 text6 = XtCreateManagedWidget("loc_pcp",xmLabelWidgetClass, bb, wargs, n);

 XtPopup(loc_shell,XtGrabNone);
 XtAddEventHandler(w, ButtonReleaseMask, FALSE, close_locate, loc_shell);
}

/********************************************* END locate_merge ******************/
/*******************************************************************/
/*  FUNCTION NAME:   close_locate()                                */
/*       FUNCTION:   close locator shell                           */
/********************************************************************

Function type:
   void

Called by function:
   (callback) locate_main
   (callback) locate_merge

Functions called:
   none

******************************************** BEGIN close_locate ************/

void close_locate(w, shell, event)
   Widget       w, shell;
   XEvent      *event;
{
 if (event->xbutton.button != 3) return;
 XtPopdown(shell);
 XtRemoveEventHandler(w, ButtonReleaseMask, FALSE, close_locate, shell);
}

/********************************************* END close_locate ************/
