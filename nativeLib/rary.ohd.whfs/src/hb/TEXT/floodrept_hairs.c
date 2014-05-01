/*
	File:		floodrept_hairs.c
	Date:		May 1997
	Author:		Paul Taylor

	Purpose:	To provide xhair routines for Flood Report DS.	
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <Xm/Xm.h>
#include "Xtools.h"
#include "time_defs.h"
#include "time_series.h"
#include "floodrept.h"
#include "floodrept_hairs.h"
#include "floodrept_draw.h"

void    floodrept_xhairAddHandlers(void)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
	

   /*
   	Handle crosshair X events.
   */
   XtAddEventHandler(frwa->mainDA, EnterWindowMask, False,
		     (XtEventHandler) floodrept_xhairEnterWindow, NULL);

   XtAddEventHandler(frwa->mainDA, LeaveWindowMask, False,
		     (XtEventHandler) floodrept_xhairLeaveWindow, NULL);
   
   XtAddEventHandler(frwa->mainDA, ButtonPressMask | ButtonReleaseMask, False,
		     (XtEventHandler) floodrept_xhairButtonPressRelease, NULL);
   
   return;
}


void	floodrept_xhairButtonPressRelease(Widget w, XtPointer ptr, XEvent *event)
{
   FloodReportWorkArea 	*frwa = getFrWorkArea();
   
   XEvent		report;

   Window		root, child;
   int			root_x, root_y;
   int			pos_x, pos_y;
   unsigned int		keys_buttons;

   
   while (XCheckMaskEvent(frwa->display, ButtonPressMask | ButtonReleaseMask, &report));

   
   if (event->type == ButtonPress)
   {
      /*
      		Set the crosshair boundary conditions.
      		Erase the previous crosshairs.
      */
      floodrept_xhairSetBounds();
      floodrept_xhairErase();
      XtRemoveEventHandler(frwa->mainDA, PointerMotionMask | PointerMotionHintMask, False,
			   (XtEventHandler) floodrept_xhairMotion, NULL);
   }

   
   if (event->type == ButtonRelease)
   {
      if (! XQueryPointer(frwa->display, frwa->mainWindow, &root, &child,
			  &root_x, &root_y, &pos_x, &pos_y, &keys_buttons))
      {
	 return;
      }
      
      
      /*
      		Set the crosshair boundary conditions.
      		Draw the crosshairs.
      */
      floodrept_xhairSetBounds();
      frwa->crosshairs.startx = pos_x;
      frwa->crosshairs.starty = pos_y;
      floodrept_xhairDraw();
      XtAddEventHandler(frwa->mainDA, PointerMotionMask | PointerMotionHintMask, False,
			(XtEventHandler) floodrept_xhairMotion, NULL);
   }
   
   
   return;
}


void	floodrept_xhairKeyRelease(Widget w, XtPointer ptr, XEvent *event)
{
   FloodReportWorkArea 	*frwa = getFrWorkArea();
   
   XEvent		report;

   int			pos_x=0, pos_y=0;

   

   while (XCheckMaskEvent(frwa->display, KeyReleaseMask, &report));

   
   if (event->type == KeyRelease)
   {
      /*
      		Set the crosshair boundary conditions.
      		Erase the previous crosshairs.
      */
      floodrept_xhairSetBounds();
      floodrept_xhairErase();
      XtRemoveEventHandler(frwa->mainDA, PointerMotionMask | PointerMotionHintMask, False,
			   (XtEventHandler) floodrept_xhairMotion, NULL);

      
      /*
      		Set the crosshair boundary conditions.
      		Draw the crosshairs.
      */
      floodrept_xhairSetBounds();
      frwa->crosshairs.startx = pos_x;
      frwa->crosshairs.starty = pos_y;
      floodrept_xhairDraw();
      XtAddEventHandler(frwa->mainDA, PointerMotionMask | PointerMotionHintMask, False,
			(XtEventHandler) floodrept_xhairMotion, NULL);
   }
   
   
   return;
}


void	floodrept_xhairMotion(Widget w, XtPointer ptr, XEvent *event)
{
   FloodReportWorkArea 	*frwa = getFrWorkArea();
   
   XEvent		report;

   Window		root, child;
   int			root_x, root_y;
   int			pos_x, pos_y;
   unsigned int		keys_buttons;

   
   while (XCheckMaskEvent(frwa->display, PointerMotionHintMask, &report));

   if (! XQueryPointer(frwa->display, frwa->mainWindow, &root, &child,
		       &root_x, &root_y, &pos_x, &pos_y, &keys_buttons))
   {
      return;
   }

   
   /*
   	Set the crosshair boundary conditions.
   */
   floodrept_xhairSetBounds();
   
   
   /*
   	Erase the previous crosshairs.
   */
   floodrept_xhairErase();

   
   /*
   	Set up the NEW crosshair coordinates.
   */
   frwa->crosshairs.startx = pos_x;
   frwa->crosshairs.starty = pos_y;
   
   
   /*
   	Check the crosshair coordinates against
   	the drawing area boundaries.
   */
   floodrept_xhairCheckBounds();
   
   
   /*
   	Draw the NEW crosshairs.
   */
   floodrept_xhairDraw();

   
   /*
   	Set the Stage Label.
   */
   floodrept_xhairSetStageLabel();

   
   return;
}


void	floodrept_xhairEnterWindow(Widget w, XtPointer ptr, XEvent *event)
{
   FloodReportWorkArea 	*frwa = getFrWorkArea();


   XtAddEventHandler(frwa->mainDA, PointerMotionMask | PointerMotionHintMask, False,
		     (XtEventHandler) floodrept_xhairMotion, NULL);

   
   /*
   	Set the crosshair boundary conditions.
   	Erase the previous crosshairs.
   */
   floodrept_xhairSetBounds();
   floodrept_xhairErase();

   
   return;
}


void	floodrept_xhairLeaveWindow(Widget w, XtPointer ptr, XEvent *event)
{
   FloodReportWorkArea 	*frwa = getFrWorkArea();

   

   XtRemoveEventHandler(frwa->mainDA, PointerMotionMask | PointerMotionHintMask, False,
			(XtEventHandler) floodrept_xhairMotion, NULL);

   
   /*
   	Set the crosshair boundary conditions.
   	Erase the previous crosshairs.
   */
   floodrept_xhairSetBounds();
   floodrept_xhairErase();
   SetLabel(floodrept_stageLA, "Stage:  ");
   


   return;

}



/*****************************************************************************/



GC	floodrept_xhairSetup(void)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   
   XGCValues	vals;
   GC		gc;
   Arg		arg[1];
   int		ac;
   
   
   /*
   	Init.
   */
   frwa->crosshairs.startx = 0;
   frwa->crosshairs.starty = 0;
   frwa->crosshairs.endx = 0;
   frwa->crosshairs.endy = 0;

   
   /*
   	Get XGCValues from mainDA.
   */
   ac = 0;
   XtSetArg(arg[ac], XmNbackground, &vals.background); ac++;
   XtGetValues(frwa->mainDA, arg, ac);
   
   
   /*
   	Set Values.
   */
   vals.function = GXxor;
   vals.foreground = GetNamedColor(frwa->mainDA, "white");
   vals.foreground = vals.foreground ^ vals.background;
   vals.line_style = LineSolid;
   
   
   /*
   	Get and return a GC with those values.
   */
   gc = XtGetGC(frwa->mainDA, 
		GCForeground | GCBackground | GCFunction | GCLineStyle, 
		&vals);	
   
   return(gc);
}


void    floodrept_xhairSetStageLabel(void)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();

   char			ltimebuf[BUFSIZ],
      			ztimebuf[BUFSIZ],
      			str[BUFSIZ];
   double		value;
   struct tm		*ltmptr,
      			*ztmptr;
   time_t		tracktime;
   
   
   /*
   	Calculate the time for the position
   	currently under the crosshairs.
   */
   tracktime = getFrDataX(frwa->crosshairs.startx, frwa);
   
   
   /*
   	Round back to nearest hour.
   */
   tracktime /= SECONDS_PER_HOUR;
   tracktime *= SECONDS_PER_HOUR;
   
   
   /*
   	Format the time string, in both local and zulu time.  
   	Note, the first format string is missing
   	a '%' in front of the 'Z'.  This is intentional.
   	The first one shows a literal Z and the
   	second shows the current local time zone.
   */
   ztmptr = gmtime(&tracktime);
   strftime(ztimebuf, sizeof(ztimebuf), "%m/%d %H:%M Z", ztmptr);
   
   ltmptr = localtime(&tracktime);
   strftime(ltimebuf, sizeof(ltimebuf), "%m/%d %H %Z", ltmptr);
   
   
   /*
   	Calculate the data value for the position
   	currently under the crosshairs.
   */
   value = getFrDataY(frwa->crosshairs.starty, frwa);
   
   
   /*
   	Create track buffer and set the label string.
   */
   
   memset(&str, '\0', sizeof(str));
   if (frwa->max_y >= 2.0)
      sprintf(str, "Stage:  %6.2f at %s", value,
	      ztimebuf);
   else
      sprintf(str, "Stage:  %7.3f at %s - %s", value,
	      ztimebuf, ltimebuf);
   SetLabel(floodrept_stageLA, str);

   
   return;   
}


void	floodrept_xhairCheckBounds(void)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   
   /*
   	Force the startx and starty fields to be in bounds. 
   */
   if (frwa->crosshairs.startx <= frwa->crosshairs.left)
      frwa->crosshairs.startx = frwa->crosshairs.left;
   
   if (frwa->crosshairs.startx >= frwa->crosshairs.right)
      frwa->crosshairs.startx = frwa->crosshairs.right;
   
   if (frwa->crosshairs.starty <= frwa->crosshairs.top)
      frwa->crosshairs.starty = frwa->crosshairs.top;
   
   if (frwa->crosshairs.starty >= frwa->crosshairs.bottom)
      frwa->crosshairs.starty = frwa->crosshairs.bottom;
   
   
   return;   
}   


void	floodrept_xhairSetBounds(void)
{
   FloodReportWorkArea *frwa = getFrWorkArea();	


   /*
   	Set the crosshair boundary conditions.
   */
   frwa->crosshairs.top = TOP_OFFSET;
   frwa->crosshairs.bottom  = frwa->mainHeight - BOTTOM_OFFSET;
   frwa->crosshairs.left = LEFT_OFFSET;
   frwa->crosshairs.right = frwa->mainWidth - RIGHT_OFFSET;
   
   
   return;   
} 


void	floodrept_xhairDraw(void)
{
   FloodReportWorkArea *frwa = getFrWorkArea();


   /*
   	Draw the vertical line.
   	Draw the horizontal line.
   */
   XDrawLine(frwa->display, frwa->mainWindow, frwa->crosshairs.gc,
	     frwa->crosshairs.startx, frwa->crosshairs.top,
	     frwa->crosshairs.startx, frwa->crosshairs.bottom);
   XDrawLine(frwa->display, frwa->mainWindow, frwa->crosshairs.gc,
	     frwa->crosshairs.left, frwa->crosshairs.starty,
	     frwa->crosshairs.right, frwa->crosshairs.starty);
   
   return;
}   

void	floodrept_xhairErase(void)
{
   FloodReportWorkArea *frwa = getFrWorkArea();
   
   XSetLineAttributes(frwa->display, frwa->crosshairs.gc, 0,
		      0 , 0 ,  0 ) ;
   floodrept_xhairDraw();
   
   return;
}
