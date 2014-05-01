/*
     File:		floodrept_draw.c
     Date:		May 1997
     Author:		Paul Taylor

     Purpose:		To provide drawing routines for Flood Report DS.	
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <values.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <X11/cursorfont.h>
#include "Xtools.h"
#include "Riverstat.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "LoadUnique.h"
#include "time_defs.h"
#include "floodrept_draw.h"
#include "floodrept_show.h"
#include "floodrept.h"
#include "time_convert.h"
#include "hybase.h"



/* ------------------------------------------------------------------------------------ */

Position getFrWinX(double xdata, FloodReportWorkArea *frwa)
{
	
   	Position x;
	   
 	x = GetWinCoord(xdata, frwa->beginTime, frwa->endTime, 
	                LEFT_OFFSET,
	                frwa->mainWidth - RIGHT_OFFSET);
   
   	return x;
}

/* ------------------------------------------------------------------------------------ */

Position getFrWinY(double ydata, FloodReportWorkArea *frwa)
{
	
   	Position y;
	   
	y = GetWinCoord(ydata, frwa->min_y, frwa->max_y,
			frwa->mainHeight-BOTTOM_OFFSET, TOP_OFFSET);
	
   	return y;
}

/* ------------------------------------------------------------------------------------ */

double getFrDataY(Position ywin, FloodReportWorkArea *frwa)
{
	
   	double ydata;

	ydata = GetDataCoord(ywin, frwa->min_y,
			     frwa->max_y,
			     frwa->mainHeight-BOTTOM_OFFSET,
			     TOP_OFFSET);
	
   	return ydata;
	   
}

/* ------------------------------------------------------------------------------------ */

double getFrDataX(Position xwin, FloodReportWorkArea *frwa)
{
	
   	double xdata;
	   
	xdata = GetDataCoord(xwin, frwa->beginTime,
			     frwa->endTime,
			     LEFT_OFFSET,
			     frwa->mainWidth - RIGHT_OFFSET);
	
   	return xdata;
	   
}

/* ------------------------------------------------------------------------------------ */

void floodrept_drawFloodEvent(void)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   
   
   /*
   	Clear out the SignificantTimes, and
	Get data for new flood event.
   */
   clearForm(fr_timesFO);
   floodrept_getFloodEvent();
   
   
   /*
   	Set the begin and end times on the main window.
   */
   floodrept_setBeginEndTimes();
   
   
   /*
   	Make max and min into nice numbers for Y axis.
   */
   floodrept_setYAxisMaxMin();
   
   
   /*
   	Free main pixmap.
   */
   if (frwa->mainPM)
      XFreePixmap(frwa->display, frwa->mainPM);
   
   frwa->mainPM = XCreatePixmap(frwa->display, frwa->mainWindow,
				frwa->mainWidth, frwa->mainHeight,
				DefaultDepthOfScreen(XtScreen(frwa->mainDA)));
   
   
   /*
   	Fill mainDA with "black".
   */
   SetColor(frwa->mainGC, frwa->mainDA, "black");
   XFillRectangle(frwa->display, frwa->mainPM, frwa->mainGC, 0, 0,
		  frwa->mainWidth,
		  frwa->mainHeight);
   
   
   /*
   	Perform all necessary drawing.
   */
   floodrept_drawPoints();
   floodrept_drawYAxis();
   floodrept_drawTAxis();
   floodrept_drawFloodLine();
   floodrept_loadSignificantTimes();
   
   
   /*
   	Copy the mainPM to the drawing area.
   */ 
   CopyPixmapToDA(frwa->mainDA, frwa->mainPM);

   return;
}

/* ------------------------------------------------------------------------------------ */

void	floodrept_getFloodEvent(void)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   
   UniqueList		*ulPtr = NULL;
   Riverstat		*riverPtr = NULL;
   
   int      		*poslist = NULL;
   int      		cnt = 0;
   int                  count;
   
   char			where[MAX_WHERE_LEN];
   char			tmp_lid[LOC_ID_LEN + 1];
   char **              values;
   char			buf[BUFSIZ];
   XmString		xmStr;

   
   if (frwa->floodtsPtr)
   {	  
      FreeFloodTs(frwa->floodtsPtr);
      frwa->floodtsPtr = NULL;
   }

   
   XmListGetSelectedPos(floodreptLI, &poslist, &cnt);
   if (poslist && (cnt > 0))
   {
      ulPtr = (UniqueList *) ListNth(&frwa->ulPtr->list, poslist[0]);
      if (ulPtr)
      {
	 memset(where, '\0', sizeof(where));
	 memset(tmp_lid, '\0', sizeof(tmp_lid));

	 /*
	 	Get floodtsPtr.
	 */
         values = ParseUnique ( ulPtr, &count );  
         
	 strncpy(tmp_lid, values[0], LOC_ID_LEN);
	 sprintf(where, " WHERE lid = '%-s' and flood_event_id = %ld ORDER BY obstime ",
		 tmp_lid, atol(values[1]) - MAX_NUM_EVENTS);
	 frwa->floodtsPtr = (FloodTs *) GetFloodTs(where);

	 
         FreeParseUnique ( values ) ; 

	 /*
	 	Get flood stage value for given lid.
	 */
	 memset(where, '\0', sizeof(where));
	 sprintf(where, " WHERE lid = '%-s' ", tmp_lid);
	 if ((riverPtr = (Riverstat *) GetRiverstat(where)) != NULL)
	 {
	    frwa->fs = riverPtr->fs;
	    FreeRiverstat(riverPtr);
	 }
	 else
	 {
	    frwa->fs = MSG;
	 }
	 
	 
	 /*
	 	Set the lid label.
	 */
	 memset(buf, '\0', sizeof(buf));
	 sprintf(buf, "Location:  %-s", tmp_lid);
	 xmStr = XmStringCreateSimple(buf);
	 XtVaSetValues(floodrept_lidLA, XmNlabelString, xmStr, NULL);
	 XmStringFree(xmStr);
      }
   }
   
   return;
}

/* ------------------------------------------------------------------------------------ */

void	floodrept_setBeginEndTimes(void)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   FloodTs		*fPtr = NULL ;

   Arg			arg[2];
   Widget		workarea;
   Dimension  		widthDA;
   

   /*
   	Set the beginTime/endTime on the "Time" axis.
   */
   frwa->beginTime = frwa->beginEventsTime - SECONDS_PER_DAY;
   frwa->endTime   = frwa->endEventsTime + SECONDS_PER_DAY;
   
   
   if ( frwa->floodtsPtr != NULL )
   {
      fPtr = (FloodTs *) ListFirst(&frwa->floodtsPtr->list);
   }

   if (fPtr)
   {
      if (isTimeError(yearsec_dt_to_timet(fPtr->obstime, &frwa->beginTime)))
	 fprintf(stderr, "ERROR in converting beginTime...\n");
      else
	 frwa->beginTime -= SECONDS_PER_DAY;
   }
   
   if ( frwa->floodtsPtr != NULL )
   {
      fPtr = (FloodTs *) ListLast(&frwa->floodtsPtr->list);
   }

   if (fPtr)
   {	  
      if (isTimeError(yearsec_dt_to_timet(fPtr->obstime, &frwa->endTime)))
	 fprintf(stderr, "ERROR in converting endTime...\n");
      else
	 frwa->endTime += SECONDS_PER_DAY;
   }
   
   
   /*
   	Determine the necessary width of the main DA.
   */
   widthDA = (PIXELS_PER_HOUR * (frwa->endTime - frwa->beginTime)
	      / SECONDS_PER_HOUR);
   if (widthDA < MIN_MAINDA_WIDTH)
      widthDA  = MIN_MAINDA_WIDTH;
   
   if (widthDA > MAX_MAINDA_WIDTH)
      widthDA = MAX_MAINDA_WIDTH;
   
   
   /*
   	Get the Scrolled Window's workarea
   */
   XtSetArg(arg[0], XmNworkWindow, &workarea);
   XtGetValues(floodrept_mainSW, arg, 1);
   
   
   /*
   	Set the width of DA and the workarea
   */
   setWidgetWidth(floodrept_mainDA, widthDA);
   frwa->mainWidth = getWidgetWidth(floodrept_mainDA);
   setWidgetWidth(workarea, frwa->mainWidth);
   
   
   return;     
}     
     
/* ------------------------------------------------------------------------------------ */

void	floodrept_setYAxisMaxMin(void)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   
   int			roundFactor = 5;
   long			lmax, lmin;
   FloodTs		*fPtr;
   

   if (frwa->fs > 0)
   {
      frwa->min_y = frwa->fs;
      frwa->max_y = frwa->fs;
   }
   else
   {
      frwa->min_y = 5;
      frwa->max_y = 10;
   }	
   
   if (frwa->floodtsPtr)
   {	
      fPtr = (FloodTs *) ListFirst(&frwa->floodtsPtr->list);
      /*
      		Go through the list and find the max and min values.
      */
      while (fPtr)
      {
	 if (fPtr->value < frwa->min_y)
	    frwa->min_y = fPtr->value;
	 
	 if (fPtr->value > frwa->max_y)
	    frwa->max_y = fPtr->value;
	 
	 fPtr = (FloodTs *) ListNext(&fPtr->node);	  
      }	  
   }			  
   
   
   /*	   
   	Round min down.  If the original min data > 0, then
   	don't round below 0.
   */
   lmin =  frwa->min_y / roundFactor;
   
   if (lmin >= 0) 
   { 	 
      frwa->min_y = (lmin - 1) * roundFactor;
      if (frwa->min_y < 0)
	 frwa->min_y = 0;
   }
   
   else /* lmin < 0 */
   {
      frwa->min_y = (lmin - 1) * roundFactor;	
   }	
   
   
   /*
   	Round max up.
   */
   lmax =  frwa->max_y / roundFactor;
   frwa->max_y = (lmax + 1) * roundFactor;
   
   
   /*
   	If the difference between max_y and min_y < 10,
   	round max_y up again.
   */
   if ( (frwa->max_y - frwa->min_y) < 10)
   {
      frwa->max_y = (lmax + 2) * roundFactor;	
   }
   
   return;
}   

/* ------------------------------------------------------------------------------------ */

void   floodrept_drawPoints(void)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   FloodTs		*fPtr = NULL;
   time_t		obsTime;
   Position		x, y;

   
   if ( frwa->floodtsPtr != NULL )
   {
      fPtr = (FloodTs *) ListFirst(&frwa->floodtsPtr->list);
   }

   while(fPtr)
   {
      if (! isTimeError(yearsec_dt_to_timet(fPtr->obstime, &obsTime)))
      {
	 x = getFrWinX(obsTime, frwa);
	 
	 y = getFrWinY(fPtr->value, frwa);
	 
	     
	 SetColor(frwa->mainGC, frwa->mainDA, "yellow");
	 
	 XDrawString(frwa->display, frwa->mainPM, frwa->mainGC,
		     x, y+2, "o", 1);
	 
	 fPtr = (FloodTs *) ListNext(&fPtr->node);	  
      }
   }
   
      
   return;
}     

/* ------------------------------------------------------------------------------------ */

void	floodrept_drawYAxis(void)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   
   Display		*display = frwa->display;
   Widget		da = frwa->axisDA;
   GC			gc = frwa->axisGC;
   Pixmap		pm = frwa->axisPM;
   
   Dimension		height = frwa->axisHeight,
      			width = frwa->axisWidth;
   double		min_y = frwa->min_y,
      			max_y = frwa->max_y;

   Pixel		pixel;
   Position		min, max, x, y;
   char			buf[BUFSIZ];
   double		cur_stage,
      			stage_inc;
   
   
   /*
   	Fill in the rectangle of the DA with the same color as the
   	Background of the Parent.
   */
   pixel = GetBackground(XtParent(da));
   XSetForeground(display, gc, pixel);
   XFillRectangle(display, pm, gc, 0, 0, width, height);
   SetColor(gc, da, "white");
   
   
   /*
   	Get min, max, and x position.
   */
   if(min_y == max_y)
   {
      min_y -= 1.0;
      max_y += 1.0;
   }
   min = getFrWinY(min_y, frwa);
   max = getFrWinY(max_y, frwa);
   
   x = width - LINE_OFFSET;
   
   
   /*
   	Draw the vertical line.
   */
   SetColor(gc, da, "white");
   XDrawLine(display, pm, gc, x, min, x, max); 
   
   
   /*
   	Draw the horizontal lines and labels for the regular intervals.
   */
   stage_inc = (max_y - min_y) / NUM_VERTICAL_INTERVALS;
   x = width - LINE_OFFSET;
   
   for (cur_stage = min_y;  cur_stage <= max_y;  cur_stage += stage_inc)
   {
   
       
      y = getFrWinY(cur_stage, frwa);
         
      XDrawLine(display, pm, gc, x-5, y, x+5, y);
      
      sprintf(buf, "%6.1f", cur_stage);
      XDrawString(display, pm, gc,
		  x - LABEL_OFFSET, y+2, buf, strlen(buf));
   }   
   
   
   /*
   	Draw the Axis Label.
   */
   floodrept_drawYAxisLabel();
   
   
   /*
   	Place the PM on the axisDA.
   	(Since nothing else needs to be done to the axisDA.)
   */
   CopyPixmapToDA(frwa->axisDA, frwa->axisPM);
   
   return;  
}

/* ------------------------------------------------------------------------------------ */

void	floodrept_drawYAxisLabel()
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   char			*label = "STAGE IN FEET";
   
   Dimension		x, y;
   char			buf[BUFSIZ];
   int			vertical_aspect,
      			len, i;
   
   
   /*
   	Initialize local variables.
   */
   vertical_aspect = 12;
   y = 120;
   x = 0;
   
   
   /*
   	Iterate through string and draw label.
   */
   len = strlen(label);
   for (i = 0; i < len; i++)
   {
      buf[0] = label[i];
      buf[1] = '\0';
      
      XDrawString(frwa->display, frwa->axisPM,
		  frwa->axisGC, x, y, buf, strlen(buf));
      
      y += vertical_aspect;
   }
   
   return;
}

/* ------------------------------------------------------------------------------------ */

void	floodrept_drawTAxis(void)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   
   Display		*display = frwa->display;
   Pixmap		pm = frwa->mainPM;
   GC			gc = frwa->mainGC;
   Widget		da = frwa->mainDA;
   
   time_t		beginTime = frwa->beginTime;
   time_t		endTime = frwa->endTime;
   Dimension		height = frwa->mainHeight,
      			width = frwa->mainWidth;
   
   struct tm		*tm_ptr;
   time_t		looptime;
   
   char			buf[BUFSIZ];
   
   Position		x, y;
   

   /*
   	Set the y position.
   */
   y = height - BOTTOM_OFFSET;
   
   
   /*
   	Draw the horizontal line.
   */
   SetColor(gc, da, "white");
   XDrawLine(display, pm, gc, 0, y, width, y); 
   
   
   /*
   	Loop through time range & draw the tick marks.
   */
   for (looptime = beginTime;  looptime <= endTime;  looptime += SECONDS_PER_HOUR)
   {
      x = getFrWinX((double)looptime, frwa);
      
      tm_ptr = gmtime(&looptime);
      XDrawLine(display, pm, gc, x, y, x, y+4);
      
      if((tm_ptr->tm_hour % 6) == 0)
      {
	 XDrawLine(display, pm, gc, x, y-4, x, y);
	 
	 if(tm_ptr->tm_hour == 0)
	 {
	    /*
	    	Draw the hour text.
	    */
	    strftime(buf, sizeof(buf), "%HZ", tm_ptr);
	    XDrawString(display, pm, gc, x-5, y+15, buf, strlen(buf));
	    
	    /*
	    	Draw the month/day text.
	    */
	    strftime(buf, sizeof(buf), "%m/%d", tm_ptr);
	    XDrawString(display, pm, gc, x-19, y+28, buf, strlen(buf));
	 }	      
	 else	/* just a regular 6 hour period */
	 {
	    /*
	    	Draw the hour text.
	    */
	    strftime(buf, sizeof(buf), "%H", tm_ptr);
	    XDrawString(display, pm, gc, x-5, y+15, buf, strlen(buf));
	 }
      }
   }
   
   
   /*
   	Return Pixmap & GC.
   */
   frwa->mainPM = pm;
   frwa->mainGC = gc;

   
   return;	
}

/* ------------------------------------------------------------------------------------ */

void	floodrept_drawFloodLine(void)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   
   Display		*display = frwa->display;
   
   Pixmap		pm = frwa->mainPM;
   GC			gc = frwa->mainGC;
   Widget		da = frwa->mainDA;

   Dimension		width = frwa->mainWidth;
   Position		y_pos;
   
   /*
   	Draw the horizontal line.
   */
   if (frwa->fs > 0.0)
   {	
      SetColor(gc, da, "red");
      y_pos = getFrWinY(frwa->fs, frwa);
      XDrawLine(display, pm, gc, 0, y_pos, width, y_pos);
   }
   
   return;
}

/* ------------------------------------------------------------------------------------ */


void	floodrept_redrawAxis(Widget w, XtPointer ptr, XtPointer cbs)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   

   if (frwa->axisPM)
   {
      CopyPixmapToDA(frwa->axisDA, frwa->axisPM);
   }
   
   return;
}

/* ------------------------------------------------------------------------------------ */


void	floodrept_redrawMain(Widget w, XtPointer ptr, XtPointer cbs)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   
   if (frwa->mainPM)
   {
      CopyPixmapToDA(frwa->mainDA, frwa->mainPM);
   }
   
   return;
}
/* ------------------------------------------------------------------------------------ */
