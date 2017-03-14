/*
	File:		rate_draw.c
	Date:
	Author:		Dale Shelton
	
	Purpose:
	
*/


#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include "HwStages.h"
#include "Rating.h"
#include "Xtools.h"
#include "rate_draw.h"
#include "rate_cbs.h"
#include "rateds.h"
#include "ParamDefs.h"




void	draw_rating(Widget w)
{
	XFontStruct		*finfo;
	GC			gc;
	Pixmap			pm;
	Display			*display;
	Window			window;
	Dimension		width,
				height,
				oldx = 0 ,
				oldy = 0 ,
				x,
				y;
			
	int 			count,
				incr,
				i,
				num_cfs_ticks,
				int_max_flow,
				divisor;
				
			
	char			buf[MAX_BUF_LEN],
				*stage = "STAGE FEET",
				*cfs = "FLOW KCFS",
				*rec = "RECORD",
				*fld = "FLOOD";

	float			y_data_range,
				y_incr;
		
	extern int		drawCurve;
	extern Rating		*rating;
	extern HwStages		*hw;
	extern float		shiftValue;
	
	
		
	Rating			*rtPtr = NULL ;
	HwStages		*hwPtr = NULL ;
	
	
	/*
                Get display attributes.
        */
        display = XtDisplay(w);
        window  = XtWindow(w);
        width   = getWidgetWidth(w);
        height  = getWidgetHeight(w);

	/*
                Create gc and pixmap.
        */
        gc = XCreateGC(display, window, 0, NULL);
        pm = XCreatePixmap(display, window, width, height,
                        DefaultDepthOfScreen(XtScreen(w)));
	
	/*
                Load font for drawing.
        */
        if ((finfo = XLoadQueryFont(display, "7x13")) != NULL)
                XSetFont(display, gc, finfo->fid);

	/*
		Set maxstage and minstage.  They are declared in rate_draw.h.
	*/
		set_stages(&maxstage,&minstage,rating,shiftValue);
		y_data_range = maxstage - minstage;
		

	/*
                Fill in background.
        */
        SetColor(gc, w, "black");
        XFillRectangle(display, pm, gc, 0, 0, width, height);

        /*
                Draw the X and Y axes.
        */
        SetColor(gc, w, "white");
        XDrawLine(display, pm, gc, R_BORDER, BORDER, R_BORDER, height - BORDER);
        XDrawLine(display, pm, gc, R_BORDER, height - BORDER,
                  width - BORDER, height - BORDER);

        /*
                Draw the labels.
        */
        x = 10;
        y = ((height - BORDER) / 2) - ((strlen(stage) * 13) / 2);
        for (i = 0; i < strlen(stage); i++)
        {
                buf[0] = stage[i];
                buf[1] = '\0';
                XDrawString(display, pm, gc, x, y, buf, 1);
                y += 13;
        }

        x = ((width - BORDER) / 2) - ((strlen(cfs) * 7) / 2) + 25;
        XDrawString(display, pm, gc, x, height - 10, cfs, strlen(cfs));
	
        /*
                Draw the stage scale values.
        */
        y_incr = 10.0;
        x = 30;
        for (i = minstage; i <= maxstage; i += y_incr)
        {
                sprintf(buf, "%6.1f", (float) i);
                y = (height - BORDER) - 
                       (( (i - minstage) / y_data_range) * (height - 2*BORDER));
                       
                XDrawLine(display, pm, gc, R_BORDER, y, R_BORDER - 3, y);
                XDrawString(display, pm, gc, x, y + 4, buf, strlen(buf));
        }
 	
        /* Test the "hw" pointer to make sure that there actually is
           stage data to process. If there isn't, then don't attempt to draw
           the stage information. */

        if ( hw != NULL )
        {
	   hwPtr = (HwStages *) ListFirst(&hw->list);
   	   if ((hwPtr->fs)&& (hwPtr->fs < maxstage))
	   {
	   	SetColor(gc, w, "red");
		y = (height - BORDER) -
		    (( (hwPtr->fs - minstage) / y_data_range) * 
                     (height - 2*BORDER));
		XDrawLine(display, pm, gc, R_BORDER, y, width - BORDER, y);
		
		SetColor(gc, w, "white");
		XDrawString(display, pm, gc, x - 100, y - 5, fld, strlen(fld));
	   }
	
	   if ((hwPtr->ms) && (hwPtr->ms < maxstage))
	   {
		SetColor(gc, w, "blue");
		y = (height - BORDER) - 
		    (( (hwPtr->ms - minstage) / y_data_range) * 
                       (height - 2*BORDER));
		XDrawLine(display, pm, gc, R_BORDER, y, width - BORDER, y);
		
		SetColor(gc, w, "white");
		XDrawString(display, pm, gc, x - 100, y - 5, rec, strlen(rec));
	   }

        }	

	/*
		get the max flow
		round it to a nice number
	*/
	_max_flow = SMALLEST_MAX_FLOW;

        /* Test to determine if the "rating" pointer is NULL.
           If it is, then don't attempt to draw the maximum
           flow. Also, don't attempt to draw the rating curve. */

        if ( rating != NULL )
        {
           rtPtr = (Rating *) ListLast(&rating->list);
	   if (rtPtr)
	   {   
	      _max_flow = rtPtr->discharge;
	      if (_max_flow < SMALLEST_MAX_FLOW)
	          _max_flow = SMALLEST_MAX_FLOW;
		
	      for (divisor = MAX_DIVISOR; divisor > 0; divisor /= 10)
	      {   
	         if ( _max_flow > divisor)
		 {   
		    int_max_flow = _max_flow;
		    int_max_flow /= divisor;
		    int_max_flow ++;
		    int_max_flow *= divisor;
		    _max_flow = int_max_flow;
		    break;
		 }
	      }
	   }

	   /*
	   	   Draw the resulting rating curve.
	   */
	   SetColor(gc, w, "lightblue");
	   rtPtr = (Rating *) ListFirst(&rating->list);
	
	   count = ListCount(&rating->list);
	   if ((count > 0) && drawCurve)
	   {
		   for (i = 0; i < count; i++)
		   {
		   	   x = (R_BORDER + ((rtPtr->discharge / _max_flow) * 
                               (width - T_BORDER)));
			   y = (height - BORDER) -
			    (( (rtPtr->stage + shiftValue - minstage) / y_data_range) * 
                               (height - 2*BORDER));
			   XDrawString(display, pm, gc, x - 2, y + 3, "*", 1);
			
			   if (i > 0)
				   XDrawLine(display, pm, gc, oldx, oldy, x, y);
				
			   oldx = x;
			   oldy = y;
		   	   rtPtr = (Rating *) ListNext(&rtPtr->node);
		   }
	   } 

           /*
                   Draw the cfs scale values.
           */
	   SetColor(gc, w, "white");
           x = R_BORDER;
	
      
	   num_cfs_ticks = (_max_flow/1000) / 10;
	   if (num_cfs_ticks <= 0 )
	   	num_cfs_ticks = 1;
	
           incr = (width - (BORDER + R_BORDER)) / num_cfs_ticks;
	
           for (i = 0; i <= (_max_flow/1000); i += 10)
           {
                   y = (height - 35);
                   sprintf(buf, "%4.1f", (float) i);
                   XDrawString(display, pm, gc, x - 15, y, buf, strlen(buf));

                   y = (height - BORDER);
                   XDrawLine(display, pm, gc, x, y, x, y + 3);
                   x += incr;
           }

        }   /* The end of the "if block" testing for the "nullness" of the
               "rating" pointer. */
	
	XCopyArea(display, pm, window, gc, 0, 0, width, height, 0, 0);
	XFreeGC(display, gc);
	XFreePixmap(display, pm);

	
	return;
}



void set_stages(float *maxstage,float *minstage, Rating *rating, float shiftValue)
{
	Rating *rPtr = NULL ;
	int	num,
		max_fail=0,
		min_fail=0;
	
	if ( rating != NULL ) rPtr = (Rating *) ListFirst(&rating->list);
	if (rPtr)
		*minstage = (float) rPtr->stage + shiftValue;
	else
		min_fail = 1;


	if ( rating != NULL ) rPtr = (Rating *) ListLast(&rating->list);
	if (rPtr)
		*maxstage = (float) rPtr->stage + shiftValue;
	else
		max_fail = 1;
	


	/*
		Adjust minstage if necessary to nearest lower 10.
	*/
	if (min_fail)
	{
		*minstage = 0.0;
	}
	else if (( *minstage > 0.0 ) && ( *minstage < 10.0))
	{
		*minstage = 0.0;	
	}	
	else if ( *minstage < 0.0 )
	{
		num =  (*minstage) /10;
		*minstage = (num-1) * 10;
	}
	
	
	/*
		Round  *maxstage to nearest higher 10
	*/
	if (max_fail)
	{
		*maxstage = 50.0;
	}
	else
	{
		num = (*maxstage) /10;
		*maxstage = (num+1) * 10;
	}
	return;
}
