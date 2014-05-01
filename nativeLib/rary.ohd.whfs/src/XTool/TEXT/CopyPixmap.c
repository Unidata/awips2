/*
	File:		CopyPixmapToDA.c
	Date:		September 1995
	Author:		Dale Shelton
	
	Purpose:
	
*/


#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include "Xtools.h"


void	CopyPixmapToDA(Widget widget, Pixmap pixmap)
{
   	Display		*display;
	Window		window;
	Dimension	height,
	   		width;
	GC		gc;
	
		
	/*
		Get the display attributes.
	*/
	display = XtDisplay(widget);
	window  = XtWindow(widget);
	height  = getWidgetHeight(widget);
	width   = getWidgetWidth(widget);
	
	
	/*
		Create the graphics context.
	*/
	gc = XCreateGC(display, window, 0, NULL);
	
	
	/*
		Copy the pixmap to the drawing area.
	*/
	if (pixmap)
	   	XCopyArea(display, pixmap, window, gc, 0, 0, width, height, 0, 0);
	
	
	/*
		Free memory allocated for the GC.
	*/
	XFreeGC(display, gc);
 	return;  
}
