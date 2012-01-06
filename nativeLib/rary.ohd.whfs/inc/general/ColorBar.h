

#ifndef COLOR_BAR2_H
#define COLOR_BAR2_H

#include "stdio.h"
#include "stdlib.h"
#include "Xm/Xm.h"
#include "MotifWidgets.h"
#include "ColorThreshold.h"


typedef struct _ColorBar
{
   	Dimension daHeight;
	Dimension daWidth;
   
   
	/*
	     Graphic variables for color bar legend 
	*/
	Display *display;
	GC gc;
	Pixmap pixmap;
	Screen *screen;
	Window window;
	
	
	/*
	     The area into which to draw
	*/
	Widget drawingArea;
	
	
	/*
	    ColorThresholArray
	*/ 
	ColorThresholdArray ctArray;

	
	/*
	    Units
	*/
	char description1[BUFSIZ];
	char description2[BUFSIZ];
	
	
	/*
		count of number of accesses
	*/
	long accessCount;

} ColorBar;
  

/*
	prototypes
*/

void initColorBar(ColorBar *colorBar,
		  Widget drawingArea,
		  const ColorThresholdArray *ctArray,
		  char *description1,
		  char *description2);

void freeColorBar(ColorBar *colorBar);

void drawColorBar(ColorBar *colorBar);

void drawColorBarSegment(ColorBar *colorBar, 
			 Position x, Position y,
			 Dimension boxHeight, Dimension boxWidth,
			 char *label, char * color);

void redrawColorBarCallback(Widget w,  XtPointer ptr,  XtPointer cbs);


#endif

