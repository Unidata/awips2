#include "ColorBar.h"
#include "map_library.h"

/*

 	Author: Chip Gobs
	Date:   7/28/97
	This may need to be more generalized at some point.
	
*/



/*************************************************************************/

void initColorBar(ColorBar *colorBar,
		  Widget drawingArea,
		  const ColorThresholdArray *ctArray,
		  char *description1,
		  char *description2)
{
     /*
     	  free up left-over memory
     */
     freeColorBar(colorBar);
     
     /*
     	   Copy drawingArea into the ColorBar structure
     */
     colorBar->drawingArea = drawingArea;
     
     
     /*
     Set the general graphics fields that are used
     by the Drawing Area.
     */
     colorBar->display = XtDisplay(colorBar->drawingArea);
     colorBar->screen = XtScreen(colorBar->drawingArea);
     
     
     /*
     Set the specific graphics fields that are used
     by the Drawing Area.
     */
     colorBar->window = XtWindow(colorBar->drawingArea);
     colorBar->gc = XCreateGC(colorBar->display,
			      colorBar->window, 0, NULL);
     
     
     /*
     	  Determine colorBar->drawingArea's dimensions
     */
     colorBar->daHeight = getWidgetHeight(colorBar->drawingArea);
     colorBar->daWidth = getWidgetWidth(colorBar->drawingArea);
  
     
     /*
     	   Create the pixmap
     */
     colorBar->pixmap = XCreatePixmap(colorBar->display,
				      colorBar->window,
				      colorBar->daWidth,
				      colorBar->daHeight,
                                      DefaultDepthOfScreen(colorBar->screen));
  
     
     /*
          copy the ColorThresholdArray to colorBar's array
     */
     copyColorThresholdArray(&colorBar->ctArray, ctArray);
  
     
     /*
     printf("%s color set:\n",thresholdSetName);
     printColorThresholdArray(*colorBar->ctArray);
     */
     
     
     
     /*
     	  Descriptions
     */
     strcpy(colorBar->description1, description1);
     strcpy(colorBar->description2, description2);
     
 
     return;   
}

/*************************************************************************/

void freeColorBar(ColorBar *colorBar)
{
     if (colorBar->pixmap)
     {
          XFreePixmap(colorBar->display, colorBar->pixmap);		  
	  colorBar->pixmap = 0 ;
	  colorBar->display = NULL ;
     }
     
     return ;
     
}

/**************************************************************************/

void drawColorBar(ColorBar *colorBar)
{

     int i;
     
     Dimension useable_height = 0;
     Dimension boxWidth = 0;
     
     Position left_x;
     Position right_x;
     Position y;
     Position boxHeight = 5;
     Position bottom_y_offset = 40;
     Position top_y_offset = 20;
     Position label_Y_Offset = 10;
     char label[BUFSIZ];
     double curValue;
     ColorThresholdArray *ctArray = &colorBar->ctArray;
  
     /*
          Clear the drawing area with black
     */
     SetColor(colorBar->gc, colorBar->drawingArea, "black");
     
     XFillRectangle(colorBar->display, colorBar->pixmap, colorBar->gc,
		       0, 0, colorBar->daWidth, colorBar->daHeight);
     
     /*
     	  set the x related variables
     */
     left_x = 85;
     right_x = 155;
     boxWidth = right_x - left_x;
     
     /*
	  	draw the label
     */
    
     SetColor(colorBar->gc, colorBar->drawingArea, "white");
     XDrawString(colorBar->display,
		 colorBar->pixmap,
		 colorBar->gc,
		 5, label_Y_Offset,
		 colorBar->description1,
		 strlen(colorBar->description1));
     
     XDrawString(colorBar->display,
		 colorBar->pixmap,
		 colorBar->gc,
		 5, label_Y_Offset+13,
		 colorBar->description2,
		 strlen(colorBar->description2));
  
     
     y = colorBar->daHeight - bottom_y_offset - boxHeight;
     
     useable_height =  (colorBar->daHeight - top_y_offset - bottom_y_offset);
     
     /*
          The missing box takes up 1 slot, the spacing takes up 1 slot,
	  and the default box takes up 1 slot, so the box_height is
	  the useable_height divided by the total number of box slots
	  taken, which is the length of the array + the 3 special slots.
     */
     boxHeight = useable_height/(ctArray->length + 3);
     
     if (boxHeight < 1)
	boxHeight = 1;
     
     /*
          Draw the segment of the color bar for the Missing Value
     */
     sprintf(label, "  MISSING ");
     drawColorBarSegment(colorBar,
			 left_x, y,
			 boxHeight, boxWidth,
			 label, ctArray->missingColorName );
     y  -= 2*boxHeight;
     
     
     
     /*
     	  Draw the segment of the color bar for the Default value
     */   
     sprintf(label, " < %7.2f---", ctArray->thresholds[0].value);
     drawColorBarSegment(colorBar,
			 left_x, y,
			 boxHeight, boxWidth,
			 label, ctArray->defaultColorName );
     
     
     /*
     	  Draw the color bar using the thresholds
     */
     for (i = 0; i < ctArray->length; i++)
     {
	 /*
		get the curValue
	 */
	  curValue  = ctArray->thresholds[i].value;
 
	  
	  /*
	       find the correct y position
	  */
          y  -= boxHeight;
	  
	  
	  /*
	  	create the number label
	  */
	  if (i == ctArray->length-1 )
	       sprintf(label,">= %7.2f---", curValue);
	  else 
	       sprintf(label,"   %7.2f---", curValue);
		       
		       
	  drawColorBarSegment(colorBar,
			      left_x, y,
			      boxHeight, boxWidth,
			      label, ctArray->thresholds[i].colorName );

     }
     
     redrawColorBarCallback(colorBar->drawingArea, colorBar, NULL);
     
}
     

/*************************************************************************/

void drawColorBarSegment(ColorBar *colorBar, 
			 Position x, Position y,
			 Dimension boxHeight, Dimension boxWidth,
			 char *label, char * color)
{
     
     /*
     draw the label
     */
     SetColor(colorBar->gc, colorBar->drawingArea, "white");
     XDrawString(colorBar->display,
		 colorBar->pixmap,
		 colorBar->gc,
		 5, y + boxHeight + 4,
		 label, strlen(label));
     
     
     /*
     draw the rectangle
     */
     SetColor( colorBar->gc , colorBar->drawingArea , color );
     XFillRectangle(colorBar->display,
		    colorBar->pixmap,
		    colorBar->gc,
		    x, y, boxWidth, boxHeight);
     
     return;
     
}

/***************************************************************************/

void redrawColorBarCallback(Widget w,  XtPointer ptr,  XtPointer cbs)
{
     ColorBar *colorBar = (ColorBar *) ptr;
     
     if (colorBar->pixmap)
     {
          CopyPixmapToDA(colorBar->drawingArea, 
			       colorBar->pixmap);
     }
     
     return;     
}

/***************************************************************************/
