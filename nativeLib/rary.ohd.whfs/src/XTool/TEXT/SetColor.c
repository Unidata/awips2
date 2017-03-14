/*
	File:		SetColor.c
	Author:		Dale Shelton
	Date:		7/21/94
	
	Purpose:

        History:

	October 31, 2005     Bryon Lawrence     Modified the PickBestColor
                                                routine to use a double
                                                rgb_diff and best_value
                                                variables instead of long
                                                to avoid overflow in 
                                                pow(2,32) statement.
	
*/

#include <math.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include "Xtools.h"


#define MAXCOLORS 256


void    SetColor(GC gc, Widget widget, char *colorName)
{
   	Display         *display = XtDisplay(widget);
        XColor          xcolor;
 
   	xcolor = GetClosestXColor(widget, colorName);
	
        XSetForeground(display, gc, xcolor.pixel);
	
        return;
}


Pixel   GetNamedColor(Widget widget, char *colorName)
{
        XColor          xcolor;
   	  
 
	xcolor = GetClosestXColor(widget, colorName);
	
        return xcolor.pixel;
}


XColor GetClosestXColor(Widget widget, char *colorName)
{
	Display         *display;
        XColor          xcolor,
                        unused;
 	Colormap        cmap;
 	   
   	display = XtDisplay(widget);
        cmap    = DefaultColormapOfScreen(XtScreen(widget));
	
	
	if ( XAllocNamedColor(display, cmap, colorName, &xcolor, &unused))
	{   
	   	/*
        	printf("got desired color in GetClosestXColor\n");
		*/	
   	}
	
	else
	{	
		xcolor = GetSubstituteXColor(widget, colorName);
	}   
	   
	return xcolor;
}


XColor GetSubstituteXColor(Widget widget, char * colorName)
{
   
   	Display         *display    = XtDisplay(widget);
	int             scr         = DefaultScreen(display);
	Colormap        cmap        = DefaultColormap(display, scr);
 
	static XColor	availColors[MAXCOLORS];
	static int	numColors,
	   		i ;
   
	XColor		desiredColor,
	   		exactColor,
			closestColor;
	   
	desiredColor.red = 1;
	desiredColor.blue = 1;
	desiredColor.green = 1;
	
	XLookupColor(display, cmap, colorName, &desiredColor, &exactColor);

	/*
		Set numColors
	*/
	numColors = DisplayCells(display, scr);
	if (numColors > MAXCOLORS)
		numColors = MAXCOLORS;

	
	/*
		Don't know what this does
	*/
	for(i = 0; i < numColors; i++)
	{
		availColors[i].pixel = i;
		availColors[i].flags = DoRed|DoGreen|DoBlue;
	}
 
	
	/*
		Check for available colors
	*/
	XQueryColors(display, cmap, availColors, numColors);
		       
	 
	closestColor = PickBestColor(desiredColor, availColors, numColors);

	
	return (closestColor);
}




XColor PickBestColor(XColor desiredColor,
		     XColor availColors[],
		     int numColors)

{
	long best_slot = 0;
	long i;
	double rgb_diff = pow(2,32);
        double best_value = rgb_diff;
   
	for(i = 0; i < numColors; i++)
	{
	   
 	        /*
			find differences between wanted (color)
			and available (Color[i])
		*/
	   
	   		
	        /* 
			find difference between colors
		*/
		rgb_diff = CalcRgbDiff(desiredColor, availColors[i]);
		
		
	        /*
			save entry if max of differences less than
			for other colors tested
		*/
     	        if(rgb_diff < best_value)
      	        {               
       			best_slot = i;
			best_value = rgb_diff;
 		}     
	}
		
	/*	
	printf("\ndesiredColor = %d %d %d\n",
	       desiredColor.red,
	       desiredColor.green,
	       desiredColor.blue);
	
	printf("bestColor = %d %d %d\n",
	       availColors[best_slot].red,
	       availColors[best_slot].green,
	       availColors[best_slot].blue);
	*/
	
	return availColors[best_slot];  
}

double CalcRgbDiff(XColor colorA, XColor colorB)
{
	double rgb_diff;
	double red_diff;
	double green_diff;
	double blue_diff;
	
	red_diff = ( double ) labs(colorA.red - colorB.red);
	green_diff =  ( double ) labs(colorA.green - colorB.green);
	blue_diff = ( double ) labs(colorA.blue - colorB.blue);


	rgb_diff = red_diff;
	
       	if(green_diff > rgb_diff) 
		rgb_diff = green_diff;
		
	if(blue_diff > rgb_diff)
		rgb_diff = blue_diff;

	
	return rgb_diff;     
}   
