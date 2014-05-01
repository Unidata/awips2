/*
	File:		ManagerRsrc.c
	Author:		Chip Gobs
	Date:		4/13/95
	
	Purpose:	Implements convenience functions to 
			access and modify Manager-related 
			resources.
*/

#include "Xtools.h"


Pixel   GetForeground(Widget widget)
{
	Pixel 	fg;
	Arg	arg[1];
	int	ac;
	
	ac = 0;
	XtSetArg(arg[ac], XmNforeground, &fg); ac++;
	XtGetValues(widget, arg, ac);

	return fg;
}



void  SetForeground(Widget widget, Pixel fg)
{
	Arg	arg[1];
	int ac;
	
	ac = 0;
	XtSetArg(arg[ac], XmNforeground, fg); ac++;
	XtSetValues(widget, arg, ac);

	return;
}


