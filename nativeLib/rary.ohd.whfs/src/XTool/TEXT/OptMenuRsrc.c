/*
	File:		OptMenuRsrc.c
	Date:		8/2/94
	Author:		Dale Shelton
	
	Purpose:	Provide method(s) for manipulating
			resources for the XmOptionMenu
			widget.
*/


#include <string.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeBG.h>
#include <Xm/CascadeB.h>
#include "Xtools.h"


Widget	GetMenuHistory(Widget w)
{
	Arg		arg[2];
	Widget		widget;
	

	/*
		Initialize local variables.
	*/
	widget = NULL;
	
	
	/*
		Get the active child of
		the menu.
	*/
	XtSetArg(arg[0], XmNmenuHistory, &widget);
	XtGetValues(w, arg, 1);
	return(widget);
}


void 	SetMenuHistory(Widget w, char *txt)
{
	WidgetList	widgetList;
	Widget		cascade,
			pulldown;
	Arg		arg[2];
	int		cnt,
			i;
	
	
	/*
		Get the widget id of the
		cascade button.
	*/
	cascade = XmOptionButtonGadget(w);
		
		
	/*
		Get the widget id of the 
		pulldown menu associated with
		the cascade button.
	*/
	XtSetArg(arg[0], XmNsubMenuId, &pulldown);
	XtGetValues(cascade, arg, 1);
	
	
	/*
		Get a list of the widget children
		of the pulldown menu.
	*/
	XtSetArg(arg[0], XmNchildren, &widgetList); 
	XtSetArg(arg[1], XmNnumChildren, &cnt);
	XtGetValues(pulldown, arg, 2);
	
	
	/*
		Iterate through the list of widget
		children searching for the that
		matches the requested text.
		
		Assumes the value of the XmNlabelString
		resource (for an individual widget) is 
		the value used for a match.  This will
		not work with widgets other than Labels,
		Buttons (Push, Toggle, Drawn).
	*/
	for (i = 0; i < cnt; i++)
		if (strcmp(GetLabel(widgetList[i]), txt) == 0)
		{
			XtSetArg(arg[0], XmNmenuHistory, widgetList[i]);
			XtSetValues(w, arg, 1);
		}

	
	return;
}


void	SetMenuPos(Widget w, int pos)
{
	WidgetList	widgetList;
	Widget		cascade,
			pulldown;
	Arg		arg[2];
	int		cnt;
	
	
	/*
		Get the widget id of the
		cascade button.
	*/
	cascade = XmOptionButtonGadget(w);
		
		
	/*
		Get the widget id of the 
		pulldown menu associated with
		the cascade button.
	*/
	XtSetArg(arg[0], XmNsubMenuId, &pulldown);
	XtGetValues(cascade, arg, 1);
	
	
	/*
		Get a list of the widget children
		of the pulldown menu.
	*/
	XtSetArg(arg[0], XmNchildren, &widgetList); 
	XtSetArg(arg[1], XmNnumChildren, &cnt);
	XtGetValues(pulldown, arg, 2);
	
	
	/*
		Make sure the requested position
		is greater than or equal to zero (0),
		and less than the count 
		of widget children.
	*/
	if (pos >= 0 && pos < cnt)
	{
		XtSetArg(arg[0], XmNmenuHistory, widgetList[pos]);
		XtSetValues(w, arg, 1);
	}

	return;
}



int	GetMenuPos(Widget w)
{
	WidgetList	widgetList;
	Widget		cascade,
			pulldown;
	Arg		arg[2];
	int		cnt,
			pos,
			i;
	
	
	/*
		Set the widget positional
		value to -1 to indicate a
		NULL value.
	*/
	pos = -1;
	
	
	/*
		Get the widget id of the
		cascade button.
	*/
	cascade = XmOptionButtonGadget(w);
		
		
	/*
		Get the widget id of the 
		pulldown menu associated with
		the cascade button.
	*/
	XtSetArg(arg[0], XmNsubMenuId, &pulldown);
	XtGetValues(cascade, arg, 1);
	
	
	/*
		Get a list of the widget children
		of the pulldown menu.
	*/
	XtSetArg(arg[0], XmNchildren, &widgetList); 
	XtSetArg(arg[1], XmNnumChildren, &cnt);
	XtGetValues(pulldown, arg, 2);
	
	
	/*
		Make sure the requested position
		is greater than or equal to zero (0),
		and less than the count 
		of widget children.
	*/
	for (i = 0; i < cnt; i++)
	{
		if (widgetList[i] == GetMenuHistory(w))
		{
			pos = i;
			break;
		}
	}
		
	return(pos);
}
