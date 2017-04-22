/*
	File:		CoreRsrc.c
	Author:		Dale Shelton
	Date:		7/21/94
	
	Purpose:	Implements convenience functions to 
			access and modify Core-related 
			resources.
*/

#include <stdlib.h>
#include "Xtools.h"

Widget  GetTopShell(Widget widget)
{
	while (widget && !XtIsTopLevelShell(widget))
		widget = XtParent(widget);
	
	if (! XtIsTopLevelShell(widget))
		return((Widget)NULL);
		
	return(widget);
}


int     getWidgetHeight(Widget widget)
{
        Dimension       height = 0;
        Arg             arg[1];
        int             ac;

        if (XtIsWidget(widget))
        {
                ac = 0;
                XtSetArg(arg[ac], XmNheight, &height); ac++;
                XtGetValues(widget, arg, ac);
        }
        return((int) height);
}


int     getWidgetWidth(Widget widget)
{
        Dimension       width = 0;
        Arg             arg[1];
        int             ac;

        if (XtIsWidget(widget))
        {
                ac = 0;
                XtSetArg(arg[ac], XmNwidth, &width); ac++;
                XtGetValues(widget, arg, ac);
        }
        return((int) width);
}


void    setWidgetHeight(Widget widget, int height)
{
        Arg             arg[1];
        int             ac;

        if (XtIsWidget(widget))
        {
                ac = 0;
                XtSetArg(arg[ac], XmNheight, height); ac++;
                XtSetValues(widget, arg, ac);
        }
        return;
}


void    setWidgetWidth(Widget widget, int width)
{
        Arg             arg[1];
        int             ac;

        if (XtIsWidget(widget))
        {
                ac = 0;
                XtSetArg(arg[ac], XmNwidth, width); ac++;
                XtSetValues(widget, arg, ac);
        }
        return;
}



Pixel   GetBackground(Widget widget)
{
	Pixel 	bg;
	Arg	arg[1];
	int	ac;
	
	
	ac = 0;
	XtSetArg(arg[ac], XmNbackground, &bg); ac++;
	XtGetValues(widget, arg, ac);

	return bg;
}

void  SetBackground(Widget widget, Pixel bg)
{
	Arg	arg[1];
	int 	ac;
	
	ac = 0;
	XtSetArg(arg[ac], XmNbackground, bg); ac++;
	XtSetValues(widget, arg, ac);

	return;
}

Position Xpos(Widget widget)
{
        Position	x = 0;
        Arg             arg[1];
        int             ac;

        if (XtIsWidget(widget))
        {
        	ac = 0;
                XtSetArg(arg[ac], XmNx, &x); ac++;
                XtGetValues(widget, arg, ac);
        }
        return( x );
}


void    SetXpos(Widget widget, Position x)
{
        Arg             arg[1];
        int             ac;

        if (XtIsWidget(widget))
        {
                ac = 0;
                XtSetArg(arg[ac], XmNx, x); ac++;
                XtSetValues(widget, arg, ac);
        }
        return;
}


Position Ypos(Widget widget)
{
        Position	y = 0;
        Arg             arg[1];
        int             ac;

        if (XtIsWidget(widget))
        {
                ac = 0;
                XtSetArg(arg[ac], XmNx, &y); ac++;
                XtGetValues(widget, arg, ac);
        }
        
        return( y );
}


void    SetYpos(Widget widget, Position y)
{
        Arg             arg[1];
        int             ac;

        if (XtIsWidget(widget))
        {  		
                ac = 0;
                XtSetArg(arg[ac], XmNy, y); ac++;
                XtSetValues(widget, arg, ac);
        }
        
        return;
}



int	IsSensitive(Widget w)
{
	Boolean		state;
	Arg		arg[1];
	
	
	/*
		Set state to -1 as a default.  This value
		indicates that the passed argument is not
		a widget or gadget.
	*/
	state = -1;
	
	
	/*
		Ensure the passed widget parameter is a 
		valid widget or gadget.  If so, get the
		current sensitivity.
	*/
	if (XtIsWidget(w) || XmIsGadget(w))
	{
		XtSetArg(arg[0], XmNsensitive, &state);
		XtGetValues(w, arg, 1);
	}
	
	
	return(state);
}


void	Sensitize(Widget w)
{
	if (XtIsWidget(w) || XmIsGadget(w))
		XtSetSensitive(w, True);
	return;
}


void	DeSensitize(Widget w)
{
	if (XtIsWidget(w) || XmIsGadget(w))
		XtSetSensitive(w, False);
	return;
}



void  PlaceWidgets(Widget widgetList[], const long numWidgets,
		      const Dimension margin, const Dimension managerWidth)
   
{
   
   	Arg	args[10];
	int	ac,
	   	i;
	Position *posList;
   
   
	
	posList = (Position *) malloc (numWidgets*sizeof(Position));
	if (posList == NULL)
		return;
	
	
	/*
		Calculate the new positions
	*/
	GetNewPositions((const Widget *) widgetList,
			posList,
			numWidgets,
			margin,
			managerWidth);
	
	
	/*
		Set the new positions using attachments, since
		setting the x positions does not seem to work.
	*/
	for(i = 0; i < numWidgets; i++)
	{
	   	ac = 0;
		XtSetArg(args[ac], XmNleftOffset, posList[i]); ac++;
		XtSetValues(widgetList[i], args, ac);
   	}   
   
	
	/*
		Free the malloc'ed memory
	*/
	if (posList)
		free(posList);
	     
   	return;
}   


void  GetNewPositions(const Widget widgetList[], Position  posList[],
	              const long numWidgets,
		      const Dimension margin, const Dimension managerWidth)
{
   
   	long	i;
	Dimension totalWidgetWidth,
		  useableWidth = managerWidth - (2*margin),
	   	  spacingWidth,
		  widgetSpacing;
	
		
	/*
		Determine the total widths of the widgets to be placed	
	*/
	totalWidgetWidth = 0;
	for (i = 0; i < numWidgets; i++)
	{
		totalWidgetWidth += getWidgetWidth(widgetList[i]);	   
	}   
	   

	/*
		Determine the spacing between widgets
	*/
	spacingWidth = useableWidth - totalWidgetWidth;
	widgetSpacing = spacingWidth/(numWidgets - 1);

	
	/*
		Determine the new positions of the widgets
	*/
	posList[0] = margin;
	for (i=1; i < numWidgets; i++)
	{
		posList[i] = posList[i-1] + getWidgetWidth(widgetList[i-1]) +
		   	     widgetSpacing;   
	}   
	   
	return;   
}   

