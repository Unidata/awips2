/*
	File:		CompositeRsrc.c
	Date:		12/27/1994
	Author:		Dale Shelton

	Purpose:	Encapsulates the Composite widget
			resources and associated 
			functionality.

*/

#include "Xtools.h"



/*
	Destroys the children of 
	the passed Composite widget parameter.
*/
void	DestroyChildren(Widget w)
{
	WidgetList	widgetList;
	Arg		arg[3];
	int		cnt,
			i;


	if (XtIsComposite(w))
	{
        	/*
			Get a list of the widget children
		*/
		XtSetArg(arg[0], XmNchildren, &widgetList);
		XtSetArg(arg[1], XmNnumChildren, &cnt);
		XtGetValues(w, arg, 2);


		/*
			Loop through all widgets and 
			destroy them in order.
		*/
		for (i = 0; i < cnt; i++)
			XtDestroyWidget(widgetList[i]);
	}

	return;
}


void   GetWidgetChildren(Widget parent, WidgetList *childList, int *childCount)
{
     Arg		arg[3];
  
     if (XtIsComposite(parent))
     {
	  /*
	  Get a list of the widget children
	  */
	  XtSetArg(arg[0], XmNchildren, childList);
	  XtSetArg(arg[1], XmNnumChildren, childCount);
	  XtGetValues(parent, arg, 2);
	  
     }
     
     return;   
}
