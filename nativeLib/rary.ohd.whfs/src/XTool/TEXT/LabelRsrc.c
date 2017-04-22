/*
	File:		LabelRsrc.c
	Author:		Dale Shelton
	Date:		7/21/94
	
	Purpose:	Implements convenience functions to get
			and set the value of a Motif XmLabel.
	
*/

#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include "Xtools.h"


/*  
	Sets a label widget's text to string.
*/
void SetLabel(Widget widget, char *string)
{
        XmString str;
        Arg arg[1];
	int ac = 0;

	if (XtIsSubclass(widget, xmLabelWidgetClass))
	{
        	str = XmStringCreateSimple(string);
        	XtSetArg(arg[ac], XmNlabelString, str);  ac++;
        	XtSetValues(widget, arg, ac);

		if (str)
        		XmStringFree(str);
        }
        return;
}

/*  
	Sets a label widget's text to string.
*/
void SetLabelWithFont(Widget widget, char *string, char* fontlist_tag)
{
        XmString str;
        Arg arg[1];
	int ac = 0;

	if (XtIsSubclass(widget, xmLabelWidgetClass))
	{
        	str = XmStringCreate(string, fontlist_tag);
        	XtSetArg(arg[ac], XmNlabelString, str);  ac++;
        	XtSetValues(widget, arg, ac);

		if (str)
        		XmStringFree(str);
        }
        return;
}

/*
	Gets a label widget's text.  Memory allocation is
	handled by Label widget and SHOULD NOT be freed...
*/
char *	GetLabel(Widget widget)
{
	XmStringCharSet	charset = XmSTRING_DEFAULT_CHARSET;
	XmString	xmStr;
	Arg		arg[1];
	int		ac;
	char		*text;

	
	if (XtIsSubclass(widget, xmLabelWidgetClass))
	{
		text = 0;
		ac = 0;
		XtSetArg(arg[ac], XmNlabelString, &xmStr); ac++;
		XtGetValues(widget, arg, ac);
		XmStringGetLtoR(xmStr, charset, &text);
	}
	return(text);
}



