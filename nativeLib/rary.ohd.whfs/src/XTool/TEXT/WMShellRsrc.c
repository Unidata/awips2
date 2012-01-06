/*
	File:		WMShellRsrc.c
	Author:		Dale Shelton
	Date:		7/21/94
	
	Purpose:
	
*/


#include "Xtools.h"


/*
	Sets the title of a widget derived from
	a WMShell widget type.
*/
void 	SetTitle(Widget widget, char *string)
{
        XmString 	str;
        Arg 		arg[2];
        int 		ac;


	ac = 0;
	if (XtIsSubclass(widget, wmShellWidgetClass))
	{
        	XtSetArg(arg[ac], XmNtitle, string);  ac++;
        	XtSetValues(widget, arg, ac);
        }       
  
        
        if (XtIsSubclass(widget, xmBulletinBoardWidgetClass)) 
        {
                str = XmStringCreateSimple(string);
        	XtSetArg(arg[ac], XmNdialogTitle, str);  ac++;
        	XtSetValues(widget, arg, ac);
        	XmStringFree(str);        
        }


        return;
}
