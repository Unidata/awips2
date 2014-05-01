/*
	File:		Cursor.c
	Author:		Mark Glaudemans
	Date:		9/13/1995
	
	Purpose:	Implements convenience functions to 
			change the screen cursor.
	
*/

#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>

#include "Xtools.h"


/*  
	Sets a cursor to a specified type.
*/
void SetCursor(Widget w, int cursor_type)
{
	Widget shell;
	Display *display;
	Window	window;
	Cursor	cursor;

	if ( ( w != NULL ) &&  ( XtIsWidget(w) ) )
	{
		shell = GetTopShell(w);
		display = XtDisplay(w);
		window = XtWindow(w);
		
		cursor = XCreateFontCursor(display, cursor_type);
		XDefineCursor(display, window, cursor);
		XmUpdateDisplay(shell);
		
        }

        return;
}


/*
	Restores the parent cursor by unsetting the
	existing cursor. 
*/
void UnsetCursor(Widget w)
{
	Widget shell;
	Display *display;
	Window	window;

	if ( ( w != NULL ) && ( XtIsWidget ( w ) ) )
	{
		shell = GetTopShell(w);
		display = XtDisplay(w);
		window = XtWindow(w);
		
		XUndefineCursor(display, window);
		XmUpdateDisplay(shell);
		
        }

        return;
}
