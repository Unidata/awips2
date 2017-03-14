/* 
 * Motif Tools Library, Version 2.0
 * $Id: Wait.c,v 1.1 2006/10/03 13:01:44 lawrence Exp $
 * 
 * Written by David Flanagan.
 * Copyright (c) 1992, 1993, 1994 by Dovetail Systems.
 * All Rights Reserved.  See the file COPYRIGHT for details.
 * This is not free software.  See the file SHAREWARE for details.
 * There is no warranty for this software.  See NO_WARRANTY for details.
 

#include <Xmt/Xmt.h>
#include <Xmt/Util.h>



 * The following procedure is by David Brooks of the OSF and appears in
 * the Motif FAQ list.  It has been renamed for the Xmt library.
 */

/*
 * This procedure will ensure that, if a dialog window is being mapped,
 * its contents become visible before returning.  It is intended to be
 * used just before a bout of computing that doesn't service the display.
 * You should still call XmUpdateDisplay() at intervals during this
 * computing if possible.
 *
 * The monitoring of window states is necessary because attempts to map
 * the dialog are redirected to the window manager (if there is one) and
 * this introduces a significant delay before the window is actually mapped
 * and exposed.  This code works under mwm, twm, uwm, and no-wm.  It
 * doesn't work (but doesn't hang) with olwm if the mainwindow is iconified.
 *
 * The argument to ForceDialog is any widget in the dialog (often it
 * will be the BulletinBoard child of a DialogShell).
 */

#include <Xm/Xm.h>

void XmtWaitUntilMapped(Widget w)

{
    Widget diashell, topshell;
    Window diawindow, topwindow;
    Display *dpy;
    XWindowAttributes xwa;
    XEvent event;
    XtAppContext cxt;
    
    for (diashell = w; !XtIsShell(diashell); diashell = XtParent(diashell)) ;
    for (topshell = diashell;
	 !XtIsTopLevelShell(topshell);
	 topshell = XtParent(topshell));
    
    if (XtIsRealized(diashell) && XtIsRealized(topshell)) {
	dpy = XtDisplay(topshell);
	diawindow = XtWindow(diashell);
	topwindow = XtWindow(topshell);
	cxt = XtWidgetToApplicationContext(diashell);
	
	/* Wait for the dialog to be mapped. */
	while (XGetWindowAttributes(dpy, diawindow, &xwa),
	       xwa.map_state != IsViewable) {
	    
	    /*
	     * unless the primary is (or becomes) unviewable or unmapped, it's
	     * probably iconified, and nothing will happen.
	     * We make an exception to this case when topwindow==diawindow
	     */
	    if ((topwindow != diawindow) &&
		(XGetWindowAttributes(dpy, topwindow, &xwa),
		 xwa.map_state != IsViewable))
		break;

	    /*
	     * At this stage, we are guaranteed there will be
	     * an event of some kind.  Beware; we are presumably
	     * in a callback, so this can recurse.
	     */
	    XtAppNextEvent(cxt, &event);
	    XtDispatchEvent(&event);
	}
    }
    
    /* The next XSync() will get an expose event if the dialog was unmapped. */
    XmUpdateDisplay(topshell);
}
