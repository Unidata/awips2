/* File: get_pixel.c
 *
 *  Obtains the color pixel value given the widget and the name
 *  of the color.
 *
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <X11/StringDefs.h>

int get_pixel_by_name(w, color_name)

Widget  w;
char    *color_name;
{
	Display         *dpy    = XtDisplay(w);
	int             scr     = DefaultScreen(dpy);
	Colormap        cmap    = DefaultColormap(dpy, scr);
	XColor          color, ignore;
	/*
	 *      Alloc named color
	 */
	if(XAllocNamedColor(dpy, cmap, color_name, &color, &ignore))
		return (color.pixel);
	else {
		printf("Warning:  Couldn't alloc color %s\n",color_name);
		return (BlackPixel(dpy, scr));
	     }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/ifp/src/Xifp/RCS/get_pixel.c,v $";
 static char rcs_id2[] = "$Id: get_pixel.c,v 1.2 2007/05/16 16:48:24 aivo Exp $";}
/*  ===================================================  */

}
