/*
	File:		SetFont.c
	Author:		Dale Shelton
	Date:		7/21/94
	
	Purpose:
	
*/


#include "Xtools.h"


void    SetFont(Display *display, GC gc, char *font)
{
        XFontStruct     *fontinfo;

        if ((fontinfo = XLoadQueryFont(display, font)) == NULL)
        {
                fprintf(stderr, "SetFont: unable to load specified font.\n");
                return;
        }
        XSetFont(display, gc, fontinfo->fid);
        return;
}
