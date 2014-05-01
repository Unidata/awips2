/**
 *
 * $Id: ColorObjP.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
 * 
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2000 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

#ifndef _XM_COLOROBJP_H
#define _XM_COLOROBJP_H

#include <X11/Intrinsic.h>

#ifdef __cplusplus
extern "C"
{
#endif

/*
 * structures and macros
 */
typedef struct
    {
      Pixel fg;
      Pixel bg;
      Pixel ts;
      Pixel bs;
      Pixel sc;
    }
  XmPixelSet;

XMLIBEXPORT Boolean
XmeGetDesktopColorCells (Screen * screen,
                         Colormap colormap,
                         XColor * colors,
                         int n_colors, int *n_colors_ret);

XMLIBEXPORT Boolean
XmeGetColorObjData (int *screen,
                    int *coloruse,
                    XmPixelSet * pixel_set,
                    unsigned short pixel_set_size,
                    short *active, short *inactive,
                    short *primary, short *secondary,
                    short *text);

/* possible return values for coloruse parameter of XmeGetColorObjData(): */

enum {
    XmCO_BLACK_WHITE,
    XmCO_LOW_COLOR,
    XmCO_MEDIUM_COLOR,
    XmCO_HIGH_COLOR
};

#ifdef __cplusplus
}
#endif

#endif				/* _XM_COLOROBJP_H */
