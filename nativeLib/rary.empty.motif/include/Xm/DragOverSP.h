/**
 *
 * $Id: DragOverSP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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

#ifndef _XM_DRAGOVERSP_H
#define _XM_DRAGOVERSP_H

#include <X11/Shell.h>
#include <X11/ShellP.h>
#include <Xm/XmP.h>
#include <Xm/DragIconP.h>
#include <Xm/DragOverS.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DOExpose(do) \
	((XtClass(do))->core_class.expose) ((Widget)(do), NULL, NULL)

typedef struct {
    XtPointer extension;
} XmDragOverShellClassPart;

typedef struct _XmDragOverShellClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ShellClassPart shell_class;
    WMShellClassPart wm_shell_class;
    VendorShellClassPart vendor_shell_class;
    XmDragOverShellClassPart dragOver_shell_class;
} XmDragOverShellClassRec;

XMLIBEXPORT extern XmDragOverShellClassRec xmDragOverShellClassRec;

typedef struct _XmBackingRec {
    Position x, y;
    Pixmap pixmap;
} XmBackingRec, *XmBacking;

typedef struct _XmDragOverBlendRec {
    XmDragIconObject sourceIcon;	/* source icon */
    Position sourceX;			/* source location x in blend */
    Position sourceY;			/* source location y in blend */
    XmDragIconObject mixedIcon;		/* blended icon */
    GC gc;				/* blend tool (remember right depth) */
} XmDragOverBlendRec, *XmDragOverBlend;
	
typedef struct _XmDragOverShellPart {
    Position hotX;			/* current hotx */
    Position hotY;			/* current hoty */
    unsigned char cursorState;		/* current cursor state */
    unsigned char mode;			/* XmWINDOW, XmPIXMAP, XmCURSOR */
    unsigned char activeMode;		/* XmWINDOW, XmPIXMAP, XmCURSOR */
    
    Position initialX;			/* initial hotx */
    Position initialY;			/* initial hoty */
    
    XmDragIconObject stateIcon;		/* current state icon */
    XmDragIconObject opIcon;		/* current operation icon */
    
    XmDragOverBlendRec cursorBlend;	/* cursor blending */
    XmDragOverBlendRec rootBlend;	/* window or pixmap blending */
    Pixel cursorForeground;		/* guess */
    Pixel cursorBackground;		/* guess */
    Cursor ncCursor;			/* non-cached cursor */
    Cursor activeCursor;		/* the current cursor */
    
    XmBackingRec backing;		/* backing store for pixdrag ? */
    Pixmap tmpPix;			/* temp store for pixdrag ? */
    Pixmap tmpBit;			/* temp store for pixdrag ? */
    Boolean isVisible;			/* shell is visible */
} XmDragOverShellPart;

typedef struct _XmDragOverShellRec {
    CorePart core;
    CompositePart composite;
    ShellPart shell;
    WMShellPart wm;
    VendorShellPart vendor;
    XmDragOverShellPart drag;
} XmDragOverShellRec;

#ifdef __cplusplus
}
#endif

#endif /* _XM_DRAGOVERSP_H */
