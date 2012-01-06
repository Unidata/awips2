/**
 *
 * $Id: ExtObjectP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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

#ifndef _XM_EXTOBJECTP_H
#define _XM_EXTOBJECTP_H

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif

enum {
    XmCACHE_EXTENSION = 1,
    XmDESKTOP_EXTENSION,
    XmSHELL_EXTENSION,
    XmPROTOCOL_EXTENSION,
    XmDEFAULT_EXTENSION
};

#ifndef XmIsExtObject
#define XmIsExtObject(w) XtIsSubclass(w, xmExtObjectClass)
#endif

/*
 * Class record constants
 */
typedef struct _XmExtRec *XmExtObject;
typedef struct _XmExtClassRec *XmExtObjectClass;

XMLIBEXPORT extern WidgetClass xmExtObjectClass;

#define XmNUM_ELEMENTS 4
/*
 * As suggested by <gatgul@voicenet.com>, define this so that the structure
 * XmExtCache has a size which is a multiple of the size of a pointer.
 * Use a simpler definition than what he suggested though.
 */
#define XmNUM_BYTES (25 * sizeof(void *) - sizeof(Boolean))

/*
 * Class Extension definitions
 */
typedef struct _XmExtClassPart {
    XmSyntheticResource *syn_resources;   
    int num_syn_resources;   
#ifdef MOTIF_DOES_THIS_SO_WE_WILL_TOO
    XtResourceList ext_resources;
    XtResourceList compiled_ext_resources;
    Cardinal num_ext_resources;
    Boolean use_sub_resources;
#endif /* notdef */
    XtPointer extension;
} XmExtClassPart, *XmExtClassPartPtr;

typedef struct _XmExtClassRec {
    ObjectClassPart object_class;
    XmExtClassPart ext_class;
} XmExtClassRec;

typedef struct {
    Widget logicalParent;
    unsigned char extensionType;
} XmExtPart, *XmExtPartPtr;

XMLIBEXPORT extern XmExtClassRec xmExtClassRec;

typedef struct _XmExtRec {
    ObjectPart object;
    XmExtPart ext;
} XmExtRec;

typedef struct _XmExtCache {
   char data[XmNUM_BYTES];
   Boolean inuse;
} XmExtCache;

XMLIBEXPORT extern char *_XmExtObjAlloc(Cardinal size);
XMLIBEXPORT extern void _XmExtObjFree(XtPointer element);
XMLIBEXPORT extern void _XmBuildExtResources(WidgetClass c);

#ifdef __cplusplus
}
#endif

#endif /* _XM_EXTOBJECTP_H */

