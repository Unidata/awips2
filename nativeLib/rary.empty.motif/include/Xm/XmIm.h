/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/XmIm.h,v 1.1 2004/08/28 19:23:27 dannybackx Exp $
 *
 * Copyright (C) 2000 LessTif Development Team
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

#ifndef _XM_XMIM_H
#define _XM_XMIM_H

#ifdef __cplusplus
extern "C" {
#endif

XMLIBEXPORT void XmImCloseXIM(Widget w);

XMLIBEXPORT void XmImFreeXIC(Widget w, XIC xic);

XMLIBEXPORT XIC XmImGetXIC(Widget w,
               XmInputPolicy input_policy,
               ArgList args,
               Cardinal num_args);

XMLIBEXPORT XIM XmImGetXIM(Widget w);

XMLIBEXPORT int XmImMbLookupString(Widget w,
                       XKeyPressedEvent *evp,
                       char *buf,
                       int nbytes,
                       KeySym *keysym,
                       int *status);

XMLIBEXPORT void XmImMbResetIC(Widget widget,
                   char **mb);

XMLIBEXPORT void XmImRegister(Widget w,
                  unsigned int reserved);

XMLIBEXPORT void XmImSetFocusValues(Widget w,
                        ArgList args,
                        Cardinal num_args);

XMLIBEXPORT void XmImSetValues(Widget w,
                   ArgList args,
                   Cardinal num_args);

XMLIBEXPORT XIC XmImSetXIC(Widget w, XIC xic);

XMLIBEXPORT void XmImRegister(Widget w,
                  unsigned int reserved);

XMLIBEXPORT void XmImUnregister(Widget w);

XMLIBEXPORT void XmImUnsetFocus(Widget w);

XMLIBEXPORT void XmImVaSetFocusValues(Widget w, ...);

XMLIBEXPORT void XmImVaSetValues(Widget w, ...);

#ifdef __cplusplus
}
#endif

#endif /* _XM_XMIM_H */
