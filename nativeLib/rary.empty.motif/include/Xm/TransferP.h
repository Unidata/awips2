/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/TransferP.h,v 1.1 2004/08/28 19:23:27 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright (C) 1997-2000 LessTif Development Team
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


#ifndef _XM_TRANSFERP_H
#define _XM_TRANSFERP_H

#include <Xm/Transfer.h>

#ifdef __cplusplus
extern "C" {
#endif

XMLIBEXPORT Atom *XmeStandardTargets(Widget widget,
                         int count, int *count_return);

XMLIBEXPORT void XmeStandardConvert(Widget widget,
                        XtPointer ignore,
			XmConvertCallbackStruct *call_data);

XMLIBEXPORT void XmeTransferAddDoneProc(XtPointer transfer_id,
                            XmSelectionFinishedProc done_proc);

XMLIBEXPORT Boolean XmeSecondarySink(Widget widget, Time time);

XMLIBEXPORT Boolean XmeSecondarySource(Widget widget,Time time);

XMLIBEXPORT void XmeSecondaryTransfer(Widget widget, Atom target,
                          XtEnum op, Time time);

XMLIBEXPORT Widget XmeDragSource(Widget widget, XtPointer location_data,
                     XEvent *event, ArgList args, Cardinal arg_count);

XMLIBEXPORT Boolean XmeNamedSink(Widget widget, Atom named_selection,
                     XtEnum op, XtPointer location_data,
                     Time time);

XMLIBEXPORT Boolean XmeNamedSource(Widget widget, Atom named_selection, 
                       Time time);

XMLIBEXPORT Boolean XmePrimarySink(Widget widget, XtEnum op, XtPointer location_data,
                       Time time);

XMLIBEXPORT Boolean XmePrimarySource(Widget widget, Time time);

XMLIBEXPORT Atom XmeGetEncodingAtom(Widget widget);

XMLIBEXPORT void XmeDropSink(Widget widget,
                 ArgList args,
                 Cardinal arg_count);


#ifdef __cplusplus
}
#endif

#endif /* _XM_TRANSFERP_H */
