/**
 * 
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/Transfer.h,v 1.1 2004/08/28 19:23:27 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright (C) 1997-2001 LessTif Development Team
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

#ifndef _XM_TRANSFER_H
#define _XM_TRANSFER_H

#include <Xm/DragDrop.h>

#ifdef __cplusplus
extern "C" {
#endif

#define XmConvertCallbackProc XtCallbackProc
#define XmSelectionDoneProc XtSelectionDoneProc
#define XmCancelSelectionProc XtCancelConvertSelectionProc
#define XmDestinationCallbackProc XtCallbackProc
#define XmSelectionCallbackProc XtSelectionCallbackProc

typedef enum {
    XmTRANSFER_DONE_SUCCEED = 0,
    XmTRANSFER_DONE_FAIL,
    XmTRANSFER_DONE_CONTINUE,
    XmTRANSFER_DONE_DEFAULT
} XmTransferStatus;

enum {
    XmMOVE = XmDROP_MOVE,
    XmCOPY = XmDROP_COPY,
    XmLINK = XmDROP_LINK,
    XmOTHER 
};

enum {
    XmSELECTION_DEFAULT = 0,
    XmSELECTION_INCREMENTAL,
    XmSELECTION_PERSIST,
    XmSELECTION_SNAPSHOT,
    XmSELECTION_TRANSACT
};

enum {
    XmCONVERTING_NONE = 0,
    XmCONVERTING_SAME = 1,
    XmCONVERTING_TRANSACT = 2,
    XmCONVERTING_PARTIAL = 4 
};

enum {
    XmCONVERT_DEFAULT = 0,
    XmCONVERT_MORE,
    XmCONVERT_MERGE,
    XmCONVERT_REFUSE,
    XmCONVERT_DONE
};


typedef struct {
    int			reason;
    XEvent		*event;
    Atom		selection;
    Atom		target;
    XtPointer		source_data;
    XtPointer		location_data;
    int			flags;
    XtPointer		parm;
    int			parm_format;
    unsigned long	parm_length;
    Atom		parm_type;
    int			status;
    XtPointer		value;
    Atom		type;
    int			format;
    unsigned long	length;
} XmConvertCallbackStruct;

typedef struct {
    int		reason;
    XEvent	*event;
    Atom	selection;
    XtEnum	operation;
    int		flags;
    XtPointer	transfer_id;
    XtPointer	destination_data;
    XtPointer	location_data;
    Time	time;
} XmDestinationCallbackStruct;

typedef struct {
    int			reason;
    XEvent		*event;
    Atom		selection;
    Atom		target;
    Atom		type;
    XtPointer		transfer_id;
    int			flags;
    int			remaining;
    XtPointer		value;
    unsigned long	length;
    int			format;
} XmSelectionCallbackStruct;

typedef struct {
    int			reason;
    XEvent		*event;
    Atom		selection;
    XtPointer		transfer_id;
    XmTransferStatus	status;
    XtPointer		client_data;
} XmTransferDoneCallbackStruct;

typedef void (*XmSelectionFinishedProc)(Widget, XtEnum,
                                        XmTransferDoneCallbackStruct*);

XMLIBEXPORT void XmTransferDone(XtPointer, XmTransferStatus);
XMLIBEXPORT void XmTransferValue(XtPointer, Atom, XtCallbackProc, XtPointer, Time);
XMLIBEXPORT void XmTransferSetParameters(XtPointer, XtPointer, int, unsigned long, Atom);
XMLIBEXPORT void XmTransferStartRequest(XtPointer);
XMLIBEXPORT void XmTransferSendRequest(XtPointer, Time);

XMLIBEXPORT Boolean XmeClipboardSink(Widget w, XtEnum op, XtPointer location_data);
XMLIBEXPORT Boolean XmeClipboardSource(Widget w, XtEnum op, Time time);


#ifdef __cplusplus
}
#endif

#endif /* _XM_TRANSFER_H */
