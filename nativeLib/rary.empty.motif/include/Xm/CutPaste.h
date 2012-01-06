/**
 *
 * $Id: CutPaste.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
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

#ifndef _XM_CUTPASTE_H
#define _XM_CUTPASTE_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

enum {
    XmClipboardFail,
    XmClipboardSuccess,
    XmClipboardTruncate,

    XmClipboardLocked = 4,

    XmClipboardBadFormat,
    XmClipboardNoData
};

/* These are for compatibility with pre-1.2 values */
enum {
    ClipboardFail,
    ClipboardSuccess,
    ClipboardTruncate,

    ClipboardLocked = 4,

    ClipboardBadFormat,
    ClipboardNoData
};

typedef struct {
    long DataId;
    long PrivateId;
} XmClipboardPendingRec, *XmClipboardPendingList;

typedef void (*XmCutPasteProc)(Widget, int *, int *, int *);
typedef void (*VoidProc)(Widget w, int *data_id, int *private_id, int *reason);

#if XmVERSION == 1
XMLIBEXPORT int XmClipboardBeginCopy(Display *display, Window window,
			 XmString clip_label, Widget widget,
			 VoidProc callback, long *item_id);
#endif
XMLIBEXPORT int XmClipboardCancelCopy(Display *display, Window window, long item_id);
XMLIBEXPORT int XmClipboardCopy(Display *display, Window window, long item_id, char *format_name,
		    XtPointer buffer, unsigned long length, long private_id, long *data_id);
XMLIBEXPORT int XmClipboardCopyByName(Display *display, Window window, long data_id,
			  XtPointer buffer, unsigned long length, long private_id);
XMLIBEXPORT int XmClipboardEndCopy(Display *display, Window window, long item_id);
XMLIBEXPORT int XmClipboardEndRetrieve(Display *display, Window window);
XMLIBEXPORT int XmClipboardInquireCount(Display *display, Window window, int *count, unsigned long *max_length);
XMLIBEXPORT int XmClipboardInquireFormat(Display *display, Window window, int index, XtPointer format_name_buf,
			     unsigned long buffer_len, unsigned long *copied_len);
XMLIBEXPORT int XmClipboardInquireLength(Display *display, Window window, char *format_name, unsigned long *length);
XMLIBEXPORT int XmClipboardInquirePendingItems(Display *display, Window window, char *format_name, 
				   XmClipboardPendingList *item_list, unsigned long *count);
XMLIBEXPORT int XmClipboardLock(Display *display, Window window);
XMLIBEXPORT int XmClipboardRegisterFormat(Display *display, char *format_name, int format_length);
XMLIBEXPORT int XmClipboardRetrieve(Display *display, Window window, char *format_name, XtPointer buffer, 
			unsigned long length, unsigned long *num_bytes, long *private_id);
XMLIBEXPORT int XmClipboardStartCopy(Display *display, Window window, XmString clip_label, Time timestamp,
			 Widget widget, XmCutPasteProc callback, long *item_id);
XMLIBEXPORT int XmClipboardStartRetrieve(Display *display, Window window, Time timestamp);
XMLIBEXPORT int XmClipboardUndoCopy(Display *display, Window window);
XMLIBEXPORT int XmClipboardUnlock(Display *display, Window window, Boolean remove_all_locks);
XMLIBEXPORT int XmClipboardWithdrawFormat(Display *display, Window window, int data_id);

#ifdef __cplusplus
}
#endif

#endif /* _XM_CUTPASTE_H */
