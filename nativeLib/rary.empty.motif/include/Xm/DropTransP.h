/**
 *
 * $Id: DropTransP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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

#ifndef _XM_DROPTRANSP_H
#define _XM_DROPTRANSP_H

#include <Xm/DropTrans.h>
#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef Widget (*XmDropTransferStartTransferProc)(Widget,
						  ArgList, 
						  Cardinal);
typedef void (*XmDropTransferAddTransferProc)(Widget,
					      XmDropTransferEntry, 
					      Cardinal);
   
typedef struct _XmDropTransferClassPart {
    XmDropTransferStartTransferProc start_drop_transfer;
    XmDropTransferAddTransferProc add_drop_transfer;
    XtPointer extension;
} XmDropTransferClassPart;
   
typedef struct _XmDropTransferClassRec {
    ObjectClassPart object_class;
    XmDropTransferClassPart dropTransfer_class;
} XmDropTransferClassRec;
   
XMLIBEXPORT extern XmDropTransferClassRec xmDropTransferClassRec;
   
typedef struct _XmDropTransferListRec {
    XmDropTransferEntry transfer_list;
    Cardinal num_transfers;
} XmDropTransferListRec, *XmDropTransferList;

typedef struct _XmDropTransferPart {
    XmDropTransferEntry drop_transfers;
    Cardinal num_drop_transfers;
    Atom selection;
    Widget dragContext;
    Time timestamp;
    Boolean incremental;
    Window source_window;
    unsigned int tag;
    XtSelectionCallbackProc transfer_callback;
    unsigned char transfer_status;
   
    Atom motif_drop_atom;
   
    XmDropTransferList drop_transfer_lists;
    Cardinal num_drop_transfer_lists;
    Cardinal cur_drop_transfer_list;
    Cardinal cur_xfer;
    Atom *cur_targets;
    XtPointer *cur_client_data;
} XmDropTransferPart;

typedef struct _XmDropTransferRec {
    ObjectPart object;
    XmDropTransferPart dropTransfer;
} XmDropTransferRec;

#ifdef __cplusplus
}
#endif

#endif /* _XM_DROPTRANSP_H */
