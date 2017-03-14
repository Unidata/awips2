/**
 *
 * $Id: List.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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

#ifndef _XM_LIST_H
#define _XM_LIST_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

XMLIBEXPORT extern WidgetClass xmListWidgetClass;

typedef struct _XmListRec *XmListWidget;
typedef struct _XmListClassRec *XmListWidgetClass;

#ifndef XmIsList
#define XmIsList(w) XtIsSubclass((w), xmListWidgetClass)
#endif

/* selection type values */ 

enum {
    XmINITIAL,
    XmMODIFICATION,
    XmADDITION 
};

XMLIBEXPORT extern Widget XmCreateList(Widget parent,
			   char *name,
			   Arg *arglist,
			   Cardinal argCount);
XMLIBEXPORT extern Widget XmCreateScrolledList(Widget parent,
				   char *name,
				   Arg *arglist,
				   Cardinal argCount);
XMLIBEXPORT extern void XmListAddItem(Widget widget,
			  XmString item,
			  int position);
XMLIBEXPORT extern void XmListAddItems(Widget widget,
			   XmString *items,
			   int item_count,
			   int position);
XMLIBEXPORT extern void XmListAddItemUnselected(Widget widget,
				    XmString item,
				    int position);
XMLIBEXPORT extern void XmListAddItemsUnselected(Widget widget,
				     XmString *items,
				     int item_count,
				     int position);
XMLIBEXPORT extern void XmListDeleteAllItems(Widget widget);
XMLIBEXPORT extern void XmListDeleteItem(Widget widget,
			     XmString item);
XMLIBEXPORT extern void XmListDeleteItems(Widget widget,
			      XmString *items,
			      int item_count);
XMLIBEXPORT extern void XmListDeleteItemsPos(Widget widget,
				 int item_count,
				 int position);
XMLIBEXPORT extern void XmListDeletePos(Widget widget,
			    int position);
XMLIBEXPORT extern void XmListDeletePositions(Widget widget,
				  int *position_list,
				  int position_count);
XMLIBEXPORT extern void XmListDeselectAllItems(Widget widget);
XMLIBEXPORT extern void XmListDeselectItem(Widget widget,
			       XmString item);
XMLIBEXPORT extern void XmListDeselectPos(Widget widget,
			      int position);
XMLIBEXPORT extern int XmListGetKbdItemPos(Widget widget);
XMLIBEXPORT extern Boolean XmListGetMatchPos(Widget widget,
				 XmString item,
				 int **position_list,
				 int *position_count);
XMLIBEXPORT extern Boolean XmListGetSelectedPos(Widget widget,
				    int **position_list,
				    int *position_count);
XMLIBEXPORT extern Boolean XmListItemExists(Widget widget,
				XmString item);
XMLIBEXPORT extern int XmListItemPos(Widget widget,
			 XmString item);
XMLIBEXPORT extern Boolean XmListPosSelected(Widget widget,
				 int position);
XMLIBEXPORT extern Boolean XmListPosToBounds(Widget widget,
				 int position,
				 Position *x,
				 Position *y,
				 Dimension *width,
				 Dimension *height);
XMLIBEXPORT extern void XmListReplaceItems(Widget widget,
			       XmString *old_items,
			       int item_count,
			       XmString *new_items);
XMLIBEXPORT extern void XmListReplaceItemsPos(Widget widget,
				  XmString *new_items,
				  int item_count,
				  int position);
XMLIBEXPORT extern void XmListReplaceItemsPosUnselected(Widget widget,
					    XmString *new_items,
					    int item_count,
					    int position);
XMLIBEXPORT extern void XmListReplaceItemsUnselected(Widget widget,
					 XmString *old_items,
					 int item_count,
					 XmString *new_items);
XMLIBEXPORT extern void XmListReplacePositions(Widget widget,
				   int *position_list,
				   XmString *item_list,
				   int item_count);
XMLIBEXPORT extern void XmListSelectItem(Widget widget,
			     XmString item,
			     Boolean notify);
XMLIBEXPORT extern void XmListSelectPos(Widget widget,
			    int position,
			    Boolean notify);
XMLIBEXPORT extern void XmListSetAddMode(Widget widget,
			     Boolean mode);
XMLIBEXPORT extern void XmListSetBottomItem(Widget widget,
				XmString item);
XMLIBEXPORT extern void XmListSetBottomPos(Widget widget,
			       int position);
XMLIBEXPORT extern void XmListSetHorizPos(Widget widget,
			      int position);
XMLIBEXPORT extern void XmListSetItem(Widget widget,
			  XmString item);
XMLIBEXPORT extern Boolean XmListSetKbdItemPos(Widget widget,
				   int position);
XMLIBEXPORT extern void XmListSetPos(Widget widget,
			 int position);
XMLIBEXPORT extern void XmListUpdateSelectedList(Widget widget);
XMLIBEXPORT extern int XmListYToPos(Widget widget,
			Position y);


#ifdef __cplusplus
}
#endif

#endif /* _XM_LIST_H */
