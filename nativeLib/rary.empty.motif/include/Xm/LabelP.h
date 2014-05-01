/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/LabelP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995-2000, 2002 LessTif Development Team
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

#ifndef _XM_LABELP_H
#define _XM_LABELP_H

#include <Xm/Label.h>
#include <Xm/PrimitiveP.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef XmUNSPECFIED
#define XmUNSPECIFIED	(~0)
#endif

#define LABEL_ACC_PAD	15

typedef struct {
	_XmString _label;
	_XmString _acc_text;
	KeySym mnemonic;
	XmStringCharSet mnemonicCharset;
	String accelerator;
	unsigned char label_type;
	unsigned char alignment;
	XmStringDirection string_direction;
	XmFontList font;

	Dimension margin_height;
	Dimension margin_width;

	Dimension margin_left;
	Dimension margin_right;
	Dimension margin_top;
	Dimension margin_bottom;

	Boolean recompute_size;

	Pixmap pixmap;
	Pixmap pixmap_insen;
 
	/* private instance variables */
	GC normal_GC;
	GC insensitive_GC;

	XRectangle TextRect;
	XRectangle acc_TextRect;

	Boolean skipCallback;

	unsigned char menu_type;  /* if inside a RowColumn, its row_column_type */

	/* 2.x resources */
	Boolean	check_set_render_table;
} XmLabelPart;

/* Define the full instance record */
typedef struct _XmLabelRec {
	CorePart core;
	XmPrimitivePart primitive;
	XmLabelPart label;
} XmLabelRec;

/* Define class part structure */
typedef struct {
	XtWidgetProc setOverrideCallback;
	XmMenuProc menuProcs;
	String translations;
	XtPointer extension;
} XmLabelClassPart;

/* Define the full class record */
typedef struct _XmLabelClassRec {
	CoreClassPart core_class;
	XmPrimitiveClassPart primitive_class;
	XmLabelClassPart label_class;
} XmLabelClassRec;

/* External definition for class record */

XMLIBEXPORT extern XmLabelClassRec xmLabelClassRec;

/* private prototypes */

void _XmCalcLabelDimensions(Widget w);

#define Lab_MarginWidth(w) \
       (((XmLabelWidget)(w))->label.margin_width)

#define Lab_MarginHeight(w) \
       (((XmLabelWidget)(w))->label.margin_height)

#define Lab_MarginTop(w) \
       (((XmLabelWidget)(w))->label.margin_top)

#define Lab_MarginBottom(w) \
       (((XmLabelWidget)(w))->label.margin_bottom)

#define Lab_MarginRight(w) \
       (((XmLabelWidget)(w))->label.margin_right)

#define Lab_MarginLeft(w) \
       (((XmLabelWidget)(w))->label.margin_left)

#define Lab_TextRect_x(w) \
       (((XmLabelWidget)(w))->label.TextRect.x)

#define Lab_TextRect_y(w) \
       (((XmLabelWidget)(w))->label.TextRect.y)

#define Lab_TextRect_width(w) \
       (((XmLabelWidget)(w))->label.TextRect.width)

#define Lab_TextRect_height(w) \
       (((XmLabelWidget)(w))->label.TextRect.height)

#define Lab_IsText(w) \
       (((XmLabelWidget)(w))->label.label_type == XmSTRING)

#define Lab_IsPixmap(w) \
       (((XmLabelWidget)(w))->label.label_type == XmPIXMAP)

#define Lab_Font(w) \
       (((XmLabelWidget)(w))->label.font)

#define Lab_Mnemonic(w) \
       (((XmLabelWidget)(w))->label.mnemonic)

#define Lab_Accelerator(w) \
       (((XmLabelWidget)(w))->label.accelerator)

#define Lab_AcceleratorText(w) \
       (((XmLabelWidget)(w))->label._acc_text)

#define Lab_MenuType(w) \
       (((XmLabelWidget)(w))->label.menu_type)

#define Lab_Shadow(w) \
       (((XmLabelWidget)(w))->primitive.shadow_thickness)

#define Lab_Highlight(w) \
       (((XmLabelWidget)(w))->primitive.highlight_thickness)

#define Lab_Baseline(w) \
       (XmStringBaseline ( \
               ((XmLabelWidget)(w))->label.font,\
               ((XmLabelWidget)(w))->label._label))

#ifdef __cplusplus
}
#endif

#endif /* _XM_LABELP_H */
