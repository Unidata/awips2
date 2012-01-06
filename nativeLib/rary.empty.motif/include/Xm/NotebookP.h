/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/NotebookP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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

#ifndef _XM_NOTEBOOKP_H
#define _XM_NOTEBOOKP_H

#include <Xm/ManagerP.h>
#include <Xm/Notebook.h>
#include <Xm/ScrollFrameT.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Notebook's constraint info. fields */
typedef struct _XmNotebookConstraintPart
{
    int page_number;
    unsigned char child_type;
    Boolean resizable;
    Boolean active;
} XmNotebookConstraintPart, *XmNotebookConstraint;

typedef struct _XmNotebookConstraintRec
{
    XmManagerConstraintPart manager;
    XmNotebookConstraintPart notebook;
} XmNotebookConstraintRec, *XmNotebookConstraintPtr;

/* Define the notebook instance part */
typedef struct {
        int current_page_number;
	int first_page_number;              
	int last_page_number;               
	unsigned char orientation;          
	unsigned char back_page_pos;        
	Cardinal back_page_number;          
	Dimension back_page_size;           
	Pixel back_page_foreground;         
	Pixel back_page_background;         
	Pixel frame_background;             
	unsigned char binding_type;         
	Pixmap binding_pixmap;              
	Pixmap spiral_pixmap;               
	Dimension binding_width;            
	Dimension margin_width;             
	Dimension margin_height;            
	Dimension major_spacing;            
	Dimension minor_spacing;            
	Dimension shadow_thickness;         
	XtCallbackList page_change_callback;
	Widget scroller;                    
	Widget scroller_child;              
	Widget next_major;                  
	Widget prev_major;                  
	Widget next_minor;                  
	Widget prev_minor;                  
	Dimension real_binding_width;       
	Dimension real_back_page_number;    
	Dimension page_width;               
	Dimension page_height;              
	Dimension status_width;             
	Dimension status_height;            
	Dimension major_width;              
	Dimension major_height;             
	Dimension minor_width;              
	Dimension minor_height;             
	Dimension scroller_width;           
	Dimension scroller_height;          
	Dimension major_scroller_width;     
	Dimension major_scroller_height;    
	Dimension minor_scroller_width;     
	Dimension minor_scroller_height;    
	Dimension frame_width;              
	Dimension frame_height;             
	Widget first_major;                 
	Widget old_top_major;               
	Widget top_major;                   
	Widget last_major;                  
	Widget first_minor;                 
	Widget old_top_minor;               
	Widget top_minor;                   
	Widget last_minor;                  
	Widget constraint_child;            
	Dimension major_shadow_thickness;   
	Dimension minor_shadow_thickness;   
	Widget major_shadow_child;          
	Widget minor_shadow_child;          
	Boolean in_setshadow;               
	unsigned char major_pos;            
	unsigned char minor_pos;            
	unsigned char binding_pos;          
	unsigned char which_tab;            
	int last_alloc_num;                 
	unsigned char scroller_status;      
	unsigned short need_scroller;       
	Boolean dynamic_last_page_num;      
	Boolean in_callback;                
	GC back_page_gc;                    
	GC frame_gc;                        
	GC binding_gc;                      
	GC foreground_gc;                   
	GC background_gc;                   
	Boolean first_change_managed;       
	XmScrollFrameData scroll_frame_data; 
} XmNotebookPart;

/* Define the full instance record */
typedef struct _XmNotebookRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmNotebookPart notebook;
} XmNotebookRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmNotebookClassPart;

/* Define the full class record */
typedef struct _XmNotebookClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmNotebookClassPart notebook_class;
} XmNotebookClassRec;

XMLIBEXPORT extern XmNotebookClassRec xmNotebookClassRec;

/*
 * actions
 */

/*
 * Access macros
 */
#define NB_FirstPageNumber(w)		\
			(((XmNotebookWidget)w)->notebook.first_page_number)
#define NB_LastPageNumber(w)		\
			(((XmNotebookWidget)w)->notebook.last_page_number)
#define NB_Orientation(w)		\
			(((XmNotebookWidget)w)->notebook.orientation)
#define NB_BackPagePos(w)		\
			(((XmNotebookWidget)w)->notebook.back_page_pos)
#define NB_BackPageNumber(w)		\
			(((XmNotebookWidget)w)->notebook.back_page_number)
#define NB_BackPageSize(w)		\
			(((XmNotebookWidget)w)->notebook.back_page_size)
#define NB_BackPageForeground(w)	\
			(((XmNotebookWidget)w)->notebook.back_page_foreground)
#define NB_BackPageBackground(w)	\
			(((XmNotebookWidget)w)->notebook.back_page_background)
#define NB_FrameBackground(w)		\
			(((XmNotebookWidget)w)->notebook.frame_background)
#define NB_BindingType(w)		\
			(((XmNotebookWidget)w)->notebook.binding_type)
#define NB_BindingPixmap(w)		\
			(((XmNotebookWidget)w)->notebook.binding_pixmap)
#define NB_SpiralPixmap(w)		\
			(((XmNotebookWidget)w)->notebook.spiral_pixmap)
#define NB_BindingWidth(w)		\
			(((XmNotebookWidget)w)->notebook.binding_width)
#define NB_MarginWidth(w)		\
			(((XmNotebookWidget)w)->notebook.margin_width)
#define NB_MarginHeight(w)		\
			(((XmNotebookWidget)w)->notebook.margin_height)
#define NB_MajorSpacing(w)		\
			(((XmNotebookWidget)w)->notebook.major_spacing)
#define NB_MinorSpacing(w)		\
			(((XmNotebookWidget)w)->notebook.minor_spacing)
#define NB_ShadowThickness(w)		\
			(((XmNotebookWidget)w)->notebook.shadow_thickness)
#define NB_PageChangeCallback(w)	\
			(((XmNotebookWidget)w)->notebook.page_change_callback)
#define NB_Scroller(w)			\
			(((XmNotebookWidget)w)->notebook.scroller)
#define NB_ScrollerChild(w)		\
			(((XmNotebookWidget)w)->notebook.scroller_child)
#define NB_NextMajor(w)			\
			(((XmNotebookWidget)w)->notebook.next_major)
#define NB_PrevMajor(w)			\
			(((XmNotebookWidget)w)->notebook.prev_major)
#define NB_NextMinor(w)			\
			(((XmNotebookWidget)w)->notebook.next_minor)
#define NB_PrevMinor(w)			\
			(((XmNotebookWidget)w)->notebook.prev_minor)
#define NB_RealBindingWidth(w)		\
			(((XmNotebookWidget)w)->notebook.real_binding_width)
#define NB_RealBackPageNumber(w)	\
			(((XmNotebookWidget)w)->notebook.real_back_page_number)
#define NB_PageWidth(w)			\
			(((XmNotebookWidget)w)->notebook.page_width)
#define NB_PageHeight(w)		\
			(((XmNotebookWidget)w)->notebook.page_height)
#define NB_StatusWidth(w)		\
			(((XmNotebookWidget)w)->notebook.status_width)
#define NB_StatusHeight(w)		\
			(((XmNotebookWidget)w)->notebook.status_height)
#define NB_MajorWidth(w)		\
			(((XmNotebookWidget)w)->notebook.major_width)
#define NB_MajorHeight(w)		\
			(((XmNotebookWidget)w)->notebook.major_height)
#define NB_MinorWidth(w)		\
			(((XmNotebookWidget)w)->notebook.minor_width)
#define NB_MinorHeight(w)		\
			(((XmNotebookWidget)w)->notebook.minor_height)
#define NB_ScrollerWidth(w)		\
			(((XmNotebookWidget)w)->notebook.scroller_width)
#define NB_ScrollerHeight(w)		\
			(((XmNotebookWidget)w)->notebook.scroller_height)
#define NB_MajorScrollerWidth(w)	\
			(((XmNotebookWidget)w)->notebook.major_scroller_width)
#define NB_MajorScrollerHeight(w)	\
			(((XmNotebookWidget)w)->notebook.major_scroller_height)
#define NB_MinorScrollerWidth(w)	\
			(((XmNotebookWidget)w)->notebook.minor_scroller_width)
#define NB_MinorScrollerHeight(w)	\
			(((XmNotebookWidget)w)->notebook.minor_scroller_height)
#define NB_FrameWidth(w)		\
			(((XmNotebookWidget)w)->notebook.frame_width)
#define NB_FrameHeight(w)		\
			(((XmNotebookWidget)w)->notebook.frame_height)
#define NB_FirstMajor(w)		\
			(((XmNotebookWidget)w)->notebook.first_major)
#define NB_OldTopMajor(w)		\
			(((XmNotebookWidget)w)->notebook.old_top_major)
#define NB_TopMajor(w)			\
			(((XmNotebookWidget)w)->notebook.top_major)
#define NB_LastMajor(w)			\
			(((XmNotebookWidget)w)->notebook.last_major)
#define NB_FirstMinor(w)		\
			(((XmNotebookWidget)w)->notebook.first_minor)
#define NB_OldTopMinor(w)		\
			(((XmNotebookWidget)w)->notebook.old_top_minor)
#define NB_TopMinor(w)			\
			(((XmNotebookWidget)w)->notebook.top_minor)
#define NB_LastMinor(w)			\
			(((XmNotebookWidget)w)->notebook.last_minor)
#define NB_ConstraintChild(w)		\
			(((XmNotebookWidget)w)->notebook.constraint_child)
#define NB_MajorShadowThickness(w)	\
			(((XmNotebookWidget)w)->notebook.major_shadow_thickness)
#define NB_MinorShadowThickness(w)	\
			(((XmNotebookWidget)w)->notebook.minor_shadow_thickness)
#define NB_MajorShadowChild(w)		\
			(((XmNotebookWidget)w)->notebook.major_shadow_child)
#define NB_MinorShadowChild(w)		\
			(((XmNotebookWidget)w)->notebook.minor_shadow_child)
#define NB_InSetshadow(w)		\
			(((XmNotebookWidget)w)->notebook.in_setshadow)
#define NB_MajorPos(w)			\
			(((XmNotebookWidget)w)->notebook.major_pos)
#define NB_MinorPos(w)			\
			(((XmNotebookWidget)w)->notebook.minor_pos)
#define NB_BindingPos(w)		\
			(((XmNotebookWidget)w)->notebook.binding_pos)
#define NB_WhichTab(w)			\
			(((XmNotebookWidget)w)->notebook.which_tab)
#define NB_LastAllocNum(w)		\
			(((XmNotebookWidget)w)->notebook.last_alloc_num)
#define NB_ScrollerStatus(w)		\
			(((XmNotebookWidget)w)->notebook.scroller_status)
#define NB_NeedScroller(w)		\
			(((XmNotebookWidget)w)->notebook.need_scroller)
#define NB_DynamicLastPageNum(w)	\
			(((XmNotebookWidget)w)->notebook.dynamic_last_page_num)
#define NB_InCallback(w)		\
			(((XmNotebookWidget)w)->notebook.in_callback)
#define NB_BackPageGc(w)		\
			(((XmNotebookWidget)w)->notebook.back_page_gc)
#define NB_FrameGc(w)			\
			(((XmNotebookWidget)w)->notebook.frame_gc)
#define NB_BindingGc(w)			\
			(((XmNotebookWidget)w)->notebook.binding_gc)
#define NB_ForegroundGc(w)		\
			(((XmNotebookWidget)w)->notebook.foreground_gc)
#define NB_BackgroundGc(w)		\
			(((XmNotebookWidget)w)->notebook.background_gc)
#define NB_FirstChangeManaged(w)	\
			(((XmNotebookWidget)w)->notebook.first_change_managed)
#define NB_ScrollFrameData(w)		\
			(((XmNotebookWidget)w)->notebook.scroll_frame_data)
#define	NB_CurrentPageNumber(w)		\
			(((XmNotebookWidget)w)->notebook.current_page_number)

/* For Constraint resources */
/* Note the NBC*() need a Notebook child as argument. */

#define NB_GetConstraintRec(w) \
    ((XmNotebookConstraint)(&((XmNotebookConstraintPtr) \
    (w)->core.constraints)->notebook))

#define	NBC_PageNumber(w)	(NB_GetConstraintRec(w)->page_number)
#define	NBC_ChildType(w)	(NB_GetConstraintRec(w)->child_type)
#define	NBC_Resizable(w)	(NB_GetConstraintRec(w)->resizable)
#define	NBC_Active(w)		(NB_GetConstraintRec(w)->active)

/*
 * The end
 */
#ifdef __cplusplus
}
#endif

#endif /* _XM_NOTEBOOKP_H */
