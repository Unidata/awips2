/** 
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/ContainerP.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
 *
 * Copyright © 1999, 2000, 2001 LessTif Development Team
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


#ifndef _XM_CONTAINERP_H
#define _XM_CONTAINERP_H

#include <Xm/Container.h>
#include <Xm/ManagerP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _XmCwidNodeRec {
	struct _XmCwidNodeRec	*next_ptr, *prev_ptr,
				*child_ptr, *parent_ptr;
	Widget			widget_ptr;
} XmCwidNodeRec, *CwidNode;

typedef struct _XmContainerXfrActionRec {
	Widget		wid;
        XEvent		*event;
        String		*params;
        Cardinal	*num_params;
        Atom		operation;
} XmContainerXfrActionRec, *ContainerXfrAction;

/* Define the container instance part */
typedef struct {
	WidgetList	selected_items;
	Widget		icon_header, anchor_cwid, druggee, size_ob,
			drag_context;
	CwidNode	first_node, last_node;
	Cardinal	*detail_order, *detail_heading;
	XSegment	*outline_segs;
	XtCallbackList	convert_cb, default_action_cb, destination_cb,
			outline_cb, selection_cb;
	XmTabList	detail_tablist;
	XmFontList	render_table;
	Pixel		select_color;
	Pixmap		collapsed_state_pixmap, expanded_state_pixmap;
	GC		normalGC, marqueeGC;
	Time		last_click_time;
	Region		cells_region;
	ContainerXfrAction	transfer_action;
	XtIntervalId	transfer_timer_id;
	XPoint		anchor_point, marquee_start, marquee_end,
			marquee_smallest, marquee_largest, dropspot;
	unsigned long	dynamic_resource;
	int		max_depth, outline_seg_count, *cells, cell_count,
			next_free_cell, current_width_in_cells,
			current_height_in_cells, drag_offset_x, drag_offset_y;
	unsigned int	selected_item_count;
	Cardinal        detail_heading_count, saved_detail_heading_count,
			detail_order_count;
	Dimension	first_col_width, real_first_col_width,
			large_cell_height, large_cell_width, small_cell_height,
			small_cell_width, real_large_cellh, real_large_cellw,
			real_small_cellh, real_small_cellw, margin_h,
			margin_w, outline_indent, ob_width, ob_height,
			prev_width, ideal_width, ideal_height;
	Boolean		first_change_managed, extending_mode, marquee_mode,
			self, toggle_pressed, extend_pressed, ob_pressed,
			cancel_pressed, kaddmode, no_auto_sel_changes,
			started_in_anchor, marquee_drawn, have_primary,
			selecting, large_cell_dim_fixed, small_cell_dim_fixed;
	unsigned char   automatic, entry_viewtype, include_model, layout_type,
			ob_policy, outline_sep_style, spatial_style,
			primary_ownership, resize_model, selection_policy,
			selection_technique, snap_model, create_cwid_type,
			selection_state;
} XmContainerPart;

/* define the full instance record */
typedef struct _XmContainerRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmContainerPart container;
} XmContainerRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmContainerClassPart;

/* Define the full class record */
typedef struct _XmContainerClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmContainerClassPart container_class;
} XmContainerClassRec;

/* Define the Constraint Resources */
typedef struct _XmContainerConstraintPart {
	Widget		entry_parent;		/* XmNentryParent */
	Widget		related_cwid;
	CwidNode	node_ptr;
	int		position_index;		/* XmNpositionIndex */
	int		depth;
	int		cell_idx;
	Boolean		visible_in_outline;
	Position	user_x;
	Position	user_y;
	unsigned char	outline_state;		/* XmNoutlineState */
	unsigned char	selection_visual;
	unsigned char	selection_state;
        unsigned char	cwid_type;
} XmContainerConstraintPart, *XmContainerContraint;

typedef struct _XmContainerConstraintRec {
    XmManagerConstraintPart manager;
    XmContainerConstraintPart container;
} XmContainerConstraintRec, *XmContainerConstraintPtr;

XMLIBEXPORT extern XmContainerClassRec xmContainerClassRec;

#define ContainerMarginHeight(w)	(((XmContainerRec *)(w))->container.margin_h)
#define ContainerMarginWidth(w)		(((XmContainerRec *)(w))->container.margin_w)

#define ContainerEntryViewType(w)	(((XmContainerRec *)(w))->container.entry_viewtype)
#define ContainerLayoutType(w)	(((XmContainerRec *)(w))->container.layout_type)
#define ContainerSelectedItems(w) (((XmContainerRec *)(w))->container.selected_items)
#define ContainerIconHeader(w)	(((XmContainerRec *)(w))->container.icon_header)
#define ContainerAnchorCWid(w)	(((XmContainerRec *)(w))->container.anchor_cwid)
#define ContainerDruggee(w)	(((XmContainerRec *)(w))->container.druggee)
#define ContainerSizeOb(w)	(((XmContainerRec *)(w))->container.size_ob)
#define ContainerDragContext(w)	(((XmContainerRec *)(w))->container.drag_context)
#define ContainerFirstNode(w)	(((XmContainerRec *)(w))->container.first_node)
#define ContainerLastNode(w)	(((XmContainerRec *)(w))->container.last_node)
#define ContainerDetailOrder(w)	(((XmContainerRec *)(w))->container.detail_order)
#define ContainerDetailHeading(w)	(((XmContainerRec *)(w))->container.detail_heading)
#define ContainerConvertCallback(w)	(((XmContainerRec *)(w))->container.convert_cb)
#define ContainerDefaultActionCallback(w)	(((XmContainerRec *)(w))->container.default_action_cb)
#define ContainerDestinationCallback(w)	(((XmContainerRec *)(w))->container.destination_cb)
#define ContainerDetailTablist(w)	(((XmContainerRec *)(w))->container.detail_tablist)
#define ContainerRenderTable(w)	(((XmContainerRec *)(w))->container.render_table)
#define ContainerSelectColor(w)	(((XmContainerRec *)(w))->container.select_color)
#define ContainerCollapsedStatePixmap(w)	(((XmContainerRec *)(w))->container.collapsed_state_pixmap)
#define ContainerExpandedStatePixmap(w)	(((XmContainerRec *)(w))->container.expanded_state_pixmap)
#define ContainerNormalGC(w)	(((XmContainerRec *)(w))->container.normalGC)
#define ContainerMarqueeGC(w)	(((XmContainerRec *)(w))->container.marqueeGC)
#define ContainerLastClickTime(w)	(((XmContainerRec *)(w))->container.last_click_time)
#define ContainerCellsRegion(w)	(((XmContainerRec *)(w))->container.cells_region)
#define ContainerTransferAction(w)	(((XmContainerRec *)(w))->container.transfer_action)
#define ContainerTransferTimerId(w)	(((XmContainerRec *)(w))->container.transfer_timer_id)
#define ContainerAnchorPoint(w)	(((XmContainerRec *)(w))->container.anchor_point)
#define ContainerMarqueeStart(w)	(((XmContainerRec *)(w))->container.marquee_start)
#define ContainerMarqueeEnd(w)	(((XmContainerRec *)(w))->container.marquee_end)
#define ContainerMarqueeSmallest(w)	(((XmContainerRec *)(w))->container.marquee_smallest)
#define ContainerMarqueeLargest(w)	(((XmContainerRec *)(w))->container.marquee_largest)
#define ContainerDropSpot(w)	(((XmContainerRec *)(w))->container.dropspot)
#define ContainerDynamicResource(w)	(((XmContainerRec *)(w))->container.dynamic_resource)
#define ContainerMaxDepth(w)	(((XmContainerRec *)(w))->container.max_depth)
#define	ContainerOutlineSegs(w)		(((XmContainerRec *)(w))->container.outline_segs)
#define ContainerOutlineSegCount(w)	(((XmContainerRec *)(w))->container.outline_seg_count)
#define ContainerCells(w)	(((XmContainerRec *)(w))->container.cells)
#define ContainerCellCount(w)	(((XmContainerRec *)(w))->container.cell_count)
#define ContainerNextFreeCell(w)	(((XmContainerRec *)(w))->container.next_free_cell)
#define ContainerCurrentWidthInCell(w)	(((XmContainerRec *)(w))->container.current_width_in_cells)
#define ContainerCurrentHeightInCell(w)	(((XmContainerRec *)(w))->container.current_height_in_cells)
#define ContainerDragOffsetX(w)	(((XmContainerRec *)(w))->container.drag_offset_x)
#define ContainerDragOffsetY(w)	(((XmContainerRec *)(w))->container.drag_offset_y)
#define ContainerSelectedItemCount(w)	(((XmContainerRec *)(w))->container.selected_item_count)
#define ContainerDetailHeadingCount(w)	(((XmContainerRec *)(w))->container.detail_heading_count)
#define ContainerSavedDetailHeadingCount(w)	(((XmContainerRec *)(w))->container.saved_detail_heading_count)
#define ContainerDetailOrderCount(w)	(((XmContainerRec *)(w))->container.detail_order_count)
#define ContainerFirstColWidth(w)	(((XmContainerRec *)(w))->container.first_col_width)
#define ContainerRealFirstColWidth(w)	(((XmContainerRec *)(w))->container.real_first_col_width)
#define ContainerLargeCellHeight(w)	(((XmContainerRec *)(w))->container.large_cell_height)
#define ContainerLargeCellWidth(w)	(((XmContainerRec *)(w))->container.large_cell_width)
#define ContainerSmallCellHeight(w)	(((XmContainerRec *)(w))->container.small_cell_height)
#define ContainerSmallCellWidth(w)	(((XmContainerRec *)(w))->container.small_cell_width)
#define ContainerRealLargeCellHeight(w)	(((XmContainerRec *)(w))->container.real_large_cellh)
#define ContainerRealLargeCellWidth(w)	(((XmContainerRec *)(w))->container.real_large_cellw)
#define ContainerRealSmallCellHeight(w)	(((XmContainerRec *)(w))->container.real_small_cellh)
#define ContainerRealSmallCellWidth(w)	(((XmContainerRec *)(w))->container.real_small_cellw)
#define ContainerOutlineIndent(w)	(((XmContainerRec *)(w))->container.outline_indent)
#define ContainerObWidth(w)	(((XmContainerRec *)(w))->container.ob_width)
#define ContainerObHeight(w)	(((XmContainerRec *)(w))->container.ob_height)
#define ContainerPreviousWidth(w)	(((XmContainerRec *)(w))->container.prev_width)
#define ContainerIdealWidth(w)	(((XmContainerRec *)(w))->container.ideal_width)
#define ContainerIdealHeight(w)	(((XmContainerRec *)(w))->container.ideal_height)
#define ContainerFirstChangeManaged(w)	(((XmContainerRec *)(w))->container.first_change_managed)
#define ContainerExtendingMode(w)	(((XmContainerRec *)(w))->container.extending_mode)
#define ContainerMarqueeMode(w)	(((XmContainerRec *)(w))->container.marquee_mode)
#define ContainerSelf(w)	(((XmContainerRec *)(w))->container.self)
#define ContainerTogglePressed(w)	(((XmContainerRec *)(w))->container.toggle_pressed)
#define ContainerExtendPressed(w)	(((XmContainerRec *)(w))->container.extend_pressed)
#define ContainerObPressed(w)	(((XmContainerRec *)(w))->container.ob_pressed)
#define ContainerCancelPressed(w)	(((XmContainerRec *)(w))->container.cancel_pressed)
#define ContainerKeyAddMode(w)	(((XmContainerRec *)(w))->container.kaddmode)
#define ContainerNoAutoSelectChanges(w)	(((XmContainerRec *)(w))->container.no_auto_sel_changes)
#define ContainerStartedInAnchor(w)	(((XmContainerRec *)(w))->container.started_in_anchor)
#define ContainerMarqueeDrawn(w)	(((XmContainerRec *)(w))->container.marquee_drawn)
#define ContainerHavePrimary(w)	(((XmContainerRec *)(w))->container.have_primary)
#define ContainerSelecting(w)	(((XmContainerRec *)(w))->container.selecting)
#define ContainerLargeCellDimFixed(w)	(((XmContainerRec *)(w))->container.large_cell_dim_fixed)
#define ContainerSmallCellDimFixed(w)	(((XmContainerRec *)(w))->container.small_cell_dim_fixed)
#define ContainerAutomaticSelection(w)	(((XmContainerRec *)(w))->container.automatic)
#define ContainerIncludeModel(w)	(((XmContainerRec *)(w))->container.include_model)
#define ContainerObPolicy(w)	(((XmContainerRec *)(w))->container.ob_policy)
#define ContainerOutlineSeparatorStyle(w)	(((XmContainerRec *)(w))->container.outline_sep_style)
#define ContainerSpatialStyle(w)	(((XmContainerRec *)(w))->container.spatial_style)
#define ContainerPrimaryOwnership(w)	(((XmContainerRec *)(w))->container.primary_ownership)
#define ContainerSpatialResizeModel(w)	(((XmContainerRec *)(w))->container.resize_model)
#define ContainerSelectionPolicy(w)	(((XmContainerRec *)(w))->container.selection_policy)
#define ContainerSelectionTechnique(w)	(((XmContainerRec *)(w))->container.selection_technique)
#define ContainerSnapModel(w)	(((XmContainerRec *)(w))->container.snap_model)
#define ContainerCreateCWidthType(w)	(((XmContainerRec *)(w))->container.create_cwid_type)
#define ContainerSelectionState(w)	(((XmContainerRec *)(w))->container.selection_state)

#define ContainerSelectionCallback(w)	(((XmContainerRec *)(w))->container.selection_cb)

#define CC_PositionIndex(cw) \
    (((XmContainerConstraintRec *)(CoreConstraints(cw)))->container.position_index)
#define CC_EntryParent(cw) \
    (((XmContainerConstraintRec *)(CoreConstraints(cw)))->container.entry_parent)
#define CC_OutlineState(cw) \
    (((XmContainerConstraintRec *)(CoreConstraints(cw)))->container.outline_state)
#define CC_UserX(cw) \
    (((XmContainerConstraintRec *)(CoreConstraints(cw)))->container.user_x)
#define CC_UserY(cw) \
    (((XmContainerConstraintRec *)(CoreConstraints(cw)))->container.user_y)
#define CC_Related(cw) \
    (((XmContainerConstraintRec *)(CoreConstraints(cw)))->container.related_cwid)
#define CC_Node(cw) \
    (((XmContainerConstraintRec *)(CoreConstraints(cw)))->container.node_ptr)
#define CC_CWidType(cw) \
    (((XmContainerConstraintRec *)(CoreConstraints(cw)))->container.cwid_type)

#define	CC_Next(cw)	(CC_Node(cw)->next_ptr)
#define	CC_Prev(cw)	(CC_Node(cw)->prev_ptr)
#define	CC_Child(cw)	(CC_Node(cw)->child_ptr)
#define	CC_Parent(cw)	(CC_Node(cw)->parent_ptr)
#define	CC_Widget(cw)	(CC_Node(cw)->widget_ptr)

#ifdef __cplusplus
}
#endif

#endif /* _XM_CONTAINERP_H */
