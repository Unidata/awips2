/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/FileSBP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
 * 
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995-2001, 2004 LessTif Development Team
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

#ifndef _XM_FILESBP_H
#define _XM_FILESBP_H

#include <Xm/FileSB.h>
#include <Xm/SelectioBP.h>

/*
 * some OSF defines, for compatability
 */
#define XmFSB_MAX_WIDGETS_VERT	10
#define XmFS_NO_MATCH		(1 << 0)
#define XmFS_IN_FILE_SEARCH	(1 << 1)
#define XmFS_DIR_SEARCH_PROC	(1 << 2)

#ifdef __cplusplus
extern "C" {
#endif

/* Define the file selection box instance part */
typedef struct {
    XmString		directory;
    XmString		pattern;
    Widget		dir_list_label;
    XmString		dir_list_label_string;
    Widget		dir_list;
    XmStringTable	dir_list_items;
    int			dir_list_item_count;
    int			dir_list_selected_item_position;
    Widget		filter_label;
    XmString		filter_label_string;
    Widget		filter_text;
    XmString		dir_mask;
    XmString		no_match_string;
    XmQualifyProc	qualify_search_data_proc;
    XmSearchProc	dir_search_proc;
    XmSearchProc	file_search_proc;
    unsigned char	file_type_mask;
    Boolean		list_updated;
    Boolean		directory_valid;
    unsigned char	state_flags;
/*
 * The stuff below is new w.r.t. Motif 1.2
 */
    XtEnum		path_mode;
    XtEnum		file_filter_style;
    Widget		dir_text;
    Widget		dir_text_label;
    XmString		dir_text_label_string;
    time_t		prev_dir_modtime;
} XmFileSelectionBoxPart;

/* Define the full instance record */
typedef struct _XmFileSelectionBoxRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmBulletinBoardPart bulletin_board;
    XmSelectionBoxPart selection_box;
    XmFileSelectionBoxPart file_selection_box;
} XmFileSelectionBoxRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmFileSelectionBoxClassPart;

/* Defint the full class record */
typedef struct _XmFileSelectionBoxClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmBulletinBoardClassPart bulletin_board_class;
    XmSelectionBoxClassPart selection_box_class;
    XmFileSelectionBoxClassPart file_selection_box_class;
} XmFileSelectionBoxClassRec;

XMLIBEXPORT extern XmFileSelectionBoxClassRec xmFileSelectionBoxClassRec;

/*
 * keep those sublcasses happy
 */
#define FS_Directory(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.directory)

#define FS_DirMask(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_mask)

#define FS_DirListLabel(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_list_label)

#define FS_DirListLabelString(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_list_label_string)

#define FS_DirList(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_list)

#define FS_DirListItems(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_list_items)

#define FS_DirListItemCount(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_list_item_count)

#define FS_FilterLabel(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.filter_label)

#define FS_FilterLabelString(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.filter_label_string)

#define FS_FilterText(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.filter_text)

#define FS_Pattern(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.pattern)

#define FS_NoMatchString(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.no_match_string)

#define FS_QualifySearchDataProc(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.qualify_search_data_proc)

#define FS_DirSearchProc(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_search_proc)

#define FS_FileSearchProc(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.file_search_proc)

#define FS_FileTypeMask(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.file_type_mask)

#define FS_ListUpdated(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.list_updated)

#define FS_DirectoryValid(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.directory_valid)

#define FS_StateFlags(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.state_flags)

#define FS_DirListSelectedItemPosition(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_list_selected_item_position)

#define FS_PathMode(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.path_mode)

#define FS_FileFilterStyle(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.file_filter_style)

#define FS_DirText(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_text)

#define FS_DirTextLabel(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_text_label)

#define FS_DirTextLabelString(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_text_label_string)

#define FS_PrevDirModtime(w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.prev_dir_mod_time)

#ifdef __cplusplus
}
#endif

#endif /* _XM_FILESBP_H */
