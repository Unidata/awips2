/**
 *
 * $Id: TransltnsP.h,v 1.2 2004/06/07 20:01:53 dannybackx Exp $
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

#ifndef _XM_TRANSLTNSP_H
#define _XM_TRANSLTNSP_H

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif

XMLIBEXPORT extern XmConst char _XmArrowB_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmBulletinB_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmCascadeB_menubar_events[];
XMLIBEXPORT extern XmConst char _XmCascadeB_p_events[];
XMLIBEXPORT extern XmConst char _XmClipWindowTranslationTable[];
XMLIBEXPORT extern XmConst char _XmComboBox_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmComboBox_defaultAccelerators[];
XMLIBEXPORT extern XmConst char _XmComboBox_dropDownComboBoxAccelerators[];
XMLIBEXPORT extern XmConst char _XmComboBox_dropDownListTranslations[];
XMLIBEXPORT extern XmConst char _XmComboBox_textFocusTranslations[];
XMLIBEXPORT extern XmConst char _XmContainer_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmContainer_traversalTranslations[];
XMLIBEXPORT extern XmConst char _XmDragC_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmDrawingA_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmDrawingA_traversalTranslations[];
XMLIBEXPORT extern XmConst char _XmDrawnB_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmFrame_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmGrabShell_translations[];
XMLIBEXPORT extern XmConst char _XmLabel_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmLabel_menuTranslations[];
XMLIBEXPORT extern XmConst char _XmLabel_menu_traversal_events[];
XMLIBEXPORT extern XmConst char _XmList_ListXlations1[];
XMLIBEXPORT extern XmConst char _XmList_ListXlations2[];
XMLIBEXPORT extern XmConst char _XmManager_managerTraversalTranslations[];
XMLIBEXPORT extern XmConst char _XmManager_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmMenuShell_translations [];
XMLIBEXPORT extern XmConst char _XmNotebook_manager_translations[];
XMLIBEXPORT extern XmConst char _XmNotebook_TabAccelerators[];
XMLIBEXPORT extern XmConst char _XmPrimitive_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmPushB_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmPushB_menuTranslations[];
XMLIBEXPORT extern XmConst char _XmRowColumn_menu_traversal_table[];
XMLIBEXPORT extern XmConst char _XmRowColumn_bar_table[];
XMLIBEXPORT extern XmConst char _XmRowColumn_option_table[];
XMLIBEXPORT extern XmConst char _XmRowColumn_menu_table[];
XMLIBEXPORT extern XmConst char _XmSash_defTranslations[];
XMLIBEXPORT extern XmConst char _XmScrollBar_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmScrolledW_ScrolledWindowXlations[];

/* REMARK: In Motif 2.0 _XmScrolledW_ClipWindowTranslationTable is missing. */
/* REMARK: In Motif 2.0 _XmScrolledW_WorkWindowTranslationTable is missing. */

XMLIBEXPORT extern XmConst char _XmSelectioB_defaultTextAccelerators[];
XMLIBEXPORT extern XmConst char _XmSpinB_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmSpinB_defaultAccelerators[];
XMLIBEXPORT extern XmConst char _XmTearOffB_overrideTranslations[];
XMLIBEXPORT extern XmConst char _XmTextF_EventBindings1[];
XMLIBEXPORT extern XmConst char _XmTextF_EventBindings2[]; 
XMLIBEXPORT extern XmConst char _XmTextF_EventBindings3[];
XMLIBEXPORT extern XmConst char _XmTextIn_XmTextEventBindings1[];
XMLIBEXPORT extern XmConst char _XmTextIn_XmTextEventBindings2[];
XMLIBEXPORT extern XmConst char _XmTextIn_XmTextEventBindings3[];
XMLIBEXPORT extern XmConst char _XmTextIn_XmTextEventBindings3[];
XMLIBEXPORT extern XmConst char _XmToggleB_defaultTranslations[];
XMLIBEXPORT extern XmConst char _XmToggleB_menuTranslations[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_fallbackBindingString[];

/* The following keybindings have been provided for backward compatibility. */

XMLIBEXPORT extern XmConst char _XmVirtKeys_acornFallbackBindingString[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_apolloFallbackBindingString[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_dgFallbackBindingString[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_decFallbackBindingString[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_dblclkFallbackBindingString[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_hpFallbackBindingString[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_ibmFallbackBindingString[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_ingrFallbackBindingString[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_megatekFallbackBindingString[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_motorolaFallbackBindingString[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_sgiFallbackBindingString[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_siemensWx200FallbackBindingString[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_siemens9733FallbackBindingString[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_sunFallbackBindingString[];
XMLIBEXPORT extern XmConst char _XmVirtKeys_tekFallbackBindingString[];

#ifdef __cplusplus
}
#endif

#endif /* _XM_TRANSLTNSP_H */
