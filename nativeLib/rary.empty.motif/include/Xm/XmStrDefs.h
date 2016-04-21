/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/XmStrDefs.h,v 1.12 2005/08/15 19:12:32 dannybackx Exp $
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

#ifndef _XM_XMSTRDEFS_H
#define _XM_XMSTRDEFS_H

#include <X11/StringDefs.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * We'll need these later on.
 */
XMLIBEXPORT extern const char _XmStrings[];
#define	XmS				""

/*
 * Stuff redefined from Xt
 */
#define XmSTRING_DEFAULT_CHARSET	XmS
#define XmSTRING_ISO8859_1		"ISO8859-1"
#define XmFONTLIST_DEFAULT_TAG		XmSFONTLIST_DEFAULT_TAG_STRING
#define XmFONTLIST_DEFAULT_TAG_STRING	XmSXmFONTLIST_DEFAULT_TAG_STRING

#define XmVaCASCADEBUTTON		"cascadeButton"
#define XmVaCHECKBUTTON			"checkButton"
#define XmVaDOUBLE_SEPARATOR		"doubleSeparator"
#define XmVaPUSHBUTTON			"pushButton"
#define XmVaRADIOBUTTON			"radioButton"
#define XmVaSEPARATOR			"separator"
#define XmVaSINGLE_SEPARATOR		"singleSeparator"
#define XmVaTOGGLEBUTTON		"checkButton"
#define XmVaTITLE			XtNtitle

#define XtCKeyboardFocusPolicy		XmCKeyboardFocusPolicy
#define XtCShellUnitType		XmCShellUnitType
#define XtNkeyboardFocusPolicy		XmNkeyboardFocusPolicy
#define XtNshellUnitType		XmNshellUnitType
#define XtRKeyboardFocusPolicy		XmRKeyboardFocusPolicy

#define XmRPrimBottomShadowPixmap       XmRBottomShadowPixmap
#define XmRPrimHighlightPixmap          XmRHighlightPixmap
#define XmRPrimTopShadowPixmap          XmRTopShadowPixmap

#define XmCAccelerators			XtCAccelerators
#define XmCAllowShellResize		XtCAllowShellResize
#define XmCArgc				XtCArgc
#define XmCArgv				XtCArgv
#define XmCBackground			XtCBackground
#define XmCBaseHeight			XtCBaseHeight
#define XmCBaseHeight			XtCBaseHeight
#define XmCBaseWidth			XtCBaseWidth
#define XmCBaseWidth			XtCBaseWidth
#define XmCBitmap			XtCBitmap
#define XmCBoolean			XtCBoolean
#define XmCBorderColor			XtCBorderColor
#define XmCBorderWidth			XtCBorderWidth
#define XmCCallback			XtCCallback
#define XmCColor			XtCColor
#define XmCColormap			XtCColormap
#define XmCCreatePopupChildProc		XtCCreatePopupChildProc
#define XmCCursor			XtCCursor
#define XmCDepth			XtCDepth
#define XmCDimension			XtRDimension
#define XmCEditMode			XtREditMode
#define XmCEditType			XtCEditType
#define XmCEventBindings		XtCEventBindings
#define XmCFile				XtCFile
#define XmCFont				XtCFont
#define XmCFontSet			XtCFontSet
#define XmCForeground			XtCForeground
#define XmCFraction			XtCFraction
#define XmCFunction			XtCFunction
#define XmCGeometry			XtCGeometry
#define XmCHSpace			XtCHSpace
#define XmCHeight			XtCHeight
#define XmCHeightInc			XtCHeightInc
#define XmCIconMask			XtCIconMask
#define XmCIconName			XtCIconName
#define XmCIconNameEncoding		XtCIconNameEncoding
#define XmCIconPixmap			XtCIconPixmap
#define XmCIconWindow			XtCIconWindow
#define XmCIconX			XtCIconX
#define XmCIconY			XtCIconY
#define XmCIconic			XtCIconic
#define XmCIndex			XtCIndex
#define XmCInitialResourcesPersistent	XtCInitialResourcesPersistent
#define XmCInitialState			XtCInitialState
#define XmCInput			XtCInput
#define XmCInsertPosition		XtCInsertPosition
#define XmCInterval			XtCInterval
#define XmCJustify			XtCJustify
#define XmCLabel			XtCLabel
#define XmCLength			XtCLength
#define XmCMappedWhenManaged		XtCMappedWhenManaged
#define XmCMargin			XtCMargin
#define XmCMaxAspectX			XtCMaxAspectX
#define XmCMaxAspectY			XtCMaxAspectY
#define XmCMaxHeight			XtCMaxHeight
#define XmCMaxWidth			XtCMaxWidth
#define XmCMenuEntry			XtCMenuEntry
#define XmCMinAspectX			XtCMinAspectX
#define XmCMinAspectY			XtCMinAspectY
#define XmCMinHeight			XtCMinHeight
#define XmCMinWidth			XtCMinWidth
#define XmCNotify			XtCNotify
#define XmCOrientation			XtCOrientation
#define XmCOverrideRedirect		XtCOverrideRedirect
#define XmCParameter			XtCParameter
#define XmCPixmap			XtCPixmap
#define XmCPosition			XtCPosition
#define XmCReadOnly			XtCReadOnly
#define XmCResize			XtCResize
#define XmCReverseVideo			XtCReverseVideo
#define XmCSaveUnder			XtCSaveUnder
#define XmCScreen			XtCScreen
#define XmCScrollDCursor		XtCScrollDCursor
#define XmCScrollHCursor		XtCScrollHCursor
#define XmCScrollLCursor		XtCScrollLCursor
#define XmCScrollProc			XtCScrollProc
#define XmCScrollRCursor		XtCScrollRCursor
#define XmCScrollUCursor		XtCScrollUCursor
#define XmCScrollVCursor		XtCScrollVCursor
#define XmCSelection			XtCSelection
#define XmCSelectionArray		XtCSelectionArray
#define XmCSensitive			XtCSensitive
#define XmCSpace			XtCSpace
#define XmCString			XtCString
#define XmCTextOptions			XtCTextOptions
#define XmCTextPosition			XtCTextPosition
#define XmCTextSink			XtCTextSink
#define XmCTextSource			XtCTextSource
#define XmCThickness			XtCThickness
#define XmCThumb			XtCThumb
#define XmCTitle			XtCTitle
#define XmCTitleEncoding		XtCTitleEncoding
#define XmCTransient			XtCTransient
#define XmCTransientFor			XtCTransientFor
#define XmCTranslations			XtCTranslations
#define XmCVSpace			XtCVSpace
#define XmCValue			XtCValue
#define XmCVisual			XtCVisual
#define XmCWaitForWm			XtCWaitForWm
#define XmCWidget			XtRWidget
#define XmCWidth			XtCWidth
#define XmCWidthInc			XtCWidthInc
#define XmCWinGravity			XtCWinGravity
#define XmCWindow			XtCWindow
#define XmCWindowGroup			XtCWindowGroup
#define XmCWmTimeout			XtCWmTimeout
#define XmCX				XtCX
#define XmCY				XtCY

#define XmNaccelerators			XtNaccelerators
#define XmNallowShellResize		XtNallowShellResize
#define XmNancestorSensitive		XtNancestorSensitive
#define XmNargc				XtNargc
#define XmNargv				XtNargv
#define XmNbackground			XtNbackground
#define XmNbackgroundPixmap		XtNbackgroundPixmap
#define XmNbaseHeight			XtNbaseHeight
#define XmNbaseHeight			XtNbaseHeight
#define XmNbaseWidth			XtNbaseWidth
#define XmNbaseWidth			XtNbaseWidth
#define XmNbitmap			XtNbitmap
#define XmNborder			XtNborder
#define XmNborderColor			XtNborderColor
#define XmNborderPixmap			XtNborderPixmap
#define XmNborderWidth			XtNborderWidth
#define XmNcallback			XtNcallback
#define XmNchildren			XtNchildren
#define XmNcolormap			XtNcolormap
#define XmNcreatePopupChildProc		XtNcreatePopupChildProc
#define XmNdepth			XtNdepth
#define XmNdestroyCallback		XtNdestroyCallback
#define XmNeditType			XtNeditType
#define XmNfile				XtNfile
#define XmNfont				XtNfont
#define XmNfontSet		        XtNfontSet
#define XmNforceBars			XtNforceBars
#define XmNforeground			XtNforeground
#define XmNfunction			XtNfunction
#define XmNgeometry			XtNgeometry
#define XmNheight			XtNheight
#define XmNheightInc			XtNheightInc
#define XmNhighlight			XtNhighlight
#define XmNiconMask			XtNiconMask
#define XmNiconName			XtNiconName
#define XmNiconNameEncoding		XtNiconNameEncoding
#define XmNiconPixmap			XtNiconPixmap
#define XmNiconWindow			XtNiconWindow
#define XmNiconX			XtNiconX
#define XmNiconY			XtNiconY
#define XmNiconic			XtNiconic
#define XmNindex			XtNindex
#define XmNinitialResourcesPersistent	XtNinitialResourcesPersistent
#define XmNinitialState			XtNinitialState
#define XmNinnerHeight			XtNinnerHeight
#define XmNinnerWidth			XtNinnerWidth
#define XmNinnerWindow			XtNinnerWindow
#define XmNinput			XtNinput
#define XmNinsertPosition		XtNinsertPosition
#define XmNinternalHeight		XtNinternalHeight
#define XmNinternalWidth		XtNinternalWidth
#define XmNjumpProc			XtNjumpProc
#define XmNjustify			XtNjustify
#define XmNlength			XtNlength
#define XmNlowerRight			XtNlowerRight
#define XmNmappedWhenManaged		XtNmappedWhenManaged
#define XmNmaxAspectX			XtNmaxAspectX
#define XmNmaxAspectY			XtNmaxAspectY
#define XmNmaxHeight			XtNmaxHeight
#define XmNmaxWidth			XtNmaxWidth
#define XmNmenuEntry			XtNmenuEntry
#define XmNminAspectX			XtNminAspectX
#define XmNminAspectY			XtNminAspectY
#define XmNminHeight			XtNminHeight
#define XmNminWidth			XtNminWidth
#define XmNname				XtNname
#define XmNnotify			XtNnotify
#define XmNnumChildren			XtNnumChildren
#define XmNorientation			XtNorientation
#define XmNoverrideRedirect		XtNoverrideRedirect
#define XmNparameter			XtNparameter
#define XmNpixmap			XtNpixmap
#define XmNpopdownCallback		XtNpopdownCallback
#define XmNpopupCallback		XtNpopupCallback
#define XmNresize			XtNresize
#define XmNreverseVideo			XtNreverseVideo
#define XmNsaveUnder			XtNsaveUnder
#define XmNscreen			XtNscreen
#define XmNscrollDCursor		XtNscrollDCursor
#define XmNscrollHCursor		XtNscrollHCursor
#define XmNscrollLCursor		XtNscrollLCursor
#define XmNscrollProc			XtNscrollProc
#define XmNscrollRCursor		XtNscrollRCursor
#define XmNscrollUCursor		XtNscrollUCursor
#define XmNscrollVCursor		XtNscrollVCursor
#define XmNselection			XtNselection
#define XmNselectionArray		XtNselectionArray
#define XmNsensitive			XtNsensitive
#define XmNshown			XtNshown
#define XmNspace			XtNspace
#define XmNstring			XtNstring
#define XmNtextOptions			XtNtextOptions
#define XmNtextSink			XtNtextSink
#define XmNtextSource			XtNtextSource
#define XmNthickness			XtNthickness
#define XmNthumb			XtNthumb
#define XmNthumbProc			XtNthumbProc
#define XmNtitle			XtNtitle
#define XmNtitleEncoding		XtNtitleEncoding
#define XmNtop				XtNtop
#define XmNtransient			XtNtransient
#define XmNtransientFor			XtNtransientFor
#define XmNtransientFor			XtNtransientFor
#define XmNtranslations			XtNtranslations
#define XmNupdate			XtNupdate
#define XmNuseBottom			XtNuseBottom
#define XmNuseRight			XtNuseRight
#define XmNvalue			XtNvalue
#define XmNvisual			XtNvisual
#define XmNwaitForWm			XtNwaitForWm
#define XmNwidth			XtNwidth
#define XmNwidthInc			XtNwidthInc
#define XmNwinGravity			XtNwinGravity
#define XmNwindow			XtNwindow
#define XmNwindowGroup			XtNwindowGroup
#define XmNwmTimeout			XtNwmTimeout
#define XmNx				XtNx
#define XmNy				XtNy

#define XmRAcceleratorTable		XtRAcceleratorTable
#define XmRAtom				XtRAtom
#define XmRBitmap			XtRBitmap
#define XmRBool				XtRBool
#define XmRBoolean			XtRBoolean
#define XmRCallProc			XtRCallProc
#define XmRCallback			XtRCallback
#define XmRCardinal			XtRCardinal
#define XmRColor			XtRColor
#define XmRColormap			XtRColormap
#define XmRCursor			XtRCursor
#define XmRDimension			XtRDimension
#define XmRDisplay			XtRDisplay
#define XmREditMode			XtREditMode
#define XmREnum			        XtREnum
#define XmRFile				XtRFile
#define XmRFloat			XtRFloat
#define XmRFont				XtRFont
#define XmRFontSet		        XtRFontSet
#define XmRFontStruct			XtRFontStruct
#define XmRFunction			XtRFunction
#define XmRGeometry			XtRGeometry
#define XmRImmediate			XtRImmediate
#define XmRInitialState		        XtRInitialState
#define XmRInt				XtRInt
#define XmRJustify			XtRJustify
#define XmRLongBoolean		        XtRLongBoolean
#define XmROrientation			XtROrientation
#define XmRObject		        XtRObject
#define XmRPixel			XtRPixel
#define XmRPixmap			XtRPixmap
#define XmRPointer			XtRPointer
#define XmRPosition			XtRPosition
#define XmRScreen	                XtRScreen
#define XmRShort			XtRShort
#define XmRString			XtRString
#define XmRStringArray		        XtRStringArray
#define XmRStringTable			XtRStringTable
#define XmRTextPosition			XtCTextPosition
#define XmRTranslationTable		XtRTranslationTable
#define XmRUnsignedChar			XtRUnsignedChar
#define XmRVisual		        XtRVisual
#define XmRWidget			XtRWidget
#define XmRWidgetClass			XtRWidgetClass
#define XmRWidgetList			XtRWidgetList
#define XmRWindow			XtRWindow


#define XmRWidgetList  XtRWidgetList

/*
 * Stuff that points to the _XmStrings array.
 *
 * This should be in sync with lib/Xm-2.1/XmStrDefs.c, and binary compatibility
 * with *Motif versions may suffer if this is modified without proper care.
 *
 * Do note that this was not copied from any version of Motif sources.
 */
#ifndef XmC
#define XmC ((char *)&_XmStrings[0])
#endif
#ifndef XmCAccelerator
#define XmCAccelerator ((char *)&_XmStrings[1])
#endif
#ifndef XmCAcceleratorText
#define XmCAcceleratorText ((char *)&_XmStrings[13])
#endif
#ifndef XmCAdjustLast
#define XmCAdjustLast ((char *)&_XmStrings[29])
#endif
#ifndef XmCAdjustMargin
#define XmCAdjustMargin ((char *)&_XmStrings[40])
#endif
#ifndef XmCAlignment
#define XmCAlignment ((char *)&_XmStrings[53])
#endif
#ifndef XmCAllowOverlap
#define XmCAllowOverlap ((char *)&_XmStrings[63])
#endif
#ifndef XmCAnimationMask
#define XmCAnimationMask ((char *)&_XmStrings[76])
#endif
#ifndef XmCAnimationPixmap
#define XmCAnimationPixmap ((char *)&_XmStrings[90])
#endif
#ifndef XmCAnimationPixmapDepth
#define XmCAnimationPixmapDepth ((char *)&_XmStrings[106])
#endif
#ifndef XmCAnimationStyle
#define XmCAnimationStyle ((char *)&_XmStrings[127])
#endif
#ifndef XmCApplyLabelString
#define XmCApplyLabelString ((char *)&_XmStrings[142])
#endif
#ifndef XmCArmCallback
#define XmCArmCallback ((char *)&_XmStrings[159])
#endif
#ifndef XmCArmColor
#define XmCArmColor ((char *)&_XmStrings[171])
#endif
#ifndef XmCArmPixmap
#define XmCArmPixmap ((char *)&_XmStrings[180])
#endif
#ifndef XmCArrowDirection
#define XmCArrowDirection ((char *)&_XmStrings[190])
#endif
#ifndef XmCAttachment
#define XmCAttachment ((char *)&_XmStrings[205])
#endif
#ifndef XmCAudibleWarning
#define XmCAudibleWarning ((char *)&_XmStrings[216])
#endif
#ifndef XmCAutoShowCursorPosition
#define XmCAutoShowCursorPosition ((char *)&_XmStrings[231])
#endif
#ifndef XmCAutoUnmanage
#define XmCAutoUnmanage ((char *)&_XmStrings[254])
#endif
#ifndef XmCAutomaticSelection
#define XmCAutomaticSelection ((char *)&_XmStrings[267])
#endif
#ifndef XmCAvailability
#define XmCAvailability ((char *)&_XmStrings[286])
#endif
#ifndef XmCBackgroundPixmap
#define XmCBackgroundPixmap ((char *)&_XmStrings[299])
#endif
#ifndef XmCBlendModel
#define XmCBlendModel ((char *)&_XmStrings[316])
#endif
#ifndef XmCBlinkRate
#define XmCBlinkRate ((char *)&_XmStrings[327])
#endif
#ifndef XmCBottomShadowColor
#define XmCBottomShadowColor ((char *)&_XmStrings[337])
#endif
#ifndef XmCBottomShadowPixmap
#define XmCBottomShadowPixmap ((char *)&_XmStrings[355])
#endif
#ifndef XmCButtonAcceleratorText
#define XmCButtonAcceleratorText ((char *)&_XmStrings[374])
#endif
#ifndef XmCButtonAccelerators
#define XmCButtonAccelerators ((char *)&_XmStrings[396])
#endif
#ifndef XmCButtonCount
#define XmCButtonCount ((char *)&_XmStrings[415])
#endif
#ifndef XmCButtonFontList
#define XmCButtonFontList ((char *)&_XmStrings[427])
#endif
#ifndef XmCButtonMnemonicCharSets
#define XmCButtonMnemonicCharSets ((char *)&_XmStrings[442])
#endif
#ifndef XmCButtonMnemonics
#define XmCButtonMnemonics ((char *)&_XmStrings[465])
#endif
#ifndef XmCButtonSet
#define XmCButtonSet ((char *)&_XmStrings[481])
#endif
#ifndef XmCButtonType
#define XmCButtonType ((char *)&_XmStrings[491])
#endif
#ifndef XmCButtons
#define XmCButtons ((char *)&_XmStrings[502])
#endif
#ifndef XmCCancelLabelString
#define XmCCancelLabelString ((char *)&_XmStrings[510])
#endif
#ifndef XmCChildHorizontalAlignment
#define XmCChildHorizontalAlignment ((char *)&_XmStrings[528])
#endif
#ifndef XmCChildHorizontalSpacing
#define XmCChildHorizontalSpacing ((char *)&_XmStrings[553])
#endif
#ifndef XmCChildPlacement
#define XmCChildPlacement ((char *)&_XmStrings[576])
#endif
#ifndef XmCChildType
#define XmCChildType ((char *)&_XmStrings[591])
#endif
#ifndef XmCChildVerticalAlignment
#define XmCChildVerticalAlignment ((char *)&_XmStrings[601])
#endif
#ifndef XmCChildren
#define XmCChildren ((char *)&_XmStrings[624])
#endif
#ifndef XmCClientData
#define XmCClientData ((char *)&_XmStrings[633])
#endif
#ifndef XmCClipWindow
#define XmCClipWindow ((char *)&_XmStrings[644])
#endif
#ifndef XmCColumns
#define XmCColumns ((char *)&_XmStrings[655])
#endif
#ifndef XmCCommandWindow
#define XmCCommandWindow ((char *)&_XmStrings[663])
#endif
#ifndef XmCCommandWindowLocation
#define XmCCommandWindowLocation ((char *)&_XmStrings[677])
#endif
#ifndef XmCConvertProc
#define XmCConvertProc ((char *)&_XmStrings[699])
#endif
#ifndef XmCCursorBackground
#define XmCCursorBackground ((char *)&_XmStrings[711])
#endif
#ifndef XmCCursorForeground
#define XmCCursorForeground ((char *)&_XmStrings[728])
#endif
#ifndef XmCCursorPosition
#define XmCCursorPosition ((char *)&_XmStrings[745])
#endif
#ifndef XmCCursorPositionVisible
#define XmCCursorPositionVisible ((char *)&_XmStrings[760])
#endif
#ifndef XmCDarkThreshold
#define XmCDarkThreshold ((char *)&_XmStrings[782])
#endif
#ifndef XmCDecimalPoints
#define XmCDecimalPoints ((char *)&_XmStrings[796])
#endif
#ifndef XmCDefaultButtonShadowThickness
#define XmCDefaultButtonShadowThickness ((char *)&_XmStrings[810])
#endif
#ifndef XmCDefaultButtonType
#define XmCDefaultButtonType ((char *)&_XmStrings[839])
#endif
#ifndef XmCDefaultCopyCursorIcon
#define XmCDefaultCopyCursorIcon ((char *)&_XmStrings[857])
#endif
#ifndef XmCDefaultFontList
#define XmCDefaultFontList ((char *)&_XmStrings[879])
#endif
#ifndef XmCDefaultInvalidCursorIcon
#define XmCDefaultInvalidCursorIcon ((char *)&_XmStrings[895])
#endif
#ifndef XmCDefaultLinkCursorIcon
#define XmCDefaultLinkCursorIcon ((char *)&_XmStrings[920])
#endif
#ifndef XmCDefaultMoveCursorIcon
#define XmCDefaultMoveCursorIcon ((char *)&_XmStrings[942])
#endif
#ifndef XmCDefaultNoneCursorIcon
#define XmCDefaultNoneCursorIcon ((char *)&_XmStrings[964])
#endif
#ifndef XmCDefaultPosition
#define XmCDefaultPosition ((char *)&_XmStrings[986])
#endif
#ifndef XmCDefaultSourceCursorIcon
#define XmCDefaultSourceCursorIcon ((char *)&_XmStrings[1002])
#endif
#ifndef XmCDefaultValidCursorIcon
#define XmCDefaultValidCursorIcon ((char *)&_XmStrings[1026])
#endif
#ifndef XmCDeleteResponse
#define XmCDeleteResponse ((char *)&_XmStrings[1049])
#endif
#ifndef XmCDesktopParent
#define XmCDesktopParent ((char *)&_XmStrings[1064])
#endif
#ifndef XmCDialogStyle
#define XmCDialogStyle ((char *)&_XmStrings[1078])
#endif
#ifndef XmCDialogTitle
#define XmCDialogTitle ((char *)&_XmStrings[1090])
#endif
#ifndef XmCDialogType
#define XmCDialogType ((char *)&_XmStrings[1102])
#endif
#ifndef XmCDirListItemCount
#define XmCDirListItemCount ((char *)&_XmStrings[1113])
#endif
#ifndef XmCDirListItems
#define XmCDirListItems ((char *)&_XmStrings[1130])
#endif
#ifndef XmCDirListLabelString
#define XmCDirListLabelString ((char *)&_XmStrings[1143])
#endif
#ifndef XmCDirMask
#define XmCDirMask ((char *)&_XmStrings[1162])
#endif
#ifndef XmCDirSearchProc
#define XmCDirSearchProc ((char *)&_XmStrings[1170])
#endif
#ifndef XmCDirSpec
#define XmCDirSpec ((char *)&_XmStrings[1184])
#endif
#ifndef XmCDirectory
#define XmCDirectory ((char *)&_XmStrings[1192])
#endif
#ifndef XmCDirectoryValid
#define XmCDirectoryValid ((char *)&_XmStrings[1202])
#endif
#ifndef XmCDisarmCallback
#define XmCDisarmCallback ((char *)&_XmStrings[1217])
#endif
#ifndef XmCDoubleClickInterval
#define XmCDoubleClickInterval ((char *)&_XmStrings[1232])
#endif
#ifndef XmCDragContextClass
#define XmCDragContextClass ((char *)&_XmStrings[1252])
#endif
#ifndef XmCDragDropFinishCallback
#define XmCDragDropFinishCallback ((char *)&_XmStrings[1269])
#endif
#ifndef XmCDragIconClass
#define XmCDragIconClass ((char *)&_XmStrings[1292])
#endif
#ifndef XmCDragInitiatorProtocolStyle
#define XmCDragInitiatorProtocolStyle ((char *)&_XmStrings[1306])
#endif
#ifndef XmCDragMotionCallback
#define XmCDragMotionCallback ((char *)&_XmStrings[1333])
#endif
#ifndef XmCDragOperations
#define XmCDragOperations ((char *)&_XmStrings[1352])
#endif
#ifndef XmCDragOverMode
#define XmCDragOverMode ((char *)&_XmStrings[1367])
#endif
#ifndef XmCDragProc
#define XmCDragProc ((char *)&_XmStrings[1380])
#endif
#ifndef XmCDragReceiverProtocolStyle
#define XmCDragReceiverProtocolStyle ((char *)&_XmStrings[1389])
#endif
#ifndef XmCDropProc
#define XmCDropProc ((char *)&_XmStrings[1415])
#endif
#ifndef XmCDropRectangles
#define XmCDropRectangles ((char *)&_XmStrings[1424])
#endif
#ifndef XmCDropSiteActivity
#define XmCDropSiteActivity ((char *)&_XmStrings[1439])
#endif
#ifndef XmCDropSiteEnterCallback
#define XmCDropSiteEnterCallback ((char *)&_XmStrings[1456])
#endif
#ifndef XmCDropSiteLeaveCallback
#define XmCDropSiteLeaveCallback ((char *)&_XmStrings[1478])
#endif
#ifndef XmCDropSiteManagerClass
#define XmCDropSiteManagerClass ((char *)&_XmStrings[1500])
#endif
#ifndef XmCDropSiteOperations
#define XmCDropSiteOperations ((char *)&_XmStrings[1521])
#endif
#ifndef XmCDropSiteType
#define XmCDropSiteType ((char *)&_XmStrings[1540])
#endif
#ifndef XmCDropStartCallback
#define XmCDropStartCallback ((char *)&_XmStrings[1553])
#endif
#ifndef XmCDropTransferClass
#define XmCDropTransferClass ((char *)&_XmStrings[1571])
#endif
#ifndef XmCDropTransfers
#define XmCDropTransfers ((char *)&_XmStrings[1589])
#endif
#ifndef XmCEditable
#define XmCEditable ((char *)&_XmStrings[1603])
#endif
#ifndef XmCEntryBorder
#define XmCEntryBorder ((char *)&_XmStrings[1612])
#endif
#ifndef XmCEntryClass
#define XmCEntryClass ((char *)&_XmStrings[1624])
#endif
#ifndef XmCExportTargets
#define XmCExportTargets ((char *)&_XmStrings[1635])
#endif
#ifndef XmCExposeCallback
#define XmCExposeCallback ((char *)&_XmStrings[1649])
#endif
#ifndef XmCExtensionType
#define XmCExtensionType ((char *)&_XmStrings[1664])
#endif
#ifndef XmCFileListItemCount
#define XmCFileListItemCount ((char *)&_XmStrings[1678])
#endif
#ifndef XmCFileListItems
#define XmCFileListItems ((char *)&_XmStrings[1696])
#endif
#ifndef XmCFileListLabelString
#define XmCFileListLabelString ((char *)&_XmStrings[1710])
#endif
#ifndef XmCFileSearchProc
#define XmCFileSearchProc ((char *)&_XmStrings[1730])
#endif
#ifndef XmCFileTypeMask
#define XmCFileTypeMask ((char *)&_XmStrings[1745])
#endif
#ifndef XmCFillOnArm
#define XmCFillOnArm ((char *)&_XmStrings[1758])
#endif
#ifndef XmCFillOnSelect
#define XmCFillOnSelect ((char *)&_XmStrings[1768])
#endif
#ifndef XmCFilterLabelString
#define XmCFilterLabelString ((char *)&_XmStrings[1781])
#endif
#ifndef XmCFontList
#define XmCFontList ((char *)&_XmStrings[1799])
#endif
#ifndef XmCForegroundThreshold
#define XmCForegroundThreshold ((char *)&_XmStrings[1808])
#endif
#ifndef XmCHelpLabelString
#define XmCHelpLabelString ((char *)&_XmStrings[1828])
#endif
#ifndef XmCHighlightColor
#define XmCHighlightColor ((char *)&_XmStrings[1844])
#endif
#ifndef XmCHighlightOnEnter
#define XmCHighlightOnEnter ((char *)&_XmStrings[1859])
#endif
#ifndef XmCHighlightPixmap
#define XmCHighlightPixmap ((char *)&_XmStrings[1876])
#endif
#ifndef XmCHighlightThickness
#define XmCHighlightThickness ((char *)&_XmStrings[1892])
#endif
#ifndef XmCHorizontalFontUnit
#define XmCHorizontalFontUnit ((char *)&_XmStrings[1911])
#endif
#ifndef XmCHorizontalScrollBar
#define XmCHorizontalScrollBar ((char *)&_XmStrings[1930])
#endif
#ifndef XmCHot
#define XmCHot ((char *)&_XmStrings[1950])
#endif
#ifndef XmCICCHandle
#define XmCICCHandle ((char *)&_XmStrings[1954])
#endif
#ifndef XmCImportTargets
#define XmCImportTargets ((char *)&_XmStrings[1964])
#endif
#ifndef XmCIncrement
#define XmCIncrement ((char *)&_XmStrings[1978])
#endif
#ifndef XmCIncremental
#define XmCIncremental ((char *)&_XmStrings[1988])
#endif
#ifndef XmCIndicatorOn
#define XmCIndicatorOn ((char *)&_XmStrings[2000])
#endif
#ifndef XmCIndicatorSize
#define XmCIndicatorSize ((char *)&_XmStrings[2012])
#endif
#ifndef XmCIndicatorType
#define XmCIndicatorType ((char *)&_XmStrings[2026])
#endif
#ifndef XmCInitialDelay
#define XmCInitialDelay ((char *)&_XmStrings[2040])
#endif
#ifndef XmCInitialFocus
#define XmCInitialFocus ((char *)&_XmStrings[2053])
#endif
#ifndef XmCInputCreate
#define XmCInputCreate ((char *)&_XmStrings[2066])
#endif
#ifndef XmCInputMethod
#define XmCInputMethod ((char *)&_XmStrings[2078])
#endif
#ifndef XmCInvalidCursorForeground
#define XmCInvalidCursorForeground ((char *)&_XmStrings[2090])
#endif
#ifndef XmCIsAligned
#define XmCIsAligned ((char *)&_XmStrings[2114])
#endif
#ifndef XmCIsHomogeneous
#define XmCIsHomogeneous ((char *)&_XmStrings[2124])
#endif
#ifndef XmCItemCount
#define XmCItemCount ((char *)&_XmStrings[2138])
#endif
#ifndef XmCItems
#define XmCItems ((char *)&_XmStrings[2148])
#endif
#ifndef XmCKeyboardFocusPolicy
#define XmCKeyboardFocusPolicy ((char *)&_XmStrings[2154])
#endif
#ifndef XmCLabelFontList
#define XmCLabelFontList ((char *)&_XmStrings[2174])
#endif
#ifndef XmCLabelInsensitivePixmap
#define XmCLabelInsensitivePixmap ((char *)&_XmStrings[2188])
#endif
#ifndef XmCLabelPixmap
#define XmCLabelPixmap ((char *)&_XmStrings[2211])
#endif
#ifndef XmCLabelString
#define XmCLabelString ((char *)&_XmStrings[2223])
#endif
#ifndef XmCLabelType
#define XmCLabelType ((char *)&_XmStrings[2235])
#endif
#ifndef XmCLightThreshold
#define XmCLightThreshold ((char *)&_XmStrings[2245])
#endif
#ifndef XmCListLabelString
#define XmCListLabelString ((char *)&_XmStrings[2260])
#endif
#ifndef XmCListMarginHeight
#define XmCListMarginHeight ((char *)&_XmStrings[2276])
#endif
#ifndef XmCListMarginWidth
#define XmCListMarginWidth ((char *)&_XmStrings[2293])
#endif
#ifndef XmCListSizePolicy
#define XmCListSizePolicy ((char *)&_XmStrings[2309])
#endif
#ifndef XmCListSpacing
#define XmCListSpacing ((char *)&_XmStrings[2324])
#endif
#ifndef XmCListUpdated
#define XmCListUpdated ((char *)&_XmStrings[2336])
#endif
#ifndef XmCLogicalParent
#define XmCLogicalParent ((char *)&_XmStrings[2348])
#endif
#ifndef XmCMainWindowMarginHeight
#define XmCMainWindowMarginHeight ((char *)&_XmStrings[2362])
#endif
#ifndef XmCMainWindowMarginWidth
#define XmCMainWindowMarginWidth ((char *)&_XmStrings[2385])
#endif
#ifndef XmCMappingDelay
#define XmCMappingDelay ((char *)&_XmStrings[2407])
#endif
#ifndef XmCMarginBottom
#define XmCMarginBottom ((char *)&_XmStrings[2420])
#endif
#ifndef XmCMarginHeight
#define XmCMarginHeight ((char *)&_XmStrings[2433])
#endif
#ifndef XmCMarginLeft
#define XmCMarginLeft ((char *)&_XmStrings[2446])
#endif
#ifndef XmCMarginRight
#define XmCMarginRight ((char *)&_XmStrings[2457])
#endif
#ifndef XmCMarginTop
#define XmCMarginTop ((char *)&_XmStrings[2469])
#endif
#ifndef XmCMarginWidth
#define XmCMarginWidth ((char *)&_XmStrings[2479])
#endif
#ifndef XmCMask
#define XmCMask ((char *)&_XmStrings[2491])
#endif
#ifndef XmCMaxItems
#define XmCMaxItems ((char *)&_XmStrings[2496])
#endif
#ifndef XmCMaxLength
#define XmCMaxLength ((char *)&_XmStrings[2505])
#endif
#ifndef XmCMaxValue
#define XmCMaxValue ((char *)&_XmStrings[2515])
#endif
#ifndef XmCMaximum
#define XmCMaximum ((char *)&_XmStrings[2524])
#endif
#ifndef XmCMenuBar
#define XmCMenuBar ((char *)&_XmStrings[2532])
#endif
#ifndef XmCMenuPost
#define XmCMenuPost ((char *)&_XmStrings[2540])
#endif
#ifndef XmCMenuWidget
#define XmCMenuWidget ((char *)&_XmStrings[2549])
#endif
#ifndef XmCMessageProc
#define XmCMessageProc ((char *)&_XmStrings[2560])
#endif
#ifndef XmCMessageWindow
#define XmCMessageWindow ((char *)&_XmStrings[2572])
#endif
#ifndef XmCMinimizeButtons
#define XmCMinimizeButtons ((char *)&_XmStrings[2586])
#endif
#ifndef XmCMinimum
#define XmCMinimum ((char *)&_XmStrings[2602])
#endif
#ifndef XmCMnemonic
#define XmCMnemonic ((char *)&_XmStrings[2610])
#endif
#ifndef XmCMnemonicCharSet
#define XmCMnemonicCharSet ((char *)&_XmStrings[2619])
#endif
#ifndef XmCMoveOpaque
#define XmCMoveOpaque ((char *)&_XmStrings[2635])
#endif
#ifndef XmCMultiClick
#define XmCMultiClick ((char *)&_XmStrings[2646])
#endif
#ifndef XmCMustMatch
#define XmCMustMatch ((char *)&_XmStrings[2657])
#endif
#ifndef XmCMwmDecorations
#define XmCMwmDecorations ((char *)&_XmStrings[2667])
#endif
#ifndef XmCMwmFunctions
#define XmCMwmFunctions ((char *)&_XmStrings[2682])
#endif
#ifndef XmCMwmInputMode
#define XmCMwmInputMode ((char *)&_XmStrings[2695])
#endif
#ifndef XmCMwmMenu
#define XmCMwmMenu ((char *)&_XmStrings[2708])
#endif
#ifndef XmCMwmMessages
#define XmCMwmMessages ((char *)&_XmStrings[2716])
#endif
#ifndef XmCNavigationType
#define XmCNavigationType ((char *)&_XmStrings[2728])
#endif
#ifndef XmCNeedsMotion
#define XmCNeedsMotion ((char *)&_XmStrings[2743])
#endif
#ifndef XmCNoMatchString
#define XmCNoMatchString ((char *)&_XmStrings[2755])
#endif
#ifndef XmCNoResize
#define XmCNoResize ((char *)&_XmStrings[2769])
#endif
#ifndef XmCNoneCursorForeground
#define XmCNoneCursorForeground ((char *)&_XmStrings[2778])
#endif
#ifndef XmCNotifyProc
#define XmCNotifyProc ((char *)&_XmStrings[2799])
#endif
#ifndef XmCNumChildren
#define XmCNumChildren ((char *)&_XmStrings[2810])
#endif
#ifndef XmCNumColumns
#define XmCNumColumns ((char *)&_XmStrings[2822])
#endif
#ifndef XmCNumDropRectangles
#define XmCNumDropRectangles ((char *)&_XmStrings[2833])
#endif
#ifndef XmCNumDropTransfers
#define XmCNumDropTransfers ((char *)&_XmStrings[2851])
#endif
#ifndef XmCNumExportTargets
#define XmCNumExportTargets ((char *)&_XmStrings[2868])
#endif
#ifndef XmCNumImportTargets
#define XmCNumImportTargets ((char *)&_XmStrings[2885])
#endif
#ifndef XmCOffset
#define XmCOffset ((char *)&_XmStrings[2902])
#endif
#ifndef XmCOkLabelString
#define XmCOkLabelString ((char *)&_XmStrings[2909])
#endif
#ifndef XmCOperationChangedCallback
#define XmCOperationChangedCallback ((char *)&_XmStrings[2923])
#endif
#ifndef XmCOperationCursorIcon
#define XmCOperationCursorIcon ((char *)&_XmStrings[2948])
#endif
#ifndef XmCOptionLabel
#define XmCOptionLabel ((char *)&_XmStrings[2968])
#endif
#ifndef XmCOptionMnemonic
#define XmCOptionMnemonic ((char *)&_XmStrings[2980])
#endif
#ifndef XmCOutputCreate
#define XmCOutputCreate ((char *)&_XmStrings[2995])
#endif
#ifndef XmCPacking
#define XmCPacking ((char *)&_XmStrings[3008])
#endif
#ifndef XmCPageIncrement
#define XmCPageIncrement ((char *)&_XmStrings[3016])
#endif
#ifndef XmCPaneMaximum
#define XmCPaneMaximum ((char *)&_XmStrings[3030])
#endif
#ifndef XmCPaneMinimum
#define XmCPaneMinimum ((char *)&_XmStrings[3042])
#endif
#ifndef XmCPattern
#define XmCPattern ((char *)&_XmStrings[3054])
#endif
#ifndef XmCPendingDelete
#define XmCPendingDelete ((char *)&_XmStrings[3062])
#endif
#ifndef XmCPopupEnabled
#define XmCPopupEnabled ((char *)&_XmStrings[3076])
#endif
#ifndef XmCPositionIndex
#define XmCPositionIndex ((char *)&_XmStrings[3089])
#endif
#ifndef XmCPostFromButton
#define XmCPostFromButton ((char *)&_XmStrings[3103])
#endif
#ifndef XmCPostFromCount
#define XmCPostFromCount ((char *)&_XmStrings[3118])
#endif
#ifndef XmCPostFromList
#define XmCPostFromList ((char *)&_XmStrings[3132])
#endif
#ifndef XmCPreeditType
#define XmCPreeditType ((char *)&_XmStrings[3145])
#endif
#ifndef XmCProcessingDirection
#define XmCProcessingDirection ((char *)&_XmStrings[3157])
#endif
#ifndef XmCPromptString
#define XmCPromptString ((char *)&_XmStrings[3177])
#endif
#ifndef XmCProtocolCallback
#define XmCProtocolCallback ((char *)&_XmStrings[3190])
#endif
#ifndef XmCPushButtonEnabled
#define XmCPushButtonEnabled ((char *)&_XmStrings[3207])
#endif
#ifndef XmCQualifySearchDataProc
#define XmCQualifySearchDataProc ((char *)&_XmStrings[3225])
#endif
#ifndef XmCRadioAlwaysOne
#define XmCRadioAlwaysOne ((char *)&_XmStrings[3247])
#endif
#ifndef XmCRadioBehavior
#define XmCRadioBehavior ((char *)&_XmStrings[3262])
#endif
#ifndef XmCRecomputeSize
#define XmCRecomputeSize ((char *)&_XmStrings[3276])
#endif
#ifndef XmCRectangles
#define XmCRectangles ((char *)&_XmStrings[3290])
#endif
#ifndef XmCRepeatDelay
#define XmCRepeatDelay ((char *)&_XmStrings[3301])
#endif
#ifndef XmCResizeCallback
#define XmCResizeCallback ((char *)&_XmStrings[3313])
#endif
#ifndef XmCResizeHeight
#define XmCResizeHeight ((char *)&_XmStrings[3328])
#endif
#ifndef XmCResizePolicy
#define XmCResizePolicy ((char *)&_XmStrings[3341])
#endif
#ifndef XmCResizeWidth
#define XmCResizeWidth ((char *)&_XmStrings[3354])
#endif
#ifndef XmCRowColumnType
#define XmCRowColumnType ((char *)&_XmStrings[3366])
#endif
#ifndef XmCRows
#define XmCRows ((char *)&_XmStrings[3380])
#endif
#ifndef XmCRubberPositioning
#define XmCRubberPositioning ((char *)&_XmStrings[3385])
#endif
#ifndef XmCSashHeight
#define XmCSashHeight ((char *)&_XmStrings[3403])
#endif
#ifndef XmCSashIndent
#define XmCSashIndent ((char *)&_XmStrings[3414])
#endif
#ifndef XmCSashWidth
#define XmCSashWidth ((char *)&_XmStrings[3425])
#endif
#ifndef XmCScaleHeight
#define XmCScaleHeight ((char *)&_XmStrings[3435])
#endif
#ifndef XmCScaleMultiple
#define XmCScaleMultiple ((char *)&_XmStrings[3447])
#endif
#ifndef XmCScaleWidth
#define XmCScaleWidth ((char *)&_XmStrings[3461])
#endif
#ifndef XmCScroll
#define XmCScroll ((char *)&_XmStrings[3472])
#endif
#ifndef XmCScrollBarDisplayPolicy
#define XmCScrollBarDisplayPolicy ((char *)&_XmStrings[3479])
#endif
#ifndef XmCScrollBarPlacement
#define XmCScrollBarPlacement ((char *)&_XmStrings[3502])
#endif
#ifndef XmCScrollSide
#define XmCScrollSide ((char *)&_XmStrings[3521])
#endif
#ifndef XmCScrolledWindowMarginHeight
#define XmCScrolledWindowMarginHeight ((char *)&_XmStrings[3532])
#endif
#ifndef XmCScrolledWindowMarginWidth
#define XmCScrolledWindowMarginWidth ((char *)&_XmStrings[3559])
#endif
#ifndef XmCScrollingPolicy
#define XmCScrollingPolicy ((char *)&_XmStrings[3585])
#endif
#ifndef XmCSelectColor
#define XmCSelectColor ((char *)&_XmStrings[3601])
#endif
#ifndef XmCSelectInsensitivePixmap
#define XmCSelectInsensitivePixmap ((char *)&_XmStrings[3613])
#endif
#ifndef XmCSelectPixmap
#define XmCSelectPixmap ((char *)&_XmStrings[3637])
#endif
#ifndef XmCSelectThreshold
#define XmCSelectThreshold ((char *)&_XmStrings[3650])
#endif
#ifndef XmCSelectedItemCount
#define XmCSelectedItemCount ((char *)&_XmStrings[3666])
#endif
#ifndef XmCSelectedItems
#define XmCSelectedItems ((char *)&_XmStrings[3684])
#endif
#ifndef XmCSelectionArrayCount
#define XmCSelectionArrayCount ((char *)&_XmStrings[3698])
#endif
#ifndef XmCSelectionLabelString
#define XmCSelectionLabelString ((char *)&_XmStrings[3718])
#endif
#ifndef XmCSelectionPolicy
#define XmCSelectionPolicy ((char *)&_XmStrings[3739])
#endif
#ifndef XmCSeparatorOn
#define XmCSeparatorOn ((char *)&_XmStrings[3755])
#endif
#ifndef XmCSeparatorType
#define XmCSeparatorType ((char *)&_XmStrings[3767])
#endif
#ifndef XmCSet
#define XmCSet ((char *)&_XmStrings[3781])
#endif
#ifndef XmCShadowThickness
#define XmCShadowThickness ((char *)&_XmStrings[3785])
#endif
#ifndef XmCShadowType
#define XmCShadowType ((char *)&_XmStrings[3801])
#endif
#ifndef XmCShellUnitType
#define XmCShellUnitType ((char *)&_XmStrings[3812])
#endif
#ifndef XmCShowArrows
#define XmCShowArrows ((char *)&_XmStrings[3826])
#endif
#ifndef XmCShowAsDefault
#define XmCShowAsDefault ((char *)&_XmStrings[3837])
#endif
#ifndef XmCShowSeparator
#define XmCShowSeparator ((char *)&_XmStrings[3851])
#endif
#ifndef XmCShowValue
#define XmCShowValue ((char *)&_XmStrings[3865])
#endif
#ifndef XmCSimpleCheckBox
#define XmCSimpleCheckBox ((char *)&_XmStrings[3875])
#endif
#ifndef XmCSimpleMenuBar
#define XmCSimpleMenuBar ((char *)&_XmStrings[3890])
#endif
#ifndef XmCSimpleOptionMenu
#define XmCSimpleOptionMenu ((char *)&_XmStrings[3904])
#endif
#ifndef XmCSimplePopupMenu
#define XmCSimplePopupMenu ((char *)&_XmStrings[3921])
#endif
#ifndef XmCSimplePulldownMenu
#define XmCSimplePulldownMenu ((char *)&_XmStrings[3937])
#endif
#ifndef XmCSimpleRadioBox
#define XmCSimpleRadioBox ((char *)&_XmStrings[3956])
#endif
#ifndef XmCSizePolicy
#define XmCSizePolicy ((char *)&_XmStrings[3971])
#endif
#ifndef XmCSliderSize
#define XmCSliderSize ((char *)&_XmStrings[3982])
#endif
#ifndef XmCSource
#define XmCSource ((char *)&_XmStrings[3993])
#endif
#ifndef XmCSourceCursorIcon
#define XmCSourceCursorIcon ((char *)&_XmStrings[4000])
#endif
#ifndef XmCSourceIsExternal
#define XmCSourceIsExternal ((char *)&_XmStrings[4017])
#endif
#ifndef XmCSourcePixmapIcon
#define XmCSourcePixmapIcon ((char *)&_XmStrings[4034])
#endif
#ifndef XmCSourceWidget
#define XmCSourceWidget ((char *)&_XmStrings[4051])
#endif
#ifndef XmCSourceWindow
#define XmCSourceWindow ((char *)&_XmStrings[4064])
#endif
#ifndef XmCSpacing
#define XmCSpacing ((char *)&_XmStrings[4077])
#endif
#ifndef XmCStartTime
#define XmCStartTime ((char *)&_XmStrings[4085])
#endif
#ifndef XmCStateCursorIcon
#define XmCStateCursorIcon ((char *)&_XmStrings[4095])
#endif
#ifndef XmCStringDirection
#define XmCStringDirection ((char *)&_XmStrings[4111])
#endif
#ifndef XmCTearOffModel
#define XmCTearOffModel ((char *)&_XmStrings[4127])
#endif
#ifndef XmCTextFontList
#define XmCTextFontList ((char *)&_XmStrings[4140])
#endif
#ifndef XmCTextString
#define XmCTextString ((char *)&_XmStrings[4153])
#endif
#ifndef XmCTextValue
#define XmCTextValue ((char *)&_XmStrings[4164])
#endif
#ifndef XmCTitleString
#define XmCTitleString ((char *)&_XmStrings[4174])
#endif
#ifndef XmCTopCharacter
#define XmCTopCharacter ((char *)&_XmStrings[4186])
#endif
#ifndef XmCTopItemPosition
#define XmCTopItemPosition ((char *)&_XmStrings[4199])
#endif
#ifndef XmCTopLevelEnterCallback
#define XmCTopLevelEnterCallback ((char *)&_XmStrings[4215])
#endif
#ifndef XmCTopLevelLeaveCallback
#define XmCTopLevelLeaveCallback ((char *)&_XmStrings[4237])
#endif
#ifndef XmCTopShadowColor
#define XmCTopShadowColor ((char *)&_XmStrings[4259])
#endif
#ifndef XmCTopShadowPixmap
#define XmCTopShadowPixmap ((char *)&_XmStrings[4274])
#endif
#ifndef XmCTransferProc
#define XmCTransferProc ((char *)&_XmStrings[4290])
#endif
#ifndef XmCTransferStatus
#define XmCTransferStatus ((char *)&_XmStrings[4303])
#endif
#ifndef XmCTraversalOn
#define XmCTraversalOn ((char *)&_XmStrings[4318])
#endif
#ifndef XmCTraversalType
#define XmCTraversalType ((char *)&_XmStrings[4330])
#endif
#ifndef XmCTreeUpdateProc
#define XmCTreeUpdateProc ((char *)&_XmStrings[4344])
#endif
#ifndef XmCTroughColor
#define XmCTroughColor ((char *)&_XmStrings[4359])
#endif
#ifndef XmCUnitType
#define XmCUnitType ((char *)&_XmStrings[4371])
#endif
#ifndef XmCUnpostBehavior
#define XmCUnpostBehavior ((char *)&_XmStrings[4380])
#endif
#ifndef XmCUnselectPixmap
#define XmCUnselectPixmap ((char *)&_XmStrings[4395])
#endif
#ifndef XmCUpdateSliderSize
#define XmCUpdateSliderSize ((char *)&_XmStrings[4410])
#endif
#ifndef XmCUseAsyncGeometry
#define XmCUseAsyncGeometry ((char *)&_XmStrings[4427])
#endif
#ifndef XmCUserData
#define XmCUserData ((char *)&_XmStrings[4444])
#endif
#ifndef XmCValidCursorForeground
#define XmCValidCursorForeground ((char *)&_XmStrings[4453])
#endif
#ifndef XmCValueChangedCallback
#define XmCValueChangedCallback ((char *)&_XmStrings[4475])
#endif
#ifndef XmCValueWcs
#define XmCValueWcs ((char *)&_XmStrings[4496])
#endif
#ifndef XmCVerifyBell
#define XmCVerifyBell ((char *)&_XmStrings[4505])
#endif
#ifndef XmCVerticalAlignment
#define XmCVerticalAlignment ((char *)&_XmStrings[4516])
#endif
#ifndef XmCVerticalFontUnit
#define XmCVerticalFontUnit ((char *)&_XmStrings[4534])
#endif
#ifndef XmCVerticalScrollBar
#define XmCVerticalScrollBar ((char *)&_XmStrings[4551])
#endif
#ifndef XmCVisibleItemCount
#define XmCVisibleItemCount ((char *)&_XmStrings[4569])
#endif
#ifndef XmCVisibleWhenOff
#define XmCVisibleWhenOff ((char *)&_XmStrings[4586])
#endif
#ifndef XmCVisualPolicy
#define XmCVisualPolicy ((char *)&_XmStrings[4601])
#endif
#ifndef XmCWhichButton
#define XmCWhichButton ((char *)&_XmStrings[4614])
#endif
#ifndef XmCWordWrap
#define XmCWordWrap ((char *)&_XmStrings[4626])
#endif
#ifndef XmCWorkWindow
#define XmCWorkWindow ((char *)&_XmStrings[4635])
#endif
#ifndef XmCXmString
#define XmCXmString ((char *)&_XmStrings[4646])
#endif
#ifndef XmNaccelerator
#define XmNaccelerator ((char *)&_XmStrings[4655])
#endif
#ifndef XmNacceleratorText
#define XmNacceleratorText ((char *)&_XmStrings[4667])
#endif
#ifndef XmNactivateCallback
#define XmNactivateCallback ((char *)&_XmStrings[4683])
#endif
#ifndef XmNadjustLast
#define XmNadjustLast ((char *)&_XmStrings[4700])
#endif
#ifndef XmNadjustMargin
#define XmNadjustMargin ((char *)&_XmStrings[4711])
#endif
#ifndef XmNalignment
#define XmNalignment ((char *)&_XmStrings[4724])
#endif
#ifndef XmNallowOverlap
#define XmNallowOverlap ((char *)&_XmStrings[4734])
#endif
#ifndef XmNallowResize
#define XmNallowResize ((char *)&_XmStrings[4747])
#endif
#ifndef XmNanimationMask
#define XmNanimationMask ((char *)&_XmStrings[4759])
#endif
#ifndef XmNanimationPixmap
#define XmNanimationPixmap ((char *)&_XmStrings[4773])
#endif
#ifndef XmNanimationPixmapDepth
#define XmNanimationPixmapDepth ((char *)&_XmStrings[4789])
#endif
#ifndef XmNanimationStyle
#define XmNanimationStyle ((char *)&_XmStrings[4810])
#endif
#ifndef XmNapplyCallback
#define XmNapplyCallback ((char *)&_XmStrings[4825])
#endif
#ifndef XmNapplyLabelString
#define XmNapplyLabelString ((char *)&_XmStrings[4839])
#endif
#ifndef XmNarmCallback
#define XmNarmCallback ((char *)&_XmStrings[4856])
#endif
#ifndef XmNarmColor
#define XmNarmColor ((char *)&_XmStrings[4868])
#endif
#ifndef XmNarmPixmap
#define XmNarmPixmap ((char *)&_XmStrings[4877])
#endif
#ifndef XmNarrowDirection
#define XmNarrowDirection ((char *)&_XmStrings[4887])
#endif
#ifndef XmNattachment
#define XmNattachment ((char *)&_XmStrings[4902])
#endif
#ifndef XmNaudibleWarning
#define XmNaudibleWarning ((char *)&_XmStrings[4913])
#endif
#ifndef XmNautoShowCursorPosition
#define XmNautoShowCursorPosition ((char *)&_XmStrings[4928])
#endif
#ifndef XmNautoUnmanage
#define XmNautoUnmanage ((char *)&_XmStrings[4951])
#endif
#ifndef XmNautomaticSelection
#define XmNautomaticSelection ((char *)&_XmStrings[4964])
#endif
#ifndef XmNavailability
#define XmNavailability ((char *)&_XmStrings[4983])
#endif
#ifndef XmNblendModel
#define XmNblendModel ((char *)&_XmStrings[4996])
#endif
#ifndef XmNblinkRate
#define XmNblinkRate ((char *)&_XmStrings[5007])
#endif
#ifndef XmNbottomAttachment
#define XmNbottomAttachment ((char *)&_XmStrings[5017])
#endif
#ifndef XmNbottomOffset
#define XmNbottomOffset ((char *)&_XmStrings[5034])
#endif
#ifndef XmNbottomPosition
#define XmNbottomPosition ((char *)&_XmStrings[5047])
#endif
#ifndef XmNbottomShadowColor
#define XmNbottomShadowColor ((char *)&_XmStrings[5062])
#endif
#ifndef XmNbottomShadowPixmap
#define XmNbottomShadowPixmap ((char *)&_XmStrings[5080])
#endif
#ifndef XmNbottomWidget
#define XmNbottomWidget ((char *)&_XmStrings[5099])
#endif
#ifndef XmNbrowseSelectionCallback
#define XmNbrowseSelectionCallback ((char *)&_XmStrings[5112])
#endif
#ifndef XmNbuttonAcceleratorText
#define XmNbuttonAcceleratorText ((char *)&_XmStrings[5136])
#endif
#ifndef XmNbuttonAccelerators
#define XmNbuttonAccelerators ((char *)&_XmStrings[5158])
#endif
#ifndef XmNbuttonCount
#define XmNbuttonCount ((char *)&_XmStrings[5177])
#endif
#ifndef XmNbuttonFontList
#define XmNbuttonFontList ((char *)&_XmStrings[5189])
#endif
#ifndef XmNbuttonMnemonicCharSets
#define XmNbuttonMnemonicCharSets ((char *)&_XmStrings[5204])
#endif
#ifndef XmNbuttonMnemonics
#define XmNbuttonMnemonics ((char *)&_XmStrings[5227])
#endif
#ifndef XmNbuttonSet
#define XmNbuttonSet ((char *)&_XmStrings[5243])
#endif
#ifndef XmNbuttonType
#define XmNbuttonType ((char *)&_XmStrings[5253])
#endif
#ifndef XmNbuttons
#define XmNbuttons ((char *)&_XmStrings[5264])
#endif
#ifndef XmNcancelButton
#define XmNcancelButton ((char *)&_XmStrings[5272])
#endif
#ifndef XmNcancelCallback
#define XmNcancelCallback ((char *)&_XmStrings[5285])
#endif
#ifndef XmNcancelLabelString
#define XmNcancelLabelString ((char *)&_XmStrings[5300])
#endif
#ifndef XmNcascadePixmap
#define XmNcascadePixmap ((char *)&_XmStrings[5318])
#endif
#ifndef XmNcascadingCallback
#define XmNcascadingCallback ((char *)&_XmStrings[5332])
#endif
#ifndef XmNchildHorizontalAlignment
#define XmNchildHorizontalAlignment ((char *)&_XmStrings[5350])
#endif
#ifndef XmNchildHorizontalSpacing
#define XmNchildHorizontalSpacing ((char *)&_XmStrings[5375])
#endif
#ifndef XmNchildPlacement
#define XmNchildPlacement ((char *)&_XmStrings[5398])
#endif
#ifndef XmNchildPosition
#define XmNchildPosition ((char *)&_XmStrings[5413])
#endif
#ifndef XmNchildType
#define XmNchildType ((char *)&_XmStrings[5427])
#endif
#ifndef XmNchildVerticalAlignment
#define XmNchildVerticalAlignment ((char *)&_XmStrings[5437])
#endif
#ifndef XmNclientData
#define XmNclientData ((char *)&_XmStrings[5460])
#endif
#ifndef XmNclipWindow
#define XmNclipWindow ((char *)&_XmStrings[5471])
#endif
#ifndef XmNcolumns
#define XmNcolumns ((char *)&_XmStrings[5482])
#endif
#ifndef XmNcommand
#define XmNcommand ((char *)&_XmStrings[5490])
#endif
#ifndef XmNcommandChangedCallback
#define XmNcommandChangedCallback ((char *)&_XmStrings[5498])
#endif
#ifndef XmNcommandEnteredCallback
#define XmNcommandEnteredCallback ((char *)&_XmStrings[5521])
#endif
#ifndef XmNcommandWindow
#define XmNcommandWindow ((char *)&_XmStrings[5544])
#endif
#ifndef XmNcommandWindowLocation
#define XmNcommandWindowLocation ((char *)&_XmStrings[5558])
#endif
#ifndef XmNconvertProc
#define XmNconvertProc ((char *)&_XmStrings[5580])
#endif
#ifndef XmNcursorBackground
#define XmNcursorBackground ((char *)&_XmStrings[5592])
#endif
#ifndef XmNcursorForeground
#define XmNcursorForeground ((char *)&_XmStrings[5609])
#endif
#ifndef XmNcursorPosition
#define XmNcursorPosition ((char *)&_XmStrings[5626])
#endif
#ifndef XmNcursorPositionVisible
#define XmNcursorPositionVisible ((char *)&_XmStrings[5641])
#endif
#ifndef XmNdarkThreshold
#define XmNdarkThreshold ((char *)&_XmStrings[5663])
#endif
#ifndef XmNdecimalPoints
#define XmNdecimalPoints ((char *)&_XmStrings[5677])
#endif
#ifndef XmNdecrementCallback
#define XmNdecrementCallback ((char *)&_XmStrings[5691])
#endif
#ifndef XmNdefaultActionCallback
#define XmNdefaultActionCallback ((char *)&_XmStrings[5709])
#endif
#ifndef XmNdefaultButton
#define XmNdefaultButton ((char *)&_XmStrings[5731])
#endif
#ifndef XmNdefaultButtonShadowThickness
#define XmNdefaultButtonShadowThickness ((char *)&_XmStrings[5745])
#endif
#ifndef XmNdefaultButtonType
#define XmNdefaultButtonType ((char *)&_XmStrings[5774])
#endif
#ifndef XmNdefaultCopyCursorIcon
#define XmNdefaultCopyCursorIcon ((char *)&_XmStrings[5792])
#endif
#ifndef XmNdefaultFontList
#define XmNdefaultFontList ((char *)&_XmStrings[5814])
#endif
#ifndef XmNdefaultInvalidCursorIcon
#define XmNdefaultInvalidCursorIcon ((char *)&_XmStrings[5830])
#endif
#ifndef XmNdefaultLinkCursorIcon
#define XmNdefaultLinkCursorIcon ((char *)&_XmStrings[5855])
#endif
#ifndef XmNdefaultMoveCursorIcon
#define XmNdefaultMoveCursorIcon ((char *)&_XmStrings[5877])
#endif
#ifndef XmNdefaultNoneCursorIcon
#define XmNdefaultNoneCursorIcon ((char *)&_XmStrings[5899])
#endif
#ifndef XmNdefaultPosition
#define XmNdefaultPosition ((char *)&_XmStrings[5921])
#endif
#ifndef XmNdefaultSourceCursorIcon
#define XmNdefaultSourceCursorIcon ((char *)&_XmStrings[5937])
#endif
#ifndef XmNdefaultValidCursorIcon
#define XmNdefaultValidCursorIcon ((char *)&_XmStrings[5961])
#endif
#ifndef XmNdeleteResponse
#define XmNdeleteResponse ((char *)&_XmStrings[5984])
#endif
#ifndef XmNdesktopParent
#define XmNdesktopParent ((char *)&_XmStrings[5999])
#endif
#ifndef XmNdialogStyle
#define XmNdialogStyle ((char *)&_XmStrings[6013])
#endif
#ifndef XmNdialogTitle
#define XmNdialogTitle ((char *)&_XmStrings[6025])
#endif
#ifndef XmNdialogType
#define XmNdialogType ((char *)&_XmStrings[6037])
#endif
#ifndef XmNdirListItemCount
#define XmNdirListItemCount ((char *)&_XmStrings[6048])
#endif
#ifndef XmNdirListItems
#define XmNdirListItems ((char *)&_XmStrings[6065])
#endif
#ifndef XmNdirListLabelString
#define XmNdirListLabelString ((char *)&_XmStrings[6078])
#endif
#ifndef XmNdirMask
#define XmNdirMask ((char *)&_XmStrings[6097])
#endif
#ifndef XmNdirSearchProc
#define XmNdirSearchProc ((char *)&_XmStrings[6105])
#endif
#ifndef XmNdirSpec
#define XmNdirSpec ((char *)&_XmStrings[6119])
#endif
#ifndef XmNdirectory
#define XmNdirectory ((char *)&_XmStrings[6127])
#endif
#ifndef XmNdirectoryValid
#define XmNdirectoryValid ((char *)&_XmStrings[6137])
#endif
#ifndef XmNdisarmCallback
#define XmNdisarmCallback ((char *)&_XmStrings[6152])
#endif
#ifndef XmNdoubleClickInterval
#define XmNdoubleClickInterval ((char *)&_XmStrings[6167])
#endif
#ifndef XmNdragCallback
#define XmNdragCallback ((char *)&_XmStrings[6187])
#endif
#ifndef XmNdragContextClass
#define XmNdragContextClass ((char *)&_XmStrings[6200])
#endif
#ifndef XmNdragDropFinishCallback
#define XmNdragDropFinishCallback ((char *)&_XmStrings[6217])
#endif
#ifndef XmNdragIconClass
#define XmNdragIconClass ((char *)&_XmStrings[6240])
#endif
#ifndef XmNdragInitiatorProtocolStyle
#define XmNdragInitiatorProtocolStyle ((char *)&_XmStrings[6254])
#endif
#ifndef XmNdragMotionCallback
#define XmNdragMotionCallback ((char *)&_XmStrings[6281])
#endif
#ifndef XmNdragOperations
#define XmNdragOperations ((char *)&_XmStrings[6300])
#endif
#ifndef XmNdragOverMode
#define XmNdragOverMode ((char *)&_XmStrings[6315])
#endif
#ifndef XmNdragProc
#define XmNdragProc ((char *)&_XmStrings[6328])
#endif
#ifndef XmNdragReceiverProtocolStyle
#define XmNdragReceiverProtocolStyle ((char *)&_XmStrings[6337])
#endif
#ifndef XmNdropFinishCallback
#define XmNdropFinishCallback ((char *)&_XmStrings[6363])
#endif
#ifndef XmNdropProc
#define XmNdropProc ((char *)&_XmStrings[6382])
#endif
#ifndef XmNdropRectangles
#define XmNdropRectangles ((char *)&_XmStrings[6391])
#endif
#ifndef XmNdropSiteActivity
#define XmNdropSiteActivity ((char *)&_XmStrings[6406])
#endif
#ifndef XmNdropSiteEnterCallback
#define XmNdropSiteEnterCallback ((char *)&_XmStrings[6423])
#endif
#ifndef XmNdropSiteLeaveCallback
#define XmNdropSiteLeaveCallback ((char *)&_XmStrings[6445])
#endif
#ifndef XmNdropSiteManagerClass
#define XmNdropSiteManagerClass ((char *)&_XmStrings[6467])
#endif
#ifndef XmNdropSiteOperations
#define XmNdropSiteOperations ((char *)&_XmStrings[6488])
#endif
#ifndef XmNdropSiteType
#define XmNdropSiteType ((char *)&_XmStrings[6507])
#endif
#ifndef XmNdropStartCallback
#define XmNdropStartCallback ((char *)&_XmStrings[6520])
#endif
#ifndef XmNdropTransferClass
#define XmNdropTransferClass ((char *)&_XmStrings[6538])
#endif
#ifndef XmNdropTransfers
#define XmNdropTransfers ((char *)&_XmStrings[6556])
#endif
#ifndef XmNeditMode
#define XmNeditMode ((char *)&_XmStrings[6570])
#endif
#ifndef XmNeditable
#define XmNeditable ((char *)&_XmStrings[6579])
#endif
#ifndef XmNentryAlignment
#define XmNentryAlignment ((char *)&_XmStrings[6588])
#endif
#ifndef XmNentryBorder
#define XmNentryBorder ((char *)&_XmStrings[6603])
#endif
#ifndef XmNentryCallback
#define XmNentryCallback ((char *)&_XmStrings[6615])
#endif
#ifndef XmNentryClass
#define XmNentryClass ((char *)&_XmStrings[6629])
#endif
#ifndef XmNentryVerticalAlignment
#define XmNentryVerticalAlignment ((char *)&_XmStrings[6640])
#endif
#ifndef XmNexportTargets
#define XmNexportTargets ((char *)&_XmStrings[6663])
#endif
#ifndef XmNexposeCallback
#define XmNexposeCallback ((char *)&_XmStrings[6677])
#endif
#ifndef XmNextendedSelectionCallback
#define XmNextendedSelectionCallback ((char *)&_XmStrings[6692])
#endif
#ifndef XmNextensionType
#define XmNextensionType ((char *)&_XmStrings[6718])
#endif
#ifndef XmNfileListItemCount
#define XmNfileListItemCount ((char *)&_XmStrings[6732])
#endif
#ifndef XmNfileListItems
#define XmNfileListItems ((char *)&_XmStrings[6750])
#endif
#ifndef XmNfileListLabelString
#define XmNfileListLabelString ((char *)&_XmStrings[6764])
#endif
#ifndef XmNfileSearchProc
#define XmNfileSearchProc ((char *)&_XmStrings[6784])
#endif
#ifndef XmNfileTypeMask
#define XmNfileTypeMask ((char *)&_XmStrings[6799])
#endif
#ifndef XmNfillOnArm
#define XmNfillOnArm ((char *)&_XmStrings[6812])
#endif
#ifndef XmNfillOnSelect
#define XmNfillOnSelect ((char *)&_XmStrings[6822])
#endif
#ifndef XmNfilterLabelString
#define XmNfilterLabelString ((char *)&_XmStrings[6835])
#endif
#ifndef XmNfocusCallback
#define XmNfocusCallback ((char *)&_XmStrings[6853])
#endif
#ifndef XmNfocusMovedCallback
#define XmNfocusMovedCallback ((char *)&_XmStrings[6867])
#endif
#ifndef XmNfocusPolicyChanged
#define XmNfocusPolicyChanged ((char *)&_XmStrings[6886])
#endif
#ifndef XmNfontList
#define XmNfontList ((char *)&_XmStrings[6905])
#endif
#ifndef XmNforegroundThreshold
#define XmNforegroundThreshold ((char *)&_XmStrings[6914])
#endif
#ifndef XmNfractionBase
#define XmNfractionBase ((char *)&_XmStrings[6934])
#endif
#ifndef XmNgainPrimaryCallback
#define XmNgainPrimaryCallback ((char *)&_XmStrings[6947])
#endif
#ifndef XmNhelpCallback
#define XmNhelpCallback ((char *)&_XmStrings[6967])
#endif
#ifndef XmNhelpLabelString
#define XmNhelpLabelString ((char *)&_XmStrings[6980])
#endif
#ifndef XmNhighlightColor
#define XmNhighlightColor ((char *)&_XmStrings[6996])
#endif
#ifndef XmNhighlightOnEnter
#define XmNhighlightOnEnter ((char *)&_XmStrings[7011])
#endif
#ifndef XmNhighlightPixmap
#define XmNhighlightPixmap ((char *)&_XmStrings[7028])
#endif
#ifndef XmNhighlightThickness
#define XmNhighlightThickness ((char *)&_XmStrings[7044])
#endif
#ifndef XmNhistoryItemCount
#define XmNhistoryItemCount ((char *)&_XmStrings[7063])
#endif
#ifndef XmNhistoryItems
#define XmNhistoryItems ((char *)&_XmStrings[7080])
#endif
#ifndef XmNhistoryMaxItems
#define XmNhistoryMaxItems ((char *)&_XmStrings[7093])
#endif
#ifndef XmNhistoryVisibleItemCount
#define XmNhistoryVisibleItemCount ((char *)&_XmStrings[7109])
#endif
#ifndef XmNhorizontalFontUnit
#define XmNhorizontalFontUnit ((char *)&_XmStrings[7133])
#endif
#ifndef XmNhorizontalScrollBar
#define XmNhorizontalScrollBar ((char *)&_XmStrings[7152])
#endif
#ifndef XmNhorizontalSpacing
#define XmNhorizontalSpacing ((char *)&_XmStrings[7172])
#endif
#ifndef XmNhotX
#define XmNhotX ((char *)&_XmStrings[7190])
#endif
#ifndef XmNhotY
#define XmNhotY ((char *)&_XmStrings[7195])
#endif
#ifndef XmNiccHandle
#define XmNiccHandle ((char *)&_XmStrings[7200])
#endif
#ifndef XmNimportTargets
#define XmNimportTargets ((char *)&_XmStrings[7210])
#endif
#ifndef XmNincrement
#define XmNincrement ((char *)&_XmStrings[7224])
#endif
#ifndef XmNincrementCallback
#define XmNincrementCallback ((char *)&_XmStrings[7234])
#endif
#ifndef XmNincremental
#define XmNincremental ((char *)&_XmStrings[7252])
#endif
#ifndef XmNindicatorOn
#define XmNindicatorOn ((char *)&_XmStrings[7264])
#endif
#ifndef XmNindicatorSize
#define XmNindicatorSize ((char *)&_XmStrings[7276])
#endif
#ifndef XmNindicatorType
#define XmNindicatorType ((char *)&_XmStrings[7290])
#endif
#ifndef XmNinitialDelay
#define XmNinitialDelay ((char *)&_XmStrings[7304])
#endif
#ifndef XmNinitialFocus
#define XmNinitialFocus ((char *)&_XmStrings[7317])
#endif
#ifndef XmNinputCallback
#define XmNinputCallback ((char *)&_XmStrings[7330])
#endif
#ifndef XmNinputCreate
#define XmNinputCreate ((char *)&_XmStrings[7344])
#endif
#ifndef XmNinputMethod
#define XmNinputMethod ((char *)&_XmStrings[7356])
#endif
#ifndef XmNinvalidCursorForeground
#define XmNinvalidCursorForeground ((char *)&_XmStrings[7368])
#endif
#ifndef XmNisAligned
#define XmNisAligned ((char *)&_XmStrings[7392])
#endif
#ifndef XmNisHomogeneous
#define XmNisHomogeneous ((char *)&_XmStrings[7402])
#endif
#ifndef XmNitemCount
#define XmNitemCount ((char *)&_XmStrings[7416])
#endif
#ifndef XmNitems
#define XmNitems ((char *)&_XmStrings[7426])
#endif
#ifndef XmNkeyboardFocusPolicy
#define XmNkeyboardFocusPolicy ((char *)&_XmStrings[7432])
#endif
#ifndef XmNlabelFontList
#define XmNlabelFontList ((char *)&_XmStrings[7452])
#endif
#ifndef XmNlabelInsensitivePixmap
#define XmNlabelInsensitivePixmap ((char *)&_XmStrings[7466])
#endif
#ifndef XmNlabelPixmap
#define XmNlabelPixmap ((char *)&_XmStrings[7489])
#endif
#ifndef XmNlabelString
#define XmNlabelString ((char *)&_XmStrings[7501])
#endif
#ifndef XmNlabelType
#define XmNlabelType ((char *)&_XmStrings[7513])
#endif
#ifndef XmNleftAttachment
#define XmNleftAttachment ((char *)&_XmStrings[7523])
#endif
#ifndef XmNleftOffset
#define XmNleftOffset ((char *)&_XmStrings[7538])
#endif
#ifndef XmNleftPosition
#define XmNleftPosition ((char *)&_XmStrings[7549])
#endif
#ifndef XmNleftWidget
#define XmNleftWidget ((char *)&_XmStrings[7562])
#endif
#ifndef XmNlightThreshold
#define XmNlightThreshold ((char *)&_XmStrings[7573])
#endif
#ifndef XmNlineSpace
#define XmNlineSpace ((char *)&_XmStrings[7588])
#endif
#ifndef XmNlistItemCount
#define XmNlistItemCount ((char *)&_XmStrings[7598])
#endif
#ifndef XmNlistItems
#define XmNlistItems ((char *)&_XmStrings[7612])
#endif
#ifndef XmNlistLabelString
#define XmNlistLabelString ((char *)&_XmStrings[7622])
#endif
#ifndef XmNlistMarginHeight
#define XmNlistMarginHeight ((char *)&_XmStrings[7638])
#endif
#ifndef XmNlistMarginWidth
#define XmNlistMarginWidth ((char *)&_XmStrings[7655])
#endif
#ifndef XmNlistSizePolicy
#define XmNlistSizePolicy ((char *)&_XmStrings[7671])
#endif
#ifndef XmNlistSpacing
#define XmNlistSpacing ((char *)&_XmStrings[7686])
#endif
#ifndef XmNlistUpdated
#define XmNlistUpdated ((char *)&_XmStrings[7698])
#endif
#ifndef XmNlistVisibleItemCount
#define XmNlistVisibleItemCount ((char *)&_XmStrings[7710])
#endif
#ifndef XmNlogicalParent
#define XmNlogicalParent ((char *)&_XmStrings[7731])
#endif
#ifndef XmNlosePrimaryCallback
#define XmNlosePrimaryCallback ((char *)&_XmStrings[7745])
#endif
#ifndef XmNlosingFocusCallback
#define XmNlosingFocusCallback ((char *)&_XmStrings[7765])
#endif
#ifndef XmNmainWindowMarginHeight
#define XmNmainWindowMarginHeight ((char *)&_XmStrings[7785])
#endif
#ifndef XmNmainWindowMarginWidth
#define XmNmainWindowMarginWidth ((char *)&_XmStrings[7808])
#endif
#ifndef XmNmapCallback
#define XmNmapCallback ((char *)&_XmStrings[7830])
#endif
#ifndef XmNmappingDelay
#define XmNmappingDelay ((char *)&_XmStrings[7842])
#endif
#ifndef XmNmargin
#define XmNmargin ((char *)&_XmStrings[7855])
#endif
#ifndef XmNmarginBottom
#define XmNmarginBottom ((char *)&_XmStrings[7862])
#endif
#ifndef XmNmarginHeight
#define XmNmarginHeight ((char *)&_XmStrings[7875])
#endif
#ifndef XmNmarginLeft
#define XmNmarginLeft ((char *)&_XmStrings[7888])
#endif
#ifndef XmNmarginRight
#define XmNmarginRight ((char *)&_XmStrings[7899])
#endif
#ifndef XmNmarginTop
#define XmNmarginTop ((char *)&_XmStrings[7911])
#endif
#ifndef XmNmarginWidth
#define XmNmarginWidth ((char *)&_XmStrings[7921])
#endif
#ifndef XmNmask
#define XmNmask ((char *)&_XmStrings[7933])
#endif
#ifndef XmNmaxLength
#define XmNmaxLength ((char *)&_XmStrings[7938])
#endif
#ifndef XmNmaximum
#define XmNmaximum ((char *)&_XmStrings[7948])
#endif
#ifndef XmNmenuAccelerator
#define XmNmenuAccelerator ((char *)&_XmStrings[7956])
#endif
#ifndef XmNmenuBar
#define XmNmenuBar ((char *)&_XmStrings[7972])
#endif
#ifndef XmNmenuCursor
#define XmNmenuCursor ((char *)&_XmStrings[7980])
#endif
#ifndef XmNmenuHelpWidget
#define XmNmenuHelpWidget ((char *)&_XmStrings[7991])
#endif
#ifndef XmNmenuHistory
#define XmNmenuHistory ((char *)&_XmStrings[8006])
#endif
#ifndef XmNmenuPost
#define XmNmenuPost ((char *)&_XmStrings[8018])
#endif
#ifndef XmNmessageAlignment
#define XmNmessageAlignment ((char *)&_XmStrings[8027])
#endif
#ifndef XmNmessageProc
#define XmNmessageProc ((char *)&_XmStrings[8044])
#endif
#ifndef XmNmessageString
#define XmNmessageString ((char *)&_XmStrings[8056])
#endif
#ifndef XmNmessageWindow
#define XmNmessageWindow ((char *)&_XmStrings[8070])
#endif
#ifndef XmNminimizeButtons
#define XmNminimizeButtons ((char *)&_XmStrings[8084])
#endif
#ifndef XmNminimum
#define XmNminimum ((char *)&_XmStrings[8100])
#endif
#ifndef XmNmnemonic
#define XmNmnemonic ((char *)&_XmStrings[8108])
#endif
#ifndef XmNmnemonicCharSet
#define XmNmnemonicCharSet ((char *)&_XmStrings[8117])
#endif
#ifndef XmNmodifyVerifyCallback
#define XmNmodifyVerifyCallback ((char *)&_XmStrings[8133])
#endif
#ifndef XmNmodifyVerifyCallbackWcs
#define XmNmodifyVerifyCallbackWcs ((char *)&_XmStrings[8154])
#endif
#ifndef XmNmotionVerifyCallback
#define XmNmotionVerifyCallback ((char *)&_XmStrings[8178])
#endif
#ifndef XmNmoveOpaque
#define XmNmoveOpaque ((char *)&_XmStrings[8199])
#endif
#ifndef XmNmultiClick
#define XmNmultiClick ((char *)&_XmStrings[8210])
#endif
#ifndef XmNmultipleSelectionCallback
#define XmNmultipleSelectionCallback ((char *)&_XmStrings[8221])
#endif
#ifndef XmNmustMatch
#define XmNmustMatch ((char *)&_XmStrings[8247])
#endif
#ifndef XmNmwmDecorations
#define XmNmwmDecorations ((char *)&_XmStrings[8257])
#endif
#ifndef XmNmwmFunctions
#define XmNmwmFunctions ((char *)&_XmStrings[8272])
#endif
#ifndef XmNmwmInputMode
#define XmNmwmInputMode ((char *)&_XmStrings[8285])
#endif
#ifndef XmNmwmMenu
#define XmNmwmMenu ((char *)&_XmStrings[8298])
#endif
#ifndef XmNmwmMessages
#define XmNmwmMessages ((char *)&_XmStrings[8306])
#endif
#ifndef XmNnavigationType
#define XmNnavigationType ((char *)&_XmStrings[8318])
#endif
#ifndef XmNneedsMotion
#define XmNneedsMotion ((char *)&_XmStrings[8333])
#endif
#ifndef XmNnoMatchCallback
#define XmNnoMatchCallback ((char *)&_XmStrings[8345])
#endif
#ifndef XmNnoMatchString
#define XmNnoMatchString ((char *)&_XmStrings[8361])
#endif
#ifndef XmNnoResize
#define XmNnoResize ((char *)&_XmStrings[8375])
#endif
#ifndef XmNnoneCursorForeground
#define XmNnoneCursorForeground ((char *)&_XmStrings[8384])
#endif
#ifndef XmNnotifyProc
#define XmNnotifyProc ((char *)&_XmStrings[8405])
#endif
#ifndef XmNnumColumns
#define XmNnumColumns ((char *)&_XmStrings[8416])
#endif
#ifndef XmNnumDropRectangles
#define XmNnumDropRectangles ((char *)&_XmStrings[8427])
#endif
#ifndef XmNnumDropTransfers
#define XmNnumDropTransfers ((char *)&_XmStrings[8445])
#endif
#ifndef XmNnumExportTargets
#define XmNnumExportTargets ((char *)&_XmStrings[8462])
#endif
#ifndef XmNnumImportTargets
#define XmNnumImportTargets ((char *)&_XmStrings[8479])
#endif
#ifndef XmNnumRectangles
#define XmNnumRectangles ((char *)&_XmStrings[8496])
#endif
#ifndef XmNoffsetX
#define XmNoffsetX ((char *)&_XmStrings[8510])
#endif
#ifndef XmNoffsetY
#define XmNoffsetY ((char *)&_XmStrings[8518])
#endif
#ifndef XmNokCallback
#define XmNokCallback ((char *)&_XmStrings[8526])
#endif
#ifndef XmNokLabelString
#define XmNokLabelString ((char *)&_XmStrings[8537])
#endif
#ifndef XmNoperationChangedCallback
#define XmNoperationChangedCallback ((char *)&_XmStrings[8551])
#endif
#ifndef XmNoperationCursorIcon
#define XmNoperationCursorIcon ((char *)&_XmStrings[8576])
#endif
#ifndef XmNoptionLabel
#define XmNoptionLabel ((char *)&_XmStrings[8596])
#endif
#ifndef XmNoptionMnemonic
#define XmNoptionMnemonic ((char *)&_XmStrings[8608])
#endif
#ifndef XmNoutputCreate
#define XmNoutputCreate ((char *)&_XmStrings[8623])
#endif
#ifndef XmNpacking
#define XmNpacking ((char *)&_XmStrings[8636])
#endif
#ifndef XmNpageDecrementCallback
#define XmNpageDecrementCallback ((char *)&_XmStrings[8644])
#endif
#ifndef XmNpageIncrement
#define XmNpageIncrement ((char *)&_XmStrings[8666])
#endif
#ifndef XmNpageIncrementCallback
#define XmNpageIncrementCallback ((char *)&_XmStrings[8680])
#endif
#ifndef XmNpaneMaximum
#define XmNpaneMaximum ((char *)&_XmStrings[8702])
#endif
#ifndef XmNpaneMinimum
#define XmNpaneMinimum ((char *)&_XmStrings[8714])
#endif
#ifndef XmNpattern
#define XmNpattern ((char *)&_XmStrings[8726])
#endif
#ifndef XmNpendingDelete
#define XmNpendingDelete ((char *)&_XmStrings[8734])
#endif
#ifndef XmNpopupEnabled
#define XmNpopupEnabled ((char *)&_XmStrings[8748])
#endif
#ifndef XmNpositionIndex
#define XmNpositionIndex ((char *)&_XmStrings[8761])
#endif
#ifndef XmNpostFromButton
#define XmNpostFromButton ((char *)&_XmStrings[8775])
#endif
#ifndef XmNpostFromCount
#define XmNpostFromCount ((char *)&_XmStrings[8790])
#endif
#ifndef XmNpostFromList
#define XmNpostFromList ((char *)&_XmStrings[8804])
#endif
#ifndef XmNpreeditType
#define XmNpreeditType ((char *)&_XmStrings[8817])
#endif
#ifndef XmNprocessingDirection
#define XmNprocessingDirection ((char *)&_XmStrings[8829])
#endif
#ifndef XmNpromptString
#define XmNpromptString ((char *)&_XmStrings[8849])
#endif
#ifndef XmNprotocolCallback
#define XmNprotocolCallback ((char *)&_XmStrings[8862])
#endif
#ifndef XmNpushButtonEnabled
#define XmNpushButtonEnabled ((char *)&_XmStrings[8879])
#endif
#ifndef XmNqualifySearchDataProc
#define XmNqualifySearchDataProc ((char *)&_XmStrings[8897])
#endif
#ifndef XmNradioAlwaysOne
#define XmNradioAlwaysOne ((char *)&_XmStrings[8919])
#endif
#ifndef XmNradioBehavior
#define XmNradioBehavior ((char *)&_XmStrings[8934])
#endif
#ifndef XmNrealizeCallback
#define XmNrealizeCallback ((char *)&_XmStrings[8948])
#endif
#ifndef XmNrecomputeSize
#define XmNrecomputeSize ((char *)&_XmStrings[8964])
#endif
#ifndef XmNrectangles
#define XmNrectangles ((char *)&_XmStrings[8978])
#endif
#ifndef XmNrefigureMode
#define XmNrefigureMode ((char *)&_XmStrings[8989])
#endif
#ifndef XmNrepeatDelay
#define XmNrepeatDelay ((char *)&_XmStrings[9002])
#endif
#ifndef XmNresizable
#define XmNresizable ((char *)&_XmStrings[9014])
#endif
#ifndef XmNresizeCallback
#define XmNresizeCallback ((char *)&_XmStrings[9024])
#endif
#ifndef XmNresizeHeight
#define XmNresizeHeight ((char *)&_XmStrings[9039])
#endif
#ifndef XmNresizePolicy
#define XmNresizePolicy ((char *)&_XmStrings[9052])
#endif
#ifndef XmNresizeWidth
#define XmNresizeWidth ((char *)&_XmStrings[9065])
#endif
#ifndef XmNrightAttachment
#define XmNrightAttachment ((char *)&_XmStrings[9077])
#endif
#ifndef XmNrightOffset
#define XmNrightOffset ((char *)&_XmStrings[9093])
#endif
#ifndef XmNrightPosition
#define XmNrightPosition ((char *)&_XmStrings[9105])
#endif
#ifndef XmNrightWidget
#define XmNrightWidget ((char *)&_XmStrings[9119])
#endif
#ifndef XmNrowColumnType
#define XmNrowColumnType ((char *)&_XmStrings[9131])
#endif
#ifndef XmNrows
#define XmNrows ((char *)&_XmStrings[9145])
#endif
#ifndef XmNrubberPositioning
#define XmNrubberPositioning ((char *)&_XmStrings[9150])
#endif
#ifndef XmNsashHeight
#define XmNsashHeight ((char *)&_XmStrings[9168])
#endif
#ifndef XmNsashIndent
#define XmNsashIndent ((char *)&_XmStrings[9179])
#endif
#ifndef XmNsashShadowThickness
#define XmNsashShadowThickness ((char *)&_XmStrings[9190])
#endif
#ifndef XmNsashWidth
#define XmNsashWidth ((char *)&_XmStrings[9210])
#endif
#ifndef XmNscaleHeight
#define XmNscaleHeight ((char *)&_XmStrings[9220])
#endif
#ifndef XmNscaleMultiple
#define XmNscaleMultiple ((char *)&_XmStrings[9232])
#endif
#ifndef XmNscaleWidth
#define XmNscaleWidth ((char *)&_XmStrings[9246])
#endif
#ifndef XmNscrollBarDisplayPolicy
#define XmNscrollBarDisplayPolicy ((char *)&_XmStrings[9257])
#endif
#ifndef XmNscrollBarPlacement
#define XmNscrollBarPlacement ((char *)&_XmStrings[9280])
#endif
#ifndef XmNscrollHorizontal
#define XmNscrollHorizontal ((char *)&_XmStrings[9299])
#endif
#ifndef XmNscrollLeftSide
#define XmNscrollLeftSide ((char *)&_XmStrings[9316])
#endif
#ifndef XmNscrollTopSide
#define XmNscrollTopSide ((char *)&_XmStrings[9331])
#endif
#ifndef XmNscrollVertical
#define XmNscrollVertical ((char *)&_XmStrings[9345])
#endif
#ifndef XmNscrolledWindowMarginHeight
#define XmNscrolledWindowMarginHeight ((char *)&_XmStrings[9360])
#endif
#ifndef XmNscrolledWindowMarginWidth
#define XmNscrolledWindowMarginWidth ((char *)&_XmStrings[9387])
#endif
#ifndef XmNscrollingPolicy
#define XmNscrollingPolicy ((char *)&_XmStrings[9413])
#endif
#ifndef XmNselectColor
#define XmNselectColor ((char *)&_XmStrings[9429])
#endif
#ifndef XmNselectInsensitivePixmap
#define XmNselectInsensitivePixmap ((char *)&_XmStrings[9441])
#endif
#ifndef XmNselectPixmap
#define XmNselectPixmap ((char *)&_XmStrings[9465])
#endif
#ifndef XmNselectThreshold
#define XmNselectThreshold ((char *)&_XmStrings[9478])
#endif
#ifndef XmNselectedItemCount
#define XmNselectedItemCount ((char *)&_XmStrings[9494])
#endif
#ifndef XmNselectedItems
#define XmNselectedItems ((char *)&_XmStrings[9512])
#endif
#ifndef XmNselectionArrayCount
#define XmNselectionArrayCount ((char *)&_XmStrings[9526])
#endif
#ifndef XmNselectionLabelString
#define XmNselectionLabelString ((char *)&_XmStrings[9546])
#endif
#ifndef XmNselectionPolicy
#define XmNselectionPolicy ((char *)&_XmStrings[9567])
#endif
#ifndef XmNseparatorOn
#define XmNseparatorOn ((char *)&_XmStrings[9583])
#endif
#ifndef XmNseparatorType
#define XmNseparatorType ((char *)&_XmStrings[9595])
#endif
#ifndef XmNset
#define XmNset ((char *)&_XmStrings[9609])
#endif
#ifndef XmNshadow
#define XmNshadow ((char *)&_XmStrings[9613])
#endif
#ifndef XmNshadowThickness
#define XmNshadowThickness ((char *)&_XmStrings[9620])
#endif
#ifndef XmNshadowType
#define XmNshadowType ((char *)&_XmStrings[9636])
#endif
#ifndef XmNshellUnitType
#define XmNshellUnitType ((char *)&_XmStrings[9647])
#endif
#ifndef XmNshowArrows
#define XmNshowArrows ((char *)&_XmStrings[9661])
#endif
#ifndef XmNshowAsDefault
#define XmNshowAsDefault ((char *)&_XmStrings[9672])
#endif
#ifndef XmNshowSeparator
#define XmNshowSeparator ((char *)&_XmStrings[9686])
#endif
#ifndef XmNshowValue
#define XmNshowValue ((char *)&_XmStrings[9700])
#endif
#ifndef XmNsimpleCallback
#define XmNsimpleCallback ((char *)&_XmStrings[9710])
#endif
#ifndef XmNsingleSelectionCallback
#define XmNsingleSelectionCallback ((char *)&_XmStrings[9725])
#endif
#ifndef XmNsizePolicy
#define XmNsizePolicy ((char *)&_XmStrings[9749])
#endif
#ifndef XmNskipAdjust
#define XmNskipAdjust ((char *)&_XmStrings[9760])
#endif
#ifndef XmNsliderSize
#define XmNsliderSize ((char *)&_XmStrings[9771])
#endif
#ifndef XmNsource
#define XmNsource ((char *)&_XmStrings[9782])
#endif
#ifndef XmNsourceCursorIcon
#define XmNsourceCursorIcon ((char *)&_XmStrings[9789])
#endif
#ifndef XmNsourceIsExternal
#define XmNsourceIsExternal ((char *)&_XmStrings[9806])
#endif
#ifndef XmNsourcePixmapIcon
#define XmNsourcePixmapIcon ((char *)&_XmStrings[9823])
#endif
#ifndef XmNsourceWidget
#define XmNsourceWidget ((char *)&_XmStrings[9840])
#endif
#ifndef XmNsourceWindow
#define XmNsourceWindow ((char *)&_XmStrings[9853])
#endif
#ifndef XmNspacing
#define XmNspacing ((char *)&_XmStrings[9866])
#endif
#ifndef XmNspotLocation
#define XmNspotLocation ((char *)&_XmStrings[9874])
#endif
#ifndef XmNstartTime
#define XmNstartTime ((char *)&_XmStrings[9887])
#endif
#ifndef XmNstateCursorIcon
#define XmNstateCursorIcon ((char *)&_XmStrings[9897])
#endif
#ifndef XmNstringDirection
#define XmNstringDirection ((char *)&_XmStrings[9913])
#endif
#ifndef XmNsubMenuId
#define XmNsubMenuId ((char *)&_XmStrings[9929])
#endif
#ifndef XmNsymbolPixmap
#define XmNsymbolPixmap ((char *)&_XmStrings[9939])
#endif
#ifndef XmNtearOffMenuActivateCallback
#define XmNtearOffMenuActivateCallback ((char *)&_XmStrings[9952])
#endif
#ifndef XmNtearOffMenuDeactivateCallback
#define XmNtearOffMenuDeactivateCallback ((char *)&_XmStrings[9980])
#endif
#ifndef XmNtearOffModel
#define XmNtearOffModel ((char *)&_XmStrings[10010])
#endif
#ifndef XmNtextAccelerators
#define XmNtextAccelerators ((char *)&_XmStrings[10023])
#endif
#ifndef XmNtextColumns
#define XmNtextColumns ((char *)&_XmStrings[10040])
#endif
#ifndef XmNtextFontList
#define XmNtextFontList ((char *)&_XmStrings[10052])
#endif
#ifndef XmNtextString
#define XmNtextString ((char *)&_XmStrings[10065])
#endif
#ifndef XmNtextTranslations
#define XmNtextTranslations ((char *)&_XmStrings[10076])
#endif
#ifndef XmNtextValue
#define XmNtextValue ((char *)&_XmStrings[10093])
#endif
#ifndef XmNtitleString
#define XmNtitleString ((char *)&_XmStrings[10103])
#endif
#ifndef XmNtoBottomCallback
#define XmNtoBottomCallback ((char *)&_XmStrings[10115])
#endif
#ifndef XmNtoPositionCallback
#define XmNtoPositionCallback ((char *)&_XmStrings[10132])
#endif
#ifndef XmNtoTopCallback
#define XmNtoTopCallback ((char *)&_XmStrings[10151])
#endif
#ifndef XmNtopAttachment
#define XmNtopAttachment ((char *)&_XmStrings[10165])
#endif
#ifndef XmNtopCharacter
#define XmNtopCharacter ((char *)&_XmStrings[10179])
#endif
#ifndef XmNtopItemPosition
#define XmNtopItemPosition ((char *)&_XmStrings[10192])
#endif
#ifndef XmNtopLevelEnterCallback
#define XmNtopLevelEnterCallback ((char *)&_XmStrings[10208])
#endif
#ifndef XmNtopLevelLeaveCallback
#define XmNtopLevelLeaveCallback ((char *)&_XmStrings[10230])
#endif
#ifndef XmNtopOffset
#define XmNtopOffset ((char *)&_XmStrings[10252])
#endif
#ifndef XmNtopPosition
#define XmNtopPosition ((char *)&_XmStrings[10262])
#endif
#ifndef XmNtopShadowColor
#define XmNtopShadowColor ((char *)&_XmStrings[10274])
#endif
#ifndef XmNtopShadowPixmap
#define XmNtopShadowPixmap ((char *)&_XmStrings[10289])
#endif
#ifndef XmNtopWidget
#define XmNtopWidget ((char *)&_XmStrings[10305])
#endif
#ifndef XmNtransferProc
#define XmNtransferProc ((char *)&_XmStrings[10315])
#endif
#ifndef XmNtransferStatus
#define XmNtransferStatus ((char *)&_XmStrings[10328])
#endif
#ifndef XmNtraversalCallback
#define XmNtraversalCallback ((char *)&_XmStrings[10343])
#endif
#ifndef XmNtraversalOn
#define XmNtraversalOn ((char *)&_XmStrings[10361])
#endif
#ifndef XmNtraversalType
#define XmNtraversalType ((char *)&_XmStrings[10373])
#endif
#ifndef XmNtraverseObscuredCallback
#define XmNtraverseObscuredCallback ((char *)&_XmStrings[10387])
#endif
#ifndef XmNtreeUpdateProc
#define XmNtreeUpdateProc ((char *)&_XmStrings[10412])
#endif
#ifndef XmNtroughColor
#define XmNtroughColor ((char *)&_XmStrings[10427])
#endif
#ifndef XmNunitType
#define XmNunitType ((char *)&_XmStrings[10439])
#endif
#ifndef XmNunmapCallback
#define XmNunmapCallback ((char *)&_XmStrings[10448])
#endif
#ifndef XmNunpostBehavior
#define XmNunpostBehavior ((char *)&_XmStrings[10462])
#endif
#ifndef XmNunselectPixmap
#define XmNunselectPixmap ((char *)&_XmStrings[10477])
#endif
#ifndef XmNupdateSliderSize
#define XmNupdateSliderSize ((char *)&_XmStrings[10492])
#endif
#ifndef XmNuseAsyncGeometry
#define XmNuseAsyncGeometry ((char *)&_XmStrings[10509])
#endif
#ifndef XmNuserData
#define XmNuserData ((char *)&_XmStrings[10526])
#endif
#ifndef XmNvalidCursorForeground
#define XmNvalidCursorForeground ((char *)&_XmStrings[10535])
#endif
#ifndef XmNvalueChangedCallback
#define XmNvalueChangedCallback ((char *)&_XmStrings[10557])
#endif
#ifndef XmNvalueWcs
#define XmNvalueWcs ((char *)&_XmStrings[10578])
#endif
#ifndef XmNverifyBell
#define XmNverifyBell ((char *)&_XmStrings[10587])
#endif
#ifndef XmNverticalFontUnit
#define XmNverticalFontUnit ((char *)&_XmStrings[10598])
#endif
#ifndef XmNverticalScrollBar
#define XmNverticalScrollBar ((char *)&_XmStrings[10615])
#endif
#ifndef XmNverticalSpacing
#define XmNverticalSpacing ((char *)&_XmStrings[10633])
#endif
#ifndef XmNvisibleItemCount
#define XmNvisibleItemCount ((char *)&_XmStrings[10649])
#endif
#ifndef XmNvisibleWhenOff
#define XmNvisibleWhenOff ((char *)&_XmStrings[10666])
#endif
#ifndef XmNvisualPolicy
#define XmNvisualPolicy ((char *)&_XmStrings[10681])
#endif
#ifndef XmNwhichButton
#define XmNwhichButton ((char *)&_XmStrings[10694])
#endif
#ifndef XmNwordWrap
#define XmNwordWrap ((char *)&_XmStrings[10706])
#endif
#ifndef XmNworkWindow
#define XmNworkWindow ((char *)&_XmStrings[10715])
#endif
#ifndef XmRAlignment
#define XmRAlignment ((char *)&_XmStrings[10726])
#endif
#ifndef XmRAnimationMask
#define XmRAnimationMask ((char *)&_XmStrings[10736])
#endif
#ifndef XmRAnimationPixmap
#define XmRAnimationPixmap ((char *)&_XmStrings[10750])
#endif
#ifndef XmRAnimationStyle
#define XmRAnimationStyle ((char *)&_XmStrings[10766])
#endif
#ifndef XmRArrowDirection
#define XmRArrowDirection ((char *)&_XmStrings[10781])
#endif
#ifndef XmRAtomList
#define XmRAtomList ((char *)&_XmStrings[10796])
#endif
#ifndef XmRAttachment
#define XmRAttachment ((char *)&_XmStrings[10805])
#endif
#ifndef XmRAudibleWarning
#define XmRAudibleWarning ((char *)&_XmStrings[10816])
#endif
#ifndef XmRAvailability
#define XmRAvailability ((char *)&_XmStrings[10831])
#endif
#ifndef XmRBackgroundPixmap
#define XmRBackgroundPixmap ((char *)&_XmStrings[10844])
#endif
#ifndef XmRBlendModel
#define XmRBlendModel ((char *)&_XmStrings[10861])
#endif
#ifndef XmRBooleanDimension
#define XmRBooleanDimension ((char *)&_XmStrings[10872])
#endif
#ifndef XmRBottomShadowPixmap
#define XmRBottomShadowPixmap ((char *)&_XmStrings[10889])
#endif
#ifndef XmRButtonType
#define XmRButtonType ((char *)&_XmStrings[10908])
#endif
#ifndef XmRCallbackProc
#define XmRCallbackProc ((char *)&_XmStrings[10919])
#endif
#ifndef XmRChar
#define XmRChar ((char *)&_XmStrings[10932])
#endif
#ifndef XmRCharSetTable
#define XmRCharSetTable ((char *)&_XmStrings[10937])
#endif
#ifndef XmRChildHorizontalAlignment
#define XmRChildHorizontalAlignment ((char *)&_XmStrings[10950])
#endif
#ifndef XmRChildPlacement
#define XmRChildPlacement ((char *)&_XmStrings[10975])
#endif
#ifndef XmRChildType
#define XmRChildType ((char *)&_XmStrings[10990])
#endif
#ifndef XmRChildVerticalAlignment
#define XmRChildVerticalAlignment ((char *)&_XmStrings[11000])
#endif
#ifndef XmRCommandWindowLocation
#define XmRCommandWindowLocation ((char *)&_XmStrings[11023])
#endif
#ifndef XmRCompoundText
#define XmRCompoundText ((char *)&_XmStrings[11045])
#endif
#ifndef XmRDefaultButtonType
#define XmRDefaultButtonType ((char *)&_XmStrings[11058])
#endif
#ifndef XmRDeleteResponse
#define XmRDeleteResponse ((char *)&_XmStrings[11076])
#endif
#ifndef XmRDialogStyle
#define XmRDialogStyle ((char *)&_XmStrings[11091])
#endif
#ifndef XmRDialogType
#define XmRDialogType ((char *)&_XmStrings[11103])
#endif
#ifndef XmRDoubleClickInterval
#define XmRDoubleClickInterval ((char *)&_XmStrings[11114])
#endif
#ifndef XmRDragInitiatorProtocolStyle
#define XmRDragInitiatorProtocolStyle ((char *)&_XmStrings[11134])
#endif
#ifndef XmRDragReceiverProtocolStyle
#define XmRDragReceiverProtocolStyle ((char *)&_XmStrings[11161])
#endif
#ifndef XmRDropSiteActivity
#define XmRDropSiteActivity ((char *)&_XmStrings[11187])
#endif
#ifndef XmRDropSiteOperations
#define XmRDropSiteOperations ((char *)&_XmStrings[11204])
#endif
#ifndef XmRDropSiteType
#define XmRDropSiteType ((char *)&_XmStrings[11223])
#endif
#ifndef XmRDropTransfers
#define XmRDropTransfers ((char *)&_XmStrings[11236])
#endif
#ifndef XmRExtensionType
#define XmRExtensionType ((char *)&_XmStrings[11250])
#endif
#ifndef XmRFileTypeMask
#define XmRFileTypeMask ((char *)&_XmStrings[11264])
#endif
#ifndef XmRFontList
#define XmRFontList ((char *)&_XmStrings[11277])
#endif
#ifndef XmRGadgetPixmap
#define XmRGadgetPixmap ((char *)&_XmStrings[11286])
#endif
#ifndef XmRHighlightPixmap
#define XmRHighlightPixmap ((char *)&_XmStrings[11299])
#endif
#ifndef XmRHorizontalDimension
#define XmRHorizontalDimension ((char *)&_XmStrings[11315])
#endif
#ifndef XmRHorizontalInt
#define XmRHorizontalInt ((char *)&_XmStrings[11335])
#endif
#ifndef XmRHorizontalPosition
#define XmRHorizontalPosition ((char *)&_XmStrings[11349])
#endif
#ifndef XmRIconAttachment
#define XmRIconAttachment ((char *)&_XmStrings[11368])
#endif
#ifndef XmRImportTargets
#define XmRImportTargets ((char *)&_XmStrings[11383])
#endif
#ifndef XmRIndicatorType
#define XmRIndicatorType ((char *)&_XmStrings[11397])
#endif
#ifndef XmRItemCount
#define XmRItemCount ((char *)&_XmStrings[11411])
#endif
#ifndef XmRItems
#define XmRItems ((char *)&_XmStrings[11421])
#endif
#ifndef XmRKeySym
#define XmRKeySym ((char *)&_XmStrings[11427])
#endif
#ifndef XmRKeySymTable
#define XmRKeySymTable ((char *)&_XmStrings[11434])
#endif
#ifndef XmRKeyboardFocusPolicy
#define XmRKeyboardFocusPolicy ((char *)&_XmStrings[11446])
#endif
#ifndef XmRLabelType
#define XmRLabelType ((char *)&_XmStrings[11466])
#endif
#ifndef XmRListMarginHeight
#define XmRListMarginHeight ((char *)&_XmStrings[11476])
#endif
#ifndef XmRListMarginWidth
#define XmRListMarginWidth ((char *)&_XmStrings[11493])
#endif
#ifndef XmRListSizePolicy
#define XmRListSizePolicy ((char *)&_XmStrings[11509])
#endif
#ifndef XmRListSpacing
#define XmRListSpacing ((char *)&_XmStrings[11524])
#endif
#ifndef XmRManBottomShadowPixmap
#define XmRManBottomShadowPixmap ((char *)&_XmStrings[11536])
#endif
#ifndef XmRManForegroundPixmap
#define XmRManForegroundPixmap ((char *)&_XmStrings[11558])
#endif
#ifndef XmRManHighlightPixmap
#define XmRManHighlightPixmap ((char *)&_XmStrings[11578])
#endif
#ifndef XmRManTopShadowPixmap
#define XmRManTopShadowPixmap ((char *)&_XmStrings[11597])
#endif
#ifndef XmRMenuWidget
#define XmRMenuWidget ((char *)&_XmStrings[11616])
#endif
#ifndef XmRMnemonic
#define XmRMnemonic ((char *)&_XmStrings[11627])
#endif
#ifndef XmRMultiClick
#define XmRMultiClick ((char *)&_XmStrings[11636])
#endif
#ifndef XmRNavigationType
#define XmRNavigationType ((char *)&_XmStrings[11647])
#endif
#ifndef XmRPacking
#define XmRPacking ((char *)&_XmStrings[11662])
#endif
#ifndef XmRPrimForegroundPixmap
#define XmRPrimForegroundPixmap ((char *)&_XmStrings[11670])
#endif
#ifndef XmRProc
#define XmRProc ((char *)&_XmStrings[11691])
#endif
#ifndef XmRProcessingDirection
#define XmRProcessingDirection ((char *)&_XmStrings[11696])
#endif
#ifndef XmRRectangleList
#define XmRRectangleList ((char *)&_XmStrings[11716])
#endif
#ifndef XmRResizePolicy
#define XmRResizePolicy ((char *)&_XmStrings[11730])
#endif
#ifndef XmRRowColumnType
#define XmRRowColumnType ((char *)&_XmStrings[11743])
#endif
#ifndef XmRScrollBarDisplayPolicy
#define XmRScrollBarDisplayPolicy ((char *)&_XmStrings[11757])
#endif
#ifndef XmRScrollBarPlacement
#define XmRScrollBarPlacement ((char *)&_XmStrings[11780])
#endif
#ifndef XmRScrollingPolicy
#define XmRScrollingPolicy ((char *)&_XmStrings[11799])
#endif
#ifndef XmRSelectedItemCount
#define XmRSelectedItemCount ((char *)&_XmStrings[11815])
#endif
#ifndef XmRSelectedItems
#define XmRSelectedItems ((char *)&_XmStrings[11833])
#endif
#ifndef XmRSelectionPolicy
#define XmRSelectionPolicy ((char *)&_XmStrings[11847])
#endif
#ifndef XmRSelectionType
#define XmRSelectionType ((char *)&_XmStrings[11863])
#endif
#ifndef XmRSeparatorType
#define XmRSeparatorType ((char *)&_XmStrings[11877])
#endif
#ifndef XmRShadowType
#define XmRShadowType ((char *)&_XmStrings[11891])
#endif
#ifndef XmRShellHorizDim
#define XmRShellHorizDim ((char *)&_XmStrings[11902])
#endif
#ifndef XmRShellHorizPos
#define XmRShellHorizPos ((char *)&_XmStrings[11916])
#endif
#ifndef XmRShellUnitType
#define XmRShellUnitType ((char *)&_XmStrings[11930])
#endif
#ifndef XmRShellVertDim
#define XmRShellVertDim ((char *)&_XmStrings[11944])
#endif
#ifndef XmRShellVertPos
#define XmRShellVertPos ((char *)&_XmStrings[11957])
#endif
#ifndef XmRSizePolicy
#define XmRSizePolicy ((char *)&_XmStrings[11970])
#endif
#ifndef XmRStringDirection
#define XmRStringDirection ((char *)&_XmStrings[11981])
#endif
#ifndef XmRTearOffModel
#define XmRTearOffModel ((char *)&_XmStrings[11997])
#endif
#ifndef XmRTopShadowPixmap
#define XmRTopShadowPixmap ((char *)&_XmStrings[12010])
#endif
#ifndef XmRTransferStatus
#define XmRTransferStatus ((char *)&_XmStrings[12026])
#endif
#ifndef XmRTraversalType
#define XmRTraversalType ((char *)&_XmStrings[12041])
#endif
#ifndef XmRUnitType
#define XmRUnitType ((char *)&_XmStrings[12055])
#endif
#ifndef XmRUnpostBehavior
#define XmRUnpostBehavior ((char *)&_XmStrings[12064])
#endif
#ifndef XmRValueWcs
#define XmRValueWcs ((char *)&_XmStrings[12079])
#endif
#ifndef XmRVerticalAlignment
#define XmRVerticalAlignment ((char *)&_XmStrings[12088])
#endif
#ifndef XmRVerticalDimension
#define XmRVerticalDimension ((char *)&_XmStrings[12106])
#endif
#ifndef XmRVerticalInt
#define XmRVerticalInt ((char *)&_XmStrings[12124])
#endif
#ifndef XmRVerticalPosition
#define XmRVerticalPosition ((char *)&_XmStrings[12136])
#endif
#ifndef XmRVirtualBinding
#define XmRVirtualBinding ((char *)&_XmStrings[12153])
#endif
#ifndef XmRVisibleItemCount
#define XmRVisibleItemCount ((char *)&_XmStrings[12168])
#endif
#ifndef XmRVisualPolicy
#define XmRVisualPolicy ((char *)&_XmStrings[12185])
#endif
#ifndef XmRWhichButton
#define XmRWhichButton ((char *)&_XmStrings[12198])
#endif
#ifndef XmRXmBackgroundPixmap
#define XmRXmBackgroundPixmap ((char *)&_XmStrings[12210])
#endif
#ifndef XmRXmString
#define XmRXmString ((char *)&_XmStrings[12229])
#endif
#ifndef XmRXmStringCharSet
#define XmRXmStringCharSet ((char *)&_XmStrings[12238])
#endif
#ifndef XmRXmStringTable
#define XmRXmStringTable ((char *)&_XmStrings[12254])
#endif
#ifndef XmVosfActivate
#define XmVosfActivate ((char *)&_XmStrings[12268])
#endif
#ifndef XmVosfAddMode
#define XmVosfAddMode ((char *)&_XmStrings[12280])
#endif
#ifndef XmVosfBackSpace
#define XmVosfBackSpace ((char *)&_XmStrings[12291])
#endif
#ifndef XmVosfBeginLine
#define XmVosfBeginLine ((char *)&_XmStrings[12304])
#endif
#ifndef XmVosfCancel
#define XmVosfCancel ((char *)&_XmStrings[12317])
#endif
#ifndef XmVosfClear
#define XmVosfClear ((char *)&_XmStrings[12327])
#endif
#ifndef XmVosfCopy
#define XmVosfCopy ((char *)&_XmStrings[12336])
#endif
#ifndef XmVosfCut
#define XmVosfCut ((char *)&_XmStrings[12344])
#endif
#ifndef XmVosfDelete
#define XmVosfDelete ((char *)&_XmStrings[12351])
#endif
#ifndef XmVosfDown
#define XmVosfDown ((char *)&_XmStrings[12361])
#endif
#ifndef XmVosfEndLine
#define XmVosfEndLine ((char *)&_XmStrings[12369])
#endif
#ifndef XmVosfHelp
#define XmVosfHelp ((char *)&_XmStrings[12380])
#endif
#ifndef XmVosfInsert
#define XmVosfInsert ((char *)&_XmStrings[12388])
#endif
#ifndef XmVosfLeft
#define XmVosfLeft ((char *)&_XmStrings[12398])
#endif
#ifndef XmVosfMenu
#define XmVosfMenu ((char *)&_XmStrings[12406])
#endif
#ifndef XmVosfMenuBar
#define XmVosfMenuBar ((char *)&_XmStrings[12414])
#endif
#ifndef XmVosfPageDown
#define XmVosfPageDown ((char *)&_XmStrings[12425])
#endif
#ifndef XmVosfPageLeft
#define XmVosfPageLeft ((char *)&_XmStrings[12437])
#endif
#ifndef XmVosfPageRight
#define XmVosfPageRight ((char *)&_XmStrings[12449])
#endif
#ifndef XmVosfPageUp
#define XmVosfPageUp ((char *)&_XmStrings[12462])
#endif
#ifndef XmVosfPaste
#define XmVosfPaste ((char *)&_XmStrings[12472])
#endif
#ifndef XmVosfPrimaryPaste
#define XmVosfPrimaryPaste ((char *)&_XmStrings[12481])
#endif
#ifndef XmVosfQuickPaste
#define XmVosfQuickPaste ((char *)&_XmStrings[12497])
#endif
#ifndef XmVosfRight
#define XmVosfRight ((char *)&_XmStrings[12511])
#endif
#ifndef XmVosfSelect
#define XmVosfSelect ((char *)&_XmStrings[12520])
#endif
#ifndef XmVosfUndo
#define XmVosfUndo ((char *)&_XmStrings[12530])
#endif
#ifndef XmVosfUp
#define XmVosfUp ((char *)&_XmStrings[12538])
#endif
#ifndef XmSFONTLIST_DEFAULT_TAG_STRING
#define XmSFONTLIST_DEFAULT_TAG_STRING ((char *)&_XmStrings[12544])
#endif
#ifndef XmSXmFONTLIST_DEFAULT_TAG_STRING
#define XmSXmFONTLIST_DEFAULT_TAG_STRING ((char *)&_XmStrings[12572])
#endif
#ifndef XmRTopItemPosition
#define XmRTopItemPosition ((char *)&_XmStrings[12602])
#endif
#ifndef XmNtearOffTitle
#define XmNtearOffTitle ((char *)&_XmStrings[12618])
#endif
#ifndef XmNTearOffTitle
#define XmNTearOffTitle ((char *)&_XmStrings[12631])
#endif
#ifndef XmNpopupHandlerCallback
#define XmNpopupHandlerCallback ((char *)&_XmStrings[12644])
#endif
#ifndef XmNconvertCallback
#define XmNconvertCallback ((char *)&_XmStrings[12665])
#endif
#ifndef XmNdestinationCallback
#define XmNdestinationCallback ((char *)&_XmStrings[12681])
#endif
#ifndef XmNselectedItem
#define XmNselectedItem ((char *)&_XmStrings[12701])
#endif
#ifndef XmCSelectedItem
#define XmCSelectedItem ((char *)&_XmStrings[12714])
#endif
#ifndef XmNselectionCallback
#define XmNselectionCallback ((char *)&_XmStrings[12727])
#endif
#ifndef XmNmatchBehavior
#define XmNmatchBehavior ((char *)&_XmStrings[12745])
#endif
#ifndef XmCMatchBehavior
#define XmCMatchBehavior ((char *)&_XmStrings[12759])
#endif
#ifndef XmNnoFontCallback
#define XmNnoFontCallback ((char *)&_XmStrings[12773])
#endif
#ifndef XmNtextPath
#define XmNtextPath ((char *)&_XmStrings[12788])
#endif
#ifndef XmNeditingPath
#define XmNeditingPath ((char *)&_XmStrings[12797])
#endif
#ifndef XmNEditingPath
#define XmNEditingPath ((char *)&_XmStrings[12809])
#endif
#ifndef XmNbidirectionalCursor
#define XmNbidirectionalCursor ((char *)&_XmStrings[12821])
#endif
#ifndef XmNBidirectionalCursor
#define XmNBidirectionalCursor ((char *)&_XmStrings[12841])
#endif
#ifndef XmNcollapsedStatePixmap
#define XmNcollapsedStatePixmap ((char *)&_XmStrings[12861])
#endif
#ifndef XmNdetailColumnHeading
#define XmNdetailColumnHeading ((char *)&_XmStrings[12882])
#endif
#ifndef XmNdetailCount
#define XmNdetailCount ((char *)&_XmStrings[12902])
#endif
#ifndef XmNdetailTabList
#define XmNdetailTabList ((char *)&_XmStrings[12914])
#endif
#ifndef XmNexpandedStatePixmap
#define XmNexpandedStatePixmap ((char *)&_XmStrings[12928])
#endif
#ifndef XmNlargeCellHeight
#define XmNlargeCellHeight ((char *)&_XmStrings[12948])
#endif
#ifndef XmNlargeCellWidth
#define XmNlargeCellWidth ((char *)&_XmStrings[12964])
#endif
#ifndef XmNlayoutType
#define XmNlayoutType ((char *)&_XmStrings[12979])
#endif
#ifndef XmNoutlineIndentation
#define XmNoutlineIndentation ((char *)&_XmStrings[12990])
#endif
#ifndef XmNoutlineLineStyle
#define XmNoutlineLineStyle ((char *)&_XmStrings[13009])
#endif
#ifndef XmNprimaryOwnership
#define XmNprimaryOwnership ((char *)&_XmStrings[13026])
#endif
#ifndef XmNselectionTechnique
#define XmNselectionTechnique ((char *)&_XmStrings[13043])
#endif
#ifndef XmNsmallCellHeight
#define XmNsmallCellHeight ((char *)&_XmStrings[13062])
#endif
#ifndef XmNsmallCellWidth
#define XmNsmallCellWidth ((char *)&_XmStrings[13078])
#endif
#ifndef XmNspatialStyle
#define XmNspatialStyle ((char *)&_XmStrings[13093])
#endif
#ifndef XmNentryParent
#define XmNentryParent ((char *)&_XmStrings[13106])
#endif
#ifndef XmNlargeIconX
#define XmNlargeIconX ((char *)&_XmStrings[13118])
#endif
#ifndef XmNlargeIconY
#define XmNlargeIconY ((char *)&_XmStrings[13129])
#endif
#ifndef XmNsmallIconX
#define XmNsmallIconX ((char *)&_XmStrings[13140])
#endif
#ifndef XmNsmallIconY
#define XmNsmallIconY ((char *)&_XmStrings[13151])
#endif
#ifndef XmCCollapsedStatePixmap
#define XmCCollapsedStatePixmap ((char *)&_XmStrings[13162])
#endif
#ifndef XmCDetailColumnHeading
#define XmCDetailColumnHeading ((char *)&_XmStrings[13183])
#endif
#ifndef XmNDetailCount
#define XmNDetailCount ((char *)&_XmStrings[13203])
#endif
#ifndef XmNDetailMask
#define XmNDetailMask ((char *)&_XmStrings[13215])
#endif
#ifndef XmCEntryViewType
#define XmCEntryViewType ((char *)&_XmStrings[13226])
#endif
#ifndef XmNLineStyle
#define XmNLineStyle ((char *)&_XmStrings[13240])
#endif
#ifndef XmCDetailTabList
#define XmCDetailTabList ((char *)&_XmStrings[13250])
#endif
#ifndef XmCExpandedStatePixmap
#define XmCExpandedStatePixmap ((char *)&_XmStrings[13264])
#endif
#ifndef XmNIncludeModel
#define XmNIncludeModel ((char *)&_XmStrings[13284])
#endif
#ifndef XmCCellHeight
#define XmCCellHeight ((char *)&_XmStrings[13297])
#endif
#ifndef XmCCellWidth
#define XmCCellWidth ((char *)&_XmStrings[13308])
#endif
#ifndef XmCLayoutType
#define XmCLayoutType ((char *)&_XmStrings[13318])
#endif
#ifndef XmCOutlineIndentation
#define XmCOutlineIndentation ((char *)&_XmStrings[13329])
#endif
#ifndef XmNPlaceModel
#define XmNPlaceModel ((char *)&_XmStrings[13348])
#endif
#ifndef XmNPrimaryOwnership
#define XmNPrimaryOwnership ((char *)&_XmStrings[13359])
#endif
#ifndef XmCSelectionTechnique
#define XmCSelectionTechnique ((char *)&_XmStrings[13376])
#endif
#ifndef XmCSpatialStyle
#define XmCSpatialStyle ((char *)&_XmStrings[13395])
#endif
#ifndef XmNEntryDetail
#define XmNEntryDetail ((char *)&_XmStrings[13408])
#endif
#ifndef XmNExpandState
#define XmNExpandState ((char *)&_XmStrings[13420])
#endif
#ifndef XmNlargeIcon
#define XmNlargeIcon ((char *)&_XmStrings[13432])
#endif
#ifndef XmNlargeIconMask
#define XmNlargeIconMask ((char *)&_XmStrings[13442])
#endif
#ifndef XmNlargeIconPixmap
#define XmNlargeIconPixmap ((char *)&_XmStrings[13456])
#endif
#ifndef XmNsmallIcon
#define XmNsmallIcon ((char *)&_XmStrings[13472])
#endif
#ifndef XmNsmallIconMask
#define XmNsmallIconMask ((char *)&_XmStrings[13482])
#endif
#ifndef XmNsmallIconPixmap
#define XmNsmallIconPixmap ((char *)&_XmStrings[13496])
#endif
#ifndef XmNIcon
#define XmNIcon ((char *)&_XmStrings[13512])
#endif
#ifndef XmCViewType
#define XmCViewType ((char *)&_XmStrings[13517])
#endif
#ifndef XmRVisualEmphasis
#define XmRVisualEmphasis ((char *)&_XmStrings[13526])
#endif
#ifndef XmNcurrentPageNumber
#define XmNcurrentPageNumber ((char *)&_XmStrings[13541])
#endif
#ifndef XmNfirstPageNumber
#define XmNfirstPageNumber ((char *)&_XmStrings[13559])
#endif
#ifndef XmNlastPageNumber
#define XmNlastPageNumber ((char *)&_XmStrings[13575])
#endif
#ifndef XmNbackPagePlacement
#define XmNbackPagePlacement ((char *)&_XmStrings[13590])
#endif
#ifndef XmNbackPageNumber
#define XmNbackPageNumber ((char *)&_XmStrings[13608])
#endif
#ifndef XmNbackPageSize
#define XmNbackPageSize ((char *)&_XmStrings[13623])
#endif
#ifndef XmNbackPageForeground
#define XmNbackPageForeground ((char *)&_XmStrings[13636])
#endif
#ifndef XmNbackPageBackground
#define XmNbackPageBackground ((char *)&_XmStrings[13655])
#endif
#ifndef XmNframeBackground
#define XmNframeBackground ((char *)&_XmStrings[13674])
#endif
#ifndef XmNbindingType
#define XmNbindingType ((char *)&_XmStrings[13690])
#endif
#ifndef XmNbindingPixmap
#define XmNbindingPixmap ((char *)&_XmStrings[13702])
#endif
#ifndef XmNbindingWidth
#define XmNbindingWidth ((char *)&_XmStrings[13716])
#endif
#ifndef XmNmajorTabSpacing
#define XmNmajorTabSpacing ((char *)&_XmStrings[13729])
#endif
#ifndef XmNminorTabSpacing
#define XmNminorTabSpacing ((char *)&_XmStrings[13745])
#endif
#ifndef XmNinnerMarginWidth
#define XmNinnerMarginWidth ((char *)&_XmStrings[13761])
#endif
#ifndef XmNinnerMarginHeight
#define XmNinnerMarginHeight ((char *)&_XmStrings[13778])
#endif
#ifndef XmNframeShadowThickness
#define XmNframeShadowThickness ((char *)&_XmStrings[13796])
#endif
#ifndef XmNpageNumber
#define XmNpageNumber ((char *)&_XmStrings[13817])
#endif
#ifndef XmCCurrentPageNumber
#define XmCCurrentPageNumber ((char *)&_XmStrings[13828])
#endif
#ifndef XmCFirstPageNumber
#define XmCFirstPageNumber ((char *)&_XmStrings[13846])
#endif
#ifndef XmCLastPageNumber
#define XmCLastPageNumber ((char *)&_XmStrings[13862])
#endif
#ifndef XmCBackPagePlacement
#define XmCBackPagePlacement ((char *)&_XmStrings[13877])
#endif
#ifndef XmCBackPageNumber
#define XmCBackPageNumber ((char *)&_XmStrings[13895])
#endif
#ifndef XmCBackPageSize
#define XmCBackPageSize ((char *)&_XmStrings[13910])
#endif
#ifndef XmCBackPageForeground
#define XmCBackPageForeground ((char *)&_XmStrings[13923])
#endif
#ifndef XmCBackPageBackground
#define XmCBackPageBackground ((char *)&_XmStrings[13942])
#endif
#ifndef XmCFrameBackground
#define XmCFrameBackground ((char *)&_XmStrings[13961])
#endif
#ifndef XmCBindingType
#define XmCBindingType ((char *)&_XmStrings[13977])
#endif
#ifndef XmCBindingPixmap
#define XmCBindingPixmap ((char *)&_XmStrings[13989])
#endif
#ifndef XmCBindingWidth
#define XmCBindingWidth ((char *)&_XmStrings[14003])
#endif
#ifndef XmCMajorTabSpacing
#define XmCMajorTabSpacing ((char *)&_XmStrings[14016])
#endif
#ifndef XmCMinorTabSpacing
#define XmCMinorTabSpacing ((char *)&_XmStrings[14032])
#endif
#ifndef XmCInnerMarginWidth
#define XmCInnerMarginWidth ((char *)&_XmStrings[14048])
#endif
#ifndef XmCInnerMarginHeight
#define XmCInnerMarginHeight ((char *)&_XmStrings[14065])
#endif
#ifndef XmCPageChangeCallback
#define XmCPageChangeCallback ((char *)&_XmStrings[14083])
#endif
#ifndef XmCPageNumber
#define XmCPageNumber ((char *)&_XmStrings[14102])
#endif
#ifndef XmCArrowLayout
#define XmCArrowLayout ((char *)&_XmStrings[14113])
#endif
#ifndef XmCArrowSensitivity
#define XmCArrowSensitivity ((char *)&_XmStrings[14125])
#endif
#ifndef XmCSpinBoxChildType
#define XmCSpinBoxChildType ((char *)&_XmStrings[14142])
#endif
#ifndef XmNarrowLayout
#define XmNarrowLayout ((char *)&_XmStrings[14159])
#endif
#ifndef XmRArrowLayout
#define XmRArrowLayout ((char *)&_XmStrings[14171])
#endif
#ifndef XmNarrowSensitivity
#define XmNarrowSensitivity ((char *)&_XmStrings[14183])
#endif
#ifndef XmRArrowSensitivity
#define XmRArrowSensitivity ((char *)&_XmStrings[14200])
#endif
#ifndef XmNdefaultArrowSensitivity
#define XmNdefaultArrowSensitivity ((char *)&_XmStrings[14217])
#endif
#ifndef XmCDefaultArrowSensitivity
#define XmCDefaultArrowSensitivity ((char *)&_XmStrings[14241])
#endif
#ifndef XmNarrowSize
#define XmNarrowSize ((char *)&_XmStrings[14265])
#endif
#ifndef XmCArrowSize
#define XmCArrowSize ((char *)&_XmStrings[14275])
#endif
#ifndef XmNspinBoxChildType
#define XmNspinBoxChildType ((char *)&_XmStrings[14285])
#endif
#ifndef XmRSpinBoxChildType
#define XmRSpinBoxChildType ((char *)&_XmStrings[14302])
#endif
#ifndef XmNposition
#define XmNposition ((char *)&_XmStrings[14319])
#endif
#ifndef XmNnumValues
#define XmNnumValues ((char *)&_XmStrings[14328])
#endif
#ifndef XmCNumValues
#define XmCNumValues ((char *)&_XmStrings[14338])
#endif
#ifndef XmNvalues
#define XmNvalues ((char *)&_XmStrings[14348])
#endif
#ifndef XmCValues
#define XmCValues ((char *)&_XmStrings[14355])
#endif
#ifndef XmNminimumValue
#define XmNminimumValue ((char *)&_XmStrings[14362])
#endif
#ifndef XmCMinimumValue
#define XmCMinimumValue ((char *)&_XmStrings[14375])
#endif
#ifndef XmNmaximumValue
#define XmNmaximumValue ((char *)&_XmStrings[14388])
#endif
#ifndef XmCMaximumValue
#define XmCMaximumValue ((char *)&_XmStrings[14401])
#endif
#ifndef XmNincrementValue
#define XmNincrementValue ((char *)&_XmStrings[14414])
#endif
#ifndef XmCIncrementValue
#define XmCIncrementValue ((char *)&_XmStrings[14429])
#endif
#ifndef XmCAutomaticSelection
#define XmCAutomaticSelection ((char *)&_XmStrings[14444])
#endif
#ifndef XmCLineStyle
#define XmCLineStyle ((char *)&_XmStrings[14463])
#endif
#ifndef XmCEntryViewType
#define XmCEntryViewType ((char *)&_XmStrings[14473])
#endif
#ifndef XmRDirection
#define XmRDirection ((char *)&_XmStrings[14487])
#endif
#ifndef XmCLayoutType
#define XmCLayoutType ((char *)&_XmStrings[14497])
#endif
#ifndef XmCPrimaryOwnership
#define XmCPrimaryOwnership ((char *)&_XmStrings[14508])
#endif
#ifndef XmCSelectionTechnique
#define XmCSelectionTechnique ((char *)&_XmStrings[14525])
#endif
#ifndef XmCSpatialStyle
#define XmCSpatialStyle ((char *)&_XmStrings[14544])
#endif
#ifndef XmCTabList
#define XmCTabList ((char *)&_XmStrings[14557])
#endif
#ifndef XmRViewType
#define XmRViewType ((char *)&_XmStrings[14565])
#endif
#ifndef XmCVisualEmphasis
#define XmCVisualEmphasis ((char *)&_XmStrings[14574])
#endif
#ifndef XmRBindingType
#define XmRBindingType ((char *)&_XmStrings[14589])
#endif
#ifndef XmCNBChildType
#define XmCNBChildType ((char *)&_XmStrings[14601])
#endif
#ifndef XmNentryViewType
#define XmNentryViewType ((char *)&_XmStrings[14613])
#endif
#ifndef XmNinsensitiveStippleBitmap
#define XmNinsensitiveStippleBitmap ((char *)&_XmStrings[14627])
#endif
#ifndef XmNlayoutDirection
#define XmNlayoutDirection ((char *)&_XmStrings[14652])
#endif
#ifndef XmNviewType
#define XmNviewType ((char *)&_XmStrings[14668])
#endif
#ifndef XmNvisualEmphasis
#define XmNvisualEmphasis ((char *)&_XmStrings[14677])
#endif
#ifndef XmCLayoutDirection
#define XmCLayoutDirection ((char *)&_XmStrings[14692])
#endif
#ifndef XmNsnapBackMultiple
#define XmNsnapBackMultiple ((char *)&_XmStrings[14708])
#endif
#ifndef XmNslidingMode
#define XmNslidingMode ((char *)&_XmStrings[14725])
#endif
#ifndef XmNsliderVisual
#define XmNsliderVisual ((char *)&_XmStrings[14737])
#endif
#ifndef XmNautoDragModel
#define XmNautoDragModel ((char *)&_XmStrings[14750])
#endif
#ifndef XmNcolorCalculationProc
#define XmNcolorCalculationProc ((char *)&_XmStrings[14764])
#endif
#ifndef XmNbitmapConversionModel
#define XmNbitmapConversionModel ((char *)&_XmStrings[14785])
#endif
#ifndef XmNcolorAllocationProc
#define XmNcolorAllocationProc ((char *)&_XmStrings[14807])
#endif
#ifndef XmNselectionMode
#define XmNselectionMode ((char *)&_XmStrings[14827])
#endif
#ifndef XmNselectedPositions
#define XmNselectedPositions ((char *)&_XmStrings[14841])
#endif
#ifndef XmNselectedPositionCount
#define XmNselectedPositionCount ((char *)&_XmStrings[14859])
#endif
#ifndef XmNSnapBackMultiple
#define XmNSnapBackMultiple ((char *)&_XmStrings[14881])
#endif
#ifndef XmNSliderVisual
#define XmNSliderVisual ((char *)&_XmStrings[14898])
#endif
#ifndef XmNSlidingMode
#define XmNSlidingMode ((char *)&_XmStrings[14911])
#endif
#ifndef XmNAutoDragModel
#define XmNAutoDragModel ((char *)&_XmStrings[14923])
#endif
#ifndef XmNColorCalculationProc
#define XmNColorCalculationProc ((char *)&_XmStrings[14937])
#endif
#ifndef XmNBitmapConversionModel
#define XmNBitmapConversionModel ((char *)&_XmStrings[14958])
#endif
#ifndef XmNColorAllocationProc
#define XmNColorAllocationProc ((char *)&_XmStrings[14980])
#endif
#ifndef XmNInsensitiveStippleBitmap
#define XmNInsensitiveStippleBitmap ((char *)&_XmStrings[15000])
#endif
#ifndef XmNSelectionMode
#define XmNSelectionMode ((char *)&_XmStrings[15025])
#endif
#ifndef XmNSelectedPositions
#define XmNSelectedPositions ((char *)&_XmStrings[15039])
#endif
#ifndef XmNSelectedPositionCount
#define XmNSelectedPositionCount ((char *)&_XmStrings[15057])
#endif
#ifndef XmNSlidingMode
#define XmNSlidingMode ((char *)&_XmStrings[15079])
#endif
#ifndef XmNShowArrows
#define XmNShowArrows ((char *)&_XmStrings[15091])
#endif
#ifndef XmNSliderVisual
#define XmNSliderVisual ((char *)&_XmStrings[15102])
#endif
#ifndef XmNShowValue
#define XmNShowValue ((char *)&_XmStrings[15115])
#endif
#ifndef XmNAutoDragModel
#define XmNAutoDragModel ((char *)&_XmStrings[15125])
#endif
#ifndef XmNSWChildType
#define XmNSWChildType ((char *)&_XmStrings[15139])
#endif
#ifndef XmNBitmapConversionModel
#define XmNBitmapConversionModel ((char *)&_XmStrings[15151])
#endif
#ifndef XmNSelectionMode
#define XmNSelectionMode ((char *)&_XmStrings[15173])
#endif
#ifndef XmNinputPolicy
#define XmNinputPolicy ((char *)&_XmStrings[15187])
#endif
#ifndef XmCInputPolicy
#define XmCInputPolicy ((char *)&_XmStrings[15199])
#endif
#ifndef XmRInputPolicy
#define XmRInputPolicy ((char *)&_XmStrings[15211])
#endif
#ifndef XmNtoggleMode
#define XmNtoggleMode ((char *)&_XmStrings[15223])
#endif
#ifndef XmCToggleMode
#define XmCToggleMode ((char *)&_XmStrings[15234])
#endif
#ifndef XmRToggleMode
#define XmRToggleMode ((char *)&_XmStrings[15245])
#endif
#ifndef XmRIndicatorOn
#define XmRIndicatorOn ((char *)&_XmStrings[15256])
#endif
#ifndef XmNSet
#define XmNSet ((char *)&_XmStrings[15268])
#endif
#ifndef XmNindeterminatePixmap
#define XmNindeterminatePixmap ((char *)&_XmStrings[15272])
#endif
#ifndef XmCIndeterminatePixmap
#define XmCIndeterminatePixmap ((char *)&_XmStrings[15292])
#endif
#ifndef XmNunselectColor
#define XmNunselectColor ((char *)&_XmStrings[15312])
#endif
#ifndef XmCUnselectColor
#define XmCUnselectColor ((char *)&_XmStrings[15326])
#endif
#ifndef XmNselectedPosition
#define XmNselectedPosition ((char *)&_XmStrings[15340])
#endif
#ifndef XmNarrowSpacing
#define XmNarrowSpacing ((char *)&_XmStrings[15357])
#endif
#ifndef XmCArrowSpacing
#define XmCArrowSpacing ((char *)&_XmStrings[15370])
#endif
#ifndef XmRMatchBehavior
#define XmRMatchBehavior ((char *)&_XmStrings[15383])
#endif
#ifndef XmRComboBoxType
#define XmRComboBoxType ((char *)&_XmStrings[15397])
#endif
#ifndef XmCSelectedPosition
#define XmCSelectedPosition ((char *)&_XmStrings[15410])
#endif
#ifndef XmNenableWarp
#define XmNenableWarp ((char *)&_XmStrings[15427])
#endif
#ifndef XmNEnableWarp
#define XmNEnableWarp ((char *)&_XmStrings[15438])
#endif
#ifndef XmNEnableWarp
#define XmNEnableWarp ((char *)&_XmStrings[15449])
#endif
#ifndef XmNmotifVersion
#define XmNmotifVersion ((char *)&_XmStrings[15460])
#endif
#ifndef XmNMotifVersion
#define XmNMotifVersion ((char *)&_XmStrings[15473])
#endif
#ifndef XmNdefaultGlyphPixmap
#define XmNdefaultGlyphPixmap ((char *)&_XmStrings[15486])
#endif
#ifndef XmNDefaultGlyphPixmap
#define XmNDefaultGlyphPixmap ((char *)&_XmStrings[15505])
#endif
#ifndef XmCRendition
#define XmCRendition ((char *)&_XmStrings[15524])
#endif
#ifndef XmNtag
#define XmNtag ((char *)&_XmStrings[15534])
#endif
#ifndef XmNTag
#define XmNTag ((char *)&_XmStrings[15538])
#endif
#ifndef XmNfontName
#define XmNfontName ((char *)&_XmStrings[15542])
#endif
#ifndef XmCFontName
#define XmCFontName ((char *)&_XmStrings[15551])
#endif
#ifndef XmNfontType
#define XmNfontType ((char *)&_XmStrings[15560])
#endif
#ifndef XmCFontType
#define XmCFontType ((char *)&_XmStrings[15569])
#endif
#ifndef XmRFontType
#define XmRFontType ((char *)&_XmStrings[15578])
#endif
#ifndef XmNloadModel
#define XmNloadModel ((char *)&_XmStrings[15587])
#endif
#ifndef XmCLoadModel
#define XmCLoadModel ((char *)&_XmStrings[15597])
#endif
#ifndef XmRLoadModel
#define XmRLoadModel ((char *)&_XmStrings[15607])
#endif
#ifndef XmNtabList
#define XmNtabList ((char *)&_XmStrings[15617])
#endif
#ifndef XmRTabList
#define XmRTabList ((char *)&_XmStrings[15625])
#endif
#ifndef XmNRenditionPixel
#define XmNRenditionPixel ((char *)&_XmStrings[15633])
#endif
#ifndef XmNunderlineType
#define XmNunderlineType ((char *)&_XmStrings[15648])
#endif
#ifndef XmCUnderlineType
#define XmCUnderlineType ((char *)&_XmStrings[15662])
#endif
#ifndef XmNstrikethruType
#define XmNstrikethruType ((char *)&_XmStrings[15676])
#endif
#ifndef XmCStrikethruType
#define XmCStrikethruType ((char *)&_XmStrings[15691])
#endif
#ifndef XmNLineType
#define XmNLineType ((char *)&_XmStrings[15706])
#endif
#ifndef XmNrenderTable
#define XmNrenderTable ((char *)&_XmStrings[15715])
#endif
#ifndef XmCRenderTable
#define XmCRenderTable ((char *)&_XmStrings[15727])
#endif
#ifndef XmRRenderTable
#define XmRRenderTable ((char *)&_XmStrings[15739])
#endif
#ifndef XmNbuttonRenderTable
#define XmNbuttonRenderTable ((char *)&_XmStrings[15751])
#endif
#ifndef XmNButtonRenderTable
#define XmNButtonRenderTable ((char *)&_XmStrings[15769])
#endif
#ifndef XmNButtonRenderTable
#define XmNButtonRenderTable ((char *)&_XmStrings[15787])
#endif
#ifndef XmNlabelRenderTable
#define XmNlabelRenderTable ((char *)&_XmStrings[15805])
#endif
#ifndef XmNLabelRenderTable
#define XmNLabelRenderTable ((char *)&_XmStrings[15822])
#endif
#ifndef XmNLabelRenderTable
#define XmNLabelRenderTable ((char *)&_XmStrings[15839])
#endif
#ifndef XmNtextRenderTable
#define XmNtextRenderTable ((char *)&_XmStrings[15856])
#endif
#ifndef XmNTextRenderTable
#define XmNTextRenderTable ((char *)&_XmStrings[15872])
#endif
#ifndef XmNTextRenderTable
#define XmNTextRenderTable ((char *)&_XmStrings[15888])
#endif
#ifndef XmNdragStartCallback
#define XmNdragStartCallback ((char *)&_XmStrings[15904])
#endif
#ifndef XmNnoRenditionCallback
#define XmNnoRenditionCallback ((char *)&_XmStrings[15922])
#endif
#ifndef XmNXmAS_IS
#define XmNXmAS_IS ((char *)&_XmStrings[15942])
#endif
#ifndef XmNIsWhiteSpaceMethod
#define XmNIsWhiteSpaceMethod ((char *)&_XmStrings[15950])
#endif
#ifndef XmNIsScanBreakMethod
#define XmNIsScanBreakMethod ((char *)&_XmStrings[15969])
#endif
#ifndef XmNCharDirection
#define XmNCharDirection ((char *)&_XmStrings[15987])
#endif
#ifndef XmNInitialCharsDirection
#define XmNInitialCharsDirection ((char *)&_XmStrings[16001])
#endif
#ifndef XmNpatternType
#define XmNpatternType ((char *)&_XmStrings[16023])
#endif
#ifndef XmNsubstitute
#define XmNsubstitute ((char *)&_XmStrings[16035])
#endif
#ifndef XmNinvokeParseProc
#define XmNinvokeParseProc ((char *)&_XmStrings[16046])
#endif
#ifndef XmNincludeStatus
#define XmNincludeStatus ((char *)&_XmStrings[16062])
#endif
#ifndef XmNosfBackTab
#define XmNosfBackTab ((char *)&_XmStrings[16076])
#endif
#ifndef XmNosfBeginData
#define XmNosfBeginData ((char *)&_XmStrings[16087])
#endif
#ifndef XmNosfDeselectAll
#define XmNosfDeselectAll ((char *)&_XmStrings[16100])
#endif
#ifndef XmNosfEndData
#define XmNosfEndData ((char *)&_XmStrings[16115])
#endif
#ifndef XmNosfEscape
#define XmNosfEscape ((char *)&_XmStrings[16126])
#endif
#ifndef XmNosfExtend
#define XmNosfExtend ((char *)&_XmStrings[16136])
#endif
#ifndef XmNosfLeftLine
#define XmNosfLeftLine ((char *)&_XmStrings[16146])
#endif
#ifndef XmNosfNext
#define XmNosfNext ((char *)&_XmStrings[16158])
#endif
#ifndef XmNosfNextField
#define XmNosfNextField ((char *)&_XmStrings[16166])
#endif
#ifndef XmNosfNextMenu
#define XmNosfNextMenu ((char *)&_XmStrings[16179])
#endif
#ifndef XmNosfNextMinor
#define XmNosfNextMinor ((char *)&_XmStrings[16191])
#endif
#ifndef XmNosfPrevField
#define XmNosfPrevField ((char *)&_XmStrings[16204])
#endif
#ifndef XmNosfPrevMenu
#define XmNosfPrevMenu ((char *)&_XmStrings[16217])
#endif
#ifndef XmNosfPrior
#define XmNosfPrior ((char *)&_XmStrings[16229])
#endif
#ifndef XmNosfPriorMinor
#define XmNosfPriorMinor ((char *)&_XmStrings[16238])
#endif
#ifndef XmNosfReselect
#define XmNosfReselect ((char *)&_XmStrings[16252])
#endif
#ifndef XmNosfRestore
#define XmNosfRestore ((char *)&_XmStrings[16264])
#endif
#ifndef XmNosfRightLine
#define XmNosfRightLine ((char *)&_XmStrings[16275])
#endif
#ifndef XmNosfSelectAll
#define XmNosfSelectAll ((char *)&_XmStrings[16288])
#endif
#ifndef XmNosfSwitchDirection
#define XmNosfSwitchDirection ((char *)&_XmStrings[16301])
#endif
#ifndef XmNnotebookChildType
#define XmNnotebookChildType ((char *)&_XmStrings[16320])
#endif
#ifndef XmCNotebookChildType
#define XmCNotebookChildType ((char *)&_XmStrings[16338])
#endif
#ifndef XmRNotebookChildType
#define XmRNotebookChildType ((char *)&_XmStrings[16356])
#endif
#ifndef XmNscrolledWindowChildType
#define XmNscrolledWindowChildType ((char *)&_XmStrings[16374])
#endif
#ifndef XmCScrolledWindowChildType
#define XmCScrolledWindowChildType ((char *)&_XmStrings[16398])
#endif
#ifndef XmRScrolledWindowChildType
#define XmRScrolledWindowChildType ((char *)&_XmStrings[16422])
#endif
#ifndef XmNselectedObjects
#define XmNselectedObjects ((char *)&_XmStrings[16446])
#endif
#ifndef XmCSelectedObjects
#define XmCSelectedObjects ((char *)&_XmStrings[16462])
#endif
#ifndef XmNselectedObjectCount
#define XmNselectedObjectCount ((char *)&_XmStrings[16478])
#endif
#ifndef XmCSelectedObjectCount
#define XmCSelectedObjectCount ((char *)&_XmStrings[16498])
#endif
#ifndef XmNcomboBoxType
#define XmNcomboBoxType ((char *)&_XmStrings[16518])
#endif
#ifndef XmCComboBoxType
#define XmCComboBoxType ((char *)&_XmStrings[16531])
#endif
#ifndef XmNtabValue
#define XmNtabValue ((char *)&_XmStrings[16544])
#endif
#ifndef XmNoffsetModel
#define XmNoffsetModel ((char *)&_XmStrings[16553])
#endif
#ifndef XmNdecimal
#define XmNdecimal ((char *)&_XmStrings[16565])
#endif
#ifndef XmNdetail
#define XmNdetail ((char *)&_XmStrings[16573])
#endif
#ifndef XmCDetail
#define XmCDetail ((char *)&_XmStrings[16580])
#endif
#ifndef XmNdetailCount
#define XmNdetailCount ((char *)&_XmStrings[16587])
#endif
#ifndef XmCDetailCount
#define XmCDetailCount ((char *)&_XmStrings[16599])
#endif
#ifndef XmNcontainerID
#define XmNcontainerID ((char *)&_XmStrings[16611])
#endif
#ifndef XmNContainerID
#define XmNContainerID ((char *)&_XmStrings[16623])
#endif
#ifndef XmNCLIENT_WINDOW
#define XmNCLIENT_WINDOW ((char *)&_XmStrings[16635])
#endif
#ifndef XmNCLIP_TEMPORARY
#define XmNCLIP_TEMPORARY ((char *)&_XmStrings[16649])
#endif
#ifndef XmNCLIPBOARD
#define XmNCLIPBOARD ((char *)&_XmStrings[16664])
#endif
#ifndef XmNCOMPOUND_TEXT
#define XmNCOMPOUND_TEXT ((char *)&_XmStrings[16674])
#endif
#ifndef XmNDELETE
#define XmNDELETE ((char *)&_XmStrings[16688])
#endif
#ifndef XmNFILE
#define XmNFILE ((char *)&_XmStrings[16695])
#endif
#ifndef XmNFILE_NAME
#define XmNFILE_NAME ((char *)&_XmStrings[16700])
#endif
#ifndef XmNINCR
#define XmNINCR ((char *)&_XmStrings[16710])
#endif
#ifndef XmNINSERT_PROPERTY
#define XmNINSERT_PROPERTY ((char *)&_XmStrings[16715])
#endif
#ifndef XmNINSERT_SELECTION
#define XmNINSERT_SELECTION ((char *)&_XmStrings[16731])
#endif
#ifndef XmSLENGTH
#define XmSLENGTH ((char *)&_XmStrings[16748])
#endif
#ifndef XmNLINK_SELECTION
#define XmNLINK_SELECTION ((char *)&_XmStrings[16755])
#endif
#ifndef XmN_MOTIF_ATOM_0
#define XmN_MOTIF_ATOM_0 ((char *)&_XmStrings[16770])
#endif
#ifndef XmN_MOTIF_BINDINGS
#define XmN_MOTIF_BINDINGS ((char *)&_XmStrings[16784])
#endif
#ifndef XmN_MOTIF_DEFAULT_BINDINGS
#define XmN_MOTIF_DEFAULT_BINDINGS ((char *)&_XmStrings[16800])
#endif
#ifndef XmN_MOTIF_CANCEL_DROP_EFFECT
#define XmN_MOTIF_CANCEL_DROP_EFFECT ((char *)&_XmStrings[16824])
#endif
#ifndef XmN_MOTIF_CLIP_HEADER
#define XmN_MOTIF_CLIP_HEADER ((char *)&_XmStrings[16850])
#endif
#ifndef XmN_MOTIF_CLIP_DATA_REQUEST
#define XmN_MOTIF_CLIP_DATA_REQUEST ((char *)&_XmStrings[16869])
#endif
#ifndef XmN_MOTIF_CLIP_DATA_DELETE
#define XmN_MOTIF_CLIP_DATA_DELETE ((char *)&_XmStrings[16894])
#endif
#ifndef XmN_MOTIF_CLIP_ITEM
#define XmN_MOTIF_CLIP_ITEM ((char *)&_XmStrings[16918])
#endif
#ifndef XmN_MOTIF_CLIP_LOCK
#define XmN_MOTIF_CLIP_LOCK ((char *)&_XmStrings[16935])
#endif
#ifndef XmN_MOTIF_CLIP_LOCK_ACCESS_VALID
#define XmN_MOTIF_CLIP_LOCK_ACCESS_VALID ((char *)&_XmStrings[16952])
#endif
#ifndef XmN_MOTIF_CLIP_MESSAGE
#define XmN_MOTIF_CLIP_MESSAGE ((char *)&_XmStrings[16982])
#endif
#ifndef XmN_MOTIF_CLIP_NEXT_ID
#define XmN_MOTIF_CLIP_NEXT_ID ((char *)&_XmStrings[17002])
#endif
#ifndef XmN_MOTIF_CLIP_TIME
#define XmN_MOTIF_CLIP_TIME ((char *)&_XmStrings[17022])
#endif
#ifndef XmN_MOTIF_CLIPBOARD_TARGETS
#define XmN_MOTIF_CLIPBOARD_TARGETS ((char *)&_XmStrings[17039])
#endif
#ifndef XmN_MOTIF_COMPOUND_STRING
#define XmN_MOTIF_COMPOUND_STRING ((char *)&_XmStrings[17064])
#endif
#ifndef XmN_MOTIF_DEFERRED_CLIPBOARD_TARGETS
#define XmN_MOTIF_DEFERRED_CLIPBOARD_TARGETS ((char *)&_XmStrings[17087])
#endif
#ifndef XmN_MOTIF_DESTINATION
#define XmN_MOTIF_DESTINATION ((char *)&_XmStrings[17121])
#endif
#ifndef XmN_MOTIF_DRAG_OFFSET
#define XmN_MOTIF_DRAG_OFFSET ((char *)&_XmStrings[17140])
#endif
#ifndef XmN_MOTIF_DROP
#define XmN_MOTIF_DROP ((char *)&_XmStrings[17159])
#endif
#ifndef XmN_MOTIF_ENCODING_REGISTRY
#define XmN_MOTIF_ENCODING_REGISTRY ((char *)&_XmStrings[17171])
#endif
#ifndef XmN_MOTIF_EXPORT_TARGETS
#define XmN_MOTIF_EXPORT_TARGETS ((char *)&_XmStrings[17196])
#endif
#ifndef XmN_MOTIF_LOSE_SELECTION
#define XmN_MOTIF_LOSE_SELECTION ((char *)&_XmStrings[17218])
#endif
#ifndef XmN_MOTIF_RENDER_TABLE
#define XmN_MOTIF_RENDER_TABLE ((char *)&_XmStrings[17240])
#endif
#ifndef XmN_MOTIF_WM_QUERY
#define XmN_MOTIF_WM_QUERY ((char *)&_XmStrings[17260])
#endif
#ifndef XmN_MOTIF_WM_ALL_CLIENTS
#define XmN_MOTIF_WM_ALL_CLIENTS ((char *)&_XmStrings[17276])
#endif
#ifndef XmNMULTIPLE
#define XmNMULTIPLE ((char *)&_XmStrings[17298])
#endif
#ifndef XmNNULL
#define XmNNULL ((char *)&_XmStrings[17307])
#endif
#ifndef XmSTARGETS
#define XmSTARGETS ((char *)&_XmStrings[17312])
#endif
#ifndef XmSTEXT
#define XmSTEXT ((char *)&_XmStrings[17320])
#endif
#ifndef XmNTIMESTAMP
#define XmNTIMESTAMP ((char *)&_XmStrings[17325])
#endif
#ifndef XmNWM_STATE
#define XmNWM_STATE ((char *)&_XmStrings[17335])
#endif
#ifndef XmNXmTRANSFER_SUCCESS
#define XmNXmTRANSFER_SUCCESS ((char *)&_XmStrings[17344])
#endif
#ifndef XmNXmTRANSFER_FAILURE
#define XmNXmTRANSFER_FAILURE ((char *)&_XmStrings[17363])
#endif
#ifndef XmNpathMode
#define XmNpathMode ((char *)&_XmStrings[17382])
#endif
#ifndef XmCPathMode
#define XmCPathMode ((char *)&_XmStrings[17391])
#endif
#ifndef XmRPathMode
#define XmRPathMode ((char *)&_XmStrings[17400])
#endif
#ifndef XmNfileFilterStyle
#define XmNfileFilterStyle ((char *)&_XmStrings[17409])
#endif
#ifndef XmCFileFilterStyle
#define XmCFileFilterStyle ((char *)&_XmStrings[17425])
#endif
#ifndef XmRFileFilterStyle
#define XmRFileFilterStyle ((char *)&_XmStrings[17441])
#endif
#ifndef XmNdirTextLabelString
#define XmNdirTextLabelString ((char *)&_XmStrings[17457])
#endif
#ifndef XmCDirTextLabelString
#define XmCDirTextLabelString ((char *)&_XmStrings[17476])
#endif
#ifndef XmNenableBtn1Transfer
#define XmNenableBtn1Transfer ((char *)&_XmStrings[17495])
#endif
#ifndef XmCEnableBtn1Transfer
#define XmCEnableBtn1Transfer ((char *)&_XmStrings[17514])
#endif
#ifndef XmNenableButtonTab
#define XmNenableButtonTab ((char *)&_XmStrings[17533])
#endif
#ifndef XmNEnableButtonTab
#define XmNEnableButtonTab ((char *)&_XmStrings[17549])
#endif
#ifndef XmNenableEtchedInMenu
#define XmNenableEtchedInMenu ((char *)&_XmStrings[17565])
#endif
#ifndef XmNEnableEtchedInMenu
#define XmNEnableEtchedInMenu ((char *)&_XmStrings[17584])
#endif
#ifndef XmNdefaultButtonEmphasis
#define XmNdefaultButtonEmphasis ((char *)&_XmStrings[17603])
#endif
#ifndef XmNDefaultButtonEmphasis
#define XmNDefaultButtonEmphasis ((char *)&_XmStrings[17625])
#endif
#ifndef XmNDefaultButtonEmphasis
#define XmNDefaultButtonEmphasis ((char *)&_XmStrings[17647])
#endif
#ifndef XmNenableToggleColor
#define XmNenableToggleColor ((char *)&_XmStrings[17669])
#endif
#ifndef XmNEnableToggleColor
#define XmNEnableToggleColor ((char *)&_XmStrings[17687])
#endif
#ifndef XmNenableToggleVisual
#define XmNenableToggleVisual ((char *)&_XmStrings[17705])
#endif
#ifndef XmNEnableToggleVisual
#define XmNEnableToggleVisual ((char *)&_XmStrings[17724])
#endif
#ifndef XmNenableDragIcon
#define XmNenableDragIcon ((char *)&_XmStrings[17743])
#endif
#ifndef XmNEnableDragIcon
#define XmNEnableDragIcon ((char *)&_XmStrings[17758])
#endif
#ifndef XmNenableUnselectableDrag
#define XmNenableUnselectableDrag ((char *)&_XmStrings[17773])
#endif
#ifndef XmNEnableUnselectableDrag
#define XmNEnableUnselectableDrag ((char *)&_XmStrings[17796])
#endif
#ifndef XmNdragOverActiveMode
#define XmNdragOverActiveMode ((char *)&_XmStrings[17819])
#endif
#ifndef XmNDragOverActiveMode
#define XmNDragOverActiveMode ((char *)&_XmStrings[17838])
#endif
#ifndef XmNinstallColormap
#define XmNinstallColormap ((char *)&_XmStrings[17857])
#endif
#ifndef XmNInstallColormap
#define XmNInstallColormap ((char *)&_XmStrings[17873])
#endif
#ifndef XmCOwnerEvents
#define XmCOwnerEvents ((char *)&_XmStrings[17889])
#endif
#ifndef XmNownerEvents
#define XmNownerEvents ((char *)&_XmStrings[17901])
#endif
#ifndef XmCGrabStyle
#define XmCGrabStyle ((char *)&_XmStrings[17913])
#endif
#ifndef XmNgrabStyle
#define XmNgrabStyle ((char *)&_XmStrings[17923])
#endif
#ifndef XmNforegroundState
#define XmNforegroundState ((char *)&_XmStrings[17933])
#endif
#ifndef XmNbackgroundState
#define XmNbackgroundState ((char *)&_XmStrings[17949])
#endif
#ifndef XmNGroundState
#define XmNGroundState ((char *)&_XmStrings[17965])
#endif
#ifndef XmNGroundState
#define XmNGroundState ((char *)&_XmStrings[17977])
#endif
#ifndef XmNSelectColor
#define XmNSelectColor ((char *)&_XmStrings[17989])
#endif
#ifndef XmNLargeIconPixmap
#define XmNLargeIconPixmap ((char *)&_XmStrings[18001])
#endif
#ifndef XmNSmallIconPixmap
#define XmNSmallIconPixmap ((char *)&_XmStrings[18017])
#endif
#ifndef XmNoutlineState
#define XmNoutlineState ((char *)&_XmStrings[18033])
#endif
#ifndef XmCOutlineState
#define XmCOutlineState ((char *)&_XmStrings[18046])
#endif
#ifndef XmNOutlineState
#define XmNOutlineState ((char *)&_XmStrings[18059])
#endif
#ifndef XmNspatialIncludeModel
#define XmNspatialIncludeModel ((char *)&_XmStrings[18072])
#endif
#ifndef XmCSpatialIncludeModel
#define XmCSpatialIncludeModel ((char *)&_XmStrings[18092])
#endif
#ifndef XmNSpatialIncludeModel
#define XmNSpatialIncludeModel ((char *)&_XmStrings[18112])
#endif
#ifndef XmNspatialResizeModel
#define XmNspatialResizeModel ((char *)&_XmStrings[18132])
#endif
#ifndef XmCSpatialResizeModel
#define XmCSpatialResizeModel ((char *)&_XmStrings[18151])
#endif
#ifndef XmNSpatialResizeModel
#define XmNSpatialResizeModel ((char *)&_XmStrings[18170])
#endif
#ifndef XmNspatialSnapModel
#define XmNspatialSnapModel ((char *)&_XmStrings[18189])
#endif
#ifndef XmCSpatialSnapModel
#define XmCSpatialSnapModel ((char *)&_XmStrings[18206])
#endif
#ifndef XmNSpatialSnapModel
#define XmNSpatialSnapModel ((char *)&_XmStrings[18223])
#endif
#ifndef XmNdetailColumnHeadingCount
#define XmNdetailColumnHeadingCount ((char *)&_XmStrings[18240])
#endif
#ifndef XmCDetailColumnHeadingCount
#define XmCDetailColumnHeadingCount ((char *)&_XmStrings[18265])
#endif
#ifndef XmNdetailOrder
#define XmNdetailOrder ((char *)&_XmStrings[18290])
#endif
#ifndef XmCDetailOrder
#define XmCDetailOrder ((char *)&_XmStrings[18302])
#endif
#ifndef XmRCardinalList
#define XmRCardinalList ((char *)&_XmStrings[18314])
#endif
#ifndef XmNdetailOrderCount
#define XmNdetailOrderCount ((char *)&_XmStrings[18327])
#endif
#ifndef XmCDetailOrderCount
#define XmCDetailOrderCount ((char *)&_XmStrings[18344])
#endif
#ifndef XmNoutlineColumnWidth
#define XmNoutlineColumnWidth ((char *)&_XmStrings[18361])
#endif
#ifndef XmCOutlineColumnWidth
#define XmCOutlineColumnWidth ((char *)&_XmStrings[18380])
#endif
#ifndef XmNoutlineChangedCallback
#define XmNoutlineChangedCallback ((char *)&_XmStrings[18399])
#endif
#ifndef XmNOutlineChangedCallback
#define XmNOutlineChangedCallback ((char *)&_XmStrings[18422])
#endif
#ifndef XmNoutlineButtonPolicy
#define XmNoutlineButtonPolicy ((char *)&_XmStrings[18445])
#endif
#ifndef XmCOutlineButtonPolicy
#define XmCOutlineButtonPolicy ((char *)&_XmStrings[18465])
#endif
#ifndef XmNOutlineButtonPolicy
#define XmNOutlineButtonPolicy ((char *)&_XmStrings[18485])
#endif
#ifndef XmNDefaultVirtualBindings
#define XmNDefaultVirtualBindings ((char *)&_XmStrings[18505])
#endif
#ifndef XmNdefaultVirtualBindings
#define XmNdefaultVirtualBindings ((char *)&_XmStrings[18528])
#endif
#ifndef XmCResizable
#define XmCResizable ((char *)&_XmStrings[18551])
#endif
#ifndef XmNDynamicPixmap
#define XmNDynamicPixmap ((char *)&_XmStrings[18561])
#endif
#ifndef XmNpageChangedCallback
#define XmNpageChangedCallback ((char *)&_XmStrings[18575])
#endif
#ifndef XmNarea
#define XmNarea ((char *)&_XmStrings[18595])
#endif
#ifndef XmNdetailShadowThickness
#define XmNdetailShadowThickness ((char *)&_XmStrings[18600])
#endif
#ifndef XmNsliderMark
#define XmNsliderMark ((char *)&_XmStrings[18622])
#endif
#ifndef XmNSliderMark
#define XmNSliderMark ((char *)&_XmStrings[18633])
#endif
#ifndef XmNSliderMark
#define XmNSliderMark ((char *)&_XmStrings[18644])
#endif
#ifndef XmNEnableBtn1Transfer
#define XmNEnableBtn1Transfer ((char *)&_XmStrings[18655])
#endif
#ifndef XmNrenditionBackground
#define XmNrenditionBackground ((char *)&_XmStrings[18674])
#endif
#ifndef XmNrenditionForeground
#define XmNrenditionForeground ((char *)&_XmStrings[18694])
#endif
#ifndef XmCRenditionBackground
#define XmCRenditionBackground ((char *)&_XmStrings[18714])
#endif
#ifndef XmCRenditionForeground
#define XmCRenditionForeground ((char *)&_XmStrings[18734])
#endif
#ifndef XmNindeterminateInsensitivePixmap
#define XmNindeterminateInsensitivePixmap ((char *)&_XmStrings[18754])
#endif
#ifndef XmCIndeterminateInsensitivePixmap
#define XmCIndeterminateInsensitivePixmap ((char *)&_XmStrings[18785])
#endif
#ifndef XmNframeChildType
#define XmNframeChildType ((char *)&_XmStrings[18816])
#endif
#ifndef XmCFrameChildType
#define XmCFrameChildType ((char *)&_XmStrings[18831])
#endif
#ifndef XmNtextField
#define XmNtextField ((char *)&_XmStrings[18846])
#endif
#ifndef XmCTextField
#define XmCTextField ((char *)&_XmStrings[18856])
#endif
#ifndef XmNenableThinThickness
#define XmNenableThinThickness ((char *)&_XmStrings[18866])
#endif
#ifndef XmCEnableThinThickness
#define XmCEnableThinThickness ((char *)&_XmStrings[18886])
#endif
#ifndef XmNprimaryColorSetId
#define XmNprimaryColorSetId ((char *)&_XmStrings[18906])
#endif
#ifndef XmCPrimaryColorSetId
#define XmCPrimaryColorSetId ((char *)&_XmStrings[18924])
#endif
#ifndef XmNsecondaryColorSetId
#define XmNsecondaryColorSetId ((char *)&_XmStrings[18942])
#endif
#ifndef XmCSecondaryColorSetId
#define XmCSecondaryColorSetId ((char *)&_XmStrings[18962])
#endif
#ifndef XmNtextColorSetId
#define XmNtextColorSetId ((char *)&_XmStrings[18982])
#endif
#ifndef XmCTextColorSetId
#define XmCTextColorSetId ((char *)&_XmStrings[18997])
#endif
#ifndef XmNactiveColorSetId
#define XmNactiveColorSetId ((char *)&_XmStrings[19012])
#endif
#ifndef XmCActiveColorSetId
#define XmCActiveColorSetId ((char *)&_XmStrings[19029])
#endif
#ifndef XmNinactiveColorSetId
#define XmNinactiveColorSetId ((char *)&_XmStrings[19046])
#endif
#ifndef XmCInactiveColorSetId
#define XmCInactiveColorSetId ((char *)&_XmStrings[19065])
#endif
#ifndef XmNuseColorObj
#define XmNuseColorObj ((char *)&_XmStrings[19084])
#endif
#ifndef XmCUseColorObj
#define XmCUseColorObj ((char *)&_XmStrings[19096])
#endif
#ifndef XmNuseTextColor
#define XmNuseTextColor ((char *)&_XmStrings[19108])
#endif
#ifndef XmCUseTextColor
#define XmCUseTextColor ((char *)&_XmStrings[19121])
#endif
#ifndef XmNuseTextColorForList
#define XmNuseTextColorForList ((char *)&_XmStrings[19134])
#endif
#ifndef XmCUseTextColorForList
#define XmCUseTextColorForList ((char *)&_XmStrings[19154])
#endif
#ifndef XmNuseMask
#define XmNuseMask ((char *)&_XmStrings[19174])
#endif
#ifndef XmCUseMask
#define XmCUseMask ((char *)&_XmStrings[19182])
#endif
#ifndef XmNuseMultiColorIcons
#define XmNuseMultiColorIcons ((char *)&_XmStrings[19190])
#endif
#ifndef XmCUseMultiColorIcons
#define XmCUseMultiColorIcons ((char *)&_XmStrings[19209])
#endif
#ifndef XmNuseIconFileCache
#define XmNuseIconFileCache ((char *)&_XmStrings[19228])
#endif
#ifndef XmCUseIconFileCache
#define XmCUseIconFileCache ((char *)&_XmStrings[19245])
#endif
/*
#ifndef XmNPixel Sets
#define XmNPixel Sets ((char *)&_XmStrings[19262])
#endif
#ifndef XmNCustomize Data:
#define XmNCustomize Data: ((char *)&_XmStrings[19273])
#endif
*/
#ifndef XmNColorServer
#define XmNColorServer ((char *)&_XmStrings[19289])
#endif
#ifndef XmNlist
#define XmNlist ((char *)&_XmStrings[19301])
#endif
#ifndef XmNList
#define XmNList ((char *)&_XmStrings[19306])
#endif
#ifndef XmNarrowOrientation
#define XmNarrowOrientation ((char *)&_XmStrings[19311])
#endif
#ifndef XmNArrowOrientation
#define XmNArrowOrientation ((char *)&_XmStrings[19328])
#endif
#ifndef XmNArrowOrientation
#define XmNArrowOrientation ((char *)&_XmStrings[19345])
#endif
#ifndef XmNpositionType
#define XmNpositionType ((char *)&_XmStrings[19362])
#endif
#ifndef XmNPositionType
#define XmNPositionType ((char *)&_XmStrings[19375])
#endif
#ifndef XmNPositionType
#define XmNPositionType ((char *)&_XmStrings[19388])
#endif
#ifndef XmNwrap
#define XmNwrap ((char *)&_XmStrings[19401])
#endif
#ifndef XmNWrap
#define XmNWrap ((char *)&_XmStrings[19406])
#endif
#ifndef XmNpositionMode
#define XmNpositionMode ((char *)&_XmStrings[19411])
#endif
#ifndef XmCPositionMode
#define XmCPositionMode ((char *)&_XmStrings[19424])
#endif
#ifndef XmRPositionMode
#define XmRPositionMode ((char *)&_XmStrings[19437])
#endif
#ifndef XmNprintOrientation
#define XmNprintOrientation ((char *)&_XmStrings[19450])
#endif
#ifndef XmNPrintOrientation
#define XmNPrintOrientation ((char *)&_XmStrings[19467])
#endif
#ifndef XmNprintOrientations
#define XmNprintOrientations ((char *)&_XmStrings[19484])
#endif
#ifndef XmNPrintOrientations
#define XmNPrintOrientations ((char *)&_XmStrings[19502])
#endif
#ifndef XmNprintResolution
#define XmNprintResolution ((char *)&_XmStrings[19520])
#endif
#ifndef XmNPrintResolution
#define XmNPrintResolution ((char *)&_XmStrings[19536])
#endif
#ifndef XmNprintResolutions
#define XmNprintResolutions ((char *)&_XmStrings[19552])
#endif
#ifndef XmNPrintResolutions
#define XmNPrintResolutions ((char *)&_XmStrings[19569])
#endif
#ifndef XmNdefaultPixmapResolution
#define XmNdefaultPixmapResolution ((char *)&_XmStrings[19586])
#endif
#ifndef XmCDefaultPixmapResolution
#define XmCDefaultPixmapResolution ((char *)&_XmStrings[19610])
#endif
#ifndef XmNstartJobCallback
#define XmNstartJobCallback ((char *)&_XmStrings[19634])
#endif
#ifndef XmNendJobCallback
#define XmNendJobCallback ((char *)&_XmStrings[19651])
#endif
#ifndef XmNpageSetupCallback
#define XmNpageSetupCallback ((char *)&_XmStrings[19666])
#endif
#ifndef XmNpdmNotificationCallback
#define XmNpdmNotificationCallback ((char *)&_XmStrings[19684])
#endif
#ifndef XmNminX
#define XmNminX ((char *)&_XmStrings[19708])
#endif
#ifndef XmNminY
#define XmNminY ((char *)&_XmStrings[19713])
#endif
#ifndef XmNmaxX
#define XmNmaxX ((char *)&_XmStrings[19718])
#endif
#ifndef XmNmaxY
#define XmNmaxY ((char *)&_XmStrings[19723])
#endif
#ifndef XmNMinX
#define XmNMinX ((char *)&_XmStrings[19728])
#endif
#ifndef XmNMinY
#define XmNMinY ((char *)&_XmStrings[19733])
#endif
#ifndef XmNMaxX
#define XmNMaxX ((char *)&_XmStrings[19738])
#endif
#ifndef XmNMaxY
#define XmNMaxY ((char *)&_XmStrings[19743])
#endif
#ifndef XmNpreeditStartCallback
#define XmNpreeditStartCallback ((char *)&_XmStrings[19748])
#endif
#ifndef XmNpreeditDoneCallback
#define XmNpreeditDoneCallback ((char *)&_XmStrings[19769])
#endif
#ifndef XmNpreeditDrawCallback
#define XmNpreeditDrawCallback ((char *)&_XmStrings[19789])
#endif
#ifndef XmNpreeditCaretCallback
#define XmNpreeditCaretCallback ((char *)&_XmStrings[19809])
#endif
#ifndef XmNverifyPreedit
#define XmNverifyPreedit ((char *)&_XmStrings[19830])
#endif
#ifndef XmNVerifyPreedit
#define XmNVerifyPreedit ((char *)&_XmStrings[19844])
#endif
#ifndef XmNenableMultiKeyBindings
#define XmNenableMultiKeyBindings ((char *)&_XmStrings[19858])
#endif
#ifndef XmSEnableMultiKeyBindings
#define XmSEnableMultiKeyBindings ((char *)&_XmStrings[19881])
#endif
#ifndef XmSButtonFontList
#define XmSButtonFontList ((char *)&_XmStrings[19904])
#endif
#ifndef XmSLabelFontList
#define XmSLabelFontList ((char *)&_XmStrings[19919])
#endif
#ifndef XmSTextFontList
#define XmSTextFontList ((char *)&_XmStrings[19933])
#endif
/*
#ifndef XmSSDT Pixel Set
#define XmSSDT Pixel Set ((char *)&_XmStrings[19946])
#endif
*/
#ifndef XmS50_foreground
#define XmS50_foreground ((char *)&_XmStrings[19960])
#endif
#ifndef XmSunspecified_pixmap
#define XmSunspecified_pixmap ((char *)&_XmStrings[19974])
#endif

/* Xft stuff */
#define	XmNfontStyle			"fontStyle"
#define	XmCFontStyle			"FontStyle"
#define	XmNfontFoundry			"fontFoundry"
#define	XmCFontFoundry			"FontFoundry"
#define	XmNfontEncoding			"fontEncoding"
#define	XmCFontEncoding			"FontEncoding"
#define	XmNfontSize			"fontSize"
#define	XmCFontSize			"FontSize"

/*
 * Not sure why these aren't in _XmStrings[].
 */
#define	XmCMinX				"XmCMinX"
#define	XmCMinY				"XmCMinY"
#define	XmCMaxX				"XmCMaxX"
#define	XmCMaxY				"XmCMaxY"

/* FIX ME */
#define	XmRDefaultArrowSensitivity	"DefaultArrowSensitivity"
#define	XmCDetailShadowThickness	"DetailShadowThickness"
#define	XmCFrameShadowThickness		"FrameShadowThickness"
#define	XmCPageChangedCallback		"PageChangedCallback"

#define XmCPositionType                 "PositionType"
#define	XmCArrowOrientation		"ArrowOrientation"
#define	XmRArrowOrientation		"ArrowOrientation"

#define XmRStringTag                    "StringTag"
/*
 * The end
 */
#ifdef __cplusplus
}
#endif

#endif /* _XM_XMSTRDEFS_H */
