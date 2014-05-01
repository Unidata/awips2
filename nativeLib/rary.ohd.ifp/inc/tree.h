/*************************************************************************

File:   tree.h

Function: public header file for Tree_widget

Type: public header file

Source: X Window Systems Programming and  Applications with Xt
		by Douglas A. Young

	-- Modified 7/17/90 for use with OSF/Motif Widgets on an IBM-RS6000

*************************************************************************/

#ifndef tree_h
#define tree_h

extern WidgetClass XstreeWidgetClass;

typedef struct _XsTreeClassRec  *XsTreeWidgetClass;
typedef struct _XsTreeRec       *XsTreeWidget;

#define XtNhorizontalSpace      "horizontalSpace"
#define XtNverticalSpace        "verticalSpace"
#define XtCPad                  "Pad"
#define XtNsuperNode            "superNode"
#define XtCSuperNode            "SuperNode"
#endif /* tree_h */
