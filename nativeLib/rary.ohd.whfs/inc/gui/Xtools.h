/*
	File:		Xtools.h
	Date:		8/10/1994
	Author:		Dale Shelton
	
	Purpose:	General Header file for Xtools.
        Modification History:
        Bryon Lawrence       June 29, 2001    Removed the prototype for the
                                              DataToString routine.  Also
                                              removed the conditional
                                              compilation directives
                                              surrounding it.  This routine
                                              already resides in the
                                              libDbmsUtils.a library.  Include
                                              DbmsUtils.h for its prototype. 
        Bryon Lawrence       November 1, 2001 Added the prototype for the
                                              FileDialog routine.
        Bryon Lawrence       October 31, 2005 Changed the return type on
                                              routine CalcRgbDiff from
                                              long to double.
*/


#ifndef Xtools_h
#define Xtools_h

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/BulletinB.h>
#include <Xm/MessageB.h>
#include <Xm/List.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <X11/cursorfont.h>


typedef  char LongText[BUFSIZ];
typedef  char RussText[100];



/* (alpha_filter & alphanum_filter:  Choose one from below) */
#define		MIXEDCASE			0
#define		MIXEDCASE_AND_VALIDSYMBOLS	1
#define		UPPERCASE			2
#define		UPPERCASE_AND_HYPHENS		3
#define		LOWERCASE			4

/* (num_filter:  Choose one from below) */
#define		INTEGERS				0
#define		INTEGERS_AND_DECIMALS			1
#define		INTEGERS_AND_DECIMALS_AND_HYPHENS	2
#define		INTEGERS_AND_HYPHENS			3
#define		INTEGERS_AND_SLASHES			4
#define		INTEGERS_AND_COLONS			5

#define		INTEGERS_AND_DECIMALS_SIGN		11

/* (may be used in conjunction with any of the above filters) */
#define		SPACES		99	/*
					   (e.g. (XtPointer)(MIXEDCASE+SPACES)
					    or   (XtPointer)(UPPERCASE+SPACES)
					    or   (XtPointer)(INTEGERS+SPACES)
					   )
					*/

#ifdef __cplusplus
extern "C" {
#endif

/*
	Composite resources.
*/
void	DestroyChildren(Widget w);
void    GetWidgetChildren(Widget parent, WidgetList *childList, int *childCount);


/*
	Core resources.
*/
Widget  GetTopShell(Widget widget);

void	setWidgetHeight(Widget widget, int width);
void	setWidgetWidth(Widget widget, int width);
int	getWidgetHeight(Widget widget);
int	getWidgetWidth(Widget widget);

int	IsSensitive(Widget w);
void	Sensitize(Widget w);
void	DeSensitize(Widget w);

Position Xpos(Widget widget);
Position Ypos(Widget widget);
void    SetXpos(Widget widget, Position x);
void    SetYpos(Widget widget, Position y);

Pixel   GetBackground(Widget widget);
void    SetBackground(Widget widget, Pixel fg);
void  PlaceWidgets(Widget widgetList[], const long numWidgets,
		      const Dimension margin, const Dimension managerWidth);
void  GetNewPositions(const Widget widgetList[], Position  posList[],
	              const long numWidgets,
		      const Dimension margin, const Dimension managerWidth);




/*
	Cursor Resources.
*/
void SetCursor(Widget w, int cursor_type);
void UnsetCursor(Widget w);


Position GetWinCoord(double data_value,
		   double min_data_coord,
		   double max_data_coord,
		   Position min_win_coord,
		   Position max_win_coord);
		   
double GetDataCoord(Position win_value,
		   double min_data_coord,
		   double max_data_coord,
		   Position min_win_coord,
		   Position max_win_coord);
		   
void   CopyPixmapToDA(Widget widget, Pixmap pixmap);

/*
	Dialog Resources.
*/
void	RemapWidget(Widget w);

/*
	Graphics Context resources.
*/
void	SetColor(GC gc, Widget widget, char *colorName);
void	SetFont(Display *display, GC gc, char *font);

Pixel	GetNamedColor(Widget widget, char *colorName);
XColor  GetClosestXColor(Widget widget, char *colorName);
XColor  GetSubstituteXColor(Widget widget, char * colorName);
XColor  PickBestColor(XColor desiredColor,
		      XColor availableColors[],
		      int numColors);
double CalcRgbDiff(XColor colorA, XColor colorB);



/*
	Label resources.
*/
char *	GetLabel(Widget widget);
void	SetLabel(Widget widget, char *string);
void    SetLabelWithFont(Widget widget, char *string, char* fontlist_tag);


/* 
	List Resources
*/
void  loadXmList(Widget xmList, LongText *items, long numItems);
void  loadXmList100(Widget xmList, RussText *items, long numItems);
		     
void CheckListStatus(Widget	w, 
		     int 	cnt,
		     int 	**select_state);
int ListRsrcGetCount(Widget w);
int ListRsrcGetSelectedCount(Widget w);
int ListRsrcGetFirstSelectedPos(Widget list);


/*
	Manager resources
*/
Pixel   GetForeground(Widget widget);
void    SetForeground(Widget widget, Pixel fg);



/*
	Menu resources.
*/
Widget	GetMenuHistory(Widget w);
void	SetMenuHistory(Widget w, char *txt);
int	GetMenuPos(Widget w);
void	SetMenuPos(Widget w, int pos);



/*
	Shell resources.
*/
void	SetTitle(Widget widget, char *string);




/*
	Text resources.
*/
void	addTextWidgetCallbacksBelowParent(Widget parent,
					  String callback_name,
					  XtCallbackProc callback_function,
					  XtPointer ptr);

void	removeTextWidgetCallbacksBelowParent(Widget parent,
					     String callback_name,
					     XtCallbackProc callback_function,
					     XtPointer ptr);

int	CheckTextBounds(Widget label, char *buf,
			char* lo_cond, double* lo, char* lo_format,
			char* hi_cond, double* hi, char* hi_format);

void    getTextFromXmString(char *text, XmString compoundString, long maxLen);





/*
	Text Filter functions.
*/
void	alpha_filter	(Widget w, XtPointer ptr, XmTextVerifyCallbackStruct *cbs);
void	alphanum_filter	(Widget w, XtPointer ptr, XmTextVerifyCallbackStruct *cbs);
void	num_filter	(Widget w, XtPointer ptr, XmTextVerifyCallbackStruct *cbs);

void	date_filter	(Widget w, XtPointer ptr, XmTextVerifyCallbackStruct *cbs);

void	seasonal_filter(Widget w, Widget om, XmTextVerifyCallbackStruct *cbs);
void	seasonal_pb_helper(Widget w, Widget om, XmPushButtonCallbackStruct *cbs);

void	time_filter	(Widget w, XtPointer ptr, XmTextVerifyCallbackStruct *cbs);
void	zip_filter	(Widget w, XtPointer ptr, XmTextVerifyCallbackStruct *cbs);



/*
	Utilities.
*/
Widget	ErrorDialog(Widget widget, char *msg);
Widget  SendDialog(Widget widget, char *msg);
Widget	QuestionDialog(Widget widget, char *msg);
void	QuestionDialogWithCallbacks(Widget parent, char *message, char *title,
				    XtCallbackProc ok_callback_function,
				    XtCallbackProc cancel_callback_function);
Widget InfoDialog(Widget widget, char *msg);
Widget FileDialog ( Widget toplevel ,
                    ArgList arglist ,
                    Cardinal argcount ,
                    XtCallbackProc ok_cb ,
                    XtPointer ok_client_data ,
                    XtCallbackProc cancel_cb ,
                    XtPointer cancel_client_data ,
                    XtCallbackProc help_cb ,
                    XtPointer help_client_data ) ;

/*
	Form Clear function..
	Inits all widget children of a Manager widget
*/
void	clearForm(Widget parent);




/* ItemChooser functions */

typedef void (*ApplyFunctionPtr)(int * selectedPositionArray, int itemCount);							   
typedef void (*CloseFunctionPtr)(int * selectedPositionArray);							   

void show_item_chooser(Widget w, 
			      	  char *dialogTitleString,
                      LongText *itemStringArray,
                      int *selectedPositionArray,
                      int itemCount,
                      int allowMultipleSelection,
                      ApplyFunctionPtr applyFunctionPtr,
                      CloseFunctionPtr closeFunctionPtr);
			      

#ifdef __cplusplus
}
#endif



#endif


