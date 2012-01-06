/****************************************************************/
/*								*/
/*	FILE:		Mods_addCallbacks.c			*/
/*								*/
/*	Code to add Callback functions to XDesigner 'static'	*/
/*	Widgets							*/
/*								*/
/*	Coded by:	Tom Adams				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		09/08/94				*/
/*      Modified by:    D. Page - 10/14/95                      */ 
/*                                10/21/95 for setqmean 	*/
/*				  10/31/95, 11/11/95            */
/*                                5/1/97        		*/
/*                      A. Vo     5/1/99 for fg,segment mods	*/
/*								*/
/*								*/
/*      NOTE:	This file contains functions that complete	*/
/*		the interface setup - these include GUI		*/
/*		elements that change dynamically and can not	*/
/*		be created statically in a GUI builder,		*/
/*		such as the available Operations or Mods for	*/
/*		an individual Segment (basin).			*/
/*								*/
/****************************************************************/





#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"



/*static void addModMenuItemsCallbacks(Mods_everythingStruct *);*/
static void addMainWinDialogPushBCallbacks(Mods_everythingStruct *);
static void addViewerDialogPushBCallbacks(Mods_everythingStruct *);
static void addViewerOtherCallbacks(Mods_everythingStruct *);
static void addSetQMeanCallbacks(Mods_everythingStruct *);

extern void opMenuItemSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void modMenuItemSelectionCB(Widget, Mods_everythingStruct *, XtPointer);
extern void destroyModsDataCB(Widget, Mods_everythingStruct *, XtPointer);
extern void set_show_mods_viewer(Widget, caddr_t, XmToggleButtonCallbackStruct);
extern void scaleChangeCB(Widget, Mods_everythingStruct *, XmScaleCallbackStruct *);
extern void handleValueEntryCB(Widget,  Mods_everythingStruct *, XmAnyCallbackStruct *);
extern void handleShowDataCB(Widget, Mods_everythingStruct *, XtPointer);
extern void opTSListSelectionCB(Widget, Mods_everythingStruct *, XtPointer);
extern void set_currentModSavedFalseCB(Widget, Mods_everythingStruct *, XmAnyCallbackStruct *);

/*------------------------------------------------------------------------------*/
/*	Main Window DialogButton Callbacks					*/
/*------------------------------------------------------------------------------*/
extern void createSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void closeSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void undoSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void preferencesSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void helpSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);

/*------------------------------------------------------------------------------*/
/*	Viewer Window DialogButton Callbacks					*/
/*------------------------------------------------------------------------------*/
extern void viewerSaveSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void viewerCloseSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void viewerDeleteSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void viewerUndoSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void viewerHelpSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);

/*------------------------------------------------------------------------------*/
/*	 Viewer Window OptionMenu and Other Callbacks	               	        */
/*------------------------------------------------------------------------------*/
extern void viewerDeleteModeSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);

/*------------------------------------------------------------------------------*/
extern void rangeSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void segmentSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void fgroupSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
/*------------------------------------------------------------------------------*/
/*	 SETQMEAN Callbacks	                                	        */
/*------------------------------------------------------------------------------*/
extern void setQMeanNextSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void setQMeanDoneSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void setQMeanCancelSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void setQMeanHelpSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void setQMeanTextCB(Widget, Mods_everythingStruct *, XmAnyCallbackStruct *);

void addWidgetCallbacks(Mods_everythingStruct *data)
{

	/*----------------------------------------------------------------------*/
	/*	ModsShell Widget Destroy Callbacks				*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(XtParent(data->widgetData->ifp_modsMessageBox), 
		XmNdestroyCallback, (XtCallbackProc)destroyModsDataCB, data);

	/*----------------------------------------------------------------------*/
	/*	New Mods Window Callbacks					*/
	/*  changed callback associated with newModsToggle to coordinate with   */
	/*  Mods Viewer button now on the main Mods menu. dp - 2 May 1997       */
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->widgetData->newModsToggle, 
		XmNvalueChangedCallback, (XtCallbackProc)set_show_mods_viewer, data);

	/*----------------------------------------------------------------------*/
	/*	Data Window Callbacks						*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->widgetData->dataEntryToggle, 
		XmNvalueChangedCallback, (XtCallbackProc)handleShowDataCB, data);

	/*----------------------------------------------------------------------*/
	/*	Scale ValueChanged Callbacks					*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->widgetData->modsScale,
		XmNvalueChangedCallback, (XtCallbackProc)scaleChangeCB, data);
	
	/*----------------------------------------------------------------------*/
	/*	TextField Callbacks 						*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->widgetData->valueDisplayEntry,
		XmNvalueChangedCallback, (XtCallbackProc)handleValueEntryCB, data);

	/*----------------------------------------------------------------------*/
	/*	Operation/Time-Series List selection Callbacks			*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->widgetData->timeSeriesList,
		XmNmultipleSelectionCallback, (XtCallbackProc)opTSListSelectionCB, data);
	XtAddCallback(data->widgetData->timeSeriesList,
		XmNsingleSelectionCallback, (XtCallbackProc)opTSListSelectionCB, data);
		
	/*----------------------------------------------------------------------*/
	/*	Dialog Controls for main window - activate Callbacks		*/
	/*----------------------------------------------------------------------*/
	addMainWinDialogPushBCallbacks(data);
	
	/*----------------------------------------------------------------------*/
	/*	Dialog Controls for Mods Viewer - activate Callbacks		*/
	/*----------------------------------------------------------------------*/
	addViewerDialogPushBCallbacks(data);

	/*----------------------------------------------------------------------*/
	/*	Other Callbacks for Mods Viewer - activate Callbacks		*/
	/*----------------------------------------------------------------------*/
	addViewerOtherCallbacks(data);
	
	/*----------------------------------------------------------------------*/
	/*	Dialog Controls for SETQMEAN mod - activate Callbacks		*/
	/*----------------------------------------------------------------------*/
	addSetQMeanCallbacks(data);
	
}



static void addMainWinDialogPushBCallbacks(Mods_everythingStruct *data)
{

	/*----------------------------------------------------------------------*/
	/*	Create button							*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->widgetData->modsCreate,
		XmNactivateCallback, (XtCallbackProc)createSelectionCB, data);

	/*----------------------------------------------------------------------*/
	/*	Close button							*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->widgetData->modsClose,
		XmNactivateCallback, (XtCallbackProc)closeSelectionCB, data);

	/*----------------------------------------------------------------------*/
	/*	Undo button							*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->widgetData->modsUndo,
		XmNactivateCallback, (XtCallbackProc)undoSelectionCB, data);

	/*----------------------------------------------------------------------*/
	/*	Preferences button						*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->widgetData->modsPreferences,
		XmNactivateCallback, (XtCallbackProc)preferencesSelectionCB, data);

	/*----------------------------------------------------------------------*/
	/*	Help button							*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->widgetData->modsHelp,
		XmNactivateCallback, (XtCallbackProc)helpSelectionCB, data);

	/*----------------------------------------------------------------------*/
	/*	Segment button							*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->widgetData->SegmentButton,
		XmNactivateCallback, (XtCallbackProc)segmentSelectionCB, data);

	/*----------------------------------------------------------------------*/
	/*	FGroup button							*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->widgetData->FgroupButton,
		XmNactivateCallback, (XtCallbackProc)fgroupSelectionCB, data);
	
	/*----------------------------------------------------------------------*/
	/*	Range button							*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->widgetData->RangeButton,
		XmNactivateCallback, (XtCallbackProc)rangeSelectionCB, data);
}


static void addViewerDialogPushBCallbacks(Mods_everythingStruct *data)
{

	/*----------------------------------------------------------------------*/
	/*	Save button							*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->viewerWidgets->viewerSave,
		XmNactivateCallback, (XtCallbackProc)viewerSaveSelectionCB, data);

	/*----------------------------------------------------------------------*/
	/*	Close button							*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->viewerWidgets->viewerClose,
		XmNactivateCallback, (XtCallbackProc)viewerCloseSelectionCB, data);

	/*----------------------------------------------------------------------*/
	/*	Delete button							*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->viewerWidgets->viewerDelete,
		XmNactivateCallback, (XtCallbackProc)viewerDeleteSelectionCB, data);

	/*----------------------------------------------------------------------*/
	/*	Undo button							*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->viewerWidgets->viewerUndo,
		XmNactivateCallback, (XtCallbackProc)viewerUndoSelectionCB, data);

	/*----------------------------------------------------------------------*/
	/*	Help button							*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->viewerWidgets->viewerHelp,
		XmNactivateCallback, (XtCallbackProc)viewerHelpSelectionCB, data);
}

static void addViewerOtherCallbacks(Mods_everythingStruct *data)
{
	/*----------------------------------------------------------------------*/
	/*	Delete Mode Option Menu button					*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->viewerWidgets->DeleteModeB,
		      XmNactivateCallback, (XtCallbackProc)viewerDeleteModeSelectionCB, data);
}

static void addSetQMeanCallbacks(Mods_everythingStruct *data)
{
	/*----------------------------------------------------------------------*/
	/*	Next button		                			*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->setQMeanWidgets->next,
		XmNactivateCallback, (XtCallbackProc)setQMeanNextSelectionCB, data);

	/*----------------------------------------------------------------------*/
	/*	Done button		                			*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->setQMeanWidgets->done,
		XmNactivateCallback, (XtCallbackProc)setQMeanDoneSelectionCB, data);

	/*----------------------------------------------------------------------*/
	/*	Cancel button		                			*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->setQMeanWidgets->cancel,
		XmNactivateCallback, (XtCallbackProc)setQMeanCancelSelectionCB, data);

	/*----------------------------------------------------------------------*/
	/*	Help button		                			*/
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->setQMeanWidgets->help,
		XmNactivateCallback, (XtCallbackProc)setQMeanHelpSelectionCB, data);
		
	/*----------------------------------------------------------------------*/
	/*	Text Field Activate Callback  (Return entered in text field)    */
	/*----------------------------------------------------------------------*/
	XtAddCallback(data->setQMeanWidgets->setQMeanText,
		XmNactivateCallback, (XtCallbackProc)setQMeanTextCB, data);
		

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_addCallbacks.c,v $";
 static char rcs_id2[] = "$Id: Mods_addCallbacks.c,v 1.5 2006/04/18 13:33:56 aivo Exp $";}
/*  ===================================================  */

}
