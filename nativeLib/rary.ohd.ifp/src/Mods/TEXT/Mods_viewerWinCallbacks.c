/****************************************************************/
/*								*/
/*	FILE:		Mods_viewerWinCallbacks.c		*/
/*								*/
/*	Callback functions for the Mods Viewer Window Dialog	*/
/*	PushButtons						*/
/*								*/
/*	Coded by:	Tom Adams				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		11/22/94                                */
/*      Modified by:    D. Page - 10/14/95      		*/
/*      		A. Vo   - 05/20/99      		*/
/*								*/
/****************************************************************/



#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "ifp_atoms.h"

extern void save_mods_from_file_edit(Mods_everythingStruct *);
extern void save_mods_from_ofs_edit(Mods_everythingStruct *);
extern void save_mods_from_ofsfgroup_edit(Mods_everythingStruct *);
extern void save_fgroupmods_from_file_edit(Mods_everythingStruct *);
extern void undo_mods_from_file_edit(Mods_everythingStruct *);
extern void undo_mods_from_ofs_edit(Mods_everythingStruct *);
extern void undo_mods_from_ofsfgroup_edit(Mods_everythingStruct *);
extern void undo_fgroupmods_from_file_edit(Mods_everythingStruct *);
extern void post_show_mods_viewer_atom(Widget, int);

void viewerSaveSelectionCB(Widget w, Mods_everythingStruct *data, 
                           XmPushButtonCallbackStruct *call_data)
{
    save_mods_from_ofs_edit(data);
    save_mods_from_ofsfgroup_edit(data);
    save_mods_from_file_edit(data);
    save_fgroupmods_from_file_edit(data);
    
    data->fromFileModsSaved = TRUE;

    data->fromFilefgModsSaved = TRUE;

    data->ofsModsSaved = TRUE; 

    data->ofsfgModsSaved = TRUE; 
} 

void viewerCloseSelectionCB(Widget w, Mods_everythingStruct *data, 
                            XmPushButtonCallbackStruct *call_data)
{
   int     showModsViewer;
   
    if(data->fromFileModsSaved == FALSE || data->ofsModsSaved == FALSE || data->fromFilefgModsSaved == FALSE
				 || data->ofsfgModsSaved == FALSE)
      XtManageChild(data->dialogStruct->modViewerNotSavedMB);
    else
    {
       XmToggleButtonSetState(data->widgetData->newModsToggle, FALSE, FALSE);
       /* added to coordinate with Mods Viewer on the main Mods menu 
        * and on the other mods window  2 May 1997 - dp
        */
       showModsViewer = FALSE;
       post_show_mods_viewer_atom(w, showModsViewer);
    }	
}


void viewerUndoSelectionCB(Widget w, Mods_everythingStruct *data, 
                           XmPushButtonCallbackStruct *call_data)
{
    undo_mods_from_ofs_edit(data);
    undo_mods_from_ofsfgroup_edit(data);
    undo_mods_from_file_edit(data);
    undo_fgroupmods_from_file_edit(data);
    
    data->fromFileModsSaved = TRUE;
    data->ofsModsSaved = TRUE; 
    data->ofsfgModsSaved = TRUE; 
    data->fromFilefgModsSaved = TRUE;

}


void viewerDeleteSelectionCB(Widget w, Mods_everythingStruct *data, 
                             XmPushButtonCallbackStruct *call_data)
{



}

void viewerHelpSelectionCB(Widget w, Mods_everythingStruct *data, 
                           XmPushButtonCallbackStruct *call_data)
{

   printf("Sorry, no help available at this time\n");

}

void viewerDeleteModeSelectionCB(Widget w, Mods_everythingStruct *data, 
                                 XmPushButtonCallbackStruct *call_data)
{
    XmString     xmStringName;
    char         *name;

    XtVaGetValues(w, XmNlabelString, &xmStringName, NULL);
    XmStringGetLtoR(xmStringName, XmFONTLIST_DEFAULT_TAG, &name);

    /* printf("Inside 'viewerDeleteModeSelectionCB()' CALLBACK function for %s\n", name); */
        
}

void saveViewerChangesCB(Widget w, Mods_everythingStruct *data, 
                         XmPushButtonCallbackStruct *call_data)
{
    if(data->fromFileModsSaved == FALSE)
    {
       save_mods_from_file_edit(data);
       data->fromFileModsSaved = TRUE;
    }
       
    if(data->ofsModsSaved == FALSE)
    {
    	save_mods_from_ofs_edit(data);
   	data->ofsModsSaved = TRUE;
    }

    if(data->ofsfgModsSaved == FALSE)
    {
    	save_mods_from_ofsfgroup_edit(data);
   	data->ofsfgModsSaved = TRUE;
    }

    if(data->fromFilefgModsSaved == FALSE)
    {
       save_fgroupmods_from_file_edit(data);
       data->fromFilefgModsSaved = TRUE;
    }   
       
}                                     

void undoViewerChangesCB(Widget w, Mods_everythingStruct *data, 
                         XmPushButtonCallbackStruct *call_data)
{
    if(data->fromFileModsSaved == FALSE)
    {
       undo_mods_from_file_edit(data);
       data->fromFileModsSaved = TRUE;
    }
        
    if(data->ofsModsSaved == FALSE)
    {
       undo_mods_from_ofs_edit(data);
       data->ofsModsSaved = TRUE;
    }   

    if(data->ofsfgModsSaved == FALSE)
    {
       undo_mods_from_ofsfgroup_edit(data);
       data->ofsfgModsSaved = TRUE;
    }   

    if(data->fromFilefgModsSaved == FALSE)
    {
       undo_fgroupmods_from_file_edit(data);
       data->fromFilefgModsSaved = TRUE;
    }   
} 

void viewerWindowChangedCB(Widget w, Mods_everythingStruct *data,
                           XmAnyCallbackStruct *call_data)
{
    if(w == data->viewerWidgets->fromFileModsText)
       data->fromFileModsSaved = FALSE;
       
    if( w == data->viewerWidgets->ofsModsText)
       data->ofsModsSaved = FALSE;

    if( w == data->viewerWidgets->ofsFGroupModsText)
       data->ofsfgModsSaved = FALSE;

    if( w == data->viewerWidgets->fromFileFGroupModsText)
       data->fromFilefgModsSaved = FALSE;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_viewerWinCallbacks.c,v $";
 static char rcs_id2[] = "$Id: Mods_viewerWinCallbacks.c,v 1.3 1999/07/27 13:21:31 page Exp $";}
/*  ===================================================  */

}                                                                     
