/****************************************************************/
/*								                                */
/*	FILE:		Mods_addValidInterfaceCallbacks.c	            */
/*								                                */
/*	Code to remove Callback functions to valid interface	    */
/*	Widgets							                            */
/*								                                */
/*	Coded by:	D. Page 				                        */
/*			NWS * Office of Hydrology * HRL		                */
/*	Date:		1 Nov. 1995				                        */
/*								                                */
/****************************************************************/

#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

void set_currentModSavedFalseCB(Widget, Mods_everythingStruct *,  
                                  XmAnyCallbackStruct *); 

void removeValidInterfaceCallbacks(Mods_everythingStruct *data)
{
   /*-----------------Scale Widget: modsScale --------------------------*/
   XtRemoveCallback(data->widgetData->modsScale,
                    XmNvalueChangedCallback, 
                    (XtCallbackProc)set_currentModSavedFalseCB, data);
	
   /*-----------------Text Field Widget: valueDisplayEntry -------------*/
   XtRemoveCallback(data->widgetData->valueDisplayEntry,
                    XmNvalueChangedCallback, 
                    (XtCallbackProc)set_currentModSavedFalseCB, data);
                    
   /*-------------List Widget: Operation/Time-Series List --------------*/
   XtRemoveCallback(data->widgetData->timeSeriesList,
                    XmNmultipleSelectionCallback, 
                    (XtCallbackProc)set_currentModSavedFalseCB, data);

   /*-------------List Widget: TS Date List ---------------------------*/
   if(data->selectedModDef->TSDates) 
      XtRemoveCallback(data->widgetData->datesValueSList,
                       XmNextendedSelectionCallback, 
                       (XtCallbackProc)set_currentModSavedFalseCB, data);
      
   /*-------------OptionMenu Widget - Mod Option ----------------------*/
   /* Added call to set_currentModSavedFalseCB in 
    * modOptionsMenuItemSelectionCB - could not find good widget to
    * formally add callback to.
    */
			
   /*-------------DataEntryToggle Widget - DateWinType ----------------*/
   /* This is handled by the done button on the data entry window -
    * either the plot or setqmean display.  The done button creates
    * the mod and sets the the appropriate flags for mod written and
    * mod changed.
    */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_removeValidInterfaceCallbacks.c,v $";
 static char rcs_id2[] = "$Id: Mods_removeValidInterfaceCallbacks.c,v 1.2 2006/04/18 15:28:07 aivo Exp $";}
/*  ===================================================  */

}
