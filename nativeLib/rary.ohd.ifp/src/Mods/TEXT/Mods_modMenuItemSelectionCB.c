/**************************************************************/
/*                                                            */
/*	FILE:		Mods_initializeMenuStruct.c           */
/*                                                            */
/*	Run-time Modifications menu initialization	      */
/*	module						      */
/*                                                            */
/*	Coded by:	Tom Adams                             */
/*			NWS * Office of Hydrology * HRL       */
/*	Date:		09/08/94                              */
/*                                                            */
/*      Modified by:    D. Page                               */
/*                      30 Oct. 1995                          */
/*                                                            */
/*      NOTE:	This file contains functions that complete    */
/*		the interface setup - these include GUI	      */
/*		elements that change dynamically and can not  */
/*		be created statically in a GUI builder,       */
/*		such as the available Operations or Mods for  */
/*		an individual Segment (basin).		      */
/*                                                            */
/**************************************************************/
#include <stdio.h>
#include <string.h>

#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

#include "dhm_mods.h"

extern void setModInterfaceElements(char *, Mods_everythingStruct *);
extern void initializeModDates(Mods_everythingStruct *data);
extern void update_modNotSavedDialog(Mods_everythingStruct *);
extern void removeValidInterfaceCallbacks(Mods_everythingStruct *);
extern void popup_dhm_mods_gui(Mods_everythingStruct *data, char []);

void modMenuItemSelectionCB(Widget w, Mods_everythingStruct *data, XtPointer callData)
{
        XmString     xmStringName;
        char         *name;
	char         modsString[MAX_MOD_STRING_LENGTH]="";
	int i=0;
        char  startDate[DATE_STRING_LENGTH];
        char endDate[DATE_STRING_LENGTH];
        char validDate[DATE_STRING_LENGTH];  
	char sacmodString[MAX_MOD_STRING_LENGTH] = "";
	char precipmodString[MAX_MOD_STRING_LENGTH] = "";
	char    curOpName[9] = "";    
                
        XtVaGetValues(w, XmNlabelString, &xmStringName, NULL);
        XmStringGetLtoR(xmStringName, XmFONTLIST_DEFAULT_TAG, &name);
		
        update_modNotSavedDialog(data);
        removeValidInterfaceCallbacks(data);
        
        strcpy(data->previousModName, data->ModName);
        memset(data->ModName, '\0', 20);
        strcpy(data->ModName, name);

	/* MR 687 begin  */
	/* Initialize date to the original set date when select different run mods*/
	initializeModDates(data);
        /* MR 687 end   */
        if(data->currentModSaved == FALSE && data->currentModChanged == TRUE)
	{
           XtManageChild(data->dialogStruct->modNotSavedMessageBox);
	}
        else
        {
           setModInterfaceElements(name, data);	
	   XmStringFree(xmStringName);
	   XtFree(name);
        }
	
	strncpy(curOpName,data->ModSettings->operation.name[0],8);	
	
        //popup_dhm_mods_gui(data,curOpName);
       

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_modMenuItemSelectionCB.c,v $";
 static char rcs_id2[] = "$Id: Mods_modMenuItemSelectionCB.c,v 1.2 2000/12/19 16:33:53 jgofus Exp $";}
/*  ===================================================  */

}



