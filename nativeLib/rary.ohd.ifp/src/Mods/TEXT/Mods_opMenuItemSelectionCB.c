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
/*      NOTE:	This file contains functions that complete    */
/*		the interface setup - these include GUI	      */
/*		elements that change dynamically and can not  */
/*		be created statically in a GUI builder,       */
/*		such as the available Operations or Mods for  */
/*		an individual Segment (basin).		      */
/*                                                            */
/**************************************************************/



#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"


extern char *setModMenuDefaultSelection(char *, ifp_modsShell_p , pullDownMenu_struct *);
extern void setModMenuItemsValid(ofsData_struct *, pullDownMenu_struct *, char*);
extern void setModInterfaceElements(char *, Mods_everythingStruct *);

void opMenuItemSelectionCB(Widget w, Mods_everythingStruct *data, XmPushButtonCallbackStruct *callData)
{
        XmString     xmStringName;
        char         *name;
        char         *defaultModName;
        
        
        
        
        XtVaGetValues(w, XmNlabelString, &xmStringName, NULL);
        XmStringGetLtoR(xmStringName, XmFONTLIST_DEFAULT_TAG, &name);
        
	/* printf("Inside 'opMenuItemSelectionCB()' CALLBACK function for %s\n", name); */
	
	setModMenuItemsValid(data->ofsData, data->modsMenu, name);
	
	/* Use NULL now, but this will be the	*/
	/* menu item name to select by user	*/
	/* preference...			*/
	defaultModName = setModMenuDefaultSelection(NULL, data->widgetData, data->modsMenu);

	setModInterfaceElements(defaultModName, data);	
	
        XmStringFree(xmStringName);
	XtFree(name);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_opMenuItemSelectionCB.c,v $";
 static char rcs_id2[] = "$Id: Mods_opMenuItemSelectionCB.c,v 1.1 1995/11/14 12:19:28 page Exp $";}
/*  ===================================================  */

}



