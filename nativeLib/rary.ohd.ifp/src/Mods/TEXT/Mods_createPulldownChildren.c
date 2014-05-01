/****************************************************************/
/*								*/
/*	FILE:	Mods_createPulldownChildren.c			*/
/*								*/
/*	Run-time Modifications Interface creation module	*/
/*								*/
/*	Coded by:	Tom Adams				*/
/*			NWS * Office of Hydrology * HRL		*/
/*								*/
/*	Date:		09/12/94				*/
/*      NOTE:	This file should only be altered using		*/
/*      	XDesigner, otherwise the programmer will	*/
/*		break the layered code design which separates	*/
/*		the GUI created with a GUI-builder		*/
/*		(XDesigner) from the dialog, callback, and	*/
/*		application (C-only code) layers.		*/
/*								*/
/****************************************************************/



#include "Mods_globalDefs.h"
#include "libXs.h"

#include "Mods_widgetStruct.h"
#include "Mods_config.h"


extern pullDownMenu_struct *initializeMenuStruct(menuItemsNames_struct *, char *, void *, caddr_t);

extern xs_menu_widget_struct *xs_create_menu_buttons(char *,Widget,xs_menu_struct*,int);
pullDownMenu_struct *create_PulldownChildren(Widget parent, menuItemsNames_struct *data, 
						char *lastName, void (*callback_func) (), caddr_t callData)
{
	
	pullDownMenu_struct	*menuData;
	
	
	/* Make space for 'menuData' and fill it with everything but 'widgetStruct' -	*/
	/* to keep the structure initialization separate from Widget creation		*/
		 
	menuData = initializeMenuStruct(data, lastName, callback_func, callData);
	
	/* Create the menu and return the Widget structure				*/
	menuData->widgetStruct =
	         (xs_menu_widget_struct *) xs_create_menu_buttons("", parent, menuData->menuItems, menuData->num);
	
	return(menuData);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_createPulldownChildren.c,v $";
 static char rcs_id2[] = "$Id: Mods_createPulldownChildren.c,v 1.1 1995/11/14 12:19:10 page Exp $";}
/*  ===================================================  */

}




