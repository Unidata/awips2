/****************************************************************/
/*                                                              */
/*	FILE:		Mods_setModMenuDefaultSelection.c	*/
/*                                                              */
/*							        */
/*                                                              */
/*	Coded by:	Tom Adams                               */
/*			NWS * Office of Hydrology * HRL         */
/*	Date:		09/26/94                                */
/*	Modified:						*/
/*                                                              */
/*      NOTE:	Sets the default Mods menu selection to the	*/
/*		first item in the list				*/
/*                                                              */
/****************************************************************/





#include "libXs.h"
#include "Mods_widgetStruct.h"
#include "Mods_config.h"
#include "Mods_ofsData_struct.h"




/*--------------------------------------------------------------*/
/*	void setModMenuDefaultSelection()			*/
/*								*/
/*	This function TEMPORARILY sets the Mods OptionMenu	*/
/*	selection to the first valid item in the menu; in the	*/
/*	future it will set the selection to 'name' which will	*/
/*	be read from the User's Preferences file and will be	*/
/*	passed as a function argument...			*/
/*								*/
/*	RETURN:	Name of the default Mod				*/
/*								*/
/*--------------------------------------------------------------*/
char *setModMenuDefaultSelection(char *name, ifp_modsShell_p widgetData, pullDownMenu_struct *modsMenu)
{

	int	i;


	/*------------------------------------------------------*/
	/* Get list of valid mods for this segment; flags are	*/
	/* set in the 'menuItems' struct array identifying	*/
	/* which Mods should appear in the menu list - if	*/
	/* 'opName' is 'ALL' Mods for all valid operations for	*/
	/* the current segment will appear in the menu,		*/
	/* otherwise only Mods for 'opName' will appear...	*/
	/*------------------------------------------------------*/


	for(i = 0; i < modsMenu->num; i++) {
		if(modsMenu->menuItems[i].active == TRUE)
			{	/* Set the default selection for the 'Mods' menu...	*/
			XtVaSetValues(widgetData->modOptionMenu, XmNmenuHistory,
					XtNameToWidget(widgetData->modPulldown, modsMenu->menuItems[i].name),
					NULL);
			break;
			}
		}

	return(modsMenu->menuItems[i].name);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_setModMenuDefaultSelection.c,v $";
 static char rcs_id2[] = "$Id: Mods_setModMenuDefaultSelection.c,v 1.1 1995/11/14 12:19:31 page Exp $";}
/*  ===================================================  */

}
