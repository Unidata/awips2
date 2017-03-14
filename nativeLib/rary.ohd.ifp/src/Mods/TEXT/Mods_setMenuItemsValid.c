/****************************************************************/
/*                                                              */
/*	FILE:		Mods_setModMenuItemsValid.c	        */
/*                                                              */
/*							        */
/*                                                              */
/*	Coded by:	Tom Adams                               */
/*			NWS * Office of Hydrology * HRL         */
/*	Date:		09/15/94                                */
/*	Modified:	09/26/94 - added 'modName' argument	*/
/*			to the function, making it applicable	*/
/*			throughout the code when different 	*/
/*			operatioons are selected		*/
/*                                                              */
/*      NOTE:	This function sets the flags in the	        */
/*		xs_menu_struct which show the Mods that	        */
/*		should appear in the Menu for the current       */
/*		segment and Unmanages the Widgets that do       */
/*		not correspond to operations in the current     */
/*		segment					        */
/*                                                              */
/****************************************************************/





#include "libXs.h"
#include "Mods_config.h"
#include "Mods_ofsData_struct.h"




extern int valid_mods_for_oper();
void setModMenuItemsValid(ofsData_struct *ofsData, pullDownMenu_struct *modsMenu, char *opName)
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
	valid_mods_for_oper(ofsData->p_float_array, ofsData->p_char_array, ofsData->ts_float_array,
		   ofsData->ts_char_array, modsMenu->menuItems, modsMenu->num, opName);


	for(i = 0; i < modsMenu->num; i++) 
	{
		if(modsMenu->menuItems[i].active == FALSE)
			XtUnmanageChild(modsMenu->widgetStruct->widget_array[i]->parent);
		else	XtManageChild(modsMenu->widgetStruct->widget_array[i]->parent);
	}



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_setMenuItemsValid.c,v $";
 static char rcs_id2[] = "$Id: Mods_setMenuItemsValid.c,v 1.1 1995/11/14 12:19:32 page Exp $";}
/*  ===================================================  */

}
