/****************************************************************/
/*                                                              */
/*	FILE:		Mods_setUserPreferences.c	        */
/*                                                              */
/*							        */
/*                                                              */
/*	Coded by:	Tom Adams                               */
/*			NWS * Office of Hydrology * HRL         */
/*	Date:		10/04/94                                */
/*	Modified:						*/
/*                                                              */
/*      NOTE:	Sets the User's preferences from a User's	*/
/*		Preferences file if one exists; otherwise	*/
/*		one is created from fallback preferences	*/
/*                                                              */
/****************************************************************/




#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

extern char *setModMenuDefaultSelection(char *, ifp_modsShell_p , pullDownMenu_struct *);
void setUserPreferences(Mods_everythingStruct *data)
{

	char	*defaultMod;
	
	
	defaultMod = (char *)setModMenuDefaultSelection(NULL, data->widgetData, data->modsMenu);




/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_setUserPreferences.c,v $";
 static char rcs_id2[] = "$Id: Mods_setUserPreferences.c,v 1.2 2006/04/18 15:28:28 aivo Exp $";}
/*  ===================================================  */

}

