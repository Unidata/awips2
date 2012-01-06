/****************************************************************/
/*								*/
/*	FILE:		Mods_getModNames.c			*/
/*								*/
/*								*/
/*								*/
/*	Coded by:	Tom Adams (TEA)				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		09/14/94				*/
/*	Modified:						*/
/*                      					*/
/*								*/
/****************************************************************/



#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_config.h"
#include "Mods_menuStruct.h"




menuItemsNames_struct *getModNames()
{
	int	i;
	int	numMods;
	
	
	menuItemsNames_struct *data = XtNew(menuItemsNames_struct);
	

/*----------------------------------------------------------------------*/
/*	This code fragment is only TEMPORARY to get things working	*/
/*----------------------------------------------------------------------*/
	numMods    = XtNumber(ModStruct_array);
	data->num  = numMods;
	
	/* printf("Number of Mods read from Modstruct_array: %d\n", numMods); */
	
	data->name = (char **)XtMalloc(sizeof(char *)*numMods);
	for(i = 0; i < numMods; i++) 
	{
		data->name[i] = (char *)XtMalloc(sizeof(char)*(strlen(ModStruct_array[i].name)+1));
				
		strcpy(data->name[i] , ModStruct_array[i].name);		
       
	}
/*----------------------------------------------------------------------*/
										
	return(data);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/ifp/src/Mods/RCS/Mods_getModNames.c,v $";
 static char rcs_id2[] = "$Id: Mods_getModNames.c,v 1.2 2007/05/16 16:25:41 aivo Exp $";}
/*  ===================================================  */

}


