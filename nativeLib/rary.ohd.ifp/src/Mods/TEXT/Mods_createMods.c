/****************************************************************/
/*								*/
/*	FILE:		Mods_createMods.c			*/
/*								*/
/*	Functions for creating and testing Mods			*/
/*								*/
/*	Coded by:	Tom Adams				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		11/22/94				*/
/*	Modified:	D. Page - 11/16/95			*/
/*								*/
/****************************************************************/



#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "ifp_atoms.h"


#define	A1_TYPE		1
#define	A2_TYPE		2
#define	B1_TYPE		3
#define	B2_TYPE		4
#define	B3_TYPE		5



extern void	save_A1(Mods_everythingStruct *, char *, date *, date *, date *);
extern void	save_A2(Mods_everythingStruct *, char *, date *, date *, date *);
extern void	save_B1(Mods_everythingStruct *, char *, date *, date *, date *, int);
extern void	save_B2(Mods_everythingStruct *, char *, date *, date *, date *, int);
extern void	save_B3(Mods_everythingStruct *, char *, date *, date *, date *, int);
extern int	test_last_two_mods(Mods_everythingStruct *);
extern void     getModStartDate(Mods_everythingStruct *, date *);
extern void     getModEndDate(Mods_everythingStruct *, date *);
extern void     getModValidDate(Mods_everythingStruct *, date *);
extern char     *get_fgroup_name();
extern int GetMenuPos(Widget);

void createMod(Mods_everythingStruct *data)
{

	date	*startDate;
	date	*endDate;
	date	*validDate;
        int     modfgrp;
        char    fgrpseg_name[20];
	char    *tmp_char;
        Widget  w;

        startDate = (date *)malloc(sizeof(date));
        endDate   = (date *)malloc(sizeof(date));
        validDate = (date *)malloc(sizeof(date));

	if(data->selectedModDef->modStartDate) 
	   getModStartDate(data, startDate);
	else startDate = NULL;

	if(data->selectedModDef->modEndDate)
	   getModEndDate(data, endDate);
	else endDate = NULL;

	if(data->selectedModDef->modValidDate) 
	   getModValidDate(data, validDate);
	else validDate = NULL;

	modfgrp = GetMenuPos(data->widgetData->optionsApplyMenu);
        tmp_char = get_fgroup_name();
     
	strcpy(fgrpseg_name, data->SegmentName);
	if (data->selectedModDef->modFGroup == SEGMENT )
	    strcpy(fgrpseg_name, data->SegmentName);
	else 
	    {
	    if (modfgrp == FGROUP)
	    /* if it is Mods for Forecast group, use FG name*/
            strcpy(fgrpseg_name, tmp_char);
            }
	
	switch(data->selectedModDef->type) {
	
		case A1_TYPE:
			    save_A1(data,fgrpseg_name , startDate, endDate, validDate);
			break;
			
		case A2_TYPE:
			    save_A2(data,fgrpseg_name , startDate, endDate, validDate);
			break;
			
		case B1_TYPE:
			    save_B1(data, fgrpseg_name, startDate, endDate, validDate, modfgrp);
			break;
			
		case B2_TYPE:
			    save_B2(data, fgrpseg_name, startDate, endDate, validDate, modfgrp);
			break;
			
		case B3_TYPE:
			    save_B3(data, fgrpseg_name, startDate, endDate, validDate, modfgrp);
			break;
	
		default:
			printf("ERROR: Mod type was not identified and can't be saved!\n");
			break;
		}

        free(startDate);
        free(endDate);
        free(validDate);

}



int modIsUnique(Mods_everythingStruct *data)
{

	/*--------------------------------------------------------------*/
	/* We need to create the Mod to test it against the previous	*/
	/* one to see if it's the same; if it is, we get rid of it &	*/
	/* reset the ModIndex, etc.					*/
	/*--------------------------------------------------------------*/
	createMod(data);

	if(data->ModIndex >= 2) /* We don't need to reference a Mod_array 
	                           if there's only one mod (index=1)    */
	{
		/* If the last two mods are identical, discard the most 
		 * recent one & decrement current ModIndex
		 * Returns FALSE so a warning popup will be displayed.
		 */
		if(test_last_two_mods(data))
		{
			XtFree((char *)data->ModArray[data->ModIndex - 1]);
			data->ModArray[data->ModIndex - 1] = NULL;
			data->ModIndex--;
			return(FALSE);
		}
	}
		
	return(TRUE);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_createMods.c,v $";
 static char rcs_id2[] = "$Id: Mods_createMods.c,v 1.7 2006/04/18 13:34:01 aivo Exp $";}
/*  ===================================================  */

}


