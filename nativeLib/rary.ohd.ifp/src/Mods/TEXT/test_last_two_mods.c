/* *******************************************************************************************

	get_moddata.c

		coded by Tom Adams      NWS/Hydrologic Research Laboratory      10/30/90
		last modified:                                                  05/02/91


		This file contains functions, callbacks & otherwise, to
		get data from the mods_popup window (Run-time Modifications)
		and put the data into the appropriate mods data structure, used
		to generate ASCII data files in FORTRAN card image format for
		the coupled NWSRFS ver. 5 FORTRAN program.

		( see: "mods_info.h" - formats A1, A2, B1, B2, or B3)

   ******************************************************************************************* */

/*
#ifndef IFP_INCLUDES
#define IFP_INCLUDES
*/

/* lines between the ++++++++++ added by gfs 031195 */
/* ++++++++++++++++++ */

/*#include "Mods_info.h"*/
#include "libXs.h"
#include "Mods_globalDefs.h"
#include "Mods_everythingStruct.h"


/* ++++++++++++++++++ */
   
/*#include "libXifp.h"*/
/*#include "ctype.h"*/
/*#include <X11/Xutil.h>*/
/*#include <X11/keysym.h>*/
/*#include "libXs.h"*/
/*#include "mods.h"*/
/*#include "mod_struct.h"*/
/*#include "ifp_atoms.h"*/

/*#define  ChooseTStypestruct*/
/*#include "mods_info.h"*/
/*#include "ifp_globals.h"*/

/*  #endif  */

void save_A1();
void save_A2();
void save_B1();
void save_B2();
void save_B3();




/* *************************************************************************************

	test_last_two_mods()
		checks to see if the last two Mods_array structures are identical;
		returns:
				TRUE    if they are identical,
				FALSE   otherwise.

   ************************************************************************************* */

int test_last_two_mods(data)
   
   Mods_everythingStruct *data;
{

	int             last;           /* index for the Last mod saved...              */
	int             previous;       /* index for the mod saved prior to last...     */
	int             theSame = FALSE;
	int             j;
	ModInfo         *last_Mod_array, *previous_Mod_array;

 /* currentModIndex has not been set yet so set it here - dp 5 May 95 */
 currentModIndex = data->ModIndex;

 /* check if there are at least 2 mods to compare */
 if(currentModIndex < 2)
    return(FALSE);

 last = currentModIndex - 1;
 previous = currentModIndex - 2;

 last_Mod_array = (ModInfo *)malloc(sizeof(ModInfo));
 *last_Mod_array = *data->ModArray[last];

 /* memset(last_Mod_array, '\0', sizeof(ModInfo)); */

 previous_Mod_array = (ModInfo *)malloc(sizeof(ModInfo));


 for(j = previous; j >= 0; j--)
	{
	*previous_Mod_array = *data->ModArray[j];

	if(last_Mod_array->type == previous_Mod_array->type)
		{/* The Mod_array types are the same...                                */
		switch(last_Mod_array->type)
			{
			case Mod_format_A1:
				theSame = compare_A1_modArrays(last_Mod_array, previous_Mod_array);
				break;

			case Mod_format_A2:
				theSame = compare_A2_modArrays(last_Mod_array, previous_Mod_array);
				break;

			case Mod_format_B1:
				theSame = compare_B1_modArrays(last_Mod_array, previous_Mod_array);
				break;

			case Mod_format_B2:
				theSame = compare_B2_modArrays(last_Mod_array, previous_Mod_array);
				break;

			case Mod_format_B3:
				theSame = compare_B3_modArrays(last_Mod_array, previous_Mod_array);
				break;

			}
		}
	if(theSame) return(theSame);    /* 'theSame' ==> 'TRUE', ie, the Mods are identical...          */
	}


return(FALSE);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/test_last_two_mods.c,v $";
 static char rcs_id2[] = "$Id: test_last_two_mods.c,v 1.1 1995/11/14 12:19:55 page Exp $";}
/*  ===================================================  */

}


