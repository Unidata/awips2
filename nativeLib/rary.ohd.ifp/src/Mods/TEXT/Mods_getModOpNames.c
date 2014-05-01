/* *******************************************************************

	Mods_getModOpNames.c

	Coded by:       Tom Adams
	Affiliation:    NWS/Office of Hydrology/Hydrologic Research Laboratory
	Date:           09/12/94
	Modified:       09/12/94
                        09/29/94 - changed to return a list of operations
                         to which mods can be applied - gfs - hrl


   ******************************************************************* */




#include "libXs.h"
#include "Mods_config.h"
#include "Mods_operationTypes.h"
#include "Mods_allowedOperTypes.h"
#include "Mods_flags.h"





/* ********************************************************************

	getModOpNames()
	
		Look through the t_array and get the operations names
		for the current segment.

                Modified to return only the operation names to which 
                mods can be applied - gfs - hrl - 24 Sept 1994.
		
		RETURN: pointer to a struct of type 'menuItemsNames_struct'
			- see Mods_config.h


   ******************************************************************** */

menuItemsNames_struct *getModOpNames(char **p_char, int *t_array)
{

	int	i;
	int	j;
	int	k;				
	int	match;				
	int	loct, operation_number;
	int	number_of_operations;

	char	**opTypes;
 	char	list_names[MAX_OPERATIONS][10];

	menuItemsNames_struct	*data;

		
	for(i = 0; i < MAX_OPERATIONS; i++) memset(list_names[i], '\0', 10);

	i = 0;
	loct = 1;

	while ((operation_number = t_array[loct - 1]) != -1 && i < MAX_OPERATIONS)
		{		
                for(k = 0; k < MAX_ALLOWED_MOD_OPERATIONS; k++)
                   { /* only include operation type if in allowed list */
                    if(strcmp(allowed_mod_operations[k], 
                              operationTypes[operation_number - 1]) == 0)
                      {
		       strcpy(list_names[i++], operationTypes[operation_number - 1]);
                       break;
                      }
                   }
		loct = t_array[loct-1 + 1];

		}

	number_of_operations = i;

	data       = (menuItemsNames_struct *) XtMalloc(sizeof(menuItemsNames_struct));
	data->name = (char **) XtMalloc(sizeof(char *) * number_of_operations);
	
	/* We want to return an array the size we need and not one with empty	*/
	/* space, so we'll malloc space & point to it... Also, we want only	*/
	/* Operation names that are unique, i.e., not duplicates - so, get      */
	/* only the operation names that are not repeated...                    */
	
	j = 0;
	opTypes = (char **) XtMalloc(number_of_operations * sizeof(char *));
	for(i = 0; i < number_of_operations; i++) {
	        if(i > 0) {
	                match = FALSE;
	                for(k = 0; k <= j; k++) { /* Check if we have a match; if we do, go to the */
	                                          /* next operation name...                        */
	                        if(strcmp(opTypes[k], list_names[i]) == 0) match = TRUE;
	                        }
	                        
	                if (match == FALSE) {
	                        j++;
		                opTypes[j] = (char *) XtMalloc(sizeof(char) * 10);
		                strcpy(opTypes[j], list_names[i]);
		                }
		        }
		else    {
		        opTypes[j] = (char *) XtMalloc(sizeof(char) * 10);
                        strcpy(opTypes[j], list_names[i]);
                        }
		}
	
	/*  do not increment number_of_operations if there are none 
	 *  dp - 16 Feb. 1996
	 */
	if (number_of_operations > 0)	
	   number_of_operations = j + 1;

	data->num = number_of_operations;
	data->name = opTypes;
	
	return(data);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_getModOpNames.c,v $";
 static char rcs_id2[] = "$Id: Mods_getModOpNames.c,v 1.5 2006/05/03 13:01:10 aivo Exp $";}
/*  ===================================================  */

}



