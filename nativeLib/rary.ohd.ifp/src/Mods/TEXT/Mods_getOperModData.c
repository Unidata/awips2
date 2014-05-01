/****************************************************************/
/*								*/
/*	FILE:		Mods_getOperModData.c			*/
/*								*/
/*	Read oper mods file & create data structs		*/
/*								*/
/*	Coded by:	George Smith				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		11/20/94				*/
/*								*/
/*      NOTE:							*/
/*								*/
/****************************************************************/

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include "Mods_operMods_struct.h"


extern char	*makeFilePath(char *, char *);

static void	readOpersPerModFile(FILE *, operMod_struct *);
extern void	readOpersPerModRecord(FILE *,
                      	 	      mod_oper_struct *);

#define LINE_LENGTH	120


/****************************************************************/
/*								*/
/*	operMod_struct *getOperModData()			*/
/*								*/
/*								*/
/*	NOTE:	An oper Mod file is read that specifies which	*/
/*		operations apply to each mod.			*/
/*								*/
/****************************************************************/

operMod_struct *getOperModData(char  *SettingsPath)
{
	int			i;
	int			num;
	char			*fileName = "OpersPerMod";
	char			*filePath;
	FILE			*file_ptr;
	operMod_struct		*data;


/* printf("Inside 'getOperModData()'...\n"); */

	filePath = makeFilePath(SettingsPath, fileName);

	if((file_ptr = fopen(filePath,"r")) != NULL)
          {
            data = XtNew(operMod_struct);
	    readOpersPerModFile(file_ptr, data);
	    fclose(file_ptr);
	    XtFree(filePath);	  /* Clean-up	*/
	    return(data);  /* Have successfully read file   */
		           /* and loaded structure.         */
	  }
	else return(NULL);	/* We return NULL to notify that an error has	*/
				/* occurred opening the OpersPerMod file	*/
}

static void readOpersPerModFile(FILE *fp, operMod_struct *data)
{

	int	i, j;
	int	return_val;
	char	*nextline;
	char	line[LINE_LENGTH];
	
	
/* printf("Inside 'readOpersPerModFile()'...\n"); */
	
	/*--------------------------------------------------------------*/
	/* Cycle through each line in the file, processing each field	*/
	/* as we go using 'sscanf()'...					*/
	/*								*/
	/*--------------------------------------------------------------*/
	/*								*/
	/* Read first line of file - contains number of mod records	*/
	/*  to follow.							*/
	/*								*/
	/*--------------------------------------------------------------*/
	
	if((nextline = fgets(line, LINE_LENGTH, fp)) != NULL) 
	{
		if((return_val = sscanf(line, "%d",	
			&(data->number_of_mods) )) != EOF)
		   ;	
		else 
		{
		   printf("FAILED: return value is %d...\n", return_val);
		   return;
		}
	}
		
	for(i = 0; i < data->number_of_mods; i++)
	{
	    data->mod_oper_info[i] = XtNew(mod_oper_struct);
	    
	    if((nextline = fgets(line, LINE_LENGTH, fp)) != NULL)
	    {
	       if((return_val = sscanf(line, "%d%s%d",	
			&(data->mod_numbers[i]),
			data->mod_names[i],
			&(data->mod_oper_info[i]->number_of_opers))
			) != EOF)
	          ;	
	       else 
	       {
		   printf("FAILED: return value is %d...\n", return_val);
		   data->mod_oper_info[i]->number_of_opers = 0;
	       }
		  
	       if(data->mod_oper_info[i]->number_of_opers > 0)
	       {
	          readOpersPerModRecord(fp, 
	          			data->mod_oper_info[i]);
	       }
	    }
	 }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_getOperModData.c,v $";
 static char rcs_id2[] = "$Id: Mods_getOperModData.c,v 1.2 1996/03/21 16:59:46 page Exp $";}
/*  ===================================================  */

}
