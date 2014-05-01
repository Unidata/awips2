/****************************************************************/
/*								*/
/*	FILE:		Mods_getModDefs.c			*/
/*								*/
/*	Read Mod Definitions file & create data structs		*/
/*								*/
/*	Coded by:	Tom Adams				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		09/08/94				*/
/*								*/
/*      NOTE:							*/
/*								*/
/****************************************************************/



#include <stdlib.h>
#include "Mods_globalDefs.h"
#include "Mods_interfaceDefStruct.h"



extern char	*makeFilePath(char *, char *);
extern int	countLines(FILE *);

static void	readModDefsFile(FILE *, Mod_interfaceDefStruct *);
static int	countNumberOfModTypes(FILE *);




/****************************************************************/
/*								*/
/*	Mod_interfaceDefStruct *createModInterfaceDefinitions()	*/
/*								*/
/*		Create the Mods Definition List for each Mod	*/
/*		type. the Mod Definition is used to identify	*/
/*		which user interface elements should be turned	*/
/*		ON/OFF (Managing/Unmanaging widgets) or dimmed	*/
/*		(setting widgets Insensitive)			*/
/*								*/
/*	NOTE:	A Mod Definitions file (fileName) is read which	*/
/*		specifies the interface settings		*/
/*								*/
/*	Ai Vo added field # 14 - SEGMENT, FGROUP, RANGE    	*/
/****************************************************************/
Mod_interfaceDefStruct *createModInterfaceDefinitions(char *SettingsPath)
{

	int			i;
	int			num;
	char			*fileName = "ModDefinitions";
	char			*filePath;
	FILE			*file_ptr;
	Mod_interfaceDefStruct	*data;



	filePath = makeFilePath(SettingsPath, fileName);
        /*DT
	printf("filePath %s\n",filePath);
        DT*/

	if((file_ptr = fopen(filePath,"r")) != NULL) {
		/*------------------------------------------------------*/
		/* Count the number of different kinds of Mods in the	*/
		/* Mods definitions file; make space for them; and	*/
		/* read the file (so, were actually reading the file	*/
		/* twice)...						*/
		/*------------------------------------------------------*/
		
		data        = XtNew(Mod_interfaceDefStruct);
		data->num   = countNumberOfModTypes(file_ptr);
		data->array = (Mod_defStruct **)XtMalloc(sizeof(Mod_defStruct *)*data->num);
	
		for(i = 0; i < data->num; i++)
			data->array[i] = (Mod_defStruct *)XtMalloc(sizeof(Mod_defStruct));
	
		fclose(file_ptr);
		
		if((file_ptr = fopen(filePath,"r")) != NULL) {
			readModDefsFile(file_ptr, data);
			fclose(file_ptr);
			XtFree(filePath);		/* Clean-up	*/
			return(data);
			}
		else return(NULL);
		
		}
	else return(NULL);	/* We return NULL to notify that an error has	*/
				/* occurred opening the ModsDefinitions file	*/
	
}





static void readModDefsFile(FILE *fp, Mod_interfaceDefStruct *data)
{

	int	i = 0;
	int	j;
	int	field[15];	/* CHANGE THE ARRAY SIZE if the number of Mod	*/
				/* Definition Items we're tracking changes!!!	*/
	char	*name;
	char	*nextline;
	char	*temp;
	char	line[MOD_DEF_LENGTH];
	char	*PARSE_TOKEN = " \t";
	
	
	
	/*--------------------------------------------------------------*/
	/* Cycle through each line in the file, processing each field	*/
	/* as we go; the 1st field is a string, the remainder are ints	*/
	/*--------------------------------------------------------------*/
	while((nextline = fgets(line, MOD_DEF_LENGTH, fp)) != NULL) {
		name = strtok(line, PARSE_TOKEN);
		strcpy(data->array[i]->name, name);
				
		/*------------------------------------------------------*/
		/* Go through the remaining fields on the line, which	*/
		/* are all ints...					*/
		/*------------------------------------------------------*/
		j = 0;
		while((temp = strtok(NULL, PARSE_TOKEN)) != NULL) {
			if(*temp == '\n') break;
			field[j++] = atoi(temp);
			}


		data->array[i]->type		= field[0];
		data->array[i]->value		= field[1];
		data->array[i]->arrowButtons	= field[2];
		data->array[i]->modStartDate	= field[3];
		data->array[i]->modEndDate	= field[4];
		data->array[i]->modValidDate	= field[5];
		data->array[i]->OpTimeSeries	= field[6];
		data->array[i]->TSDates		= field[7];
		data->array[i]->entryDialog	= field[8];
		data->array[i]->modOption	= field[9];
		data->array[i]->OpTSType	= field[10];
		data->array[i]->DataWinType	= field[11];
		data->array[i]->OpTSSelectType	= field[12];
		data->array[i]->modStartDateOffset = field[13];
		data->array[i]->modFGroup       = field[14];
                data->array[i]->UHGList         = field[15];
	
/* -------------------------------------------------------------------
		printf("%s %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",	
			data->array[i]->name,
			data->array[i]->type,
			data->array[i]->value,
			data->array[i]->arrowButtons,
			data->array[i]->modStartDate,
			data->array[i]->modEndDate,
			data->array[i]->modValidDate,
			data->array[i]->OpTimeSeries,
			data->array[i]->TSDates,
			data->array[i]->entryDialog,
			data->array[i]->modOption,
			data->array[i]->OpTSType,
			data->array[i]->DataWinType,
			data->array[i]->OpTSSelectType,
			data->array[i]->modStartDateOffset,
			data->array[i]->modFGroup);
                        data->array[i]->UHGList;
 -------------------------------------------------------------------- */		
		i++;	
		}
		
}



static int countNumberOfModTypes(FILE *fp)
{

	int	num = 0;


	num = countLines(fp);	
	/* printf("%d different Mod types recognized...\n", num); */

	return(num);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_getModDefs.c,v $";
 static char rcs_id2[] = "$Id: Mods_getModDefs.c,v 1.4 2004/08/05 17:38:36 wkwock Exp $";}
/*  ===================================================  */

}

