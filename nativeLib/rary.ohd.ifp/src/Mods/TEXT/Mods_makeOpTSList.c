/****************************************************************/
/*                                                              */
/*	FILE:		Mods_makeOpTSList.c			*/
/*                                                              */
/*							        */
/*                                                              */
/*	Coded by:	Tom Adams                               */
/*			NWS * Office of Hydrology * HRL         */
/*	Date:		11/09/94                                */
/*                                                              */
/*      NOTE:							*/
/*								*/
/*                                                              */
/****************************************************************/



#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "Mods_opTSDataStruct.h"


OpTSTypeStruct_p makeOpTSList(mod_data *, int);


/************************************************************************/
/*									*/
/*	void resetOpTSList()						*/
/*									*/
/*									*/
/************************************************************************/

void resetOpTSList(Mods_everythingStruct *data, mod_data *modsData)
{

	int		i, num;
	XmString	*xmStringList;


	XmListDeleteAllItems(data->widgetData->timeSeriesList);

	data->opTSdata = makeOpTSList(modsData, data->selectedModDef->OpTSType);

	XtVaSetValues(data->widgetData->timeSeriesList, 
			XmNitems,		data->opTSdata->xmStringList,
			XmNitemCount,		data->opTSdata->num,
			XmNvisibleItemCount,	5,
			NULL);
			
	XtVaGetValues(data->widgetData->timeSeriesList, XmNitemCount, &num, NULL);
	for(i = 0; i < num; i++) XmStringFree(data->opTSdata->xmStringList[i]);
}

									
/************************************************************************/
/*									*/
/*	OpTSTypeStruct_p makeOpTSList()					*/
/*									*/
/*									*/
/************************************************************************/

OpTSTypeStruct_p makeOpTSList(mod_data *modsData, int OpTSType)
{
	char		string[50], name[9], type[9];
	char		deltaTString[9];
	char		temp[9];
	char		*firstBlank;
	char		*blank = " ";
	int		j;
	int		k = 0;
	int		length;
	
	OpTSTypeStruct_p	data;



	data = XtNew(OpTSTypeStruct_t);
	
	while(strlen(modsData->operation.type[k]))
		{
		memset(string, '\0', 50);
		memset(deltaTString, '\0', 9);
		memset(type, '\0', 9);
		memset(name, '\0', 9);
		memset(temp, '\0', 9);
		strncpy(type, modsData->operation.type[k], 8);
		strncpy(name, modsData->operation.name[k], 8);

		firstBlank = strstr(type, blank);
		if(firstBlank != NULL) *firstBlank = '\0';

		firstBlank = strstr(name, blank);
		if(firstBlank != NULL) *firstBlank = '\0';

		if(OpTSType) {	/*------------------------------------------------------*/
				/* Test whether the Mod has the:			*/
				/*	(1) Operation - Type/Name FORMAT, or		*/
				/*	(2) Time-series - ID/Data Type/Delta-T FORMAT	*/
				/*							*/
				/* OpTSType == TRUE => (2) FORMAT			*/
				/*------------------------------------------------------*/
					
			strcpy(string, name);
			strcat(string, "  ");
			
			if(modsData->time_series.delta_t[k] <= 9) {
				strcpy(deltaTString, " ");
				sprintf(temp, "%d", modsData->time_series.delta_t[k]);
				strcat(deltaTString, temp);
				}
			else sprintf(deltaTString, "%d", modsData->time_series.delta_t[k]);
		
			strcat(string, type);
			length = strlen(type);
			if(length < 8) {
				for(j = 1; j <= 8 - length; j++) strcat(string, " ");
				}

			strcat(string, deltaTString);
			data->xmStringList[k] = XmStringCreateSimple(string);
						}
		else    {
			strcpy(string, type);
			strcat(string, "  ");
			strcat(string, name);
			data->xmStringList[k] = XmStringCreateSimple(string);
			}
		k++;
		}
		
	data->num = k;
	
	return(data);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_makeOpTSList.c,v $";
 static char rcs_id2[] = "$Id: Mods_makeOpTSList.c,v 1.1 1995/11/14 12:19:24 page Exp $";}
/*  ===================================================  */

}



