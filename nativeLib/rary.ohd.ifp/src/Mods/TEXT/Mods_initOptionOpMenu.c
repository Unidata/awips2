/****************************************************************/
/*                                                              */
/*	FILE:		Mods_initOptionsOpMenu.c		                */
/*                                                              */
/*							                                    */
/*                                                              */
/*	Coded by:	Tom Adams                                       */
/*			NWS * Office of Hydrology * HRL                     */
/*	Date:		11/09/94                                        */
/*      Modifications:  D. Page                                 */
/*                      05/11/95                                */
/*                                                              */
/*      NOTE:							                        */
/*								                                */
/*                                                              */
/****************************************************************/



#include <stdio.h>
#include "libXs.h"
#include "Mods_everythingStruct.h"


#define		LINE_LENGTH	100


extern int			countLines(FILE *);
extern char			*makeFilePath(char *, char *);
extern pullDownMenu_struct	*create_PulldownChildren(Widget, menuItemsNames_struct *, char *, void *, caddr_t);
extern void			modOptionsMenuItemSelectionCB(Widget, Mods_everythingStruct *, XtPointer);

static int			readOptionsFile(char *, char *, OptionsOpMenuStruct_p, menuItemsNames_struct *);
static void			initModOptionsDataStruct(Mods_everythingStruct *);
static menuItemsNames_struct	*initNamesDataStruct(OptionsOpMenuStruct_p);



/****************************************************************/
/*                                                              */
/*	void initializeOptionsOpMenu()				*/
/*                                                              */
/*	RETURNS:	NONE					*/
/*                                                              */
/*	NOTE:							*/
/*                                                              */
/****************************************************************/

void initializeOptionsOpMenu(char *modName, Mods_everythingStruct *data)
{

	int	i;
	menuItemsNames_struct	*namesStruct;
 	
	/*----------------------------------------------*/
	/* Initialize the Mod Option data structure	*/
	/*----------------------------------------------*/
	initModOptionsDataStruct(data);
	
	/*----------------------------------------------*/
	/* Make space for a 'menuItemsNames_struct'	*/
	/* structure.                                   */
	/*----------------------------------------------*/
	namesStruct = initNamesDataStruct(data->Options);
	
	/*----------------------------------------------*/
	/* Read the Options data file for the selected	*/
	/* Mod; FALSE is returned if there is a problem	*/
	/* reading the file...				*/
	/* Create space and fill the OptionsOpMenuStruct*/
	/* and the namesStruct - dp 051195              */
	/*----------------------------------------------*/
	if(!readOptionsFile(modName, data->ModSettingsPath, 
	   data->Options, namesStruct)) return;
		
	data->Options->menuData = create_PulldownChildren(data->widgetData->optionsMenuPulldown, namesStruct, NULL,
								modOptionsMenuItemSelectionCB, (caddr_t)data);
	   	   				
	XtVaSetValues(data->widgetData->optionsMenu, XmNmenuHistory,
			data->Options->menuData->widgetStruct->widget_array[0]->parent, NULL);
			
	XtFree((char *)namesStruct->name);
	XtFree((char *)namesStruct);

}



static int readOptionsFile(char *modName, char *path, OptionsOpMenuStruct_p data,
                           menuItemsNames_struct *labels)
{	
	
	int	i = 0;
	int	number;			/* Number of lines read...		 */
	int     label_length   = 20;    /* String length allowed for menu labels */
	int	keyword_length = 9;	/* String length allowed for Keywords	 */
	char	*name;
	char	*nextline;
	char	line[LINE_LENGTH];
	char	*dir = "ModOptions/";
	char	*fileString;
	char	*filePath;
	FILE	*fp;
	


	/* printf("Inside 'readOptionsFile()'.../n"); */

	
	fileString = (char *)XtMalloc(sizeof(char)*(strlen(modName)+strlen(dir)+1));
	strcpy(fileString, dir);
	strcat(fileString, modName);
	
							
	/*----------------------------------------------*/
	/* Create the path name to the Options file for	*/
	/* the selected Mod; attempt to open the file -	*/
	/* if we can't, return NULL so the calling	*/
	/* function can handle things gacefully;	*/
	/* otherwise proceed...				*/
	/*----------------------------------------------*/
	filePath = makeFilePath(path, fileString);	
	fp = fopen(filePath, "r");
	if(fp == NULL) {
		printf("Could not open file: %s\n", filePath);
		XtFree(fileString);
		XtFree(filePath);
		return(FALSE);
		}
	
	number = countLines(fp);	/* Count the number of lines to read	*/
	fclose(fp);

	data->num = number;
	labels->num = number;
	/*--------------------------------------------------------------*/
	/* Create arrays of size 'number' for holding Keywords & their	*/
	/* values...                                                    */
	/* Also create arrays for holding labels - dp - 051195          */
	/*--------------------------------------------------------------*/
	labels->name    = (char **)XtMalloc(sizeof(char *)*number);
	data->keyword	= (char **)XtMalloc(sizeof(char *)*number);
	data->value	= (int *)XtMalloc(sizeof(int)*number);
	
	fp = fopen(filePath, "r");
	while((nextline = fgets(line, LINE_LENGTH, fp)) != NULL) {

                labels->name[i]  = (char *)XtMalloc(sizeof(char)*label_length);
                data->keyword[i] = (char *)XtMalloc(sizeof(char)*keyword_length);
		
		if((number = sscanf(line, "%s%s%d",
			labels->name[i], data->keyword[i], 
			&data->value[i])) == 3) 
                   ;
		else printf("FAILED: %d values read...\n", number);
	
		i++;	
		}
		
	XtFree(filePath);

	return(TRUE);
}



static void initModOptionsDataStruct(Mods_everythingStruct *data)
{

	int	i;
	

	if(data->Options != NULL) {

	/*------------------------------------------------------*/
	/* If we get here, then we want to free the space	*/
	/* pointed to by 'Options', then reinitialize it...	*/
	/*							*/
	/* ALSO: It looks like we need to completely destroy	*/
	/*       the PulldownMenu itself & it's children before	*/
	/*       we can reinitialize it for the next Mod with	*/
	/*       Options to set...				*/
	/*------------------------------------------------------*/

		XtDestroyWidget(data->widgetData->optionsMenuPulldown);
			
		data->widgetData->optionsMenuPulldown =
			(Widget)XmCreatePulldownMenu(data->widgetData->optionsMenu, "optionsMenuPulldown", NULL, 0);

		XtVaSetValues(data->widgetData->optionsOptionButton,
			XmNsubMenuId, data->widgetData->optionsMenuPulldown, NULL);

	
		XtFree((char *)data->Options->menuData->widgetStruct);
		XtFree((char *)data->Options->menuData->menuItems);
		XtFree((char *)data->Options->menuData);
	
		XtFree((char *)data->Options->keyword);
		XtFree((char *)data->Options->value);
		XtFree((char *)data->Options);
		}

	data->Options = XtNew(OptionsOpMenuStruct_t);

}	


static menuItemsNames_struct *initNamesDataStruct(OptionsOpMenuStruct_p data)
{

 	menuItemsNames_struct	*namesStruct;


	namesStruct = (menuItemsNames_struct *)XtMalloc(sizeof(menuItemsNames_struct));

	return(namesStruct);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_initOptionOpMenu.c,v $";
 static char rcs_id2[] = "$Id: Mods_initOptionOpMenu.c,v 1.2 2006/04/18 15:27:37 aivo Exp $";}
/*  ===================================================  */

}

