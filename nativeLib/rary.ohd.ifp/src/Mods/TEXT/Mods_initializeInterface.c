/**************************************************************/
/*                                                            */
/*	FILE:		Mods_initializeInterface.c            */
/*                                                            */
/*	Run-time Modifications Interface initialization	      */
/*	module						      */
/*                                                            */
/*	Coded by:	Tom Adams                             */
/*			NWS * Office of Hydrology * HRL       */
/*	Date:		09/08/94                              */
/*                                                            */
/*      Modified by:    D. Page - 29 Oct. 1995                */
/*                      added call to createModNotSavedDialog */
/*                      7 Nov. 1995 - added call to           */
/*                      createModTheSameDialog                */
/*                      11 Nov. 1995 - added call to          */
/*                      createModViewerNotSavedDialog         */
/*                                                            */
/*      NOTE:	This file contains functions that complete    */
/*		the interface setup - these include GUI	      */
/*		elements that change dynamically and can not  */
/*		be created statically in a GUI builder,       */
/*		such as the available Operations or Mods for  */
/*		an individual Segment (basin).		      */
/*                                                            */
/**************************************************************/




#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "ifp_atoms.h"



extern void			addWidgetCallbacks(Mods_everythingStruct *);
extern char			*getOptionMenuSelection(Widget);
extern void			setUserPreferences(Mods_everythingStruct *);
extern void			setModInterfaceElements(char *, Mods_everythingStruct *);
extern char			*makeFilePath(char *, char *);
extern Mod_interfaceDefStruct	*createModInterfaceDefinitions(char *);
extern Mod_limitsStruct		*getModValueLimits(char *, float *, char **);
extern void			initializeModDates(Mods_everythingStruct *);	
extern void			createValueChangeDialogs(Mods_everythingStruct *);
extern void                     createModNotSavedDialog(Mods_everythingStruct *);
extern void                     createModTheSameDialog(Mods_everythingStruct *);
extern void                     createModViewerNotSavedDialog(Mods_everythingStruct *);
extern void                     createModDateErrorDialog(Mods_everythingStruct *); /*Added by gzhou 11/26/2003*/
extern int			getUnits(Display *, Atom, Atom);
extern operMod_struct* getOperModData(char *);
static void 			setModsTitle(Widget, char *);
static void			initializeDialogs(Mods_everythingStruct *);
static unitsStruct_p		initializeUnitsStruct(Display *);
extern int UhgFlag;/*is UHGCDATE plot in display?*/



void initializeInterface(char *segmentName, Mods_everythingStruct *data)
{
 
 
 	char	*defaultModName;
 	

 	
 	/*--------------------------------------------------------------*/
 	/* Make the title for the Mod shell window show what the	*/
 	/* current segment (forecast point) is; put it in the universal */
 	/* -ly available data structure 'Mods_everythingStruct'...	*/
	/*--------------------------------------------------------------*/
 	setModsTitle(XtParent(data->widgetData->ifp_modsMessageBox), segmentName);
 	memset(data->SegmentName, '\0', 20);
 	strcpy(data->SegmentName, segmentName);

  	/*--------------------------------------------------------------*/
  	/* Create the structures that identify the appropriate GUI	*/
  	/* elements for each Mod; read Mods interface definition	*/
  	/* file... defines which GUI elements should be turned-on	*/
  	/* (implying, which Widgets are managed or sensitive when the	*/
  	/* Mod is selected)						*/
	/*--------------------------------------------------------------*/
	data->interfaceDefs = createModInterfaceDefinitions(data->ModSettingsPath);
	if(data->interfaceDefs == NULL) printf("Error opening Mod Definitions file...\n");

  	/*--------------------------------------------------------------*/
  	/* Certain Mods have upper and lower bounds for their values;	*/
  	/* so, create a data structure for holding the Mod Limits and	*/
  	/* read the Mod Limits file...					*/
 	/*--------------------------------------------------------------*/
	data->modLimits = getModValueLimits(data->ModSettingsPath,
	                                    data->ofsData->p_float_array,
	                                    data->ofsData->p_char_array);
	if(data->modLimits == NULL) printf("Error opening Mod Limits file...\n");
	
	/*--------------------------------------------------------------*/
  	/* Certain Operations apply to each Mod.  Read the OpersPerMod	*/
  	/* file and fill the operModData portion of the everything	*/
  	/* structure.							*/
 	/*--------------------------------------------------------------*/
	data->operModData = (operMod_struct *)getOperModData(data->ModSettingsPath);
	                                    
	if(data->operModData == NULL) printf("Error opening OpersPerMod file...\n");
	
  	/*--------------------------------------------------------------*/
  	/* Make space for a structure to hold Value Limits for checking	*/
  	/* and updating ScaleWidget value				*/
  	/*--------------------------------------------------------------*/
	data->scaleValueLimits = (modLimitsDef *)XtMalloc(sizeof(modLimitsDef));
	
	
     	/*--------------------------------------------------------------*/
     	/* Initialize Dates:						*/
     	/*								*/
     	/*	(1) START DATE						*/
     	/*	(2) END DATE						*/
     	/*	(3) VALID DATE						*/
      	/*								*/
	/*--------------------------------------------------------------*/
	initializeModDates(data);
	
	
	/*--------------------------------------------------------------*/
	/* Make space for the structure to hold the Units set in IFP	*/
	/* Map, Universal Techniques and get the settings...		*/		
	/*--------------------------------------------------------------*/
	data->units = initializeUnitsStruct(XtDisplay(data->widgetData->ifp_modsShell));
	
	 	
   	/*--------------------------------------------------------------*/
   	/* Set User preferences; the user's Preferences file is read,	*/
   	/* if none exists, one is created from default preferences	*/
 	/*--------------------------------------------------------------*/
 	setUserPreferences(data);
 	
 	
   	/*--------------------------------------------------------------*/
   	/* We need to get the current Mod name selected in the		*/
   	/* OptionMenu so we can turn-on the correct interface elements	*/
 	/*--------------------------------------------------------------*/
 	defaultModName = getOptionMenuSelection(data->widgetData->modOptionMenu);
 	
   	/*--------------------------------------------------------------*/
   	/*	Initialize:						*/
   	/*								*/
	/*	Information, Warning, and Error	Dialogs			*/
   	/*--------------------------------------------------------------*/
 	initializeDialogs(data);
 	
 	
   	/*--------------------------------------------------------------*/
   	/* Turn-on the appropriate interface elements for the current	*/
   	/* Mod selected which, in this case, is the first Mod in the	*/
   	/* list of available Mods for the default Operation selected	*/
 	/*--------------------------------------------------------------*/
         
        if( UhgFlag == 0) data->mp_doneClick = 1;
        data->create_flag = UhgFlag;
	setModInterfaceElements(defaultModName, data);	

 	/*--------------------------------------------------------------*/
 	/* We try to add most Callback functions at this point - for	*/
 	/* book-keeping purposes...					*/
 	/*--------------------------------------------------------------*/
	addWidgetCallbacks(data);

 }



static void setModsTitle(Widget shell, char *segmentName)
{

	char	*string = "Mods for:  ";
	char	*title;
	
	
	title = (char *)XtMalloc(sizeof(char) * (strlen(string)+strlen(segmentName)+1));
	strcpy(title, string);
	strcat(title, segmentName);
	

	XtVaSetValues(shell, XmNtitle, title, NULL);

	XtFree(title);

}


static void initializeDialogs(Mods_everythingStruct *data)
{

	data->dialogStruct = XtNew(dialogWidgetStruct_t);

	createValueChangeDialogs(data);
	createModNotSavedDialog(data);
	createModTheSameDialog(data);
        createModViewerNotSavedDialog(data);
		createModDateErrorDialog(data);  /*Added by gzhou 11/26/2003*/
        
}


static unitsStruct_p initializeUnitsStruct(Display *display)
{

	unitsStruct_p units;

	
	units = XtNew(unitsStruct_t);
	
	units->General_Units = getUnits(display, IFPA_mods_general_units, IFPA_mods_general_units_type);
	units->NWSRFS_Units  = getUnits(display, IFPA_general_units, IFPA_general_units_type);
	units->API_Units     = getUnits(display, IFPA_mods_API_units, IFPA_mods_API_units_type);
	units->SAC_Units     = getUnits(display, IFPA_mods_SAC_units, IFPA_mods_SAC_units_type);

	return(units);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_initializeInterface.c,v $";
 static char rcs_id2[] = "$Id: Mods_initializeInterface.c,v 1.4 2006/04/18 15:27:47 aivo Exp $";}
/*  ===================================================  */

}
