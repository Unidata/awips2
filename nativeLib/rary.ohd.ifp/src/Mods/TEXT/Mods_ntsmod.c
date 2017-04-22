/**************************************************************/
/*                                                            */
/*	FILE:		ntsmod.c                              */
/*                                                            */
/*	Run-time Modifications Interface creation module      */
/*                                                            */
/*	Coded by:	Tom Adams                             */
/*			NWS * Office of Hydrology * HRL       */
/*	Date:		08/16/94                              */
/*                                                            */
/*      Modified by:    D. Page - 17 May 1995, 9 Oct. 1995,   */
/*                      12 Oct. 1995, 29, 30, 31 Oct. 1995    */
/*                      11 Nov. 1995                          */
/*                                                            */
/*      NOTE:	This file provides the code layer between     */
/*		cex25.c and the XDesigner generated interface */
/*		code in mods.c; interface initializations are */
/*		called from ntsmod()...                       */
/*                                                            */
/**************************************************************/




#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"




static void createModsMenus(Mods_everythingStruct *);
static Mod_flagsStruct_p initializeFlagsStruct();

extern menuItemsNames_struct	*getModOpNames(char **, int *);
extern menuItemsNames_struct	*getModNames();
extern pullDownMenu_struct	*create_PulldownChildren(Widget, menuItemsNames_struct *, char *, void *, caddr_t);
extern void			initializeInterface(char *, Mods_everythingStruct *);
extern void			setModMenuItemsValid(ofsData_struct *, pullDownMenu_struct *, char *);
extern void			opMenuItemSelectionCB(Widget, Mods_everythingStruct *, XtPointer);
extern void			modMenuItemSelectionCB(Widget, Mods_everythingStruct *, XtPointer);
extern int                      get_apps_defaults(char *, int *, char *, int *);
extern void read_globalPrefs(Mods_everythingStruct *);
/*--------------------------------------------------------------*/
/*								*/
/*	ntsmod() :						*/
/*								*/
/*		Non-time-series Modifications -			*/
/*								*/
/*		Code layer that calls for the creation and	*/
/*		initialization of the interface supporting the	*/
/*		NWSRFS run-time modifications that are NOT	*/
/*		modifications of time-series. This function is	*/
/*		called once from cex25 when the user selects	*/
/*		'Begin' within the IFP and subsequently,	*/
/*		whenever the user selects:			*/
/*								*/
/*		(1) 'Next',					*/
/*		(2) 'Rerun', or					*/
/*		(3) 'Continue'					*/
/*								*/
/*	NOTE:	The 'SettingsPath' points to where all files	*/
/*		are read for initializing and changing the GUI 	*/
/*		depending on Mod selection.                     */
/*--------------------------------------------------------------*/

Mods_everythingStruct *ntsmod(float *p_float, char **p_char, float *ts_float,
	      char **ts_char, float *d_float, int *t_array, 
	      char *currentSegment, Widget parent)
{
	
	ifp_modsShell_p		widgetStruct;
	viewerShell_p		viewerStruct;
	setQMeanShell_p		setQMeanStruct;
	Mods_everythingStruct	*everything;
	ofsData_struct		*ofsData;
	char                    SettingsPath[100];
	int                     len, len2;

        /* Set the path for the ModSettings directory - dp - 5/17/95 */
        memset(SettingsPath, '\0', 100);
        len = strlen("ifp_options_dir");
        get_apps_defaults("ifp_options_dir", &len, SettingsPath, &len2);
        strcat(SettingsPath, "/ModSettings/");
        
	if(currentSegment == NULL)
           printf("The segment name is NULL\n");

	/*--------------------------------------------------------------*/
	/* Make space for a struct to carry around all the data		*/
	/* and initialize it...						*/
	/*--------------------------------------------------------------*/
	everything = XtNew(Mods_everythingStruct);
	
	
	/*--------------------------------------------------------------*/
	/* Make space for a struct to hold 'universal' flags and	*/
	/* initialize it						*/
	/*--------------------------------------------------------------*/
	everything->flags = initializeFlagsStruct();
	
	
	/*--------------------------------------------------------------*/
	/* Make space for a struct to carry around the OFS data		*/
	/* and initialize it...						*/
	/*--------------------------------------------------------------*/	
	ofsData = XtNew(ofsData_struct);
	ofsData->p_float_array  = p_float;
	ofsData->p_char_array   = p_char;
	ofsData->ts_float_array = ts_float;
	ofsData->ts_char_array  = ts_char;
	ofsData->d_float_array  = d_float;
	ofsData->t_array        = t_array;
  	
  	
	/*--------------------------------------------------------------*/
  	/* Create the main body of the Mods interface - this code is	*/
  	/* created STRICTLY within "XDesigner"; therefore any		*/
  	/* alterations except within XDesigner are VERBOTTEN!!!		*/
	/*--------------------------------------------------------------*/	
	widgetStruct	= create_ifp_modsShell (parent);
	viewerStruct	= create_viewerShell (parent);
	setQMeanStruct	= create_setQMeanShell (parent);
	
	
	/*--------------------------------------------------------------*/
	/* Initialize 'everything' struct...				*/
	/*--------------------------------------------------------------*/
	everything->ModSettingsPath = XtMalloc(sizeof(char)*(strlen(SettingsPath)+1));
	strcpy(everything->ModSettingsPath, SettingsPath);
	
	everything->ofsData		= ofsData;
	everything->widgetData		= widgetStruct;
	everything->viewerWidgets	= viewerStruct;
	everything->setQMeanWidgets	= setQMeanStruct;
	everything->Options		= NULL;
	everything->operModData         = NULL;
	everything->ModIndex            = 0;
	everything->fromFileMods_str    = (char *)NULL;
	everything->ofsMods_str         = (char *)NULL;
	everything->fromFilefgMods_str         = (char *)NULL;
	
	/*--------------------------------------------------------------*/
	/* Finish-off the Mods interface with the creation of some	*/
	/* Motif OptionMenus that change dynamically, depending on the	*/
	/* current Segment (i.e., Basin) which we know implicitly - at	*/
	/* least - from the OFS data passed to us...			*/
	/*--------------------------------------------------------------*/
	createModsMenus(everything);
	
        /* -------------------------------------------------------------*
         * Initialize currentModSaved to TRUE 
         * -------------------------------------------------------------*/
        everything->currentModSaved = TRUE;

        /* -------------------------------------------------------------*
         * Initialize currentModChanged to FALSE 
         * -------------------------------------------------------------*/
        everything->currentModChanged = FALSE;

        /* -------------------------------------------------------------*
         * Initialize fromFileModsSaved and ofsModsSaved to TRUE 
         * -------------------------------------------------------------*/
        everything->fromFileModsSaved = TRUE;
        everything->ofsModsSaved = TRUE;
        everything->fgroupModsSaved = TRUE;
        
        /* -------------------------------------------------------------*
         * Initialize the Mod_globalPrefs_t structure and 
         * read the ModGlobalPrefs file to set global preferences 
         * -------------------------------------------------------------*/
        everything->Mod_globalPrefs = XtNew(Mod_globalPrefs_t);
        read_globalPrefs(everything);

	/*--------------------------------------------------------------*/
	/* Initialize the interface:					*/
	/*	(1) Read in the file that establishes what GUI elements	*/
	/*	    should be turned-on given the (i) current segment	*/
	/*	    and	(ii) the default Mod that's selected;		*/
	/*	(2) Create & initialize the data structure to handle	*/
	/*	    switching on/off GUI elements			*/
	/*	(3) turn-on the appropriate GUI elements...		*/
	/*--------------------------------------------------------------*/
	initializeInterface(currentSegment, everything);

      	return(everything);

}




/*----------------------------------------------------------------------*/
/*									*/
/*	createModsMenus() :						*/
/*									*/
/*		Calling function to complete the creation &		*/
/*		initialization of the Mods GUI objects			*/		
/*									*/
/*	(1) Get the names of the Operations for the current segment	*/
/*	(2) Get the names of ALL the non-TS Run-time Modifications	*/
/*	(3) Create the pulldown children for the Motif OptionMenus for	*/
/*	    both							*/
/*	(4) Make visible only those Mods that are associated with	*/
/*	    operations for the current segment				*/
/*									*/
/*----------------------------------------------------------------------*/

static void createModsMenus(Mods_everythingStruct *data)
{

 	menuItemsNames_struct	*opNames;
 	menuItemsNames_struct	*modNames;
 	
        extern void opMenuItemSelectionCB(Widget, Mods_everythingStruct *, XtPointer);
        extern void genericCB(Widget, Mods_everythingStruct *, XtPointer);
	
	opNames  = getModOpNames(data->ofsData->p_char_array, data->ofsData->t_array);
	modNames = getModNames();
	
	/*--------------------------------------------------------------*/
	/* Create the OptionMenu children widgets (the actual options);	*/
	/* the arguments are - in order:				*/
	/*	(1) Parent Widget, a Pulldown MenuPane,			*/
	/*	(2) Names array structure for the Pulldown children,	*/
	/*	(3) Name of the last item in the menu list, if it is	*/
	/*	    different from the kinds of items found in (2);	*/
	/*	    NULL otherwise,					*/
	/*	(4) Callback function which is called when the menu	*/
	/*	    items are selected; NOTE: this allows only one	*/
	/*	    Callback function to be called regardless of which	*/
	/*	    item is selected, and				*/
	/*	(5) The client data passed to the callback function;	*/
	/*	    the data can be of any type for the function:	*/
	/*	    'create_PulldownChildren()'...			*/
	/*--------------------------------------------------------------*/
	data->opsMenu = 
		(pullDownMenu_struct *)create_PulldownChildren(data->widgetData->opsPulldown, 
                               opNames, "ALL", opMenuItemSelectionCB, (caddr_t)data);
	data->modsMenu =
		(pullDownMenu_struct *)create_PulldownChildren(data->widgetData->modPulldown, 
                               modNames, NULL, modMenuItemSelectionCB, (caddr_t)data);
		
	/*--------------------------------------------------------------*/
	/* Set the default selection for the Operations menu...		*/
	/* NOTE: XtNameToWidget gets the widget whose labelString is	*/
	/* "ALL" with parent Widget 'opsPulldown'...			*/
	/*--------------------------------------------------------------*/
	XtVaSetValues(data->widgetData->opsOptionMenu, XmNmenuHistory,
			XtNameToWidget(data->widgetData->opsPulldown, "ALL"), NULL);
	
	
	setModMenuItemsValid(data->ofsData, data->modsMenu, "ALL");	/* Default is 'ALL'	*/


}



static Mod_flagsStruct_p initializeFlagsStruct()
{

	Mod_flagsStruct_p	data;

	
	data = XtNew(Mod_flagsStruct_t);
								
	/*------------ Initialize the member values ------------*/
	data->OperationSelected = 0;
	data->errorMessageDisplayed = 0;
	data->warningPoppedUp = 0;
	data->haveBeenWarned = 0;

	return(data);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_ntsmod.c,v $";
 static char rcs_id2[] = "$Id: Mods_ntsmod.c,v 1.3 2006/04/18 15:28:01 aivo Exp $";}
/*  ===================================================  */

}
