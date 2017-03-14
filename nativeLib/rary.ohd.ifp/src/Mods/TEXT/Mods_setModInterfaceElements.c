/**************************************************************/
/*                                                            */
/*	FILE:		Mods_setModInterfaceElements.c	      */
/*                                                            */
/*                                                            */
/*	Coded by:	Tom Adams                             */
/*			NWS * Office of Hydrology * HRL       */
/*	Date:		10/13/94                              */
/*                                                            */
/*      Modified by:    D. Page                               */
/*                      28, 30 Oct. 1995                      */
/*                      10 Nov. 1995                          */
/*      		                                      */
/*                                                            */
/**************************************************************/




#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "ifp_atoms.h"



extern void		initializeValidInterfaceElements(char *, Mods_everythingStruct *);
extern void             reset_dates(Mods_everythingStruct *);

static Mod_defStruct	*getSelectedModDef(char *, Mod_interfaceDefStruct *);
static modLimitsDef	*getSelectedModLimitsDef(char *, Mod_limitsStruct *);


/****************************************************************/
/*								*/
/*	void setModInterfaceElements()				*/
/*								*/
/*	CALLED FROM:						*/
/*		'modMenuItemSelectionCB()'			*/
/*		'opMenuItemSelectionCB()'			*/
/*		'initializeInterface()'				*/
/*								*/
/*	(1) Call getSelectedModDef() - this needs to be done	*/
/*	    1st so we know how to change the Mod window for	*/
/*	    the selected Mod;					*/
/*	(2) Call applyModDef() - this turns ON/OFF the appro-	*/
/*	    priate interface elements by graying-out (setting	*/
/*	    insensitive) Widgets and Unmanaging them;		*/
/*      (3) Resets the StartDate, EndDate, and ValidDate        */
/*          of the mod appropriately                            */
/*	(4) Call getSelectedModLimitsDef() - this obtains the	*/
/*	    the value ranges (if any) for the current Mod	*/
/*	(5) Call initializeValidInterfaceElements() - this sets	*/
/*	    widget labels, date ranges, ScaleWidget ranges,	*/
/*	    etc. to their appropriate settings depending on	*/
/*	    which Mod name is passed...				*/
/*								*/
/****************************************************************/
extern void applyModDef(Mod_defStruct *, ifp_modsShell_p);

void setModInterfaceElements(char *modName, Mods_everythingStruct *data)
{
     static char first = 0;
     Widget mp_main_plotShellID;
     if(modName != (char *) NULL)
     { 	
		memset(data->ModName, '\0', 20);
 	    strcpy(data->ModName, modName);
              
 	    data->selectedModDef = getSelectedModDef(modName, data->interfaceDefs);		/*  1  */
 	    applyModDef(data->selectedModDef, data->widgetData);                            /*  2  */
 	
 		/* Call to routine to reset the dates for this mod */
 		reset_dates(data);                                                              /*  3  */
 	      
 		data->modValueLimits = getSelectedModLimitsDef(modName, data->modLimits);	/*  4  */
 		initializeValidInterfaceElements(modName, data);
        
        /* 
	 AiV  5/03/04 
         detroy mod plot widget if it is not closed by user to avoid
         
        */
        if(data->create_flag == 0) {
           first = 0; 
           data->mp_doneClick= 0;
        }
        
        if(data->mp_doneClick == 1) first = 0;

		/* destroy mod plot widget id of the (UH, UHGD) display only */
		if(first >= 1)/*Last modname is UHGCHNG,UHGCDATE,RRICHNG,ROCHNG*/
		{	
        	if(mp_main_plotShellID != NULL)
                    XtDestroyWidget(mp_main_plotShellID);
			first=0;
			mp_main_plotShellID = NULL;
		}

        if(strcmp(modName,"UHGCHNG")==0 || strcmp(modName,"UHGCDATE")==0||
           strcmp(modName,"RRICHNG")==0 || strcmp(modName,"ROCHNG")==0)
        {   
            first++; if(first > 10) first = 3;  
            if(data->create_flag == 1){
                mp_main_plotShellID  = data->modsPlotData->main_plot_shell;                      
            } 
            data->mp_doneClick = 0;
        } 
        /*AiV end 5/03/04  */      			
     }
   /*  else
        printf("WARNING:  modName is NULL in setModInterfaceElements\n");
   */
 
}


static Mod_defStruct *getSelectedModDef(char *modName, Mod_interfaceDefStruct *interfaceDefs)
{

	int	i = 0;

	/* Cycle through 'til we get a match	*/
	while(strcmp(modName, interfaceDefs->array[i]->name) != 0) 
	{
		i++;
		if(i >= interfaceDefs->num) 
		{
			printf("ERROR: Could not find match with %s!\n", modName);
			return(NULL);
		}
	}
		
	return(interfaceDefs->array[i]);
}


static modLimitsDef *getSelectedModLimitsDef(char *modName, Mod_limitsStruct *limitsDefs)
{

	int	i = 0;


	/* Cycle through 'til we get a match	*/
	while(strcmp(modName, limitsDefs->array[i]->name) != 0) 
	{
		i++;
		if(i >= limitsDefs->num) 
		{
			printf("ERROR: Could not find match with %s!\n", modName);
			return(NULL);
		}
	}
	
	return(limitsDefs->array[i]);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_setModInterfaceElements.c,v $";
 static char rcs_id2[] = "$Id: Mods_setModInterfaceElements.c,v 1.4 2006/04/18 15:28:18 aivo Exp $";}
/*  ===================================================  */

}
