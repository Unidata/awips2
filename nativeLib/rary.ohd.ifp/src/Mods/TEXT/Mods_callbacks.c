/**************************************************************/
/*                                                            */
/*	FILE:		Mods_callbacks.c                      */
/*                                                            */
/*	Run-time Modifications Interface creation module      */
/*                                                            */
/*	Coded by:	Tom Adams                             */
/*			NWS * Office of Hydrology * HRL       */
/*	Date:		06/01/94                              */
/*                                                            */
/*      Modified by:    D. Page - 29, 31 Oct. 1995            */
/*                      added resetInputFocusToDefaultWindow, */
/*                      reset_currentModSaved,                */
/*                      call_setModInterfaceElements,         */
/*                      call_createSelectionCB                */
/*                      11 Nov. 1995 - added                  */
/*                      modViewerChangedCB                    */
/*                      21 Feb. 1996 - added check on         */
/*                      handleShowDataCB to see if            */
/*                      ifp_modsMessageBox managed            */
/*                      AV      - 02/13/01                    */
/*                      added SACCO mod                       */
/*                                                            */
/**************************************************************/


#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "ifp_atoms.h"
#include "Mods_defStruct.h"
#include "c_call_f/getsacco.h" /*--Added by AV --*/
#include "dhm_mods.h"

void popdown_shell(Widget, Widget, XtPointer);
void destroyModsDataCB(Widget, Mods_everythingStruct *, XtPointer);
void handleShowDataCB(Widget, Mods_everythingStruct *, XtPointer);
void toggleBManageShellCB(Widget, Mods_everythingStruct *, XtPointer);
void modOptionsMenuItemSelectionCB(Widget, Mods_everythingStruct *, XtPointer);
void manageDialogCB(Widget w, Widget messageBox, XtPointer);
void unmanageDialogCB(Widget, Widget, XtPointer);
void opTSListSelectionCB(Widget, Mods_everythingStruct *, XmListCallbackStruct *);
void resetInputFocusToDefaultWindowCB(Widget, Widget, XmAnyCallbackStruct *);
void set_currentModSavedTrueCB(Widget, Mods_everythingStruct *, XmAnyCallbackStruct *);
void set_currentModSavedFalseCB(Widget, Mods_everythingStruct *, XmAnyCallbackStruct *);
void call_setModInterfaceElementsCB(Widget, Mods_everythingStruct *, XmAnyCallbackStruct *);
void call_createSelectionCB(Widget w, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
void modViewerChangedCB(Widget w, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
void call_setModInterfaceElementsCB(Widget , Mods_everythingStruct *, XmAnyCallbackStruct *);

extern void showTSPlot(Widget, Mods_everythingStruct *);
extern void load_new_mods_edit(Mods_everythingStruct *);
extern void load_new_fgroupmods_edit(Mods_everythingStruct *);
extern void load_mods_from_file_edit(Mods_everythingStruct *);
extern void load_fgroupmods_from_file_edit(Mods_everythingStruct *);
extern void load_mods_from_ofs_edit(Mods_everythingStruct *);
extern void load_mods_from_ofsfgroup_edit(Mods_everythingStruct *);
extern void viewerWindowChangedCB(Widget, Mods_everythingStruct *, XmAnyCallbackStruct *);
extern void setModInterfaceElements(char *, Mods_everythingStruct *);
extern int isModDatesValid(Mods_everythingStruct *);
extern void showInvalidDateErrorMsg(Widget, int);
extern void createSelectionCB(Widget, Mods_everythingStruct *,
XmPushButtonCallbackStruct *);

/*extern show_createSaccoDS(Widget , Mods_everythingStruct * );*/
void    show_saccoDS(Widget , Mods_everythingStruct *);



#if 0
void popdown_shell(Widget w, Widget shell_widget, XtPointer call_data)
{

	XtPopdown(shell_widget);

}
#endif

/****************************************************************/
/*								*/
/*	destroyModsDataCB()					*/
/*								*/
/*	This Callback fuction is called when the parent of the	*/
/*	Mods MessageBox widget is destroyed through its		*/
/*	DestroyCallback						*/
/*								*/
/*	PURPOSE:						*/
/*								*/
/*	To free all space Malloc'ed when 'ntsmod()' is called,	*/
/*	so that we won't continue to gobble-up memory whereby	*/
/*	things begin to fail left and right.			*/
/*								*/
/*	ALSO:	(1) Notice that 'ofsData' is NOT free'd - this	*/
/*		is intentional, because if we do free it there	*/
/*		is no getting it back, because it's read-in &	*/
/*		created once when the OFS starts-up;		*/
/*								*/
/*		(2) 'widgetData' is intentionally NOT free'd	*/
/*		because Xt handles destruction of all child	*/
/*		widgets when the Mod Shell widget is destroyed;	*/
/*								*/
/*		(3) The ORDER things are free'd IS IMPORTANT,	*/
/*		i.e., we need to free things furthest down the	*/
/*		structure list first & work are way up the list	*/
/*		until we get to 'data'...			*/
/*								*/
/****************************************************************/
void destroyModsDataCB(Widget w, Mods_everythingStruct *data, XtPointer call_data)
{

	/* printf("Inside 'destroyModsDataCB()'...\n"); */

	XtFree(data->ModSettingsPath);		/* Mod Settings Path		*/

	XtFree((char *)data->opsMenu->widgetStruct);	/* Operations Menu data		*/
	XtFree((char *)data->opsMenu->menuItems);
	XtFree((char *)data->opsMenu);

	XtFree((char *)data->modsMenu->widgetStruct);	/* Mods Menu data		*/
	XtFree((char *)data->modsMenu->menuItems);
	XtFree((char *)data->modsMenu);

	XtFree((char *)data->interfaceDefs->array);	/* Interface Defs		*/
	XtFree((char *)data->interfaceDefs);

	XtFree((char *)data->modLimits);		/* Mod value range limits	*/
	XtFree((char *)data->scaleValueLimits);		/* Mod value range limits	*/

	XtFree((char *)data->ModDates->StartDate);	/* Mod Dates			*/
	XtFree((char *)data->ModDates->EndDate);
	XtFree((char *)data->ModDates->ValidDate);
	XtFree((char *)data->ModDates);

	XtFree((char *)data);				/* Everything			*/


}



void handleShowDataCB(Widget w, Mods_everythingStruct *data, XtPointer call_data)
{
    char opname[9]; /*aivo added 2/13/01 */
    char optype[9];
    float sac[7];   /* sac[0] = UZTWC ,sac[1] = UZFWC */
    		    /* sac[2] = LZTWC ,sac[4] = LZFSC */
    		    /* sac[3] = LZFPC ,sac[5] = ADIMC */
    float co[7];
    int  i,j;
    static int first = 1;
    data->create_flag = 0;
    data->mp_doneClick = 1;

    data->WaterLvl  = (WaterLevelType *) malloc(sizeof(WaterLevelType));
    /* Initialize waterlevel       */
    if(first == 1)
    {
        for(i = 0; i < 7; i++)
        {
            data->WaterLvl->MaxWaterLevels[i] = -999.0;
            data->WaterLvl->CurrentWaterLevels[i] = -999.0;
            data->WaterLvl->NewWaterLevels[i] = -999.0;
        }
	first = 0;
    }



    strncpy(optype,(char *)data->ModSettings->operation.type,8);
    optype[8] = '\0';

    strncpy(data->WaterLvl->Model,(char *)data->ModSettings->operation.type,strlen((char *)data->ModSettings->operation.type));
    data->WaterLvl->Model[8] = '\0';


    strncpy(data->WaterLvl->Seg,data->SegmentName,strlen(data->SegmentName));
    data->WaterLvl->Seg[8] = '\0';

    for(j = 0; j < data->opTSdata->num; j++)
        {
            if(operations_number[j] == 1)
            {

                strncpy(data->WaterLvl->ID,data->ModSettings->operation.name[j], 8);

                strncpy(opname,data->ModSettings->operation.name[j],8);

            }  /* end of if(operations...)  */
            data->WaterLvl->ID[8] = '\0';
            opname[8] = '\0';
        }  /*  end of for(j...)  */

    if(XtIsManaged(data->widgetData->ifp_modsMessageBox))
    {
	switch(data->selectedModDef->DataWinType)
	{


		case SETQMEAN_TYPE:
			if(XtIsManaged(data->setQMeanWidgets->setQMeanMsgBox))
			{
			   XtUnmanageChild(data->setQMeanWidgets->setQMeanMsgBox);
			   XmToggleButtonSetState(data->widgetData->dataEntryToggle,
			                          FALSE, FALSE);
			}
			else
			   XtManageChild(data->setQMeanWidgets->setQMeanMsgBox);
			break;

		case TIME_SERIES_TYPE:
data->create_flag = 1;
			showTSPlot(w, data);
			break;
		case SACCO_TYPE:    /* aivo added 2/13/01 */

/*data->create_flag = 0; */

			getsacco_(opname,sac,co);

			for(i = 0; i < 7; i++)
			{
        		    data->WaterLvl->MaxWaterLevels[i] = sac[i];
        		    data->WaterLvl->CurrentWaterLevels[i] = co[i];
			}


			/* FGIX max is set to 0.0 allsway, min for FGIX = -100 */

			data->WaterLvl->MaxWaterLevels[6] = 0.0;


		        /*
                        printf("IN mods_callback: SAC_ARRAY= %f %f %f %f %f %f %f\n",sac[0],
								   sac[1],
								   sac[2],
								   sac[3],
								   sac[4],
								   sac[5],
                                                                   sac[6]);
			printf("IN mods_callback: CO_ARRAY= %f %f %f %f %f %f %f\n",co[0],
								   co[1],
								   co[2],
								   co[3],
								   co[4],
								   co[5],
                                                                   co[6]);
			printf("IN mods_callback: currentWaterlevl= %f %f %f %f %f %f  %f\n",data->WaterLvl->CurrentWaterLevels[0],
								   data->WaterLvl->CurrentWaterLevels[1],
								   data->WaterLvl->CurrentWaterLevels[2],
								   data->WaterLvl->CurrentWaterLevels[3],
								   data->WaterLvl->CurrentWaterLevels[4],
								   data->WaterLvl->CurrentWaterLevels[5],
                                                                   data->WaterLvl->CurrentWaterLevels[6]);
			printf("IN mods_callback: MaxWaterlevl= %f %f %f %f %f %f  %f\n",data->WaterLvl->MaxWaterLevels[0],
								   data->WaterLvl->MaxWaterLevels[1],
								   data->WaterLvl->MaxWaterLevels[2],
								   data->WaterLvl->MaxWaterLevels[3],
								   data->WaterLvl->MaxWaterLevels[4],
								   data->WaterLvl->MaxWaterLevels[5],
                                                                   data->WaterLvl->MaxWaterLevels[6]);

                       */


                        if(!saccoScaleDS)
			    show_saccoDS(w, data);
                        else
                        {
                            XtUnmanageChild(saccoScaleDS);
                            XmToggleButtonSetState(data->widgetData->dataEntryToggle,
			                          FALSE, FALSE);
                            saccoScaleDS = NULL;
                        }



			break;

		case NONE:
			break;

	}
    }
    /*
    * The data was not actually created if this else is triggered, so reset create_flag
    * to be 0. (Hank Herr, 2004-11-16)
    */
/*    else
    {
        data->create_flag = 0;
    }
*/
}


void toggleBManageShellCB(Widget w, Mods_everythingStruct *data, XtPointer call_data)
{
	if(XtIsManaged(data->viewerWidgets->viewerForm))
	 {
	   /* Remove the valueChanged callbacks for fromFileModsText
	    * and ofsModsText (if set).
	    */
	   /*printf("viewer manage-Remove fileModsText\n");*/

	   XtRemoveCallback(data->viewerWidgets->fromFileModsText,
	                    XmNvalueChangedCallback,(XtCallbackProc)viewerWindowChangedCB, data);

	   XtRemoveCallback(data->viewerWidgets->fromFileFGroupModsText,
	                    XmNvalueChangedCallback, (XtCallbackProc)viewerWindowChangedCB, data);



	   if(data->Mod_globalPrefs->ofs_mods_editable)
	      {
	      XtRemoveCallback(data->viewerWidgets->ofsModsText,
	      	               XmNvalueChangedCallback, (XtCallbackProc)viewerWindowChangedCB, data);
	      XtRemoveCallback(data->viewerWidgets->ofsFGroupModsText,
	                    XmNvalueChangedCallback, (XtCallbackProc)viewerWindowChangedCB, data);
	      }

           if(data->fromFileModsSaved == FALSE || data->ofsModsSaved == FALSE || data->ofsfgModsSaved == FALSE
                                             || data->fromFilefgModsSaved == FALSE)
              XtManageChild(data->dialogStruct->modViewerNotSavedMB);
           else
	      XtUnmanageChild(data->viewerWidgets->viewerForm);

        }
	else
	{
	   /* Load data for the 5 windows */
	   load_new_mods_edit(data);
	   load_mods_from_file_edit(data);
	   load_fgroupmods_from_file_edit(data);
	   load_mods_from_ofs_edit(data);
	   load_mods_from_ofsfgroup_edit(data);

	   /* Now add the valueChanged callbacks for fromFileModsText
	    * and ofsModsText (if needed) after data has been loaded
	    */
	   XtAddCallback(data->viewerWidgets->fromFileModsText,
		         XmNvalueChangedCallback, (XtCallbackProc)viewerWindowChangedCB, data);

	   XtAddCallback(data->viewerWidgets->fromFileFGroupModsText,
		         XmNvalueChangedCallback, (XtCallbackProc)viewerWindowChangedCB, data);

	   if(data->Mod_globalPrefs->ofs_mods_editable)
	   {
	      XtAddCallback(data->viewerWidgets->ofsFGroupModsText,
		         XmNvalueChangedCallback, (XtCallbackProc)viewerWindowChangedCB, data);

	      XtAddCallback(data->viewerWidgets->ofsModsText,
		            XmNvalueChangedCallback, (XtCallbackProc)viewerWindowChangedCB, data);
	   }


	   /* Check if ofs mods are editable, if not, set the
	    * ofsMods window resource to FALSE (default is TRUE)
	    */
	   if(data->Mod_globalPrefs->ofs_mods_editable == FALSE)
	     { XtVaSetValues(data->viewerWidgets->ofsModsText,
	                    XmNeditable, FALSE, NULL);
	      XtVaSetValues(data->viewerWidgets->ofsFGroupModsText,
	                    XmNeditable, FALSE, NULL);
	     }

	   /* Set the fromFileModsSaved, fromFilefgModsSaved, ofsfgModsSaved and ofsModsSaved flags */
	   data->fromFileModsSaved = TRUE;
	   data->fromFilefgModsSaved = TRUE;
	   data->ofsModsSaved = TRUE;
	   data->ofsfgModsSaved = TRUE;

	   XtManageChild(data->viewerWidgets->viewerForm);
	}
}



void modOptionsMenuItemSelectionCB(Widget w, Mods_everythingStruct *data, XtPointer callData)
{

        XmString     xmStringName;
        char         *name;


        XtVaGetValues(w, XmNlabelString, &xmStringName, NULL);
        XmStringGetLtoR(xmStringName, XmFONTLIST_DEFAULT_TAG, &name);

        /* Call this CB to set that a mod element has been changed */
        set_currentModSavedFalseCB(w, data, NULL);

	 /*printf("Inside the CALLBACK function for %s\n", name); */


}

void manageDialogCB(Widget w, Widget messageBox, XtPointer call_data)
{

	XtManageChild(messageBox);

}

void unmanageDialogCB(Widget w, Widget messageBox, XtPointer call_data)
{

	XtUnmanageChild(messageBox);

}
extern void popup_dhm_mods_gui(Mods_everythingStruct *data, char []);
/* added code for popup dprecip or dSACST mods in Operation name selection widget */
void opTSListSelectionCB(Widget w, Mods_everythingStruct *data, XmListCallbackStruct *call_data)
{

	int	i, j = 0;
	char    *opName;
	char    curOpName[9] = "";
	/*char   modsString[MAX_MOD_STRING_LENGTH]="";
	char   dsacCurOpname[NWSRFS_ID_LENGTH+1]="";

	char sacmodString[MAX_MOD_STRING_LENGTH] = "";
	char precipmodString[MAX_MOD_STRING_LENGTH] = "";
	*/
	/*----------------------------------------------------------------------*/
	/* Reset the arrays to guarantee that we correctly set the flags	*/
	/*----------------------------------------------------------------------*/
	for(i = 0; i < MAX_OPERATIONS; i++) {
		operations_number[i] = FALSE;
		data->flags->opNumber[i] = FALSE;
	}
	/*----------------------------------------------------------------------*/
	/* Reset the arrays based on the item(s) selected in the List           */
	/* Added code for SINGLE_SELECT option - dp - 28 Oct. 96   		*/
	/*----------------------------------------------------------------------*/
	if(data->selectedModDef->OpTSSelectType)  /* MULTIPLE_SELECT_TYPE  */
	{
	   for(i = 0; i < call_data->selected_item_count; i++)
	   {
		operations_number[call_data->selected_item_positions[i]-1] = TRUE;
		data->flags->opNumber[call_data->selected_item_positions[i]-1] = TRUE;
	   }
        }
        else                                     /* SINGLE_SELECT_TYPE  */
        {
	   operations_number[call_data->item_position - 1] = TRUE;
	   data->flags->opNumber[call_data->item_position - 1] = TRUE;

           opName=strtok(data->ModSettings->operation.name[call_data->item_position - 1]," ");

	   strncpy(curOpName,opName,8);

	   popup_dhm_mods_gui(data,curOpName);


        }



}

/* *******************************************************************************

	resetInputFocusToDefaultWindowCB()

   ******************************************************************************* */

void resetInputFocusToDefaultWindowCB(Widget w, Widget shell_widget,
                                      XmAnyCallbackStruct *call_data)
{

   XSetInputFocus(XtDisplay(w), DefaultRootWindow(XtDisplay(w)),
                  RevertToPointerRoot, CurrentTime);

}

/* *******************************************************************************

	set_currentModSavedTrueCB()

     Sets currentModSaved to TRUE and sets the window property
     Also sets currentModChanged to FALSE
   ******************************************************************************* */

void set_currentModSavedTrueCB(Widget w, Mods_everythingStruct *data,
                               XmAnyCallbackStruct *call_data)
{
   Display         *display;

   display = XtDisplay(data->widgetData->ifp_modsShell);
   data->currentModSaved = TRUE;
   data->currentModChanged = FALSE;

   XChangeProperty(
	display,
	DefaultRootWindow(display),
	IFPA_current_mod_saved,
	IFPA_current_mod_saved_type,
	8,
	PropModeReplace,
	(unsigned char *)&data->currentModSaved,
	sizeof(int)
	);

}

/* *******************************************************************************

	set_currentModSavedFalseCB()

     Sets currentModChanged to TRUE
     Also sets currentModChanged to FALSE and sets window property
   ******************************************************************************* */

void set_currentModSavedFalseCB(Widget w, Mods_everythingStruct *data,
                                XmAnyCallbackStruct *call_data)
{
   Display         *display;

   display = XtDisplay(w);

   data->currentModChanged = TRUE;
   data->currentModSaved = FALSE;

   XChangeProperty(
	display,
	DefaultRootWindow(display),
	IFPA_current_mod_saved,
	IFPA_current_mod_saved_type,
	8,
	PropModeReplace,
	(unsigned char *)&data->currentModSaved,
	sizeof(int)
	);

}

/* *******************************************************************************

	call_setModInterfaceElementsCB()

   ******************************************************************************* */

void call_setModInterfaceElementsCB(Widget w, Mods_everythingStruct *data,
                                    XmAnyCallbackStruct *call_data)
{
   char      name[20];

   memset(name, '\0', 20);
   strcpy(name, data->ModName);

   /* Need to copy data->ModName to another variable to pass into
    * setModInterfaceElements because it gets reset in the
    * routine and is lost. - dp - 30 Oct. 1995
    */
   setModInterfaceElements(name, data);
}

/* *******************************************************************************

	call_createSelectionCB()

      Resets the data->ModName to the previousModName before calling routine
      to create the mod.  After, it copies the save_mod_name back to
      ModName.

   ******************************************************************************* */

void call_createSelectionCB(Widget w, Mods_everythingStruct *data,
                            XmPushButtonCallbackStruct *call_data)
{
   char      save_mod_name[20];
int isValidMod;

if (!strcmp(data->selectedModDef->name,"UHGCDATE"))
{	isValidMod=isModDatesValid(data);
	if (isValidMod<0)
	{	showInvalidDateErrorMsg(w,isValidMod);
		return;
	}

}

/*kwzif(w==data->dialogStruct->modNotSavedMessageBox &&
   isModDatesValid(data) < 0 && data->currentModChanged == TRUE)
{
	ShowErrorDialog (data->widgetData->modsClose,"***invalid mod date***");
	return ;
}kwz*/
   memset(save_mod_name, '\0', 20);

   strcpy(save_mod_name, data->ModName);
   strcpy(data->ModName, data->previousModName);
   /*printf("before calling createSelectionC\n"); */
   createSelectionCB(w, data, NULL);

   strcpy(data->ModName, save_mod_name);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_callbacks.c,v $";
 static char rcs_id2[] = "$Id: Mods_callbacks.c,v 1.9 2006/04/18 13:34:00 aivo Exp $";}
/*  ===================================================  */

}

