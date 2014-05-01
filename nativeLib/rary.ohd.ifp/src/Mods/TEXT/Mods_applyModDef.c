/****************************************************************/
/*								*/
/*	FILE:		Mods_applyModDef.c			*/
/*								*/
/*	Turn ON/OFF GUI elements by setting the appropriate 	*/
/*	widgets insensitive - this is done solely by data	*/
/*	passed in the 'selectedModDef' structure		*/
/*								*/
/*	Coded by:	Tom Adams				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		10/18/94				*/
/*								*/
/*	Modified by:    D. Page - 7 Nov. 1995			*/
/*			Added code for Valid dates		*/
/*	                A. Vo   - 6 May. 1999			*/
/*			Added Fgroup, Segment, Range selection	*/
/*			A. Vo   - 6 May 2004			*/
/*			Added code to handle display for        */					
/*			UHGCDATE                                */					
/****************************************************************/

#include "Mods_globalDefs.h"
#include "Mods_widgetStruct.h"
#include "Mods_defStruct.h"
#include "ifp_atoms.h"
/****************************************************************/
/*								*/
/*	applyModDef()						*/
/*								*/
/*								*/
/*	PURPOSE: Set widgets in the main Mods window either	*/
/*		 insensitive or we Unmanage it, depending	*/
/*		 which is appropriate for the Mod		*/
/*								*/
/*	NOTE:	Widgets must be Managed to set them insensitive	*/
/*								*/
/****************************************************************/

void applyModDef(Mod_defStruct *modDef, ifp_modsShell_p widgetData)
{


	static char first_initialize = 1;

	
	
	
	/*-------------	Scale Widget: value change -------------*/
	if(modDef->value) {
		XtUnmanageChild(widgetData->noValueEntryLabel);
		XtManageChild(widgetData->valueDisplayEntry);
		XtManageChild(widgetData->modsScale);
		}
	else	{
		XtUnmanageChild(widgetData->valueDisplayEntry);
		XtUnmanageChild(widgetData->modsScale);
		XtManageChild(widgetData->noValueEntryLabel);
		}
	
	/*-------------	Arrow Widget: date change --------------*/
	if(modDef->arrowButtons) XtManageChild(widgetData->dateArrowBFrame);
	else XtUnmanageChild(widgetData->dateArrowBFrame);	
			
	/*------------------------------------------------------*/
	/* If neither the START nor the END dates show for the	*/
	/* Mod, notify the user...				*/
	/*------------------------------------------------------*/
	if(!modDef->modStartDate && !modDef->modEndDate 
	   && !modDef->modValidDate)
		XtManageChild(widgetData->noDatesLabel);
		
		
	/*------------- Mod START date change ------------------*/	
	switch(modDef->modStartDate){
		case OFF:
			XtSetSensitive(widgetData->startDateFrame, TRUE);
			XtUnmanageChild(widgetData->startDateFrame);
			break;
			
		case ON:
			XtSetSensitive(widgetData->startDateFrame, TRUE);
			XtManageChild(widgetData->startDateFrame);
			XtUnmanageChild(widgetData->noDatesLabel);
			break;
		
		case INSENSITIVE:
			if(!XtIsManaged(widgetData->startDateFrame))	
				XtManageChild(widgetData->startDateFrame);	
			XtSetSensitive(widgetData->startDateFrame, FALSE);
			XtUnmanageChild(widgetData->noDatesLabel);
			break;
		
		default:
			XtSetSensitive(widgetData->startDateFrame, TRUE);
			XtUnmanageChild(widgetData->startDateFrame);
			break;
		}
		
	
	/*------------- Mod END date change --------------------*/	
	switch(modDef->modEndDate){
		case OFF:
			XtSetSensitive(widgetData->endDateFrame, TRUE);
			XtUnmanageChild(widgetData->endDateFrame);
			break;
			
		case ON:
			XtSetSensitive(widgetData->endDateFrame, TRUE);
			XtManageChild(widgetData->endDateFrame);
			XtUnmanageChild(widgetData->noDatesLabel);
			break;
		
		case INSENSITIVE:
			if(!XtIsManaged(widgetData->endDateFrame))
				XtManageChild(widgetData->endDateFrame);
			XtSetSensitive(widgetData->endDateFrame, FALSE);
			XtUnmanageChild(widgetData->noDatesLabel);
			break;
		
		default:
			XtSetSensitive(widgetData->endDateFrame, TRUE);
			XtUnmanageChild(widgetData->endDateFrame);
			break;
		}
		
	
	/*------------- Mod Valid date change --------------------*/	
	switch(modDef->modValidDate){
		case OFF:
			XtSetSensitive(widgetData->validDateFrame, TRUE);
			XtUnmanageChild(widgetData->validDateFrame);
			break;
			
		case ON:
			XtSetSensitive(widgetData->validDateFrame, TRUE);
			XtManageChild(widgetData->validDateFrame);
			XtUnmanageChild(widgetData->noDatesLabel);
			break;
		
		case INSENSITIVE:
			if(!XtIsManaged(widgetData->validDateFrame))	
				XtManageChild(widgetData->validDateFrame);
			XtSetSensitive(widgetData->validDateFrame, FALSE);
			XtUnmanageChild(widgetData->noDatesLabel);
			break;
		
		default:
			XtUnmanageChild(widgetData->validDateFrame);
			break;
		}
		
	switch(modDef->modFGroup){
		case SEGMENT:
			XtVaSetValues(widgetData->optionsApplyMenu,XmNmenuHistory,
						     widgetData->SegmentButton, NULL);
			XtSetSensitive(widgetData->SegmentButton, TRUE);
			XtSetSensitive(widgetData->FgroupButton, FALSE);
			XtSetSensitive(widgetData->RangeButton, FALSE);
			break;
		case FGROUP:
			XtVaSetValues(widgetData->optionsApplyMenu,XmNmenuHistory,
						     widgetData->SegmentButton, NULL);
			XtSetSensitive(widgetData->SegmentButton, TRUE);
			XtSetSensitive(widgetData->FgroupButton, TRUE);
			XtSetSensitive(widgetData->RangeButton, TRUE);
			break;
		case RANGE:
			XtSetSensitive(widgetData->RangeButton, TRUE);
			XtSetSensitive(widgetData->SegmentButton, FALSE);
			XtSetSensitive(widgetData->FgroupButton, FALSE);
			break;
		default:
			XtSetSensitive(widgetData->SegmentButton, TRUE);
			break;
                }	
		/*DT***/



	/*-------------	Operation/Time-series List -------------*/	
	if(modDef->OpTimeSeries) XtSetSensitive(widgetData->timeSeriesFrame, TRUE);
	else XtSetSensitive(widgetData->timeSeriesFrame, FALSE);
	
	/*-------------	Dates List (date values) ---------------*/
        
    if (!strcmp(modDef->name,"UHGCDATE"))
	  {
		XtUnmanageChild(widgetData->noDatesValuesLabel);
		XtManageChild(widgetData->datesValueSW);
		}
	else if(modDef->TSDates) {
		XtUnmanageChild(widgetData->noDatesValuesLabel);
		XtManageChild(widgetData->datesValueSW);
		}
	else	{
		XtUnmanageChild(widgetData->datesValueSW);
		XtManageChild(widgetData->noDatesValuesLabel);
		}
	
	/*-------------	Toggle Button: Data Dialog -------------*/
	if(modDef->entryDialog) XtManageChild(widgetData->dataEntryToggle);
	else XtUnmanageChild(widgetData->dataEntryToggle);
	
	/*-------------	Mod Options - Option Menu --------------*/
	if(modDef->modOption) {
		XtManageChild(widgetData->optionsMenu);
		XtUnmanageChild(widgetData->noOptionsLabel);
		}
	else	{
		XtManageChild(widgetData->noOptionsLabel);
		XtUnmanageChild(widgetData->optionsMenu);
		}



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_applyModDef.c,v $";
 static char rcs_id2[] = "$Id: Mods_applyModDef.c,v 1.4 2004/08/05 17:35:08 wkwock Exp $";}
/*  ===================================================  */

}

