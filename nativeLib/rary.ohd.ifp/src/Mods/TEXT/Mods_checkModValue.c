/************************************************************************/
/*									*/
/*	FILE:		Mods_checkModValue.c				*/
/*									*/
/*	Coded by:	Tom Adams					*/
/*			NWS * Office of Hydrology * HRL			*/
/*	Date:		11/15/94					*/
/*									*/
/*      NOTE:	Taken from event_funcs.c				*/
/*									*/
/************************************************************************/



#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"




int checkValue(float displayValue, Mods_everythingStruct *data)
{

	Display		*display;



	display = XtDisplay(data->widgetData->ifp_modsShell);
	
	
	if(displayValue <= data->scaleValueLimits->lower_warning_limit) 
	{
		if(displayValue == data->scaleValueLimits->lower_warning_limit
			&& data->scaleValueLimits->lower_warning_inclusive == YES) 
		{
			if(data->flags->warningPoppedUp == YES) 
			{
				XtUnmanageChild(data->dialogStruct->valueWarningMessageBox);
				XtUnmanageChild(data->dialogStruct->valueErrorMessageBox);
				data->flags->warningPoppedUp = NO;
				data->flags->errorMessageDisplayed = NO;
			}
			return(TRUE);
		}

		if(displayValue <= data->scaleValueLimits->lower_error_limit) 
		{
			if(displayValue == data->scaleValueLimits->lower_error_limit
				&& data->scaleValueLimits->lower_error_inclusive == YES) 
			{
				if(data->flags->warningPoppedUp == NO)
					if(data->flags->haveBeenWarned != YES) 
					{
						XtManageChild(data->dialogStruct->valueWarningMessageBox);
						data->flags->warningPoppedUp = YES;
						data->flags->haveBeenWarned = YES;
					}
				return(TRUE);
			}

			if(data->flags->warningPoppedUp == NO) 
			{
				XtManageChild(data->dialogStruct->valueWarningMessageBox);
				data->flags->warningPoppedUp = YES;
				data->flags->haveBeenWarned = YES;
			}
				
			XtManageChild(data->dialogStruct->valueErrorMessageBox);
			data->flags->errorMessageDisplayed = YES;
			XBell(display, 100);
			return(FALSE);
		}

		if(data->flags->warningPoppedUp == NO)
			if(data->flags->haveBeenWarned != YES) 
			{
				XtManageChild(data->dialogStruct->valueWarningMessageBox);
				data->flags->warningPoppedUp = YES;
				data->flags->haveBeenWarned = YES;
			}

		if(data->flags->errorMessageDisplayed == YES) 
		{
			XtUnmanageChild(data->dialogStruct->valueErrorMessageBox);
			data->flags->errorMessageDisplayed = NO;
		}

		return(TRUE);
	}

	if(displayValue >= data->scaleValueLimits->upper_warning_limit) 
	{
		if(displayValue == data->scaleValueLimits->upper_warning_limit
			&& data->scaleValueLimits->upper_warning_inclusive == YES) 
		{
		
			if(data->flags->warningPoppedUp == YES) 
			{
				XtUnmanageChild(data->dialogStruct->valueWarningMessageBox);
				XtUnmanageChild(data->dialogStruct->valueErrorMessageBox);
				data->flags->warningPoppedUp = NO;
				data->flags->errorMessageDisplayed = NO;
			}
			return(TRUE);
		}

		if(displayValue >= data->scaleValueLimits->upper_error_limit) 
		{
			if(displayValue == data->scaleValueLimits->upper_error_limit 
				&& data->scaleValueLimits->upper_error_inclusive == YES) 
			{
				
				if(data->flags->warningPoppedUp == NO)
					if(data->flags->haveBeenWarned != YES) 
					{
						XtManageChild(data->dialogStruct->valueWarningMessageBox);
						data->flags->warningPoppedUp = YES;
						data->flags->haveBeenWarned = YES;
					}
				return(TRUE);
			}
			if(data->flags->warningPoppedUp == NO) 
			{
				XtManageChild(data->dialogStruct->valueWarningMessageBox);
				data->flags->warningPoppedUp = YES;
				data->flags->haveBeenWarned = YES;
			}
				
			XtManageChild(data->dialogStruct->valueErrorMessageBox);
			data->flags->errorMessageDisplayed = YES;
			XBell(display, 100);
			return(FALSE);
		}

		if(data->flags->warningPoppedUp == NO)
			if(data->flags->haveBeenWarned != YES) 
			{
			
				XtManageChild(data->dialogStruct->valueWarningMessageBox);
				data->flags->warningPoppedUp = YES;
				data->flags->haveBeenWarned = YES;
			}

		if(data->flags->errorMessageDisplayed == YES) 
		{
			XtUnmanageChild(data->dialogStruct->valueErrorMessageBox);
			data->flags->errorMessageDisplayed = NO;
		}

		return(TRUE);
	}


	if(data->flags->warningPoppedUp == YES) 
	{
		XtUnmanageChild(data->dialogStruct->valueWarningMessageBox);
		XtUnmanageChild(data->dialogStruct->valueErrorMessageBox);
		data->flags->warningPoppedUp = NO;
		data->flags->errorMessageDisplayed = NO;
	}

	return(TRUE);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_checkModValue.c,v $";
 static char rcs_id2[] = "$Id: Mods_checkModValue.c,v 1.1 1995/11/14 12:19:07 page Exp $";}
/*  ===================================================  */

}


