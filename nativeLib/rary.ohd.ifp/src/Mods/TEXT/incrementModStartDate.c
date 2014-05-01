/**************************************************************/
/*                                                            */
/*	FILE:		incrementStartDate.c	              */
/*                                                            */
/*                                                            */
/*	Coded by:	D. Page                               */
/*			NWS * Office of Hydrology * HRL       */
/*	Date:		28 Oct. 1995                          */
/*                                                            */
/*      NOTE:		This function will increment the      */
/*                      start date widget one time step       */
/*                                                            */
/**************************************************************/




#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "DateHandling.h"

void incrementModStartDate(Mods_everythingStruct *data)
{
   dateFieldStruct_p	controlData;
   
   XtVaGetValues(data->widgetData->startTimeTextF, XmNuserData, 
                 &controlData, NULL);
                 
   controlData->dateStruct = data->ModDates->StartDate;
   
   controlData->CBFuncs->increment(data->widgetData->startTimeTextF, 
                                   controlData, NULL);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/incrementModStartDate.c,v $";
 static char rcs_id2[] = "$Id: incrementModStartDate.c,v 1.2 1998/04/08 11:16:45 page Exp $";}
/*  ===================================================  */

}
