/**************************************************************/
/*                                                            */
/*	FILE:		reset_dates.c	                      */
/*                                                            */
/*                                                            */
/*	Coded by:	D. Page                               */
/*			NWS * Office of Hydrology * HRL       */
/*	Date:		11/10/94                              */
/*                                                            */
/*                                                            */
/**************************************************************/


#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "ifp_atoms.h"

/*  Routine to reset all dates to their original values.
 *  This is called from setModInterfaceElements at the start
 *  of each mod to make sure dates are set correctly.
 */
 
extern date   *GetDate(Display *, Atom, Atom);
extern char   *ConvertMonthToString(int);
extern void   TextFieldDisplayDate(date *, char *, Widget, Widget,
			           Widget , Widget , Widget);
extern void   incrementModStartDate(Mods_everythingStruct *);
 
void reset_dates(Mods_everythingStruct *data)
{
   Display    *display;
   char       *monthString;

   display = XtDisplay(data->widgetData->ifp_modsShell);
   
   /* Reset the StartDate for this mod */
   data->ModDates->StartDate = GetDate(display, IFPA_run_start_date,               /*  3  */
 	                               IFPA_run_start_date_type);
   monthString = ConvertMonthToString(data->ModDates->StartDate->month);
 	
   /*------------------------------------------------------*/
   /* Put the date & time in the Widgets			*/
   /*------------------------------------------------------*/
   TextFieldDisplayDate(data->ModDates->StartDate, monthString,
                        data->widgetData->startMonthTextF,
                        data->widgetData->startDayTextF,
                        data->widgetData->startYearTextF,
                        data->widgetData->startTimeTextF,
                        data->widgetData->startTZoneLabel);

   /* Update the StartDate for this mod if necessary */                            
   if(data->selectedModDef->modStartDateOffset != 0)
      incrementModStartDate(data);
      
    /* Reset the EndDate for this mod */
   data->ModDates->EndDate = GetDate(display, IFPA_run_end_date,
 	                               IFPA_run_end_date_type);
   monthString = ConvertMonthToString(data->ModDates->EndDate->month);
 	
   /*------------------------------------------------------*/
   /* Put the date & time in the Widgets	           */
   /*------------------------------------------------------*/
   TextFieldDisplayDate(data->ModDates->EndDate, monthString,
                        data->widgetData->endMonthTextF,
                        data->widgetData->endDayTextF,
                        data->widgetData->endYearTextF,
                        data->widgetData->endTimeTextF,
                        data->widgetData->endTZoneLabel);

      
    /* Reset the ValidDate for this mod */
   data->ModDates->ValidDate = GetDate(display, IFPA_run_end_obs_date,
 	                               IFPA_run_end_obs_date_type);
   monthString = ConvertMonthToString(data->ModDates->ValidDate->month);
 	
   /*------------------------------------------------------*/
   /* Put the date & time in the Widgets		   */
   /*------------------------------------------------------*/
   TextFieldDisplayDate(data->ModDates->ValidDate, monthString,
                        data->widgetData->validMonthTextF,
                        data->widgetData->validDayTextF,
                        data->widgetData->validYearTextF,
                        data->widgetData->validTimeTextF,
                        data->widgetData->validTZoneLabel);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/reset_dates.c,v $";
 static char rcs_id2[] = "$Id: reset_dates.c,v 1.1 1995/11/14 12:19:52 page Exp $";}
/*  ===================================================  */

}
