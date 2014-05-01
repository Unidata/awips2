/****************************************************************/
/*								*/
/*	FILE:		Mods_showTSPlot.c			*/
/*								*/
/*	NOTE:							*/
/*								*/
/*	Creates and pops-up a Time-series change window for	*/
/*	the current Mod (if applicable); calls 'mods_plot()'	*/
/*	- taken from what was formerly 'mods_subs.c'		*/
/*								*/
/*	Coded by:	Tom Adams				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		11/21/94				*/
/*								*/
/*	Modified by:    D. Page - 2 Nov 1995			*/
/*			passed data to mods_plot		*/
/*								*/
/****************************************************************/



#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"


extern void UhgCDateClickCB(Widget, Mods_everythingStruct *, XmListCallbackStruct*);
extern void mods_plot(char *, date *, date *, date *, int,
		 	float *, char **, float *, float *, char **,
			mod_data *, Widget, mods_plot_struct *);
XmListCallbackStruct *curpos;

extern int uhgFlagToggle,CURIDX;/*global variables that set in initValidInterfaceElements CURIDX=cur ts ind*/
 
void showTSPlot(Widget w, Mods_everythingStruct *data)
{

	int			NumOperations;

        curpos = (XmListCallbackStruct *)malloc(sizeof(XmListCallbackStruct));

	data->modsPlotData = (mods_plot_struct *)XtMalloc(sizeof(mods_plot_struct));

	data->modsPlotData->num_pts = 0;
	
	XtVaGetValues(data->widgetData->timeSeriesList, XmNitemCount, &NumOperations, NULL);

	mods_plot(data->ModName,			/* Run-time Mod name		*/
		  data->ModDates->StartDate,		/* Start date of the run	*/
		  data->ModDates->EndDate,		/* End date of the run		*/
		  data->ModDates->ValidDate,		/* End observed date		*/
		  NumOperations,			/* Num. of time series avail.	*/
		  data->ofsData->p_float_array,		/* p_float array		*/
		  data->ofsData->p_char_array,		/* p_char array			*/
		  data->ofsData->d_float_array,		/* d_float arra			*/
		  data->ofsData->ts_float_array,	/* ts_float array		*/
		  data->ofsData->ts_char_array,		/* ts_char array		*/
		  data->ModSettings,			/* Pointer to 'mod_data' struct	*/
		  w,					/* Widget for plot button	*/
		  (mods_plot_struct *)data);			        /* Pointer Mods_everythingStruct*/
                  
    /*Handle toggle show plot button when mpdone window is clicked */         
    if( strcmp(data->selectedModDef->name,"UHGCDATE") == 0 && uhgFlagToggle == 1 ){
     
        curpos->item_position = CURIDX;
        UhgCDateClickCB(data->widgetData->datesValueSList,data,curpos);
        
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_showTSPlot.c,v $";
 static char rcs_id2[] = "$Id: Mods_showTSPlot.c,v 1.3 2006/04/18 15:28:35 aivo Exp $";}
/*  ===================================================  */

}

