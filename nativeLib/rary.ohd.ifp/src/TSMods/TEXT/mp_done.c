#include "mods_plot.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
/* AiV add global variables to properly close the UHG, ROCHNG, RICCHNG and 
/* UHGCDATE when user does not click on "done" button before selecting
/* another mod 5/03/04
*/
extern int UhgFlag;/*is UHGCDATE plot in display?*/
void mp_done(Widget w, Mods_everythingStruct *data, 
             XmAnyCallbackStruct *call_data)
{
   int       i;                       /* Counter */
   int       j;                       /* Counter */
   int       time_series_change_made; /* flag */
   int isValidDate;

if (!strcmp(data->selectedModDef->name,"UHGCDATE"))
{
isValidDate=isModDatesValid(data);
if (isValidDate < 0)
{	showInvalidDateErrorMsg(w,isValidDate);
	return ;
}
}
   time_series_change_made = FALSE;
   /* Ai added 05/02/2004 */
   data->modsPlotData->end_obs = data->modsPlotData->Orgend_obs;
   data->modsPlotData->num_pts = data->modsPlotData->Orgnum_pts;
   data->modsPlotData->max_x   = data->modsPlotData->Orgmax_x;

   /* set the ts_mod_made flag if a change has been made */
   for(i=0; i<data->modsPlotData->num_ts_sel; i++)
   {
      for(j=0; j<data->modsPlotData->num_pts-1; j++)
	 if(data->modsPlotData->ts_array[i][j] != 
	    data->modsPlotData->orig_ts_array[i][j])
	 {
	    time_series_change_made = TRUE;
	    break;
	 }
      if(time_series_change_made == TRUE) break;
   }

  /*
   * if a time series was changed, call routine to create the mods
   */
   if(time_series_change_made == TRUE)
      createSelectionCB(w, data, NULL); 

/*when isModDatesValid(data) >= 0, it is an invalid mod and we don't want to
*pop down the [mod plot: ...] window.
*/
   /* Reset the plot button toggle to sensitive and to off */
   XtSetSensitive(data->modsPlotData->plot_button_control, TRUE);
   XmToggleButtonSetState(data->modsPlotData->plot_button_control, FALSE, FALSE);
   if(data->modsPlotData->main_plot_shell != NULL)
       XtDestroyWidget(data->modsPlotData->main_plot_shell);
   data->modsPlotData->main_plot_shell = NULL; 
 
   /* AiV add global variables to properly close the UHG,ROCHNG, RICCHNG and 
   /* UHGCDATE when user does not click on "done" button before selecting
   /* another mod 5/03/04
   */
   data->create_flag = 0;
   data->mp_doneClick = 1;
   UhgFlag = 0;
  
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/mp_done.c,v $";
 static char rcs_id2[] = "$Id: mp_done.c,v 1.4 2004/08/05 17:53:20 wkwock Exp $";}
/*  ===================================================  */

}















