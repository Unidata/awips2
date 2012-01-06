#include "GeneralUtil.h"
#include "HvDisplayControl.h"
#include "HvDisplayControlProto.h"
#include "hv_refreshTimer.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_presets.h"
#include "pointcontrol_show.h"
#include "PointDisplayControl.h"
#include "TokenizeOptionString.h"

/*********************************************************************

   This file contains functions for initializing the hydroview
   display, including the point display and the areal display.

*********************************************************************/


/*********************************************************************/
/*   utility function for getting the static data in the display     */
/*   control structure, in lieu of having it as a global variable    */
/*********************************************************************/

HvDisplayControl * getHvDisplayControl ( )
{
   static HvDisplayControl hvDisplayControl;
   return & hvDisplayControl;
}


/*********************************************************************/
/* This function should only be called once at startup,
   when the Point Data is not suppressed. */

void initHvDisplayControl ( Widget top_widget ,
                            HvDisplayControl * hdc ,
                            void display_routine ( ) ,
                            int add_refresh_timer )
{
   char header[] = "initHvDisplayControl(): ";
   int retrieval_required = 1 ;
   int status ;
   OptionValuePair * pOptionValueHead = NULL ;
   PointDataPresets * pPresetsHead = NULL ;
   PointDataPresets * pPresetsNode = NULL ;

   /* Set the user-supplied display routine that will be used to
      plot the station information on a map.  This is the routine that
      will be called from the point control GUI "Map Data" button. */
   //pc_SetDisplay ( display_routine ) ;

   /* Check the PointDataPresets table for predefined option sets. */
   pPresetsHead = get_PointDataPresetsList ( ) ;

   if ( pPresetsHead != NULL )
   {
      /* If there are some, get the highest ranking one. */
      pPresetsNode = ( PointDataPresets * ) ListFirst ( & pPresetsHead->list ) ;

      if ( pPresetsNode != NULL )
      {
         /* Parse this option set. */
         status = TokenizeOptionString ( pPresetsNode->preset_string ,
                                & pOptionValueHead ) ;

         if ( status != TOKENIZE_OPTION_OK )
         {
            fprintf ( stderr , "\nIn routine 'pc_PredefinedOptionSelectCB':\n"
                               "An error was encountered by "
                               "TokenizeOptionString.  Error code is %d.\n"
                               "Cannot process Preset Option id %s.\n" ,
                               status , pPresetsNode->preset_id ) ;
         }

      }
   }


   /* initialize the point control settings.  this needs to be done
      before the HvOptions are initialized, since some of the
      HvOptions use the pointcontrol settings for their values. */
   set_pc_options ( pOptionValueHead ) ;


   /*  Free the memory used by the linked list of option value pairs. */
   if ( pOptionValueHead != NULL )
   {
      FreeOptionValueList ( & pOptionValueHead ) ;
      pOptionValueHead = NULL ;
   }

   /* add refresh data time out */
   if ( add_refresh_timer == 1 )
   {
      /* get the point data.  the data is later loaded into the
         HydroView structure when the station point data are displayed. */
      printf("%s before pc_process_request \n", header);
      pc_process_request ( retrieval_required ) ;
      printf("%s after pc_process_request \n", header);
      addRefreshTimeOut ( top_widget ) ;
   }

   return ;
}

/*********************************************************************/

void initBackgroundDisplay(BackgroundDisplaySettings *bds)
{

   bds->showRivers     = True;
   bds->showReservoirs = True;

   bds->showMajorStreamsLakes = True;
   bds->showAllStreamsLakes   = False;

   bds->showBasins     = False;

   bds->showCounties   = False;
   bds->showZones      = False;
   bds->showCities     = False;

   bds->showMajorRoads = False;
   bds->showAllRoads   = False;

   bds->showRadars     = False;

   return;
}


/*********************************************************************/
/* this function is for the areal information */

void initHvProdlist(HvDisplayControl *hdc)
{
   char	logmsg[60];

   /* initialize for gui */

   hdc->displaySettings.areal.selectedItemPos = -1;


   /* read the desired products raw info from the database.
      this info is used later to build the Areal product list */

   hdc->displaySettings.areal.DesiredControls =
      readArealProductControlFromDb(&(hdc->displaySettings.areal.numDesiredControls));

   sprintf(logmsg, "numDesiredProducts read= %ld\n",
	   hdc->displaySettings.areal.numDesiredControls);
//   profileTime(logmsg);


   return;
}


/*********************************************************************/

void initHvOptions(HvDisplayControl *hdc)
{
   /* copy the settings from the point_control settings */

   transfer_pc_options(hdc);

   /* areal data options */

   hdc->displaySettings.areal.fillArea   = True;
   hdc->displaySettings.areal.showId     = False;
   hdc->displaySettings.areal.showName   = True;
   hdc->displaySettings.areal.showValue1 = True;
   hdc->displaySettings.areal.showValue2 = False;

   hdc->displaySettings.areal.comparisonType = COMPARE_RATIO;

   return;
}

/*********************************************************************/

void transfer_pc_options(HvDisplayControl *hdc)
{

   /* transfer the settings from the point control world
      into the hydroview world */

   pc_options_struct * pc_options = get_pc_options();

   hdc->displaySettings.point.showId   = pc_options->id;
   hdc->displaySettings.point.showName = pc_options->name;
   hdc->displaySettings.point.showTime = pc_options->time;

   hdc->displaySettings.point.showElevation = pc_options->elevation;
   hdc->displaySettings.point.showParamCode = pc_options->paramCode;


   /* if not displaying the value, then don't show the second
      value (flood level) either */

   hdc->displaySettings.point.showValue1 = pc_options->value;

   if (!pc_options->value)
      hdc->displaySettings.point.showValue2 = False;
   else
      hdc->displaySettings.point.showValue2 =
                         pc_options->fldlevel | pc_options->derive_stage_flow ;


   /* in the point control world, the icon setting is for
      show icon; in the hydroview world it means suppress icon,
      so flip-flop the values */

   if (pc_options->icon)
      hdc->displaySettings.point.suppressIcons = False;
   else
      hdc->displaySettings.point.suppressIcons = True;

   hdc->displaySettings.point.valueordepart = pc_options->valuetype;
   hdc->displaySettings.point.stagebasis    = pc_options->stagebasis;
   hdc->displaySettings.point.stageflow     = pc_options->derive_stage_flow;


   return;
}


/*********************************************************************/
/*  manage the retrieval and loading of the latest river data
   for use in coloring the icons */

void load_latest_river_data(HvDisplayControl *hdc)
{
   char header[] = "load_latest_river_data";
   pc_options_struct	pc_options_RIVER;
   ReportList		*riverHead;

   fprintf(stdout, "%s - I have been called \n", header);

   /* define the options as per the river request */


   initialize_options(& pc_options_RIVER);


   set_pc_options_river(&pc_options_RIVER);


   /* now make the request for the data */

   riverHead = pc_process_onetime(pc_options_RIVER);


   /* now load any returned data into the special report for the
      river data associated with each station */

   load_latest_river_reports(hdc, riverHead);


   /* can't forget to free the memory */

   FreeReportList(riverHead);


   if (pc_options_RIVER.type_source_chosen_array != NULL)
   {
       free(pc_options_RIVER.type_source_chosen_array);
       pc_options_RIVER.type_source_chosen_array = NULL;
   }

   return;
}


/*********************************************************************/
/* set up the options for the onetime request to get the
   latest river data used for coloring the icons */

void set_pc_options_river(pc_options_struct *pc_options_RIVER)
{
   static int 	first = 1;
   int   	i;
   int  gad_token_len = 0, gad_value_len = 0;
   char tokenstr[80];

   pc_options_struct * pc_options = get_pc_options();


   if (check_ShefProcObs())
      pc_options_RIVER->process_mode = PROC_AS_OBS;
   else
      pc_options_RIVER->process_mode = PROC_AS_PROC;

   pc_options_RIVER->process_selected = 0;


   pc_options_RIVER->element_type = RIVER_AD_HOC_TYPE;
   strcpy(pc_options_RIVER->selectedAdHocElementString, "xx");
   pc_options_RIVER->PCandPP = 0;
   pc_options_RIVER->Primary = 1;

   pc_options_RIVER->filter_by_typesource = 0;

   strcpy(pc_options_RIVER->type_source_chosen_array[0], "xx");
   pc_options_RIVER->type_source_chosen_count = 1;

   /* time mode settings. the duration is set later in this function. */

   pc_options_RIVER->time_mode = LATEST;

   time(&pc_options_RIVER->valid_timet);
   pc_options_RIVER->dur_hours = 24;

   /* Added by BAL on 1/2/02. The service backup filter. */
   pc_options_RIVER->filter_by_hsa = 0 ;

   /* data source filtering */

   pc_options_RIVER->filter_by_datasource = 0;
   pc_options_RIVER->data_sources_chosen_count = 0;
   for (i = 0; i < pc_options_RIVER->data_sources_chosen_count; i++)
   {
      strcpy(pc_options_RIVER->data_sources_chosen_array[i], "dummy");
   }


   /* data filter settings. get the data for all locations,
      not just the forecast points. */

   pc_options_RIVER->suppress_missing = 0;
   /*
   pc_options_RIVER->suppress_zeros   = 0;
   */
   pc_options_RIVER->fcstpts_only     = 0;


   /* map display options */

   pc_options_RIVER->value = 1;
   pc_options_RIVER->id    = 1;
   pc_options_RIVER->name  = 0;
   pc_options_RIVER->time  = 0;
   pc_options_RIVER->icon  = 1;

   pc_options_RIVER->fldlevel = 0;
   pc_options_RIVER->valuetype = TYPE_VALUE;


   /* set the duration and the stagebasis based on the token settings
      initially, thereafter use the settings from the pc_options-> */

   /* set the duration and the stagebasis based on the token settings
      initially, thereafter use the settings from the pc_options-> */

   if (first)
   {
      gad_token_len = strlen("hv_riverbasis");
      get_apps_defaults("hv_riverbasis", &gad_token_len,
			tokenstr, &gad_value_len);
      if (gad_value_len <= 0)
	 pc_options->stagebasis = BASIS_MOFO;
      else
      {
	 if (strcmp(tokenstr, "obs") == 0)
	    pc_options->stagebasis = BASIS_OBS;

	 else if (strcmp(tokenstr, "fcst") == 0)
	    pc_options->stagebasis = BASIS_FCST;

	 else if (strcmp(tokenstr, "maxobsfcst") == 0)
	    pc_options->stagebasis = BASIS_MOFO;

	 else
	 {
	    fprintf(stderr, "invalid river basis defined %s; using default.\n",
		    tokenstr);
	    pc_options->stagebasis = BASIS_MOFO;
	 }
      }

      gad_token_len = strlen("hv_durhours");
      get_apps_defaults("hv_durhours", &gad_token_len, tokenstr, &gad_value_len);

      if (gad_value_len <= 0)
	 pc_options->dur_hours = 24;
      else
	 pc_options->dur_hours = atoi(tokenstr);

      first = 0;
   }
   else
   {
      pc_options_RIVER->dur_hours  = pc_options->dur_hours;
      pc_options_RIVER->stagebasis = pc_options->stagebasis;
   }

   return;
}


/*********************************************************************/

int check_if_rivertype()
{
   int	result;

   pc_options_struct * pc_options = get_pc_options();

   /* river data is being displayed if */

   if (pc_options->element_type == RIVER_AD_HOC_TYPE ||
       (pc_options->process_selected &&
	(pc_options->selectedAdHocElementString[0] == 'H' ||
	 pc_options->selectedAdHocElementString[0] == 'Q')))
      result = 1;
   else
      result = 0;

   return(result);

}

/*********************************************************************/
