#include "libXifp.h"
#include "globals.h"
#include "ifp_inc/Date.h"
#include "ifp_atoms.h"
#include "struct_defs.h"
#include "c_call_f/number_of_fgroups.h"
#include "c_call_f/read_fcstandco_groupnames.h"
#include "c_call_f/set_ofs_lock.h"
#include "c_call_f/free_ofs_lock.h"

#define USE_PREVIOUS_IFP_FILES          1
#define COPY_CURRENT_OFS_FILES          2


extern void reset_interface_components(the_widget_struct *);

/* Routine to parse out the command line arguments and
 * set necessary window properties to allow startup
 * of IFP_Map without bringing up the Forecast Group
 * selection box or start_dates.
 */

int handle_command_line_args(int num_args, char **args, 
                             the_widget_struct *some_widgets)
{
   date      start_run_date, end_obs_date, end_run_date;
   char      tmp[5];
   char      forecastGroup[9];
   char      forecastGroup_no_blanks[9];
   Display   *display;
   Window    root;
   char      script_command[80];
   char      lock_type[6];       /* holds type of lock (read or write) */
   int       icond;              /* flag for condition set by lock program */
   int       len, len2;          /* length of strings for get_apps_defaults */
   int       whichDataToGet;     /* flag */
   int       numFGs;             /* Number of Forecast Groups...                */
   char      *fgIDs;             /* Forecast Group IDs, 8-characters long, max. */
   char      *cgIDs;             /* Carryover Group IDs, 8-characters long, max.*/
   char      *FGname;
   int       use_ok;
      
/* parse first argument - start run date */
   memset(tmp, '\0', 5);
   strncpy(tmp,&args[1][0],2);      
   start_run_date.month = atoi(tmp);
   strncpy(tmp,&args[1][2],2);
   start_run_date.day = atoi(tmp);
   strncpy(tmp,&args[1][4],4);
   start_run_date.year = atoi(tmp);
   memset(tmp, '\0', 5);
   strncpy(tmp,&args[1][8],2);
   start_run_date.hour = atoi(tmp);
   memset(start_run_date.time_zone, '\0', 5);
   memset(start_run_date.time_zone, ' ', 4);
   strncpy(start_run_date.time_zone,&args[1][10],
           strlen(&args[1][10]));

/* parse second argument - end obs date */
   memset(tmp, '\0', 5);
   strncpy(tmp,&args[2][0],2);      
   end_obs_date.month = atoi(tmp);
   strncpy(tmp,&args[2][2],2);
   end_obs_date.day = atoi(tmp);
   strncpy(tmp,&args[2][4],4);
   end_obs_date.year = atoi(tmp);
   memset(tmp, '\0', 5);
   strncpy(tmp,&args[2][8],2);
   end_obs_date.hour = atoi(tmp);
   memset(end_obs_date.time_zone, '\0', 5);
   memset(end_obs_date.time_zone, ' ', 4);   
   strncpy(end_obs_date.time_zone,&args[2][10],
           strlen(&args[2][10]));

/* parse third argument - end run date */
   memset(tmp, '\0', 5);
   strncpy(tmp,&args[3][0],2);      
   end_run_date.month = atoi(tmp);
   strncpy(tmp,&args[3][2],2);
   end_run_date.day = atoi(tmp);
   strncpy(tmp,&args[3][4],4);
   end_run_date.year = atoi(tmp);
   memset(tmp, '\0', 5);
   strncpy(tmp,&args[3][8],2);
   end_run_date.hour = atoi(tmp);
   memset(end_run_date.time_zone, '\0', 5);
   memset(end_run_date.time_zone, ' ', 4);   
   strncpy(end_run_date.time_zone,&args[3][10],
           strlen(&args[3][10]));

/* handle fourth argument - forecast group */
   memset(forecastGroup, '\0', 9);
   memset(forecastGroup, ' ', 8);
   strncpy(forecastGroup, args[4], strlen(args[4]));
   
   /* Check to make sure it's a valid forecastGroup -
    * take code to figure out names from 
    * make_ForecastGroup_selectionBox
    */
    
   /* Find out how many forecast groups there are so we 
    * can malloc space for them
    */
    NUMBER_OF_FGROUPS(&numFGs);

    fgIDs = (char *) malloc(8 * numFGs + 1);
    cgIDs = (char *) malloc(8 * numFGs + 1);
    
    memset(fgIDs, '\0', 8 * numFGs + 1);
    memset(cgIDs, '\0', 8 * numFGs + 1);
    
    READ_FCSTANDCO_GROUPNAMES(fgIDs, cgIDs);
    FGname = strstr(fgIDs, forecastGroup);        
    if((strncmp(FGname, forecastGroup, 8) != 0) || (FGname == (char *) NULL))
    {
       printf("Invalid Forecast Group = %s\n", forecastGroup);
       return(1);
    }   
          
   
/* handle fifth argument - whichDataToGet */
   if(strncmp("PREV", args[5], 4) == 0)
   {
      memset(forecastGroup_no_blanks, '\0', 9);
      strncpy(forecastGroup_no_blanks, args[4], strlen(args[4]));
      /* add check to see if it's legal to use previous files - dp 23 Sept. 97 */
      use_ok = ok_use_previous_files(FGname);
      if(use_ok)
         whichDataToGet = USE_PREVIOUS_IFP_FILES;
      else
      { 
         printf("Mods file for %s doesn't exist.  Need to copy files\n", forecastGroup_no_blanks);
         printf("Copying OFS files will begin");
         whichDataToGet = COPY_CURRENT_OFS_FILES;
      }
   }
   else
      whichDataToGet = COPY_CURRENT_OFS_FILES;
     
/* Now post all the proper atoms */
   display = XtDisplay(global_toplevel);
   root = DefaultRootWindow(display);

   XChangeProperty
        (
        display,
        root,
        IFPA_run_start_date,
        IFPA_run_start_date_type,
        8,
        PropModeReplace,
        (unsigned char *)&start_run_date,
        sizeof(date)
        );

   XChangeProperty
	(
	display,
	root,
	IFPA_run_end_obs_date,
	IFPA_run_end_obs_date_type,
	8,
	PropModeReplace,
	(unsigned char *)&end_obs_date,
	sizeof(date)
	);
 
   XChangeProperty
	(
	display,
	root,
	IFPA_run_end_date,
	IFPA_run_end_date_type,
	8,
	PropModeReplace,
	(unsigned char *)&end_run_date,
	sizeof(date)
        );
	    
   XChangeProperty
        (
        display,
        root,
        IFPA_time_zone_code,
        IFPA_time_zone_code_type,
        8,
        PropModeReplace,
        start_run_date.time_zone,
        strlen(start_run_date.time_zone)
        );
         
   XChangeProperty
        (
        display,
        root,
        IFPA_forecast_group,
        IFPA_forecast_group_type,
        8,
        PropModeReplace,
        forecastGroup,
        strlen(forecastGroup)
        );

   XFlush(display);
   
   /* Now call all the proper scripts and programs to get started */
   revert_to_default_view(some_widgets->main_canvas, some_widgets, NULL);
   
   /*  The following was from handleFGSelection callback */
   FGBasin_ID = (char **)map_areas_in_fg(forecastGroup, &NumBasinsInCurrFcstGroup);
   strcpy(some_widgets->selected_ForecastGroupName, forecastGroup);
   XmToggleButtonSetState(some_widgets->FcstGroup_widget, TRUE, FALSE); 
   add_overlays(some_widgets->main_canvas, some_widgets, NULL);   
   XtVaSetValues(some_widgets->ForecastGroup_label,
                 XtVaTypedArg, XmNlabelString, XmRString, 
                 some_widgets->selected_ForecastGroupName,
	         strlen(some_widgets->selected_ForecastGroupName)+1,
	         NULL);
   
   /*  The following was from load_FGroup_data callback */
   XtSetSensitive(some_widgets->begin_widget, TRUE);
   XtSetSensitive(some_widgets->showDeletedSegments_widget, TRUE);
   XtSetSensitive(some_widgets->showFGroup_Topology_widget, TRUE);
   XtSetSensitive(some_widgets->setDates_widget, TRUE);
   XtSetSensitive(some_widgets->techniques_widget, TRUE);

   XtRemoveEventHandler(some_widgets->main_canvas, ButtonPressMask, FALSE,
   		     select_ForecastGroup, some_widgets);
   XtAddEventHandler(some_widgets->main_canvas, ButtonPressMask, FALSE,
   		  select_basin, some_widgets);

   /* In the future the 'State' & 'Notify' arguments will be set from              */
   /* a user Preferences structure...                                              */
   XmToggleButtonSetState(some_widgets->showFGroup_Topology_widget, TRUE, FALSE);

   XFlush(display);
         
   /* call routine to get the path name for script files */
   memset(script_command, '\0', 80);
   len = strlen("ifp_scripts_dir");
   get_apps_defaults("ifp_scripts_dir", &len, script_command, &len2);
   strcat(script_command, "/get_ofs_data");

   if(whichDataToGet == COPY_CURRENT_OFS_FILES)
   { /*--------------------------------------------------------------------*/
     /* Call SET_OFS_LOCK to put a lock on the ofs files to be copied.     */
     /*    If successful (icond=0); continue.                              */
     /*    If not (icond>0); stop the program.                             */
     /*--------------------------------------------------------------------*/

      memset(lock_type, '\0', 6);
      strcpy(lock_type, "read");
      SET_OFS_LOCK(lock_type, &icond);
      if(icond > 0)
      {
         printf("Exiting IFP_Map: could not get copy of OFS files\n");
         exit(1);
      }
      system(script_command);
      FREE_OFS_LOCK(&icond);
   }

   /* call routine to get the path name for script files */
   memset(script_command, '\0', 80);
   len = strlen("ifp_scripts_dir");
   get_apps_defaults("ifp_scripts_dir", &len, script_command, &len2);

   strcat(script_command, "/start_ifp_map_script");

   system(script_command);
      
   create_FcstGroup_schematic(forecastGroup, some_widgets);
   zoom_forecastGroup(some_widgets);
   reset_interface_components(some_widgets); 

   return(0);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/handle_command_line_args.c,v $";
 static char rcs_id2[] = "$Id: handle_command_line_args.c,v 1.5 2006/04/07 13:30:05 aivo Exp $";}
/*  ===================================================  */

}
