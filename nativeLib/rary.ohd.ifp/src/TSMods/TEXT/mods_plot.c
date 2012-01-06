/* mods_plot.c: display and modify additional time series.
*/
/*AiV 4/30/04 added codes to handle horizontal scaling for UHGCHDATE mod
/*                        and UHGCHNG mod
*/
#include "mods_plot.h"
#include "ifp_atoms.h"
#include "libXs.h"
#include "ifp_globals.h"
#include "Mods_everythingStruct.h"
#include "c_call_f/fcitzc.h"
#include "c_call_f/julda.h"

/******************************************************************************************/
extern int UhgFlag;/*is UHGCDATE plot in display?*/
void mods_plot(
	char     *currentModName,              /* Run-time Mod name                    */
	date     *tsStartDate,                 /* Start date of the run                */
	date     *tsEndDate,                   /* End date of the run                  */
	date     *tsEndObsDate,                /* End date of observed data            */
	int      num_ts,                       /* Number of time series                */
	float    *p_float,                     /* p_array for UHGCHNG                  */
	char     p_char_array[][4],
	float    *d_float,                     /* d_array for ROCHNG & RRICHNG         */
	float    *ts_float,                    /* ts_array needed for findts           */
	char     **ts_char_array,              /* ts_char array for 'findts'           */
	mod_data *the_mods_data,               /* mods data to get the UH array index  */
	Widget   plot_button_control,          /* widget for plot_button */
	Mods_everythingStruct *data            /* Pointer Mods_everythingStruct */
	) 
{
  Atom          type;
  int           num_ts_sel, mod_type_sw;
  char          *tsid[MAX_TSMOD_OPER];
  int           uh_index, uh_start_index, unit_sw;
  float         *start_ts[MAX_TSMOD_OPER]; /* pointer to the first location of each time
					      series to be plotted
					      start_ts[time series index][next value in sequence]
					    */
  int           dt;
  int           array_index;
  int           end, end_obs, num_pts;
  int           jul_day_end, jul_hr_end, jul_day_end_obs, jul_hr_end_obs;
  int           dlsdum, jul_day_st, jul_hr_st, zondum = 1;
  int           i, j, k;
  int           format, nitems, left;
  long          atom_offset = 0;
  int           num_mp_ts_color;
  int           len;    /* holds length of currentSegment string */
  static float  **mp_ts_array, **mp_orig_ts_array;
  char          **day_hr, *myt;
  static char   *mp_ts_color[] = {"cyan", "magenta", "spring green"};
  char          *currentSegment;
  Widget        wUHGCDATE;
  

/* --------------------end of declarations-------------------------------------------*/

if (!strcmp(currentModName,"UHGCHDATE"))
/*  Set plot button on mods window to sensitive for UHGCHDATE*/
XtSetSensitive(plot_button_control, TRUE);
else
/*  Set plot button on mods window to insensitive */
XtSetSensitive(plot_button_control, FALSE);
data->modsPlotData->plot_button_control = plot_button_control;

  if(XGetWindowProperty
	(
	XtDisplay(global_toplevel),
	DefaultRootWindow(XtDisplay(global_toplevel)),
	IFPA_current_segment,
	atom_offset,
	(long) 9,
	FALSE,
	IFPA_current_segment_type,
	&type,
	&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&currentSegment
	) == Success && type == IFPA_current_segment_type)
	    {
	     ;
	    }
  else
	    {
	     currentSegment = (char *) malloc(9);
	     strcpy(currentSegment, "NoneYet");
	    }

len = strlen(currentSegment);
data->modsPlotData->seg_name = (char *) malloc(len+1);
memset(data->modsPlotData->seg_name, '\0', len+1);
strcpy(data->modsPlotData->seg_name, currentSegment);

/*data->modsPlotData->seg_name = currentSegment;*/

/* Get start, end, and end_obs data */

/* replace calls to julda2 with FCITZC and JULDA for y2k */
FCITZC(&zondum, &dlsdum, tsStartDate->time_zone);
JULDA(&jul_day_st, &jul_hr_st,
	&tsStartDate->month,
	&tsStartDate->day,
	&tsStartDate->year,
	&tsStartDate->hour,
	&zondum, &dlsdum, tsStartDate->time_zone);
	
FCITZC(&zondum, &dlsdum, tsEndDate->time_zone);
JULDA(&jul_day_end, &jul_hr_end,
	&tsEndDate->month,
	&tsEndDate->day,
	&tsEndDate->year,
	&tsEndDate->hour,
	&zondum, &dlsdum, tsEndDate->time_zone);
	
FCITZC(&zondum, &dlsdum, tsEndObsDate->time_zone);
JULDA(&jul_day_end_obs, &jul_hr_end_obs,
       &tsEndObsDate->month,
       &tsEndObsDate->day,
       &tsEndObsDate->year,
       &tsEndObsDate->hour,
       &zondum, &dlsdum, tsEndObsDate->time_zone);

data->modsPlotData->start_run = 24*(jul_day_st - 1) + jul_hr_st;
data->modsPlotData->end_run = 24*(jul_day_end - 1) + jul_hr_end;
data->modsPlotData->valid_run = 24*(jul_day_end_obs - 1) + jul_hr_end_obs;

set_unit_flags();
if (mods_general_units == 0)
   unit_sw = ENGLISH;
else
   unit_sw = METRIC;
/*AV - 4/15/04 added code for UHGCDATE */
mod_type_sw = -999;
if(strcmp(currentModName, "UHGCHNG")== 0)mod_type_sw = UH;
if(strcmp(currentModName, "UHGCDATE")== 0)mod_type_sw = UHD;
if(mod_type_sw == UH || mod_type_sw == UHD) /* If current Mod is UHGCHNG
                                               or UHGCDATE */
{
   
   num_ts_sel = 1;
/*AV 4/5/04 commented out - UHGCDATE 
   mod_type_sw = UH;
*/
   for(j = 0; j < num_ts; j++)        /*      Find which UH we're doing       */
      if(operations_number[j] == 1)
      {
	 /* set the chosen operation name */
	 data->modsPlotData->op_name[0] = (char*)malloc(sizeof(char)*9);
	 memset(data->modsPlotData->op_name[0], '\0', 9);
	 strncpy(data->modsPlotData->op_name[0],
		 the_mods_data->operation.name[j], 8);
	 break;
      }
   tsid[0] = (char*)malloc(sizeof(char)*30);
   memset(tsid[0], '\0', 30);
   strncpy(tsid[0], p_char_array[the_mods_data->operation.locp[j]-1 +1], 20);

   /* The following code is commented out.  Decided to display UH in the 
      units of the mods_general_units set above  - dp 8/26/94 
   */
   /* get units flag from p_float: 0=metric 1=English
      set unit_sw to agree with convention from universal tech: 1=metric,
	  0=English
  
   unit_sw = p_float[the_mods_data->operation.locp[j]-1 + 22];
   if(unit_sw == 1) unit_sw = ENGLISH;
   else unit_sw = METRIC;
   */
   
   uh_index = the_mods_data->operation.locp[j]-1;
   uh_start_index = p_float[uh_index + 21];
   uh_start_index += (uh_index - 1);       /*           Index for the 1st value    */
					   /*              in the p_float array    */
   start_ts[0] = &p_float[uh_start_index];
   dt = p_float[uh_index + 15];
   end = p_float[the_mods_data->operation.locp[j]-1 + 9];
   /* set end_obs to end for UH mod */
   end_obs = end;


}
else  /* If the current Mod is either ROCHNG or RRICHNG */
{
   if(strcmp(currentModName, "RRICHNG") == 0)
      mod_type_sw = RRICHNG;
   if(strcmp(currentModName, "ROCHNG") == 0)
      mod_type_sw = ROCHNG;
/*
 * The following 2 lines commented out because they are not used.
 * Problem found by CodeCenter.  gfs 7/28/92
 *
 *  mdata_start = 2;
 *  mnew_start = 2;
 */
   k = 0;
   /*printf("num_ts = %d\n", num_ts);*/
   for(j = 0; j < num_ts; j++)   /* Find which time series we're doing */
   {
      if(operations_number[j] == 1)
      {
	 get_time_series(currentModName,
			 the_mods_data->operation.name[j],
			 the_mods_data->operation.type[j],
			 &the_mods_data->operation.locp[j],
			 the_mods_data->time_series.id[j],
			 the_mods_data->time_series.datatype[j],
			 &the_mods_data->time_series.delta_t[j],
			 p_float,
			 p_char_array);                  
	 array_index = findts_c(
				the_mods_data->time_series.id[j],
				the_mods_data->time_series.datatype[j],
				the_mods_data->time_series.delta_t[j],
				ts_float,
				ts_char_array);
	 dt = the_mods_data->time_series.delta_t[j];
	 if(array_index == 0)
	 {
	    printf("The time series was not found\n");
	    return;
	 }
	 start_ts[k] = &d_float[array_index-1];

	 /* Find the number of time steps between the Start & End dates  */
	 end = (24*(jul_day_end - 1) + jul_hr_end -
		24*(jul_day_st -  1) - jul_hr_st)/dt + .001;

	 if(end <= 0)
	 {
	    printf("end <= 0\n");
	    break;
	 }

	 end_obs = (24*(jul_day_end_obs - 1) + jul_hr_end_obs -
		    24*(jul_day_st -  1) - jul_hr_st)/dt + 0.001;
	 if(end_obs > end)
	    end_obs = end;

	 /* set chosen operation names */
	 data->modsPlotData->op_name[k] = (char*)malloc(sizeof(char)*9);
	 memset(data->modsPlotData->op_name[k], '\0', 9);
	 strncpy(data->modsPlotData->op_name[k],
		 the_mods_data->operation.name[j], 8);

	 k++;
      }
   }
      num_ts_sel = k;
}
/* ----------------------------------- end of temp.----------------------------------------------*/

num_pts = end + 1;

/* Set flag for type of precipitation input time series */
if(mod_type_sw == RRICHNG)
   for(i=0; i<num_ts_sel; i++)
      if(strncmp(the_mods_data->time_series.datatype[i], "RAIM", 4) == 0)
      {
	 data->modsPlotData->px_type_flag = RAIM;
	 break;
      }
      else if(strncmp(the_mods_data->time_series.datatype[i], "MAP ", 4) == 0)
	 data->modsPlotData->px_type_flag = MAP;
else
   data->modsPlotData->px_type_flag = (int)NULL;

/*  Allocate space for the ts_array and orig_ts_array data arrays  */
mp_ts_array      = (float **)malloc((num_ts_sel) * sizeof(float *));
mp_orig_ts_array = (float **)malloc((num_ts_sel) * sizeof(float *));
for(i=0; i<num_ts_sel; i++)
{
   mp_ts_array[i]      = (float *)malloc(num_pts * sizeof(float));
   mp_orig_ts_array[i] = (float *)malloc(num_pts * sizeof(float));
}

mp_Create_ts_array(num_ts_sel, mp_ts_array, mp_orig_ts_array,
		   end, start_ts, mod_type_sw, unit_sw, the_mods_data);

/*  Allocate space for the x_axis label array and fill it */
day_hr = (char **)malloc(num_pts * sizeof(char *));
for(i=0; i<num_pts; i++)
   day_hr[i] = (char *)malloc(8 * sizeof(char));
myt = (char *)malloc(13 * sizeof(char));

mp_date_hr(myt, dt, day_hr, num_pts, jul_day_st, jul_hr_st,
	   tsStartDate->time_zone, mod_type_sw);

/* Allocate space for ts_color pointers */
data->modsPlotData->ts_color = (char **)malloc((num_ts_sel) *
			  sizeof(char *));
for(i = 0; i < num_ts_sel; i++)
{
   data->modsPlotData->ts_color[i] = (char*)malloc(sizeof(char)*20);
}
data->modsPlotData->legend = (Widget*)malloc(num_ts_sel*sizeof(Widget));

/* Assign values for ts_color arrays */
num_mp_ts_color = XtNumber(mp_ts_color);
for(i = 0; i < num_ts_sel; i++)
   data->modsPlotData->ts_color[i] = 
      (i < num_mp_ts_color) ? mp_ts_color[i] : mp_ts_color[i%num_mp_ts_color];

data->modsPlotData->ts_array = mp_ts_array;
data->modsPlotData->orig_ts_array = mp_orig_ts_array;
data->modsPlotData->num_ts_sel = num_ts_sel;
data->modsPlotData->end_obs = end_obs;
data->modsPlotData->num_pts = num_pts;
data->modsPlotData->mod_type_sw = mod_type_sw;
data->modsPlotData->max_y = 0.0;
data->modsPlotData->min_y = 0.0;
data->modsPlotData->max_x = end;
data->modsPlotData->min_x = 0.0;
data->modsPlotData->day_hr = day_hr;
data->modsPlotData->unit_sw = unit_sw;
data->modsPlotData->ts_index = 0;
data->modsPlotData->ipt = 1000;
data->modsPlotData->start_end_sw = START;
data->modsPlotData->ts_change_flag = START;
data->modsPlotData->myt = myt;
data->modsPlotData->delta_t = dt;

/*---------------------------------------------------------------*/
/*AiV 4/30/04 store timeseries data to a temp variables.  Use these
 values to plot data when horizon scaling is changed 
*/  
data->modsPlotData->Orgend_obs = data->modsPlotData->end_obs;
data->modsPlotData->Orgnum_pts = data->modsPlotData->num_pts;
data->modsPlotData->Orgmax_x   = data->modsPlotData->max_x;

data->create_flag = 1;
UhgFlag = 1;
/* ----------AiV end  4/30/04        -----------------------------*/


/*--------------------------------------------*/
/*  Call plot_mod to create and draw plot     */
/*--------------------------------------------*/
plot_mod(data);
   
/* raise the ifp_modsShell Widget to the top */
if(strcmp(currentModName, "UHGCDATE") == 0)  /* If current Mod is UHGCHNG */
{
  
  wUHGCDATE = data->widgetData->ifp_modsShell; 
  XFlush(XtDisplay(data->widgetData->ifp_modsShell));
  sleep(1);/*wait for the modsPlotData Widget finish drawing */
 
  XFlush(XtDisplay(data->widgetData->ifp_modsShell));
  
  XRaiseWindow(XtDisplay(data->widgetData->ifp_modsShell),
                       XtWindow(data->widgetData->ifp_modsShell));
  
  XFlush(XtDisplay(data->widgetData->ifp_modsShell));
}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/mods_plot.c,v $";
 static char rcs_id2[] = "$Id: mods_plot.c,v 1.10 2006/04/07 14:34:41 aivo Exp $";}
/*  ===================================================  */

} /* end of mods_plot */
