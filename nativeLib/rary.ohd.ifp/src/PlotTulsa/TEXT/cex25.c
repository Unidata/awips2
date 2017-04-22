/*
 *File: cex25.c
 *
 * Plots time series data determining the proper scales, data ranges,
 * and labels.
 *
 * Allows for user segment selection.
 *
 * Uses P and D arrays .
 *
 * Allocate space for data arrays.
 *
 * Calls FORTRAN program fdcode to determine if missing values
 * are allowed for the datatype (type_string).
 *
 */


#include "cex25.h"
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <Xm/Xm.h>
#include "ifp_atoms.h"

#include "plot.h"
#include "menus.h"
#include "ifp_struct.h"
#include "libXifp.h"

#include "event_loop_exit_status.h"
#include "techniques.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

/*--Added by AV --*/
#include "c_call_f/rcfset.h"
#include "c_call_f/fctzc.h"
#include "c_call_f/fserch.h"
#include "c_call_f/umovex.h"
#include "c_call_f/fdcode.h"
/*--end Added by AV --*/

#define NO_STATUS       0
#define NORMAL          1
#define ALERT           2
#define FLOOD           3



extern	SEL_RANGE range_selected;
extern void fill_ts_lengths (int*, int*, float[], int*, TS_INFO *, plot_cb_struct *);
extern Mods_everythingStruct *ntsmod();
extern int event_loop(tables_cb_struct *, plot_cb_struct *, Widget,
	       float*, char*[], int*, float*, float*, Mods_everythingStruct *);

extern int valid_mods_for_oper();
extern void post_nts_mods_available_atom(Widget,int);
void            get_warning_flow();
Widget          global_toplevel;

/* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> */
int cex25(p_float, p_char, ts_float, ts_char, locp, rc_float, rc_char, rc_int,
    d_float, d_locts, start_run, end_obs_data, end_run, first_data_day,
    t_array, first_time, event_loop_exit_status, mp, c_float)

	char                    p_char[][4];    /* Parameter character data   */
	char                    ts_char[][4];   /* Time series character data */
	char                    rc_char[][4];   /* rating curve character array */
	int                     rc_int[];       /* rating curve interger value */
	int                     d_locts[];      /* location of the time series in the data array */
	int                     *locp;          /* pointer to the location of the beginning of
						   the parameter array. */
	int                     t_array[];      /* time series array */
	int                     *first_time;
	int                     *start_run;
	int                     *end_obs_data;
	int                     *end_run;
	int                     *first_data_day;
	int                     *event_loop_exit_status;
	int                     *mp;            /* maximum parameter array size */
	float                   ts_float[];     /* time series floating point array */
	float                   rc_float[];     /* rating curve floating point array */
	float                   d_float[];      /* data floating point array */
	float                   p_float[];      /* parameter data array */
	float                   c_float[];      /* carry over data array */

{
  Atom                  type;
  Widget		modShell;		/* toplevel (Dialog) Shell Widget for 'other Mods	*/
  Display		*display;
  char                  **tsid;                 /* time series id */
  char                  *myt;                   /* month, year, time zone pointer */
  char                  *plot_symbol[MAX_TS];
  char                  **ts_menu_names;        /* address of time series menu names pointer */
  char                  *currentSegment, *goto_downstream_segment;
  char                  time_zone_code[5], code_string[4];
  int                   type_index;             /* location of the data type in the time series array */
  int                   pr_index;               /* list, plot index location in the Tulsa parameter array */
  int                   list_mask[MAX_TS];
  int                   data_offset;
  int                   obs_mask[MAX_TS];       /* observation data plot mask */
  int                   plot_mask[MAX_TS];      /* time series plot mask */
  int                   type_count[MAX_TS];     /* data type count */
  int                   Its;                    /* time series index value */
  int                   i, j, k, l;             /* counters */
  int                   end;                    /* used to calculate total number of data points */
  int                   dt;                     /* Sample time interval */
  int                   num_ts;                 /* Number of time series used in the current forcast program */
  int                   end_obs;                /* end observations plot data point */
  int                   end_obs_px_ro;
  int                   num_ts_menus=0;         /* number of time series menus */
  int                   tul_locp, tul_locp2;    /* location of the start of tulsa parameter arrays */
  int                   *start_dis, *end_dis;
  int                   symbol_index;
  int                   data_len;
  int                   format, nitems, left;   /* window properties */
  int                   ts_index[MAX_TS];       /* time series index */
  int                   *start_month, *start_day, *start_hour;
  long                  atom_offset = 0;
  double                Ymin = 0.0, Ymax = 60.0;
  rc_struct             rc_data;                /* rating curve data structure */
  tables_cb_struct      tables_data;
  plot_cb_struct        plot_data;
  /*  some_display_widgets  the_display_widgets;  */
  combined_struct       ptm_data;               /* tables and plot data structure pointer */
  TS_INFO               ts_info[MAX_TS];        /* time series information structure */
  int                   total_run_hours;
  int                   num_table_rows;         /* number of data rows visible: passed to tultable */
  int                   num_table_cols;         /* number of data columns visible: passed to tultable */

  int                   locp_test;              /* pointer to the location of the beginning of the test
						   parameter array */
  int                   numop;                  /* number of tulsa plot operations */
  int                   num_plot_left;         /* number of tulsa plot & plot TS operations remaining */
  int                   rating_curve_in_plot;   /* rating curve plot flag */
  char                  nameout[8];

  char                  seg_status[8];          /* d array segment status */
  int                   num_rr_oper;            /* number of rainfall runoff operations */
  int                   *loc_px;                /* location of the first members of the px time series */
  int                   *loc_ro;                /* location of the first members of the ro time series */
  int                   num_rr_pts;             /* number of rainfall runoff data points */
  char                  **px_id;                /* px time series id */
  char                  **ro_id;                /* ro time series id */
  float                 **px, **ro;             /* address of the px and ro time series pointers */
  int                   *px_dt, *ro_dt;         /* px and ro time interval pointers */
  char                  type_string[4];
  char                  std_units[4], dimen[4], time_scale[4];
  int                   missing_allowed;        /* Indicates if missing data are allowed for
						   this data type in the forecast component. */
  int                   nv_dt;                  /* number of values per time interval for this data type. */
  int                   nadd;                   /* Number of pieces of additional information */
  int                   err_flag;               /* Error flag, 0 no error, 1 indicates not a valid data type */
  univ_techniques_struct *univ_tech;
  float                 nbase;                  /* plot base value, lowest ploted value */

  int                   minimum_scale_increment;
  float                 minimum_maximum_discharge;

  char                  currentSegmentNoBlanks[9];
  char                  goto_downstream_segmentNoBlanks[9];

  int                   len, len2;              /* to hold string lengths for get_apps_defaults */
  char                  a_num_cols[10];         /* to hold ascii number value from get_apps_defaults */
  int                   found;                  /* flag to hold return value of get_apps_defaults */
  int                   total_num_plots;        /* total number of Tulsa Plot operations in segment */
  int                   *first_plot;            /* flag for first Tulsa plot in segment */
  int                   num_nts_mods;           /* number of non time series mods in segment */
  int                   nts_mods_available;     /* flag for if nts_mods are available in segment */
  int                   novrsn;                 /* Tul Plot version number */
  int                   increment_step;         /* move 12 18 positions depend upon version number */
  int                   data_len_plot, data_len2;
  int                   data_len_table, dt_table;
  int                   dt_ts, dt_plot;         /* Time series time interval */
  int                   over_head;
  char                  operation_name[9];
  int                   rc_flag=0;
  int                   end_obs_table;          /* Number of obs for the table */
  int                   for_plot;               /* flag for if labels are for the plot or table */
  Mods_everythingStruct	*data;
  int                   current_plot_num,      /* Number of the plot in the current segment */
                        rerun_plot_num;        /* Number of the plot that rerun was selected from */
  int                   *tschngMod;            /* Flag for if tschng mod has been made */
  int                   *mod_files_updated;    /* Flag for if mod files have been changed */
  int                   *fgmod_files_updated;    /* Flag for if fgmod files have been changed */
  int                   is_save_gif_on;        /* Flag for setting of save_gif_file atom */
  int                   activateRerunFlag;     /* Flag used to set the IFPA_activate_rerun atom */
  int             	*FGmods_save;   /* save fgroup_mods data */

  /* <><><><><><><><><><><><><><><>END OF DECLARATIONS><><><><><><><><><><><><><><><><><><><><><><><><><><> */


  for(i = 0; i < MAX_TS; i++)
     list_mask[i] = obs_mask[i] = plot_mask[i] = ts_index[i] = 0;

  for(i = 0; i < MAX_TS; i++)
     tables_data.end[i] = -1;

  for(i = 0; i < MAX_TS; i++)
     tables_data.delta_t[i] = -1;

  start_month = (int*)malloc(sizeof(int));
  start_day   = (int*)malloc(sizeof(int));
  start_hour  = (int*)malloc(sizeof(int));

  first_plot        = (int*)malloc(sizeof(int));

  memset(seg_status, '\0', 8);

  if((*first_time)++ < 1)crwdgt();

  set_unit_flags();

  /* set flag if rating curve shift mods in effect */
  RCFSET(&rc_flag);
  plot_data.h_q_plot_indicator = 0;

  display = XtDisplay(global_toplevel);

  if(XGetWindowProperty
	(
	display,
	DefaultRootWindow(display),
	IFPA_current_segment,
	atom_offset,
	(long) 9,
	FALSE,
	(Atom)IFPA_current_segment_type,
	(Atom *) &type,
	(int *)&format,
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
/*
 *  See if goto_downstream_segment atom is set
 */
  if(XGetWindowProperty
	(
	display,
	DefaultRootWindow(display),
	IFPA_goto_downstream_segment,
	atom_offset,
	(long) 9,
	FALSE,
	(Atom)IFPA_goto_downstream_segment_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&goto_downstream_segment
	) == Success && type == IFPA_goto_downstream_segment_type)
	    {
	    /*
	     * If goto_downstream atom is set see if the current
	     *  segment is the downstream segment.
	     *
	     * First copy ids into new variables, removing
	     *  trailing blanks.
	     */
	     memset(currentSegmentNoBlanks, '\0', 9);
	     memset(goto_downstream_segmentNoBlanks, '\0', 9);
	     for(i = 0; i < 8; i++)
		{
		 if(currentSegment[i] == ' ' ||
		    currentSegment[i] == '\0')     break;
		 else   currentSegmentNoBlanks[i] = currentSegment[i];
		}
	     for(i = 0; i < 8; i++)
		{
		 if(goto_downstream_segment[i] == ' ' ||
		    goto_downstream_segment[i] == '\0')     break;
		 else   goto_downstream_segmentNoBlanks[i] = goto_downstream_segment[i];
		}

	     if(strcmp(currentSegmentNoBlanks,
		       goto_downstream_segmentNoBlanks) == 0)
	       {
	     /*
	      * If so, remove the goto_downstream_segment atom
	      *  and continue through cex25.
	      */
		XDeleteProperty(display,
				DefaultRootWindow(display),
				IFPA_goto_downstream_segment);
	       }
	     else
	       {
	     /*
	      * If goto_downstream_segment atom is on and current
	      *  segment does not match downstream segment,
	      *  make sure that current segment is computationally
	      *  upstream of the current one.  We may have gone past
	      *  the chosen downstream segment if it didn't have
	      *  any Tulsa plot operations in it.  If the chosen downstream
	      *  segment is upstream of the current one, treat it as if the
	      *  names match.  If the chosen downstream segment is still
	      *  downstream of the current one, set event_loop_exit_status
	      *  to Control_skipping_Tulsa_plot event_loop_exit_status
	      *  and return.
	      */
		char  *run_segments, tempNoBlanks[9];
		int   number_of_run_segments;

		XGetWindowProperty
		      (
		      display,
		      DefaultRootWindow(display),
		      IFPA_run_segments,
		      atom_offset,
		      (long) 801,
		      FALSE,
		      (Atom)IFPA_run_segments_type,
		      (Atom *)&type,
		      (int *)&format,
		      (unsigned long *)&nitems,
		      (unsigned long *)&left,
		      (unsigned char **)&run_segments
		      );
		number_of_run_segments = strlen(run_segments) / 8;

		for(i = 0; i < number_of_run_segments; i++)
		   {
		    memset(tempNoBlanks, '\0', 9);
		    k = 0;
		    for(j = i*8; j < (i*8)+8; j++)
		       {
			if(run_segments[j] == ' ' ||
			   run_segments[j] == '\0')     break;
			else   tempNoBlanks[k++] = run_segments[j];
		       }
		    if(strcmp(tempNoBlanks,
			      currentSegmentNoBlanks) == 0)
		      {
		     /*
		      * Found current segment first in computational list.
		      * Still need to continue downstream to chosen segment
		      *  without going through Tulsa plot.
		      *
		      * The following code (to the return statement)
		      *  matches the code at the end of this function
		      *  used for NEXT or CONTINUE.
		      * This structure matches the one in run_partial.h
		      */
		     typedef struct
			{
			 char      segment_name[9];
			 int       status_id;
			}  segment_status_struct;

		     segment_status_struct segment_status;

		     strcpy(segment_status.segment_name, currentSegment);

		     /*  Set flow level status of the segment just completed  */
		     if(p_float[*locp-1 + 1] > 0.5)
		       {
			ptm_data.seg_name = currentSegment;
			rc_info(p_float, rc_float, rc_int, rc_char, locp, &rc_data);

			get_warning_flow(&rc_data);

			segment_status_from_d_array(currentSegment,
					   rc_data.warning_flow,
					   rc_data.flood_flow,
					   ts_float, ts_char, d_float,
					   seg_status);

			for(i = 0; i < 8; i++)
			   {
			    if(seg_status[i] == ' ')
			      {
			       seg_status[i] = '\0';
			       break;
			      }
			   }

			if(strcmp(seg_status, "Normal") == 0)
						segment_status.status_id = NORMAL;
			else if(strcmp(seg_status, "Alert") == 0)
						segment_status.status_id = ALERT;
			else if(strcmp(seg_status, "Flood") == 0)
						segment_status.status_id = FLOOD;
			else segment_status.status_id = NO_STATUS;
		       }
		     else
		       {                         /* No rating curve - unknown status */
			segment_status.status_id = NO_STATUS;
		       }
/*
 * printf("in cex25, for segment %s, status of %d\n",
 *         segment_status.segment_name, segment_status.status_id);
 */
		     XChangeProperty(
				     display,
				     DefaultRootWindow(display),
				     IFPA_segment_status,
				     IFPA_segment_status_type,
				     8,
				     PropModeReplace,
				     (unsigned char *)&segment_status,
				     sizeof(segment_status_struct)
				    );

		       XFlush(display);

		       *event_loop_exit_status =
					Control_skipping_Tulsa_plot;
		       return 0;
		      }
		    else if(strcmp(tempNoBlanks,
				   goto_downstream_segmentNoBlanks) == 0)
		      {
		     /*
		      * Found chosen downstream segment first in
		      *  computational list.  Delete goto_downstream_segment
		      *  atom and go through Tulsa plot.
		      */
		       XDeleteProperty(display,
				       DefaultRootWindow(display),
				       IFPA_goto_downstream_segment);
		       i = number_of_run_segments;
		      }
		    else
		      {
		       ;   /* just continue through list of run segments */
		      }
		   }   /* end for (i = 0, number_of_run_segments) */
	       }   /* end else (currentSegment != goto_downstream_segment) */
	    }   /* end if (goto_downstream_segment exists) */

/*
 * Get time zone code from Universal Techniques atom.
 */
  if(XGetWindowProperty
	(
	display,
	DefaultRootWindow(display),
	IFPA_univ_techniques,
	atom_offset,
	(long) sizeof(univ_techniques_struct),
	FALSE,
	(Atom)IFPA_univ_techniques_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&univ_tech
	) == Success && type == IFPA_univ_techniques_type)
  {
     memset(time_zone_code, '\0', 5);
     FCTZC(&(univ_tech->output_time_zone),
	   &(univ_tech->output_daylight_savings),
	   code_string);
     strncpy(time_zone_code, code_string, 4);
  }
  else
     strncpy(time_zone_code, "Z   ", 4);

/*
 *  See if there are any plot-tul operations after this one.
 *  This will determine whether 'Next' or 'Continue' buttons
 *   are sensitive in Run_NWSRFS Control menu.
 */

  numop = 25;
  num_plot_left = -1;
  locp_test = *locp;
  while(locp_test > 0)
       {
	FSERCH(&numop, nameout, &locp_test, p_float, p_char, mp);
	num_plot_left++;
       }
 /*find all plot TS left*/

  numop = 18;
  locp_test = *locp;
  num_plot_left--;
  while(locp_test > 0)
       {
	FSERCH(&numop, nameout, &locp_test, p_float, p_char, mp);
	num_plot_left++;
       }


  XChangeProperty(
	  display,
	  DefaultRootWindow(display),
	  IFPA_number_of_TulPlots,
	  IFPA_number_of_TulPlots_type,
	  8,
	  PropModeReplace,
	  (unsigned char *)&num_plot_left,
	  sizeof(int)
	  );

/* check to see if this is the first plot of the run
   set IFPA_tot_num_Tulplots if it is - dp 07/27/95
*/
  if(XGetWindowProperty
      (
      display,
      DefaultRootWindow(display),
      IFPA_first_plot,
      atom_offset,
      (long) sizeof(int),
      FALSE,
      (Atom)IFPA_first_plot_type,
      (Atom *)&type,
      (int *)&format,
      (unsigned long *)&nitems,
      (unsigned long *)&left,
      (unsigned char **)&first_plot
      ) == Success && type == IFPA_first_plot_type)
  {
     if(*first_plot)
     {
        total_num_plots = num_plot_left + 1;
        XChangeProperty(
	     display,
	     DefaultRootWindow(display),
	     IFPA_tot_num_TulPlots,
	     IFPA_tot_num_TulPlots_type,
	     8,
	     PropModeReplace,
	     (unsigned char *)&total_num_plots,
	     sizeof(int)
	     );
      }
  }
  else
     printf(" first_plot atom not posted");

/* Check to see if the number of the current tulsa plot
 * is before the number of the plot they were in when
 * Rerun was chosen from the menu.  Used to rerun the segment
 * to the plot-tul where mod was made.  dp - 26 Feb. 1997
 * Add code to first check to see if the save_gif option
 * is set to TRUE.  If it is, don't run thru to the rerun plot.
 */
  is_save_gif_on = get_save_gif_atom(global_toplevel );
  current_plot_num = get_plot_num(global_toplevel);
  rerun_plot_num   = get_rerun_plot_num_atom(global_toplevel);
  if(rerun_plot_num > 0 && (is_save_gif_on == FALSE))
  {
     if(current_plot_num < rerun_plot_num)
     {
        *event_loop_exit_status = Control_CONTINUE;
        *first_plot = FALSE;
        post_first_plot_atom(first_plot);
        activateRerunFlag = TRUE;
	XChangeProperty(
		display,
		DefaultRootWindow(display),
		IFPA_activate_rerun,
		IFPA_activate_rerun_type,
		8,
		PropModeReplace,
		(unsigned char *)&activateRerunFlag,
		sizeof(int)
		);
        return 0;
     }

     /* reset rerun_plot_num_atom */
     if(current_plot_num == rerun_plot_num)
     {
        rerun_plot_num = 0;
        post_rerun_plot_num_atom(&rerun_plot_num);
     }
  }

  rating_curve_in_plot = FALSE;

  if(p_float[*locp-1 + 1] > 0.5)
      rating_curve_in_plot = TRUE;

  XChangeProperty(
	  display,
	  DefaultRootWindow(display),
	  IFPA_rating_curve_available,
	  IFPA_rating_curve_available_type,
	  8,
	  PropModeReplace,
	  (unsigned char *)&rating_curve_in_plot,
	  sizeof(int)
	  );

  ptm_data.seg_name = currentSegment;
  rc_info(p_float, rc_float, rc_int, rc_char, locp, &rc_data);
  novrsn = p_float[*locp-1];
  dt = p_float[*locp-1 + 7];
  memset(operation_name,'\0',9);
  i = ((*locp - 6) * 4) + 1;
  j = 1;
  k = 8;
  UMOVEX(p_char, &i, operation_name, &j, &k);
  start_dis = (int*)malloc(sizeof(int));
  *start_dis = *start_run + dt;         /* Start display at end of first time interval. */
  end_dis = (int*)malloc(sizeof(int));
  *end_dis = *end_run;
  end = ((*end_run - *start_run)/dt + 0.01);
  data_len = end+1;                     /* actual number of data points */
  total_run_hours = *end_run - *start_run;
  plot_data.total_run_hours = total_run_hours;
  nbase = p_float[*locp-1 + 13];

  num_ts = p_float[*locp-1 + 8];

  /* Code added for Version 2 of PLOT-TUL operation - to plot time series at
   * different delta t
   * Restructured code and made changes to get correct dt_table, data_len_table
   * dt_plot, and data_len_plot.   dp - 7 Jan. 99
   */
  if (novrsn == 2)
  {
     if((p_float[*locp-1 + 3] > 60.0) || (p_float[*locp-1 + 3] < 60.0 && p_float[*locp-1 + 9] < 4))
           over_head = 30;
        else
           over_head = 43;

     /* initialize variables */
     dt_plot = dt_table = 0;
     data_len_plot = data_len_table = 0;

     for (j=0; j<num_ts; j++)
     {
        dt_ts = p_float[*locp-1+over_head+12+(j*18)];
        data_len2 = ((*end_run - *start_run)/dt_ts + 0.01) + 1;

        pr_index = *locp-1 + over_head + 5 + (j*18);
        if (p_char[pr_index][0] == 'P' || p_char[pr_index][0] == 'B')
        {
           if (data_len2 > data_len_plot)
              data_len_plot = data_len2;
           if (dt_plot != 0)
           {
              if (dt_ts < dt_plot)
                 dt_plot = dt_ts;
           }
           else
              dt_plot = dt_ts;
        }
        if (p_char[pr_index][0] == 'L' || p_char[pr_index][0] == 'B')
        {
           if (data_len2 > data_len_table)
              data_len_table = data_len2;
           if (dt_table != 0)
           {
              if (dt_ts < dt_table)
                 dt_table = dt_ts;
           }
           else
              dt_table = dt_ts;
        }
     }
     /* Add in checks to initialize dt_plot and data_len_plot or dt_table
      * and dt_len_table if no plotted or no listed time series.  Assume
      * if there's no plotted, there is listed and vice versa - dp 8 May 1999
     */
     if (dt_plot == 0)
     {
	dt_plot = dt_ts;
        data_len_plot = data_len_table;
     }
     if (dt_table == 0)
     {
	dt_table = dt_ts;
      	data_len_table = data_len_plot;
     }
  }
  else  /* novrsn=1 */
  {
     data_len_plot = data_len;
     data_len_table = data_len;
     dt_plot = dt;
     dt_table = dt;
  }


/*  Allocate space for the day_hour (menu) and day_hrs (axes) arrays */
  day_hour = (char **)malloc((data_len_table) * sizeof(char *));
  day_hrs  = (char **)malloc((data_len_plot) * sizeof(char *));
  myt = (char *)malloc(13 * sizeof(char));

  for(i=0; i<data_len_table; i++)
  {
     day_hour[i] = (char *)malloc(8 * sizeof(char));
  }
  for(i=0; i<data_len_plot; i++)
  {
     day_hrs[i] = (char *)malloc(8 * sizeof(char));
  }

/* date_hr rewritten to work on one set of labels at a time
   dp - 24 Jan 1997
*/
  for_plot = TRUE;
/* Fill the day_hrs array for the plot */
  date_hr(myt, dt_plot, day_hrs, &data_len_plot, for_plot,
	     end_dis, locp, p_float, start_run,
	     start_month, start_day, start_hour, time_zone_code);
/* Fill the day_hour array for the table */
  for_plot = FALSE;
  date_hr(myt, dt_table, day_hour, &data_len_table, for_plot,
	     end_dis, locp, p_float, start_run,
	     start_month, start_day, start_hour, time_zone_code);


  data_offset = ((run_start_hr - file_start_hr)/dt - 0.99);

  end_obs = (*end_obs_data - *start_run)/dt_plot;

  /* The following line is a TEMPORARY fix for calculating end_obs
     for the tultable when all time series are the same length.
     dp - 24 Jan. 1997   updated by ddt - Mar. 1997
     PERMANENT fix: dp - 18 Apr. 97
  */
  end_obs_table = (*end_obs_data - *start_run)/(dt_table);

  plot_data.end_obs = end_obs;

 /*
  * Units of minimum_scale_increment are cfs.
  * Units of minimum_maximum_discharge are cms.
  */
  minimum_scale_increment = p_float[*locp-1 + 5];
  if((p_float[*locp-1 + 3] > 60.0) ||   /* plot is 101 cols wide */
     (p_float[*locp-1 + 3] < 60.0 && p_float[*locp-1 + 9] < 4)) /* plot is 51 cols && < 4 ts listed */
    {
     tul_locp = 30;
     minimum_maximum_discharge = 10.0 * minimum_scale_increment / 35.308;
    }
  else                                  /* plot is 51 columns wide */
    {
     tul_locp = 43;
     minimum_maximum_discharge =  5.0 * minimum_scale_increment / 35.308;
    }
  tul_locp2 = tul_locp;                 /* tul_locp saved for use in MakeTSInfoIndex */

/*  Allocate space for the tsid (time series identification) array  */
  tsid = (char **)malloc((num_ts) * sizeof(char *));
  for(i=0; i<num_ts; i++)
     tsid[i] = (char *)malloc(14 * sizeof(char));
  Make_ts_name(novrsn, num_ts, tsid, p_char, locp, tul_locp);

  for(i = 0; i < num_ts; i++)
	{
	type_count[i] = 0;
	type_check[i] = 0;
	}

 /*
  *  Call to find information about time series
  */
  FindTSInfo(novrsn, ts_float, ts_char, ts_info, p_char, p_float, locp, tul_locp2,
             num_ts, dt);

  fill_ts_lengths(start_run, end_run, p_float, locp, ts_info, &plot_data);

/*  Allocate space for the ts_array and orig_ts_array data arrays  */

  ts_array      = (float **)malloc((num_ts) * sizeof(float *));
  orig_ts_array = (float **)malloc((num_ts) * sizeof(float *));

  for(i=0; i<num_ts; i++)
  {

       ts_array[i]      = (float *)malloc(plot_data.end[i] * sizeof(float));

       orig_ts_array[i] = (float *)malloc(plot_data.end[i] * sizeof(float));
  }


  Create_ts_array(num_ts, ts_array, d_float, d_locts,
		  data_offset, end, orig_ts_array, ts_info, start_run, end_run, novrsn);

  find_num_rrm(p_float, p_char, ts_float, ts_char, *mp, &num_rr_oper);

  loc_px = (int *) malloc(num_rr_oper * sizeof(int));
  loc_ro = (int *) malloc(num_rr_oper * sizeof(int));

  find_rrm_io_ts(p_float, p_char, ts_float, ts_char, *mp, &num_rr_oper,
		 loc_px, loc_ro);

  if(num_rr_oper > 0)
    {
   /*  Allocate space for the px, ro data arrays */
     px_id = (char **)malloc(num_rr_oper * sizeof(char *));
     ro_id = (char **)malloc(num_rr_oper * sizeof(char *));
     for(i=0; i<num_rr_oper; i++)
     {
	px_id[i] = (char *)malloc(14 * sizeof(char));
	ro_id[i] = (char *)malloc(14 * sizeof(char));
     }
     px_dt = (int *)malloc(num_rr_oper * sizeof(int));
     ro_dt = (int *)malloc(num_rr_oper * sizeof(int));

     px = (float **)malloc(num_rr_oper * sizeof(float *) + 1 );
     ro = (float **)malloc(num_rr_oper * sizeof(float *) + 1 );

     fill_px_ro_ts(ts_float, ts_char, d_float, px, ro, total_run_hours,
		   num_rr_oper, loc_px, loc_ro, &num_rr_pts,
		   px_id, px_dt, ro_id, ro_dt);

     if (num_rr_oper > 0)
        {
           for (l=0;l<num_rr_oper;l++)
               {
                  dt = *px_dt;
                  end_obs_px_ro = (*end_obs_data - *start_run)/dt;
               }
        }

     plot_data.end_obs_px_ro = end_obs_px_ro;
     plot_data.num_rr_oper = num_rr_oper;
     plot_data.num_rr_pts  = num_rr_pts;
     plot_data.px_id       = px_id;
     plot_data.ro_id       = ro_id;
     plot_data.px          = px;
     plot_data.ro          = ro;

    }
  else
    {     /* No rrm operations in the current segment */
     plot_data.num_rr_oper = 0;
     plot_data.num_rr_pts  = 0;
     plot_data.px_id       = NULL;
     plot_data.ro_id       = NULL;
     plot_data.px          = NULL;
     plot_data.ro          = NULL;

    }
  /* Find out which ts are to be plotted or listed and record it
     with plot_mask or list_mask. */
  for(i = 0; i < num_ts; i++)
  {
	pr_index = *locp-1 + tul_locp + 5;
	type_index = tul_locp + *locp +1;
	symbol_index = *locp-1 + tul_locp + 6;
	plot_symbol[i] = (char*)malloc(3);
	memset(plot_symbol[i], '\0', 3);
	strncpy(plot_symbol[i], p_char[symbol_index], 1);
	Its = i;

	if(p_char[pr_index][0] == 'L' || p_char[pr_index][0] == 'B')
	{
	   list_mask[i] = 1;
	   if (novrsn == 2)
	   {
              tables_data.delta_t[num_ts_menus] = p_float[*locp-1 + tul_locp + 12];
              tables_data.end[num_ts_menus] =
                 ((*end_run - *start_run)/tables_data.delta_t[num_ts_menus] + 0.01) + 1;
           }
           else
           {
              tables_data.delta_t[num_ts_menus] = p_float[*locp-1 + 7];
              tables_data.end[num_ts_menus] = (data_len_table - 1) + 1;
           }

	   num_ts_menus++;
	}
	else
	   list_mask[i] = 0;

	strncpy(type_string, p_char[type_index], 4);

	/* Call FORTRAN program fdcode to determine if missing values
	are allowed for the datatype (type_string).  If allowed,
	(missing_allowed = 1) then the time series is an observed one.
	If not allowed, the time series does not have observed data.
	*/
	FDCODE(type_string, std_units, dimen, &missing_allowed, &nv_dt,
	       time_scale, &nadd, &err_flag);

	if((missing_allowed == 1) &&
	   (p_char[pr_index][0] == 'P' || p_char[pr_index][0] == 'B'))
	   {
	    obs_mask[i] = PLOT;
	   }
	else
	   obs_mask[i] = NOPLOT;
	if((p_char[pr_index][0] == 'P' || p_char[pr_index][0] == 'B') &&
	    obs_mask[i] == NOPLOT)
	    {
	     plot_mask[i] = PLOT;
             if(strncmp(dimen, "L   ", 4) == 0)  /* If a pool elevation */
                minimum_maximum_discharge = 0.0; /* plot, do not use    */
            }                                    /* min_scale value.    */
	else
	    plot_mask[i] = NOPLOT;

	/* if rc shift mod in effect, determine type of plot 1=Q, 2=H */
	if(rc_flag>0 && (obs_mask[i]==PLOT || plot_mask[i]==PLOT) ){
	   if(strncmp(dimen, "L3/T", 4) == 0){
	      plot_data.h_q_plot_indicator = 1;
	   }
	   else
	       if(strncmp(dimen, "L   ", 4) == 0) {
	          plot_data.h_q_plot_indicator = 2;
	       }
	}
        increment_step=12;
        if (novrsn == 2)
        {
           increment_step = 18;
        }
        tul_locp = tul_locp + increment_step;
/*      tul_locp = tul_locp + 12; */
  }

/* Set the discharge label data type variable */
  memset(plot_data.disch_label_data_type, '\0', 4);

  for(i=0; i<num_ts; i++)
     if( (plot_mask[i] == PLOT) || (i == num_ts-1) )
     {
	strncpy(plot_data.disch_label_data_type, ts_info[i].data_type, 4);
	break;
     }

/* Only make the ts_menu_names if num_ts_menus > 0 - dp - 11 Oct.96 */

  if(num_ts_menus > 0)

  {
     ts_menu_names = (char **)malloc((num_ts_menus) * sizeof(char *));
     for(i=0; i<num_ts_menus; i++)
        ts_menu_names[i] = (char *)malloc(14 * sizeof(char));
     Make_ts_menu_names(num_ts, tsid, list_mask, ts_menu_names, ts_index);
  }

  ptm_data.tables = &tables_data;
  tables_data.list_ts_index = ts_index;
  ptm_data.plot = &plot_data;

  if(data_len_table <= 13)
     num_table_rows = data_len_table-1;
  else
     num_table_rows = 13;

/* See if apps_default set for number of columns - if it is, set
   num_table_cols to that number (if that many are available)
   if not, set num_table_cols to default of 2 (if that many are available).
*/
  len = strlen("ifp_num_columns");
  if((found=get_apps_defaults("ifp_num_columns", &len, a_num_cols, &len2)) == 0)
  {
     /* apps_default was set */
     int valid;   /* to hold result from isdigit */

     /* check to see if a_num_cols contains all numbers */
     for(i=0; i<len2; i++)
        if( (valid = isdigit(a_num_cols[i])) == 0)
        {
           printf("WARNING!! ifp_num_columns apps_default has invalid characters.\n");
           printf("It will be temporarily reset\n");
           printf("Please fix the value for ifp_num_columns in your .Apps_defaults file\n");
           num_table_cols = num_ts_menus;
           break;
        }

     /* a_num_cols does not overflow the array size and a_num_cols valid */
     if (len2 < 10 && valid != 0)
     {
        num_table_cols = atoi(a_num_cols);

        if(num_table_cols < 0 || num_table_cols > MAX_TS)
        {
           printf("WARNING!! ifp_num_columns apps_default set to invalid number of %d\n",
                  num_table_cols);
           printf("It will be temporarily reset\n");
           printf("Please fix the value for ifp_num_columns in your .Apps_defaults file\n");
           num_table_cols = num_ts_menus;
        }
        else    /* check to see if requesting more columns than there is data */
           if(num_table_cols > num_ts_menus)
              num_table_cols = num_ts_menus;
     }
     else if(valid != 0)   /* a_num_cols does overflow the array size with digits */
     {
        printf("WARNING!! ifp_num_columns apps_default set an undetermined invalid value\n");
        printf("It will be temporarily reset but IFP may have problems!!\n");
        printf("Please fix the value for ifp_num_columns in your .Apps_defaults file\n");
        num_table_cols = num_ts_menus;
     }
  }
  else if (num_ts_menus < 2)    /* apps_default not set */
     num_table_cols = num_ts_menus;
  else                          /* default: 2 columns */
     num_table_cols = 2;

  tables_data.ts_info = ts_info;
  tables_data.tultable = NULL;
  tables_data.num_ts_menus = num_ts_menus;  /* added to fix problem when
                                               num_table_cols=0 and tultable
                                               not called - dp - 22 May 96
                                             */
  tables_data.ts_menu_names = ts_menu_names;  /* added 24 Oct. - see above */


/* Set the tables_data.table_created flag - added for fix when num_table_cols=0 and
   tultable not called.
*/
  if (num_table_cols > 0)
  {
     tables_data.table_created = TRUE;
     tultable(num_ts_menus, data_len_table-1, num_table_cols, num_table_rows, ts_menu_names,
	      day_hour, ts_array, orig_ts_array, list_mask, num_ts,
	      &tables_data, &ptm_data, end_obs_table);
  }
  else if(num_table_cols == 0)
     tables_data.table_created = FALSE;

  /* initialize some variables used in the rubberbanding to
     clean up uninitialized memory reads found with purify.
     Must be done here not in start_rubber_band. dp - 12 July 95
  */
  plot_data.cpt.x = 0;
  plot_data.cpt.y = 0;

  tulplot(ts_array, tsid, plot_symbol, day_hrs, plot_mask, obs_mask,
	  &num_ts, data_len_plot, orig_ts_array, myt, &rc_data, &plot_data,
	  &ptm_data, minimum_maximum_discharge, ts_info, nbase, operation_name);

  data = (Mods_everythingStruct *)ntsmod(p_float, p_char, ts_float, ts_char, d_float, t_array,
                currentSegment, global_toplevel);

  /* Add code to check for the number of non-time series mods in the segment.
   * dp - 15 Feb. 1996
   */
  num_nts_mods = valid_mods_for_oper(p_float, p_char, ts_float, ts_char,
                                     NULL, -1, "ALL");

  if (num_nts_mods > 0)
  {
     nts_mods_available = TRUE;
     post_nts_mods_available_atom(global_toplevel, nts_mods_available);

     *event_loop_exit_status =
	     event_loop(&tables_data, &plot_data, data->widgetData->ifp_modsMessageBox,
			p_float, p_char, t_array, ts_float, c_float, data);

  }
  else
  {
     nts_mods_available = FALSE;
     post_nts_mods_available_atom(global_toplevel, nts_mods_available);
     *event_loop_exit_status =
	     event_loop(&tables_data, &plot_data, plot_data.main_plot_shell,
			p_float, p_char, t_array, ts_float, c_float, data);
  }

     if(*event_loop_exit_status == Control_QUIT)
       {
	if(num_plot_left > 0)
	  {                     /* leave without saving output time series */
	   *event_loop_exit_status = Control_QUIT_NO_SAVE;
	  }
       }

  if(*event_loop_exit_status == Control_RERUN)
    {
        /*  Get values of some window properties before going on */
      	if(XGetWindowProperty
	   (
	    display,
	    DefaultRootWindow(display),
	    IFPA_tschng_mod,
	    atom_offset,
	    (long) 9,
	    FALSE,
	    (Atom)IFPA_tschng_mod_type,
	    (Atom *)&type,
	    (int *)&format,
	    (unsigned long *)&nitems,
	    (unsigned long *)&left,
	    (unsigned char **)&tschngMod
	   ) == Success && type == IFPA_tschng_mod_type)
	 {
	    ;
	 }
	 else
	 {
            tschngMod  = (int*)malloc(sizeof(int));
	    *tschngMod = FALSE;
	 }

	 if(XGetWindowProperty
	    (
	     display,
	     DefaultRootWindow(display),
	     IFPA_mod_files_updated,
	     atom_offset,
             (long) 9,
	     FALSE,
	     (Atom)IFPA_mod_files_updated_type,
	     (Atom *)&type,
	     (int *)&format,
	     (unsigned long *)&nitems,
	     (unsigned long *)&left,
	     (unsigned char  **)&mod_files_updated
	     ) == Success && type == IFPA_mod_files_updated_type)
	 {
            ;
         }
	 else
	 {
	    /* No IFPA_mod_files_updated property, set to FALSE...               */
	    mod_files_updated = (int *)malloc(sizeof(int));
	    *mod_files_updated = FALSE;
	 }

    /*
     *  Call to bring up display to set keywords for changed time series.
     */
     if(*tschngMod == TRUE)
        fill_tschng_keywords(p_float, p_char, ts_float, ts_char, t_array,
		     first_time, ts_info, &ptm_data, start_run, end_obs_data,
		     locp, data);

     /* Add call to post the number of the current plot-tul in the
      * segment if a tschng mod or other mod has been made.  Used
      * to rerun the segment to the plot-tul where mod was made.
      * Only post if a mod has been made (ts or non-ts) or if one
      * of the mod files has been modified.
      *  dp - 26 Feb. 1997
      */

     if((data->ModIndex > 0) || (*tschngMod == TRUE) ||
        (*mod_files_updated == TRUE))
        post_rerun_plot_num_atom(&current_plot_num);


     /* write out the new mods to a file */
     FillDecPlaceArray();
     Write_mods(data);
	/* reset range index to max: show_mods and Mods_mainWinPushBCallbacks use this value*/
     range_selected.min_range=range_selected.last;


    }   /* end of Control_RERUN */

/******************************************************************/

  else if(*event_loop_exit_status == Control_NEXT        ||
	  *event_loop_exit_status == Control_GO_UPSTREAM)
    {
     /*
      * This structure matches the one in run_partial.h
      */

     typedef struct
	{
	 char      segment_name[9];
	 int       status_id;
	}  segment_status_struct;

     segment_status_struct segment_status;


      /* check to see if a write for FG mods posted*/
      if(XGetWindowProperty
	(
		display,
		DefaultRootWindow(display),
      		IFPA_FGmods_save,
      		atom_offset,
      		(long) sizeof(int),
      		FALSE,
      		(Atom)IFPA_FGmods_save_type,
      		(Atom *)&type,
      		(int *)&format,
      		(unsigned long *)&nitems,
      		(unsigned long *)&left,
      		(unsigned char **)&FGmods_save
      		) == Success && type == IFPA_FGmods_save_type)
  		{
      			if(XGetWindowProperty
	   		(
	    		display,
	    		DefaultRootWindow(display),
	    		IFPA_tschng_mod,
	    		atom_offset,
	    		(long) 9,
	    		FALSE,
	    		(Atom)IFPA_tschng_mod_type,
	    		(Atom *)&type,
	    		(int *)&format,
	    		(unsigned long *)&nitems,
	    		(unsigned long *)&left,
	    		(unsigned char **)&tschngMod
	   		) == Success && type == IFPA_tschng_mod_type)
	 		{
	    		;
	 		}
	 		else
	 		{
        		    tschngMod  = (int*)malloc(sizeof(int));
			    *tschngMod = FALSE;
	 		}

	 		if(XGetWindowProperty
	    		(
	     		display,
	     		DefaultRootWindow(display),
	     		IFPA_fgmod_files_updated,
	     		atom_offset,
             		(long) 9,
	     		FALSE,
	     		(Atom)IFPA_fgmod_files_updated_type,
	     		(Atom *)&type,
	     		(int *)&format,
	     		(unsigned long *)&nitems,
	     		(unsigned long *)&left,
	     		(unsigned char **)&fgmod_files_updated
	     		) == Success && type == IFPA_fgmod_files_updated_type)
	 		{
            		;
         		}
	 		else
	 		{
	    		/* No IFPA_mod_files_updated property, set to FALSE...               */
	    		fgmod_files_updated = (int *)malloc(sizeof(int));
	    		*fgmod_files_updated = FALSE;
	 		}

    			if((data->ModIndex > 0) || (*tschngMod == TRUE) ||
        			(*fgmod_files_updated == TRUE))
        			post_rerun_plot_num_atom(&current_plot_num);

    			FillDecPlaceArray();
    			Write_mods((Mods_everythingStruct *)data);
  		*FGmods_save  = (int )malloc(sizeof(int));
  		*FGmods_save = FALSE;
		/* reset range index to max: show_mods and Mods_mainWinPushBCallbacks use this value*/
     		range_selected.min_range=range_selected.last;
                } /* end if FGmods_save */

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
     strcpy(segment_status.segment_name, currentSegment);

     /*  Set flow level status of the segment just completed  */
     if(rc_data.RC == TRUE)
       {
	segment_status_from_d_array(currentSegment,
			   rc_data.warning_flow,
			   rc_data.flood_flow,
			   ts_float, ts_char, d_float,
			   seg_status);

	for(i = 0; i < 8; i++)
	   {
	    if(seg_status[i] == ' ')
	      {
	       seg_status[i] = '\0';
	       break;
	      }
	   }

	if(strcmp(seg_status, "Normal") == 0)
				segment_status.status_id = NORMAL;
	else if(strcmp(seg_status, "Alert") == 0)
				segment_status.status_id = ALERT;
	else if(strcmp(seg_status, "Flood") == 0)
				segment_status.status_id = FLOOD;
	else segment_status.status_id = NO_STATUS;
       }
     else
       {                         /* No rating curve - unknown status */
	segment_status.status_id = NO_STATUS;
       }

    XChangeProperty(
		    display,
		    DefaultRootWindow(display),
		    IFPA_segment_status,
		    IFPA_segment_status_type,
		    8,
		    PropModeReplace,
		    (unsigned char *)&segment_status,
		    sizeof(segment_status_struct)
		   );

     XFlush(display);

    }       /* end of control_NEXT || Control_GO_UPSTREAM */

 return 0;
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/ifp/src/PlotTulsa/RCS/cex25.c,v $";
 static char rcs_id2[] = "$Id: cex25.c,v 1.25 2007/05/16 16:41:20 aivo Exp $";}
/*  ===================================================  */

}
