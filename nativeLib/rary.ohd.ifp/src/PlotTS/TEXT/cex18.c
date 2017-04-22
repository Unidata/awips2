/* ***************************************** */
/*      File:           cex18.c              */
/*      Date:           Feb 2002             */
/*      Author:         Wen Kwock            */
/*      Purpose:                             */
/*                                           */
/* ***************************************** */
 

#include <time.h>

#include "Mods_globalDefs.h"
#include "Mods_defStruct.h" 
#include "TSPlots.h"
#include "TSWidget.h"
#include "SaveTSMod.h"

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <Xm/Xm.h>
#include "ifp_atoms.h"
  /*#include "cex25.h"*/
#include "plot.h"
#include "menus.h"
#include "ifp_struct.h"
#include "libXifp.h"
/*#include "mod_struct.h"*/
#include "event_loop_exit_status.h"
#include "techniques.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

#include "c_call_f/rcfset.h" 
#include "c_call_f/fctzc.h"
#include "c_call_f/fserch.h"
#include "c_call_f/umovex.h"
#include "c_call_f/fdcode.h"
#include "c_call_f/mdyh1.h"


#define NO_STATUS       0
#define NORMAL          1
#define ALERT           2
#define FLOOD           3

union UnionType
{
  char chs[5] ;
  float fl ;
  int integer ;
};

Widget     global_toplevel;
PLOTMGR    plotmgr, *PlotMgr = &plotmgr;  


int ModIndex=0;

void PrintTSdata() ;
void ExtractPlotTSData(float *p_float,float *d_float,int *d_locts,int *idarun,int *ihrrun, 
              int *ldarun, int *lhrrun, int *ida,int *idadat, int *ihr,
	      int *NOUTZ,int *NOUTDS,int *IPLHY,int *locp) ;
void CBForCex18GUI(Widget w, Widget *data, XmDrawingAreaCallbackStruct *call_data) ;

void SaveTSMod() ;
void SaveOtherMods (Mods_everythingStruct *data) ;


int TS_fill_tschng_keywords(ModKeywords *TSMods,int NumTS,char p_char[][4],int *t_int) ;

void cex18 (p_float, p_char, ts_float, ts_char, locp, rc_float, rc_char, rc_int,
    d_float, d_locts,idarun, ihrrun, ldarun, lhrrun , ldacpd, lhrcpd, first_data_day,
    t_array, first_time, event_loop_exit_status, mp, c_float,
    ida,ihr,NOUTZ,NOUTDS,IPLHY)

	char                    p_char[][4];    /* Parameter character data   */
	char                    ts_char[][4];   /* Time series character data */
	char                    rc_char[][4];   /* rating curve character array */
	int                     rc_int[];       /* rating curve interger value */
	int                     d_locts[];      /* location of the time series in the data array */
	int                     *locp;          /* pointer to the location of the beginning of
						   the parameter array. */
	int                     t_array[];      /* time series array */
	int                     *first_time;
	int                     *first_data_day;
	int                     *event_loop_exit_status;
	int                     *mp;            /* maximum parameter array size */
	float                   ts_float[];     /* time series floating point array */
	float                   rc_float[];     /* rating curve floating point array */
	float                   d_float[];      /* data floating point array */
	float                   p_float[];      /* parameter data array */
	float                   c_float[];      /* carry over data array */

int *idarun;
int *ihrrun;
int *ldarun;
int *lhrrun;
int *ldacpd;
int *lhrcpd;
int *ida;
int *ihr;
int *NOUTZ;
int *NOUTDS;
int *IPLHY ;

{
  union UnionType JointFlt ;
  float *PO= &(p_float[*locp-1]);
  int POIndex,NHY;
  char TmpStr[9] ;


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
  int                   i, j, k, l,n;             /* counters */
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
  univ_techniques_struct *univ_tech;
  float                 nbase;                  /* plot base value, lowest ploted value */

  int                   minimum_scale_increment;

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

  ModKeywords TSMods[MAXPLOTS*MAXTRACES];
  int NumTS ;

  struct tm TmpTime ;
  int	 TimeZone;

  void		get_flood_flow(rc_struct *);

  /* <><><><><><><><><><><><><><><>END OF DECLARATIONS><><><><><><><><><><><><><><><><><><><><><><><><><><> */
 /* printf ("At begin of cex18.c[ihrrun=%d]---kwz\n",*ihrrun) ;*/
    /*Added by Guoxian Zhou*/
    /*Pick up the flood data and the LSTCMPDY data and stored in the PlotMgr object */

    MDYH1(ldacpd,lhrcpd,&TmpTime.tm_mon,&TmpTime.tm_mday,&TmpTime.tm_year,
          &TmpTime.tm_hour,NOUTZ,NOUTDS,&TimeZone) ;
    if (TmpTime.tm_hour==24)
    { TmpTime.tm_hour=0;
      TmpTime.tm_mday++;
    }
    TmpTime.tm_mon--;
    TmpTime.tm_year-=1900 ;
    TmpTime.tm_isdst= -1; 
    PlotMgr->end_obs = mktime(&TmpTime) ;

	if(p_float[*locp-1 + 1] > 0.5)
	{

		rc_info(p_float, rc_float, rc_int, rc_char, locp, &rc_data);
		get_flood_flow(&rc_data);
	}

	PlotMgr->rc_data = &rc_data;

/*End of modification by Guoxian Zhou*/


  
/***************Check whether needs to work on cex18*************/
 
/*   CHECK IF HYDROGRAPHS SHOULD BE PLOTTED
*/
  NHY=0 ;
/*
*   COUNT THE NUMBER OF HYDROGRAPHS
*/
  JointFlt.chs[4]='\0';
  for (i=0;i<(int)PO[7];i++)
  {    
    POIndex=10+9*i ;
    JointFlt.fl = PO[POIndex+6] ;
    strcpy(TmpStr,JointFlt.chs) ;
    if (strcmp(TmpStr,"CMSD") || strcmp(TmpStr,"CMS ")) NHY++ ;
  }
/*
*   IF ALL PLOTS ARE HYDROGRAPHS AND HYDROGRAPHS ARE NOT
*   TO BE PLOTTED - RETURN
*/
      if (NHY==(int)PO[7] && *IPLHY==-1) return ;
/*
*   IF AT LEAST ONE PLOT IS A HYDROGRAPH AND HYDROGRAPHS ARE
*   TO BE PLOTTED NO MATTER WHAT THE CRITERIA ARE - MAKE THE
*   PLOT OPTION = 3
*/
/*****  if(NHY>0 && IPLHY==1) NOPT=3
*/

/***************************************************************/
/********************This is the part from cex25****************/
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
  
  display = XtDisplay(global_toplevel);

  if(XGetWindowProperty
	(
	display,
	DefaultRootWindow(display),
	IFPA_current_segment,
	atom_offset,
	(long) 9,
	FALSE,
	IFPA_current_segment_type,
	(Atom *)&type,
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
	IFPA_goto_downstream_segment_type,
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
		      IFPA_run_segments_type,
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
		       return;
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
	IFPA_univ_techniques_type,
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
 
  numop = 18;  /*find all plot TS left*/
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
      IFPA_first_plot_type,
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
        return;
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



  ExtractPlotTSData (p_float,d_float,d_locts,idarun,ihrrun, ldarun, lhrrun, ida, first_data_day,ihr,
                     NOUTZ, NOUTDS,IPLHY,locp) ;
                     
  show_MainTSDS() ;
  /*************Display the TS data structure *************/
  /*PrintTSdata() ; */
 
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
       XChangeProperty(
         XtDisplay(global_toplevel),
         DefaultRootWindow(XtDisplay(global_toplevel)),
         IFPA_nts_mods_available,
         IFPA_nts_mods_available_type,
         8,
         PropModeReplace,
         (unsigned char *)&nts_mods_available,
         sizeof(int)
         );
     XSync(XtDisplay(global_toplevel), 0);
     *event_loop_exit_status = TS_event_loop(data->widgetData->ifp_modsMessageBox,
			p_float, p_char, t_array, ts_float, c_float, data);
     
  }
  else
  {
     nts_mods_available = FALSE;
     post_nts_mods_available_atom(global_toplevel, nts_mods_available);
     *event_loop_exit_status = TS_event_loop(TSMainDS,
			p_float, p_char, t_array, ts_float, c_float, data);
    
  }
  
     if(*event_loop_exit_status == Control_QUIT)
       {
	if(num_plot_left > 0)
	  {                     /* leave without saving output time series */
	   *event_loop_exit_status = Control_QUIT_NO_SAVE;
           
	  }
       }

/*******save mod and rerun below here***********/

    InitTSMods (TSMods, &NumTS) ;

/*for (n=0;n<NumTS;n++)
{ printf ("TSMods[%d]:TSID=%s,TSType=%s,TimeInterval=%d\n",
       n,TSMods[n].TSID,TSMods[n].TSType,TSMods[n].TimeInterval) ;
}testing---kwz*/

    if(*event_loop_exit_status == Control_RERUN)
    {
      if (NumTS>0)
      { current_plot_num = get_plot_num(global_toplevel);
        post_rerun_plot_num_atom(&current_plot_num);

        TS_fill_tschng_keywords(TSMods,NumTS,p_char,t_array);
        SaveTSMod(TSMods,NumTS) ;
      }
      
      SaveOtherMods (data) ;
      
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
      		IFPA_FGmods_save_type,
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
	    		IFPA_tschng_mod_type,
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
	     		IFPA_fgmod_files_updated_type,
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
    			Write_mods(data);
  		FGmods_save  = (int *)malloc(sizeof(int));
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

/********************The part from cex25 ends here**************/
/***************************************************************/


return;
}

/*******************************************************************/
/*********************Subroutine for extract plot TS data***********/

void ExtractPlotTSData (float *P,float *D,int *ilocd,int *idarun,int *ihrrun, 
              int *ldarun, int *lhrrun, int *ida,int *idadat, int *ihr,
	      int *NOUTZ,int *NOUTDS,int *IPLHY,int *locp)
{
  GRAPH     *GraphPtr ;
  TRACE     *TracePtr ;
  union UnionType JointFlt ;
  int i, j, k, kk, L, NumTSSum, POIndex, DIndex, StepNum;
  int Month, Day, Year, Hour, TimeZone, NHY, TmpNum, JulianDay, JulianHour;
  char TmpStr[9] ;
  struct tm TmpTime ;
  extern Mods_everythingStruct *sacData;
  float *PO= &(P[*locp-1]);
  Atom type;
  int format;
  unsigned long nitems,left;
  Display *display;
  Window  win; 
  char                  std_units[4], dimen[4], time_scale[4];
  int                   missing_allowed;        /* Indicates if missing data are allowed for
						   this data type in the forecast component. */
  int                   nv_dt;                  /* number of values per time interval for this data type. */
  int                   nadd;                   /* Number of pieces of additional information */
  int                   err_flag;               /* Error flag, 0 no error, 1 indicates not a valid data type */
  int                   min_intrval = 24;  /*AV added 4/3/2002 */
  float                 tmp_ymax= 0.0, tmp_ymin= 0.0;  
  MDYH1(ida,ihr,&Month,&Day,&Year,&Hour,NOUTZ,NOUTDS,&TimeZone) ;
/***************Set up the structure*************/
  PlotMgr->NOUTZ=*NOUTZ ;
  PlotMgr->NOUTDS=*NOUTDS ;

  PlotMgr->Year=Year ;
  PlotMgr->Month=Month ;

  JointFlt.chs[4]='\0';  
  JointFlt.integer=TimeZone;
  strcpy(PlotMgr->TimeZone,JointFlt.chs) ;
  PlotMgr->nPlots = (int) PO[7] ;
  strcpy (PlotMgr->Description,"") ;
  for (i=1;i<6;i++)
  {
    JointFlt.fl = PO[i] ;
    strcat(PlotMgr->Description,JointFlt.chs) ;
    
  }
 
  JointFlt.fl = P[*locp-6] ;
  strcpy(PlotMgr->OperationName,JointFlt.chs) ;
  JointFlt.fl = P[*locp-5] ;
  strcat(PlotMgr->OperationName,JointFlt.chs) ;
  
  display = XtDisplay(global_toplevel);
  if(XGetWindowProperty ( 
        display, DefaultRootWindow(display),
        IFPA_current_segment, 
        0, 
        (long) 9,
	    FALSE,
        IFPA_current_segment_type,
	    (Atom *)&type, 
        (int *)&format, 
        (unsigned long *)&nitems, 
        (unsigned long *)&left, 
        (unsigned char **)&PlotMgr->SegName
    ) != Success || type != IFPA_current_segment_type)
  {
    PlotMgr->SegName = (char *) malloc(9);
    strcpy(PlotMgr->SegName, "NoneYet");
  }

  /*PlotMgr->Graph = (GRAPH*)calloc( PlotMgr->nPlots, sizeof(GRAPH));*/
  /*AV added          */
  NumTSSum = 0 ;
  
 
  
  
/*********fetch plot data*******************/
  for (i=0;i<PlotMgr->nPlots;i++)
  { 
    GraphPtr = (GRAPH *)&PlotMgr->Graph[i];
    POIndex = 10+9*i ; /*see IPO=11+9*(I-1) in HEAD18.F*/
    JointFlt.fl = PO[POIndex+7] ;
    strcpy(GraphPtr->Unit,JointFlt.chs);
    
    GraphPtr->ntraces = (int) PO[POIndex+5];
    
/*    GraphPtr->Id=0 ;*/
    
    GraphPtr->x=0 ;
    GraphPtr->y=0 ;
    GraphPtr->w=0 ;
    GraphPtr->h=0 ;
    GraphPtr->orgW=0 ;
    GraphPtr->orgH=0 ;
    GraphPtr->last_x=0 ;
    GraphPtr->last_y=0 ;
    GraphPtr->bg=0 ;
    
    TmpTime.tm_sec= 0 ; 
    TmpTime.tm_min=0 ;
    TmpTime.tm_isdst= -1; 
    MDYH1(idarun,ihrrun,&TmpTime.tm_mon,&TmpTime.tm_mday,&TmpTime.tm_year,
          &TmpTime.tm_hour,NOUTZ,NOUTDS,&TimeZone) ;
    if (TmpTime.tm_hour==24)
    { TmpTime.tm_hour=0;
      TmpTime.tm_mday++;
    }
    TmpTime.tm_mon--;
    TmpTime.tm_year-=1900 ;
    GraphPtr->xmin = mktime(&TmpTime) ;
    MDYH1(ldarun,lhrrun,&TmpTime.tm_mon,&TmpTime.tm_mday,&TmpTime.tm_year,
          &TmpTime.tm_hour,NOUTZ,NOUTDS,&TimeZone) ;
    if (TmpTime.tm_hour==24)
    { TmpTime.tm_hour=0;
      TmpTime.tm_mday++;
    }
    TmpTime.tm_mon--;
    TmpTime.tm_year-=1900 ;
    TmpTime.tm_isdst= -1; 
    GraphPtr->xmax = mktime(&TmpTime) ;
    GraphPtr->old_xmin = GraphPtr->xmin;
    GraphPtr->old_xmax = GraphPtr->xmax;
    GraphPtr->org_xmin = GraphPtr->xmin;
    GraphPtr->org_xmax = GraphPtr->xmax;

    GraphPtr->CFact = PO[POIndex+8];
    GraphPtr->Const = PO[POIndex+9];
    
    GraphPtr->ymin = PO[POIndex+3];
    GraphPtr->ymax = PO[POIndex+4];
    GraphPtr->old_ymin = GraphPtr->ymin;
    GraphPtr->old_ymax = GraphPtr->ymax;
    GraphPtr->org_ymin = GraphPtr->ymin;
    GraphPtr->org_ymax = GraphPtr->ymax;
    
    
    GraphPtr->data_inc = 20.0 ;
    GraphPtr->old_data_inc = GraphPtr->data_inc ;
    GraphPtr->org_data_inc = GraphPtr->data_inc ;
    GraphPtr->symbol_type=0 ;
    GraphPtr->initialize=0 ;

   
    GraphPtr->Algorithm = 0 ;
    
    /*********fetch TIME SERIES data*******************/  
    tmp_ymax = -99999.0;
    tmp_ymin = 99999.0;
    for (j=0;j<GraphPtr->ntraces;j++)
    {
      
      GraphPtr->trace_on[j]=0 ;
      TracePtr = (TRACE *)&GraphPtr->traces[j] ;
      
      POIndex = 10+9*PlotMgr->nPlots+12*NumTSSum;
      JointFlt.fl = PO[POIndex+1] ;
      strcpy(TracePtr->OperationID,JointFlt.chs);
      JointFlt.fl = PO[POIndex+2] ;
      strcat(TracePtr->OperationID,JointFlt.chs);
      
      JointFlt.fl = PO[POIndex+3] ;
      strcpy(TracePtr->TraceType,JointFlt.chs);

      
       strcpy(TracePtr->tmpTraceType,JointFlt.chs);
      
      TracePtr->TimeInterval = PO[POIndex+4] ;
      
      JointFlt.fl = PO[POIndex+5] ;
      strcpy(TracePtr->Trace_Title,JointFlt.chs);
      JointFlt.fl = PO[POIndex+6] ;
      strcat(TracePtr->Trace_Title,JointFlt.chs);
      JointFlt.fl = PO[POIndex+7] ;
      strcat(TracePtr->Trace_Title,JointFlt.chs);
      
      JointFlt.fl = PO[POIndex+8] ;
      TracePtr->Trace_Symbol = JointFlt.chs[0];

   
      
      TracePtr->npts =((*ldarun-*idarun)*24+(*lhrrun-*ihrrun))/(TracePtr->TimeInterval);
      TracePtr->ymax= GraphPtr->ymax;
      TracePtr->ymin= GraphPtr->ymin;
      TracePtr->xmax= 0.0;
      TracePtr->xmin= 0.0;
      TracePtr->txmax= GraphPtr->xmax;
      TracePtr->txmin= GraphPtr->xmin;
      
      
      if( TracePtr->TimeInterval < min_intrval ){
      
         min_intrval = TracePtr->TimeInterval;
      
      }
      
      
      /*********fetch TIME SERIES data*******************/
      DIndex = ((((*ida-*idadat)*24)/TracePtr->TimeInterval)*(int)PO[POIndex+11] +
        ((*ihr-1)/TracePtr->TimeInterval)* (int)PO[POIndex+11]+ 
        (int)PO[POIndex+12] + ilocd[NumTSSum] - 2);
      NumTSSum++;
      JulianDay=*idarun;
      JulianHour=*ihrrun ;
      TmpTime.tm_sec= 0 ; 
      TmpTime.tm_min=0 ; 
      
      for (k=0;k<TracePtr->npts;k++)
      {
      
	JulianDay=JulianDay+(JulianHour+TracePtr->TimeInterval-1) / 24 ;
	JulianHour=(JulianHour+TracePtr->TimeInterval-1) % 24 + 1 ;
	TracePtr->TSData[k].t = JulianDay*24 + JulianHour;
        MDYH1(&JulianDay,&JulianHour,&TmpTime.tm_mon,&TmpTime.tm_mday,&TmpTime.tm_year,
              &TmpTime.tm_hour,NOUTZ,NOUTDS,&TimeZone) ;

	if (TmpTime.tm_hour==24)
	{ 
           TmpTime.tm_hour = 0;
	   TmpTime.tm_mday++;
	}
        TmpTime.tm_mon--;
	TmpTime.tm_year-=1900;
        TmpTime.tm_isdst= -1;       
        TracePtr->TSData[k].x = mktime(&TmpTime) ;
        TracePtr->TSData[k].y = D[DIndex];                  
	DIndex += (int)PO[POIndex+11];    
       	TracePtr->TSData[k].yOrg = TracePtr->TSData[k].y;
        TracePtr->TSData[k].yedit = TracePtr->TSData[k].y;
        TracePtr->TSData[k].status = 0;
        /* storing ymax and ymin */
        if(TracePtr->TSData[k].y > tmp_ymax) tmp_ymax = TracePtr->TSData[k].y;
        if(TracePtr->TSData[k].y < tmp_ymin && 
               (TracePtr->TSData[k].y > -998.99 || TracePtr->TSData[k].y < -999.01)) {
                    tmp_ymin = TracePtr->TSData[k].y;
        }
      }      
      FDCODE(TracePtr->TraceType, std_units, dimen, &TracePtr->ObsFlag, &nv_dt,
             time_scale, &nadd, &err_flag);

    } /*end ntraces */    
       GraphPtr->ymax = tmp_ymax;
       GraphPtr->ymin = tmp_ymin;
  
  }/* end nplots */
  
  
  PlotMgr->idtp  = min_intrval; 
    
  
  PlotMgr->org_xmin = GraphPtr->xmin;
  PlotMgr->org_xmax = GraphPtr->xmax;
  PlotMgr->xmin = PlotMgr->org_xmin;
  
  PlotMgr->num_days = (PlotMgr->org_xmax  - PlotMgr->org_xmin)/SECONDS_PER_DAY;
  
/*  if ( PlotMgr->num_days < 90 )
    PlotMgr->duration = PlotMgr->num_days-1;
  else
    PlotMgr->duration = 90; 
*//*duration is in number of hours not number of days---kwz*/
 if ( PlotMgr->num_days < 10 )
    PlotMgr->duration = (PlotMgr->org_xmax - PlotMgr->org_xmin)/SECONDS_PER_HOUR+1;
  else
    PlotMgr->duration = 10*24+1; /*10 days+1 hour*/
 
  PlotMgr->xmax = PlotMgr->xmin + PlotMgr->duration*SECONDS_PER_HOUR;
 
  
  /*PlotMgr->idtp = 1/6/12/24;*/
/*  PlotMgr->num_tics    = PlotMgr->num_days*24.00/PlotMgr->idtp;
  PlotMgr->slider_size = PlotMgr->duration *24.00/PlotMgr->idtp;
  PlotMgr->slider_incr = 24.00/PlotMgr->idtp;
  PlotMgr->period      = PlotMgr->num_days;
*//*change by kwz*/
  PlotMgr->num_tics    = (PlotMgr->org_xmax - PlotMgr->org_xmin)/SECONDS_PER_HOUR/PlotMgr->idtp+1;
  PlotMgr->slider_size = PlotMgr->duration/PlotMgr->idtp;
  PlotMgr->slider_incr = 1;
  PlotMgr->period      = PlotMgr->num_days;
  
  /*convert to the right units , Metrics or English */
  PlotMgr->NWSRFS_Units = getUnits(display, IFPA_general_units, IFPA_general_units_type);
  
  if (PlotMgr->NWSRFS_Units==0) /*0=English unit*/
  { 
    
    for (i=0;i<PlotMgr->nPlots;i++)
    { 
      GraphPtr = (GRAPH *)&PlotMgr->Graph[i];  
      tmp_ymax = -99999.0;
      tmp_ymin =  99999.0;
      for (j=0;j<GraphPtr->ntraces;j++)
      {
        TracePtr = (TRACE *)&GraphPtr->traces[j] ;          
        for (k=0;k<TracePtr->npts;k++){ 
        
          if(TracePtr->TSData[k].y != -999.0){
             TracePtr->TSData[k].y = TracePtr->TSData[k].y*GraphPtr->CFact + GraphPtr->Const;
             
          }
          TracePtr->TSData[k].yOrg   = TracePtr->TSData[k].y;
          TracePtr->TSData[k].yedit  = TracePtr->TSData[k].y;
          TracePtr->TSData[k].status = 0;
          /* storing ymax and ymin */          
          if(TracePtr->TSData[k].y > tmp_ymax) {
              tmp_ymax = TracePtr->TSData[k].y;
              
          }
          if(TracePtr->TSData[k].y < tmp_ymin && 
               (TracePtr->TSData[k].y > -998.99 || TracePtr->TSData[k].y < -999.01)) {
                    tmp_ymin = TracePtr->TSData[k].y;
          }
        }
     
      }
   
          GraphPtr->ymax = tmp_ymax;
          GraphPtr->ymin = tmp_ymin;
    
    }
  }/*end of if*/

  /*for r22-51,3-04-03---kwz*/
  /*If data(PlotMgr->NWSRFS_Units) is in metric unit, then label(PlotMgr->Graph[i].Unit) should be in metric unit*/
  if(PlotMgr->NWSRFS_Units==1)/*metric unit*/
    for(i=0;i<PlotMgr->nPlots;i++)
      if(strcmp(PlotMgr->Graph[i].Unit,"IN  ")==0)
        strcpy(PlotMgr->Graph[i].Unit,"MM  ") ;

  /* Read AiMinMax.dat file - only when re-run perform */
  read_ymaxminFile() ;

  PlotMgr->General_Units = getUnits(display, IFPA_mods_general_units, IFPA_mods_general_units_type);
} /***End of subroutine ExtractPlotTSData()***/


/********************** delete this subroutine when complete *******/
/*********************Subroutine for display plot TS data***********/

void PrintTSdata ()
{
  int i,j,k ;
  GRAPH     *GraphPtr ;
  TRACE     *TracePtr ;
  
  printf("PlotMgr->num_tics = %d\n",PlotMgr->num_tics);
  printf("PlotMgr->slider_size = %d\n",PlotMgr->slider_size);
  printf("PlotMgr->slider_incr = %d\n",PlotMgr->slider_incr);
  printf("PlotMgr->period = %d\n",PlotMgr->period);
  

  printf ("********Data in PlotMgr*************\n") ;
  printf("Year=%d,Month=%d,nPlots=%d,Description=%s,TimeZone=%s,OperationName=%s,SegName=%s,NWSRFS_Units=%d.\n",PlotMgr->Year,PlotMgr->Month,PlotMgr->nPlots,PlotMgr->Description,PlotMgr->TimeZone,PlotMgr->OperationName,PlotMgr->SegName,PlotMgr->NWSRFS_Units) ;     

  for (i=0;i<PlotMgr->nPlots;i++)
  {
    GraphPtr = &PlotMgr->Graph[i];
    printf ("\n********Data in GRAPH*************\n") ;
    
    printf ("Unit=%s,ntraces=%d,\n",
         GraphPtr->Unit,GraphPtr->ntraces) ;
             
    printf ("x=%d,y=%d,w=%d,h=%d,orgW=%d,orgH=%d,last_x=%d,last_y=%d,bg=%d\n",
      GraphPtr->x,GraphPtr->y,GraphPtr->w,GraphPtr->h,GraphPtr->orgW,
       GraphPtr->orgH,GraphPtr->last_x,GraphPtr->last_y,GraphPtr->bg) ;

    printf ("GraphPtr->xmin = %s",ctime(&GraphPtr->xmin));
    printf ("GraphPtr->xmax = %s",ctime(&GraphPtr->xmax));
    printf ("GraphPtr->old_xmin=%s",ctime(&GraphPtr->old_xmin));
    printf ("GraphPtr->old_xmax=%s",ctime(&GraphPtr->old_xmax));
    printf ("GraphPtr->org_xmin=%s",ctime(&GraphPtr->org_xmin));
    printf ("GraphPtr->org_xmax=%s",ctime(&GraphPtr->org_xmax));
    
    printf ("GraphPtr->ymin=%f,GraphPtr->ymax=%f,  GraphPtr->old_ymin=%f,GraphPtr->old_ymax=%f,org_ymin=%f,org_ymax=%f,data_inc=%f,old_data_inc=%f,org_data_inc=%f\n ",
      GraphPtr->ymin,GraphPtr->ymax,GraphPtr->old_ymin,GraphPtr->old_ymax,GraphPtr->org_ymin,
      GraphPtr->org_ymax,GraphPtr->data_inc,GraphPtr->old_data_inc,GraphPtr->org_data_inc);

    printf ("symbol_type=%d,initialize=%d,Algorithm=%d,CFact=%f,Const=%f\n",GraphPtr->symbol_type,
      GraphPtr->initialize,GraphPtr->Algorithm,GraphPtr->CFact,GraphPtr->Const) ;

    for (j=0;j<GraphPtr->ntraces;j++)
    {
      TracePtr = &GraphPtr->traces[j] ;
      printf ("\n********Data in traces*************\n") ;
      printf ("OperationID=%s,TraceType=%s,TimeInterval=%d,Trace_Title=%s,Trace_Symbol=%c\n",
            TracePtr->OperationID,TracePtr->TraceType,TracePtr->TimeInterval,TracePtr->Trace_Title,
            TracePtr->Trace_Symbol) ;

      printf ("npts=%d,ymax=%f,ymin=%f,xmax=%f,xmin=%f,trace_on=%d\n",
            TracePtr->npts,TracePtr->ymax,TracePtr->ymin,TracePtr->xmax,
					TracePtr->xmin,GraphPtr->trace_on[j]);

      printf ("txmin=%s",ctime(&TracePtr->txmin)) ;
      printf ("txmax=%s",ctime(&TracePtr->txmax)) ;
      for (k=0;k<TracePtr->npts;k++)
      {
        printf ("x=%s,y=%5.2f,status=%c\n",ctime(&TracePtr->TSData[k].x),
				TracePtr->TSData[k].y,TracePtr->TSData[k].status) ;
      }
      printf ("ObsFlag=%d",TracePtr->ObsFlag) ;

    }
    
  } 
}

/**************************************************/

void CBForCex18GUI (Widget w, Widget *data, XmDrawingAreaCallbackStruct *call_data) 
{

printf ("cex18 gui exposed.\n") ;

}
/***********************************/


extern char *make_mod_A1_string(Mods_everythingStruct *, int, Display *);
extern char *make_mod_A2_string(Mods_everythingStruct *, int, Display *);
extern char *make_mod_B1_string(Mods_everythingStruct *, int, Display *);
extern char *make_mod_B2_string(Mods_everythingStruct *, int, Display *);
extern char *make_mod_B3_string(Mods_everythingStruct *, int, Display *);

void SaveOtherMods (Mods_everythingStruct *data)
{ FILE            *modfp;
  char            fname[80];
  char            *buffer;
  char            all_newMods_str[1000];
  int             buffer_len=0;
  int             tot_newMods_len=0;
  int             i;
  Display         *display;

  strcpy(fname, getenv("HOME"));
  strcat(fname, "/.ifp_files/mods/");
  strcat(fname, plotmgr.SegName);
  if (index(fname, ' ') != NULL) memset(index(fname, ' '), '\0', 1);

  modfp = fopen(fname, "a");
  if(modfp == NULL)
  {
    printf("Permission to open %s was denied.\n", fname);
    return;
  }
  
/*****************/
   
   /* Just return if there are no new mods */     
   if(data->ModIndex < 1)
      return;

   memset(all_newMods_str, '\0', 1000);
   
   display = XtDisplay(data->viewerWidgets->newModsText);    
        
   for(i = 0; i < data->ModIndex; i++)
   {
      switch (data->ModArray[i]->type)
      {
         case Mod_format_A1:
           buffer = (char *) make_mod_A1_string(data, i, display);
            break;

         case Mod_format_A2:
           buffer = (char *) make_mod_A2_string(data, i, display);
            break;

         case Mod_format_B1:
           buffer = (char *) make_mod_B1_string(data, i, display);
            break;

         case Mod_format_B2:
           buffer = (char *) make_mod_B2_string(data, i, display);
            break;

         case Mod_format_B3:
           buffer = (char *) make_mod_B3_string(data, i, display);
            break;
      }

      buffer_len = strlen(buffer) + 1;
      tot_newMods_len = tot_newMods_len + buffer_len;
      
      strcat(all_newMods_str, buffer);
      
   }
   		    
  fprintf(modfp,"%s",all_newMods_str);
  fclose(modfp);
  /* free the buffer space */
  if(buffer != NULL)
     free(buffer);



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTS/RCS/cex18.c,v $";
 static char rcs_id2[] = "$Id: cex18.c,v 1.6 2006/04/07 16:00:15 aivo Exp $";}
/*  ===================================================  */

}/**end of SaveOtherMods()**/
