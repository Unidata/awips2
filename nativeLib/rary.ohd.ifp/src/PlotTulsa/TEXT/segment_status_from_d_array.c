/* File: segment_status_from_d_array.c
 *
 * Uses flood or alert flow values to check for the QINE time series in this
 * segment.
 *
 * From the QINE time series in the ts array gets the location of the start
 * of the data in the d array and compare values against the flood and alert
 * flows.
 *
 * Converts flood and warning flow to metric units if needed to properly
 * compare with the values in the d array.
 *
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include "c_call_f/fconvt.h"
#include "ifp_struct.h"

int      NWSRFS_general_units;   /* If 0: ENGLISH, 1: METRIC  */

extern void tschng_mod_made();
extern void draw_info_lines(Widget, combined_struct *);
extern int find_in_ts (char *, char *, int, float[],char*[]);
extern void getrundates(int *, int*, int*, int*, int*);

void segment_status_from_d_array(currentSegment,
			    warning_flow,
			    flood_flow,
			    ts_float, ts_char, d_float,
			    seg_status)

 char   *currentSegment;  /* current segment pointer */
 char   *seg_status;      /* segment status pointer */
 char   ts_char[][4];     /* time series character data */
 float  warning_flow;     /* warning river flow value */
 float  flood_flow;       /* flood river flow value */
 float  ts_float[];       /* time series floating point data */
 float  d_float[];        /* data array */
{
 int            istrhr;   /* start date of run */
 int            ilcdhr;   /* end date of observations */
 int            iendhr;   /* end date of run */
 int            isdahr;   /* hour of first data for any time series */
 int            icount;   /* counter */
 int            num;      /* number of data points */
 int            i, ii, iii, j; /* counters */
 int            current_delta_t; /* current time interval */
 int            locts;    /* location of time series array */
 int            locd;     /* location of data array */
 int            max_vals; /* maximum data value */
 int            alert_status, flood_status;
 static int     delta_t[] = {1, 2, 3, 4, 6, 8, 12, 24}; /* time intervals */
 static char    dtype[5] = {"QINE"};
 float          warning_flow_metric, flood_flow_metric;
 char           std_units[4], dimen[4];
 int            err_flag;
 char           std_Eng_units[4];
 float          q_mult_conver_factor, q_add_constant;
 char           temp_segnam[9];
 int            already_blank;

 if(warning_flow < 0.0 && flood_flow < 0.0)
   {
    strcpy(seg_status, "Unknown");
    return;
   }
/*
 * Have flood or alert flow values - see if there is a QINE
 *  time series in this segment.
 * Modified 9/16/93 by gfs to search for the QINE t.s. with
 *  truncated versions of the segment name as the segment id.
 * This change requested by ABRFC because they append characters
 *  (i.e., X) to the end of the usual segment names when testing
 *  changes to parameters such as adding MAPX time series
 *  to a segment.  They don't change the QINE t.s. in the
 *  Processed Data Base but want to find a match to get segment
 *  status while testing these changes.
 */
 memset(temp_segnam, '\0', 9);
 strncpy(temp_segnam, currentSegment, 8);
 current_delta_t = 0;
 locts = 0;

 for(j = 8; j > 0; j--)
    {
     already_blank = FALSE;  /* First pass through, use whole name. */
     if(j < 8)
       {
	if(temp_segnam[j] == ' ')
	  {
	   already_blank = TRUE;
	  }
	else
	  {                       /* Set segment name to blanks, */
	   already_blank = FALSE; /* character by character,     */
	   temp_segnam[j] = ' ';  /* starting at end of name.    */
	  }
       }
     if(already_blank == FALSE)   /* Don't search for t.s. if    */
       {                          /* current character is blank. */
	for(i = 0; i < XtNumber(delta_t); i++)
	   {
	    if((locts = find_in_ts(temp_segnam, dtype, delta_t[i],
				   ts_float, ts_char)) > 0)
	      {                               /* have found time series */
	       current_delta_t = delta_t[i];
	       break;
	      }
	   }
       }
     if(current_delta_t > 0)
	break;
    }
 if(locts == 0)
   {                                            /* no QINE time series   */
    strcpy(seg_status, "Unknown");
    return;
   }
/*
 * Have found QINE time series in the ts array. Now get the location
 *  of the start of data in the d array and compare values against
 *  the flood and alert flows.
 */
 locd = ts_float[locts-1 + 7];

 max_vals = (24/current_delta_t) * 31;

 icount = 1;

 getrundates(&istrhr, &ilcdhr, &iendhr, &isdahr, &icount);

 num = (iendhr - istrhr - 1)/current_delta_t + 1;

 alert_status = FALSE;
 flood_status = FALSE;
/*
 * Convert flood and warning flow to metric units if needed so
 *  we can properly compare with the values in the d array.
 */
 /*
  * Add check for missing flood_flow or warning_flow.
  * Do not set flood_status if flood_flow is missing.
  * Do not set alert_status if warning_flow is missing.
  * Modified by gfs - hrl - 23 Sept 1994
  */
 flood_flow_metric = -999.0;
 warning_flow_metric = -999.0;

 if(NWSRFS_general_units == 0)
   {
    strncpy(std_units, "CMS ", 4);
    strncpy(dimen, "L3/T", 4);
    FCONVT(std_units, dimen, std_Eng_units, &q_mult_conver_factor,
	   &q_add_constant, &err_flag);

    if(flood_flow >= 0.0)
       flood_flow_metric = flood_flow / q_mult_conver_factor;
    if(warning_flow >= 0.0)
       warning_flow_metric = warning_flow / q_mult_conver_factor;
   }
 else
   {
    flood_flow_metric = flood_flow;
    warning_flow_metric = warning_flow;
   }

 for(i = 0; i < num; i++)
    {
     if(d_float[locd-1 + i] < 0.0) break; /* have reached missing data */
     if((d_float[locd-1 + i] >= flood_flow_metric) &&
        (flood_flow_metric >= 0.0))
          flood_status = TRUE;
     if(flood_status == TRUE) break;      /* if flood no more to check */
     if((d_float[locd-1 + i] >= warning_flow_metric) &&
        (warning_flow_metric >= 0.0))
          alert_status = TRUE;
    }
/*
 * printf("in segment_status_from_d_array, current segment = %s\n",
 *         currentSegment);
 * printf("d_float[locd-1] through [locd-1 + max_vals-1] =\n");
 *
 * for (ii = 0; ii < max_vals; ii+=10)
 *  {
 *   for (iii = 0; iii < 10; iii++)
 *      if(ii + iii < max_vals)
 *         printf("%f, ", d_float[locd-1 + ii + iii]);
 *   printf("\n");
 *  }
 *
 * printf("  warning flow = %f, flood flow = %f, i = %d, d_float[%d] = %f\n",
 *        warning_flow_metric, flood_flow_metric, i, locd-1 + i,
 *        d_float[locd-1 + i]);
 *
 * printf("  num = %d, flood_status = %d, alert_status = %d\n",
 *        num, flood_status, alert_status);
 */
 if(flood_status == TRUE)       strcpy(seg_status, "Flood");

 else if(alert_status == TRUE)  strcpy(seg_status, "Alert");

 else                           strcpy(seg_status, "Normal");

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/segment_status_from_d_array.c,v $";
 static char rcs_id2[] = "$Id: segment_status_from_d_array.c,v 1.2 2002/02/11 19:44:33 dws Exp $";}
/*  ===================================================  */

}
