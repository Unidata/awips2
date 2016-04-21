/* File: fill_px_ro_ts.c
 *
 * fill_px_ro_ts function takes the location info from the ????????
 * function and fills the input (MAP or RAIM) and output (INFW) time
 * series' by copying the values from the d_array into the px and ro
 * arrays. It also determines the number of points in the px and ro
 * arrays based on the total run hours and the delta_t of the first
 * px time series, if any of the others are different it prints out
 * a message to the screen.
 */

#include <X11/Intrinsic.h>
#include "libXifp.h"
#include "mod_struct.h"
#define  ChooseTStypestruct
#include "mods_info.h"
#include "libXs.h"
#include "ifp_globals.h"
#include "c_call_f/fdcode.h"/*-- added by AV -- */
#include "c_call_f/fconvt.h"

void  fill_px_ro_ts(ts_float, ts_char, d_float, px, ro,
		    total_run_hours, num_rr_oper, loc_px, loc_ro,
		    num_rr_pts, px_id, px_dt, ro_id, ro_dt)
   float    ts_float[];         /* Time series floating point data */
   float    d_float[];          /* data array */
   float    **px;               /* address of the px time series pointer */
   float    **ro;               /* address of the ro time series pointer */
   char     ts_char[][4];       /* Time series character data */
   int      total_run_hours;
   int      num_rr_oper;        /* number of rainfall runoff operations */
   int      *num_rr_pts;        /* number of rainfall runoff data points */
   int      loc_px[], loc_ro[]; /* location of the first members of the px and ro time series */
   char     **px_id, **ro_id;   /* address of the px and ro time series id pointers */
   int      *px_dt, *ro_dt;     /* px and ro sample time intervals pointers */
{
   int      i,j;                /* counters */
   int      px_d_loc, ro_d_loc; /* location in d array for the start of the data for the px and ro
				   time series */
   int      px_ro_dt;           /* px time series interval */
   char     type_string[4], std_units[4], dimen[4], time_scale[4];
   int      missing_allowed, nv_dt, nadd, err_flag;
   float    mult_conver_factor, add_constant;
   char     std_Eng_units[4];

   /* Calculate the number of rainfall-runoff points (num_rr_points)
      based on the delta-t of the first px time series.
   */
   px_ro_dt = ts_float[loc_px[0]-1 + 5];
   *num_rr_pts = total_run_hours/px_ro_dt;

   for(i=0; i<num_rr_oper; i++)
   {
      /* make id strings for px and ro time series */
      memset(px_id[i], '\0', 14);
      strncpy(px_id[i], ts_char[loc_px[i]-1 + 2], 8);
      strcat(px_id[i], ".");
      strncat(px_id[i], ts_char[loc_px[i]-1 + 4], 4);

      memset(ro_id[i], '\0', 14);
      strncpy(ro_id[i], ts_char[loc_ro[i]-1 + 2], 8);
      strcat(ro_id[i], ".");
      strncat(ro_id[i], ts_char[loc_ro[i]-1 + 4], 4);

      /* get the delta_t values for each time series */
      px_dt[i] = ts_float[loc_px[i]-1 + 5];
      ro_dt[i] = ts_float[loc_ro[i]-1 + 5];
      if(px_dt[i] != px_ro_dt || ro_dt[i] != px_ro_dt)
      {
	 printf("Warning: delta t for rainfall runoff operation %d\n",i);
	 printf("         does not match that of the first one-ignored\n");
	 printf("         for now.\n");
      }

      /* set location in d array for start of data */
      px_d_loc = ts_float[loc_px[i]-1 + 7];
      ro_d_loc = ts_float[loc_ro[i]-1 + 7];

      /*
       * malloc space for the px and ro time series based
       *  on the dt of the MAP time series (*num_rr_pts).
       * This change made by gfs, 12/13/91.
       */
      
      px[i] = (float *)malloc((*num_rr_pts) * sizeof(float) + 1);
      
      
      ro[i] = (float *)malloc((*num_rr_pts) * sizeof(float) + 1);
      
      /* If English units are chosen, get conversion factors and
	 fill px and ro arrays with converted values.  If metric
	 units are chosen fill arrays - no conversion needed.
      */
      if(NWSRFS_general_units == 0)
      {
	 strncpy(type_string, ts_char[loc_px[i]-1 + 4], 4);
	 FDCODE(type_string, std_units, dimen, &missing_allowed, &nv_dt,
		time_scale, &nadd, &err_flag);
	 FCONVT(std_units, dimen, std_Eng_units, &mult_conver_factor,
		&add_constant, &err_flag);

	 /* fill px and ro data arrays */
	 for(j=0; j<*num_rr_pts; j++)
	 {
	    if(d_float[px_d_loc-1 + j] < 0.0)
	       px[i][j] = 0.0;
	    else
	       px[i][j] = d_float[px_d_loc-1 + j] * mult_conver_factor
			     + add_constant;

	    if(d_float[ro_d_loc-1 + j] < 0.0)
	       ro[i][j] = 0.0;
	    else
	       ro[i][j] = d_float[ro_d_loc-1 + j] * mult_conver_factor
			     + add_constant;
	 }
      }
      else
      {
	 /* fill px and ro data arrays */
	 for(j=0; j<*num_rr_pts; j++)
	 {
	    if(d_float[px_d_loc-1 + j] < 0.0)
	       px[i][j] = 0.0;
	    else
	       px[i][j] = d_float[px_d_loc-1 + j];

	    if(d_float[ro_d_loc-1 + j] < 0.0)
	       ro[i][j] = 0.0;
	    else
	       ro[i][j] = d_float[ro_d_loc-1 + j];
	 }
      }
      
   }  /* end of i for loop */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/fill_px_ro_ts.c,v $";
 static char rcs_id2[] = "$Id: fill_px_ro_ts.c,v 1.2 2002/02/11 19:28:11 dws Exp $";}
/*  ===================================================  */

}
