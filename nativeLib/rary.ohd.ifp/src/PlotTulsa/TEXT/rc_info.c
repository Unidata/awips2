/* File: rc_info.c
 *
 * Converts when needed, discharge rates and stage data to metric.
 *
 * Sets the maximum and minimum values of time series.
 *
 * Sets the maximum value for the discharge axis and increments for
 * plotting the x axis.
 *
 * Sets the maximum value for the stage axis and increments for plotting
 * the y axis.
 *
 */

#include "plot.h"
#include "ifp_struct.h"
#include "libXifp.h"
#include "mod_struct.h"
#define  ChooseTStypestruct
#include "mods_info.h"
#include "libXs.h"
#include "ifp_globals.h"
#include "c_call_f/fconvt.h"

void rc_info(p_float, rc_float, rc_int, rc_char, locp, rc_data)

   float        p_float[];            /* floating point parameter array data */
   float        rc_float[];           /* rating curve floating point data */
   int          *locp;                /* pointer to the location of the beginning of
					 the parameter array  */
   int          rc_int[];             /* rating curve integer value */
   char         rc_char[][4];         /* rating curve character array */
   rc_struct    *rc_data;             /* rating curve data structure pointer */
{
   int       i;                       /* counter */
   char      std_units[4];            /* metric units */
   char      dimen[4];                /* dimensions */
   int       err_flag;                /* error flag */
   char      std_Eng_units[4];        /* standard English units value */
   float     stg_mult_conver_factor;  /* stage multiplication conversion factor */
   float     stg_add_constant;        /* stage addition constant */
   float     q_mult_conver_factor;    /* discharge multiplication conversion factor */
   float     q_add_constant;          /* discharge addition constant */

   if(p_float[*locp] < 0.5)  /* Is rating curve available? */
      rc_data->RC = FALSE;
   else
   {
      rc_data->RC = TRUE;
      rc_data->flood_stg = rc_float[21];
      rc_data->warning_stg = rc_float[25];
      rc_data->warning_flow = 0.0;         /* not stored in OFS db */
      rc_data->flood_flow = rc_float[22];
      memset(rc_data->rc_id, '\0', 9);
      strncpy(rc_data->rc_id, rc_char[0], 8);
      memset(rc_data->rc_station_name, '\0', 21);
      strncpy(rc_data->rc_station_name, rc_char[7], 20);
      rc_data->rc_num = rc_int[27];
      rc_data->loc_q = rc_int[28] + 74;
      rc_data->loc_stg = rc_int[29] + 74;
      rc_data->rating_curve_q =
			    (float*)malloc(rc_data->rc_num*sizeof(float));
      rc_data->rating_curve_stg =
			    (float*)malloc(rc_data->rc_num*sizeof(float));
      rc_data->max_record_stg = rc_float[42];
      rc_data->max_record_q = rc_float[43];
      rc_data->interpol_method = rc_float[54];
      if(rc_data->interpol_method == 0)
	 rc_data->axis_type = LOG;
      else
	 rc_data->axis_type = LINEAR;

      if(NWSRFS_general_units == 0)    /* English units */
      {
	 strncpy(std_units, "M   ", 4);
	 strncpy(dimen, "L   ", 4);
	 FCONVT(std_units, dimen, std_Eng_units, &stg_mult_conver_factor,
		&stg_add_constant, &err_flag);

	 strncpy(std_units, "CMS ", 4);
	 strncpy(dimen, "L3/T", 4);
	 FCONVT(std_units, dimen, std_Eng_units, &q_mult_conver_factor,
		&q_add_constant, &err_flag);

	 for(i = 0; i < rc_data->rc_num; i++)
	 {
	    rc_data->rating_curve_stg[i] = rc_float[rc_data->loc_stg + i] *
					   stg_mult_conver_factor +
					   stg_add_constant;
	    rc_data->rating_curve_q[i] = rc_float[rc_data->loc_q + i] *
					 q_mult_conver_factor +
					 q_add_constant;
	 }
	 rc_data->stg_mult_conver_factor = stg_mult_conver_factor;
	 rc_data->stg_add_constant = stg_add_constant;
	 rc_data->q_mult_conver_factor = q_mult_conver_factor;
	 rc_data->q_add_constant = q_add_constant;

	/* convert flood discharge */
	 if (rc_data->flood_flow >= 0.0)
	    rc_data->flood_flow = rc_data->flood_flow *
		                  q_mult_conver_factor +
			          q_add_constant;

	/* convert flood stage */
	 if (rc_data->flood_stg >= 0.0)
	    rc_data->flood_stg = rc_data->flood_stg *
			         stg_mult_conver_factor +
			         stg_add_constant;

	/* convert warning stage */
	 if (rc_data->warning_stg >= 0.0)
	    rc_data->warning_stg = rc_data->warning_stg *
				   stg_mult_conver_factor +
				   stg_add_constant;

	/* convert max of record discharge */
	 if (rc_data->max_record_q >= 0.0)
	    rc_data->max_record_q = rc_data->max_record_q *
				    q_mult_conver_factor +
				    q_add_constant;

	/* convert max of record stage */
	 if (rc_data->max_record_stg >= 0.0)
	    rc_data->max_record_stg = rc_data->max_record_stg *
				      stg_mult_conver_factor +
				      stg_add_constant;

      }
      else  /* NWSRFS_general_units == 1  metric units */
      {
	 for(i = 0; i < rc_data->rc_num; i++)
	 {
	    rc_data->rating_curve_stg[i] = rc_float[rc_data->loc_stg + i];
	    rc_data->rating_curve_q[i] = rc_float[rc_data->loc_q + i];
	 }
      }

      /*printf("i=%d  stg = %f  q = %f\n", i,
	       rc_data->rating_curve_stg[i], rc_data->rating_curve_q[i]);
      */

      /* set max and min values of time series */
      rc_data->max_q = rc_data->rating_curve_q[rc_data->rc_num-1];
      rc_data->max_stg = rc_data->rating_curve_stg[rc_data->rc_num-1];
      /*rc_data->min_q = rc_data->rating_curve_q[0];*/
      rc_data->min_q = 0.0;
      rc_data->min_stg = rc_data->rating_curve_stg[0];

      /* set max value for q_axis and increment for plotting x axis values */
      scale_max_min(rc_data->min_q, rc_data->max_q, 0.0,
		    &rc_data->min_q, &rc_data->q_axis_max,
		    &rc_data->rc_q_increment);

      /* set max value for stg_axis and increment for plotting y axis values */
      scale_max_min(rc_data->min_stg, rc_data->max_stg, 0.0,
		    &rc_data->min_stg, &rc_data->stg_axis_max,
		    &rc_data->rc_stg_increment);
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/rc_info.c,v $";
 static char rcs_id2[] = "$Id: rc_info.c,v 1.3 2002/02/11 19:32:22 dws Exp $";}
/*  ===================================================  */

}




