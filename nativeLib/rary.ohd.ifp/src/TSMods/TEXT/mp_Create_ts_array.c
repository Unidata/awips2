#include "mods_plot.h"
#include <X11/Intrinsic.h>
#include "libXs.h"
#include "ifp_globals.h"
#include "c_call_f/fdcode.h"
#include "c_call_f/fconvt.h"

/* File: mp_Create_ts_array.c
 *
 * mp_Create_ts_array:  function to fill the time series and original
 *                      time series arrays with data for the UHCHNG,
 *                      ROCHNG, and RRICHNG time series mods called from.
 *                      mods_plot(mp).
 */

 void mp_Create_ts_array(num_ts, mp_ts_array, mp_orig_ts_array,
			 end, start_ts, mod_type_sw, unit_sw, the_mods_data)

   int             num_ts;             /* Number of time series                   */
   int             end;                /* Number of values in the time series     */
   int             mod_type_sw;        /* Switch giving the type of mod being plotted */
   int             unit_sw;            /* Unit switch (no longer used - dp 8/26/94) */
   float           **mp_ts_array;      /* Address of pointer to time series array  */
   float           **mp_orig_ts_array; /* Address of pointer to original time series array  */
   float           *start_ts[];        /* Array holding the starting location in the d array
					   of the time series plotted                       */
   mod_data        *the_mods_data;
{
   int             i, j;               /* counters              */
   char            type_string[4];     /* data type code */
   char            std_units[4];       /* Code for the standard forecast component */
   char            dimen[4];           /* Data type dimension                      */
   char            time_scale[4];      /* Time scale interval */
   int             missing_allowed;    /* Indicates if missing data are allowed for
					  this data type in the forecast component. */
   int             nv_dt;              /* number of values per time interval for this data type. */
   int             nadd;               /* Number of pieces of additional information */
   int             err_flag;           /* Error flag, 0 no error, 1 indicates not a valid data type */
   char            std_Eng_units[4];   /* Standard English units                   */
   float           mult_conver_factor; /* Multiplication conversion factor         */
   float           add_constant;       /* Addition constant                        */


   if(mods_general_units == 0)  /* English units */
   {
      /* determine the conversion factors from metric to English units */
    
      if(mod_type_sw == UH || mod_type_sw == UHD )
      {
	   mult_conver_factor = .897;
	   add_constant       = 0.00;
      }
      else  /* RRICHNG or ROCHNG */
      {
	 strncpy(type_string, the_mods_data->time_series.datatype[0], 4);
	 FDCODE(type_string, std_units, dimen, &missing_allowed, &nv_dt,
		time_scale, &nadd, &err_flag);
	 FCONVT(std_units, dimen, std_Eng_units, &mult_conver_factor,
		&add_constant, &err_flag);
      }
      
      /* fill arrays with converted values */
      for(i = 0; i < num_ts; i++)
         for(j = 0; j <= end; j++)
	    if(start_ts[i][j] >= 0.00)     /* check to see if set to missing (-999) */
	    {
	       mp_ts_array[i][j] = start_ts[i][j] * mult_conver_factor
	   	                   + add_constant;
	       mp_orig_ts_array[i][j] = start_ts[i][j] * mult_conver_factor
				        + add_constant;
	       /*printf("ts_array[%d][%d] = %f\n", i, j,
		         mp_ts_array[i][j]);*/
	    }
	    else  /* value is -999 so set it to 0.00 in the array */
	    {
	       mp_ts_array[i][j] = 0.00;
	       mp_orig_ts_array[i][j] = 0.00;
	    }
   }
   else  /* metric units */
      for(i = 0; i < num_ts; i++)
	 for(j = 0; j <= end; j++)
	    if(start_ts[i][j] >= 0.00)     /* check to see if set to missing (-999) */
	    {
	       mp_ts_array[i][j] = start_ts[i][j];
	       mp_orig_ts_array[i][j] = start_ts[i][j];
	    }
	    else  /* value is -999 so set it to 0.00 in the array */
	    {
	       mp_ts_array[i][j] = 0.00;
	       mp_orig_ts_array[i][j] = 0.00;
	    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/mp_Create_ts_array.c,v $";
 static char rcs_id2[] = "$Id: mp_Create_ts_array.c,v 1.4 2004/09/09 16:13:35 wkwock Exp $";}
/*  ===================================================  */

}
