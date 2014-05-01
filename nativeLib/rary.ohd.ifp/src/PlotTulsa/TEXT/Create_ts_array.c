#include <X11/Intrinsic.h>
#include "libXifp.h"
#include "mod_struct.h"
#define  ChooseTStypestruct
#include "mods_info.h"
#include "libXs.h"
#include "ifp_globals.h"
#include "c_call_f/fdcode.h"
#include "c_call_f/fconvt.h"

/* File: Create_ts_array.c
 *
 * Create_ts_array()
 *
 *
 * fdcode() returns information on all valid data types for the forecast component.
 *
 * fconvt() converts when required metric units to standard English units used by the
 * forcast component.
 *
 */

void Create_ts_array(num_ts, ts_array, d_float, d_locts,
		     data_offset, end, orig_ts_array, ts_info, start_run, end_run, novrsn)

   int             data_offset;        /* Offset from the first element of the
					  time series array */
   int             end;                /* Number of data points in the time series */
   int             d_locts[];          /* Location of time series in the data array */
   float           **ts_array;         /* Address of pointer to time series array  */
   float           **orig_ts_array;    /* Address of pointer to original time series array */
   float           d_float[];          /* Time series floating point data     */
   TS_INFO         *ts_info;           /* Pointer to time series information structure */
   int             *start_run;         
   int             *end_run;
   int             novrsn;             /* Tul Plot version number */
{
   int             i,j;                /* counters                          */
   int             index;              /* array index                       */
   int             missing_allowed;    /* Indicates if missing data are allowed for
					  this data type in the forecast component. */
   int             nv_dt;              /* number of values per time interval for this data type. */
   int             nadd;               /* Number of pieces of additional information */
   int             err_flag;           /* Error flag, 0 no error, 1 indicates not a valid data type */
   char            std_Eng_units[4];   /* Standard English units                   */
   char            type_string[4];     /* data type code */
   char            std_units[4];       /* Code for the standard forecast component */
   char            dimen[4];           /* Data type dimension                      */
   char            time_scale[4];      /* Time scale interval */
   float           mult_conver_factor; /* Multiplication conversion factor         */
   float           add_constant;       /* Addition constant                        */
   int             dttmp;              /* time series time interval */
   int             end_ts;             /* actual number of data point in the time series */

   for(i = 0; i < num_ts; i++)
   {
      index = d_locts[i] + data_offset - 1;
      end_ts = end;
      if (novrsn == 2)
      {
         dttmp = ts_info[i].delta_t;
         end_ts = ((*end_run - *start_run)/dttmp + 0.01);
      }
      
      if(NWSRFS_general_units == 0)
      {
	 strncpy(type_string, ts_info[i].data_type, 4);
	 FDCODE(type_string, std_units, dimen, &missing_allowed, &nv_dt,
		time_scale, &nadd, &err_flag);
	 FCONVT(std_units, dimen, std_Eng_units, &mult_conver_factor,
		&add_constant, &err_flag);
         
         for(j = 0; j <= end_ts; j++)  
	 {
	    if(d_float[index + j] < -990)
	    {
	       orig_ts_array[i][j] = d_float[index + j];
	       ts_array[i][j]      = d_float[index + j];
          /*   printf("ts_array[%d][%d] = %f d_array[%d] = %f\n",
                        i, j, ts_array[i][j], index+j, d_float[index+j]); */
	    }
	    else
	    {
	       orig_ts_array[i][j] = d_float[index + j] * mult_conver_factor
					+ add_constant;
	       ts_array[i][j]      = d_float[index + j] * mult_conver_factor
					+ add_constant;
	  /*   printf("ts_array[%d][%d] = %f d_array[%d] = %f\n",
			i, j, ts_array[i][j], index+j, d_float[index+j]); */
	       
	    }
	 }
      }
      else
      {
	 for(j = 0; j <= end_ts; j++)
	 {
	    orig_ts_array[i][j] = d_float[index + j];
	    ts_array[i][j]      = d_float[index + j];
	    /*printf("ts_array[%d][%d] = %f d_array[%d] = %f\n",
		     i, j, ts_array[i][j], index+j, d_float[index+j]);
	    */
	 }
      }
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/Create_ts_array.c,v $";
 static char rcs_id2[] = "$Id: Create_ts_array.c,v 1.4 2002/02/11 19:24:19 dws Exp $";}
/*  ===================================================  */

}
/* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> */
