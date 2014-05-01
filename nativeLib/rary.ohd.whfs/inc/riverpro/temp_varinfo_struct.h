/************************************************************************
   temp_varinfo_struct.h
   
   
  ***********************************************************************/


#ifndef TEMP_VARINFO_STRUCT_H
#define TEMP_VARINFO_STRUCT_H

#include "template_defs.h"
#include "DbmsDefs.h"

/* contains the attributes for defining the variable structure */

typedef struct 
{
   int		varindex;
   
   /* the below fields are for the core PE variable attributes */
   
   char		lid[LOC_ID_LEN + 1];
   char		pe[SHEF_PE_LEN + 1];
   int		dur;
   char		ts[SHEF_TS_LEN + 1];
   char		extremum[SHEF_EX_LEN + 1];
   char         probcode[SHEF_PROB_LEN + 1];
 
   int		latest_flag;
   int		next_flag;
   time_t	datatime;
   int		hr_window;
   
   
   /* derived instructions for supported derived instructions.
      CHG_VAL, MIN_VAL, MAX_VAL, AUTO_ACCUM, GAGEZERO, MSL, FLOW, STAGE */
   
   int		derive_flag;   /* master indicator */
   
   int		chg_flag;
   int		min_flag;
   int		max_flag;
   int		accum_flag;
   int		gagezero_flag;
   int 		msl_flag;
   int		flow_flag;
   int		stage_flag;
   int          metric_flag;
   int          pv_flag;
   int          pv_value; /*applies to pv_flag*/
   
   int		derive_numhrs;  /* applies to some derive flags */
   
   /* this field indicates whether the pe variable is used
      for the value or the time of the value.  one could
      get the same information from the varindex, but this
      makes it easier since the varindex would need to be
      compared against the variable name to see whether
      its for a value or a time. */
   
   int			time_flag;
} varinfo_struct;

#endif
