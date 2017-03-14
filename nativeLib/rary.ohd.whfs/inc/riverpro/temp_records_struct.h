/************************************************************************
   temp_records_struct.h
   
   PURPOSE
   Include file for structures with the format, variables, and spectime
   buffers of the template, whether used directly for the tabular
   template or indirectly for the other templates.
      		
   NOTES
   The size field in the format_struct record is used to store
   other data besides the size.
   
  ***********************************************************************/


#ifndef TEMP_RECORDS_STRUCT_H
#define TEMP_RECORDS_STRUCT_H

#include "temp_item_struct.h"
#include "temp_varinfo_struct.h"
#include "template_defs.h"
#include "DbmsDefs.h"


/* contains the info for the format buffer of the template;
   the type is the type of the variable for which the format applies;
   the type_size is the type of the format specifier;
   size is the format specifier size in the case of most variables;
   for time type variables, it is an index to the time variable. 
   the type_size, if applicable, must agree with the size value */

typedef char type_usertime[30];


typedef struct
{
   int			num_of_formats;
   itemtypes		type[MAX_FORMAT_ITEMS];
   itemtypes		type_size[MAX_FORMAT_ITEMS];
   values		size[MAX_FORMAT_ITEMS];
   type_usertime	usertime[MAX_FORMAT_ITEMS];
} format_struct;





/* contains the info for the variables buffer of the tabular template */

typedef struct
{
   int			num_of_variables;
   varinfo_struct	varinfo[MAX_FORMAT_ITEMS];
} variable_struct;


/* contains the info for the specific times buffer */

typedef struct
{
   int		num_of_spectimes;
   time_t 	basetime;
   int 		relday[MAX_SPECTIMES];
   int 		relhour[MAX_SPECTIMES];
   int 		window[MAX_SPECTIMES];
} spectime_struct;


/* contains the info for the pe variable time buffer.
   this is used for a temporary holding area of instructions */

typedef struct
{
   int		latest_flag;
   int		next_flag;
   time_t 	basetime;
   int 		hr_window;
} petime_struct;

#endif
