/* mod_data.h */
/* Originally created by G. Smith - 950225
 * 
 * Used to define the mod_data structure that is
 * shared by run-time Mods routines and other
 * ifp functions.
 */

#ifndef mod_data_h
#define mod_data_h

typedef struct
	{
	 char       name[20][8];
	 char       type[20][8];
	 int        locp[20];
	} oper;

typedef struct
	{
	 char       id[20][8];
	 char       datatype[20][8];
	 int        delta_t[20];
	} time_ser;

typedef struct
	{
	int             mod_number;
	int             valid_for_fg;
	int             start_date;
	int             end_date;
	int             number_in_list;
	oper            operation;
	time_ser        time_series;
	}       mod_data;
	
#endif
