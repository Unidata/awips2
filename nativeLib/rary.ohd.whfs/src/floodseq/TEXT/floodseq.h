#ifndef FLOODSEQ_MAIN_H
#define FLOODSEQ_MAIN_H

#include "DbmsUtils.h"
#include "DbmsAccess.h"
#include "dbmserrs.h"
#include "DbmsDefs.h"

#include "Location.h"
#include "Riverstat.h"
#include "Height.h"
#include "FloodTs.h"

#include "time_convert.h"


/* defines */
 
#define MAX_INTERVAL_IN_HOURS  72
#define MISSING_VALUE          -9999.
#define MISSING_TIMESTR        "1970-01-01 00:00:00"


/* prototypes */

void do_floodseq(char 	*lid,
		 int	debug_mode);
void get_fldstg(char	*lid,
		int	*status, 
		double	*fs,
		char	*pe);
void display_usage_and_abort(char *command_name);
void end_msg(time_t	end_time);


bool hsa_list_syntax_is_good(const char* hsa_list, int hsa_list_len);
void log_syntax_error(const char* message, const char *hsa_list);

void floodseq_loc(char	*lid,
		  int	debug_mode);

void set_initial_info(FloodTs 	*floodtsHead,
		      Height	*heightHead,
		      double 	fldstg,
		      double 	*valprev,
		      dtime_t	*timeprev,
		      long	*sequence_number);

void log_floodts_rec(FloodTs ftsRec);
int is_dup(int return_code);
int check_interval(dtime_t	heighttime,
		   dtime_t	floodtime);

 
#endif
