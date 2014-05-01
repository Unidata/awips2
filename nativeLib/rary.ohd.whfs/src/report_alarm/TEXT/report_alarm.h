
#ifndef REPORT_ALARM_H
#define REPORT_ALARM_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "DbmsDefs.h"          
#include "DbmsUtils.h"

#include "time_convert.h"
#include "get_limits.h"
#include "alert_util.h"

#include "AlertAlarmVal.h"      /* database tables */
#include "Location.h"
#include "State.h"
#include "ShefPe.h"
#include "ShefDur.h"
#include "ShefEx.h"
#include "DataLimits.h"



#define MISSING_VAL -9999.
#define DEFAULT_HOURS 24


/* mode definitions */

#define REPORT_ALL              100
#define REPORT_UNREPORTED       101
#define REPORT_RECENT           102
#define REPORT_FRESH            103
#define REPORT_NEAREST          104
#define REPORT_NEAR_NOW         105
#define REPORT_LATEST_MAXFCST   106
#define REPORT_NEW_OR_INCREASED 107

/* defaults */

#define DEFAULT_MODE         REPORT_NEAREST
#define DEFAULT_MINUTES      180
#define DEFAULT_MIN_VAL_DIFF 0.0

/* define a structure for holding all the user options */

typedef struct
{
   char	 product_id[11];
   int	 minutes;
   int	 mode; 
   char	 filter[7];
   char  PEfilter[SHEF_PE_LEN+1];
   char	 file_suffix[40];
   float min_val_diff;
}  report_options_struct;


/* prototypes */

void get_report_options(int			argc,
		        char			**argv,
		        report_options_struct	*opt);

void do_report_alarm(report_options_struct 	opt,
		     int			*num_alarms);


AlertAlarmVal *get_alarm_data(report_options_struct 	opt,
			      time_t			tnow);

AlertAlarmVal *process_group_data(report_options_struct 	opt,
				  FILE				*filePtr,
				  time_t			tnow,
				  AlertAlarmVal 		*aaHead,
				  AlertAlarmVal			*aaLastPtr,
				  int				*num_alarms);

void write_report_header(report_options_struct	opt,
			 time_t			tnow,
			 FILE			*filePtr);

void write_report_trailer(report_options_struct	opt,
			  FILE			*filePtr,
			  int			num_alarms);

void write_group_report(report_options_struct 	opt,
			FILE			*filePtr,
			time_t			tnow,
			int			group_cnt,
			AlertAlarmVal 		*startPtr,
			AlertAlarmVal		*endPtr,
			int			*num_alarms);

void write_group_header(FILE		*filePtr,
			AlertAlarmVal	*aaPtr);

void bld_timestamp(time_t 	timet,
		   char		*timestr);
		   
void print_report(FILE           *filePtr,
                  AlertAlarmVal  *aaPtr,
		  char           *valid_str,
		  char           *basis_info,
		  char           *dur_info,
		  char           *extr_info);		   
			  
	  			    
void build_string(AlertAlarmVal          *aaPtr,
	          time_t                 *valid_timet,
	          char                   *valid_ansi,
	          char                   *valid_str,
	          char                   *basis_ansi,
	          char                   *dur_info,
	          char                   *extr_info,			  
                  char                   *basis_info);			  
			  			  
			  
void update_database(AlertAlarmVal   *aaPtr,
                     char            *tnow_ansi,
		     char            *valid_ansi,
		     char            *basis_ansi);
		     

AlertAlarmVal *find_prev_actionPtr(AlertAlarmVal  *aaPtr,
                                   AlertAlarmVal  *startPtr,
                                   int            reverse_order,
				   int		  group_cnt);
				   
				   
AlertAlarmVal *find_max_fcstPtr(AlertAlarmVal    *aaPtr,
				AlertAlarmVal    *startPtr,
				int              reverse_order,
				int              group_cnt);  
				

int find_new_cnt(AlertAlarmVal         *aaPtr,
                 AlertAlarmVal         *startPtr,
		 time_t                tnow,
		 report_options_struct opt,
		 int                   reverse_order,
		 int                   group_cnt);
		 
		 				                               		     			  
			  
AlertAlarmVal *find_start_highPtr(AlertAlarmVal         *aaPtr,
                                  AlertAlarmVal         *startPtr,
				  AlertAlarmVal         *prev_actionPtr,
				  report_options_struct opt,
				  int                   reverse_order,
				  int                   group_cnt);
				  

int calculate_group_report(time_t             	tnow,
			   int                	group_cnt,
			   AlertAlarmVal      	*earliestPtr,
			   AlertAlarmVal      	*latestPtr,
			   report_options_struct opt);
			    				  
				  			  
#endif
