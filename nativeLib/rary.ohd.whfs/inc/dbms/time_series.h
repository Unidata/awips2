/*
	File:		time_series.h
*/


#ifndef TIME_SERIES_H
#define TIME_SERIES_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "DbmsDefs.h"
#include "time_defs.h"

#include "Report.h"
#include "ToolDefs.h"


Report* ts_malloc (time_t 	begin_time,
		   time_t 	end_time,
		   time_t 	interval,
		   int 		*elements);
		      
int ts_normalize(Report report_ts[],
		 Report norm_ts[],
		 int 	num_reports,
                 int 	norm_elements,
		 time_t begin_time,
		 time_t end_time,
                 time_t interval,
		 int 	interp_mode);	      
		  
double ts_assign_value(Report 	report_ts[],
		       int 	report_num,	    
		       time_t 	t,
		       time_t 	interval,
		       int 	interp_mode);
		  
int ts_accum_to_inc(Report 	input_ts[],
		    Report 	output_ts[],
		    int 	report_num);	

int ts_accum_to_inc2(Report 	input_ts[],
		    Report 	output_ts[],
		    int 	report_num);	



void ts_fill(const 	Report 	report_ts[],
	     const 	int 	num_reports,
	     const 	time_t 	begin_time,
	     const	time_t 	end_time,
	     const	time_t 	interval,
		       	int 	norm_mode,
		       	Report 	norm_ts[],
			int	*num_norm);

double ts_assignval(const 	Report 	report_ts[],
		    const	int 	num_reports,
		    const	time_t 	norm_time,
		    const	time_t 	interval,
		    		int 	norm_mode);

		  




int read_precip_obs_ts(char *lid, time_t begin, time_t end,
		       Report **r_ts, int *r_count);
		       

#endif
