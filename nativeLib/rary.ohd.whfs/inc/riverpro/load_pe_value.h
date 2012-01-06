/*********************************************************************
   load_pe_value.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef LOAD_PE_VALUE_H
#define LOAD_PE_VALUE_H

#define TRACE_THRESHOLD .00999
#define DEFAULT_MIN_PERCENT  0.25

#include "rpf_err_defs.h"            /* riverpro definitions */
#include "rpf_general_defs.h" 
#include "temp_records_struct.h"
#include "function_defs.h"

#include "rpf_logs.h"                /* other riverpro function protos */
#include "time_convert.h"
#include "build_ugc.h"

#include "Observation.h"             /* database structures */
#include "Forecast.h"
#include "IngestFilter.h"
#include "ShefPETrans.h"
#include "LoadUnique.h"
#include "ShefPe.h"
#include "ShefProb.h"
#include "DbmsDefs.h"		     /* other definitions */
#include "CurPP.h"
#include "RawPP.h"

#include "precip_total.h"            /* functions outside of RiverPro */
//#include "load_precip.h"
//#include "CurPrecip.h"
#include "QualityCode.h"
#include "rating_util.h"              /* for rating table conversions */
#include "set_timevals.h"         
#include "get_total_precip.h"

void load_pe_value(int			filter_qcrange,
		   char			*lid,
		   varinfo_struct	varinfo, 
		   values 		*rawvalue,
		   char			*dqcode,
		   char			longstring[]);

void load_pe_obsvalue(int		filter_qcrange,
		      char		*lid,
		      varinfo_struct	varinfo, 
		      values 		*rawvalue,
		      char		*dqcode,
		      char		longstring[]);

void load_pe_fcstvalue(int		filter_qcrange,
		       char		*lid,
		       varinfo_struct	varinfo, 
		       values 		*rawvalue,
		       char		*dqcode,
		       char		longstring[]);

Observation * load_pe_obsdata(int		filter_qcrange,
			      char		*lid,
			      varinfo_struct	varinfo);

Forecast * load_pe_fcstdata(int			filter_qcrange,
			    char		*lid,
			    varinfo_struct	varinfo);

void build_pe_where(int			filter_qcrange,
		    char		*lid,
		    varinfo_struct	varinfo,
		    char		*ts,
		    char		*tablename,
		    char		*where);

void load_precip_value(char		*lid,
		       varinfo_struct	varinfo, 
		       values 		*rawvalue,
		       char		*dqcode);


void get_nearest_obs(Observation 	*obsHead,
		     time_t		datatime,
		     time_t		seconds_window,
		     int		time_flag,
		     values 		*rawvalue,
		     char		*dqcode);

void get_nearest_fcst(Forecast 	*fcstHead,
		      time_t	datatime,
		      time_t	seconds_window,
		      int	time_flag,
		      values 	*rawvalue,
		      char	*dqcode);


void get_derived_obs(Observation 	*obsHead, 
		     varinfo_struct	varinfo, 
		     values 		*rawvalue, 
		     char 		*dqcode);

void get_derived_fcst(Forecast 		*fcstHead, 
		      varinfo_struct	varinfo, 
		      values 		*rawvalue, 
		      char 		*dqcode);

void get_converted_value(char		*lid,
			 varinfo_struct	varinfo,
			 values 	*rawvalue);

void get_PPdur_match(CurPP	*cppHead,
		     int	dur,
		     time_t	datatime,
		     int	latest_flag,
		     time_t	*valid_time,
		     float 	*value, 
		     char 	*valind);

void translate_weather(float	fltvalue,
		       char	longstring[]);

void check_if_translate(char 	*pe,
			float 	value,
			char 	*translation);
			
void unit_conversion(values          *rawvalue,
		     varinfo_struct  varinfo);	
		     
void load_paired_value(values         *rawvalue,
                       varinfo_struct varinfo);		     		

#endif
