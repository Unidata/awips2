/*****************************************************************
   compute_stage_info.h
   
   PURPOSE
  
   
   ***************************************************************/
#ifndef COMPUTE_STAGE_INFO_H
#define COMPUTE_STAGE_INFO_H

#include <time.h>

#include "rpf_general_defs.h"        /* definitions */
#include "rpf_err_defs.h"         
#include "cat_and_product_defs.h"   

#include "fp_struct.h"               /* structures */
#include "grp_struct.h"
#include "county_struct.h"
#include "misc_struct.h"       

#include "rpf_converts.h"            /* functions protos */
#include "rpf_logs.h"

#include "DbmsUtils.h"		       /* database defs */
#include "DbmsDefs.h"

#define DEFAULT_STAGE_WINDOW  0.5

void compute_stage_info(int		fpindex,
			fp_struct	*fp);

void compute_obs_info(int		fpindex,
		      fp_struct		*fp);

void compute_fcst_info(int		fpindex,
		       fp_struct	*fp);

void load_trend_info(int 		fpindex,
		     fp_struct		*fp);

void load_stage_in_interval(int		fpindex,
			    int		interval,
			    time_t	system_time,
			    fp_struct	*fp);

void load_special_stages(int 		fpindex,
			 fp_struct	*fp,
			 int		obs_or_fcst);

void compute_grp_county_full_info(fp_struct		*fp,
				  int			numgrps,
				  grp_struct	        *grp,
				  int                	numcnty,
				  county_struct      	*cnty);


void compute_fp_risefall(int        fpindex,
                         fp_struct  *fp);
			 
			 
void compute_detailed_fcst_info(int         fpindex,
                                fp_struct   *fp);	
				
void load_detailed_trend_info(int       fpindex,
                              fp_struct *fp);
			      
void twopoints_flat_hydrograph(double      pt1_value,
		               time_t      pt1_time,
		               double      pt2_value,
		               time_t      pt2_time,
			       int         *cnt,
			       int         fpindex,
			       fp_struct   *fp); 
			      
void threepoints_crestflat_hydrograph(double      pt1_value,
		                      time_t      pt1_time,
		                      double      pt2_value,
		                      time_t      pt2_time,
				      double      pt3_value,
				      time_t      pt3_time,
			              int         *cnt,
			              int         fpindex,
			              fp_struct   *fp); 

void threepoints_valleyflat_hydrograph(double      pt1_value,
		                       time_t      pt1_time,
		                       double      pt2_value,
		                       time_t      pt2_time,
				       double      pt3_value,
				       time_t      pt3_time,
			               int         *cnt,
			               int         fpindex,
			               fp_struct   *fp); 				      
				
void fourpoints_flat_hydrograph(double      pt1_value,
		                time_t      pt1_time,
		                double      pt2_value,
		                time_t      pt2_time,
			        int         *cnt,
			        int         fpindex,
			        fp_struct   *fp); 
			      				
			      			      			      	
			      
void rise_hydrograph(double       pt1_value,
                     time_t 	   pt1_time,
		     double       pt2_value,
		     time_t       pt2_time,
		     int          *cnt,
		     int          fpindex,
		     fp_struct    *fp,
		     int          case_index,
		     int          second_rise_fourpoints);
			     
void fall_hydrograph(double       pt1_value,
                     time_t       pt1_time,
		     double       pt2_value,
		     time_t       pt2_time,
		     int          *cnt,
		     int          fpindex,
		     fp_struct    *fp,
		     int          case_index);	
		     
double round_float(double input_num);

void load_tidal_trendinfo(fp_struct  *fp,
                          int        fpindex);		     		    
			     		      					 

#endif

