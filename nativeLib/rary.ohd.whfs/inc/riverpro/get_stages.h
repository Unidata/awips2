/*********************************************************************
   
   get_stages.h
      
   ******************************************************************/

#ifndef GET_STAGES_H
#define GET_STAGES_H

#include "rpf_err_defs.h"              /* definitions */
#include "rpf_general_defs.h"         
#include "cat_and_product_defs.h"     

#include "fp_struct.h"                 /* structures */
#include "grp_struct.h"
#include "county_struct.h"
#include "misc_struct.h"
#include "pcc_struct.h"
#include "vtecinfo_struct.h"

#include "rpf_logs.h"                  /* protos */
#include "compute_stage_info.h"


#include "time_convert.h"              /* other protos */
#include "bldts_height.h"
#include "get_best_ts.h"
#include "get_curobs_maxfcst.h"
#include "set_timevals.h"

#include "DbmsDefs.h"                  /* database utility */
#include "DbmsAccess.h"

#include "RpfParams.h"                 /* database tables */
#include "RiverStatus.h"


extern int load_fcstfull_flag;


void compute_fp_mofo_info(int		fpindex,
			  fp_struct	*fp);

void compute_fp_prev_info(int		fpindex,
			  fp_struct	*fp);

void compute_grp_county_mofo(fp_struct		*fp,
		             int		numgrps,
		             grp_struct	        *grp,
			     int                numcnty,
			     county_struct      *cnty);		     
 
void get_fpmofo_and_ts_ifneeded(int	        fpindex,
		                fp_struct       *fp,
			        vtecinfo_struct *vtecinfo,
			        pcc_struct      *pcc);
				   
void apply_obsdata_filter(int		        fpindex,
	                  fp_struct	        *fp,
			  vtecinfo_struct	*vtecinfo,
			  pcc_struct            *pcc);
			  
int compute_stage_cat(float	cat_vals[],
		      double	dataval);

void check_stagedata(int		fpindex,
		     fp_struct		*fp);
		     
void load_rpf_fcstdata_lidpe(char 	      *tablename,                               
		                char	      *lid,
			        char	      *pe,
				time_t        end_validtimet,	
				time_t        basis_btime,
				int           fpindex,		
				fp_struct     *fp);
				
void load_rpffcst_item(char 	    *lid, 
		       char	    *pe,
		       char	    *ts,
		       time_t    end_validtimet,
		       time_t    basis_btime,
		       int       fpindex,
		       fp_struct *fp,		
		       int *     max_fcst_found);
			  
void bldts_rpffcstriv(char		*lid,
		      char		*pe,
		      char 	        *ts_filter,
		      int		use_qcfilter,    
		      int		use_latest,
		      time_t	        end_validtimet,
		      time_t            basis_btime,
		      Report     	**fcst,
		      int 		*num_fcst);
		      
void get_rpf_curobs(char	   *lid,
		    char 	   *pe,
		    int		   obs_hours_ago,					 
		    int		   *obs_found,
		    struct Report  *obsReport);		      			  				
						     

 
#endif
