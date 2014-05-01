/************************************************************************
   fp_struct.h
   
   PURPOSE
   Serves as include file for most of the information that 
   is associated with each forecast point.  This includes the 
   static E19 type data, and the dynamic data such as the
   previous product info for the forecast point, the current
   observed and max forecast info for the forecast point (aka MOFO
   or OMF), and the full time series info for the forecast point.
   
   NOTES ABOUT STAGE/DISCHARGE DATA:
   RiverPro needs stage/discharge data for the following purposes:
   
   (1) for use in determining recommendations, i.e.
   a) product type, which in turn implies a default pcc set;
   b) included points; and
   c) impact and crest comparisons for each forecast point.   
   The data for recomms is read at startup, is always available,
   and is updated as necessary or upon user request.  These data are
   stored in the fp and grp structures, for individual forecast point
   and forecast groups, respectively.
   d) Consider recommendation product and forecast points included for
   cnrfc modes, add fields omf_monitorcat, prev_omf_monitorcat,
   monitorrise_or_fall.
   
   (2) for user display of particular stage data (interactive only).   
   The data for display is retrieved on an as-needed basis.
      
   (3) for the template variables used when generating the product.   
   The data for template variables is read as the product is
   being created, with only a single station's data buffered
   in memory.
   
   Data for previous products, i.e. carryover data, is contained in the
   fp structure. The categorical stage values are determined as-needed.
   
   The compute_stage... functions determine the derived data for the stage
   data.  The functions are: obs, fcst, obsfcst, fpprev, grp.   
   Except for the compute grp info function, they all operate on one
   forecast point at a time.
     
   In the "engine" code, the compute_... functions are called by:   
   rpf_main() --> compute_stage_info()
   			|-->compute_obs_info(), compute_fcst_info(), [per fp]
			|-->compute_other_grp_info()
      
   In the "gui" code, the compute_functions are called by:   
   stageview_actions.c, import_stageview()
   			|-->compute_obs_info(), compute_fcst_info() [per fp]
   rpf_actions.c, prod_create() --> compute_stage_info()
   			|--> see above
   rpf_actions.c, also by issue_product, reset_to_recs
   recinfo_actions.c, also by refresh_data
   offices_actions. - for switching offices
   

   
  ***********************************************************************/

#ifndef FP_STRUCT_H
#define FP_STRUCT_H

#include <time.h>

#include "DbmsDefs.h"
#include "cat_and_product_defs.h"
#include "Report.h"


/* define the structure definition */
 
typedef struct
{
   
   /* static E19 type data */
   
   char		id[LOC_ID_LEN + 1];
   char 	name[LOC_NAME_LEN + 1];
   
   char		county[COUNTY_LEN + 1];
   char 	state[STATE_LEN + 1];
   
   char 	stream[STREAM_NAME_LEN + 1];
   double 	bf;
   double 	wstg;
   double 	fs;
   double 	fq;
   double       aq;
   
   char		pe[SHEF_PE_LEN +1];
   int		use_latest_fcst;
   
   char 	proximity[PROXIMITY_LEN + 1];
   char 	reach[SHORT_LEN + 1];
   char		grpid[LOC_ID_LEN + 1];
      
   float 	cat[MAX_CAT];
   
   char		hsa[HYD_SERV_LEN + 1];
   char		primary_back[HYD_SERV_LEN + 1];
   char		secondary_back[HYD_SERV_LEN + 1];
      
   /*rec_type previously used for
       recommendation type, now it is used to determine
       output the stage or flow value for Impact and historical
       comparison variables*/
   
   char         rec_type[RECTYPE_LEN + 1];
    
   /* stage/flow change for non-steady assumption  */

   double        chg_threshold; 
   
   /* look back hours for observed data, look forward hours for fcst data,
      adjusted end hours for PVTEC line */
      
   int          backhrs;      
   int          forwardhrs;
   double       adjustendhrs;

   /* dynamic previous product information; loaded by load_fpprev_data()
      in get_fp_grp_county.c */
   
   int		prev_avail;
   
   char	   	prev_prod_categ[PROD_CATEG_LEN + 1];   
   time_t  	prev_prodtime;
   
   double       prev_curobs_val;
   time_t  	prev_curobs_time;
   
   double       prev_maxfcst_val;
   time_t  	prev_maxfcst_time;
   time_t  	prev_maxfcst_ctime;
   
   int 	   	prev_omf_cat;   
   
    
   /* current river data loaded together with the fp and grp info in
      call to get_mofo() in get_stages.c. note that the time of the
      curobs and maxfcst are stored in the Report structure, not
      as separate variables like they are for groups and counties. */
   
   Report	curobs;
   int		curobs_cat;
   
   Report	maxfcst;
   int 		maxfcst_cat; 
   
   
   /* these values are determined via compute_fp_mofo_info() 
      in get_stages.c */
   
   float	omf;
   int		omf_cat;
   time_t	omf_time;
   
      
   /* the risefall flags are determined via compute_fp_prev_info() 
      in get_stages.c. note that these variables refer to the
      rise or fall with reference to the previous product, not
      the current time series. */
   
   int rise_or_fall;  
   int obs_rise_or_fall;
   int fcst_rise_or_fall;
      
   
   /*------------------------------------------------------------------*/
   /*------------------------------------------------------------------*/
   /* observed and forecast values for complete time-series. 
      loaded in get_obsfcst_ts() in get_stages.c.
      keep track of when the full time series data are loaded. 
      the use_obsH keeps track of how much of the observed time
      series to use, based on the previous VTEC event end time */
   
   int		numobsH;
   int 	        use_obsH;
   time_t	obs_cutoff_time;
   time_t	obs_load_time;
   Report	*obsH;
   
   int		numfcstH;
   Report	*fcstH;
   
   time_t	fullts_loaded_time;
   
   /*--------------------------------------------------------------*/
   /* derived observed values from the full time-series*/
      
   /* values that will be set if at least one stage value;
      these values are determined in compute_obs_info() */
   
   int obs_cur_index;
   int obs_max_index;
   double obs_FSdeparture;
   
   int obs_max24_index;
   int obs_max06_index;
   
   
   /* values that may or may not be set depending upon situation,
      but will only be set if at least one stage value available;
      these values are determined via compute_obs_info() */
   
   time_t obs_fallbelow_time;
   time_t obs_riseabove_time;
   
   double obs_crest_value;
   time_t obs_crest_time;
   

   /*---------------------------------------------------------------*/
   /* derived forecast values from the full time-series*/
   
   /* these values that will be set if at least one stage value;
      these values are determined in compute_fcst_info() */
   
   int fcst_max_index;
   double fcst_FSdeparture;
   int fcst_xfcrest_index;
   
      
   /* values that may or may not be set depending upon situation,
      but will only be set if at least one stage value available;
      these values are determined via compute_fcst_info() */
   
   time_t fcst_fallbelow_time;
   time_t fcst_riseabove_time;
   
   double fcst_crest_value;
   time_t fcst_crest_time;
   
   
   /* derived detailed forecast values from the full time-series;
      determined in comput_detailed_fcst_info() */

   double  fcst_max_value;
   time_t  fcst_maxvalue_time;
   double  fcst_min_value;
   time_t  fcst_minvalue_time;
   double  fcst_first_value;
   time_t  fcst_firstvalue_time;
   double  fcst_last_value;
   time_t  fcst_lastvalue_time;
   

   /* derived trend phrase index, stage value and time from 
      load_detailed_fcst_info() */
   
   int    action_index[MAXNUM_OF_TRENDPHRASE];
   double action_value[MAXNUM_OF_TRENDPHRASE];
   time_t action_time[MAXNUM_OF_TRENDPHRASE];
   
   
   /* values for if this forecast point is tidal point */
    
   int  fcstpoint_type;
   
   
   /* overall fall, rise times */
   
   time_t fallbelow_time;
   time_t riseabove_time;
   
   
   /* the type source corresponding to the overall fall, rise, crest */
   
   char  fallbelow_ts[SHEF_TS_LEN + 1];
   char  riseabove_ts[SHEF_TS_LEN + 1];
   char  crest_ts[SHEF_TS_LEN + 1];
   
   
   /* trend values; for observed trend and overall trend */
   
   int	obs_trend;
   int	trend;
      
} fp_struct;

#endif
