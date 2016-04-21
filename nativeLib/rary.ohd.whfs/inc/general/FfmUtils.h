#ifndef FFMS_UTIL_H
#define FFMS_UTIL_H

#include <time.h>
#include <math.h>

#include "ContingencyValue.h"
#include "ProcValue.h"
#include "DPARadar.h"
//#include "Stage2Result.h"

#include "ArealProductSettings.h"

/* defines */

#ifndef PI
#define PI 3.1415926
#endif

#define OUTSIDE_RADAR_RANGE -8888

#define DEFAULT_MIN_DURATION_FILLED 0.83
#define DEFAULT_MIN_AREA_COVERED .80
#define DEFAULT_STAGE1_MINUTES_WINDOW 10
#define DEFAULT_STAGE1LIST_LOOKBACK 3


/* product retrieval functions */

void bld_areal_product(ArealProductTypeDescriptor	prod_descr,
		       ArealProductSpecifier		prod_spec,
		       ArealProduct			*product);

void bld_precip_data(ArealProductTypeDescriptor	prod_descr,
		     ArealProductSpecifier	prod_spec,
		     ArealProduct		*product);

void bld_stage1_data(ArealProductTypeDescriptor	prod_descr,
		     ArealProductSpecifier	prod_spec,
		     float			min_duration_filled,
		     ArealProduct		*product);
   
void bld_stage2_data(ArealProductTypeDescriptor	prod_descr,
		     ArealProductSpecifier	prod_spec,
		     float			min_duration_filled,
		     ArealProduct		*product);

ArealData * bld_ptprecip(ArealProductTypeDescriptor	prod_descr,
			 ArealProductSpecifier		prod_spec,
			 float				min_duration_filled,
			 int				*precip_status,
			 long				*precip_cnt);

ArealData * bld_qpf_data(ArealProductTypeDescriptor	prod_descr,
			 ArealProductSpecifier		prod_spec,
			 int				*precip_status,
			 long				*precip_cnt);

void bld_ffg_data(ArealProductTypeDescriptor	prod_descr,
		  ArealProductSpecifier		prod_spec,
		  ArealProduct			*product);

float * accum_grids(ArealProductTypeDescriptor	prod_descr,
		    ArealProductSpecifier	prod_spec,
		    int				*numhrs_found);
/*
Stage2Result * find_match_stage2(Stage2Result	*stage2Head,
				 time_t		desired_hour);
*/
void assign_special_bins(char 	*radid,
			 float	*outgrid_vals);

ArealData * bld_ffg_area(char 	*boundary_type,
			 int	ffg_hours,
			 time_t	ffgtime,
			 int	since_flag,
			 int	*ffg_status,
			 long	*ffg_cnt);

void map_ffg_grid_to_point(char		*radid,
			   time_t	endtime, 
			   ArealProduct	*product);


/* bld ffg grid functions ---------------------------------------------------*/

float *  bld_ffg_grid(char		*radid,
		      char		*areatypes_usage,
		      int		ffg_hours,
		      time_t		sincetime,
		      int		*ffg_status);

float * bld_qpf_grid(char		*radid,
		     int		qpf_hours,
		     time_t		validtime,
		     int		*qpf_status);

int load_data_gridcells(double			south_row,
			double			west_col,
			char			*lid,
			double			value,
			float			ffg_grid[]);


/* accum_map functions ------------------------------------------------------*/

ArealData * accum_map(ArealProductTypeDescriptor	prod_descr,
		      ArealProductSpecifier		prod_spec,
		      float				min_duration_filled,
		      int				*status,
		      long				*precip_cnt);

int do_map_area(char 		*lid,
		PrecipType	precipType,
		time_t		begintime,
		time_t		endtime,
		float		*accumval,
		int		*hrs_filled);

void total_map_data(time_t 		start_time,
		    time_t 		obs_time,
		    ProcValue 		*procHead,
		    float		*value,
		    int			*hrs_filled);

/* special accum_map function -------------------------------------------*/

ArealData * accum_map_from_grid(ArealProductTypeDescriptor	prod_descr,
				ArealProductSpecifier		prod_spec,
				float				*grid_vals,
				int				*status,
				long				*precip_cnt);


/* check_precip grid prods ---------------------------------------------- */


typedef struct ArealGridStatus
{
   time_t		grid_timet;
   ArealDataStatus	dataStatus;
}  ArealGridStatus;


int check_precipgrid_prods(ArealProductTypeDescriptor	class_descr,
			   ArealProductSpecifier	*prod_spec,
			   int				nums_prodspecs,
			   ArealDataStatus		*product_status);

void scan_spec_timerange(ArealProductSpecifier	*prod_spec,
			 int			nums_prodspecs,
			 time_t			*startTime,
			 time_t			*endTime);

ArealGridStatus * load_precip_gridlist(PrecipType	precip_type,
				       char 		*sourceId,
				       time_t		startTime,
				       time_t		endTime,
				       int		*num_grids);  

void check_precip_avail(ArealProductTypeDescriptor 	class_descr,
			ArealProductSpecifier		*prod_spec,
			int				nums_prodspecs,
			char				*sourceId,
			float				min_percent_duration,
			int				minutes_window,
			ArealGridStatus			*gridstatus_list,
			int				num_grids,
			ArealDataStatus			*product_status);



/* build prodlist functions ---------------------------------------------- */

void bld_prodlist(ArealProductTypeDescriptor	prod_descr,
		  int				one_hour_only,
		  ArealProductSpecifier		*preset_specs,
		  int				num_presets_specs,
		  int				max_specs,
		  ArealProductSpecifier 	*specs,
		  int				*num_specs);

void bld_stg1list(ArealProductTypeDescriptor	prod_descr,
		  int				one_hour_only,
		  ArealProductSpecifier		*preset_specs,
		  int				num_preset_specs,		  
		  int				max_specs,
		  ArealProductSpecifier 	*specs,
		  int				*num_specs);

void bld_stg2list(ArealProductTypeDescriptor	prod_descr,
		  int				one_hour_only,
		  ArealProductSpecifier		*preset_specs,
		  int				num_preset_specs, 
		  int				max_specs,
		  ArealProductSpecifier 	*specs,
		  int				*num_specs);

void bld_ptpreciplist(int			max_specs,
		      ArealProductSpecifier	*specs,
		      int			*num_specs);

void bld_qpflist(ArealProductTypeDescriptor	prod_descr,
		 int				max_specs,
		 ArealProductSpecifier		*specs,
		 int				*num_specs);

int bld_ffglist(ArealProductTypeDescriptor	prod_descr,
		int				max_specs,
		ArealProductSpecifier		*specs,
		int				*num_specs);

int bld_ffglist_hrs_avail(ArealProductTypeDescriptor	prod_descr,
			  int				max_specs,
			  ArealProductSpecifier		*specs,
			  int				*num_specs);

void bld_comparisonlist(ArealProductTypeDescriptor	prod_descr,
			int				one_hour_only,
			ArealProductSpecifier		*preset_specs,
			int				num_preset_specs,  
			int				max_specs,
			ArealProductSpecifier 		*specs,
			int				*num_specs);


/* support functions in bld_prodlist  ---------------------------------- */

void set_default_prodtime(time_t	*endTime,
			  time_t	*duration);

void set_boundary_string(ResolutionLevel	res,
			 char			*boundary_type);



/* support functions ----------------------------------------------------- */

typedef enum
{
   ON_HOUR, NEAR_HOUR, OFF_HOUR
} onhour_flagtype;

int check_recent_onhr(time_t	data_timet,
		      int	minute_window,
		      int	max_hrs);

void check_onhour(time_t		data_timet,
		  int			minute_window,
		  onhour_flagtype	*hr_flag,
		  time_t		*nearest_hour);

DPARadar * find_best_dpatime(int	minutes_window,
			     DPARadar	*dpaHead,
			     time_t	desired_time);

void get_min_duration(float	*min_duration);
void get_stage1_window(int	*minute_window);
void get_min_area(float	*min_area);



int check_for_ffg(ResolutionLevel	res,
		  time_t		lookbacktime,
		  int			ffg_hours);

int check_for_radargrids(char 		*radid,
			 PrecipType	precipType,
			 time_t		endtime,
			 int		duration);

int check_grid_file(char 	*filename,
		    PrecipType	precipType);

int get_latest_ffgtime(time_t	lookbacktime,
			int	ffg_hours,
			time_t	*validtime);


int find_radar_coords(char	*radid,
		      double 	*south_row,
		      double 	*west_col,
		      int	*numbins_230);

int cvt_preciptype_to_ts(PrecipType	precipType,
                         char		*ts);

void print_floatarray(float     *floatArray,
                      int       numRows,
                      int       numColumns,
                      char      *fileName);

void print_shortarray(short     *shortArray,
                      int       numRows,
                      int       numColumns,
                      char      *fileName);

/* special processing functions ----------------------------------------- */


int process_map_area(int        logall_flag,
                     double     south_row,
                     double     west_col,
                     int        numbins_230,
                     char       *area_id,
                     time_t     gridtime,
                     int        zero_flag,
                     float      bias,
                     float      *grid_vals,
                     float      *avg_val,
                     float      *percent_area_valid);

#endif
