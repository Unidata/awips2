#ifndef FFM_SUMMARY_H
#define FFM_SUMMARY_H

#include <time.h>
#include <math.h>


#include "ArealProductSettings.h"


/* defs */

#define NUM_SUMMARY_DURS 5


/* structs ********************************/


/* the percent, diff, and rate are derived for each duration. also an index
   is determined indicating the duration with the most critical value, for
   the percent, diff, and rate */

typedef struct DurStats
{
   double	precip;
   double	precip_max;
   double	precip_min;
   double	ffg;
   
   double 	percent;
   double	diff;
   double	rate;
}  DurStats;


typedef struct FfmSummaryStats
{
   char		area_id[LOC_ID_LEN + 1];
   char		name[LOC_AREANAME_LEN + 1];
   char		boundary_type[LONG_CODE_LEN + 1];
   double	area;
   
   DurStats	dur_stats[NUM_SUMMARY_DURS];
   int		percent_index;
   int		diff_index;
   int		rate_index;
      
   int 		pass_filter;
}  FfmSummaryStats;



/* protos *********************************/

ArealProduct *  malloc_summaryprods(int num_durations);

int get_summary_data(ArealProductTypeDescriptor		prod_descr,      
		     ArealProductSpecifier		prod_spec,
		     int				num_durations,
		     int				*hr_durations,
		     ArealProduct			*products,
		     time_t				precip_timet[],
		     time_t				ffg_timet[]);

FfmSummaryStats * bld_summary_stats(ArealProductTypeDescriptor	prod_descr,
				    ArealProductSpecifier	prod_spec,
				    ArealProduct		*products,
				    int				*num_areas,      
				    int				num_durations,
				    int				*hr_durations);     

void compute_ma_val(int		logall_flag,
		    double	south_row,
		    double	west_col,
		    int		numbins_230,
		    float	*grid_vals,
		    int		zero_flag,
		    char	*area_id,
		    int		numrows,
		    long	rows[],
		    long	beg_cols[],
		    long	end_cols[],
		    float	*avg_val,
		    float	*max_val,
		    float	*min_val,
		    float	*percent_valid,
		    int		*status);

void find_critical_durs(FfmSummaryStats		*ffm_stats,
			int			area_index,
			int			num_durations);

void set_stats_missing(FfmSummaryStats		*ffm_stats,
		       int			area_index,
		       int			num_durations);

#endif
