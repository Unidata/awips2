/* Structure parameters for the time series */
/* 12/5/96                                   */
/*                                           */

#ifndef ts_info_pm_h 
#define ts_info_pm_h 

typedef struct {
        int     ts_type, delta_t;
        char    ts_id[9], data_type[5];
        }       TS_INFO;
#endif
