#ifndef GAGEQC_DEFS_H
#define GAGEQC_DEFS_H

/*******************************************************************************
* FILENAME:            gageqc_defs.h
* DESCRIPTION:         Contains constants used by the QPEmapper application.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       January 9, 2006
* ORGANIZATION:        HSEB OHD11
* MACHINE:             Linux
* MODIFICATION HISTORY:
*    DATE         PROGRAMMER        DESCRIPTION/REASON
*    1/9/2006     Bryon Lawrence    Created.
********************************************************************************
*/
#define DEFAULT_ENDING_6HOUR_OBS_TIME 6 /* The default ending obs time for
                                           temperature and freezing level
                                           data. */
#define MAX_GAGEQC_BASINS 2000        /* The maximum number of basins that
                                        may be defined for an office. */
#define MAX_GAGEQC_DAYS 10           /* The maximum number of days DailyQC
                                        may be run for. */
#define MAX_GAGEQC_WFOS 20           /* The maximum number of WFOs in the RFC
                                        area. */
#define NUM_COLORMAP_LEVELS 16       /* The number of colors in the color
                                        map. */					
#define NUM_CONSISTENCY_LEVELS 3     /* The number of levels in the spatial
                                        consistency check. */
#define GAGEQC_AREANAME_LEN 100      /* The max length of the DQC area name. */
#define GAGEQC_FILENAME_LEN 150      /* The length of a gageqc file path and 
					name. */
#define GAGEQC_MESSAGE_LEN 256       /* The length of a gageqc message. */
#define GAGEQC_TOPO_BUF 100
#define MAX_FREEZING_STATIONS 20000    /* The maximum number of freezing
					stations. */
#define LOG_MESSAGE_LEN 150          /* The max length of a log message. */
#define MAX_STATION_RECORD_LEN 200   /* The max length of  a record in 
                                        the station file list record. */
#define QPEMAPPER_SITE_NAME_LEN 20   /* The max length of a site name in
                                        QPEmapper. */ 

#define    GAGE_ID_LEN     8
#define    MESSAGE_LEN     512 
#define    HHMMSS_LEN      8
#define    YYYYMMDDHH_LEN  10
#define    FNAME_LEN       128
#define    TOKEN_VALUE_LEN 512

#define    MAX_TOKEN_SIZE  250
#define    PEDTSEP_LEN     8
#define    MAX_GAGEQC_TYPE 9

#define    DEFAULT_MAX_PRECIP_NEIGHBORS   30
#define    DEFAULT_MAX_TEMP_NEIGHBORS     20
#define    DEFAULT_PRECIP_DEVIATION       3.0 
#define    DEFAULT_TEMPERATURE_DEVIATION  10.0
#define    DEFAULT_MIN_GOOD_STATIONS      5
#define    DEFAULT_DQC_COPY_TO_IHFS       0
#define    DEFAULT_DQC_COPY_TO_ARCHIVE    0
#define    DEFAULT_SAVE_NETCDF            0
#define    DEFAULT_SAVE_GRIB              0

#define    DAILYQC_FAILED 1
#define    DAILYQC_OK     0
#define    NUM_QCTYPE     3

struct _dqc_run_date
{
	int dqc_data_year;
	int dqc_data_month;
	int dqc_data_day;
        int dqc_num_days;
};

struct _dqc_run_date dqc_run_date;
struct _dqc_run_date dqc_run_date_new;

#endif
