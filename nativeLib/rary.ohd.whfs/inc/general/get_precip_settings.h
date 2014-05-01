#ifndef GET_PRECIP_SETTINGS_H
#define GET_PRECIP_SETTINGS_H


/* constants */
#define NOT_PRECIP    0
#define RAWPC         1
#define RAWPP         2
#define RAWPOTHER     3 

#define DEFAULT_PC_WINDOW        10
#define DEFAULT_PPBEFORE_WINDOW  10
#define DEFAULT_PPAFTER_WINDOW   10
#define DEFAULT_PPQ_WINDOW       2.0

#define DEFAULT_SUM_PC_REPORTS_VALUE 0 /* 0 = Don't sum, 1 = sum PC reports. */

#define SHEFHOUR_IN_MINUTES 60
#define SHEFHOUR_IN_HOURS   1001
#define SHEFDAILY_24HR      2001
#define SHEFDAILY_7AM       5004
#define SHEFQUARTLY_6HR     1006


/* define the workfile suffix used by programs for 
   temporary stroage of GPP input file */

#define GPP_WORKFILE_SUFFIX "work"


/* prototypes */

int get_precip_index(char   *pe);


void get_precip_window(int *pc_window,
                       int *pp_beforewindow,
		       int *pp_afterwinow);
		       
/* This routine was created on 1/3/2006 to retrieve the 
 * value of the intppq token. */		       
void get_6hour_precip_window ( float * ppq_window );		       

int check_precip_window(char *pe,
                        time_t validtime);

int check_sum_pc_reports ( );

#endif
