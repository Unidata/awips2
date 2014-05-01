/******************************************************************************

Program Name : build_hourly.h --- Header file for build_hourly application

AUTHOR      :  Varalakshmi Rajaram
DATE        :  June 2005

*******************************************************************************/

#ifndef build_hourly_h
#define build_hourly_h 

#include<stdio.h>
#include<unistd.h>
#include<time.h>
#include<sys/time.h>
#include<stdlib.h>
#include<string.h>
#include<errno.h>

#include "DbmsAccess.h"
#include "time_convert.h"
#include "GeneralUtil.h"
#include "RawPP.h"
#include "CurPP.h"
#include "get_total_precip.h"
#include "get_precip_settings.h"

#define CMD_LINE_ARG_ERR       -1
#define CMD_LINE_TIME_FMT_ERR  -1
#define DB_ERROR               -2
#define SUCCESS                 0
#define ERROR                  -1

#define MID_SIZE_BUFF          256
#define MIN_SIZE_BUFF          32

#define SAME                   11
#define PTYPE                  22

/* Variables for version information */

static char buildhourly_name [ ] =  "BuildHourly" ;
static char buildhourly_ver [ ] = "OB8.1" ;
static char buildhourly_date [ ] ="March 26, 2007" ;

typedef struct 
{
 char   dbname[MIN_SIZE_BUFF];
 int    lb_window;
 float  min_percent_fill;
 int    type_source; 
 time_t end_time;
}build_hourly_options;


int get_build_hourly_options(FILE* lfp,
                               int    argc,
                               char** argv,
                               build_hourly_options* build_hourly_opt);

int perform_build_hourly(FILE* logfp, build_hourly_options build_hourly_opt,
                           char load_dir[]);

void exit_fun(FILE* lfp, int erno);

FILE* open_build_hourly_log(char* filename);

int create_build_hourly_work_file(FILE* logfp,
                                  build_hourly_options build_hourly_opt,
                                  struct total_precip precip_agg,
                                  char* end_time_asc, char load_dir[]);

void log_arguments(FILE* logfp, build_hourly_options build_hourly_opt);

int create_build_hourly_load_file(FILE* logfp, char load_dir[]);

#endif

