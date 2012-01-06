#ifndef obsfcst_monitor_h
#define obsfcst_monitor_h

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<unistd.h>
#include<errno.h>
#include "DbmsDefs.h"
#include "FcstHeight.h"
#include "FcstDischarge.h"
#include "Observation.h"
#include "Forecast.h"
#include "Height.h"
#include "Discharge.h"
#include "AlertAlarmVal.h"
#include "alert_util.h"
#include "time_convert.h"
#include "LoadUnique.h"

#define SUCCESS 0
#define ERROR   -1
#define MISSING_VAL -9999.

extern int errno;

static char obsfcst_name [ ] =  "ObsFcst Monitor" ;
static char obsfcst_ver [ ] = "OB8.1" ;
static char obsfcst_date [ ] ="March 21, 2007" ;

/* The routines used to retrieve the version information. */
//static char * get_obs_fcstname ( ) { return obs_fcstname ; } ;
//static char * get_obs_fcstver ( ) { return obs_fcstver ; } ;
//static char * get_obs_fcstdate ( ) { return obs_fcstdate ; } ;

struct parsed_uniq_rec
{
  char       lid           [LOC_ID_LEN + 1];
  char       pe            [SHEF_PE_LEN + 1];
  char       ts            [SHEF_TS_LEN + 1];
  char       bt            [20];
};

struct obsfcst_options
{
  char filter[4];
  char dbname[20];
  char fcsttablename[50];
  int  lb_basistime;
  int  lb_validtime;
  int  tw_match_validtime;
};

int compare_fcst_obs(FILE* logfp,
                     struct obsfcst_options obsfcst_opts);

UniqueList*  get_unique_fcst_list(FILE* logfp,
                                  struct obsfcst_options obsfcst_opts);

void* get_max_fcst_basis_time(FILE* logfp, FcstHeight* fcst_ptr,
                            char* max_basis_time_ansi);

FILE* open_obsfcst_monitor_log();

Height* sort_list(Height* list);

Forecast** get_valid_fcst_ptrs_touse(FILE* logfp,
                             Forecast* fcst_ptr,
                             char* max_basis_time_ansi,
                             int* cnt_of_valid_fcstptrs_touse);

int write_to_alertalarm(FILE* logfp,
                        Forecast* fcst_ptr,
                        Observation* obs_ptr,
                        int check_diff_ret);

int print_fcst_with_matchobs(FILE* logpf,
                             Forecast* fcst_ptr,
                             Observation* obs_ptr);

int validate_fcsttablename(char* tablename);

Forecast* select_forecast(char* fcsttablename,
                          char* where_clause);

Observation* select_observation(char* fcsttablename,
                                char* where_clause);

int log_cmd_line_args(FILE* logfp,
                      struct obsfcst_options obsfcst_opt);
                      

struct parsed_uniq_rec get_parsed_uniq_rec(FILE* logfp,
                                    UniqueList* uniq_lst_ptr);

int print_obs_recs(FILE* logfp, 
                   Observation* obs_ptr);

int print_fcst_recs(FILE* logfp, 
                    Forecast* fcst_ptr);

Observation* get_obs_list(FILE* logfp,
                              Forecast* fcst_ptr,
                              struct obsfcst_options obsfcst_opts);

Observation* find_obs_match(FILE* logfp,
                            Forecast* fcst_ptr,
                            Observation* obs_ptr,
                            struct obsfcst_options obsfcst_opts);

int validate_obsfcst_opts(FILE* logfp,
                          struct obsfcst_options obsfcst_obs);

int get_obsfcst_monitor_options(FILE* logfp,
                               int    argc,
                               char** argv,
                               struct obsfcst_options* obsfcst_opts);

int log_hostname(FILE* logfp);

int exit_fun(FILE* logfp, 
             int val);

void drawline(FILE* logfp);

void FreeMemory(void* obs_ptr,
          char* fcsttablename);

int check_diff(FILE* logfp,
               Forecast* fcst,
               Observation* obs,
               struct obsfcst_options obsfcst_opts);

#endif

