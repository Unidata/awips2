#include "build_hourly.h"

/******************************************************************************
Program Name : build_hourly.c
Description  : Read the CurPP table, look for sub hourly data ( dur < 1001 ),
               do an aggregate of their values and prepare load files for
               gage_pp to update in the HourlyPP table.

Command line
Arguments    : -d database name -l look back window -p min percent filled
               -t type handling [-e end time]

Explanation  : The database name in the cmd line will be the db this program
               will be operating with.The look back window is the number of
               previous hours that this program will have to for, and prepare
               the aggregate value.The min percent filled specified the range
               of minutes within an hour for which the data has to be retrieved.
               If not the program writes MISSING VALUE to load file.
               The type source argument will be SAME / PTYPE.
               SAME - then if the records in the curpp have type source as RZ
               then the aggregate of the records will also have its type source
               as RZ. If PTYPE - then the type source RZ will be changed to
               PZ, RR to PR and so on. The record which have obstime beyond the
               end time will not be considered by the program. Also the hour
               field of the end time is only considered, meaning the mins and
               seconds are ignored. The end time will have the format
               YYYY-MM-DD HH:MM:SS

START_SCRIPT:  run_build_hourly will be a cron job which runs for every hour.

LOG_FILE    :  <gage_pp_log directory>/build_hourly_MMDD

AUTHOR      :  Varalakshmi Rajaram
DATE        :  June 2005

*******************************************************************************/



int build_hourly_main(int argc, const char** argv)
{

  char  	  log_filename[BUFSIZ];
  char            token_value [512];
  char            load_dir [512];
  char            hname[MIN_SIZE_BUFF];

  int             ret;
  int             log_token_len = 12;

  FILE*           logfp       = (FILE*) NULL;
  time_t          cur_time;
  struct tm*      tm_struct;


  build_hourly_options  build_hourly_opt;

  /* Read the token value of gage_pp_log for log directory */

  memset(token_value,'\0',sizeof(token_value));
  get_apps_defaults("gage_pp_log", &log_token_len, token_value, &ret);
  if(ret == 0)
  {
     printf ("\nERROR: Token value for token gage_pp_log is not available");
     fflush(NULL);
     exit_fun(logfp, ERROR);
  }

  cur_time=time(NULL);
  tm_struct = (struct tm*) gmtime(&cur_time);
  sprintf(log_filename, "%s/build_hourly_%02d%02d.log", token_value,
                         tm_struct->tm_mon+1, tm_struct->tm_mday);

  logfp = open_build_hourly_log(log_filename);
  if (logfp == (FILE*)NULL)
  {
    exit_fun(logfp, ERROR);
  }

  fprintf(logfp,"\n----------------------------------------------------------");

  memset(hname,'\0',sizeof(hname));
  if (gethostname(hname,MIN_SIZE_BUFF) != 0)
  {
    fprintf(logfp,"\nUnable to get the hostname error [%s]",strerror(errno));
    exit_fun(logfp,ERROR);
  }
  else
    fprintf(logfp,"\nHostname [%s]",hname);

  fprintf(logfp, "\n%s %s - %s\n", buildhourly_name, buildhourly_ver, buildhourly_date );


  /* Get the command line arguments */
  ret=get_build_hourly_options(logfp, argc, argv, &build_hourly_opt);

  if (ret == CMD_LINE_ARG_ERR)
  {
     fprintf(logfp,"\n Usage: build_hourly  -d <db name> -l <lookback hours>                                                    -p <min_percent_filled> -t <type hanlding>                                                            [-e <end_time YYYY-MM-DD HH:MM:SS> ]");
     exit_fun(logfp, ERROR);
  }
  else if (ret == DB_ERROR)
  {
     log_arguments(logfp, build_hourly_opt);
     exit_fun(logfp, ERROR);
  }
  else
     log_arguments(logfp, build_hourly_opt);

  log_token_len = 13;
  memset(load_dir,'\0',sizeof(load_dir));
  get_apps_defaults("gage_pp_data", &log_token_len, load_dir, &ret);
  if (ret==0)
  {
     fprintf(logfp,"\nERROR:Token value for gage_pp_data is not available");
     exit_fun(logfp, ERROR);
  }

  /* Build the hourly aggregate */
  if (perform_build_hourly(logfp, build_hourly_opt, load_dir) == ERROR)
  {
       exit_fun(logfp, ERROR);
  }

  /* Close the database */
  if ((ret = CloseDbms()) != 0)
  {
     fprintf(logfp,"\nError %d in closing database\n", ret);
     exit_fun(logfp, ERROR);
  }
  else
  {
     fprintf(logfp, "\nClosing database");
     exit_fun(logfp, SUCCESS);
  }

  fclose(logfp);
  return 0;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}/* End of Main funciton */

