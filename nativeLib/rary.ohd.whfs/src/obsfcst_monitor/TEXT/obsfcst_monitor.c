#include "obsfcst_monitor.h"

int obsfcst_monitor_main(int argc, const char ** argv)
{
 int         status;
 FILE*       logfp = NULL;
 struct obsfcst_options obsfcst_opts;

 if (argc<6)
 {
   fprintf(stderr, "%s %s %s %s %s","\nUsage:obsfcst_monitor <dbname> <fcst tablename>",
                   "<HSA filter/ALL>",
                   "<lookback hrs for basis time>",
                   "<lookback hrs for valid time>",
                   "<matching valid time window in mins>");

   return ERROR;
 }

 logfp = open_obsfcst_monitor_log();
 if (logfp == NULL)
 {
   return ERROR;
 }

 log_hostname(logfp);

 status = get_obsfcst_monitor_options(logfp, argc, argv, &obsfcst_opts);

 log_cmd_line_args(logfp, obsfcst_opts);

 if( validate_obsfcst_opts(logfp, obsfcst_opts) == ERROR)
 {
     exit_fun(logfp, ERROR);
 }

 if ((status = OpenDbms(obsfcst_opts.dbname)) != 0)
 {
     fprintf(logfp,"\nError %d opening database: %s", status, obsfcst_opts.dbname);
     exit_fun(logfp, ERROR);
 }
 else
 {
     fprintf(logfp,"\nOpening database : [%s]", obsfcst_opts.dbname);
 }

 if (compare_fcst_obs(logfp, obsfcst_opts) == ERROR )
 {
     exit_fun(logfp, ERROR);
 }

 if ((status = CloseDbms()) != 0)
 {
     fprintf(logfp,"\nError %d closing database\n", status);
     exit_fun(logfp, ERROR);
 }
 else
 {
     fprintf(logfp,"\nClosing database");
     exit_fun(logfp, SUCCESS);
 }

 return SUCCESS;
}

int exit_fun(FILE* logfp,
             int val)
{
  time_t cur_time;
  char   time_str[20];

  if (val == ERROR)
   fprintf(logfp, "\nExiting since error occured...");
  else
   fprintf(logfp, "\nExiting...");

  cur_time=time(NULL);
  memset(time_str, '\0', sizeof(time_str));
  timet_to_yearsec_ansi(cur_time, time_str);

  fprintf(logfp,"\nApplication Ending Time: [%s]",time_str);

  drawline(logfp);

  fclose(logfp);

  return 0;
}

