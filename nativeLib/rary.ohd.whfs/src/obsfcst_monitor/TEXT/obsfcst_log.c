#include "obsfcst_monitor.h"

FILE* open_obsfcst_monitor_log()
{
  FILE* logfp;
  char  token_value[100];
  int   ret=0;
  int   log_token_len=22;
  char  logfilename[BUFSIZ];
  time_t cur_time;
  struct tm* tm_struct;

  memset(token_value,'\0',sizeof(token_value));
  get_apps_defaults("obsfcstmonitor_log_dir", &log_token_len, token_value, &ret);
  if(ret == 0)
  {
     fprintf (stderr, "\nERROR: Token value for token obsfcstmonitor_log_dir is not available");
     fflush(NULL);
     return NULL;
  }

  cur_time=time(NULL);
  tm_struct = (struct tm*) gmtime(&cur_time);
  sprintf(logfilename, "%s/obsfcst_monitor_log_%02d%02d", token_value,
                         tm_struct->tm_mon+1, tm_struct->tm_mday);

  logfp =  fopen(logfilename,"a+");
  if (logfp == NULL)
  {
    fprintf(stderr, "\nERROR: Unable to open the log file [%s]", logfilename);
  }

  return logfp;
}

  
int log_cmd_line_args(FILE* logfp, 
                      struct obsfcst_options obsfcst_opts)
{
  char time_str[20];
  time_t cur_time;
  char line[80];

  memset(line,'-',80);
  cur_time=time(NULL);
  memset(time_str, '\0', sizeof(time_str));
  timet_to_yearsec_ansi(cur_time, time_str);

  fprintf(logfp,"\nApplication Starting Time: [%s]",time_str);
  fprintf(logfp, "\n%s %s - %s\n", obsfcst_name, obsfcst_ver, obsfcst_date );

  fprintf(logfp, "\nCmd line args are ...");
  fprintf(logfp, "\nDatabase [%s]", obsfcst_opts.dbname);
  fprintf(logfp, " FcstTablename [%s]", obsfcst_opts.fcsttablename);
  fprintf(logfp, "\nFilter [%s] Lookback hrs for fcst basis time [%d hrs]", 
                     obsfcst_opts.filter, obsfcst_opts.lb_basistime);
  fprintf(logfp, "\nLookback hrs for valid time [%d hrs]", 
                     obsfcst_opts.lb_validtime);
  fprintf(logfp, " Matching validtime window [%d mins]", 
                     obsfcst_opts.tw_match_validtime);
  drawline(logfp);
   
  return SUCCESS;
}

int log_hostname(FILE* logfp)
{
 char hname[50];

 drawline(logfp);
 memset(hname,'\0',sizeof(hname));
 if (gethostname(hname,50) != 0)
 {
   fprintf(logfp,"\nUnable to get the hostname error [%s]",strerror(errno));
   return ERROR;
 }
 else
   fprintf(logfp,"\nHostname [%s]",hname);

 return SUCCESS;
}

void drawline(FILE* logfp)
{
 char line[80];

 memset(line,'\0',80);
 memset(line,'-',79);
 fprintf(logfp,"\n%s",line);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
