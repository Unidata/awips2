/******************************************************************************

Program Name : build_hourly_util_functions.c--Sub program for build_hourly.c

Num of funcs : 6

AUTHOR       :  Varalakshmi Rajaram
DATE         :  June 2005
*******************************************************************************/


/******************************************************************************
Function Num : 1
Function Name: void exit_fun(FILE* lfp, int erno);

Arguments    : i/p -----FILE* logfp -- log file to log the infm 
                        int   erno  -- Integer value

Explanation  : If erno == -1 then 
                 print "Exiting since error occured..." in logfile
               else
                 print "Exiting ..." in logfile


*******************************************************************************/

#include "build_hourly.h"

void exit_fun(FILE* logfp, int erno)
{
  time_t cur_time;
  char   time_str[MIN_SIZE_BUFF];

  if (erno == ERROR)
  {
   fprintf(logfp, "\nExiting since error occured...");
   fclose(logfp);
   exit(0);
  }
  else
   fprintf(logfp, "\nExiting...");

  cur_time=time(NULL);
  memset(time_str, '\0', sizeof(time_str));
  timet_to_yearsec_ansi(cur_time, time_str);

  fprintf(logfp,"\nApplication Ending Time: [%s]",time_str);

  fprintf(logfp,"\n--------------------------------------------------------");

}

/******************************************************************************
 Function Num  : 2
 Function Name : get_build_hourly_options

 Arguments     : i/p : FILE*  logfp-- log file to log the infm
                       int    argc -- cnt of cmd line arg passed to main
                       char** argv -- cmd line args passed to main
                 o/p : build_hourly_options* build_hourly_opt -- 
                                      structure to hold the cmd line args

 Return        : int status : SUCCESS -- Everything is fine
                              DB_ERROR-- Unable to open db
                              CMD_LINE_TIME_FMT_ERR--error in format of end time
                                    passed in cmd line
                              CMD_LINE_ARG_ERR-- Other cmd line arg errors

Explanation   : If argc < 9 then error is returned
                else the cmd line args are read and the values are
                written to the structure build_hourly_opt.
                The database is also opened, if an error occurs then error is 
                returned.

******************************************************************************/

int get_build_hourly_options(FILE* logfp, 
                               int    argc, 
                               char** argv, 
                               build_hourly_options* build_hourly_opt)
{

  extern char *optarg;
  extern int  optind;
  extern int  optopt;

  int    ret;
  int    dbstatus = 0;
  int    status   = SUCCESS;

  if (argc < 9)
  {
    status = CMD_LINE_ARG_ERR;
    return status;
  }

  
  memset(build_hourly_opt, 0, sizeof(build_hourly_options));

  /* Get the command line arguments */
  while ((ret = getopt(argc, argv, "d:l:p:t:e:")) != -1)
  {

    switch(ret)
    {
      case 'd':
            /* Get the dbname and open the db */
            if ((dbstatus = OpenDbms(optarg)) != 0)
            {
               fprintf(logfp,"\nError %d opening database: %s\n", dbstatus, optarg);
               status = DB_ERROR;
            }
            else
            {
               fprintf(logfp, "\nOpening database: [%s]", optarg);
            }
            strcpy(build_hourly_opt->dbname, optarg);
            break;

      case 'l':
            /* Get the look back hours */
            build_hourly_opt->lb_window = atoi(optarg);
            break;

      case 'p':
            /* Get the minimum percent filled */
            build_hourly_opt->min_percent_fill = atof(optarg);
            break;

      case 't':
            /* Get the type source     */
            if(strcasecmp(optarg, "PTYPE") == 0)
                build_hourly_opt->type_source = 22;
            else 
                build_hourly_opt->type_source = 11; /* SAME is the defult one */
            break;

      case 'e':
            /* Get the end time */
            if (yearsec_ansi_to_timet(optarg, &build_hourly_opt->end_time) == -1)
                status = CMD_LINE_TIME_FMT_ERR;
            break;

      default:
            status = CMD_LINE_ARG_ERR; /* Invalid options */

    }  /* End of switch */

  } /* End of while getopt */

  
  /* If end_time is not given in cmd line or if the given end_time format is
     not YYYY-MM-DD HH:MM:SS then current time will be taken as end time */

  if (build_hourly_opt->end_time == 0 || status == CMD_LINE_TIME_FMT_ERR)
  {
     build_hourly_opt->end_time = time(NULL);
     status = SUCCESS;
  }
  
  return status;

}/* End of get_build_hourly_options function */


/******************************************************************************
 Function Num  : 3
 Function Name : create_build_hourly_work_file

 Arguments     : i/p : FILE*  logfp-- log file to log the infm
                       build_hourly_options build_hourly_opt--structure 
                           containing the cmd line args except db name
                       struct total_precip precip_agg--structure returned by
                           call to get_total_raw_precip, contains the aggregate
                           precip information
                       char* end_time_asc--ending time of the time window slot
                           for that hour.
                       char load_dir[]--path mentioned in apps default to 
                           create load file for gage_pp

 Return        : int status : SUCCESS -- Everything is fine
                              ERROR-- Unable to create the load file

 Explanation   : The load directory is where the load file will be created.
                 Create a file BUILD_HOURLY.work in that area.
                 Write the record contents from precip_agg into the file, 
                 which would be updated to hourlypp table by gage_pp process.
                 If cur percent filled < min percent filled then -9999.0 
                 (VALUE MISSING) will be written in the file.
                 Rename this file into load file BUILDHORLY.MMDD.YYYYHHMMSSss

******************************************************************************/


int create_build_hourly_work_file(FILE* logfp, 
                                  build_hourly_options build_hourly_opt,
                                  struct total_precip precip_agg,
                                  char* end_time_asc,char load_dir[])
{
  FILE*      work_fp = (FILE*)NULL;

  char       work_filename [BUFSIZ];

  int        status       = SUCCESS;

  memset(work_filename,'\0',sizeof(work_filename));
  sprintf(work_filename,"%s/BUILD_HOURLY.%s",load_dir,GPP_WORKFILE_SUFFIX);

  work_fp = (FILE*)fopen(work_filename, "a+");
  
  if (work_fp == (FILE*)NULL)
  {
    fprintf(logfp, "\nError while opening work file [%s] error [%s]",
                      work_filename, strerror(errno));
    status = ERROR;
  }
  else
  {
    float value_to_load;
    char product_time[25];
    time_t     cur_time;

    cur_time= time(NULL);
    timet_to_yearsec_ansi(cur_time, product_time);


    /* If percent filled >= threshold set then value to send is the precip 
       aggregate, else send missing value */

    if (precip_agg.percent_filled >= build_hourly_opt.min_percent_fill)
    {
      value_to_load = precip_agg.value;
    /*else
      value_to_load = -9999.0; */

    fprintf(work_fp,"%s|%s|%d|%s|%s|%s|%.3lf|%c|%d|%d|%s|%s|%s\n",
                     precip_agg.lid, precip_agg.PE, 1001,
                     precip_agg.TS, "Z",
                     end_time_asc, value_to_load,
                     precip_agg.qc, 1879048191, 1,
                     "BUILDHORLY", product_time, product_time);
    }
    fclose(work_fp);
  }
  
  return status;
}

/******************************************************************************
 Function Num  : 4
 Function Name : open_build_hourly_log(char* filename)

 Arguments     : i/p : char* filename -- log file name with path 

 Return        : FILE* -- File pointer for the opened log file

 Explanation   : Open the log file using "a+" mode, and return the file pointer.
                 If unable to open then return NULL.

******************************************************************************/


FILE* open_build_hourly_log(char* filename)
{
  FILE* fp = (FILE*) NULL;
  
  fp = fopen(filename, "a+");
  if (fp == (FILE*) NULL)
  {
    printf("\nUnable to open the log file [%s] error [%s]",
              filename, strerror(errno)); 
  }

  return fp;
}

/******************************************************************************
 Function Num  : 5
 Function Name : log_arguments(FILE* logfp, 
                               build_hourly_options build_hourly_opt)

 Arguments     : i/p : FILE* logfp--log file to log the cmd line args
                       build_hourly_options build_hourly_opt--structure
                           containing the cmd line args except db name

 Return        : void

 Explanation   : Read the build_hourly_opt structure for command line args and
                 write to log file

******************************************************************************/

void log_arguments(FILE* logfp, build_hourly_options build_hourly_opt)
{
  char str[BUFSIZ];
  char time_str[MIN_SIZE_BUFF];
  time_t cur_time;

  cur_time=time(NULL);
  memset(time_str, '\0', sizeof(time_str));
  timet_to_yearsec_ansi(cur_time, time_str);

  fprintf(logfp,"\nApplication Starting Time: [%s]",time_str);

  fprintf(logfp,"\nBuild Hourly Application is invoked with following arg:");

  memset(time_str, '\0', sizeof(time_str));
  timet_to_yearsec_ansi(build_hourly_opt.end_time, time_str);

  memset(str, '\0', sizeof(str));
  sprintf(str,"\nDataBase Name: [%s] Look Back Hours: [%d]", 
               build_hourly_opt.dbname, build_hourly_opt.lb_window);
  fprintf(logfp,"%s",str);

  memset(str, '\0', sizeof(str));
  sprintf(str,"\nMinimum Percent To Be Filled: [%f] End Time: [%s]",
               build_hourly_opt.min_percent_fill, time_str);
  fprintf(logfp,"%s",str);

  memset(str, '\0', sizeof(str));
  sprintf(str,"\nType Handling: [%s]",((build_hourly_opt.type_source==22)?
                          "Change R to P in Type ":
                          "Retain existing Type "));
  fprintf(logfp,"%s",str);
}


/******************************************************************************
 Function Num  : 6
 Function Name : create_build_hourly_load_file

 Arguments     : i/p : FILE*  logfp-- log file to log the infm
                       char  load_dir[]-- dir where the load file for gage_pp
                              will be created

 Return        : int status : SUCCESS -- Everything is fine
                              ERROR-- Unable to create the load file

 Explanation  :Rename BUILD_HOURLY.work to loadfile BUILDHORLY.MMDD.YYYYHHMMSSss

******************************************************************************/
int create_build_hourly_load_file(FILE* logfp, char load_dir[])
{
  char       load_filename [BUFSIZ],
             work_filename [BUFSIZ],
             temp_time     [MIN_SIZE_BUFF];
  int        ret          =0,
             status       = SUCCESS;

  time_t         cur_time;
  struct timeval time_val;
  struct tm*     time_ptr;

  cur_time= time(NULL);
  time_ptr = gmtime(&cur_time);
  memset(temp_time, 0, sizeof(temp_time));
  sprintf(temp_time, "%2.2d%2.2d.%2.2d%2.2d%2.2d",
            time_ptr->tm_mon + 1, time_ptr->tm_mday,
            time_ptr->tm_hour, time_ptr->tm_min, time_ptr->tm_sec);

  memset(&time_val, 0, sizeof(struct timeval));
  gettimeofday(&time_val,NULL);

  memset(work_filename,'\0',sizeof(work_filename));
  sprintf(work_filename,"%s/BUILD_HOURLY.%s",load_dir,GPP_WORKFILE_SUFFIX);

  memset(load_filename,'\0',sizeof(load_filename));
  sprintf(load_filename, "%s/BUILDHORLY.%s%ld",
                           load_dir,temp_time,time_val.tv_usec);

  ret = rename(work_filename, load_filename);

  if (ret == -1)
  {
     fprintf(logfp,"\nError in renaming workfile:%s to loadfile:%s Error:%s",
                        work_filename, load_filename, strerror(errno));
     status = ERROR;
  }
  else
     status = SUCCESS;
  
  return status;
}
