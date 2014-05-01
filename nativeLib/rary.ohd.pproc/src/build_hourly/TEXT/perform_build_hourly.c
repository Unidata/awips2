/******************************************************************************

Program Name : perform_build_hourly.c ---- Sub program for build_hourly.c

Num of funcs : 1

Function Name: int perform_build_hourly(FILE* logfp, 
                                     build_hourly_options build_hourly_opt);

Arguments    : i/p -----FILE* logfp -- log file to log the infm
                        build_hourly_options build_hourly_opt -- structure
                           containing command line arguments passed to
                           hourly builder program except the database name
               o/p -----int -- SUCCESS --- for success full completion
                               ERROR   --- when 
                                           create_send_build_hourly_load_file 
                                           returns ERROR 

Explanation : If the end time is 2005-06-25 08:15:50, look back hours - 2
              min percent filled - 0.75 -t PTYPE
              The following steps are performed.
              1.Get the records from curpp where obstime between
              2005-06-25 07:00:00 and 2005-06-25 08:00:00
              and dur < 1001
              2.Aggregate their values 
              3.Call function create_send_build_hourly_load_file
              4.Perform the above 3 steps for obstime between
                2005-06-25 06:00:00 and 2005-06-25 07:00:00

AUTHOR      :  Varalakshmi Rajaram
DATE        :  June 2005

*******************************************************************************/

#include "build_hourly.h"

int perform_build_hourly(FILE* logfp, 
                           build_hourly_options build_hourly_opt,
                           char  load_dir[])
{
  time_t slot_start_timet;
  time_t slot_finish_timet;
  time_t mod_end_timet;

  struct tm *tm_struct;

  int    window_cnt,
         num_pc_records, /* We will not use this except in fn call
                            get_total_raw_precip. The fn expects
                            valid address, hence we declare and pass*/
         num_pp_records,
         status = SUCCESS,
         work_file_created = ERROR;

  char   where_clause[BUFSIZ];

  CurPP*  curpp_ptr = (CurPP*) NULL;
  CurPP* headpp_ptr = (CurPP*) NULL;

  struct total_precip precip_agg;

  mod_end_timet = build_hourly_opt.end_time;

  tm_struct = (struct tm*) gmtime(&mod_end_timet);

  /* If the current time has some secs or mins in it,get rid of them
     and just consider only the hours */

  if (tm_struct->tm_sec != 0)
      mod_end_timet = mod_end_timet - tm_struct->tm_sec;

  if (tm_struct->tm_min != 0)
      mod_end_timet = mod_end_timet - (tm_struct->tm_min * 60);

  fprintf(logfp, "\nCommand line argument END_TIME rounded to the hour %s", 
                     asctime(gmtime(&mod_end_timet)));

  fprintf(logfp, " *** below means < %f percent of hourly period has data",floor(build_hourly_opt.min_percent_fill*100.0));
  fflush(logfp);
  
  for (window_cnt = 0; window_cnt < build_hourly_opt.lb_window; window_cnt++)
  {
    char begin_time_asc [ ANSI_YEARSEC_TIME_LEN + 1 ] ;
    char end_time_asc   [ ANSI_YEARSEC_TIME_LEN + 1 ] ;

    slot_start_timet  = mod_end_timet -(window_cnt +1)*3600;
    slot_finish_timet = mod_end_timet -(window_cnt)*3600;
 
    fprintf(logfp,"\n*******************************************************");
    fprintf(logfp,"\nAccumulation_Interval_Start_Time=%s",
                   asctime(gmtime(&slot_start_timet)));
    fprintf(logfp,"Accumulation_Interval_Finish_Time=%s",
                   asctime(gmtime(&slot_finish_timet)));

    memset(begin_time_asc, '\0', sizeof(begin_time_asc));
    memset(end_time_asc, '\0', sizeof(end_time_asc));

    timet_to_yearsec_ansi ( slot_start_timet , begin_time_asc );
    timet_to_yearsec_ansi ( slot_finish_timet , end_time_asc );

    memset(where_clause, '\0', sizeof(where_clause));
    sprintf(where_clause," WHERE value != -9999.0 AND obstime >= '%s' AND obstime <= '%s' AND dur < 1001 ORDER BY lid ASC, ts ASC, dur DESC, obstime DESC",
            begin_time_asc, end_time_asc);

    fprintf(logfp, "\nQuerying CurPP with where clause");
    fprintf(logfp, "\n[%s]", where_clause);

    headpp_ptr = (CurPP*)GetCurPP(where_clause);
    curpp_ptr = headpp_ptr;
    
    if(headpp_ptr != NULL)
    {
       fprintf(logfp, "\nNumber Of Records Retrieved = %d\n",
                     ListCount(&headpp_ptr->list));

       while( curpp_ptr != NULL )
       {
          memset(&precip_agg, 0, sizeof(struct total_precip));

          precip_agg =  get_total_raw_precip( NULL, (RawPP**) &curpp_ptr, 
                                           slot_start_timet,
                                           slot_finish_timet, 
                                           REPORT_MISSING_BELOW_MIN_PERCENT,
                                           build_hourly_opt.min_percent_fill,
                                           0,
                                           1, &num_pc_records, &num_pp_records);
    
          if (build_hourly_opt.type_source == PTYPE)
          {
             precip_agg.TS[0] = 'P';
          }
      
          if (precip_agg.percent_filled >= build_hourly_opt.min_percent_fill)
          {
              /*fprintf(logfp, "\n%s %s %s val:%.3lf minutes:%.3lf num_recs:%d",*/
              fprintf(logfp, "\n%s %s %s val:%.3lf minutes:%.3lf num_recs:%d",
                  precip_agg.lid , precip_agg.PE , precip_agg.TS ,
                  precip_agg.value, precip_agg.percent_filled*60.0, num_pp_records);
          }
          else
          {
              fprintf(logfp, "\n%s %s %s val:%.3lf minutes:%.3lf num_recs:%d ***",
                  precip_agg.lid , precip_agg.PE , precip_agg.TS ,
                  precip_agg.value, precip_agg.percent_filled*60.0, num_pp_records);
          }


          if (create_build_hourly_work_file(logfp, build_hourly_opt, 
                               precip_agg, end_time_asc,load_dir) == ERROR)
          {
              status = ERROR;
              work_file_created = ERROR;
          }
          else
          {
              work_file_created = SUCCESS;
          }
        
       } /* End of while */

       FreeCurPP(headpp_ptr);
    }
    else
    {
      fprintf(logfp, "\nNo Records were found...");
    }


  } /* End of for loop for time window slots depending on loop back hour */
  
  if(work_file_created == SUCCESS) /* build hourly work file is created */
  {
     if (create_build_hourly_load_file(logfp, load_dir) == ERROR)
              status = ERROR;
  }

  return status;

}/* End of fn perform_build_hourly */
