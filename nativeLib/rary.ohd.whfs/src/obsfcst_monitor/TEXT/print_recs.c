#include "obsfcst_monitor.h"

int print_fcst_with_matchobs(FILE* logfp, 
                             Forecast* fcst_ptr, 
                             Observation* obs_ptr)
{
    char obstime[20];
    char prodtime[20];
    char posttime[20];
    char time_str[20];

    memset(time_str, '\0', 20);
    yearsec_dt_to_ansi(fcst_ptr->validtime, time_str);
    fprintf(logfp, "\n\nFcst-%s %lf", time_str, fcst_ptr->value);

    memset(obstime,'\0',20);
    memset(prodtime,'\0',20);
    memset(posttime,'\0',20);
    yearsec_dt_to_ansi(obs_ptr->obstime, obstime);
    yearsec_dt_to_ansi(obs_ptr->producttime, prodtime);
    yearsec_dt_to_ansi(obs_ptr->postingtime, posttime);
   
    fprintf(logfp, " Obs-%s %s %f",
                   obs_ptr->ts, 
                   obstime, obs_ptr->value);
 
#ifdef DEBUG
    fprintf(logfp, "\nThe matching obs record is ");
    fprintf(logfp, "\n%s %s %d %s %s %s %f %s %ld %d %s %s %s", 
                   obs_ptr->lid, obs_ptr->pe, 
                   obs_ptr->dur, 
                   obs_ptr->ts, 
                   obs_ptr->extremum, 
                   obstime, obs_ptr->value,
                   obs_ptr->shef_qual_code, 
                   obs_ptr->quality_code,
                   obs_ptr->revision, 
                   obs_ptr->product_id, 
                   prodtime, posttime);
 
    fprintf(logfp,"\nThe fcst value [%f] obsvalue [%f] obs-fcst [%f]", 
                   fcst_ptr->value, obs_ptr->value,
                   obs_ptr->value - fcst_ptr->value);
#endif
                   
    fflush(logfp);

    return SUCCESS;
}

int print_fcst_recs(FILE* logfp,
                    Forecast* fcst_ptr)
{
  Forecast* fcst_ptr_temp = fcst_ptr;
  char      time_str[20];

  fprintf(logfp, "\nThe forecast recs are ");
  while(fcst_ptr_temp != NULL)
  {
       memset(time_str, '\0', 20);
       yearsec_dt_to_ansi(fcst_ptr_temp->basistime, time_str);

       fprintf(logfp, "\n%s %s %s %lf basis %s",
                              fcst_ptr_temp->lid, fcst_ptr_temp->pe,
                              fcst_ptr_temp->ts, fcst_ptr_temp->value,
                              time_str);
       memset(time_str, '\0', 20);
       yearsec_dt_to_ansi(fcst_ptr_temp->validtime, time_str);
       fprintf(logfp," valid %s", time_str);

       fcst_ptr_temp = (Forecast *) ListNext(&fcst_ptr_temp->node);
  }
  
  return SUCCESS;
}

int print_obs_recs(FILE* logfp,
                   Observation* obs_ptr)
{
    Observation* temp_ptr;
    char obstime[20];
    char prodtime[20];
    char posttime[20];

    temp_ptr=obs_ptr;
    fprintf(logfp, "\nObs recs within this window are ");

    while(temp_ptr != NULL)
    {
          memset(obstime, '\0', 20);
          memset(prodtime, '\0', 20);
          memset(posttime, '\0', 20);
          yearsec_dt_to_ansi(temp_ptr->obstime, obstime);
          yearsec_dt_to_ansi(temp_ptr->producttime, prodtime);
          yearsec_dt_to_ansi(temp_ptr->postingtime, posttime);

          fprintf(logfp, "\n%s %s %d %s %s %s %f %s %ld %d %s %s %s",
                   temp_ptr->lid, temp_ptr->pe,
                   temp_ptr->dur,
                   temp_ptr->ts,
                   temp_ptr->extremum,
                   obstime, temp_ptr->value,
                   temp_ptr->shef_qual_code,
                   temp_ptr->quality_code,
                   temp_ptr->revision,
                   temp_ptr->product_id,
                   prodtime, posttime);
           temp_ptr=(Observation*)temp_ptr->node.next;
    }
   
   return SUCCESS;
}

