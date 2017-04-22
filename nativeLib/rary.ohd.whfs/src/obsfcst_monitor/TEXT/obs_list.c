#include "obsfcst_monitor.h"

Observation* get_obs_list(FILE* logfp,
                              Forecast* fcst_ptr,
                              struct obsfcst_options obsfcst_opts)
{
   Observation* obs_ptr;
   Forecast*    fcst_ptr_temp = NULL;
   Forecast*    fcst_ptr_last = NULL;
   time_t       obs_time_t;
   time_t       temp_time_tmin, temp_time_tmax;
   char         obs_time_ansi_min[ANSI_YEARSEC_TIME_LEN + 1];
   char         obs_time_ansi_max[ANSI_YEARSEC_TIME_LEN + 1];
   char         where_clause[BUFSIZ];
   char*        fcsttablename = NULL;
   int          time_window;

   time_window = obsfcst_opts.tw_match_validtime *60 ;
   fcsttablename = strdup(obsfcst_opts.fcsttablename);

   fcst_ptr_temp = fcst_ptr;

   memset(obs_time_ansi_min, '\0', ANSI_YEARSEC_TIME_LEN + 1);
   memset(obs_time_ansi_max, '\0', ANSI_YEARSEC_TIME_LEN + 1);
 
   yearsec_dt_to_timet(fcst_ptr_temp->validtime, &obs_time_t);
   obs_time_t -= time_window;
   timet_to_yearsec_ansi(obs_time_t, obs_time_ansi_min);

   fcst_ptr_last = (Forecast*)ListLast(&fcst_ptr_temp->list);
   if( fcst_ptr_last != fcst_ptr_temp)
   {
       yearsec_dt_to_timet(fcst_ptr_last->validtime, &obs_time_t);
       obs_time_t += time_window;
       timet_to_yearsec_ansi(obs_time_t, obs_time_ansi_max);
   }
   else
   {
        yearsec_dt_to_timet(fcst_ptr_temp->validtime, &obs_time_t);
        obs_time_t += time_window;
        timet_to_yearsec_ansi(obs_time_t, obs_time_ansi_max);
   }

#ifdef DEBUG
   fprintf(logfp, "\nTime window for reading the obs table max[%s] min[%s]",
                      obs_time_ansi_max, obs_time_ansi_min);
#endif
  

   memset(where_clause,'\0',sizeof(where_clause));
   sprintf(where_clause, "WHERE lid='%s' AND pe='%s' AND value!=%8.1f AND "
                         " obstime>='%s' and obstime<='%s' ORDER BY obstime",
           fcst_ptr_temp->lid, fcst_ptr_temp->pe, MISSING_VAL,
           obs_time_ansi_min, obs_time_ansi_max);

   fprintf(logfp, "\nWhere clause for obstable\n %s", where_clause);

   obs_ptr= (Observation*) select_observation(fcsttablename, where_clause);

   return obs_ptr;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
