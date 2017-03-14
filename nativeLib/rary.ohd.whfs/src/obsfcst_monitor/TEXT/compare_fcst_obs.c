#include "obsfcst_monitor.h"


int compare_fcst_obs(FILE* logfp,
                     struct obsfcst_options obsfcst_opts)
{
   UniqueList*uni_lst_ptr        = NULL , 
              *uni_lst_ptr_tmp    = NULL ;

   char       where_clause  [BUFSIZ];
   char       errmsg[250];
   int        status = 0;
   time_t     cur_time;
   char       cur_time_str[20];

   Forecast* fcst_ptr = NULL;
  
   fprintf(logfp, "\n\nMonitoring %s", obsfcst_opts.fcsttablename);

   uni_lst_ptr = get_unique_fcst_list(logfp, obsfcst_opts);

   if ( uni_lst_ptr == NULL )
   {
      fprintf(logfp, "\nNo matching rows were found while getting the unique list");
      return  SUCCESS;
   }

   uni_lst_ptr_tmp = (UniqueList *) ListFirst(&uni_lst_ptr->list);

   /* Traverse the unique list of lid,pe,ts,basistime combination.*/
   while (uni_lst_ptr_tmp)
   {
      struct parsed_uniq_rec uniq_rec1, uniq_rec2;
      
      while(1)
      {
        uniq_rec1 = get_parsed_uniq_rec(logfp, uni_lst_ptr_tmp);
        if (ListNext(&uni_lst_ptr_tmp->node) == NULL)
           break;
        uniq_rec2 = get_parsed_uniq_rec(logfp, (UniqueList*)ListNext(&uni_lst_ptr_tmp->node));

        if( (strcmp(uniq_rec1.lid,uniq_rec2.lid) == 0) &&
            (strcmp(uniq_rec1.pe,uniq_rec2.pe) == 0) &&
            (strcmp(uniq_rec1.ts,uniq_rec2.ts) == 0) &&
            (strcmp(uniq_rec1.bt,uniq_rec2.bt) < 0) )
                uni_lst_ptr_tmp = (UniqueList*)ListNext(&uni_lst_ptr_tmp->node);
        else
            break;
      }

      fprintf(logfp,"\n\nProcessing lid,pe,ts,bt :%s %s %s %s",
                  uniq_rec1.lid,
                  uniq_rec1.pe,
                  uniq_rec1.ts,
                  uniq_rec1.bt);

      memset(where_clause,'\0',sizeof(where_clause));
      sprintf(where_clause, " WHERE lid='%s' AND pe='%s' AND ts='%s' AND"
	                    " basistime='%s' AND value!=%8.1f ORDER BY"
			    " validtime ",
              uniq_rec1.lid, uniq_rec1.pe, uniq_rec1.ts, 
              uniq_rec1.bt, MISSING_VAL);

      fprintf(logfp, "\nWhere clause for fcst table\n%s", where_clause);

      fcst_ptr = (Forecast*) select_forecast(obsfcst_opts.fcsttablename, where_clause);

      if (fcst_ptr == NULL)
      {
        fprintf(logfp,"\nNo fcst recs for %s %s %s %s",
                          uniq_rec1.lid, uniq_rec1.pe, uniq_rec1.ts, 
                          uniq_rec1.bt);
        uni_lst_ptr_tmp = (UniqueList *) ListNext(&uni_lst_ptr_tmp->node);
        continue;
      }
      else
      {
        Observation* obs_ptr = NULL;
        Forecast* fcst_ptr_temp;
#ifdef DEBUG
        print_fcst_recs(logfp, fcst_ptr);
#endif
        fflush(logfp);
        fcst_ptr_temp = fcst_ptr;

        if(fcst_ptr_temp)
        {
           obs_ptr = get_obs_list(logfp, fcst_ptr_temp, obsfcst_opts);

           if (obs_ptr == NULL)
           {
             fprintf(logfp,"\nNo Obsrecs within this window ");
             fflush(logfp);
             uni_lst_ptr_tmp = (UniqueList *) ListNext(&uni_lst_ptr_tmp->node);
             FreeFcstHeight(fcst_ptr);
             continue;
           }
           fflush(logfp);

#ifdef DEBUG
           print_obs_recs(logfp, obs_ptr);
#endif
        }

        while(fcst_ptr_temp)
        {
              Observation* obs_ptr_final_match = NULL;
              int          check_diff_ret = 0;

              obs_ptr_final_match = find_obs_match(logfp, fcst_ptr_temp, obs_ptr,
                                                   obsfcst_opts);

              if (obs_ptr_final_match == NULL)
              {
                  fcst_ptr_temp = (Forecast *) ListNext(&fcst_ptr_temp->node);
                  continue;
              }

              check_diff_ret = check_diff(logfp,fcst_ptr_temp,obs_ptr_final_match,
                                                            obsfcst_opts);

              if ((check_diff_ret == 1) || (check_diff_ret == 2))
              {
                 if (write_to_alertalarm(logfp, fcst_ptr_temp,obs_ptr_final_match,
                                         check_diff_ret) == ERROR)
                   {
                       return ERROR;
                   }
              }

              fcst_ptr_temp = (Forecast *) ListNext(&fcst_ptr_temp->node);
       }

       FreeMemory(obs_ptr, obsfcst_opts.fcsttablename);
      }

      uni_lst_ptr_tmp = (UniqueList *) ListNext(&uni_lst_ptr_tmp->node);
      FreeFcstHeight(fcst_ptr);
   }

   FreeUnique(uni_lst_ptr);
  
 
   fprintf(logfp,"\n\nPurging the AlertAlarmVal recs...");
   memset(errmsg, '\0', sizeof(errmsg));

   cur_time=time(NULL);
   memset(cur_time_str, '\0', sizeof(cur_time_str));
   timet_to_yearsec_ansi(cur_time, cur_time_str);
   fprintf(logfp,"\nBefore purging alertalarmval [%s]", cur_time_str);

   status = purge_fcst_alerts("diff", errmsg);

   cur_time=time(NULL);
   memset(cur_time_str, '\0', sizeof(cur_time_str));
   timet_to_yearsec_ansi(cur_time, cur_time_str);
   fprintf(logfp,"\nAfter purging alertalarmval [%s]", cur_time_str);

   if (status < 0)
   {
      fprintf(logfp,"\n%s", errmsg);
   }
   else
   {
       sprintf(errmsg, "\n%d basistime fcst recs checked for AlertAlarmVal purge",
                 status);
       fprintf(logfp,"%s", errmsg);
   }

  return SUCCESS;
}

Forecast* select_forecast(char* fcsttablename,
                          char* where_clause)
{
  Forecast* fcst_ptr = (Forecast*) NULL;
  if ( strcasecmp(fcsttablename, "FcstHeight") == 0 )
      fcst_ptr = (Forecast*) SelectFcstHeight(where_clause);
  else if ( strcasecmp(fcsttablename, "FcstDischarge") == 0 )
      fcst_ptr = (Forecast*) SelectFcstDischarge(where_clause);
 
  return fcst_ptr;
}

Observation* select_observation(char* fcsttablename,
                                char* where_clause)
{
  Observation* obs_ptr = (Observation*) NULL;
  if ( strcasecmp(fcsttablename, "FcstHeight") == 0 )
      obs_ptr = (Observation*) SelectHeight(where_clause);
  else if ( strcasecmp(fcsttablename, "FcstDischarge") == 0 )
      obs_ptr = (Observation*) SelectDischarge(where_clause);
 
  return obs_ptr;
}

void FreeMemory(void* obs_ptr, 
          char* fcsttablename)
{
  if( strcasecmp(fcsttablename,"FcstHeight") == 0)
      FreeHeight((Height*) obs_ptr);
  if( strcasecmp(fcsttablename,"FcstDischarge") == 0)
      FreeDischarge((Discharge*) obs_ptr);

  return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
