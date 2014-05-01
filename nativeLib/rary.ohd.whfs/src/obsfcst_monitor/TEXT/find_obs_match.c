#include "obsfcst_monitor.h"

Observation* find_obs_match(FILE* logfp,
                            Forecast* fcst_ptr, 
                            Observation* obs_ptr,
                            struct obsfcst_options obsfcst_opts)
{
    Observation* obs_ptr_match1 = NULL;
    Observation* obs_ptr_match2 = NULL;
    Observation* obs_ptr_final_match = NULL;
    Observation* temp_ptr;
    Observation* temp_ptr1;
    Forecast*    fcst_ptr_temp = fcst_ptr;
    int     time_diff1 = 0;
    int     time_diff2 = 0;
    time_t  valid_timet;
    time_t  temp_obstimet;
    int     found = 0;

    memset(&valid_timet, 0, sizeof(time_t));
    yearsec_dt_to_timet(fcst_ptr_temp->validtime,&valid_timet);

    temp_ptr=obs_ptr;
    temp_ptr1=obs_ptr;

    /* Find the first available node that matches in the sort list 
    that has the obstime >= validtime */
    while(temp_ptr != NULL)
    {
          memset(&temp_obstimet, 0, sizeof(time_t));
          yearsec_dt_to_timet(temp_ptr->obstime, &temp_obstimet);
          if( temp_obstimet >= valid_timet)
          {
             found =1;
             break;
          }
          temp_ptr=(Observation*)temp_ptr->node.next;
    }

    if (found)
    {
        /* If the exact match was found in the first node or last node
        of obsptr list then it will be the final match */
        if (temp_obstimet == valid_timet)
        {
           obs_ptr_final_match = temp_ptr;
        }
        if ( temp_ptr->node.prev == NULL )
        {
           obs_ptr_final_match = temp_ptr;
        }
        else
        {
            obs_ptr_match1 = (Observation*)temp_ptr->node.prev;
            obs_ptr_match2 = temp_ptr;

            memset(&temp_obstimet, 0, sizeof(time_t));
            yearsec_dt_to_timet(obs_ptr_match1->obstime, &temp_obstimet);
            time_diff1 = valid_timet - temp_obstimet;
   
            memset(&temp_obstimet, 0, sizeof(time_t));
            yearsec_dt_to_timet(obs_ptr_match2->obstime, &temp_obstimet);
            time_diff2 = temp_obstimet - valid_timet;
            if( ( time_diff1 == time_diff2 ) ||
                ( time_diff1 > time_diff2 ) )
            {
                obs_ptr_final_match = obs_ptr_match2;
            }
            else if ( time_diff1 < time_diff2 )
            {
                 obs_ptr_final_match = obs_ptr_match1;
            }
         }
     }
     if (found == 0)
     {
        temp_ptr=obs_ptr;
        obs_ptr_final_match = (Observation*)ListLast(&temp_ptr->list);
     }

     /* Check if the match is within the window 2hrs */
     memset(&temp_obstimet, 0, sizeof(time_t));
     yearsec_dt_to_timet(obs_ptr_final_match->obstime, &temp_obstimet);

     if( !((temp_obstimet <= (valid_timet + (obsfcst_opts.tw_match_validtime*60)))
          && (temp_obstimet >= (valid_timet - (obsfcst_opts.tw_match_validtime*60)))) )
     {
          obs_ptr_final_match = NULL; /* Not a match since > windowtime*/
     }

     if (obs_ptr_final_match != NULL)
     {
         print_fcst_with_matchobs(logfp, fcst_ptr_temp, obs_ptr_final_match);
     }

    return obs_ptr_final_match;
}
