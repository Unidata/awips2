#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "report_alarm.h"
#include "GeneralUtil.h"

/*********************************************************************
   get_alarm_data()
   
   Get the appropirate AlertAlarm data from the database,
   based on the user request
   
   *******************************************************************/

AlertAlarmVal *get_alarm_data(report_options_struct	opt,
			      time_t			tnow)
   
{    
   char			where[500];
   char			subclause[100];
   AlertAlarmVal	*aaHead = NULL;
   int			cnt;

   
   /* build the beginning of the where clause according
      to the report mode.  */
   
   if (opt.mode == REPORT_ALL || 
       opt.mode == REPORT_RECENT ||
       opt.mode == REPORT_NEAR_NOW ||
       opt.mode == REPORT_NEAREST ||
       opt.mode == REPORT_LATEST_MAXFCST ||
       opt.mode == REPORT_FRESH ||
       opt.mode == REPORT_NEW_OR_INCREASED)
   {
      sprintf(where, "WHERE ");
   }
   
   
   /* if getting only unreported data, let the query filter
      out the reported data*/
   
   else if (opt.mode == REPORT_UNREPORTED)
   {
      sprintf(where, " WHERE action_time IS NULL AND");
   }
     
  
   /* append the where clauses based on the type-source */
   
   if (strstr(opt.filter, "O") != NULL && strstr(opt.filter, "F") == NULL)
      strcat(where, " (ts LIKE 'R%' OR ts LIKE 'P%') "); 
   
   else if (strstr(opt.filter, "O") == NULL && strstr(opt.filter, "F") != NULL)
      strcat(where, " (ts LIKE 'F%' OR ts LIKE 'C%') ");  
   
   else
      strcat(where,
	     " (ts LIKE 'R%' OR ts LIKE 'P%' OR ts LIKE 'F%' OR ts LIKE 'C%') "); 
    
   
   /* append the where clause based on the alert/alarm category field */
   
   if (strstr(opt.filter, "T") != NULL && strstr(opt.filter, "M") == NULL)
      sprintf(subclause, " AND aa_categ= '%s' ", ALERT_CATEGSTR); 
   
   else if (strstr(opt.filter, "T") == NULL && strstr(opt.filter, "M") != NULL)
      sprintf(subclause, " AND aa_categ= '%s' ", ALARM_CATEGSTR);
   
   else
      sprintf(subclause , " ");
      
   strcat(where, subclause);
   
   
   /* append the where clause based on the alert/alarm check field */
   {
     int roc_flag , up_flag , low_flag , diff_flag ;
     roc_flag = 0;
     up_flag = 0;
     low_flag = 0;
     diff_flag = 0;
     
     if (strstr(opt.filter, "R") != NULL)
         roc_flag = 1;
     if (strstr(opt.filter, "L") != NULL)
         low_flag = 1;
     if (strstr(opt.filter, "U") != NULL)
         up_flag = 1;
     if (strstr(opt.filter, "D") != NULL)
         diff_flag = 1;

     if(roc_flag != 1 && up_flag != 1 && low_flag != 1 && diff_flag != 1)
     {
         sprintf(subclause, " ");
         strcat(where, subclause);
     }
     else
     {
         int initial_entry = 0, loop_cnt = 0;
         sprintf(subclause, " AND aa_check in (");
         strcat(where, subclause);
      
         for(loop_cnt = 1; loop_cnt <=4; loop_cnt++)
         {
	     switch(loop_cnt)
	     {
	      case 1:
	           if(up_flag == 1)
	           {
	              if(initial_entry == 0)
		      {
	                 sprintf(subclause, "'%s'", UPPER_CHECKSTR);
                         strcat(where, subclause);
		         initial_entry = 1;
		      }
		      else
		      {
	                 sprintf(subclause,",'%s'", UPPER_CHECKSTR);
                         strcat(where, subclause);
                      }
	           }
		   break;
	      case 2:
	           if(low_flag == 1)
	           {
	              if(initial_entry == 0)
		      {
	                 sprintf(subclause, "'%s'", LOWER_CHECKSTR);
                         strcat(where, subclause);
		         initial_entry = 1;
		      }
		      else
		      {
	                 sprintf(subclause, ",'%s'", LOWER_CHECKSTR);
                         strcat(where, subclause);
                      }
	           }
		   break;
	      case 3:
	           if(roc_flag == 1)
	           {
	              if(initial_entry == 0)
		      {
	                 sprintf(subclause, "'%s'", ROC_CHECKSTR);
                         strcat(where, subclause);
		         initial_entry = 1;
		      }
		      else
		      {
	                 sprintf(subclause, ",'%s'", ROC_CHECKSTR);
                         strcat(where, subclause);
                      }
	           }
		   break;
	      case 4:
	           if(diff_flag == 1)
	           {
	              if(initial_entry == 0)
		      {
	                 sprintf(subclause, "'%s'", DIFF_CHECKSTR);
                         strcat(where, subclause);
		         initial_entry = 1;
		      }
		      else
		      {
	                 sprintf(subclause, ",'%s'", DIFF_CHECKSTR);
                         strcat(where, subclause);
                      }
	           }
		   break;
	       default:
                   break;
             }/* End of switch */
         }/* End of for loop */

	 strcpy(subclause," ) ");
	 strcat(where, subclause);

     }/* End of else **/

   }
   
   
   /* adjust the query according to any PE filter */
  
   if (strcmp(opt.PEfilter, "") != 0)
   {
      sprintf(subclause, " AND pe = '%s' ", opt.PEfilter);
      strcat(where, subclause);
   }

   
   /* adjust the query by considering the forecast data should be retrieved only
   when the validtime is equal or later than NOW*/
   
   strcat(where, " AND (ts NOT LIKE 'F%' OR validtime >= current_timestamp) ");
   
   
   /* specify the sort order. this is very important as later algorithms 
      depend on this order */
   
   strcat(where, " ORDER BY lid ASC, pe, ts, aa_check, validtime DESC ");
   
   printf("Where clause for get alertalarmval: %s\n", where);
   fflush(NULL);
   
   /* now get the data */
   
   aaHead = GetAlertAlarmVal(where); 
   
   if (aaHead != NULL)     
     cnt = ListCount(&aaHead->list);
   else
     cnt = 0;
      
   
   return(aaHead);     
} 


