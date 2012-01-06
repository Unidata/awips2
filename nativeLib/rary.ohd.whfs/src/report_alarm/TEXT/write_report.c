#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <sqlca.h>
#include <math.h>

#include "report_alarm.h"


/*********************************************************************
   
   write the group header line
   
   *********************************************************************/
void write_group_header(FILE		*filePtr,
			AlertAlarmVal	*aaPtr)
{
   Location		*locPtr = NULL;
   State		*statePtr = NULL;
   ShefPe		*pePtr = NULL;
   char			where[500];
   char			type_info[20];
   char 		pe_info[SHEF_PE_NAME_LEN + 1 + 10];
   DataLimits		*limitsPtr = NULL;
   char			alert_upper_str[15], alarm_upper_str[15],
                        alert_lower_str[15], alarm_lower_str[15],
                        alert_diff_str[15], alarm_diff_str[15];
   char			alertroc_str[15], alarmroc_str[15];
   int			status, limits_found;
   time_t		timet;
   
   
   /* get the location info for this group */
   
   sprintf(where, " WHERE lid='%s' ", aaPtr->lid);
   locPtr = GetLocation(where);
   if (locPtr == NULL)
   {
      fprintf(filePtr, "ERROR retrieving location info for %s\n\n", aaPtr->lid);
      return;
   }
   
   sprintf(where, " WHERE state='%s' ", locPtr->state);
   statePtr = GetState(where);
   
   
   /* get a description for this physical element */
   
   sprintf(where, " WHERE pe = '%s' ", aaPtr->pe);
   pePtr = GetShefPe(where);
   
   if (pePtr == NULL)
      sprintf(pe_info, "UndefinedName");
   else
      sprintf(pe_info, "%s", pePtr->name);
   
   
   /* make a description of the type portion of the type-source field */
   
   if (aaPtr->ts[0] == 'C')
      sprintf(type_info, "Contingency"); 
   
   else if (aaPtr->ts[0] == 'F')
      sprintf(type_info, "Forecast");
   
   else if (aaPtr->ts[0] == 'P')
      sprintf(type_info, "Processed");
   
   else
      sprintf(type_info, "Observed");
   
   
   /* get the limits for this group */
   
   status = yearsec_dt_to_timet(aaPtr->validtime, &timet);
   limitsPtr = get_limits(aaPtr->lid, aaPtr->pe, aaPtr->dur, timet,
			  &limits_found);  
   
   
   /* write header lines to the file for this group */
   
   fprintf(filePtr, "\n%s (%s) %s County, %s\n",
	   locPtr->name, locPtr->lid, locPtr->county, statePtr->name);
   
   fprintf(filePtr, "%s %s (%s %s)\n",
	   pe_info, type_info, aaPtr->pe, aaPtr->ts);
   
   if (limits_found)
   {
      if (limitsPtr->alert_upper_limit != MISSING_VAL)
	 sprintf(alert_upper_str, "%.1f", limitsPtr->alert_upper_limit);
      else
	 strcpy(alert_upper_str, "undef");
      
      if (limitsPtr->alarm_upper_limit != MISSING_VAL)
	 sprintf(alarm_upper_str, "%.1f", limitsPtr->alarm_upper_limit);
      else
	 strcpy(alarm_upper_str, "undef");

      if (limitsPtr->alert_lower_limit != MISSING_VAL)
	 sprintf(alert_lower_str, "%.1f", limitsPtr->alert_lower_limit);
      else
	 strcpy(alert_lower_str, "undef");
      
      if (limitsPtr->alarm_lower_limit != MISSING_VAL)
	 sprintf(alarm_lower_str, "%.1f", limitsPtr->alarm_lower_limit);
      else
	 strcpy(alarm_lower_str, "undef");

      if (limitsPtr->alert_diff_limit != MISSING_VAL)
	 sprintf(alert_diff_str, "%.1f", limitsPtr->alert_diff_limit);
      else
	 strcpy(alert_diff_str, "undef");
      
      if (limitsPtr->alarm_diff_limit != MISSING_VAL)
	 sprintf(alarm_diff_str, "%.1f", limitsPtr->alarm_diff_limit);
      else
	 strcpy(alarm_diff_str, "undef");
      
      if (limitsPtr->alert_roc_limit != MISSING_VAL)
	 sprintf(alertroc_str, "%.1f", limitsPtr->alert_roc_limit);
      else
	 strcpy(alertroc_str, "undef");
      
      if (limitsPtr->alarm_roc_limit != MISSING_VAL)
	 sprintf(alarmroc_str, "%.1f", limitsPtr->alarm_roc_limit);
      else
	 strcpy(alarmroc_str, "undef");
      
      fprintf(filePtr, "Limits: Upper limit Value=%s/%s  Lower limit Value=%s/%s                                                        Diff limit Value=%s/%s ROC= %s/%s\n",
	      alert_upper_str, alarm_upper_str, alert_lower_str, alarm_lower_str, 
              alert_diff_str, alarm_diff_str,  alertroc_str, alarmroc_str);
   }
   else
   {
      fprintf(filePtr, "Alert/Alarm limits not available.\n");
   }
   
   
   /* free the memory for the recent database retrievals */
   
   if (locPtr)
   {
      FreeLocation(locPtr);
      locPtr = NULL;
   }
      
   if (statePtr)
   {
      FreeState(statePtr);
      statePtr = NULL;
   }   
   if (pePtr)
   {
      FreeShefPe(pePtr);
      pePtr = NULL;
   }
   
   return;
}


/*********************************************************************
   
   write_report_header()
   write header info on this report to the log file.
   
   *******************************************************************/

void write_report_header(report_options_struct	opt,
			 time_t			tnow,
			 FILE			*filePtr)
{
   
   char	filter_str[100];
   
   /* write the current time */
   
   fprintf(filePtr,"***REPORT OF ALERT/ALARM DATA FROM THE HYDROLOGIC DATABASE***\n\n");
   fprintf(filePtr,"CREATED AT:  %s", asctime(gmtime(&tnow)));
   
   
   /* write the product id */
   
   fprintf(filePtr, "PRODUCT ID:  %s\n", opt.product_id);
   
   
   /* write a phrase desribing the report mode.
      the modes which consider the action_time are:
      UNREPORTED, FRESH, NEW_OR_INCREASED.
      The ones that don't consider it are:
      ALL, RECENT, NEAREST, NEAR_NOW, LATEST_MAXFCST */
   
   if (opt.mode == REPORT_ALL)
   {
      fprintf(filePtr, "REPORT MODE: ALL");
   }
   
   else if (opt.mode == REPORT_UNREPORTED)
   {
      fprintf(filePtr, "REPORT MODE: UNREPORTED");
   }
      
   else if (opt.mode == REPORT_RECENT)
   {
      fprintf(filePtr, "REPORT MODE: RECENT");
   }
      
   else if (opt.mode == REPORT_NEAR_NOW)
   {
      fprintf(filePtr, "REPORT MODE: NEAR NOW");
   } 
   
   else if (opt.mode == REPORT_NEAREST)
   {
      fprintf(filePtr, "REPORT MODE: NEAREST");
   }
   
   else if (opt.mode == REPORT_LATEST_MAXFCST)
   {
      fprintf(filePtr, "REPORT MODE: LATEST_MAXFCST");
   }   

   else if (opt.mode == REPORT_FRESH)
   {
      fprintf(filePtr, "REPORT MODE: FRESH");
   }
      
   else if (opt.mode == REPORT_NEW_OR_INCREASED)
   {
      fprintf(filePtr, "REPORT MODE: NEW_OR_INCREASED");
   }
   
   fprintf(filePtr, " (see end of report for mode description).\n");
  
   
   /* note which class of data is being considered via the filter */   
   
   sprintf(filter_str, "DATA FILTER: ");
   
   if (strlen(opt.filter) == 0)
   {
      strcat(filter_str, "All alerts/alarms considered (i.e. no filter).");
      fprintf(filePtr, "%s\n", filter_str);
   }
   
   else
   {
      strcat(filter_str, "Only considering");
      
      if (strstr(opt.filter, "O") != NULL && strstr(opt.filter, "F") == NULL)
	 strcat(filter_str, " observed"); 
      
      else if (strstr(opt.filter, "O") == NULL && strstr(opt.filter, "F") != NULL)
	 strcat(filter_str, " forecast");
      
      
      if (strstr(opt.filter, "R") != NULL )
	 strcat(filter_str, " rate-of-change"); 
      
      if (strstr(opt.filter, "L") != NULL )
	 strcat(filter_str, " upper_limit"); 
      
      if (strstr(opt.filter, "L") != NULL )
	 strcat(filter_str, " lower_limit");
      
      if (strstr(opt.filter, "D") != NULL)
	 strcat(filter_str, " Diff_limit"); 

      if (strstr(opt.filter, "T") != NULL && strstr(opt.filter, "M") == NULL)
	 strcat(filter_str, " alerts"); 
      
      else if (strstr(opt.filter, "T") == NULL && strstr(opt.filter, "M") != NULL)
	 strcat(filter_str, " alarms");
      
      else
	 strcat(filter_str, " alerts and alarms");
      
      
      fprintf(filePtr, "%s\n", filter_str);
      
   }
   
   sprintf(filter_str, "PE FILTER: ");
   
   if (strlen(opt.PEfilter) == 0)
   {
      strcat(filter_str, 
	     "  All Physical Elements are considered (i.e. no filter).");
      fprintf(filePtr, "%s\n", filter_str);
   }
   else
   {
      strcat(filter_str, "  Only considering ");
      strcat(filter_str, opt.PEfilter);
      strcat(filter_str, " physical element data");
      fprintf(filePtr, "%s\n", filter_str);     
   }  
   
   if  (opt.mode == REPORT_RECENT ||
	opt.mode == REPORT_NEAR_NOW ||
	opt.mode == REPORT_FRESH ||
	opt.mode == REPORT_NEW_OR_INCREASED)
      fprintf(filePtr, "NUM MINUTES: %d\n", opt.minutes); 
    
   
   fprintf(filePtr,
	   "--------------------------------------------------------------------\n");
   
   
   return;
}


/*********************************************************************
   
   write_report_trailer()
   write trailer info on this report to the log file.
   
   *******************************************************************/

void write_report_trailer(report_options_struct	opt,
			  FILE			*filePtr,
			  int			num_alarms)
{
   /* if no alarms found, then write message */
   
   if (num_alarms == 0)
      fprintf(filePtr, "\nNO ALERT/ALARM DATA TO REPORT FOR GIVEN REQUEST.\n\n");
   
   else
   {
      fprintf(filePtr, "\n%d ALERT/ALARMS REPORTED.\n", num_alarms);
      
      /* write lines that describes the info grouping and columns */
      
      fprintf(filePtr,
	      "\n-------------------------------------------------------------------\n");
      fprintf(filePtr,
	      "Limits: shown above are the alert threshold/alarm threshold.\n"
	      "Info grouped by location, physical element, type-source and check type.\n"
	      "Upper, lower, diff and rate-of-change (roc) limits are shown for each group.\n"
	      "Columns shown are: threat type > level, time, value [details].\n"
	      "For forecast data, the time of the forecast is also given.\n");
      fprintf(filePtr,
	      "Threat types: roc  =>value shown exceeded rate-of-change threshold\n"
	      "              lower<=value exceeded threshold\n\n"
	      "              upper=>value exceeded threshold\n\n"
	      "              diff=>value exceeded threshold\n\n");
   }

   
   /* describe the report mode in somewhat verbose terms. */
   
   if (opt.mode == REPORT_ALL)
   {
      fprintf(filePtr, "REPORT MODE: ALL\n");
      fprintf(filePtr, "Listing all data.\n");
   }
   
   else if (opt.mode == REPORT_UNREPORTED)
   {
      fprintf(filePtr, "REPORT MODE: UNREPORTED\n");
      fprintf(filePtr, "Listing all unreported records.\n");
   }
      
   else if (opt.mode == REPORT_RECENT)
   {
      fprintf(filePtr, "REPORT MODE: RECENT\n");
      fprintf(filePtr, 
	      "Listing observed and forecast records posted within the\n"
	      "past %d minutes\n",
	      opt.minutes);
   }
      
   else if (opt.mode == REPORT_NEAR_NOW)
   {
      fprintf(filePtr, "REPORT MODE: NEAR NOW\n");
      fprintf(filePtr, 
	      "Listing observed value within the past %d minutes\n"
	      "and forecast value within the next %d minutes.\n",
	      opt.minutes, opt.minutes);
   }
   
   else if (opt.mode == REPORT_NEAREST)
   {
      fprintf(filePtr, "REPORT MODE: NEAREST\n");
      fprintf(filePtr, 
	      "Listing most recent observed value and earliest forecast value.\n");
   }
   
   else if (opt.mode == REPORT_LATEST_MAXFCST)
   {
      fprintf(filePtr, "REPORT MODE: LATEST_MAXFCST\n");
      fprintf(filePtr, 
	      "Listing most recent observed value and maximum forecast value.\n");
   } 
      
   else if (opt.mode == REPORT_FRESH)
   {
      fprintf(filePtr, "REPORT MODE: FRESH\n");
      fprintf(filePtr, 
	      "For observed data, listing all records that are later than %d\n"
	      "minutes after the time of the most recent reported value.\n"
	      "For forecast data, listing the maximum forecast value if it\n"
	      "was not reported within the past %d minutes.\n",  
	      opt.minutes, opt.minutes);
   }
   
   else if (opt.mode == REPORT_NEW_OR_INCREASED)
   {
      fprintf(filePtr, "REPORT MODE: NEW_OR_INCREASED\n");
      fprintf(filePtr, 
	      "For observed data, listing unreported records if, the previous\n"
	      "reported was later than %d minutes ago OR if the report\n"
	      "has a higher value than the last reported record.\n"
	      "For forecasted data, listing the maximum forecast value if it\n"
	      "was not reported within the past %d minutes.\n",
	      opt.minutes, opt.minutes);
   }
   
   
   fprintf(filePtr, "\nEND OF REPORT\n");
   
   return;
}


/*********************************************************************
   
   bld_timestamp()
   reformat a timet variable into a string.
   
   *********************************************************************/

void bld_timestamp(time_t	timet,
		   char 	*timestr)
   
{    
   struct tm	*tm_struct;
   
   
   tm_struct = gmtime(&timet);
     
   strftime(timestr, 20, "%a-%b%d-%H:%M", tm_struct);
   
   
   return;
   
} 

