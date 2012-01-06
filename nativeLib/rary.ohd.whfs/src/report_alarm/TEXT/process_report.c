#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <sqlca.h>
#include <math.h>

#include "report_alarm.h"
#include "GeneralUtil.h"


/***********************************************************************
   do_report_alarm()
   
   Control the processing for the multiple lid,pe combinations.
   The design is focused on performing the report operations in a
   timely fashion.  Therefore, all alertalarm data are retrieved
   at one time, and these functions then track the pointers to the
   linked list information in order to generate the report.
   
   The modes vary in complexity.  A brief discussion of the modes...
   
   ALL and UNREPORTED are very simple modes; all mean all and 
   unreported means unreported.  These modes are handled easily
   by filtering the retrieved data when it is read from the database.
   
   RECENT and NEAR_NOW are also simple modes.  RECENT means
   recently posted, NEAR_NOW means observed or forecast data 
   near now.
   
   NEAREST is also pretty simple, in that it reports the nearest
   observed of forecast data, in terms of time.
   
   LATEST_MAXFCST is also straightforward in that it reports the 
   latest (i.e. nearest) observed data, but unlike the NEAREST mode,
   reports the maximum forecast value.
   
   FRESH and NEW_OR_INCREASED use the most complex concepts.
   
   The following modes do not consider the action_time:
   ALL, RECENT, NEAR_NOW, NEAREST, LATEST_MAXFCST   
   These other modes do consider the action_time:
   UNREPORTED, FRESH, NEW_OR_INCREASED
   All modes update the action_time for the reported records.
   
   The following modes used the minutes field:
   RECENT, NEAR_NOW, FRESH, NEW_OR_INCREASED.
   
   *********************************************************************/

void do_report_alarm(report_options_struct 	opt,
		     int			*num_alarms)
   
{
   AlertAlarmVal	*aaHead = NULL, *aaPtr = NULL;
   AlertAlarmVal	*aaLastPtr = NULL,
                        *tempPtr = NULL;
   FILE			*filePtr;
   char			filename[200];
   time_t		tnow;
   
   int          	len=0, rlen=0, istatus=0;
   char         	rdir_path[128];
   
   
   
   /* get the current time for consistent use throughout the program */
   
   time(&tnow);
   
   
   /* get the data. the order this data are retrieved and the SQL 
      where clause filter that is applied to the retrieval are 
      both critical to the later algorithms that apply the 
      report mode.  when getting the data, the order is:
      ORDER BY lid ASC, pe, ts, aa_check, validtime DESC;
      and the where clause may contain filters for the
      action_time (for unreported mode), and for the
      ts, aa_categ, aa_check, and pe which are filters
      that can be specified on the command line. */
   
   aaHead = get_alarm_data(opt, tnow);
   
   
   /* open the output file */
   
   
   len = strlen("whfs_product_dir");
   istatus = get_apps_defaults("whfs_product_dir", &len, rdir_path, &rlen);
   
   if (istatus != 0)
   {
      printf("whfs_product_dir directory undefined. Aborting\n");
      exit (0);
   }
   else
   {
      if (strlen(opt.file_suffix) > 0)
	 sprintf(filename, "%s/%s.%s", rdir_path, opt.product_id, opt.file_suffix); 
      else
	 sprintf(filename, "%s/%s", rdir_path, opt.product_id); 	 
   }
   
   filePtr = fopen(filename, "w");
   
   if (filePtr == NULL)
   {
      printf("Error opening file %s.  Aborting\n", filename);
      exit (0);
   } 
   
   
   /* write some intro type information to the output file */
   
   write_report_header(opt, tnow, filePtr);
   
   
   /* save a copy of the pointer to the last item which is
      needed later when processing the very last item in
      the linked list */
   
   if (aaHead != NULL)  
     aaLastPtr = (AlertAlarmVal *) ListLast(&aaHead->list);
   else
     aaLastPtr = NULL;
   
   
   
   /* loop on the values but process the data in groups
      all the while adjusting the linked list pointer to 
      control the stepping through the linked list */
   
   if (aaHead != NULL)
     aaPtr = (AlertAlarmVal *) ListFirst(&aaHead->list);
   else
     aaPtr = NULL;
   
   while (aaPtr)      
   {             
      /* perform the analysis for this location_id/physical
	 element/ts/aa_check group combination.
	 In the course of processing the data for this lid, pe
	 combination, the function marches through the linked list
	 until the next group combination is found, or the end of the
	 list is reached.  Afterwards, the location of the pointer is updated.
	 if not at the end, then continue with the new combination. */
      
      tempPtr = process_group_data(opt, filePtr, tnow, aaPtr, aaLastPtr,
                                   num_alarms);
      aaPtr = tempPtr;     
      
   } 
   
   
   /* write trailer info into the file */
   
   write_report_trailer(opt, filePtr, *num_alarms);
   
   
   /* close the output file */
   
   fclose(filePtr);
   
   
  /* free the data memory */
   
  if (aaHead != NULL)
  {
      FreeAlertAlarmVal(aaHead);
      aaHead = NULL;
  }    
   
   return;
   
}  


/*********************************************************************
   
   process_group_data()
   Control the processing for the current group.
   
   Note that the function argument called aaHead is not truly a
   head-of-the-list pointer for the entire list, but rather it
   marks the beginning of the pointer
   
   *********************************************************************/

AlertAlarmVal *process_group_data(report_options_struct 	opt,
				  FILE				*filePtr,
				  time_t			tnow,
				  AlertAlarmVal 		*aaHead,
				  AlertAlarmVal			*aaLastPtr,
				  int				*num_alarms)  
{  
   AlertAlarmVal	*aaPtr = NULL;
   AlertAlarmVal	*startPtr = NULL, *endPtr = NULL;
   char			cur_lid[LOC_ID_LEN + 2];
   char			cur_pe[SHEF_PE_LEN + 1]; 
   char			cur_ts[SHEF_TS_LEN + 1];
   char                 cur_aa_check[7];
   int			group_cnt;
   int                  num; 
   
   
   /* intialize the current lid,pe,aa_check info */
   
   if (aaHead != NULL)
   {
     strcpy(cur_lid, aaHead->lid);
     strcpy(cur_pe,  aaHead->pe);
     strcpy(cur_ts,  aaHead->ts);
     strcpy(cur_aa_check, aaHead->aa_check);
   }  
   group_cnt = 0;
   
   
   /* loop on the records in order to find the last record
      for this group */
   
   aaPtr = aaHead;  
   while (aaPtr)     
   {      
      /* if no longer working with the same location id/pe/ts/aa_check
	 combination, exit the loop because we are in a new group. */
      
      if (strcmp(aaPtr->lid,      cur_lid)      != 0 ||
	  strcmp(aaPtr->pe,       cur_pe)       != 0 || 
	  strcmp(aaPtr->ts,       cur_ts)       != 0 ||
	  strcmp(aaPtr->aa_check, cur_aa_check) != 0) 
	 break;
      
      
      /* increment the count for use later */
      
      group_cnt++;
      
      
      /* get the next record */
      
      aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);     
   } 
   
   
   /* this function is given the starting record within the linked
      list for the next group combination.  make a copy of this pointer
      and define a pointer to the last entry for the group being processed */
   
   startPtr = aaHead;
   
   if (aaPtr == (AlertAlarmVal *)NULL)
      endPtr = aaLastPtr;
   
   else
      endPtr = (AlertAlarmVal *)ListPrev(&aaPtr->node);
   
   
   /* at this point we have a pointer to the first and last 
      record for this group of records. now finally the program
      can process the data and possibly produce some output */
   
   /* calculate the number of reports which satisfy the alert-alarm situation.
      this is needed to see if there really are any records to report
      for this group. */
   
   num = calculate_group_report(tnow, group_cnt, startPtr, endPtr, opt);
   
   if (num != 0)
   {
      /* write the alert-alarm group header */
      
      write_group_header(filePtr,startPtr); 
      
      
      /* generate alert-alarm report and update database */
      
      write_group_report(opt, filePtr, tnow, group_cnt, startPtr, endPtr,
			 num_alarms);
   }
   
   
   /* return with either the pointer to the next record in the group 
      or the null pointer, which indicates the end of the entire list. */
   
   return(aaPtr);
   
} 


/*********************************************************************
   
   write_group_report()
   
   Write info on the alert/alarm for the current group
   
   If report mode is:
   
   ALL - then show all records.
   
   UNREPORTED - then still show all records knowing that the original
   database retrieval limited the retrieval to just those that were unreported.
   
   NEAREST - then only show the first record, knowing that that is the
   nearest one for this group.
   
   NEAR_NOW - then still show all records knowing that the original
   database retrieval limited the time window to the appropriate minutes.
   (validtime)
   
   RECENT - then still show all records knowing that the original 
   database retrieval limited the time window to the appropriate minutes.
   (for observed data: validtime, for forecast data:postingtime)
   
   FRESH - then only show records not reported within xxx minutes for
   observed data.  for forecast data, report the data with the maximum
   value if it is not reported within xxx minutes.
   
   LATEST_MAXFCST - then show nearest data for the observed data; show the data 
   with the maximum value for the forecast data.
   
   NEW_OR_INCREASED - show unreported data if the previous report
   is later than xxx minutes ago, or if the report has a higher
   value than the last reported.
   
   ********************************************************************/
void write_group_report(report_options_struct 	opt,
			FILE			*filePtr,
			time_t			tnow,
			int			group_cnt,
			AlertAlarmVal 		*earliestPtr,
			AlertAlarmVal		*latestPtr,
			int			*num_alarms)
{
   
   AlertAlarmVal	*aaPtr = NULL, *prev_actionPtr = NULL, *max_fcstPtr = NULL;
   AlertAlarmVal	*startPtr = NULL, *endPtr = NULL;   
   time_t		valid_timet;
   char			valid_ansi[ANSI_TIME_LEN + 1];
   char			basis_ansi[ANSI_TIME_LEN + 1];
   char			valid_str[20];
   char			tnow_ansi[ANSI_TIME_LEN + 1];
   char			dur_info[SHEF_DUR_NAME_LEN + 1];
   char			extr_info[SHEF_EX_NAME_LEN + 1];
   char			basis_info[50];
   time_t		prev_action_timet, prev_posting_timet;
   double 		prev_value;
   int 			status, cnt;
   int			reverse_order;
   time_t               tnow_after, tnow_before;      
   time_t               posting_timet;
   time_t               max_fcst_action_timet;
   
   
   /* define the values used in some of the report modes */
   
   tnow_after  = tnow + (opt.minutes * 60);
   tnow_before = tnow - (opt.minutes * 60);
   
   status = timet_to_yearsec_ansi(tnow, tnow_ansi);    
   
   
   /* initialize local pointers to dictate the order of processing.
      set it to the to first record for this group if
      processing observed or processed data; otherwise reverse the order 
      so that the forecast data nearest the current time are considered first.
      this special method is necessary because the alertalarm data are
      retrieved all at once in descending time. */
   
   if (earliestPtr->ts[0] == 'R' || earliestPtr->ts[0] == 'P')
   {
      startPtr = earliestPtr;
      endPtr   = latestPtr;
      reverse_order = 0;
   }
   else
   {
      startPtr = latestPtr;
      endPtr   = earliestPtr;
      reverse_order = 1;
   }
   aaPtr = startPtr;
   
   
   /* now loop on the data for this group and write the data
      records according to the report mode */
   
   /* for report mode ALL and UNREPORTED --------------------------
      for these modes, simply report all the retrieved data since
      the SQL filter did all the necessary filtering */
   
   if (opt.mode == REPORT_ALL ||
       opt.mode == REPORT_UNREPORTED)
   {
      cnt = 0;
      while (cnt < group_cnt)
      {
	 /* build a presentable string for the duration code value */
	 
	 build_string(aaPtr, &valid_timet, valid_ansi, valid_str,
		      basis_ansi, dur_info, extr_info, basis_info);
	 
	 
	 /* now write the info and update the database */	
	 
	 print_report(filePtr, aaPtr, valid_str, basis_info, dur_info, extr_info);
	 (*num_alarms)++;
	 
	 
	 /* since this value is reported, then update the action_time 
	    in the database.  */
	 
	 update_database(aaPtr, tnow_ansi, valid_ansi, basis_ansi);
	 
	 
	 /* get the next record depending on the order of the processing
	    and increment the count of the number processed */
	 
         if (!reverse_order)
	    aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);
         else
	    aaPtr = (AlertAlarmVal *) ListPrev(&aaPtr->node);
	 
	 cnt++;            
      }
   } 
   
   
   /* for report mode NEAREST---------------------------------------
      for this mode simply return the 'first' record, knowing
      that for observed it is the latest, and for forecast it
      is the first. */
   
   if (opt.mode == REPORT_NEAREST)
   {
     
      build_string(aaPtr, &valid_timet, valid_ansi, valid_str,
		   basis_ansi, dur_info, extr_info, basis_info);
      
      /* now write the info and update the database */
      
      print_report(filePtr, aaPtr, valid_str, basis_info, dur_info, extr_info);
      (*num_alarms)++;
      
      update_database(aaPtr,tnow_ansi,valid_ansi,basis_ansi);
      
   }
   
   
   /* for report mode NEAR_NOW--------------------------------------- */
   
   if (opt.mode == REPORT_NEAR_NOW)
   {
      cnt = 0;            
      while (cnt < group_cnt)
      { 
	 build_string(aaPtr, &valid_timet, valid_ansi, valid_str,
		      basis_ansi, dur_info, extr_info, basis_info);
	 
         if ((aaPtr->ts[0] == 'F' || aaPtr->ts[0] == 'C') &&
	     valid_timet < tnow_after)
	 {	    
	    print_report(filePtr, aaPtr, valid_str,
			 basis_info, dur_info, extr_info);
	    (*num_alarms)++;
	    
	    update_database(aaPtr, tnow_ansi, valid_ansi, basis_ansi);		    
	 }  
	 
	 
	 if ((aaPtr->ts[0] == 'R' || aaPtr->ts[0] == 'P') &&
	    valid_timet > tnow_before)
	 {
	    print_report(filePtr, aaPtr, valid_str,
			 basis_info, dur_info, extr_info);
	    (*num_alarms)++;
	    
	    update_database(aaPtr, tnow_ansi, valid_ansi, basis_ansi);	    
	 }
	 
	 if (!reverse_order)
	    aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);
	 else
	    aaPtr = (AlertAlarmVal *) ListPrev(&aaPtr->node);
	 
	 cnt++;  
      }	
   }
   
   
   /* for report mode RECENT. --------------------------------------
      The recent mode is based on the posting time. */
   
   if (opt.mode == REPORT_RECENT)
   {
      cnt = 0;            
      while (cnt < group_cnt)
      { 
	 build_string(aaPtr, &valid_timet, valid_ansi, valid_str, basis_ansi,\
		      dur_info, extr_info, basis_info);
	 status = yearsec_dt_to_timet(aaPtr->postingtime, &posting_timet);
	 
	 if (posting_timet > tnow_before)
	 { 
	    print_report(filePtr, aaPtr, valid_str, basis_info,
			 dur_info, extr_info);
	    (*num_alarms)++;
	    
	    update_database(aaPtr, tnow_ansi, valid_ansi, basis_ansi); 	    
	 }
	 	 
	 if (!reverse_order)
	    aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);
	 else
	    aaPtr = (AlertAlarmVal *) ListPrev(&aaPtr->node);
	 
	 cnt++;  
      }	
   }
   
   
   /* for report mode LATEST_MAXFCST --------------------------------- */
   
   if (opt.mode == REPORT_LATEST_MAXFCST)
   {
      
      if (aaPtr->ts[0] == 'R' || aaPtr->ts[0] == 'P') 
      {
         build_string(aaPtr, &valid_timet, valid_ansi, valid_str, basis_ansi,
		      dur_info, extr_info, basis_info);	
	 
	 print_report(filePtr, aaPtr, valid_str, basis_info, dur_info, extr_info);
	 
	 (*num_alarms)++;
  	 update_database(aaPtr, tnow_ansi, valid_ansi, basis_ansi); 	 	
      }
      
      else if (aaPtr->ts[0] == 'F' || aaPtr->ts[0] == 'C')
      {
	 max_fcstPtr = find_max_fcstPtr(aaPtr, startPtr, reverse_order,
					group_cnt);
	 
         build_string(max_fcstPtr, &valid_timet, valid_ansi,
	              valid_str, basis_ansi,
	              dur_info, extr_info, basis_info);  
	 
	 print_report(filePtr, max_fcstPtr, valid_str, basis_info,
		      dur_info, extr_info);
	 
	 (*num_alarms)++;
         update_database(max_fcstPtr, tnow_ansi, valid_ansi,
			 basis_ansi);	           
      }  
   } 
   
    
   /* for report mode FRESH----------------------------------------- */
   
   if (opt.mode == REPORT_FRESH)
   {
      cnt = 0;
      
      /* get the prev_actionPtr and its time */
      
      prev_actionPtr = find_prev_actionPtr(aaPtr,startPtr, reverse_order,
					   group_cnt);
      if (prev_actionPtr == (AlertAlarmVal *)NULL)
	 prev_action_timet = 0;
      else
	 status = yearsec_dt_to_timet(prev_actionPtr->validtime,
				      &prev_action_timet);
	    
      
      /* get the maximum forecast data and its time */
      
      max_fcstPtr = find_max_fcstPtr(aaPtr, startPtr, reverse_order, group_cnt);
      if (max_fcstPtr == (AlertAlarmVal *)NULL)
         max_fcst_action_timet = 0;
      else
      {     
        if (IsNull(DATETIME, &max_fcstPtr->action_time))
	   max_fcst_action_timet = 0;
        else
	   status = yearsec_dt_to_timet(max_fcstPtr->action_time,
				      &max_fcst_action_timet);
      }
      
      /* lopp on the records */
      
      while (cnt < group_cnt)
      {
	 build_string(aaPtr, &valid_timet, valid_ansi, valid_str, basis_ansi,
		      dur_info, extr_info, basis_info); 
	 
	 
	 if (aaPtr->ts[0] == 'R' || aaPtr->ts[0] == 'P')	       
	 {
	    if (valid_timet > prev_action_timet + (opt.minutes * 60))
	    {  	   
	       print_report(filePtr, aaPtr, valid_str, basis_info,
			    dur_info, extr_info);
	       (*num_alarms)++;
	       update_database(aaPtr, tnow_ansi, valid_ansi, basis_ansi); 	     
	    }
	 } 
	 
	 else if (aaPtr->ts[0] == 'F' || aaPtr->ts[0] == 'C')
	 {	    
	    build_string(max_fcstPtr, &valid_timet, valid_ansi, 
			 valid_str, basis_ansi,
			 dur_info, extr_info, basis_info);  
	    
	    
	    if (max_fcst_action_timet < tnow - (opt.minutes * 60))
	    {
	       print_report(filePtr, max_fcstPtr, valid_str, 
			    basis_info, dur_info, extr_info);
	       (*num_alarms)++;
	       
	       update_database(max_fcstPtr, tnow_ansi, valid_ansi,
			       basis_ansi); 
	       
	       break;
	    } 
	 }
	 
	 if (!reverse_order)
	    aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);
	 else
	    aaPtr = (AlertAlarmVal *) ListPrev(&aaPtr->node);
	 
	 cnt++;      		     
      }
      
   }
     
   
   /* for report mode NEW_OR_INCREASED ----------------------------- */
   
   if (opt.mode == REPORT_NEW_OR_INCREASED)
   {
      cnt = 0; 
            
      /* get the last reported record and its time and value. */
      
      prev_actionPtr = find_prev_actionPtr(aaPtr, startPtr, reverse_order, 
					   group_cnt);
      if (prev_actionPtr == (AlertAlarmVal *)NULL)
      {
	 prev_posting_timet = 0;
	 prev_value = -88888.;
      }
      else
      {
	 status = yearsec_dt_to_timet(prev_actionPtr->postingtime,
				      &prev_posting_timet);
	 prev_value = prev_actionPtr->value;
      }
            
      
      while (cnt < group_cnt)
      {		 
	 build_string(aaPtr, &valid_timet, valid_ansi, valid_str, basis_ansi,
		      dur_info, extr_info, basis_info);
	 
	 /* at most, only report unreported records */
	 
	 if (IsNull(DATETIME, &aaPtr->action_time)) 
	 {
	    yearsec_dt_to_timet(aaPtr->postingtime, &posting_timet);
	    
	    
	    /* if the record was posted after the last posted report 
	       plus XX minutes (i.e the report is 'new'), or if the
	       record has a higher value than the last posted record's
	       value (i.e. the report is 'increased'), then report it. */
	    
	    if ((posting_timet > prev_posting_timet + (opt.minutes*60)) ||
		(aaPtr->value > prev_value))
	    {
	       print_report(filePtr, aaPtr, valid_str, basis_info,
			    dur_info, extr_info);
	       (*num_alarms)++;
	       update_database(aaPtr, tnow_ansi, valid_ansi, basis_ansi);          
	    }	    
	 }
	 	 
	 if (!reverse_order)
	    aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);
	 else
	    aaPtr = (AlertAlarmVal *) ListPrev(&aaPtr->node);
	 
	 cnt++;
      }
   }
   
   
   return;   
}


/******************************************************************
   print_report()
   
   print each report in the report file.
   
   *****************************************************************/
void  print_report(FILE            *filePtr,
                   AlertAlarmVal    *aaPtr,
		   char             *valid_str,
		   char             *basis_info,
		   char             *dur_info,
		   char             *extr_info)
{
   
   if (strcmp(aaPtr->aa_check, UPPER_CHECKSTR) == 0) 
   {
      fprintf(filePtr, "  %s > %s  %s %7.1f %s",
	      aaPtr->aa_check, aaPtr->aa_categ, valid_str,
	      aaPtr->value, basis_info);
   }
   else if (strcmp(aaPtr->aa_check, LOWER_CHECKSTR) == 0)
   {
      fprintf(filePtr, "  %s < %s  %s %7.1f %s",
	      aaPtr->aa_check, aaPtr->aa_categ, valid_str,
	      aaPtr->value, basis_info);
   }
   else
   {
      fprintf(filePtr, "  %s > %s  %s %7.1f (value = %7.1f) %s",
	      aaPtr->aa_check, aaPtr->aa_categ, valid_str,
	      aaPtr->suppl_value, aaPtr->value, basis_info); 
   }	    	  
   
   
   if (strlen(dur_info) > 0 || strlen(extr_info) > 0)
      fprintf(filePtr, "(%s%s)\n", dur_info, extr_info);
   
   else
      fprintf(filePtr, "\n");
   
   
   return;
}           


/*****************************************************************
   build_string()
   
   build a presentable string for duration, extremum code and convert
   valid time to time_t format.
   ******************************************************************/
void build_string(AlertAlarmVal     *aaPtr,
	          time_t            *valid_timet,
	          char              *valid_ansi,
	          char              *valid_str,
	          char              *basis_ansi,
	          char              *dur_info,
                  char              *extr_info,
	          char              *basis_info)     
   
{   
   char  	where[500];
   int   	status;
   char  	basis_str[20];    
   ShefDur 	*durPtr = NULL;
   ShefEx  	*extrPtr = NULL;
   time_t   	basis_timet;
   
   
   /* build a presentable string for the duration code value */
   
   if (aaPtr->dur != 0)
   {
      sprintf(where, " WHERE dur=%d ", aaPtr->dur);
      durPtr = GetShefDur(where);
      if (durPtr == NULL)
      {
	 sprintf(dur_info, "Duration=%d ", aaPtr->dur);
      }
      else
      {
	 sprintf(dur_info, "%s ", durPtr->name);
	 FreeShefDur(durPtr);
      }
   }
   else
      strcpy(dur_info, "");
   
   
   /* build a presentable string for the extremum code value */
   
   if (strcmp(aaPtr->extremum, "Z") != 0)
   {
      sprintf(where, " WHERE extremum='%s' ", aaPtr->extremum);
      extrPtr = GetShefEx(where);
      if (extrPtr == NULL)
      {
	 sprintf(extr_info, "Extremum=%s ", aaPtr->extremum);
      }
      else
      {
	 sprintf(extr_info, "%s ", extrPtr->name);
	 FreeShefEx(extrPtr);
      }
   }
   else
      strcpy(extr_info, "");
   
   
   /* convert the valid time for use in the update statement
      and for presenting the time in the output */
   
   status = yearsec_dt_to_timet(aaPtr->validtime, valid_timet);      
   status = timet_to_yearsec_ansi(*valid_timet, valid_ansi);
   bld_timestamp(*valid_timet, valid_str);
   
   
   /* if forecast or contingency data, then show the basis time
      in a presentable fashion. also convert the basis time for 
      use in the update statement later. */
   
   status = yearsec_dt_to_timet(aaPtr->basistime, &basis_timet);      
   status = timet_to_yearsec_ansi(basis_timet, basis_ansi);
   
   if (aaPtr->ts[0] == 'F' || aaPtr->ts[0] == 'C')
   {
      bld_timestamp(basis_timet, basis_str);
      sprintf(basis_info, "fcast %s", basis_str);
   }
   else
      strcpy(basis_info, "");
   
   
   return;      	 
}  


/*********************************************************************
   update_database()
   
   Update the action time to now.  Note that because certain report
   modes include data that has already been reported, it is possible that
   the action_time is not null; in this case, the action_time
   field will show the last time the record was reported.
   
   *******************************************************************/
void update_database (AlertAlarmVal     *aaPtr,
		      char              *tnow_ansi,
		      char              *valid_ansi,
		      char              *basis_ansi)
{
   char where[500]="";
   int  status; 
   
   sprintf(where, " UPDATE alertalarmval SET action_time = '%s' "
	   "WHERE lid='%s' AND pe='%s' AND "
	   " dur=%d AND ts='%s' AND extremum='%s' AND "
	   " probability=%f AND validtime='%s' AND basistime='%s' AND "
	   " aa_categ = '%s' AND aa_check = '%s' ", tnow_ansi,
	   aaPtr->lid, aaPtr->pe, aaPtr->dur, aaPtr->ts, aaPtr->extremum,
	   aaPtr->probability, valid_ansi, basis_ansi,
	   aaPtr->aa_categ, aaPtr->aa_check);
   
   status = execStatement(where);
   if (status < 0)
   {
      printf("Postgres error %d executing %s\n", status, where);
   }
   else if (status == 100)
   {
      printf("Warning: Postgres SQL retrun code=%d, executing %s\n", status, where);
   }   
   
   return;  
}	 


/***************************************************************************
   find_prev_actionPtr()
   
   Find the most recent record which was reported already.
   It tacitly assumes that the data is actioned in the same order
   as the valid time.  If nothing has been reported yet, then
   it returns a null pointer.
   
   **************************************************************************/
AlertAlarmVal *find_prev_actionPtr(AlertAlarmVal  *aaPtr,
				   AlertAlarmVal  *startPtr,
				   int            reverse_order,
				   int		  group_cnt)   
{   
   AlertAlarmVal  	*prev_actionPtr = NULL;
   int 			cnt;
   
   /* initialize the returned value */
   
   prev_actionPtr = (AlertAlarmVal *)NULL;
    
   
   /* loop on the records */
   
   cnt = 0;                
   while (cnt < group_cnt)
   {
      if (!IsNull(DATETIME, &aaPtr->action_time))
      {    
	 prev_actionPtr = aaPtr;
	 break;
      }
      
      /* get the next record to process, if processing the forecast data,
	 then work in the reverse order */
      
      if(!reverse_order)
	 aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);      
      else
	 aaPtr = (AlertAlarmVal *) ListPrev(&aaPtr->node); 
      
      cnt++;
   }
   
   
   return(prev_actionPtr);
} 


/*****************************************************************
   find_max_fcstPtr()
   
   Find the pointer in the forecast combination group with the
   maximum value.  This function should not return with a null
   pointer; i.e. if there are data, there should always be 
   a maximum value.
   
   This function is used for the FRESH mode and LATEST_MAXFCST mode.
   
   *****************************************************************/
AlertAlarmVal *find_max_fcstPtr(AlertAlarmVal    *aaPtr,
				AlertAlarmVal    *startPtr,
				int              reverse_order,
				int              group_cnt)
{   
   AlertAlarmVal   	*max_fcstPtr;
   int             	cnt;
   double 		max_fcst_value;
   
   
   /* initialize the returned value */
   
   max_fcstPtr = (AlertAlarmVal *)NULL;
   
   
   /* only process the data if it is forecast type data */
   
   if (aaPtr->ts[0] == 'F' || aaPtr->ts[0] == 'C')
   {
      cnt = 0;       
      max_fcst_value = -8888.;
      
      
      while (cnt < group_cnt)
      {
	 if (aaPtr->value >= max_fcst_value)
	 {
	    max_fcstPtr = aaPtr;
	    max_fcst_value = max_fcstPtr->value;
	 }
	 
	 
	 /* for forecast data, the order is always reversed,
	    but leave this check in for clarity/consistency */
	 
	 if (!reverse_order)
	    aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);
	 else
	    aaPtr = (AlertAlarmVal *) ListPrev(&aaPtr->node);
	 
	 cnt++;
      }             
   }
   
   
   return(max_fcstPtr);
}     	 


/*********************************************************************************
   find_new_cnt()
   
   Loop on the records in the group to see if there are any records that have
   within the last XX minutes that have not been reported.
   The returned argument is used in the report mode: NEW_OR_INCREASED.
   
   ********************************************************************************/
int find_new_cnt(AlertAlarmVal          *aaPtr,
                 AlertAlarmVal          *startPtr,
                 time_t                 tnow,
		 report_options_struct  opt,
		 int                    reverse_order,
		 int                    group_cnt)
{     
   int     new_cnt;
   int     cnt;
   time_t  posting_timet;
   
   
   /* initialize the return argument */
   
   new_cnt = 0;
   
   
   /* loop */
   
   cnt = 0;
   
   /* see how many records have been posted within the time window */
   
   while (cnt < group_cnt)
   {
      yearsec_dt_to_timet(aaPtr->postingtime, &posting_timet);
      
      if (posting_timet > (tnow - (opt.minutes * 60)) && 
	  IsNull(DATETIME, &aaPtr->action_time))	 
      {
	 new_cnt++;
      } 
      	 
      
      if (!reverse_order)
	 aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);
      else
	 aaPtr = (AlertAlarmVal *) ListPrev(&aaPtr->node);
      
      cnt++; 
   }
   
   
   return(new_cnt);   
}  	 


/*****************************************************************
   find_start_highPtr()
   
   For report mode new_or_increased, loop on the data in the group
   to find if data has a value higher than that of the last reported
   value and assign the data to the start_highPtr.
   *****************************************************************/
AlertAlarmVal *find_start_highPtr(AlertAlarmVal         *aaPtr,
				  AlertAlarmVal         *startPtr,
				  AlertAlarmVal         *prev_actionPtr,
				  report_options_struct opt,
				  int                   reverse_order,
				  int                   group_cnt)
{   
   AlertAlarmVal   	*start_highPtr;
   int   		cnt;
   double 		prev_value;
   
   
   /* initialize the return argument */
   
   start_highPtr = (AlertAlarmVal *)NULL;
   
   
   /* if there is no previously reported value, then the 
      value is always 'higher' than the previously 
      reported since there was no previously reported */
   
   if (prev_actionPtr == (AlertAlarmVal *)NULL)
      prev_value = -8888.;
   else 
      prev_value = prev_actionPtr->value;
   
   
   /* loop */
   
   cnt = 0;  
   while (cnt < group_cnt)
   {      
      if ((aaPtr->ts[0] == 'P' || aaPtr->ts[0] == 'R') && 
	  (aaPtr->value - prev_value) > opt.min_val_diff)    
      {	   
	 start_highPtr = aaPtr;
	 break;
      }
      
      if (!reverse_order)
	 aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);
      else
	 aaPtr = (AlertAlarmVal *) ListPrev(&aaPtr->node);
      
      cnt++;
   }             
   
   
   return(start_highPtr);
}     	 


/********************************************************************
   calculate_group_report()
   
   Calculate the number of reported data for the lid-pe-ts-aa_check
   group. Actually, the number returned is really only used
   to see whether it is zero or non-zero, so for once this function
   finds one record to report, it returns to save time, so
   the number is not a true count.
   
   ******************************************************************/
int calculate_group_report(time_t                tnow,
			   int                   group_cnt,
			   AlertAlarmVal         *earliestPtr,
			   AlertAlarmVal         *latestPtr,
			   report_options_struct opt)
{
   AlertAlarmVal	*startPtr = NULL, *endPtr= NULL;
   AlertAlarmVal 	*aaPtr = NULL;
   AlertAlarmVal	*prev_actionPtr = NULL, *max_fcstPtr = NULL;
   int  		cnt;
   int			reverse_order;
   int			status, num;
   time_t  		valid_timet, posting_timet;
   time_t		prev_action_timet, prev_posting_timet;
   double		prev_value;
   
   
   /* initialize the returned argument */
   
   num = 0;
   
   
   /* determine the start and end pointe
      based on the order that the data were sorted */
   
   if (earliestPtr->ts[0] == 'R' || earliestPtr->ts[0] == 'P')
   {
      startPtr = earliestPtr;
      endPtr   = latestPtr;
      reverse_order = 0;   			    
   }
   else
   {
      startPtr = latestPtr;
      endPtr   = earliestPtr;
      reverse_order = 1;
   }
   aaPtr = startPtr;
   
   
   /* process for each of the modes to see what the 
      count of the number of to-be-reported records is */
   
   if (opt.mode == REPORT_NEAR_NOW)
   {
      cnt = 0;    
      while (cnt < group_cnt)
      {
	 yearsec_dt_to_timet(aaPtr->validtime, &valid_timet);
	 
	 if ((aaPtr->ts[0] == 'F' || aaPtr->ts[0] == 'C') && 
	     valid_timet < (tnow + opt.minutes*60))
	 {
	    num++;
	    break;
	 }
	 
	 if ((aaPtr->ts[0] == 'R' || aaPtr->ts[0] == 'P') &&
	     valid_timet > (tnow - opt.minutes*60)) 
	 {
	    num++;
	    break;
	 }
	 	 
	 if (!reverse_order)
	    aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);
	 else
	    aaPtr = (AlertAlarmVal *) ListPrev(&aaPtr->node);
	 
	 cnt++;
      }
   }
   
   
   if (opt.mode == REPORT_RECENT)
   {
      cnt = 0;    
      while (cnt < group_cnt)
      {
	 yearsec_dt_to_timet(aaPtr->postingtime, &posting_timet);
	 
	 if (posting_timet > (tnow - opt.minutes*60))
	 {
	    num++; 
	    break;
	 }
	 	 
	 if (!reverse_order)
	    aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);
	 else
	    aaPtr = (AlertAlarmVal *) ListPrev(&aaPtr->node);
	 
	 cnt++;
      }
   }
   
   
   if (opt.mode == REPORT_FRESH)
   {
      /* get the latest action time reported, if there is one.
	 for forecast data, this is the action time of the maximum
	 forecast value */
      
      if (aaPtr->ts[0] == 'R' || aaPtr->ts[0] == 'P')
      {	 
	 prev_actionPtr = find_prev_actionPtr(aaPtr, startPtr, reverse_order,
					      group_cnt);
	 
	 if (prev_actionPtr == (AlertAlarmVal *)NULL)
	    prev_action_timet = 0;
	 else
	    status = yearsec_dt_to_timet(prev_actionPtr->validtime,
					 &prev_action_timet);
      }
           
      if (aaPtr->ts[0] == 'F' || aaPtr->ts[0] == 'C')
      {
	 max_fcstPtr = find_max_fcstPtr(aaPtr, startPtr, reverse_order,
					group_cnt);
	 if (max_fcstPtr == (AlertAlarmVal *)NULL)
	    prev_action_timet = 0;
	 else
	 {   
	    if (IsNull(DATETIME, &max_fcstPtr->action_time))
	       prev_action_timet = 0;
	    else
	       status = yearsec_dt_to_timet(max_fcstPtr->action_time,
					 &prev_action_timet);
         } 					 
      }
            
      cnt = 0;
      while (cnt < group_cnt)
      {	 
	 yearsec_dt_to_timet(aaPtr->validtime, &valid_timet);
	 
	 if (aaPtr->ts[0] == 'R' || aaPtr->ts[0] == 'P')	       
	 {
	    if (valid_timet > prev_action_timet  + (opt.minutes * 60))
	    {
	       num++;
	       break;
	    }
	 } 
	 
	 else if (aaPtr->ts[0] == 'F' || aaPtr->ts[0] == 'C')
	 {	   	    
	    if (prev_action_timet < (tnow - (opt.minutes * 60)))
	    {
	       num++;	       
	       break;
	    } 
	 }
	 
	 if (!reverse_order)
	    aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);
	 else
	    aaPtr = (AlertAlarmVal *) ListPrev(&aaPtr->node);
	 
	 cnt++;      		     
      }           
    } 
   
   
   if (opt.mode == REPORT_NEW_OR_INCREASED)
   {
      cnt = 0; 
            
      /* get the last reported record and its time and value. */
      
      prev_actionPtr = find_prev_actionPtr(aaPtr, startPtr, reverse_order, 
					   group_cnt);
      if (prev_actionPtr == (AlertAlarmVal *)NULL)
      {
	 prev_posting_timet = 0;
	 prev_value = -88888.;
      }
      else
      {
	 status = yearsec_dt_to_timet(prev_actionPtr->postingtime,
				      &prev_posting_timet);
	 prev_value = prev_actionPtr->value;
      }
            
      
      while (cnt < group_cnt)
      {		 
	 /* at most, only report unreported records */
	 
	 if (IsNull(DATETIME, &aaPtr->action_time)) 
	 {
	    yearsec_dt_to_timet(aaPtr->postingtime, &posting_timet);
	    
	    /* if the record was posted after the last posted report 
	       plus XX minutes (i.e the report is new), or if the
	       record has a higher value than the last posted record's
	       value (i.e. the report is 'increased'), then report it. */
	    
	    if (posting_timet > prev_posting_timet + (opt.minutes*60))
	    {
	       num++;
	       break;
	    }
	    
	    else if (aaPtr->value > prev_value)
	    {
	       num++;
	       break;
	    }
	 }
	 	 
	 if (!reverse_order)
	    aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);
	 else
	    aaPtr = (AlertAlarmVal *) ListPrev(&aaPtr->node);
	 
	 cnt++;
      }
   }
   
   
   /* all these modes are guaranteed to use at least one value from
      the retrieved data. rather than count them, since all we really
      care is whether or not the count is non-zero, set the value
      and return. */
   
   
   if (opt.mode == REPORT_ALL     || opt.mode == REPORT_UNREPORTED ||
       opt.mode == REPORT_NEAREST || opt.mode == REPORT_LATEST_MAXFCST)      
      num++;
     
   
   return(num);   
}     	 
	            
	    
