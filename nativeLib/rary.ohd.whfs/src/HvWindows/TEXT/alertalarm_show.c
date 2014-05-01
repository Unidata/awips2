/************************************************************************
   
   Functions to handle the setup control of alert alarm information.
   
     
   ************************************************************************/

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>


#include "alertalarm.h"
#include "alertalarm_show.h"
#include "get_loc_info.h"

/* variables global to this file */

static int 		numAArecords;
static AlertAlarmVal	*aaHead = NULL;

/************************************************************************
   alertalarm_show()
   Load the dialog shell.
   
   ************************************************************************/

void alertalarm_show(Widget w)
{  

   if (! alertalarmDS)
   {
      /* create the dialog shell and add the callbacks */
   
      create_alertalarmDS(GetTopShell(w));
      add_alertalarm_cbs();
   }     
   
   
   if (! XtIsManaged(alertalarmDS))
   {
      /* manage the windows now before doing the selections */ 
   
      XtManageChild(alertalarmFO);
      XtManageChild(alertalarmDS);
   
   
      /* initially show all records and sort by time, not location */
   
      SetMenuPos(aa_typeOM,  0);
      SetMenuPos(aa_aaOM, 0);
      SetMenuPos(aa_checkOM, 0);
    
  
      XmToggleButtonSetState(aa_sorttimeTB, 1, 0);
      XmToggleButtonSetState(aa_sortlocTB,  0, 0);
     
         
      /* load the scrolled lists */
   
      fill_aa_list();
   }
   
   return;
}


/************************************************************************
   aalistCB()
   Listbox call-back. Load details for selected item.
   
   ************************************************************************/
void aalistCB(Widget w, XtPointer client_data, XtPointer call_data)
{

   int		status;
   time_t	timet;
   char		field_str[40];
   
   AlertAlarmVal *aaPtr = NULL;
   int		listcnt;
   int		pos;
   int		*poslist;
   int		limits_found;
   DataLimits	*limitsPtr = NULL;
   
   
   
   /* if no real records are in the list, then blank out
      the detail fields below the list */
   
   if (numAArecords == 0)
   {
      XmTextSetString(alert_upper_limitTX, "");
      XmTextSetString(alert_lower_limitTX, "");
      XmTextSetString(alarm_upper_limitTX, "");
      XmTextSetString(alarm_lower_limitTX, "");
      XmTextSetString(alert_roc_limitTX, "");
      XmTextSetString(alarm_roc_limitTX, "");
      XmTextSetString(alert_diff_limitTX, "");
      XmTextSetString(alarm_diff_limitTX, "");
      
      XmTextSetString(aa_posttimeTX, "");
      XmTextSetString(aa_reptimeTX,  "");
      XmTextSetString(aa_prodidTX,   "");
      XmTextSetString(aa_prodtimeTX, "");
      
      
      /* if no real records to show, don't allow meaningless actions. */
      
      XtSetSensitive(aa_showgraphPB, False);
      XtSetSensitive(aa_showtablePB, False);
      
      return;
   }
   
   
   /* get the index to the selected item and get its info */
   
   XmListGetSelectedPos(w, &poslist, &listcnt);
   pos = poslist[0];
   XtFree((char *) poslist); 
   
   aaPtr = (AlertAlarmVal *) ListNth(&aaHead->list, pos);
   
   
   /* load the posting time */
   
   status = yearsec_dt_to_timet(aaPtr->postingtime, &timet);
   strcpy(field_str, format_aatime(timet));
   XmTextSetString(aa_posttimeTX, field_str);
   
      
   /* load the product id and time */

   XmTextSetString(aa_prodidTX, aaPtr->product_id);

   status = yearsec_dt_to_timet(aaPtr->producttime, &timet);
   strcpy(field_str, format_aatime(timet));
   XmTextSetString(aa_prodtimeTX, field_str);
   
   
   /* load the reporting time */
   
   status = yearsec_dt_to_timet(aaPtr->action_time, &timet);
   if (timet != 0)      
      strcpy(field_str, format_aatime(timet));
   else
      strcpy(field_str, " Unreported ");
   XmTextSetString(aa_reptimeTX, field_str);
   
   
   /* load the four alert alarm values */
   
   status = yearsec_dt_to_timet(aaPtr->validtime, &timet);
   limitsPtr = get_limits(aaPtr->lid, aaPtr->pe, aaPtr->dur, timet,
			  &limits_found);
   
   if (limits_found)
   {
      if (limitsPtr->alert_upper_limit != MISSING_VAL)
	 sprintf(field_str, "%7.1f", limitsPtr->alert_upper_limit);
      else
	 strcpy(field_str, " UNDEF ");
      XmTextSetString(alert_upper_limitTX, field_str);
      
      if (limitsPtr->alert_lower_limit != MISSING_VAL)
	 sprintf(field_str, "%7.1f", limitsPtr->alert_lower_limit);
      else
	 strcpy(field_str, " UNDEF ");
      XmTextSetString(alert_lower_limitTX, field_str);
     
      if (limitsPtr->alert_diff_limit != MISSING_VAL)
	 sprintf(field_str, "%7.1f", limitsPtr->alert_diff_limit);
      else
	 strcpy(field_str, " UNDEF ");
      XmTextSetString(alert_diff_limitTX, field_str);
      
      if (limitsPtr->alarm_diff_limit != MISSING_VAL)
	 sprintf(field_str, "%7.1f", limitsPtr->alarm_diff_limit);
      else
	 strcpy(field_str, " UNDEF ");
      XmTextSetString(alarm_diff_limitTX, field_str);
      
      if (limitsPtr->alert_roc_limit != MISSING_VAL)
	 sprintf(field_str, "%7.1f", limitsPtr->alert_roc_limit);
      else
	 strcpy(field_str, " UNDEF ");
      XmTextSetString(alert_roc_limitTX, field_str);
      
      if (limitsPtr->alarm_upper_limit != MISSING_VAL)
	 sprintf(field_str, "%7.1f", limitsPtr->alarm_upper_limit);
      else
	 strcpy(field_str, " UNDEF ");
      XmTextSetString(alarm_upper_limitTX, field_str);

      if (limitsPtr->alarm_lower_limit != MISSING_VAL)
	 sprintf(field_str, "%7.1f", limitsPtr->alarm_lower_limit);
      else
	 strcpy(field_str, " UNDEF ");
      XmTextSetString(alarm_lower_limitTX, field_str);
      
      
      if (limitsPtr->alarm_roc_limit != MISSING_VAL)
	 sprintf(field_str, "%7.1f", limitsPtr->alarm_roc_limit);
      else
	 strcpy(field_str, " UNDEF ");
      XmTextSetString(alarm_roc_limitTX, field_str);
   }
   
   else
   {
      XmTextSetString(alert_upper_limitTX, " MISSING ");
      XmTextSetString(alarm_upper_limitTX, " MISSING ");
      XmTextSetString(alert_lower_limitTX, " MISSING ");
      XmTextSetString(alarm_lower_limitTX, " MISSING ");
      XmTextSetString(alert_diff_limitTX, " MISSING ");
      XmTextSetString(alarm_diff_limitTX, " MISSING ");
      XmTextSetString(alert_roc_limitTX, " MISSING ");
      XmTextSetString(alarm_roc_limitTX, " MISSING ");
   }
         
   
   /* allow the user to select the time series buttons */
   
   XtSetSensitive(aa_showgraphPB, True);
   XtSetSensitive(aa_showtablePB, True);
   
   return;      
}


/************************************************************************
   aa_cleardetails()
   
   **********************************************************************/

void aa_cleardetails()
{
   
   return;
}

/************************************************************************
   delete_alertalarm()
   Pop up warning message  for deleting the selected item in the list, 
   and call the function to process it.
*************************************************************************/
void delete_alertalarm(Widget w, XtPointer client_data, XtPointer call_data)
{
   Widget   qstDS, okPB;
   char      buf[500];
   
   memset(&buf, '\0', sizeof(buf));
   sprintf(buf, "Do yu wish to delete this entry?");
   qstDS = QuestionDialog(alertalarmDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   /*Get the XmMessageBox OK button, and associate a callback to it */
   
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, aa_delete, NULL);
   
   if (! XtIsManaged(qstDS))
      XtManageChild(qstDS);      

   return;
} 

/************************************************************************
   aa_delete()
   delete the selected item in the alert alarm list, delete it from
   database, load data to the list.
*************************************************************************/
void aa_delete(Widget w, XtPointer client_data, XtPointer call_data)
{
   int    *poslist, cnt, nth_pos;
   AlertAlarmVal   *aaPtr = NULL;
   char   where[1500];
   int    error = 0, status;
   char   validtime_ansi[ANSI_TIME_LEN + 1], basistime_ansi[ANSI_TIME_LEN+1];
   
   
   SetCursor(alertalarmFO, XC_watch);
   
   XmListGetSelectedPos(aa_valLS, &poslist, &cnt);
   if (cnt > 0)
   { 
      nth_pos = poslist[0];
      aaPtr = (AlertAlarmVal *) ListNth(&aaHead->list, nth_pos);   
   
      status = yearsec_dt_to_ansi(aaPtr->validtime, validtime_ansi);
      status = yearsec_dt_to_ansi(aaPtr->basistime, basistime_ansi);
         
      memset(&where, '\0', sizeof(where));
      sprintf(where, " WHERE (lid = '%s') AND (pe = '%s') AND "
                     "(dur = %d) AND (ts = '%s') AND (extremum='%s')"
		     " AND (probability=%f) AND (aa_categ='%s') AND "
		     "(aa_check='%s') AND (validtime='%s') AND "
		     "(basistime='%s')", aaPtr->lid, aaPtr->pe,
		     aaPtr->dur, aaPtr->ts, aaPtr->extremum, aaPtr->probability,
		     aaPtr->aa_categ, aaPtr->aa_check, validtime_ansi,
		     basistime_ansi);
         
      error = DeleteAlertAlarmVal(where);
      
      if (!error)
      {
      	 XmListDeleteAllItems(aa_valLS);
	 		       
      }
      else
      {
         UnsetCursor(alertalarmFO);
	 ErrorDialog(alertalarmDS, DbErrorString(error));
	 return;
      }
      
  }
  /*load and display data in the list*/
      
  fill_aa_list();  	 
  
  UnsetCursor(alertalarmFO);
  
  return;

}   

/************************************************************************
   
    update_aalistCB()
    reached thru callback 
    
   **********************************************************************/
void update_aalistCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   
   fill_aa_list();
   
   return;
}


/************************************************************************
   fill_aa_list()
   Fill main list.
   
   ************************************************************************/
void fill_aa_list()
{
   XmStringTable        xmStr;
   Arg                  arg[10];
   int                  ac, i;
   long			cnt;		
   char			liststr[200];
   
   char 	where[180], checkstr[40]; 	
   char         dt_ansi[ANSI_TIME_LEN+1];	/* ASCII version of datetime */
   char         validtime_ansi[17];		/* YYYY-MM-DD HH:MM format  */
   char         basistime_ansi[17];		
   char		qcstr[MAXLEN_QCSYMBOL];   
   AlertAlarmVal	*aaPtr;
   int			type_option, check_option, aa_option;
   int          returnvalue;
   double        dlat,dlon;
   char         name[ALLOWED_NAME_LEN +1];
   
   /* get the value of the option menus which will dictate the where clause.
      for the typeOM, the values are 0=all, 1=obs, 2=fcst.
      for the alertalarmOM, the values are 0=both, 1=alarm, 2=alert.
      for the checkOM, the values are 0=any, 1=roc, 2=upper, 3=lower, 4=diff*/
   
   type_option  = GetMenuPos(aa_typeOM);
   aa_option    = GetMenuPos(aa_aaOM);
   check_option = GetMenuPos(aa_checkOM);      
   
   
   /* define the where clause using the three options */
   
   if (type_option == 0)
      sprintf(where, " WHERE ts LIKE '%%' ");
   else if (type_option == 1)
      sprintf(where, " WHERE (ts LIKE 'R%%' OR ts LIKE 'P%%') ");
   else 
      sprintf(where, " WHERE (ts LIKE 'F%%' OR ts LIKE 'C%%') ");
      
   
   if (aa_option == 0)
      sprintf(checkstr, " ");
   else if (aa_option == 1)
      sprintf(checkstr, " AND aa_categ= '%s' ", ALARM_CATEGSTR);
   else if (aa_option == 2)
      sprintf(checkstr, " AND aa_categ= '%s' ", ALERT_CATEGSTR);
      
   strcat(where, checkstr);
   
   
   if (check_option == 0)
      sprintf(checkstr, " ");      
   else if (check_option == 1)
      sprintf(checkstr, " AND aa_check= '%s' ", ROC_CHECKSTR);      
   else if (check_option == 2)
      sprintf(checkstr, " AND aa_check= '%s' ", UPPER_CHECKSTR);
   else if (check_option == 3)
      sprintf(checkstr, " AND aa_check= '%s' ", LOWER_CHECKSTR);
   else if (check_option == 4)
      sprintf(checkstr, " AND aa_check= '%s' ", DIFF_CHECKSTR);
      
   strcat(where, checkstr);
   
   
   /* sort by location, depending upon whether getting
      all, observed, or forecast data */
   
   if (XmToggleButtonGetState(aa_sortlocTB))
   {
      if (type_option != 2)
	 strcat(where, " ORDER BY lid, validtime DESC, ts ASC, basistime DESC ");
      else
	 strcat(where, " ORDER BY lid, validtime ASC,  ts ASC, basistime DESC ");
   }
   
   
   /* sort by time, in order based on whether getting all, observed,
      or forecast data */
   
   else
   {
      if (type_option != 2)
	 strcat(where, " ORDER BY validtime DESC, lid, basistime DESC");
      else
	 strcat(where, " ORDER BY validtime ASC,  lid, basistime DESC");
   }
   
   
   /* now get the data */
   
   aaHead = GetAlertAlarmVal(where);
   
   /* get the count of records for the list */
   
   if ( aaHead != NULL ) 
   {
      numAArecords = ListCount ( & aaHead->list ) ;
      cnt = numAArecords ;
   }
   else
   {
      numAArecords = 0 ;
      cnt = 1 ;
   }
   
   /* allocate the Motif list strings */
   
   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
   
   
   /* load the list strings */
   
   if (aaHead == NULL)						
   {								
      sprintf(liststr, "No data matching the alert/alarm conditions. ");      
      xmStr[0] = XmStringCreateSimple(liststr);	
      
      
   }
   
   else
   {
      aaPtr = (AlertAlarmVal *) ListFirst(&aaHead->list);
      
      for (i = 0; i < cnt; i++)
      {
	 
	 /* reformat some of the fields for improved display */
	 
	 yearsec_dt_to_ansi(aaPtr->validtime, dt_ansi);	
	 strncpy(validtime_ansi, &dt_ansi[5], 11);             
	 validtime_ansi[11] = '\0'; 
	 
	 
	 /* for time vars, need full year info and two separate fields 
	    for possible parsing later. only show the basis time
	    if processing observed type data */
	 
	 if (aaPtr->ts[0] == 'R' || aaPtr->ts[0] == 'P')
	 {
	    strcpy(basistime_ansi,  "----- -----");
	 }
	 else
	 {
	    yearsec_dt_to_ansi(aaPtr->basistime, dt_ansi);	
	    strncpy(basistime_ansi, &dt_ansi[5], 11);             
	    basistime_ansi[11] = '\0';
	 }
	 
	 
	 build_qc_symbol(aaPtr->quality_code, qcstr);
	 
	 /*get name for the lid*/
	 
	 if (aaPtr->lid != NULL)
  	   returnvalue = get_loc_info(aaPtr->lid,&dlat,&dlon,name);
	   
	 /* don't bother showing the following fields: probability value, 
	    shef qualifier code, shef revision code, action time.
	    If aa_check is not 'roc', then assign the value of roc as "--" */
	 
	 if ( (strcmp(aaPtr->aa_check, ROC_CHECKSTR) == 0)  ||
              (strcmp(aaPtr->aa_check, DIFF_CHECKSTR) == 0)  )
	 {
	    
	    sprintf(liststr, "%-8s %-20s %2s %4d %2s %1s "	
		    "%8.2f %8.2f %1s   %-6s%-6s %11s %11s",	
		    aaPtr->lid, name,aaPtr->pe, aaPtr->dur, aaPtr->ts, aaPtr->extremum,                         
		    aaPtr->value, aaPtr->suppl_value, qcstr, aaPtr->aa_check, 
		    aaPtr->aa_categ,validtime_ansi, basistime_ansi); 
	 }
	 
	 else 
	 {	    
	    sprintf(liststr, "%-8s %-20s %2s %4d %2s %1s "	
		    "%8.2f %8s %1s   %-6s%-6s %11s %11s",	
		    aaPtr->lid, name,aaPtr->pe, aaPtr->dur, aaPtr->ts, 
		    aaPtr->extremum, aaPtr->value, "   --  ", 
		    qcstr, aaPtr->aa_check, aaPtr->aa_categ,
		    validtime_ansi, basistime_ansi);
	 }
	 
	 
	 
	 xmStr[i] = XmStringCreateSimple(liststr);
	 
	 aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node);		
      }	
      
   }
   
   
   /* load the list in the scrolled list */
   
   ac = 0;							
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;		
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;		
   XtSetValues(aa_valLS, arg, ac);				
   
   
   /* free the memory */
   
   for (i = 0; i < cnt; i++) XmStringFree(xmStr[i]) ;
   XtFree ( ( char * ) xmStr ) ;				
   
   /* Select first item in list */
   
   XmListSetPos(aa_valLS, 1);
   XmListSelectPos(aa_valLS,   1, 1);
   
   return;
}


/*********************************************************************
   invoke_aa_timeseries()
   
   PURPOSE
   Invoke the time series control window.
   
   *********************************************************************/

void invoke_AAtimeseries(Widget w, XtPointer client_data,
                         XtPointer call_data)
{
   time_t 	validtime, begintime, endtime;   
   TSGEN_INFO 	ts_invoke;
   int 		display_mode;
   AlertAlarmVal	*aaPtr;
   int		pos, *poslist;
   int		listcnt;
   int		status;
   
   
   
   /* get the index to the item currently selected */
   
   XmListGetSelectedPos(aa_valLS, &poslist, &listcnt);
   pos = poslist[0];
   XtFree((char *) poslist); 
   
   aaPtr = (AlertAlarmVal *) ListNth(&aaHead->list, pos);
   
   
   /* get the graph or table choice as passed through the callback.
      this argument is then used below when invoking the time series. */
   
   display_mode = (int )client_data;
   
      
   /* set the begining of the period of record 12 hours previous to
      validtime and the end to 12 hours beyond */
      
   status = yearsec_dt_to_timet(aaPtr->validtime, &validtime);
   
   begintime = validtime - (12 * SECONDS_PER_HOUR);
   endtime   = validtime + (12 * SECONDS_PER_HOUR);
   
   
   /* load in the structure that controls the time series invocation */
   
   strcpy(ts_invoke.lid, aaPtr->lid);     
   
   ts_invoke.display_mode   = display_mode;
   ts_invoke.group_check    = FALSE;      
   ts_invoke.standalone     = NON_STANDALONE;  
   ts_invoke.pedtse_defined = TRUE;
   
   ts_invoke.Begin_time = begintime;
   ts_invoke.End_time   = endtime;
   
   
   /* load the PEDTSE info structure, for just ONE PEDTSE */
   
   ts_invoke.nitems = 1;
   
   strcpy(ts_invoke.pedtse[0].pe,       aaPtr->pe);
   strcpy(ts_invoke.pedtse[0].ts,       aaPtr->ts);
   strcpy(ts_invoke.pedtse[0].extremum, aaPtr->extremum);
   ts_invoke.pedtse[0].dur = aaPtr->dur;
   
   /* call the Timeseries Control Window */
   
   show_TSControlDS(w, ts_invoke);
   
   return;
   
}


/************************************************************************
   
   Close the window.
   
   ************************************************************************/

void close_alertalarmCB()
{
   if (XtIsManaged(alertalarmDS))
   {
      XtDestroyWidget(alertalarmDS);
      alertalarmDS = NULL;
   }
   
   
   /* free memory for database retrieval */
   
   if (aaHead != NULL)   
   {					
      FreeAlertAlarmVal(aaHead);				
      aaHead = (AlertAlarmVal *)NULL;				
   }							
   
   return;
}


/************************************************************************
   
   Add the stations selection callbacks.
   
   ************************************************************************/

void add_alertalarm_cbs()
{
   
   Atom	wmAtom;
   
   
   /* callbacks for sort-by radio buttons. */
   
   XtAddCallback(aa_sortlocTB, XmNvalueChangedCallback, update_aalistCB, NULL);
   
   
   /* callbacks for filter option menu pushbuttons */
   
   XtAddCallback(aa_typeallPB,  XmNactivateCallback, update_aalistCB, NULL);
   XtAddCallback(aa_obsPB,      XmNactivateCallback, update_aalistCB, NULL);
   XtAddCallback(aa_fcstPB,     XmNactivateCallback, update_aalistCB, NULL);
   
   XtAddCallback(aa_bothPB,     XmNactivateCallback, update_aalistCB, NULL);
   XtAddCallback(aa_alertPB,    XmNactivateCallback, update_aalistCB, NULL);
   XtAddCallback(aa_alarmPB,    XmNactivateCallback, update_aalistCB, NULL);
   
   XtAddCallback(aa_checkallPB, XmNactivateCallback, update_aalistCB, NULL);
   XtAddCallback(aa_upper_limitPB,      XmNactivateCallback, update_aalistCB, NULL);
   XtAddCallback(aa_lower_limitPB,      XmNactivateCallback, update_aalistCB, NULL);
   XtAddCallback(aa_diff_limitPB,      XmNactivateCallback, update_aalistCB, NULL);
   XtAddCallback(aa_rocPB,      XmNactivateCallback, update_aalistCB, NULL);
   

   /* callbacks on scrolled list of products */
   
   XtAddCallback(aa_valLS, XmNdefaultActionCallback,   aalistCB, NULL);
   XtAddCallback(aa_valLS, XmNbrowseSelectionCallback, aalistCB, NULL); 
   
   
   /* callbacks for graphical and tabular Timeseries buttons */
   
   XtAddCallback(aa_showgraphPB, XmNactivateCallback, invoke_AAtimeseries,
		 (XtPointer *)GRAPH_TS);
   XtAddCallback(aa_showtablePB, XmNactivateCallback, invoke_AAtimeseries,
		 (XtPointer *)TABULAR_TS);		 
   
   
   /* callbacks on atom widget and close button */ 
   
   wmAtom = XmInternAtom(XtDisplay(alertalarmDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(alertalarmDS, wmAtom, close_alertalarmCB, NULL);
   XtAddCallback(aa_closePB, XmNactivateCallback, close_alertalarmCB, NULL);
   
   /*callbacks on delete button*/
   
   XtAddCallback(aa_deletePB, XmNactivateCallback, delete_alertalarm, NULL);
   
   
   return;
}


/************************************************************************
   format_aatime()
   
   PURPOSE   
   Formats a time string for output.
   
   *********************************************************************/

char *format_aatime(time_t timeval)
{
   static char outstr[SHORT_LEN];
   struct tm *tm_struct;
   
   tm_struct = gmtime(&timeval);
   strftime(outstr, SHORT_LEN, "%a %m/%d %H:%M", tm_struct); 
   
   return(outstr);
}
