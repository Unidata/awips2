/************************************************************************
   
   Name:		latestrep_show.c	
   Functions to handle the display of latest observed information.
   
   
   ***********************************************************************/

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

#include "time_convert.h"
#include "DbmsUtils.h"
#include "Xtools.h"

#include "LatestObsValue.h"
#include "Dcp.h"
#include "Telem.h"

#include "latestrep.h"
#include "latestrep_show.h"

#include "QualityCode.h"
#include "get_loc_info.h"
#include "time_convert.h"
#include "time_defs.h"

/* Variables global to this file */

LatestObsValue	*latestobsvalueHead = NULL ;
Dcp 		*dcpHead = NULL ;
Telem 		*telemHead = NULL ;

int 		records_found;
XtIntervalId 	timer_id;
int 		timer_pending=0;
int 		sort_type;
int 		sort_type = LOCATION;
int 		list_type = LIST_ALL;


/* Externals */
/**** Changed for POSTGRES
extern long SQLCODE;
*****/


/************************************************************************
   
   Load the latest_report dialog shell.
   
   ***********************************************************************/

void latestrep_show(Widget w, char* lid)
{
   /* initialize values */
   
   
   /* create the dialog shell and add the callbacks */
   
   create_latestrepDS(GetTopShell(w));
   add_latestrep_cbs();      
   
   
   /* start "clock" */
   
   timer_id = XtAppAddTimeOut(XtWidgetToApplicationContext(latestrepFO),
			      1000L, currenttimeTimer, NULL);
   timer_pending = 1;
   
   
   /* manage the windows now before doing the selections */   
   
   XtManageChild(latestrepFO);
   XtManageChild(latestrepDS);
   
   
   /* initialize data memory */
   
   latestobsvalueHead = (LatestObsValue *)(NULL);
   dcpHead            = (Dcp *)(NULL);
   telemHead          = (Telem *)(NULL);
   
   
   /* Set default duration */
   
   XmTextSetString(durationTX, DURATION_DEFAULT);
   
   
   /* Desensitize widgets */
   
   XtSetSensitive(durationTX, False);
   XtSetSensitive(hoursLB,    False);
   
   
   /* init listboxes */
   
   fill_top_listbox();
   
   
   return;
}


/************************************************************************
   
   Fill top listbox.
   
   **********************************************************************/
void fill_top_listbox()
{
   LatestObsValue *latestobsvaluePtr;
   char where[1024];
   XmStringTable xmStr;
   long cntIn;
   long cntOut;
   char ansi_postingtime[ANSI_TIME_LEN+1];
   char ansi_obstime[ANSI_TIME_LEN+1];
   char liststr[1024];
   int ac;
   int duration_value;
   int i, j;
   Arg arg[10];
   char  duration[50];
   char* valstr;
   time_t Time;
   char   TimeString[100];
   char   final_time_string [ ANSI_YEARSEC_TIME_LEN + 1 ] = {'\0'};
   int    returnvalue;
   double  dlat,dlon;
   char    name[ALLOWED_NAME_LEN + 1];
   time_t  time_in_ticks;
   
   typedef struct lidnode *LidPtr;	
   typedef struct lidnode
   {
      LidPtr next;
      char   lid[LOC_ID_LEN + 1];
   } LidNode;
   
   LidPtr lidHead;
   LidPtr lidPtr;
   LidPtr lidPtrTmp;
   int    LidFound;
   
   
   /*  Set watch cursor */
   
   SetCursor(latestrepFO, XC_watch);
   
   
   /* Initialize lid linked-list */
   
   lidHead = lidPtr = NULL;
   
   
   /* Free any memory allocated to database routines */
   
   free_dbgen();  
   
   
   /*	Query LatestObsValue */
   
   if (LIST_ALL == list_type)
   {
      sprintf(where, " ");
   }
   
   else if (LIST_DURATION == list_type)
   {
      Time = time(0);
      strftime(TimeString, 95, "%Y-%m-%d %H:%M:%S", gmtime(&Time));	
      valstr = XmTextGetString(durationTX);
      strncpy(duration, valstr, 45);
      duration [ 45 ] = '\0' ;
      XtFree(valstr);

      /* 06/03/2005 - Added logic to replace the use of INTERVAL. */
      /* The original query: */
      /* sprintf(where, " WHERE lid NOT IN "
	                " (SELECT DISTINCT lid FROM latestobsvalue "
	                "   WHERE obstime > ('%s' - "
		        " INTERVAL '%s HOUR')  ) ",
	                TimeString, duration);    */

      /* Convert the timestring to a time_t value. */  
      yearsec_ansi_to_timet ( TimeString, & time_in_ticks ); 
      duration_value = atoi ( duration );

      /* Subtract the duration from the time in ticks. */
      time_in_ticks -= ( duration_value * SECONDS_PER_HOUR ); 

      /* Convert the ticks time back to an ansi time string. */
      timet_to_yearsec_ansi ( time_in_ticks, final_time_string );

      /* Build the query ... */
      sprintf ( where, " WHERE lid NOT IN "
	               " ( SELECT DISTINCT lid FROM latestobsvalue "
	               "   WHERE obstime > '%s') ",
	               final_time_string );
   }
   
   else if (LIST_NEVER == list_type)
   {
      sprintf(where, " WHERE lid = 'X-N/A-X' "
	      "UNION "
	      "SELECT lid, "
	      "  '', 0, '', '', "
	      "  '1-1-1 00:00:00', "
	      " 0.0, 0, '', 0,'', "
	      "  '1-1-1 00:00:00', "
	      "  '1-1-1 00:00:00' "
	      "  FROM location "
	      " WHERE lid NOT IN (SELECT DISTINCT lid FROM latestobsvalue) ");
   }
   
   
   /* define the sort order */
   
   if (LOCATION == sort_type)
      strcat(where, "ORDER BY 1 ASC, 6 DESC");
   
   else if (TIME == sort_type)
      strcat(where, "ORDER BY 6 DESC");
      
   latestobsvalueHead = GetLatestObsValue(where);
   
   
   printf("where=%s\n", where);
   cntIn = ListCount(&latestobsvalueHead->list);
   printf("cnt=%ld\n", cntIn);
   
   
   /* find the number of unique stations for the list for later
      use in allocating the X list. */
   
   if (latestobsvalueHead != NULL)
   {
      records_found = 1;
      cntIn = ListCount(&latestobsvalueHead->list);
      latestobsvaluePtr = 
	 (LatestObsValue *) ListFirst(&latestobsvalueHead->list);
      cntOut = 0;
      
      for (i = 0; i < cntIn; i++)
      {
	 /* See if this is a new lid */
	 lidPtrTmp = lidHead;
	 LidFound = 0;
	 while(NULL != lidPtrTmp) {
	    if (0 == strcmp(lidPtrTmp->lid, latestobsvaluePtr->lid)) {
	       LidFound = 1;
	       break;
	    }
	    lidPtrTmp = lidPtrTmp->next;
	 }
	 
	 /* If not found, add to list */
	 if (0 == LidFound)
	 {
	    cntOut++;
	    
	    if (NULL == lidPtr)
	    {
	       lidPtr = (LidPtr) malloc(sizeof(LidNode));
	       lidPtr->next = NULL;
	       strcpy(lidPtr->lid, latestobsvaluePtr->lid);
	       lidHead = lidPtr;
	    }
	    else
	    {
	       lidPtrTmp = (LidPtr) malloc(sizeof(LidNode));
	       lidPtrTmp->next = NULL;
	       
	       lidPtr->next = lidPtrTmp;
	       strcpy(lidPtrTmp->lid, latestobsvaluePtr->lid);
	       lidPtr = lidPtrTmp;
	    }
	 }
	 
	 /* Position to next record */
	 
	 latestobsvaluePtr = (LatestObsValue *) ListNext(&latestobsvaluePtr->node);
      }
   }
   else 
   {
      records_found = 0;
      cntIn = 1;
      cntOut = 1;
   }
   
   /*	Free memory for lid linked-list and initialize */
   
   lidPtr = lidHead;
   while (NULL != lidPtr) 
   {
      lidPtrTmp = lidPtr->next;
      free(lidPtr);
      lidPtr = lidPtrTmp;
   }
   lidHead = lidPtr = NULL;
   
   
   /* allocate the Motif list strings */
   
   xmStr = (XmStringTable) XtMalloc(cntOut * sizeof(XmString *));
   
   
   /* load the strings into the Motif strings */
   
   if (latestobsvalueHead == NULL)
   {
      sprintf(liststr, "No data found");
      xmStr[0] = XmStringCreateSimple(liststr);
   }
   else
   {
      latestobsvaluePtr = 
	 (LatestObsValue *) ListFirst(&latestobsvalueHead->list);
      j = 0;
      for (i = 0; i < cntIn; i++)
      {
	 /* Make sure this lid has not been seen yet */
	 /* See if this is a new lid */
	 
	 lidPtrTmp = lidHead;
	 LidFound = 0;
	 while(NULL != lidPtrTmp)
	 {
	    if (0 == strcmp(lidPtrTmp->lid, latestobsvaluePtr->lid))
	    {
	       LidFound = 1;
	       break;
	    }
	    lidPtrTmp = lidPtrTmp->next;
	 }
	 
	 /* Insert if not a new lid:
	    check if the special date of Jan 1... is used (see above) */
	 
	 if (0 == LidFound)
	 {
	    yearsec_dt_to_ansi(latestobsvaluePtr->postingtime, 
			       ansi_postingtime);
	    if (0 == strcmp("2001-01-01 00:00:00", ansi_postingtime))
	    {
	       strcpy(ansi_postingtime, "N/A");
	    }
	    
	    
	    yearsec_dt_to_ansi(latestobsvaluePtr->obstime, 
			       ansi_obstime);
	    if (0 == strcmp("2001-01-01 00:00:00", ansi_obstime))
	    {
	       strcpy(ansi_obstime, "N/A");
	    }
	    
	    /*get name for the lid*/
	    
	    if (latestobsvaluePtr->lid != NULL)
	      returnvalue = get_loc_info(latestobsvaluePtr->lid,
	                                 &dlat,&dlon,name);
					 
	    sprintf(liststr,"%-8s %-20s %-20s"		   		
		    "                "
		    "%-20s",
		    latestobsvaluePtr->lid,name,
		    ansi_obstime,
		    ansi_postingtime
		    );
	    xmStr[j++] = XmStringCreateSimple(liststr);
	    
	    
	    /* Add to linked-list so we don't print this lid again */
	    
	    if (NULL == lidPtr) 
	    {
	       lidPtr = (LidPtr) malloc(sizeof(LidNode));
	       lidPtr->next = NULL;
	       strcpy(lidPtr->lid, latestobsvaluePtr->lid);
	       lidHead = lidPtr;
	    }
	    else 
	    {
	       lidPtrTmp = (LidPtr) malloc(sizeof(LidNode));
	       lidPtrTmp->next = NULL;
	       
	       lidPtr->next = lidPtrTmp;
	       strcpy(lidPtrTmp->lid, latestobsvaluePtr->lid);
	       lidPtr = lidPtrTmp;
	    }
	 }
	 latestobsvaluePtr = (LatestObsValue *) ListNext(&latestobsvaluePtr->node);
      }
   }
   
   /* load the list in the scrolled list */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cntOut);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(latestlistLS, arg, ac);
   
   
   /* Free the memory for list widget */
   
   for (i = 0; i < cntOut; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);
   if (latestobsvalueHead != NULL) 
   {
      free(latestobsvalueHead);
      latestobsvalueHead = (LatestObsValue *)NULL;
   }
   
   
   /* Free memory for lid linked-list */
   
   lidPtr = lidHead;
   while (NULL != lidPtr) 
   {
      lidPtrTmp = lidPtr->next;
      free(lidPtr);
      lidPtr = lidPtrTmp;
   }
   
   /* Select first item in list if any data was found */
   
   if (1 == records_found) 
   {     
      XmListSetPos(latestlistLS, 1);
      XmListSelectPos(latestlistLS,   1, 1);
   }
   
   /*	Restore cursor */
   UnsetCursor(latestrepFO);
   
   return;
}


/************************************************************************
   
   Fill bottom listbox.
   
   **********************************************************************/
void fill_bottom_listbox(char* parent_lid)
{
   LatestObsValue *latestobsvaluePtr;
   char where[220];
   XmStringTable xmStr;
   long cnt;
   char ansi_producttime[ANSI_TIME_LEN+1];
   char ansi_postingtime[ANSI_TIME_LEN+1];
   char ansi_obstime[ANSI_TIME_LEN+1];
   char	rev_str[2], qc_str[MAXLEN_QCSYMBOL];
   char liststr[250];
   int ac;
   int i;
   Arg arg[10];
   
   
   /* Set watch cursor */
   
   SetCursor(latestrepFO, XC_watch);
   
   
   /* Free any memory allocated to database routines */
   
   free_dbgen();  
   
   
   /* Query LatestObsValue */
   
   sprintf(where, " WHERE lid = '%s' "
	   " ORDER BY obstime DESC, pe DESC, ts DESC, dur DESC, extremum DESC",
	   parent_lid);
   latestobsvalueHead = GetLatestObsValue(where);
   
   
   /* Load "latestlistLS" */
   
   if (latestobsvalueHead != NULL) 
      cnt = ListCount(&latestobsvalueHead->list);

   else 
      cnt = 1;

   
   /* allocate the Motif list strings */
   
   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
   
   
   /* load the strings into the Motif strings */
   
   if (latestobsvalueHead == NULL) 
   {
      liststr[0]='\0';
      xmStr[0] = XmStringCreateSimple(liststr);
   }
   
   else 
   {
      latestobsvaluePtr = 
	 (LatestObsValue *) ListFirst(&latestobsvalueHead->list);
      for (i = 0; i < cnt; i++) 
      {
	 
	 /* first translate the times to strings in the form
	    of yyyy-dd-mm hh:mm:ss. then remove the seconds portion */
	 
	 yearsec_dt_to_ansi(latestobsvaluePtr->obstime,     ansi_obstime);
	 yearsec_dt_to_ansi(latestobsvaluePtr->producttime, ansi_producttime);
	 yearsec_dt_to_ansi(latestobsvaluePtr->postingtime, ansi_postingtime);
	 
	 memset(&ansi_obstime[16],     0, 1);
	 memset(&ansi_postingtime[16], 0, 1);
	 memset(&ansi_producttime[16], 0, 1);
	 
	 
	 /* translate the revision code and the internal quality code */
	 
	 if (latestobsvaluePtr->revision == 0)
	    strcpy(rev_str, "F");
	 else
	    strcpy(rev_str, "T");
	 
        /************************************************************************
          Special handling of the quality control codes is done via calls to the 
          check_qccode() function.   
         ************************************************************************/

         build_qc_symbol(latestobsvaluePtr->quality_code, qc_str);


	 sprintf(liststr, "%8s  %2s %4d %2s %1s   %16s %10.2f   "
		 "%1s  %1s  %1s   %10s %16s   %16s",
		 latestobsvaluePtr->lid,
		 latestobsvaluePtr->pe,
		 latestobsvaluePtr->dur,
		 latestobsvaluePtr->ts,
		 latestobsvaluePtr->extremum,
		 ansi_obstime,
		 latestobsvaluePtr->value,
		 latestobsvaluePtr->shef_qual_code,
		 qc_str,
		 rev_str,
		 latestobsvaluePtr->product_id,
		 ansi_producttime,
		 ansi_postingtime);
	 
	 xmStr[i] = XmStringCreateSimple(liststr);
	 
	 latestobsvaluePtr = (LatestObsValue *) ListNext(&latestobsvaluePtr->node);
      }
   }
   
   
   /* load the list in the scrolled list */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(latestlocLS, arg, ac);
   
   
   /* free the memory */
   
   for (i = 0; i < cnt; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);
   
   if (latestobsvalueHead != NULL) 
   {
      free(latestobsvalueHead);
      latestobsvalueHead = (LatestObsValue *)NULL;
   }
   
   
   /* Restore cursor */
   
   UnsetCursor(latestrepFO);
   
   return;
}


/************************************************************************
   
   Pushbutton call-backs
   
   **********************************************************************/
void sort_locationCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   sort_type = LOCATION;
   fill_top_listbox();
   
   return;
}

void sort_timeCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   sort_type = TIME;
   
   fill_top_listbox();
   
   return;
}


void filter_allCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   list_type = LIST_ALL;
   
   XtSetSensitive(durationTX, False);
   XtSetSensitive(hoursLB,    False);
   
   XtSetSensitive(sort_timePB, True);
   
   fill_top_listbox();
   
   return;
}

void filter_olderthanCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   list_type = LIST_DURATION;
   
   XtSetSensitive(durationTX, True);
   XtSetSensitive(hoursLB,    True); 
   
   XtSetSensitive(sort_timePB, True);
   
   fill_top_listbox();
   
   return;
}

void filter_neverCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   list_type = LIST_NEVER;
   
   XtSetSensitive(durationTX, False);
   XtSetSensitive(hoursLB,    False); 
   
   SetMenuPos(latest_sortOM, 0);
   XtSetSensitive(sort_timePB, False);
   
   fill_top_listbox();
   
   return;
}


/************************************************************************
   
   Top listbox call-back
   
   **********************************************************************/
void latestlistCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   /* Used to isolate values from listbox */
   char  lid[15];
   char  dummy[300];
   
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
   char* choice;
   char  where[300];
   
   Dcp* dcpPtr;
   Telem* telemPtr;
   
   if (1 == records_found) 
   {
      
      /* Get choice */
      
      XmStringGetLtoR(cbs->item, XmFONTLIST_DEFAULT_TAG, &choice);
      sscanf(choice, "%s %s", lid, dummy);
      XtFree(choice);
      
      /* Get info from DCP table */
      
      free_dbgen();  
      
      sprintf(where, " WHERE lid = '%s' ", lid);
      dcpHead = GetDcp(where);
      
      if (dcpHead != NULL) 
      {
	 dcpPtr = (Dcp *) ListFirst(&dcpHead->list);
	 XmTextSetString(dcpfreqTX, dcpPtr->rptfreq);
	 XmTextSetString(dcprepTX, dcpPtr->rptime);
      }
      else 
      {
	 XmTextSetString(dcpfreqTX, "N/A");
	 XmTextSetString(dcprepTX, "N/A");
      }
      
      if (dcpHead != NULL) 
      {
	 free(dcpHead);
	 dcpHead = (Dcp *)NULL;
      }
      
      
      /* Get info from Telem table */
      
      free_dbgen();  
      
      sprintf(where, " WHERE lid = '%s' ", lid);
      telemHead = GetTelem(where);
      
      if (telemHead != NULL) 
      {
	 telemPtr = (Telem *) ListFirst(&telemHead->list);
	 XmTextSetString(telemfreqTX, telemPtr->rptfreq);
      }
      else 
      {
	 XmTextSetString(telemfreqTX, "N/A");
      }
      
      if (telemHead != NULL) 
      {
	 free(telemHead);
	 telemHead = (Telem *)NULL;
      }
      
      /* Fill bottom listbox */
      
      fill_bottom_listbox(lid);
      
   }
   
   
   /* start "clock" */
   
   if (0 == timer_pending) 
   {
      timer_id = XtAppAddTimeOut(XtWidgetToApplicationContext(latestrepFO),
				 1L, currenttimeTimer, NULL);
      timer_pending = 1;
   }
   
   
   return;
}


/************************************************************************
   
   Bottom listbox call-back
   
   **********************************************************************/
void latestlocCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   XmListDeselectAllItems(w);
   
   return;   
}


/************************************************************************
   
   Duration textbox call-back
   
   **********************************************************************/
void durationCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   
   char  duration[50];
   char* valstr;
   char* p;
   long  lduration;
   
   /* get duration */
   
   valstr = XmTextGetString(durationTX);
   strncpy(duration, valstr, 45);
   XtFree(valstr);
   
   /* make sure that a valid # days is supplied */
   
   p = duration;
   while (0 != *p) 
   {
      if (!isdigit(*p)) 
      {
	 XmTextSetString(durationTX, DURATION_DEFAULT);
	 return;
      }
      p++;
   }
   
   sscanf(duration, "%ld", &lduration);
   if ((lduration < DURATION_MIN) || (lduration > DURATION_MAX)) 
   {
      XmTextSetString(durationTX, DURATION_DEFAULT);
      return;
   }
   fill_top_listbox();
   
   return;   
}


/************************************************************************
   
   Timer call-back
   
   **********************************************************************/
void currenttimeTimer(XtPointer client_data, XtIntervalId *id)
{
   time_t Time;
   char   TimeString[100];
   
   timer_pending = 0;
   
   Time = time(0);
   strftime(TimeString, 95, "%Y-%m-%d %H:%M:%S", gmtime(&Time));	
   
   XmTextSetString(curtimeTX, TimeString);
   
   /* start "clock" */
   /*
   timer_id = XtAppAddTimeOut(XtWidgetToApplicationContext(latestrepFO),
   1000L, currenttimeTimer, NULL);
   timer_pending = 1;
   */
   
   return;
}


/************************************************************************
   
   Close the window.
   
   **********************************************************************/

void ok_latestrepCB()
{
   /* remove timer */
   
   if (1 == timer_pending) 
   {
      XtRemoveTimeOut(timer_id);
      timer_pending = 0;
   }
   
   /* unmanage everything */
   
   XtDestroyWidget(latestrepDS);
   latestrepDS = NULL;
   
   /*
   XtUnmanageChild(latestrepFO);
   XtUnmanageChild(latestrepDS);
   */
   
   /* free any allocated memory */
   
   free_dbgen();
   
   return;
}



/************************************************************************
   
   Free any memory allocated for the data.
   
   **********************************************************************/

void free_dbgen()
{
   if (latestobsvalueHead != NULL)
   {
      free(latestobsvalueHead);
      latestobsvalueHead = (LatestObsValue *)NULL;
   }
   
   if (dcpHead != NULL) 
   {
      free(dcpHead);
      dcpHead = (Dcp *)NULL;
   }
   
   if (telemHead != NULL) 
   {
      free(telemHead);
      telemHead = (Telem *)NULL;
   }
   
   return;
}


/************************************************************************
   
   Add the stations selection callbacks.
   
   ************************************************************************/

void add_latestrep_cbs()
{
   
   Atom	wmAtom;
   
   /* callbacks on atom widget & OK button */ 
   
   wmAtom = XmInternAtom(XtDisplay(latestrepDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(latestrepDS, wmAtom, ok_latestrepCB, NULL);
   
   XtAddCallback(latestrep_okPB, XmNactivateCallback, ok_latestrepCB, NULL);
   
   /* callbacks on scrolled list of products */
   XtAddCallback(latestlistLS, XmNdefaultActionCallback,   latestlistCB,
		 NULL);
   XtAddCallback(latestlistLS, XmNbrowseSelectionCallback, latestlistCB,
		 NULL);
   XtAddCallback(latestlocLS, XmNdefaultActionCallback,   latestlocCB,
		 NULL);
   XtAddCallback(latestlocLS, XmNbrowseSelectionCallback, latestlocCB,
		 NULL);
   
   /* start "clock" */
   
   timer_id = XtAppAddTimeOut(XtWidgetToApplicationContext(latestrepFO),
			      1000L, currenttimeTimer, NULL);
   timer_pending = 1;
   
   
   /* callbacks for sort option menu */
   
   XtAddCallback(sort_locationPB, XmNactivateCallback, 
   		 sort_locationCB, NULL);
   XtAddCallback(sort_timePB, XmNactivateCallback, 
   		 sort_timeCB, NULL);
   
   
   /* callbacks for list option menu */
   
   XtAddCallback(filter_allPB, XmNactivateCallback, 
   		 filter_allCB, NULL);
   XtAddCallback(filter_olderthanPB, XmNactivateCallback, 
   		 filter_olderthanCB, NULL);
   XtAddCallback(filter_neverPB, XmNactivateCallback, 
   		 filter_neverCB, NULL);
   
   
   /* callback for days textbox */
   
   XtAddCallback(durationTX, XmNlosingFocusCallback, durationCB, NULL);
   XtAddCallback(durationTX, XmNactivateCallback, durationCB, NULL);
   
   return;
}



