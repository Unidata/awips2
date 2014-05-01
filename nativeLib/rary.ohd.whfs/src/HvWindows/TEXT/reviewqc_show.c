 /*
	Name:		reviewqc_show.c	
	Description:	Support for Questionable and Bad data window
			fka QC review	

*/

/************************************************************************
   
   Functions to handle the setup control of purge data information.
      

   
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

#include "CurPC.h"
#include "CurPP.h"

#include "time_convert.h"

#include "reviewqc.h"
#include "reviewqc_show.h"

#include "Observation.h"

#include "QualityCode.h"
#include "DbmsUtils.h"
#include "Xtools.h"

#include "tsgen_info.h"
#include "TSControl_show.h"
#include "get_loc_info.h"

#include "get_limits.h"

/* variables global to this file */

int	records_found=0;
char	current_lid[9];
char	current_pe[3];
char	current_ts[3];
char	current_ext[2];
int	current_dur=0;
dtime_t	current_obstime;

Observation	*observHead = NULL;



/************************************************************************
   
   Load the purge data dialog shell.
   
************************************************************************/

void reviewqc_show(Widget w, char* lid)
{
   /* initialize values */
   
   records_found = 0;


   /* create the dialog shell and add the callbacks */
   
   create_reviewqcDS(GetTopShell(w));
   add_reviewqc_cbs();      
  
  
   /* manage the windows now before doing the selections */ 
     
   XtManageChild(reviewqcFO);
   XtManageChild(reviewqcDS);
  
  
   /* initially sort by time and Desensitize location */ 
   
   XmToggleButtonSetState(sorttimeTB, 1, 0);
   XmToggleButtonSetState(filterlocationTB, 0, 0);
   XtSetSensitive(listlocTX, False);

   /* initially set obs type menu option to Height */ 
   
   SetMenuPos(obstablesOM, T_HEIGHT);
   
   
   /* load initial lid */
   
   XmTextSetString(listlocTX, lid);
      
      
   /* load the scrolled list of Q & B data */
   
   load_QB_scrolledlist();
      
   return;
}


/************************************************************************
   
   Filter toggle call-back
   
************************************************************************/
void filter_toggleCB(Widget w, XtPointer client_data, XtPointer call_data)
{
	if (XmToggleButtonGetState(filterlocationTB))
	{
		XtSetSensitive(listlocTX, True);

		XmToggleButtonSetState(sortlocationTB, 0, 0);
		XtSetSensitive(sortlocationTB, False);
		XmToggleButtonSetState(sorttimeTB, 1, 0);
		XmToggleButtonSetState(sort_shef_qualTB, 1, 0);
		XmToggleButtonSetState(sort_quality_codeTB, 1, 0);
	}
	else
	{
		XtSetSensitive(listlocTX, False);

		XtSetSensitive(sortlocationTB, True);
	}

	load_QB_scrolledlist();
	
	return;
}


/************************************************************************
   
   Listbox call-back
   
************************************************************************/
void qclistCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   Observation	*obsPtr;
   int		*poslist = NULL;
   int		count;
   int		pos;
   char		current_qc_desc[MAXLEN_QCDESCR];
   char		range_max_str[30];
   char		range_min_str[30];
   char		range_field_str[40];
   DataLimits   *limitsPtr = NULL;
   int	 	limits_found;
	
   if (records_found == 0)
     return;


   /*  Find the selected position in the list widget and go to that position in the linked list. */
	    
   XmListGetSelectedPos(qclistLS, &poslist, &count);
   pos = poslist[0];
        
   if (( obsPtr = (Observation *) ListNth(&observHead->list, pos)));
   {
     strcpy(current_lid, obsPtr->lid);
     strcpy(current_pe, obsPtr->pe);
     current_dur = obsPtr->dur;
     strcpy(current_ts, obsPtr->ts);
     strcpy(current_ext, obsPtr->extremum);
     current_obstime = obsPtr->obstime;
     build_qc_descr(obsPtr->quality_code, current_qc_desc);
     if (check_qcbit(REASONRANGE_QC, obsPtr->quality_code) == FALSE_QC)
     {
       limitsPtr = get_limits(obsPtr->lid, obsPtr->pe, obsPtr->dur, obsPtr->obstime, &limits_found);
       if (limits_found)
       {
          if (limitsPtr->reason_range_max != MISSING_VAL)
             sprintf(range_max_str, "Max : %f", limitsPtr->reason_range_max);
          else
             strcpy(range_max_str, " Max : MISSING VALUE ");

	  if (limitsPtr->reason_range_min != MISSING_VAL)
	     sprintf(range_min_str, "Min : %f", limitsPtr->reason_range_min);
          else
             strcpy(range_min_str, " Min : MISSING VALUE ");
                         
	  sprintf(range_field_str, " <%s  %s> ", range_min_str, range_max_str );
       }
       else
       {
          strcpy(range_field_str, "WARNING: NO DATA LIMITS FOUND!!!");
       }
       strcat(current_qc_desc, range_field_str);
     }
     XmTextSetString(iqdescriptionTE, current_qc_desc);
   }
	
   return;
}


/************************************************************************

   Location ID textbox call-back
   Number of Days textbox call-back
   Table Type option-menu call-back
   Sort toggle call-back
   
************************************************************************/
void update_listCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   load_QB_scrolledlist();
   
   return;
}

/************************************************************************
   
  Build the where clause depending on whether the location id is required
   
************************************************************************/


void build_lid_where_clause( bool filter_by_location, const char * lid, char *lid_sub_clause )
{

  if ( filter_by_location )
  {
	sprintf(lid_sub_clause, " WHERE lid = '%s' AND " , lid);
 
  }
  else
  {
        strcpy(lid_sub_clause, " WHERE ");
  }
        
}


void build_lid_qc_where_subclause( bool filter_by_location, const char * lid, char *lid_qc_subclause )
{

   char lid_sub_clause[100];
   char	qc_where_subclause[MAXLEN_QCWHERE];  /* QC subclause to WHERE clause */

   build_lid_where_clause( filter_by_location, lid, lid_sub_clause );
   build_qc_where(QC_NOT_PASSED, qc_where_subclause);

   sprintf ( lid_qc_subclause, " %s  %s" , lid_sub_clause, qc_where_subclause );

}


/************************************************************************
   
   Load the Questionable abd Bad data scrolled list (qclistLS).
   
************************************************************************/
void load_QB_scrolledlist()
{
   XmStringTable        xmStr;
   Arg                  arg[10];
   int                  ac, i;
   long			cnt;
   char			liststr[200];
   char			days_string[5];

   Observation	*obsPtr;
      
   char	where[BUFSIZ];			/* WHERE clause */
   char	lid_qc_subclause[BUFSIZ/2];  /* QC subclause to WHERE clause */
   char	tablename[30];			/* Name of table to query */
   char	dt_ansi[ANSI_TIME_LEN+1];	/* ANSI version of datetime */
   char	obstime_ansi[17];		/* YYYY-MM-DD HH:MM format obstime */
   char	prodtime_ansi[12];		/* MM/DD HH:MM format prodtime */
   char	posttime_ansi[12];		/* MM/DD HH:MM format posttime */
   char	*valstr=NULL;			/* Used to get string from text widget*/
   char	lid[LOC_ID_LEN+1];		/* Used to store lid read from text widget */
   char	revision_flag;			/* Display T/F for revision 0/1 */
   char	qc_symbol[MAXLEN_QCSYMBOL];	/* Display G/Q/B for quality code */
   int  returnvalue;
   double dlat,dlon;
   char  name[ALLOWED_NAME_LEN +1];
   bool filter_by_location;              /* True if filter_by_location is reqd else false */
   char	filter_time_ansi[ANSI_TIME_LEN+1];	/* ANSI version of current time */
   time_t filter_time= 0;
   int  day_count = 0;
  
 
  /*free any memory allocated to Observation linked list */
	
   free_obs_linkedList();   
  
  /* read number of days from text box to be used in WHERE clause */
	
   if ((valstr = XmTextGetString(daysTX)))
   {
      strcpy(days_string, valstr);
      day_count = atoi(days_string);
      XtFree(valstr);
      valstr=NULL;
   }

  /* Get location ID for WHERE clause */

   if ((valstr = XmTextGetString(listlocTX)))
   {
      strcpy(lid, valstr);
      XtFree(valstr);
      valstr=NULL;
   }
   else
   {
      strcpy(lid,"");
   }

   /* build the quality code part of the WHERE clause */
   /*build_qc_where(QC_NOT_PASSED, qc_where_subclause);*/

   /* Filter using location in WHERE clause */ 
    filter_by_location = XmToggleButtonGetState(filterlocationTB);    
    build_lid_qc_where_subclause(filter_by_location,lid, lid_qc_subclause );


    /* get the current time, go back a number of days and create the ansi string for that timestamp */
    time(&filter_time);
    filter_time = filter_time - ( day_count * SECONDS_PER_DAY );
    timet_to_yearsec_ansi(filter_time, filter_time_ansi);   
    
      	 
    if (XmToggleButtonGetState(sortlocationTB))
    {
        sprintf(where, " %s AND obstime > '%s' ORDER BY lid, obstime DESC", 
			       lid_qc_subclause, filter_time_ansi);
    }
    else if (XmToggleButtonGetState(sort_quality_codeTB))
    {
        sprintf(where, " %s AND obstime > '%s' ORDER BY quality_code DESC, lid, obstime DESC ",
			       lid_qc_subclause, filter_time_ansi);
    }
    else if (XmToggleButtonGetState(sort_shef_qualTB))
    {
        sprintf(where, " %s AND obstime > '%s' ORDER BY shef_qual_code DESC, lid, obstime DESC", 
			       lid_qc_subclause, filter_time_ansi);
    }
    else
    {
        sprintf(where, " %s AND obstime > '%s' ORDER BY obstime DESC, lid ", 
			       lid_qc_subclause, filter_time_ansi);
    }



   	/* Determine table */
	
	switch(GetMenuPos(obstablesOM))
	{
	case T_AGRICULTURAL:
		strcpy(tablename, "Agricultural");
		break;

	case T_DISCHARGE:
		strcpy(tablename, "Discharge");
		break;

	case T_EVAPORATION:
		strcpy(tablename, "Evaporation");
		break;
		
	case T_FISHCOUNT:
		strcpy(tablename, "FishCount");
		break;

	case T_GATEDAM:
		strcpy(tablename, "GateDam");
		break;

	case T_GROUND:
		strcpy(tablename, "Ground");
		break;

	case T_HEIGHT:
		strcpy(tablename, "Height");
		break;

	case T_ICE:
		strcpy(tablename, "Ice");
		break;

	case T_LAKE:
		strcpy(tablename, "Lake");
		break;

	case T_MOISTURE:
		strcpy(tablename, "Moisture");
		break;
		
	case T_POWER:
		strcpy(tablename, "Power");
		break;

	case T_RAWPC:
		strcpy(tablename, "RawPC");
		break;
		
	case T_RAWPP:
		strcpy(tablename, "RawPP");
		break;
		
        case T_RAWPOTHER:
		strcpy(tablename, "RawPother");
		break;					  		

	case T_PRESSURE:
		strcpy(tablename, "Pressure");
		break;

	case T_RADIATION:
		strcpy(tablename, "Radiation");
		break;

	case T_SNOW:
		strcpy(tablename, "Snow");
		break;

	case T_TEMPERATURE:
		strcpy(tablename, "Temperature");
		break;

	case T_WATERQUALITY:
		strcpy(tablename, "WaterQuality");
		break;
		
	case T_WEATHER:
		strcpy(tablename, "Weather");
		break;

	case T_WIND:
		strcpy(tablename, "Wind");
		break;

	case T_YUNIQUE:
		strcpy(tablename, "YUnique");
		break;
	}


	/* set watch cursor */
	
	SetCursor(reviewqcFO, XC_watch);
	
	/* get head of list, and count */
	
	observHead = GetObservation(where, tablename);
   	if (observHead != NULL)
      		cnt = ListCount(&observHead->list);
   	else
      		cnt = 1;


  	/* allocate the Motif list strings */
	
   	xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));


   	/* load the strings into the Motif strings */
	
	if (observHead == NULL)
	{
		if (XmToggleButtonGetState(filterlocationTB))
			sprintf(liststr, "No Questionable or Bad data found "
			"within the past %d days in table '%s' for location '%s'",
			day_count, tablename, lid);
		else
			sprintf(liststr, "No Questionable or Bad data found "
			"within the past %d days in table '%s'", day_count, tablename);

		xmStr[0] = XmStringCreateSimple(liststr);
		records_found = 0;
		XtSetSensitive(review_tablePB, False);
		XtSetSensitive(review_graphPB, False);
		XtSetSensitive(review_setmissingPB, False);
		XtSetSensitive(review_deletePB, False);
		XmTextSetString(iqdescriptionTE, "");

	}
	
	else
	{
		records_found = 1;
		XtSetSensitive(review_tablePB, True);
		XtSetSensitive(review_graphPB, True);
		XtSetSensitive(review_setmissingPB, True);
		XtSetSensitive(review_deletePB, True);

		obsPtr = (Observation *) ListFirst(&observHead->list);
		for (i = 0; i < cnt; i++)
		{
			yearsec_dt_to_ansi(obsPtr->obstime, dt_ansi);
                        strncpy(obstime_ansi, dt_ansi, 16);
                        obstime_ansi[16] = '\0';

                        yearsec_dt_to_ansi(obsPtr->producttime, dt_ansi);
                        strncpy(prodtime_ansi, &dt_ansi[5], 11);
                        prodtime_ansi[11] = '\0';

                        yearsec_dt_to_ansi(obsPtr->postingtime, dt_ansi);
                        strncpy(posttime_ansi, &dt_ansi[5], 11);
                        posttime_ansi[11] = '\0';

			if (obsPtr->revision == 1)
			   revision_flag = 'T';
			else
			   revision_flag = 'F';

			build_qc_symbol(obsPtr->quality_code, qc_symbol);

                        /*get name for the lid*/
			
			if (obsPtr->lid != NULL)
			   returnvalue = get_loc_info(obsPtr->lid, &dlat,&dlon,name);
			   
                        sprintf(liststr, "%-8s %-20s %2s  %4d  %2s  %1s  "
                                         "%8.2f  %16s   %c   %1s   "
                                         "%1s  %10s %11s  %11s",
                                obsPtr->lid, name, obsPtr->pe, obsPtr->dur, obsPtr->ts,
                                obsPtr->extremum,
                                obsPtr->value, obstime_ansi, revision_flag,
                                obsPtr->shef_qual_code,
                                qc_symbol, obsPtr->product_id,
                                prodtime_ansi, posttime_ansi);

      			xmStr[i] = XmStringCreateSimple(liststr);
      			
      			obsPtr = (Observation *) ListNext(&obsPtr->node);
		}
   	}


   	/* load the list in the scrolled list */
	
   	ac = 0;
   	XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   	XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   	XtSetValues(qclistLS, arg, ac);


   	/* free the memory */
	
   	for (i = 0; i < cnt; i++)
      		XmStringFree(xmStr[i]);
   	XtFree((char *)xmStr);


   	/* restore cursor */
	
   	UnsetCursor(reviewqcFO);


	/* Select first item in list */
	
	XmListSetPos(qclistLS, 1);
	XmListSelectPos(qclistLS,   1, 1);

	return;
}


/************************************************************************
   
   Free any memory allocated to Observation linked list.
   
************************************************************************/

void free_obs_linkedList()
{

   if (observHead != NULL)
   {
      FreeObservation(observHead);
      observHead = (Observation *)NULL;
   }

   return;
}


/************************************************************************
   
   Close the window.
   
************************************************************************/

void close_reviewqcCB()
{
   
   XtDestroyWidget(reviewqcDS);
   reviewqcDS = NULL;
   
   free_obs_linkedList();

   return;
}


/*********************************************************************
   invoke_timeseries()
   
   PURPOSE
   Invoke the time series control window.
   
   *********************************************************************/

void invoke_timeseries(Widget w, XtPointer client_data, XtPointer call_data)
{
    time_t 	obstime, begintime, endtime;   
    TSGEN_INFO 	ts_invoke;
    int 	display_mode;
   

    /* get the graph or table choice as passed through the callback.
      this argument is then used below when invoking the time series. */

     display_mode = (int )client_data;
   

    /* convert the obstime from dtime_t to time_t */
    
    yearsec_dt_to_timet(current_obstime, &obstime);


    /* set the begining of the period of record 12 hours previous to
       obstime and the end to 12 hours beyond */
       
    begintime = obstime - (12 * SECONDS_PER_HOUR);
    endtime   = obstime + (12 * SECONDS_PER_HOUR);
      
      
    /* load in the structure that controls the time series invocation */
      
    strcpy(ts_invoke.lid, current_lid);     
      
    ts_invoke.display_mode   = display_mode;
    ts_invoke.group_check    = FALSE;      
    ts_invoke.standalone     = NON_STANDALONE;  
    ts_invoke.pedtse_defined = TRUE;
      
    ts_invoke.Begin_time = begintime;
    ts_invoke.End_time   = endtime;
      
      
    /* load the PEDTSE info structure, for just ONE PEDTSE */
    
    ts_invoke.nitems = 1;
      
    strcpy(ts_invoke.pedtse[0].pe, current_pe);
    strcpy(ts_invoke.pedtse[0].ts, current_ts);
    strcpy(ts_invoke.pedtse[0].extremum, current_ext);
    ts_invoke.pedtse[0].dur = current_dur;
      

    /* call the Timeseries Control Window */
    
    show_TSControlDS(w, ts_invoke);
    
    return;   
}


/************************************************************************
    
   Set data to missing
   
************************************************************************/

void setmissing_reviewqcCB(Widget w, XtPointer ptr, XtPointer cbs)
{ 
     
     char	  where[BUFSIZ];
     char	  *error_message;       
     char	  tablename[TABLE_NAME_LEN];
     
     Observation  *obsPtr = NULL;

     Observation  obsRow;       
     RejectedData rejectedData;

     long	rv;
     int	i;
     int	*poslist = NULL;
     int	count=0, cnt=0;
     int	pos=0;
     time_t     currentTime = 0;
     dtime_t    postingTime;
     float oldValue = 0;
     
     /* Find the selected positions in the list widget. */
     
     XmListGetSelectedPos(qclistLS, &poslist, &count);
     if (count == 0 )
        return;
	
	
     /* set postingtime to current time */
     
     time(&currentTime);
     timet_to_yearsec_dt(currentTime, &postingTime);
               
	       
     /* code to update an observation to MISSING */
 
     cnt = ListCount(&observHead->list);
     if (cnt == 0)
        return;
	
	
     /* use a loop since there may be more than one row selected */
     
     for (i = 0; i < count; i++)
     {
        pos = poslist[i];
	 
	/* get the original data from the entry selected */
	obsPtr = (Observation *) ListNth(&observHead->list, pos);
	
	/* set the update structure with data from the original entry */
	strcpy(obsRow.lid, obsPtr->lid);
        strcpy(obsRow.pe, obsPtr->pe);
        obsRow.dur = obsPtr->dur;
        strcpy(obsRow.ts, obsPtr->ts);
        strcpy(obsRow.extremum, obsPtr->extremum);
        obsRow.postingtime = postingTime;
	strcpy(obsRow.product_id, obsPtr->product_id);
	obsRow.producttime = obsPtr->producttime;
	oldValue = obsPtr->value;
	
	getTableName(obsPtr->pe, obsPtr->ts, tablename);
     
     
        /* set value to MISSING, read time widgets and replace in structure */
	
	obsRow.value = MISSING;
	obsRow.obstime = obsPtr->obstime;
     
     
        /* set the shef_qual_code with a "M" for Manual edit */
	
        strcpy(obsRow.shef_qual_code, "M");	       
        createUpdDelWhereObs(where, &obsRow); 
     
     
        /* already a record with same key do an update */
	
     	set_qccode(QC_MANUAL_PASSED, &obsRow.quality_code);
	obsRow.revision = 1;

	rv = UpdateObservation(&obsRow, where, tablename);
	if (rv != 0) /* error */
	{
	     error_message = DbErrorString(rv);
	     ErrorDialog(tabularDS, error_message);
	     if (error_message)
	     {
	       free(error_message);
	       error_message = NULL;
	     }
        }
	else /* successful update of an observation */
	{
	     setRejectedDataObs(&obsRow, &rejectedData, oldValue);
	       
	     rv = PutRejectedData(&rejectedData);
	     if ( rv != 0)
	     {
	        error_message = DbErrorString(rv);
		    
		ErrorDialog(tabularDS, error_message);
		if (error_message)
		{
		  free(error_message);
		  error_message = NULL;
		}
              }
          } /* successful update of an observation */
       } /* for count */
       
       
     /* redisplay the scrolled list after setting values to MISSING */
     
     load_QB_scrolledlist();
          
     if (poslist)
     {
          free(poslist);
	  poslist = NULL;
     }
	  
     return;
}


/************************************************************************
    
   Delete selected data
   
************************************************************************/

void delete_reviewqcCB(Widget w, XtPointer ptr, XtPointer cbs)
{ 
     
     char	  where[BUFSIZ];
     char	  *error_message;       
     char	  tablename[TABLE_NAME_LEN];
     char	  command[BUFSIZ]; 
     
     Observation  *obsPtr = NULL;
     RejectedData rejectedData;

     long	result;
     int	manResult;
     int	execResult;
     int	i;
     int	*poslist = NULL;
     int	count=0, cnt=0;
     int	pos=0;
      
      
     /* Find the selected positions in the list widget.*/
     
     XmListGetSelectedPos(qclistLS, &poslist, &count);
     if (count == 0 )
        return;
     
     
     /* code to delete selected data */
 
     cnt = ListCount(&observHead->list);
     if (cnt == 0)
        return;
	
	
     /* use a loop since there may be more than one row selected */
     
     for (i = 0; i < count; i++)
     {
        pos = poslist[i];
	
	/* get the original data from the entry selected */
	obsPtr = (Observation *) ListNth(&observHead->list, pos);
	 
	createUpdDelWhereObs(where, obsPtr); 
	getTableName(obsPtr->pe, obsPtr->ts, tablename);
     
	result = DeleteObservation(where, tablename);
	
	if (result != 0) /* error */
	{
	     error_message = DbErrorString(result);
	     ErrorDialog(tabularDS, error_message);
	     if (error_message)
	     {
	       free(error_message);
	       error_message = NULL;
	     }
        }
	else /* successful delete of observation */
	{
	     /* if precip then delete from curprecip table as well  */
             if ((obsPtr->pe[0] == 'P' ) && (obsPtr->pe[1] != 'A' &&
		                             obsPtr->pe[1] != 'D' &&
			                     obsPtr->pe[1] != 'E' &&
                                             obsPtr->pe[1] != 'L'))
	     {
                if ( obsPtr->pe[1] == 'C' )
                {
                   DeleteCurPC ( where );
                }
                else if ( obsPtr->pe[1] == 'P' )
                {
                   DeleteCurPP ( where );
                }
	     }
		
	     /* if height or discharge then calculate new RiverStatus as well */
	     else if ( obsPtr->pe[0] == 'H' || obsPtr->pe[0] == 'Q' )
	     { 
	       sprintf(command, "load_obs_river('%s', '%s', '%s')",
	       obsPtr->lid, obsPtr->pe, obsPtr->ts); 

	       execResult = execFunction(command);
	       if (execResult != 0)
	       {   
		  error_message = DbErrorString(execResult);
		  ErrorDialog(tabularDS, error_message);
		  if (error_message)
		  {
		    free(error_message);
		    error_message = NULL;
		  }   
	       } 
	     }  
	      
	     setRejectedDataObs(obsPtr, &rejectedData, obsPtr->value);
	       
	     manResult = PutRejectedData(&rejectedData);
	     if ( manResult != 0)
	     {
	        error_message = DbErrorString(result);
		    
		ErrorDialog(tabularDS, error_message);
		if (error_message)
		{
		  free(error_message);
		  error_message = NULL;
		}
              }
          } /* successful delete of an observation */
       } /* for count */
       
	     
     /* redisplay the scrolled list after deleting selected values */
     
     load_QB_scrolledlist();
          
     if (poslist)
     {
          free(poslist);
	  poslist = NULL;
     }
	  
     return;
}


/************************************************************************
    
   Add the review QC callbacks.
   
************************************************************************/

void add_reviewqc_cbs()
{
   
   Atom	wmAtom;
   
   
   /* callback for location textbox */
   
   XtAddCallback(listlocTX, XmNlosingFocusCallback, update_listCB, NULL);
   XtAddCallback(listlocTX, XmNactivateCallback,    update_listCB, NULL);
   XtAddCallback(listlocTX, XmNmodifyVerifyCallback,
                (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
   
   
   /* callback for days textbox */
   
   XtAddCallback(daysTX, XmNlosingFocusCallback, update_listCB, NULL);
   XtAddCallback(daysTX, XmNactivateCallback,    update_listCB, NULL);
   XtAddCallback(daysTX, XmNmodifyVerifyCallback,
                (XtCallbackProc)num_filter, (XtPointer)INTEGERS);

   
   /* callbacks for filter toggles */
   
   XtAddCallback(filterlocationTB, XmNvalueChangedCallback, filter_toggleCB, NULL);


   /* callbacks for sort-by radio buttons */
   
   XtAddCallback(sortlocationTB, XmNvalueChangedCallback, update_listCB, NULL);
   XtAddCallback(sort_shef_qualTB, XmNvalueChangedCallback, update_listCB, NULL);
   XtAddCallback(sort_quality_codeTB, XmNvalueChangedCallback, update_listCB, NULL);
   		
   /* callbacks on obs type option-menu */
   
   XtAddCallback(agriculturePB, XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(dischargePB,   XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(evaporationPB, XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(fishcountPB,   XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(gatedamPB,     XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(groundPB,      XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(heightPB,      XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(icePB,         XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(lakePB,        XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(moisturePB,    XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(powerPB,       XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(precipPCPB,    XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(precipPPPB,    XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(precipOPB,     XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(pressurePB,    XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(radiationPB,   XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(snowPB,        XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(temperaturePB, XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(waterqualityPB,XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(weatherPB,     XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(windPB,        XmNactivateCallback, update_listCB, NULL);
   XtAddCallback(yuniquePB,     XmNactivateCallback, update_listCB, NULL);
     
     
   /* callbacks on scrolled list of products */
   
   XtAddCallback(qclistLS, XmNdefaultActionCallback,   qclistCB, NULL);
   XtAddCallback(qclistLS, XmNextendedSelectionCallback, qclistCB, NULL);


   /* callbacks for graphical and tabular Timeseries buttons */
   
   XtAddCallback(review_tablePB, XmNactivateCallback, invoke_timeseries,
		 (XtPointer *)TABULAR_TS);
   XtAddCallback(review_graphPB, XmNactivateCallback, invoke_timeseries,
		 (XtPointer *)GRAPH_TS);


   /* callbacks on atom widget & Close button */ 
     
   wmAtom = XmInternAtom(XtDisplay(reviewqcDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(reviewqcDS, wmAtom,        close_reviewqcCB, NULL);
   XtAddCallback(review_closePB, XmNactivateCallback, close_reviewqcCB, NULL);
   
   
   /* callbacks for Set Missing and Delete buttons */
   
   XtAddCallback(review_setmissingPB, XmNactivateCallback, setmissing_reviewqcCB, NULL);
   XtAddCallback(review_deletePB, XmNactivateCallback, delete_reviewqcCB, NULL);
   
   
   return;
}

