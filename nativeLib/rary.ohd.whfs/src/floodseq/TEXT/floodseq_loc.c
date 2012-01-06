#include <sqlca.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <sqlca.h>

#include "floodseq.h"
#include "QualityCode.h"

/****************************************************************
   
   floodseq_loc
   Process a single station for the flood sequence function.
   
   An sql query to help see which stations have multiple entries:
   
select unique(lid||pe) from ingestfilter
where pe like 'H%' and pe !='HI' and ts like 'R%'
and ingest='T' and lid in

(select lid from ingestfilter
where pe like 'H%' and pe !='HI' and 
ts like 'R%' and ingest='T'
group by 1 having count(*) > 1)

and lid in (select lid from riverstat
where fs > 0.0 and fs is NOT NULL)

this will still require human review as the output will 
include stations with multiple ts entries.

      
   **************************************************************/
void floodseq_loc(char *lid,
		  int	debug_mode)
{
   char   	where[300], pe_str[40];
   char         qc_where_subclause[40]; 
   int	  	return_code, status;
   double	fldstg;
   int      	inserted_floodts_entry;
   long		interval_hours;	
   time_t   	height_obstimet;
   int		suspect_fldstg_msg, sequence_number_used;
   long		sequence_number;
   char		pe[SHEF_PE_LEN + 1];
      
   FloodTs* 	floodtsHead;	
   char     	floodts_obstime[ANSI_TIME_LEN+1];
   time_t     	last_floodts_time;
      
   FloodTs  	newFloodTs;
   
   double   	value_previous;	
   dtime_t  	obstime_previous;
      
   Height* 	heightHead;
   Height* 	heightPtr;

   bool         isUnique;
   
   
   /* ---------------------------------------------------------------------- */
   
   /* get and check flood-stage and physical element */
   
   get_fldstg(lid, &return_code, &fldstg, pe);
   if (return_code < 0)
      return;
   
   
   /* determine latest catloged flood event
      by getting the latest entry from floodts */
   
   sprintf(where, " WHERE lid = '%s' ORDER BY obstime DESC", lid);	
   floodtsHead = GetFloodTs(where);
   
   
   /* now get the stage data. if floodts data already exists then
      only get the stage data since that time.  this is where the '-1' marker
      is most helpful to avoid re-processing data.  get a specific physical
      element's data if requested.  */  
   
   if (strlen(pe) > 0)
      sprintf(pe_str, " AND pe='%s' ", pe);
   else 
      strcpy(pe_str, " ");

   /****************************************************************
      Special handling of the quality control code (quality_code):
      If an invalid clause is contained in the qc_where_subclause
      array upon build_qc_where() function return, the SQL database 
      query will fail.                                                
    ****************************************************************/

   build_qc_where(QC_NOT_FAILED, qc_where_subclause); 
   

   if (floodtsHead != NULL) 
   {
      status = yearsec_dt_to_ansi(floodtsHead->obstime, floodts_obstime);      
      sprintf(where, " WHERE lid = '%s' %s AND obstime > '%s' "
	      " AND value != %f AND %s ORDER BY obstime ASC ",
	      lid, pe_str, floodts_obstime, MISSING_VALUE, qc_where_subclause);
   }
   
   else
   {	 
      sprintf(where, " WHERE lid = '%s' %s "
	      " AND value != %f AND %s ORDER BY obstime ASC ",
	      lid, pe_str, MISSING_VALUE, qc_where_subclause);
   }
    
   heightHead = GetHeight(where);
   
   if (heightHead == NULL) 
   {
      if (floodtsHead != NULL)
	 printf("  %s ignoring since no data since last run at %s.\n",
	        lid, floodts_obstime);
      else
	 printf("  %s ignoring since no stage data.\n", lid);
	 
      return;
   }      
   
   if (debug_mode)
   {
      printf(" *fldstg=%f fldts=%ld %s\n",
	     fldstg, floodtsHead->flood_event_id, floodts_obstime); 
   }
   
   
   /* determine sequence number to start with and set the
      previous value and time for later use. */
   
   set_initial_info(floodtsHead, heightHead, fldstg,
		    &value_previous,
		    &obstime_previous,
		    &sequence_number);
   
   
   /* delete special -1 entry from the Floodts table */
   
   sprintf(where, " WHERE lid = '%s' AND flood_event_id = -1 ", lid);
   if (DeleteFloodTs(where) != ERR_OK) 
   {
      printf("ERROR %d deleting 'flood_event_id = -1'\n",
	     return_code);
      return;
   }
   
   
   /* note the last floodts time for later use */
   
   if (value_previous != MISSING_VALUE)
      yearsec_dt_to_timet(obstime_previous, &last_floodts_time);
   else      
      last_floodts_time = 0;
   
   
   if (debug_mode)
   {
      status = yearsec_dt_to_ansi(obstime_previous, floodts_obstime);	  
      printf(" *newseq, previousHt= %ld %f %s\n", sequence_number, value_previous,
	     floodts_obstime);
   }
   
   
   /* loop through each matching height record ----------------------------- */
   
   inserted_floodts_entry = 0;
   suspect_fldstg_msg     = 0;
   sequence_number_used   = 0;
   
   heightPtr  = (Height *) ListFirst(&heightHead->list);
   while (heightPtr != NULL)
   {			      
      
      /* skip height entries where obstime same as previous
	 value, due to constraint on floodts table */
      
      yearsec_dt_to_timet(heightPtr->obstime, &height_obstimet);
      if (height_obstimet == last_floodts_time) 
	 goto skip_dup_time;
      
      
      /* print warning if floodstage seems suspect */
      
      if ((suspect_fldstg_msg == 0) && (abs(fldstg - heightPtr->value) > 100.))
      {
	 suspect_fldstg_msg = 1;
	 printf("  %s - large diff between stage (%.2f) and fldstg (%.2f).\n",
		lid, heightPtr->value, fldstg);
      }
      
      
      /* if too much time has passed between this event and the previously
	 noted flood event, then start new event. */
      
      if (last_floodts_time > 0 && sequence_number_used == 1) 
      { 
	 status = yearsec_dt_to_timet(heightPtr->obstime, &height_obstimet);
	 interval_hours = (height_obstimet - last_floodts_time) / 3600 ;
	 
	 if (interval_hours > MAX_INTERVAL_IN_HOURS) 
	 {
	    sequence_number     += 1;
	    sequence_number_used = 0;
	    value_previous       = MISSING_VALUE;
	    status = yearsec_ansi_to_dt(MISSING_TIMESTR, &obstime_previous);
	    
	    printf(" incremented event num; %ld days %ld hrs since last event.\n",
		   (interval_hours/24), interval_hours - ((interval_hours/24)*24));
	 }
      }
      
      
      /* ----------------------------------------------------------- */
      
      /* if the previous value was below flood stage, then
	 we are not currently tracking an event. */
      
      if (debug_mode)
      {
	 printf(" *prev,cur= %f %f\n", value_previous, heightPtr->value);
      }
      
      if (value_previous < fldstg)
      {
	 /* CASE 1: NOT within event, and below fs - do nothing */
	 
	 if (heightPtr->value < fldstg)
	 {
	    ;
	 }
	 
	 
	 /* CASE 2: NOT within event, and above fs - store last event
	    value and also this current event value */
	 
	 else
	 {
	    
	    /* store last event */
	    
	    if (value_previous != MISSING_VALUE) 
	    {
	       strcpy(newFloodTs.lid, lid);
	       newFloodTs.obstime        = obstime_previous;
	       newFloodTs.flood_event_id = sequence_number;
	       newFloodTs.value          = value_previous;
	       
	       return_code = InsertIfUniqueFloodTs(&newFloodTs, &isUnique);
	       if (is_dup(return_code))
	       {
		  /* this is not an error.  This code is reached during
		     a flood event if a single value falls below flood-stage,
		     then rises.  Due to the primary key on floodts, it is not
		     possible to have two records with the same lid/obstime
		     combination.  Therefore the same below stage value can not
		     be the last value for one event AND also be the start
		     value for the immediately following  event. */
	       }
	       
	       else if (return_code != 0)
	       {		  
		  printf("ERROR %d inserting previous value (case 2a)\n",
			 return_code);
		  log_floodts_rec(newFloodTs);		  
	       }
	       
	       else
	       {
		  sequence_number_used   = 1;
		  inserted_floodts_entry = 1;   
		  printf(" inserted case 2a :");
		  log_floodts_rec(newFloodTs);		  
	       }
	    }
	    
	    
	    /* store this event */
	    
	    strcpy(newFloodTs.lid, lid);	    
	    newFloodTs.obstime        = heightPtr->obstime;
	    newFloodTs.flood_event_id = sequence_number;
	    newFloodTs.value          = heightPtr->value;
	    
	    return_code = InsertIfUniqueFloodTs(&newFloodTs, &isUnique);
	    if (is_dup(return_code))
	    {
	       printf(" ignoring dup (case 2b): ");
	       log_floodts_rec(newFloodTs);
	    }
	    else if (return_code != 0) 
	    {
	       printf("ERROR %d inserting (case 2b)\n",
		      return_code);
	       log_floodts_rec(newFloodTs);
	       return;
	    }
	    else
	    {
	       sequence_number_used   = 1;
	       inserted_floodts_entry = 1;   
	       printf(" inserted case 2b :");
	       log_floodts_rec(newFloodTs);		  
	    }
	 }
      }
      
      
      /* if the previous value was above flood stage, then
	 we are in the process of tracking an ongoing event. */
      
      else 
      {	
	 /* CASE 3: within event, and above fs - store value for event */
	 
	 if (heightPtr->value >= fldstg) 
	 {	    
	    strcpy(newFloodTs.lid, lid);
	    newFloodTs.obstime        = heightPtr->obstime;
	    newFloodTs.flood_event_id = sequence_number;
	    newFloodTs.value          = heightPtr->value;
	    
	    return_code = InsertIfUniqueFloodTs(&newFloodTs, &isUnique);
	    if (is_dup(return_code))
	    {
	       printf(" ignoring dup (case 3) :");
	       log_floodts_rec(newFloodTs);
	    }
	    else if (return_code != 0) 
	    {
	       printf("ERROR %d inserting (case 3) :\n",
		      return_code);
	       log_floodts_rec(newFloodTs);		  
	       return;
	    }
	    else
	    {
	       sequence_number_used   = 1;
	       inserted_floodts_entry = 1;
	       printf(" inserted case 3  :");
	       log_floodts_rec(newFloodTs);		  
	    }
	 }
	       
	       
	 /* CASE 4: within event, and below fs - store value for event, 
	    terminate sequence.  increment the sequence number just in
	    case the later height values begin a new event */
	 
	 else
	 {
	    strcpy(newFloodTs.lid, lid);
	    newFloodTs.obstime        = heightPtr->obstime;
	    newFloodTs.flood_event_id = sequence_number;
	    newFloodTs.value          = heightPtr->value;
	    
	    return_code = InsertIfUniqueFloodTs(&newFloodTs, &isUnique);
	    if (is_dup(return_code))
	    {
	       printf(" ignoring dup (case 4) :");
	       log_floodts_rec(newFloodTs);
	    }	    
	    else if (return_code != 0) 
	    {
	       printf("ERROR %d inserting (case 4)\n",
		      return_code);
	       log_floodts_rec(newFloodTs);		  
	       return;
	    }
	    else
	    {
	       inserted_floodts_entry = 1;
	       printf(" inserted case 4  :");
	       log_floodts_rec(newFloodTs);
	       
	       sequence_number++;
	       sequence_number_used = 0;
	    }
			 
	 }
      }
	    
      
      skip_dup_time:
	 ;
      
      
      /* update values for next iteration */
      
      value_previous   = heightPtr->value;      
      obstime_previous = heightPtr->obstime;
      
      
      /* if a floodts record was inserted, note its time for later checks */
      
      if (inserted_floodts_entry)
	 status = yearsec_dt_to_timet(newFloodTs.obstime, &last_floodts_time);
      
      
      /* position to next entry in height table */
      
      heightPtr = (Height*) ListNext(&heightPtr->node);
   }
   
   
   /* after looping through all the stage data, if no new entries were
      made to the floodts info, then store the date of the last
      stage data encountered.  this is for use during the next
      run to indicate what data were available at the time of
      the previous run.  */
   
   if (inserted_floodts_entry == 0)
   {
      strcpy(newFloodTs.lid, lid);
      newFloodTs.obstime        = obstime_previous;
      newFloodTs.flood_event_id = -1;
      newFloodTs.value          = value_previous; 
      
      return_code = InsertIfUniqueFloodTs(&newFloodTs, &isUnique);
      if (return_code != 0)
      {
	 printf("ERROR %d inserting flood_event_id = -1\n",
		return_code);
	 log_floodts_rec(newFloodTs);		  
	 return;
      }
   }
   
   
   /* free memory. it is not freed until here because there
      may some variables that reference pointers in this memory. */
   
   FreeFloodTs(floodtsHead);
   FreeHeight(heightHead);
   
   
   return;      
   }
      

/****************************************************************
   
   set_initial_info()
   
   ***********************************************************/
void set_initial_info(FloodTs 	*floodtsHead,
		      Height	*heightHead,
		      double    fldstg,
		      double 	*valprev,
		      dtime_t	*timeprev,
		      long	*seqnum)
   
{
   double	value_previous;
   dtime_t	obstime_previous;
   long 	sequence_number;
   
   int      	interval_exceeded;
   FloodTs	*floodtsPtr;
   int		status;
   

   
   if (floodtsHead != NULL) 
   {       
      floodtsPtr = (FloodTs*) ListFirst(&floodtsHead->list); 
      
      if (floodtsPtr->flood_event_id == -1)
      {
	 /* there is at least one previous event cataloged in the floodts
	    table.  if the new stage data is much later than the event or
	    if there has not been a run since the last event dropped
	    below flood stage, then start a new sequence. */
	 
	 if (ListNext(&floodtsPtr->node) != NULL) 
	 {
	    floodtsPtr = (FloodTs *) ListNext(&floodtsPtr->node);
	    
	    sequence_number  = floodtsPtr->flood_event_id;	    
	    
	    interval_exceeded = check_interval(heightHead->obstime,
					       floodtsPtr->obstime);
	    
	    if (interval_exceeded || floodtsPtr->value < fldstg)
	    {
	       sequence_number++;
	       value_previous   = MISSING_VALUE;
	       status = yearsec_ansi_to_dt(MISSING_TIMESTR, &obstime_previous);	       

	       if (!interval_exceeded)
	         printf("  incremented event num; previous event over.\n"); 
	    }
	    else
	    {
	       value_previous   = floodtsPtr->value;
	       obstime_previous = floodtsPtr->obstime;
	       printf("  same event num; ongoing event.\n");
	    }
	 }
	 
	 
	 /* there are no events cataloged, only the '-1' marker is in
	    the floodts table. */
	 
	 else 
	 {
	    sequence_number = 1;	    
	    value_previous  = MISSING_VALUE;
	    status = yearsec_ansi_to_dt(MISSING_TIMESTR, &obstime_previous);
	    
	    printf("  same event num; no previous events.\n");
	 }	 
      }
      
      
      /* either still in the middle of an event or the this is the
	 first time the sequencer has been run since the flood 
	 event was marked as having ended. if the new stage data is
	 much later than the flood event or if the previous event
	 was over, then start a new sequence. */
      
      else
      {
	 sequence_number  = floodtsPtr->flood_event_id;	 
	 
	 interval_exceeded = check_interval(heightHead->obstime,
					    floodtsPtr->obstime);
	 
	 if (interval_exceeded || floodtsPtr->value < fldstg)
	 {
	    sequence_number++;
	    value_previous   = MISSING_VALUE;
	    status = yearsec_ansi_to_dt(MISSING_TIMESTR, &obstime_previous);
	    if (!interval_exceeded)
	      printf("  incremented event num; previous event over.\n"); 
	 }
	 else
	 {
	    value_previous   = floodtsPtr->value;
	    obstime_previous = floodtsPtr->obstime;
	    printf("  same event num; ongoing event.\n");
	 }
      }
   }
   
   
   /* no previous flood event has been recorded, so initialize  
      sequence number and previous value info. */
   
   else
   {
      sequence_number = 1;      
      value_previous  = MISSING_VALUE;
      status = yearsec_ansi_to_dt(MISSING_TIMESTR, &obstime_previous);
            
      printf("  same event num; no previous events.\n");
   }

   
   /* load the values to be returned */
   
   *valprev  = value_previous;
   *timeprev = obstime_previous;
   *seqnum   = sequence_number;
   
   
   return;
}
  
      
/****************************************************************
   
   log_floodts_rec
   
   **************************************************************/
void log_floodts_rec(FloodTs ftsRec)
{
   char   	err_time[100];
   int		status;
   
   status = yearsec_dt_to_ansi(ftsRec.obstime, err_time);
   printf("%s|%s|%ld|%.3f|\n",
	  ftsRec.lid, err_time,
	  ftsRec.flood_event_id, ftsRec.value);
   
   return;
}


/****************************************************************
   
   is_dup()
   
   **************************************************************/
int is_dup(int return_code)
{  
   
   if (return_code == -239 || return_code == -268)
      return(1);
   else
      return(0);
   
}


/****************************************************************
   
   check_interval()
   check if too much time has passed between the current
   height value and the previous floodts value.
   
   **************************************************************/
int check_interval(dtime_t	heighttime,
		   dtime_t	floodtime)   
{
   time_t	htime, ftime;
   long 	interval_hours;
   int		exceed_interval;
   int		status;
   
   status = yearsec_dt_to_timet(heighttime, &htime);
   status = yearsec_dt_to_timet(floodtime,  &ftime);
   
   interval_hours = (htime - ftime) / 3600 ;
   
   if (interval_hours > MAX_INTERVAL_IN_HOURS) 
   {
      exceed_interval = 1;
      printf("  incremented event num; %ld days %ld hrs since last event.\n",
	     (interval_hours / 24),
	     interval_hours - ((interval_hours / 24) * 24));
   }
   
   else
      exceed_interval = 0;
   
   return(exceed_interval);
}

