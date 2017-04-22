/*********************************************************************
   process_vtecinfo.c 
   
   init_vtecinfo()
   init_vtecevent()
   
   load_prevevent_info()
   rec_vtecproduct()
   
   load_vtecinfolines()   
   load_vtecinfo_1stline()
   load_vtecinfo_2ndline()
   
   get_previous_event()
   get_previous_inactive_event()
   
   load_previous_event()
   copy_previous_event()
   
   check_if_event_active()
   
   compute_new_etn()
   compute_vtec_severity()
   compute_vtec_record()

   create_vteclines()
   
   save_vtec_events()
   save_corvtec_events()
   check_if_vtec()
   
   format_vtectime()
   format_rtime() 
   
   set_fallbelow_time()
   
   
   *******************************************************************/

#include <stdio.h>             /* standard io library functions */
#include <string.h>            /* library string functions */
#include <stdlib.h>            /* standard library functions */
#include <ctype.h>             /* for isalpha() */
#include <time.h>

#include "process_vtecinfo.h" 


/*********************************************************************
   init_vtecinfo()
   
   PURPOSE
   This function initializes all the fields in the vtec info structure.

   *******************************************************************/
void init_vtecinfo(int			numfps,
                   pcc_struct           *pcc,
		   vtecinfo_struct	*vtecinfo)
{
   int	i;
   
   
   log_msg("", "Initializing vtec recommendations to undefined...");
     
   for (i = 0; i < numfps; i++)
   {       
      init_vtecevent(pcc, &vtecinfo[i]);            
   }
   
   return;
}

     
/*********************************************************************
   init_vtecevent()
   
   PURPOSE
   This function initializes all the fields for a single event.
   First, it initializes the fields in the P-vtec line.  Some of the
   codes are based on info in the pcc file; all the other fields are
   dependent on the data.  The office identifier is defined by the 
   office currently selected as part of the service backup support
   in RiverPro.
   
   By design, the VTEC previous fields and the VTEC-based recommendation
   info are not initialized.  This info is loaded managed separately.
   
   Also, note the slight use of the pcc product info to initialize this info.
   The pcc info is not loaded until the recommended product is determined.
   
   *******************************************************************/
void init_vtecevent(pcc_struct          *pcc,
		    vtecinfo_struct	*vtecevent)
{
   
   /* flag indicating the state of the info in the structure. */
      
   vtecevent->vtecinfo_state = VTEC_FULLINFO_UNMODIFIED;
      
     
   /* use the pcc info for the value of the vtec mode category (OTEX) */
   
   strcpy(vtecevent->vtec_cat, 
	  convert_index_to_vtecOTEmode(pcc->product.vtec_cat));
	  
	  
   /* P-VTEC line. initialize the info to be a ROU.HY.S event */
	              
   strcpy(vtecevent->action, "ROU");          
   strcpy(vtecevent->phenom, "HY");         
   strcpy(vtecevent->signif, "S");         

   vtecevent->etn       = 0; 
   
   vtecevent->begintime = MISSINGVAL;      
   vtecevent->endtime   = MISSINGVAL; 

      
   /* initialize the fields in the H-VTEC line */
      
   strcpy(vtecevent->severity,    "U");         
   strcpy(vtecevent->immed_cause, "UU");         
   vtecevent->risetime  = MISSINGVAL;      
   vtecevent->cresttime = MISSINGVAL;      
   vtecevent->falltime  = MISSINGVAL;      
   strcpy(vtecevent->record,      "UU"); 
   
   /* the crest value is not displayed in H-VTEC line,
      but it will be used to determine the cresttime */
      
   vtecevent->crest_value = MISSINGVAL;
              
   /*Initialize type source fields in H-VTEC line*/
   
   strcpy(vtecevent->rise_ts,  "");
   strcpy(vtecevent->crest_ts, "");
   strcpy(vtecevent->fall_ts,  "");
   
   vtecevent->expiretime = MISSINGVAL;
   
   return;
}


/*********************************************************************
   load_prevevent_info()
   
   PURPOSE
   Get the previous event info.  This is used mostly for the recommendation 
   algorithm.  For this reason, it primarily is concerned with previous FL.W
   products.  However, the user may later select a pcc setting with a default
   signif of A or Y.  For these cases, the previous FL.A or FL.Y info is
   also needed.  Therefore get all previous products except for Statements.
   
   *******************************************************************/

void load_prevevent_info(int 			numfps,
			 fp_struct 		*fp,
			 misc_struct            *misc,
			 vtecinfo_struct	*vtecinfo)
{
   VTECevent	*eventHead = NULL;
   VTECevent	*preveventPtr = NULL;
   char		msgstr[120], where[500];
   int 		i;
   char         selected_office[HYD_SERV_LEN + 1];
      
   /* display log message */
   
   sprintf(msgstr, "Loading VTEC-based previous event info..."); 
   log_msg("", msgstr);
   printf("%s\n", msgstr); fflush(NULL);
   
   if (misc->selected_office != NULL)
     strcpy(selected_office, misc->selected_office);
     
   
   /* get the prior VTECevent info for later use in determining
      the active status of the proposed event.
      The data are retrieved once now instead of repeatedly inside the later
      looping to minimize database queries.  Note that W,A,Y significance
      events are considered; this results in ROU.HY.S events not being
      considered.  Consider previous events with any productmode (OTEX).  
      The mode setting defined in the susequently chosen default pcc file 
      can be of any value. */
     
   sprintf(where, " WHERE signif != '%s' AND office_id = '%s' "
	   " ORDER BY producttime DESC",  SIGNIF_STATEMENT, selected_office);
	   
   /* Use VTECpractice table if workstation mode is "PRACTICE" */
   
   if (misc->workstation_mode == PRACTICE_MODE)
      eventHead = (VTECevent *)GetVTECpractice(where);		   
   else   
      eventHead = GetVTECevent(where);
   
   /* process each forecast point */
   
   for (i = 0; i < numfps; i++)
   {     
       
      /* for each of the three event significance codes, get the previous
         event info. */
	
      /* first see if there is any previous event. then load previous event 
         info for use later when determining recommended action. this loads
	 missing indicators if no previous info exists. */
      
      
      preveventPtr = get_previous_event(fp[i].id, eventHead,
			                "", SIGNIF_WARNING);
      load_previous_event(preveventPtr, &(vtecinfo[i].prev_flw)); 
      
			        
      preveventPtr = get_previous_event(fp[i].id, eventHead,
					"", SIGNIF_WATCH);                        
      load_previous_event(preveventPtr, &(vtecinfo[i].prev_fla)); 
			  
			  			         
      preveventPtr = get_previous_event(fp[i].id, eventHead,
                                        "", SIGNIF_ADVISORY);                      
      load_previous_event(preveventPtr, &(vtecinfo[i].prev_fly)); 
      
      
      /* load in the previous event which is inactive.  this is needed 
         to determine the time window for which to retrieve the 
	 observed river data, since we do not want to consider data
	 which is associated with the previous event. */
	 
      preveventPtr = get_previous_inactive_event(fp[i].id, eventHead,
			                         "", SIGNIF_WARNING,
						 misc->system_time);
      load_previous_event(preveventPtr, &(vtecinfo[i].inactive_flw));
       
       
      preveventPtr = get_previous_inactive_event(fp[i].id, eventHead,
			                         "", SIGNIF_WATCH,
						 misc->system_time);
      load_previous_event(preveventPtr, &(vtecinfo[i].inactive_fla));
       
       
      preveventPtr = get_previous_inactive_event(fp[i].id, eventHead,
			                         "", SIGNIF_ADVISORY,
						 misc->system_time);
      load_previous_event(preveventPtr, &(vtecinfo[i].inactive_fly)); 
   }
   
   
   /* free memory now that it is not needed. */
   
   if (eventHead != NULL)
      FreeVTECevent(eventHead);
   
   
   return;
}


/*********************************************************************
   rec_vtecproduct()
   
   PURPOSE
   Determines the product recommendations based on VTEC characteristics.
   This functions combines parts of the load_vtecinfo_lines() and
   load_vtecinfo_1stline() functionality, mixed in with checks of river
   category. 
   
   Any changes to this algorithm must be applied in both the
   recommendation determination in this function and in the assignment
   operation of these other functions!!!
   
   The action codes possibly recommended are:
   NEW (if no active event ongoing)
   CON, EXT, CAN, EXP (if active event)
   ROU (if point is in group with point recommended for other reasons)
   This function does not recommend EXB, EXA, COR, UPG
   
   *******************************************************************/

void rec_vtecproduct(int		numfps,
		     fp_struct		*fp,
		     int		numgrps,
		     grp_struct		*grp,
		     misc_struct	*misc,
		     vtecinfo_struct	*vtecinfo)
{
   int 		i, j;
   int		fpindex;
   char		msgstr[200];
   int		active;
   time_t	begintime, endtime;
   int		most_severe_product;
   int		point_included;
   char 	beginstr[VTEC_TIMECODE_LEN + 1] = "";  
   char 	endstr[VTEC_TIMECODE_LEN + 1]   = ""; 
   int		shift_flag;
    
  
   /* set the current time for possible use later */
   
   time(&misc->system_time);
   
   
   /* display log message */
   
   sprintf(msgstr, "Computing full VTEC-based recommendations...");
   log_msg("", msgstr);
   printf("%s\n", msgstr);
    
   
   /* process each forecast point, using the previous event info
      already extracted. */
   
   for (i = 0; i < numfps; i++)
   {                  
      /* the previous event info is already loaded separately. use 
	 this info to determine the active status of any FL.W event. */
      
      if (vtecinfo[i].prev_flw.event_found == TRUE)
	 active = check_if_event_active(vtecinfo[i].prev_flw.action, 
					vtecinfo[i].prev_flw.endtime, 
					misc->system_time);
      else
	 active = FALSE;
      
      
      /* the active state is saved at this point since this is the state used
         in the below recommendations.  we don't want to recompute the active
	 state when displaying the recomm info in the gui since the system
	 clock change may alter the active state, but the recomms are not
	 recomputed, so then they would not correspond. the active field in 
	 this structure is the ongoing value of this, while the prev_active
	 is the field associated with the recomm info based on the previous
	 events... */
      
      vtecinfo[i].prev_flw.active = active;
      
      
      /* define the times based on the pass thru times.
         have special check in case the original rise-above used for 
	 the original begin time has passed out of the data retrieval
	 window, and a rise is noted again for the same event - for this 
	 case we need to ensure that once a (previious) begin time is 
	 missing, then the begin from then on should always be missing.
         shift the ending time as requested via token, to match
	 the ending time actually determined later. */
      
      begintime = fp[i].riseabove_time;
      if (vtecinfo[i].prev_flw.begintime == MISSINGVAL)
         begintime = MISSINGVAL;      
      
      shift_flag = TRUE;
      endtime   = set_fallbelow_time(i, fp, shift_flag);
           
       
      /* if active event, determine recommended followup info */
      
      if (active == TRUE)
      {	    
	 /* recommend cancel if the max of the current obs and max fcst is below
	    minor flood.  this check is used instead of checking if the endtime is
	    before the current time, to avoid the possible case of all data being below
	    flood level due to some data sitation; anyway if the max level
	    is below flood, then it is certain that the endtime is below 
	    flood level. */
	 
	 if (fp[i].omf_cat == NONFLOOD)
	    strcpy(vtecinfo[i].rec_action, "CAN");
	  
	 	 	 
         /* in general, if the proposed begin or endtime is not the same as the
	    previous event's end time, then the action code is EXTended. there 
	    are some exceptions.  by rule, the being time can not be changed
	    once the begin time is reached. */
      
         else
         {	    
	    if ((begintime                      != MISSINGVAL && 
		 vtecinfo[i].prev_flw.begintime != MISSINGVAL) &&
                (begintime != vtecinfo[i].prev_flw.begintime &&
		 vtecinfo[i].prev_flw.begintime > misc->system_time))
	       strcpy(vtecinfo[i].rec_action, "EXT");	     	            	     
	    
            else if (endtime != vtecinfo[i].prev_flw.endtime)
	       strcpy(vtecinfo[i].rec_action, "EXT");	     	            	     
	    
	    else 		 
	       strcpy(vtecinfo[i].rec_action, "CON");
         }
	    
 	 
	 /* now set the recommended product reason/index; 
	    if categorical rise occurred, then recommend FLW product.
	    if there was a rise, then we know the omf > NONFLOOD,
	    so the action for this active event is either EXT or CON. */
	 
	 if (fp[i].rise_or_fall == RISE)
	 {
	    vtecinfo[i].rec_prod_reason = FLW_INCREASED_FLOODING;
	    vtecinfo[i].rec_prod_index  = FLW;
	 }
	 
	 
	 /* if no rise occurred for an active event, then either flooding
	    is continuing or has ended */
	 
	 else
	 {
	    if (fp[i].omf_cat > NONFLOOD || fp[i].omf_cat == NULLCAT)
	    {
	       vtecinfo[i].rec_prod_reason = FLS_CONTINUED_FLOODING;
	       vtecinfo[i].rec_prod_index  = FLS;
	    }
	    else
	    {
	       vtecinfo[i].rec_prod_reason = FLS_ENDED_FLOODING;
	       vtecinfo[i].rec_prod_index  = FLS;
	    }
	 }	 
      } 
      
      
      /* if forecast point is not in an active event */
      
      else
      {
	 /* if flooding occurring then recommend FLW NEW */
	 
	 if (fp[i].omf_cat > NONFLOOD)
	 {
	    vtecinfo[i].rec_prod_reason = FLW_NEW_FLOODING;
	    vtecinfo[i].rec_prod_index  = FLW;
	    strcpy(vtecinfo[i].rec_action, "NEW");
	 }
	 
	 
	 /* if no flooding, recommend an EXP if event recently ended.
	    otherwise recommend an RVS */ 
	 
	 else if (fp[i].omf_cat == NONFLOOD)
	 {
	    if ((vtecinfo[i].prev_flw.endtime != MISSINGVAL) &&
		(vtecinfo[i].prev_flw.endtime > (misc->system_time - ENDTIME_WITHIN)) &&
		(strcmp(vtecinfo[i].prev_flw.action, "CAN") != 0) &&
		(strcmp(vtecinfo[i].prev_flw.action, "EXP") != 0))
	    {	       
	       vtecinfo[i].rec_prod_reason = FLS_EXPIRED_FLOODING;
	       vtecinfo[i].rec_prod_index  = FLS;
	       strcpy(vtecinfo[i].rec_action, "EXP");	    
	    }
	    else
	    {
	       vtecinfo[i].rec_prod_reason = RVS_NO_FLOODING;
	       vtecinfo[i].rec_prod_index  = RVS;
	       strcpy(vtecinfo[i].rec_action, NO_ACTION);
	    }
	 }
	 
	 else
	 {
	    vtecinfo[i].rec_prod_reason = RVS_NO_DATA;
	    vtecinfo[i].rec_prod_index  = RVS;
	    strcpy(vtecinfo[i].rec_action, NO_ACTION);
	 }
      }
   }  /* end of forecast point loop */
   
   
   /* loop on all forecast points and find the most severe product is the
      recommended product */
   
   most_severe_product = OTHER_PROD;
   
   for (i = 0; i < numfps; i++)
   {
      if (vtecinfo[i].rec_prod_index > most_severe_product)
	 most_severe_product = vtecinfo[i].rec_prod_index;
   }
   
   misc->rec_prod_index = most_severe_product;

   
   /* now that the recommended product is known, include all forecast points
      that match this product, or include all points if a non-FLS/FLW product */
   
   for (i = 0; i < numfps; i++)
   {
      if ((vtecinfo[i].rec_prod_index == most_severe_product) ||
	  most_severe_product <= RVS)
	 misc->rec_fps_included[i] = TRUE;
      else
	 misc->rec_fps_included[i] = FALSE;            
   }
   
  
   /* make another pass on the forecast points by group, to check if a special
      group option is set when the product is an FLS or FLW.  if so, then if
      at least one point in group is included, include all points with data
      in the group that do not have an event recommended. the latter check 
      is needed to ensure that actions associated with an FLW are not 
      placed in an FLS product. */
   
   for (i = 0; i < numgrps; i++)
   {
      if (strcmp(grp[i].rec_allpts_in_group, "Y") == 0 &&
	  misc->rec_prod_index > RVS)
      {
         point_included = FALSE;
	 
         for (j = 0; j < grp[i].numfps; j++)
         {
	    fpindex = grp[i].fpindex[j];
	    
	    if (misc->rec_fps_included[fpindex] == TRUE)
	    {
	       point_included = TRUE;
	       break;
	    }
	 }
	 
	 if (point_included)
	 {
            for (j = 0; j < grp[i].numfps; j++)
            {
	       fpindex = grp[i].fpindex[j];
	       
	       if (strcmp(vtecinfo[fpindex].rec_action, NO_ACTION) == 0 &&
	           vtecinfo[fpindex].rec_prod_reason != RVS_NO_DATA)
	       {
	          misc->rec_fps_included[fpindex] = TRUE;
		  
	          if (misc->rec_prod_index == FLS)
		     vtecinfo[fpindex].rec_prod_reason = FLS_GROUP_IN_FLS;
		  else
		     vtecinfo[fpindex].rec_prod_reason = FLW_GROUP_IN_FLW;
		  
	          vtecinfo[fpindex].rec_prod_index = misc->rec_prod_index;
	          strcpy(vtecinfo[fpindex].rec_action, "ROU");
	       }
	    }
	 }
      }
   }
   
   
   /* filter out any points that do not have data for non-FLS/FLW products */
   
   if (most_severe_product <= RVS)
   {
      for (i = 0; i < numfps; i++)
      {
	 if (fp[i].omf_cat == NULLCAT)
	    misc->rec_fps_included[i] = FALSE;
      }
   }
   
   
   /* determine which groups (i.e. portions thereof) are included
      based upon whether any forecast points in the group are included */
   
   for (i = 0; i < numgrps; i++)
   {
      misc->rec_grps_included[i] = FALSE;
      
      for (j = 0; j < grp[i].numfps; j++)
      {
	 fpindex = grp[i].fpindex[j];
	 
	 if (misc->rec_fps_included[fpindex] == TRUE)
	 {
	    misc->rec_grps_included[i] = TRUE;
	    break;
	 }
      }
   }
   
   
   /* log info on the recomms. log the recs themselves, then the 
      supporting info, including the previous info and the current info. */
   
   sprintf(msgstr, "Fcstpt/grp: rec_action reason");
   log_msg("", msgstr);
   
   sprintf(msgstr, "  previous: active action begin-end");
   log_msg("", msgstr);
   
   sprintf(msgstr, "  current: maxcat rise/fall begin-end");
   log_msg("", msgstr);
      
   for (i = 0; i < numgrps; i++)
   {      
      for (j = 0; j < grp[i].numfps; j++)
      {
	 fpindex = grp[i].fpindex[j];
	 
	 sprintf(msgstr, "%s (%s): %s %s",
		 fp[fpindex].id, grp[i].id,
		 vtecinfo[fpindex].rec_action,
		 convert_prodreason_to_descr(vtecinfo[fpindex].rec_prod_reason));
	 log_msg("", msgstr);


         /* log previous info */
	 
         strcpy(beginstr,
	        format_vtectime(vtecinfo[fpindex].prev_flw.begintime, 
		                misc, VTEC_OTHERTIME));
         strcpy(endstr,
	        format_vtectime(vtecinfo[fpindex].prev_flw.endtime, 
		                misc, VTEC_OTHERTIME));
			       
	 if (vtecinfo[fpindex].prev_flw.event_found)
	 {  
	    sprintf(msgstr, "  previous: %d %s %s-%s",
		    vtecinfo[fpindex].prev_flw.active, 
		    vtecinfo[fpindex].prev_flw.action, 
		    beginstr, endstr);
	    log_msg("", msgstr);
 
	 }
	 else
	    log_msg("", "  previous: not found");
		
		
	 /* log current info */
	 		       
         strcpy(beginstr,
	        format_vtectime(fp[fpindex].riseabove_time, 
		                misc, VTEC_OTHERTIME));
         strcpy(endstr,
	        format_vtectime(fp[fpindex].fallbelow_time, 
		                misc, VTEC_OTHERTIME));
				   
	 sprintf(msgstr, "  current: %s %d %s-%s",
		 convert_catindex_to_name(fp[fpindex].omf_cat),
		 fp[fpindex].rise_or_fall, 
		 beginstr, endstr);
	 log_msg("", msgstr);
      }
   }
   
   return;
}


/*********************************************************************
   load_vtecinfo_lines()
   
   PURPOSE
   Loads the recommended values for both VTEC line.  These represent
   the actual values used for all the VTEC fields, including both
   the P-VTEC and H-VTEC lines.  These recommendation are above and
   beyond the recommendations that were made at program startup and other 
   moments in the program which recommend only parts of the P-VTEC action
   fields (i.e. action, phenom, signif, but not begin and end times). 
  
   Note that the fields are only set if the values previously recommended
   thru this function have not been edited manually.
   
   However, as new (especially curobs and max fcst) data are ingested, 
   this may have an impact on the desired begin and end time in the P-VTEC
   line; the program does not automatically update these fields.  

   *******************************************************************/
void load_vtecinfo_lines(int			numfps, 
			 fp_struct		*fp,
			 int 			numgrps,
			 grp_struct		*grp,
			 int			numcnty,
			 county_struct 		*cnty,
			 pcc_struct		*pcc,
			 misc_struct		*misc,
			 vtecinfo_struct	*vtecinfo)
{
   int 		i;
   char		msgstr[120], where[500];
   int		included_cnt, loaded_cnt;
   VTECevent	*eventHead = NULL;
   
   
   /* set the current time for possible use later */
   
   time(&misc->system_time);
   
   
   /* display log message */
   
   sprintf(msgstr, "Loading full VTEC settings for up to %d forecast points...", 
	   numfps); 
   log_msg("", msgstr);
   printf("%s\n", msgstr); fflush(NULL);
            
   
   /* initialize */
   
   included_cnt = loaded_cnt = 0;
        
   
   /* get the prior VTECevent info for use later in determining the preliminary ETN. 
      The data are retrieved once now instead of repeatedly inside the later
      looping to minimize database queries. Do not filter by productmode
      since the ETN algorithm considers the pool of ETNs for all product modes.
      Note the ROU.HY.S events are not considered. */
             
   sprintf(where, " WHERE signif != '%s' AND office_id = '%s' ",
	   SIGNIF_STATEMENT, misc->selected_office);  
	   
   /* Use VTECpractice table if workstation mode is "PRACTICE" */
   
   if (misc->workstation_mode == PRACTICE_MODE)
      eventHead = (VTECevent *)GetVTECpractice(where);		    
   else   
      eventHead = GetVTECevent(where); 


   /* now load the info for the included areas, in part by using the information
      assembled above. */
   
   for (i = 0; i < numfps; i++)
   {                  
      strcpy(vtecinfo[i].geoid, fp[i].id);
      
      /* only load the info for included points if the data has not been
	 modified by the user. this allows the users edits to be preserved. */
      
      if (misc->fps_included[i] == TRUE)
      {
	 included_cnt++;
	 
	 if (vtecinfo[i].vtecinfo_state == VTEC_FULLINFO_UNMODIFIED)
	 {
	    /* initialize the data */
	    
            init_vtecevent(pcc, &vtecinfo[i]);
	    	    
	    loaded_cnt++;
	    
	    
	    /* load the info into the vtecinfo structure. all the other
	       args are used for supporting information */

	    load_vtecinfo_1stline(fp, grp, cnty, pcc, misc,
				  eventHead, i, numfps, vtecinfo);

	    load_vtecinfo_2ndline(fp, grp, cnty, pcc, misc, i, vtecinfo);
  	 }
      }
   }
      
   
   /* free any memory */

   if (eventHead != NULL)
   {
      FreeVTECevent(eventHead);
      eventHead = NULL;
   }


   /* log a message */

   sprintf(msgstr, "Num forecast points total/included/loaded: %d %d %d...",
	   numfps, included_cnt, loaded_cnt);

   log_msg("", msgstr);
   printf("%s\n", msgstr); fflush(NULL);


   return;
}


/*********************************************************************
   load_vtecinfo_1stline()

   PURPOSE
   Loads the recommended values for the first VTEC line fields.
   This line includes the following fields:
   mode, action, office id, phenom, significance, etn, begintime, endtime
   
   The previous event info for each point is already loaded in the 
   vtecinfo structure.  The full set of previous event info, which 
   is needed for ETN computation, is passed in the "eventHead" variable.
   
   IMPORTANT!!! the algorithm used in this function MUST be kept 
   consistent with the algorithm used for the vtec-based product
   recommendations in rec_vtec_lines().

   *******************************************************************/

void load_vtecinfo_1stline(fp_struct		*fp,
			   grp_struct		*grp,
			   county_struct 	*cnty,
			   pcc_struct		*pcc,
			   misc_struct		*misc,
			   VTECevent		*eventHead,
			   int			fpindex,
			   int			numfps,
			   vtecinfo_struct	*vtecinfo)
{
   int			i;
   int			etn;
   char			geoid[VTEC_GEOID_LEN + 1];
   int			active;
   prev_vtecinfo_struct prev_vtecinfo;
   int			extended_begin;
   int			shift_flag;


   /* set convenient local variables */

   i = fpindex;
   strcpy(geoid, fp[i].id);
   
   extended_begin = FALSE;
   
   
   /* copy the previous event info for the particular significance code so
      the code below can conveniently use the same variable, regardless
      of the significance code in effect.
      it is assumed that the same significance code is in throughout
      the product. the user can manually override it if desired. */
   		      
   if (strcmp(pcc->product.vtec_default_signif, SIGNIF_WARNING) == 0)
      copy_previous_event(vtecinfo[i].prev_flw, &prev_vtecinfo);
      
   else if (strcmp(pcc->product.vtec_default_signif, SIGNIF_WATCH) == 0)
      copy_previous_event(vtecinfo[i].prev_fla, &prev_vtecinfo);
      
   else if (strcmp(pcc->product.vtec_default_signif, SIGNIF_ADVISORY) == 0)
      copy_previous_event(vtecinfo[i].prev_fly, &prev_vtecinfo);
 		       

   /* some fields are not set in this function:
      the vtec mode (i.e. O/T/E) is set for the product as a whole, as 
      defined in the pcc information, and is already initialized.
      the vtec office code is not tracked for each event; it is managed
      within the RiverPro general info, and set when creating the actual
      VTEC lines. */
      
   /* load the begin time and end time. since these values are based on the
      rise above and fall below flood level times, the times may be undefined. 
      based on the action code, these values may be modified later.
      shift the ending time as requested for the P-VTEC endtime. */
   
   vtecinfo[i].begintime = fp[i].riseabove_time; 
   
   shift_flag = TRUE;  
   vtecinfo[i].endtime   = set_fallbelow_time(i, fp, shift_flag);
                  
          
   /* now check if the event is active */
   
   if (prev_vtecinfo.event_found == TRUE)
      active = check_if_event_active(prev_vtecinfo.action,
				     prev_vtecinfo.endtime,
				     misc->system_time);
   else
      active = FALSE;
      
      
   /* if the event is not active, then the action code will be NEW, in which
      case the default signif code will be used.
      if the event is active, then check which specific action code
      should be used, and use the previous signif code regardless of what
      the default signif code is. */
      
   /* check to see if event is CONtinued, EXTtended in time, or CANcelled.
      Since the ids cannot be combined, it is not possible to 
      EXA extend the event in area, or EXB in both time or area, so these
      options are not considered.  A ROUtine action can be recommended if
      under a special case; it is the default action. The options of UPGraded,
      CORrected, and EXPirede must be set interactively; this
      algorithm will not recommend them. */
      
   if (active == TRUE)
   {           
      /* if we are dealing with a FL.W event (not advisory nor watch), then
         assume CANcel if the max level is below flood.  
	 in this case, by rule, the end time is set to the previous end
	 time if it exists;  otherwise it is set to the current time. */
      
      if (fp[i].omf_cat == NONFLOOD &&
          (strcmp(pcc->product.vtec_default_signif, SIGNIF_WARNING) == 0))
      {
	 strcpy(vtecinfo[i].action, "CAN");
	 
	 if (prev_vtecinfo.endtime == MISSINGVAL)
	    vtecinfo[i].endtime = CURRENT_TIME;
	 else
	    vtecinfo[i].endtime = prev_vtecinfo.endtime;
      }
      
      
      /* in general, if the proposed begin or endtime is not the same as the 
         previous event's end time, then the action code is EXTended. */
      
      else
      {	
	 if (strcmp(pcc->product.vtec_default_signif, SIGNIF_WARNING) == 0)
	 {
	    if ((vtecinfo[i].begintime   != MISSINGVAL && 
		 prev_vtecinfo.begintime != MISSINGVAL) &&
		(vtecinfo[i].begintime != prev_vtecinfo.begintime &&
		 prev_vtecinfo.begintime > misc->system_time))
	    {
	       strcpy(vtecinfo[i].action, "EXT");
	       extended_begin = TRUE;
	    }
	    
	    else if (vtecinfo[i].endtime != prev_vtecinfo.endtime)
	       strcpy(vtecinfo[i].action, "EXT");	
	    
	    else 		 
	       strcpy(vtecinfo[i].action, "CON");
	 }
	 
	 
	 /* for advisory and watch events, force to be a CON with the same ending
	    time.  without this special check, there would be an EXT with a blank
	    end time */
	 
	 else
	 {
	    vtecinfo[i].endtime = prev_vtecinfo.endtime;   
	    strcpy(vtecinfo[i].action, "CON");
	 }	 
      }
      
      
      /* set the begin time for any non-new events that are in the past
         to be missing so that a string of zeroes results, as per rule.
	 There is no exception for an EXTended event that occurs because of a change in 
	 the begin time and where the previous begin time is still in the future. */
      
      /*if (vtecinfo[i].begintime <= misc->system_time &&
	  extended_begin == FALSE)
	  vtecinfo[i].begintime = MISSINGVAL;*/
	  
       if (vtecinfo[i].begintime <= misc->system_time) 
       {
          /* EXT event, if the proposed begin time is in the past, set the
	     begin time as current system time */
	     
          if (extended_begin == TRUE)
	     vtecinfo[i].begintime = misc->system_time;
	  else   
	     vtecinfo[i].begintime = MISSINGVAL;
       }	   
      
      
       /* set the begin time for any now-new events that previous event's 
          riseabove time is based on R*, and previous event's begin time is
	  in the past and it is not ended yet */
	 
       if (strcmp(prev_vtecinfo.rise_ts, "R*") == 0 &&
	   prev_vtecinfo.begintime <= misc->system_time &&
	   prev_vtecinfo.endtime   >= misc->system_time)     
	  vtecinfo[i].begintime = MISSINGVAL;
	     
	     
      /* for the non-new events, if the event already started (previous 
         begintime is in the past or previous begintime is missing), then
         the proposed event's begin time can not be changed, should all zero */
       
       if (prev_vtecinfo.begintime <= misc->system_time)
           vtecinfo[i].begintime = MISSINGVAL; 
       	     
      
      /* use the same previous info if the event is active */
      
      strcpy(vtecinfo[i].phenom, prev_vtecinfo.phenom);;
      strcpy(vtecinfo[i].signif, prev_vtecinfo.signif);
      vtecinfo[i].etn =          prev_vtecinfo.etn;      
   } 
   
   
   /* if the previous event is not active, then we have a NEW, EXPired,
      or ROUtine event */
   
   else 
   {           
      /* do ROU.HY.S if for some reason ROU is in pcc file (it shouldn't be). */
      
      if (strcmp(pcc->product.vtec_default_signif, SIGNIF_STATEMENT) == 0)
      {
	 strcpy(vtecinfo[i].action, "ROU"); 
	 strcpy(vtecinfo[i].phenom, "HY");     
         strcpy(vtecinfo[i].signif, "S");
      }
      
      
      /* do FL.x if significance defined as advisorY or wAtch */
      
      else if ((strcmp(pcc->product.vtec_default_signif, SIGNIF_WATCH) == 0) ||
	       (strcmp(pcc->product.vtec_default_signif, SIGNIF_ADVISORY) == 0))
      {
	 strcpy(vtecinfo[i].action, "NEW"); 
	 strcpy(vtecinfo[i].phenom, "FL");
	 strcpy(vtecinfo[i].signif, pcc->product.vtec_default_signif);
      }
      
      
      /* for FL.W, do a NEW event if flooding. if recent non-CAN event, do EXPire, 
	 otherwise do a ROU.HY.S. */
      
      else
      {
	 if (fp[i].omf_cat > NONFLOOD)
	 {	 
	    strcpy(vtecinfo[i].action, "NEW"); 
	    strcpy(vtecinfo[i].phenom, "FL");  
	    strcpy(vtecinfo[i].signif, "W");
	 }
	 
	 else if ((prev_vtecinfo.endtime != MISSINGVAL) &&
		  (prev_vtecinfo.endtime > (misc->system_time - ENDTIME_WITHIN)) &&
		  (strcmp(prev_vtecinfo.action, "CAN") != 0) &&
		  (strcmp(prev_vtecinfo.action, "EXP") != 0))
	 {
	    strcpy(vtecinfo[i].action, "EXP"); 
	    strcpy(vtecinfo[i].phenom, "FL");  
	    strcpy(vtecinfo[i].signif, "W");
	 }
	 
	 else
	 {
	    strcpy(vtecinfo[i].action, "ROU"); 
	    strcpy(vtecinfo[i].phenom, "HY");     
	    strcpy(vtecinfo[i].signif, "S");
	 }
      }
      
      
      /* if the begin time is missing, or if the begin time is in the past,
	 then set it to the current time, assuming it is not a ROU or EXP event 
	 if and ROU, then set begin to missing in case an old rise-above is still 
	 in the obs time series. if an EXP make sure it is missing. */
      
      if (strcmp(vtecinfo[i].action, "ROU") == 0 ||
	  strcmp(vtecinfo[i].action, "EXP") == 0) 
      {
	 vtecinfo[i].begintime = MISSINGVAL;
      }
      
      else
      {
         if (vtecinfo[i].begintime == MISSINGVAL) 
	    vtecinfo[i].begintime = CURRENT_TIME;
	 
         else if (vtecinfo[i].begintime < misc->system_time) 
	    vtecinfo[i].begintime = CURRENT_TIME;
      }
      
      
      /* for EXPired events, set the endtime to match that of the previous event.
	 if ROU or for NEW, if the endtime is in the past, then set it to missing.
	 this can happen if there is a second flood event which does not
	 have a fall-below time, and the first flood event had a rise-above
	 and a fall-below time.  */
      
      if (strcmp(vtecinfo[i].action, "EXP") == 0)
	 vtecinfo[i].endtime = prev_vtecinfo.endtime;
      
      else if ((vtecinfo[i].endtime != MISSINGVAL &&
		vtecinfo[i].endtime < misc->system_time) ||
	       (strcmp(vtecinfo[i].action, "ROU") == 0))
	 vtecinfo[i].endtime = MISSINGVAL;
      
      /* for the new flood advisory, phenom=FL, signif=Y, action=NEW
       The event end time is set to */
      
      if ((strcmp(vtecinfo[i].action, "NEW") == 0 ) &&
          (strcmp(vtecinfo[i].phenom, "FL" ) == 0 ) &&
	  (strcmp(vtecinfo[i].signif, "Y" ) == 0))
          vtecinfo[i].endtime = setFLYendtime(misc);	  
	  
      /* determine the etn for the event. EXPired events reuse the old ETN.
	 ROU events do not get an ETN. */
	 
      if (strcmp(vtecinfo[i].action, "EXP") == 0)
	 etn = prev_vtecinfo.etn;
      else
	 etn = compute_new_etn(i, pcc, misc, numfps, vtecinfo, eventHead);
      
      vtecinfo[i].etn = etn;                 
   }


   return;
}


/*********************************************************************
   load_vtecinfo_2ndline()
   
   PURPOSE
   Sets the recommended values for the (6) H-VTEC (2nd line) fields.
   
   *******************************************************************/

void load_vtecinfo_2ndline(fp_struct		*fp,
			   grp_struct		*grp,
			   county_struct 	*cnty,
			   pcc_struct		*pcc,
			   misc_struct		*misc,
			   int			fpindex,
			   vtecinfo_struct	*vtecinfo)
{
   int			i;
   char			severity_code[VTEC_SEVER_LEN + 1];
   char 		record_code[VTEC_RECORD_LEN + 1];
   prev_vtecinfo_struct prev_vtecinfo;
   int 			max_cat; 
   
   
   /* load a convenient local variable and set the geoid */
   
   i = fpindex;
   
    
   /* copy the previous event info for the particular significance code so
      the code below can conveniently use the same variable, regardless
      of the significance code in effect. */
   
   /* copy_previous_eventOLD(i, vtecinfo, pcc->product.vtec_default_signif,
                       &prev_vtecinfo); */
		       
   if (strcmp(pcc->product.vtec_default_signif, SIGNIF_WARNING) == 0)
      copy_previous_event(vtecinfo[i].prev_flw, &prev_vtecinfo);
      
   else if (strcmp(pcc->product.vtec_default_signif, SIGNIF_WATCH) == 0)
      copy_previous_event(vtecinfo[i].prev_fla, &prev_vtecinfo);
      
   else if (strcmp(pcc->product.vtec_default_signif, SIGNIF_ADVISORY) == 0)
      copy_previous_event(vtecinfo[i].prev_fly, &prev_vtecinfo);
    
    
   /* handle the immediate cause field separate from the other fields.
      if it is a NEW event, load a default value. if a ROU event, set to unknown. 
      otherwise, reuse the previous events cause. */
   
   if (strcmp(vtecinfo[i].action, "NEW") == 0)
      strcpy(vtecinfo[i].immed_cause, "ER");
   
   else if (strcmp(vtecinfo[i].action, "ROU") == 0)
      strcpy(vtecinfo[i].immed_cause, "UU");
   
   else
   {
      if (strlen(prev_vtecinfo.immed_cause) > 0)
         strcpy(vtecinfo[i].immed_cause, prev_vtecinfo.immed_cause);
      else
         strcpy(vtecinfo[i].immed_cause, "ER");
   }
	 
         
   
   /* if the action is ROU, which implies a phenom.signif=>hydro.statement, 
      then do not load anything into the second line, 
      and allow the initial default values to remain. */

   if (strcmp(vtecinfo[i].action, "ROU") == 0)
   {
      strcpy(vtecinfo[i].severity,  "N"); 
      
      vtecinfo[i].risetime  = MISSINGVAL;      
      vtecinfo[i].cresttime = MISSINGVAL;      
      vtecinfo[i].falltime  = MISSINGVAL;
        
      strcpy(vtecinfo[i].record,  "OO"); 
   }
      
   
   /* if generating a watch, advisory, or warning event. */  
   
   else 
   {
     /* if generating a watch event, then use blank type values. */
      
      if (strcmp(vtecinfo[i].signif, SIGNIF_WATCH) == 0)
      {   
	 strcpy(vtecinfo[i].severity,  "0"); 
	 
	 vtecinfo[i].risetime  = MISSINGVAL;      
	 vtecinfo[i].cresttime = MISSINGVAL;      
	 vtecinfo[i].falltime  = MISSINGVAL;
	 
	 strcpy(vtecinfo[i].record,  "OO"); 
      }
      
      /* if generating a advisory event, then use blank type values. */
      
      else if (strcmp(vtecinfo[i].signif, SIGNIF_ADVISORY) == 0)
      {   
	 strcpy(vtecinfo[i].severity,  "N"); 
	 
	 vtecinfo[i].risetime  = MISSINGVAL;      
	 vtecinfo[i].cresttime = MISSINGVAL;      
	 vtecinfo[i].falltime  = MISSINGVAL;
	 
	 strcpy(vtecinfo[i].record,  "OO"); 
      } 
            
      /* if generating a warning event. */
      
      else 
      {  	 		       	 
	 
	 /* we add rise_ts, fall_ts and crest_ts to track whether the 
	    rise/fall/crest is due to obs or fcst. */
	 
	 /* load the rise above time. */ 
		 	 	   
	 /* For NEW event */
	   
	 if (strcmp(vtecinfo[i].action, "NEW") == 0)
	 {	    
	    if (strcmp(fp[i].riseabove_ts, "R*") == 0)
            { 		     
	       vtecinfo[i].risetime = fp[i].obs_riseabove_time;
	       strcpy(vtecinfo[i].rise_ts, "R*");
	    }
	    else if (strcmp(fp[i].riseabove_ts, "F*") == 0)   
	    {
	       vtecinfo[i].risetime = fp[i].fcst_riseabove_time;
	       strcpy(vtecinfo[i].rise_ts, "F*");	      
	    }   
	    else
	    {
	       vtecinfo[i].risetime = MISSINGVAL;   	     
	       strcpy(vtecinfo[i].rise_ts, "");
	    }   
	 }
	 
	 else
	 {  
	    /* For non-NEW actions, check the type source for the fp[].riseabove_ts 
	       and the stored previously issued event's type source 
	       prev_vtecinfo.rise_ts, then determine the event's rise above
	       time is based on forecast or observed data. */
	    
	    /* if current riseabove is based on forecast, the previous event's
	       riseabove was based on forecast, use the current forecast */
	       
	    if ((strcmp(fp[i].riseabove_ts,    "F*") == 0) &&
	        (strcmp(prev_vtecinfo.rise_ts, "F*") == 0))
            {
	       vtecinfo[i].risetime = fp[i].fcst_riseabove_time;
	       strcpy(vtecinfo[i].rise_ts, "F*");
	    }
	    
	    
	    /* if current riseabove is based on obs, the previous event's
	       riseabove was based on forecast, use the current obs */
	       
	    else if ((strcmp(fp[i].riseabove_ts,    "R*") == 0) &&
	             (strcmp(prev_vtecinfo.rise_ts, "F*") == 0))
            {
	       vtecinfo[i].risetime = fp[i].obs_riseabove_time;	
	       strcpy(vtecinfo[i].rise_ts, "R*");	          		 
	    }
	    
	    
	    /* if current there is no riseabove, the previous event's riseabove
	       was based on forecast, use current */
	    
	    else if ((strcmp(fp[i].riseabove_ts,    "") == 0) &&
	             (strcmp(prev_vtecinfo.rise_ts, "F*") == 0))
            {
	       vtecinfo[i].risetime = MISSINGVAL;	
	       strcpy(vtecinfo[i].rise_ts, "");	          		 
	    }   
	    
	           
	    /* if the current riseabove is based on forecast, the previous event's
	        riseabove was based on obs, reuse the previous issued events'
		riseabove if it is not ended*/
		   
	    else if ((strcmp(fp[i].riseabove_ts, "F*") == 0) &&
	             (strcmp(prev_vtecinfo.rise_ts, "R*") == 0))
            {	       	       	   	   
	       vtecinfo[i].risetime = prev_vtecinfo.risetime;
	       strcpy(vtecinfo[i].rise_ts, "R*");	          	  
	    }
	    
	       	   
            /* if the current riseabove is based on obs, the previous event's
	        riseabove was based on obs, reuse the previous issued events'
		riseabove if it is not ended.*/
		   
	    else if ((strcmp(fp[i].riseabove_ts, "R*") == 0) &&
	             (strcmp(prev_vtecinfo.rise_ts, "R*") == 0))
            {	       
	       vtecinfo[i].risetime = prev_vtecinfo.risetime;
	       strcpy(vtecinfo[i].rise_ts, "R*");	       	  
	    }
	    
	    
	    /* if current event has no riseabove time, the previous event's 
	       riseabove was based on obs, reuse the previous issue events's
	       riseabove it is not ended */
	       
	    else if ((strcmp(fp[i].riseabove_ts, "") == 0) &&
	             (strcmp(prev_vtecinfo.rise_ts, "R*") == 0))
            {	       
	       vtecinfo[i].risetime = prev_vtecinfo.risetime;
	       strcpy(vtecinfo[i].rise_ts, "R*");	       	  
	    }
	    
	    
	    /* if current event's riseabove is based on obs, there is no previous'
	       event's riseabove time, use current obs one */
	       
	    else if ((strcmp(fp[i].riseabove_ts, "R*") == 0) &&
	             (strcmp(prev_vtecinfo.rise_ts, "") == 0))
            {	       
	       vtecinfo[i].risetime = fp[i].obs_riseabove_time;
	       strcpy(vtecinfo[i].rise_ts, "R*");	       	  
	    } 
	    
	    
	    /* if current event's riseabove is based on fcst, there is no previous'
	       event's riseabove time, use the current fcst one. */ 
	       
	    else if ((strcmp(fp[i].riseabove_ts, "F*") == 0) &&
	             (strcmp(prev_vtecinfo.rise_ts, "") == 0))
            {	       
	       vtecinfo[i].risetime = fp[i].fcst_riseabove_time;
	       strcpy(vtecinfo[i].rise_ts, "F*");	       	  
	    }        	    		   
	    
	    
	    /* if current's event does not have riseabove and there is no riseabove
	       in the previous event. */
	       
	    else if ((strcmp(fp[i].riseabove_ts, "") == 0) &&
	             (strcmp(prev_vtecinfo.rise_ts, "") == 0))
            {	       
	       vtecinfo[i].risetime = MISSINGVAL;
	       strcpy(vtecinfo[i].rise_ts, "");	       	  
	    } 
	      
	    else
	    {
	       vtecinfo[i].risetime = MISSINGVAL;
	       strcpy(vtecinfo[i].rise_ts, "");
	    }   
	 }
	    
	    
	 /* load crest time and crest_value --------------- */ 
         
	 /* For NEW action */ 
	    
	 if (strcmp(vtecinfo[i].action, "NEW") == 0)
	 {	    
	    if (strcmp(fp[i].crest_ts, "F*") == 0)
            { 		     
	       vtecinfo[i].cresttime   = fp[i].fcstH[fp[i].fcst_max_index].validtime;
	       vtecinfo[i].crest_value = fp[i].fcstH[fp[i].fcst_max_index].value;
	       strcpy(vtecinfo[i].crest_ts, "F*");	       
	    }   
	    else if (strcmp(fp[i].crest_ts, "R*") == 0)
	    {
	       vtecinfo[i].cresttime   = fp[i].obsH[fp[i].obs_max_index].validtime;
	       vtecinfo[i].crest_value = fp[i].obsH[fp[i].obs_max_index].value;
	       strcpy(vtecinfo[i].crest_ts, "R*");	  	       
	    }   
	    else
	    {
	       vtecinfo[i].cresttime   = MISSINGVAL;   	     
	       vtecinfo[i].crest_value = MISSINGVAL;
	       strcpy(vtecinfo[i].crest_ts, "");
	    }   
	 }
	 else
	 {
	    /* For non-NEW actions,
	       check the type source for the fp[].crest_ts and the stored
	       previously issued event's type source prev_vtecinfo.crest_ts,
	       then determine the event's crest time is based on forecast or
	       observed data. */ 
	    
	    /* if the current crest time is based on forecast, the previous 
	       event's cresttime was based on forecast, use current forecast */
	       
	    if ((strcmp(fp[i].crest_ts,         "F*") == 0) &&
	        (strcmp(prev_vtecinfo.crest_ts, "F*") == 0))
            {
	       vtecinfo[i].cresttime   = fp[i].fcstH[fp[i].fcst_max_index].validtime;
	       vtecinfo[i].crest_value = fp[i].fcstH[fp[i].fcst_max_index].value;
	       strcpy(vtecinfo[i].crest_ts, "F*");
	    }
	    
	    
	    /* if the current cresttime is based on obs, the previous event's
	       cresttime was based on forecast, use the current obs */
	       
	    else if ((strcmp(fp[i].crest_ts,         "R*") == 0) &&
	             (strcmp(prev_vtecinfo.crest_ts, "F*") == 0))
            {
	       vtecinfo[i].cresttime   = fp[i].obsH[fp[i].obs_max_index].validtime;
	       vtecinfo[i].crest_value = fp[i].obsH[fp[i].obs_max_index].value;	
	       strcpy(vtecinfo[i].crest_ts, "R*");
	    }
	    
	    
	    /* if there is no current cresttime, the previous event's cresttime
	       was based on forecast, use the current one */
	       
	    else if ((strcmp(fp[i].crest_ts,         "")   == 0) &&
	             (strcmp(prev_vtecinfo.crest_ts, "F*") == 0))
            {
	       vtecinfo[i].cresttime = MISSINGVAL;
	       vtecinfo[i].crest_value = MISSINGVAL;	
	       strcpy(vtecinfo[i].crest_ts, "");
	    }   
	    
	    
	    /* if the current cresttime is based on forecast, the previous event's
	       cresttime was based on obs, compare the previous event's crest value
	       with the current crest value */
	       
	    else if ((strcmp(fp[i].crest_ts,         "F*") == 0) &&
	             (strcmp(prev_vtecinfo.crest_ts, "R*") == 0))
            {
	       if (fp[i].fcstH[fp[i].fcst_max_index].value > prev_vtecinfo.crest_value)
	       {
	          vtecinfo[i].cresttime   = fp[i].fcstH[fp[i].fcst_max_index].validtime;	
		  vtecinfo[i].crest_value = fp[i].fcstH[fp[i].fcst_max_index].value;
	          strcpy(vtecinfo[i].crest_ts, "F*");
	       }
	       else
	       {	          		      	  
		  vtecinfo[i].cresttime   = prev_vtecinfo.cresttime;
		  vtecinfo[i].crest_value = prev_vtecinfo.crest_value;	
		  strcpy(vtecinfo[i].crest_ts, "R*");	        
	       }  
	    }	
	    
	    
	    /* if the current cresttime is based on obs, the previous event's
	       cresttime is based on obs, compare the previous event's crest
	       value with the current crest value */
	       
	    else if ((strcmp(fp[i].crest_ts,         "R*") == 0) &&
	             (strcmp(prev_vtecinfo.crest_ts, "R*") == 0))     		       	   
            {
	       if (fp[i].obsH[fp[i].obs_max_index].value > prev_vtecinfo.crest_value)
	       {
	          vtecinfo[i].cresttime   = fp[i].obsH[fp[i].obs_max_index].validtime;	
		  vtecinfo[i].crest_value = fp[i].obsH[fp[i].obs_max_index].value;
	          strcpy(vtecinfo[i].crest_ts, "R*");
	       }
	       else
	       {	          
	          vtecinfo[i].cresttime   = prev_vtecinfo.cresttime;
		  vtecinfo[i].crest_value = prev_vtecinfo.crest_value;
	          strcpy(vtecinfo[i].crest_ts, "R*");
	       }	  
	    }
	    
	    /* If there is no current crest time, the previous event's cresttime
	       was based on obs, ruse the previous obs one if still valid */
	       
	    else if ((strcmp(fp[i].crest_ts,         "") == 0) &&
	             (strcmp(prev_vtecinfo.crest_ts, "R*") == 0))     		       	   
            {   	       
	       vtecinfo[i].cresttime = prev_vtecinfo.cresttime;
	       vtecinfo[i].crest_value = prev_vtecinfo.crest_value;
	       strcpy(vtecinfo[i].crest_ts, "R*");		  
            }		  
	    
	    /* if the current cresttime is based on obs, there is no previous
	       cresttime, use the current obs. */
	    
	    else if ((strcmp(fp[i].crest_ts,         "R*") == 0) &&
	             (strcmp(prev_vtecinfo.crest_ts, "") == 0))     		       	   
            {   
	       vtecinfo[i].cresttime = fp[i].obsH[fp[i].obs_max_index].validtime;	
	       vtecinfo[i].crest_value = fp[i].obsH[fp[i].obs_max_index].value;		      
	       strcpy(vtecinfo[i].crest_ts, "R");   
	    }
	    
	    /* if the current cresttime is based on fcst, there is no previous
	       cresttime, use the current fcst. */
	    
	    else if ((strcmp(fp[i].crest_ts,         "F*") == 0) &&
	             (strcmp(prev_vtecinfo.crest_ts, "") == 0))   
            {   
	       vtecinfo[i].cresttime = fp[i].fcstH[fp[i].fcst_max_index].validtime;	
	       vtecinfo[i].crest_value = fp[i].fcstH[fp[i].fcst_max_index].value;
	       strcpy(vtecinfo[i].crest_ts, "F*");
	    }
	    
	    /* if there is no current cresttime , there is no previous
	       cresttime. */
	    
	    else if ((strcmp(fp[i].crest_ts,         "") == 0) &&
	             (strcmp(prev_vtecinfo.crest_ts, "") == 0))     		       	   
            {   
	       vtecinfo[i].cresttime = MISSINGVAL;	
	       vtecinfo[i].crest_value = MISSINGVAL;
	       strcpy(vtecinfo[i].crest_ts, "");
	    }      
	    
	    else
	    {
	       vtecinfo[i].cresttime = MISSINGVAL;
	       vtecinfo[i].crest_value = MISSINGVAL;
	       strcpy(vtecinfo[i].crest_ts, "");
	    }   
	 }
         	 	          
	      
	 /* load the fall below time ------------------ */
	 		 
	 /* For NEW action */	 
	  
	 if (strcmp(vtecinfo[i].action, "NEW") == 0)
	 {
	    if (strcmp(fp[i].fallbelow_ts, "F*") == 0)
            { 		     
	       vtecinfo[i].falltime = fp[i].fcst_fallbelow_time;
	       strcpy(vtecinfo[i].fall_ts, "F*");
	    }	    	    
	    else
	    {
	       vtecinfo[i].falltime = MISSINGVAL;
	       strcpy(vtecinfo[i].fall_ts, "");
	    }   
	 }
	 
	 else         
	 {
	    /* For non-NEW actions,
	       check the type source for the fp[].fallbelow_ts and the stored 
	       previously issued event's type source prev_vtecinfo.fall_ts, 
	       then determine the event's fallbelow time is based on forecast
	       or observed data.  */
	    
	    /* if current fallbelow is based on forecast, the previous event's
	       fallbelow was based on forecast, use the current forecast */
	       
	    if ((strcmp(fp[i].fallbelow_ts,    "F*") == 0) &&
	        (strcmp(prev_vtecinfo.fall_ts, "F*") == 0))
            {		 
	       vtecinfo[i].falltime  = fp[i].fcst_fallbelow_time;	      
	       strcpy(vtecinfo[i].fall_ts, "F*");
	    } 
	    
	    
	    /* if current fallbelow is based on obs, the previous event's 
	       fallbelow was based on forecast, use the current obs */
	         
	    else if ((strcmp(fp[i].fallbelow_ts,    "R*") == 0) &&
	             (strcmp(prev_vtecinfo.fall_ts, "F*") == 0))
	    {	      		      
	       vtecinfo[i].falltime = fp[i].obs_fallbelow_time;	     
	       strcpy(vtecinfo[i].fall_ts, "R*");
	    }
	    
	    
	    /* if there is no current fallbelow time, the previous event's
	       fallbelow was based on forecast, use the current one */
	       
	    else if ((strcmp(fp[i].fallbelow_ts,    "")   == 0) &&
	             (strcmp(prev_vtecinfo.fall_ts, "F*") == 0))
	    {	      		      
	       vtecinfo[i].falltime = MISSINGVAL;	     
	       strcpy(vtecinfo[i].fall_ts, "");
	    }   
	    
	    
	    /* if current fallbelow is based on forecast, the previous event's
	       fallbelow was based on obs, then use the current forecast */
	       
	    else if ((strcmp(fp[i].fallbelow_ts,    "F*") == 0) &&
	             (strcmp(prev_vtecinfo.fall_ts, "R*") == 0))
	    {	      		      
	       vtecinfo[i].falltime = fp[i].fcst_fallbelow_time;	   
	       strcpy(vtecinfo[i].fall_ts, "F*");
	    }
	    
	    
	    /* if current fallbelow is based on obs, the previous event's
	       fallbelow was based on obs, then use the latest obs */
	       
	    else if ((strcmp(fp[i].fallbelow_ts,    "R*") == 0) &&
	             (strcmp(prev_vtecinfo.fall_ts, "R*") == 0))  
	    {	      
	       vtecinfo[i].falltime = fp[i].obs_fallbelow_time;	       	     
	       strcpy(vtecinfo[i].fall_ts, "R*");
	    }
	    
	    
	    /* if there is no current fallbelow, the previous event's
	       fallbelow was based on obs, use the current one */
	       
	    else if ((strcmp(fp[i].fallbelow_ts,    "") == 0) &&
	             (strcmp(prev_vtecinfo.fall_ts, "R*") == 0))  
	    {	      
	       vtecinfo[i].falltime = MISSINGVAL;	       	     
	       strcpy(vtecinfo[i].fall_ts, "");
	    }
	    
	    
	    /* if the current fallbelow is based on obs, there is no
	       previous fallbelow, use the current one */
	       
	    else if ((strcmp(fp[i].fallbelow_ts,    "R*") == 0) &&
	             (strcmp(prev_vtecinfo.fall_ts, "")   == 0))  
	    {	      
	       vtecinfo[i].falltime = fp[i].obs_fallbelow_time;	       	     
	       strcpy(vtecinfo[i].fall_ts, "R*");
	    } 
	    
	    /* if the current fallbelow is based on fcst, there is no
	       previous fallbelow, use the current one */
	       
	    else if ((strcmp(fp[i].fallbelow_ts,    "F*") == 0) &&
	             (strcmp(prev_vtecinfo.fall_ts, "") == 0))  
	    {	      
	       vtecinfo[i].falltime = fp[i].fcst_fallbelow_time;	       	     
	       strcpy(vtecinfo[i].fall_ts, "F*");
	    }       
	    
	    
	    /* if there is no current fallbelow, there is no
	       previous fallbelow */
	       
	    else if ((strcmp(fp[i].fallbelow_ts,    "") == 0) &&
	             (strcmp(prev_vtecinfo.fall_ts, "") == 0))  
	    {	      
	       vtecinfo[i].falltime = MISSINGVAL;	       	     
	       strcpy(vtecinfo[i].fall_ts, "");
	    } 
	    
	    else
	    {  
	       vtecinfo[i].falltime = MISSINGVAL;
	       strcpy(vtecinfo[i].fall_ts, "");
	    }
	 }
	 
	 
	 /* load the flood severity from crest_value */
	 		 
	 max_cat = compute_stage_cat(fp[i].cat, vtecinfo[i].crest_value);
	 strcpy(severity_code, compute_vtec_severity(max_cat));
	 strcpy(vtecinfo[i].severity,  severity_code); 
	 
	 
	 /* load the flood record status field from the crest_value info */
	 
	 strcpy(record_code, 
		compute_vtec_record(vtecinfo[i].crest_value, fp[i].cat[4], fp[i].pe));
	 strcpy(vtecinfo[i].record,  record_code);            	 	     	 		   
      }
   }
       
   return;
}


/*********************************************************************
   get_previous_event()
   
   PURPOSE
   Gets the previous event, if one exists, for the event proposed
   for the current geo-id. An optional product mode (OTEX) can be 
   passed in to restrict the match.
     
   *******************************************************************/
VTECevent * get_previous_event(char		*geoid,
		               VTECevent	*eventHead,
			       char		*mode_filterstr,
			       char		*signif_str)
{
   VTECevent 	*prev_eventPtr = NULL;
   int		previous_found; 
   
   
   /* initialize */
   
   previous_found = FALSE;
   
   
   /* look for the last event for this geo entity (i.e. point, county, 
      or group).  the original query sorts by producttime DESC */
   
   if (eventHead != NULL)
      prev_eventPtr = (VTECevent *) ListFirst(&eventHead->list);
   else
      prev_eventPtr = NULL;
 
   
   while (prev_eventPtr)
   {
      /* get the latest previous event that matches the geo id and 
         the requested event significance code */

      if ((strcmp(prev_eventPtr->geoid, geoid)       == 0) &&
          (strcmp(prev_eventPtr->signif, signif_str) == 0))
      {	 
	 /* check that the product mode matches, if checking
	    for that match */
	 
	 if (strlen(mode_filterstr) == 0 ||
	     (strlen(prev_eventPtr->productmode) > 0 &&
	      strstr(mode_filterstr, prev_eventPtr->productmode) != NULL))
	 {
	    previous_found = TRUE;
	    break;
	 }
      }
      
      prev_eventPtr = (VTECevent *) ListNext(&prev_eventPtr->node);
   }
   
   
   if (previous_found)
      return(prev_eventPtr);
   else
      return(NULL);   
}
 

/*********************************************************************
   get_previous_inactive_event()
   
   PURPOSE
   Gets the previous inactive event, if one exists, for the event
   proposed for the current geoid. An optional product mode (OTEX) 
   can be passed in to restrict the match.
     
   *******************************************************************/
VTECevent * get_previous_inactive_event(char		*geoid,
		                        VTECevent	*eventHead,
			                char		*mode_filterstr,
			                char		*signif_str,
					time_t		curtimet)
{
   VTECevent 	*prev_eventPtr = NULL;
   int		previous_found;
   int 		active, status;
   time_t	prev_endtime; 
   int		active_etn;
   
   
   /* initialize */
   
   previous_found = FALSE;
   active_etn = MISSINGVAL;
   
   
   /* initialize the pointer for the upcoming search */
   
   if (eventHead != NULL)
      prev_eventPtr = (VTECevent *) ListFirst(&eventHead->list);
   else
      prev_eventPtr = NULL;
      
 
   /* look for the last inactive event for this point. the original query
      sorted by producttime DESC */
         
   while (prev_eventPtr)
   {
      /* get the latest previous event that matches the id and 
         the requested event significance code */

      if ((strcmp(prev_eventPtr->geoid,  geoid)      == 0) &&
          (strcmp(prev_eventPtr->signif, signif_str) == 0))
      {	 
	 /* check that the product mode matches, if checking
	    for that match */
	 
	 if (strlen(mode_filterstr) == 0 ||
	     (strlen(prev_eventPtr->productmode) > 0 &&
	      strstr(mode_filterstr, prev_eventPtr->productmode) != NULL))
	 {
	    /* check whether the event is inactive */

            if (IsNull(DATETIME, &(prev_eventPtr->endtime)))
	       prev_endtime = MISSINGVAL;
            else
	       status = yearsec_dt_to_timet(prev_eventPtr->endtime,
	                                    &prev_endtime); 
	      
            active = check_if_event_active(prev_eventPtr->action,
			  prev_endtime,	curtimet);
			  
			  
	    /* note the ETN for the first active event found.
	       this is used below. */	
	    
	    if (active == TRUE && active_etn == MISSINGVAL)
	    {
	       active_etn = prev_eventPtr->etn;
	    }	  
			  
	    
	    /* only consider previous inactive events that are not an 
	       earlier issuance for the current active event, as noted
	       by the ETN.  this is needed to avoid using earlier issuances
	       of the current event, which may have their end time in the 
	       past and therefore be considered expired, but the event was
	       extended and is still an ongoing event and therefore must
	       not be considered as the inactive event. */
	       		  
	    if (active == FALSE)
	    {
	       if (prev_eventPtr->etn != active_etn)
	       {
	          previous_found = TRUE;
	          break;
	       }
	    }
	 }
      }
      
      prev_eventPtr = (VTECevent *) ListNext(&prev_eventPtr->node);
   }
   
   
   /* return with the results */
   
   if (previous_found)
      return(prev_eventPtr);
   else
      return(NULL);   
}

  
/*********************************************************************
   load_previous_event()
   
   PURPOSE
   Loads info on the previous matching event into the 
   previous info portion of the vtecinfo structure.
     
   *******************************************************************/
void load_previous_event(VTECevent	        *prevPtr,
			  prev_vtecinfo_struct 	*prev_xxx)
{   
   int	status;
   

   /* load the structure */
       
   if (prevPtr)
   {
        prev_xxx->event_found = TRUE;
      
        strcpy(prev_xxx->product_id, prevPtr->product_id);          
        if (IsNull(DATETIME, &(prevPtr->producttime)))
	  prev_xxx->producttime = MISSINGVAL;
        else
	  status = yearsec_dt_to_timet(prevPtr->producttime,&(prev_xxx->producttime)); 
					 
        if (IsNull(DATETIME, &(prevPtr->expiretime)))
	  prev_xxx->expiretime = MISSINGVAL;
        else
	  status = yearsec_dt_to_timet(prevPtr->expiretime, &(prev_xxx->expiretime)); 
       					      
        strcpy(prev_xxx->action, prevPtr->action);          
        strcpy(prev_xxx->phenom, prevPtr->phenom);         
        strcpy(prev_xxx->signif, prevPtr->signif); 
      
        prev_xxx->etn = prevPtr->etn;   
      
      
        /* translate the begintime and endtime of the previous event. */
      
        if (IsNull(DATETIME, &(prevPtr->begintime)))
	  prev_xxx->begintime = MISSINGVAL;
        else
	  status = yearsec_dt_to_timet(prevPtr->begintime, &(prev_xxx->begintime)); 
      
        if (IsNull(DATETIME, &(prevPtr->endtime)))
	  prev_xxx->endtime    = MISSINGVAL;
        else
	  status = yearsec_dt_to_timet(prevPtr->endtime, &(prev_xxx->endtime));
	 
	 
	/* load in the H-VTEC info similarly */
	 
	strcpy(prev_xxx->severity,    prevPtr->severity); 
	strcpy(prev_xxx->immed_cause, prevPtr->immed_cause); 
	strcpy(prev_xxx->record,      prevPtr->record);               
         
        if (IsNull(DATETIME, &(prevPtr->risetime)))
	  prev_xxx->risetime    = MISSINGVAL;
        else
	  status = yearsec_dt_to_timet(prevPtr->risetime, &(prev_xxx->risetime));
	 
        if (IsNull(DATETIME, &(prevPtr->cresttime)))
	  prev_xxx->cresttime    = MISSINGVAL;
        else
	  status = yearsec_dt_to_timet(prevPtr->cresttime, &(prev_xxx->cresttime));
	 
        if (IsNull(DATETIME, &(prevPtr->falltime)))
	  prev_xxx->falltime    = MISSINGVAL;
        else
	  status = yearsec_dt_to_timet(prevPtr->falltime, &(prev_xxx->falltime));
					 
        strcpy(prev_xxx->rise_ts,  prevPtr->risets);
	strcpy(prev_xxx->crest_ts, prevPtr->crests);
	strcpy(prev_xxx->fall_ts,  prevPtr->fallts);	 
	prev_xxx->crest_value = prevPtr->crest_value;					 
   }
    
   
   /* load in empty values if no previous event exists */
   
   else
   {
        prev_xxx->event_found = FALSE;
      
        strcpy(prev_xxx->product_id, "");          
        prev_xxx->producttime = MISSINGVAL;
        prev_xxx->expiretime  = MISSINGVAL;
	 
        strcpy(prev_xxx->action, "");          
        strcpy(prev_xxx->phenom, "");         
        strcpy(prev_xxx->signif, ""); 
           
        prev_xxx->etn       = MISSINGVAL;   
      
        prev_xxx->begintime = MISSINGVAL;
        prev_xxx->endtime   = MISSINGVAL;
	 
	strcpy(prev_xxx->severity,    "");          
	strcpy(prev_xxx->immed_cause, ""); 
	strcpy(prev_xxx->record,      "");               
         
	prev_xxx->risetime  = MISSINGVAL;	 
	prev_xxx->cresttime = MISSINGVAL;	 
	prev_xxx->falltime  = MISSINGVAL;
	 
	strcpy(prev_xxx->rise_ts,  "");
	strcpy(prev_xxx->crest_ts, "");
	strcpy(prev_xxx->fall_ts,  "");	 
        prev_xxx->crest_value = MISSINGVAL;    
   }
 
   return;
}


/*********************************************************************
   copy_previous_event()
   
   PURPOSE
   Copy info on the previous matching event into a
   convenient single structure.
     
   *******************************************************************/
   
void copy_previous_event(prev_vtecinfo_struct   prev_xxx, 
                         prev_vtecinfo_struct   *prev_vtecinfo)
{

  prev_vtecinfo->event_found = prev_xxx.event_found;
      
  strcpy(prev_vtecinfo->product_id, prev_xxx.product_id);          
  prev_vtecinfo->producttime = prev_xxx.producttime;
  prev_vtecinfo->expiretime  = prev_xxx.expiretime;
      
  strcpy(prev_vtecinfo->action, prev_xxx.action);          
  strcpy(prev_vtecinfo->phenom, prev_xxx.phenom);         
  strcpy(prev_vtecinfo->signif, prev_xxx.signif); 
           
  prev_vtecinfo->etn = prev_xxx.etn;   
      
  prev_vtecinfo->begintime = prev_xxx.begintime;
  prev_vtecinfo->endtime   = prev_xxx.endtime;
     
  strcpy(prev_vtecinfo->severity,    prev_xxx.severity);          
  strcpy(prev_vtecinfo->immed_cause, prev_xxx.immed_cause);          
  strcpy(prev_vtecinfo->record,      prev_xxx.record);               
  prev_vtecinfo->risetime   = prev_xxx.risetime;
  prev_vtecinfo->cresttime  = prev_xxx.cresttime;
  prev_vtecinfo->falltime   = prev_xxx.falltime;
     
  strcpy(prev_vtecinfo->rise_ts,  prev_xxx.rise_ts);
  strcpy(prev_vtecinfo->crest_ts, prev_xxx.crest_ts);
  strcpy(prev_vtecinfo->fall_ts,  prev_xxx.fall_ts);
     
  prev_vtecinfo->crest_value = prev_xxx.crest_value;
  
  
  return; 
}


/*********************************************************************
   check_if_event_active()
   
   PURPOSE
   Given a single VTEC event, determines if the event is active.
   
   *******************************************************************/
int check_if_event_active(char			*prev_action,
			  time_t		prev_endtime,			  
			  time_t		curtimet)
{
   int active = FALSE;
                 

   /* if the event is explicity expired or cancelled,
      then the event tracking is not active.
      also, all ROU events are considered inactive.
      otherwise, see if the event end time has not passed yet */
   
   if ((strcmp(prev_action, "CAN") == 0) || 
       (strcmp(prev_action, "EXP") == 0) ||
       (strcmp(prev_action, "ROU") == 0) ||
       (strlen(prev_action) == 0))
      active = FALSE;
   
   else	 
   {      
      /* if the endtime is not specified, assume the event is still active. 
         unspecified can occur if the endtime has a missing value indicator
	 or is set to 0.  A 0 value can occur because the time may be NULL in
	 the database, which is converted to a 0 value by the time conversion
	 functions. */
      
      if (prev_endtime == MISSINGVAL || prev_endtime == 0)
	 active = TRUE;
      
      
      /* if the end time is past the current time, then the event
	 is still active. */
      
      else
      {
	 if (prev_endtime > curtimet)
	    active = TRUE;
	 else
	    active = FALSE;
      }
   }
   
   
   return(active);
}


/*********************************************************************
   compute_new_etn()
   
   PURPOSE
   Computes the event tracking number for the proposed new event.
   An event is defined for a given office, phenomena, and
   significance, for a given year.  For example, the ETN is tracked
   for a "FL"ood "W"arning for office XXX for the year YYYY.
   The sets of existing events passed in includes events for all
   product modes (i.e. OTEX).
   
   NOTES
   Under very special cases, this algorithm may recommend
   etn=1 if there is an event 1 (for example) from the previous year
   that is still active. 
   For example, if there are not any events all year, but then a lot of
   flooding around the new year, then it is possible that there is active event 
   with ETN=1 for one year, and ETN=1 for the next year, for a separate event.
   
      
   *******************************************************************/
int compute_new_etn(int			fpindex,
		    pcc_struct		*pcc,     /* not used... */
		    misc_struct		*misc,
		    int			numfps,
		    vtecinfo_struct	*vtecinfo,
		    VTECevent       	*eventHead)
{
   int 		etn;
   int		i, k;
   int		max_prev_etn;
   int		number_free;
   
   
   /* initialize */
   
   etn = 0;
   
   
   /* ROU action events do not have a valid ETN. for this case,
      accept the default value and return without doing anything. */
      
   if (strcmp(vtecinfo[fpindex].action, "ROU") == 0)
   {
     return(etn);
   }
   
   
   /* find max etn in previously issued events, that match the proposed events
      characteristics and for the current year */
   
   max_prev_etn = find_max_prev_etn(eventHead, vtecinfo[fpindex].signif,
				    misc->system_time);
          
     
   /* Consider the reset of etn at the begining of calendar year.
      Increment etn from highest etn for current year and "NEW" action
      event, also the etn could not be equal to the etn of active event
      in the previous year */
   
   etn = max_prev_etn + 1;
   
   for (i = etn; i < MAX_ETN + 1; i++)
   {	    
      /* get the next available number for the event currently 
	 being processed. when checking the other events, only consider
	 those that are included in the product and which are not ROU events.
	 remember that numfps is tha max number of events for the segment
	 mode, not the actual number of events. */ 
      
      number_free = TRUE;
      
      for (k = 0; k < numfps; k++)
      {	 	     
	 if (misc->fps_included[k] == TRUE && 
	     strcmp(vtecinfo[k].action, "NEW") == 0)
	 {	 	 
	    if (i == vtecinfo[k].etn)
	    {
	       number_free = FALSE;
	       break;
	    }
	 }
      }
      
      if (number_free == TRUE)
      {   
	 etn = i;
	 break;
      }
   }
   
   
   /* just in case... 1 is better than missing */
   
   if (etn <= 0) 
      etn = 1; 
   
   else if (etn > MAX_ETN)
   {
      fprintf(stderr,
	      "ATTENTION: Next available ETN for the given product "
	      " phenomena, significance code is > 9999. Verify this in "
	      "VTECevent table and purge VTECevent table.\n");
      log_msg("",
	      "Next available ETN for phenom, signif is > 9999. "
	      " Verify then purge VTECevent table for signif, phenom.");
   }
   
   
   /* return with the recommended number */
   
   return(etn);
}


/************************************************************************
   find_max_prev_etn()
   
   PURPOSE
   Find max etn in previously issued events, that match the proposed events
   characteristics and for the current year 
   
   **********************************************************************/
int find_max_prev_etn(VTECevent *eventHead,
		      char	*signif_filter,
		      time_t	current_time)
{
   int 		max_etn;
   VTECevent 	*prev_eventPtr = NULL;
   struct tm	*time_struct;
   time_t	prodtimet;
   int		current_year, event_year;
   
   /* get the year associated with the current time, for use below */
   
   time_struct = gmtime(&current_time);
   current_year = time_struct->tm_year;

	    
   /* find max etn in previously issued events, that match the proposed events
      characteristics and for the current year */
   
   max_etn = 0;
   
   if (eventHead != NULL)
   {
      prev_eventPtr = (VTECevent *) ListFirst(&eventHead->list);
      
      while (prev_eventPtr)
      {
	 /* only consider NEW events that match the signif code and which are
	    for the current year.  the office and phenom already match, based 
	    on the prior query of the VTECevent table, so no need to 
	    check those fields. */
	 
	 if (((strlen(signif_filter) == 0) ||
	      (strcmp(prev_eventPtr->signif, signif_filter) == 0)) &&
	     (strcmp(prev_eventPtr->action, "NEW") == 0))
	 {
	    /* check that this event being checked is for the current year */	    
	    
	    yearsec_dt_to_timet(prev_eventPtr->producttime, &prodtimet);
	    time_struct = gmtime(&prodtimet);
	    event_year = time_struct->tm_year;
 	    
	    if (event_year == current_year)
	    {	  
	       if (prev_eventPtr->etn > max_etn)
		  max_etn = prev_eventPtr->etn;
	    }
	 }
	 
	 prev_eventPtr = (VTECevent *) ListNext(&prev_eventPtr->node);
      }
   }
 
   
   return(max_etn);
}

/************************************************************************
   compute_vtec_severity()
   
   PURPOSE
   Given the maximum flood category, determines the H-VTEC severity code.
   
   **********************************************************************/
char * compute_vtec_severity(int maxcat)
{
   static char severity_str[VTEC_SEVER_LEN + 1]; 
   
   if (maxcat >= MAJOR)
      strcpy(severity_str, "3");
   
   else if (maxcat == MODERATE)
      strcpy(severity_str, "2");
      
   else if (maxcat == MINOR)
      strcpy(severity_str, "1");
   
   else if (maxcat > NULLCAT)
      strcpy(severity_str, "N");
   
   else if (maxcat == NULLCAT)
      strcpy(severity_str, "U");
   
   else
      strcpy(severity_str, "U");
   
   return(severity_str);
}


/*********************************************************************
   compute_vtec_record()
   
   PURPOSE
   Given the maximum flood category, determines the "record"
   flood comparison code for the H-VTEC line.
   
   *******************************************************************/
char * compute_vtec_record(float max_value,
                           float record_value,
			   char  *primary_pe)
{
   static char	record_str[VTEC_RECORD_LEN + 1];
   
   static float stage_threshold = DEFAULT_VTECRECORD_STAGE;
   static float flow_threshold  = DEFAULT_VTECRECORD_FLOW;
   float	threshold;
   static int	first = TRUE;
   char         token_string[60];
   int          token_len, string_len;
   char         msgstr[100];
   
  
   
   /* get the threshold offsets for determining what "near" really means.
      there is a threshold for stage data, and one for flow data. */
   
   if (first) 
   {
     token_len = strlen("vtec_record_stageoffset");
     get_apps_defaults("vtec_record_stageoffset", &token_len, token_string, 
                       &string_len);
     if (string_len > 0)
     {
       stage_threshold = atof(token_string);
       if (stage_threshold <= 0.0 )
       {
           stage_threshold = DEFAULT_VTECRECORD_STAGE;
	   sprintf(msgstr, "Error in specified value for token: %s.\n"
	                   "Using default value of %f.\n",
			   token_string, stage_threshold);
	   log_msg("", msgstr);
       }
     }
     
     token_len = strlen("vtec_record_flowoffset");
     get_apps_defaults("vtec_record_flowoffset", &token_len, token_string, 
                       &string_len);
     if (string_len > 0)
     {
       flow_threshold = atof(token_string);
       if (flow_threshold <= 0.0 )
       {
           flow_threshold = DEFAULT_VTECRECORD_FLOW;
	   sprintf(msgstr, "Error in specified value for token: %s.\n"
	                   "Using default value of %f.\n",
			   token_string, flow_threshold);
	   log_msg("", msgstr);
       }
     }
    
     first = FALSE;
   }     
    
   
   /* make sure that both values are defined.
      check if the current value is near the record value */
   
   if (max_value != MISSINGVAL && record_value != MISSINGVAL)
   {
      if (primary_pe[0] == 'Q')
      	 threshold = flow_threshold;
      else
	 threshold = stage_threshold;
      
      if (max_value >= (record_value - threshold))
	 strcpy(record_str, "NR");
      else
	 strcpy(record_str, "NO");	 
   }
     
   else
      strcpy(record_str, "UU");
   
   
   return(record_str);
}


/*******************************************************************
   create_vteclines()
   
   PURPOSE
   Creates the actual VTEC format lines used for output
   in the product.
   
   *****************************************************************/

void create_vteclines(vtecinfo_struct	*vtecinfo,
		      misc_struct	*misc,		      
		      int		fpindex,
		      fp_struct		*fp,
		      char		*Pvtec_line,
		      char		*Hvtec_line)
{
   char begintime[VTEC_TIMECODE_LEN + 1], endtime[VTEC_TIMECODE_LEN + 1];
   char cresttime[VTEC_TIMECODE_LEN + 1];
   int	i;
   int	officelen;
   char office_str[RFC_LEN + 1];
   
   static int 		first_gad = TRUE;
   static char	  	prefix_str[20];
   int    		gad_token_len, gad_value_len;
   float                flood_level;
   
   /* initialize */
   
   strcpy(Pvtec_line, "");
   strcpy(Hvtec_line, "");


   /* set the current time again */
   
   time(&misc->system_time);
   
   
   /* get the optionally defined single-character office prefix.
      if not defined, then use the letter K. */
   
   if (first_gad)
   {
      gad_token_len = strlen("office_prefix");
      get_apps_defaults("office_prefix", &gad_token_len, 
			prefix_str, &gad_value_len);
			
      if (strlen(prefix_str) != 1)
         strcpy(prefix_str, "K");
   
      first_gad = FALSE;
   }
   
      
   /* append a fourth character to the office id if necessary */
      
   memset(office_str, '\0', RFC_LEN + 1);   
   officelen = strlen(misc->selected_office);
   
   /* special check for San Juan site, it use office prefix as "T" */
   
   if (strcmp(misc->selected_office, "JSJ") == 0)
      strcpy(prefix_str, "T");
      
   
   if (officelen >= 4)
      sprintf (office_str, "%4s", misc->selected_office);
   else
      sprintf (office_str, "%1s%s", prefix_str, misc->selected_office);
     
     
   /* create convenient local variable for the forecast point index */
   
   i = fpindex;
      
   
   /* set begintime and endtime */
   
   strcpy(begintime, format_vtectime(vtecinfo[i].begintime, misc, VTEC_BEGINTIME));  
   strcpy(endtime,   format_vtectime(vtecinfo[i].endtime,   misc, VTEC_ENDTIME));
   
   
   /* format the regular P-VTEC line */
        
   sprintf(Pvtec_line, "/%s.%s.%s.%s.%s.%.4d.%s-%s/", 
   	   vtecinfo[i].vtec_cat, vtecinfo[i].action, office_str,
	   vtecinfo[i].phenom,   vtecinfo[i].signif, vtecinfo[i].etn,
	   begintime, endtime);
   
   
   /* create the H-VTEC line */
     
   strcpy(begintime, format_vtectime(vtecinfo[i].risetime,  misc, VTEC_OTHERTIME));  
   
   /* if the crest value is below the flood value (stage or flow), set the crest time
   as missing or order to output all zeros in the product */
      
   flood_level = get_flood_level(i, fp);
   
   if ((flood_level - vtecinfo[i].crest_value) > 0.0001)
       strcpy(cresttime, "000000T0000Z");
   else    
       strcpy(cresttime, format_vtectime(vtecinfo[i].cresttime, misc, VTEC_OTHERTIME));   
       
   strcpy(endtime,   format_vtectime(vtecinfo[i].falltime,  misc, VTEC_OTHERTIME));   
   
   sprintf(Hvtec_line, "/%s.%s.%s.%s.%s.%s.%s/", 
	   vtecinfo[i].geoid, vtecinfo[i].severity, vtecinfo[i].immed_cause,
	   begintime, cresttime, endtime, vtecinfo[i].record);
   
   return;
}


/******************************************************************
   save_vtec_events()
   
   PURPOSE
   Save the info on the issued events to the database.
   
   ******************************************************************/

void save_vtec_events(int		numfps,
		      fp_struct		*fp,
		      int		numgrps,
		      grp_struct	*grp, 
		      int		numcnty,
		      county_struct	*cnty,
		      pcc_struct	*pcc,
		      misc_struct	*misc,
		      vtecinfo_struct	*vtecinfo)
{
   int 		i;
   int		status;
   int		num_included_events;
   VTECevent	vtecevent;
   char		msgstr[80];
   
   
   /* set the number of events */
   
   num_included_events = 0;
   
   
   /* load the fields that are not unique to each event being saved;
      i.e. loading need not be inside loop below. */
   
   strcpy(vtecevent.product_id, pcc->product.product_cnx);
   status = timet_to_yearsec_dt(misc->system_time, &vtecevent.producttime);
   
   
   /* when saving the office id, note that the 4th character is not
      prepended to the 3-character selected_office variable; 
      so only 3 chars are saved. */
   
   strcpy(vtecevent.office_id, misc->selected_office);
                   	   
   
   /* loop on the number of events and save the info */
   
   for (i = 0; i < numfps; i++)
   {      
      /* set the id */
      
      strcpy(vtecevent.geoid, fp[i].id);
		         
    
      /* save the event info as necessary.  save even the ROU.Y.S events
	 so that they are there for the record; they are not used for 
	 future recommendations */
      
      if (misc->fps_included[i] == TRUE)
      {	 
	 num_included_events++;
	 
	 
	 /* vtec 1st line info */
	 
	 strcpy(vtecevent.action, vtecinfo[i].action);      
	 strcpy(vtecevent.phenom, vtecinfo[i].phenom);             
	 strcpy(vtecevent.signif, vtecinfo[i].signif);   
	 vtecevent.etn = vtecinfo[i].etn;
	 
	 
	 /* if the begin time is CURRENT, store it as missing.  This is done
	    so that the next time Riverpro checks the begin time, it doesn't 
	    not recommend a EXT since the begin time may be different.  This 
	    can happen if the event is issued after the riseabove item, in which 
	    cases the begintime defaults to the current time by rule. */
	    
	 if (vtecinfo[i].begintime == MISSINGVAL)
	    status = SetNull(DATETIME,
			     &vtecevent.begintime);
	 else if (vtecinfo[i].begintime == CURRENT_TIME)
	    status = SetNull(DATETIME,
			     &vtecevent.begintime);
        else
	    status = timet_to_yearsec_dt(vtecinfo[i].begintime, 
					 &vtecevent.begintime);
	 
	 if (vtecinfo[i].endtime == MISSINGVAL)
	    status = SetNull(DATETIME, 
			     &vtecevent.endtime);
         else if (vtecinfo[i].endtime == CURRENT_TIME)
            status = SetNull(DATETIME, 
	                     &vtecevent.endtime);
	 else
	    status = timet_to_yearsec_dt(vtecinfo[i].endtime,   
					 &vtecevent.endtime);
	 
	 
	 /* vtec 2nd line info */
	 
	 strcpy(vtecevent.severity,    vtecinfo[i].severity);          
	 strcpy(vtecevent.immed_cause, vtecinfo[i].immed_cause);
	 
	 if (vtecinfo[i].risetime == MISSINGVAL)
	    status = SetNull(DATETIME, 
			     &vtecevent.risetime);
	 else
	    status = timet_to_yearsec_dt(vtecinfo[i].risetime,  
					 &vtecevent.risetime);
	 
	 if (vtecinfo[i].cresttime == MISSINGVAL)
	    status = SetNull(DATETIME, 
			     &vtecevent.cresttime);
	 else
	    status = timet_to_yearsec_dt(vtecinfo[i].cresttime, 
					 &vtecevent.cresttime);
	 
	 if (vtecinfo[i].falltime == MISSINGVAL)
	    status = SetNull(DATETIME, &vtecevent.falltime);
	 else
	    status = timet_to_yearsec_dt(vtecinfo[i].falltime,  
					 &vtecevent.falltime);

	 strcpy(vtecevent.record,      vtecinfo[i].record); 
	 
	 strcpy(vtecevent.productmode, vtecinfo[i].vtec_cat);
	 
	 strcpy(vtecevent.risets, vtecinfo[i].rise_ts);

	 strcpy(vtecevent.crests, vtecinfo[i].crest_ts);
	 
	 strcpy(vtecevent.fallts, vtecinfo[i].fall_ts);
	 
	 vtecevent.crest_value = vtecinfo[i].crest_value;
	 
	 if (vtecinfo[i].expiretime == MISSINGVAL)
	    status = SetNull(DATETIME, &vtecevent.expiretime);
	 else
	    status = timet_to_yearsec_dt(vtecinfo[i].expiretime,  
					 &vtecevent.expiretime);
	 
	 /* now insert the record into the database */
	 
	 /* Use VTECpractice table if workstation mode is "PRACTICE" */
   
         if (misc->workstation_mode == PRACTICE_MODE)
            status = PutVTECpractice((VTECpractice *)(&vtecevent));	
	 else   
	    status = PutVTECevent(&vtecevent);
	    
	 if (status != 0)
	 {
	    sprintf(msgstr, "%s (%d)", vtecevent.geoid, status);
	    log_msg(VTECEVENT_SAVEFAIL, msgstr);
	 }	 
      }
   }
   
   
   /* log a message */
   
   sprintf(msgstr, "Saved VTEC information for %d events", num_included_events);
   log_msg("", msgstr);
   
   
   return;
}

/******************************************************************
   save_corvtec_events()
   
   PURPOSE
   Save the correct vtec info on the issued events to the database.
   
   ******************************************************************/

void save_corvtec_events(int		 numfps,
			 fp_struct	 *fp,
			 int		 numgrps,
			 grp_struct	 *grp, 
			 int		 numcnty,
			 county_struct	 *cnty,
			 pcc_struct	 *pcc,
			 misc_struct	 *misc,
			 vtecinfo_struct *vtecinfo)
{
   int 		i, j;
   int		status;
   VTECevent	vtecevent;
   VTECevent    *vtecHead = NULL;
   char		msgstr[80];
   char         where[300];
   int          corevent_set = FALSE;
   
   /* loop on the number of corrected forecast points  */
   
   for (i = 0; i < misc->cor_prevprod.event_cnt; i++)
   {
      /* The primary key in VTECevent/VTECpractice table is
         (geoid+product_id+producttime). Retrieve record */
      
      sprintf(where, 
	   " WHERE  geoid ='%s' AND product_id = '%s' AND producttime = '%s' ",
	     misc->cor_prevprod.event_id[i], misc->cor_prevprod.prodid,
	     misc->cor_prevprod.prod_ansi_time);
      
      if (misc->workstation_mode == PRACTICE_MODE)
         vtecHead = (VTECevent *)GetVTECpractice(where);	     
      else
         vtecHead = GetVTECevent(where);	 
     
      if (vtecHead != NULL)
       {
         /* set the event with COR action and current time as producttime */
	 
         status = timet_to_yearsec_dt(misc->system_time, &vtecevent.producttime);
	 
	 /* change the action to COR if the event changed */
	 
	 for (j = 0; j < misc->cor_prevprod.corevent_cnt; j++)
	 {
	    if (strcmp(misc->cor_prevprod.event_id[i], misc->cor_prevprod.cor_fpid[j]) == 0)
	    {
	        corevent_set = TRUE;
		break;
	    }	
	    else	
	        corevent_set = FALSE;
         }  
	 			
	 if (corevent_set == TRUE)			 
	    strcpy(vtecevent.action, "COR");
	 else
	    strcpy(vtecevent.action, vtecHead->action);   
         
	 /* copy other fields from retrieval info */
	 
	 strcpy(vtecevent.geoid,       vtecHead->geoid);
	 strcpy(vtecevent.product_id,  vtecHead->product_id);
	 strcpy(vtecevent.productmode, vtecHead->productmode);
	 strcpy(vtecevent.office_id,   vtecHead->office_id);
	 strcpy(vtecevent.phenom,      vtecHead->phenom);             
	 strcpy(vtecevent.signif,      vtecHead->signif);   
	 vtecevent.etn = vtecHead->etn;
	 vtecevent.begintime =  vtecHead->begintime;
	 vtecevent.endtime = vtecHead->endtime;
	 strcpy(vtecevent.severity,    vtecHead->severity);          
	 strcpy(vtecevent.immed_cause, vtecHead->immed_cause);
	 vtecevent.risetime = vtecHead->risetime;
	 vtecevent.cresttime = vtecHead->cresttime;
	 vtecevent.falltime = vtecHead->falltime;
	 strcpy(vtecevent.record,      vtecHead->record); 		 
	 strcpy(vtecevent.risets,      vtecHead->risets);
	 strcpy(vtecevent.crests,      vtecHead->crests);	 
	 strcpy(vtecevent.fallts,      vtecHead->fallts);	 
	 vtecevent.crest_value = vtecHead->crest_value;
         vtecevent.expiretime = vtecHead->expiretime;
	 
         /* Use VTECpractice table if workstation mode is "PRACTICE" */

        if (misc->workstation_mode == PRACTICE_MODE)
           status = PutVTECpractice((VTECpractice *)(&vtecevent));	
	else   
	   status = PutVTECevent(&vtecevent);
	   
	if (status != 0)
	{
	   sprintf(msgstr, "%s (%d)", vtecevent.geoid, status);
	   log_msg(VTECEVENT_SAVEFAIL, msgstr);
	}   
        
	/* free space */
	 
	 if (misc->workstation_mode == PRACTICE_MODE)
	    FreeVTECpractice((VTECpractice *)vtecHead);
	 else   
	    FreeVTECevent(vtecHead);
	    
     } /* end of valid record */    
     
     else /* can't find the record */
     {
         sprintf(msgstr,
	         "Can not find record from VTECevent/VTECPractice\n"
		 "product id, lid, producttime: %s %s %s \n",
		  misc->cor_prevprod.prodid, 
		  misc->cor_prevprod.cor_fpid[i],
		  misc->cor_prevprod.prod_ansi_time);
         log_msg("", msgstr);     
     }
  }
  
  /* log a message */
   
   sprintf(msgstr, "Saved corrected VTEC information for %d events", misc->cor_prevprod.corevent_cnt);
   log_msg("", msgstr);
  
return;

}  

/******************************************************************
   check_if_vtec()
   
   PURPOSE
   Check if generating a VTEC product.
   
   ******************************************************************/
int check_if_vtec(pcc_struct	*pcc)
{
   int	vtec_set;
   
   
   if ((!pcc->product.nwr_flag) &&
       (pcc->product.segment_mode == SEGMENT_MODE_POINT) &&
       pcc->product.vtec_flag)
      vtec_set = TRUE;
         
   else
      vtec_set = FALSE;
   
   
   return(vtec_set);
}


/******************************************************************
   format_vtectime()
   
   PURPOSE
   Formats the vtec time field for inclusion in the generated
   product, accounting for missing values and times dependent
   upon the current time.
   
   ****************************************************************/
char *format_vtectime(time_t		timeval,
		      misc_struct	*misc,
		      int		field_index)
{
   static char  outstr[VTEC_TIMECODE_LEN+1];
   struct tm    *tm_struct;
   
   
   if (timeval == MISSINGVAL)
   {
      sprintf(outstr, "000000T0000Z");
   }
   
   else if (timeval == CURRENT_TIME)
   {
   
      /* if in interactive mode, then use a time code to represent 
         the time to be translated later */
	 
      if(misc->batch_mode == TRUE)
      {
         tm_struct = gmtime(&misc->system_time);
         strftime(outstr, VTEC_TIMECODE_LEN+1, "%y%m%dT%H%MZ", tm_struct);
      }
      
      else
      {
         if      (field_index == VTEC_BEGINTIME)
            strcpy(outstr, TIMESTR_VTEC_BEGIN);
	    
	 else if (field_index == VTEC_ENDTIME)
            strcpy(outstr, TIMESTR_VTEC_END);
	    
	 else
	 {
	    fprintf(stderr, "ERROR: invalid field set to current time.\n");
            strcpy(outstr, "**BAD_TIME**");
	 }
      }
   }
   
   else
   {
      tm_struct = gmtime(&timeval);
      strftime(outstr, VTEC_TIMECODE_LEN+1, "%y%m%dT%H%MZ", tm_struct);
   }
   
   return(outstr);
}


/************************************************************************
   format_rtime()
   
   PURPOSE   
   Formats a time string for output.
   
   *********************************************************************/

char *format_rtime(time_t timeval)
{
   static char outstr[SHORT_LEN];
   struct tm *tm_struct;
   
   
   if (timeval == MISSINGVAL)
      strcpy(outstr, "undefined");


   /* this format uses 11 chars, as in "12/25-12:15" */

   else
   {
      tm_struct = gmtime(&timeval);
      strftime(outstr, SHORT_LEN, "%m/%d-%H:%M", tm_struct);
   }
   
   return(outstr);
}

/************************************************************************
  setFLYendtime()
   
  PURPOSE   
  For the NEW FLY event, set the event's end time as the issuance time
  plus the time specified in token FLYendtime_shift_hours which default is
  36hours
   
   *********************************************************************/

time_t setFLYendtime(misc_struct *misc)
		    
{
   static int  first = TRUE;
   static int  FLYendtime_shift_hours = DEFAULT_FLYENDTIME_SHIFT_HOURS;
   char        token_string[60];
   int         token_len, string_len;
   char        msgstr[100];
   time_t      flyevent_endtime;
   
   if (first) 
   {
     token_len = strlen("FLYendtime_shift_hours");
     get_apps_defaults("FLYendtime_shift_hours", &token_len, token_string, 
                       &string_len);
     if (string_len > 0)
     {
       FLYendtime_shift_hours = atoi(token_string);
       if (FLYendtime_shift_hours < 0 )
       {
           FLYendtime_shift_hours = DEFAULT_FLYENDTIME_SHIFT_HOURS;
	   sprintf(msgstr, "Error in specified value for token: %s.\n"
	                   "Using default value of %d.\n",
			   token_string, FLYendtime_shift_hours);
	   log_msg("", msgstr);
       }
     }
         
     first = FALSE;     
   }  
   
   flyevent_endtime = misc->system_time + (FLYendtime_shift_hours * 3600.0);
   
   return flyevent_endtime;

}
/************************************************************************
  set_fallbelow_time()
   
  PURPOSE   
  Sets the virtual fall-below time based on any actual fall below time, 
  considering some special cases.
   
   *********************************************************************/

time_t set_fallbelow_time(int		fpindex,
			  fp_struct 	*fp,
			  int		shift_flag) 
{
   time_t 	fallbelow_time;
   double	flood_level;
   static int	first = TRUE;
   char         token_string[60];
   int          token_len, string_len;
   static int	shift_hours = DEFAULT_ENDTIME_SHIFT_HOURS;
   char         msgstr[100];
   
   
   if (first) 
   {
     token_len = strlen("rpf_endtime_shifthrs");
     get_apps_defaults("rpf_endtime_shifthrs", &token_len, token_string, 
                       &string_len);
     if (string_len > 0)
     {
       shift_hours = atoi(token_string);
       if (shift_hours < 0 || shift_hours > 48)
       {
           shift_hours = DEFAULT_ENDTIME_SHIFT_HOURS;
	   sprintf(msgstr, "Error in specified value for token: %s.\n"
	                   "Using default value of %d.\n",
			   token_string, shift_hours);
	   log_msg("", msgstr);
       }
     }
         
     first = FALSE;     
   }  
   
      
   /* initialize */

   if (shift_flag)  
   {
      if (fp[fpindex].fallbelow_time != MISSINGVAL) 
      {
         if (fp[fpindex].adjustendhrs != MISSINGVAL)
            fallbelow_time = fp[fpindex].fallbelow_time + (fp[fpindex].adjustendhrs*3600);
	 else   
            fallbelow_time = fp[fpindex].fallbelow_time + (shift_hours*3600);
      }	    
      else
         fallbelow_time = MISSINGVAL;
   }	  	 
   else
      fallbelow_time = fp[fpindex].fallbelow_time;

   
   /* adjust endtime for the special case where the forecast drops
      below flood level, which results in valid fall-below time, but
      then it rises above flood level. in this case, we don't know
      when the final fall-below time is, so set it to missing/unknown. */
   
   if (fp[fpindex].numfcstH > 0)
   {
      
      flood_level = get_flood_level(fpindex, fp);
	 
      if (flood_level != MISSINGVAL)
      {
	 if (fp[fpindex].fcstH[fp[fpindex].numfcstH - 1].value > flood_level)
	    fallbelow_time = MISSINGVAL;
      }
   }
   
      
   return(fallbelow_time);
}


/************************************************************************
   get_flood_level()
   
   PURPOSE   
   Get the flood level value based on primary pe.
   
   *********************************************************************/
float get_flood_level(int	fpindex,
		      fp_struct	*fp)
{
   float flood_level;
   
   
   if (fp[fpindex].pe[0] == 'Q')
      flood_level = fp[fpindex].fq;
   
   else
      flood_level = fp[fpindex].fs;
   
   
   return(flood_level);
}


/*********************************************************************
   load_previous_eventOLD()
   
   PURPOSE
   Loads info on the previous matching event into the vtecinfo
   structure.
     
   *******************************************************************/
void load_previous_eventOLD(int 		fp_index,
			 char			*signif_str,
 			 VTECevent		*prevPtr,
			 vtecinfo_struct 	*vtecinfo)

{   
   int	i;
   int	status;
   
   
   /* use convenience variable */

   i = fp_index;
    

   /* load the structure */
       
   if (prevPtr)
   {
      if (strcmp(signif_str, SIGNIF_WARNING) == 0)
      {
         vtecinfo[i].prev_flw.event_found = TRUE;
      
         strcpy(vtecinfo[i].prev_flw.product_id, prevPtr->product_id);          
         if (IsNull(DATETIME, &(prevPtr->producttime)))
	    vtecinfo[i].prev_flw.producttime = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->producttime,
				         &(vtecinfo[i].prev_flw.producttime)); 
					 
         if (IsNull(DATETIME, &(prevPtr->expiretime)))
	    vtecinfo[i].prev_flw.expiretime = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->expiretime,
				         &(vtecinfo[i].prev_flw.expiretime)); 
       					      
         strcpy(vtecinfo[i].prev_flw.action, prevPtr->action);          
         strcpy(vtecinfo[i].prev_flw.phenom, prevPtr->phenom);         
         strcpy(vtecinfo[i].prev_flw.signif, prevPtr->signif); 
      
         vtecinfo[i].prev_flw.etn = prevPtr->etn;   
      
      
         /* translate the begintime and endtime of the previous event. */
      
         if (IsNull(DATETIME, &(prevPtr->begintime)))
	    vtecinfo[i].prev_flw.begintime = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->begintime,
			   	         &(vtecinfo[i].prev_flw.begintime)); 
      
         if (IsNull(DATETIME, &(prevPtr->endtime)))
	    vtecinfo[i].prev_flw.endtime    = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->endtime, 
		   		         &(vtecinfo[i].prev_flw.endtime));
	 
	 
	 /* load in the H-VTEC info similarly */
	 
	 strcpy(vtecinfo[i].prev_flw.severity,    prevPtr->severity);          
	 strcpy(vtecinfo[i].prev_flw.immed_cause, prevPtr->immed_cause); 
	 strcpy(vtecinfo[i].prev_flw.record,      prevPtr->record);               
         
         if (IsNull(DATETIME, &(prevPtr->risetime)))
	    vtecinfo[i].prev_flw.risetime    = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->risetime, 
		   		         &(vtecinfo[i].prev_flw.risetime));
	 
         if (IsNull(DATETIME, &(prevPtr->cresttime)))
	    vtecinfo[i].prev_flw.cresttime    = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->cresttime, 
		   		         &(vtecinfo[i].prev_flw.cresttime));
	 
         if (IsNull(DATETIME, &(prevPtr->falltime)))
	    vtecinfo[i].prev_flw.falltime    = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->falltime, 
		   		         &(vtecinfo[i].prev_flw.falltime));
					 
         strcpy(vtecinfo[i].prev_flw.rise_ts,  prevPtr->risets);
	 strcpy(vtecinfo[i].prev_flw.crest_ts, prevPtr->crests);
	 strcpy(vtecinfo[i].prev_flw.fall_ts,  prevPtr->fallts);	 
	 vtecinfo[i].prev_flw.crest_value = prevPtr->crest_value;					 
	 
      }
      
      
      else if (strcmp(signif_str, SIGNIF_WATCH) == 0)
      {
         vtecinfo[i].prev_fla.event_found = TRUE;
      
         strcpy(vtecinfo[i].prev_fla.product_id, prevPtr->product_id);          
         if (IsNull(DATETIME, &(prevPtr->producttime)))
	    vtecinfo[i].prev_fla.producttime = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->producttime,
				         &(vtecinfo[i].prev_fla.producttime)); 
      
         if (IsNull(DATETIME, &(prevPtr->expiretime)))
	    vtecinfo[i].prev_fla.expiretime = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->expiretime,
				         &(vtecinfo[i].prev_fla.expiretime));
					 
         strcpy(vtecinfo[i].prev_fla.action, prevPtr->action);          
         strcpy(vtecinfo[i].prev_fla.phenom, prevPtr->phenom);         
         strcpy(vtecinfo[i].prev_fla.signif, prevPtr->signif); 
      
         vtecinfo[i].prev_fla.etn = prevPtr->etn;   
      
      
         /* translate the begintime and endtime of the previous event. */
      
         if (IsNull(DATETIME, &(prevPtr->begintime)))
	    vtecinfo[i].prev_fla.begintime = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->begintime,
			   	         &(vtecinfo[i].prev_fla.begintime)); 
      
         if (IsNull(DATETIME, &(prevPtr->endtime)))
	    vtecinfo[i].prev_fla.endtime    = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->endtime, 
		   		         &(vtecinfo[i].prev_fla.endtime));
	 
	 
	 /* load in the H-VTEC info similarly */
	 
	 strcpy(vtecinfo[i].prev_fla.severity,    prevPtr->severity);          
	 strcpy(vtecinfo[i].prev_fla.immed_cause, prevPtr->immed_cause); 
	 strcpy(vtecinfo[i].prev_fla.record,      prevPtr->record);               
         
         if (IsNull(DATETIME, &(prevPtr->risetime)))
	    vtecinfo[i].prev_fla.risetime    = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->risetime, 
		   		         &(vtecinfo[i].prev_fla.risetime));
	 
         if (IsNull(DATETIME, &(prevPtr->cresttime)))
	    vtecinfo[i].prev_fla.cresttime    = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->cresttime, 
		   		         &(vtecinfo[i].prev_fla.cresttime));
	 
         if (IsNull(DATETIME, &(prevPtr->falltime)))
	    vtecinfo[i].prev_fla.falltime    = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->falltime, 
		   		         &(vtecinfo[i].prev_fla.falltime));
					 
	 strcpy(vtecinfo[i].prev_fla.rise_ts,  prevPtr->risets);
	 strcpy(vtecinfo[i].prev_fla.crest_ts, prevPtr->crests);
	 strcpy(vtecinfo[i].prev_fla.fall_ts,  prevPtr->fallts);				 
	 vtecinfo[i].prev_fla.crest_value = prevPtr->crest_value;
         					 
      }

      if (strcmp(signif_str, SIGNIF_ADVISORY) == 0)
      {
         vtecinfo[i].prev_fly.event_found = TRUE;
      
         strcpy(vtecinfo[i].prev_fly.product_id, prevPtr->product_id);          
         if (IsNull(DATETIME, &(prevPtr->producttime)))
	    vtecinfo[i].prev_fly.producttime = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->producttime,
				         &(vtecinfo[i].prev_fly.producttime)); 
      
         if (IsNull(DATETIME, &(prevPtr->expiretime)))
	    vtecinfo[i].prev_fly.expiretime = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->expiretime,
				         &(vtecinfo[i].prev_fly.expiretime));
					 
         strcpy(vtecinfo[i].prev_fly.action, prevPtr->action);          
         strcpy(vtecinfo[i].prev_fly.phenom, prevPtr->phenom);         
         strcpy(vtecinfo[i].prev_fly.signif, prevPtr->signif); 
      
         vtecinfo[i].prev_fly.etn = prevPtr->etn;   
      
      
         /* translate the begintime and endtime of the previous event. */
      
         if (IsNull(DATETIME, &(prevPtr->begintime)))
	    vtecinfo[i].prev_fly.begintime = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->begintime,
			   	         &(vtecinfo[i].prev_fly.begintime)); 
      
         if (IsNull(DATETIME, &(prevPtr->endtime)))
	    vtecinfo[i].prev_fly.endtime    = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->endtime, 
		   		         &(vtecinfo[i].prev_fly.endtime));
	 
	 
	 /* load in the H-VTEC info similarly */
	 
	 strcpy(vtecinfo[i].prev_fly.severity,    prevPtr->severity);          
	 strcpy(vtecinfo[i].prev_fly.immed_cause, prevPtr->immed_cause); 
	 strcpy(vtecinfo[i].prev_fly.record,      prevPtr->record);               
         
         if (IsNull(DATETIME, &(prevPtr->risetime)))
	    vtecinfo[i].prev_fly.risetime    = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->risetime, 
		   		         &(vtecinfo[i].prev_fly.risetime));
	 
         if (IsNull(DATETIME, &(prevPtr->cresttime)))
	    vtecinfo[i].prev_fly.cresttime    = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->cresttime, 
		   		         &(vtecinfo[i].prev_fly.cresttime));
	 
         if (IsNull(DATETIME, &(prevPtr->falltime)))
	    vtecinfo[i].prev_fly.falltime    = MISSINGVAL;
         else
	    status = yearsec_dt_to_timet(prevPtr->falltime, 
		   		         &(vtecinfo[i].prev_fly.falltime));
					 
	 strcpy(vtecinfo[i].prev_fly.rise_ts,  prevPtr->risets);
	 strcpy(vtecinfo[i].prev_fly.crest_ts, prevPtr->crests);
	 strcpy(vtecinfo[i].prev_fly.fall_ts,  prevPtr->fallts);				 
         vtecinfo[i].prev_fly.crest_value = prevPtr->crest_value;
	          					 
      }
   }
   
   
   /* load in empty values if no previous event exists */
   
   else
   {
      if (strcmp(signif_str, SIGNIF_WARNING) == 0)
      {
         vtecinfo[i].prev_flw.event_found = FALSE;
      
         strcpy(vtecinfo[i].prev_flw.product_id, "");          
         vtecinfo[i].prev_flw.producttime = MISSINGVAL;
         vtecinfo[i].prev_flw.expiretime = MISSINGVAL;
	 
         strcpy(vtecinfo[i].prev_flw.action, "");          
         strcpy(vtecinfo[i].prev_flw.phenom, "");         
         strcpy(vtecinfo[i].prev_flw.signif, ""); 
           
         vtecinfo[i].prev_flw.etn       = MISSINGVAL;   
      
         vtecinfo[i].prev_flw.begintime = MISSINGVAL;
         vtecinfo[i].prev_flw.endtime   = MISSINGVAL;
	 
	 strcpy(vtecinfo[i].prev_flw.severity,    "");          
	 strcpy(vtecinfo[i].prev_flw.immed_cause, ""); 
	 strcpy(vtecinfo[i].prev_flw.record,      "");               
         
	 vtecinfo[i].prev_flw.risetime  = MISSINGVAL;	 
	 vtecinfo[i].prev_flw.cresttime = MISSINGVAL;	 
	 vtecinfo[i].prev_flw.falltime  = MISSINGVAL;
	 
	 strcpy(vtecinfo[i].prev_flw.rise_ts,  "");
	 strcpy(vtecinfo[i].prev_flw.crest_ts, "");
	 strcpy(vtecinfo[i].prev_flw.fall_ts,  "");	 
	 vtecinfo[i].prev_flw.crest_value = MISSINGVAL; 
      }
      
      else if (strcmp(signif_str, SIGNIF_WATCH) == 0)
      {
         vtecinfo[i].prev_fla.event_found = FALSE;
      
         strcpy(vtecinfo[i].prev_fla.product_id, "");          
         vtecinfo[i].prev_fla.producttime = MISSINGVAL;
         vtecinfo[i].prev_fla.expiretime = MISSINGVAL;
	 
         strcpy(vtecinfo[i].prev_fla.action, "");          
         strcpy(vtecinfo[i].prev_fla.phenom, "");         
         strcpy(vtecinfo[i].prev_fla.signif, ""); 
           
         vtecinfo[i].prev_fla.etn       = MISSINGVAL;   
      
         vtecinfo[i].prev_fla.begintime = MISSINGVAL;
         vtecinfo[i].prev_fla.endtime   = MISSINGVAL;
	 
	 strcpy(vtecinfo[i].prev_fla.severity,    "");          
	 strcpy(vtecinfo[i].prev_fla.immed_cause, ""); 
	 strcpy(vtecinfo[i].prev_fla.record,      "");               
         
	 vtecinfo[i].prev_fla.risetime  = MISSINGVAL;	 
	 vtecinfo[i].prev_fla.cresttime = MISSINGVAL;	 
	 vtecinfo[i].prev_fla.falltime  = MISSINGVAL;
	 
	 strcpy(vtecinfo[i].prev_fla.rise_ts,  "");
	 strcpy(vtecinfo[i].prev_fla.crest_ts, "");
	 strcpy(vtecinfo[i].prev_fla.fall_ts,  "");				 
	 vtecinfo[i].prev_fla.crest_value = MISSINGVAL;
      }
      
      else if (strcmp(signif_str, SIGNIF_ADVISORY) == 0)
      {
         vtecinfo[i].prev_fly.event_found = FALSE;
      
         strcpy(vtecinfo[i].prev_fly.product_id, "");          
         vtecinfo[i].prev_fly.producttime = MISSINGVAL;
         vtecinfo[i].prev_fly.expiretime = MISSINGVAL;
	 
         strcpy(vtecinfo[i].prev_fly.action, "");          
         strcpy(vtecinfo[i].prev_fly.phenom, "");         
         strcpy(vtecinfo[i].prev_fly.signif, ""); 
           
         vtecinfo[i].prev_fly.etn       = MISSINGVAL;   
      
         vtecinfo[i].prev_fly.begintime = MISSINGVAL;
         vtecinfo[i].prev_fly.endtime   = MISSINGVAL;
	 
	 strcpy(vtecinfo[i].prev_fly.severity,    "");          
	 strcpy(vtecinfo[i].prev_fly.immed_cause, ""); 
	 strcpy(vtecinfo[i].prev_fly.record,      "");               
         
	 vtecinfo[i].prev_fly.risetime  = MISSINGVAL;	 
	 vtecinfo[i].prev_fly.cresttime = MISSINGVAL;	 
	 vtecinfo[i].prev_fly.falltime  = MISSINGVAL;
	 
	 strcpy(vtecinfo[i].prev_fly.rise_ts,  "");
	 strcpy(vtecinfo[i].prev_fly.crest_ts, "");
	 strcpy(vtecinfo[i].prev_fly.fall_ts,  "");				 
         vtecinfo[i].prev_fly.crest_value = MISSINGVAL;
      }
   }
 
   return;
}


/*********************************************************************
   copy_previous_event()  OLD version
   
   PURPOSE
   Copy info on the previous matching event into a
   convenient single structure.
     
   *******************************************************************/  
void copy_previous_eventOLD(int               i, 
                         vtecinfo_struct      *vtecinfo, 
			 char                 *signif_str, 
                         prev_vtecinfo_struct *prev_vtecinfo)
{

  if (strcmp(signif_str, SIGNIF_WARNING) == 0)
  {
     prev_vtecinfo->event_found = vtecinfo[i].prev_flw.event_found;
      
     strcpy(prev_vtecinfo->product_id, vtecinfo[i].prev_flw.product_id);          
     prev_vtecinfo->producttime = vtecinfo[i].prev_flw.producttime;
     prev_vtecinfo->expiretime = vtecinfo[i].prev_flw.expiretime;
      
     strcpy(prev_vtecinfo->action, vtecinfo[i].prev_flw.action);          
     strcpy(prev_vtecinfo->phenom, vtecinfo[i].prev_flw.phenom);         
     strcpy(prev_vtecinfo->signif, vtecinfo[i].prev_flw.signif); 
           
     prev_vtecinfo->etn = vtecinfo[i].prev_flw.etn;   
      
     prev_vtecinfo->begintime = vtecinfo[i].prev_flw.begintime;
     prev_vtecinfo->endtime   = vtecinfo[i].prev_flw.endtime;
     
     strcpy(prev_vtecinfo->severity,    vtecinfo[i].prev_flw.severity);          
     strcpy(prev_vtecinfo->immed_cause, vtecinfo[i].prev_flw.immed_cause);          
     strcpy(prev_vtecinfo->record,      vtecinfo[i].prev_flw.record);               
     prev_vtecinfo->risetime   = vtecinfo[i].prev_flw.risetime;
     prev_vtecinfo->cresttime  = vtecinfo[i].prev_flw.cresttime;
     prev_vtecinfo->falltime   = vtecinfo[i].prev_flw.falltime;
     
     strcpy(prev_vtecinfo->rise_ts,  vtecinfo[i].prev_flw.rise_ts);
     strcpy(prev_vtecinfo->crest_ts, vtecinfo[i].prev_flw.crest_ts);
     strcpy(prev_vtecinfo->fall_ts,  vtecinfo[i].prev_flw.fall_ts);
     
     prev_vtecinfo->crest_value = vtecinfo[i].prev_flw.crest_value;
  }
      
  else if (strcmp(signif_str, SIGNIF_WATCH) == 0)
  {
     prev_vtecinfo->event_found = vtecinfo[i].prev_fla.event_found;
      
     strcpy(prev_vtecinfo->product_id, vtecinfo[i].prev_fla.product_id);          
     prev_vtecinfo->producttime = vtecinfo[i].prev_fla.producttime;
     prev_vtecinfo->expiretime = vtecinfo[i].prev_fla.expiretime;
      
     strcpy(prev_vtecinfo->action, vtecinfo[i].prev_fla.action);          
     strcpy(prev_vtecinfo->phenom, vtecinfo[i].prev_fla.phenom);         
     strcpy(prev_vtecinfo->signif, vtecinfo[i].prev_fla.signif); 
           
     prev_vtecinfo->etn = vtecinfo[i].prev_fla.etn;   
      
     prev_vtecinfo->begintime = vtecinfo[i].prev_fla.begintime;
     prev_vtecinfo->endtime   = vtecinfo[i].prev_fla.endtime;
     
     strcpy(prev_vtecinfo->severity,    vtecinfo[i].prev_fla.severity);          
     strcpy(prev_vtecinfo->immed_cause, vtecinfo[i].prev_fla.immed_cause);          
     strcpy(prev_vtecinfo->record,      vtecinfo[i].prev_fla.record);               
     prev_vtecinfo->risetime   = vtecinfo[i].prev_fla.risetime;
     prev_vtecinfo->cresttime  = vtecinfo[i].prev_fla.cresttime;
     prev_vtecinfo->falltime   = vtecinfo[i].prev_fla.falltime;
     
     strcpy(prev_vtecinfo->rise_ts,  vtecinfo[i].prev_fla.rise_ts);
     strcpy(prev_vtecinfo->crest_ts, vtecinfo[i].prev_fla.crest_ts);
     strcpy(prev_vtecinfo->fall_ts,  vtecinfo[i].prev_fla.fall_ts);
     
     prev_vtecinfo->crest_value = vtecinfo[i].prev_fla.crest_value;
  }
      
  else if (strcmp(signif_str, SIGNIF_ADVISORY) == 0)
  {
     prev_vtecinfo->event_found = vtecinfo[i].prev_fly.event_found;
      
     strcpy(prev_vtecinfo->product_id, vtecinfo[i].prev_fly.product_id);          
     prev_vtecinfo->producttime = vtecinfo[i].prev_fly.producttime;
     prev_vtecinfo->expiretime = vtecinfo[i].prev_fly.expiretime;
      
     strcpy(prev_vtecinfo->action, vtecinfo[i].prev_fly.action);          
     strcpy(prev_vtecinfo->phenom, vtecinfo[i].prev_fly.phenom);         
     strcpy(prev_vtecinfo->signif, vtecinfo[i].prev_fly.signif); 
           
     prev_vtecinfo->etn = vtecinfo[i].prev_fly.etn;   
      
     prev_vtecinfo->begintime = vtecinfo[i].prev_fly.begintime;
     prev_vtecinfo->endtime   = vtecinfo[i].prev_fly.endtime;
     
     strcpy(prev_vtecinfo->severity,    vtecinfo[i].prev_fly.severity);          
     strcpy(prev_vtecinfo->immed_cause, vtecinfo[i].prev_fly.immed_cause);          
     strcpy(prev_vtecinfo->record,      vtecinfo[i].prev_fly.record);               
     prev_vtecinfo->risetime   = vtecinfo[i].prev_fly.risetime;
     prev_vtecinfo->cresttime  = vtecinfo[i].prev_fly.cresttime;
     prev_vtecinfo->falltime   = vtecinfo[i].prev_fly.falltime;
     
     strcpy(prev_vtecinfo->rise_ts,  vtecinfo[i].prev_fly.rise_ts);
     strcpy(prev_vtecinfo->crest_ts, vtecinfo[i].prev_fly.crest_ts);
     strcpy(prev_vtecinfo->fall_ts,  vtecinfo[i].prev_fly.fall_ts);
     
     prev_vtecinfo->crest_value = vtecinfo[i].prev_fly.crest_value;
  }
 
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/whfs_lib/src/RPFEngine/RCS/process_vtecinfo.c,v $";
 static char rcs_id2[] = "$Id: process_vtecinfo.c,v 1.16 2007/06/25 18:09:22 deng Exp $";}
/*  ===================================================  */

}

