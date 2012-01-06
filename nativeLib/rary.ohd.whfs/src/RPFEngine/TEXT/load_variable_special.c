/*********************************************************************
   load_variable_special.c
   
   
   build_countylist()
   write_counties()
   build_riverlist()
   write_rivers()
   
   build_grplist()
   build_grpfplist()
   
   load_location_str()
   load_riverstat_str()
   load_riverstat_float()
   load_descrip_str()
   
   alpha_compare()
   
   load_action_str()                   
   load_hsa_str()                     
   load_officename_str()               
   load_eventtime_str() 
                
   load_stgflow_name()                 
   load_stgflow_units() 
                 
   build_loccntylist()    
   build_statecntylist_byaction()     
   write_statecounties() 
   build_statelist_byaction()          
   load_impcomp_units()                
   
   load_loclat()
   load_loclon()
   
   ********************************************************************/

#include <string.h>                  /* string library functions */
#include <time.h>                    /* time library functions */
#include <stdlib.h>                  /* standard library functions */
#include <ctype.h>

#include "load_variable_special.h"  /* function prototypes */

      

/*********************************************************************
   build_countylist()
   
   PURPOSE
   Build the list of counties.
   
   NOTES
   This approach uses a pairing of arrays, where one array has the 
   county number and the other has the corresponding state.
   
   ********************************************************************/
void build_countylist(const grp_struct		*grp,
	              const fp_struct		*fp,
		            misc_struct		*misc,
		      const pcc_struct		*pcc,
			    char		*longstring)
{
   int 		i, j;
   int		numitems, slen;
   int 		grpindex, fpindex, cnt;
   char 	*statecounty[MAX_COUNTIES];
   int 		numlist, matchfound;   
   Countynum 	*countynumPtr = NULL, *countynum = NULL;
   char		where[MAXLEN_STRING];
   char 	teststr[MAXLEN_STRING];
   int		include_fp;
   int          state_la_found = FALSE;
   
   /* initialize */
   
   memset(longstring, 0, MAXLEN_LONGSTR);
   numlist = 0;
   
   
   /* loop on all the forecast points;  this loop works in a 
      unique way since numfps variable is not available; it uses
      the grp structure and num grps included information instead */
   
   for (cnt = grpindex = 0; cnt < misc->numgrps_included; grpindex++)
   {
      if (misc->grps_included[grpindex] == TRUE)
      {
	 cnt++;
	 
	 for (i = 0; i < grp[grpindex].numfps; i++)
	 {
	    fpindex = grp[grpindex].fpindex[i];
	    
	    /* check if the point is included if this is an NWR product. */
	    
	    if (pcc->product.nwr_flag)
	       include_fp = check_loc_in_tower(fp[fpindex].id,
					       misc->nwrtransPtr, 
					       misc->loctransPtr);
	    else
	       include_fp = TRUE;
	       
	    
	    /* only consider if the forecast point is included */
	    
	    if (misc->fps_included[fpindex] == TRUE)
	    {		 
	       /* load the counties that are defined for this forecast point*/
	       
	       sprintf(where, " WHERE lid = '%s' ", fp[fpindex].id);
	       
	       countynumPtr = GetCountynum(where);
	       if (countynumPtr == NULL)
		   numitems = 0;
	       else
		   numitems = ListCount(&countynumPtr->list);
	       
	       if (numitems <= 0)
		  log_msg(NO_COUNTYNUM, fp[fpindex].id);
	       
	       else
	       {
		  /* get the info for this entry and loop on the
		     number defined for the forecast point */
		  
		  countynum = (Countynum *) ListFirst(&countynumPtr->list);
		  while (countynum != NULL && numlist + 1 < MAX_COUNTIES)
		  {
		     /* check if the state is Louisiana, if it is, change
		     the word "county" or "counties" to "parish" or 
		     "parishes" */		     		     
		     
		     if (strcasecmp(countynum->state, "LA") == 0)
		        state_la_found = TRUE;						
			
		     /* check if there is an existing entry for this state
			county combination */
		     
		     sprintf(teststr, "%s %s", 
			     countynum->state, countynum->county);
		     
		     matchfound = FALSE;
		     for (j = 0; j < numlist; j++)
		     {
			if (strcmp(teststr, statecounty[j]) == 0)
			{
			   matchfound = TRUE;
			   break;
			}
		     }
		     
		     
		     /* only load new info if it isn't loaded already */
		     
		     if (matchfound == FALSE)
		     {						
			/* allocate and store the state and county name */
			
			slen = strlen(teststr);			
			statecounty[numlist] = (char *)malloc(slen + 1);
			if (statecounty[numlist] == NULL)
			   log_msg(FAILED_MALLOC, 
				   "for statecounty in build_countylist");
			strcpy(statecounty[numlist], teststr);    
			
			numlist++;
		     }
		     
		     
		     /* get the next entry for the forecast point */
		     
		     countynum = (Countynum *) ListNext(&countynum->node);    
		  }
		  
		  /* free the memory for this forecast point */
		  
		  FreeCountynum(countynumPtr);
		  
	       }     /* end of if on whether info found for forecast point */	 
	    }        /* end of if on whether forecast point included */
	 }           /* end of for loop on forecast points */
      } /* end of if on whether current group is included */
   }    /* end of loop on included groups */
   
   
   /* at this point, we know the number of states and their ids, and for
      each state, there is a list of counties for the state;
      now build the string */
    
   write_counties(numlist, statecounty, longstring, state_la_found);
   
   
   /* free the allocated memory */
   
   for (i = 0; i < numlist; i++)
      free(statecounty[i]);
   
   return;
}


/*********************************************************************
   write_counties()
   
   PURPOSE
   Write the list of counties given the state and counties 
   that are included.
   
   NOTES
   The state and county are stored in the same string, with the 
   two-character state, a space, and then the county name.
   
   ********************************************************************/
void write_counties(const	int		numlist,
	            		char 		*statecounty[],
	       			char		*longstring,
				int             state_al_found)
{	    
   
   int 		i, j;
   int 		pos;
   char 	*spaceloc= NULL;
   int 		curcount;
   char 	*statenames[MAX_UGCSTATES];
   char 	*stateabbs[MAX_UGCSTATES];
   State 	*statePtr = NULL;
   int 		numstates;
   int 		foundflag;
   char 	where[MAXLEN_STRING];
   char 	tempstr[MAXLEN_STRING];

   
   /* sort the list of states and associated counties */   
   
   qsort((char *)(statecounty), (size_t)numlist,
	 (size_t)sizeof(char *), alpha_compare);
   
   
   /* determine the number of states in the list and load
      their abbreviated names and their full names */
   
   numstates = 0;
   for (i = 0; i < numlist; i++)
   {
      /* loop on the number of states already identified,
	 use the state abbreviation to search, not the full name
	 which may have had its case converted */
      
      foundflag = FALSE;
      for (j = 0; j < numstates; j++)
      {
	 if (strncmp(statecounty[i], stateabbs[j], 2) == 0)
	 {
	    foundflag = TRUE;
	    break;
	 }
      }
      
      
      /* if entry not found, then get the full name of the state and 
	 add it to the list */
      
      if (foundflag == FALSE)
      {
	 memset(tempstr, 0, MAXLEN_STRING);
	 strncpy(tempstr, statecounty[i], 2);
	 sprintf(where, " WHERE state = '%s' ", tempstr); 
	 statePtr = GetState(where);
	 
	 if (statePtr != NULL)
	 {
	    statenames[numstates] = (char *)malloc(strlen(statePtr->name) + 1);
	    if (statenames[numstates] == NULL)
	       log_msg(FAILED_MALLOC, "for statenames[] in write_counties");
	    strcpy(statenames[numstates], statePtr->name);
	    
	    stateabbs[numstates] = (char *)malloc(strlen(tempstr + 1));
	    if (stateabbs[numstates] == NULL)
	       log_msg(FAILED_MALLOC, "for stateabbs[] in write_counties");
	    strcpy(stateabbs[numstates], tempstr);
	    
	    numstates++;
	 }
      }
   }
   
   
   /* loop on the number of states */
   
   pos = 0;
   
   for (i = 0; i < numstates; i++)
   {
      /* special handle for state as Louisana */
      
      if (state_al_found == TRUE)
      {
         /*if only one state and this state is LA */
	 
	 if (numstates == 1)
	 {
	    if (i == 0)
	    {
	       strcat(longstring, "parishes in ");	 
	       pos = pos + 13;
	    }
	    else
	    {
	       strcat(longstring, " and in ");
	       pos = pos + 8;
	    }      
         }
	 /* if state AL is one of the states */
	 else 
	 {
	    if (i == 0)
	    {
	       strcat(longstring, "counties/parishes in ");	 
	       pos = pos + 22;
	    }
	    else
	    {
	       strcat(longstring, " and in ");
	       pos = pos + 8;
	    }
	 }         
      } /* end of special handle for LA state */
      else
      {	    
	 /* first load the leading phrase giving the state name.
	    always use mixed case since we can convert to upper 
	    case later. */

	 if (i == 0 || numstates == 0)
	 {
	    strcat(longstring, "counties in ");

	    pos = pos + 13;
	 }

	 else
	 {
	    strcat(longstring, " and in ");	       
	    pos = pos + 8;
	 }
      }
      
      /* assume displaying mixed case, so convert all but the first
	 character of what may be a multi-word state name to lowercase.
	 if using upper case, then convert the text just before write. */
      
      strcpy(tempstr, statenames[i]);
      
      convert_str_to_lowcase(tempstr);
      tempstr[0] = toupper(tempstr[0]);
      if ((spaceloc = strstr(tempstr, " ")) != NULL)
      {
	 j = strlen(statenames[i]) - strlen(spaceloc) + 1;
	 tempstr[j] = toupper(tempstr[j]);
      }
      
      
      /* now load in the state string */
      
      strcat(longstring, tempstr);
      pos = pos + strlen(tempstr);
      strcat(longstring, ": ");
      pos = 2;
      
      
      /* loop thru all the counties.  for those
	 that match the state load them now. */      
      
      curcount = 0;
      for (j = 0; j < numlist; j++)
      {
	 memset(tempstr, 0, MAXLEN_STRING);
	 strncpy(tempstr, statecounty[j], 2);
	 
	 if (strcmp(tempstr, stateabbs[i]) == 0)
	 {
	    if (curcount != 0)
	    {
	       strcat(longstring, "...");
	       pos = pos + 3;
	    }
	    curcount++;
	    
	    strcpy(tempstr, statecounty[j]);
	    strcat(longstring, &tempstr[3]);
	    pos = pos + strlen(statecounty[j] - 3);
	    
	 }
      }             
   } 
   
   
   /* check that the string length was not exceeded */
   
   i = strlen(longstring);
   if (i > MAXLEN_LONGSTR - 1) log_msg(EXCEED_LONGSTR, "");
   
   
   return;
}


/*********************************************************************
   build_riverlist()
   
   PURPOSE
   Build the list of rivers.
   
   NOTES
   This approach uses a pairing of arrays, where one array has the 
   county number and the other has the corresponding state.
   
   ********************************************************************/
void build_riverlist(const grp_struct	*grp,
	             const fp_struct	*fp,
		           misc_struct	*misc,
		     const pcc_struct	*pcc,
			   char		*longstring)
{
   int 		i, j;
   int 		numitems, slen;
   int 		grpindex, fpindex, cnt;
   char 	*rivers[MAX_RIVERS];
   int 		numlist, matchfound;   
   Riverstat 	*rsPtr = NULL;
   char 	where[MAXLEN_STRING];
   int		include_fp;
   
   
   /* initialize */
   
   memset(longstring, 0, MAXLEN_LONGSTR);
   numlist = 0;
   
   
   /* loop on all the forecast points;  this loop works in a 
      unique way since numfps variable is not available; it uses
      the grp structure and grps included information instead */
   
   for (cnt = grpindex = 0; cnt < misc->numgrps_included; grpindex++)
   {
      if (misc->grps_included[grpindex] == TRUE)
      {
	 cnt++;
	 
	 for (i = 0; i < grp[grpindex].numfps; i++)
	 {
	    fpindex = grp[grpindex].fpindex[i];
	    
	    
	    /* check if the point is included if this is an NWR product. */
	    
	    if (pcc->product.nwr_flag)
	       include_fp = check_loc_in_tower(fp[fpindex].id,
					       misc->nwrtransPtr, 
					       misc->loctransPtr);
	    else
	       include_fp = TRUE;
	    
	    
	    /* only consider if the forecast point is included in the product
	       and is within the tower area, for NWR products. */
	    
	    if (misc->fps_included[fpindex] == TRUE && include_fp)
	    {		 
	       /* load the river that is defined for this forecast point */
	       
	       sprintf(where, " WHERE lid = '%s' ", fp[fpindex].id);
	       
	       rsPtr = GetRiverstat(where);
	       if (rsPtr == NULL)
	       	   numitems =  0;
	       else
	           numitems = ListCount(&rsPtr->list);
	       
	       if (rsPtr != NULL && numlist + 1 < MAX_COUNTIES)
	       {		  
		  /* check if there is an existing entry for this river */
		  
		  matchfound = FALSE;
		  for (j = 0; j < numlist; j++)
		  {
		     if (strcmp(rsPtr->stream, rivers[j]) == 0)
		     {
			matchfound = TRUE;
			break;
		     }
		  }
		  
		  
		  /* only load new info if it isn't loaded already */
		  
		  if (matchfound == FALSE)
		  {						
		     /* allocate and store the river name */
		     
		     slen = strlen(rsPtr->stream);
		     if (slen > 0)
		     {
			rivers[numlist] = (char *)malloc(slen + 1);
			if (rivers[numlist] == NULL)
			   log_msg(FAILED_MALLOC,				   
				   "for rivers in build_riverlist");
			strcpy(rivers[numlist], rsPtr->stream);    
			
			numlist++;
		     }
		  }
		  
	       }
	       
	       /* free the memory for this forecast point */
	       
	       FreeRiverstat(rsPtr);
	       
	    }        /* end of if on whether forecast point included */
	 }           /* end of for loop on forecast points */
      } /* end of if on whether current group is included */
   }    /* end of loop on included groups */
   
   
   /* at this point, we know the number of states and their ids, and for
      each state, there is a list of counties for the state;
      now build the string */
   
   write_rivers(numlist, rivers, longstring);
   
   
   /* free the allocated memory */
   
   for (i = 0; i < numlist; i++)
      free(rivers[i]);
   
   return;
}


/*********************************************************************
   write_rivers()
   
   PURPOSE
   Write the list of rivers.
   
   ********************************************************************/
void write_rivers(const	int		numlist,
	            	char 		*rivers[],
	       		char		*longstring)
{	       
   int i;
   int pos;
   int curcount;


   /* initialize */
   
   pos = 0;
   
   
   /* sort the list of rivers */
   
   qsort((char *)(rivers), (size_t)numlist,
	 (size_t)sizeof(char *), alpha_compare);
         
   
   /* loop on the number of rivers */
   
   curcount = 0;
   
   for (i = 0; i < numlist; i++)
   {      
      if (curcount != 0)
      {
	 strcat(longstring, "...");
	 pos = pos + 3;
      }
      curcount++;
      
      strcat(longstring, rivers[i]);
      pos = pos + strlen(rivers[i]);      
      
   }
   
   
   /* check that the string length was not exceeded */
   
   i = strlen(longstring);
   if (i > MAXLEN_LONGSTR - 1) log_msg(EXCEED_LONGSTR, "");
   
   
   return;
}


/*********************************************************************
   build_grplist()
   
   PURPOSE
   Build the list of groups.
   
   
   ********************************************************************/
void build_grplist(const grp_struct	*grp,
		   const fp_struct	*fp,
		         misc_struct	*misc,
		   const pcc_struct	*pcc,
		   char			*longstring)
{
   int	grpindex, cnt;
   int	slen1, slen2;
   int	include_group;
   
   
   /* loop on the groups until all the included groups 
      have been considered */
   
   cnt = 0;
   for (grpindex = 0; cnt < misc->numgrps_included; grpindex++)
   {
      if (misc->grps_included[grpindex] == TRUE)
      {	 
	 
	 cnt++;
	 
	 /* if creating an NWR product, then make sure that at least
	    one point in the group is in the current tower's area. */
	 
	 if (pcc->product.nwr_flag)
	 {
	    include_group = check_grp_in_tower(grp, grpindex, fp, misc);    
	 }
	 else
	    include_group = TRUE;
	 
	 
	 /* insert the text data */
	 
	 if (include_group)
	 {
	    slen1 = strlen(grp[grpindex].name);
	    slen2 = strlen(longstring);
	    
	    if ((slen1 + slen2 + 5) < MAXLEN_LONGSTR)
	    {	       
	       if (strlen(longstring) > 0) strcat(longstring, "...");
	       strcat(longstring, grp[grpindex].name);
	    }
	    else
	    {
	       log_msg(EXCEED_LONGSTR, "for <GrpList>");
	       break;
	    }
	 }
      }	 
   }
   
   return;
}


/*********************************************************************
   build_grp_fplist()
   
   PURPOSE
   Build the list of forecast points in a group . 
   This does not write the name of the forecast group at the
   beginning of the list, and only considers one group.
   
   In the event of NWR products, this function need not check whether
   the group is included.  This is because this variable is specific
   to a single group, and is only included if at least one forecast 
   point is within the given NWR tower area.  It still needs to check
   which of the group's forecast points are to be included.   
   
   Note: use the same function to build the list of forecast points
   in a county.

   ********************************************************************/
void build_grp_fplist(const int		  grpindex,
		      const grp_struct	  *grp,
		      const fp_struct	  *fp,
		            misc_struct	  *misc,
		      const pcc_struct	  *pcc,
		      char		  *longstring,
		      const int           numcnty,
		      const county_struct *cnty)
{
   int	i, fpindex;
   int	slen1, slen2;
   int	include_fp;
   
   
   /* initialize */
   
   include_fp = FALSE;
   
   
   /* create the variable differently depending upon 
      whether in group or county mode */
   
   if (pcc->product.segment_mode == SEGMENT_MODE_COUNTY)
   {
      /* loop on the forecast points in the county*/
      
      for (i = 0; i < cnty[grpindex].numfps; i++)
      {
	 fpindex = cnty[grpindex].fpindex[i];
	 
	 /* make sure that forecast point should be included in the product. */
	 
	 if (misc->fps_included[fpindex] == TRUE) 
	    include_fp = TRUE;       
	 else
	    include_fp = FALSE;
	 
	 
	 /* if included, then load in the info */
	 
	 if (include_fp)
	 {	    
	    slen1 = strlen(fp[fpindex].name);
	    slen2 = strlen(longstring);
	    
	    if ((slen1 + slen2 + 5) < MAXLEN_LONGSTR)
	    {	       
	       if (strlen(longstring) > 0) strcat(longstring, "...");
	       strcat(longstring, fp[fpindex].name);
	    }
	    
	    else
	    {
	       log_msg(EXCEED_LONGSTR, "for <GrpFPList>");
	       break;
	    }
	 }      
      }
      
   } 
   
   
   /* the normal method is to use group info */
   
   else
   {
      /* loop on the forecast points in the group */
      
      for (i = 0; i < grp[grpindex].numfps; i++)
      {
	 fpindex = grp[grpindex].fpindex[i];
	 
	 
	 /* make sure that forecast point should be included in the product. */
	 
	 if (misc->fps_included[fpindex] == TRUE)
	 {	 
	    if (pcc->product.nwr_flag)
	       include_fp = check_loc_in_tower(fp[fpindex].id,
					       misc->nwrtransPtr, 
					       misc->loctransPtr);
	    else
	       include_fp = TRUE;
	 }
	 else
	    include_fp = FALSE;
	 
	 
	 /* if included, then load in the info */
	 
	 if (include_fp)
	 {	  
	    slen1 = strlen(fp[fpindex].name);
	    slen2 = strlen(longstring);
	    
	    if ((slen1 + slen2 + 5) < MAXLEN_LONGSTR)
	    {	       
	       if (strlen(longstring) > 0) strcat(longstring, "...");
	       strcat(longstring, fp[fpindex].name);
	    }
	    
	    else
	    {
	       log_msg(EXCEED_LONGSTR, "for <GrpFPList>");
	       break;
	    }
	 }      
      }
   }
     
   
   return;
}


/*********************************************************************
   build_grps_fplist()
   
   PURPOSE
   Build the list of forecast points in a group. 
   This DOES include the name of the forecast group preceding
   the list, and all groups are considered.
   
   Forms a string with the form:
   grpname1 at fp1...fp2...fp3...grpname2 at fp1...fp2...fp3 etc.
   

   ********************************************************************/
void build_grps_fplist(const grp_struct		*grp,
		       const fp_struct		*fp,
		             misc_struct	*misc,
		       const pcc_struct		*pcc,
		       char			*longstring,
		       const int                numcnty,
		       const county_struct      *cnty)
{
   int	grpcnt, fp_in_grp_cnt;
   int	j;
   int	grpindex, fpindex;
   int	slen1, slen2;
   int	include_group, include_fp;
 
   
   /* loop on the number of groups included */
   
   grpcnt = 0;
   
   for (grpindex = 0; grpcnt < misc->numgrps_included; grpindex++)
   {
      if (misc->grps_included[grpindex] == TRUE)
      {
	 grpcnt++;
	 
	 /* if an NWR product, check if the group has included 
	    points within the tower area. */
	 
	 if (pcc->product.nwr_flag)
	 {
	    include_group = check_grp_in_tower(grp, grpindex, fp, misc);	    
	 }
	 else
	    include_group = TRUE;
	 
	 
	 if (include_group)
	 {	    
	    if (strlen(longstring) > 0) strcat(longstring, "...");
	    
	    strcat(longstring, grp[grpindex].name);
	    strcat(longstring, " at "); 
	    
	    fp_in_grp_cnt    = 0;
	    
	    for (j = 0; j < grp[grpindex].numfps; j++)
	    {
	       fpindex = grp[grpindex].fpindex[j];
	       
	       if (misc->fps_included[fpindex] == TRUE)
	       {
		  if (pcc->product.nwr_flag)
		     include_fp = check_loc_in_tower(fp[fpindex].id,
						     misc->nwrtransPtr, 
						     misc->loctransPtr);
		  else
		     include_fp = TRUE;
		  
		  if (include_fp)
		  {
		     slen1 = strlen(fp[fpindex].name);
		     slen2 = strlen(longstring);
		     
		     fp_in_grp_cnt++;
		     
		     if ((slen1 + slen2 + 5) < MAXLEN_LONGSTR)
		     {	       
			if (fp_in_grp_cnt > 1) strcat(longstring, "...");
			strcat(longstring, fp[fpindex].name);
		     }
		     
		     else
		     {
			log_msg(EXCEED_LONGSTR, "for <GrpsFPList>");
			return;
		     }
		  }
	       }
	    }  /* end of if on whether to include group if an NWR product */
	 }     /* end of loop on forecast points in group */	 
      }        /* end of if block on whether group included */
   }           /* end of loop on forecast groups */
   
      
   return;
}


/*********************************************************************
   load_location_str()
   
   PURPOSE	
   
   ********************************************************************/
char * load_location_str(const	char 	*lid,
			 char		*fieldname)
{
   Location	*locPtr = NULL;
   State	*statePtr = NULL;
   char 	where[60];
   static char	retstr[80];
   
   
   strcpy(retstr, "MISSING");
   
   sprintf(where, " WHERE lid = '%s' ", lid); 
   locPtr = GetLocation(where);
   
   if (locPtr != NULL)
   {
      if (strcasecmp(fieldname, "ID") == 0)
	 strcpy(retstr, locPtr->lid);
      
      else if (strcasecmp(fieldname, "IDNAME") == 0)
	 strcpy(retstr, locPtr->name);
      
      else if (strcasecmp(fieldname, "COUNTY") == 0)
	 strcpy(retstr, locPtr->county);
      
      else if (strcasecmp(fieldname, "STATEID") == 0)
	 strcpy(retstr, locPtr->state);
      
      else if (strcasecmp(fieldname, "STATENAME") == 0)
      {
	 sprintf(where, " WHERE state = (select state from location "
		 " where lid='%s' ) ", lid);
	 statePtr = GetState(where);
	 if (statePtr != NULL)
	 {
	    strcpy(retstr, statePtr->name);
	    FreeState(statePtr);
	 }
      }
      
      else
	 fprintf(stderr, "Invalid field in call to load_location_str().\n");
      
      
      FreeLocation(locPtr);
   }
   
   return(retstr);
}

   
/*********************************************************************
   load_riverstat_str
   
   PURPOSE	
   
   ********************************************************************/
char * load_riverstat_str(const char 	*lid, 
			  char		*fieldname)
{
   Riverstat	*rsPtr = NULL;
   char 	where[60];
   static char	retstr[80];
   
   
   strcpy(retstr, "MISSING");
   
   sprintf(where, " WHERE lid = '%s' ", lid); 
   rsPtr = GetRiverstat(where);
   
   if (rsPtr != NULL)
   {
      if (strcasecmp(fieldname, "RIVER") == 0)
	 strcpy(retstr, rsPtr->stream);
      else
	 fprintf(stderr, "Invalid field in call to load_riverstat_str().\n");
      
      FreeRiverstat(rsPtr);
   }
   
   
   return(retstr);
}


/*********************************************************************
   load_riverstat_float()
   
   PURPOSE	
   
   ********************************************************************/
float load_riverstat_float(const char   *lid,
			   char		*fieldname)
{
   Riverstat	*rsPtr = NULL;
   char 	where[60];
   float	value;
   
   
   /* initialize */
   
   value = (float )MISSINGVAL;
   
   sprintf(where, " WHERE lid= '%s' ", lid); 
   rsPtr = GetRiverstat(where);
   
   if (rsPtr != NULL)
   {
      if (strcasecmp(fieldname, "ZDATUM") == 0)
      {
	 if (IsNull(DOUBLE, &rsPtr->zd) == NOTNULL)
	    value = rsPtr->zd;
      }
      
      else if (strcasecmp(fieldname, "BANKSTG") == 0)
      {
	 if (IsNull(DOUBLE, &rsPtr->bf) == NOTNULL)
	    value = rsPtr->bf;
      }
      
      else if (strcasecmp(fieldname, "WSTG") == 0)
      {
	 if (IsNull(DOUBLE, &rsPtr->wstg) == NOTNULL)
	    value = rsPtr->wstg;
      }
      
      else if (strcasecmp(fieldname, "FLDSTG") == 0)
      {
	 if (IsNull(DOUBLE, &rsPtr->fs) == NOTNULL)
	    value = rsPtr->fs;
      }
      
      else if (strcasecmp(fieldname, "FLDFLOW") == 0)
      {
	 if (IsNull(DOUBLE, &rsPtr->fq) == NOTNULL)
	    value = rsPtr->fq;
      }
      else
	 fprintf(stderr, "Invalid field in call to load_riverstat_float().\n");
      
      FreeRiverstat(rsPtr);
   }
   
      
   return(value);
}


/*********************************************************************
   load_descrip_str()
   
   PURPOSE	
   
   ********************************************************************/
char * load_descrip_str(const char 	*lid, 
			char		*fieldname)
{
   Descrip	*desPtr = NULL;
   char 	where[60];
   static char	retstr[80];
   
   
   strcpy(retstr, "MISSING");
   
   sprintf(where, " WHERE lid = '%s' ", lid); 
   desPtr = GetDescrip(where);
   
   if (desPtr != NULL)
   {
      if (strcasecmp(fieldname, "REACH") == 0)
	 strcpy(retstr, desPtr->reach);
      
      else if (strcasecmp(fieldname, "PROXIMITY") == 0)
	 strcpy(retstr, desPtr->proximity);
      
      else
	 fprintf(stderr, "Invalid field in call to load_descrip_str().\n");
      
      FreeDescrip(desPtr);
   }
   
   
   return(retstr);
}


/*********************************************************************
   alpha_compare()
   
   PURPOSE
   
   ********************************************************************/
int alpha_compare(const void *name1, 
                  const void *name2)
{
   int result;
   char ** n1 = (char **) name1;
   char ** n2 = (char **) name2;

   result = strcmp(*n1, *n2); 
     
   return(result);
}


/*********************************************************************
  getLocTransmitPtr()
  
  set up object retrieval mechanism to allow one-time 
  loading of data.
   
  *********************************************************************/

LocTransmit * getLocTransmitPtr()
{
   static int first = TRUE;
   static LocTransmit *ltPtr = NULL;
   
   if (first)
   {
      ltPtr = GetLocTransmit("");
      first = FALSE;
   }
   
   return ltPtr;
}


/*****************************************************************
  load_action_str()

  load action code from vtecinfo structure which is specific for 
  forecast point,  this info will be stored in template variable <Action>.
  
******************************************************************/
char * load_action_str(const int         fpindex,
                      vtecinfo_struct  *vtecinfo,
		      const pcc_struct  *pcc)
{
   static char   retstr[VTEC_ACTION_LEN + 1];
   
   
   if (vtecinfo[fpindex].action != NULL)
   {
      /*point specific*/
   
     if (pcc->product.segment_mode == SEGMENT_MODE_POINT)
     {
	if (strcmp(vtecinfo[fpindex].action, "CAN") == 0)
           strcpy(retstr, "CAN");
	else if (strcmp(vtecinfo[fpindex].action, "CON") == 0)
           strcpy(retstr, "CON");
	else if (strcmp(vtecinfo[fpindex].action, "EXA") == 0)
           strcpy(retstr, "EXA");
	else if (strcmp(vtecinfo[fpindex].action, "EXT") == 0)
           strcpy(retstr, "EXT");
	else if (strcmp(vtecinfo[fpindex].action, "EXB") == 0)
           strcpy(retstr, "EXB");	
	else if (strcmp(vtecinfo[fpindex].action, "EXP") == 0)
           strcpy(retstr, "EXP");
	else if (strcmp(vtecinfo[fpindex].action, "NEW") == 0)
           strcpy(retstr, "NEW");
	else if (strcmp(vtecinfo[fpindex].action, "UPG") == 0)
           strcpy(retstr, "UPG");
	else if (strcmp(vtecinfo[fpindex].action, "ROU") == 0)
           strcpy(retstr, "ROU");	 	  	 	  
	else if (strcmp(vtecinfo[fpindex].action, "COR") == 0)
           strcpy(retstr, "COR");	 	  	 	  
	else
        {
           strcpy(retstr, "MSG");
           log_msg("", "Warning - Invalid action code.\n");
        }  
     }
     
     else
     {  
        /* set template variable <Action> as MSG if not Point specific
	   segmentation mode */
	       	      		 
        strcpy(retstr, "MSG");
	log_msg("", "Warning - <Action> is set MSG if not segment by forecast point.\n");
     } 
     
     /* set template variable <Action> as MSG if not vtec mode*/
     
     if (pcc->product.vtec_flag != TRUE)
     {
        strcpy(retstr, "MSG");
	log_msg("", "Warning - <Action> is set MSG if not VTEC mode.\n");
     }	
    	  
   }
   
   else
   {	  
      strcpy(retstr, "MSG");
      log_msg("", "Warning - Invalid action code.\n");	  		      
   }  
   
   return(retstr);
}     


/*****************************************************************
load_hsaname_str()

PURPOSE: Load field "ofc" from Admin table for the variable <OfficeName>

******************************************************************/
char * load_officename_str(char  *fieldname)
{ 
   Admin       *adminPtr = NULL;
   char 	where[60];
   static char	retstr[80];
   
   
   strcpy(retstr, "MISSING");
   
   sprintf(where, " "); 
   adminPtr = GetAdmin(where);
   
   if (adminPtr != NULL)
   {
      if (strcasecmp(fieldname, "OFFICENAME") == 0)
      {
         if (adminPtr->ofc != NULL)
	    strcpy(retstr, adminPtr->ofc);
      }	    
      else
	 fprintf(stderr, "Invalid field in call to load_officename_str().\n");
      
      FreeAdmin(adminPtr);
   }
   
   return(retstr);
}   


/*****************************************************************

load_stgflow_name()

PURPOSE: Determine whether using "flow" or "stage" based on the 
primary pe from RiverStat table for the variable <StgFlowName>
 
******************************************************************/
char * load_stgflow_name(char *locid)
{ 
   static char	retstr[80];
   
   Riverstat   *riverstatPtr = NULL;
   char 	where[100];
   char		msgstr[100];
  
   
   /* default value */
   
   strcpy(retstr, "stage");
   
   
   /* get the value */
      
   sprintf(where, " WHERE lid = '%s' ", locid); 
   riverstatPtr = GetRiverstat(where);
   
   if (riverstatPtr != NULL)
   {
      if (riverstatPtr->primary_pe[0] == 'Q')
	 strcpy(retstr, "flow");
      else
	 strcpy(retstr, "stage");
	       
      FreeRiverstat(riverstatPtr);
   }
   
   else
   {
     sprintf(msgstr, "Location id %s is not found in RiverStat table.", locid);
     log_msg("", msgstr);
   }
  
   
   return(retstr);
}


/*****************************************************************

load_stgflow_units()

PURPOSE: Determine the units for "flow" or "stage" base on the primary
pe from RiverStat table for the variable <StgFlowUnits>

******************************************************************/
char * load_stgflow_units(char *locid)
{ 
   static char	retstr[80];
   Riverstat   *riverstatPtr = NULL;
   char 	where[100];
   char 	msgstr[100];
   
   
   /* default */
   
   strcpy(retstr, "feet");
      
      
   /* get the value */
      
   sprintf(where, " WHERE lid = '%s' ", locid); 
   riverstatPtr = GetRiverstat(where);
   
   if (riverstatPtr != NULL)
   {
      if (riverstatPtr->primary_pe[0] == 'Q')
	 strcpy(retstr, "cfs");
      else
	 strcpy(retstr, "feet");
	       
      FreeRiverstat(riverstatPtr);
   }
   
   else
   {
     sprintf(msgstr, "Location id %s is not found in RiverStat table.", locid); 
     log_msg("", msgstr);
   }
  
   
   return(retstr);
}


/*****************************************************************

load_loclat()

PURPOSE: Load the latitude for the location from RiverStat table
 for the variable <locLat>

******************************************************************/
float  load_loclat(char *locid)
{    
   float        retval;
   Riverstat   *riverstatPtr = NULL;
   char 	where[100];
   char 	msgstr[100];   
   
   /* default */
      
   retval = (float) MISSINGVAL;
             
   /* get the latitude */
      
   sprintf(where, " WHERE lid = '%s' ", locid); 
   riverstatPtr = GetRiverstat(where);
   
   if (riverstatPtr != NULL)
   {      
      retval = riverstatPtr->lat;     	       
      FreeRiverstat(riverstatPtr);
   }
   
   else
   {
     sprintf(msgstr, "Latitude for location %s is not found in RiverStat table.", locid); 
     log_msg("", msgstr);
   }
     
   return(retval);
}

/*****************************************************************

load_loclon()

PURPOSE: Load the longitude for the location from RiverStat table
 for the variable <locLon>

******************************************************************/
float load_loclon(char *locid)
{    
   float        retval;
   Riverstat   *riverstatPtr = NULL;
   char 	where[100];
   char 	msgstr[100];   
   
   /* default */
      
   retval = (float) MISSINGVAL;
            
   /* get the latitude */
      
   sprintf(where, " WHERE lid = '%s' ", locid); 
   riverstatPtr = GetRiverstat(where);
   
   if (riverstatPtr != NULL)
   {      
       retval = riverstatPtr->lon;
       FreeRiverstat(riverstatPtr);
   }
   
   else
   {
     sprintf(msgstr, "Longitude for location %s is not found in RiverStat table.", locid); 
     log_msg("", msgstr);
   }
     
   return(retval);
}


/*****************************************************************

load_eventtime_str()

Create the time range phrase for variable <EventTime> by using from
<EventStartTime> to <EventEndTime>. Only T_WWA format can be allowed.

******************************************************************/
char * load_eventtime_str(const int               fpindex,
                                vtecinfo_struct   *vtecinfo,
	      	          const pcc_struct        *pcc,
			        misc_struct       *misc)
{
   static char retstr[MAXLEN_STRING];
   char        dtstr_start[MAXLEN_STRING];
   char        dtstr_end[MAXLEN_STRING];
   time_t      eventstarttime;
   time_t      eventendtime;
   
   
   /* null the datetime string */
   
   memset(dtstr_start, 0, MAXLEN_STRING);
   memset(dtstr_end, 0, MAXLEN_STRING);
   memset(retstr, 0, MAXLEN_STRING);
   
   if (pcc->product.segment_mode == SEGMENT_MODE_POINT)
   {
      if (vtecinfo[fpindex].begintime == CURRENT_TIME)
         eventstarttime = misc->system_time;
      else 	 
         eventstarttime = vtecinfo[fpindex].begintime;
      
      eventendtime = vtecinfo[fpindex].endtime;
      
      if (eventstarttime == MISSINGVAL)
         strcpy(dtstr_start, "MSG");
      else	 
         format_wwa_timephrase(misc, eventstarttime, dtstr_start);
      
      if (eventendtime == MISSINGVAL)
         strcpy(dtstr_end, "FURTHER NOTICE");
      else 	 	 
         format_wwa_timephrase(misc, eventendtime, dtstr_end);
      
      /* if the event becomes effective within 3 hours from the issuance time, 
         just use UNTIL */
	 
      if ((eventstarttime <= (misc->system_time + 3*3600) &&
         (eventstarttime >= misc->system_time)) || (eventstarttime ==
	 MISSINGVAL))
      {
         strcpy(retstr, "UNTIL ");	 
      }
      else	
      { 	 
         strcpy(retstr, "FROM ");
         strcat(retstr, dtstr_start);
         if (eventendtime == MISSINGVAL)
           strcat(retstr, " UNTIL ");
         else  	
           strcat(retstr, " TO ");
      }	   
      strcat(retstr, dtstr_end);       
                 
      /* if the begin time and end time phrase are the same */
      
      if (dtstr_start != NULL && dtstr_end != NULL &&
          strcmp(dtstr_start, dtstr_end) == 0 )
      {
      	strcpy(retstr, "DURING ");
      	strcat(retstr, dtstr_start);      
      }	
      	         
   }
   
   else
   {
      /* set template variable <EventTime> as blank for non-point specific
         segmentation mode */
	       	      		 
      strcpy(retstr, "MSG");
      log_msg("", "Warning - <EventTime> is set MSG for segmented by "
                  "group,county modes and non-segment mode.\n");
   } 
     
  
   return(retstr);
}   	  	 	 


/*********************************************************************
   build_loccntylist()
   
   PURPOSE
   Build the list of counties for a forecast point based on event.
   
  
 ********************************************************************/
void build_loccntylist(char	*locid,		       
		       char	*longstring)
{
   int 		i, j;
   int		numitems, slen;
   char 	*county[MAX_COUNTIES];
   int 		numlist, matchfound;   
   Countynum 	*countynumPtr = NULL, *countynum = NULL;
   char		where[MAXLEN_STRING];
   char 	teststr[MAXLEN_STRING];
   int          state_la_found = FALSE;
   int          state_nonla_found = FALSE;
   char         stateid[STATE_LEN+1];
   
   /* initialize */
   
   memset(longstring, 0, MAXLEN_LONGSTR);
   numlist = 0;
   strcpy(stateid, "LA");  
      	    	    
   /* load the counties that are defined for this location */

   sprintf(where, " WHERE lid = '%s' ", locid);

   countynumPtr = GetCountynum(where);
   if (countynumPtr == NULL)
	numitems = 0;
   else
	numitems = ListCount(&countynumPtr->list);

   if (numitems <= 0)
       log_msg(NO_COUNTYNUM, locid);

   else
   {
       /* get the info for this entry and loop on the
	  number defined for the location*/

       countynum = (Countynum *) ListFirst(&countynumPtr->list);
       while (countynum != NULL && numlist + 1 < MAX_COUNTIES)
       {
	   /* check if there is an existing entry for this state
	      county combination */

	   sprintf(teststr, "%s", countynum->county);
	   
	   /* check if the state is Louisiana and if only one state or
	      more than one state */
	      
	   if (strcasecmp(countynum->state, stateid) == 0)
	   {
	      if (strcasecmp(countynum->state, "LA") == 0)
   	         state_la_found = TRUE;	   				 
	   }
	   else 	   
	      state_nonla_found = TRUE;
	         
	   strcpy(stateid, countynum->state);  

	   matchfound = FALSE;
	   for (j = 0; j < numlist; j++)
	   {
	      if (strcmp(teststr, county[j]) == 0)
	      {
		 matchfound = TRUE;
		 break;
	      }
	   }


	   /* only load new info if it isn't loaded already */

	   if (matchfound == FALSE)
	   {						
	      /* allocate and store the state and county name */

	      slen = strlen(teststr);			
	      county[numlist] = (char *)malloc(slen + 1);
	      if (county[numlist] == NULL)
		 log_msg(FAILED_MALLOC, "for county in build_loccntylist");
		 
	      if (teststr != NULL)		
	         strcpy(county[numlist], teststr);    

	      numlist++;
	   }


	  /* get the next entry for the location */

	  countynum = (Countynum *) ListNext(&countynum->node);    
       }

       /* free the memory for this forecast point */

       FreeCountynum(countynumPtr);

   }
   
   
   /* build the string */
   
   if (numlist != 0)
   { 
      for (j = 0; j < numlist; j++)
      {
         if (county[j] != NULL)
	 {
	    /* special handle for state as Louisiana */
	    
	    if (state_la_found == TRUE)
	    {
	       /*only LA state */
	       
	       if (state_nonla_found == FALSE)
	       {
	          if (j == 0)
		  {
		     strcpy(longstring, county[j]); 
		     if (numlist == 1)
	        	strcat(longstring, " PARISH");
		  }
		  else
		  {
		     if (j == numlist - 1 )
		     {
	        	 strcat(longstring, " AND ");
			 strcat(longstring, county[j]);
			 strcat(longstring, " PARISHES");
		      }
		      else
		      {   
	        	 strcat(longstring, "...");  
                	 strcat(longstring, county[j]);
	              }
		  }  /* end of more than one county */
	       } /* end of state_nonla_found = FALSE */
	       
	       else
	       {
	          if (j == 0)
		  {
		     strcpy(longstring, county[j]); 
		     if (numlist == 1)
	        	strcat(longstring, " COUNTY/PARISH");
		  }
	          else
		  {
		     if (j == numlist - 1 )
		     {
	        	 strcat(longstring, " AND ");
			 strcat(longstring, county[j]);
			 strcat(longstring, " COUNTIES/PARISHES");
		      }
		      else
		      {   
	        	 strcat(longstring, "...");  
                	 strcat(longstring, county[j]);
	              }
		  }  /* end of more than one county */	       	       
	       }  /*end of state_nonla_found = TRUE */	
               	        	    
	    } /* if state LA is found */
	    else
	    { 
	      if (j == 0)
	      {
        	strcpy(longstring, county[j]); 
		if (numlist == 1)
	           strcat(longstring, " COUNTY");

	      }  
              else
	      {
		 if (j == numlist - 1 )
		 {
	             strcat(longstring, " AND ");
		     strcat(longstring, county[j]);
		     strcat(longstring, " COUNTIES");
		  }
		  else
		  {   
	             strcat(longstring, "...");  
                     strcat(longstring, county[j]);
	          }
	      }  /* end of more than one county */
	    }
	 }     /* end of county is found */
      }        /* end of loop of counties */
   }
   
   
   /* check that the string length was not exceeded */
   
   i = strlen(longstring);
   if (i > MAXLEN_LONGSTR - 1) log_msg(EXCEED_LONGSTR, "");   	    	
   
   
   /* free the allocated memory */
   
   for (i = 0; i < numlist; i++)
      free(county[i]);
      
   
   return;
}

/*********************************************************************
   build_statecntylist_byaction()
   
   PURPOSE
   Build the list of counties in a state for forecast points included
   in a same act-ion event. For example "in statename...countyname1...
   countyname2 AND countynameN"
   
   NOTES
   
   
   ********************************************************************/
void build_statecntylist_byaction(const int    		fpindex1,
	                          const fp_struct	*fp,
			          const grp_struct      *grp,
			                misc_struct     *misc,
			          const pcc_struct      *pcc,
			                vtecinfo_struct  *vtecinfo,		       
			                char	  	 *longstring)
{   
   int 		i, j;
   int		numitems, slen;
   int 		grpindex, fpindex, cnt;
   char 	*statecounty[MAX_COUNTIES];
   int 		numlist, matchfound;   
   Countynum 	*countynumPtr = NULL, *countynum = NULL;
   char		where[MAXLEN_STRING];
   char 	teststr[MAXLEN_STRING];
   int		include_fp;
   int          state_la_found = FALSE;
   
   /* initialize */
   
   memset(longstring, 0, MAXLEN_LONGSTR);
   numlist = 0;
   
   
   /* loop on all the forecast points;  this loop works in a 
      unique way since numfps variable is not available; it uses
      the grp structure and num grps included information instead */
   
   for (cnt = grpindex = 0; cnt < misc->numgrps_included; grpindex++)
   {
      if (misc->grps_included[grpindex] == TRUE)
      {
	 cnt++;
	 
	 for (i = 0; i < grp[grpindex].numfps; i++)
	 {
	    fpindex = grp[grpindex].fpindex[i];
	    
	    /* check if the point is included if this is an NWR product. */
	    
	    if (pcc->product.nwr_flag)
	       include_fp = check_loc_in_tower(fp[fpindex].id,
					       misc->nwrtransPtr, 
					       misc->loctransPtr);
	    /*check if the point has the same action with the one with fpindex*/
	    
	    else if (strcmp(vtecinfo[fpindex1].action, vtecinfo[fpindex].action) != 0)
	       include_fp = FALSE;
	    				       
	    else
	       include_fp = TRUE;
	    
	    /* only consider if the forecast point is included */
	    
	    if (misc->fps_included[fpindex] == TRUE && include_fp)
	    {		 
	       /* load the counties that are defined for this forecast point*/
	       
	       sprintf(where, " WHERE lid = '%s' ", fp[fpindex].id);
	       
	       countynumPtr = GetCountynum(where);
	       if (countynumPtr == NULL)
		   numitems = 0;
	       else
		   numitems = ListCount(&countynumPtr->list);
	       
	       if (numitems <= 0)
		  log_msg(NO_COUNTYNUM, fp[fpindex].id);
	       
	       else
	       {
		  /* get the info for this entry and loop on the
		     number defined for the forecast point */
		  
		  countynum = (Countynum *) ListFirst(&countynumPtr->list);		  		  
		    
		  while (countynum != NULL && numlist + 1 < MAX_COUNTIES)
		  {
		     /* check if the state is Louisiana, if it is, change word "county"
		      or "counties" to "parish" or "parishes"*/
		   		     		     
		     if (strcasecmp(countynum->state, "LA") == 0)
		        state_la_found = TRUE;   					      
			
		     /* check if there is an existing entry for this state
			county combination */
		     
		     sprintf(teststr, "%s %s", 
			     countynum->state, countynum->county);
		     
		     matchfound = FALSE;
		     for (j = 0; j < numlist; j++)
		     {
			if (strcmp(teststr, statecounty[j]) == 0)
			{
			   matchfound = TRUE;
			   break;
			}
		     }
		     
		     
		     /* only load new info if it isn't loaded already */
		     
		     if (matchfound == FALSE)
		     {						
			/* allocate and store the state and county name */
			
			slen = strlen(teststr);			
			statecounty[numlist] = (char *)malloc(slen + 1);
			if (statecounty[numlist] == NULL)
			   log_msg(FAILED_MALLOC, 
				   "for statecounty in build_locstatecntylist");
		        if (teststr != NULL)		   
			   strcpy(statecounty[numlist], teststr);    
			
			numlist++;
		     }
		     
		     
		     /* get the next entry for the forecast point */
		     
		     countynum = (Countynum *) ListNext(&countynum->node);    
		  }
		  
		  /* free the memory for this forecast point */
		  
		  FreeCountynum(countynumPtr);
		  
	       }     /* end of if on whether info found for forecast point */	 
	    }        /* end of if on whether forecast point included */
	 }           /* end of for loop on forecast points */
      } /* end of if on whether current group is included */
   }    /* end of loop on included groups */
   
   
   /* at this point, we know the number of states and their ids, and for
      each state, there is a list of counties for the state;
      now build the string */
    
   write_statecounties(numlist, statecounty, longstring, state_la_found);
   
   
   /* free the allocated memory */
   
   for (i = 0; i < numlist; i++)
      free(statecounty[i]);
   
   return;
}


/*********************************************************************
   write_statecounties()
   
   PURPOSE
   Write the list of counties given the state and counties 
   that are included. There is special handle to the state
   is Louisiana.
   
   NOTES
   The state and county are stored in the same string, with the 
   two-character state, a space, and then the county name.
   
   ********************************************************************/
void write_statecounties(const	int	numlist,
	                 char 	        *statecounty[],
	       	         char	        *longstring,
			 int            state_al_found)
{	    
   
   int 		i, j;
   int 		pos;
   char 	*spaceloc= NULL;
   int 		curcount;
   char 	*statenames[MAX_UGCSTATES];
   char 	*stateabbs[MAX_UGCSTATES];
   State 	*statePtr = NULL;
   int 		numstates;
   int 		foundflag;
   char 	where[MAXLEN_STRING];
   char 	tempstr[MAXLEN_STRING];

   
   /* sort the list of states and associated counties */
   
   qsort((char *)(statecounty), (size_t)numlist,
	 (size_t)sizeof(char *), alpha_compare);
   
   
   /* determine the number of states in the list and load
      their abbreviated names and their full names */
   
   numstates = 0;
   for (i = 0; i < numlist; i++)
   {
      /* loop on the number of states already identified,
	 use the state abbreviation to search, not the full name
	 which may have had its case converted */
      
      foundflag = FALSE;
      for (j = 0; j < numstates; j++)
      {
	 if (strncmp(statecounty[i], stateabbs[j], 2) == 0)
	 {
	    foundflag = TRUE;
	    break;
	 }
      }
      
      
      /* if entry not found, then get the full name of the state and 
	 add it to the list */
      
      if (foundflag == FALSE)
      {
	 memset(tempstr, 0, MAXLEN_STRING);
	 strncpy(tempstr, statecounty[i], 2);
	 sprintf(where, " WHERE state = '%s' ", tempstr); 
	 statePtr = GetState(where);
	 
	 if (statePtr != NULL)
	 {
	    statenames[numstates] = (char *)malloc(strlen(statePtr->name) + 1);
	    if (statenames[numstates] == NULL)
	       log_msg(FAILED_MALLOC, "for statenames[] in write_statecounties");
	     
	    if (statePtr->name != NULL)  
	       strcpy(statenames[numstates], statePtr->name);
	    
	    stateabbs[numstates] = (char *)malloc(strlen(tempstr + 1));
	    if (stateabbs[numstates] == NULL)
	       log_msg(FAILED_MALLOC, "for stateabbs[] in write_statecounties");
	     
	    if (tempstr != NULL)   
	       strcpy(stateabbs[numstates], tempstr);
	    
	    numstates++;
	 }
      }
   }
   
   
   /* loop on the number of states */
   
   pos = 0;
   
   for (i = 0; i < numstates; i++)
   {
      /* special handle for state as Louisiana */
      
      if (state_al_found == TRUE)
      {
         /* if only one state and this state is LA */
	 
	 if (numstates == 1)
	 {
	    if (i == 0)
	    {
	       strcat(longstring, "parishes in ");
	       pos = pos + 13;
	    }
	    else
	    {
	       strcat(longstring, " and in ");	       
	       pos = pos + 8;
	    
	    }
	 }
	 /* if state AL is one of the state */
	 
	 else 
	 {
	    if ( i == 0 )
	    {
	       strcat(longstring, "counties/parishes in ");
	       pos = pos + 22;
            }
	    else
	    {
	       strcat(longstring, " and in ");	       
	       pos = pos + 8;
	    }
	 }	       
      } /* end of special handle for LA state */
      else
      {
	/* first load the leading phrase giving the state name.
	   always use mixed case since we can convert to upper 
	   case later. */

	if (i == 0 || numstates == 0)
	{
	   strcat(longstring, "counties in ");

	   pos = pos + 13;
	}

	else
	{
	   strcat(longstring, " and in ");	       
	   pos = pos + 8;
	}
      }
      
      /* assume displaying mixed case, so convert all but the first
	 character of what may be a multi-word state name to lowercase.
	 if using upper case, then convert the text just before write. */
      
      if (statenames[i] != NULL)
         strcpy(tempstr, statenames[i]);
      
      convert_str_to_lowcase(tempstr);
      tempstr[0] = toupper(tempstr[0]);
      if ((spaceloc = strstr(tempstr, " ")) != NULL)
      {
	 j = strlen(statenames[i]) - strlen(spaceloc) + 1;
	 tempstr[j] = toupper(tempstr[j]);
      }
      
      
      /* now load in the state string */
      
      strcat(longstring, tempstr);
      pos = pos + strlen(tempstr);
      strcat(longstring, "...");
      pos = 2;
      
      
      /* loop thru all the counties.  for those
	 that match the state load them now. */      
      
      curcount = 0;
      for (j = 0; j < numlist; j++)
      {
	 memset(tempstr, 0, MAXLEN_STRING);
	 strncpy(tempstr, statecounty[j], 2);
	 
	 if (strcmp(tempstr, stateabbs[i]) == 0)
	 {
	    if (curcount != 0)
	    {
	       strcat(longstring, "...");
	       pos = pos + 3;
	    }
	    curcount++;
	    
	    if (statecounty[j] != NULL)   
	       strcpy(tempstr, statecounty[j]);
	    strcat(longstring, &tempstr[3]);
	    pos = pos + strlen(statecounty[j] - 3);
	    
	 }
      }             
   } 
   
   
   /* check that the string length was not exceeded */
   
   i = strlen(longstring);
   if (i > MAXLEN_LONGSTR - 1) log_msg(EXCEED_LONGSTR, "");
   
   
   return;
}

/*********************************************************************
   build_statelist_byaction()
   
   PURPOSE
   Build the list of states for forecast points included
   in a same action event. For example "in statename1...statename2"
   
   NOTES
   
   
   ********************************************************************/
void build_statelist_byaction(const int    	     fpindex1,
	                      const fp_struct	     *fp,
			      const grp_struct       *grp,
			            misc_struct      *misc,
			      const pcc_struct       *pcc,
			      vtecinfo_struct        *vtecinfo,		       
			      char	  	     *longstring)
{   
   int 		i, j;
   int		numitems, slen;
   int 		grpindex, fpindex, cnt;
   char 	*state[MAX_COUNTIES];
   int 		numlist, matchfound;   
   Countynum 	*countynumPtr = NULL, *countynum = NULL;
   char		where[MAXLEN_STRING];
   char 	teststr[MAXLEN_STRING];
   int		include_fp;
   char 	*statenames[MAX_UGCSTATES];
   State 	*statePtr = NULL;
   int          curcount, pos;
   
   /* initialize */
   
   memset(longstring, 0, MAXLEN_LONGSTR);
   numlist = 0;
   pos = 0;
   
   /* loop on all the forecast points;  this loop works in a 
      unique way since numfps variable is not available; it uses
      the grp structure and num grps included information instead */
   
   for (cnt = grpindex = 0; cnt < misc->numgrps_included; grpindex++)
   {
      if (misc->grps_included[grpindex] == TRUE)
      {
	 cnt++;
	 
	 for (i = 0; i < grp[grpindex].numfps; i++)
	 {
	    fpindex = grp[grpindex].fpindex[i];
	    
	    /* check if the point is included if this is an NWR product. */
	    
	    if (pcc->product.nwr_flag)
	       include_fp = check_loc_in_tower(fp[fpindex].id,
					       misc->nwrtransPtr, 
					       misc->loctransPtr);
	    /*check if the point has the same action with the one with fpindex*/
	    
	    else if (strcmp(vtecinfo[fpindex1].action, vtecinfo[fpindex].action) != 0)
	       include_fp = FALSE;
	    				       
	    else
	       include_fp = TRUE;
	    
	    /* only consider if the forecast point is included */
	    
	    if (misc->fps_included[fpindex] == TRUE && include_fp)
	    {		 
	       /* load the counties that are defined for this forecast point*/
	       
	       sprintf(where, " WHERE lid = '%s' ", fp[fpindex].id);
	       
	       countynumPtr = GetCountynum(where);
	       if (countynumPtr == NULL)
		   numitems = 0;
	       else
		   numitems = ListCount(&countynumPtr->list);
	       
	       if (numitems <= 0)
		  log_msg(NO_COUNTYNUM, fp[fpindex].id);
	       
	       else
	       {
		  /* get the info for this entry and loop on the
		     number defined for the forecast point */
		  
		  countynum = (Countynum *) ListFirst(&countynumPtr->list);
		  while (countynum != NULL && numlist + 1 < MAX_COUNTIES)
		  {
		     /* check if there is an existing entry for this state
			county combination */
		     
		     sprintf(teststr, "%s", countynum->state);
		     
		     matchfound = FALSE;
		     for (j = 0; j < numlist; j++)
		     {
			if (strcmp(teststr, state[j]) == 0)
			{
			   matchfound = TRUE;
			   break;
			}
		     }
		     
		     
		     /* only load new info if it isn't loaded already */
		     
		     if (matchfound == FALSE)
		     {						
			/* allocate and store the state and county name */
			
			slen = strlen(teststr);			
			state[numlist] = (char *)malloc(slen + 1);
			if (state[numlist] == NULL)
			   log_msg(FAILED_MALLOC, 
				   "for state in build_statecntylist_byaction");
		        if (teststr != NULL)		   
			   strcpy(state[numlist], teststr);    
			
			numlist++;
		     }
		     
		     
		     /* get the next entry for the forecast point */
		     
		     countynum = (Countynum *) ListNext(&countynum->node);    
		  }
		  
		  /* free the memory for this forecast point */
		  
		  FreeCountynum(countynumPtr);
		  
	       }     /* end of if on whether info found for forecast point */	 
	    }        /* end of if on whether forecast point included */
	 }           /* end of for loop on forecast points */
      } /* end of if on whether current group is included */
   }    /* end of loop on included groups */
   
   /*build the string */
   
   curcount = 0;
       
   for (j = 0; j < numlist; j++)
   {
      if (state[j] != NULL)
      {
	 sprintf(where, " WHERE state = '%s' ", state[j]); 
	 statePtr = GetState(where);

	 if (statePtr != NULL && statePtr->name != NULL)
	 {
	     statenames[j] = (char *)malloc(strlen(statePtr->name) + 1);

	     if (statenames[j] == NULL)
	        log_msg(FAILED_MALLOC, "for statenames[] in build_statelist_byaction");

	     if (statePtr->name != NULL)
	        strcpy(statenames[j], statePtr->name);

	     if (statenames[j] != NULL)
	     {
		 if(curcount != 0)
	         {	               
	             strcat(longstring, "...");
	             pos = pos + 3;
                 } 
                 curcount++;

                 strcat(longstring, statenames[j]);
                 pos = pos + strlen(statenames[j]);      

	      }       	 
	 }  	      
      }	
   }    
   
   
   /* check that the string length was not exceeded */
   
   i = strlen(longstring);
   if (i > MAXLEN_LONGSTR - 1) log_msg(EXCEED_LONGSTR, "");
   
   /* free the allocated memory */
   
   for (i = 0; i < numlist; i++)
   {
      if (state[i] != NULL)         
	 free(state[i]);
      if (statenames[i] != NULL)
         free(statenames[i]);
   }
   	       
   return;
}


/*********************************************************
  load_locarea_str()

  PURPOSE
  load locarea field from LocArea table for each point
  
**********************************************************/
char * load_locarea_str(const	char    *lid,
		        char	        *fieldname)
{
   LocArea	*locareaPtr = NULL;
   char 	where[60];
   static char	retstr[SHORT_LEN + 1];
   
   
   strcpy(retstr, "MISSING");
   
   sprintf(where, " WHERE lid = '%s' ", lid); 
   locareaPtr = GetLocArea(where);
   
   if (locareaPtr != NULL)
   {
      if (strcasecmp(fieldname, "AREA") == 0)
      {
         if (locareaPtr->area != NULL)
	    strcpy(retstr, locareaPtr->area);
      }	    
	 
      else
	 fprintf(stderr, "Invalid field in call to load_locarea_str().\n");
      
      
      FreeLocArea(locareaPtr);
   }
   
   return(retstr);
}
 
 
/*********************************************************************
   build_riverlist_byaction()
   
   PURPOSE
   Build the list of rivers for forecast points included
   in a same action event. For example "in rivername1...rivername2"
   
   NOTES
   
   
   ********************************************************************/
void build_riverlist_byaction(const int    	      fpindex1,
	                      const fp_struct	      *fp,
			      const grp_struct        *grp,
			            misc_struct       *misc,
			      const pcc_struct        *pcc,
			      vtecinfo_struct         *vtecinfo,   
			      char	  	      *longstring)			      
{
   int 		i, j;
   int 		numitems, slen;
   int 		grpindex, fpindex, cnt;
   char 	*rivers[MAX_RIVERS];
   int 		numlist, matchfound;   
   Riverstat 	*rsPtr = NULL;
   char 	where[MAXLEN_STRING];
   int		include_fp;
   int          curcount, pos;
   
   /* initialize */
   
   memset(longstring, 0, MAXLEN_LONGSTR);
   numlist = 0;
   pos = 0;
   curcount = 0;
   
   /* loop on all the forecast points;  this loop works in a 
      unique way since numfps variable is not available; it uses
      the grp structure and grps included information instead */
   
   for (cnt = grpindex = 0; cnt < misc->numgrps_included; grpindex++)
   {
      if (misc->grps_included[grpindex] == TRUE)
      {
	 cnt++;
	 
	 for (i = 0; i < grp[grpindex].numfps; i++)
	 {
	    fpindex = grp[grpindex].fpindex[i];
	    
	    
	    /* check if the point is included if this is an NWR product. */
	    
	    if (pcc->product.nwr_flag)
	       include_fp = check_loc_in_tower(fp[fpindex].id,
					       misc->nwrtransPtr, 
	                                       misc->loctransPtr);
					       
	    /*check if the point has the same action with the one with index as fpindex*/
	    
	    else if (strcmp(vtecinfo[fpindex1].action, vtecinfo[fpindex].action) != 0)
	       include_fp = FALSE;				       
	    
	    else
	       include_fp = TRUE;
	    
	    
	    /* only consider if the forecast point is included in the product
	       and is within the tower area, for NWR products. */
	    
	    if (misc->fps_included[fpindex] == TRUE && include_fp)
	    {		 
	       /* load the river that is defined for this forecast point */
	       
	       sprintf(where, " WHERE lid = '%s' ", fp[fpindex].id);
	       
	       rsPtr = GetRiverstat(where);
	       if (rsPtr == NULL)
	       	   numitems =  0;
	       else
	           numitems = ListCount(&rsPtr->list);
	       
	       if (rsPtr != NULL && numlist + 1 < MAX_COUNTIES)
	       {		  
		  /* check if there is an existing entry for this river */
		  
		  matchfound = FALSE;
		  for (j = 0; j < numlist; j++)
		  {
		     if (strcmp(rsPtr->stream, rivers[j]) == 0)
		     {
			matchfound = TRUE;
			break;
		     }
		  }
		  
		  
		  /* only load new info if it isn't loaded already */
		  
		  if (matchfound == FALSE)
		  {						
		     /* allocate and store the river name */
		     
		     slen = strlen(rsPtr->stream);
		     if (slen > 0)
		     {
			rivers[numlist] = (char *)malloc(slen + 1);
			if (rivers[numlist] == NULL)
			   log_msg(FAILED_MALLOC,				   
				   "for rivers in build_riverlist_byaction");
			strcpy(rivers[numlist], rsPtr->stream);    
			
			numlist++;
		     }
		  }
		  
	       }
	       
	       /* free the memory for this forecast point */
	       
	       FreeRiverstat(rsPtr);
	       
	    }        /* end of if on whether forecast point included */
	 }           /* end of for loop on forecast points */
      } /* end of if on whether current group is included */
   }    /* end of loop on included groups */
            
   /* loop on the number of rivers */
      
   for (j = 0; j < numlist; j++)
   {      
      if (curcount != 0)
      {
	 strcat(longstring, "...");
	 pos = pos + 3;
      }
      curcount++;
      
      strcat(longstring, rivers[j]);
      pos = pos + strlen(rivers[j]);      
      
   }
   
   /* at this point, we know the number of states and their ids, and for
      each state, there is a list of counties for the state;
      now build the string */
   
   /*write_rivers(numlist, rivers, longstring);*/
   
   
   /* free the allocated memory */
   
   for (i = 0; i < numlist; i++)
      free(rivers[i]);
   
   return;
}

			      
/*********************************************************************
   load_impcomp_units()
   
   PURPOSE
   This variable value could be ft for stage or cfs for
   discharge depending on rec_type set (PE based or not).
   
********************************************************************/
char *load_impcomp_units(const fp_struct   *fp,   
                         const int         fpindex)
{  
   static char	retstr[80];  
      
   /* default */
   
   strcpy(retstr, "feet");
   
   /*steal this rec_type field to use for determine for impact
   or historical comparison, the value is stage or flow based on
   PE variable or not*/
      
   if (strcmp(fp[fpindex].rec_type, "NPE") == 0)
   {
      if (fp[fpindex].pe[0] == 'Q')      
        strcpy(retstr, "feet");
      else
        strcpy(retstr, "cfs");
   }
   else
   {
     if (fp[fpindex].pe[0] == 'Q')      
        strcpy(retstr, "cfs");
     else
        strcpy(retstr, "feet");		 
   }
 
  return(retstr);



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}     		    		       
	                   
