/*********************************************************************
   build_ugc.c
   
   build_ugc()
   build_ugc_nwr()
   build_ugc_segment()
   build_ugc_segment_county()
   
   write_ugc_areas()
   write_ugc_expiretime()
     
   ********************************************************************/

#include <string.h>                  /* string library functions */
#include <time.h>                    /* time library functions */
#include <stdlib.h>                  /* standard library functions */
#include <ctype.h>

#include "build_ugc.h"  /* function prototypes */

/*********************************************************************
   build_ugc()
   
   PURPOSE
   Build the list of UGC (Universal Generic Codes) based on the 
   forecast points that are included in the product.  This function
   builds the list based on either county or zone numbers.
   
   NOTES
   This function loops on the number of included forecast points
   and for each ugc associated with each forecast point, allocates
   memory to keep track of which ugc states/numbers have been included. 
   The actual string list of ugc's is assembled by a separate function.
      
   ********************************************************************/
void build_ugc(grp_struct	*grp,
	       fp_struct	*fp,
	       pcc_struct	*pcc,
	       misc_struct	*misc,
	       vtecinfo_struct	*vtecinfo,
	       char		county_zone_flag,	
	       char 		*longstring)
{
   int 		i, j, state_index;
   int 		grpindex, fpindex;
   int		linepos, cnt;
   char 	*stateid[MAX_UGCSTATES];
   int 		*ugcmask[MAX_UGCSTATES];
   int 		numstates;   
   Zonenum 	*zonenumPtr = NULL, *zonenum = NULL;
   Countynum 	*countynumPtr = NULL, *countynum = NULL;
   Counties 	*countiesHead = NULL;
   int 		numitems, itemval;
   char 	itemstate[STATE_LEN + 1];
   char 	where[MAXLEN_STRING];
   int 		continue_loop;
   int		nwr_flag = FALSE;
   char 	tempstring[MAXLEN_STRING];
   int	        max_line_length; 
   
   /* initialize */
   
   memset(longstring, 0, MAXLEN_LONGSTR);
   numstates = 0;
   
   
   /* loop on all the forecast points;  this loop works in a 
      unique way since numfps variable is not available; it uses
      the grp structure and grps included information instead */
   
   for (cnt = grpindex = 0; cnt < misc->numgrps_included; grpindex++)
   {
      if (misc->grps_included[grpindex] == TRUE)
      {
	 cnt++;
	 
	 for (fpindex = 0; fpindex < grp[grpindex].numfps; fpindex++)
	 {
	    i = grp[grpindex].fpindex[fpindex];
	    
	    
	    /* only consider if the forecast point is included */
	    
	    if (misc->fps_included[i] == TRUE)
	    {		 
	       /* load the zones or counties that are defined for this forecast point*/
	       
	       sprintf(where, " WHERE lid = '%s' ", fp[i].id);
	       
	       if (county_zone_flag == 'Z')
	       {
		  zonenumPtr = GetZonenum(where);
		  if (zonenumPtr == NULL)
		  	numitems = 0;
		  else
		  	numitems = ListCount(&zonenumPtr->list);
	       }
	       else
	       {
		  countynumPtr = GetCountynum(where);
		  if (countynumPtr == NULL)
			numitems = 0;
		  else
			numitems = ListCount(&countynumPtr->list);
	       }
	       
	       
	       if (numitems <= 0)
	       {
		  if (county_zone_flag == 'Z')
		     log_msg(NO_ZONENUM, fp[i].id);
		  else
		     log_msg(NO_COUNTYNUM, fp[i].id);
	       }
	       
	       else
	       {
		  /* get the info for this entry and loop on the
		     number of items defined for the forecast point */
		  
		  if (county_zone_flag == 'Z')
		     zonenum   = (Zonenum *) ListFirst(&zonenumPtr->list);
		  else
		     countynum = (Countynum *) ListFirst(&countynumPtr->list);
		  
		  do 
		  {
		     /* load the current item's value and state string;
			for county data, the actual county number is stored
			in a separate table as the foreign key is based on the
			county name, not the number */
		     
		     if (county_zone_flag == 'Z')
		     {
			itemval = atoi(zonenum->zonenum);
			strcpy(itemstate, zonenum->state);
		     }
		     else
		     {
			sprintf(where, " WHERE county = '%s' AND state = '%s' ",
				countynum->county, countynum->state);
			countiesHead = GetCounties(where);		  
			itemval = atoi(countiesHead->countynum);
			FreeCounties(countiesHead);
			
			strcpy(itemstate, countynum->state);
		     }
		     
		     
		     /* check if the current state is already allocated for;
			if so get the current state index */
		     
		     state_index = MISSINGVAL;
		     for (j = 0; j < numstates; j++)
		     {
			if (strcmp(itemstate, stateid[j]) == 0)
			{
			   state_index = j;
			   break;
			}
		     }
		     
		     
		     /* if the state has not been allocated, then allocate room 
			for its information and get the state index */
		     
		     if (state_index == MISSINGVAL)
		     {
			/* make sure there is room in the associated arrays */
			
			if (numstates + 1 > MAX_UGCSTATES)
			   log_msg(EXCEED_UGCSTATES, "");
			
			else
			{		  
			   /* allocate room to store the state id and then
			      load the id */
			   
			   stateid[numstates] = (char *)malloc(STATE_LEN+1);
			   if (stateid[numstates] == NULL)
			      log_msg(FAILED_MALLOC, "for stateid in build_ugclist");
			   strcpy(stateid[numstates], itemstate);
			   
			   
			   /* allocate room to store the list of numbers for the  
			      given state and initialize the list */
			   
			   ugcmask[numstates] = (int *)malloc(MAX_UGCVAL*sizeof(int));
			   if (ugcmask[numstates] == NULL)
			      log_msg(FAILED_MALLOC, "for ugcmask in build_ugclist");
			   memset(ugcmask[numstates], FALSE, MAX_UGCVAL*sizeof(int));
			   
			   
			   /* define the current state index and increment the  
			      number of states */
			   
			   state_index = numstates;
			   numstates++;
			}
		     }
		     
		     
		     /* now load the zone number for the state */
		     
		     if (state_index != MISSINGVAL) 
			ugcmask[state_index][itemval] = TRUE;
		     
		     
		     /* get the next entry for the forecast point */
		     
		     continue_loop = TRUE;
		     if (county_zone_flag == 'Z')
		     {
			zonenum = (Zonenum *) ListNext(&zonenum->node);
			if (zonenum == NULL) continue_loop = FALSE;
		     }
		     else
		     {
			countynum = (Countynum *) ListNext(&countynum->node);
			if (countynum == NULL) continue_loop = FALSE;
		     }
		     
		  } while (continue_loop == TRUE);
		  
		  
		  /* free the memory for this forecast point */
		  
		  if (county_zone_flag == 'Z')
		     FreeZonenum(zonenumPtr);
		     
		  else
		     FreeCountynum(countynumPtr);
		  
	       }     /* end of if on whether info found for forecast point */	 
	    }        /* end of if on whether forecast point included */
	 }           /* end of for loop on forecast points */
      } /* end of if on whether current group is included */
   }    /* end of loop on included groups */
	 
   
   /* at this point, we know the number of states and their ids, and for
      each state, there is a mask identifying which ugc numbers are in the
      list; now build the areas portion of the ugc string */
   
   write_ugc_areas(numstates, county_zone_flag, stateid, ugcmask, 
	           misc, nwr_flag, 
	           longstring, &linepos);
		   
   
   /* insert the GMT expiration time at the end of the ugc list.
      insert a blank if word wrap may be needed */
              
   max_line_length = get_line_width();
   
   if (linepos + 7 >= max_line_length)
      strcat(longstring, " ");
   
   fpindex = MISSINGVAL;   
   strcpy(tempstring, 
          write_ugc_expiretime(misc, pcc, vtecinfo, fpindex));  	   
   strcat(longstring, tempstring);
   
   
   /* check that the string length was not exceeded */
   
   i = strlen(longstring);
   if (i > MAXLEN_LONGSTR - 1) log_msg(EXCEED_UGCLIST, "");
   
   
   /* free the allocated memory */
   
   for (i = 0; i < numstates; i++)
   {
      free(stateid[i]);
      free(ugcmask[i]);
   }
   
   return;
}


/*********************************************************************
   build_ugc_nwr()
   
   PURPOSE
   Builds the listening area codes string needed for the
   NWR product header.
   
   Given the tower being considered and the list of 
   assocations between towers and locations, the locations
   being considered are determined.  Knowing the location, 
   the counties associated with it for nwr purposes are determined.
   Note this is different that the counties associated with it
   for conventional UGC purposes.  Then knowing the associated 
   counties, get their state and FIPS code info and assemble
   the information in preparation for building the list for the
   nwr product.  
   
   ********************************************************************/

void build_ugc_nwr(int			numfps,
		   fp_struct		*fp,
		   misc_struct		*misc,
		   NWRTransmitter	*ntransPtr,
		   LocTransmit		*loctransHead,
		   char 		*longstring)
{
   int			i, j, linepos;
   int			within_tower;
   char 		where[300];
   Countynum 		*countynumHead = NULL, *countynumPtr = NULL;   
   Counties 		*countiesHead = NULL;
   int 			state_index;
   char 		*stateid[MAX_UGCSTATES];
   int 			*ugcmask[MAX_UGCSTATES];
   int 			numstates;   
   int 			itemval;
   char 		itemstate[STATE_LEN + 1];
   char			county_zone_flag = 'C';
   int			nwr_flag = TRUE;
   
   
   /* initialize */
   
   log_msg("", "Building UGC info for NWR header...");
   
   memset(longstring, 0, MAXLEN_LONGSTR);
   numstates = 0;
   
   /* loop on the forecast points */
   
   for (i = 0; i < numfps; ++i)
   {                
      /* only process if the forecast point is to be included */
      
      if (misc->fps_included[i] == TRUE)
      {
	 /* now check if this forecast point should be included
	    in the product */
	 
	 within_tower = check_loc_in_tower(fp[i].id, ntransPtr, loctransHead);
	 if (within_tower == TRUE)
	 {
	    /* get the list of counties associated with the tower
	       for this forecast point.  we know there will always be at
	       least one in this list since above we found that the 
	       location was deemed within the tower area */
	    
	    sprintf(where, " WHERE lid='%s' AND (county||state) in "
		    " (SELECT (county||state) FROM countytransmit "
		    " WHERE call_sign = '%s' ) ", 
		     fp[i].id, ntransPtr->call_sign);
	    countynumHead = GetCountynum(where);
	    
	    
	    /* loop on the associated counties */
	    if (countynumHead != NULL)
		countynumPtr = (Countynum *) ListFirst(&countynumHead->list);
	    while(countynumPtr)
	    {	       
	       /* now get the fips code for the county which is
		  unfortunately stored in a separate parent table.
		  store the fips code and state id in temp variables. */
	       
	       sprintf(where, " WHERE county = '%s' AND state = '%s' ",
		       countynumPtr->county, countynumPtr->state);
	       countiesHead = GetCounties(where);
	       
	       itemval = atoi(countiesHead->countynum);
	       strcpy(itemstate, countiesHead->state);
	       
	       FreeCounties(countiesHead);
	       
	       
	       /* use code identical to that used in the function for
		  building the non-NWR UGS list */
	       
	       /* check if the current state is already allocated for;
		  if so get the current state index */
	       
	       state_index = MISSINGVAL;
	       for (j = 0; j < numstates; j++)
	       {
		  if (strcmp(itemstate, stateid[j]) == 0)
		  {
		     state_index = j;
		     break;
		  }
	       }
	       
	       
	       /* if the state has not been allocated, then allocate room 
		  for its information and get the state index */
	       
	       if (state_index == MISSINGVAL)
	       {
		  /* make sure there is room in the associated arrays */
		  
		  if (numstates + 1 > MAX_UGCSTATES)
		     log_msg(EXCEED_UGCSTATES, "");
		  
		  else
		  {		  
		     /* allocate room to store the state id and then
			load the id */
		     
		     stateid[numstates] = (char *)malloc(STATE_LEN+1);
		     if (stateid[numstates] == NULL)
			log_msg(FAILED_MALLOC, "for stateid in build_ugclist");
		     strcpy(stateid[numstates], itemstate);
		     
		     
		     /* allocate room to store the list of numbers for the  
			given state and initialize the list */
		     
		     ugcmask[numstates] = (int *)malloc(MAX_UGCVAL*sizeof(int));
		     if (ugcmask[numstates] == NULL)
			log_msg(FAILED_MALLOC, "for ugcmask in build_ugclist");
		     memset(ugcmask[numstates], FALSE, MAX_UGCVAL*sizeof(int));
		     
		     
		     /* define the current state index and increment the  
			number of states */
		     
		     state_index = numstates;
		     numstates++;
		  }
	       }
	       
	       
	       /* now load the zone number for the state */
	       
	       if (state_index != MISSINGVAL) 
		  ugcmask[state_index][itemval] = TRUE;
	       
	       
	       countynumPtr = (Countynum *) ListNext(&countynumPtr->node);
	    }
	    
	    FreeCountynum(countynumHead);
	 }
      }
   }
   
   
   /* at this point, we know the number of states and their ids, and for
      each state, there is a mask identifying which ugc numbers are in the
      list; now build the areas portion of the ugc string */
   
   write_ugc_areas(numstates, county_zone_flag, stateid, ugcmask, 
	           misc, nwr_flag,
	           longstring, &linepos);
   
   
   /* check that the string length was not exceeded */
   
   i = strlen(longstring);
   if (i > MAXLEN_LONGSTR - 1) log_msg(EXCEED_UGCLIST, "");
   
   
   /* free the allocated memory */
   
   for (i = 0; i < numstates; i++)
   {
      free(stateid[i]);
      free(ugcmask[i]);
   }
   
   
   return;   
}


/*********************************************************************
   build_ugc_segment()
   
   PURPOSE
   Build the list of UGC (Universal Generic Codes) based on the 
   forecast points within the specified group that are included in the 
   product.  This function builds the list based on either county or 
   zone numbers.
   
   NOTES
   This function loops on the number of included forecast points
   and for each ugc associated with each forecast point, allocates
   memory to keep track of which ugc states/numbers have been included. 
   The actual string list of ugc's is assembled by a separate function.
   
   Use the expire_time explicitly passed in as its own argument, rather than
   the more-general expire_time field in the misc structure.  this is done
   to accomodate the possible special adjustments needed for the VTEC 
   monitoring of expire time.
      
   ********************************************************************/
void build_ugc_segment(grp_struct	*grp,
	               fp_struct	*fp,
		       pcc_struct	*pcc,
	               misc_struct	*misc,
		       vtecinfo_struct	*vtecinfo,
                       int              grpindex_used,
                       int              fpindex_used,
	               char		county_zone_flag,	
	       	       char 		*longstring)
{
   int 		i, j, state_index, linepos;
   int 		fpindex;
   char 	*stateid[MAX_UGCSTATES];
   int 		*ugcmask[MAX_UGCSTATES];
   int 		numstates;   
   Zonenum 	*zonenumPtr = NULL, *zonenum = NULL;
   Countynum 	*countynumPtr = NULL, *countynum = NULL;
   Counties 	*countiesHead = NULL;
   int 		numitems, itemval;
   char 	itemstate[STATE_LEN + 1];
   char 	where[MAXLEN_STRING];
   int 		continue_loop;
   int		nwr_flag = FALSE;
   int          fpindex_end;
   char 	tempstring[MAXLEN_STRING];
   int          max_line_length;
 
   
   /* initialize */
   
   memset(longstring, 0, MAXLEN_LONGSTR);
   numstates = 0;
   
   
   /* do error check just to be safe */
   
   if (fpindex_used == MISSINGVAL && grpindex_used == MISSINGVAL)
   {
      fprintf(stderr,
	      "ERROR - invalid call to build_ugc_segment(). No index defined\n");
      log_msg("", "ERROR - invalid call to build_ugc_segment()");
      return;
   }
   
	 
   /* if fpindex is MISSING, do all the forecast points in the specified group.
      The fpindex will loop based on the grp list of fps. */
   
   if (fpindex_used == MISSINGVAL)
      fpindex_end   = grp[grpindex_used].numfps;
   
   
   /* otherwise, force it to do just one iteration of the loop. */
   
   else
      fpindex_end   = 1;
   
   
   /* loop on all the forecast points;  this loop works in a 
      unique way since numfps variable is not available; it uses
      the grp structure and grps included information instead */
   
   /* we know that the forecast group is included or we would never have
      called this function, so that part of build_ugc is left out. */
      
   for (fpindex = 0; fpindex < fpindex_end; fpindex++)
   {
      /* if fpindex_used is MISSING, then we are doing an entire group,
	 so get the index to the forecast point in fp according
	 to the group */
      
      if (fpindex_used == MISSINGVAL)
         i = grp[grpindex_used].fpindex[fpindex];
      
      
      /* otherwise, set the index to be fpindex_used */
      
      else
         i = fpindex_used;
      
      
      /* only consider if the forecast point is included */
	    
      if (misc->fps_included[i] == TRUE)
      {		 
         /* load the zones or counties defined for this forecast point*/
	       
	 sprintf(where, " WHERE lid = '%s' ", fp[i].id);
	       
	 if (county_zone_flag == 'Z')
	 {
	    zonenumPtr = GetZonenum(where);
	    if (zonenumPtr == NULL)
		numitems = 0;
	    else
		numitems = ListCount(&zonenumPtr->list);
	 }
	 else
	 {
	    countynumPtr = GetCountynum(where);
	    if (countynumPtr == NULL)
		numitems = 0;
	    else
		numitems = ListCount(&countynumPtr->list);
	 }
	       
	 if (numitems <= 0)
	 {
	    if (county_zone_flag == 'Z')
	       log_msg(NO_ZONENUM, fp[i].id);
	    else
	       log_msg(NO_COUNTYNUM, fp[i].id);
	 }
	       
	 else
	 {
	    /* get the info for this entry and loop on the
	       number ofitems defined for the forecast point */
		  
	    if (county_zone_flag == 'Z')
	       zonenum   = (Zonenum *) ListFirst(&zonenumPtr->list);
	    else
	       countynum = (Countynum *) ListFirst(&countynumPtr->list);
		  
	    do 
	    {
	       /* load the current item's value and state string;
	          for county data, the actual county number is stored
	          in a separate table as the foreign key is based on the
		  county name, not the number */
		     
	       if (county_zone_flag == 'Z')
	       {
		  itemval = atoi(zonenum->zonenum);
		  strcpy(itemstate, zonenum->state);
	       }
	       else
	       {
		  sprintf(where, " WHERE county = '%s' AND state = '%s' ",
		     countynum->county, countynum->state);
		  countiesHead = GetCounties(where);		  
		  itemval = atoi(countiesHead->countynum);
		  FreeCounties(countiesHead);
			
		  strcpy(itemstate, countynum->state);
	       }
		
	       
	       /* check if the current state is already allocated for;
		  if so get the current state index */
		     
	       state_index = MISSINGVAL;
	       for (j = 0; j < numstates; j++)
	       {
		  if (strcmp(itemstate, stateid[j]) == 0)
		  {
		     state_index = j;
		     break;
		  }
	       }
		
	       
	       /* if the state has not been allocated, then allocate room 
	          for its information and get the state index */
		     
	       if (state_index == MISSINGVAL)
	       {
		  /* make sure there is room in the associated arrays */
			
		  if (numstates + 1 > MAX_UGCSTATES)
		     log_msg(EXCEED_UGCSTATES, "");
			
		  else
		  {		  
		     /* allocate room to store the state id and then
			load the id */
			   
		     stateid[numstates] = (char *)malloc(STATE_LEN+1);
		     if (stateid[numstates] == NULL)
			log_msg(FAILED_MALLOC, "for stateid in build_ugclist");
		     strcpy(stateid[numstates], itemstate);
		
		     
		     /* allocate room to store the list of numbers for the  
			given state and initialize the list */
			   
		     ugcmask[numstates] = (int *)malloc(MAX_UGCVAL*sizeof(int));
		     if (ugcmask[numstates] == NULL)
			log_msg(FAILED_MALLOC, "for ugcmask in build_ugclist");
		     memset(ugcmask[numstates], FALSE, MAX_UGCVAL*sizeof(int));
		
		     
		     /* define the current state index and increment the  
		        number of states */
			   
		     state_index = numstates;
		     numstates++;
		  }
	       }
		     
	       /* now load the zone number for the state */
		     
	       if (state_index != MISSINGVAL) 
		  ugcmask[state_index][itemval] = TRUE;
	
	       
	       /* get the next entry for the forecast point */
		     
	       continue_loop = TRUE;
	       if (county_zone_flag == 'Z')
	       {
		  zonenum = (Zonenum *) ListNext(&zonenum->node);
		  if (zonenum == NULL) continue_loop = FALSE;
	       }
	       
	       else
	       {
		  countynum = (Countynum *) ListNext(&countynum->node);
		  if (countynum == NULL) continue_loop = FALSE;
	       }
		     
	    } while (continue_loop == TRUE);
		  
	    /* free the memory for this forecast point */
		  
	    if (county_zone_flag == 'Z')
	       FreeZonenum(zonenumPtr);
	    else
	       FreeCountynum(countynumPtr);
		  
	 }     /* end of if on whether info found for forecast point */	 
      }        /* end of if on whether forecast point included */
   }           /* end of for loop on forecast points */

   
   /* at this point, we know the number of states and their ids, and for
      each state, there is a mask identifying which ugc numbers are in the
      list; now build the ugc string */
   
   write_ugc_areas(numstates, county_zone_flag, stateid, ugcmask, 
	           misc, nwr_flag, 
	           longstring, &linepos);
		   
		   
   /* insert the GMT expiration time at the end of the ugc list.
      insert a blank if word wrap may be needed */
   
   max_line_length = get_line_width();
   
   if (linepos + 7 >= max_line_length)
      strcat(longstring, " ");
      
   strcpy(tempstring, 
          write_ugc_expiretime(misc, pcc, vtecinfo, fpindex_used));  	   
   strcat(longstring, tempstring);
    
   
   /* check that the string length was not exceeded */
   
   i = strlen(longstring);
   if (i > MAXLEN_LONGSTR - 1) log_msg(EXCEED_UGCLIST, "");
   
   
   /* free the allocated memory */
   
   for (i = 0; i < numstates; i++)
   {
      free(stateid[i]);
      free(ugcmask[i]);
   }
   
   return;
}


/*****************************************************************
   build_ugc_segment_county()
   
   PURPOSE
   Build the UGC for forecast data grouped by county.
   This assumes a county-based UGC, so there is only one county
   to list, since the info is grouped by county.
   The expire time for segment county uses the expire time
   for the product, for all the events in the product.
   Use the special longstring variable to return the data
   since this can be a long string.
   
   *****************************************************************/
void build_ugc_segment_county(county_struct	*cnty,
                              int       	cntyindex,
			      misc_struct	*misc,
			      char  		*longstring)
{
   struct tm 	*exptime;
   char		expiretime[16];
   
   
   /* initialize */
   
   memset(longstring, 0, 40);
   
   
   /* set the expire time string */
   
   exptime = gmtime(&misc->expire_time);  
   sprintf(expiretime, "%2.2d%2.2d%2.2d",
	   exptime->tm_mday, exptime->tm_hour, exptime->tm_min);
   
   
   /* load in the string */
   
   sprintf(longstring, "%s%s%s-%s-", 
	   cnty[cntyindex].state, "C", cnty[cntyindex].countynum,
	   expiretime);
   
   return;
}


/*********************************************************************
   write_ugc_areas()
   
   PURPOSE
   Write the list of UGC (Universal Generic Codes) given the zone
   numbers and states that are included.
   
   NOTES
   The nwr_flag is used to control whether to insert spaces for use
   by the word wrap function, and to control whether to include
   the expiration information.
   
   ********************************************************************/
void write_ugc_areas(int		numstates,
	             char		county_zone_flag,
   	             char 		*stateid[],
	       	     int 		*ugcmask[],
	             misc_struct	*misc,
	             int		nwr_flag,
	             char		*longstring,
		     int		*linepos)
{	       
   int 		i, j;
   int		pos;
   int 		firstval, lastval;
   char 	tempstring[MAXLEN_STRING];
   int		max_line_length;
  
   
   /* get the line length for formatting the string */
   
   max_line_length = get_line_width();
   
   
   /* loop on the number of states */
   
   pos = 0;
   for (i = 0; i < numstates; i++)
   {
      /* first load the two-character state id and add the
	 letter Z or C to indicate this is a zone or county list.
	 make sure there is room for the longest possible string to come
	 that would have the form: SSZ###>###-, for 11 characters.
	 if not, then insert a blank so that the word wrap  */
      
      if (!nwr_flag && (pos + 11 >= max_line_length))
      {
	 strcat(longstring, " ");
	 pos = 0;
      }
      
      strcat(longstring, stateid[i]);
      if (county_zone_flag == 'Z')
	 strcat(longstring, "Z");
      else
	 strcat(longstring, "C");
      
      pos = pos + 3;
      
      
      /* loop thru the number of possible ugc numbers; along the way
	 identify any sequences of consecutive numbers that are included
	 in the mask by determining the first and last position
	 in the series */
      
      firstval = MISSINGVAL;
      lastval  = MISSINGVAL;
      
      for (j = 0; j < MAX_UGCVAL; j++)
      {
	 /* if the mask for this number is set and the first value
	    is not set, then set it */
	 
	 if (ugcmask[i][j] == TRUE)
	 {
	    if (firstval == MISSINGVAL) firstval = j;
	 }
	 
	 
	 /* if the mask for this number is not set then if there is a series
	    currently being defined, set the last code */
	 
	 else
	 {
	    if (firstval != MISSINGVAL)
	    {
	       lastval = j - 1;
	       
	       
	       /* if the series is only of length one. */
	       
	       if (firstval == lastval)
	       {
		  if (!nwr_flag && (pos + 4 >= max_line_length))
		  {
		     strcat(longstring, " ");
		     pos = 0;
		  }
		  
		  sprintf(tempstring, "%3.3d-", firstval);
		  strcat(longstring, tempstring);
		  pos = pos + 4;
	       }
	       
	       
	       /* if the series is a complete sequence */
	       
	       else
	       {
		  if (!nwr_flag && (pos + 8 >= max_line_length))
		  {
		     strcat(longstring, " ");
		     pos = 0;
		  }
		  
		  sprintf(tempstring, "%3.3d>%3.3d-", firstval, lastval);
		  strcat(longstring, tempstring);
		  pos = pos + 8;
	       }
	       	       	       
	       
	       /* reset the indices */
	       
	       firstval = MISSINGVAL;
	       lastval  = MISSINGVAL;	       
	    }
	 }
      }  /* end of loop on possible numeric codes for current state */
      
      
      /* if there is leftover, unclosed series left, as can occur if the last
	 possible ugcnum is set, then add it to the string */
      
      if (firstval != MISSINGVAL)
      {
	 if (!nwr_flag && (pos + 8 >= max_line_length))
	 {
	    strcat(longstring, " ");
	    pos = 0;
	 }
	 
	 sprintf(tempstring, "%3.3d>%3.3d-", firstval, MAX_UGCVAL);
	 strcat(longstring, tempstring);
	 pos = pos + 8; 	 
      }
   }  /* end of loop on states */
   
   
   /* for the nwr list, suppress the last "-" character
      which is left hanging just before the expiration time */
      
   if (nwr_flag)  
   {
      pos = strlen(longstring);
      memset(&(longstring[pos - 1]), 0, 1);
   }
         
       
   /* return the position on the current line just in case 
      a word wrap adjustment is needed. */
      
   *linepos = pos;
   
   return;
}

 
/*********************************************************************

   write_ugc_expiretime()

   PURPOSE
   Create the actual UGC expire time or create a time code value
   which is translated upon issuance, since the UGC time depends in 
   part on the current time, which changes between product generation
   and product issuance.
   
   Rules for encoding when not in batch mode:
   -- if non-VTEC: <UGC+###> for relative time
   -- if non-VTEC: actual, if manually entered absolute time
   -- if VTEC CAN/EXP action:  <UGC+###>
   -- if VTEC other: <UGCr:+###,endtime> if relative time
                     <UGCa:abstime,endtime> if manual absolute time;
		      where endtime is VTECendtime 
		      
   The misc->expire_set variable indicates whether the expire time
   is based on an absolute time manually entered by the user, or by
   a relative time defined for the product in the pcc file.
   
   If an absolute time is defined, it is stored in misc->expire_time.
   If generating VTEC, the vtec action and endtime also can effect
   the expire time.

*********************************************************************/

char * write_ugc_expiretime(misc_struct  	*misc,
                            pcc_struct	 	*pcc,
			    vtecinfo_struct	*vtecinfo,
			    int			fpindex)
{
   time_t	mins_from_now;
   struct tm 	*time_tm;
   static char 	tempstring[MAXLEN_STRING]= "";
   char         timestr1[MAXLEN_STRING], timestr2[MAXLEN_STRING];
   time_t	actual_time = MISSINGVAL; 
      
      
   /* format the UGC info based on whether the info will be
      later translated, which depends on whether the 
      product was generated in batch mode or not.  if in batch, 
      simply use the already defined expire time. */
	 
   if (misc->batch_mode == TRUE)
   {
      time_tm = gmtime(&(misc->expire_time));      
      sprintf(tempstring, "%2.2d%2.2d%2.2d-", 
              time_tm->tm_mday, time_tm->tm_hour, time_tm->tm_min);       
   }
      
      
   /* if generating product interactively, consider all the scenarios
      when determining how the encode the expire time */      
      	 
   else
   { 
      /* even if in vtec mode, need to make sure that this UGC is being
         built for a segment with a defined fpindex, and not just for the 
	 general UGC varaible */
	 
      if (pcc->product.vtec_flag == TRUE && fpindex != MISSINGVAL) 
      {               	    
	 /* if the action is "CAN" or "EXP", force the expiration time as 
	    current system time + 30 minutes */ 
	    
	 if (strcmp(vtecinfo[fpindex].action, "CAN") == 0 ||
	     strcmp(vtecinfo[fpindex].action, "EXP") == 0)
	 {	       	   
	    actual_time = misc->system_time + (3600 / 2);
	    
            mins_from_now = 30;
            sprintf(tempstring, "%s%ld>-", TIMESTR_UGC_OFFSET, mins_from_now); 
	 }
	          
	       	       
	 else
	 {	
	    /* encode absolute and endtime if absolute time was specified
	       by the user. remember to adjust the year and month by their
	       offset. */
	           		  
            if (misc->expire_set == UGC_ABS_MANUAL)
	    {
               time_tm = gmtime(&(misc->expire_time));     
                  sprintf(timestr1, "%4.4d-%2.2d-%2.2d %2.2d:%2.2d", 
		       time_tm->tm_year + 1900, time_tm->tm_mon + 1, 
		       time_tm->tm_mday,
		       time_tm->tm_hour, time_tm->tm_min);       
		             
               if (vtecinfo[fpindex].endtime != MISSINGVAL && 
	           (strcmp(vtecinfo[fpindex].action, "ROU") != 0))
	       {
	          time_tm = gmtime(&(vtecinfo[fpindex].endtime));     
                  sprintf(timestr2, "%4.4d-%2.2d-%2.2d %2.2d:%2.2d", 
		          time_tm->tm_year + 1900, time_tm->tm_mon + 1, 
			  time_tm->tm_mday,
		          time_tm->tm_hour, time_tm->tm_min);       
               }
	       else
	          sprintf(timestr2, TIMESTR_MSG_ENDTIME);
		  
               sprintf(tempstring, "%s%s,%s>-", TIMESTR_UGC_ABSOLUTE, 
		       timestr1, timestr2); 
	    }
	    
	    
	    /* encode relative offset and endtime if no absolute time was 
	       specified by the user. adjust year and month with offset. */
	    
	    else
	    {		  	  
               mins_from_now = (misc->expire_time - misc->system_time) / 60;
		  
               if (vtecinfo[fpindex].endtime != MISSINGVAL &&
	           (strcmp(vtecinfo[fpindex].action, "ROU") != 0))
	       {
                  time_tm = gmtime(&(vtecinfo[fpindex].endtime));     
                  sprintf(timestr2, "%4.4d-%2.2d-%2.2d %2.2d:%2.2d", 
		          time_tm->tm_year + 1900, time_tm->tm_mon + 1,
			  time_tm->tm_mday,
		          time_tm->tm_hour, time_tm->tm_min); 
	       }      
	       else
	          sprintf(timestr2, TIMESTR_MSG_ENDTIME);
		  
               sprintf(tempstring, "%s%ld,%s>-", TIMESTR_UGC_RELATIVE, 
		       mins_from_now, timestr2); 
	    }


            /* determine the actual time even though it is overridden
	       later when the substitution occurs. 
	       the expire time should be no later than the end time under 
	       certain cases, to allow the proper monitoring of the event 
	       expiration, which is based on the expire time, not the end time.
	       also, if the expiration time is in the past or is earlier
	       than the NOW+60 minutes, adjust the expiration time as 
	       the NOW + 60 minutes */

	    actual_time = misc->expire_time;
	    
	    if ((strcmp(vtecinfo[fpindex].action, "ROU") != 0) &&
		 vtecinfo[fpindex].endtime != MISSINGVAL)
	    {
	       if (misc->expire_time > vtecinfo[fpindex].endtime)
	          actual_time = vtecinfo[fpindex].endtime;
	    }   
			
	    if (misc->expire_time < misc->system_time + 3600)
	    {
	       actual_time = misc->system_time + 3600;		
	    }
	 }
	       
	 /* even though this value will be overridden at issuance time, 
	    set it here anyways. */
	       
	 vtecinfo[fpindex].expiretime = actual_time; 
    
      }
      
      /* if no vtec, then ignore vtec rules for adjusting expire time
         and use time either defined by offset or manually set by user */
	  
      else
      {
         if (misc->expire_set == UGC_ABS_MANUAL)
	 {
            time_tm = gmtime(&(misc->expire_time));     
            sprintf(tempstring, "%2.2d%2.2d%2.2d-", 
	            time_tm->tm_mday, time_tm->tm_hour, time_tm->tm_min);       
	 }
	 
	 else
	 {
            /* use string <UGC+xx> to represent expiration time in the created
               product, where xx is minutes from current time */
      
            mins_from_now = (misc->expire_time - misc->system_time) / 60;
            sprintf(tempstring, "%s%ld>-", TIMESTR_UGC_OFFSET, mins_from_now); 
	 }
      }     
   }  
        
  
   return(tempstring);
}
      

                   
