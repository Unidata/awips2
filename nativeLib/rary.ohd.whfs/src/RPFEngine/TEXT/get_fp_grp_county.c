/*********************************************************************
   get_fp_grp_county.c
   
   get_fp_grp_county()   
   -->load_fpdata()
   -->load_grpdata()    
   -->count_grp_fps()
   load_cntydata()   
   load_fpprev_data()
   
   ********************************************************************/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

#include "get_fp_grp_county.h"
#include "process_vtecinfo.h"

/*********************************************************************
   get_fp_grp_county()
   
   PURPOSE
   Collects most of the static data for each forecast point, the 
   forecast groups and county.  The code checks that the group specified for 
   a given forecast point is a valid, defined group.
   
   NOTES   
   (1) information on forecast point impact statements (FLDSTMNT)
   and crests (CREST) are loaded up on the fly later in the program.
   
   ********************************************************************/

void get_fp_grp_county(char		selected_office[],
		       int		*numfps,
		       fp_struct	**fp,
		       int		*numgrps,
		       grp_struct	**grp,
		       int              *numcnty,
		       county_struct    **cnty)   
{
   FpInfo	*fpinfoHead = NULL;
   RpfFcstGroup	*rpffcstgroup = NULL, *grpPtr = NULL;
  
   int 		i;   
   int		*num_in_grp = NULL;
   char		where[300];
   
   UniqueList   *ulHead = NULL; 
   int          ulcnt = 0;
   
          
   /* get all of the forecast group info, which has the defined groups. 
      check the count on the number of groups */
   
   rpffcstgroup = GetRpfFcstGroup(" ORDER BY ordinal, group_id ASC");
   
   *numgrps = 0;
   if (rpffcstgroup != NULL)
      *numgrps = ListCount(&rpffcstgroup->list);
   
   if (*numgrps <= 0) log_msg(NO_GRPS, "");
   
   
   /* get the information for each forecast point from the FPINFO view.
      only gets those locations for the currently selected office/hsa.
      check the count of the number of forecast points */   
   
   sprintf(where, " WHERE hsa = '%s' ORDER BY ordinal, lid ASC ",
	   selected_office);
   log_msg("", where);
   
   fpinfoHead = GetFpInfo(where);
   
   *numfps = 0;
   if (fpinfoHead != NULL)
      *numfps = ListCount(&fpinfoHead->list);
   if (*numfps <= 0) log_msg(NO_FPS, "");
    
    
   /* malloc the space for the num_in_grp array */
         
   num_in_grp = (int *)malloc(sizeof(int) * (*numgrps));
   if (num_in_grp == NULL) 
      log_msg(FAILED_MALLOC, "of num_in_grp array in get_fp_grp");   
      

   /* loop on the number of grps defined in the table and 
      determine the number of forecast points included per group.
      this is necessary since some groups may not be used, either
      because they have no forecast points, or because their
      forecast points are for a different office. */

   *numgrps = 0;
   if (rpffcstgroup != NULL)
	grpPtr = (RpfFcstGroup *) ListFirst(&rpffcstgroup->list);
   for (i = 0; grpPtr; i++)
   {
      num_in_grp[i] = count_grp_fps(grpPtr->group_id, fpinfoHead);
      if (num_in_grp[i] > 0 ) (*numgrps)++;
      
      grpPtr = (RpfFcstGroup *) ListNext(&grpPtr->node);
   }
   
           
   /* malloc memory for the fp info structure for valid forecast points
      and load the information for the forecast points */
   
   *fp = (fp_struct *)malloc(sizeof(fp_struct) * (*numfps));
   if (*fp == NULL) 
      log_msg(FAILED_MALLOC, "of fp structure in get_fp_grp_county");
   
   memset(*fp, 0, sizeof(fp_struct) * (*numfps));
   
   load_fpdata(*numfps, fpinfoHead, *fp);
      
   
   /* malloc memory for the group data structure and
      load the data for the forecast groups being considered */
   
   *grp = (grp_struct *) malloc(sizeof(grp_struct) * (*numgrps));
   if (*grp == NULL) 
      log_msg(FAILED_MALLOC, "of grp structure in get_fp_grp_county");
   
   memset(*grp, 0, sizeof(grp_struct) * (*numgrps));
   
   load_grpdata(*numfps, *fp, *numgrps, num_in_grp, rpffcstgroup, *grp);
   
   
   /* load forecast points grouped by the county */  
   /* get unique county||state for forecast points for current office */
  
   *numcnty = 0;
   
   sprintf(where, " WHERE lid IN (SELECT lid FROM rpffcstpoint) AND "
                      " lid IN (SELECT lid FROM location WHERE hsa = '%s'  ",		  
                  selected_office);	   
   strcat(where, "AND  (type IS NULL OR type NOT LIKE '%I%') )");
     
   ulHead = LoadUnique("county||state", "Countynum", where, &ulcnt);  
   
   if (ulHead == NULL)
   {
      printf("  no records in Countynum table\n");
      return;
   }
   else 
   {     
      /* malloc memory for the county data structure and load the
	 data for the forecast groups in the county */

      *cnty = (county_struct *)malloc(sizeof(county_struct) * (ulcnt));
      if (*cnty == NULL) 
	 log_msg(FAILED_MALLOC, "of cnty structure in get_fp_grp_county");

      memset(*cnty, 0, sizeof(county_struct) * (ulcnt));

      *numcnty = load_cntydata(selected_office, ulHead, *cnty, *fp, *numfps);
   }
  
   /* free the data memory */
   
   if (ulHead != NULL)
      FreeUnique(ulHead); 
   
   if (rpffcstgroup != NULL)
      FreeRpfFcstGroup(rpffcstgroup);
   
   if (fpinfoHead != NULL)
      FreeFpInfo(fpinfoHead);
   
   
   /* free the local memory */
   
   if (num_in_grp != NULL)
      free(num_in_grp);
   
   return;
}


/*********************************************************************
   load_fpdata()
   
   PURPOSE
   Load the data from the tables and views into the structure actually 
   used by the application.  This function loads the info for all forecast 
   points, not just one at a time.
   
   ********************************************************************/
void load_fpdata(const		int		numfps, 
		 		FpInfo		*fpinfoHead,
		 		fp_struct	*fp)
{
   FpInfo	*fpPtr = NULL;
   int 		i;
   Crest	*crestHead;   
   char		where[160];
   time_t       obshrs, fcsthrs, basishrs;
   static int	        first = TRUE;
   char                 token_string[60];
   int                  token_len, string_len;
   static int	        shift_hours = DEFAULT_ENDTIME_SHIFT_HOURS;
   
   /* retrive the backhrs, forwardhrs, adjustendhrs fields
       use default values if they are NULL in database */
    get_hrvals(&obshrs, &fcsthrs, &basishrs);

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

	}
      }

      first = FALSE;     
    }  
    	 
   /* load all the previous event information related to vtec.
      this information is then used to derive the detailed 
      obs/fcst stage info from the previous product info. */
   
   
    
   
   /* loop on the number of forecast points */
   
   if (fpinfoHead != NULL)
	fpPtr = (FpInfo *) ListFirst(&fpinfoHead->list);
   
   for (i = 0; fpPtr; i++)
   {      
      /* load in those fields that map directly */
      
      strcpy(fp[i].id,		fpPtr->lid);
      strcpy(fp[i].name,	fpPtr->name);
      strcpy(fp[i].county, 	fpPtr->county);
      strcpy(fp[i].state,	fpPtr->state);
      strcpy(fp[i].stream,	fpPtr->stream);
      strcpy(fp[i].proximity,	fpPtr->proximity);
      strcpy(fp[i].reach,	fpPtr->reach);
      strcpy(fp[i].grpid,	fpPtr->group_id);
      strcpy(fp[i].pe, 	        fpPtr->pe); 
      
      strcpy(fp[i].hsa, 		fpPtr->hsa); 
      strcpy(fp[i].primary_back, 	fpPtr->primary_back); 
      strcpy(fp[i].secondary_back, 	fpPtr->secondary_back); 
      
      /*load the use_latest_fcst. This field will be used to load
      timesereis */
      
      if (strcmp(fpPtr->use_latest_fcst, "T") == 0 )
         fp[i].use_latest_fcst = TRUE;
      else
         fp[i].use_latest_fcst = FALSE;
	 
      /* retrieve the chg_threhold used in load_detail_trend_info function */
      
      if (IsNull(DOUBLE, &fpPtr->chg_threshold) == NOTNULL)
         fp[i].chg_threshold = fpPtr->chg_threshold;
      else
      {
         log_msg(MISSING_CHGTHRESHOLD, fp[i].id);
	 fp[i].chg_threshold = MISSINGVAL;
      }      	 	 	
                
      if (IsNull(INT, &fpPtr->backhrs) == NOTNULL)
         fp[i].backhrs = fpPtr->backhrs;
      else
      {                  
	 fp[i].backhrs = obshrs;
      }      	 	 	
      
      if (IsNull(INT, &fpPtr->forwardhrs) == NOTNULL)
         fp[i].forwardhrs = fpPtr->forwardhrs;
      else
      {         	
	 fp[i].forwardhrs = fcsthrs;
      }  
         
      if (IsNull(DOUBLE, &fpPtr->adjustendhrs) == NOTNULL)
         fp[i].adjustendhrs = fpPtr->adjustendhrs;
      else
      {
         /*log_msg(MISSING_ADJUSTENDHRS, fp[i].id);
	 fp[i].adjustendhrs = MISSINGVAL;*/
	 fp[i].adjustendhrs = shift_hours;
	 
      }      	 	 	
   
      /*Steal to use the rec_type field in rpffcstpoint, it previously used for
          recommendation type, not it is used to determine if to use stage or flow for the
          impact/crest varibles. If it is "PE" means the value will be based on primary pe,
	  if it is "NPE", then opposit*/
      
      if (fpPtr->rec_type != NULL)  
         strcpy(fp[i].rec_type, fpPtr->rec_type);  
      else
         strcpy(fp[i].rec_type, "PE");  /*default*/
      
      /* load the flood stage, flood flow, bankfull stage,
	 and warning stage and check that the data are specified */
      
      if (IsNull(DOUBLE, &fpPtr->fs) == NOTNULL)
	 fp[i].fs = fpPtr->fs;
      else
      {
	 log_msg(MISSING_FS, fp[i].id);
	 fp[i].fs = MISSINGVAL;
      }
      
      if (IsNull(DOUBLE, &fpPtr->fq) == NOTNULL)
	 fp[i].fq = fpPtr->fq;
      else
      {
	 log_msg(MISSING_FQ, fp[i].id);
	 fp[i].fq = MISSINGVAL;
      }
      
      if (IsNull(DOUBLE, &fpPtr->bf) == NOTNULL)
	 fp[i].bf = fpPtr->bf;
      else
      {
	 log_msg(MISSING_BF, fp[i].id);
	 fp[i].bf = MISSINGVAL;
      }
      
      if (IsNull(DOUBLE, &fpPtr->wstg) == NOTNULL)
	 fp[i].wstg = fpPtr->wstg;
      else
      {
	 log_msg(MISSING_WSTG, fp[i].id);
	 fp[i].wstg = MISSINGVAL;
      }
      
      if (IsNull(DOUBLE, &fpPtr->action_flow) == NOTNULL)
	 fp[i].aq = fpPtr->action_flow;
      else
      {
	 log_msg(MISSING_WFLOW, fp[i].id);
	 fp[i].aq = MISSINGVAL;
      }
      
      
      /* load in the categorical values.  note that cat[0] is unused to
	 ensure that fldcat1 agrees with cat[1] for convenience sake and
	 also since category 0 is used to imply a no flood condition */
                
      fp[i].cat[0] = MISSINGVAL;
      
     
      /* load the minor/moderate/major stage or flow based on the primary_pe
	 specified for the station */
	 
      if ( fp[i].pe[0] != 'Q')
      {
	if (IsNull(DOUBLE, &fpPtr->minor_stage) == NOTNULL) 
	   fp[i].cat[1] = fpPtr->minor_stage;
	else
	   fp[i].cat[1] = MISSINGVAL;
	   
        if (IsNull(DOUBLE, &fpPtr->moderate_stage) == NOTNULL)
           fp[i].cat[2] = fpPtr->moderate_stage;
	else
	   fp[i].cat[2] = MISSINGVAL;
	   
	if (IsNull(DOUBLE, &fpPtr->major_stage) == NOTNULL)
	   fp[i].cat[3] = fpPtr->major_stage;  
        else
	   fp[i].cat[3] = MISSINGVAL;
     
      }
      
      /* only load Q* PE's if there are not certain types of
         non-flow based Q* types */
      
      else if (fp[i].pe[1] != 'B' && fp[i].pe[1] != 'C' &&
	       fp[i].pe[1] != 'E' && fp[i].pe[1] != 'F' &&
	       fp[i].pe[1] != 'V')
      {
	 if (IsNull(DOUBLE, &fpPtr->minor_flow) == NOTNULL) 
	   fp[i].cat[1] = fpPtr->minor_flow;
	 else
	    fp[i].cat[1] = MISSINGVAL;

         if (IsNull(DOUBLE, &fpPtr->moderate_flow) == NOTNULL)
            fp[i].cat[2] = fpPtr->moderate_flow;
	 else
	    fp[i].cat[2] = MISSINGVAL;

	 if (IsNull(DOUBLE, &fpPtr->major_flow) == NOTNULL)
	    fp[i].cat[3] = fpPtr->major_flow;  
         else
	    fp[i].cat[3] = MISSINGVAL;	         

      }
            
      /* if the primary pe is defined strangely */
      
      else
      {
	 fp[i].cat[1] = MISSINGVAL;
	 fp[i].cat[2] = MISSINGVAL;
	 fp[i].cat[3] = MISSINGVAL;
      }   
     
      
      /* load the record stage or flow from the crest table based on the
	 primary_pe for the station and store as a categorical value */
      
      if (fp[i].pe[0] != 'Q')
      {
	 sprintf(where, " WHERE lid='%s' AND prelim='R' and stage IS NOT NULL ",
		 fp[i].id);
	 crestHead = GetCrest(where);
	 if (crestHead != (Crest *)NULL)
	 {
	    if (crestHead->stage != 0)
	       fp[i].cat[4] = crestHead->stage;
	    else
	       fp[i].cat[4] = MISSINGVAL;
	    
	    FreeCrest(crestHead);
	 }
	 else
	    fp[i].cat[4] = MISSINGVAL;
      }
      
      else if (fp[i].pe[1] != 'B' && fp[i].pe[1] != 'C' &&
               fp[i].pe[1] != 'E' && fp[i].pe[1] != 'F' &&
	       fp[i].pe[1] != 'V')
      {
	 sprintf(where, " WHERE lid='%s' AND prelim='R' and q IS NOT NULL ",
		 fp[i].id);
	 crestHead = GetCrest(where);
	 if (crestHead != (Crest *)NULL)
	 {
	    if (crestHead->q != 0)
	       fp[i].cat[4] = crestHead->q;
	    else
	       fp[i].cat[4] = MISSINGVAL;
	    
	    FreeCrest(crestHead);
	 }
	 else
	    fp[i].cat[4] = MISSINGVAL;
      }
      else
         fp[i].cat[4] = MISSINGVAL; 
      
      
      
      /* issue message as appropriate */
      
      if (fp[i].cat[1] == MISSINGVAL || fp[i].cat[2] == MISSINGVAL ||
	  fp[i].cat[3] == MISSINGVAL || fp[i].cat[4] == MISSINGVAL)
      {
	 log_msg(MISSING_CATVAL, fp[i].id);
      }
      
      
      /* check the validity of the minor, moderate,
	 and major stage data */
      
      if (fp[i].cat[1] != MISSINGVAL && fp[i].cat[2] != MISSINGVAL &&
	  fp[i].cat[1] > fp[i].cat[2])
	 log_msg(INVALID_CATVALS, fp[i].id);
      
      if (fp[i].cat[2] != MISSINGVAL && fp[i].cat[3] != MISSINGVAL &&
	  fp[i].cat[2] > fp[i].cat[3])
	 log_msg(INVALID_CATVALS, fp[i].id);
            
      
      /* initialize the full time series memory.
	 if there was data loaded, it is free with the fp structure
	 in the free_fpgrp function. */
      
      fp[i].numobsH = fp[i].numfcstH = 0;
      fp[i].use_obsH = 0;
      fp[i].obsH    = fp[i].fcstH    = NULL;
      fp[i].fullts_loaded_time = MISSINGVAL;
      
                       
      /* get the next forecast point */
      
      fpPtr = (FpInfo *) ListNext(&fpPtr->node);
   }
      
   
   return;
}


/*********************************************************************
   load_grpdata()
   
   PURPOSE
   Loads the group data. 
   
   NOTES     
   Add loading grpPtr->rec_all_included to grp structure  11/2002
   ********************************************************************/
void load_grpdata(const	int		numfps,
		  const	fp_struct	*fp,
		  const	int		numgrps,
		  const	int		*num_in_grp,
		  	RpfFcstGroup	*rpffcstgroupPtr,
		 	grp_struct	*grp)
{
   RpfFcstGroup  *grpPtr = NULL;
   int i, j;
   int fp_index, index;
   
   
   /* loop on the groups defined */

   if (rpffcstgroupPtr != NULL)
	grpPtr = (RpfFcstGroup *) ListFirst(&rpffcstgroupPtr->list);
   index = 0;
   
   for (i = 0; grpPtr; i++)
   {
      /* if group is included, then load the data, including the array
	 of indices to the forecast points in the group */
      
      if(num_in_grp[i] > 0)
      {
	 strcpy(grp[index].id, grpPtr->group_id);
	 strcpy(grp[index].name, grpPtr->group_name);
	 grp[index].numfps = num_in_grp[i];
	 strcpy(grp[index].rec_allpts_in_group, grpPtr->rec_all_included);
	 
	/* before loading the real data for grp[index].rec_allpts_in_group,
	   use "Y" for test purpose */
	   
	/* strcpy(grp[index].rec_allpts_in_group, "Y");  */
	
	
	 grp[index].fpindex = (int *) malloc(sizeof(int) * num_in_grp[i]);
	 if (grp[index].fpindex == NULL)
	    log_msg(FAILED_MALLOC, "of grp[].fpindex in load_grpdata");
	 
	 fp_index = 0;
	 for (j = 0; j < numfps; j++)
	 {
	    if (strcmp(grp[index].id, fp[j].grpid) == 0)
	    {
	       grp[index].fpindex[fp_index] = j;
	       fp_index++;	      
	    }
	 }
	 
	 
	 /* increment the index into the group structure */
	 
	 index++;
      }
      
      grpPtr = (RpfFcstGroup *) ListNext(&grpPtr->node);
   }   
   
   
   return;
}


/*********************************************************************
   count_grp_fps()
   
   PURPOSE
   Checks whether a given group should be included based on the 
   whether forecast points reference the given group.
     
   ********************************************************************/
int count_grp_fps(const	char		*grpid,
		      	FpInfo		*fpinfoHead)

{
   int 		count;
   FpInfo	*fpPtr = NULL;
   
   
   /* initialize */
   
   count = 0;
   
   
   /* loop on the forecast points and count how many forecast
      points referenced the given group */
   
   if (fpinfoHead != NULL)
	fpPtr = (FpInfo *) ListFirst(&fpinfoHead->list);
   
   while(fpPtr)
   {
      if (strcmp(grpid, fpPtr->group_id) == 0) count++;      
      fpPtr = (FpInfo *) ListNext(&fpPtr->node);
   }   
   
   return(count);
}


/***********************************************************
   load_cntydata()
   
   Purpose
   load forecast points grouped by county for the current office.
***************************************************************/
int load_cntydata(char             selected_office[],
                  UniqueList       *ulHead,
                  county_struct    *cnty,
		  const fp_struct  *fp,
	          const int        numfps)
{
   UniqueList   *ulPtr = NULL;
   Countynum    *countynumHead = NULL, *countynumPtr = NULL;
   Counties     *countiesHead = NULL;
   int          cnt, i, ul_count;
   int          k,j;
   char         where[300];
   char         ** values = NULL;
   
   /* loop thru the unique list and load info for each county||state  */
   
   if(ulHead != NULL)
      ulPtr = (UniqueList *) ListFirst(&ulHead->list);
   
   cnt = 0;
   while (ulPtr)
   {
      /*separate the concatenate state and county, with PostGres, not space
        appended on each field, instead with "|" and other char*/
	
      values = ParseUnique ( ulPtr, &ul_count );

      /* Make sure the correct number of fields were parsed from the
         load unique output.  */
	 
      if ( ( values == NULL ) || ( ul_count < 2 ) )
      {
         printf ("  Could not parse the LoadUnique output.\n" ); 
         break;
      }
      
      /* extract the county and state from the unique string in ulPtr */
      
      memset(cnty[cnt].county, 0, COUNTY_LEN + 1);
      strncpy(cnty[cnt].county, values[0], COUNTY_LEN);
      
      memset(cnty[cnt].state, 0, STATE_LEN +1);
      strncpy(cnty[cnt].state, values[1], STATE_LEN);
      
      
      /* load info linking each forecast point with the above county and state */       
      /* first, load countynum info from Counties table for above county and 
	 state */
      
      sprintf(where, " WHERE county = '%s' AND state = '%s'", cnty[cnt].county,
	      cnty[cnt].state);		      
      countiesHead = GetCounties(where);
      if (countiesHead == NULL)
	 return 0;
      else	   		       
	 strcpy(cnty[cnt].countynum,countiesHead->countynum);
      
      /* sprintf(where, " WHERE county = '%s' AND state = '%s' AND "
	 " lid IN (SELECT lid FROM rpffcstpoint) AND "
	 " lid IN (SELECT lid FROM location where hsa = '%s'  AND"
	 " (type IS NULL OR type NOT LIKE '%I%' ))",	      
	 cnty[cnt].county, cnty[cnt].state, selected_office);     
	 */
      
      sprintf(where, " WHERE county = '%s' AND state = '%s' AND "
	      " lid IN (SELECT lid FROM rpffcstpoint) AND "
	      " lid IN (SELECT lid FROM location where hsa = '%s' ",        
	      cnty[cnt].county, cnty[cnt].state, selected_office);  
      
      strcat(where, " AND (type IS NULL OR type NOT LIKE '%I%') ) ");
      
      countynumHead = GetCountynum(where);
      if (countynumHead != NULL)
      {
	 /* set the number of forecast points in each county */
	 
	 cnty[cnt].numfps = ListCount(&countynumHead->list);  
	 countynumPtr = (Countynum *) ListFirst(&countynumHead->list);
      }
      else
         cnty[cnt].numfps = 0;
      
      
      /* second malloc the space for the fpindex */
      
      cnty[cnt].fpindex = (int *)malloc(sizeof(int) * (cnty[cnt].numfps + 1));
      if (cnty[cnt].fpindex == NULL)
	 log_msg(FAILED_MALLOC, "of cnty[].fpindex in load_cntydata");
      
      
      /* third, loop on the forecast points included in the county and load info*/       	 
      
      if (cnty[cnt].numfps > 0)
      {   
	 k = 0;
	 for (i = 0; countynumPtr; i++)	 
	 {         
	    for (j = 0; j< numfps; j++)
	    {
	       if (strcmp(fp[j].id, countynumPtr->lid) == 0)
	       {
		  /* have extra error check just in case */
		  
		  if (k >= cnty[cnt].numfps)
		  {
		     fprintf(stderr, 
			     "ERROR: too many fps for co,state=%s:%s:\n", 
			     cnty[cnt].county, cnty[cnt].state);
		     break;
		  }   
		  cnty[cnt].fpindex[k] = j;
		  k++;
		  break;
	       }   	      
	    } 	  	 
	    countynumPtr = (Countynum *) ListNext(&countynumPtr->node);	 
	 }
	 
	 if (k != cnty[cnt].numfps)
	 {
	    fprintf(stderr, 
	           "ERROR: problem with number of points under county,state=%s:%s\n",
		    cnty[cnt].county, cnty[cnt].state);
	    	    
	 }
	 
	 /* increment the index into the cnty structure */
	 
	 cnt++;
      }
      
      
      /* last, free the database space */    
      
      if (countynumHead != NULL)
	 FreeCountynum(countynumHead);
      
      if (countiesHead != NULL)
	 FreeCounties(countiesHead);
      
      ulPtr = (UniqueList *) ListNext(&ulPtr->node);  	 
   }
   
   return(cnt);
}


/*********************************************************************
   
   load_fpprev_data()
   
   PURPOSE
   Loads in previous product information, knowing the previous
   active VTEC event info.   
   
   *********************************************************************/
void load_fpprev_data(int 		numfps,
		      fp_struct		*fp,
		      vtecinfo_struct	*vtecinfo,
		      misc_struct       *misc)
{
   FpPrevProd	*fpprevHead = NULL;
 /*  FpPrevProdPractice *fpprevpracticehead = NULL;*/
   char		where[150];
   int		status;
   int 		fpindex;
   char         ansi_ptime[ANSI_TIME_LEN + 1];
   int 		match_found;
   char		logmsg[120];
   
   
   /* loop on each forecast point */
   
   for (fpindex = 0; fpindex < numfps; fpindex++)
   {
      
      /* initialize */
      
      match_found = FALSE;
      
      
      /* read the previous product details for the given previous event.
         consider only the FL.W previous events - i.e. do NOT consider 
	 FL.A or FL.Y events.  if an active previous vtec vtec event was 
	 found, use that event product time to obtain the details from
	 FpPrevProd. */
      
      if (vtecinfo[fpindex].prev_flw.event_found)
      {
	 status = timet_to_yearsec_ansi(vtecinfo[fpindex].prev_flw.producttime, 
					ansi_ptime);
	 sprintf(where, " WHERE lid = '%s' AND producttime = '%s'",
		 fp[fpindex].id, ansi_ptime);      
	 
	 /* retrive from FpPrevProdPractice table if "Practice" works station
	 mode */
	 
	 if (misc->workstation_mode == PRACTICE_MODE)
	    fpprevHead = (FpPrevProd *)GetFpPrevProdPractice(where);
	 else   	 
	    fpprevHead = GetFpPrevProd(where);
	 
	 if (fpprevHead != NULL)
	 {
	    match_found = TRUE;
	    fp[fpindex].prev_avail = TRUE;
	    
  	    
	    /* now load the data */
	    
	    strcpy(fp[fpindex].prev_prod_categ, fpprevHead->prod_categ);	 
	    status = yearsec_dt_to_timet(fpprevHead->producttime, 
					 &(fp[fpindex].prev_prodtime));
	    
	    fp[fpindex].prev_curobs_val = fpprevHead->obsvalue;
	    status = yearsec_dt_to_timet(fpprevHead->obstime, 
					 &(fp[fpindex].prev_curobs_time));
	    
	    fp[fpindex].prev_maxfcst_val = fpprevHead->max_fcstvalue;	
	    status = yearsec_dt_to_timet(fpprevHead->validtime,
					 &(fp[fpindex].prev_maxfcst_time));
	    status = yearsec_dt_to_timet(fpprevHead->basistime,
					 &(fp[fpindex].prev_maxfcst_ctime));
	    
	    
	    /* derive the value of the omfc for the carryover info */
	    
	    if (fp[fpindex].prev_curobs_val > fp[fpindex].prev_maxfcst_val)
	       fp[fpindex].prev_omf_cat =
		  compute_stage_cat(fp[fpindex].cat, fp[fpindex].prev_curobs_val);
	    else
	       fp[fpindex].prev_omf_cat =
		  compute_stage_cat(fp[fpindex].cat, fp[fpindex].prev_maxfcst_val);
	    
	    
	    FreeFpPrevProd(fpprevHead);
	 }
	 
	 
	 /* an event should also be in the FpPrevProd table, unless the purging 
	    of the FpPrevProd is too soon */
	 
	 else
	 {
	    sprintf(logmsg, 
		    "ERROR - Could not find FpPrevProd info for event for %s "
		    " with producttime=%s",
		    fp[fpindex].id, ansi_ptime);	    
	    log_msg("", logmsg);
	 }
      }
      
      
      /* for any forecast points that do not have any active previous event,
	 initialize the info since the location's recommendations are still
	 computed. */
      
      if (match_found == FALSE)
      {
	 fp[fpindex].prev_avail         = FALSE;
	 
	 strcpy(fp[fpindex].prev_prod_categ, "RVS");
	 fp[fpindex].prev_prodtime      = 0;
	 
	 fp[fpindex].prev_curobs_val    = MISSINGVAL;
	 fp[fpindex].prev_curobs_time   = 0;
	 
	 fp[fpindex].prev_maxfcst_val   = MISSINGVAL;
	 fp[fpindex].prev_maxfcst_time  = 0;
	 fp[fpindex].prev_maxfcst_ctime = 0;
	 
	 fp[fpindex].prev_omf_cat       = NULLCAT;
	 
	 log_msg(MISSING_CARRYOVER, fp[fpindex].id);
      }
      
   }
   
   
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/whfs_lib/src/RPFEngine/RCS/get_fp_grp_county.c,v $";
 static char rcs_id2[] = "$Id: get_fp_grp_county.c,v 1.5 2007/02/22 15:20:21 deng Exp $";}
/*  ===================================================  */

}
