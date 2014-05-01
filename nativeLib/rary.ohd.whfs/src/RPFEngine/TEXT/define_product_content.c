/*********************************************************************
   define_product_content.c
   
   define_product_content()
   load_product_content()
   set_prod_fps()
   set_pccfilename()
   
   fill_pcc_impact_info()
   load_impact()
   fill_pcc_comparison_info()
   load_crest()
   
   check_impact_daterange()
   compare_date()
   
   ********************************************************************/

#include <string.h>                  /* string library functions */
#include <time.h>                    /* time library functions */
#include <stdlib.h>                  /* standard library functions */

#include <rating_util.h>              

#include "define_product_content.h"  /* function prototypes */
extern char paramdir[];


/*********************************************************************
   define_product_content()
   
   PURPOSE
   Define the name of the product definitions and load the contents
   of the product being generated.
   
   ********************************************************************/

void define_product_content(int				numfps,
			    fp_struct			*fp,
			    int				numgrps,
			    grp_struct			*grp,
			    int				numcnty,
			    county_struct		*cnty,
			    templatenames_struct	*templatenames,
			    misc_struct			*misc,
			    pcc_struct			*pcc,
			    vtecinfo_struct		*vtecinfo)
{   
   char pcc_file[MAXLEN_FILENAME];
   int	prod_index;

   
   /* if a pcc file was not specified on a command line, then set the name
      of the default pcc file to be used. */
   
   if (strcmp(misc->startup_pcc_file, "\0") == 0)
   {
      prod_index = convert_prodid_to_index(pcc->product.prod_categ);
      set_pccfilename(misc, prod_index, pcc_file);
   }
   else
      strcpy(pcc_file, misc->startup_pcc_file);
   
   
   /* load the definitions */
   
   load_product_content(numfps, fp, numgrps, grp, numcnty, cnty,
			pcc_file, templatenames, misc, 
			pcc, vtecinfo);
			
			                                     
   return;
}
 

/*********************************************************************
  load_product_content()
   
   PURPOSE
   Define the contents of the product being generated, already
   knowing the selected product to generate and the selected
   forecast points to include.
   
   NOTES
   
   ********************************************************************/
   
void load_product_content(int			numfps,
			  fp_struct		*fp,
			  int			numgrps,
			  grp_struct		*grp,
			  int			numcnty,
			  county_struct		*cnty,
			  char 			pcc_file[],
			  templatenames_struct	*templatenames,
			  misc_struct		*misc,
			  pcc_struct		*pcc,
			  vtecinfo_struct	*vtecinfo)
{   
   int 	i; 
   
   /* read the product content control file, whether it be
      the default or a user-supplied name */
   
   read_pcc(numfps, fp, numgrps, grp, pcc_file, pcc);
   
   
   /* when a new pcc definition is loaded, this may be for a different
      VTEC significance of for no VTEC, this will affect the obs data to
      use, so apply the filter */

   log_msg ("", "After pcc reload, update obs usage based on VTEC status.");
   printf("After pcc reload, update obs usage info based on VTEC status.\n");
   
   for (i = 0; i < numfps; i++)
   {     
      if (fp[i].numobsH > 0  && pcc->product.vtec_flag == TRUE)
      {
         apply_obsdata_filter(i, fp, vtecinfo, pcc); 
      }
      
      else
      {  
         fp[i].use_obsH        = fp[i].numobsH;
         fp[i].obs_cutoff_time = fp[i].obs_load_time;
      }
   }
    
    
   /* verify that the templates specified in the pcc file are valid */
   
   verify_templatenames(numfps, fp, numgrps, grp, templatenames, pcc);
   

   /* if the workstation mode is test or practice, force the VTEC mode 
      to be test also. */
   
   if (misc->workstation_mode == TEST_MODE ||
       misc->workstation_mode == PRACTICE_MODE)
      pcc->product.vtec_cat = VTEC_CAT_TEST;

     
   /* fill in the pcc information having to do with the 
      impact statements and the comparison section */
   
   fill_pcc_impact_info(numfps, fp, misc, pcc); 
   fill_pcc_comparison_info(numfps, fp, misc, pcc);
   
   
   /* set any product type and forecast points that may 
      be specified in the product file */
   
   set_prodfps(pcc, numfps, fp, numgrps, grp, misc);
   
   
   /* any expire times and issuance number info that might have been set 
      manually by the user should have those changes rejected since a 
      new set of pcc info has been read in, which includes new expire 
      time offsets. setting these vars below will result in them
      being recomputed automatically by the program. */
      
   misc->issuance_set = FALSE;
   misc->expire_set   = MISSINGVAL;
   
   
   /* initialize the vtec info since it can be affected
      by a new pcc file, depending on the vtec segment mode */
   
   init_vtecinfo(numfps, pcc, vtecinfo);
      
               
   return;
}


/*********************************************************************
   set_prodfps()
   
   PURPOSE
   Set the product type and/or forecast points to the values 
   specified in the pcc information, if any are given.
   
   NOTES
   
   ********************************************************************/
   
void set_prodfps(pcc_struct		*pcc,
		 int			numfps,
		 fp_struct		*fp,
		 int			numgrps,
		 grp_struct		*grp,
		 misc_struct		*misc)
{
   int i, j, cnt;
   int fpindex;
   
   /* if a set of points to include in the product was specified,
      then use it */
   
   if (pcc->product.include_fp_flag == TRUE)
   {
      for (i = 0; i < numfps; i++)
	 misc->fps_included[i] = pcc->product.include_fp[i];
      
      
      /* determine which groups (i.e. portions thereof) are included based
	 upon which forecast points are included */
      
      init_array(misc->grps_included, FALSE, numgrps);
      cnt = 0;
      for (i = 0; i < numgrps; i++)
      {
	 for (j = 0; j < grp[i].numfps; j++)
	 {
	    fpindex = grp[i].fpindex[j];
	    if (misc->fps_included[fpindex] == TRUE)
	    {
	       misc->grps_included[i] = TRUE;
	       cnt++;
	       break;
	    }
	 }
      }
      misc->numgrps_included = cnt;
   }
   
   return;
}


/*********************************************************************
   set_pccfilename()
   
   PURPOSE
   Set the name of the pcc file to use.

   
   NOTES
   Even though this function returns only one value, it is defined
   as a void function so this function need not allocate the memory
   for the string pointer.
   
   ********************************************************************/
void set_pccfilename(misc_struct	*misc,
		     int		prod_categ_index,
		     char 		pcc_file[])
{
   
   /* first define the path */
   
   sprintf(pcc_file, "%s/", paramdir);
   
   
   /* append the filename sans the office id */
   
   if (prod_categ_index == RVS)
      strcat(pcc_file, RVS_PCCFILE);
   
   else if (prod_categ_index == FLW)
      strcat(pcc_file, FLW_PCCFILE);
   
   else if (prod_categ_index == FLS)
      strcat(pcc_file, FLS_PCCFILE);
   
   
   /* log error message if product reason not recognized */
   
   else
      log_msg(BADPRODINDEX, "");
   
   
   /* append the selected office to the filename */
   
   strcat(pcc_file, ".");
   strcat(pcc_file, misc->selected_office);
   
   
   return;
   
}


/*********************************************************************
   fill_pcc_impact_info()
   
   PURPOSE
   Fill in which impact statement to include.  Specifically, it tries to
   fills in the value for lower_value, upper_value, and range_set.
   The automatic selection of the stage range performed by this 
   fucntion will result in only a single impact statement being
   included since the lower and upper limits of the range are actually
   identical.  To include multiple impact statements, the stage_range
   PCC keyword must be used to explicitly specify the range. 
   
   NOTES
   
   ********************************************************************/

void fill_pcc_impact_info(int		numfps,                  
			  fp_struct	*fp,
			  misc_struct	*misc,
			  pcc_struct	*pcc)
{
   int fpindex;
   int numstmt=0;
   float       refvalue;
   Floodstmt *floodstmtPtr = NULL;
   char        systemdate[DATE_LEN+1];
   struct tm   *system_time;
   char       where[MAXLEN_STRING];   
   char       loc_id[LOC_ID_LEN + 1]="";
   float      final_refvalue;
   char       imp_pe;
           
   /* convert the format of the updated current time */

   time(&misc->system_time);
   system_time = gmtime(&misc->system_time);   
   strftime(systemdate, DATE_LEN + 1, "%m/%d/%Y", system_time);  
   
   log_msg("", "Determining recommended impact statements to include...");
   
   
   /* loop on the number of forecast points */
   
   for (fpindex = 0; fpindex < numfps; fpindex++)
   {      
      /* initialize the values that indicate the reference value and the
	 reference value type for each forecast point; these are
	 used for template variables */
      
      pcc->impact.refstage[fpindex] = MISSINGVAL;
      pcc->impact.reftype[fpindex]  = MISSINGVAL;
      
      if (fp[fpindex].id != NULL)
         strcpy(loc_id, fp[fpindex].id); 
   
      /* only consider those forecast points which have not had their
	 impact stage range set */
      
      if (pcc->impact.range_set[fpindex] == FALSE && 
	  (fp[fpindex].curobs_cat  != NULLCAT || 
	   fp[fpindex].maxfcst_cat != NULLCAT))
      {	 	 
	 refvalue = MISSINGVAL;
	 final_refvalue = MISSINGVAL;
	 
	 /* find the reference value to use for the search
	 the CUR_OBSSTAGE,MAX_FCSTSTAGE and MAX_STAGE could be stage
	 or flow depending on the Primary PE */
	 
	 if (pcc->impact.refstage_index == CUR_OBSSTAGE)
	 {
	    if (fp[fpindex].curobs.value != MISSINGVAL)
	    {
	       refvalue = fp[fpindex].curobs.value;
	       pcc->impact.reftype[fpindex] = CUR_OBSSTAGE;
	    }
	    
	    
	    /* try the forecast value if the observed is missing */
	    
	    else if (fp[fpindex].maxfcst.value != MISSINGVAL)
	    {
	       refvalue = fp[fpindex].maxfcst.value;
	       pcc->impact.reftype[fpindex] = MAX_FCSTSTAGE;
	    }
	 }
	 
	 else if (pcc->impact.refstage_index == MAX_FCSTSTAGE)
	 {
	    if (fp[fpindex].maxfcst.value != MISSINGVAL)
	    {
	       refvalue = fp[fpindex].maxfcst.value;
	       pcc->impact.reftype[fpindex] = MAX_FCSTSTAGE;
	    }
	    
	    
	    /* try the observed value if the forecast is missing */
	    
	    else if (fp[fpindex].curobs.value != MISSINGVAL)
	    {
	       refvalue = fp[fpindex].curobs.value;
	       pcc->impact.reftype[fpindex] = CUR_OBSSTAGE;
	    }
	 }
	 
	 else if (pcc->impact.refstage_index == MAX_STAGE)
	 {
	    if ((int )fp[fpindex].omf != MISSINGVAL)
	    {
	       refvalue = fp[fpindex].omf;
	       if (refvalue == (float )fp[fpindex].curobs.value)
		  pcc->impact.reftype[fpindex] = CUR_OBSSTAGE;
	       else
		  pcc->impact.reftype[fpindex] = MAX_FCSTSTAGE;
	    }
	 }
	 
	 /*determine the final refvalue depending on if this
	 points is based on primary pe or not*/
	 
	 if (strcmp(fp[fpindex].rec_type, "NPE") == 0)
	 {
	    if (fp[fpindex].pe[0] == 'Q')
	       final_refvalue = discharge2stage(loc_id,refvalue);
	    else
	       final_refvalue = stage2discharge(loc_id,refvalue);
	 }
	 else
	 {
	     final_refvalue = refvalue;
	 }
	          
	 /* check if the reference value, if found, is within a
	    certain amount from the flood value, if available. this 
	    prevents impacts from being included for very low values. */
	 
	 if ((strcmp(fp[fpindex].rec_type, "NPE") == 0 &&
	     fp[fpindex].pe[0] != 'Q') ||
	     (strcmp(fp[fpindex].rec_type, "PE") == 0 &&
	     fp[fpindex].pe[0] == 'Q'))
	 {
	   imp_pe = 'Q';
	   
	   if (final_refvalue != MISSINGVAL && fp[fpindex].fq != MISSINGVAL)
	   {
	     if (final_refvalue < (fp[fpindex].fq
	                  *(1 - (pcc->impact.fq_filter_offset/100.0))))
	     {
	       final_refvalue = MISSINGVAL;
	       pcc->impact.reftype[fpindex] = MISSINGVAL;
	     }
	   }
	 }
	 else	 
	 {
	   imp_pe = 'H';
	   
	   if (final_refvalue != MISSINGVAL && fp[fpindex].fs != MISSINGVAL)
	   {
	     if (final_refvalue < (fp[fpindex].fs + pcc->impact.fs_filter_offset))
	     {
	       final_refvalue = MISSINGVAL;
	       pcc->impact.reftype[fpindex] = MISSINGVAL;
	     }
	   }
	 }   
	 
	 
	 /* now search for impact, but only if valid reference value */
	 
	 if (final_refvalue != MISSINGVAL)
	 {	    
	    pcc->impact.refstage[fpindex] = final_refvalue;
	                
	    /* load the impact information for the forecast point  */
	    
	    sprintf (where, " WHERE lid = '%s' AND impact_pe like '%c%%' ORDER BY impact_value DESC", 
	             fp[fpindex].id, imp_pe);
		     
	    floodstmtPtr = GetFloodstmt(where);
	    
	    if (floodstmtPtr == NULL)
	    {
	       if (misc->fps_included[fpindex] == TRUE)
		  log_msg(MISSING_IMPACT, fp[fpindex].id);
	    	numstmt = 0;
	    }
	    
	    else
	    {
	       numstmt = ListCount(&floodstmtPtr->list);
	       load_impact(fpindex, final_refvalue, systemdate,
	       floodstmtPtr, pcc, fp);
	       FreeFloodstmt(floodstmtPtr);
	    }
	 }
	 
	 else
	 {
	    /* log_msg(MISSING_REFIMP, fp[fpindex].id); */
	 }
	 
      }     /* end of if for whether impact not set yet or no stage data */
   }        /* end of loop on number of forecast points */
   
   return;
}


/*********************************************************************
   load_impact()
   
   PURPOSE
   Loads the appropriate impact statement, if one exists, for the
   forecast point.
   
   NOTES
   This function does not base its selection on the value of the 
   rising and falling flag.  If multiple records meet the filter 
   criteria and some are rising and some are falling, then the
   last one considered wins.
   
   ********************************************************************/
void load_impact(int		fpindex,
		 float		refvalue,
		 char		systemdate[],
		 Floodstmt	*floodstmtPtr,
		 pcc_struct	*pcc,
		 fp_struct      *fp)
{
   int 		within_dates;
   float 	value_diff, cur_diff;
   float 	lowervalue, uppervalue;
   Floodstmt 	*stmt = NULL;
   char 	startdate[DATE_LEN+1], enddate[DATE_LEN+1];
  
   
   /* initialize the stage difference to some high value  */
   
   value_diff = 320000;
   
   if ((strcmp(fp[fpindex].rec_type, "NPE") == 0 &&
       fp[fpindex].pe[0] != 'Q') ||
       (strcmp(fp[fpindex].rec_type, "PE") == 0 &&
       fp[fpindex].pe[0] == 'Q'))
              
   {    
      lowervalue = refvalue * (1 - (pcc->impact.flow_lwindow/100.0));
      uppervalue = refvalue * (1 + (pcc->impact.flow_uwindow/100.0));
   }
   else
   {    
      lowervalue = refvalue + pcc->impact.stage_lwindow;
      uppervalue = refvalue + pcc->impact.stage_uwindow;
   }
   pcc->impact.lower_stage[fpindex] = 320000.;
   pcc->impact.upper_stage[fpindex] = -100.;	    
   
   
   /* now loop on the impact statements defined for this
      forecast point, and knowing the reference stage, find the
      impact statements with the closest stage to the the specified stage */
   
   if (floodstmtPtr != NULL)
	stmt = (Floodstmt *)ListFirst(&floodstmtPtr->list);

   while (stmt != NULL)
   {      
      
      /* check that the current system date is within the 
	 time of year defined for this impact statement */
      
      sprintf(startdate, "%s/1970", stmt->datestart);
      sprintf(enddate,   "%s/1970", stmt->dateend);
      
      within_dates = 
	 check_impact_daterange(systemdate, startdate, enddate);
      
      if (within_dates == TRUE)
      {	    	 
	 /* only consider the impact stage if it is below the upper
	    limit of the value window and it is either above the
	    lower limit, or we are using the option that says take
	    all of the impacts below the upper window. Note the impact
	    stage in the FLoodStmt table could be stage value or flow*/
	 
	 if (stmt->impact_value <= uppervalue &&
	     (pcc->impact.search_type == BELOW_UPPER_STGWINDOW ||
	      stmt->impact_value >= lowervalue))	 
	 {
	    /* check if this value is the best based on the search type
	       and if so, load in the info */
	    
	    if (pcc->impact.search_type == CLOSEST_IN_STGWINDOW)
	    {
	       absdiff(refvalue, stmt->impact_value, cur_diff);
	       
	       if (cur_diff < value_diff)
	       {
		  pcc->impact.lower_stage[fpindex] = stmt->impact_value;
		  pcc->impact.upper_stage[fpindex] = stmt->impact_value;	    
		  pcc->impact.range_set[fpindex]   = TRUE;
		  
		  /* determine the absolute diff for later comparison */
		  
		  absdiff(refvalue, stmt->impact_value, value_diff);
	       }
	    }
	    
	    else if (pcc->impact.search_type == HIGHEST_IN_STGWINDOW)
	    {
	       if (pcc->impact.range_set[fpindex] != TRUE ||
		   stmt->impact_value > pcc->impact.lower_stage[fpindex])
	       {
		  pcc->impact.lower_stage[fpindex] = stmt->impact_value;
		  pcc->impact.upper_stage[fpindex] = stmt->impact_value;	    
		  pcc->impact.range_set[fpindex]   = TRUE;
	       }
	    }
	    
	    else if (pcc->impact.search_type == BELOW_UPPER_STGWINDOW)
	    {
	       if (stmt->impact_value > pcc->impact.upper_stage[fpindex])
		  pcc->impact.upper_stage[fpindex] = stmt->impact_value;
	       if (stmt->impact_value < pcc->impact.lower_stage[fpindex])
		  pcc->impact.lower_stage[fpindex] = stmt->impact_value;	    
	       pcc->impact.range_set[fpindex]   = TRUE;
	    }
	 }
      }
      stmt = (Floodstmt *)ListNext(&stmt->node);
   }  
   
   return;
}


/*********************************************************************
   fill_pcc_comparison_info()
   
   PURPOSE

   Fill in which crest comparison statement to include.
   Specifically, it tries to fill in the values compare_date, compare_stage,
   and compare_set.  These values are then used when creating the
   comparison subsection.
   
   NOTES
   Current method only allows one previous crest to be considered
   for inclusion in product. Also, it does not re-search using "larger"
   windows if a crest is not found within the stage and time windows.
   
   ********************************************************************/

void fill_pcc_comparison_info(int		numfps,
			      fp_struct		*fp,
			      misc_struct	*misc,
			      pcc_struct	*pcc)
{
   int 		fpindex;
   int 		numcrest;
   float 	refstage, final_refstage;
   char 	begin_time[DATE_LEN+1];
   char 	where[MAXLEN_STRING];
   struct tm 	*system_time;
   Crest 	*crestPtr;
   char         loc_id[LOC_ID_LEN + 1] = "";         
         
   
   /* define the earliest time to be considered by subtracting
      the lookback years from the updated system date */
   
   time(&misc->system_time);   
   system_time = gmtime(&misc->system_time);
   sprintf(begin_time, "01/01/%i", 
	   system_time->tm_year + 1900 - pcc->comparison.lookback_years);
   
   log_msg("", "Determining recommended crest comparisons to include...");
   
   
   /* loop on the number of forecast points */
   
   for (fpindex = 0; fpindex < numfps; fpindex++)
   {
      /* initialize the values that indicate the reference stage and the
	 reference stage type for each forecast point; these are
	 used for template variables */
      
      pcc->comparison.refstage[fpindex] = MISSINGVAL;
      pcc->comparison.reftype[fpindex]  = MISSINGVAL;
            
      if (fp[fpindex].id != NULL)
        strcpy(loc_id, fp[fpindex].id); 
      
      /* only consider those forecast points which have not yet had their
	 the crest comparison data set and which have some data */
      
      if (pcc->comparison.compare_set[fpindex] == FALSE &&
	  (fp[fpindex].curobs_cat != NULLCAT ||
	   fp[fpindex].maxfcst_cat != NULLCAT)) 
      {	 
	 refstage = MISSINGVAL;
	 final_refstage = MISSINGVAL;
	 
	 /* find the stage window to use for the search;
	    if there is no reference value available, then do not
	    set any comparison date; the variable index
	    is used later to determine whether a stage was found */
	 
	 if (pcc->comparison.refstage_index == CUR_OBSSTAGE)
	 {
	    if (fp[fpindex].curobs.value != MISSINGVAL)
	    {
	       refstage = fp[fpindex].curobs.value;
	       pcc->comparison.reftype[fpindex] = CUR_OBSSTAGE;
	    }
	    
	    
	    /* if reference value missing, try max forecast */
	    
	    else if (fp[fpindex].maxfcst.value != MISSINGVAL)
	    {
	       refstage = fp[fpindex].maxfcst.value;
	       pcc->comparison.reftype[fpindex] = MAX_FCSTSTAGE;
	    }	    
	 }
	 
	 else if (pcc->comparison.refstage_index == MAX_FCSTSTAGE)
	 {
	    if (fp[fpindex].maxfcst.value != MISSINGVAL)
	    {
	       refstage = fp[fpindex].maxfcst.value;
	       pcc->comparison.reftype[fpindex] = MAX_FCSTSTAGE;
	    }
	    
	    
	    /* if reference value missing, try observed */
	    
	    else if (fp[fpindex].curobs.value != MISSINGVAL)
	    {
	       refstage = fp[fpindex].curobs.value;
	       pcc->comparison.reftype[fpindex] = CUR_OBSSTAGE;
	    }
	 }
	 
	 else if (pcc->comparison.refstage_index == MAX_STAGE)
	 {
	    if ((int )fp[fpindex].omf != MISSINGVAL)
	    {
	       refstage = fp[fpindex].omf;
	       if (refstage == fp[fpindex].curobs.value)
		  pcc->comparison.reftype[fpindex] = CUR_OBSSTAGE;
	       else
		  pcc->comparison.reftype[fpindex] = MAX_FCSTSTAGE;
	    }	       
	 }
	 
	 /*determine the final refvalue depending on if this
	 points is based on primary pe or not*/
	 
	 if (strcmp(fp[fpindex].rec_type, "NPE") == 0)
	 {
	    if (fp[fpindex].pe[0] == 'Q')
	       final_refstage = discharge2stage(loc_id,refstage);
	    else
	       final_refstage = stage2discharge(loc_id,refstage);
	 }
	 else
	 {
	     final_refstage = refstage;
	 }
	 
	 /* check if the final reference stage, if found, is within a
	    certain amount from the flood stage, if available. this 
	    prevents crests from being included for very low values. */
	 	 
	 if ((strcmp(fp[fpindex].rec_type, "NPE") == 0 &&
	     fp[fpindex].pe[0] != 'Q') ||
	     (strcmp(fp[fpindex].rec_type, "PE") == 0 &&
	     fp[fpindex].pe[0] == 'Q'))
	 {    
	   if (final_refstage != MISSINGVAL && fp[fpindex].fq != MISSINGVAL)
	   {
	      if (final_refstage < (fp[fpindex].fq * 
	                    (1 - (pcc->comparison.fq_filter_offset/100.0))))
	      {
		 final_refstage = MISSINGVAL;
		 pcc->comparison.reftype[fpindex] = MISSINGVAL;
	      }
	   }	 	 
	 }
	 else
	 {
	   if (final_refstage != MISSINGVAL && fp[fpindex].fs != MISSINGVAL)
	   {
	      if (final_refstage < (fp[fpindex].fs + pcc->comparison.fs_filter_offset))
	      {
		 final_refstage = MISSINGVAL;
		 pcc->comparison.reftype[fpindex] = MISSINGVAL;
	      }
	   }
	 
	 }
	 /* if value found for particular forecast point */
	 
	 if (final_refstage != MISSINGVAL)
	 {	    	    
	    pcc->comparison.refstage[fpindex] = final_refstage;
	    
	    
	    /* load the crest data for this forecast point.
	       The crest data is base on flow or stage will depend on
	       how the rec_type field is set and whis is the primary PE value.
	       For example, for NPE rec_type, if the primary PE is stage, the
	       the final value should be flow, the query will look for flow
	       from crest table*/
	       	    
	    if (((strcmp(fp[fpindex].rec_type, "NPE") == 0) &&
	          fp[fpindex].pe[0] != 'Q') ||
	        ((strcmp(fp[fpindex].rec_type, "PE") == 0) &&
	          fp[fpindex].pe[0] == 'Q'))
	    {	
	        sprintf (where, " WHERE lid = '%s' AND q IS NOT NULL ",
		                   fp[fpindex].id);
            }				   
	    else	    
	        sprintf (where, " WHERE lid = '%s' AND stage IS NOT NULL ",   
				 fp[fpindex].id);
		     
		     
	    crestPtr = GetCrest(where);
	    
	    if (crestPtr == NULL)
	    {
	       numcrest = 0;
	       if (misc->fps_included[fpindex] == TRUE)
		  log_msg(MISSING_HISTCREST, fp[fpindex].id);
	    }
	    
	    else
	    {
	       numcrest = ListCount(&crestPtr->list);
	       load_crest(fpindex, final_refstage, begin_time, 
	                  crestPtr, pcc, fp);
	       
	       /* free the table memory */
	       
	       FreeCrest(crestPtr);
	    }
	    
	 }  
	 
	 else
	 {
	    /* log_msg(MISSING_REFCREST, fp[fpindex].id); */
	 }
	 
      }      /* end of if block on whether crest not set */
   }         /* end of loop on forecast points */
   
   return;
}


/*********************************************************************
   load_crest()
   
   PURPOSE
   Determine the appropriate crest value for a forecast point.
   
   NOTES
   
   ********************************************************************/
void load_crest(int 		fpindex,
		float 		refstage,
		char		begin_time[],
		Crest		*crestPtr,
		pcc_struct	*pcc,
		fp_struct       *fp)
{   
   Crest        *crest = NULL, *comp_crest = NULL;
   float 	f1, f2;
   int 		timeok, loadcrest;
   float 	stage_lower, stage_upper, comp_value;
   char 	crestdate[DATE_LEN+1] = "";
   char         loc_id[LOC_ID_LEN + 1]=""; 
   float        small_offset;
   
   /*initialize comp_value*/
   
   comp_value = -9999.0;
   small_offset = 0.001;
   
   if (fp[fpindex].id != NULL)
      strcpy(loc_id, fp[fpindex].id);
   
   /* define the stage limits */
   
   if ((strcmp(fp[fpindex].rec_type, "NPE") == 0 &&
       fp[fpindex].pe[0] != 'Q') ||
      (strcmp(fp[fpindex].rec_type, "PE") == 0 &&
       fp[fpindex].pe[0] == 'Q'))
   {
     stage_lower = refstage * (1 - (pcc->comparison.flow_lwindow/100.0));
     stage_upper = refstage * (1 + (pcc->comparison.flow_uwindow/100.0));    
   }  
   else
   {
     stage_lower = refstage + pcc->comparison.stage_lwindow;
     stage_upper = refstage + pcc->comparison.stage_uwindow;
  
   }
   /* now loop on the historical crests defined for this
      forecast point, and knowing the reference stage/flow window and 
      the time lookback period, find the most recent crest statement
      that meets the criteria */

   if (crestPtr != NULL)
	crest = (Crest *)ListFirst(&crestPtr->list);

   while (crest != NULL)
   {
      if (strcmp(fp[fpindex].rec_type, "NPE") == 0)
      {
         if (fp[fpindex].pe[0] == 'Q')
	    comp_value = discharge2stage(loc_id, crest->q);
	 else
	    comp_value = stage2discharge(loc_id, crest->stage);
      }
      else
      { 	       	 
	 if (fp[fpindex].pe[0] == 'Q')
            comp_value = crest->q;
	 else
            comp_value = crest->stage;
      }  	 	       	       
      /* all of the search types assume that the historical stage
	 must be within the defined stage window */
      
      if (comp_value >= stage_lower && comp_value <= stage_upper)
      {	
	  /* convert the database date format into compatible
	  format, based on the new comp_value, find the crest date
	  corresponding to this comp_value */
	 
	 
	 if (crestPtr != NULL)
	     comp_crest = (Crest *)ListFirst(&crestPtr->list);

	 while (comp_crest != NULL)
	 {
             if ((strcmp(fp[fpindex].rec_type, "NPE") == 0 &&
                  fp[fpindex].pe[0] != 'Q') ||
                 (strcmp(fp[fpindex].rec_type, "PE") == 0 &&
                  fp[fpindex].pe[0] == 'Q'))
             {
	         if (fabs(comp_crest->q - comp_value) <= small_offset)
		 {
		    date_t_to_USA_date(comp_crest->datcrst, crestdate);
		    break;
		 }
	     }
	     else
	     {
	         if (fabs(comp_crest->stage - comp_value) <= small_offset)
		 {
		    date_t_to_USA_date(comp_crest->datcrst, crestdate);
		    break;
		 }
             }
	     
	     		    	       
	     comp_crest = (Crest *)ListNext(&comp_crest->node);
	 
	  }/*end of loop of comp_crest*/
	   
	 /* if the search type uses a time window, then check if
	    the crest date is after the earliest allowable time */
	 
	 if (pcc->comparison.search_type == RECENT_IN_WINDOWS ||
	     pcc->comparison.search_type == CLOSEST_IN_WINDOWS)
	 {
	    if (compare_date(crestdate, begin_time) > 0)
	       timeok = TRUE;
	    else
	       timeok = FALSE;
	 }
	 else
	    timeok = TRUE;
	 
	 
	 /* if the time check is ok, then continue */
	 
	 if (timeok)
	 {			     
	    /* if this is the first crest that meets the criteria
	       so far, then load it regardless; if this is not
	       the first crest statement, then check if it is
	       better than the previously found crest, as defined
	       by criteria specific to each search type */
	    
	    if (pcc->comparison.compare_set[fpindex] == FALSE)
	    {
	       strcpy(pcc->comparison.compare_date[fpindex],
		      crestdate);	       
	       pcc->comparison.compare_stage[fpindex] =
	                                         comp_value;
	       pcc->comparison.compare_set[fpindex] = TRUE;
	    }
	    
	    else
	    {
	       loadcrest = FALSE;
	       	 
	       
	       /* use the more recent crest */
	       
	       if (pcc->comparison.search_type == RECENT_IN_WINDOWS ||
		   pcc->comparison.search_type == RECENT_IN_STGWINDOW)
	       {		  
		  if (compare_date(crestdate, 
			       pcc->comparison.compare_date[fpindex]) > 0)
		     loadcrest = TRUE;
	       }
	       
	       
	       /* use the stage closest to the reference stage */
	       
	       else if (pcc->comparison.search_type == CLOSEST_IN_STGWINDOW ||
			pcc->comparison.search_type == CLOSEST_IN_WINDOWS)
	       {		  
		  absdiff(comp_value, refstage, f1);
		  absdiff(pcc->comparison.compare_stage[fpindex], refstage, f2);
		  if (f1 <= f2) loadcrest = TRUE;
	       }
	       
	       
	       /* use the highest stage within the stage window */
	       
	       else if(pcc->comparison.search_type == HIGHEST_IN_STGWINDOW)
	       {		  
		  if (comp_value > pcc->comparison.compare_stage[fpindex])
		     loadcrest = TRUE;
	       }
	       
	       
	       /* if crest is better, then load it */
	       
	       if (loadcrest == TRUE)
	       {
		  strcpy(pcc->comparison.compare_date[fpindex], crestdate);
		  pcc->comparison.compare_stage[fpindex] = comp_value;
	       }
	    }
	 }   /* end of if check on whether time is okay */		  
      }  /* end of if check on whether stage within limits */
      
      crest = (Crest *)ListNext(&crest->node);
      
   }  /* end of loop on number of crests */
   
   return;
}


/*********************************************************************
   check_impact_daterange()
   
   PURPOSE
   Check if the system time is within the date range specified 
   for the impact statement.
   
   NOTES
   
   ********************************************************************/
int check_impact_daterange(char system_time[],
			   char start[],
			   char end[])
{
   int within_dates;
   
   int sysday, sysmonth, sysnum;
   int error_flag;
   int day1, month1, num1;
   int day2, month2, num2;
   int numread;
   
   /* initialize */
   
   error_flag = FALSE;
   within_dates= FALSE;
   
   
   /* extract the day and month fields and convert to number */
   
   numread = sscanf(start, "%d/%d", &month1, &day1);
   if (numread != 2)
   {
      log_msg(INVALID_DATE, start);
      error_flag = TRUE;
   }
   num1 = month1*40 + day1;
   
   numread = sscanf(end, "%d/%d", &month2, &day2);
   if (numread != 2)
   {
      log_msg(INVALID_DATE, end);
      error_flag = TRUE;
   }
   num2 = month2*40 + day2;

   numread = sscanf(system_time, "%d/%d", &sysmonth, &sysday);
   if (numread != 2)
   {
      log_msg(INVALID_DATE, end);
      error_flag = TRUE;
   }
   sysnum = sysmonth*40 + sysday;
   
   
   /* initialize */
   
   within_dates = FALSE;
   if (num1 == num2 || error_flag) return(within_dates);
   
   
   /* do the check differently depending on whether the start date is
      before the end date (e.g. Jan-Oct) or is after (e.g. Nov-Feb),
      such that it straddles the new year */
   
   if (num1 < num2)
   {
      if (num1 <= sysnum && sysnum <= num2) within_dates = TRUE;
   }
   
   
   /* if system date past the start date (e.g. Dec past Nov)
      or before the end date (e.g. Jan before Feb),
      then it is within the range */
   
   else
   {     
      if (num1 <= sysnum || sysnum <= num2 ) within_dates = TRUE;     
   }
   
   return(within_dates);
}


/*********************************************************************
   compare_date()
   
   PURPOSE
   Compares two dates given in the mm/dd/yyyy format to see which
   is earlier than the other.
      
   NOTES
   Returns value of date1 - date2 as either -1, 0, +1.
   
   ********************************************************************/
int compare_date(char	date1[],
		 char	date2[])
{
   int day1, month1, year1;
   int day2, month2, year2;
   int numread;
   int reldiff;
   int error_flag;
   
   
   /* initialize */
   
   reldiff = 0;
   error_flag = FALSE;
   
   
   /* extract the day, month, and year fields */
   
   numread = sscanf(date1, "%d/%d/%d", &month1, &day1, &year1);
   if (numread != 3)
   {
      log_msg(INVALID_DATE, date1);
      error_flag = TRUE;
   }

   
   /* extract the day, month, and year fields */
   
   numread = sscanf(date2, "%d/%d/%d", &month2, &day2, &year2);
   if (numread != 3)
   {
      log_msg(INVALID_DATE, date2);
      error_flag = TRUE;
   }
   
   if (error_flag == TRUE) return(0);
   

   /* see whether the two dates are equal */
   
   if (day1 == day2 && month1 == month2 && year1 == year2)
      reldiff = 0;
   
   
   /* if not equal, see which one is earlier */
   
   else
   {
      if (year1 > year2) 
	 reldiff = +1;
      else if (year1 < year2)
	 reldiff = -1;
      else
      {
	 if (month1 > month2)
	    reldiff = +1;
	 else if (month1 < month2)
	    reldiff = -1;
	 else
	 {
	    if (day1 > day2)
	       reldiff = +1;
	    else
	       reldiff = -1;
	 }
      }
   }
   
   return(reldiff);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
