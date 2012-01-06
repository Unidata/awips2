 /*********************************************************************
   load_variable_value.c
   
   These functions are responsbile for retrieving and loading most of the
   template variables.  The exception is the physical-element (PE) variables.
   

   load_variable_value()
   
   load_fp_variable_value()
   load_locinfo_variable_value()
   load_grp_variable_value()
   load_pcc_variable_value()
   
   load_stagegrp_variable_value()
   
   load_stage_ofp_variable_value()   
   load_stage_ffp_variable_value()
   load_stage_xfp_variable_value()
   load_stagespec_variable_value()
   
   load_misc_variable_value()
   
   load_stage_trendfp_vairable_value()
   load_trendfp_phrase()
   load_trendfp_stage()
   load_trendfp_time()
   
   MODIFICATION HISTORY      DATE         DESCRIPTION
   load_stage_xfp_variable_value()        load <StgFlowName>
                                               <StgFlowUnits> 
							
   load_event_variable_value() 08/2004    load <Action>, <HSA> <HSAname>
                                          <EventBeginTime>, <EventEndTime>,
				          <PrevEventBeginTime>, <PrevEventEndTime>,
					  <VRTime>, <VCTime>,
					  <VFTime>, <VCValue>
					  
				          <LocCntyList>, <ActionStateCntyList>
					  <ActionStateList>
   load_fp_variable_value()               <PrevCat>, <PrevCatName>,
                                          <PrevObsCat>, <PrevObsCatName>,
				          <PrevMaxFcstCat>,<PrevMaxFcstCatName>   
				          <ImpCompUnits>
					     
   *********************************************************************/

#include <string.h>           
#include <stdlib.h>          
#include <stdio.h>
#include <ctype.h>

#include "load_variable_value.h"
#include "Location.h"


extern template_variable_struct TEMPLATE_VARIABLES_TABLE[];


/*********************************************************************
   load_variable_value()
   
   PURPOSE
   Loads the value of a single variable into a union variable that
   can hold integer, float, or string.

   
   NOTES
   The longstring variable is used to return the value if it is
   contained within a long character string.  This method is used
   to allow this same function to be used for loading variable
   values for both the condition stack and the phrase subsitution,
   while avoiding the waste of space for the values for each item
   in the condition stack simply to accomodate the very few 
   strings that are large...
  
   *******************************************************************/
void load_variable_value(fp_struct	  	*fp,                          
		         grp_struct   	        *grp,
		 	 const int              numcnty,
			 county_struct          *cnty,
			 misc_struct		*misc,
			 vtecinfo_struct        *vtecinfo,
			 pcc_struct	        *pcc,
			 varinfo_struct         *varinfo,
			 const int 		idindex,
			 char			locid[],
			 const spectime_struct	*spectime,
			 const int		spectime_index,
			 values		        *rawvalue,
			 char			dqcode[],
			 char			longstring[])   
{         
   int struct_index;
   int checklen;
   char msgstr[MAXLEN_STRING];
   int missingflag;
   int varindex;
   
   
   /* initialize */
   
   varindex = MISSINGVAL;
   
   if (varinfo == NULL)
      printf("ERROR: varinfo is invalid\n");
   else
      varindex = varinfo->varindex;
      
         
   /* initialize to missing values */
   
   memset(dqcode,     0, 1);
   memset(longstring, 0, 1);
   
   if (TEMPLATE_VARIABLES_TABLE[varindex].type == VAR_INT)
      rawvalue->i = MISSINGVAL;
   
   else if (TEMPLATE_VARIABLES_TABLE[varindex].type == VAR_FLT)
      rawvalue->f = (float )MISSINGVAL;
   
   else if (TEMPLATE_VARIABLES_TABLE[varindex].type == VAR_TIM)
      rawvalue->t = (time_t )MISSINGVAL;
   
   else   
      memset(rawvalue->s, 0, 1);
   
   
   /* get the index to the structure that contains the data;
      for those variables that are keyed by a forecast group or
      forecast point, make sure that there is a valid index 
      to the group or point defined;  */   
   
   struct_index = TEMPLATE_VARIABLES_TABLE[varindex].source_struct;      
   
   
   /* note that for general "fp" variables, it no longer has to be
      for a forecast point in most cases */
   
   if (struct_index == FP)
      load_fp_variable_value(varindex, idindex, locid, fp, vtecinfo, pcc,
                             misc, rawvalue, longstring);
			     
   else if (struct_index == LOCINFO)
      load_locinfo_variable_value(varindex, locid, rawvalue, longstring);
			        
   else if (struct_index == GRP && idindex != MISSINGVAL)
      load_grp_variable_value(varindex, idindex, grp, rawvalue, pcc, numcnty,
                              cnty);
   
   else if (struct_index == PCC && idindex != MISSINGVAL)
      load_pcc_variable_value(varindex, idindex, pcc, rawvalue);
   
   else if (struct_index == STAGES_SPEC && idindex != MISSINGVAL)
      load_stagespec_variable_value(varindex, idindex, fp,
				    spectime, spectime_index,
				    rawvalue, dqcode);
   
   else if (struct_index == STAGES_FP_OBS   && idindex != MISSINGVAL)
      load_stage_ofp_variable_value(varindex, idindex, fp, 
				    misc, rawvalue, dqcode);

   else if (struct_index == STAGES_FP_FCST  && idindex != MISSINGVAL)
      load_stage_ffp_variable_value(varindex, idindex, fp, 
				    misc, rawvalue, dqcode);

   else if (struct_index == STAGES_FP_OTHER && idindex != MISSINGVAL)
      load_stage_xfp_variable_value(varindex, idindex, fp, 
				    misc, rawvalue);
   
   else if (struct_index == STAGES_GRP && idindex != MISSINGVAL)
      load_stagegrp_variable_value(varindex, idindex, grp, misc, rawvalue,
                                   cnty, pcc);   
         
   else if (struct_index == MISC)
      load_misc_variable_value(varindex, idindex, grp, fp, misc, pcc, rawvalue,
			       longstring, numcnty, cnty, vtecinfo);
   
   /* load detailed trend phrases for forecast data*/ 			       
   
   else if (struct_index == STAGES_FP_TREND && idindex != MISSINGVAL)
      load_stage_trendfp_variable_value(varindex, idindex, fp, longstring, misc, pcc);
      
   /*load data based on events*/
      load_event_variable_value(varindex, idindex, locid, fp, grp, rawvalue, 
                                vtecinfo, pcc, misc, longstring);
                               
      
   /*-----------------------------------------------------------------*/
   /* check string lengths and issue warning messages for missing data */
   
   /* double check that the strings are not overloaded; 
      these checks assume the length of the strings !!! */
   
   if (TEMPLATE_VARIABLES_TABLE[varindex].type == VAR_STR)
   {
      checklen = strlen(rawvalue->s);
      if (checklen > MAXLEN_VALUESTR)
      {
	 sprintf(msgstr, "in load...value, %d chars", checklen);
	 log_msg(EXCEED_VALUESTR, msgstr);
      }
      checklen = strlen(longstring);
      if (checklen > MAXLEN_LONGSTR)
      {
	 sprintf(msgstr, "in load...value, %d chars", checklen);
	 log_msg(EXCEED_LONGSTR, msgstr);
      }
      
      /* issue warning message if a missing category string is being 
	 returned. text of message depends on whether processing
	 forecast point data, group data, or other data.
	 note that cat names are assumed to be the only string type
	 variable that can be missing. */
      
      if (strcmp(rawvalue->s, misc->miss_cat) == 0)
      {
	 memset(msgstr, 0, MAXLEN_STRING);
	 if (struct_index == GRP || struct_index == STAGES_GRP)
	 {
	    if (idindex == MISSINGVAL)
	       sprintf(msgstr, "Undefined group index for %s", 
		       TEMPLATE_VARIABLES_TABLE[varindex].name);
	    else
	       sprintf(msgstr, "%s %s", grp[idindex].id,
		       TEMPLATE_VARIABLES_TABLE[varindex].name);
	 }
	 
	 else if (struct_index == MISC)
	    sprintf(msgstr, "%s", TEMPLATE_VARIABLES_TABLE[varindex].name);
	 
	 else
	 {
	    if (idindex == MISSINGVAL)
	       sprintf(msgstr, "Undefined location index %s", 
		       TEMPLATE_VARIABLES_TABLE[varindex].name);
	    else
	       sprintf(msgstr, "%s %s", fp[idindex].id,
		       TEMPLATE_VARIABLES_TABLE[varindex].name);
	 }
	 
	 log_msg(MISSING_CATNAME, msgstr);
      }
   }
   
   
   /* if dealing with numeric data, check for missing data */
   
   else
   {
      /* check if the value is missing */
      
      missingflag = FALSE;
      
      if (TEMPLATE_VARIABLES_TABLE[varindex].type == VAR_INT &&
	  rawvalue->i == MISSINGVAL)
	 missingflag = TRUE;
      else if (TEMPLATE_VARIABLES_TABLE[varindex].type == VAR_FLT &&
	       (int )rawvalue->f == MISSINGVAL)
	 missingflag = TRUE;
      else if (TEMPLATE_VARIABLES_TABLE[varindex].type == VAR_TIM &&
	       (int )rawvalue->t == MISSINGVAL)
	 missingflag = TRUE;
      
      
      /* if value missing, then build message string and display warning */
      
      if (missingflag == TRUE)
      {
	 memset(msgstr, 0, MAXLEN_STRING);	 
	 if (struct_index == GRP || struct_index == STAGES_GRP)
	 {
	    if (idindex == MISSINGVAL)
	       sprintf(msgstr, "undefined group index for %s", 
		       TEMPLATE_VARIABLES_TABLE[varindex].name);
	    else
	       sprintf(msgstr, "%s %s", grp[idindex].id,
		       TEMPLATE_VARIABLES_TABLE[varindex].name);
	 }
	 
	 else if (struct_index == MISC )
	    sprintf(msgstr, "%s", TEMPLATE_VARIABLES_TABLE[varindex].name);
	 
	 else
	 {
	    if (idindex == MISSINGVAL)
	       sprintf(msgstr, "undefined location index for %s", 
		       TEMPLATE_VARIABLES_TABLE[varindex].name);
	    else
	       sprintf(msgstr, "%s %s", fp[idindex].id,
		       TEMPLATE_VARIABLES_TABLE[varindex].name);
	 }
	 
	 
	 /* now log the message */
	 
	 if (TEMPLATE_VARIABLES_TABLE[varindex].type == VAR_INT)
	    log_msg(MISSING_INTVAL, msgstr);
	 else if (TEMPLATE_VARIABLES_TABLE[varindex].type == VAR_FLT)
	    log_msg(MISSING_FLTVAL, msgstr);
	 else if (TEMPLATE_VARIABLES_TABLE[varindex].type == VAR_TIM)
	    log_msg(MISSING_TIMVAL, msgstr);
      }
   }
   
   
   /* do the unit conversion for the variable if required */
   
   if (rawvalue->f != MISSINGVAL && varinfo->metric_flag == TRUE)
   { 
       if (TEMPLATE_VARIABLES_TABLE[varindex].flow_factor != 1.0 &&
           fp[idindex].pe[0] == 'Q')
	   rawvalue->f = TEMPLATE_VARIABLES_TABLE[varindex].flow_factor *
	                 rawvalue->f;
       else			 
           rawvalue->f = TEMPLATE_VARIABLES_TABLE[varindex].regular_factor * 
                         rawvalue->f;
   }
      
   return;
}


/*********************************************************************
   load_fp_variable_value()
   
   PURPOSE
   Loads the value of a single forecast point data.
      
   NOTES
   
   *******************************************************************/
void load_fp_variable_value(int	      	      varindex,
			    const int	      fpindex,
			    char	      locid[],
			    fp_struct	      *fp,
			    vtecinfo_struct   *vtecinfo,
                            pcc_struct        *pcc,
			    misc_struct       *misc,				  
			    values 	      *rawvalue,
			    char	      longstring[])
{  
   int catval; 
   
   if (fpindex != MISSINGVAL)
   {
      /* get the minor category stage */
      
      if (vmatch(varindex, "<MinCatVal>") == 0)
	 rawvalue->f = fp[fpindex].cat[MINOR];
      
      
      /* get the moderate category stage */
      
      else if (vmatch(varindex, "<ModCatVal>") == 0)
	 rawvalue->f = fp[fpindex].cat[MODERATE];
      
      
      /* get the major category stage */
      
      else if (vmatch(varindex, "<MajCatVal>") == 0)
	 rawvalue->f = fp[fpindex].cat[MAJOR];
      
      
      /* get the record category stage */
      
      else if (vmatch(varindex, "<RecCatVal>") == 0)
	 rawvalue->f = fp[fpindex].cat[RECORD];   
	 
	 
      /* get the previously issued event's river category value */
   
      else if (vmatch(varindex, "<PrevCat>") == 0)
      {           	 	 	 	         
         if (fp[fpindex].prev_omf_cat != NULLCAT)
            rawvalue->i = fp[fpindex].prev_omf_cat;
         else
            rawvalue->i = MISSINGVAL;     	 	         
      }
   
   
      /* get the previously issued event's river category name */
   
      else if (vmatch(varindex, "<PrevCatName>") == 0)
      {            	
	 if (fp[fpindex].prev_omf_cat != NULLCAT)
         {
	   strcpy(rawvalue->s, convert_catindex_to_name(fp[fpindex].prev_omf_cat));
	   convert_str_to_lowcase(rawvalue->s);
	   rawvalue->s[0] = toupper(rawvalue->s[0]);
         }
         else
	   strcpy(rawvalue->s, misc->miss_cat);
	 	     
      }
      
      /* get the previously issued event's observed river category value */
      
      else if (vmatch(varindex, "<PrevObsCat>") == 0)
      {	
	 if (fp[fpindex].prev_curobs_val != MISSINGVAL)
	 {	    
	    catval = compute_stage_cat(fp[fpindex].cat,
				       fp[fpindex].prev_curobs_val);
	    rawvalue->i = catval;
	 }

	 else
	    rawvalue->i = MISSINGVAL;
      }
 
   
      /* get the previous observed river category name */

      else if (vmatch(varindex, "<PrevObsCatName>") == 0)
      {	 
	 if (fp[fpindex].prev_curobs_val != MISSINGVAL)
	 {	   
	    catval = compute_stage_cat(fp[fpindex].cat,
				       fp[fpindex].prev_curobs_val);
	    strcpy(rawvalue->s, convert_catindex_to_name(catval));
	    convert_str_to_lowcase(rawvalue->s);
	    rawvalue->s[0] = toupper(rawvalue->s[0]);
	 }
	 else
	    strcpy(rawvalue->s, misc->miss_cat);
      }
      
      /*get the previously issued event's max river forecast category value */
      
      else if (vmatch(varindex, "<PrevMaxFcstCat>") == 0)
      {	
	 if (fp[fpindex].prev_maxfcst_val != MISSINGVAL)
	 {	    
	    catval = compute_stage_cat(fp[fpindex].cat,
				       fp[fpindex].prev_maxfcst_val);
	    rawvalue->i = catval;
	 }

	 else
	    rawvalue->i = MISSINGVAL;
      }
 
   
      /* get the previous max forecast river category name */

      else if (vmatch(varindex, "<PrevMaxFcstCatName>") == 0)
      {	 
	 if (fp[fpindex].prev_maxfcst_val != MISSINGVAL)
	 {	   
	    catval = compute_stage_cat(fp[fpindex].cat,
				       fp[fpindex].prev_maxfcst_val);
	    strcpy(rawvalue->s, convert_catindex_to_name(catval));
	    convert_str_to_lowcase(rawvalue->s);
	    rawvalue->s[0] = toupper(rawvalue->s[0]);
	 }
	 else
	    strcpy(rawvalue->s, misc->miss_cat);
      }
          
      
      /* get the units for stage (ft) or flow(cfs) based on rec_type set */
      
      else if (vmatch(varindex, "<ImpCompUnits>") == 0)
      {
   	strcpy(rawvalue->s, load_impcomp_units(fp,fpindex));
	convert_str_to_lowcase(rawvalue->s); 
      }	 
   }
         
   
   return;   
}


/*********************************************************************
   load_locinfo_variable_value()
   
   PURPOSE
   Loads the value of a single location data.
      
   NOTES
   
   *******************************************************************/
void load_locinfo_variable_value(int         	varindex,
				 char	   	locid[],
				 values 	*rawvalue,
				 char	   	longstring[])				 
{
   /* get the raw forecast point id from the database */
   
   if (vmatch(varindex, "<Id>") == 0)
      strcpy(rawvalue->s, load_location_str(locid, "Id"));
   
   
   /* get the forecast point name from the database */
   
   else if (vmatch(varindex, "<IdName>") == 0)
      strcpy(rawvalue->s, load_location_str(locid, "IdName"));

   
   /* get the forecast point county from the database */
   
   else if (vmatch(varindex, "<County>") == 0)
      strcpy(rawvalue->s, load_location_str(locid, "County"));
   
   
   /* get the forecast point state id from the database */
   
   else if (vmatch(varindex, "<StateId>") == 0)
      strcpy(rawvalue->s, load_location_str(locid, "StateId"));
   
   /* get the forecast point state name from the database*/
   
   else if (vmatch(varindex, "<StateName>") == 0)
      strcpy(rawvalue->s, load_location_str(locid, "StateName"));
   
   
   /* get the river/stream name from the database */
   
   else if (vmatch(varindex, "<River>") == 0)
      strcpy(rawvalue->s, load_riverstat_str(locid, "River"));
   
   
   /* get the bankfull stage from the database */
   
   else if (vmatch(varindex, "<BankStg>") == 0)
      rawvalue->f = load_riverstat_float(locid, "BankStg");
   
   
   /* get the warning stage from the database */
   
   else if (vmatch(varindex, "<WStg>") == 0)
      rawvalue->f = load_riverstat_float(locid, "WStg");
   
   
   /* get the z datum elevation from the database */
   
   else if (vmatch(varindex, "<ZDatum>") == 0)
      rawvalue->f = load_riverstat_float(locid, "ZDatum");
   
   
   /* get the flood stage from the database */
   
   else if (vmatch(varindex, "<FldStg>") == 0)
      rawvalue->f = load_riverstat_float(locid, "FldStg");
  
   
   /* get the flood flow from the database */
   
   else if (vmatch(varindex, "<FldFlow>") == 0)
      rawvalue->f = load_riverstat_float(locid, "FldFlow");
   
   
   /* get the reach description from the database;
      note that this value is written to longstring!!! */
   
   else if (vmatch(varindex, "<Reach>") == 0)
      strcpy(longstring, load_descrip_str(locid, "Reach"));
   
   
   /* get the proximity */
   
   else if (vmatch(varindex, "<Proximity>") == 0)
      strcpy(rawvalue->s, load_descrip_str(locid, "Proximity"));
   
   
   /* get area description from LocArea table */
   
   else if (vmatch(varindex, "<LocGeoArea>") == 0)
      	strcpy(rawvalue->s, load_locarea_str(locid, "Area"));
	
	
   /* get the county's name list for the location.
      the value is assigned to longstring. */
   
   else if (vmatch(varindex, "<LocCntyList>") == 0)
      build_loccntylist(locid, longstring);
            
      
   /* get the string "flow" or "stage" depending on the primary pe value from
      RiverStat table. */
   
   else if (vmatch(varindex, "<StgFlowName>") == 0)
   {
   	strcpy(rawvalue->s, load_stgflow_name(locid));
	convert_str_to_lowcase(rawvalue->s); 
   }
   	
   
   /* get the units string for "flow" or "stage" depending on the primary pe
      value from the RiverStat table. */
   
   else if (vmatch(varindex, "<StgFlowUnits>") == 0)
   {
   	strcpy(rawvalue->s, load_stgflow_units(locid));
	convert_str_to_lowcase(rawvalue->s); 
   }	
   
   /* get the latitude for the location from RiverStat table */
   
   else if (vmatch(varindex, "<LocLat>") == 0)
   {
        rawvalue->f = load_loclat(locid);	   
   }
   
   /* get the longitude for the location from RiverStat table */
   
   else if (vmatch(varindex, "<LocLon>") == 0)
   {
        rawvalue->f = load_loclon(locid);          
   }
  
  
   return;
}


/*********************************************************************
   load_grp_variable_value()
   
   PURPOSE
   Loads the value of a single variable from the structure.
   
   NOTES
   
   *******************************************************************/
void load_grp_variable_value(int	     varindex,
			     const int	     grpindex,
			     grp_struct	    *grp,
			     values 	    *rawvalue,
			     pcc_struct     *pcc,
			     int             numcnty,
			     county_struct  *cnty)
{   
   char   idstr[80];
   char   where[300];
   State  *stateHead;
   char   statename[STATE_NAME_LEN + 1] = "";
   char   countyname[COUNTY_LEN + 1] = "";
      
   
   /* get the group id or the county, state id*/
   
   if (vmatch(varindex, "<GrpId>") == 0)
   {
      if (pcc->product.segment_mode != SEGMENT_MODE_COUNTY)
	 strcpy(rawvalue->s, grp[grpindex].id);
      
      else if (pcc->product.segment_mode == SEGMENT_MODE_COUNTY) 
      {
	 /* trim the trailing blanks off the county name. */
	 
	 strcpy(countyname,  trim_countyname(cnty[grpindex].county));
	 
	 strcpy(idstr, countyname);
	 strcat(idstr, ", ");
	 strcat(idstr, cnty[grpindex].state);
	 strcpy(rawvalue->s, idstr);
      }
   }
   
   
   /* get the group id name or county/state name */
   
   if (vmatch(varindex, "<GrpIdName>") == 0)
   {
      if (pcc->product.segment_mode != SEGMENT_MODE_COUNTY) 	  
	 strcpy(rawvalue->s, grp[grpindex].name);
      
      else if (pcc->product.segment_mode == SEGMENT_MODE_COUNTY) 
      {
	 /* load state whole name for the forecast point */
	 
	 sprintf(where, " WHERE state='%s'", cnty[grpindex].state);
	 
	 stateHead = GetState(where);
	 if (stateHead != NULL)
	    strcpy(statename, stateHead->name);
	 
	 /* trim the trailing blanks off the county name. */
	 
	 strcpy(countyname,  trim_countyname(cnty[grpindex].county));
	 
	 
	 /* concatenate the county name and state name */
	 
	 strcpy(idstr, countyname);
	 strcat(idstr, " county, ");
	 strcat(idstr, statename);
	 strcpy(rawvalue->s, idstr);
      }
   }
   
   
   return;   
}


/*********************************************************************
   load_pcc_variable_value()
   
   PURPOSE
   Loads the value of a single variable from the structure.
      
   NOTES
   
   *******************************************************************/
void load_pcc_variable_value(int	   varindex,
			     const int	   fpindex,
			     pcc_struct    *pcc,
			     values 	   *rawvalue)
{   
   /* get the historical comparison crest date and stage*/
   
   if (vmatch(varindex, "<HistCrestDate>") == 0)
      strcpy(rawvalue->d, pcc->comparison.compare_date[fpindex]);

   else if (vmatch(varindex, "<HistCrestStg>") == 0)
      rawvalue->f = pcc->comparison.compare_stage[fpindex];

   
   return;
}


/*********************************************************************
   load_stagegrp_variable_value()
   
   PURPOSE
   Loads the value of a single variable from the structure.
      
   
   *******************************************************************/
void load_stagegrp_variable_value(int			varindex,
	 		          const int		grpindex,
			          grp_struct		*grp,
				  misc_struct		*misc,
			   	  values 		*rawvalue,
				  county_struct 	*cnty,
				  pcc_struct            *pcc)
{
   int catval = NULLCAT;

   /*  get the max current observed category for the group and county */
   
   if (vmatch(varindex, "<GrpMaxCurCat>") == 0)
   {    
     if (pcc->product.segment_mode == SEGMENT_MODE_COUNTY)
        rawvalue->i = cnty[grpindex].max_curobs_cat;
     else
        rawvalue->i = grp[grpindex].max_curobs_cat; 	
   }
   
   
   /* get the max current observed category name for the group and county.
      set to lower case; if using upper case, the string will
      be uppercased just before write. */
   
   else if (vmatch(varindex, "<GrpMaxCurCatName>") == 0)
   {
     if (pcc->product.segment_mode == SEGMENT_MODE_COUNTY)
         catval = cnty[grpindex].max_curobs_cat;
      else
         catval = grp[grpindex].max_curobs_cat; 	 
	 	 
      if (catval != NULLCAT)
      {
	 strcpy(rawvalue->s, convert_catindex_to_name(catval));
	 convert_str_to_lowcase(rawvalue->s);
	 rawvalue->s[0] = toupper(rawvalue->s[0]);
      }
      
      else
	 strcpy(rawvalue->s, misc->miss_cat);
   }
   
   
   /*  get the max forecast category for the group and county*/
   
   else if (vmatch(varindex, "<GrpMaxFcstCat>") == 0)
   {
         
      if (pcc->product.segment_mode == SEGMENT_MODE_COUNTY)
         rawvalue->i = cnty[grpindex].max_maxfcst_cat;
      else
         rawvalue->i = grp[grpindex].max_maxfcst_cat;	 
   }
   
   
   /* get the max forecast category name for the group and county */
   
   else if (vmatch(varindex, "<GrpMaxFcstCatName>") == 0)
   {         
      if (pcc->product.segment_mode == SEGMENT_MODE_COUNTY)
         catval = cnty[grpindex].max_maxfcst_cat;
      else
         catval = grp[grpindex].max_maxfcst_cat;	 
	 	 
      if (catval != NULLCAT)
      {
	 strcpy(rawvalue->s, convert_catindex_to_name(catval));
	 convert_str_to_lowcase(rawvalue->s);
	 rawvalue->s[0] = toupper(rawvalue->s[0]);
      }
      else
	 strcpy(rawvalue->s, misc->miss_cat);
   }
   
   
   /* get the observed and max forecast category for the group and county*/
   
   else if (vmatch(varindex, "<GrpOMFCat>") == 0)
   {   
      if (pcc->product.segment_mode == SEGMENT_MODE_COUNTY)
         rawvalue->i = cnty[grpindex].max_omf_cat;
      else
         rawvalue->i = grp[grpindex].max_omf_cat; 	 
   }
   
   
   /* get the observed and max forecast category name for the group */
   
   else if (vmatch(varindex, "<GrpOMFCatName>") == 0)
   {        
       if (pcc->product.segment_mode == SEGMENT_MODE_COUNTY)
         catval = cnty[grpindex].max_omf_cat;
       else
         catval = grp[grpindex].max_omf_cat;	 
	 	 
      if (catval != NULLCAT)
      {
	 strcpy(rawvalue->s, convert_catindex_to_name(catval));
	 convert_str_to_lowcase(rawvalue->s);
	 rawvalue->s[0] = toupper(rawvalue->s[0]);
      }
      else
	 strcpy(rawvalue->s, misc->miss_cat);
   }
      
   
   /* get the flag that defines whether any observed data available
      for this forecast group; use other variables to help indicate this */
   
   else if (vmatch(varindex, "<GrpObsFound>") == 0)
   {   
       if (pcc->product.segment_mode == SEGMENT_MODE_COUNTY)
         catval = cnty[grpindex].max_curobs_cat;
       else
         catval = grp[grpindex].max_curobs_cat;	 
	 
      if (catval !=NULLCAT)
	 rawvalue->i = TRUE;
      else
	 rawvalue->i = FALSE;
   }
   
   
   /* get the flag that defines whether any forecast data available
      for this forecast group */
   
   else if (vmatch(varindex, "<GrpFcstFound>") == 0)
   {
        
      if (pcc->product.segment_mode == SEGMENT_MODE_COUNTY)
         catval = cnty[grpindex].max_maxfcst_cat;
      else
         catval = grp[grpindex].max_maxfcst_cat;	 
	 	 
      if (catval != MISSINGVAL)
	 rawvalue->i = TRUE;
      else
	 rawvalue->i = FALSE;
   }
      
      
   return;   
}


/*********************************************************************
   load_stage_ofp_variable_value()
   
   PURPOSE
   Loads the value of a single variable from the structure
   for observed stage data for a forecast point.
         
   *******************************************************************/
void load_stage_ofp_variable_value(int		 	varindex,
	 		           const int		fpindex,
				   fp_struct		*fp,
				   misc_struct		*misc,
			   	   values 		*rawvalue,
			           char		        dqcode[])
{
   int stgindex;
   int catval;
   float stageval;
   time_t timeval;
      
   
   /* get the observed stage value */
   
   if (vmatch(varindex, "<ObsStg>") == 0)
   {
      stgindex = fp[fpindex].obs_cur_index;
      if (stgindex != MISSINGVAL)
      {
	 rawvalue->f = fp[fpindex].obsH[stgindex].value;
	 strcpy(dqcode, fp[fpindex].obsH[stgindex].shef_qual_code);
      }
      else
	 rawvalue->f = (float )MISSINGVAL;
   }
   
   
   /* get the observed category value */
   
   else if (vmatch(varindex, "<ObsCat>") == 0)
   {
      stgindex = fp[fpindex].obs_cur_index;
      if (stgindex != MISSINGVAL)
      {
	 stageval = (float )fp[fpindex].obsH[stgindex].value;
	 catval = compute_stage_cat(fp[fpindex].cat,
				    fp[fpindex].obsH[stgindex].value);
	 rawvalue->i = catval;
      }
      
      else
	 rawvalue->i = MISSINGVAL;
   }
 
   
   /* get the observed category name */
   
   else if (vmatch(varindex, "<ObsCatName>") == 0)
   {
      stgindex = fp[fpindex].obs_cur_index;
      if (stgindex != MISSINGVAL)
      {
	 stageval = (float )fp[fpindex].obsH[stgindex].value;
	 catval = compute_stage_cat(fp[fpindex].cat,
				    fp[fpindex].obsH[stgindex].value);
	 strcpy(rawvalue->s, convert_catindex_to_name(catval));
	 convert_str_to_lowcase(rawvalue->s);
	 rawvalue->s[0] = toupper(rawvalue->s[0]);
      }
      else
	 strcpy(rawvalue->s, misc->miss_cat);
   }

   
   /* get the observed category time */
   
   else if (vmatch(varindex, "<ObsTime>") == 0)
   {
      stgindex = fp[fpindex].obs_cur_index;
      if (stgindex != MISSINGVAL)
	 rawvalue->t = fp[fpindex].obsH[stgindex].validtime;
      else
	 rawvalue->t = (time_t )MISSINGVAL;
   }
   
   
   /* get the observed crest stage */
   
   else if (vmatch(varindex, "<ObsCrestStg>") == 0)
   {
      if (fp[fpindex].obs_crest_value != MISSINGVAL)
	 rawvalue->f = fp[fpindex].obs_crest_value;
      else
	 rawvalue->f = (float )MISSINGVAL;
	 
      strcpy(dqcode, "");
   }
   
   
   /* get the time of the observed crest */
   
   else if (vmatch(varindex, "<ObsCrestTime>") == 0)
   {
      if (fp[fpindex].obs_crest_time != MISSINGVAL)
	 rawvalue->t = fp[fpindex].obs_crest_time;
      else
	 rawvalue->t = (time_t )MISSINGVAL;
   }   
   
   
   /* get the max observed stage for the last 24 hours */
   
   else if (vmatch(varindex, "<MaxObsStg24>") == 0)
   {
      stgindex = fp[fpindex].obs_max24_index;
      if (stgindex != MISSINGVAL)
      {
	 rawvalue->f = fp[fpindex].obsH[stgindex].value;
	 strcpy(dqcode, fp[fpindex].obsH[stgindex].shef_qual_code);
      }
      else
	 rawvalue->f = (float )MISSINGVAL;
   }
   
   
   /* get the max observed stage for the last 06 hours */
   
   else if (vmatch(varindex, "<MaxObsStg06>") == 0)
   {
      stgindex = fp[fpindex].obs_max06_index;
      if (stgindex != MISSINGVAL)
      {
	 rawvalue->f = fp[fpindex].obsH[stgindex].value;
	 strcpy(dqcode, fp[fpindex].obsH[stgindex].shef_qual_code);
      }
      else
	 rawvalue->f = (float )MISSINGVAL;
   }
      
   
   /* get the observed rise above flood stage time */
   
   else if (vmatch(varindex, "<ObsRiseFSTime>") == 0)
   {
      timeval = fp[fpindex].obs_riseabove_time;
      if (timeval != MISSINGVAL)
	 rawvalue->t = timeval;
      else
	 rawvalue->t = (time_t )MISSINGVAL;
   }
   
   
   /* get the observed fall below flood stage time */
   
   else if (vmatch(varindex, "<ObsFallFSTime>") == 0)
   {
      timeval = fp[fpindex].obs_fallbelow_time;
      if (timeval != MISSINGVAL)
	 rawvalue->t = timeval;
      else
	 rawvalue->t = (time_t )MISSINGVAL;
   }
   
   
   /* get the departure from flood stage */
   
   else if (vmatch(varindex, "<ObsFSDeparture>") == 0)
      rawvalue->f = fp[fpindex].obs_FSdeparture;
   
   
   /* get the absolute value of the departure from flood stage */
   
   else if (vmatch(varindex, "<ObsFSDepartureA>") == 0)
   {
      rawvalue->f = fp[fpindex].obs_FSdeparture;
      if (rawvalue->f != (float )MISSINGVAL && rawvalue->f < 0)
	 rawvalue->f = rawvalue->f * (-1);
   }
   
   
   /* get the number of observed stage values */
   
   else if (vmatch(varindex, "<NumObsStg>") == 0)
      rawvalue->i = fp[fpindex].numobsH;


   return;
}
      
      
/*********************************************************************
   load_stage_ffp_variable_value()
   
   PURPOSE
   Loads the value of a single variable from the structure
   for forecast stage data for a forecast point.
      
   NOTES
   
   *******************************************************************/
void load_stage_ffp_variable_value(int			varindex,
	 		           const int		fpindex,
				   fp_struct		*fp,
				   misc_struct   	*misc,
			   	   values 		*rawvalue,
			           char			dqcode[])
{
   int stgindex, stgindex1, stgindex2;
   int catval;
   time_t timeval;
   float stageval;
      
   
   /* get the max forecast stage value */
   
   if (vmatch(varindex, "<MaxFcstStg>") == 0)
   {
      stgindex = fp[fpindex].fcst_max_index;
      if (stgindex != MISSINGVAL)
      {
	 rawvalue->f = fp[fpindex].fcstH[stgindex].value;
	 strcpy(dqcode, fp[fpindex].fcstH[stgindex].shef_qual_code); 
      }
      else
	 rawvalue->f = (float )MISSINGVAL;
   }
   
   
   /* get the max forecast category */
   
   else if (vmatch(varindex, "<MaxFcstCat>") == 0)
   {
      stgindex = fp[fpindex].fcst_max_index;
      if (stgindex != MISSINGVAL)
      {
	 stageval = (float )fp[fpindex].fcstH[stgindex].value;
	 catval = compute_stage_cat(fp[fpindex].cat, stageval);
	 rawvalue->i = catval;
      }
      else
	 rawvalue->i = MISSINGVAL;
   }
   
   
   /* get the max forecast category name*/
   
   else if (vmatch(varindex, "<MaxFcstCatName>") == 0)
   {
      stgindex = fp[fpindex].fcst_max_index;
      if (stgindex != MISSINGVAL)
      {
	 stageval = (float )fp[fpindex].fcstH[stgindex].value;
	 catval = compute_stage_cat(fp[fpindex].cat, stageval);
	 strcpy(rawvalue->s, convert_catindex_to_name(catval));
 	 convert_str_to_lowcase(rawvalue->s);
	 rawvalue->s[0] = toupper(rawvalue->s[0]);
     }
      else
	 strcpy(rawvalue->s, misc->miss_cat);
   }
   
   
   /* get the max forecast time */
   
   else if (vmatch(varindex, "<MaxFcstTime>") == 0)
   {
      stgindex = fp[fpindex].fcst_max_index;
      if (stgindex != MISSINGVAL)
	 rawvalue->t = fp[fpindex].fcstH[stgindex].validtime;
      else
	 rawvalue->t = (time_t )MISSINGVAL;
   }
   
   
   /* get the forecast crest stage */
   
   else if (vmatch(varindex, "<FcstCrestStg>") == 0)
   {
      if (fp[fpindex].fcst_crest_value != MISSINGVAL)
	 rawvalue->f = fp[fpindex].fcst_crest_value;
      else
	 rawvalue->f = (float )MISSINGVAL;
	 
      strcpy(dqcode, ""); 
   }
   
   
   /* get the time of the forecast crest */
   
   else if (vmatch(varindex, "<FcstCrestTime>") == 0)
   {
      if (fp[fpindex].fcst_crest_time != MISSINGVAL)
	 rawvalue->t = fp[fpindex].fcst_crest_time;
      else
	 rawvalue->t = (time_t )MISSINGVAL;
   }   


   
   /* get the forecast rise above flood stage time */
   
   else if (vmatch(varindex, "<FcstRiseFSTime>") == 0)
   {
      timeval = fp[fpindex].fcst_riseabove_time;
      if (timeval != MISSINGVAL)
	 rawvalue->t = timeval;
      else
	 rawvalue->t = (time_t )MISSINGVAL;
   }
   
   
   /* get the forecast fall below flood stage time */
   
   else if (vmatch(varindex, "<FcstFallFSTime>") == 0)
   {
      timeval = fp[fpindex].fcst_fallbelow_time;
      if (timeval != MISSINGVAL)
	 rawvalue->t = timeval;
      else
	 rawvalue->t = (time_t )MISSINGVAL;
   }

   
   /* get the departure from flood stage */
   
   else if (vmatch(varindex, "<FcstFSDeparture>") == 0)
      rawvalue->f = fp[fpindex].fcst_FSdeparture;
   
   
   /* get the absolute value of the departure from flood stage */
   
   else if (vmatch(varindex, "<FcstFSDepartureA>") == 0)
   {
      rawvalue->f = fp[fpindex].fcst_FSdeparture;
      if (rawvalue->f != (float )MISSINGVAL && rawvalue->f < 0)
	 rawvalue->f = rawvalue->f * (-1);
   }
   
   
   /* get the number of forecast stage values */
   
   else if (vmatch(varindex, "<NumFcstStg>") == 0)
      rawvalue->i = fp[fpindex].numfcstH;
      
   /* use the forecast FFX crest stage if exist */
   
   else if (vmatch(varindex, "<XCrestStg>") == 0)
   {
      stgindex = fp[fpindex].fcst_xfcrest_index;      
      if (stgindex != (int) MISSINGVAL)
      {
          if (fp[fpindex].fcstH[stgindex].value != MISSINGVAL)
      	     rawvalue->f = fp[fpindex].fcstH[stgindex].value;
	  else
	     rawvalue->f = (float )MISSINGVAL;   
      }	     
      else
	 rawvalue->f = (float )MISSINGVAL;
	 
      strcpy(dqcode, ""); 
   }    
   
   /* use the forecast FFX crest stage if exist */
   
   else if (vmatch(varindex, "<XCrestTime>") == 0)
   {
      stgindex = fp[fpindex].fcst_xfcrest_index;
      if (stgindex != (int) MISSINGVAL)
      {
         if (fp[fpindex].fcstH[stgindex].validtime != MISSINGVAL)
	    rawvalue->t = fp[fpindex].fcstH[stgindex].validtime;
         else
	    rawvalue->t = (time_t)MISSINGVAL;	       
      }
      else
         rawvalue->t = (time_t)MISSINGVAL;		 
   }
       
   /* use the forecast FFX crest stage if exist, use the regular
     crest value if FFX does not exist */
   
   else if (vmatch(varindex, "<XRCrestStg>") == 0)
   {
      stgindex = fp[fpindex].fcst_xfcrest_index;
      
      if (stgindex != (int) MISSINGVAL)
      {
         if (fp[fpindex].fcstH[stgindex].value != MISSINGVAL)
	    rawvalue->f = fp[fpindex].fcstH[stgindex].value;
      }   	 
      else if (fp[fpindex].fcst_crest_value != MISSINGVAL)
	 rawvalue->f = fp[fpindex].fcst_crest_value;
	 
      else
	 rawvalue->f = (float )MISSINGVAL;	 
	 
      strcpy(dqcode, ""); 
   }   
   
   /* use the forecast FFX crest time if exist, use the regular
     crest time if FFX does not exist */
   
   else if (vmatch(varindex, "<XRCrestTime>") == 0)
   {
      stgindex = fp[fpindex].fcst_xfcrest_index;
      
      if (stgindex != (int) MISSINGVAL)
      {
         if (fp[fpindex].fcstH[stgindex].validtime != MISSINGVAL)
	    rawvalue->t = fp[fpindex].fcstH[stgindex].validtime;
      }	 
      else if (fp[fpindex].fcst_crest_time != MISSINGVAL)
	 rawvalue->t = fp[fpindex].fcst_crest_time;
	 
      else
	 rawvalue->t = (time_t)MISSINGVAL;	 	      
   }   
   
   /* use the forecast FFX crest stage if exist, use the regular
     maximum forecast value if FFX does not exist */
   
   else if (vmatch(varindex, "<XRMaxStg>") == 0)
   {
      stgindex1 = fp[fpindex].fcst_xfcrest_index;
      stgindex2 = fp[fpindex].fcst_max_index;
      
      if (stgindex1 != (int) MISSINGVAL)
      {
         if (fp[fpindex].fcstH[stgindex1].value != MISSINGVAL)
         {	  
	    rawvalue->f = fp[fpindex].fcstH[stgindex1].value;
	    strcpy(dqcode, fp[fpindex].fcstH[stgindex1].shef_qual_code);
         } 	 
      }	 
      else if (stgindex2 != (int) MISSINGVAL)
      {
         if(fp[fpindex].fcstH[stgindex2].value != MISSINGVAL)
         { 	       
	    rawvalue->f = fp[fpindex].fcstH[stgindex2].value;
	    strcpy(dqcode, fp[fpindex].fcstH[stgindex2].shef_qual_code);
         }	 
      } 	 
      else
	 rawvalue->f = (float )MISSINGVAL;	 	 
   }   
   
   /* use the forecast FFX crest time if exist, use the regular
     maximum fcst time if FFX does not exist */
   
   else if (vmatch(varindex, "<XRMaxTime>") == 0)
   {
      stgindex1 = fp[fpindex].fcst_xfcrest_index;
      stgindex2 = fp[fpindex].fcst_max_index;
      
      if (stgindex1 != (int) MISSINGVAL)
      {
         if (fp[fpindex].fcstH[stgindex1].validtime != MISSINGVAL)
	    rawvalue->t = fp[fpindex].fcstH[stgindex1].validtime;
      }      	 
      else if (stgindex2 != (int) MISSINGVAL)
      {
         if(fp[fpindex].fcstH[stgindex2].validtime != MISSINGVAL)
	    rawvalue->t = fp[fpindex].fcstH[stgindex2].validtime;
      }	 
      else
	 rawvalue->t = (time_t)MISSINGVAL;	 	      
   } 
   
   /* use the forecast maximum crest stage */
   
   else if (vmatch(varindex, "<MaxCrestStg>") == 0)
   {
      compute_detailed_fcst_info(fpindex, fp); 
   
      if (fp[fpindex].fcst_max_value != MISSINGVAL)
         rawvalue->f = fp[fpindex].fcst_max_value;     
      else
         rawvalue->f = (float) MISSINGVAL;
	 
      strcpy(dqcode, ""); 
   }   
   
   /* use the forecast maximum crest time */
   
   else if (vmatch(varindex, "<MaxCrestTime>") == 0)
   {
      compute_detailed_fcst_info(fpindex, fp);
      
      if (fp[fpindex].fcst_maxvalue_time != MISSINGVAL)
         rawvalue->t = fp[fpindex].fcst_maxvalue_time;
      else
         rawvalue->t = (time_t )MISSINGVAL;	        	      
   } 
   
   /* use the first X data if exist, otherwise, ue forecast maximum 
      crest stage */
   
   else if (vmatch(varindex, "<XMaxCrestStg>") == 0)
   {
      stgindex = fp[fpindex].fcst_xfcrest_index;
      compute_detailed_fcst_info(fpindex, fp);
      
      if (stgindex != (int) MISSINGVAL)
      {
         if (fp[fpindex].fcstH[stgindex].value != MISSINGVAL)
	    rawvalue->f = fp[fpindex].fcstH[stgindex].value;	 
      }
      else if (fp[fpindex].fcst_max_value != MISSINGVAL)
         rawvalue->f = fp[fpindex].fcst_max_value;
	 
      else
         rawvalue->f = (float )MISSINGVAL;
      
      strcpy(dqcode, ""); 
   }   
   
   /* use the first X data if exist, otherwise, ue forecast maximum 
      crest time */
   
   else if (vmatch(varindex, "<XMaxCrestTime>") == 0)
   {
      stgindex = fp[fpindex].fcst_xfcrest_index;
      compute_detailed_fcst_info(fpindex, fp);
      
      if (stgindex != (int) MISSINGVAL)
      {
         if (fp[fpindex].fcstH[stgindex].validtime != MISSINGVAL)
	    rawvalue->t = fp[fpindex].fcstH[stgindex].validtime;	 
      }
      else if (fp[fpindex].fcst_maxvalue_time != MISSINGVAL)
         rawvalue->t = fp[fpindex].fcst_maxvalue_time;
	 
      else
         rawvalue->t = (time_t )MISSINGVAL;
           
   }            
   
   /* get the forecast look forward hours */
   
   else if (vmatch(varindex, "<FcstHrs>") == 0)
   {
       rawvalue->i = fp[fpindex].forwardhrs;
   
   }
   
   /* get the forecast look forward time */
   
   else if (vmatch(varindex, "<FcstHrsTime>") == 0)
   {
       
       rawvalue->t = (time_t) (misc->system_time + 3600 * fp[fpindex].forwardhrs);
   }    
   
   return;
}


/*********************************************************************
   load_stage_xfp_variable_value()
   
   PURPOSE
   Loads the value of a single variable from the structure for 
   stage data for a forecast point that is not solely observed
   or forecast.
      
   NOTES
   
   *******************************************************************/
void load_stage_xfp_variable_value(int		 	varindex,
	 		           const int		fpindex,
				   fp_struct	  	*fp,
				   misc_struct		*misc,
				   values 		*rawvalue)
{
   int	catindex;
   int	trendval;

   
   /* get the observed and max forecast value (stage or discharge) */
   
   if (vmatch(varindex, "<OMFVal>") == 0)
      rawvalue->f = fp[fpindex].omf;
   
   
   /* get the observed and max forecast category value */
   
   else if (vmatch(varindex, "<OMFCat>") == 0)
   {
      catindex = fp[fpindex].omf_cat;
      if (catindex != NULLCAT)
	 rawvalue->i = fp[fpindex].omf_cat;
      else
	 rawvalue->i = MISSINGVAL;
   }
   
   
   /* get the observed and max forecast category name */
   
   else if (vmatch(varindex, "<OMFCatName>") == 0)
   {
      catindex = fp[fpindex].omf_cat;
      if (catindex != NULLCAT)
      {
	 strcpy(rawvalue->s, convert_catindex_to_name(catindex));
	 convert_str_to_lowcase(rawvalue->s);
	 rawvalue->s[0] = toupper(rawvalue->s[0]);
      }
      else
	 strcpy(rawvalue->s, misc->miss_cat);
   }
     
   
   /* get the observed trend description. convert to lower case;
      if using upper case, will be converted before writing out. */
   
   else if (vmatch(varindex, "<ObsStgTrend>") == 0)
   {
      trendval = fp[fpindex].obs_trend;
      strcpy(rawvalue->s, convert_trendval_to_descr(trendval));
      convert_str_to_lowcase(rawvalue->s);
   }
   
   
   /* get the overall trend description. convert to lower case;
      if using upper case, will be converted before writing out. */
   
   else if (vmatch(varindex, "<StgTrend>") == 0)
   {
      trendval = fp[fpindex].trend;
      strcpy(rawvalue->s, convert_trendval_to_descr(trendval));
      convert_str_to_lowcase(rawvalue->s);
   }
   
	    
   return;   
}


/*********************************************************************
   load_stagespec_variable_value()
   
   PURPOSE
   Loads the value of a specific observed or forecast stage 
   value or time for the time dictated by the stage time information.
      
   NOTES
   
   *******************************************************************/
void load_stagespec_variable_value(int		   varindex,
				   const int		   fpindex,
				   fp_struct		  *fp,
				   const spectime_struct   *spectime,
				   const int		   spectime_index,
	                           values 		  *rawvalue,
				   char			   dqcode[])
{
   int 		i, numsecs;
   time_t 	curtime;
   float 	curval;
   int		val_start, val_end;
   time_t 	exact_time, begin_window, end_window;
   time_t	savetime = 0;
   time_t 	savediff, curdiff;
   int 		obs_or_fcst, stg_or_time;
   int		valset;
   char		cur_dqcode[SHEF_QC_LEN + 1];
      
   
   /* check that the index is valid */
   
   if (spectime_index < 0 || spectime_index >= spectime->num_of_spectimes)
   {
      log_msg(MISSING_SPECTIME, "");
      return;
   }
   
   
   /* define the stage time specified by offsetting the basetime
      by the number of days and hours specified */
   
   numsecs = spectime->relhour[spectime_index]*3600 +
      spectime->relday[spectime_index]*3600*24;
   exact_time = spectime->basetime + (time_t)numsecs; 
   
   
   /* define the stage time window based on the stage time and 
      and the window size in hours; the abs function works on 
      for integer values */
   
   begin_window = exact_time - 
      (time_t )abs(spectime->window[spectime_index]*3600);
   end_window = exact_time + 
      (time_t )abs(spectime->window[spectime_index]*3600);
   
   
   /* determine some flags for convenience use later and initialize */  
   
   if (vmatch(varindex, "<SpecObsStg>") == 0)
   {
      obs_or_fcst = 0;
      stg_or_time = 0;
   }
   else if (vmatch(varindex, "<SpecObsStgTime>") == 0)
   {
      obs_or_fcst = 0;
      stg_or_time = 1;
   }   
   else if (vmatch(varindex, "<SpecFcstStg>") == 0)
   {
      obs_or_fcst = 1;
      stg_or_time = 0;
   }
   else if (vmatch(varindex, "<SpecFcstStgTime>") == 0)
   {
      obs_or_fcst = 1;
      stg_or_time = 1;
   }	    
   else
      return;
   
   
   /* process depending upon whether looking for an observed 
      stage of a forecast stage */
   
   valset = FALSE;
   
   if (obs_or_fcst == 0)
   {
      val_start = fp[fpindex].numobsH - fp[fpindex].use_obsH;
      val_end   = fp[fpindex].numobsH;
   }
   else
   {
      val_start = 0;
      val_end = fp[fpindex].numfcstH;
   }
   
   
   /* loop on the number of values */
   
   for (i = val_start; i < val_end; i++)
   {
      /* define the value and time to loop on */
      
      if (obs_or_fcst == 0)
      {
	 curtime = fp[fpindex].obsH[i].validtime;
	 curval  = fp[fpindex].obsH[i].value;
	 strcpy(cur_dqcode, fp[fpindex].obsH[i].shef_qual_code); 
      }
      else
      {
	 curtime = fp[fpindex].fcstH[i].validtime;
	 curval  = fp[fpindex].fcstH[i].value;
	 strcpy(cur_dqcode, fp[fpindex].fcstH[i].shef_qual_code); 
      }
      
      
      /* check if the stage time is equal to the specified 
	 stage time, if so, exit loop */
      
      if (curtime == exact_time)
      {
	 if (stg_or_time == 0)
	 {
	    rawvalue->f = curval;
	    strcpy(dqcode, cur_dqcode); 
	 }
	 else
	    rawvalue->t = curtime;
	 
	 break;
      }
      
      
      /* if stage time is within the time window, then save it
	 unless any previously found time is closer to the
	 exact time specified */
      
      if (begin_window <= curtime && curtime <= end_window)
      {
	 if (valset == FALSE)
	 {
	    if (stg_or_time == 0)
	    {
	       rawvalue->f = curval;
	       strcpy(dqcode, cur_dqcode); 
	    }
	    else
	       rawvalue->t = curtime;
	    
	    savetime = curtime;
	    valset = TRUE;
	 }
	 
	 else
	 {
	    absdiff(savetime, exact_time, savediff);
	    absdiff(curtime,  exact_time, curdiff);
	    
	    if (curdiff < savediff)
	    {
	       if (stg_or_time == 0)
	       {
		  rawvalue->f = curval;
		  strcpy(dqcode, cur_dqcode); 
	       }
	       else
		  rawvalue->t = curtime;
	       
	       savetime = curtime;
	    }
	 }
      }
   }  /* end of for loop on number of values */
   
   return;
}


/*********************************************************************
   load_misc_variable_value()
   
   PURPOSE
   Loads the value of a single variable from the structure.
      
  
   *******************************************************************/
void load_misc_variable_value(int	        varindex,
			      const int         grpindex,
			      grp_struct        *grp,
			      fp_struct	        *fp,
			      misc_struct	*misc,
			      pcc_struct	*pcc,
			      values 	        *rawvalue,
			      char	        longstring[],
		              const int         numcnty,
			      county_struct     *cnty,
			      vtecinfo_struct   *vtecinfo)
{         
   
   /* get the product category or id */
   
   if (vmatch(varindex, "<ProdCateg>") == 0)
      strcpy(rawvalue->s, pcc->product.prod_categ);
   
   else if (vmatch(varindex, "<ProdId>") == 0)
      strcpy(rawvalue->s, pcc->product.product_cnx);
     
   
   /* get the current date/time */
   
   else if (vmatch(varindex, "<CurDate>") == 0)
      rawvalue->t = misc->system_time;
   
   
   /* get the issuance number */
   
   else if (vmatch(varindex, "<IssuanceNumber>") == 0)
      rawvalue->i = misc->issnum;
   
   
   /* get the impact description which is stored in a special
      holding area in this structure, or get the impact stage */
   
   else if (vmatch(varindex, "<ImpactDescr>") == 0)
      strcpy(longstring, misc->longstring);
   
   else if (vmatch(varindex, "<ImpactStg>") == 0)
      rawvalue->f = misc->flt;
    
   
   /* load the various offsets times for future days */
   
   else if (vmatch(varindex, "<Day0>") == 0)
      rawvalue->t = misc->system_time + (time_t )3600*24*0;
   
   else if (vmatch(varindex, "<Day1>") == 0)
      rawvalue->t = misc->system_time + (time_t )3600*24*1;
    
   else if (vmatch(varindex, "<Day2>") == 0)
      rawvalue->t = misc->system_time + (time_t )3600*24*2;
   
   else if (vmatch(varindex, "<Day3>") == 0)
      rawvalue->t = misc->system_time + (time_t )3600*24*3;
   
   else if (vmatch(varindex, "<Day4>") == 0)
      rawvalue->t = misc->system_time + (time_t )3600*24*4;
   
   else if (vmatch(varindex, "<Day5>") == 0)
      rawvalue->t = misc->system_time + (time_t )3600*24*5;
      
   else if (vmatch(varindex, "<Day6>") == 0)
      rawvalue->t = misc->system_time + (time_t )3600*24*6;
      
   else if (vmatch(varindex, "<Day7>") == 0)
      rawvalue->t = misc->system_time + (time_t )3600*24*7;
   
   
   /* build the ugclist by county or zone and put it in
      the special longstring */
   
   else if (vmatch(varindex, "<UGCListZ>") == 0)
      build_ugc(grp, fp, pcc, misc, vtecinfo, 'Z', 
                longstring);   
      
   else if (vmatch(varindex, "<UGCListC>") == 0)
      build_ugc(grp, fp, pcc, misc, vtecinfo, 'C', 
                longstring);   
    
   
   /* load a string that lists the forecast point names in the
      group that are included in the product */
   
   else if (vmatch(varindex, "<GrpFPList>") == 0)
      build_grp_fplist(grpindex, grp, fp, misc, pcc, longstring, numcnty,
                       cnty);
   
   
   /* build the list of rivers and their forecast points, that
      are included in the product, and put it in the special longstring */
   
   else if (vmatch(varindex, "<GrpsFPList>") == 0)
      build_grps_fplist(grp, fp, misc, pcc, longstring, numcnty, cnty);
   
   
   /* load a string that lists the forecast group names
      that are included in the product */
   
   else if (vmatch(varindex, "<GrpList>") == 0)
      build_grplist(grp, fp, misc, pcc, longstring);      
   
   
   /* build the list of counties that are included in the product
      and put it in the special longstring */
   
   else if (vmatch(varindex, "<CountyList>") == 0)
      build_countylist(grp, fp, misc, pcc, longstring);
   
   
   /* build the list of rivers that are included in the product
      and put it in the special longstring */
   
   else if (vmatch(varindex, "<RiverList>") == 0)
      build_riverlist(grp, fp, misc, pcc, longstring);
           
   /* get the number of groups included in the product */
   
   else if ((pcc->product.segment_mode != SEGMENT_MODE_COUNTY) &&
           (vmatch(varindex, "<NumGrps>") == 0))  
        rawvalue->i = misc->numgrps_included;
	
   else if ((pcc->product.segment_mode == SEGMENT_MODE_COUNTY) &&
            (vmatch(varindex, "<NumGrps>") == 0))
	rawvalue->i = misc->numcnty_included; 
	
	   	
   /* get HSA from misc->selected_office, it can change when switching office */
    	 
   else if (vmatch(varindex, "<HSA>") == 0)
   {
      if (misc->selected_office != NULL)      
        strcpy(rawvalue->s, misc->selected_office);
      else
        strcpy(rawvalue->s, "MISSING");		
   } 
   
   	
   /* get office name from admin table */
   
   else if (vmatch(varindex, "<OfficeName>") == 0)
        strcpy(rawvalue->s, load_officename_str("OfficeName"));
	 
         
   return;   
}


/***************************************************************
  load_stage_trendfp_variable_value()
  
  PURPOSE
  Loads the detailed trend phrases from structure fcsttrend_info  
  for forecast data. 					
					
****************************************************************/
void load_stage_trendfp_variable_value(int          	varindex,
                                       const int        fpindex,
				       fp_struct        *fp,
				       char             longstring[],
				       misc_struct  	*misc,
				       pcc_struct       *pcc)
{
    extern fcsttrend_info_struct  *fcsttrend_info;
    extern int	 tidal_cnt;

    static char lastTZset[TZ_LEN +1] = "";
    static char setTZcmd[25] = " ";
    
    int 	k, action_index;
    char 	trendphrase_str[MAXLEN_LONGSTR], trendstage_str[MAXLEN_STRING];     
    char 	concat_all_str[MAXLEN_LONGSTR], trendtime_str[MAXLEN_STRING];
    char 	where[800];
    int   	putenv_rc;
    struct tm 	*tidal_maxmin_time;    
    char 	date_str[20], previous_date_str[20]="", time_str[15];
    char 	time_value_str[MAXLEN_LONGSTR], tidal_str[MAXLEN_LONGSTR];        
    int  	first_tidal_flag;
//    int         risetrend_flag;    
    char 	*strfound = NULL;
    Location 	*locPtr = NULL;
    FcstHeight  *fcstheightPtr = NULL;
    char        msgstr[100]=""; 
    int         not_only_rise = FALSE;
     
    /* initialize */
    
    memset(trendphrase_str, '\0', MAXLEN_LONGSTR);
    memset(trendstage_str, '\0', MAXLEN_STRING);
    memset(trendtime_str, '\0', MAXLEN_STRING);
    memset(concat_all_str, '\0', MAXLEN_LONGSTR);
    memset(date_str,'\0',20);
    memset(time_str,'\0', 15);
    memset(time_value_str,'\0', MAXLEN_LONGSTR);
    memset(tidal_str, '\0', MAXLEN_LONGSTR);
    
    first_tidal_flag = FALSE;
   // risetrend_flag = FALSE;
    
    
    
    if (vmatch(varindex, "<FcstTrend>") == 0)
    {
        sprintf(msgstr, "Processing variable <FcstTrend>...");   
        log_msg("", msgstr);
	
        /* get the detailed info on the for forecast points */
	
       compute_detailed_fcst_info(fpindex, fp); 
       load_detailed_trend_info(fpindex,fp);	                   
       
       
       /* determine the time zone used on the system for this forecast point   
          if the use individual location's time zone flag set */
   
       if (pcc->product.timeZoneFlag == 1)
       {
	   /* Retrieve the location record from the DBMS and use its
	      tzone field to set the TZ variable. */	

	   sprintf(where, " WHERE lid = '%s' ", fp[fpindex].id);
	   
	   if ( (locPtr = GetLocation(where)) != (Location *) NULL )
	   {		
		memset( setTZcmd, 0, 25);
		sprintf(setTZcmd, "TZ=%s", locPtr->tzone);
		putenv_rc = putenv(setTZcmd);
		
		memset( lastTZset, 0, TZ_LEN + 1);
	        strncpy(lastTZset, locPtr->tzone, TZ_LEN);		


                /* free the location table pointer if not null */
       
                FreeLocation(locPtr);
	        locPtr = NULL;
           }
       }  
       
       
       /* create trend phrase for tidal point*/
       
       if (fp[fpindex].fcstpoint_type == TIDAL)
       {
          for (k = 0; k <= tidal_cnt; k++)
	  {
             if (fp[fpindex].action_time[k] != MISSINGVAL &&
	         fp[fpindex].action_value[k] != MISSINGVAL)
	     {	
		tidal_maxmin_time = localtime(&fp[fpindex].action_time[k]);
		strftime(date_str, 20, "    %a     %m/%d\n", tidal_maxmin_time);
		strftime(time_str, 15, "%I %M %p   ", tidal_maxmin_time);
		sprintf(time_value_str, "%s    %6.1f\n", 
		                       time_str, fp[fpindex].action_value[k]);

                		   
        	if (strcmp(date_str, previous_date_str) == 0 || 
	            strcmp(previous_date_str, "") == 0)
		{		   		   	        
		   if (first_tidal_flag == FALSE)
		   {
		      strcpy(tidal_str, "\n");
		      strcat(tidal_str, date_str);		      
		      first_tidal_flag = TRUE;
		   }   		
		}
		else 
		{		   		           
		   strcat(tidal_str, date_str);
		  
		}	
                
		if (date_str != NULL)
		   strcpy(previous_date_str, date_str);
		   
		strcpy(date_str, "");
		strcat(tidal_str, time_value_str);
	      } 	
	  }	  
	  	  
	  strcpy(longstring, tidal_str);	  	  	  
       }
       
       
       /* create trend phrase for normal point, weir point and Fremont point */
       
       else 
       {  	   
	  /* load trend phrase, associated stage/flow and time */

	  for (k = 0; k < MAXNUM_OF_TRENDPHRASE; k++)
	  {
	    //risetrend_flag = 0;
	    
            if (fp[fpindex].action_index[k] == MISSINGVAL)
               break;
            else
            {
	       strcpy(trendphrase_str,load_trendfp_phrase(k,fpindex, fp, fcsttrend_info));
               strcpy(trendstage_str, load_trendfp_stage(k,fpindex, fp));
               strcpy(trendtime_str,load_trendfp_time(k,fpindex, fp, misc));
	       
	       
	       /* check if add "with continued rise expected" at the end of
	          "Forecast to rise to near xx ft " if the trendphrase_str 
		  includes "forecast to rise to near" and the pe in shef code
		  is HI, the type is F for forecast data and the value is 
		  1 or 5 which mean rising */
	       
	       /* check if has trend phrase other than "rise" */
	       
	       convert_str_to_lowcase(trendphrase_str);
	       if (((strfound = strstr(trendphrase_str, "rise above")) == NULL) &&
	            ((strfound = strstr(trendphrase_str, "rise to near")) == NULL))  
               { 
	          not_only_rise = TRUE;
	       }
	       else
	          not_only_rise = FALSE;
		      	  
	          /* retrieve pe and its value from fcstheight table for this point */
		    
		 /* sprintf(where, "WHERE lid='%s' AND pe='HI' AND probability < 0.0 "
		                 " AND value='1' AND validtime > CURRENT_TIMESTAMP "
				 " OR value='5' AND basistime=(SELECT MAX(basistime) "
			         " FROM fcstheight WHERE lid='%s' AND pe='HI') ",
				 fp[fpindex].id, fp[fpindex].id);
				       
		  fcstheightPtr = GetFcstHeight(where);
		  if (fcstheightPtr != NULL)
		     risetrend_flag = 1;
		}  */   	             		   
		   		   
            }
	    
	    if (trendphrase_str != NULL && trendstage_str != NULL && 
	        trendtime_str   != NULL)
	    {
	       
	       strcat(concat_all_str, trendphrase_str);
	       
               if (strcmp(trendstage_str, "") != 0)
	       {
		  strcat(concat_all_str, " ");	       	 
	          strcat(concat_all_str, trendstage_str);
	       }
	       
	       if (strcmp(trendtime_str, "") != 0) 
	       {
		 strcat(concat_all_str, " ");
		 
		 /* add "thru" proposition for FLUCTUATE_NEAR and 
		    WEIR_REMAIN_OVERFLOW index */
		    
		 if (fp[fpindex].action_index[k] >= TRENDPHRASE_BASE_INDEX)
	            action_index = fp[fpindex].action_index[k] - TRENDPHRASE_BASE_INDEX;
		 else
	            action_index = fp[fpindex].action_index[k];

		 if (action_index == FLUCTUATE_NEAR || 
		     action_index == WEIR_REMAIN_OVERFLOW)
		     strcat(concat_all_str, "thru ");
		 else 	     	        
	             strcat(concat_all_str, "");
               } 		  
	       strcat(concat_all_str, trendtime_str);	
	           	   	  
	       
	       /* add "with continued rise expected " if risetrend_flag = 1 */
	       
	     /*  if (risetrend_flag == 1)
	         strcat(concat_all_str, " with continued rise expected"); */
            }
	  }
	  
	  /* check if need to add "with continued rise expected" at the end of
	     the trendphrase. If the pe in shef code
	    is HI, the type is F for forecast data and the value is 
	    1 or 5 which mean rising */


	  /* retrieve pe and its value from fcstheight table for this point */

	  sprintf(where, "WHERE lid='%s' AND pe='HI' AND probability < 0.0 "
		         " AND value='1' AND validtime > CURRENT_TIMESTAMP "
			 " OR value='5' AND basistime=(SELECT MAX(basistime) "
			 " FROM fcstheight WHERE lid='%s' AND pe='HI') ",
			 fp[fpindex].id, fp[fpindex].id);

	  fcstheightPtr = GetFcstHeight(where);
	  if (fcstheightPtr != NULL && not_only_rise == FALSE)
	  {
	     strcat(concat_all_str, " with continued rise expected");   
	  }	    	            

         
          /* final the output sentences */
	  
	  strcpy(longstring, concat_all_str);
	  if (strcmp(longstring, "") != 0)
	  {
             strcat(longstring, ".");
             convert_str_to_lowcase(longstring);
	  }
       }    
       	
	
       /* if tzone for an individual location was used
          then set TZ back to original default value */
	 
       if (pcc->product.timeZoneFlag == 1)
       {
	  if (strcmp(lastTZset, misc->defaultTZ) != 0)
	  {
		memset( setTZcmd, 0, 25);
		sprintf(setTZcmd, "TZ=%s",misc->defaultTZ);
		putenv_rc = putenv(setTZcmd);
		
		memset( lastTZset, 0, TZ_LEN + 1);
		strncpy(lastTZset, misc->defaultTZ, TZ_LEN);
	  }
       }		    
     }              
       
       
     return;
}      	    
	
	    
/*****************************************************************
    load_trendfp_phrase()
    Purpose: load corresponding trend phrases from fcst_trend_data.xxx
             according to the action_index.	       
*******************************************************************/
char  *load_trendfp_phrase(int                   k,
                           const int             fpindex,
			   fp_struct             *fp,
			   fcsttrend_info_struct *fcsttrend_info) 
{
    int trendphrase_index = 0, i;
    char *phrase_str;
    
    /*initialize */
    
    phrase_str = (char *) malloc(sizeof(char) * MAXLEN_LONGSTR);
    if (phrase_str == NULL)
    {
       log_msg(FAILED_MALLOC, "of phrase_str in load_trendfp_phrase");
       exit(-1);
    }
    else   
       strcpy(phrase_str, "");
    
    /*if fp[].action_index greater than the TRENDPHRASE_BASE_INDEX (now is 50),
      use "then" in trend phrase */
      
    if (fp[fpindex].action_index[k] < TRENDPHRASE_BASE_INDEX)
       trendphrase_index = fp[fpindex].action_index[k];
    else
    {
       trendphrase_index = fp[fpindex].action_index[k] - TRENDPHRASE_BASE_INDEX;
       strcat(phrase_str, " then ");
    }
   
    
    /*loop the number of trend phrase and find the phrase for each
      trend index */
      
    for (i=0; i < fcsttrend_info->num_phrase; i++)
    {
        if (trendphrase_index == fcsttrend_info->trend_index[i])
	{
	   strcat(phrase_str, fcsttrend_info->trend_phrase[i]);
	   
	   break;
	}             			   			       
    }
    
    if (strcmp(phrase_str, " then ") == 0)
	      strcpy(phrase_str, "");
	      				       	
    return (phrase_str);
}    


/*********************************************************************
   load_trendfp_stage()
   
   PURPOSE 
   For each forecast point, load the stage/flow value according to
   the different fp[].action_index[] which determines the trend for the full
   time series 
   
***********************************************************************/
char *load_trendfp_stage(int             k,
                         const int       fpindex,
			 fp_struct       *fp)
{

     char *stage_str;
     char *value_to_str;
     
     /*initialize */
     
     stage_str = (char *) malloc(sizeof(char) * MAXLEN_STRING);
     if (stage_str == NULL)
     {
        log_msg(FAILED_MALLOC, "of stage_str in load_trendfp_stage");
	exit(-1);
     }  	
     else	
        strcpy(stage_str, "");
     
     value_to_str = (char *) malloc(sizeof(char) * 10);
     if (value_to_str == NULL)
     {
        log_msg(FAILED_MALLOC, "of value_to_str in load_trendfp_stage");
	exit(-1);
     }
     else
        strcpy(value_to_str, "");
		
     if (fp[fpindex].action_value[k] != MISSINGVAL)
     {  
        /*value_to_str = gcvt(fp[fpindex].action_value[k],4, value_to_str); */
	
	sprintf(value_to_str, "%.1f", fp[fpindex].action_value[k]);
	
	if (value_to_str != NULL)
	{
	   if (fp[fpindex].pe[0] != 'Q')
	      strcat(value_to_str, " FT");
	   else
	      strcat(value_to_str, " CFS");
	         
           strcat(stage_str, value_to_str);
	}  
     }	
     
	
     return(stage_str);
}
     		
     			    
/******************************************************************
    load_trendfp_time()
    
    PURPOSE 
    For each forecast point, load the time value for
    stage/flow according to the different fp[].action_index[] which
    determines the trend for the full time series.
    
*******************************************************************/
char *load_trendfp_time(int               k,
                        const int         fpindex,
			fp_struct         *fp,
			misc_struct 	*misc)
{
    char *time_str;
    char dtstr[MAXLEN_STRING];
        
    /*initialize */
    
    memset(dtstr, '\0', MAXLEN_STRING);
    
    time_str = (char *) malloc(sizeof(char) * MAXLEN_STRING);	
    if (time_str == NULL)
    {
       log_msg(FAILED_MALLOC, "of time_str in load_trendfp_time");
       exit(-1);
    }
    else
       strcpy(time_str, "");          
       
    /*load formmated time info*/
    
    if (fp[fpindex].action_time[k] != MISSINGVAL)
    {    
      format_timephrase(misc, fp[fpindex].action_time[k], dtstr);   
    
      if (dtstr != NULL)
         strcpy(time_str, dtstr);
    }	 
    
    return(time_str);
    
}                 		


/*********************************************************************
   load_event_variable_value()
   
   PURPOSE
   Loads the value of variables based on events.      
   
   *******************************************************************/
void load_event_variable_value(int		   	   varindex,
		  	         const int		   fpindex,
			    	 char			   locid[],
			         fp_struct	           *fp,
				 grp_struct                *grp,
				 values                    *rawvalue,
				 vtecinfo_struct           *vtecinfo,
                            	 pcc_struct                *pcc,
			    	 misc_struct               *misc,
			         char		           longstring[])
{  
   time_t temp_timet; 
   
      
   /* get the state|county list for based on event */
   
   if (vmatch(varindex, "<ActionStateCntyList>") == 0)
      build_statecntylist_byaction(fpindex, fp, grp, misc, pcc, vtecinfo, 
                                   longstring);
   
   
   /* get the state list for forecast points with the same action */
       
   else if (vmatch(varindex, "<ActionStateList>") == 0) 
      build_statelist_byaction(fpindex, fp, grp, misc, pcc, vtecinfo, 
                               longstring);   
   
   
   /* get the river list for forecast points with the same action */
   
   else if (vmatch(varindex, "<ActionRiverList>") == 0)
     build_riverlist_byaction(fpindex, fp, grp, misc, pcc, vtecinfo, 
                              longstring);
      
      
   /* get the action code for each event */
   
   else if (vmatch(varindex, "<Action>") == 0)
      strcpy(rawvalue->s, load_action_str(fpindex, vtecinfo, pcc));
    		
		   	 	  	  
   /* get the event's ending time */
   
   else if (vmatch(varindex, "<EventEndTime>") == 0)
   {
      if (vtecinfo[fpindex].endtime == MISSINGVAL)
         strcpy(longstring, "FURTHER NOTICE");
      else	 
         rawvalue->t = vtecinfo[fpindex].endtime;
   }
   
      
   /* get the event's starting time */
   
   else if (vmatch(varindex, "<EventBeginTime>") == 0)
   {
      if (vtecinfo[fpindex].begintime == CURRENT_TIME)
         rawvalue->t = misc->system_time;
      else 	 
         rawvalue->t = vtecinfo[fpindex].begintime;
   }
     
   /* get the event's rise above flood stage time, this time 
    is determined by algorithm to compare the observed/foreast
    rise above time type source with the previous issued event's
    rise above time type source */
   
   else if (vmatch(varindex, "<VRTime>") == 0)
   {   
       if (vtecinfo[fpindex].risetime != MISSINGVAL)  	 
          rawvalue->t = vtecinfo[fpindex].risetime;
       else
          strcpy(longstring, "MISSING");
   }
   
   /* get the event's crest time, this time 
   is determined by algorithm to compare the observed/foreast
   crest time type source with the previous issued event's
   crest time type source */

   else if (vmatch(varindex, "<VCTime>") == 0)
   {      	 
       if (vtecinfo[fpindex].cresttime != MISSINGVAL)
          rawvalue->t = vtecinfo[fpindex].cresttime;
       else
          strcpy(longstring, "MISSING");
   }
   
   /* get the event's crest value */

   else if (vmatch(varindex, "<VCValue>") == 0)
   {      
       rawvalue->f = vtecinfo[fpindex].crest_value;
   }     
        
   
   /* get the event's fall below flood stage time, this time 
    is determined by algorithm to compare the observed/foreast
    fall below time type source with the previous issued event's
    fall below time type source */
   
   else if (vmatch(varindex, "<VFTime>") == 0)
   {    
       if (vtecinfo[fpindex].falltime != MISSINGVAL) 	 
          rawvalue->t = vtecinfo[fpindex].falltime;
       else
          strcpy(longstring, "MISSING");	  
   }
   
   /* get the T_WWA format for <EventTime> */
   
   else if (vmatch(varindex, "<EventTime>") == 0)
      strcpy(rawvalue->s, load_eventtime_str(fpindex, vtecinfo, pcc, misc)); 
      
      
   /* get the previously issued event's begin time.  it must use the 
      event type which matches the current event type. */
   
   else if (vmatch(varindex, "<PrevEventBeginTime>") == 0)
   {
      if (strcmp(vtecinfo[fpindex].signif, SIGNIF_WARNING) == 0)
         temp_timet = vtecinfo[fpindex].prev_flw.begintime;
	 
      else if (strcmp(vtecinfo[fpindex].signif, SIGNIF_WATCH) == 0)
         temp_timet = vtecinfo[fpindex].prev_fla.begintime;
	 
      else if (strcmp(vtecinfo[fpindex].signif, SIGNIF_ADVISORY) == 0)
         temp_timet = vtecinfo[fpindex].prev_fly.begintime;
	 
      else
         temp_timet = MISSINGVAL;
	 
	 
      if (temp_timet == MISSINGVAL)
         strcpy(longstring, "FURTHER NOTICE");
      else	 
         rawvalue->t = temp_timet;
   } 
   
       
   /* get the previously issued event's ending time.  it must use the 
      event type which matches the current event type. */
   
   else if (vmatch(varindex, "<PrevEventEndTime>") == 0)
   {
      if (strcmp(vtecinfo[fpindex].signif, SIGNIF_WARNING) == 0)
         temp_timet = vtecinfo[fpindex].prev_flw.endtime;
	 
      else if (strcmp(vtecinfo[fpindex].signif, SIGNIF_WATCH) == 0)
         temp_timet = vtecinfo[fpindex].prev_fla.endtime;
	 
      else if (strcmp(vtecinfo[fpindex].signif, SIGNIF_ADVISORY) == 0)
         temp_timet = vtecinfo[fpindex].prev_fly.endtime;
	 
      else
         temp_timet = MISSINGVAL;
	 
	 
      if (temp_timet == MISSINGVAL)
         strcpy(longstring, "FURTHER NOTICE");
      else	 
         rawvalue->t = temp_timet;
   } 
         
		
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/whfs_lib/src/RPFEngine/RCS/load_variable_value.c,v $";
 static char rcs_id2[] = "$Id: load_variable_value.c,v 1.6 2007/02/22 15:40:18 deng Exp $";}
/*  ===================================================  */

}     
    
