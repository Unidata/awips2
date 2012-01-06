/**********************************************************************
   rpf_converts.c  
   
   convert_prodid_to_index()
   convert_prodreason_to_index()
   convert_prodreason_to_descr()
   convert_prodindex_to_id()
   
   convert_grpid_to_index()
   convert_fpid_to_index()
   
   convert_sectionname_to_index()
   convert_index_to_sectionname()
   convert_subsectionname_to_index()
   convert_index_to_sectionname()
   
   convert_refstage_to_index()
   convert_index_to_refstage()
   convert_index_to_refstagevar()
   
   convert_nwrflag_to_index()
   convert_index_to_nwrflag()
   
   convert_nwralert_to_index()
   convert_index_to_nwralert()
   
   convert_searchtype_to_index()
   convert_index_to_searchtype()
   
   convert_textcase_to_index()
   convert_index_to_textcase()
   
   convert_catindex_to_name()
   convert_catname_to_index()
   
   convert_str_to_upcase()
   convert_str_to_lowcase()
   
   convert_fporder_to_index()
   convert_index_to_fporder()
   convert_trendval_to_descr()
   
   convert_locdatetime_to_time()
   convert_gmdatetime_to_time()
   convert_time_to_gmdatetime()
   
   convert_timezoneflag_to_index()
   convert_index_to_timezoneflag()
   
   convert_vtecflag_to_index()
   convert_index_to_vtecflag()
   
   convert_vtecOTEmode_to_index()
   convert_index_to_vtecOTEmode()
   
   convert_index_to_vtecmodestr()
   
   trim_countyname()
   
   convert_index_to_ugcmode()
   convert_ugcmode_to_index()
   
 
   
  *******************************************************************/


#include <string.h>               /* string library */
#include <time.h>                 /* time library */
#include <ctype.h>                /* character type library */
#include <stdio.h>                /* standard io library */

#include "rpf_converts.h"         /* function prototypes */


/***********************************************************************
   convert_prodid_to_index()
   
   PURPOSE
   Given a product id, converts it to the numerical index.   
     
   ***********************************************************************/

int convert_prodid_to_index(char prodid[])
   
{
   int index;
   
   
   /* loop on the possible values, if not found log error */
   
   if (strcmp(prodid, "RVS") == 0)
      index = RVS;
   
   else if (strcmp(prodid, "FLS") == 0)
      index = FLS;
   
   
   else if (strcmp(prodid, "FLW") == 0)
      index = FLW;
   
   
   /* if doesn't match one of the specifically tracked
      product categories */

   else
      index = OTHER_PROD;
   
   return(index);
}


/***********************************************************************
   convert_prodreason_to_index()
   
   PURPOSE
   Given a numeric value that describes the reason for issuing a
   certain product, this function converts it to the index for the 
   particular product being issued.
   
   ***********************************************************************/

int convert_prodreason_to_index(int prodreason)
   
{
   int index;
   
   
   /* for each of the possible reason for issuing a certain product,
      assign the appropriate product index */
   
   if (prodreason == RVS_NO_DATA || 
       prodreason == RVS_NO_FLOODING)
      index = RVS;
      
   else if (prodreason == FLW_NEW_FLOODING       ||
	    prodreason == FLW_INCREASED_FLOODING ||
	    prodreason == FLW_GROUP_IN_FLW)
      index = FLW;
   
   else if (prodreason == FLS_CONTINUED_FLOODING ||
            prodreason == FLS_EXPIRED_FLOODING   ||
            prodreason == FLS_ENDED_FLOODING     ||
	    prodreason == FLS_GROUP_IN_FLS)
      index = FLS;
         
   else
   {
      log_msg(BADPRODREASON, "");
      index = RVS;
   }
   
   return(index);
}


/***********************************************************************
   convert_prodreason_to_id()
   
   PURPOSE
   Given a numeric value that describes the reason for issuing a
   certain product, this function converts it to a descriptive 
   string.
      
   NOTES
   
   ***********************************************************************/

char *convert_prodreason_to_descr(int prodreason)
   
{
   static char descr[MAXLEN_STRING];
   
   
   /* for each of the possible reason for issuing a certain product,
      assign the appropriate product index */
   
   memset(descr, 0, MAXLEN_STRING);
   
   if (prodreason == RVS_NO_DATA)
      strcpy(descr, "RVS: No Data");

   else if (prodreason == RVS_NO_FLOODING)
      strcpy(descr, "RVS: No Flooding");
      
   else if (prodreason == FLW_NEW_FLOODING)
      strcpy(descr, "FLW: New Flooding");
   
   else if (prodreason == FLW_INCREASED_FLOODING) 
      strcpy(descr, "FLW: Higher Flooding");
      
   else if (prodreason == FLW_GROUP_IN_FLW) 
      strcpy(descr, "FLW: In FLW Group");
         
   else if (prodreason == FLS_CONTINUED_FLOODING)
      strcpy(descr, "FLS: Continued Flooding");

   else if (prodreason == FLS_EXPIRED_FLOODING)
      strcpy(descr, "FLS: Expired Flooding");
      
   else if (prodreason == FLS_ENDED_FLOODING)
      strcpy(descr, "FLS: End Flooding");
      
   else if (prodreason == FLS_GROUP_IN_FLS) 
      strcpy(descr, "FLS: In FLS Group");
         
   else
   {
      log_msg(BADPRODREASON, "");
      strcpy(descr, "RVS: Default");
   }
   
   return(descr);
}


/***********************************************************************
   convert_prodindex_to_id()
   
   PURPOSE
   Given a numeric value that defines the product type, this function
   converts it to a string id.
   
   NOTES
   FLT is used as an identifier for a terminating flood statement.
   
   ***********************************************************************/

char *convert_prodindex_to_id(int prodindex)
   
{
   static char prodid[PRODUCT_LEN];
   
   
   /* for each of the possible products, 
      assign the appropriate product index */
      
   if (prodindex == RVS)
      strcpy(prodid, "RVS");
      
   else if (prodindex == FLS)
      strcpy(prodid, "FLS");
      
   else if (prodindex == FLW)
      strcpy(prodid, "FLW");
   
   else if (prodindex == OTHER_PROD)
      strcpy(prodid, "RVS");
   	 
   else
   {
      log_msg(BADPRODINDEX, "");      
      strcpy(prodid, "RVS");
   }
   
   return(prodid);
}


/***********************************************************************
   convert_grpid_to_index()
   
   PURPOSE
   Given a group id, converts it to the index used in the id structures.
   
   
   NOTES
   
   ***********************************************************************/

int convert_grpid_to_index(char		*grpid,
			   int		numgrps,
			   grp_struct	*grp)
   
{
   int index;
   
   
   /* loop on the number of groups. 
      when a match is found, break out of the loop and return*/
   
   for(index = 0; index < numgrps; ++index)
   {
      if(strcmp(grpid, grp[index].id) == 0)
	 return(index);
   }
   
   
   /* if no match found, issue warning message */
   
   log_msg(BAD_GRP_ID, grpid);
   index = MISSINGVAL;
   
   return(index);
}


/***********************************************************************
   convert_fpid_to_index()
   
   PURPOSE
   Given a forecast point id, converts it to the index used in the
   id structure.   
   
   ***********************************************************************/

int convert_fpid_to_index(char		*fpid,
			  int		numfps,
			  fp_struct	*fp)
   
{
   int index;
   
   
   /* loop on the number of forecast points. 
      when a match is found, break out of loop and return */
   
   for(index = 0; index < numfps; ++index)
   {
      if(strcmp(fpid, fp[index].id) == 0)
	 return(index);
   }
   
   
   /* if no match found, issue warning message */
   
   log_msg(BAD_FP_ID, fpid);
   index = MISSINGVAL;
   
   return(index);
}


/***********************************************************************
   convert_sectionname_to_index()
   
   PURPOSE
   Given the name of a product section, this function converts the name
   into a numeric index.
   
   ***********************************************************************/

int convert_sectionname_to_index(char *sectionname)
   
{
   int index;
   
   
   /* loop on the number of possible section names;
      if no match, then log message and abort */
   
   if (strcmp(sectionname, "HEADER") == 0)
      index = HEADER;
      
   else if (strcmp(sectionname, "BASIS") == 0)
      index = BASIS;
      
   else if (strcmp(sectionname, "HEADLINE") == 0)
      index = HEADLINE;
      
   else if (strcmp(sectionname, "SUMMARY") == 0)
      index = SUMMARY;
            
   else if (strcmp(sectionname, "TABULAR") == 0)
      index = TABULAR;
   
   else if (strcmp(sectionname, "POINT_SPECIFIC") == 0)
      index = POINT_SPECIFIC;
      
   else if (strcmp(sectionname, "DATA_ROUNDUP") == 0)
      index = DATA_ROUNDUP;
      
   else if (strcmp(sectionname, "IMPACT_STATEMENT") == 0)
      index = IMPACT_STATEMENT;
      
   else if (strcmp(sectionname, "HISTORICAL_COMPARISON") == 0)
      index = HISTORICAL_COMPARISON;
  
   else if (strcmp(sectionname, "CALL_TO_ACTION") == 0)
      index = CALL_TO_ACTION;
   
   else
   {
      log_msg(BADSECTIONNAME, sectionname);
      index = MISSINGVAL;
   }
   
   return(index);
}


/***********************************************************************
   convert_index_to_sectionname()
   
   PURPOSE
   Given the index of a product section, this function converts the index
   into the string name.
   
   
   NOTES
   returns an empty string (0 length) if it fails to find index.
   
   ***********************************************************************/

char *convert_index_to_sectionname(int index)
   
{
   static char sectionname[MAXLEN_STRING];
   char logmsg[80];
   
   
   /* loop on the number of possible section names;
      if no match, then log message and abort */
      
   if (index == HEADER)
      strcpy(sectionname, "HEADER");
      
   else if (index == BASIS)
      strcpy(sectionname, "BASIS");
      
   else if (index == HEADLINE)
      strcpy(sectionname, "HEADLINE");
       
   else if (index == SUMMARY)
      strcpy(sectionname, "SUMMARY"); 

   else if (index == TABULAR)
      strcpy(sectionname, "TABULAR");
   
   else if (index == POINT_SPECIFIC)
      strcpy(sectionname, "POINT_SPECIFIC");
      
   else if (index == DATA_ROUNDUP)
      strcpy(sectionname, "DATA_ROUNDUP");
      
   else if (index == IMPACT_STATEMENT)
      strcpy(sectionname, "IMPACT_STATEMENT");
      
    else if (index == HISTORICAL_COMPARISON)
      strcpy(sectionname, "HISTORICAL_COMPARISON");
  
   else if (index == CALL_TO_ACTION)
      strcpy(sectionname, "CALL_TO_ACTION");
   
   else
   {
      sprintf(logmsg, "index=%d", index);
      log_msg(BADSECTIONNAME_INDEX, logmsg);
      strcpy(sectionname, "");
   }
   
   return(sectionname);
}

/***********************************************************************
   convert_subsectionname_to_index()
   
   PURPOSE
   Given the name of a point-specific subsection, this function converts
   the name into a numeric index.
   
   NOTES
   
   ***********************************************************************/

int convert_subsectionname_to_index(char *subsectionname)
   
{
   int index;
   
   
   /* loop on the number of possible subsection names;
      if no match, then log message and abort */
   
   if (strcmp(subsectionname, "DATA_ROUNDUP") == 0)
      index = DATA_ROUNDUP;
   
   else if (strcmp(subsectionname, "IMPACT_STATEMENT") == 0)
      index = IMPACT_STATEMENT;

   else if (strcmp(subsectionname, "HISTORICAL_COMPARISON") == 0)
      index = HISTORICAL_COMPARISON;
   
   else
   {
      log_msg(BADSUBSECTIONNAME, subsectionname);
      index = MISSINGVAL;
   }
   
   return(index);
}


/***********************************************************************
   convert_index_to_subsectionname()
   
   PURPOSE
   Given the index of a product subsection, this function converts the index
   into the string name.
   
   
   NOTES
   returns an empty string (0 length) if it fails to find index.
   
   ***********************************************************************/

char *convert_index_to_subsectionname(int index)
   
{
   static char subsectionname[MAXLEN_STRING];
   char logmsg[80];
   
   
   /* loop on the number of possible section names;
      if no match, then log message and abort */
   if (index == DATA_ROUNDUP)
      strcpy(subsectionname, "DATA_ROUNDUP");
   
   else if (index == IMPACT_STATEMENT)
      strcpy(subsectionname, "IMPACT_STATEMENT");

   else if (index == HISTORICAL_COMPARISON)
      strcpy(subsectionname, "HISTORICAL_COMPARISON");
      
   else
   {
      sprintf(logmsg, "index=%d", index);
      log_msg(BADSUBSECTIONNAME_INDEX, logmsg);
      strcpy(subsectionname, "");
   }
   
   return(subsectionname);
}



/***********************************************************************
   convert_refstage_to_index()
   
   PURPOSE
   Given the string which specifies the reference stage to use 
   when searching stages, it returns its index value.
   
   NOTES
   This supports the names as defined in the PCC files and
   in the gui interface.
   
   ***********************************************************************/

int convert_refstage_to_index(char *refstage)
{
   int index;
   
   /* check against the allowable values;
      if not valid, then log error;
      for */
   
   if (strcmp(refstage, "MAXOBS") == 0 || strcmp(refstage, "OBS") == 0 ||
       strcmp(refstage, "Current Observed") == 0)
      index = CUR_OBSSTAGE;
   
   else if (strcmp(refstage, "MAXFCST") == 0 ||
	    strcmp(refstage, "Max Forecast") == 0)
      index = MAX_FCSTSTAGE;
   
   else if (strcmp(refstage, "MAX") == 0 ||
	    strcmp(refstage, "Current Obs/Max Fcst") == 0)
      index = MAX_STAGE;
   
   else
   {
      log_msg(BADREFSTAGE, refstage);
      index = MAX_STAGE;
   }
	 
	 
   return(index);
}

/***********************************************************************
   convert_index_to_refstage()
   
   PURPOSE
   Given the index which specifies the reference stage to use 
   when searching stages, it returns its string value.
   
   NOTES
   This supports the names as used for the gui interface.
   
   ***********************************************************************/

char *convert_index_to_refstage(int index)
{
   static char refstr[MAXLEN_STRING];
   
   /* check against the allowable values;
      if not valid, then log error  */
   
   if (index == CUR_OBSSTAGE)
      strcpy(refstr, "Current Observed");
   
   else if (index == MAX_FCSTSTAGE)
      strcpy(refstr, "Max Forecast");

   else if (index == MAX_STAGE)
      strcpy(refstr, "Current Obs/Max Fcst");

   else
   {
      log_msg(BADREFSTAGE, "index value");
      strcpy(refstr, "Current Obs/Max Fcst");
   }
	 	 
   return(refstr);
}


/***********************************************************************
   convert_index_to_refstagevar()
   
   PURPOSE
   Given the index which specifies the reference stage to use 
   when searching stages, it returns its string value 
   for use in the output variables in the template.
   
   NOTES
   This supports the names used for the template.
   
   ***********************************************************************/

char *convert_index_to_refstagevar(int index)
{
   static char refstr[MAXLEN_STRING];
   
   /* check against the allowable values;
      if not valid, then log error  */
   
   if (index == CUR_OBSSTAGE)
      strcpy(refstr, "OBSERVED");
   
   else if (index == MAX_FCSTSTAGE)
      strcpy(refstr, "FORECAST");

   else if (index == MAX_STAGE)
      strcpy(refstr, "MAXIMUM");

   else
   {
      log_msg(BADREFSTAGE, "index value");
      strcpy(refstr, "UNKNOWN");
   }
	 	 
   return(refstr);
}


/***********************************************************************
   convert_nwrflag_to_index()
   
   PURPOSE
   Given the nwr switch, returns index to it.
   
   NOTES
   This supports names as given in the pcc file 
   
   ***********************************************************************/

int convert_nwrflag_to_index(char *nwr_option)
{
   int index;
   
   /* check against the allowable values;
      if not valid, then log error */
   
   if (strcmp(nwr_option, "YES") == 0)
      index = TRUE;
   
   else if (strcmp(nwr_option, "NO") == 0 )
      index = FALSE;
      
   else
   {
      log_msg(BADNWRFLAG, nwr_option);	 
      index = FALSE;
   }
	 
   return(index);
}


/***********************************************************************
   convert_index_to_nwrflag()
   
   PURPOSE
   Given the switch setting for the nwr, returns a string defining it.
   
   NOTES
   This uses the names as defined in the pcc file
   
   ***********************************************************************/

char *convert_index_to_nwrflag(int index)
{
   static char nwr_option[MAXLEN_STRING];
   
   
   /* check against the allowable values;
      if not valid, then log error */
   
   if (index == TRUE)
      strcpy(nwr_option, "YES");
   
   else if (index == FALSE)
      strcpy(nwr_option, "NO");
          
   else
   {
      log_msg(BADNWRFLAG, "index value");	 
      strcpy(nwr_option, "NO");
   }
	 
   return(nwr_option);
}


/***********************************************************************
   convert_nwralert_to_index()
   
   PURPOSE
   Given the nwr alert setting, returns index to it.
   
   
   ***********************************************************************/

int convert_nwralert_to_index(char *nwr_option)
{
   int index;
   
   
   /* check against the allowable values;
      if not valid, then log error */
   
   if (strcmp(nwr_option, "BOTH") == 0)
      index = NWR_ALERT_BOTH;
   
   else if (strcmp(nwr_option, "SAME_ONLY") == 0 )
      index = NWR_ALERT_SAME_ONLY;
   
   else if (strcmp(nwr_option, "NEITHER") == 0 )
      index = NWR_ALERT_NEITHER;
   
   else
   {
      log_msg(BADNWRALERT, nwr_option);	 
      index = FALSE;
   }
   
   return(index);
}


/***********************************************************************
   convert_index_to_nwralert()
   
   PURPOSE
   Given the switch setting for the nwr alert, returns a string defining it.
      
   ***********************************************************************/

char *convert_index_to_nwralert(int index)
{
   static char nwr_option[MAXLEN_STRING];
   
   /* check against the allowable values;
      if not valid, then log error */
   
   if (index == NWR_ALERT_BOTH )
      strcpy(nwr_option, "BOTH");
   
   else if (index == NWR_ALERT_SAME_ONLY)
      strcpy(nwr_option, "SAME_ONLY");
   
   else if (index == NWR_ALERT_NEITHER)
      strcpy(nwr_option, "NEITHER");
          
   else
   {
      log_msg(BADNWRALERT, "index value");	 
      strcpy(nwr_option, "NEITHER");
   }
	 
   return(nwr_option);
}

/***********************************************************************
   convert_searchtype_to_index()
   
   PURPOSE
   Given the type of impact statement or historical crest
   search type, returns index to it.
   
   NOTES
   This supports names as given in the pcc file and in the GUI
   
   When writing back (i.e. saving) the pcc file, the names used there
   must agree with these names.
   
   ***********************************************************************/

int convert_searchtype_to_index(char *searchtype)
{
   int index;
   
   /* check against the allowable values;
      if not valid, then log error */
   
   if (strcmp(searchtype, "RECENT_IN_WINDOWS") == 0 ||
       strcmp(searchtype, "Recent in Stage/Flow,Year Window") == 0)
      index = RECENT_IN_WINDOWS;
   
   else if (strcmp(searchtype, "CLOSEST_IN_WINDOWS") == 0 ||
	    strcmp(searchtype, "Closest in Stage/Flow,Year Window") == 0)
      index = CLOSEST_IN_WINDOWS;
   
   else if (strcmp(searchtype, "RECENT_IN_STGWINDOW") == 0 ||
	    strcmp(searchtype, "Recent in Stage/Flow Window") == 0)
      index = RECENT_IN_STGWINDOW;
   
   else if (strcmp(searchtype, "CLOSEST_IN_STGWINDOW") == 0 ||
	    strcmp(searchtype, "Closest in Stage/Flow Window") == 0)
      index = CLOSEST_IN_STGWINDOW;
   
   else if (strcmp(searchtype, "HIGHEST_IN_STGWINDOW") == 0 ||
	    strcmp(searchtype, "Highest in Stage/Flow Window") == 0)
      index = HIGHEST_IN_STGWINDOW;
   
   else if (strcmp(searchtype, "BELOW_UPPER_STGWINDOW") == 0 ||
	    strcmp(searchtype, "All Below Upper Stage/Flow") == 0)
      index = BELOW_UPPER_STGWINDOW;
   
   else
   {
      log_msg(BADCRESTSEARCH, searchtype);	 
      index = RECENT_IN_WINDOWS;
   }

   return(index);
}


/***********************************************************************
   convert_index_to_searchtype()
   
   PURPOSE
   Given the index to the impact statement or historical crest
   search type, returns a string defining it.
   
   NOTES
   This uses the names as defined for the GUI options
   
   This function is NOT for use when saving the definitions
   of the pcc file.  There is no convert function for that;
   instead, the values are hardcoded in the write_pcc function...
   
   ***********************************************************************/

char *convert_index_to_searchtype(int index)
{
   static char searchtype[MAXLEN_STRING];
   
   
   /* check against the allowable values;
      if not valid, then log error */
   
   if (index == RECENT_IN_WINDOWS)
      strcpy(searchtype, "Recent in Stage/Flow,Year Window");
   
   else if (index == CLOSEST_IN_WINDOWS)
      strcpy(searchtype, "Closest in Stage/Flow,Year Window");
   
   else if (index == RECENT_IN_STGWINDOW)
      strcpy(searchtype, "Recent in Stage/Flow Window");

   else if (index == CLOSEST_IN_STGWINDOW)
      strcpy(searchtype, "Closest in Stage/Flow Window");
     
   else if (index == HIGHEST_IN_STGWINDOW)
      strcpy(searchtype, "Highest in Stage/Flow Window");
      
   else if (index == BELOW_UPPER_STGWINDOW)
      strcpy(searchtype, "All Below Upper Stage/Flow");
      
   else
   {
      log_msg(BADCRESTSEARCH, "index value");	 
      strcpy(searchtype, "CLOSEST_IN_WINDOWS");
   }
	 
   return(searchtype);
}


/***********************************************************************
   convert_textcase_to_index()
   
   PURPOSE
   Given the type of product text case, returns index to it.
   
   NOTES
   
   ***********************************************************************/

int convert_textcase_to_index(char *casetype)
{
   int index;
   
   
   /* check against the allowable values;
      if not valid, then log error */
   
   if (strcmp(casetype, "FORCEUPPER") == 0 )
      index = CASE_FORCEUPPER;
      
   else if (strcmp(casetype, "MIXED") == 0)
      index = CASE_MIXED;
      
   else
   {
      log_msg(BADTEXTCASE, casetype);	 
      index = CASE_FORCEUPPER;
   }
	 
   return(index);
}


/***********************************************************************
   convert_index_to_textcase()
   
   PURPOSE
   Given the index to the product text case, returns string for it.
   
   NOTES
   
   ***********************************************************************/

char * convert_index_to_textcase(int caseindex)
{
   static char casetype[MAXLEN_STRING];
   
   
   /* check against the allowable values;
      if not valid, then log error */
   
   if (caseindex == CASE_FORCEUPPER)
      strcpy(casetype, "FORCEUPPER");
      
   else if (caseindex == CASE_MIXED)
      strcpy(casetype, "MIXED");
      
   else
   {
      log_msg(BADTEXTCASE, "bad index");	 
      strcpy(casetype, "FORCEUPPER");
   }
	 
   return(casetype);
}


/***********************************************************************
   convert_catindex_to_name()
   
   PURPOSE
   Convert an index to the stage category to the category name.  
   
   NOTES
   
   ***********************************************************************/

char *convert_catindex_to_name(int catindex)
   
{
   static char catname[MAXLEN_CATNAME];   
   
   /* loop thru on the possible values and set the category name */
   
   if (catindex == NONFLOOD)
      strcpy(catname, "NONFLOOD");

   else if (catindex == MINOR)
      strcpy(catname, "MINOR");
   
   else if (catindex == MODERATE)
      strcpy(catname, "MODERATE");
   
   else if (catindex == MAJOR)
      strcpy(catname, "MAJOR");
      
   else if (catindex == RECORD)
      strcpy(catname, "RECORD");
   
   else if (catindex == NULLCAT)
      strcpy(catname, "UNKNOWN");
   
   else
   {
      log_msg(INVALID_CATINDEX, "");
      strcpy(catname, "UNKNOWN");
   }
   
   return(catname);
}


/***********************************************************************
   convert_catname_to_index()
   
   PURPOSE
   Convert an index to the stage category to the category name.  
   
   NOTES
   
   ***********************************************************************/

int convert_catname_to_index(char catname[])
   
{
   int index;
   
   /* loop thru on the possible values and set the category index */
   
   if (strcmp(catname, "NONFLOOD") == 0)
      index = NONFLOOD;
   
   else if (strcmp(catname, "MINOR") == 0)
      index = MINOR;
   
   else if (strcmp(catname, "MODERATE") == 0) 
      index = MODERATE;
   
   else if (strcmp(catname, "MAJOR") == 0)
      index = MAJOR;
   
   else if (strcmp(catname, "RECORD") == 0) 
      index = RECORD;
  
   /* if no match, then issue message */
   else
   {
      log_msg(INVALID_CATNAME, catname);
      index = NONFLOOD;
   }
   
   return(index);
}


/***********************************************************************
   convert_str_to_upcase()
   
   PURPOSE
   Convert a character string to upper case.  
   
   NOTES
   
   ***********************************************************************/
void convert_str_to_upcase(char *string)
{
   int i;
   size_t stringlen;

   /* get the length of the string */
   
   stringlen = strlen(string);
   
   
   /* loop on each character and convert */
   
   for (i = 0; i < stringlen; i++)
   {
   	if ( islower(string[i]) )
		string[i] = toupper(string[i]);
   }

   return;
}


/***********************************************************************
   convert_str_to_lowcase()
   
   PURPOSE
   Convert a character string to lower case.  
   
   NOTES
   
   ***********************************************************************/
void convert_str_to_lowcase(char *string)
{
   int i;
   size_t stringlen;

   /* get the length of the string */
   
   stringlen = strlen(string);
   
   
   /* loop on each character and convert  */
   
   for (i = 0; i < stringlen; i++)
   {
   	if ( isupper(string[i]) )
		string[i] = tolower(string[i]);
   }

   return;
}


/***********************************************************************
   convert_fporder_to_index()
   
   PURPOSE
   Convert a specified forecast point order option to an index.  
   
   NOTES
   The values can be either those required in the PCC file or those
   specified interactively through the gui.
   
   ***********************************************************************/

int convert_fporder_to_index(char *orderstr)
{
   int index;
   
   
   /* check against the allowable values; include support for 
      historical keywords that have been renamed.
      if not valid, then log error  */
   
   if (strcmp(orderstr, "DEFAULT") == 0 ||
       strcmp(orderstr, "ALPHA")   == 0)
      index = ORDER_DEFAULT;
   
   else if (strcmp(orderstr, "GROUP_DEFAULT") == 0 ||
	    strcmp(orderstr, "GROUP_ALPHA")   == 0)
      index = ORDER_GROUP_DEFAULT;

   else if (strcmp(orderstr, "GROUP_FP") == 0)
      index = ORDER_GROUP_FP;
      
   else if (strcmp(orderstr, "ACTION_ORDER") == 0)
      index = ORDER_BY_ACTION;   

   else
   {
      log_msg(INVALID_FPORDER, orderstr);
      index = ORDER_DEFAULT;
   }
   
   return(index);
}


/***********************************************************************
   convert_index_to_fporder()
   
   PURPOSE
   Convert an index for the specified forecast point order option to
   a string.  
   ***********************************************************************/

char *convert_index_to_fporder(int index)
{
   static char orderstr[MAXLEN_STRING];
   
   /* check against the allowable values;
      if not valid, then log error  */
   
   if (index == ORDER_DEFAULT)
      strcpy(orderstr, "Default");
   
   else if (index == ORDER_GROUP_DEFAULT)
      strcpy(orderstr, "Group_Default");

   else if (index == ORDER_GROUP_FP)
      strcpy(orderstr, "Group_Fp");
   
   else if (index == ORDER_BY_ACTION)
      strcpy(orderstr, "Action_Order");
      
   else
   {
      log_msg(INVALID_FPORDER, "bad index");
      strcpy(orderstr, "Default");
   }
   
   return(orderstr);
}


/***********************************************************************
   convert_trendval_to_descr()
   
   PURPOSE
   Convert an index for the specified forecast point order option to
   a string.  
   ***********************************************************************/

char *convert_trendval_to_descr(int index)
{
   static char trendstr[MAXLEN_STRING];
   
   /* check against the allowable values;
      if not valid, then log error  */
   
   if (index == RISE)
      strcpy(trendstr, "RISING");
   
   else if (index == UNCHANGED)
      strcpy(trendstr, "STEADY");
   
   else if (index == FALL)
      strcpy(trendstr, "FALLING");
   
   else
   {
      strcpy(trendstr, "UNKNOWN");
   }
   
   return(trendstr);
}


/***********************************************************************
   convert_locdatetime_to_time()
   
   PURPOSE
   Convert the date and time combination, given in local time, into 
   time_t variable for internal use.  
   
   NOTES
   The date and time are of the form "mm/dd/yyyy" and "hh:mm".
   
   ***********************************************************************/

time_t convert_locdatetime_to_time(char dait[],
			  	   char tyme[])
{
   struct tm time_s;
   time_t data_time;
   int slen, numread;
   int error_flag;
   char msgstr[MAXLEN_STRING];
   
   /* initialize */
   
   error_flag = FALSE;
   sprintf(msgstr, "%s %s", dait, tyme);

   /* check the lengths of the strings; note that a month and hour can
      be either 1 or 2 characters */
   
   slen = strlen(dait);
   if (slen > DATE_LEN || slen < DATE_LEN - 1) error_flag = TRUE;
   
   slen = strlen(tyme);
   if (slen > TIME_LEN || slen < TIME_LEN - 1) error_flag = TRUE;
   
   
   /* get the portions of the string, adjust them, 
      and load it into the time structure */
   
   numread = sscanf(dait, "%2d/%2d/%4d", &time_s.tm_mon, &time_s.tm_mday,
		    &time_s.tm_year);
   if (numread !=3) error_flag = TRUE;
   time_s.tm_year -= 1900;
   time_s.tm_mon -= 1;
   
   numread = sscanf(tyme, "%2d:%2d", &time_s.tm_hour, &time_s.tm_min);
   if (numread != 2) error_flag = TRUE;
   
   /* do some basic checks of the values */
   
   if (time_s.tm_mon  < 0 || time_s.tm_mon  > 11 ||
       time_s.tm_mday < 1 || time_s.tm_mday > 31 ||
       time_s.tm_year < 0 || time_s.tm_year > 200)  error_flag = TRUE;
   if (time_s.tm_hour < 0 || time_s.tm_hour > 23 ||
       time_s.tm_min  < 0 || time_s.tm_min  > 59)   error_flag = TRUE;
   
   
   /* initialize the rest of time structure;
      don't worry about tm_wday and tm_yday */
   
   time_s.tm_sec =  0;
   time_s.tm_isdst = 0;  /* this results in standard time being defined
			    even when savings time is in effect !!! */
   
   /* if error occured, notify and hardcode date and time */
   
   if (error_flag == TRUE)
   {
      log_msg(INVALID_DATETIME, msgstr);
      data_time = 0;
   }
   
   
   /* load the time structure into the time_t variable */
   
   else
   {
      data_time = mktime(&time_s);
      if (data_time == -1)
      {
	 log_msg(INVALID_MKTIME, msgstr);
	 data_time = 0;
      }
   }
	 
   return(data_time);
}


/***********************************************************************
   convert_gmdatetime_to_time()
   
   PURPOSE
   Convert the date and time combination, given in gm time, into 
   time_t variable for internal use.  
   
   NOTES
   The date and time are of the form "mm/dd/yyyy" and "hh:mm".
   
   ***********************************************************************/

time_t convert_gmdatetime_to_time(char dait[],
			  	  char tyme[])
{
   struct tm time_s;
   time_t data_time;
   int slen, numread;
   int error_flag;
   char msgstr[MAXLEN_STRING];
   
   /* initialize */
   
   error_flag = FALSE;
   sprintf(msgstr, "%s %s", dait, tyme);

   
   /* check the lengths of the strings; note that a month, day, 
      hour, or minute can be either 1 or 2 characters */
   
   slen = strlen(dait);
   if (slen > DATE_LEN || slen < DATE_LEN - 2) error_flag = TRUE;   
   slen = strlen(tyme);
   if (slen > TIME_LEN || slen < TIME_LEN - 2) error_flag = TRUE;
   
   
   /* get the portions of the string, adjust them, 
      and load it into the time structure */
   
   numread = sscanf(dait, "%d/%d/%d", &time_s.tm_mon, &time_s.tm_mday,
		    &time_s.tm_year);
   if (numread !=3) error_flag = TRUE;
   time_s.tm_year -= 1900;
   time_s.tm_mon -= 1;
   
   numread = sscanf(tyme, "%d:%d", &time_s.tm_hour, &time_s.tm_min);
   if (numread != 2) error_flag = TRUE;
   
   
   /* do some basic checks of the values */
   
   if (time_s.tm_mon  < 0 || time_s.tm_mon  > 11 ||
       time_s.tm_mday < 1 || time_s.tm_mday > 31 ||
       time_s.tm_year < 0 || time_s.tm_year > 200)  error_flag = TRUE;
   if (time_s.tm_hour < 0 || time_s.tm_hour > 23 ||
       time_s.tm_min  < 0 || time_s.tm_min  > 59)   error_flag = TRUE;
   
   
   /* initialize the rest; don't worry about tm_wday and tm_yday */
   
   time_s.tm_sec =  0;
   time_s.tm_isdst = 0;
   
   
   /* if error occured, notify and hardcode date and time;/
      otherwise load the time structure into the time_t variable */
   
   if (error_flag == TRUE)
   {
      log_msg(INVALID_DATETIME, msgstr);
      data_time = 0;
   }
      
   else
   {
      data_time = gm_mktime(&time_s);
      if (data_time == -1)
      {
	 log_msg(INVALID_MKTIME, msgstr);
	 data_time = 0;
      }
   }
	 
   return(data_time);
}


/***********************************************************************
   convert_time_to_gmdatetime()
   
   PURPOSE
   Convert the date and time combination specified for various data into 
   time_t variable for internal use.  
   
   NOTES
   The date and time are of the form "mm/dd/yyyy" and "hh:mm".
   
   ***********************************************************************/

void convert_time_to_gmdatetime(time_t	data_time,
			              char 	*dait,
				      char	*tyme)
{
   struct tm *tmPtr;
   int numwrote;
   int error_flag;
   
   
   /* initialize */
   
   error_flag = FALSE;   
   
   
   /* convert to time structure */   
   
   tmPtr = gmtime(&data_time);
   
   /* get the portions of the string, adjust them, 
      and load it into the time structure */
   
   tmPtr->tm_year += 1900;
   tmPtr->tm_mon += 1;
   numwrote = sprintf(dait, "%2.2d/%2.2d/%4.4d", tmPtr->tm_mon, tmPtr->tm_mday,
		      tmPtr->tm_year);
   if (numwrote != DATE_LEN) error_flag = TRUE;
   
   numwrote = sprintf(tyme, "%2.2d:%2.2d", tmPtr->tm_hour, tmPtr->tm_min);
   if (numwrote != TIME_LEN) error_flag = TRUE;
      
   
   /* if error occured, notify and hardcode date and time */
   
   if (error_flag == TRUE)
   {
      log_msg(INVALID_TIME, "");
      strcpy(dait, "01/01/1970");
      strcpy(tyme, "00:00");
   }      
	 
   return;
}


/***********************************************************************
   convert_segment_mode_to_index()
   
   PURPOSE
   Given a segmentation mode, converts it to the numerical index.   
   
   NOTES
   Created by Hank Herr on 1/11/2001 for segmentation
   Added COUNTY mode on 02/15/02 by Deng, Jingtao
   ***********************************************************************/

int convert_segment_mode_to_index(char mode[])
   
{  
   int	modecode;
   
   if (strcmp(mode, "NONE") == 0)
      modecode = SEGMENT_MODE_NONE;
   
   else if (strcmp(mode, "POINT") == 0)
      modecode = SEGMENT_MODE_POINT;
   
   else if (strcmp(mode, "GROUP") == 0)
      modecode = SEGMENT_MODE_GROUP;
   
   else if (strcmp(mode, "COUNTY") == 0)
      modecode = SEGMENT_MODE_COUNTY;
   
   else
   {
      log_msg(BADSEGMODE, "");      
      modecode = SEGMENT_MODE_NONE;
   }
   
   
   return(modecode);
}


/***********************************************************************
   convert_index_to_segment_mode()
   
   PURPOSE
   Given a segmentation mode, converts it to the numerical index.   
   
   NOTES
   Created by Hank Herr on 1/11/2001 for segmentation
   Added COUNTY mode on 02/15/02 by Deng, Jingtao
   ***********************************************************************/

char *convert_index_to_segment_mode(int index)
   
{
   static char mode[MAXLEN_STRING];
   
   strcpy(mode, "");
   
   if (index == SEGMENT_MODE_NONE)
      strcpy(mode, "NONE");
   
   else if (index == SEGMENT_MODE_POINT)
      strcpy(mode, "POINT");
   
   else if (index == SEGMENT_MODE_GROUP)
      strcpy(mode, "GROUP");
   
   else if (index == SEGMENT_MODE_COUNTY)
      strcpy(mode, "COUNTY");
   
   else
   {
      log_msg(BADSEGMODEINDEX, "");
      strcpy(mode, "NONE");
   }
   
   return mode;
}


/***********************************************************************
   convert_vtecflag_to_index()
   
   PURPOSE
   Given a VTEC switch, converts it to the numerical index.   
   
   NOTES
   This supports names as given in the pcc file.
  
   ***********************************************************************/

int convert_vtecflag_to_index(char *vtec_option)
   
{
   int index;
   
   /* check against the allowable values, if not valid, then log error */
   
   if (strcmp(vtec_option, "YES") == 0)
      index = TRUE;
   
   else if (strcmp(vtec_option, "NO") == 0)
      index = FALSE;
   
   else
   {
      log_msg(BADVTECFLAG, vtec_option);
      index = FALSE;
   } 
   
   
   return(index);
}

/***********************************************************************
   convert_index_to_vtecflag()
   
   PURPOSE
   Given the switch setting for the VTEC, returns a string defining it.
   
   NOTES
   This uses the names as defined in the pcc file.
   ***********************************************************************/

char *convert_index_to_vtecflag(int index)  
{
   static char vtec_option[MAXLEN_STRING];
   
   
   /* heck against the allowable value, if not valid, then log error */
   
   if (index == TRUE)
      strcpy(vtec_option, "YES");
   
   else if (index == FALSE)
      strcpy(vtec_option, "NO");
   
   else
   {
      log_msg(BADVTECFLAG, "index value");
      strcpy(vtec_option, "NO");
   }        
   
   return(vtec_option);  
}


/***********************************************************************
   convert_timezoneflag_to_index()
   
   PURPOSE
   Given a TIMEZONE switch, converts it to the numerical index.   
   
   NOTES
   This supports names as given in the pcc file.
  
   ***********************************************************************/

int convert_timezoneflag_to_index(char *timezone_option)
   
{
   int index;
   
   /* check against the allowable values, if not valid, then log error */
   
   if (strcmp(timezone_option, "YES") == 0)
      index = TRUE;
   
   else if (strcmp(timezone_option, "NO") == 0)
      index = FALSE;
   
   else
   {
      log_msg(BADTIMEZONEFLAG, timezone_option);
      index = FALSE;
   } 
   
   
   return(index);
}


/***********************************************************************
   convert_index_to_timezoneflag()
   
   PURPOSE
   Given the switch setting for the TIMEZONE, returns a string defining it.
   
   NOTES
   This uses the names as defined in the pcc file.
   ***********************************************************************/

char *convert_index_to_timezoneflag(int index)
   
{
   static char timezone_option[MAXLEN_STRING];
   
   /* check against the allowable value, if not valid, then log error */
   
   if (index == TRUE)
      strcpy(timezone_option, "YES");
   
   else if (index == FALSE)
      strcpy(timezone_option, "NO");
   
   else
   {
      log_msg(BADTIMEZONEFLAG, "index value");
      strcpy(timezone_option, "NO");
   }        
   
   
   return(timezone_option);   
}


/***********************************************************************
   convert_index_to_vtecmodestr()
   
   PURPOSE
   Given a segmentation mode, converts it to the string field
   values recognized for the geo vtec mode.   
   
   ***********************************************************************/

char *convert_index_to_vtecmodestr(int segment_mode)   
{
   static char mode[VTEC_MODE_LEN + 1];
 
   
   strcpy(mode, "");
   
   if (segment_mode      == SEGMENT_MODE_POINT)
      strcpy(mode, "PT");
   
   else if (segment_mode == SEGMENT_MODE_GROUP)
      strcpy(mode, "GRP");
   
   else if (segment_mode == SEGMENT_MODE_COUNTY)
      strcpy(mode, "CNTY");
   
   else
   {
      log_msg(BADSEGMODEINDEX, "");
      strcpy(mode, "PT");
   }
   
   return mode;
}


/***********************************************************
   trim_countyname()
   
   Purpose
   load forecast points grouped by county for the current office.
   ************************************************************/
char *	trim_countyname(char *countyin)
{
   int		i;
   static char	countyout[COUNTY_LEN + 1];
   
   
   /* trim off any trailing non-alpha characters */
   
   if (countyin != NULL)
   {     
      for (i = strlen(countyin); i > 0; i--)
      {
	 if (isalpha(countyin[i]))
	 {
	    strcpy(countyout, "");
	    strncat(countyout, countyin, (i + 1));
	    break;
	 } 
      }
   }
   
   else
   {
      fprintf(stderr, "County name passed to trim_countyname() is blank.\n");
      strcpy(countyout, "");
   }
   
   return(countyout);  
}

/***********************************************************************
   convert_vtecOTEmode_to_index()
   
   PURPOSE
   Given a VTEC product mode, converts it to the numerical index.
   ***********************************************************************/

int convert_vtecOTEmode_to_index(char mode[])   
{  
   int	modecode;
   
   if (strcmp(mode, "O") == 0)
      modecode = VTEC_CAT_OPERATIONAL;
   
   else if (strcmp(mode, "E") == 0)
      modecode = VTEC_CAT_EXPERIMENTAL;
   
   else if (strcmp(mode, "T") == 0)
      modecode = VTEC_CAT_TEST;            
   
   else if (strcmp(mode, "X") == 0)
      modecode = VTEC_CAT_EXPOPER;
      
   else 
   {
      log_msg(BAD_OTE_MODE, "");              
      modecode = VTEC_CAT_OPERATIONAL;     
   }
      
   return(modecode);
}


/***********************************************************************
   convert_index_to_vtecOTEmode()
   
   PURPOSE
   Given a numerical index, converts it to the VTEC product mode
   
   ***********************************************************************/

char *convert_index_to_vtecOTEmode(int index)   
{
   static char mode[MAXLEN_STRING];
   
   strcpy(mode, "");
   
   if (index == VTEC_CAT_OPERATIONAL)
      strcpy(mode, "O");
   
   else if (index == VTEC_CAT_EXPERIMENTAL)
      strcpy(mode, "E");
   
   else if (index == VTEC_CAT_TEST)
      strcpy(mode, "T");
   
   else if (index == VTEC_CAT_EXPOPER)
      strcpy(mode, "X");
      
   else
      strcpy(mode, "O");
         
   return mode;
}

/***********************************************************************
   convert_index_to_ugcmode()
   
   PURPOSE
   Given a numerical index, converts it to the product UGC mode
   
   ***********************************************************************/

char *convert_index_to_ugcmode(int index)   
{
   static char mode[MAXLEN_STRING];
   
   strcpy(mode, "");
   
   if (index == UGC_BY_COUNTY)
      strcpy(mode, "COUNTY");
   
   else if (index == UGC_BY_ZONE)
      strcpy(mode, "ZONE");      
   
   else
      strcpy(mode, "COUNTY");
         
   return mode;
}

/***********************************************************************
   convert_ugcmode_to_index()
   
   PURPOSE
   Given a product UGC mode, converts it to the numerical index.
   ***********************************************************************/

int convert_ugcmode_to_index(char mode[])   
{  
   int	modecode;
   
   if (strcmp(mode, "COUNTY") == 0)
      modecode = UGC_BY_COUNTY;
   
   else if (strcmp(mode, "ZONE") == 0)
      modecode = UGC_BY_ZONE;  
   
   else 
   {
      log_msg(BAD_UGC_MODE, "");              
      modecode = UGC_BY_COUNTY;     
   }
      
   return(modecode);
}

