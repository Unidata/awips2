/*********************************************************************
   load_phrase_data.c


   load_phrase_data()
   
   MODIFICATION HISTORY DATE    PROGRAMMER   DESCRIPTION
   load_phrase_data()   05/2004  Jingtao Deng Add vtecinfo as argument

   *******************************************************************/

#include <stdio.h>             /* standard io library functions */
#include <string.h>            /* library string functions */
#include <stdlib.h>            /* standard library functions */

#include "load_phrase_data.h" 
#include "template_defs.h"

/*********************************************************************
   load_phrase_data()
   
   PURPOSE
   Loads the data into the variables specified within the phrase.
   
   NOTES
   This function load the data into newphrase, which is a pointer
   to a pointer.  Its approach for loading this data is coordinated
   with the calling functions in the manner in which mallocs are 
   performed.
   
   Converting strings to upper case is done just before writing
   the output.
   
   *******************************************************************/

void load_phrase_data(const int				fpindex,
		      const int				product_section,
		            fp_struct			*fp,
		            grp_struct			*grp,
		      const int                         numcnty,
		            county_struct               *cnty,
		            misc_struct			*misc,
		            vtecinfo_struct             *vtecinfo,
		      const int				phrasenum,
		      const template_info_struct 	*template_info,
		            pcc_struct			*pcc,
		      	    char			**newphrase)  
{
   extern template_variable_struct TEMPLATE_VARIABLES_TABLE[];
   const int 	NOREADVAR = -1;
   
   static char 	*phrase_ptr;
   static int 	first_time = TRUE;
   int		newpos;
   
   int 		phraselen, pos;
   int 		varstart, varlen;
   int 		startflag, endflag;
   char 	varname[MAXLEN_VARNAME];
   itemtypes 	vartype;
   values 	rawvalue;
   char 	longstring[MAXLEN_LONGSTR];
   char 	valuestr[MAXLEN_LONGSTR];
   int 		valuelen, stagevar_cnt;
   char 	msgstr[MAXLEN_STRING + MAXLEN_STRING];
   char		dqcode[SHEF_QC_LEN + 1];
   int 		pad_switch = FALSE;
   varinfo_struct	varinfo;
   int		format_index;
   char		locid[LOC_ID_LEN + 1];
   int          filter_qc = 1;
   
   /* initialize */
   
   varstart = NOREADVAR;
   newpos   = 0;
   stagevar_cnt = 0;
   
   memset(longstring, 0, MAXLEN_LONGSTR);
   memset(valuestr,   0, MAXLEN_LONGSTR);
   
   memset(locid,      0, LOC_ID_LEN + 1);
   
   *newphrase = NULL;
     
   
   /* for the first time through this function, allocate storage for 
      the output string assembled by this function;  */ 
   
   if (first_time)
   {
      phrase_ptr = (char *)malloc(MAXLEN_NEWPHRASE + 1);
      if (phrase_ptr == NULL) 
	 log_msg(FAILED_MALLOC, "for phrase in load_phrase_data");
      first_time = FALSE;
   }
   
     
   /* zero out the memory each time and load a convenience value for
      the length of the original phrase  */
   
   memset(phrase_ptr, 0, MAXLEN_NEWPHRASE + 1);    
   phraselen = template_info->phraselen[phrasenum]; 
      
   
   /* loop on the number of characters in the original phrase string */
   
   for (pos = 0; pos < phraselen; ++pos)
   {
      /* check if the character is the start or end of a variable name */
      
      startflag = strncmp(&template_info->phrase[phrasenum][pos], "<", 1);
      endflag   = strncmp(&template_info->phrase[phrasenum][pos], ">", 1);
      
      
      /* if this character starts a variable name, then save the position */
      
      if (startflag == 0)
	 varstart = pos;
      
      
      /* if this character ends a variable name definition, then
	 load the value of the variable into the string;
	 allow ">" (end variable definition character) to 
	 be there by itself for purposes other than defining
	 a variable */
      
      else if (endflag == 0 && varstart != NOREADVAR)
      {	 
	 varlen = pos - varstart + 1;
	 	 
	 
	 /* check that the variable is properly defined */
	 
	 if (varlen  > MAXLEN_VARNAME - 1)
	 {
	    sprintf(msgstr, "%d in template %s", phrasenum+1, 
		    template_info->name);
	    log_msg(EXCEED_VARNAME, msgstr);
	    break;
	 }
	 	 
	 strncpy(varname, &template_info->phrase[phrasenum][varstart], varlen);
	 memset(&varname[varlen], 0, 1);
	 
	 
	 /* check that the variable is a valid type and get its index;
	    this is done here instead of when the template is read
	    since the phrase data is parsed only when the phrase is built,
	    which is only when any associated condition is true;
	    if valid, then check that the variable is permitted to be used
	    in this product section */
	 
	 vartype = RPF_INVALID;
	 check_if_variable(varname, &vartype, &varinfo);
	 		
	 if (vartype == RPF_INVALID)
 	 {
	    sprintf(msgstr, "%d in template %s; variable %s ", phrasenum+1, 
		    template_info->name, varname);
	    log_msg(INVALID_VARNAME_PHRASE, msgstr);
	    varinfo.varindex = MISSINGVAL;
	   /* break;*/
	 }
	 
	 else if
	    (check_var_access(TEMPLATE_VARIABLES_TABLE[varinfo.varindex].accesslist,
			      product_section) != TRUE)
	 {
	    log_msg(VAR_NOT_ALLOWED, varname);
	    break;
	 }

	 
	 /* if this is a stage value that is dependent upon a specified
	    stage time, then determine which stage time goes with the
	    stage value. the approach used is that the first encounter
	    with a stage value (that uses stage times) uses the first
	    stage time specified, the second uses the second, etc. */
	 
	 if (strcmp(varname, "<SpecObsStg>")      == 0 ||
	     strcmp(varname, "<SpecFcstStg>")     == 0 ||
	     strcmp(varname, "<SpecObsStgTime>")  == 0 ||
	     strcmp(varname, "<SpecFcstStgTime>") == 0) stagevar_cnt++;
	 
	 
	 /* get the variable value and load it into the rawvalue 
	    union; for the variables that are long string variables, the
	    value is defined in longstring and not in the union rawvalue */
	 
	 if (fpindex != MISSINGVAL)
	    strcpy(locid, fp[fpindex].id);
	 
	 if (varinfo.varindex != MISSINGVAL)
	 {
	    if (strcmp(TEMPLATE_VARIABLES_TABLE[varinfo.varindex].name,
		       "<PEVal>") == 0 ||
		strcmp(TEMPLATE_VARIABLES_TABLE[varinfo.varindex].name,
		       "<PETime>") == 0)
	    {	       
	       load_pe_value(filter_qc, locid, varinfo, 
			     &rawvalue, dqcode, longstring);
	    }
	    else
	    { 
	       load_variable_value(fp, grp, numcnty, cnty, misc, vtecinfo, pcc,
			     &varinfo, fpindex, locid,
			     &template_info->spectime, stagevar_cnt - 1,
			     &rawvalue, dqcode, longstring);
	    }	 
	 
	   /* if the data is not defined in the longstring variable, 
	      then load from the rawvalue union; format the raw value
	      into a string and its length; formats for longstring are
	      ignored,  therefore they can not be truncated or rounded off. */
	 
	   if (strlen(longstring) == 0)
	   {	    
	    /* defined the format index as missing and format the
	       data.  the null string is the missing_val_str which is 
	       supported for the tabular section. */ 
	    
	      format_index =
	       get_format_index(varinfo.varindex,
				&template_info->variable, &template_info->format,
				TEMPLATE_VARIABLES_TABLE[varinfo.varindex].type);
	    
	      format_vals(rawvalue, dqcode, varinfo.varindex,
			&template_info->format, format_index, 
			misc, pad_switch,  "",
			valuestr, &valuelen, pcc, locid);	 	 
	   }
	   else
	   {
	      strcpy(valuestr, longstring);
	      valuelen = strlen(valuestr);
	   }
	 }
	 else
	 {
	    strcpy(valuestr,"");
	    valuelen = strlen(valuestr);
	 }   
	 
	 /* copy the value string and increment the length */
	 
	 if ((newpos + valuelen) > MAXLEN_NEWPHRASE - 1)
	 {
	    sprintf(msgstr, "; varname= %s", varname);
	    log_msg(EXCEED_NEWPHRASE, msgstr);
	 }
	 else
	 {
	    strncpy(phrase_ptr + newpos, valuestr, valuelen);
	    newpos = newpos + valuelen;
	 }
	 
	 
	 /* reset the varstart variable to indicate that no
	    variable definition is currently being read */
	 
	 varstart = NOREADVAR;
      }
      
      
      /* if no variable definition currently being read, then
         simply echo back the text verbatim */
      
      else if (varstart == NOREADVAR)
      {
         if ((newpos + 1) > MAXLEN_NEWPHRASE - 1)
	 {
	    sprintf(msgstr, "; template phrase char=%s",
	            &template_info->phrase[phrasenum][pos]);
		    log_msg(EXCEED_NEWPHRASE, msgstr);
	 }
	 else
	 {
	    strncpy(phrase_ptr + newpos, 
		 &template_info->phrase[phrasenum][pos], 1);
	    ++newpos;
	 }
      }
   }  /* end of loop on characters in phrase */
   
      
   /* check if new string length can fit into space reserved
      for old string; if not then issue message */
   
   if (newpos > MAXLEN_NEWPHRASE) log_msg(EXCEED_NEWPHRASE, "end");
   
   
   /* assign the assembled phrase into the returned pointer variable */
   
   *newphrase = phrase_ptr;

   
   return;
}
