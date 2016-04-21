/*********************************************************************
   create_tabular_section.c
   
   create_tabular_section ()   
   load_tabular_values()
   write_leftover_fps()
  
   MODIFICATION HISTORY     DATE  PROGRAMMER      DESCRIPTION
   create_tabular_section() 05/2004 Jingtao Deng  Add vtecinfo as argument
   load_tabular_values()    05/2004 Jingtao Deng  Add vtecinfo as argument
   write_leftover_fps()     05/2004 Jingtao Deng  Add vtecinfo as argument
   *******************************************************************/

#include <stdio.h>             /* standard io library functions */
#include <string.h>            /* library string functions */
#include <stdlib.h>            /* standard library functions */

#include "create_tabular_section.h"

extern char paramdir[];

/*********************************************************************
   create_tabular_section()
   
   PURPOSE
   Create the tabular section of the product. 
   
   NOTES
   This function has similarities to the get_template_info function.
   In the future these functions may be merged.
   
   The template_format and template_variable structures are 
   independent of the format and variable structures used within
   the template_info structure for use by all the other sections of the
   product.  This approach was used to avoid dragging the rest of 
   template_info into the tabular section generation, which is done 
   in a fundamentally different manner than the other sections.
   
   *******************************************************************/
void create_tabular_section(int			numfps,
			    fp_struct		*fp,
			    int			numgrps,
			    grp_struct		*grp,
			    int                 numcnty,
			    county_struct       *cnty,
			    pcc_struct		*pcc, 
			    misc_struct		*misc,
			    vtecinfo_struct     *vtecinfo,
			    FILE 		*outfile_ptr) 
{
   FILE *file_ptr;
   char *fgets_ptr;
   char	*loc_idPtr;
   char template_file[MAXLEN_FILENAME];
   char *name_read;
   char rawline[MAXLEN_TEMPLATEREC];
   char fileline[MAXLEN_TEMPLATEREC];
   char outputline[MAXLEN_NEWPHRASE];
   int 	record_type, name_found;
   int 	fpindex;
   char msgstr[160];
   char	missing_val_str[MISS_LEN + 1];
   int 	cur_grpindex;
   int 	i, skip_grpline, skip_msgdata, skip_allmsg;
   int	skip_output, add_grpname;
   int 	*fps_written;
   int  linenum;
   int  section_index = TABULAR;
   
   format_struct 	*tabular_format;
   variable_struct 	*tabular_variable;
   spectime_struct 	*tabular_spectime;
   
   
   /* allocate the storage for the local structures */
   
   tabular_format = (format_struct *)malloc(sizeof(format_struct));
   if (tabular_format == NULL) 
      log_msg(FAILED_MALLOC, 
	      "for tabular_format in create_tabular_section");

   tabular_variable = (variable_struct *)malloc(sizeof(variable_struct));
   if (tabular_variable == NULL) 
      log_msg(FAILED_MALLOC, 
	      "for tabular_variable in create_tabular_section");

   tabular_spectime = (spectime_struct *)malloc(sizeof(spectime_struct));
   if (tabular_spectime == NULL)
      log_msg(FAILED_MALLOC, 
	      "for tabular_spectime in create_tabular_section");   

   fps_written = (int *)malloc(sizeof(int) * numfps);
   if (fps_written == NULL)
      log_msg(FAILED_MALLOC, 
	      "for fps_written in create_tabular_section");
   for (i = 0; i < numfps; i++)
      fps_written[i] = FALSE;

   
   /* initialize */
   
   name_found = FALSE;
   tabular_format->num_of_formats     = 0;
   tabular_variable->num_of_variables = 0;
   tabular_spectime->num_of_spectimes = 0;
   cur_grpindex = MISSINGVAL;
   add_grpname  = FALSE;
   skip_grpline = FALSE;
   skip_msgdata = FALSE;
   skip_allmsg = FALSE;
   strcpy(missing_val_str, misc->miss_val);
   linenum = 0;
   
   
   /* set the template file, name, and type */
   
   sprintf(template_file, "%s/%s.%s",
	   paramdir, TABULAR_TEMPLATEFILE, misc->selected_office); 
   
   
   /* open the template file */
   
   file_ptr = fopen(template_file, "r");
   if (file_ptr == NULL) log_msg(FILE_OPENERR, template_file);
   
   
   /* write informational message */
   
   sprintf(msgstr, "Processing template %s from file %s...",
	  pcc->tabular.template, template_file);
   log_msg("", msgstr);

   
   /* loop until the end of the file is reached or until the
      specified template is found and read */
   
   for(;;)
   {
      /* get a line from the input template file;
	 if end-of-file, then exit the loop */
      
      fgets_ptr = fgets(rawline, MAXLEN_TEMPLATEREC, file_ptr);
      if (fgets_ptr == NULL) break;
      
      
      /* read any continuation lines to get them out of the
	 way of subsequent calls to to determine the record type */
      
      get_continuation_lines(rawline, file_ptr, &linenum, fileline);

      
      /* determine the type of record, i.e. whether it be a 
	 template name, condition, or phrase */
      
      determine_template_record_type(fileline, section_index, linenum, 
				     &record_type);      
      
      
      /* process the record depending on its type */
      /* do nothing if a comment record read */
      
      if (record_type == TEMPLATE_COMMENT)
	 ;
      
      /* if non-comment line, then process accordingly */
      
      else
      {
	 /* if the template has not been found, then only considder 
	    template name records; i.e. ignore all other record types;
	    then check if the names match; if they do, then define
	    the record that is expected */
	 
	 if (name_found == FALSE)
	 {
	    if (record_type == TEMPLATE_NAME)
	    {
	       /* get the name of the template, convert to uppercase
		  and check if the names match */
	       
	       name_read = strtok(fileline, " \t\n");
	       if (name_read != NULL)
	       {
	         convert_str_to_upcase(name_read);
	         if (strcmp(name_read, pcc->tabular.template) == 0) 
		  name_found = TRUE;
	       }	  
	    }
	 }	 
	 
	 
	 /* if selected template name has been found, then all records
	    should be for the current template unless a new template
	    definition begins */
	 
	 else if (name_found == TRUE)
	 {
	    
	    /* if the record type is a name, then stop reading
	       because this is the beginning of the next template.
	       it is important that this be the first check in 
	       this if block */
	    
	    if (record_type == TEMPLATE_NAME)
	       break;
	    
	    else if (record_type == TEMPLATE_LITERAL)
	    {	       
	       write_phrase_text(pcc->product.tcase, fileline, outfile_ptr);
	       write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	    }
	    
	    
	    /* if the line defines the format to use, load the info */
	    
	    else if (record_type == TEMPLATE_FORMAT)
	       read_format(fileline, tabular_format);

	    
	    /* if the line defines the variables to use, load the info */
	    
	    else if (record_type == TEMPLATE_VARIABLE)
	       read_varlist(fileline, tabular_variable);
	    
	    
	    /* if the line defines the times for the stage values */
	    
	    else if (record_type == TEMPLATE_SPECTIME)
	       read_spectime(misc->system_time, fileline, tabular_spectime);
	    
	    
	    /* if the line defines forecast group name lines to be added */
	    
	    else if (record_type == TEMPLATE_GRPNAME)
	    {
	       read_grpname(fileline, &skip_grpline, &add_grpname);
	    }
	    
	    
	    /* if the line defines how to handle missing data.  this field
	       serves two purposes; if the string says skip, then that
	       means to not output lines with missing data; if it says
	       something else, then that means to use the specifiec string
	       as the missing data indicator.  */
	    
	    else if (record_type == TEMPLATE_MSGDATA)
	    {
	       read_msgdata(fileline, &skip_msgdata, 
			    &skip_allmsg, missing_val_str);
	    }
	    
	    /* if the line specifies that miscellaneous output
	       is to be generated */
	    
	    else if (record_type == TEMPLATE_MISCWRT)
	    {
	       fpindex   = MISSINGVAL;
	       loc_idPtr = (char *)NULL; 
	       memset(outputline, 0, MAXLEN_NEWPHRASE);
	       
	       load_tabular_values(fp, grp, numcnty, cnty, pcc, misc,vtecinfo,
				   fpindex, loc_idPtr,
				   tabular_format, tabular_variable,
				   tabular_spectime,
				   skip_msgdata, skip_allmsg, missing_val_str,
				   &skip_output, outputline);
	       
	       if (!skip_output)
	       {
		  write_phrase_text(pcc->product.tcase, outputline, outfile_ptr);
		  write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	       }
	       else
		  log_msg(SKIP_TAB_LINE, "in MISCWRT request");
	    }
	    
	    
	    /* if the line is a forecast point id, get the data for it
	       and output the line */
	    
	    else if (record_type == TEMPLATE_FPID)
	    {	       	       
	       /* set the forecast point index */
	       
	       loc_idPtr = strtok(fileline, " \t\n");
	       if (loc_idPtr != NULL)
	       {
	         fpindex   = convert_fpid_to_index(loc_idPtr, numfps, fp);
	         if (fpindex != MISSINGVAL)
	         {
		  /* only load and write the info if the forecast point
		     is to be included in the product */
		  
		   if (misc->fps_included[fpindex] == TRUE )
		   {
		     /* if the grp name record was given, then add
			a line with the group name for the first point 
			in every forecast group */
		     
		     if (add_grpname == TRUE)
		     {
			i = convert_grpid_to_index(fp[fpindex].grpid,
						   numgrps, grp);
			if (cur_grpindex != i)
			{
			   if (skip_grpline == TRUE) 
			      write_phrase_text(pcc->product.tcase, "\n",
						outfile_ptr);
			   cur_grpindex = i;
			   
			   
			   /* use copy of string because it may get
			      converted to upper case */
			   
			   strcpy(outputline, grp[i].name);
			   write_phrase_text(pcc->product.tcase, outputline, 
					     outfile_ptr);
			   write_phrase_text(pcc->product.tcase, "\n", 
					     outfile_ptr);
			}
		     }
		     
		     
		     /* load in the values for the forecast point and
			output the line */
		     
		     memset(outputline, 0, MAXLEN_NEWPHRASE);
		     load_tabular_values(fp, grp, numcnty, cnty, pcc, 
		                         misc, vtecinfo, fpindex, loc_idPtr,
					 tabular_format, tabular_variable,
					 tabular_spectime,
					 skip_msgdata, skip_allmsg, missing_val_str,
					 &skip_output, outputline);
		     
		     if (!skip_output)
		     {
			write_phrase_text(pcc->product.tcase, outputline, 
					  outfile_ptr);
			write_phrase_text(pcc->product.tcase, "\n", 
					  outfile_ptr);
		     }
		     else
			log_msg(SKIP_TAB_LINE, "in FP_ID request");
		     
		     fps_written[fpindex] = TRUE;
		  }
	        }
	      } 
	    }

	    
	    /* if the line is a location id, get the data for it
	       and output the line. this is a generalized version
	       of the fp_id record. */
	    
	    else if (record_type == TEMPLATE_LOCID)
	    {	       	       
	       /* set the forecast point index */
	       
	       loc_idPtr = strtok(fileline, " \t\n");
	       if (loc_idPtr != NULL)
	       {
		  fpindex = MISSINGVAL;
		  
		  /* load in the values for the forecast point and
		     output the line */
		  
		  memset(outputline, 0, MAXLEN_NEWPHRASE);
		  load_tabular_values(fp, grp, numcnty, cnty, pcc, misc,
				      vtecinfo, fpindex, loc_idPtr,
				      tabular_format, tabular_variable,
				      tabular_spectime,
				      skip_msgdata, skip_allmsg, missing_val_str,
				      &skip_output, outputline);
		  
		  if (!skip_output)
		  {
		     write_phrase_text(pcc->product.tcase, outputline, 
				       outfile_ptr);
		     write_phrase_text(pcc->product.tcase, "\n", 
				       outfile_ptr);
		  }
		     else
			log_msg(SKIP_TAB_LINE, "in LOCID request");
	       }
	       
	       else
		  log_msg(UNDEFINED_LID, fileline);
	    }
	    
	    
	    /* if unexpected template record, issue message and abort */
	    
	    else
	       log_msg(INVALID_RECORD_TYPE, fileline);
	    
	 } /* end of if block for name found case */
      }    /* end of if block for non-comment case*/
   }       /* end of for loop on reads */
   
   
   
   /* if never found the template name, issue error message and abort */
   
   if (name_found == FALSE)
      log_msg(TEMPLATENAME_NOTFOUND, pcc->tabular.template);
   
   
   /* close the file */
   
   fclose(file_ptr);
   
   
   /* write any fp info that should have been written but were not
      because the id was not in the tabular template */

   write_leftover_fps(numfps, fp, numgrps, grp, numcnty, cnty, pcc,misc,vtecinfo,
		      tabular_format, tabular_variable, tabular_spectime,
		      add_grpname, skip_grpline, skip_msgdata, skip_allmsg, missing_val_str,
		      fps_written, outfile_ptr);
  
   
   /* automatically insert a trailing blank line */
   
   write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);

   
   /* log last set of format and variable buffers for diagnostic use */
   /*!!!
   log_template_buffers(tabular_format, tabular_variable, tabular_spectime);
   */
   
   
   /* free the memory for the local structures */
   
   free(fps_written);
   free(tabular_format);
   free(tabular_variable);
   free(tabular_spectime);
      
   return;
}


/*********************************************************************
   load_tabular_values()
   
   PURPOSE
   Loads the values of the variables specified in the variable buffer
   using the format specified in the format buffer for use with
   the tabular section.
   
   Pass in the index to any forecast point, and to accomodate the
   data retrievals for non-locations, pass in the location id also.
      
   NOTES
   This function is similar to load_phrase_data().
   
   *******************************************************************/
void load_tabular_values(fp_struct		*fp, 
			 grp_struct		*grp,
			 int                    numcnty,
			 county_struct          *cnty,
			 pcc_struct		*pcc, 
			 misc_struct		*misc,
			 vtecinfo_struct        *vtecinfo,
			 int			fpindex,
			 char			*lid,
			 format_struct		*tabular_format,
			 variable_struct	*tabular_variable,
			 spectime_struct	*tabular_spectime,
			 int			skip_msgdata,
			 int                    skip_allmsg,
			 char			missing_val_str[],
			 int			*skip_output,
			 char 			outputline[])
{
   extern template_variable_struct TEMPLATE_VARIABLES_TABLE[];
   
   values 	rawvalue;
   int 		i, j, newpos;
   int 		varlist_index;
   char 	valuestr[MAXLEN_NEWPHRASE];
   int 		valuelen, varindex;
   char 	longstring[MAXLEN_LONGSTR];
   char		dqcode[SHEF_QC_LEN + 1];
   int 		stagevar_cnt;
   int 		pad_switch = TRUE;
   int		format_index;
   char		local_lid[LOC_ID_LEN + 1];
   int		missing_val_cnt;
   int		width;
   int		filter_qc = 1;
   int          var_cnt;
   
   /* initialize, note that the output string is initialized
      in the calling function */
   
   varlist_index   = 0;
   newpos          = 0;
   stagevar_cnt    = 0;
   missing_val_cnt = 0;
   var_cnt = 0;
   memset(longstring, 0, 1);
   
   
   /* loop on the format specified */
   
   for (i = 0; i < tabular_format->num_of_formats; i++)
   {
      /* if the format specifies a variable then get the value */
      
      if (isvar(tabular_format->type[i]) == TRUE)
      {	 
	 /* if a variable format is specified, first check that 
	    there is a variable defined for this format */
	 
	 if (varlist_index >= tabular_variable->num_of_variables)
	    log_msg(TOOFEW_VARS, pcc->tabular.template);
	 
	 
	 /* process the variable */
	 
	 else
	 {	
	    /* load a convenient value */
	    
	    varindex = tabular_variable->varinfo[varlist_index].varindex;
	    
	    
	    /* if this is a stage value that is dependent upon a specified
	       stage time, then determine which stage time goes with the
	       stage value. the approach used is that the first encounter
	       with a stage value (that uses stage times) uses the first
	       stage time specified, the second uses the second, etc. */
	    
	    if ((strcmp(TEMPLATE_VARIABLES_TABLE[varindex].name,
			"<SpecObsStg>")      == 0) ||
		(strcmp(TEMPLATE_VARIABLES_TABLE[varindex].name, 
			"<SpecFcstStg>")     == 0) ||
		(strcmp(TEMPLATE_VARIABLES_TABLE[varindex].name,
			"<SpecObsStgTime>")  == 0) ||
		(strcmp(TEMPLATE_VARIABLES_TABLE[varindex].name, 
			"<SpecFcstStgTime>") == 0))  stagevar_cnt++;
	    
	    
	    /* check that the variable is permitted for the
	       tabular section */
	    
	    if (check_var_access(TEMPLATE_VARIABLES_TABLE[varindex].accesslist,
				 TABULAR) != TRUE)
	    {
	       log_msg(VAR_NOT_ALLOWED,
		       TEMPLATE_VARIABLES_TABLE[varindex].name);
	       break;
	    }
	    
	    
	    /* get the variable value and load it into the rawvalue union,
	       whether it be for a forecast point or a data location. */
	    
	    if (fpindex == MISSINGVAL)
	    {
	       if (lid == NULL)
		  memset(local_lid, 0, LOC_ID_LEN + 1);
	       else if (strlen(lid) == 0)
		  memset(local_lid, 0, LOC_ID_LEN + 1);
	       else
		  strcpy(local_lid, lid);
	    }
	    else
	       strcpy(local_lid, fp[fpindex].id);
	    
	    
	    if (strcmp(TEMPLATE_VARIABLES_TABLE[varindex].name,
		       "<PEVal>") == 0 ||
		strcmp(TEMPLATE_VARIABLES_TABLE[varindex].name,
		       "<PETime>") == 0)
	    {	
	       var_cnt = var_cnt + 1;       
	       load_pe_value(filter_qc, local_lid,
			     tabular_variable->varinfo[varlist_index], 
			     &rawvalue, dqcode, longstring);
	    }
	    
	    else
	    {
	       var_cnt = var_cnt + 1;
	       load_variable_value(fp, grp, numcnty, cnty, misc, vtecinfo,pcc,
				   &(tabular_variable->varinfo[varlist_index]), 
				   fpindex, local_lid, 
				   tabular_spectime, stagevar_cnt - 1, 
				   &rawvalue, dqcode, longstring);
	    }	    
            
	    /* if the value is not a "long" string, such as the reach
	       variable, then format in the normal manner */
	    
	    if (strlen(longstring) == 0)
	    {
	       
	       /* keep track of the number of values that are missing
		  for possible use later. */
	       
	       if (rawvalue.f == (float  )MISSINGVAL || 
		   rawvalue.i == (int    )MISSINGVAL ||
		   rawvalue.t == (time_t )MISSINGVAL)
		  missing_val_cnt++;
	       
	       
	       /* format the string using either the default format or
		  the format embodied in the format structure */	      
	      
	       format_index = i;
	       format_vals(rawvalue, dqcode, varindex,
			   tabular_format, format_index,  misc,
			   pad_switch, missing_val_str,
			   valuestr, &valuelen, pcc, local_lid);
	    }
	    
	    
	    /* if the value is a longstring, then truncate or pad
	       accordingly */
	    
	    else
	    {
	       valuelen = strlen(longstring);
	       
	       format_index = i;
	       
	       /* longstrings may be used for string variables or
		  possibly for float variables.  when used for float variables,
		  the format given in the template is still for a float, even
		  though the value may be output as a string.  the float
		  format is ignored for the most part except for the 
		  padding/truncating issue.  the width of the float format
		  is stored differently than the width of the string format.
		  this needs to be considered when getting the
		  width. 2-25-2000 */
	       
	       if (tabular_format->type_size[format_index] == RPF_INT)
		  width = (int )tabular_format->size[format_index].i;
	       else
		  width = (int )tabular_format->size[format_index].f;
	       
	       if (valuelen < width)
	       {
		  strcpy(valuestr, longstring);
		  for (j = valuelen; j < width; j++)
		     valuestr[j] = ' ';
		  valuestr[width] = '\0';
	       }      
	       
	       else if (valuelen > width)
	       {
		  strncpy(valuestr, longstring, width);
		  valuestr[width] = '\0';
	       }      
	       
	       else
		  strcpy(valuestr, longstring);
	       
	       valuelen = strlen(valuestr);
	    }
	    
	    
	    /* copy the value string and increment the length */
	    
	    strncpy(outputline + newpos, valuestr, valuelen);
	    newpos = newpos + valuelen;
	    
	    /* increment the variable format index */
	    
	    varlist_index++;	    
	 }
      }
      
      
      /* if a string literal is specified */
      
      else if (tabular_format->type[i] == RPF_STR)
      {
	 /* find the length of the literal, which is stored in the 
	    misnamed size field union. then convert it to uppercase
	    if specified. */
	 
	 valuelen = strlen(tabular_format->size[i].s);
	 
	 
	 /* copy the value string and increment the length */
	 
	 strncpy(outputline + newpos, tabular_format->size[i].s,
		 valuelen);
	 newpos = newpos + valuelen;
      }
      
      
      /* if a blank space string is specified. a zero length blank
	 space string implies a forced-newline so insert the special
	 foreced-newline sequence, which is then interpreted when writing
	 the text */
      
      else if (tabular_format->type[i] == RPF_SPACE)
      {
	 if (tabular_format->size[i].i == 0)
	 {
	    strncpy(outputline + newpos, FORCED_NL_CHARS, FORCED_NL_NUMCHARS);
	    newpos = newpos + FORCED_NL_NUMCHARS;
	 }
	 else
	 {
	    for (j = 0; j < tabular_format->size[i].i; j++)
	    {
	       strncpy(outputline + newpos, " ", 1);
	       newpos = newpos + 1;
	    }
	 }
      }
   }  /* end of loop on the number of formats */ 
   
   
   /* check if new string length can fit into space reserved
      for old string; if not then issue message and abort */
   
   if (newpos > MAXLEN_NEWPHRASE) log_msg(EXCEED_NEWPHRASE, "");
   
   
   /* check if the string is too long for the tabular output */
   
   if (strlen(outputline) > MAXLEN_STRING)
   { 
      outputline[MAXLEN_STRING] = '\0';
      log_msg(EXCEED_LINELIMIT, outputline);
   }
   
   
   /* check if the line should be skipped because of missing data */
   
   if (skip_msgdata && missing_val_cnt > 0)
      *skip_output = TRUE;
   else if (skip_allmsg && missing_val_cnt > 0 &&
          (missing_val_cnt == var_cnt))
      *skip_output = TRUE;	     
   else 
      *skip_output = FALSE;
   
   return;
}


/*********************************************************************
   write_leftover_fps()
   
   PURPOSE
   Loops on all the forecast points and checks that those forecast
   points that should be included were indeed included in the product.
   If not, then the forecast point is automatically inserted at the end 
   of the tabular section, using the format, variables, and spectimes,
   that were in effect at the end of the template.  This ensures that
   all included forecast points are in the product, regardless of 
   whether they are in the template.  In an extreme case, no forecast
   points need be specified in the tabular template, although they
   would all share the default template format, variables, and
   spectimes, and would be in a fixed grp-fp order.  
   
   *******************************************************************/

void write_leftover_fps(int			numfps,
			fp_struct		*fp,
			int			numgrps,
			grp_struct		*grp,
			int                     numcnty,
			county_struct           *cnty,
			pcc_struct		*pcc, 
			misc_struct		*misc,
			vtecinfo_struct         *vtecinfo,
			format_struct		*tabular_format,
			variable_struct	 	*tabular_variable,
			spectime_struct		*tabular_spectime,
			int 			add_grpname,
			int			skip_grpline,
			int			skip_msgdata,
			int                     skip_allmsg,
			char			missing_val_str[],
			int			*fps_written,
			FILE			*outfile_ptr)
{
   int 	i, k;
   int 	fpindex;
   int 	cur_grpindex;
   char	outputline[MAXLEN_NEWPHRASE];
   int 	*fporder;
   char	*loc_idPtr;
   int	skip_output;
   
   
   /* order the points in the manner specified */
   
   fporder = (int *)malloc(sizeof(int) * numfps);
   if (fporder == NULL) 
      log_msg(FAILED_MALLOC, "of fporder in writeeleftover_fps_");   
   order_fps(numfps, fp, numgrps, grp, vtecinfo, pcc->product.grpfp_order, fporder);
   
   cur_grpindex = MISSINGVAL;
   
   
   /* loop on the forecast points within the groups */   
   
   for (i = 0; i < numfps; i++)
   {
      fpindex = fporder[i];
      
      if (fpindex == MISSINGVAL)
         break;
	 
      /* if the forecast point should be included in the
	 tabular section but wasn't then include it */
      
      if (misc->fps_included[fpindex] == TRUE &&
	  fps_written[fpindex] == FALSE)
      {
	 /* if the grp name record was given, then add
	    a line with the group name for the first point 
	    in every forecast group */
	 
	 if (add_grpname == TRUE)
	 {
	    k = convert_grpid_to_index(fp[fpindex].grpid,
				       numgrps, grp);
	    if (cur_grpindex != k)
	    {
	       if (skip_grpline == TRUE) 
		  write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	       
	       cur_grpindex = k;
	       strcpy(outputline, grp[k].name);
	       write_phrase_text(pcc->product.tcase, outputline, outfile_ptr);
	       write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	    }
	 }
	 
	 
	 /* load in the values for the forecast point and
	    output the line */
	 
	 log_msg(MISSING_TABULAR_FP, fp[fpindex].id);
	 
	 loc_idPtr = (char *)NULL;
	 memset(outputline, 0, MAXLEN_NEWPHRASE);
	 load_tabular_values(fp, grp, numcnty, cnty, pcc, misc,
			     vtecinfo, fpindex, loc_idPtr,
			     tabular_format, tabular_variable,
			     tabular_spectime, skip_msgdata,skip_allmsg,
			      missing_val_str,
			     &skip_output, outputline);
	 
	 if (!skip_output)
	 {
	    write_phrase_text(pcc->product.tcase, outputline, outfile_ptr);
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	 }
	 else
	    log_msg(SKIP_TAB_LINE, "for leftover fcst pts");
	 
      }
   }
   
   free(fporder);
   
   return;
}
