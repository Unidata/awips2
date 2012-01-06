 /************************************************************************
   buf_template.c

   buf_template()
   read_condition()
   read_phrase()
   read_format()
   
   grab_format_item()
   
   read_varlist()
   read_spectime()
   read_grpname()
   read_msgdata()
   
   get_num_items()
   verify_condition_syntax()
   
   get_item_info()
   check_if_token()
   check_if_number()
   check_if_string()
   check_if_missval()
   check_if_boolean()
   check_if_variable()
   check_var_access()
   check_if_metric()
   **********************************************************************/

#include <string.h>                  /* string library functions */
#include <stdio.h>                   /* standard io library functions */
#include <stdlib.h>                  /* standard library functions */
#include <ctype.h>                   /* character type library functions */
#include <time.h>                    /* time library functions */

#include "buf_template.h"       /* function protos */

/*********************************************************************
   buf_template()
   
   PURPOSE
   This function opens the template file and finds the specified template.
   It then calls the routines for loading the info into the structure.
   
   NOTES
   This function is not used for loading of the tabular section template;
   it is used for all other product templates.  The variable and format
   records in the template file provide information that is stored in
   structure within the template_info structure, so that template_info
   has all it needs regarding the template specifications.

   Note that the FORMAT, VARIABLE, and STGTIME records can be located
   throughout the template, but only the last occurrence is used
   for any portion of the template. The CONDITION and PHRASE records
   must be paired always (assuming a CONDITION is accepted).

   ********************************************************************/

void buf_template(const char 			template_file[],
		  const char 			template_name[],
		  const int			template_type,
		  const time_t			system_time,
		       	template_info_struct	*template_info)

{
   FILE *file_ptr;
   char fileline[MAXLEN_TEMPLATEREC];
   char contline[MAXLEN_TEMPLATEREC];
   char *name_read;
   char *fgets_ptr;
   int 	record_type, record_expected;
   int 	name_found;
   char msgstr[MAXLEN_STRING + MAXLEN_STRING];
   int section_index = MISSINGVAL;
   int linenum;
   
   
   /* need to free the memory allocated for the stacks and the
      phrases in the previously loaded template */
   
   free_template_memory(template_info);
   
   
   /* output a status message */
   
   sprintf(msgstr, "Processing template %s from file %s...",
	   template_name, template_file);
   log_msg("", msgstr);
   
   
   /* open the template file */
   
   file_ptr = fopen(template_file, "r");
   if (file_ptr == NULL) log_msg(FILE_OPENERR, template_file);
     
   
   /* initialize */
   
   record_expected = TEMPLATE_NAME;
   name_found = FALSE; 
   linenum = 0;

   template_info->format.num_of_formats = 0;
   template_info->variable.num_of_variables = 0;
   template_info->spectime.num_of_spectimes = 0;
   
   
   /* loop until the end of the file or until the
      specified template is found and read */
   
   for(;;)
   {
      /* get a line from the input template  file;
	 if end-of-file, then exit the loop; also check that the length
	 is not exceeded and read any continuation lines that may follow */
      
      fgets_ptr = fgets(fileline, MAXLEN_TEMPLATEREC, file_ptr);
      if (fgets_ptr == NULL) break;
      
      linenum++;
      get_continuation_lines(fileline, file_ptr, &linenum, contline);				
      
      
      /* determine the type of record, i.e. whether it be a 
	 template name, condition, or phrase */
      
      determine_template_record_type(contline, section_index, linenum,
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
	       /* get the name of the template; note that the conditional
		  and phrase lines are NOT converted to uppercase */
	       
	       name_read = strtok(contline, " :\n");
	       if (name_read != NULL)
	       {
	          convert_str_to_upcase(name_read);
	       
	          if (strcmp(name_read, template_name) == 0)
	          {
		    strcpy(template_info->name, name_read);
		    name_found = TRUE;
		    if (template_type == PHRASE_WITH_CONDITION)
		       record_expected = TEMPLATE_CONDITION;
		    else
		       record_expected = TEMPLATE_PHRASE;
		  }     
	       }
	    }	    
	 }	 
	 
	 
	 /* if selected template name has been found, then all records
	    should be for the current template unless a new template
	    definition begins */
	 
	 else if (name_found == TRUE)
	 {
	    
	    /* if the record type is a name, then stop reading;
	       it is important that this be the first check in 
	       this if block */
	    
	    if (record_type == TEMPLATE_NAME)
	       break;
	    
	    
	    /* if the record type is a format line for the variable format;
	       this record should be coupled with a variables list */
	    
	    else if (record_type == TEMPLATE_FORMAT)
	       read_format(contline, &template_info->format);
	    
	    
	    /* if the record type is a values line that identifies
	       which values go with which formats */
	    
	    else if (record_type == TEMPLATE_VARIABLE)
	       read_varlist(contline, &template_info->variable);
	    
	    else if (record_type == TEMPLATE_SPECTIME)
	       read_spectime(system_time, 
			     contline, &template_info->spectime);
	    
	    
	    /* if a conditional expression is expected and
	       that is what has been read, process it, otherwise
	       log an error message */
	    
	    else if (record_expected == TEMPLATE_CONDITION)
	    {
	       if (record_type == TEMPLATE_CONDITION)
	       {
		  /* read the logic in the conditional expressions and load
		     it into the stack structure.  verify the syntax of the
		     conditional expressions.  now that the condition is read,
		     the template phrase is expected next */
		  
		  read_condition(contline, file_ptr, template_info);
		  verify_condition_syntax(template_info);
		  record_expected = TEMPLATE_PHRASE;
	       }
	       
	       
	       /* if expected a condition and didn't get one, 
		  log message and keep on looking */
	       
	       else		  
		  log_msg(TEMPLATECONDITION_NOTFOUND, template_name);
	    }
	    
	    
	    /* if a phrase or bullet phrase is expected and that is what has
	       been read, then process it, otherwise log an error message */
	    
	    else if (record_expected == TEMPLATE_PHRASE)
	    {	      			       		    	  
	       if (record_type == TEMPLATE_PHRASE ||
	           record_type == TEMPLATE_BULLETSTR ||
		   record_type == TEMPLATE_INDENTSTR)	        
	       {
		  /* read the phrase and determine the next record type
		     expected based on the template type; if no conditions,
		     then define the phrase as being included always. */
		  
		  read_phrase(contline, file_ptr, template_info, record_type);
		  
		  if (template_type == PHRASE_WITH_CONDITION)
		     record_expected = TEMPLATE_CONDITION;
		  
		  else
		  {
		     record_expected = TEMPLATE_PHRASE;
		     template_info->
			include_phrase[template_info->num_phrases - 1] = TRUE;
		  }
	       }
	       
	       
	       /* if expected a phrase and didn't get one, 
		  log message and keep on looking */
	       
	       else
		  log_msg(TEMPLATEPHRASE_NOTFOUND, template_name);
	    }
	    
	    
	    /* if tabular record type then issue warning message since
	       this function is for non-tabular templates */
	    
	    else
	       log_msg(TABREC_IN_NONTAB, "");
	    
	 } /* end of if block for name found case */	 
      }    /* end of if block for non-comment case*/
   }       /* end of for loop for reading file */
   
   
   /* if never found the template name, issue error message;
      if expected a conditional phrase when the template body ended,
      and no phrases found at all, then issue error message.
      for these two cases, there is no need to take any action,
      since the number of phrases and conditions will be 0 */
   
   if (name_found == FALSE)
      log_msg(TEMPLATENAME_NOTFOUND, template_name);
      
   if (record_expected == TEMPLATE_CONDITION &&
       template_info->num_phrases == 0)
      log_msg (TEMPLATECONDITION_NOTFOUND, template_name);
   
   
   /* if expected a phrase when the template body ended, and the 
      phrase is mandatory since its associated condition was 
      already read, issue error message. Since this situation
      may result in memory already having been allocated, free
      the memory; this results in a template that will 
      produce no product output */
   
   if (record_expected == TEMPLATE_PHRASE &&
       template_type == PHRASE_WITH_CONDITION)
   {
      log_msg(TEMPLATEPHRASE_NOTFOUND, template_name);
      free_template_memory(template_info);
   }
   
   
   /* if the number of formats specified is not equal to the 
      number of variables, issue a message */
   
   if (template_info->format.num_of_formats != 
       template_info->variable.num_of_variables)
      log_msg(FORM_VAR_MISMATCH, template_name);

   
   /* close the file */
   
   fclose(file_ptr);

   
   /* write the template info to a file for diagnostic use */
   /*!!!
   log_template_info(template_info);
   */
   
   return;
}


/*********************************************************************
   read_condition()
   
   PURPOSE
   This function gets the logic portion of the template and loads
   it into the structure containing the information on the
   conditional expression.
   
   ********************************************************************/
void read_condition(char			*conditionstr,
		    FILE			*file_ptr,
		    template_info_struct	*template_info)

{
   extern template_variable_struct TEMPLATE_VARIABLES_TABLE[];

   template_item_struct *template_items = NULL;
   char 		*item;
   char                 localitem[MAXLEN_TEMPLATEREC]= "";
   char			cpconditionstr[MAXLEN_TEMPLATEREC];
   char                 *itemposition;
   int 			slot, num_items;
   itemtypes 		type;
   values 		value;
   int 			error_flag;
   varinfo_struct	varinfo;

   
   /* check that the number of allowable phrases in the 
      template hsa not been exceeded; can do this check for
      conditions because the phrases and conditions are paired */
   
   if (template_info->num_phrases >= MAX_NUMPHRASES)
   {
      log_msg(EXCEED_MAXNUM_PHRASES, template_info->name);
      return;
   }
   
   else
      error_flag = FALSE;
   
  
   /* get the number of items for use in sizing the stack */
   
   num_items = get_num_items(conditionstr);  
   
  
   /* allocate room in the stack for this conditional expression;
      this allocates the full amount of memory needed with a
      starting address pointed at by template_tokens, which is then
      treated as an array for pushing and popping;
      if unsuccessful, then log error message */
   
   template_items = (template_item_struct *)
   malloc(num_items*sizeof(template_item_struct));
   if (template_items == NULL) 
      log_msg(FAILED_MALLOC, "for template_items in read_condition");


   /* get get the first token in the line, then loop on the tokens */
   
   if (conditionstr != NULL)
      strcpy(cpconditionstr, conditionstr);
      
   item = strtok(conditionstr, " \t\n");

   if (item == NULL)
   {
      log_msg(INVALID_CONDITION_STR, conditionstr);
      return;
   }
   
   else
   {	 
     for (slot = 0; slot < num_items; slot++) 
     {
      /* determine the type of the item, and if applicable, its
	 constant value or variable name */     

         get_item_info(item, &type, &value, &varinfo);

         /* if the item is a variable, check that it is one that is
	    allowed to be in the condition stack */
	 
         if (isvar(type) == TRUE && 
	     TEMPLATE_VARIABLES_TABLE[varinfo.varindex].access_condition == FALSE)
         {
	    log_msg(INVALID_VAR_IN_CONDITION,
		    TEMPLATE_VARIABLES_TABLE[varinfo.varindex].name);
	    error_flag = TRUE;
         }
	 
	 
         /* load the information into the stack structure and 
	    get the next item */
	 
         expr_pushstack(type, value, varinfo, slot, template_items);
	 
         /* get the next token */
	 
	 itemposition = strstr(cpconditionstr, item);
	 if (itemposition != NULL)
	 {
	    strncpy(localitem,itemposition + strlen(item),
		    (strlen(cpconditionstr) - strlen(item)));
	    strcpy(cpconditionstr, localitem);		 
	    item = strtok(localitem, " \t\n");	    
         } 
     }
   }
   
   /* if error occurred, then clear out the condition stack by 
      setting it to contain one single entry of False. */
   
   if (error_flag == TRUE)
   {
      slot    = 0;
      type    = RPF_INT;
      value.i = FALSE;
      expr_pushstack(type, value, varinfo, slot, template_items);
      num_items = 1;

   } 
   
   
   /* increment the number of conditions in the template after 
      loading the address of the bottom of the stack into the
      template_info structure and the size of the stack pointer */
   
   template_info->bottom_condition_stack[template_info->num_conditions] = 
      template_items;
   template_info->stacksize[template_info->num_conditions] = num_items;
   ++template_info->num_conditions;
   
 
   return;
}


/*********************************************************************
   read_phrase()
   
   PURPOSE
   This function gets the phrase portion of the template and loads in
   into the structure.
   
   ********************************************************************/
void read_phrase(char 			phrasestr[],
		 FILE			*file_ptr,
		 template_info_struct	*template_info,
		 int                    record_type)
{
   int phrasenum;
   int phraselen;
   
   
   /* check that the number of allowable phrases in the 
      template hsa not been exceeded */
   
   if (template_info->num_phrases >= MAX_NUMPHRASES)
   {
      log_msg(EXCEED_MAXNUM_PHRASES, template_info->name);
      return;
   }
     
      
   /* load in the current phrase number for convenience sake 
      and then increment the number of phrases */
   
   phrasenum = (template_info->num_phrases++);
    
   
   /* allocate memory storage for the phrase and assign it in
      the template info structure; if an error occurred, then
      log error message */
   
   phraselen = strlen(phrasestr) + 1;
   template_info->phrase[phrasenum] = (char *)malloc(phraselen);   
   if (template_info->phrase[phrasenum] == NULL)
      log_msg(FAILED_MALLOC, "for phrase in read_phrase");
   
   
   /* load the phrase into the allocated memory */
   
   strcpy(template_info->phrase[phrasenum], phrasestr);
   template_info->phraselen[phrasenum] = phraselen;
   
   
   /* load bulletstr_flag */
   
   if (record_type == TEMPLATE_BULLETSTR )
      template_info->bulletstr_flag[phrasenum] = TRUE;
   else
      template_info->bulletstr_flag[phrasenum] = FALSE;
      
   /* load indentstr_flag */
   
   if (record_type == TEMPLATE_INDENTSTR )
      template_info->indentstr_flag[phrasenum] = TRUE;
   else
      template_info->indentstr_flag[phrasenum] = FALSE;      
      
   return;
}


/*********************************************************************
   read_format()
   
   PURPOSE
   Loads in the format specified for the format buffer of the
   template.
   
   NOTES
   For the tabular section, the format info is used explicitly to determine
   how to format a line for the variables given in the variable info.
   
   For the other sections, the format info is used as a "library" of
   format definitions for variables only for use when those variables
   are later requested in a phrase.
   
   *******************************************************************/
void read_format(char		*fileline, 
		 format_struct	*format)
{
   extern time_format_struct TIME_FORMAT_LIST[];
   extern int NUM_OF_TIME_FORMATS;

   char 	format_item[MAXLEN_STRING];
   char 	format_literal[MAXLEN_STRING];
   char 	format_type[2];
   char 	format_size[MAXLEN_STRING];
   int 		cnt, i, j, numread;
   int 		width, remainder, literal_len;
   itemtypes	type_size;
   values 	value_size;
   int 		error_flag;
   
   
   /* initialize */
   
   format->num_of_formats = 0;
   cnt = 0;
   
   
   /* grab the first format item in the list; if the format is a quoted 
      string the format is returned in format_literal, if it is a 
      conventional format, it is returned in format_item */ 
   
   memset(format_item, '\0', MAXLEN_STRING);
   memset(format_literal, '\0', MAXLEN_STRING);
   grab_format_item(fileline, format_item, format_literal);
   
   
   /* loop on the number of format items found in the line */
   
   while ((strlen(format_item) > 0 || strlen(format_literal) > 0) &&
	  cnt < MAX_FORMAT_ITEMS)
   {      
      /* initialize */
      
      format->type[cnt] = MISSINGVAL;
      type_size    = MISSINGVAL;
      value_size.i = 0;
      error_flag   = FALSE;
      
      
      /* extract the format type and size if the format is not
	 for a quoted string */
      
      if (strlen(format_item) > 0)
      {
	 numread = sscanf(format_item, "%1s%s ", format_type, format_size);
	 if (numread != 2)
	 {
	    log_msg (INVALID_FORMAT, format_item);
	    error_flag = TRUE;
	 }
      }
      else
	 strcpy(format_type, "\"");
      
      
      /* process if a blank space format */
      
      if (strcmp(format_type, "X") == 0)
      {
	 check_if_number(format_size, &type_size, &value_size);	    
	 if (type_size == RPF_INT)
	 {
	    if (value_size.i < 0 || value_size.i >= 80)
	    {
	       log_msg(INVALID_X_FORMAT, format_item);
	       error_flag = TRUE;
	    }
	    else
	    {
	       format->type[cnt]      = RPF_SPACE;
	       format->type_size[cnt] = RPF_INT;
	       format->size[cnt].i    = value_size.i;
	    }
	 }
	 else
	 {
	    log_msg(INVALID_X_FORMAT, format_item);
	    error_flag = TRUE;
	 }
      }
      
      
      /* process if an integer format */
      
      else if (strcmp(format_type, "I") == 0)
      {
	 check_if_number(format_size, &type_size, &value_size);
	 if (type_size == RPF_INT)
	 {
	    if (value_size.i <= 0 || value_size.i >= 15)
	    {
	       log_msg(INVALID_I_FORMAT, format_item);
	       error_flag = TRUE;
	    }
	    else
	    {
	       format->type[cnt]      = VAR_INT;
	       format->type_size[cnt] = RPF_INT;
	       format->size[cnt].i    = value_size.i;
	    }
	 }
	 
	 
	 /* allow for type of I2.2 which is a float size,
	    use the modulo function tricks to check basic validity */
	 
	 else if (type_size == RPF_FLT)
	 {
	    width = (int )value_size.f;
	    if (value_size.f > 0)
	       remainder = (int )(value_size.f * 10) % 10;
	    else
	       remainder = MISSINGVAL;
	    
	    if (width != remainder || width <= 0 || width > 12)
	    {
	       log_msg(INVALID_I_FORMAT, format_item);
	       error_flag = TRUE;
	    }
	    else
	    {
	       format->type[cnt]      = VAR_INT;
	       format->type_size[cnt] = RPF_FLT;
	       format->size[cnt].f    = value_size.f;
	    }
	 }
	 
	 else
	 {
	    log_msg(INVALID_I_FORMAT, format_item);
	    error_flag = TRUE;
	 }
      }
      
      
      /* process if a float format */
      
      else if (strcmp(format_type, "F") == 0)
      {
	 check_if_number(format_size, &type_size, &value_size);
	 if (type_size == RPF_FLT)
	 {
	    width = (int )value_size.f;
	    if (value_size.f > 0)
	       remainder = (int )(value_size.f * 10) % 10;
	    else
	       remainder = MISSINGVAL;
	    
	    if (remainder > 5 || remainder < 0 ||
	       (width - remainder) <= 0 ||
		width <= 0 || width > 12)
	    {
	       log_msg(INVALID_F_FORMAT, format_item);
	       error_flag = TRUE;
	    }
	    else
	    {
	       format->type[cnt]      = VAR_FLT;
	       format->type_size[cnt] = RPF_FLT;
	       format->size[cnt].f    = value_size.f;
	    }
	 }
	 
	 else
	 {
	    log_msg(INVALID_F_FORMAT, format_item);
	    error_flag = TRUE;
	 }
      }
      
      
      /* process if a string format */
      
      else if (strcmp(format_type, "S") == 0)
      {
	 check_if_number(format_size, &type_size, &value_size);
	 if (type_size == RPF_INT)
	 {
	    if (value_size.i <= 0 || value_size.i >= 80)
	    {
	       log_msg(INVALID_S_FORMAT, format_item);
	       error_flag = TRUE;
	    }
	    else
	    {
	       format->type[cnt]      = VAR_STR;
	       format->type_size[cnt] = RPF_INT;
	       format->size[cnt].i    = value_size.i;
	    }
	 }
	 else
	 {
	    log_msg(INVALID_S_FORMAT, format_item);
	    error_flag = TRUE;
	 }
      }
      
      
      /* process if a time format, use the size variable to
	 store the index to the selected format even though the
	 variable name is not really descriptive of its use 
	 in this case */
      
      else if (strcmp(format_type, "T") == 0)
      {
	 /* loop on the list of allowable formats for the time format */
	 
	 format->size[cnt].i = MISSINGVAL;
	 
	 for (i = 0; i < NUM_OF_TIME_FORMATS; i++)
	 {	    
	    if (strcmp(format_item, TIME_FORMAT_LIST[i].name) == 0)
	       format->size[cnt].i = TIME_FORMAT_LIST[i].index;
	 }
		
	 
	 /* if the format does not match one of the provided fixed-type
	    formats, check if it is a user defined format, as noted
	    by having the first three characters match with T_U(ser) */
	 
	 if (format->size[cnt].i == MISSINGVAL)
	 {
	    if (strncmp(format_item, "T_U", 3) == 0)
	    {
	       width = strlen(format_item);
	       if (width > 3 && width < 20)
	       {
		  /* loop on format and replace any ^
		     characters with spaces */
		  
		  for (j = 3; j < width; j++)
		  {
		     if (format_item[j] == '^') format_item[j] = ' ';
		  }
		  
		  strcpy(format->usertime[cnt], &format_item[3]);
		  format->size[cnt].i = T_USER;		  
	       }
	    }
	 }
	 
	 if (format->size[cnt].i == MISSINGVAL)
	 {
	    log_msg(INVALID_T_FORMAT, format_item);
	    error_flag = TRUE;
	 }
	 else
	    format->type[cnt] = VAR_TIM;
	 
      }

      
      /* process if a string constant; store any string literal
	 into the string portion of the values union, even though the 
	 union has a name of "size" */
      
      else if (strcmp(format_type, "\"") == 0)
      {	 
	 /* find the length of the string; if the length is too long
	    issue warning message; share the string limit
	    size used for the condition values; */
	 
	 literal_len = strlen(format_literal);
	 if (literal_len > MAXLEN_VALUESTR)
	 {
	    error_flag = TRUE;
	    log_msg(INVALID_L_FORMAT, format_literal);
	 }
	 else
	 {	       
	    
	    format->type[cnt] = RPF_STR;
	    format->type_size[cnt] = RPF_STR;
	    strcpy(format->size[cnt].s, format_literal);
	 }
      }
      
      
      /* log error message */
      
      else
      {
	 error_flag = TRUE;
	 log_msg (INVALID_FORMAT, "");
      }
      
      
      /* if no errors, increment the count and get the next format item */
      
      if (error_flag == TRUE)
      {
	 format->num_of_formats = 0;
	 return;
      }
      else
      {
	 cnt++;
	 memset(format_item, '\0', MAXLEN_STRING);
	 memset(format_literal, '\0', MAXLEN_STRING);
	 grab_format_item(fileline, format_item, format_literal);
      }
   }
   
   
   /* set the actual count, number of format items and 
      initialize the leftover format slots as invalid */
   
   format->num_of_formats = cnt;   
   for (i = cnt; i < MAX_FORMAT_ITEMS - 1; i++)
      format->type[i] = MISSINGVAL;
   
   return;
}


/*********************************************************************
   grab_format_item()
   
   PURPOSE
   Grabs the next format, if one exists, from a format specifier line.
   
   NOTES
   If a format is found, the function modifies the pointer to the format
   line for use in searching for the next format.  The function returns
   a format in either the format_item argument, for non quoted string
   formats (i.e. literals), or in the format_literal argument, for the 
   other conventional formats.  If no formats defined then Null values
   are returned.
   
   
   *******************************************************************/
void grab_format_item(char *fileline,
		      char *format_item,
		      char *format_literal)
{
   int 	i, numchars;
   int 	start_literal, start_format;
   char scratch[MAXLEN_TEMPLATEREC];
   
   
   /* initialize */
      
   start_literal = MISSINGVAL;
   start_format = MISSINGVAL;
   numchars = 0;
   
   
   /* loop on the characters in the format line */
      
   for (i = 0; i < strlen(fileline); i++)
   {
      /* if currently reading a conventional format item, get 
	 next character; if the next character is a space character,
	 then copy format item and return */
      
      if (start_format != MISSINGVAL)
      {
	 if (isspace(fileline[i]) == 0)
	    numchars++;
	 else
	 {
	    strncpy(format_item, &fileline[start_format], numchars);
	    strcpy(scratch, &fileline[i]);
	    strcpy(fileline, scratch); 
	    return;
	 }
      }
      
      
      /* if currently reading a literal format, get 
	 next character; if the next character is a closing quote,
	 then copy the literal without the quote characters and return */
      
      else if (start_literal != MISSINGVAL)
      {
	 if (fileline[i] != '\"')
	    numchars++;
	 else
	 {
	    strncpy(format_literal, &fileline[start_literal+1], numchars-1);
	    
	    strcpy(scratch, &fileline[i+1]);
	    strcpy(fileline, scratch); 
	    return;
	 }
      }
      
      
      /* look for first format item or literal; if found, then
	 note starting position */
      
      else
      {
	 if (isspace(fileline[i]) == 0)
	 {
	    numchars++;
	    if (fileline[i] == '\"')
	       start_literal = i;
	    else
	       start_format = i;
	 }	 
      }
   }
   
   return;
}


/*********************************************************************
   read_varlist()
   
   PURPOSE
   Loads in the variables specified for the variables buffer 
   used for the template.
   
   *******************************************************************/
void read_varlist(char 			*fileline,
		  variable_struct	*variable)
{
   char 	*token;
   itemtypes 	type_variable;
   int 		cnt;
   int		line_pos;
   int		original_len;
   
   
   /* initialize */
   
   cnt = 0;
   variable->num_of_variables = 0;
   line_pos = 0;
   
      
   /* grab the first variable token */
   
   original_len = strlen(fileline);
   token = strtok(fileline, " \t\n");      
   
   
   /* loop on the number of tokens */
   
   for (;;)
   {
      type_variable = MISSINGVAL;
      variable->varinfo[cnt].varindex = MISSINGVAL;
      
      
      /* if reached maximum number of items, then stop reading */
      
      if (token == NULL)
	 break;
      
      else if (cnt >= MAX_FORMAT_ITEMS)
      { 
	 log_msg(EXCEED_NUM_VAR, fileline);
	 break;
      }           
  
      
      /* check if this token is a valid variable and gets its
	 index number and other attributes associated with 
	 the variable */
      
      check_if_variable(token, &type_variable,
			&(variable->varinfo[cnt]));
      
      
      /* if valid, then increment count; 
	 if invalid variable then don't load it and stop reading list */
      
      if (isvar(type_variable) == TRUE)
	 cnt++;      
      else
      {
	 log_msg(INVALID_VARIABLE, token);
	 break;
      }
      
      
      /* get the next token.  this needs to start at an explicit
	 location since the check_if_variable function calls
	 the check_if_PEvar which also uses the strtok function
	 to step thru the parts of the variable specification 
	 subfields, so using the NULL argument for string token
	 will not work.  as part of this approach, it needs to track
	 the line position for use when parsing the tokens */
      
      line_pos = line_pos + strlen(&fileline[line_pos]) + 1;
      
      if (line_pos < original_len)
	 token = strtok(&fileline[line_pos], " \t\n");
      else
	 token = NULL;
   }
   
   
   /* set the actual count before leaving */
   
   variable->num_of_variables = cnt;

   return;
}


/*********************************************************************
   read_spectime()
   
   PURPOSE
   Loads in the specific times given for the observed and
   forecast stages in the template.
   
   format: refday basehour [dayoffset hroffset window] [repeat as needed]
   
   *******************************************************************/
void read_spectime(const time_t			system_time,
		    	  char 			*fileline,
		          spectime_struct	*spectime)
{
   char *token;
   int cnt;
   struct tm *basetime;
   char daystr[MAXLEN_STRING];
   int error_flag;
   
   
   /* initialize */
   
   cnt = 0;
   error_flag = FALSE;
   spectime->num_of_spectimes = 0;
   
   
   /* grab the first token */
   
   token = strtok(fileline, " \t\n");
   
   
   /* the first two tokens must be a date and an hour value;
      the date can be either a specific data or the keyword TODAY. */
   
   if (token == NULL)
      error_flag = TRUE;
   
   else if (strncmp(token, "TODAY", 5) == 0)
   {
      basetime = gmtime(&system_time);
      strftime(daystr, MAXLEN_STRING, "%m/%d/%Y", basetime);
   }
   
   else
      strcpy(daystr, token);
   
   
   /* get the time string if there have been no errors */
      
   if (error_flag == FALSE)
   {
      token = strtok(NULL, " \n");     
      if (token == NULL)
	 error_flag = TRUE;
      
      
      /* now that the date and time string have been obtained, 
	 convert them to a time variable */
      
      else      	 
	 spectime->basetime = convert_gmdatetime_to_time(daystr, token);
   }
   
   
   /* loop on the number of groups of time definitions, where
      each should have a day, hour, and window value */
   
   while (error_flag == FALSE)
   {
      /* get the relative day value, if no more sets specified then exit */
      
      token = strtok(NULL, " \n");
      if (token != NULL)
      {
	 /* check if count exceeds the maximum; if so 
	    issue warning message and ignore rest of line */
	 
	 if (cnt > MAX_SPECTIMES)
	 {
	    log_msg(EXCEED_NUM_SPECTIMES, fileline);
	    cnt--;
	    break;
	 }
	 spectime->relday[cnt] = atoi(token);
      }
      
      else
	 break;
      
      
      /* get the relative hour value */
      
      token = strtok(NULL, " \n");
      if (token != NULL)
      {
	 spectime->relhour[cnt] = atoi(token);
	 if (spectime->relhour[cnt] > +24 ||
	     spectime->relhour[cnt] < -24) error_flag = TRUE;
      }
      
      else
	 error_flag = TRUE;
      
      
      /* get the hour window value */
      
      token = strtok(NULL, " \n");
      if (token != NULL)
      {
	 spectime->window[cnt] = atoi(token);
	 if (spectime->window[cnt] > +24 || spectime->window[cnt] < -24) 
	    error_flag = TRUE;
      }
      
      else
	 error_flag = TRUE;

      
      /* increment the count */
      
      cnt++;	 
	 
   }
   
   
   /* set the number of entries */
   
   if (error_flag == FALSE)
      spectime->num_of_spectimes = cnt;
   
   else
      log_msg(INVALID_SPECTIME, "");

   
   return;
}


/*********************************************************************
   read_grpname()
   
   PURPOSE
   Read the grpname line which simply indicates whether to skip a line
   when the group name line is written, and by its presence indicates
   that the group names should be included
   
   *******************************************************************/
void read_grpname(char 	*fileline,
		  int 	*skip_grpline,
		  int	*include_grpname)
{
   char *token;
      
   
   /* grab the first token and check its value */
   
   token = strtok(fileline, " \t\n");
   
   if (token != NULL)
   {
     if (strcmp(token, "SKIP") == 0 || strcmp(token, "skip") == 0)
     {
	*skip_grpline    = TRUE;
	*include_grpname = TRUE;
     }

     else if (strcmp(token, "OFF") == 0 || strcmp(token, "off") == 0)
     {
	*skip_grpline    = TRUE;  /* this value gets ignore anyway */
	*include_grpname = FALSE;
     }

     else
     {
	*skip_grpline    = FALSE;  
	*include_grpname = TRUE;
     }
   
   }
   return;
}


/*********************************************************************
   read_msgdata()
   
   PURPOSE
   Read the missing data line which indicates whether to skip lines
   which contain missing data or specify the string to use to 
   indicate missing data.
   
   *******************************************************************/
void read_msgdata(char 	*fileline,
		  int 	*skip_msgdata,
		  int   *skip_allmsg,
		  char	*missing_str)
{
   char *token;
   
      
   /* grab the first token. */
   
   token = strtok(fileline, " \t\n");
   
   if (token != NULL)
   {  
     /* set the flag accordingly */
   
     if (strcmp(token, "SKIP") == 0 ||
        strcmp(token, "skip") == 0)
     {
        *skip_msgdata = TRUE;
        memset(missing_str, 0, 1);
     }
     else if (strcmp(token, "SKIPALL") == 0 ||
	    strcmp(token, "skipall") == 0)
     {
        *skip_allmsg = TRUE;
	
	token = strtok(NULL, " \t\n");
	if (token == NULL)
	   memset(missing_str, 0, 1);
	else
	   strcpy(missing_str, token);   
     }		     
     else
     {
       *skip_msgdata = FALSE;
       *skip_allmsg  = FALSE;
       
       if (token != NULL)
          strcpy(missing_str, token);
       else
          memset(missing_str, 0, 1);
     }
   }
   
   return;
}


/*********************************************************************
   get_num_items()
   
   PURPOSE
   This function gets the number of items in the conditional
   expression, where items included variables and what are
   defined as tokens in the application context, which are
   relational and logical operators and separators.
   
   NOTES
   
   ********************************************************************/
int get_num_items(char	*fileline)

{
   char 	local_line[MAXLEN_TEMPLATEREC];
   int 		cnt;
   char 	*item;
   
   
   /* make a copy of the string for local use; this leaves fileline
      unchanged for use later */
   
   if (fileline != NULL)
      strcpy(local_line, fileline);
   else
   { 
      log_msg(INVALID_CONDITION_STR, fileline);
      return 0;
   }   
   
   
   /* initialize */
   
   cnt = 0;
   item = strtok(local_line," \t\n");
   
   
   /* loop on the number of tokens and increment the count*/
   
   while (item != NULL)
   {
      cnt++;
      item = strtok(NULL, " \t\n");
   }
   
   return(cnt);
}


/*********************************************************************
   verify_condition_syntax()
   
   PURPOSE
   This function checks the syntax of the conditional expression.
  
   NOTES
   The variable side indicates which side (i.e. left, right, or none)
   the previous item was.  Only variables or constants, regardless of
   whether they are numeric or string, can be given a "side"  value.
   All other items get a value of none.
   
   A basic syntax rules are that all relational expressions must
   be enclosed within parentheses.
   
   Note that this function checks only the most recent condition for the
   given template.
   
   ********************************************************************/
void verify_condition_syntax(template_info_struct	*template_info)
{
   int 			stacknum, index;
   int 			num_open_paren;
   int 			prevtype = 0;
   int			curtype = 0;
   template_item_struct *template_item;
   int 			LEFT = -1, RIGHT = 0, NONE = 1, side;
   int 			error_flag;
   char 		msgstr[MAXLEN_STRING];
   
   
   /* define a convenient variable to identify which stack in the 
      template is being checked; then get the stack address */
   
   stacknum = template_info->num_conditions - 1;
   if (stacknum < 0 )
   {
     return;
   }  
   else
   {   
     template_item = template_info->bottom_condition_stack[stacknum];
   
   
   /* initialize variables */
   
   side = NONE;
   num_open_paren = 0;
   error_flag = FALSE;
   
   
   /* build a message string for any possible error message */
   
   sprintf(msgstr, "for condition %d in template %s",
	   stacknum + 1, template_info->name);
   
   
   /* check if the stack is comprised of only one item, which
      is permitted if it is in integer constant, since this
      is what a boolean (T/F) is translated to when read */
   
   if (template_info->stacksize[stacknum] == 1 && 
       template_item[0].type == RPF_INT) return;
      
   
   /* loop on the number of items in the stack */
   
   for (index = 0; 
    index < template_info->stacksize[stacknum] && error_flag == FALSE; ++index)
   {
      /* get the type from the stack */
      
      curtype =  template_item[index].type;
	 
      
      /* a left paren can only follow another left paren or a logical
	 operator, or can be the first item in the conditional expression */
      
      if (curtype == LEFT_PAREN)
      {
	 if (index != 0 &&
	     (prevtype != LEFT_PAREN && islogop(prevtype) == FALSE))
	 {
	    log_msg(INVALID_LPAREN_LOC, msgstr);
	    error_flag = TRUE;
	 }
	 else
	 {
	    num_open_paren++;
	    side = NONE;
	 }
      }
      
      
      /* a right paren can only follow a right-side variable or constant,
	 or another right paren */
      
      else if (curtype == RIGHT_PAREN)
      {
	 if (prevtype != RIGHT_PAREN && side != RIGHT)
	 {
	    log_msg(INVALID_RPAREN_LOC, msgstr);
	    error_flag = TRUE;
	 }
	 else
	 {
	    num_open_paren--;
	    if (num_open_paren < 0)
	    {
	       log_msg(UNBAL_PAREN, msgstr);
	       error_flag = TRUE;
	    }
	    else
	       side = NONE;
	 }
      }
      
      
      /* an integer, float, or time variable or constant can only follow a
	 left paren or a relational operator; i.e. don't allow them to 
	 follow a logical operator or be the first item in the stack */
      
      else if (isnumber(curtype) == TRUE)
      {
	 if (prevtype != LEFT_PAREN && isrelop(prevtype) == FALSE)
	 {
	    log_msg(INVALID_NUMBER_LOC, msgstr);
	    error_flag = FALSE;
	 }
	 
	 else
	 {	 
	    if (isrelop(prevtype) == TRUE)
	       side = RIGHT;
	    else
	       side = LEFT;
	 }
      }
      
      
      /* a string variable or constant can only follow a left paren
	 or a string operator; i.e. don't allow them to follow
	 a logical operator or be the first item in the stack */
      
      else if (isstring(curtype) == TRUE)
      {
	 if (prevtype != LEFT_PAREN && isstrop(prevtype) == FALSE)
	 {
	    log_msg(INVALID_STRING_LOC, msgstr);
	    error_flag = TRUE;
	 }
	 
	 else
	 {	 
	    if (isstrop(prevtype) == TRUE)
	       side = RIGHT;
	    else
	       side = LEFT;
	 }
      }
      
      
      /* a relational operator can only follow a leftside integer or
	 float variable or constant */
      
      else if (isrelop(curtype) == TRUE)
      {
	 if (isnumber(prevtype) == FALSE || side != LEFT)
	 {
	    log_msg(INVALID_RELOP_LOC, msgstr);
	    error_flag = TRUE;
	 }
	 
	 else
	    side = NONE;
      }
      
      
      /* a string operator can only follow a leftside string variable
	 or constant */
      
      else if (isstrop(curtype) == TRUE)
      {
	 if (isstring(prevtype) == FALSE || side != LEFT)
	 {
	    log_msg(INVALID_STROP_LOC, msgstr);
	    error_flag = TRUE;
	 }
	 else
	    side = NONE;
      }
      
      
      /* a logical operator can only follow a right paren */
      
      else if (islogop(curtype) == TRUE)
      {
	 if (prevtype != RIGHT_PAREN)
	 {
	    log_msg(INVALID_LOGOP_LOC, msgstr);
	    error_flag = TRUE;
	 }
	 else
	    side = NONE;
      }
      
      
      /* set the previous type before continuing to the next item */
      
      prevtype = curtype;
	 
   }
   
   
   /* check if parentheses are not balanced */
   
   if (num_open_paren != 0)
   {
      log_msg(UNBAL_PAREN, msgstr);
      error_flag = TRUE;
   }
   
   
   /* check that the last item is a parentheses.
      this forces the entire condition to be enclosed in parentheses, which
      make the evaluation of the condition a little easier. */
   
   if (curtype != RIGHT_PAREN)
   {
      log_msg(MISSING_ENDPAREN, msgstr);
      error_flag = TRUE;
   }
   
   
   /* if an error was detected then set the expression so that it is
      assumed to be false */
   
   if (error_flag == TRUE)
   {
      template_info->stacksize[stacknum] = 1;
      template_item[0].type = RPF_INT;
      template_item[0].value.i = FALSE;
   }
  }	 
   return;
}


/*********************************************************************
   get_item_info()
   
   PURPOSE
   This function gets the information for this item in the 
   conditional expression.
   
   ********************************************************************/
void get_item_info(char			*item,
		   itemtypes		*type,
		   values		*value,
		   varinfo_struct	*varinfo)

{
   
   /* initialize the type to nothing */
   
   *type = MISSINGVAL;
   
   
   /* check if this token is a logical operator, a relational
      operator, or a separator */
   
   check_if_token(item, type);
    
   
   /* check if the token is a float or integer */
   
   if (*type == MISSINGVAL)
      check_if_number(item, type, value);
 
   
   /* check if the token is a string */
   
   if (*type == MISSINGVAL )
      check_if_string(item, type, value);

   
   /* check if the token is a boolean */
   
   if (*type == MISSINGVAL )
      check_if_boolean(item, type, value);
   
   
   /* check if the token is a missing data token */
   
   if (*type == MISSINGVAL )
      check_if_missval(item, type, value);

   
   /* check if a variable name is specified */
   
   if (*type == MISSINGVAL)
      check_if_variable(item, type, varinfo);
  
   
   
   /* if the token does not match any of the above allowable types, 
      it is invalid; log error message and set to be a false value 
      integer. this will probably result in the condition syntax being
      unacceptable and another error message, and  the condition
      won't be set. */
   
   if (*type == MISSINGVAL)
   {
      log_msg(BAD_ITEMTYPE_IN_LOGIC, item);
      *type = RPF_INT;
      value = FALSE;
   }
      
   return;
}


/***********************************************************************
   check_if_token()
   
   PURPOSE
   Checks if the item read from the template matches any of the
   reserved logical or relational operators or any of the 
   special tokens.
      
   ***********************************************************************/
void check_if_token(const char      *item,
		          itemtypes *type)
{
   extern template_token_struct TEMPLATE_TOKENS_TABLE[];
   extern int NUM_OF_TEMPLATE_TOKENS;

   int i;
   
   
   /* loop on the number of items in the table  */
   
   for (i = 0; i < NUM_OF_TEMPLATE_TOKENS; ++i)
   {
      /* if the token matches the string, load in the info */
      
      if (strcmp(item, TEMPLATE_TOKENS_TABLE[i].name) == 0)
      {
	 *type = TEMPLATE_TOKENS_TABLE[i].type;
	 break;
      }
   }
   
   return;
}


/***********************************************************************
   check_if_number()
   
   PURPOSE
   Checks if the token is a integer or real number, and if so, 
   loads in the appropriate values.
   
   NOTES
   
   ***********************************************************************/
void check_if_number(const char       *token, 
		           itemtypes  *type, 
		           values     *value)

{
   int tokenlen;
   int i;
   int numpoints, errorflag;
   
   
   /* get the length of the token */
   
   tokenlen = strlen(token);
   
   
   /* initialize */
   
   numpoints = 0;
   errorflag = 0;
   
   
   /* loop on the number of characters and make sure that a digit
      or a decimal point is specified */
   
   for (i= 0; i < tokenlen; i++)
   {
      if (token[i] == '.') 
	 ++numpoints;
      
      else if (token[i] == '+' || token[i] == '-')
      {
	 if (i != 0) 
	 {
	    errorflag = 1;
	    break;
	 }
      }
      
      else
      {
	 if (isdigit(token[i]) == 0)
	 {
	    errorflag = 1;
	    break;
	 }
      }
   }
   
   
   /* if errorflag false then all digits or decimal points 
      specified; convert the value as appropriate */
   
   if (errorflag == 0)
   {
      if (numpoints == 1)
      {
	 *type = RPF_FLT;
	 value->f = atof(token);
      }
      
      else if (numpoints == 0)
      {
	 *type = RPF_INT;
	 value->i = atoi(token);
      }
      
      else
	 errorflag = 1;
   }
   
   
   /* if errorflag set, simply do not load the value and the type */
   
   
   return;
}


/***********************************************************************
   check_if_string()
   
   PURPOSE
   Checks if the token is a string and if so loads in the values. 
   
   ***********************************************************************/
void check_if_string(const char      *token, 
		           itemtypes *type, 
		           values    *value)
   
{
   int tokenlen;
   
   
   /* find the length of the string */
   
   tokenlen = strlen(token);
   
   
   /* make sure that the first and last characters in the string
      delineate the string; if so then add a terminating null
      character and copy the string */
   
   if (token[0] == '"' && token[tokenlen-1] == '"')
   {
      *type = RPF_STR;
      strncpy(value->s, &token[1], tokenlen - 1);
      value->s[tokenlen-2] = '\0';
      if (tokenlen > MAXLEN_VALUESTR) log_msg(EXCEED_VALUESTR, "");
   }  
   
   return;
}


/***********************************************************************
   check_if_missval()
   
   PURPOSE
   Checks if the token is a token for a MISSING value. 
   
   NOTES
   The missing indicator value is immediately converted
   to an integer value to allow for proper evaluation purposes.

   
   ***********************************************************************/
void check_if_missval(const char      *token, 
		            itemtypes *type, 
		            values    *value)
   
{
   /* check if token is a true indicator */
   
   if (strcmp(token, "MISSING") == 0)
   {
      *type = RPF_INT;
      value->i = MISSING_TOKEN;
   }

   return;
}


/***********************************************************************
   check_if_boolean()
   
   PURPOSE
   Checks if the token is a boolean and if so loads in the values. 
   
   NOTES
   Booleans live a very short life as they are immediately translated
   into an integer value.
   
   ***********************************************************************/
void check_if_boolean(const char      *token, 
		            itemtypes *type, 
		            values    *value)
   
{
   /* check if token is a true indicator */
   
   if (strcmp(token, "TRUE") == 0)
   {
      *type = RPF_INT;
      value->i = TRUE;
   }
   
   
   /* check if token is a false indicator */
   
   else if (strcmp(token, "FALSE") == 0)
   {
      *type = RPF_INT;
      value->i = FALSE;
   }      

   return;
}


/***********************************************************************
   check_if_variable()
   
   PURPOSE
   Checks if the token read from the template matches any of the
   reserved variable names.
   
   NOTES
   This section only checks the access based on the product section.
   The check as to whether the variable is allowed in a condition is
   made in the read_condition function.
   
   ***********************************************************************/
void check_if_variable(char		*item,
		       itemtypes	*type,
		       varinfo_struct	*varinfo)
{
   extern template_variable_struct TEMPLATE_VARIABLES_TABLE[];
   extern int NUM_OF_TEMPLATE_VARIABLES;
   int	i;
   int	status, metric_status;
   
   
   /* initialize the type of template token */
   
   *type = MISSINGVAL;
   varinfo->metric_flag = FALSE;
   status = metric_status = -1;
   
   /* loop on the number of items in the table */
   
   for (i = 0; i < NUM_OF_TEMPLATE_VARIABLES; ++i)
   {
      /* if the item matches the variable name, load in the info;
	 any check for whether the variable is permitted to be in
	 for a particular phrase or condition is performed when the
	 phrase/condition is processed */
      
      if (strcmp(item, TEMPLATE_VARIABLES_TABLE[i].name) == 0)
      {
	 *type = TEMPLATE_VARIABLES_TABLE[i].type;
	 varinfo->varindex = i;
	 break;
      }      
   }
   
   /* check if the variable is metric_flag specified variable*/
      
   if (*type == MISSINGVAL)
      
      metric_status = check_if_metric(item,varinfo,type);
	 
   /* check if the variable is the Obs or Fcst PE variable, which is
      only an internal label.  the user should NOT be specifying
      the <PeVal> or <PeTime> type variables;
      so it is not advertised to the user. */
   
   if (*type == MISSINGVAL)
   {     
      status = check_if_PEvar(item, varinfo);
      
      if (status >= 0)
      {	 
	 for (i = 0; i < NUM_OF_TEMPLATE_VARIABLES; ++i)
	 {
	    
	    /* time variables need their own identity in the variable 
	       list since they will be stored as time_t and formatted
	       as time also, unlike the float values */
	    
	    if (varinfo->time_flag)
	    {
	       if (strcmp("<PETime>",  TEMPLATE_VARIABLES_TABLE[i].name) == 0)
	       {
		  *type = TEMPLATE_VARIABLES_TABLE[i].type;
		  varinfo->varindex = i;
		  break;
	       }
	    }
	    else
	    {
	       if (strcmp("<PEVal>",  TEMPLATE_VARIABLES_TABLE[i].name) == 0)
	       {
		  *type = TEMPLATE_VARIABLES_TABLE[i].type;
		  varinfo->varindex = i;
		  break;
	       }
	    }
	 }
      }
   }
   
   
      
   
   if (*type == MISSINGVAL && status < 0 && metric_status < 0)
       *type = RPF_INVALID;
   
   
   return;
}

 
/***********************************************************************
   check_var_access()
   
   PURPOSE
   Check if the product section permits usage of the variable within
   the section.   
   
   NOTES
   This approach controls variable usage by whether it can be included
   in a particular product section.  No distinction is made between
   the variable is in the conditional expression for a phrase, or in
   the phrase itself.  This can be added in the future if necessary.
   
   ***********************************************************************/
int check_var_access(const char	*accesslist,
		     const int	product_section) 
{  
   int access;
   
   
   /* initialize */
   
   access = FALSE;
   
   
   /* check if the access list for the variable includes the product
      section currently being processed */
   
   if (product_section == HEADER &&
       strchr(accesslist, HEADER_CODE) != NULL) access = TRUE;
   
   else if (product_section == SUMMARY && 
	    strchr(accesslist, SUMMARY_CODE) != NULL) access = TRUE;
	    
   else if (product_section == HEADLINE && 
	    strchr(accesslist, HEADLINE_CODE) != NULL) access = TRUE;
	    
   else if (product_section == TABULAR && 
	    strchr(accesslist, TABULAR_CODE) != NULL) access = TRUE;
   
   else if (product_section == DATA_ROUNDUP && 
            strchr(accesslist, ROUNDUP_CODE) != NULL) access = TRUE;
   else if (product_section == IMPACT_STATEMENT && 
	    strchr(accesslist, IMPACT_CODE) != NULL) access = TRUE;
   
   else if (product_section == HISTORICAL_COMPARISON && 
	    strchr(accesslist, COMPARISON_CODE) != NULL) access = TRUE;
   
   
   return(access);
}

/**************************************************************
check_if_metric()
PURPORSE
check if the varibale has metric_flag which means to do the
unit conversion.e.g. <ObsStg,MET>
**************************************************************/
int check_if_metric(char            *fullfield,
                    varinfo_struct  *varinfo,
		    itemtypes       *type)
{
    extern template_variable_struct  TEMPLATE_VARIABLES_TABLE[];
    extern int  NUM_OF_TEMPLATE_VARIABLES;
    int    slen, i, len, status;
    char   item[MAXLEN_VARNAME], msgstr[100];
    char   *token;
    char   token1[MAXLEN_VARNAME],token_str[MAXLEN_VARNAME];
    char   token2[4];
   
    /*initialize*/
    
    status = -1;
        
    slen = strlen(fullfield);
    if (slen < 30)
    {
       if (fullfield[0] != '<' || fullfield[slen - 1] != '>')
          return(status);
       else
        strcpy(item, &fullfield[1]);
	   
    }
    else
       return(status);
  
     
     strcpy(token1, "");
     strcpy(token2, "");
       
    /*find part before "," in the item*/
  
    len = strcspn(item,",");
    if (len != 0)
    {
      strncpy(token_str,item,len);
      token_str[len] = '\0';
      
      strcpy(token1,"<");
      strcat(token1,token_str);
      strcat(token1,">");
    }
    
    /*find the part after "," in the item*/
    
    token=strchr(item,',');
    if (token)
    {
       if (strlen(token) != 5)
          return(status);
       else
       {	  
         strncpy(token2, token+1, 3);
         token2[3] = '\0';
        
         if (strcmp(token2, "MET") == 0)
         {
           for (i=0; i< NUM_OF_TEMPLATE_VARIABLES; ++i)
           {
             if (strcmp(token1, TEMPLATE_VARIABLES_TABLE[i].name) == 0)
             {
	        *type = TEMPLATE_VARIABLES_TABLE[i].type;
	        varinfo->varindex = i;
	        varinfo->metric_flag = TRUE;
		status = 1;
	        break;
	     }
	     else
	        status = -1;
           }
          }
    
          else
          {
             sprintf(msgstr, "Invalid metric value: %s", token2);
             log_msg("",msgstr);
             status = -1;
          }
	}  
     }
     else
        status = -1;        	    
        
return(status);
}          	  	  
