/*********************************************************************
   read_lines.c
   
   read_generic_templates()
   get_continuation_lines() 
   determine_template_record_type()
   
   ********************************************************************/

#include <string.h>               
#include <stdio.h>               
#include <stdlib.h>

#include "read_lines.h"              



extern char paramdir[];

/*********************************************************************
   read_generic_templates()
   
   PURPOSE
   Read the template file in a generic fashion so that it
   can be used for all of the template read sections.
   
   ********************************************************************/

void read_generic_templates(char 		*file_suffix,
			    char		*selected_office,
			    int			section_index,
			    temp_name_struct	*templates)

{
   FILE *file_ptr;
   char *fgets_ptr;
   char fileline[MAXLEN_TEMPLATEREC];
   char dummy[MAXLEN_TEMPLATEREC];
   char template_file[MAXLEN_FILENAME];
   char *name;
   int continue_read, record_type;
   int slen;
   int linenum;
   
   
   /* build the name of the template file based on the suffix
      and the hsa name */
   
   sprintf(template_file, "%s/%s.%s", paramdir, file_suffix, selected_office);
   
   
   /* open the template file */
   
   file_ptr = fopen(template_file, "r");
   if(file_ptr == NULL) log_msg(FILE_OPENERR, template_file);
   
   
   /* initialize */
   
   continue_read = 1;
   templates->number = 0;
   linenum = 0;
   
   
   /* loop on the number of lines in the file */
   
   do
   {      
      /* get a line from the file, if a null pointer is returned,
	 then assume at end of file and return */
      
      fgets_ptr = fgets(fileline, MAXLEN_TEMPLATEREC, file_ptr);
      if (fgets_ptr != NULL) 
      {
	 convert_str_to_upcase(fileline);
	 
	 /* determine the type of record, i.e. whether it be a 
	    template name, condition, or phrase */
	 
	 linenum++;	 
	 determine_template_record_type(fileline, section_index, linenum,
					&record_type);
	 
	 
	 /* read any continuation lines to get them out of the
	    way of subsequent calls to to determine the record type */
	 
	 get_continuation_lines(fileline, file_ptr, &linenum, dummy);				
	 
	 
	 /* if this record is a template name, get the name and load it */
	 
	 if (record_type == TEMPLATE_NAME)
	 {
	    /* assume the template name is terminated by one of the
	       characters in the string token function below */
	    
	    name = strtok(fileline, ":\n \t");
	    
	    
	    /* load in the template name and increment the number */
	    
	    if (name != NULL)
	    {
	      slen = strlen(name) + 1;
	      if (slen > MAXLEN_TEMPLATENAME)
		 log_msg(EXCEED_TEMPLATENAME, name);

	      else
	      {
		 templates->name[templates->number] = (char *)malloc(slen);
		 if (templates->name[templates->number] == NULL)
		    log_msg(FAILED_MALLOC, 
			    "of templates->name[] in read_generic_templates");
		 memset(templates->name[templates->number], '\0', slen);
		 
		 strcpy(templates->name[templates->number], name);
		 templates->number++;


		 /* if there are more templates than allowed, then log an error
		    message and stop reading from the file */

		 if (templates->number >= MAX_TEMPLATES)
		 {
		    log_msg(EXCEED_MAXTEMPLATES, template_file);
		    continue_read = 0;
		 }
	      }
	    }
	    else
	       log_msg(NO_RECORD_CONTENT,
		       " template name record, name undefined");
	    				    	     
	 }  
      }  /* end of if block when fgets_ptr is null */
      
      else
	 continue_read = 0;
      
   } while(continue_read);	 
   
   
   /* close the template file */
   
   fclose(file_ptr);
   
   
   /* sort the template names */   
   
   qsort((char *)(templates->name), (size_t )templates->number,
	 sizeof(char *), name_compare);
   
   return;   
}


/*********************************************************************
   name_compare
   
   PURPOSE	
   
   ********************************************************************/
int name_compare(const void *name1, 
		 const void *name2)
{
   int result;
   char ** n1 = (char **) name1;
   char ** n2 = (char **) name2;

   result = strcmp(*n1, *n2); 
     
   return(result);
}


/*********************************************************************
   get_continuation_lines()
   
   PURPOSE
   This function reads the given string and loads it and any continuation
   lines into a returned string.
      
   NOTES
   This function assumes that the longest string it can handle
   is of size MAXLEN_TEMPLATEREC.
   
   ********************************************************************/
void get_continuation_lines(char	*fileline,
		    	    FILE	*file_ptr,
			    int		*linenum,
			    char	outline[])
{
   char	nextline[MAXLEN_TEMPLATEREC];
   int 	outlen, nextlen;
   int	newline_len;
   char	*fgets_ptr;
   int	continuation_found;
   char	msgstr[250];
   

   /* initialize */
   
   outlen = 0;
   memset(nextline, 0, MAXLEN_TEMPLATEREC);
   
   
   /* copy the first line always, and get the length */
   
   strcpy(outline, fileline);
   outlen = strlen(outline);
   
   
   /* determine if the current line ends with a continuation
      symbol. */
   
   if (outlen < 2) return;
   
   if (outline[outlen - 2] == '&')
      continuation_found = TRUE;
   else
      continuation_found = FALSE;
   
     
   /* if continuation line, then get the next line(s) */
   
   while (continuation_found)
   {      
      
      /* get the next line */
      
      fgets_ptr = fgets(nextline, MAXLEN_TEMPLATEREC, file_ptr);
      
      if (fgets_ptr == NULL)
      {
	 log_msg(MISSING_CONT_LINE, outline);
	 
	 outlen = strlen(outline);
	 memset(&outline[outlen - 2], 0, 1);
	 strcat(outline, "\n");
	 
	 return;
      }
      
      
      /* increment the line number for possible later logging */
      
      else          
	 *linenum = *linenum + 1;
      
      
      /* set the length; subtract 2 to account for the continuation
	 symbol and the newline character */
      
      outlen      = strlen(outline) - 2;
      nextlen     = strlen(nextline);
      newline_len = outlen + nextlen;
                 
      
      /* check the length of the new string. if ok copy the next
	 line, starting just before the continuation symbol from
	 the previous line */
      
      if (newline_len > MAXLEN_TEMPLATEREC - 1)
      {
	 sprintf(msgstr, "limit=%d\n   first 40 chars:", MAXLEN_TEMPLATEREC - 1);
	 strncat(msgstr, outline, 40);
	 log_msg(EXCEED_TEMPLATEREC, msgstr);
	 memset(&outline[outlen], 0, 1);
	 return;
      }
            
      else
	 strcpy(&outline[outlen], nextline);      
      
      
      /* check if there is a continuation symbol */
      
      if (nextline[nextlen - 2] == '&')
	 continuation_found = TRUE;
      else
	 continuation_found = FALSE;
   }
   
      
   return;
}


/*********************************************************************
   determine_template_record_type()
   
   PURPOSE
   Determines the type of record in the template file.
   
   NOTES
   If a record type is not specified, then a phrase record type is
   assumed.
   
   ********************************************************************/

void determine_template_record_type(char *fileline,
				    int  section_index,
				    int  linenum,
				    int  *record_type)
				    
{
   char *type_read;
   int typelen;
   char msgstr[300];
   char section_str[30];
   
   
   /* initialize section name for log message */
   
   if (section_index == MISSINGVAL)
      sprintf(section_str, " ");
   else
      strcpy(section_str, convert_index_to_sectionname(section_index));   
	 
   
   /* check if it is a comment line */
   
   if (fileline[0] == COMMENT_SYMBOL)
      *record_type = TEMPLATE_COMMENT;
   
   else
   { 
      /* get the first token in the file line;
	 if no colon, then assume that it is a phrase record */
      
      type_read = strstr(fileline, ":");
      if (type_read == NULL)
      {
	 if (strlen(fileline) < 280)
	    sprintf(msgstr, "%s section, line %d: %s",
		    section_str, linenum, fileline);
	 else
	 {
	    sprintf(msgstr, "%s section, line %d: (line too long)",
		    section_str, linenum);
	 }
	 
	 *record_type = TEMPLATE_PHRASE;
	 log_msg(ASSUMED_PHRASETYPE, msgstr);
      }
      
      
      /* a token was defined so determine which type it is */
      
      else
      {
	 type_read = strtok(fileline, " :");
	 
	 if (type_read != NULL)
	 {		 
	    /* convert type read to upper case*/
	    
	    convert_str_to_upcase(type_read);
	    
	    
	    /* check the type of template line this is */
	    
	    if (strcmp("NAME", type_read) == 0)
	       *record_type = TEMPLATE_NAME;
	    
	    else if (strcmp("CONDITION", type_read) == 0)
	       *record_type = TEMPLATE_CONDITION;
	    
	    else if (strcmp("PHRASESTR", type_read) == 0)	           
	       *record_type = TEMPLATE_PHRASE;	     
	    
	    else if (strcmp("BULLETSTR", type_read) == 0)
	       *record_type = TEMPLATE_BULLETSTR;
	       
	    else if (strcmp("INDENTSTR", type_read) == 0)
	       *record_type = TEMPLATE_INDENTSTR;   
	    
	    else if (strcmp("FORMATS", type_read) == 0)
	       *record_type = TEMPLATE_FORMAT;
	    
	    else if (strcmp("VARLIST", type_read) == 0)
	       *record_type = TEMPLATE_VARIABLE;
	    
	    else if (strcmp("SPECSTAGETIME", type_read) == 0 ||
		     strcmp("SPECTIME",      type_read) == 0)
	       *record_type = TEMPLATE_SPECTIME;
	    
	    else if (strcmp("LITERAL", type_read) == 0)
	       *record_type = TEMPLATE_LITERAL;
	    
	    else if (strcmp("FP_ID", type_read) == 0)
	       *record_type = TEMPLATE_FPID;
	    
	    else if (strcmp("LOCID", type_read) == 0)
	       *record_type = TEMPLATE_LOCID;
	    
	    else if (strcmp("GRPNAME", type_read) == 0)
	       *record_type = TEMPLATE_GRPNAME;
	    
	    else if (strcmp("MISCWRT", type_read) == 0)
	       *record_type = TEMPLATE_MISCWRT;
	    
	    else if (strcmp("MSGDATA", type_read) == 0)
	       *record_type = TEMPLATE_MSGDATA;
	    
	    else if (strcmp("ACTION_BEGIN", type_read) == 0)
	       *record_type = TEMPLATE_ACTIONBEGIN;
            
	    else if (strcmp("ACTION_END", type_read) == 0)
	       *record_type = TEMPLATE_ACTIONEND;	       
	    
	    else if (strcmp("EVENT_BEGIN", type_read) == 0)
	       *record_type = TEMPLATE_EVENTBEGIN;
	    
	    else if (strcmp("EVENT_END", type_read) == 0)
	       *record_type = TEMPLATE_EVENTEND;         
	    
	    else
	    {
	       if (strlen(type_read) < 280)
		  sprintf(msgstr, "%s section, line %d: %s",
			  section_str, linenum, type_read);
	       else
		  sprintf(msgstr, "%s section, line %d: (line too long)",
			  section_str, linenum);
	       
	       log_msg(BAD_RECORD_TYPE, msgstr);
	       *record_type = TEMPLATE_COMMENT;
	    }
	    
	    
	    /* remove the record type definition portion of the string; 
	       do this by getting the length of the extracted token string, 
	       then skipping two bytes (i.e. the characters :\0 ); this will 
	       allow later processing to deal only with the relevant part
	       of the string */
	    
	    typelen = strlen(type_read);
	    strcpy(fileline, (type_read + (typelen + 1)) );
	 }
	 
	 else
	 {
	    sprintf(msgstr, "%s section, line %d: %s",
		    section_str, linenum,
		    " fileline in determine_template_record_type");
	    
	    log_msg(NO_RECORD_CONTENT, msgstr);
	    *record_type = TEMPLATE_COMMENT;
	 }   	 
      }      
   }
   
   return;
}
