/*********************************************************************
   create_headline_section.c
   
   create_headline_section()

*******************************************************************/

#include <stdio.h>             /* standard io library functions */
#include <string.h>            /* library string functions */
#include <stdlib.h>            /* standard library functions */

#include "create_headline_section.h"

extern char paramdir[];
long int end_pos;

/*********************************************************************
   create_headline_section()
   
   PURPOSE
   Create the headline section of the product. 
   
   NOTES
   
   *******************************************************************/
void create_headline_section(const int		   numfps,
			    fp_struct		 *fp,
			    const int	           numgrps,
			    grp_struct		 *grp,
			    const int              numcnty,
			    county_struct        *cnty,
			    pcc_struct		 *pcc, 
			    misc_struct		 *misc,
			    vtecinfo_struct      *vtecinfo,
			    template_info_struct *template_info,
			    FILE 		 *outfile_ptr) 
{  
             
   FILE *file_ptr;
   char *fgets_ptr;
   char template_file[MAXLEN_FILENAME];
   char *name_read;
   char rawline[MAXLEN_TEMPLATEREC];
   char fileline[MAXLEN_TEMPLATEREC];
   char *newphrase;
   int 	record_type, name_found, record_expected;
   int 	fpindex1, fpindex2;
   char msgstr[200];
   int  action_block, event_block;
   int  fp_byaction_included, phrasenum;
   char action_str[MAXLEN_TEMPLATEREC];
   int  section_index = HEADLINE;
   int  linenum;
   int  bullet_1st = TRUE, indent_1st = TRUE;
   char *action_token = NULL;
   int 	line_written;
   char template_name[MAXLEN_TEMPLATENAME];
   long int pos, begin_pos;  
   int grpindex, fp_included_eventblock;
   int i, j;
   
   /* need to free the memory allocated for the stacks and the
      phrases in the previously loaded template */
   
   free_template_memory(template_info);
             
   /* initialize */
   
   record_expected = TEMPLATE_NAME;
   name_found = FALSE;
   linenum = 0;
   pos = begin_pos = 0;
   
   template_info->format.num_of_formats = 0;
   template_info->variable.num_of_variables = 0;
  
   
   action_block = event_block = fp_included_eventblock = FALSE;
   fpindex1 = fpindex2 = MISSINGVAL;
   fp_byaction_included = FALSE;
   memset(action_str, 0, MAXLEN_TEMPLATEREC);
        
    
   /* set the template file, name, and type */
   
   sprintf(template_file, "%s/%s.%s", 
	   paramdir, HEADLINE_TEMPLATEFILE, misc->selected_office); 
   
   strcpy(template_name, pcc->headline.template);
      
   /* open the template file */
   
   file_ptr = fopen(template_file, "r");
   if (file_ptr == NULL) log_msg(FILE_OPENERR, template_file);
   
   
   /* write informational message */
   
   sprintf(msgstr, "Processing template %s from file %s...",
	  pcc->headline.template, template_file);
   log_msg("", msgstr);	
   
   /* loop until the end of the file is reached or until the
      specified template is found and read */
   
   for(;;)
   {
      /* get a line from the input template file;
	 if end-of-file, then exit the loop */
      
      fgets_ptr = fgets(rawline, MAXLEN_TEMPLATEREC, file_ptr);
      if (fgets_ptr == NULL) break;
      linenum++;
      
      /* read any continuation lines to get them out of the
	 way of subsequent calls to to determine the record type */
      
      get_continuation_lines(rawline, file_ptr, &linenum, fileline);
      
      /*get the position of file reading*/
      
      pos = ftell(file_ptr);
      
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
	         if (strcmp(name_read, pcc->headline.template) == 0) 
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
	    
	    /*process condition outside action_block*/
	      
	    else if (record_type == TEMPLATE_CONDITION)
	    {		       		    
               /*only read conditions outside action block*/			    
	       /* read the logic in the conditional expressions and load
	          it into the stack structure.  verify the syntax of the
	          conditional expressions.  now that the condition is read,
	          the template phrase is expected next */
		  
	       if (action_block == FALSE)
	       {	  
	          read_condition(fileline, file_ptr, template_info);
	          verify_condition_syntax(template_info);
	          record_expected = TEMPLATE_PHRASE;	   
	       }	  
	    }
	         	 
	       
	    else if (record_type == TEMPLATE_PHRASE || record_type == 
	             TEMPLATE_BULLETSTR || record_type == TEMPLATE_INDENTSTR)
	    {		       		   
	       read_phrase(fileline,file_ptr,template_info,record_type);  
	       	       
	       /*output phrase itself if the phrase is before action_begin: or
	       after action_end:*/
	       
	       if (action_block == FALSE && event_block == FALSE)
	       {
	          phrasenum = template_info->num_phrases -1;
		  grpindex = 0;
		  		  		    			
	           /*if there is condition, check condition first, */
			   
		   if (record_expected == TEMPLATE_PHRASE)
		   {		      
		      check_conditions(grpindex, fp, grp, numcnty, cnty, 
			               misc, vtecinfo, pcc, HEADLINE,
				       template_info);

		      if (template_info->include_phrase[template_info->num_conditions-1]
		                  == TRUE)
		      {			       
			 load_phrase_data(grpindex,HEADLINE, fp, grp, 
				          numcnty,cnty, misc, vtecinfo,
					  phrasenum,template_info, pcc,
					  &newphrase);

		         write_phrase_bullindent_text(template_info, pcc,
			  newphrase, outfile_ptr, phrasenum, bullet_1st,
			                       indent_1st);	

		      }
		      
		      record_expected = TEMPLATE_NAME;
		    }
		    else
		    {
		       load_phrase_data(grpindex, HEADLINE, fp, grp, numcnty, cnty,
		                   misc, vtecinfo, phrasenum, template_info,
				   pcc, &newphrase);
				   
		       write_phrase_bullindent_text(template_info, pcc, newphrase,
		                          outfile_ptr, phrasenum, bullet_1st,
			                               indent_1st);		
                    }						       	   
	         
	       }        
	       /*process phrases within action_block. This feature is only for
	           vtec flag is set and for the segmented by forecast point*/	       
	       else 
	       {
	          		  
		  if (pcc->product.vtec_flag == TRUE &&
		      pcc->product.segment_mode == SEGMENT_MODE_POINT)
		  {	
		         	   		     
	             /*process phrases is between "action_begin:" and
		         "event_begin:" */ 
		       
	             if (action_block == TRUE && event_block == FALSE)
		     {
		     	for (fpindex1 = 0; fpindex1 < numfps; fpindex1++)
			{
			   if ((misc->fps_included[fpindex1] == TRUE) && 
			       (action_str != NULL) &&
        		    (strcmp(vtecinfo[fpindex1].action, action_str)==0))	
			   {
			      fp_byaction_included = TRUE;
			      break;
			   }        
			   
			}
						
			
			if (fp_byaction_included)
			{
			   phrasenum = template_info->num_phrases -1 ;
	                   			  
			   load_phrase_data(fpindex1,HEADLINE, fp, grp, 
				              numcnty,cnty, misc, vtecinfo,
					      phrasenum,template_info, pcc,
					      &newphrase);

		           write_phrase_bullindent_text(template_info, pcc,
			      newphrase, outfile_ptr, phrasenum, bullet_1st,
			                           indent_1st);						       	 
			    			    			       
			}   
		     }		   		     		    
		     		    	     		         	     	   
		  }/*end of vtec mode and segmented by point mode */   
	       }/*end of action_block*/ 	     		   
	    } /*end of TEMPLATE_PHRASE*/
	    
	    /* if the line defines the format to use, load the info */

	    else if (record_type == TEMPLATE_FORMAT)
	    {
	       /*read format outside event block*/
	       
	       if (event_block == FALSE)
	          read_format(fileline, &template_info->format);
            } 
	    
	    /* if the line defines the variables to use, load the info */

	    else if (record_type == TEMPLATE_VARIABLE)
	    {
	       /*read varlist ouside event block*/
	       
	       if (event_block == FALSE)
	          read_varlist(fileline, &template_info->variable);
	    }	  

	    else if (record_type == TEMPLATE_ACTIONBEGIN)
	    {		       	       
                action_block = TRUE; 
		event_block = FALSE;
		       	      
		fp_byaction_included = FALSE;
               
                 
		/*get the action code*/

		action_token = strtok(fileline, " \t\n");
		if (action_token != NULL)
		{	        		
		   strcpy(action_str, action_token);
		   convert_str_to_upcase(action_str);
		}     	  
		else
		{	               
		   sprintf(msgstr, "Warning - Undefined action in template %s "
	                           "from file %s...",
	                           pcc->headline.template, template_file);
	           log_msg("", msgstr);	  
		} 	  
                
	    }   

	    else if (record_type == TEMPLATE_EVENTBEGIN)
	    {		       	         	                  
	       event_block = TRUE;	      	       
               begin_pos = pos;
	       fp_included_eventblock = FALSE;
		  
	       /*check if  action_block is TRUE*/

	       if (action_block == FALSE)
	       {
	          sprintf(msgstr, "Warning - No keyword ACTION_BEGIN "
		                  "defined before EVENT_BEGIN.");
		  log_msg("", msgstr);  

		  event_block = FALSE; 
	       }
	       else
	       {
	          /*process event block only when vtec flag is set and
		       segmented by forecast point */
		  
		  if (pcc->product.vtec_flag == TRUE &&
		      pcc->product.segment_mode == SEGMENT_MODE_POINT)
		  {		  		 		        	          
	            for (i = 0; i < numgrps; i++)
		    {
		     for (j = 0; j< grp[i].numfps; j++)
		     {
		       fpindex2 = grp[i].fpindex[j];
		          
		       line_written = FALSE;

		       if ((misc->fps_included[fpindex2] == TRUE) && 
			   (action_str != NULL) &&
                           (strcmp(vtecinfo[fpindex2].action, action_str)==0))
                       {  
			   fp_included_eventblock = TRUE;
			   
			   process_event_block(fpindex2, HEADLINE, fp, grp,
			                     numcnty, cnty, misc, vtecinfo,
					     template_info, template_name, pcc, 
					     file_ptr, outfile_ptr, &line_written,
					     begin_pos); 
		       }
		     }/*end of loop of forecast points within one group*/     			
		    }/*end of loop of groups */
		    
		    if (fp_included_eventblock == TRUE)
		    {		       
		       event_block = FALSE;
		       if (end_pos != 0 && end_pos > begin_pos)
		          fseek(file_ptr,end_pos,SEEK_SET);
		       else
		       {
		         sprintf(msgstr, "No keyword event_end: found or check "
			                 "the event block for %s "
		                         "in headline section", template_name);
		         log_msg("", msgstr);
		       }
		    }
		    		       	 
		  }   
		  else
		  {
		     sprintf(msgstr, "For processing event block, it should be "
		                     "VTEC mode and segmented by point.");
                     log_msg("",msgstr);
		  }   				                       
	       }	  
	        	  
	    } /*end of event_begin*/    
	    else if (record_type ==  TEMPLATE_EVENTEND)
	    {
	      	          	       	       
	       /*check if  action_block is already set*/

	       if (action_block == FALSE)
	       {
	          sprintf(msgstr, "Warning - No keyword ACTION_BEGIN "
		                  "defined before EVENT_END.");
		  log_msg("", msgstr);  		  		  
		  	  

	       }
	       
               /*check if  event_block is already set*/

	       if (event_block == FALSE)
	       {
	          sprintf(msgstr, "Warning - No keyword EVENT_BEGIN "
		                  "defined before EVENT_END.");
		  log_msg("", msgstr);  		  		  
		  	  

	       }
	       
	       event_block = FALSE;	      	          	  	       	  		  	       	    
               fp_included_eventblock = FALSE; 
	       
	    }  
	    else if (record_type == TEMPLATE_ACTIONEND)
	    {	    
	      	       	       
	       /*check if  "ACTION_BEGIN" is read*/

	       if (action_block == FALSE)
	       {
	          sprintf(msgstr, "Warning - No keyword ACTION_BEGIN "
		                  "defined before ACTION_END.");
		  log_msg("", msgstr);  
		  event_block = FALSE; 

	       } 

	       action_block = FALSE;
	       	 
	    }              

	    /* if unexpected template record, issue message and abort */

	    else
	       log_msg(INVALID_RECORD_TYPE, fileline);

	 }/*end of name is found*/
      }/*end of non-comment line*/
   }/*end of for loop reads*/
   
      	 
   /* if never found the template name, issue error message and abort */

   if (name_found == FALSE)
     log_msg(TEMPLATENAME_NOTFOUND, pcc->headline.template);


   /* close the file */

   fclose(file_ptr);
            
  
return;
}
   	      	    
/****************************************************************************
write_phrase_bullindent_text()

Purpose: write phrase or bulletstr or indentstr based on the flag set.
*****************************************************************************/
void write_phrase_bullindent_text(template_info_struct  *template_info,
                                  pcc_struct            *pcc,
				  char                  *newphrase, 
				  FILE                  *outfile_ptr,
				  const int             phrasenum,
				  int                   bullet_1st,
				  int                   indent_1st)
{        
    /*using of "bulletstr" keyword to format bullet 
       text*/

    if (template_info->bulletstr_flag[phrasenum] == 1)  
    {       
        write_bullet_text(pcc->product.tcase, newphrase,
			  outfile_ptr);
    }

    /*using of "indentstr" keyword to format indent
      text*/

    else if (template_info->indentstr_flag[phrasenum] == 1)
    {
       
       write_indent_text(pcc->product.tcase, newphrase,
			  outfile_ptr);
    }	     	 
    else   
    {       
       if (newphrase[0] != '\n')
       {
         write_phrase_text(pcc->product.tcase, newphrase,
			 outfile_ptr);
   	 write_phrase_text(pcc->product.tcase, "\n",	     
			 outfile_ptr);		
       }
       else
	 write_phrase_text(pcc->product.tcase, newphrase,
			 outfile_ptr);			        
               
    }					  

return;

}


/**************************************************************
process_event_block()

process the conditions and phrases within the keywords "event_begin:"
and "event_end:" for each included forecast points with the
same action

***************************************************************/
void process_event_block(const int             fpindex,
                         const int             product_section,
		         fp_struct             *fp, 
			 grp_struct            *grp,
                         const int  	       numcnty,
		         county_struct         *cnty,
		         misc_struct	       *misc,
		         vtecinfo_struct       *vtecinfo,
		         template_info_struct  *template_info,
		         const char            template_name[],
		         pcc_struct            *pcc,	
		         FILE                  *file_ptr,	         
		         FILE                  *outfile_ptr,
			 int                   *line_written,
		         long int              begin_pos)
{
   int phrasenum;
   char *newphrase;
   int  bullet_1st = TRUE, indent_1st = TRUE;
   
 
   
   /* get and load all the information within the event_block */
   
   buf_event_info(template_info, template_name, file_ptr,
		  product_section, begin_pos);
		  
				 
   /* check the result of the condition */
   
   check_conditions(fpindex, fp, grp, numcnty, cnty, misc, vtecinfo, pcc, 
                    product_section,template_info);
		    
		    
   /* loop on each of the phrases and if the associated condition is
      true, then load the data into the variables contained in the phrase
      itself */

   for (phrasenum = 0; phrasenum < template_info->num_phrases; ++phrasenum)
   {
      if (template_info->include_phrase[phrasenum] == TRUE)
      {
	 load_phrase_data(fpindex, product_section, fp, grp, numcnty, cnty,
	                  misc, vtecinfo, phrasenum, template_info, pcc, 
			  &newphrase);
			  
	 if (fp[fpindex].fcstpoint_type == TIDAL)
	 {
	    convert_str_to_upcase(newphrase);
	    fprintf(outfile_ptr, newphrase); 
	 }
	 else
	 {
	     write_phrase_bullindent_text(template_info, pcc, newphrase, 
	                                  outfile_ptr, phrasenum, bullet_1st,
			                  indent_1st);	
         }
	 *line_written = TRUE;					  
						       
      }
   }
   
 			

   return;
}


/**************************************************************
   buf_event_info()
   
   PURPOSE
   This function buffer the info within the event block from  the template file
   and finds the specified template.
   It then calls the routines for loading the info into the structure.
   
**************************************************************/
void buf_event_info(template_info_struct    *template_info,
                   const char                template_name[],
                   FILE                    *file_ptr, 
		   const int                 product_section,
		   long int                  begin_pos)
{
   char fileline[MAXLEN_TEMPLATEREC];
   char contline[MAXLEN_TEMPLATEREC];
   char *fgets_ptr;
   int 	record_type;
   int linenum;
   long int pos;
   char fakecondition[40]="";
   char msg_str[200];
   
 /*  long int end_pos = 0;*/
   
    /* need to free the memory allocated for the stacks and the
      phrases in the previously loaded template */
   
    free_template_memory(template_info);
    
    
    /* initialize */
   
 /*   record_expected = TEMPLATE_NAME;*/
    linenum = 0;

    template_info->format.num_of_formats = 0;
    template_info->variable.num_of_variables = 0;


    /* loop until the keyword "event_end:" is found*/

    for(;;)
    {
       /* get a line from the input template  file;
	  if end-of-file or "event_end:" is found, 
	  then exit the loop; also check that the length
	  is not exceeded and read any continuation lines that may follow */

       fgets_ptr = fgets(fileline, MAXLEN_TEMPLATEREC, file_ptr);
       if (fgets_ptr == NULL) break;
       	  

       linenum++;
       get_continuation_lines(fileline, file_ptr, &linenum, contline);	

       pos = ftell(file_ptr);
       
       
       /* determine the type of record, i.e. whether it be condition, or phrase */

       determine_template_record_type(contline, product_section, linenum,
				      &record_type); 


       /* process the record depending on its type */
       /* do nothing if a comment record read */

       if (record_type == TEMPLATE_COMMENT)
	  ;


       /* if non-comment line, then process accordingly */

       else
       {	 	 	 
	  /* if the record type is a format line for the variable format;
	     this record should be coupled with a variables list */

	  if (record_type == TEMPLATE_FORMAT)
	     read_format(contline, &template_info->format);


	  /* if the record type is a values line that identifies
	     which values go with which formats */

	  else if (record_type == TEMPLATE_VARIABLE)
	     read_varlist(contline, &template_info->variable);


	  /* if a conditional expression is expected and
	     that is what has been read, process it, otherwise
	     log an error message */

	  else if (record_type== TEMPLATE_CONDITION)
	  {	   	   
	     /* read the logic in the conditional expressions and load
		it into the stack structure.  verify the syntax of the
		conditional expressions.  now that the condition is read,
		the template phrase is expected next */

	     read_condition(contline, file_ptr, template_info);
	     verify_condition_syntax(template_info);
	/*     record_expected = TEMPLATE_PHRASE;	   	  */
	  }


	  /* if a phrase or bullet phrase is expected and that is what has
	     been read, then process it, otherwise log an error message */

	  else if (record_type == TEMPLATE_PHRASE ||
		 record_type == TEMPLATE_BULLETSTR ||
		 record_type == TEMPLATE_INDENTSTR)	        
	  {	     
	     read_phrase(contline, file_ptr, template_info, record_type);

             /*if no codition before phrase, then fake the condition as
	     true-means include the phrase as always*/
	      
	     if (template_info->num_phrases > template_info->num_conditions) 	     
	     {
		strcpy(fakecondition, "TRUE");		
                read_condition(fakecondition, file_ptr, template_info);
	        verify_condition_syntax(template_info);
	     } 	

	  }	  
          
	  /* if read the keyword "event_end:", put the file pointer back to the
	     begining of this keyword and break */
	  
	  else if (record_type == TEMPLATE_EVENTEND)
	  {
	     end_pos = pos;		          
             fseek(file_ptr, begin_pos, SEEK_SET);
	     break;
	  }
	    
	      
	  /* not suitable keywords then issue warning message */

	  else
	     log_msg("", "Not suitable record type in Headline section");

	   
       }    /* end of if block for non-comment case*/
    }       /* end of for loop for reading file */


    /* if expected a conditional phrase when the template body ended,
       and no phrases found at all, then issue error message.
       for these two cases, there is no need to take any action,
       since the number of phrases and conditions will be 0 */    

    if (template_info->num_phrases == 0)
       log_msg(TEMPLATEPHRASE_NOTFOUND, template_name);


    /* if the number of formats specified is not equal to the 
       number of variables, issue a message */

    if (template_info->format.num_of_formats != 
	template_info->variable.num_of_variables)
       log_msg(FORM_VAR_MISMATCH, template_name);


    /* if the number of conditions is not equal to the number of phrases, issue a
       message*/
    
    if (template_info->num_conditions != template_info->num_phrases)
    {
       sprintf(msg_str, "The number of conditions is not equal to "
                       "the number of phrases in %s", template_name);
       log_msg("", msg_str);		        
    }   
    
    return;
 }

