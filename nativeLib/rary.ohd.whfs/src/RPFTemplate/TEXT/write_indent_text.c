
#include <time.h>   
#include <stdio.h>  
#include <string.h>
#include <ctype.h>

#include "format_vals.h"    /* function prototypes */


/*********************************************************************
   write_indent_text()
   
   PURPOSE
   Writes the product text starting with 2 spaces to the output product.
   Especially used in Headline section.
   
   NOTES
   Although this function uses a string buffering technique,
   for each call to this function with a string, the string
   will always be output before exiting the function.
   
   Note that all newlines characters are suppressed unless they
   are in the first character position.  If any characters follow
   a newline in the first position, those characters are ignored.
   Newlines should be added explicitly by the calling functions, although   
   a special character sequence is recognized that will force a newline.   
   Multiple blanks are not compressed by this function.
   
   Key variables:
   
   column - the location on the output device
   phrase_pos - the location in the input phrase string
   
   
   *******************************************************************/

void write_indent_text(int		textcase_type,
		       char 		*rawphrase,
		       FILE 		*outfile_ptr)
{
   char		*phrase = NULL;
   char		*token  = NULL;
   int  	rawphrase_len, phrase_len;
   int		token_len;
   int		num_trailing_blanks, forced_nl_found;
   static int 	 bcolumn = 0;
   int 		phrase_pos;
   int 		status, i, j;
   int		max_line_width;
 
   
   /* for informational use */
   
   log_msg("p", rawphrase);
       
   
   /* get the line width */
   
   max_line_width = get_line_width();
   
   
   
   /* convert to uppercase if requested */
   
   if (textcase_type == CASE_FORCEUPPER)
   {
      convert_str_to_upcase(rawphrase);
   }
   
      
   /* copy the raw phrase to a new string, stripping off
      the newlines in the process */
   
   rawphrase_len = strlen(rawphrase);
   phrase = (char *)malloc(sizeof(char) * (rawphrase_len + 1));
   memset(phrase, 0, (rawphrase_len + 1));
   
   phrase[0] = rawphrase[0];
   j = 1;
   for (i = 1; i < rawphrase_len; i++)
   {
      if (strncmp(&rawphrase[i], "\n", 1) != 0)
      {
	 phrase[j] = rawphrase[i];
	 j++;
      }
   }
   phrase_len = strlen(phrase);
   
   
   /* check if the character being sent in is a newline 
      character, in the first column of course! */
   
   if (phrase[0] == '\n')
   { 
      status = fputs("\n", outfile_ptr);
      bcolumn = 0;
      
      return;
   }
   
   /*add new line character before each bullet text*/
   
 /*    status = fputs("\n", outfile_ptr);*/
   
     
   /* add "*" at the begining of bullet text*/
   
   status = fputs("  ", outfile_ptr);
   bcolumn = 1;        	      
	        
   /* loop on the 'tokens' in the input phrase, where a token is a
      run of characters ending with a blank, or three dots, or
      the designated forced-newline token. 
      the returned token includes the trailing blank(s) */
   
   phrase_pos = 0;
   
   while ((token = get_phrase_token(phrase, phrase_pos,
				    &token_len,
				    &num_trailing_blanks,
				    &forced_nl_found)) != (char *)NULL)
   {
      
      /* set the new position for the next time thru the loop.
	 this is set here at the top of the loop since the token_len
	 variable may be adjusted below to account for the
	 forced new line characters. */	 
      
      phrase_pos = phrase_pos + token_len;
      
      
      /* if foreced newline found, then null out the special sequence of
	 characters so they will not actually be output and adjust the
	 token_len so the special sequence will not itself be written
	 if it is at the end of regular text in the returned token. */
      
      if (forced_nl_found)
      {
	 memset(&token[token_len - FORCED_NL_NUMCHARS], 0, FORCED_NL_NUMCHARS);
	 token_len = token_len -  FORCED_NL_NUMCHARS;
      }
      
      
      /*process token */
     
      if (token_len > 0)
      {
	 
	 /* if there is room in the current output line for this
	    token, then write it */
	 

	 if (bcolumn + token_len <= max_line_width)
	 {
	    /*do not fputs token if it is whole blank*/
	    
	    if (token[0] != ' ')
	       status = fputs(token, outfile_ptr);
	    
	    bcolumn = bcolumn + token_len;
	 }

	 
	 /* if there is no room, then check if there is room if one
	    did not write trailing blanks.  if so, then write it. 
	    otherwise start a new line and write the token on that line */
	 
	 else
	 {
	    if (bcolumn + token_len - num_trailing_blanks <= max_line_width)
	    {
	    
	       for (i = 0; i < token_len - num_trailing_blanks; i++)
	       {
	          if (token[0] != ' ')	       
		    status = fputc(token[i], outfile_ptr);
	       }
	       status = fputs("\n", outfile_ptr);
	       
	       /*put two spaces before the bullet text for the second line and
	       so on*/
	       
	       status = fputs("  ", outfile_ptr);
	       bcolumn = 2;
	    }
	    
	    else
	    {
	       status = fputs("\n", outfile_ptr);
	       
	       /*put two spaces before the bullet text for the second line and
	           so on*/	       	      
	       
	       status = fputs("  ", outfile_ptr);		  
	       status = fputs(token, outfile_ptr);
	       bcolumn = token_len + 2;
	       
	       if (bcolumn > max_line_width)
	       {
		  log_msg(EXCEED_STRING, "while writing indent text output");
	       }
	    }	    
	 }
      }
      
      
      /* if the returned token string included the forced
	 newline sequence, the output a new line character */
      
      if (forced_nl_found)
      {
	 status = fputs("\n", outfile_ptr);
	 status = fputs("  ", outfile_ptr);
	 bcolumn = 1;
      }

      
      /* free the memory allocated by the get_phrase_token function */
      
      if (token != NULL)
         free(token);
      else
      {
         log_msg(FREE_NULL_MEMORY, "in write_indent_text(), token");
      }
   }
   
   
   /*add new line character at the end of each bulletstr*/
   
   status = fputs("\n", outfile_ptr);
   
   /* flush the buffer to ensure that the contents are actually written */
   
   fflush(outfile_ptr);
   
   if (phrase != NULL)
      free(phrase);
   else
   {
      log_msg(FREE_NULL_MEMORY, "in write_indent_text(), phrase");
   }
   
   return;
}


