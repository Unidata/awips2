
#include <time.h>   
#include <stdio.h>  
#include <string.h>
#include <ctype.h>

#include "format_vals.h"    /* function prototypes */


/*********************************************************************
   write_phrase_text()
   
   PURPOSE
   Writes the product text/phrases to the output product.
   
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
   output_pos - the location in the output string 
   phrase_pos - the location in the input phrase string
   
   
   *******************************************************************/

void write_phrase_text(int		textcase_type,
		       char 		*rawphrase,
		       FILE 		*outfile_ptr)
{
   char		*phrase = NULL;
   char		*token  = NULL;
   int  	rawphrase_len, phrase_len;
   int		token_len;
   int		num_trailing_blanks, forced_nl_found;
   static int 	column = 0;
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
      column = 0;
      
      return;
   }
      
   
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
      
      
      /* process the token */
     
      if (token_len > 0)
      {
	 
	 /* if there is room in the current output line for this
	    token, then write it */
	 

	 if (column + token_len <= max_line_width)
	 {
	    status = fputs(token, outfile_ptr);
	    column = column + token_len;
	 }

	 
	 /* if there is no room, then check if there is room if one
	    did not write trailing blanks.  if so, then write it. 
	    otherwise start a new line and write the token on that line */
	 
	 else
	 {
	    if (column + token_len - num_trailing_blanks <= max_line_width)
	    {
	       for (i = 0; i < token_len - num_trailing_blanks; i++)
	       {	       
		  status = fputc(token[i], outfile_ptr);
	       }
	       status = fputs("\n", outfile_ptr);
	       column = 0;
	    }
	    
	    else
	    {
	       status = fputs("\n", outfile_ptr);
	       status = fputs(token, outfile_ptr);
	       column = token_len;
	       
	       if (column > max_line_width)
	       {
		  log_msg(EXCEED_STRING, "while writing phrase output");
	       }
	    }	    
	 }
      }
      
      
      /* if the returned token string included the forced
	 newline sequence, the output a new line character */
      
      if (forced_nl_found)
      {
	 status = fputs("\n", outfile_ptr);
	 column = 0;
      }

      
      /* free the memory allocated by the get_phrase_token function */
      
      if (token != NULL)
         free(token);
      else
      {
         log_msg(FREE_NULL_MEMORY, "in write_phrase_text(), token");
      }
   }
   
   
   /* flush the buffer to ensure that the contents are actually written */
   
   fflush(outfile_ptr);
   
   if (phrase != NULL)
      free(phrase);
   else
   {
      log_msg(FREE_NULL_MEMORY, "in write_phrase_text(), phrase");
   }
   
   return;
}


/***********************************************************************
   
   Given a phrase string and its starting position, grab the next token.
   always return trailing blanks and trailing dots (ellipsis).
   the phrase position uses an index that starts at 0, not 1.
   
   If the forced new line sequence is found, it is treated the same as the
   ellipsis in that it is returned with any preceding token; the calling
   function then must treat any part preceding the forced new line part
   separate from the forced new line characters.
   
   return the actual token string as the function return value.
   return the token length and the number of trailing blanks as arguments.
   
   this function allocates the character space for the token;
   the calling program is responsible for freeing this memory.
   
   ************************************************************************/

char * get_phrase_token(char 	*phrase,
			int  	phrase_pos,
			int	*return_token_len,
			int	*return_num_trailing_blanks,
			int	*return_forced_nl_found)
{  
   char	*token;
   int	token_len;
   int	num_trailing_blanks;
   int	forced_nl_found;
   
   int	phrase_len, i;
   int	NO_BLANKS_FOUND = -1;
   int	blank_pos;
      
   
   /* initialize */
   
   token = (char *)NULL;
   token_len           = 0;
   num_trailing_blanks = 0;
   forced_nl_found     = 0;
   blank_pos = NO_BLANKS_FOUND;
   
   
   /* check that there is any phrase left to check */
   
   phrase_len = strlen(phrase);
   if (phrase_pos >= phrase_len)
      return(token);
      
   
   /* loop on the characters in the string */
   
   for (i = phrase_pos; i < phrase_len; i++)
   {      
      /* now search for the first blank, three dots 
	 or the forced-newline sequence */
      
      if (blank_pos == NO_BLANKS_FOUND)
      {
         if (phrase[i] == ' ')
            blank_pos = i;         
	 
         else if (strncmp(&phrase[i], "...", 3) == 0)
         {
            token_len = i - phrase_pos + 3;
            break;
         }
	 
         else if (strncmp(&phrase[i], FORCED_NL_CHARS, FORCED_NL_NUMCHARS) == 0)
         {
            token_len = i - phrase_pos + FORCED_NL_NUMCHARS;
	    forced_nl_found = 1;
            break;
         }
      }
      
      
      /* if already found a blank character, then see if there
         are more following it.  when finding the end, return
         with the values */
      
      else if (blank_pos != NO_BLANKS_FOUND) 
      {
         if (phrase[i] != ' ')
         {
            token_len = (i - 1) - phrase_pos + 1;
            num_trailing_blanks = i - blank_pos;
            break;
         }
      }  
   }
   
   /* if the token ends with the end of the phrase, then assume that
      we need to return with the whole string. */
   
   if (token_len == 0)
   {
      token_len = phrase_len - phrase_pos;
      if (blank_pos != NO_BLANKS_FOUND)
         num_trailing_blanks = phrase_len - blank_pos;
   }
   
      
   /* set and return with the values */
   
   token = (char *)malloc(sizeof(char) * (token_len +1));
   memset(token, 0, (token_len + 1));
   strncpy(token, &phrase[phrase_pos], token_len);
     
   *return_token_len           = token_len;
   *return_num_trailing_blanks = num_trailing_blanks;
   *return_forced_nl_found     = forced_nl_found;
   
   
   return(token);
}


/*********************************************************************
  
   write_line_text()
   
   Write a line of text, without any concern for word wrap, newlines,
   etc.
   
   Note that this function does not convert the output to uppercase.
   
   *******************************************************************/
void write_line_text(char 	*rawline,
		     FILE 	*outfile_ptr)
{
   
   fprintf(outfile_ptr, rawline);
   
   return;
}


/*********************************************************************
  
   get_line_width()
   
   Get the line width from the user token.
   
   *******************************************************************/
int get_line_width()
{
   int  tokenlen=0, rlen=0;
   int	istatus=0;
   char	linewidth_str[10];
   
   static int linewidth;
   static int first = 0;
   

   /* to save access time, only do this the first time called */
   
   if (first == 0)
   {
      tokenlen = strlen("rpf_linewidth");
      istatus = get_apps_defaults("rpf_linewidth", &tokenlen,
                                  linewidth_str, &rlen);


      if (istatus != 0)
         linewidth = DEFAULT_LINE_WIDTH;
      else
         linewidth = atoi(linewidth_str);
	 
      first = 1;
   }
      
   return(linewidth);
}
