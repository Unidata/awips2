/*
	File:		WordWrap.c
	Date:		4/14/1997
	Author:		Paul Taylor
	
	Purpose:	Provide support for performing word wrapping on text.

	History:	Russell Erb	08/06/2001
			removed unused variable `index'
*/


#include <stdio.h>
#include "GeneralUtil.h"
#include "WordWrap.h"


#define DEBUG_MODE	0	/* 0=False, 1=True */


char*	Text_WordWrap(char* buffer, int num_cols, int left_margin)
{
   static char	buf[TEXT_MAX_WORDWRAP];
   
   static char	readbuf[TEXT_MAX_WORDWRAP];
   int		rbuf_len;
   int		rbuf_pos;
   char*	rbuf_ptr = NULL;
   
   char		readstr[TEXT_BUFSIZ];
   int		rstr_len;
   
   int		remaining;  /* num of spaces remaining */
   int		adj;


   /*
   	Init.
   */
   memset(buf, '\0', sizeof(buf));
   memset(readstr, '\0', sizeof(readstr));
   strcpy(readbuf, Text_RemoveEmbeddedNewlines(buffer));
   rbuf_len = strlen(readbuf);

   /* Test to determine if the input buffer is NULL */
   if ( buffer == NULL )
   {
      return buf ;
   }
/*
printf("---WordWrap---BEFORE\n<%s>, rbuf_len == %i, num_cols == %i\n",
	  readbuf, rbuf_len, num_cols);
*/

   
   /*
   	Loop through and concatenate all of the strings in readbuf,
	appending a '\n' character to the end of a "line" of text.
   */
   remaining = num_cols;
   adj = 0;
   for (rbuf_pos=0; rbuf_pos<rbuf_len; rbuf_pos+=rstr_len)
   {
      /*
           Get a copy of the new "string".
	   Compute the length of it.
      */
      rbuf_ptr = Text_sgets(&readbuf[rbuf_pos]);
      strcpy(readstr, rbuf_ptr);
      rstr_len = strlen(readstr);

      /*
           String is longer than num_cols.
	   Output only as much as the num_cols.
      */
      if (rstr_len > num_cols)
      {
	 strncpy(&buf[rbuf_pos + adj], readstr, num_cols);
	 buf[rbuf_pos + num_cols + adj] = '\n';	/* finish out the "line" */
	 adj++;

	 /* add only the num_cols in next loop! */
	 
	 rbuf_pos += (num_cols - rstr_len);
	 remaining = num_cols;
	 continue;
      }


      /*
           Find out how many spaces are remaining
	   and select the appropriate branch to follow.
	   Finish out by copying readstr into buf.
      */
      remaining -= rstr_len;   
      if (remaining < 0)
      {
	 /*
	      Too much to fit on the given "line".
	      Put the string on the next "line".
	 */
	 buf[rbuf_pos + adj] = '\n';		/* finish out the "line" */
         adj++;

	 strncpy(&buf[rbuf_pos + adj], readstr, rstr_len);
	 remaining = num_cols - rstr_len;
	 continue;
      }
      else if (remaining == 0)
      {
	 /*
	      Just enough to fit on the given "line".
	 */
	 strcat(readstr, "\n");			/* finish out the "line" */
	 strcpy(&buf[rbuf_pos + adj], readstr);
	 adj++;

	 remaining = num_cols;
	 continue;
      }

      strcpy(&buf[rbuf_pos + adj], readstr); /* copy readstr */
   }


   strcpy(buf, Text_RemoveLeadingWhitespace(buf));
   if (left_margin > 0)
      strcpy(buf, Text_AddLeftMargin(buf, left_margin));
/*
printf("---WordWrap---AFTER\n");
printf("%s",buf);
*/
   return(buf);
}


char*	Text_sgets(char* buffer)
{
   static char	buf[TEXT_MAX_WORDWRAP];
   
   int		i;
   int		str_flag = 0; /* assume False */

   /*
   	Init.
   */
   memset(buf, '\0', sizeof(buf));
   strcpy(buf, buffer);
   
   for(i=0; i<TEXT_MAX_WORDWRAP; i++)
   {
      if(buf[i] == ' ')
      {
	 if (str_flag)
	 {
	    buf[i] = '\0';
	    return(buf);
	 }
      }
      else
	 str_flag = 1; /* there is a string */
   }
   buf[TEXT_MAX_WORDWRAP-1] = '\0';
   return(buf);
}


char*	Text_RemoveEmbeddedNewlines(char* buffer)
{
   static char	input[TEXT_MAX_WORDWRAP];	/* input buffer */
   static char  output[TEXT_MAX_WORDWRAP];	/* output buffer */
   int		i = 0;
   int		j = 0;

   /*
   	Init.
   */
   memset(input, '\0', sizeof(input));
   memset(output, '\0', sizeof(output));

   if ( buffer == NULL )
   {
      return output ;
   }

   strcpy(input, buffer);
   
   /*
   	Process the input buffer.
   */
   while(input[i] != '\0')
   {
      while ((input[i] != '\n') && (input[i] != '\0'))
      {
	 output[j] = input[i];
	 i++;
	 j++;
      }
      
      if (input[i] == '\0')
	 break;
      else
      {
	 if(strlen(output) > 1)
	 {
	    output[j] = ' ';		/* replace newline with a space */

	    if(output[j-1] == '.')
	    {
	       output[j+1] = ' ';	/* add a 2nd space after a period */
	       j++;
	    }
	    
	    j++;
	 }
	 
	 i++;
      }
   }
   
   output[j] = '\0';
   return(output);
}


char*	Text_RemoveLeadingWhitespace(char* buffer)
{
   static char	input[TEXT_MAX_WORDWRAP];	/* input buffer */
   static char  output[TEXT_MAX_WORDWRAP];	/* output buffer */
   int		i = 0;
   int		j = 0;

   /*
   	Init.
   */
   memset(input, '\0', sizeof(input));
   memset(output, '\0', sizeof(output));
   strcpy(input, buffer);
   
   /*
   	Process the input buffer.
   */
   while(input[i] != '\0')
   {
      while (input[i] == ' ')
	 i++;
      
      while ((input[i] != '\n') && (input[i] != '\0'))
      {
	 output[j] = input[i];
	 i++;
	 j++;
      }
      
      if (input[i] == '\0')
	 break;
      else
      {
	 output[j] = '\n';
	 i++;
	 j++;
      }
   }
   
   output[j] = '\0';
   return(output);
}


char*	Text_AddLeftMargin(char *buffer, int left_margin)
{
   static char	spacebuf[BUFSIZ];	/* spaces to add after every '\n' */

   static char	input[TEXT_MAX_WORDWRAP];	/* input buffer */
   static char  output[TEXT_MAX_WORDWRAP];	/* output buffer */
   int		i = 0;
   int		j = 0;
   int		sb_len;
   int		count;

   /*
   	Init.
   */
   memset(input, '\0', sizeof(input));
   memset(output, '\0', sizeof(output));
   memset(spacebuf, '\0', sizeof(spacebuf));
   memset(spacebuf, ' ', left_margin);
   strcpy(input, buffer);
   sb_len = strlen(spacebuf);

   /*
   	Process the input buffer.
   */
   while(input[i] != '\0')
   {
      while ((input[i] != '\n') && (input[i] != '\0'))
      {
	 output[j] = input[i];
	 i++;
	 j++;
      }

      if (input[i] == '\0')
	 break;
      else
      {
	 output[j] = input[i]; /* copy '\n' character */
	 i++;
	 j++;
	 
	 strncpy(&output[j], spacebuf, sb_len);
	 
	 count = sb_len;
	 while(count > 0)
	 {
	    j++;
	    count--;
	 }
      }
   }
   
   output[j] = '\0';
   return(output);
}


void	Text_GetLine(char** text, int num_cols, char** line_str)
{
   char*	next_text = NULL;	/* used in Objective #1 */

   char*	text_cpy = NULL;	/* used in Objective #2 */
   char		tmp_line_str[TEXT_BUFSIZ];	/* used in Objective #2 */
   
   int		i;

   /*
        Test to see if *test is NULL. 
   */
   if ( * text == NULL )
   {
      return ;
   }
   
   /*
   	Goal: Make a copy of text.
   */
   Text_AddString(&text_cpy, *text, 1);

   
   /*
   	Goal: Find the next '\n' or '\0' character in "text".

   	Next, divide the "text" into 2 pieces (based on the first
	'\n' or '\0' that is encountered).
   */

   i = 0;
   while (   ( (*text)[i] != '\n' ) &&
	     ( (*text)[i] != '\0' )   )
   {
      i++;
   }
   
   
   /*
   	Objective #1: Prepare for next function call through "text".
   	(For integrity reasons, let's use the "text" version of text).
   */
   if ( (*text)[i] == '\n' )
   {
      Text_AddString(&next_text, &((*text)[i+1]), 1);
      Text_FreeString(text); /* free old version of text (and set to NULL) */
      *text = next_text;
   }
   else /* a '\0' was found */
   {
      Text_FreeString(text); /* free old version of text (and set to NULL) */
   }

   
   /*
   	Objective #2: Return desired line through "line_str".
   	(For integrity reasons, let's use the "text_cpy" version of text).
   */
   memset(tmp_line_str, '\0', sizeof(tmp_line_str));   
   strncpy(tmp_line_str, text_cpy, i);
   Text_FreeString(&text_cpy);
   Text_AddString(line_str, tmp_line_str, 1);
   

   return;
}


int	Text_CountNewLines(char* text)
{
	int	i = 0;
	int	j = 0;

        if ( text == NULL ) return 0 ;

	while(1)
	{
	   if(text[i] == '\0') break;
	   if(text[i] == '\n') j++;
	   i++;
	}
	
	return(j);
}


void	Text_Paginate(char** text)
{
	char*	nl_buf = NULL;
	int	nl_bytes = 0;
	int	nl_count = 0;

	int	leftover = 0;
	int	newlines = Text_CountNewLines(*text);

        if ( *text == NULL ) return ;

#if (DEBUG_MODE)
printf("in Text_Paginate()...\n");
#endif
	
	/*
		Find number of "lines" leftover on last page.
	*/
	leftover = newlines;
	while(leftover > TEXT_LINES_PER_PAGE)
	   leftover -= TEXT_LINES_PER_PAGE;
	if(leftover == 0)
	   return;
	
	/*
		Necessary to add newlines:

		(Start with original text.)
		Create a string of newlines.
		Add string of newlines to original text to get "new" text.
	*/
	nl_count = TEXT_LINES_PER_PAGE - leftover;
	nl_bytes = (nl_count * sizeof(char));
	nl_buf = (char *) malloc (nl_bytes + 1);
	memset(nl_buf, '\n', nl_bytes);
	nl_buf[nl_count] = '\0';

	Text_AddString(text, nl_buf, 0);
	Text_FreeString(&nl_buf);
	       

	return;
}
