/* --------------------------------------------------------------------
  
       FUNCTION
         strpat

       PURPOSE  
         Search for string patterns, using template characters,
         for numbers and/or alpha characters patterns in a buffer
	 using a template.

       VERSION and UPDATES
         1.0    DEC  95  David G. Brandon
                Original Version
         1.1    FEB 25, 96 DGB
                Allow for a phantom char to be passed over.
         1.2    NOV 28 97 DGB
                Check for endless while loops.
         1.3    DEC 06 1999 DGB
	        Add the string length of the buffer in the argument
		list.  Previous versions assummed the lf was the end
		of a line.  This length option will allow an arbitrary
		buffer of characters.
		
		Include a template character in the argument list.  
		It will be the 'either' character.  It will signify
		that the character checked in the buffer can be
		either a digit or alpha character.
		
		The phantom chararcter allows a symbol other than 
		a digit when checking for a digit.  It has no affect
		for the alpha template.
	
 *--------------------------------------------------------------------- */

#define MAX_LOOP 5000                                      /* dgb:11/28/97 */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <dirent.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <fcntl.h>
#include <sys/stat.h>

extern int    VERBOSE, DEBUG ;


			/* *********************** */
			/* pattern search function */
			/* *********************** */

int strpat(char buffer[],char token[],int numtemp,int chartemp,int either, int phantom,int buffer_len)
{

int token_len,  cutoff_len;
int i,j,k;
int ii, jj;                                       /* dgb:11/28/97 */

token_len  = strlen(token);
cutoff_len = buffer_len - token_len;


i = 0;

   ii = 0;                                                 /* dgb:11/28/97 */
   while ( i < cutoff_len +1 )
   {
      j = 0;
      k = i;
      jj = 0;                                              /* dgb:11/28/97 */
      while ( j < token_len )
      {
         if (  token[j] == numtemp  )
         {

            if ( buffer[k] != '-' )
            {
               if ( buffer[k] != '.' )
               {
                  if ( buffer[k] != phantom )
                  {
                     if ( !isdigit(buffer[k]) )
                     {

                        if (  token[j] != buffer[k]  )
                           break;
                     }
                  }
               }
            }
         }
         else
         if (  token[j] == chartemp  )
         {
               if ( !isalpha(buffer[k]) )
               {
                  if (  token[j] != buffer[k]  )
                     break;
               }
         }
	 else
	 if (  token[j] == either  )                            /* dgb:12/06 99 */
	 {                                                      /* dgb:12/06 99 */
	      if ( !isdigit(buffer[k]) && !isalpha(buffer[k]) ) /* dgb:12/06 99 */
	      {                                                 /* dgb:12/06 99 */
	           if (  token[j] !=  buffer[k]  )              /* dgb:12/06 99 */
		         break;                                 /* dgb:12/06 99 */
	      }                                                 /* dgb:12/06 99 */
	 }                                                      /* dgb:12/06 99 */
         else
         if (  token[j] != buffer[k]  )
               break;

         j++;
         k++;

         if ( j == token_len )
         {

            return i; 
         }
         jj++;                                             /* dgb:11/28/97 */
      }   
      ii++;                                                /* dgb:11/28/97 */
     i++;
   }

   return -1;

}
