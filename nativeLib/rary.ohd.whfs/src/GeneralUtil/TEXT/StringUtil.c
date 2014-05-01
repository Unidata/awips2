/*
	File:		StringUtil.c
	Date:		4/14/1997
	Author:		Paul Taylor

	Purpose:	Provides general support functions for ascii text.

	History:	Russell Erb	08/06/2001
			Added ctype.h
                        Bryon Lawrence  09/28/2004
			Moved chgupper and chglower routines from 
			map library strutil.c file.
*/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <Xm/Xm.h>
#include "GeneralUtil.h"

#define DEBUG_MODE	0	/* 0=False, 1=True */

void	Text_AddString(char** buf_str, char* append_str, int init_flag)
{
   char*  tmp_str = NULL;
   int	  buf_str_bytes = 0;
   int	  append_str_bytes;

   /*
        Check to make sure that the string to be appended is not NULL.
        If it is, then exit from this routine.
   */
   if ( append_str == NULL )
   {
      return ;
   }

   /*
   	Compute byte sizes.
   */
   if ( *buf_str != NULL )
   { 
      buf_str_bytes = strlen(*buf_str) * sizeof(char);
   }

   append_str_bytes = strlen(append_str) * sizeof(char);

   /*
   	Branch based on init_flag.
   */
   if (init_flag == True)
   {
      /*
	  Free old space (if any).
	  Determine how much space to allocate, then do it.
	  Fill with nulls.
      */
      Text_FreeString(buf_str);
      if(append_str_bytes > 0)	/* concat */
      {
	 *buf_str = (char *) malloc (append_str_bytes + 1);
	 if (*buf_str != NULL)
	 {
	    memset(*buf_str, '\0', append_str_bytes + 1);
	    strcpy(*buf_str, append_str);
         }
	 else
	 {
	    fprintf(stderr,"ERROR: Unable to perform malloc...\n");
	 }
      }
   }
   else
   {
      if(append_str_bytes > 0)	/* concat */
      {
	 tmp_str = (char *) malloc (buf_str_bytes + append_str_bytes + 1);
	 if (tmp_str != NULL)
	 {
	    memset(tmp_str, '\0', buf_str_bytes + append_str_bytes + 1);
            if ( *buf_str != NULL ) strcpy ( tmp_str , * buf_str ) ;
	    strcat(tmp_str, append_str);
	    
#if (DEBUG_MODE)
if (strlen(tmp_str) == 0)
{
printf("ERROR: buf_str was NULL & append_str is NULL and shouldn't be!!\n");
printf("       %i bytes (incl NULL slot) have been allocated unnecessarily !!!\n",
       buf_str_bytes + append_str_bytes + 1);
}
#endif


#if (DEBUG_MODE)
printf("Checking string lengths (orig + append => new): %ld + %ld = %ld --> ",
       strlen(*buf_str), strlen(append_str), strlen(tmp_str));

   if ((strlen(*buf_str) + strlen(append_str)) != strlen(tmp_str))
printf("ERROR in string lengths!!\n");
   else
printf("string lengths okay...\n");
#endif

/*
printf("(init_flag), <buf>[buf_str_bytes], <append>[append_str_bytes], <tmp_str>[tmp_str_bytes] == (%i),<%-s>[%ld],<%-s>[%ld],<%-s>[%ld]\n",
       init_flag, *buf_str, buf_str_bytes, append_str, append_str_bytes, tmp_str, strlen(tmp_str));
*/
   
	    if ( *buf_str != NULL ) Text_FreeString ( buf_str ) ;
	    *buf_str = tmp_str;
	 }
	 else
	 {
	    fprintf(stderr,"ERROR: Unable to perform malloc...\n");
	 }
      }
   }

   
   return;
}


void	Text_FreeString(char** free_string)
{
#if (DEBUG_MODE)
printf("in Text_FreeString...\n");
#endif

   if (*free_string)
   {
#if (DEBUG_MODE)
      printf("  Freeing %-ld bytes...<%s>", strlen(*free_string)+1, *free_string);
#endif

      free(*free_string);

#if (DEBUG_MODE)
      printf("done!\n");
#endif

      *free_string = NULL;
   }
   
   return;
}


/******************************************************************************
*
* Routine: IsBlankStr
*
* Description: This routine returns true if given string is blank, returns
* false if there is a character other then tab, space or end of string
*
* Input: str: the string you wish to check for blanks
*
* Output:
*
* Return Value: true if the string is blank or NULL
*               false if the string is not blank
*
******************************************************************************/

int	IsBlankStr(char *str)
{
  int i;

  /* if str is empty, return true */

  if (str == NULL)
    return(True);

  /* check every character.  If a character is not a space, tab, carriage
     return, newline, vertical tab or formfeed, return false */

  for (i=0; i < strlen(str); i++)
  {
    if (!isspace(str[i]))
    {
      return(False);
    }
  }

  return(True);
}

/******************************************************************************
*
* Routine: strip_tblanks
*
* Description: This routine strips off any trailing blanks in the 
*              user-provided string.  Note that the returned string
*              may be truncated.
*
* Input: str: the string you wish to check for trailing blanks
*
* Output:  str : The string with any trailing blanks removed.
*
******************************************************************************/

void strip_tblanks(char    *str)
{
   int i;
   int slen;
   
   
   slen = strlen(str);
   
   /* if the space is not a white space character, then
      assume it is the end of the string */
   
   for (i = slen; i > 0; i--)
   {
      if (isspace(str[i - 1]) == 0)
	 break;
   }
   
   memset(&str[i], 0, 1);
   
   return;
}


/******************************************************************************
*
* Routine: strip_lblanks
*
* Description: This routine strips off any leading blanks in the 
*              user-provided string.  Note that the returned string
*              may be truncated.
*
* Input: str: the string you wish to check for leading blanks
*
* Output:  str : The string with any leading blanks removed.
*
******************************************************************************/

void strip_lblanks(char    *str)
{
   int i;
   int slen;
   char	* strtemp = NULL ;
   
   slen = strlen(str);
         
   /* loop thru string until we find the first non-blank char */
   
   for (i = 0; i < slen; i++)
   {
      if (isspace(str[i]) == 0)
	 break;
   }
   
   /* check for error condition */
   
   if (i == slen)
   {
      fprintf(stderr, "Error in strip_lblanks: no non-blank chars found.\n");
      return;
   }
   
   
   /* make a local copy of the string,
      then reset the original string */
   
   strtemp = (char *)malloc(slen + 1);
   if (strtemp == NULL)
   {
      fprintf(stderr, "Error in malloc in strip_lblanks.\n");
      return;
   }
   
   memset(strtemp, 0, slen + 1);
   strcpy(strtemp, str);
   
   strcpy(str, &strtemp[i]);
   
   
   if ( strtemp != NULL )
   {
      free ( strtemp ) ;
      strtemp = NULL ;
   }

   return;
}

/******************************************************************************
*
* Routine: compress_blanks
*
* Description: This routine each occurrence of a sequence of multiple
*              blanks into a single blank. 
*
* Input:  str:  The string to compress blanks in.
* Output: str:  The string with the blanks compressed.
*
******************************************************************************/

void compress_blanks(char    *str)
{
   int	i;
   int	slen;
   int	within_blankrun;
   char	* newstr = NULL ;
   
   slen = strlen(str);
   
   newstr = (char *)malloc(slen + 1);
      
   if ( newstr == NULL )
   {
      fprintf(stderr, "Error in malloc in compress_blanks.\n");
      return;
   }

   memset(newstr, 0, slen + 1);
   
   within_blankrun = 0;
   
   for (i = 0; i < slen; i++)
   {
      if (isspace(str[i]) != 0)
      {
	 if (!within_blankrun)
	 {
	    within_blankrun = 1;
	    strncat(newstr, &str[i], 1);
	 }
      }
      
      else
      {
	 strncat(newstr, &str[i], 1);
	 within_blankrun = 0;
      }
   }
   
   strcpy ( str , newstr ) ;

   if ( newstr != NULL )
   {
      free ( newstr ) ;
      newstr = NULL ;
   }
   
   return;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME: chgupper
* PURPOSE:     This routine converts the given string to upper case letters.
*
* ARGUMENTS:
*   TYPE          DATA TYPE   NAME   DESCRIPTION/UNITS
*   Input/Output  char *      str    The string to be converted to upper case.
*
* RETURNS:
*   No return value.
*
* APIs UTILIZED:
*   Non nonsystem APIs used.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME   DESCRIPTION
*   int        i      A loop index variable.
*
* DATA FILES AND/OR DATABASE:
*   No external datasources required.
*
* ERROR HANDLING:
*   No error handling.
********************************************************************************
*/
void chgupper ( char * str )
{
  int i ;

  /* if str is empty, return */
  if (str == NULL)
    return;

  /* loop thru every character and change to upper case */

  for (i=0;i < strlen(str); i++){
    str[i] = toupper(str[i]);
  }
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:     chglower
* PURPOSE:         Changes all of the characters in a string to lower case.
*
* ARGUMENTS:
*   TYPE          DATA TYPE   NAME    DESCRIPTION/UNITS
*   Input/Output  char *      str     The string whose characters need
*                                     to be changed to lower case.
* RETURNS:
*   No return values.
*
* APIs UTILIZED:
*   No nonsystem APIs are utilized.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME     DESCRIPTION
*   int        i        A loop index variable.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/
void chglower(char *str)
{
  int i;

  /* if str is empty, return */

  if (str == NULL)
    return;

  /* loop thru every character and change to lower case */

  for (i=0;i < strlen(str); i++){
    str[i] = tolower(str[i]);
  }
}

/*******************************************************************
   Check the length of the Fortran-originated string.
   (copied from site-specific code)
   
   Note: The end position passed is the Fortran position.
   
   *****************************************************************/


int Fstrlen(const       char    *Fstr,
	    const       int     endpos)
{
   int i;
   
   /* exit loop if the single character being checked is
      printable and is not a whitespace character */
   
   for (i = endpos; i > 0; i--)
   {
      if ((isgraph(Fstr[i - 1]) != 0)  &&
          (isspace(Fstr[i - 1]) == 0))
         break;
   }
   
   return(i);
}

/*******************************************************************
   Check the length of the C-originated string.
   
   Note: The end position returned is the Fortran position.
   
   *****************************************************************/
int cstrlen(const       char    *Cstr)
{
   int i;
   
   i = strlen(Cstr);
   
   
   return(i);
}

