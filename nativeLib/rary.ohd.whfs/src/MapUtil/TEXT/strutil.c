/******************************************************************************
 *
 * Module: strutil.c
 *
 * Description: This module contains routines that deal with strings
 *
 * Routines: is_blank
 *           strip_trailing_blanks
 *           strip_leading_blanks
 *
 * History:
 *
 * Bryon Lawrence       09/28/2004       Moved the chgupper and chglower
 *                                       routines to the GeneralUtils library.
 *
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define False  0
#define True 1


/******************************************************************************
* 
* Routine: is_blank
* 
* Description: This routine returns true if given string is blank, returns
* false if there is a character other then tab, space or end of string
*
* Input: str: the string you wish to check for blanks
*
* Output: 
*
* Return Value: true if the string is blank or NULL
*               false if string is not black
*
******************************************************************************/

int is_blank(char *str)
{
  int i;

  /* if str is empty, return true */

  if (str == NULL)
    return(True);
  
  /* check every character.  If a character is not a space, tab, carriage 
     return, newline, vertical tab or formfeed, return false */
 
  for (i=0;i < strlen(str); i++){
    if (!isspace(str[i])){
      return(False);
    }
  }
  
  return(True);
}


/******************************************************************************
* 
* Routine: strip_trailing_blanks
* 
* Description: This routine removes blanks and the end of the given string
*
* Input: str: the string you wish to remove trailing blanks
*
* Output: str: the changed string
*
* Return Value: void
*
******************************************************************************/

void strip_trailing_blanks(char *str)
{
  int i;

  /* if str is empty, return */

  if (str == NULL)
    return;

  /* find the first non space character */

  for(i=strlen(str)-1;i>=0;i--)
    if (!isspace(str[i]))
      break;

  /* set the end of string */

  str[i+1] = 0;
}

/******************************************************************************
* 
* Routine: strip_leading_blanks
* 
* Description: This routine removes all leading blanks from the given string
*
* Input: str: the string you wish to removed leading blanks
*
* Output: str: the changed string
*
* Return Value: void
*
******************************************************************************/

void strip_leading_blanks(char *str)
{
  char *ptr;

  /* if str is empty, return */

  if (str == NULL)
    return;
  
  /* look for the first non space character */

  ptr = str;
  while(isspace(ptr[0]) && ptr[0] != 0)
    ptr++;

  /* copy string */

  if (ptr[0] != 0)
    strcpy(str,ptr);
}

/******************************************************************************
 * 
 * Routine: strip_leading_trailing_blanks
 * 
 * Description: This routine removes all leading and trailing  blanks from the 
 * given string
 *
 * Input: str: the string you wish to removed leading blanks
 *
 * Output: str: the changed string
 *
 * Return Value: void
 *
 *****************************************************************************/

void strip_leading_trailing_blanks(char *str)
{
  strip_leading_blanks(str);
  strip_trailing_blanks(str);
}

/******************************************************************************
 *****************************************************************************/

int getFloat(char *value,float *val)
{
  int i,j;
  int dot=-1,dotflag=0,minus=1;
  int t1;
  float f1,f2;

  *val = 0.000000000;
  strip_leading_trailing_blanks(value);
  for(i=0;i<strlen(value);i++){
    switch (value[i]){
      case '-':
	if (i != 0)
	  return(0);
  	
	minus = -1;
	break;

      case '.': 
	if (dotflag){
	  return(0);
	}
	else{
	  dotflag = 1;
	  dot = i;
	}
	
	break;
		
       default: 
	if (!isdigit(value[i]) && value[i] != ' '){
	  return(0);
	}
      }
  }
  
  if (dot == -1){
    t1 =  atoi(value);
    *val = t1 * 1.0;
  }
  else{
    t1 = atoi(&value[dot+1]);
    f2 = t1;
    for(j=0;j<strlen(&value[dot+1]);j++){
      f2 = f2 * 0.1;
    }

    value[dot] = 0;
    t1 = atoi(value);
    f1 = t1;
    *val = f1 + (minus * f2);
  }

  return(1);
}
