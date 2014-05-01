/*
	File:		TextFilter.c
	Date:		March 27, 1997
	Author:		Paul Taylor, Dale Shelton
	
	Purpose:	Convenience functions (typically callbacks)
			which provide "filtering" on Text widgets.	
*/


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include "Filter.h"
#include "Xtools.h"


/*****************************************************************************/


/*
	Allows only alphabetic characters, with OPTIONAL spaces,
	and if necessary, converts them to uppercase OR lowercase.
*/
void	alpha_filter(Widget w, XtPointer ptr, XmTextVerifyCallbackStruct *cbs)
{
   int	option = (int) ptr;
   int	i;
   

   for (i = 0; i < cbs->text->length; i++)
   {
      if (! isAlpha(cbs->text->ptr[i]))
	 cbs->doit = False;

      
      if ((option=(option - SPACES)) >= 0)  /* (alphabetic characters + SPACES) */
      {
	 if (isSpace(cbs->text->ptr[i]))
	    cbs->doit = True;
      }
      else
      {
	 option += SPACES;
      }
      
      
      if (option == MIXEDCASE)
      {
      }
      if (option == MIXEDCASE_AND_VALIDSYMBOLS)
      {
	 if (isApostrophe(cbs->text->ptr[i]))
	    cbs->doit = True;
	 
	 if (isSlash(cbs->text->ptr[i]))
	    cbs->doit = True;
      }
      if (option == UPPERCASE)
      {
	 if (islower(cbs->text->ptr[i]))
	    cbs->text->ptr[i] = toupper(cbs->text->ptr[i]);
      }
      if (option == UPPERCASE_AND_HYPHENS)
      {
	 if (islower(cbs->text->ptr[i]))
	    cbs->text->ptr[i] = toupper(cbs->text->ptr[i]);
	 
	 if (isHyphen(cbs->text->ptr[i]))
	    cbs->doit = True;
      }
      if (option == LOWERCASE)
      {
	 if (isupper(cbs->text->ptr[i]))
	    cbs->text->ptr[i] = tolower(cbs->text->ptr[i]);
      }
   }

   return;
}


/*****************************************************************************/


/*
	Allows only alpha-numeric characters, with OPTIONAL spaces,
	and if necessary, converts all alphabetic characters to uppercase
	OR lowercase.
*/
void	alphanum_filter(Widget w, XtPointer ptr, XmTextVerifyCallbackStruct *cbs)
{
   int	option = (int) ptr;
   int	i;
   
   
   for (i = 0; i < cbs->text->length; i++)
   {
      if (! isAlphaNum(cbs->text->ptr[i]))
	 cbs->doit = False;
      
      
      if ((option=(option - SPACES)) >= 0)  /* (alphanumeric characters + SPACES) */
      {
	 if (isSpace(cbs->text->ptr[i]))
	    cbs->doit = True;
      }
      else
      {
	 option += SPACES;
      }
      
      
      if (option == MIXEDCASE)
      {
      }
      if (option == MIXEDCASE_AND_VALIDSYMBOLS)
      {
	 if (isApostrophe(cbs->text->ptr[i]))
	    cbs->doit = True;
	 
	 if (isSlash(cbs->text->ptr[i]))
	    cbs->doit = True;
      }
      if (option == UPPERCASE)
      {
	 if (islower(cbs->text->ptr[i]))
	    cbs->text->ptr[i] = toupper(cbs->text->ptr[i]);
      }
      if (option == UPPERCASE_AND_HYPHENS)
      {
	 if (islower(cbs->text->ptr[i]))
	    cbs->text->ptr[i] = toupper(cbs->text->ptr[i]);
	 
	 if (isHyphen(cbs->text->ptr[i]))
	    cbs->doit = True;
      }
      if (option == LOWERCASE)
      {
	 if (isupper(cbs->text->ptr[i]))
	    cbs->text->ptr[i] = tolower(cbs->text->ptr[i]);
      }
   }

   return;
}


/*****************************************************************************/


/*
	Allows only numeric characters, with OPTIONAL spaces,
	and OPTIONAL decimals, hyphens, OR slashes.
*/
void	num_filter(Widget w, XtPointer ptr, XmTextVerifyCallbackStruct *cbs)
{
   int	option = (int) ptr;
   int	i;
   
   
   for (i = 0; i < cbs->text->length; i++)
   {
      if (! isNumeric(cbs->text->ptr[i]))
	 cbs->doit = False;

      
      if ((option=(option - SPACES)) >= 0)  /* (numeric characters + SPACES) */
      {
	 if (isSpace(cbs->text->ptr[i]))
	    cbs->doit = True;
      }
      else
      {
	 option += SPACES;
      }
      
      
      if (option == INTEGERS)
      {
      }
      if (option == INTEGERS_AND_DECIMALS)
      {
	 if (isDot(cbs->text->ptr[i]))
	    cbs->doit = True;
      }
      if (option == INTEGERS_AND_DECIMALS_AND_HYPHENS)
      {
	 if (isDot(cbs->text->ptr[i]))
	    cbs->doit = True;
	 
	 if (isHyphen(cbs->text->ptr[i]))
	    cbs->doit = True;
      }
      if (option == INTEGERS_AND_HYPHENS)
      {
	 if (isHyphen(cbs->text->ptr[i]))
	    cbs->doit = True;
      }
      if (option == INTEGERS_AND_SLASHES)
      {
	 if (isSlash(cbs->text->ptr[i]))
	    cbs->doit = True;
      }
      if (option == INTEGERS_AND_COLONS)
      {
	 if (isColon(cbs->text->ptr[i]))
	    cbs->doit = True;
      }
      if (option == INTEGERS_AND_DECIMALS_SIGN)
      {
	 if (isDot(cbs->text->ptr[i]))
	    cbs->doit = True;

	 if (isMinus(cbs->text->ptr[i]))
	    cbs->doit = True;
      }
   }
   
   
   return;
}


/*****************************************************************************/

/*
	Local defines.
*/
#define MON_POS_ONE	0
#define MON_POS_TWO	1
#define DAY_POS_ONE	3
#define DAY_POS_TWO	4

/* UNUSED? */
void	date_filter(Widget w, XtPointer ptr, XmTextVerifyCallbackStruct *cbs)
{
   char		buf[3],
      		*str;
   int		num;
   
   if (cbs->text->length == 1)
    {
    if (isNumeric(cbs->text->ptr[0]))
     {
	 num = atoi(cbs->text->ptr);
	 str = XmTextGetString(w);
	 switch (cbs->currInsert)
	 {
	    case MON_POS_ONE:
	       if (num > 1)
		  cbs->doit = False;
	       break;
	       
	    case MON_POS_TWO:
	       if (atoi(str) == 1)
		  if (num > 2)
		     cbs->doit = False;
	       
	       if (atoi(str) == 0)
		  if (num < 1)
		     cbs->doit = False;
	       break;
	       
	    case DAY_POS_ONE:
	       strncpy(buf, str, 2);
	       if (atoi(buf) == 2)
		  if (num > 2)
		     cbs->doit = False;
	       
	       if (num > 3)
		  cbs->doit = False;
	       break;
	       
	    case DAY_POS_TWO:
	       break;
	       
	    default:
	       break;
	 }
	 
	 /*
	 	Free the alloc'd memory for the string.
	 */
	 XtFree(str);
      }
      else
      {
	 cbs->doit = False;
      }
  }
   return;
}


/*****************************************************************************/


/*
	Local defines.
*/
#define	HOUR_POS_ONE	0
#define HOUR_POS_TWO	1
#define MIN_POS_ONE	3

/* UNUSED? */
void	time_filter(Widget w, XtPointer ptr, XmTextVerifyCallbackStruct *cbs)
{
   char   	*str;
   int		num;
   
   if (cbs->text->length == 1)
     {
      if (isNumeric(cbs->text->ptr[0]))
      {
	 num = atoi(cbs->text->ptr);
	 str = XmTextGetString(w);
	 switch (cbs->currInsert)
	 {
	    case HOUR_POS_ONE:
	       if (num > 2)
		  cbs->doit = False;
	       break;
	       
	    case HOUR_POS_TWO:
	       if (atoi(str) == 2)
		  if (num > 3)
		     cbs->doit = False;
	       
	       break;
	       
	       
	    case MIN_POS_ONE:
	       if (num >  5)
		  cbs->doit = False;
	       break;
	       
	       
	    default:
	       break;
	 }
	 
	 /*
	 	Free the alloc'd memory for the string.
	 */
	 XtFree(str);
      }
      else
      {
        cbs->doit = False;
      }
   }
   return;
}


/*****************************************************************************/

/*
	WHAT DOES THIS DO??
*/
/* UNUSED? */
void	zip_filter(Widget w, XtPointer ptr, XmTextVerifyCallbackStruct *cbs)
{
	char	c;
	int	len;
	
	len = XmTextGetLastPosition(w);
	if (cbs->text->length == 1)
	{
		c = cbs->text->ptr[0];
		printf("CHAR: %c\n", c);
	}

	return;
}


