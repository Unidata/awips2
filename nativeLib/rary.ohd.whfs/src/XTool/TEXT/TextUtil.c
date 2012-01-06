/*
	File:		TextUtil.c
	Date:		12/20/1996
	Author:		Paul Taylor

	Purpose:	Provides support functions for the Text widget.
*/


#include <stdio.h>
#include <limits.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>

#include "Xtools.h"
#include "GeneralUtil.h"
#include "DbmsUtils.h"




void	addTextWidgetCallbacksBelowParent(Widget parent,
					  String callback_name,
					  XtCallbackProc callback_function,
					  XtPointer ptr)
{
   WidgetList	widgetList;
   int		cnt, i;
   
   
   XtVaGetValues(parent,
		 XmNchildren, &widgetList,
		 XmNnumChildren, &cnt,
		 NULL);

   if (cnt > 0)
   {
      for(i=0; i<cnt; i++)
      {
	 if (XtIsSubclass(widgetList[i], xmManagerWidgetClass))
	 {
	    addTextWidgetCallbacksBelowParent(widgetList[i], callback_name,
					      callback_function, ptr);
	 }
	 else if (XmIsText(widgetList[i]))
	 {
	    XtAddCallback(widgetList[i], callback_name,
			  callback_function, ptr);
	 }
      }
   }

   
   return;
}


void	removeTextWidgetCallbacksBelowParent(Widget parent,
					     String callback_name,
					     XtCallbackProc callback_function,
					     XtPointer ptr)
{
   WidgetList	widgetList;
   int		cnt, i;
   
   
   XtVaGetValues(parent,
		 XmNchildren, &widgetList,
		 XmNnumChildren, &cnt,
		 NULL);

   if (cnt > 0)
   {
      for(i=0; i<cnt; i++)
      {
	 if (XtIsSubclass(widgetList[i], xmManagerWidgetClass))
	 {
	    removeTextWidgetCallbacksBelowParent(widgetList[i], callback_name,
						 callback_function, ptr);
	 }
	 else if (XmIsText(widgetList[i]))
	 {
	    XtRemoveCallback(widgetList[i], callback_name,
			     callback_function, ptr);
	 }
      }
   }

   
   return;
}



int	CheckTextBounds(Widget label, char *buf,
			char* lo_cond, double* lo, char* lo_format,
			char* hi_cond, double* hi, char* hi_format)
{
   Widget	info;
   char		msg[1024];
   char		temp[1024];
   
   char*	labelString = NULL;
   int		rv = 0;
   char		lo_char = '\0';
   char		hi_char = '\0';
   
   

   /* assign limit characters */
   
   if (strcmp(lo_cond, GT) == 0)	lo_char = '(';
   if (strcmp(lo_cond, GE) == 0)	lo_char = '[';
   
   if (strcmp(hi_cond, LT) == 0)	hi_char = ')';
   if (strcmp(hi_cond, LE) == 0)	hi_char = ']';
   
   
   
   /* do bounds checking & return result */
   
   rv = CheckBounds(buf, lo_cond, lo, hi_cond, hi);
   if (rv == 0)
   {
      if ((labelString = GetLabel(label)))
      {
	 memset(&msg, '\0', sizeof(msg));
	 memset(&temp, '\0', sizeof(temp));

	 sprintf(msg, "LIMITS for \"%-s\" are: %c",
		 strtok(labelString,":"), lo_char);
	 if ((*lo) == INT_MIN)
	    sprintf(temp, "%-s", "-INFINITY");
	 else
	 {
	    if (strchr(lo_format,'i'))
	       sprintf(temp, lo_format, (int) *lo);
	    else
	       sprintf(temp, lo_format, *lo);
	 }
	 strcat (msg, temp);
	 strcat (msg, " , ");
	 
	 
	 memset(&temp, '\0', sizeof(temp));
	 if ((*hi) == INT_MAX)
	    sprintf(temp, "%-s", "INFINITY");
	 else
	 {
	    if (strchr(hi_format,'i'))
	       sprintf(temp, hi_format, (int) *hi);
	    else
	       sprintf(temp, hi_format, *hi);
	 }
	 strcat (msg, temp);
	 
	 
	 memset(&temp, '\0', sizeof(temp));
	 sprintf(temp, "%c\n\nPlease enter a value within these bounds...",
		 hi_char);
	 strcat (msg, temp);
	 
	 
	 info = InfoDialog(XtParent(label), msg);
	 SetTitle(info, "Out of Bounds Error");
      }
   }

      
   return(rv);  /* result of bounds-checking: 0==failure, 1==success */
}

/* Modification History:

   October 31, 2005    Bryon Lawrence  Changed call to memcpy to memset.
                                       The call to memcpy was incorrect. */

void	getTextFromXmString(char *text, XmString compoundString, long maxLen)
{
   
   XmStringContext	context;
   XmStringCharSet	c;
   XmStringDirection 	d;
   Boolean		sep;
   char			*tempText;

   
   XmStringInitContext(&context, compoundString);
   XmStringGetNextSegment(context, &tempText, &c, &d, &sep);
   XmStringFreeContext(context);
   
   memset(text, '\0', maxLen * (sizeof(char)));
   
   strncpy(text, tempText, maxLen - 1);
   
   
   return;   
}   
   


