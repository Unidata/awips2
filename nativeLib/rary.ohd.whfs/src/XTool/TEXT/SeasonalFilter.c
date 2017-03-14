/*
	File:		SeasonalFilter.c
	Date:		January 1996
	Author:		Paul Taylor

	Purpose:	Provides callbacks for XmText widgets which
			force user input to conform to a monthly value
			ranging from 1-31 (with max dependent on the
			particular month in question--
			
				Feb:29,Apr:30,Jun:30,Sept:30,Nov:30,
				with the rest being 31).
				

	Usage:		The following sample widget layout consists of a
			label, followed by an option menu, followed by
			a text widget.  The text widgets have seasonal_filter
			callbacks associated with them, and the PBs in the
			option menus have seasonal_pb_helper callbacks
			associated with them:
			
			
			Begin (Seasonal):	BeginMonthOM	BeginTE
	
			End (Seasonal):		EndMonthOM	EndTE
*/


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include "Filter.h"
#include "Xtools.h"


/*
	Local defines.
*/
#define PPT_DEBUG	0	/* 1==debugging, 0==no debugging */

#define DAY_POS_ONE	0
#define DAY_POS_TWO	1

extern Widget	fdbeginTxt;
extern Widget	fdendTxt;

extern Widget	fsbegOM;
extern Widget	fsendOM;

static int ppt_mon_max[12] = {31,29,31,30,31,30,31,31,30,31,30,31};
			/*    J  F  M  A  M  J  Jl A  S  O  N  D   */


void	seasonal_filter(Widget w, Widget om, XmTextVerifyCallbackStruct *cbs)
{
	static int begin_zero_padding = True; /* initial state is '01' (Jan) */
	static int end_zero_padding = False; /* initial state is '31' (Dec) */

	int	pos,
		num = 0 ,
		i,
		diff;
	
	char	buf[3],
		*str;
	
	div_t	j;



	/*
		Gather necessary information.
	*/
	pos = GetMenuPos(om);		/* pos (months) are from 0-11 */
      
        if ( cbs->text != NULL )
        {
	   num = atoi(cbs->text->ptr);	/* numeric character just entered */
        }

	str = XmTextGetString(w);	/* Text widget as a string */
	i = atoi( strncpy(buf, str, 2) ); /* total numeric of Text widget */
	diff = cbs->endPos - cbs->startPos;	/* == 2 if double-click... */
	j = div(i,10);	/* number helper */	/* ...(in most cases) */


#if(PPT_DEBUG)
printf("j.quot,j.rem = %i,%i\n",j.quot,j.rem);
printf("endPos,startPos = %i,%i\n",cbs->endPos,cbs->startPos);
if(cbs->reason == XmCR_LOSING_FOCUS)
	printf("losing focus...\n");
else if (cbs->reason == XmCR_MODIFYING_TEXT_VALUE)
{
	printf("modifying text value on position ");
	printf("<%i>\n",cbs->currInsert);
}
#endif



	/*
		Initialize variables for use in preventing '00'.
	*/
	if (w==fdbeginTxt)
	{
#if(PPT_DEBUG)
	   printf("value of BEGIN text field is <%c%c>\n",buf[0],buf[1]);
#endif
	   if (buf[0] == '0')	begin_zero_padding = True;
	   else			begin_zero_padding = False;
	}
	else if (w==fdendTxt)
	{
#if(PPT_DEBUG)
	   printf("value of END text field is <%c%c>\n",buf[0],buf[1]);
#endif
	   if (buf[0] == '0')	end_zero_padding = True;
	   else			end_zero_padding = False;
	}




	/*
		Check for losing focus. For example, it may be necessary
		to add a zero to the beginning of the typed entry.
		(i.e. '5' -> '05').
	*/
	if ((cbs->reason == XmCR_LOSING_FOCUS) && (i < 10))
	{
	   if (i == 0) /* check for illegal value */
	   {
		if (w==fdbeginTxt)
		{
		   sprintf(buf,"01");
		   
		   /* CREATES A POTENTIAL BUG IF UNCOMMENTED
		   ErrorDialog(w,"Begin Day cannot be 0. Default value was used.");
		   */
		}
		if(w==fdendTxt)
		{
		   sprintf(buf,"%i",ppt_mon_max[pos]);
		   
		   /* CREATES A POTENTIAL BUG IF UNCOMMENTED
		   ErrorDialog(w,"End Day cannot be 0. Default value was used.");
		   */
		}
	   }
	   else	/* value is legal */
	   {
	   	sprintf(buf,"0%i",i);
	   }
	   
	   XtVaSetValues(w, XmNvalue, buf, NULL);	/* update widget */
	}




	/*
		Accept 1 character at a time.
	*/
	if ( cbs->text == NULL || cbs->text->length != 1 )
	{
		if ( str != NULL ) free ( str ) ;
		return ;
	}

	/*
		Accept numeric characters only.
	*/
	if (! isNumeric(cbs->text->ptr[0]))
	{
		cbs->doit = False;
		if(str != NULL)	free(str);
		return;
	}




#if(PPT_DEBUG)
printf("------------------------\n");
printf("diff,num,i = %i,%i,%i\n",diff,num,i);
printf("currInsert,newInsert,startPos,endPos = %i,%i,%i,%i\n",
        cbs->currInsert,cbs->newInsert,cbs->startPos,cbs->endPos);
printf("buf[0],buf[1] = <%c,%c>\n",buf[0],buf[1]);
#endif



	/*
		Value checking for single-char mouse highlighting.
	*/
	if ((diff == 1) && (buf[1] != '\0'))
	{
#if(PPT_DEBUG)
printf("<<A>>\n");
#endif
	   if (cbs->startPos == 0)	/* typeover character 1 */
	   {
		/* prevent '00' from occuring. */
		if ((num == 0) && (buf[1] == '0'))
		{
			cbs->doit = False;
		}

		/* set to False if violates ppt_mon_max. */
		if (((num*10)+j.rem) > ppt_mon_max[pos])
			cbs->doit = False;
	   }

	   if (cbs->startPos == 1)	/* typeover character 2 */
	   {
		/* prevent '00' from occuring. */
		if ((num == 0) && (buf[0] == '0'))
		{
			cbs->doit = False;
		}
		
		/* set to False if violates ppt_mon_max. */
		if (((j.quot*10)+num) > ppt_mon_max[pos])
			cbs->doit = False;
	   }
	   
	   if(str != NULL)	free(str);
	   return;
	}




	/*
		Value checking for entering 1st digit.
	*/
	if ( (cbs->startPos == 0) &&
	     (cbs->endPos < 2) &&
	     (buf[1] == '\0') &&
	     ((diff != 0) || ((num != 0)&&(i == 0))) )
	{
#if(PPT_DEBUG)
printf("<<B>>\n");
#endif
		cbs->doit = True;
		if(str != NULL)	free(str);
		return;
	}
	if ( (diff != 2) &&
	     (cbs->currInsert == DAY_POS_ONE) &&
	     (((num*10)+i) > ppt_mon_max[pos]) )
	{
#if(PPT_DEBUG)
printf("<<C>>\n");
#endif
		cbs->doit = False;
		if(str != NULL)	free(str);
		return;
	}
#if(PPT_DEBUG)
printf("=====================\n");
#endif




	/*
		Value checking for entering 2nd digit.
	*/
	if ( (cbs->currInsert == DAY_POS_TWO) &&
	     (((i*10)+num) > ppt_mon_max[pos]) )
	{
	   	cbs->doit = False;
		if(str != NULL)	free(str);
		return;
	}
	if (w==fdbeginTxt)	/* if changing Seasonal Begin Day... */
	{
		if ( (diff != 2) &&
		     (begin_zero_padding == True) && (num == 0) )
			cbs->doit = False;
	}	
	else if (w==fdendTxt)	/* if changing Seasonal End Day... */
	{
		if ( (diff != 2) &&
		     (end_zero_padding == True) && (num == 0) )
			cbs->doit = False;
	}




	/*
		Free the alloc'd memory for the string.
	*/
	if(str != NULL)	free(str);
	return;
}


void	seasonal_pb_helper(Widget w, Widget om,
			   XmPushButtonCallbackStruct *cbs)
{
	int	pos;
	char	buf[3];
	
	/*
		Gather necessary information.
	*/
	pos = GetMenuPos(om);	/* pos (months) are from 0-11 */


	/*
		Screen maximum limits on Text widgets.
	*/
	if (om == fsbegOM)	/* if changing Seasonal Begin Month... */
	{
	   if ( atoi( strncpy(buf,XmTextGetString(fdbeginTxt),2) ) >
	   	ppt_mon_max[pos] )
	   {
	      sprintf(buf,"01");
	      XtVaSetValues(fdbeginTxt, XmNvalue, buf, NULL);
	   }
	}
	
	if (om == fsendOM)	/* if changing Seasonal End Month... */
	{
	   if ( atoi( strncpy(buf,XmTextGetString(fdendTxt),2) ) >
	   	ppt_mon_max[pos] )
	   {
	      sprintf(buf,"%i",ppt_mon_max[pos]);
	      XtVaSetValues(fdendTxt, XmNvalue, buf, NULL);
	   }
	}

	return;
}

