/*
	File:		loc_cbs.c
	Date:		March 1996
	Author:		Dale Shelton / Chip Gobs
			Paul Taylor
	
	Purpose:	Provides support for the Location TLS.
	
*/

#include <time.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/ToggleB.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>
/**** POSTGRES
#include <sqlhdr.h>
*****/
#include "Location.h"
#include "Filter.h"
#include "Xtools.h"
#include "DbmsDefs.h"
#include "ParamDefs.h"
#include "loc_cbs.h"
#include "loc.h"
#include "cnty_cbs.h"
#include "cnty.h"
#include "agencyoffice_show.h"


void	loc_callbacks(void)
{
	Atom	wmAtom;
	
	/*
		Window manager callbacks.
	*/
	wmAtom = XmInternAtom(XtDisplay(locDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(locDS, wmAtom, close_loc, NULL);
	
	
	/*
		Button callbacks.
	*/
	XtAddCallback(locgeoPB, XmNactivateCallback, loc_checkPB, NULL);
	XtAddCallback(locaddPB, XmNactivateCallback, loc_checkPB, NULL);
	
	XtAddCallback(cntystPB, XmNactivateCallback, show_cntys, NULL);
	XtAddCallback(rvsTB,    XmNvalueChangedCallback, load_locdate, NULL);

	XtAddCallback(lokPB,    XmNactivateCallback, ok_loc, lmainOM);
	XtAddCallback(lapplyPB, XmNactivateCallback, apply_loc, lmainOM);
	XtAddCallback(lclosePB, XmNactivateCallback, close_loc, NULL);
	XtAddCallback(ldeletePB,XmNactivateCallback, loc_del_conf, NULL);

	XtAddCallback(loccopyPB,XmNactivateCallback, show_loccopy, NULL);
	
	
	XtAddCallback(locagcyofficePB, XmNactivateCallback,
		      show_agencyoffice_callback, NULL);
	

	/*
		Add TextFilter callbacks.
	*/
	loc_addTextFilterCallbacks();
	
	
	return;
}


void	loc_addTextFilterCallbacks(void)
{
   XtAddCallback(lidTxt, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)MIXEDCASE);
   XtAddCallback(latTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
				(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));
   XtAddCallback(lonTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
				(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));
   XtAddCallback(elevTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_SIGN);
   XtAddCallback(lsnTxt, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE_AND_HYPHENS);
   XtAddCallback(rvsTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
   XtAddCallback(detailTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);

   XtAddCallback(lshdatumTxt,XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)(MIXEDCASE+SPACES));
   XtAddCallback(lshuTxt,    XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtAddCallback(lssbdTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
   XtAddCallback(stntypeTxt, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)MIXEDCASE);
   
   return;
}


void	loc_removeTextFilterCallbacks(void)
{
   XtRemoveCallback(lidTxt, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)MIXEDCASE);
   XtRemoveCallback(latTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
				(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));
   XtRemoveCallback(lonTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
				(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));
   XtRemoveCallback(elevTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
   XtRemoveCallback(lsnTxt, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE_AND_HYPHENS);
   XtRemoveCallback(rvsTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
   XtRemoveCallback(detailTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
   
   XtRemoveCallback(lshdatumTxt,XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)(MIXEDCASE+SPACES));
   XtRemoveCallback(lshuTxt,    XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtRemoveCallback(lssbdTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
   XtRemoveCallback(stntypeTxt, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)MIXEDCASE);

   return;
}


void	loccopy_callbacks(void)
{
	Atom	wmAtom;
	
	/*
		Window manager callbacks.
	*/
	wmAtom = XmInternAtom(XtDisplay(loccopyDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(loccopyDS, wmAtom, close_loccopy, NULL);

	
	/*
		Button callbacks.
	*/
	XtAddCallback(loccopy_okPB,    XmNactivateCallback, ok_loccopy,    NULL);
	XtAddCallback(loccopy_cancelPB,XmNactivateCallback, close_loccopy, NULL);

	
	/*
		Text callbacks.
	*/
	XtAddCallback(loccopydestTE, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)MIXEDCASE);
	
	
	return;
}


void	set_date(Widget w, XtPointer ptr, XmToggleButtonCallbackStruct *cbs)
{
	struct tm	*timer;
	time_t		curr;
	
	
	if (XmToggleButtonGetState(w))
	{
		ctime(&curr);
		timer = localtime(&curr);
	}
	return;
}

void	show_agencyoffice_callback(Widget w, XtPointer ptr, XtPointer cbs)
{
     char lid[LOC_ID_LEN+1];
     strcpy(lid, CurrentLid());
     show_agencyoffice(w, lid);    
}


void	show_cntys(Widget w, XtPointer ptr, XtPointer cbs)
{
	ShowCntyDs(GetTopShell(w), FROM_LOC);
	return;
}


void	show_loccopy(Widget w, XtPointer ptr, XtPointer cbs)
{
	ShowLocCopyDs(GetTopShell(w));
   	return;
}


void	lid_filter(Widget w, XtPointer ptr, XmTextVerifyCallbackStruct *cbs)
{
	static Location		*loc = NULL;
	Location		*locPtr = NULL;
	
	char			buf[9],
				*lid;		
	
	
	if (cbs->text->length > 0)
		strncpy(buf, cbs->text->ptr, cbs->text->length);
		
		
	if (! loc)
		loc = GetLocation("");
	
		
	locPtr = (Location *) ListFirst(&loc->list);
	while (locPtr)
	{
		lid = strtok(locPtr->lid, " ");
		if (strcmp(lid, buf) == 0)
		{
			cbs->doit = False;
			break;
		}
		locPtr = (Location *) ListNext(&locPtr->node);
	}
	
	return;
}


void    load_locdate(Widget w, XtPointer ptr, XtPointer cbs)
{
   Location	*loc;
   
   char    	sdate[DATE_LEN + 2],
      		where[MAX_WHERE_LEN],
      		lid[LOC_ID_LEN + 1],
      		* buf = NULL ;
   
   struct tm	*tm_ptr;
   time_t	starttime;
   
   
   loc_removeTextFilterCallbacks();
   
   
   if (XmToggleButtonGetState(rvsTB))
   {
      time(&starttime);
      tm_ptr = localtime(&starttime);
      
      strftime(sdate, DATE_LEN+2, "%m/%d/%Y", tm_ptr);
      XmTextSetString(rvsTxt,sdate);
   }
   
   else
   {
      if ( ( buf = XmTextGetString(lidTxt) )  != NULL )
      {
	 strcpy(lid, buf);
	 XtFree(buf);
      }
      
      
      sprintf(where," WHERE lid = '%s' ",lid);       	
      if ((loc = GetLocation(where)) != NULL)
      {
         date_t_to_USA_date ( loc->lrevise, sdate );
         FreeLocation(loc);
      }
      else
      {	
         strcpy(sdate, "");
      }
      
      XmTextSetString(rvsTxt,sdate);     		
   }
   
   
   
   loc_addTextFilterCallbacks();
   
   return;
}
