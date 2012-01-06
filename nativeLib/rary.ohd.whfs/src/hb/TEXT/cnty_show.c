/*
	File:		cnty_show.c
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:	Provide support for the popup County/State DS.
	
	Edited:		Russ Erb - October 1998
*/

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include "Counties.h"
#include "Xtools.h"
#include "DbmsDefs.h"
#include "ParamDefs.h"
#include "loc.h"
#include "cnty_cbs.h"
#include "cnty.h"
#include "nwrtower.h"

int	calling_dialog;

void	ShowCntyDs(Widget w, int called_from)
{
	Atom	wmAtom;
	
	/* set global variable to be used in copy_loc_cntyst() and clear_cnty() */
	calling_dialog = called_from;

	if (! cntyDS)
	{
	   
	        /*
			Create county dialog	
		*/
		create_cntyDS(GetTopShell(w));
			
		/*
			Window manager callbacks.
		*/
		wmAtom = XmInternAtom(XtDisplay(cntyDS), "WM_DELETE_WINDOW", False);
		XmAddWMProtocolCallback(cntyDS, wmAtom, close_cnty, NULL);
		
		XtAddCallback(cntyokPB, XmNactivateCallback, copy_loc_cntyst, NULL);
		XtAddCallback(cntyclearPB, XmNactivateCallback, clear_cnty, NULL);
		XtAddCallback(cntyclosePB, XmNactivateCallback, close_cnty, NULL);
		
		XtAddCallback(cntyLB, XmNdefaultActionCallback, copy_loc_cntyst, NULL);
		load_cntysts();
		
		XtManageChild(cntyFM);
		XtManageChild(cntyDS);
	}
	
	return;
}


Counties**	getCountiesBase()
{
	static Counties *countiesHead = NULL;
	
	return (&countiesHead);
}   


void	load_cntysts(void)
{
	XmStringTable	xmStr = NULL ;
	Arg		arg[2];
	Counties	**cntyBase,
	   		*cntyHead,
			*cntyPtr;
	char		buf[80];
	int		cnt,
			i;
	
	/*
		Get the county info and load the
		county dialog.
	*/
	cntyBase = getCountiesBase();
	cntyHead = GetCounties(" WHERE state != 'XX' ORDER BY state, county ");
	if ((cnt = ListCount(&cntyHead->list)) > 0)
	{
		xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
		cntyPtr = (Counties *) ListFirst(&cntyHead->list);
		for (i = 0; i < cnt; i++)
		{
			sprintf(buf, "%-20s %-2s", cntyPtr->county, cntyPtr->state);
			xmStr[i] = XmStringCreateSimple(buf);
			cntyPtr  = (Counties *) ListNext(&cntyPtr->node);
		}
	}
	
	
	/*
		Load the list box.
	*/
	XtSetArg(arg[0], XmNitemCount, cnt);
	XtSetArg(arg[1], XmNitems, xmStr);
	XtSetValues(cntyLB, arg, 2);
	XmListSelectPos(cntyLB, 1, True);
	
	
	
	/*
		Cleanup and return.
	*/
	for (i = 0; i < cnt; i++)
		XmStringFree(xmStr[i]);
	XtFree((char *) xmStr);
	
	
	/*
		assign the head ptr to the base, to avoid global
		variables
	*/
	*cntyBase = cntyHead;
	return;
}


void	close_cnty(Widget w, XtPointer ptr, XtPointer cbs)
{
   Counties 	**cntyBase = NULL,
      		*cntyHead = NULL;

/*
printf("cntyBase = %p, *cntyBase = %p\n", cntyBase, *cntyBase);
*/
   
   cntyBase = getCountiesBase();

/*   
printf("cntyBase = %p, *cntyBase = %p\n", cntyBase, *cntyBase);
*/
   
   if (cntyBase)
   {   
      cntyHead = *cntyBase;
      if (cntyHead)
      {
	 FreeCounties(cntyHead);
      }
   }
   
   
   if(XtIsManaged(cntyDS))
   {
      XtDestroyWidget(cntyDS);
      cntyDS = NULL;
   }
   
   return;
}


void	clear_cnty(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (calling_dialog == FROM_LOC)
      XmTextSetString(cntystTxt, "XXXXXXXXXXXXXXXXXXXX, XX");
   else if (calling_dialog == FROM_NWR)
      XmTextSetString(nwrcntystTE, "XXXXXXXXXXXXXXXXXXXX, XX");
      
   close_cnty(NULL, NULL, NULL);
   
   return;
}


void    copy_loc_cntyst(Widget w, XtPointer ptr, XtPointer cbs)
{	
   char *cnty;
   char *state;
   char buf[BUFSIZ];
   
   
   /*
   	Sets cntystTxt widget on the Location dialog.
   	or nwrcntystTE widget on the NWR tower dialog.
   */	
   cnty = ChosenCnty();
   state = ChosenState();
   
   
   strcpy(buf, cnty);
   strcat(buf, ", ");
   strcat(buf, state);
   
   if (calling_dialog == FROM_LOC)
      XmTextSetString(cntystTxt, buf);
   else if (calling_dialog == FROM_NWR)
      XmTextSetString(nwrcntystTE, buf);
  
   
   close_cnty(NULL, NULL, NULL);
   
   return;
}


char*	ChosenCnty(void)
{

        char            *cnty = NULL;
        int             pos,
	   		*poslist = NULL,
                        cnt=0;
	Counties	**cntyBase,
	   		*cntyHead,
			*cntyPtr;
	
        XmListGetSelectedPos(cntyLB, &poslist, &cnt);
        if(cnt > 0)
	{
	       pos = poslist[0];
	       if (poslist)
	               free(poslist);
	       
	       cntyBase = getCountiesBase();
	       cntyHead = *cntyBase;
	       cntyPtr = (Counties *) ListNth(&cntyHead->list, pos);
	       
	       if (cntyPtr)
		  cnty = cntyPtr->county;
 	}
	       
        return(cnty);
}


char*	ChosenState(void)
{
        char            *state = NULL;
        int             pos,
	   		*poslist = NULL,
                        cnt=0;
	Counties	**cntyBase,
	   		*cntyHead,
			*cntyPtr;
	
        XmListGetSelectedPos(cntyLB, &poslist, &cnt);
        if(cnt > 0)
	{
	       pos = poslist[0];
	       if (poslist)
	               free(poslist);
	       
	       cntyBase = getCountiesBase();
	       cntyHead = *cntyBase;
	       cntyPtr = (Counties *) ListNth(&cntyHead->list, pos);
	       
	       if (cntyPtr)
		  state = cntyPtr->state;
 	}

        return(state);
}
