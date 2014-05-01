/*
	File:		grp_show.c
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:	Provide support for the Forecast Point
			Group Assignment DS.
	
*/

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include "RpfFcstGroup.h"
#include "Xtools.h"
#include "DbmsDefs.h"
#include "ParamDefs.h"
#include "river.h"
#include "grp.h"
#include "grp_cbs.h"


char	*rgroup_none = "(Not a Forecast Point)";


void	ShowGrpDs(Widget w, XtPointer ptr, XtPointer cbs)
{
	Atom	wmAtom;


	if (! glDS)
	{
		create_glDS(GetTopShell(w));
	
		/*
			Window manager callbacks.
		*/
		wmAtom = XmInternAtom(XtDisplay(glDS), "WM_DELETE_WINDOW", False);
		XmAddWMProtocolCallback(glDS, wmAtom, close_grp, NULL);
		XtAddCallback(glclosePB, XmNactivateCallback, close_grp, NULL);
		XtAddCallback(glokPB, XmNactivateCallback, close_grp, NULL);
		XtAddCallback(glokPB, XmNactivateCallback, copy_grp, NULL);
		XtAddCallback(glclearPB, XmNactivateCallback, clear_grp, NULL);
		XtAddCallback(glLB, XmNdefaultActionCallback, close_grp, NULL);
		XtAddCallback(glLB, XmNdefaultActionCallback, copy_grp, NULL);
	}
	
	if (! XtIsManaged(glDS))
	{
	   if (load_grps())
	   {
	      XtManageChild(glFM);
	      XtManageChild(glDS);
	   }
	   else
	   {
	      XtDestroyWidget(glDS);
	      glDS = NULL;
	   }
	}
	
	return;
}


int	load_grps(void)
{
	XmStringTable	xmStr;
	Arg		arg[2];
	RpfFcstGroup	*grpinfo,
			*grpinfoPtr;
	char		buf[MAX_BUF_LEN],
	   		msg[MAX_BUF_LEN];
	int		cnt,
			i;
	
	
	
	/*
		Get the county info and load the
		group info dialog.
	*/
	grpinfo = GetRpfFcstGroup(" ORDER by group_id ");
	if ((cnt = ListCount(&grpinfo->list)) > 0)
	{
		xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
		grpinfoPtr = (RpfFcstGroup *) ListFirst(&grpinfo->list);
		for (i = 0; i < cnt; i++)
		{
			sprintf(buf, "%-20s", grpinfoPtr->group_id);
			xmStr[i] = XmStringCreateSimple(buf);
			grpinfoPtr  = (RpfFcstGroup *) ListNext(&grpinfoPtr->node);
		}
	}
	else /* no fcst groups have been defined */
	{
	   sprintf(msg, "No forecast groups have been defined under \"Setup\"");
	   ErrorDialog(riverDS, msg);
	   return(0);
	}
	
	
	/*
		Load the list box.
	*/
	XtSetArg(arg[0], XmNitemCount, cnt);
	XtSetArg(arg[1], XmNitems, xmStr);
	XtSetValues(glLB, arg, 2);
	XmListSelectPos(glLB, 1, True);
	
	
	
	/*
		Cleanup and return.
	*/
	for (i = 0; i < cnt; i++)
		XmStringFree(xmStr[i]);
	XtFree((char *) xmStr);
	
	FreeRpfFcstGroup(grpinfo);
	return(1);
}


void	close_grp(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (XtIsManaged(glDS))
   {
      XtDestroyWidget(glDS);
      glDS = NULL;
   }
   
   return;
}


void	clear_grp(Widget w, XtPointer ptr, XtPointer cbs)
{
   XmTextSetString(rgroupTxt, rgroup_none);
   close_grp(w, NULL, NULL);
   
   return;
}


void    copy_grp(Widget w, XtPointer ptr, XtPointer cbs)
{	
   /*
   	Sets a text widget of the RiverGage 
   	dialog. rgroupTxt belongs to RiverGage.
   */
   
   XmTextSetString(rgroupTxt, ChosenGroup());
   return;
}


char*	ChosenGroup(void)
{

        XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
        XmString        *xmStr;
        Arg             arg[MAX_ARGS];
        char            *text,
                        *grp;
        int             ac;

        /*
                Get the currently selected list item.
        */
        ac = 0;
        XtSetArg(arg[ac], XmNselectedItems, &xmStr); ac++;
        XtGetValues(glLB, arg, ac);


        /*
                Parse the selected items to get the
                grp.  
        */
        grp = 0;
        if (XmStringGetLtoR(xmStr[0], charset, &text))
        {
                grp = strtok(text, " \t");
        } 
        
        /*
        XtFree(text);  
        XmStringFree( *xmStr );
	*/
        return(grp);
}

