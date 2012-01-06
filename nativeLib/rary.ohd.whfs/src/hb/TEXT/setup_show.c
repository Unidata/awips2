/*
	File:		setup_show.c
	Date:		May 1995
	Author:		Dale Shelton, Chip Gobs, Paul Taylor (4/3/97)
	
	Purpose:	Provides support for Reference Fields DS.
*/

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>
#include "Admin.h"
#include "DbmsUtils.h"
#include "Xtools.h"
#include "setup_funcs.h"
#include "setup_tree.h"
#include "setup_cbs.h"
#include "setup.h"
#include "hybase.h"

extern struct _SetupTree setup [ ] ;
int			setup_state ;

void	setup_show(Widget w)
{
	if (! rfieldsDS)
	{
	   create_rfieldsDS(GetTopShell(w));
	   setup_callbacks();
	}
	
	if (! XtIsManaged(rfieldsDS))
	{
	   XtManageChild(rfieldsFO);
	   XtManageChild(rfieldsDS);
	   
	   SetMenuPos(rfieldsOM, 0);
	   setup_reset(GetMenuHistory(rfieldsOM), NULL);
           /******
            Removing menu options
           ******/
           XtUnmanageChild(coopCommsPB);
           XtUnmanageChild(coopRecipPB);
           XtUnmanageChild(coopSponsPB);
           XtUnmanageChild(dcpOwnerPB);
           XtUnmanageChild(gageOwnerPB);
           XtUnmanageChild(gageMaintPB);
           XtUnmanageChild(gageTypePB);
           XtUnmanageChild(hsaPB);
           XtUnmanageChild(netPB);
           XtUnmanageChild(rfcPB);
           XtUnmanageChild(townerPB);
           XtUnmanageChild(tpayorPB);
           XtUnmanageChild(ttypePB);
           XtUnmanageChild(wfoPB);
	}
	
	return;
}


void	setup_callbacks(void)
{
	Atom		atom;

		
	/*
		Add callbacks.
	*/
	atom = XmInternAtom(XtDisplay(rfieldsDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(rfieldsDS, atom, setup_close, NULL);
	
	
	/*
		Option menu callbacks.
	*/
/*******The following are removed from the option menu

	XtAddCallback(coopCommsPB, XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(coopRecipPB, XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(coopSponsPB, XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(dcpOwnerPB,  XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(gageTypePB,  XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(gageMaintPB, XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(gageOwnerPB, XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(hsaPB,       XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(netPB,       XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(rownerPB,    XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(rtypePB,     XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(rfcPB,       XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(ttypePB,     XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(tpayorPB,    XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(townerPB,    XmNactivateCallback, setup_reset, NULL);
	XtAddCallback(wfoPB,       XmNactivateCallback, setup_reset, NULL);
*/
        XtAddCallback(rownerPB,    XmNactivateCallback, setup_reset, NULL);
        XtAddCallback(rtypePB,     XmNactivateCallback, setup_reset, NULL);

	
	
	/*
		Main form callbacks.
	*/
	XtAddCallback(rfields_deletePB, XmNactivateCallback, setup_del_conf, NULL);
	XtAddCallback(rfields_addPB,    XmNactivateCallback, setup_insert,   NULL);
	XtAddCallback(rfields_updatePB, XmNactivateCallback, setup_update,   NULL);
	XtAddCallback(rfields_okPB,     XmNactivateCallback, setup_close,    NULL);
	return;
}



void	setup_reset(Widget w, XtPointer ptr, XtPointer cbs)
{
	int	pos;
	
	
	XmTextSetString(rfieldsTE, "");
	pos = GetMenuPos(rfieldsOM);
	setup[pos].load();
	return;
}


char*	setup_value(void)
{
	XmStringCharSet		charset = XmSTRING_DEFAULT_CHARSET;
	XmString		*xmStr;
	Arg			arg[2];
	char			*text;
  
	
        /*
                Get the currently selected list item.
        */
        XtSetArg(arg[0], XmNselectedItems, &xmStr);
        XtGetValues(rfieldsLI, arg, 1);


        /*
                Get the text value from the XmList & return it.
        */
        if (XmStringGetLtoR(xmStr[0], charset, &text))
	   return(text);
	else
	   return(NULL);
}


void	setup_load(XmStringTable *xmStr, int count)
{
	Arg	arg[1];
	int	num;
        XmStringTable xmStrTable ;

        if ( *xmStr == NULL )
        {
           return ;
        }

        xmStrTable = *xmStr ;
	
	num = 0;
	XtSetArg(arg[0], XmNitemCount, &num);
	XtGetValues(rfieldsLI, arg, 1);
	
	if (num > 0)
		XmListDeleteAllItems(rfieldsLI);
		
	XmListAddItems( rfieldsLI , ( XmString * ) xmStrTable , count , 1 ) ;
	XmListSelectPos(rfieldsLI, 1, True);
	return; 
}


void	setup_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete this entry?");
   qstDS = QuestionDialog(rfieldsDS, buf);
   SetTitle(qstDS,"Delete Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, setup_delete, NULL);
   
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	setup_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
	char	where[MAX_WHERE_LEN];
	int	pos;
	
	
	SetCursor(rfieldsFO, XC_watch);
	
	pos = GetMenuPos(rfieldsOM);	
	sprintf(where, " WHERE %s = '%s' ", setup[pos].name, setup_value());
	setup[pos].delete(where);
	setup_reset(w, NULL, NULL);
	
	UnsetCursor(rfieldsFO);
	
	return;
}


void	setup_insert(Widget w, XtPointer ptr, XtPointer cbs)
{
	char		*buf;
	int		pos;

	
	pos = GetMenuPos(rfieldsOM);
	buf = XmTextGetString(rfieldsTE);
	if (IsNull(CHAR, buf) == NOTNULL)
	{
		strcpy(setup[pos].field, buf);
		setup[pos].insert(setup[pos].dataPtr);
		setup_reset(w, NULL, NULL);
	}
	else
	{
		ErrorDialog(rfieldsDS, "Insertion of NULL values is not allowed");
	}	
	return;
}


void	setup_update(Widget w, XtPointer ptr, XtPointer cbs)
{
	char		where[MAX_WHERE_LEN],
			*buf;
	int		pos;
	
	
	pos = GetMenuPos(rfieldsOM);
	buf = XmTextGetString(rfieldsTE);
	if (IsNull(CHAR, buf) == NOTNULL)
	{
		strcpy(setup[pos].field, buf);
		sprintf(where, " WHERE %s = '%s' ", setup[pos].name, setup_value());
		setup[pos].update(setup[pos].dataPtr, where);
		setup_reset(w, NULL, NULL);
	}
	else
	{
		ErrorDialog(rfieldsDS, "Update of NULL values is not allowed.");
	}
	
	return;
}


void	setup_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if(XtIsManaged(rfieldsDS))
   {
      XtDestroyWidget(rfieldsDS);
      rfieldsDS = NULL;
   }
   
   return;
}
