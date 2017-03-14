/*
	File:		refer_show.c
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:	Provide support for the References DS.
	
*/


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <X11/cursorfont.h>
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "Refer.h"
#include "user_prefs.h"
#include "refer_cbs.h"
#include "refer.h"
#include "hybase.h"


char		refer_lid[LOC_ID_LEN + 1];
int		refer_state;


void	refer_show(Widget w, const char *lid)
{
	if (! referDS)
	{	
		create_referDS(GetTopShell(w));
		refer_callbacks();
	}

	
	if (! XtIsManaged(referDS))
	{
		strcpy(refer_lid, lid);
		refer_state = refer_load(refer_lid);
		set_window_title(referDS, "References", refer_lid);
		XtManageChild(referFM);
		XtManageChild(referDS);
	}
	
	return;
}


void	refer_callbacks(void)
{
	Atom		atom;
	
	/*
		Window manager callbacks.
	*/
	atom = XmInternAtom(XtDisplay(referDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(referDS, atom, refer_close, NULL);
	
	
	/*
		Widget callbacks.
	*/
        XtAddCallback(referLB, XmNdefaultActionCallback, refer_import, NULL);
        XtAddCallback(referLB, XmNbrowseSelectionCallback, refer_import, NULL);
	XtAddCallback(rfokPB, XmNactivateCallback, refer_ok, NULL);
	XtAddCallback(rfapplyPB, XmNactivateCallback, refer_apply, NULL);
	XtAddCallback(rfclosePB, XmNactivateCallback, refer_close, NULL);
	XtAddCallback(rfnewPB, XmNactivateCallback, refer_new, NULL);
	XtAddCallback(rfdelPB, XmNactivateCallback, refer_del_conf, NULL);
	return;
}


void	refer_ok(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (refer_save())
      refer_close(w, NULL, NULL);
   return;
}


void	refer_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
 	(void) refer_save();
	return;
}


int	refer_save(void)
{
	Refer		refer;
	char		*buf = NULL ,
			key[MAX_WHERE_LEN],
			msg[MAX_BUF_LEN];
	int		error,
	   		pos,
			rtn;
				
			
	/*
		Associate lid to field entry.
	*/
	memset(&refer, '\0', sizeof(refer));
	strcpy(refer.lid, refer_lid);
	
	
	/*
		Get values from XmText widgets.
	*/	
	if ( ( buf = XmTextGetString(rfrefTxt) ) != NULL )
	{	
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(refer.reference, buf);
		XtFree(buf);
	}
		

	/*
                Insert/Update the entered dbms
                record, based on the state of
                the state variable.
        */
	rtn = True;
        if (refer_state)
       	{
		refer_key(key, &pos);
		if (strlen(key))
		{
			if ((error = UpdateRefer(&refer, key)) != 0)
			{
			 	sprintf(msg, "Unable to update record: %d\n", error);
				ErrorDialog(referDS, msg);
				rtn = False;
			}
		}
       	}
        else
	{
        	if ((error = PutRefer(&refer)) != 0)
		{
		 	sprintf(msg, "Unable to insert record: %d\n", error);
			ErrorDialog(referDS, msg);
			rtn = False;
		}
	}
        	
        	
        /*
        	Reset Insert/Update state to 
        	True, and return.
        */
        refer_state = refer_load(refer_lid);
	return(rtn);
}


void	refer_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (XtIsManaged(referDS))
   {
	XtDestroyWidget(referDS);
	referDS = NULL;
   }

   return;
}


void	refer_new(Widget w, XtPointer ptr, XtPointer cbs)
{
	refer_clear();
	Sensitize(rfokPB);
	Sensitize(rfapplyPB);
	refer_state = False;
	return;
}


void	refer_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{        
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete this entry?");
   qstDS = QuestionDialog(referDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, refer_delete, NULL);
   
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	refer_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
	char	key[MAX_WHERE_LEN];
	int	pos;


	/*
		Delete record from dbms,
		and reload list and select pos.
	*/
	SetCursor(referFM, XC_watch);
	refer_key(key, &pos);
	if (key)
	{
		DeleteRefer(key);
		(void) refer_load(refer_lid);
		XmListSelectPos(referLB, pos, True);
	}
	UnsetCursor(referFM);
	
	return;
}


void	refer_clear(void)
{
	XmTextSetString(rfrefTxt, "");
	return;
}


void	refer_key(char *key, int *pos)
{
	Refer		*refer,
			*refPtr;
	char		where[MAX_WHERE_LEN];
	int		*poslist,
			cnt;
	
	
	XmListGetSelectedPos(referLB, &poslist, &cnt);
	sprintf(where, " WHERE lid = '%s' ORDER BY reference DESC ", refer_lid);
	if ((refer = GetRefer(where)) != NULL)
	{
		refPtr = (Refer *) ListNth(&refer->list, poslist[0]);
		if (refPtr)
		{
			memset(where, '\0', sizeof(where));
			sprintf(where, " WHERE lid = '%s' AND reference = '%s' ",
				refPtr->lid, refPtr->reference);

			*pos = poslist[0];
			strcpy(key, where);
			XtFree((char *)poslist);
		}
		
		FreeRefer(refer);
	}
	
	return;
}




int	refer_load(const char *lid)
{
	XmStringTable	xmStr;
	Refer		*refer,
			*refPtr;
	char		where[MAX_WHERE_LEN],
			buf[MAX_BUF_LEN];
	int		state,
			cnt,
			i;
 
         /*
                Set state variable to False,
                pending retrieval of a record
                from the database.  If a record is
                retrieved, state variable will
                be set to true.
        */
        refer_clear();
	XmListDeleteAllItems(referLB);
        state = False;
   
        
        sprintf(where, " WHERE lid = '%s' ORDER BY reference DESC ", lid);
        if ((refer = GetRefer(where)) != NULL)
        {
                cnt     = ListCount(&refer->list);
                xmStr   = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
                refPtr   = (Refer *) ListFirst(&refer->list);
                for (i = 0; refPtr; i++)
                {
			sprintf(buf, "%s", refPtr->reference);
                        xmStr[i] = XmStringCreateSimple(buf);
                        refPtr = (Refer *) ListNext(&refPtr->node);
                }


     		XmListAddItems(referLB, xmStr, cnt, 1);
		XmListSelectPos(referLB, 1, True);
                
		
        	/*
                	cleanup.
        	*/
        	for (i = 0; i < cnt; i++)
                	XmStringFree(xmStr[i]);
                XtFree((char *) xmStr);
                FreeRefer(refer);
                
                
		Sensitize(rfokPB);
		Sensitize(rfapplyPB);
                Sensitize(rfdelPB);
                state = True;
        }
	else
	{
		DeSensitize(rfokPB);
		DeSensitize(rfapplyPB);
		DeSensitize(rfdelPB);
	}


        return(state);	
}


void    refer_import(Widget w, XtPointer ptr, XmListCallbackStruct *cbs)
{
        Refer           *refers,
                        *refPtr;
        char            where[MAX_WHERE_LEN] ;

        /*
                External variable(s).
        */
        extern char     refer_lid[LOC_ID_LEN + 1];


        /*
                Retrieve data for the current lid from the
                dbms, and search for the currently selected
                offset in the XmList widget.  Update
                Xm widgets on XmForm work area with
                appropriate values.
        */
        sprintf(where, " WHERE lid = '%s' ORDER BY reference DESC ", refer_lid);
        if ((refers = GetRefer(where)) != NULL)
        {
        	refPtr  = (Refer *) ListNth(&refers->list, cbs->item_position);
        	if (refPtr)
                	XmTextSetString(rfrefTxt, refPtr->reference);
 

		/*
                	Cleanup.        
        	*/
        	FreeRefer(refers);
        }
        
        
        return;
}
