/*
	File:		flood_show.c
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:	Provide support for the Flood Damage DS.
	
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
#include "Flood.h"
#include "user_prefs.h"
#include "flood_cbs.h"
#include "flood.h"
#include "hybase.h"


char		flood_lid[LOC_ID_LEN + 1];
int		flood_state;


void	flood_show(Widget w, const char *lid)
{
	if (! floodDS)
	{	
		create_floodDS(GetTopShell(w));
		flood_callbacks();
	}

	
	if (! XtIsManaged(floodDS))
	{
		strcpy(flood_lid,lid);
	   	flood_state = flood_load(flood_lid);
		set_window_title(floodDS, "Flood Damage", flood_lid);
		XtManageChild(floodFM);
		XtManageChild(floodDS);
		XtUnmanageChild(flhelpPB);
	}
	
	return;
}


void	flood_callbacks(void)
{
	Atom		atom;
	
	/*
		Window manager callbacks.
	*/
	atom = XmInternAtom(XtDisplay(floodDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(floodDS, atom, flood_close, NULL);
	
	
	/*
		General widget callbacks.
	*/
        XtAddCallback(flstgLB,    XmNdefaultActionCallback,  flood_import,  NULL);
        XtAddCallback(flstgLB,    XmNbrowseSelectionCallback,flood_import,  NULL);
	XtAddCallback(flokPB,     XmNactivateCallback,       flood_ok,      NULL);
	XtAddCallback(flapplyPB,  XmNactivateCallback,       flood_apply,   NULL);
	XtAddCallback(flclsPB,    XmNactivateCallback,       flood_close,   NULL);
	XtAddCallback(flnewPB,    XmNactivateCallback,       flood_new,     NULL);
	XtAddCallback(fldeletePB, XmNactivateCallback,       flood_del_conf,NULL);
	

	/*
		TextFilter callbacks.
	*/
	XtAddCallback(flstageTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
	
	
	return;
}


void	flood_ok(Widget w, XtPointer ptr, XtPointer cbs)
{
   	if (flood_save())
   		flood_close(w, NULL, NULL);
	return;
}


void	flood_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
  	(void) flood_save();
	return;
}


int	flood_save(void)
{
	Flood		flood;
	char		*buf,
			key[MAX_WHERE_LEN],
			msg[MAX_BUF_LEN];
	int		error,
	   		pos,
			rtn;
			
			
	/*
		Associate lid to field entry.
	*/
	memset(&flood, '\0', sizeof(flood));
	strcpy(flood.lid, flood_lid);
	rtn = True;
	
	
	/*
		Get values from XmText widgets.
	*/
	if ( (buf = XmTextGetString(flstageTxt)) )
	{
		flood.stage = atof(buf);
		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(flstmtTxt)) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(flood.dispstmt, buf);
		XtFree(buf);
	}
	
		
	if ( (buf = XmTextGetString(fldamageTxt)) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
		   	strcpy(flood.damage, buf);
		XtFree(buf);
	}
	
	
	/*
		Ensure the entered stage is greater than zero.
	*/
	
	/*
	 * Modified by guoxian zhou 07-2004
	 * The flood damage should be accepted positive, zero, and 
	 * negative value.
	 * So comment out the following check function.

	if (! (flood.stage > 0.0))
	{
	 	sprintf(msg, "A non-zero stage must be entered.");
		ErrorDialog(floodDS, msg);
		DeSensitize(flokPB);
		DeSensitize(flapplyPB);
		return(False);
	}

	*/

	
	/*
                Insert/Update the entered dbms
                record, based on the state of
                the state variable.
        */
        if (flood_state)
       	{
		flood_key(key, &pos);
		if (strlen(key))
		{
			if ((error = UpdateFlood(&flood, key)) != 0)
			{
			 	sprintf(msg, "Unable to update record: %d\n", error);
				ErrorDialog(floodDS, msg);
				rtn = False;
			}
		}
       	}
        else
	{
        	if ((error = PutFlood(&flood)) != 0)
		{
		 	sprintf(msg, "Unable to insert record: %d\n", error);
			ErrorDialog(floodDS, msg);
			rtn = False;
		}
        }
		
        	
        /*
        	Reset Insert/Update state to 
        	True, and return.
        */
        flood_state = flood_load(flood_lid);
	return(rtn);
}


void	flood_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if(XtIsManaged(floodDS))
   {
      XtDestroyWidget(floodDS);
      floodDS = NULL;
   }
   
   return;
}


void	flood_new(Widget w, XtPointer ptr, XtPointer cbs)
{
   	clearForm(floodFM);
	Sensitize(flokPB);
	Sensitize(flapplyPB);
	flood_state = False;
	return;
}


void	flood_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{        
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete this entry?");
   qstDS = QuestionDialog(floodDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, flood_delete, NULL);
   
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	flood_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
	char	key[MAX_WHERE_LEN];
	int	pos;


	/*
		Delete record from dbms,
		and reload list and select pos.
	*/
	SetCursor(floodFM, XC_watch);
	flood_key(key, &pos);
	if (strlen(key))
	{
		(void) DeleteFlood(key);
		(void) flood_load(flood_lid);
		XmListSelectPos(flstgLB, pos, True);
	}
	UnsetCursor(floodFM);
	
	return;
}


void	flood_key(char *key, int *pos)
{
   Flood	*flood,
      		*fldPtr;
   
   char		where[MAX_WHERE_LEN];
   
   int		*poslist,
      		cnt;
   
   
   XmListGetSelectedPos(flstgLB, &poslist, &cnt);
   sprintf(where, " WHERE lid = '%s' ORDER BY stage DESC ", flood_lid);
   if ((flood = GetFlood(where)) != NULL)
   {
      fldPtr = (Flood *) ListNth(&flood->list, poslist[0]);
      if (fldPtr)
      {
	 memset(where, '\0', sizeof(where));
	 sprintf(where, " WHERE lid = '%s' AND ROUND(CAST(stage AS NUMERIC), 2) = %8.2f ",
		 fldPtr->lid, fldPtr->stage);
	 
	 *pos = poslist[0];
	 strcpy(key, where);
	 XtFree((char *)poslist);
      }
      
      FreeFlood(flood);
   }
   
   return;
}




int	flood_load(const char *lid)
{
	XmStringTable	xmStr;
	Flood		*flood,
			*fldPtr;
	char		where[MAX_WHERE_LEN],
			buf[MAX_BUF_LEN],
			stgbuf[9];
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
	clearForm(floodFM);
	XmListDeleteAllItems(flstgLB);
        state = False;    
        cnt   = 0;
        
        
        sprintf(where, " WHERE lid = '%s' ORDER BY stage DESC ", lid);
        if ((flood = GetFlood(where)) != NULL)
        {
                cnt     = ListCount(&flood->list);
                xmStr   = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
                fldPtr   = (Flood *) ListFirst(&flood->list);
                for (i = 0; fldPtr; i++)
                {
			DataToString(&fldPtr->stage, DOUBLE, stgbuf, "%8.2lf", "");
			sprintf(buf, "%-8s       %-60s",
                                stgbuf, fldPtr->dispstmt);
                        xmStr[i] = XmStringCreateSimple(buf);
                        fldPtr = (Flood *) ListNext(&fldPtr->node);
                }


		XmListAddItems(flstgLB, xmStr, cnt, 1);
                XmListSelectPos(flstgLB, 1, True);

        	/*
                	cleanup.
        	*/
        	for (i = 0; i < cnt; i++)
                	XmStringFree(xmStr[i]);
		XtFree((char *) xmStr);
                FreeFlood(flood);
                
                
		Sensitize(flokPB);
		Sensitize(flapplyPB);
                Sensitize(fldeletePB);
                state = True;
        }
        else
        {
		DeSensitize(flokPB);
		DeSensitize(flapplyPB);
		DeSensitize(fldeletePB);
        }


        return(state);	
}


void    flood_import(Widget w, XtPointer ptr, XmListCallbackStruct *cbs)
{
	Flood		*flood,
                       	*fldPtr;
        char           	where[MAX_WHERE_LEN],
                        buf[DATE_LEN + 1];

        /*
                External variable(s).
        */
        extern char     flood_lid[LOC_ID_LEN + 1];


        /*
                Retrieve data for the current lid from the
                dbms, and search for the currently selected
                offset in the XmList widget.  Update
                Xm widgets on XmForm work area with
                appropriate values.
        */
        sprintf(where, " WHERE lid = '%s' ORDER BY stage DESC ", flood_lid);
        if ((flood = GetFlood(where)) != NULL)
	{
	   fldPtr  = (Flood *) ListNth(&flood->list, cbs->item_position);
	   if (fldPtr)
	   {
	      DataToString(&fldPtr->stage, DOUBLE, buf, "%8.2lf", "");
	      XmTextSetString(flstageTxt, buf);
	      XmTextSetString(flstmtTxt, fldPtr->dispstmt);
	      XmTextSetString(fldamageTxt, fldPtr->damage);
	   }
	   
	   FreeFlood(flood);
	}


        return;
}


