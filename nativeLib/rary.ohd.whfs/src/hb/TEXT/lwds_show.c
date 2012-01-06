/*
	File:		lwds_show.c
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:	Provide support for the Low Water DS.
	
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
/*** POSTGRES
#include <sqlhdr.h>
*****/
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "GeneralUtil.h"
#include "Lowwater.h"
#include "user_prefs.h"
#include "lwds_cbs.h"
#include "lwds.h"
#include "hybase.h"
#include "time_convert.h"


char		lwds_lid[LOC_ID_LEN + 1];
int		lwds_state;


void	lwds_show(Widget w, const char *lid)
{
	if (! lwDS)
	{	
		create_lwDS(GetTopShell(w));
		lwds_callbacks();
	}

	
	if (! XtIsManaged(lwDS))
	{
		strcpy(lwds_lid, lid);
		lwds_state = lwds_load(lwds_lid);
		set_window_title(lwDS, "Low Water", lwds_lid);
		XtManageChild(lwFM);
		XtManageChild(lwDS);
	}
	
	return;
}


void	lwds_callbacks(void)
{
	Atom		atom;
	
	/*
		Window manager callbacks.
	*/
	atom = XmInternAtom(XtDisplay(lwDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(lwDS, atom, lwds_close, NULL);
	
	
	/*
		General widget callbacks.
	*/
        XtAddCallback(lwLB,      XmNdefaultActionCallback, lwds_import, NULL);
        XtAddCallback(lwLB,      XmNbrowseSelectionCallback, lwds_import, NULL);
	XtAddCallback(lwokPB,    XmNactivateCallback, lwds_ok, NULL);
	XtAddCallback(lwapplyPB, XmNactivateCallback, lwds_apply, NULL);
	XtAddCallback(lwclosePB, XmNactivateCallback, lwds_close, NULL);
	XtAddCallback(lwnewPB,   XmNactivateCallback, lwds_new, NULL);
	XtAddCallback(lwdelPB,   XmNactivateCallback, lwds_del_conf, NULL);
	
	
	/*
		TextFilter callbacks.
	*/
	lwds_add_textfilter_callbacks();

	
	return;
}


void	lwds_add_textfilter_callbacks(void)
{
	XtAddCallback(lwstgTxt,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
	XtAddCallback(lwdateTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
	XtAddCallback(lwqTxt,    XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
	
	return;
}


void	lwds_remove_textfilter_callbacks(void)
{
	XtRemoveCallback(lwstgTxt,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
	XtRemoveCallback(lwdateTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
	XtRemoveCallback(lwqTxt,    XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
	
	return;
}


void	lwds_ok(Widget w, XtPointer ptr, XtPointer cbs)
{
	if (lwds_save())
		lwds_close(w, NULL, NULL);
	return;
}


void	lwds_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
	lwds_save();
	return;
}


int	lwds_save(void)
{
	Lowwater	lw;
	char		*buf,
			key[MAX_WHERE_LEN],
			msg[MAX_BUF_LEN];
	long		date;
	int		error,
	   		pos,
			rtn,
                        status;
			
			
	/*
		Associate lid to field entry.
	*/
	memset(&lw, '\0', sizeof(lw));
	strcpy(lw.lid, lwds_lid);
	
	
	/*
		Retrieve values and load into dbms.	
	*/
	if ( (buf = XmTextGetString(lwstgTxt)) )
	{
	   if (!IsBlankStr(buf))
	      lw.stage = atof(buf);
	   else
	      (void) SetNull(DOUBLE, (void *) &lw.stage);

	   XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(lwqTxt)) )
	{
	   if((lw.q = atol(buf)) < 0)
	   {
	      (void) SetNull(INT, (void *) &lw.q);
	   }

	   if (IsBlankStr(buf)) 
	   {
		(void) SetNull(INT, (void *) &lw.q);
	   }
	   
	   XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(lwdateTxt)) )
	{
		if (four_digit_year(buf) != 0)
		{
		  sprintf(msg, "Invalid date '%s' entered, 4 digit year required.\n", 
			  buf);
		  ErrorDialog(lwDS, msg);
		  /* leave function, do not save data */
		  return(False);
		}
		else
		{
                  status = USA_date_to_date_t ( buf, &date );

		  if ( status != 0 )
		  {
		    sprintf(msg, "Invalid date '%s' entered, check month\n"
                                 "and day.\n", buf);
		    ErrorDialog(lwDS, msg);
		    /* leave function, do not save data */
		    return(False);
		  }
		  else
	   	    lw.lwdat = date;
	   	}

	 	XtFree(buf);  
	}
	
	
	if ( (buf = XmTextGetString(lwnoteTxt)) )
	{
	   	strcpy(lw.lwrem, buf);
	 	XtFree(buf);  
	}
	
	
	
	/*
                Insert/Update the entered dbms
                record, based on the state of
                the state variable.
        */
	rtn = True;
        if (lwds_state)
       	{
		lwds_key(key, &pos);
		if (strlen(key))
		{
			if ((error = UpdateLowwater(&lw, key)) != 0)
			{
				sprintf(msg, "Unable to update record: %d\n", error);
				ErrorDialog(lwDS, msg);
				rtn = False;
			}
		}
       	}
        else
	{
        	if ((error = PutLowwater(&lw)) != 0)
		{
		 	sprintf(msg, "Unable to insert record: %d\n", error);
			ErrorDialog(lwDS, msg);
			rtn = False;
		}
	}        	
      
  	
        /*
        	Reset Insert/Update state to 
        	True, and return.
        */
        lwds_state = lwds_load(lwds_lid);
	return(rtn);
}


void	lwds_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if(XtIsManaged(lwDS))
   {
      XtDestroyWidget(lwDS);
      lwDS = NULL;
   }
   
   return;
}


void	lwds_new(Widget w, XtPointer ptr, XtPointer cbs)
{
   	clearForm(lwFM);
	Sensitize(lwokPB);
	Sensitize(lwapplyPB);
	lwds_state = False;
	return;
}


void	lwds_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{        
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete this entry?");
   qstDS = QuestionDialog(lwDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, lwds_delete, NULL);
   
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	lwds_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
	char	key[MAX_WHERE_LEN];
	int	pos;


	/*
		Delete record from dbms,
		and reload list and select pos.
	*/
	SetCursor(lwFM, XC_watch);
	lwds_key(key, &pos);
	if (strlen(key))
	{
		DeleteLowwater(key);
		(void) lwds_load(lwds_lid);
		XmListSelectPos(lwLB, pos, True);
	}
	UnsetCursor(lwFM);
	
	return;
}


void	lwds_clear(void)
{
	XmTextSetString(lwstgTxt,  "");
	XmTextSetString(lwqTxt,    "");
	XmTextSetString(lwdateTxt, "");
	XmTextSetString(lwnoteTxt, "");
	return;
}


void	lwds_key(char *key, int *pos)
{
	Lowwater	*lw,
			*lwPtr;
	char		where[MAX_WHERE_LEN];
	int		*poslist,
			cnt;
	
	
	XmListGetSelectedPos(lwLB, &poslist, &cnt);
	sprintf(where, " WHERE lid = '%s' ORDER BY stage DESC, lwdat DESC ", lwds_lid);
	if ((lw = GetLowwater(where)) != NULL)
	{
		lwPtr = (Lowwater *) ListNth(&lw->list, poslist[0]);
		if (lwPtr)
		{
			memset(where, '\0', sizeof(where));
			sprintf(where, " WHERE lid = '%s' and lwdat = '%ld' ",
				lwPtr->lid, lwPtr->lwdat);

			*pos = poslist[0];
			strcpy(key, where);
			XtFree((char *)poslist);
		}
		
		FreeLowwater(lw);
	}
	
	
	return;
}


int	lwds_load(const char *lid)
{
	XmStringTable	xmStr;
	Arg		arg[5];
	Lowwater	*lw,
			*lwPtr;
	char		where[MAX_WHERE_LEN],
			buf[MAX_BUF_LEN],
			datebuf[DATE_LEN + 1],
			flowbuf[9],
			stgbuf[12];
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
	clearForm(lwFM);
	XmListDeleteAllItems(lwLB);
       	state = False;    
        cnt   = 0;
        
        
        sprintf(where, " WHERE lid = '%s' ORDER BY stage DESC, lwdat DESC ", lid);
        if ((lw = GetLowwater(where)) != NULL)
        {
                cnt     = ListCount(&lw->list);
                xmStr   = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
                lwPtr   = (Lowwater *) ListFirst(&lw->list);
                for (i = 0; lwPtr; i++)
                {
		   if(! IsNull(DOUBLE, (void*)&lwPtr->stage))
		      DataToString(&lwPtr->stage, DOUBLE, stgbuf, "%8.2lf", "");
		   else
		      strcpy(stgbuf, "");
		   
		   if(! IsNull(INT, (void*)&lwPtr->q))
		      DataToString(&lwPtr->q, INT, flowbuf, "%8d", "");
		   else
		      strcpy(flowbuf, "");
		   
                   date_t_to_USA_date ( lwPtr->lwdat, datebuf );
		   
		   sprintf(buf, "%-24s%-27s%-12s", stgbuf, flowbuf, datebuf);
		   xmStr[i] = XmStringCreateSimple(buf);
		   lwPtr = (Lowwater *) ListNext(&lwPtr->node);
                }


                XtSetArg(arg[0], XmNitemCount, cnt);
                XtSetArg(arg[1], XmNitems, xmStr);
                XtSetValues(lwLB, arg, 2);
                XmListSelectPos(lwLB, 1, True);

        	/*
                	cleanup.
        	*/
        	for (i = 0; i < cnt; i++)
                	XmStringFree(xmStr[i]);
		XtFree((char *) xmStr);
                FreeLowwater(lw);
                
                
		Sensitize(lwokPB);
		Sensitize(lwapplyPB);
                Sensitize(lwdelPB);
                state = True;
        }
        else
        {
	   	DeSensitize(lwokPB);
		DeSensitize(lwapplyPB);
         	DeSensitize(lwdelPB);
        }


        return(state);	
}


void    lwds_import(Widget w, XtPointer ptr, XmListCallbackStruct *cbs)
{
        Lowwater            	*lw,
                        	*lwPtr;
        char            	where[MAX_WHERE_LEN],
                        	buf[DATE_LEN + 1];

        /*
                External variable(s).
        */
        extern char     	lwds_lid[LOC_ID_LEN + 1];


	
	lwds_remove_textfilter_callbacks();

	
        /*
                Retrieve data for the current lid from the
                dbms, and search for the currently selected
                offset in the XmList widget.  Update
                Xm widgets on XmForm work area with
                appropriate values.
        */
        sprintf(where, " WHERE lid = '%s' ORDER BY stage DESC, lwdat DESC ", lwds_lid);
        if ((lw = GetLowwater(where)) != NULL)
        {
        	lwPtr  = (Lowwater *) ListNth(&lw->list, cbs->item_position);
        	if (lwPtr)
        	{
		   if(! IsNull(DOUBLE, (void*)&lwPtr->stage))
		      DataToString(&lwPtr->stage, DOUBLE, buf, "%8.2lf", "");
		   else
		      strcpy(buf, "");
		   XmTextSetString(lwstgTxt, buf);
		   
		   if(! IsNull(INT, (void*)&lwPtr->q))
		      DataToString(&lwPtr->q, INT, buf, "%8d", "");
		   else
		      strcpy(buf, "");
		   XmTextSetString(lwqTxt, buf);
		   
                   date_t_to_USA_date ( lwPtr->lwdat, buf );
		   XmTextSetString(lwdateTxt, buf);
		   
		   if (lwPtr->lwrem)
		      XmTextSetString(lwnoteTxt, lwPtr->lwrem);
        	}


       		/*
                	Cleanup.        
        	*/
        	FreeLowwater(lw);
        }

	
	lwds_add_textfilter_callbacks();
	
	
        return;
}
