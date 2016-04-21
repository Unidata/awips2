/*
	File:		pubds_show.c
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:	Provide support for the Publications DS.
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
/***** POSTGRES
#include <sqlhdr.h>
*****/
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "Pub.h"
#include "user_prefs.h"
#include "pubds_cbs.h"
#include "pubds.h"
#include "hybase.h"
#include "time_convert.h"

char		pubds_lid[LOC_ID_LEN + 1];
int		pubds_state;


void	pubds_show(Widget w, const char *lid)
{
   if (! pubDS)
   {	
      create_pubDS(GetTopShell(w));
      pubds_callbacks();
   }
   
   if (! XtIsManaged(pubDS))
   {
      strcpy(pubds_lid, lid);
      pubds_state = pubds_load(pubds_lid);
      set_window_title(pubDS, "Publications", (char *)pubds_lid);
      XtManageChild(pubFM);
      XtManageChild(pubDS);
   }
   
   return;
}


void	pubds_callbacks(void)
{
	Atom		atom;
	
	/*
		Window manager callbacks.
	*/
	atom = XmInternAtom(XtDisplay(pubDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(pubDS, atom, pubds_close, NULL);
	
	
	/*
		General widget callbacks.
	*/
        XtAddCallback(pubLB,      XmNdefaultActionCallback,  pubds_import,  NULL);
        XtAddCallback(pubLB,      XmNbrowseSelectionCallback,pubds_import,  NULL);
	XtAddCallback(pubokPB,    XmNactivateCallback,       pubds_ok,      NULL);
	XtAddCallback(pubapplyPB, XmNactivateCallback,       pubds_apply,   NULL);
	XtAddCallback(pubclosePB, XmNactivateCallback,       pubds_close,   NULL);
	XtAddCallback(pubnewPB,   XmNactivateCallback,       pubds_new,     NULL);
	XtAddCallback(pubdelPB,   XmNactivateCallback,       pubds_del_conf,NULL);

	
	/*
		TextFilter callbacks.
	*/
	XtAddCallback(pubbegTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
	XtAddCallback(pubendTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
	
	return;
}


void	pubds_ok(Widget w, XtPointer ptr, XtPointer cbs)
{
	if (pubds_save())
		pubds_close(w, NULL, NULL);
	return;
}


void	pubds_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
  	pubds_save();
	return;
}


int	pubds_save(void)
{
	Pub		pub;
	char		*buf,
			key[MAX_WHERE_LEN],
			msg[MAX_BUF_LEN];
	long		date;
	int		error,
	   		pos,
			rtn;
        int             status;
			
	/*
		Associate lid to field entry.
	*/
	memset(&pub, '\0', sizeof(pub));
	strcpy(pub.lid, pubds_lid);
	
	
	/*
		Get values from XmText widgets.
	*/
	if ( (buf = XmTextGetString(pubbegTxt)) )
	{
		if (four_digit_year(buf) != 0)
		{
		  sprintf(msg, "Invalid date '%s' entered, 4 digit year required.\n", 
			  buf);
		  ErrorDialog(pubDS, msg);
		  /* leave function, do not save data */
		  return(False);
		}
		else
		{
                  
                  status = USA_date_to_date_t ( buf, &date );

		  if ( status != 0 )
		  {
		    sprintf(msg, "Invalid date '%s' entered, check month and\n"
                                 "day.\n", buf);
		    ErrorDialog(pubDS, msg);
		    /* leave function, do not save data */
		    return(False);
		  }
		  else
		    pub.pbegin = date;
		}
			
		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(pubendTxt)) )
	{
                if(strnlen(buf) ==0)
                {
                   SetNull(LONG, (void *) &pub.pend);
                }

		else if (four_digit_year(buf) != 0)
		{
		  sprintf(msg, "Invalid date '%s' entered, 4 digit year required.\n", 
			  buf);
		  ErrorDialog(pubDS, msg);
		  /* leave function, do not save data */
		  return(False);
		}
		else
		{
                  status = USA_date_to_date_t ( buf, &date );

		  if ( status != 0 )
		  {
		    sprintf(msg, "Invalid date '%s' entered, check month and\n"
                                 "day.\n", buf);
		    ErrorDialog(pubDS, msg);

		    /* leave function, do not save data */
		    return(False);
		  }
		  else
		    pub.pend = date;
		}
			
		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(publocTxt)) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(pub.ppub, buf);
		XtFree(buf);
	}
			

	/*
                Insert/Update the entered dbms
                record, based on the state of
                the state variable.
        */
	rtn = True;
        if (pubds_state)
       	{
		pubds_key(key, &pos);
		if (strlen(key))
			if ((error = UpdatePub(&pub, key)) != 0)
			{
			 	sprintf(msg, "Unable to update record: %d\n", error);
				ErrorDialog(pubDS, msg);
				rtn = False;
			}
       	}
        else
	{
        	if ((error = PutPub(&pub)) != 0)
		{
		   	sprintf(msg, "Unable to insert record: %d\n", error);
			ErrorDialog(pubDS, msg);
			rtn = False;
		}
	}
        	
        	
        /*
        	Reset Insert/Update state to 
        	True, and return.
        */
        pubds_state = pubds_load(pubds_lid);
	return(rtn);
}


void	pubds_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (XtIsManaged(pubDS))
   {
      XtDestroyWidget(pubDS);
      pubDS = NULL;
   }
   
   return;
}


void	pubds_new(Widget w, XtPointer ptr, XtPointer cbs)
{
   clearForm(pubFM);
   Sensitize(pubokPB);
   Sensitize(pubapplyPB);
   pubds_state = False;
   
   return;
}


void	pubds_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{        
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete this entry?");
   qstDS = QuestionDialog(pubDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, pubds_delete, NULL);
   
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	pubds_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
	char	where[MAX_WHERE_LEN];
	int	pos;


	/*
		Delete record from dbms,
		and reload list and select pos.
	*/
	SetCursor(pubFM, XC_watch);
	pubds_key(where, &pos);
	if (strlen(where))
	{
		DeletePub(where);
		(void) pubds_load(pubds_lid);
		XmListSelectPos(pubLB, pos, True);
	}
	UnsetCursor(pubFM);
	
	return;
}


void	pubds_clear(void)
{
	XmTextSetString(pubbegTxt, "");
	XmTextSetString(pubendTxt, "");
	XmTextSetString(publocTxt, "");
	return;
}


void	pubds_key(char *key, int *pos)
{
	Pub		*pubds,
			*pubPtr;
	char		where[MAX_WHERE_LEN];
	int		*poslist,
			cnt;
	
	
	XmListGetSelectedPos(pubLB, &poslist, &cnt);
	sprintf(where, " WHERE lid = '%s' ORDER BY pend DESC ", pubds_lid);
	if ((pubds = GetPub(where)) != NULL)
	{
		pubPtr = (Pub *) ListNth(&pubds->list, poslist[0]);
		if (pubPtr)
		{
			memset(where, '\0', sizeof(where));
			sprintf(where, " WHERE lid = '%s' AND pbegin = '%ld' "
			       " AND ppub = '%s' ",
				pubPtr->lid, pubPtr->pbegin, pubPtr->ppub);

			*pos = poslist[0];
			strcpy(key, where);
			XtFree((char *)poslist);
		}
		
		FreePub(pubds);
	}
	
	
	return;	   
}


int	pubds_load(const char *lid)
{
	XmStringTable	xmStr;
	Arg		arg[5];
	Pub		*pub,
			*pubPtr;
	char		where[MAX_WHERE_LEN],
			buf[MAX_BUF_LEN],
			begbuf[DATE_LEN + 1],
			endbuf[DATE_LEN + 1];
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
	clearForm(pubFM);
        state = False;
   
        
        sprintf(where, " WHERE lid = '%s' ORDER BY pbegin DESC ", lid);
        if ((pub = GetPub(where)) != NULL)
        {
                cnt      = ListCount(&pub->list);
                xmStr    = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
                pubPtr   = (Pub *) ListFirst(&pub->list);

                for (i = 0; pubPtr; i++)
                {
                        date_t_to_USA_date ( pubPtr->pbegin, begbuf );
                        date_t_to_USA_date ( pubPtr->pend, endbuf );

			sprintf(buf, "%-11s                              %-11s",
                                begbuf, endbuf);
                        xmStr[i] = XmStringCreateSimple(buf);
                        pubPtr = (Pub *) ListNext(&pubPtr->node);
                }


                XtSetArg(arg[0], XmNitemCount, cnt);
                XtSetArg(arg[1], XmNitems, xmStr);
                XtSetValues(pubLB, arg, 2);
                XmListSelectPos(pubLB, 1, True);
                

        	/*
                	cleanup.
        	*/
        	for (i = 0; i < cnt; i++)
                	XmStringFree(xmStr[i]);  
		XtFree((char *) xmStr);
                FreePub(pub);
                
		Sensitize(pubokPB);
		Sensitize(pubapplyPB);
                Sensitize(pubdelPB);
                state = True;
        }
	else
	{
		XmListDeleteAllItems(pubLB);
		DeSensitize(pubokPB);
		DeSensitize(pubapplyPB);
		DeSensitize(pubdelPB);
	}
	
	
        return(state);	
}


void    pubds_import(Widget w, XtPointer ptr, XmListCallbackStruct *cbs)
{
        Pub            		*pub,
                        	*pubPtr;
        char            	where[MAX_WHERE_LEN],
                        	buf[DATE_LEN + 1];

        /*
                External variable(s).
        */
        extern char     	pubds_lid[LOC_ID_LEN + 1];


        /*
                Retrieve data for the current lid from the
                dbms, and search for the currently selected
                offset in the XmList widget.  Update
                Xm widgets on XmForm work area with
                appropriate values.
        */
        sprintf(where, " WHERE lid = '%s' ORDER BY pbegin DESC ", pubds_lid);
        if ((pub = GetPub(where)) != NULL)
        {
        	pubPtr  = (Pub *) ListNth(&pub->list, cbs->item_position);
        	if (pubPtr)
        	{
                        date_t_to_USA_date ( pubPtr->pbegin, buf );
                	XmTextSetString(pubbegTxt, buf);
 
                        date_t_to_USA_date ( pubPtr->pend, buf );
                	XmTextSetString(pubendTxt, buf);
                	XmTextSetString(publocTxt, pubPtr->ppub);
        	}

        	/*
                	Cleanup.        
        	*/
        	FreePub(pub);
        }
        
        
        return;
}
