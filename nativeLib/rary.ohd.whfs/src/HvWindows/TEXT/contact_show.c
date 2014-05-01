/*
	File:		contact_show.c
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:	Provide support for the Contacts DS.

	Modification History:

	Bryon Lawrence   3/10/2004  Added the E-Mail field.
	
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
#include "Contacts.h"
#include "contact_show.h"
#include "contact.h"
#include "set_window_title.h"


char		contact_lid[LOC_ID_LEN + 1];
int		contact_state;


void	contact_show(Widget w, const char *lid, Boolean editable)
{
	if (! contactDS)
	{	
		create_contactDS(GetTopShell(w));
		contact_callbacks();
	}
	

	if (! XtIsManaged(contactDS))
	{
		strcpy(contact_lid,lid);
		contact_state = contact_load(contact_lid);
		
                set_title(contactDS, "Contacts", contact_lid);
		
		
	   if ( ! editable )
	   {
		XtUnmanageChild(cokPB);
		XtUnmanageChild(capplyPB);
		XtUnmanageChild(cnewPB);
		XtUnmanageChild(cdelPB);
	   }
		
		XtManageChild(contactFM);
		XtManageChild(contactDS);
	}
	
	return;
}


void	contact_callbacks(void)
{
	Atom		atom;
	
	/*
		Window manager callbacks.
	*/
	atom = XmInternAtom(XtDisplay(contactDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(contactDS, atom, contact_close, NULL);
	
	
	/*
		Widget callbacks.
	*/
        XtAddCallback(contactLB, XmNdefaultActionCallback, contact_import, NULL);
        XtAddCallback(contactLB, XmNbrowseSelectionCallback, contact_import, NULL);
	XtAddCallback(cokPB, XmNactivateCallback, contact_ok, NULL);
	XtAddCallback(capplyPB, XmNactivateCallback, contact_apply, NULL);
	XtAddCallback(ccancelPB, XmNactivateCallback, contact_close, NULL);
	XtAddCallback(cnewPB, XmNactivateCallback, contact_new, NULL);
	XtAddCallback(cdelPB, XmNactivateCallback, contact_del_conf, NULL);
	
	
	/*
		Add TextFilter callbacks.
	*/
	contact_add_callbacks();
	
	
	return;
}


void	contact_add_callbacks(void)
{
/*   XtAddCallback(contactTxt,XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)(MIXEDCASE_AND_VALIDSYMBOLS+SPACES)); */
   XtAddCallback(cphoneTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);
   XtAddCallback(csequenceTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   
   return;
}


void	contact_remove_callbacks(void)
{
/*   XtRemoveCallback(contactTxt,XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)(MIXEDCASE_AND_VALIDSYMBOLS+SPACES)); */
   XtRemoveCallback(cphoneTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);
   XtRemoveCallback(csequenceTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   
   return;
}


void	contact_ok(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (contact_save())
      contact_close(w, NULL, NULL);
   return;
}


void	contact_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
   	(void) contact_save();
	return;
}


int	contact_save(void)
{
	Contacts	contacts;
	char		*buf,
			key[BUFSIZ],
			msg[BUFSIZ];
	int		error,
	   		pos = 0,
			rtn;
			
			
	/*
		Associate lid to field entry.
	*/
	memset(&contacts, '\0', sizeof(contacts));
	strcpy(contacts.lid, contact_lid);
	
	
	/*
		Get values from XmText widgets.
	*/
	if ( ( buf = XmTextGetString(csequenceTxt) ) )
	{
                contacts.priority = atoi(buf);
		XtFree(buf);
	}

	if ( ( buf = XmTextGetString(cemailTxt) ) )
        {
                memset ( contacts.email , '\0' , OBS_EMAIL_LEN + 1 ) ;
                strcpy ( contacts.email , buf ) ;
                XtFree ( buf ) ;
        }

	if ( ( buf = XmTextGetString(contactTxt) ) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(contacts.contact, buf);
		XtFree(buf);
	}
		

	if ( ( buf = XmTextGetString(cphoneTxt) ) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(contacts.phone, buf);
		XtFree(buf);
	}


	if ( ( buf = XmTextGetString(concernsTxt) ) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(contacts.remark, buf);
		XtFree(buf);
	}


	/*
                Insert/Update the entered dbms
                record, based on the state of
                the state variable.
        */
	rtn = True;
        if (contact_state)
       	{
		contact_key(key, &pos);
		if (strlen(key))
		{
			if ((error = UpdateContacts(&contacts, key)) != 0)
			{
			 	sprintf(msg, "Unable to update record: %d\n", error);
				ErrorDialog(contactDS, msg);
				rtn = False;
			}
		}
       	}
        else
	{
        	if ((error = PutContacts(&contacts)) != 0)
		{
		 	sprintf(msg, "Unable to insert record: %d\n", error);
			ErrorDialog(contactDS, msg);
			rtn = False;
		}
        }
	
        	
        /*
        	Reset Insert/Update state to True.
		
		Find the newly added/modified entry & select the position.
		Return.
        */
        contact_state = contact_load(contact_lid);
	
	find_contact_pos(&contacts, &pos);
	XmListSelectPos(contactLB, pos, True);
	    
	return(rtn);
}


void	contact_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if(XtIsManaged(contactDS))
   {
      XtDestroyWidget(contactDS);
      contactDS = NULL;
   }

   return;
}


void	contact_new(Widget w, XtPointer ptr, XtPointer cbs)
{
   	clearForm(contactFM);
	Sensitize(cokPB);
	Sensitize(capplyPB);
	contact_state = False;
	return;
}


void	contact_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{        
   Widget	qstDS,
		okPB;
   char		buf[BUFSIZ];
   
   
   sprintf(buf, "Do you wish to delete this entry?");
   qstDS = QuestionDialog(contactDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, contact_delete, NULL);
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	contact_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
   char	key[BUFSIZ];
   int	pos;
   
   
   /*
   	Delete record from dbms,
   	and reload list and select pos.
   */
   SetCursor(contactFM, XC_watch);
   contact_key(key, &pos);
   if (strlen(key))
   {
      DeleteContacts(key);
      (void) contact_load(contact_lid);
      XmListSelectPos(contactLB, pos, True);
   }
   UnsetCursor(contactFM);
   
   return;
}


void	contact_key(char *key, int *pos)
{
	Contacts	*contacts,
			*cPtr;
	char		where[BUFSIZ];
	int		*poslist,
			cnt;
	
	
	XmListGetSelectedPos(contactLB, &poslist, &cnt);
	sprintf(where, " WHERE lid = '%s' ORDER BY priority ASC ", contact_lid);
	if ((contacts = GetContacts(where)) != NULL)
	{
		cPtr = (Contacts *) ListNth(&contacts->list, poslist[0]);
		if (cPtr)
		{
			memset(where, '\0', sizeof(where));
			sprintf(where, " WHERE lid = '%s' AND contact = '%s' ",
				cPtr->lid, cPtr->contact);

			*pos = poslist[0];
			strcpy(key, where);
			XtFree((char *)poslist);
		}
      
		FreeContacts(contacts);
	}
	
	return;
}



int	contact_load(const char *lid)
{
	XmStringTable	xmStr;
	Contacts	*contact = NULL ,
			*ctPtr = NULL ;
	char		where[BUFSIZ],
			buf[BUFSIZ];
	int		state,
			cnt,
			i;

	
	/*
		Initialize state of display.
	*/
	clearForm(contactFM);
 	XmListDeleteAllItems(contactLB);
	state = False;
        
        
        sprintf(where, " WHERE lid = '%s' ORDER BY priority ASC ", lid);
        if ((contact = GetContacts(where)) != NULL)
        {
                cnt     = ListCount(&contact->list);
                xmStr   = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
                ctPtr = (Contacts *) ListFirst(&contact->list);
                for (i = 0; ctPtr; i++)
                {
                        sprintf(buf, "%2ld"
                                     "              %-24s" 
                                     "             %-18s",
                                ctPtr->priority, ctPtr->contact, ctPtr->phone);
                        xmStr[i] = XmStringCreateSimple(buf);
                        ctPtr = (Contacts *) ListNext(&ctPtr->node);
                }


		XmListAddItems(contactLB, xmStr, cnt, 1);
                XmListSelectPos(contactLB, 1, True);
 
        	/*
                	cleanup.
        	*/
        	for (i = 0; i < cnt; i++)
                	XmStringFree(xmStr[i]);
		XtFree((char *) xmStr);
                FreeContacts(contact);
 
                
		Sensitize(cokPB);
		Sensitize(capplyPB);
                Sensitize(cdelPB);
                state = True;
        }
	else
	{
		DeSensitize(cokPB);
		DeSensitize(capplyPB);
	   	DeSensitize(cdelPB);
	}

	
	return(state);	
}


void    contact_import(Widget w, XtPointer ptr, XmListCallbackStruct *cbs)
{
        Contacts            	*contacts,
                        	*cPtr;
        char            	where[BUFSIZ],
                        	buf[DATE_LEN + 1];

        /*
                External variable(s).
        */
        extern char     	contact_lid[LOC_ID_LEN + 1];


	contact_remove_callbacks();
	
	
        /*
                Retrieve data for the current lid from the
                dbms, and search for the currently selected
                offset in the XmList widget.  Update
                Xm widgets on XmForm work area with
                appropriate values.
        */
        sprintf(where, " WHERE lid = '%s' ORDER BY priority ASC ", contact_lid);
        if ((contacts = GetContacts(where)) != NULL)
        {
		cPtr = (Contacts *) ListNth(&contacts->list, cbs->item_position);
        	if (cPtr)
        	{
        	        sprintf(buf, "%2ld", cPtr->priority);
                	XmTextSetString(csequenceTxt,  buf);
                	XmTextSetString(contactTxt,  cPtr->contact);
                	XmTextSetString(cphoneTxt,   cPtr->phone);
                	XmTextSetString(concernsTxt, cPtr->remark);
                        XmTextSetString(cemailTxt, cPtr->email);
        	}


        	/*
                	Cleanup.        
        	*/
        	FreeContacts(contacts);     
        }
        
        
	contact_add_callbacks();
	
	
        return;
}


void	find_contact_pos(Contacts *cPtr, int *pos)
{
   char		buf[BUFSIZ];
   XmString	xmStr;

   
   sprintf(buf, "%2ld"
                "              %-24s"
                "             %-18s",
	   cPtr->priority, cPtr->contact, cPtr->phone);
   xmStr = XmStringCreateSimple(buf);
   *pos = XmListItemPos(contactLB, xmStr);
   
   
   return;
}


