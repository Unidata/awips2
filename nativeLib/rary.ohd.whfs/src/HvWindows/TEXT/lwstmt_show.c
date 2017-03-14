/*
	File:		lwstmt_show.c
	Date:		Jan. 20, 2006
	Author:		Jingtao Deng
	
	Purpose:	Provide support for the Low Statement DS.

	Modification History:

	
	
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
#include <Xm/SelectioB.h>
#include <X11/Composite.h>

#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "lwstmt.h"
#include "lwstmt_show.h"
#include "set_window_title.h"


/*******************************
 Declaration of global variables 
 *******************************/
 
char	   lwstmt_lid[LOC_ID_LEN + 1];
int        lwstmt_state;

/**************************
 The ShowLwstmtDs() function 
 **************************/
 
void	ShowLwstmtDs(Widget w, const char *lid, Boolean editable)
{
	if (! lwstmtDS)
	{	
	   create_lwstmtDS(GetTopShell(w));
	   lwstmt_callbacks();
	}
	
	if (! XtIsManaged(lwstmtDS))
	{
	   strcpy(lwstmt_lid,lid);
	   lwstmt_state = lwstmt_load(lwstmt_lid);                 
		
	   if ( ! editable )
	   {
	      XtUnmanageChild(lwstmtokPB);
	      XtUnmanageChild(lwstmtapplyPB);
	      XtUnmanageChild(lwstmtnewPB);
	      XtUnmanageChild(lwstmtdelPB);
	   }	
	   	
	   XtManageChild(lwstmtFM);
	   XtManageChild(lwstmtDS);
	   set_title(lwstmtDS, "Low Water Statement", lwstmt_lid);	      
	}
	
	return;
}

/******************************
 The lwstmt_callbacks() function 
 ******************************/
 
void	lwstmt_callbacks(void)
{
	/* Declaration of automatic (local) variables */

	Atom		wmAtom;
	
	/* Window manager callbacks. */
		
	wmAtom = XmInternAtom(XtDisplay(lwstmtDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(lwstmtDS, wmAtom, lwstmt_close, NULL);
		
	/* Widget callbacks. */
	
        XtAddCallback(lwstmtLB,      XmNdefaultActionCallback,   lwstmt_import,   NULL);
        XtAddCallback(lwstmtLB,      XmNbrowseSelectionCallback, lwstmt_import,   NULL);
	XtAddCallback(lwstmtokPB,    XmNactivateCallback,        lwstmt_ok,       NULL);
	XtAddCallback(lwstmtapplyPB, XmNactivateCallback,        lwstmt_apply,    NULL);
	XtAddCallback(lwstmtcancelPB,XmNactivateCallback,        lwstmt_close,    NULL);
	XtAddCallback(lwstmtnewPB,   XmNactivateCallback,        lwstmt_new,      NULL);
	XtAddCallback(lwstmtdelPB,   XmNactivateCallback,        lwstmt_del_conf, NULL);
	
	/* add Textfilter callbacks */
	
	lwstmt_add_callbacks();
	
				
	return;
}

void	lwstmt_add_callbacks(void)
{
        XtAddCallback(crtrankTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
	XtAddCallback(lwvalTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(upvalTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
        XtAddCallback(peTxt,      XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter, (XtPointer)UPPERCASE);
	
   return;
}

void	lwstmt_remove_callbacks(void)
{
        XtAddCallback(crtrankTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
	XtAddCallback(lwvalTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(upvalTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(peTxt,      XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter, (XtPointer)UPPERCASE);
  
   return;
}

/***********************
 The lwstmt_ok() function 
 ***********************/
 
void    lwstmt_ok(Widget w, XtPointer ptr, XtPointer cbs)
{
	if (lwstmt_save())
	  lwstmt_close(w, NULL, NULL);
   
   return;
}

/*********************************
 The lwstmt_apply() function
 ********************************/
 
void	lwstmt_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
   	
	(void)lwstmt_save();	
	
	return;
}


int	lwstmt_save()
{
        LWstmt          lwstmt;
	char		*buf,
			key[BUFSIZ],
			msg[BUFSIZ];
	int		error,
	   		pos = 0,
			rtn;	
			
        bool            isUnique;					
			
	/* Associate lid to field entry. */
		
	memset(&lwstmt, '\0', sizeof(lwstmt));	
	strcpy(lwstmt.lid, (char *) lwstmt_lid);
		
	/* Get values from XmText widgets. */
	
	if ( ( buf = XmTextGetString(peTxt) ) )
	{
           strcpy(lwstmt.pe, buf);
	   XtFree((char *) buf);
	}

	if ( ( buf = XmTextGetString(crtTxt) ) )
        {               
           strcpy (lwstmt.lw_criteria , buf ) ;
           XtFree ((char *) buf ) ;
        }

	if ( ( buf = XmTextGetString(srcTxt) ) )
	{
	    strcpy(lwstmt.lw_source, buf);
	    XtFree((char *) buf);
	}
		

	if ( ( buf = XmTextGetString(stmtTxt) ) )
	{		
	    strcpy(lwstmt.statement, buf);
	    XtFree((char *) buf);
	}


	if ( ( buf = XmTextGetString(crtrankTxt) ) )
	{
	    lwstmt.criteria_rank = atoi(buf);
	    XtFree((char *) buf);
	}
	
	if ( ( buf = XmTextGetString(lwvalTxt) ) )
	{
    	    lwstmt.lower_value = atof(buf);	    
	    XtFree((char *) buf);
	}

        if ( ( buf = XmTextGetString(upvalTxt) ) )	
	{            
	    lwstmt.upper_value = atof(buf);	    
	    XtFree((char *) buf);
	}
	
	/* Insert/Update the entered dbms
        record, based on the state of
        the state variable. */
	
	rtn = True;
        if (lwstmt_state)
       	{
	   lwstmt_key(key, &pos);
	   if (strlen(key))
	   {
	      if ((error = UpdateLWstmt(&lwstmt, key)) != 0)
	      {
		 sprintf(msg, "Unable to update record: %d\n", error);
		 ErrorDialog(lwstmtDS, msg);
		 rtn = False;
	      }
	   }
       	}
        else
	{
	   error = InsertIfUniqueLWstmt(&lwstmt, &isUnique);
	   
	   if ( !isUnique )
	   {
	      printf("\nIgnoring duplicate Record... ");
	   }
	   else if (error != 0)
	   {
	      sprintf(msg, "Unable to insert record: %d\n", error);
	      ErrorDialog(lwstmtDS, msg);
	      rtn = False;  
	       
	   }	  	             
        }
	        	
        /* Reset Insert/Update state to True. Find the newly added/modified entry & 
	select the position. Return. */
	
        lwstmt_state = lwstmt_load(lwstmt_lid);
	
	find_lwstmt_pos(&lwstmt, &pos);
	XmListSelectPos(lwstmtLB, pos, True);
	    
	return(rtn);
}

/**************************
 The lwstmt_close() function 
 **************************/
 
void	lwstmt_close(Widget w, XtPointer ptr, XtPointer cbs)
{	

	if(XtIsManaged(lwstmtDS))
	{
	   XtDestroyWidget(lwstmtDS);
	   lwstmtDS = NULL;
	}

	return;
}

/************************
 The lwstmt_new() function 
 ************************/
 
void	lwstmt_new(Widget w, XtPointer ptr, XtPointer cbs)
{      
				
	/* clear the form. */
	
   	clearForm(lwstmtFM);
	Sensitize(lwstmtokPB);
	Sensitize(lwstmtapplyPB);
        lwstmt_state = False;
	
	return;
}

/*****************************
 The lwstmt_del_conf() function
 *****************************/
 
void	lwstmt_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{        
	Widget	qstDS,
		okPB;
	char	buf[MAX_BUF_LEN];


	sprintf(buf, "Do you wish to delete this entry?");
	qstDS = QuestionDialog(lwstmtDS, buf);
	SetTitle(qstDS, "Delete Confirmation");

	/* Get the XmMessageBox ok button,
   	     and associate a callback to it. */

	okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
	XtAddCallback(okPB, XmNactivateCallback, lwstmt_delete, NULL);

	if(! XtIsManaged(qstDS))
	   XtManageChild(qstDS);

	return;
}

/***************************
 The lwstmt_delete() function
 ***************************/

void	lwstmt_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
	char	key[BUFSIZ];
	int	pos;	

	/***************************************************
	 Store the position of the currently selected record
	 ***************************************************/

	 SetCursor(lwstmtFM, XC_watch);

	 lwstmt_key(key, &pos);

	 if (strlen(key))
	 {
	    DeleteLWstmt(key);
	    lwstmt_load(lwstmt_lid);
	    XmListSelectPos(lwstmtLB, pos, True);
	 }

	 UnsetCursor(lwstmtFM);

	 	 	
	return;
}

/**********************************************************
  lwstmt_key()
***********************************************************/
  
void	lwstmt_key(char *key, int *pos)
{
	LWstmt  	*lwstmt,
			*lwPtr;
	char		where[BUFSIZ];
	int		*poslist,
			cnt;	
	
	XmListGetSelectedPos(lwstmtLB, &poslist, &cnt);
	sprintf(where, " WHERE lid = '%s' ORDER BY criteria_rank, lower_value ASC ", lwstmt_lid);
	
	if ((lwstmt = GetLWstmt(where)) != NULL)
	{
	   lwPtr = (LWstmt *) ListNth(&lwstmt->list, poslist[0]);
	   if (lwPtr)
	   {
	      memset(where, '\0', sizeof(where));
	      sprintf(where, " WHERE lid = '%s' AND pe = '%s' "
	                     "AND lower_value >= %f AND lower_value <= %f AND "
			     "criteria_rank = '%ld'" ,
		      lwPtr->lid, lwPtr->pe, lwPtr->lower_value - 0.001,
		      lwPtr->lower_value + 0.001,
		      lwPtr->criteria_rank);

	      *pos = poslist[0];
	      strcpy(key, where);
	      XtFree((char *)poslist);
	   }
           
	   if (lwstmt != NULL)
	     FreeLWstmt(lwstmt);
	}
	
	return;
}

/***************************************************
  lwstmt_load()
  Load lid|Low Value|Upper Value|Criteria Rank|Criteria
  into the list
****************************************************/  

int	lwstmt_load(const char *lid)
{
	XmStringTable	xmStr;
	LWstmt  	*lwstmt = NULL ,
			*lwPtr = NULL ;
	char		where[BUFSIZ],
			buf[BUFSIZ];
	int		state,
			cnt,
			i;
	
	/* Initialize state of display. */
	
	clearForm(lwstmtFM);
 	XmListDeleteAllItems(lwstmtLB);
	state = False;        
        
        sprintf(where, " WHERE lid = '%s' ORDER BY criteria_rank, lower_value ASC ", lid);
        if ((lwstmt = GetLWstmt(where)) != NULL)
        {
           cnt     = ListCount(&lwstmt->list);
           xmStr   = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
           lwPtr = (LWstmt *) ListFirst(&lwstmt->list);
           
	   for (i = 0; lwPtr; i++)
           {             
              sprintf(buf, " %9.2f    %9.2f    %2s                 %4ld",
                     lwPtr->lower_value, lwPtr->upper_value,
		     lwPtr->pe, lwPtr->criteria_rank);			     
              xmStr[i] = XmStringCreateSimple(buf);
              lwPtr = (LWstmt *) ListNext(&lwPtr->node);
           }

	   XmListAddItems(lwstmtLB, xmStr, cnt, 1);
           XmListSelectPos(lwstmtLB, 1, True);

           /* cleanup. */
	   
           for (i = 0; i < cnt; i++)
                   XmStringFree(xmStr[i]);
	   
	   XtFree((char *) xmStr);
	   
	   if (lwstmt != NULL)
             FreeLWstmt(lwstmt);


	   Sensitize(lwstmtokPB);
	   Sensitize(lwstmtapplyPB);
           Sensitize(lwstmtdelPB);
           state = True;
        }
	else
	{
	   DeSensitize(lwstmtokPB);
	   DeSensitize(lwstmtapplyPB);
	   DeSensitize(lwstmtdelPB);
	}

	
	return(state);	
}

/*************************************************************
 lwstmt_import()
 Fuction for lwstmtLB callback. Read data from LWstmt table

 **************************************************************/

void    lwstmt_import(Widget w, XtPointer ptr, XmListCallbackStruct *cbs)
{
        LWstmt           	*lwstmt,
                        	*lwPtr;
        char            	where[BUFSIZ],
                        	buf[20]="";

        /* External variable(s). */
        
	extern char     	lwstmt_lid[LOC_ID_LEN + 1];


	lwstmt_remove_callbacks();
	
	
        /* Retrieve data for the current lid from the
          dbms, and search for the currently selected
          offset in the XmList widget.  Update
          Xm widgets on XmForm work area with
          appropriate values. */
	  
        sprintf(where, " WHERE lid = '%s' ORDER BY criteria_rank, lower_value ASC ", lwstmt_lid);
       
        if ((lwstmt = GetLWstmt(where)) != NULL)
        {
	   lwPtr = (LWstmt *) ListNth(&lwstmt->list, cbs->item_position);
           if (lwPtr)
           {
	      XmTextSetString(peTxt,       lwPtr->pe);
              XmTextSetString(crtTxt,      lwPtr->lw_criteria);
              XmTextSetString(srcTxt,      lwPtr->lw_source);
	      XmTextSetString(stmtTxt,     lwPtr->statement);
              sprintf(buf, "%ld",          lwPtr->criteria_rank);
              XmTextSetString(crtrankTxt,  buf);
	      sprintf(buf, "%9.2f",           lwPtr->lower_value);
              XmTextSetString(lwvalTxt,    buf);
	      sprintf(buf, "%9.2f",           lwPtr->upper_value);
              XmTextSetString(upvalTxt,    buf);	                                 
           }


           /* Free memory */
	   
	   if (lwstmt != NULL)
             FreeLWstmt(lwstmt);     
        }        	
	
        return;
}


void	find_lwstmt_pos(LWstmt *lwPtr, int *pos)
{
	char		buf[BUFSIZ];
	XmString	xmStr;	
		      
	sprintf(buf, " %9.2f    %9.2f    %2s                 %4ld",
                     lwPtr->lower_value, lwPtr->upper_value,
		     lwPtr->pe, lwPtr->criteria_rank);		     
	xmStr = XmStringCreateSimple(buf);
	*pos = XmListItemPos(lwstmtLB, xmStr);

	return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}


