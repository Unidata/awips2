/*
	File:		mgage_show.c
	Date:		March 1995
	Author:		Dale Shelton
	
	Purpose:	Provide support for the Gage History DS.	
*/


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>
/***** POSTGRES
#include <sqlhdr.h>
*****/
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "GageMaint.h"
#include "GageOwner.h"
#include "GageType.h"
#include "Gage.h"
#include "Xtools.h"
#include "mgage_cbs.h"
#include "mgage.h"
#include "hybase.h"
#include "time_convert.h"
#include "user_prefs.h"

int		mgage_state;
char		mgage_lid[LOC_ID_LEN + 1];

Widget		mgownerPB;
Widget		mgmaintPB;
Widget		mgtypePB;

void	ShowMgageDs(Widget w, char *lid)
{
	if (! mgageDS)
	{
		create_mgageDS(GetTopShell(w));
		mgage_btns();
		mgage_callbacks();
	}
	
	if (! XtIsManaged(mgageDS))
	{
		strcpy(mgage_lid, lid);
		set_window_title(mgageDS, "Gage History", mgage_lid);
		XtManageChild(mgageFM);
		XtManageChild(mgageDS);
		mgage_state = mgage_load(mgage_lid);
	}
	
	return;
}


void	mgage_callbacks(void)
{
	Atom	wmAtom;

	
	/*
		Window manager callbacks.
	*/
	wmAtom = XmInternAtom(XtDisplay(mgageDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(mgageDS, wmAtom, close_mgage, NULL);
	
	
	/*
		General widget callbacks.
	*/
	XtAddCallback(glistLB,   XmNdefaultActionCallback, mgage_import, NULL);
	XtAddCallback(glistLB,   XmNbrowseSelectionCallback, mgage_import, NULL);
	XtAddCallback(mgokPB,    XmNactivateCallback, mgage_ok, NULL);
	XtAddCallback(mgapplyPB, XmNactivateCallback, mgage_apply, NULL);
	XtAddCallback(mgnewPB,   XmNactivateCallback, mgage_new, NULL);
	XtAddCallback(mgdelPB,   XmNactivateCallback, mgage_del_conf, NULL);
	XtAddCallback(mgclosePB, XmNactivateCallback, close_mgage,  NULL);	

	
	/*
		TextFilter callbacks.
	*/
	XtAddCallback(mgbeginTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
	XtAddCallback(mgendTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
	
	
	return;
}


void	mgage_btns(void)
{
	GageOwner	*go, *goPtr;
	GageMaint	*gm, *gmPtr;
	GageType	*gt, *gtPtr;
        XmStringTable   xmStr;
        int             cnt = 0,
                        i = 0;
	
	
	go    = GetGageOwner("");
        if (go)
	{ 
		goPtr = (GageOwner *) ListFirst(&go->list);
	        cnt = ListCount(&goPtr->list);
	}
	else
	{
		goPtr = NULL;
		cnt = 0; 
	}
        xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
	while (goPtr)
	{
                xmStr[i] = XmStringCreateSimple(goPtr->owner);
                i++;
		goPtr = (GageOwner *) ListNext(&goPtr->node);
	}
        XmListAddItems(ownerLI, xmStr, cnt, 1);
        for (i = 0; i < cnt; i++)
          XmStringFree (xmStr[i]);
        XtFree((char *) xmStr);
	


	gm    = GetGageMaint("");
	if (gm)
	{
		gmPtr = (GageMaint *) ListFirst(&gm->list);
                cnt = ListCount(&gmPtr->list);
	}
	else
	{
		gmPtr = NULL;
		cnt = 0;
	}
        i = 0;
        xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
	while (gmPtr)
	{
                xmStr[i] = XmStringCreateSimple(gmPtr->maint);
                i++;
		gmPtr = (GageMaint *) ListNext(&gmPtr->node);
	}
        XmListAddItems(maintLI, xmStr, cnt, 1);
        for (i = 0; i < cnt; i++)
          XmStringFree (xmStr[i]);
        XtFree((char *) xmStr);
	


	gt    = GetGageType("");
 	if (gt)
	{
		gtPtr = (GageType *) ListFirst(&gt->list);
       		cnt = ListCount(&gtPtr->list);
	}
	else
	{
		gtPtr = NULL;
		cnt = 0;
	}
        i = 0;
        xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
	while (gtPtr)
	{
                xmStr[i] = XmStringCreateSimple(gtPtr->type);
                i++;
		gtPtr = (GageType *) ListNext(&gtPtr->node);
	}
        XmListAddItems(typeLI, xmStr, cnt, 1);
        for (i = 0; i < cnt; i++)
          XmStringFree (xmStr[i]);
        XtFree((char *) xmStr);

	
	if (go) FreeGageOwner(go);
	if (gm) FreeGageMaint(gm);
	if (gt) FreeGageType(gt);
	return;
}

void	mgage_ok(Widget w, XtPointer ptr, XtPointer cbs)
{
	if (mgage_save())
		close_mgage(w, NULL);
	return;
}


void	mgage_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
	(void) mgage_save();
	return;
}


int	mgage_save(void)
{
	Gage		gage;
	char		msg[BUFSIZ],
			key[BUFSIZ],
			*buf,
                        *SLdata = NULL;
	long		date;
	int		error,
	   		pos,
	   		rtn,
                        cnt = 0,
                        *position = NULL,
                        status;
        XmString        *strlist;
	
	
	/*
		Associate the appropriate lid.
	*/
	memset(&gage, '\0', sizeof(gage));
	strcpy(gage.lid, mgage_lid);
	
	
        /*     
                Get values from the scrolled lists (typeLI, ownerLI, maintLI)
        */

        XmListGetSelectedPos(typeLI, &position, &cnt);
        if (cnt)
        { 
          XtVaGetValues(typeLI, XmNitems, &strlist, NULL);
          XmStringGetLtoR(strlist[position[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
          strcpy(gage.type, SLdata);
        }
         
        XmListGetSelectedPos(ownerLI, &position, &cnt);
        if (cnt)
        {
          XtVaGetValues(ownerLI, XmNitems, &strlist, NULL);
          XmStringGetLtoR(strlist[position[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
          strcpy(gage.owner, SLdata);
        }
 
        XmListGetSelectedPos(maintLI, &position, &cnt);
        if (cnt)
        {
          XtVaGetValues(maintLI, XmNitems, &strlist, NULL);
          XmStringGetLtoR(strlist[position[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
          strcpy(gage.maint, SLdata);
        }

	/*
		Ensure a gage begin date has been
		entered, this is a required key field.
	*/
	if ( (buf = XmTextGetString(mgbeginTxt)) )
	{
		if (four_digit_year(buf) != 0)
		{
		  sprintf(msg, "Invalid Begin date '%s' entered, 4 digit year required.\n", 
			  buf);
		  ErrorDialog(mgageDS, msg);
		  /* leave function, do not save data */
		  return(False);
		}
		else
		{
                  status = USA_date_to_date_t ( buf, & date );

		  if ( status != 0 )
		  {
		    sprintf(msg, "Invalid Begin date '%s' entered, check month\n"
                                 "and day.\n", buf);
		    ErrorDialog(mgageDS, msg);
		    /* leave function, do not save data */
		    return(False);
		  }
		  else
		    gage.gbegin = date;
		}
			
		XtFree(buf);
	}
	

	if ( (buf = XmTextGetString(mglocTxt)) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(gage.remark, buf);
		XtFree(buf);
	}
		
	
	if ( (buf = XmTextGetString(mgendTxt)) )
	{
		if ( strnlen(buf) == 0)
      {
         SetNull(LONG, (void *) &gage.gend);
      }
      else if (four_digit_year(buf) != 0)
		{
		  sprintf(msg, "Invalid End date '%s' entered, 4 digit year required.\n", 
			  buf);
		  ErrorDialog(mgageDS, msg);
		  /* leave function, do not save data */
		  return(False);
		}
		else
		{
        status = USA_date_to_date_t ( buf, & date );

		  if ( status != 0 )
		  {
		    sprintf(msg, "Invalid End date '%s' entered, check month\n"
                                 "and day.\n", buf);
		    ErrorDialog(mgageDS, msg);
		    /* leave function, do not save data */
		    return(False);
		  }
		  else
		    gage.gend = date;
		}
			
		XtFree(buf);
	}
	
	
	/*
                Insert/Update the entered dbms
                record, based on the state of
                the state variable.
        */
	rtn = True;
        if (mgage_state)
       	{
		mgage_key(key, &pos);
		if (strlen(key))
		{
			if ((error = UpdateGage(&gage, key)) != 0)
			{
			 	sprintf(msg, "Unable to update record: %d\n", error);
				ErrorDialog(mgageDS, msg);
				rtn = False;
			}
		}
       	}
        else
	{
        	if ((error = PutGage(&gage)) != 0)
		{
		 	sprintf(msg, "Unable to insert record: %d\n", error);
			ErrorDialog(mgageDS, msg);
			rtn = False;
		}
        }
	
        	
        /*
        	Reset Insert/Update state to 
        	True, and return.
        */
        mgage_state = mgage_load(mgage_lid);
  	return(rtn);
}


void	mgage_new(Widget w, XtPointer ptr, XtPointer cbs)
{
	/*
		Set the widget children to NULL.
	*/
	clearForm(mgageFM);
	
	
	/*
		Set Insert/Update state and return.
	*/
	Sensitize(mgokPB);
	Sensitize(mgapplyPB);
	mgage_state = False;
	return;
}


void	mgage_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete this entry?"); 
   qstDS = QuestionDialog(mgageDS, buf);
   SetTitle(qstDS,"Delete Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, delete_mgage, NULL);
   
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	delete_mgage(Widget w, XtPointer ptr, XtPointer cbs)
{
	char	key[MAX_WHERE_LEN];
	int	pos;


	/*
		Delete record from dbms,
		and reload list and select pos.
	*/
	SetCursor(mgageFM, XC_watch);
	mgage_key(key, &pos);
	if (strlen(key))
	{
		DeleteGage(key);
		(void) mgage_load(mgage_lid);
		XmListSelectPos(glistLB, pos, True);
	}
	UnsetCursor(mgageFM);
	
	return;
}

void	close_mgage(Widget w, XtPointer ptr, XtPointer cbs)
{
   if(XtIsManaged(mgageDS))
   {
      XtDestroyWidget(mgageDS);
      mgageDS = NULL;
   }
   
   return;
}


void	mgage_key(char *key, int *pos)
{
   Gage		*gage,
      		*gPtr;
   
   char		where[BUFSIZ];
   
   int		*poslist,
      		cnt;
   
   
   /*
   	Get the currently selected item, 
   	and build the appropriate where clause.
   */
   XmListGetSelectedPos(glistLB, &poslist, &cnt);
   create_gage_where(where, (const char*) mgage_lid);
   
   /*
   	Retrieve the gage information.
   */
   if ((gage = GetGage(where)) != NULL)
   {
      gPtr = (Gage *) ListNth(&gage->list, poslist[0]);
      memset(where, '\0', sizeof(where));
      sprintf(where, " WHERE lid = '%s' "
	      " AND type = '%s' AND gbegin = '%ld' ",
	      gPtr->lid, gPtr->type, gPtr->gbegin);
      
      *pos = poslist[0];
      strcpy(key, where);
      
      
      /*
      		Free any allocated memory.
      */
      XtFree((char *)poslist);
      FreeGage(gage);
   }
   
   return;
}


int     mgage_load(const char *lid)
{
        XmStringTable   xmStr;
        Gage            *gage,
        		*gagePtr;
        char            where[MAX_WHERE_LEN],
                        buf[BUFSIZ],
                        begin[DATE_LEN + 1],
                        end[DATE_LEN + 1];
        int             state,
                        cnt,
                        i;


	/*
		Initialize window state.
	*/
	XmListDeleteAllItems(glistLB);
 	clearForm(mgageFM);
	state = False;

	
	/*
		Retrieve the values and load the list.
	*/	
	create_gage_where(where, (const char*) lid);
	
        if ((gage = GetGage(where)) != NULL)
        {
                cnt     = ListCount(&gage->list);
                xmStr   = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
                gagePtr = (Gage *) ListFirst(&gage->list);

                for (i = 0; gagePtr; i++)
                {
                        date_t_to_USA_date ( gagePtr->gbegin, begin );
                        date_t_to_USA_date ( gagePtr->gend, end );
                        sprintf(buf, "%-15s  %-15s  %-10s    %-10s",
                                gagePtr->type, gagePtr->owner,
                                begin, end);
                        xmStr[i] = XmStringCreateSimple(buf);
                        gagePtr = (Gage *) ListNext(&gagePtr->node);
                }

      
		/*
			Load the list.
		*/
		XmListAddItems(glistLB, xmStr, cnt, 1);
                XmListSelectPos(glistLB, 1, True);

		
		/*
			Set internal state variables.
		*/
		Sensitize(mgokPB);
		Sensitize(mgapplyPB);
                Sensitize(mgdelPB);
                state = True;

		
		/*
			Free any allocated memory.
		*/
                for (i = 0; i < cnt; i++)
                        XmStringFree(xmStr[i]);
                XtFree((char *) xmStr);
       		FreeGage(gage);
        }

        else
        {
 		DeSensitize(mgokPB);
		DeSensitize(mgapplyPB);
	   	DeSensitize(mgdelPB);
		clearForm(mgageFM);
        }

         return(state);
}


void    mgage_import(Widget w, XtPointer ptr, XmListCallbackStruct *cbs)
{
   Gage		*gage,
      		*gPtr;
   
   char		where[BUFSIZ],
      		buf[DATE_LEN + 1];
   XmString     item;
   int          pos = 0;
   
   /*
   	External variable(s).
   */
   extern char     mgage_lid[LOC_ID_LEN + 1];
   
   
   
   clearForm(mgageFM);
   
   
   /*
   	Retrieve data for the current lid from the
   	dbms, and search for the currently selected
   	offset in the XmList widget.  Update
   	Xm widgets on XmForm work area with
   	appropriate values.
   */
   create_gage_where(where, (const char*) mgage_lid);
   
   if ((gage = GetGage(where)) != NULL)
   {
      gPtr = (Gage *) ListNth(&gage->list, cbs->item_position);
      if (gPtr)
      {
         item = XmStringCreateSimple(gPtr->type);
         pos = XmListItemPos(typeLI, item);
         XmListSetPos(typeLI, pos);
         XmListSelectPos(typeLI, pos, True);
         XmStringFree(item);

         item = XmStringCreateSimple(gPtr->owner);
         pos = XmListItemPos(ownerLI, item);
         XmListSetPos(ownerLI, pos);
         XmListSelectPos(ownerLI, pos, True);
         XmStringFree(item);

         item = XmStringCreateSimple(gPtr->maint);
         pos = XmListItemPos(maintLI, item);
         XmListSetPos(maintLI, pos);
         XmListSelectPos(maintLI, pos, True);
         XmStringFree(item);

         date_t_to_USA_date ( gPtr->gbegin, buf );
	 XmTextSetString(mgbeginTxt, buf);
	 
	 if (IsNull(INT, &gPtr->gend) == NOTNULL)
	 {
            date_t_to_USA_date ( gPtr->gend, buf );
	    XmTextSetString(mgendTxt, buf);
	 }
	 
	 
	 XmTextSetString(mglocTxt, gPtr->remark);
	 Sensitize(mgdelPB);
      }
      
      
      FreeGage(gage);
   }
   
   return;
}	



void	create_gage_where(char *where, const char *lid)
{
  	sprintf(where, " WHERE lid = '%s' ORDER BY gend DESC, gbegin DESC ",
     		lid);

	return;
}



