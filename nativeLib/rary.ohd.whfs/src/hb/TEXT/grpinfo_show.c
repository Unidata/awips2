/**************************************************************************
   File:		grpinfo_show.c
   Purpose:	Provide support for the RiverPro Forecast Groups/Points DS.
   ************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <X11/cursorfont.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>

#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"

#include "RpfFcstGroup.h"
#include "RpfFcstPoint.h"
#include "Location.h"
#include "Hsa.h"

#include "grpinfo_cbs.h"
#include "grpinfo.h"
#include "hybase.h"

#include "set_timevals.h"
#include "process_vtecinfo.h"


int grpinfo_state;
int grpinfo_add_new_fcstgroup = False;

/********************************************************************
   Open and display the window.
   ******************************************************************/

void grpinfo_show(Widget w)
{
   
   if (! grpDS)
   {	
      create_grpDS(GetTopShell(w));
      grpinfo_callbacks();
   }
   
   
   if (! XtIsManaged(grpDS))
   {
      grpinfo_state = grpinfo_load();
      
      XtManageChild(grpFM);
      XtManageChild(grpDS);
   }
   
   return;
}


/********************************************************************
   Add the callbacks.
   ******************************************************************/

void grpinfo_callbacks(void)
{
   Atom		atom;
   Hsa      *hsa, *hsaPtr;
   int      cnt = 0, i = 0;
   XmStringTable xmStr;
   
   /* window manager callbacks */
   
   atom = XmInternAtom(XtDisplay(grpDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(grpDS, atom, grpinfo_close, NULL);
   
   
   /* widget callbacks */

   XtAddCallback(grp_rectype_peTB, XmNvalueChangedCallback, rec_petypeCB, NULL);
   XtAddCallback(grp_rectype_npeTB, XmNvalueChangedCallback, rec_npetypeCB, NULL);
  
   
   XtAddCallback(grpfgLB,    XmNdefaultActionCallback,  grpinfo_importfg,NULL);
   XtAddCallback(grpfgLB,    XmNbrowseSelectionCallback,grpinfo_importfg,NULL);
 
   XtAddCallback(grpfpLB,    XmNdefaultActionCallback,  grpinfo_importfp,NULL);
   XtAddCallback(grpfpLB,    XmNbrowseSelectionCallback,grpinfo_importfp,NULL);
   
   XtAddCallback(grpapplyfgPB, XmNactivateCallback,     grpinfo_applyfg, NULL);
   XtAddCallback(grpapplyfpPB, XmNactivateCallback,     grpinfo_applyfp, NULL);
   
   XtAddCallback(grpclosePB, XmNactivateCallback,       grpinfo_close,   NULL);
   XtAddCallback(grpaddPB,   XmNactivateCallback,       grpinfo_add,     NULL);
   XtAddCallback(grpdelPB,   XmNactivateCallback,       grpinfo_del_conf,NULL);
 
   XtAddCallback(grpnumTxt,  XmNmodifyVerifyCallback, 
		 (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtAddCallback(grpnum2Txt, XmNmodifyVerifyCallback, 
		 (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtAddCallback(grpidTxt,   XmNmodifyVerifyCallback, 
		 (XtCallbackProc)alphanum_filter, (XtPointer)MIXEDCASE);
   XtAddCallback(grp_chgwindowTX, XmNmodifyVerifyCallback, 
		 (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
		 
   XtAddCallback(grp_backhrsTX, XmNmodifyVerifyCallback, 
		 (XtCallbackProc)num_filter, (XtPointer)INTEGERS);  
		 
   XtAddCallback(grp_fwdhrsTX, XmNmodifyVerifyCallback, 
		 (XtCallbackProc)num_filter, (XtPointer)INTEGERS);  		  
   
   XtAddCallback(grp_ajtendhrsTX, XmNmodifyVerifyCallback, 
		 (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
		 

   
   hsa = GetHsa(" ORDER BY hsa ");
   hsaPtr = (Hsa *) ListFirst(&hsa->list);
   cnt = ListCount(&hsaPtr->list);
   xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
   while (hsaPtr)
   {
           xmStr[i] = XmStringCreateSimple(hsaPtr->hsa);
           i++;
           hsaPtr = (Hsa *) ListNext(&hsaPtr->node);
   }
   XmListAddItems(pri_hsaLI, xmStr, cnt, 1);
   XmListAddItems(sec_hsaLI, xmStr, cnt, 1);
   if (hsa)  FreeHsa(hsa);
   
   return;
}


/********************************************************************
   Perform the actions of the Apply Forecast Group button.
   ******************************************************************/

void grpinfo_applyfg(Widget	w, 
		     XtPointer	ptr, 
		     XtPointer	cbs)
{
   RpfFcstGroup	*grpPtr  = NULL;
   RpfFcstGroup	*grpinfo = grpinfo_getFcstGroupInfo(NULL, False);
   RpfFcstGroup	grpRec;
   
   char		*buf, *buf2, *buf3;
   char		rec_all_str[BOOL_LEN + 1];
   char		msg[80];   
   int		*poslist, cnt, error = 0;
   
   
   /* get the info for the selected position from the list */
      
   XmListGetSelectedPos(grpfgLB, &poslist, &cnt);
   if (cnt > 0)
   {
      grpPtr  = (RpfFcstGroup *) ListNth(&grpinfo->list, poslist[0]);
   }
   
   
   /* get the fields from the window */
   
   if ( (buf = XmTextGetString(grpidTxt)) ) 
   {
      if ( (buf2 = XmTextGetString(grpnameTxt)) ) 
      {
	 if ( (buf3 = XmTextGetString(grpnumTxt)) ) 
	 {
	    if ((IsNull(CHAR, buf)  == NOTNULL) &&
		(IsNull(CHAR, buf2) == NOTNULL) &&
		(IsNull(CHAR, buf3) == NOTNULL))
	    {	       
               if (XmToggleButtonGetState(grp_recallTB))
		  strcpy(rec_all_str, "Y");
	       else
		  strcpy(rec_all_str, "N");
	       
	       /* if adding an group */
	       
	       if (grpinfo_add_new_fcstgroup)
	       {
		  strcpy(grpRec.group_id, buf);
		  strcpy(grpRec.group_name, buf2);
		  grpRec.ordinal = atol(buf3);
		  strcpy(grpRec.rec_all_included, rec_all_str);
		  
		  error = PutRpfFcstGroup(&grpRec);
		  if (error < 0)
		  {
		     sprintf(msg, "ERROR: Could not add/update Forecast Group\n");
		     ErrorDialog(grpDS, msg);
		  }
		  else
		     grpinfo_show_new_group(&grpRec);  /* selects new group */
	       }
	       
	       /* if updating a group */
	       
	       else
	       {
		  grpinfo_updateFcstGroupInfo(grpPtr, buf, buf2, atol(buf3), 
					      rec_all_str);
		  grpRec = *grpPtr;
		  grpinfo_show_new_group(&grpRec);  /* selects new group */
	       }
	       
	       
	       if (cnt > 0)
		  XtFree((char *)poslist);
	       
	       XtFree(buf);
	       XtFree(buf2);
	       XtFree(buf3);
	       
	       return;
	    }
	    XtFree(buf3);
	 }
	 XtFree(buf2);
      }
      XtFree(buf);
   }
   
   
   if (cnt > 0)
      XtFree((char *)poslist);
   
   sprintf(msg, "ERROR: Could not add/update Forecast Group\n");
   ErrorDialog(grpDS, msg);
   
   
   return;
}


/********************************************************************
   Perform the actions of the Apply Forecast Point button.
   ******************************************************************/

void grpinfo_applyfp(Widget	w, 
		     XtPointer	ptr, 
		     XtPointer	cbs)
{
   RpfFcstPoint	*ptPtr  = NULL;
   RpfFcstPoint	*ptinfo = grpinfo_getFcstPointInfo(NULL, False);
   RpfFcstPoint	ptRec;
   
   char		*buf1, *buf2, msg[80];   
   int		*poslist, cnt, *poslist2, cnt2;
   char		rectype_str[RECTYPE_LEN + 1];
   char		primary[HYD_SERV_LEN + 1];
   char		secondary[HYD_SERV_LEN + 1];
   char         *SLdata = NULL;
   XmString     *strlist;
   char         *buf3, *buf4, *buf5;
   
   
   /* determine the selected forecast point */
   
   XmListGetSelectedPos(grpfpLB, &poslist, &cnt);
   ptPtr  = (RpfFcstPoint *) ListNth(&ptinfo->list, poslist[0]);
        
   
   /* update the forecast point info; note that one cannot create a new
      forecast point entry */
   
   if ( (buf1 = XmTextGetString(grpnum2Txt)) && 
        (buf2 = XmTextGetString(grp_chgwindowTX)) &&
	(buf3 = XmTextGetString(grp_backhrsTX)) &&
	(buf4 = XmTextGetString(grp_fwdhrsTX)) &&	
	(buf5 = XmTextGetString(grp_ajtendhrsTX)))
   {
      if ((IsNull(CHAR, buf1)  == NOTNULL) && 
	  (IsNull(CHAR, buf2)  == NOTNULL) &&
	  (IsNull(CHAR, buf3)  == NOTNULL) &&
	  (IsNull(CHAR, buf4)  == NOTNULL) &&
	  (IsNull(CHAR, buf5)  == NOTNULL)) 
      {
	if (XmToggleButtonGetState(grp_rectype_npeTB))
	strcpy(rectype_str, "NPE");
	else
	   strcpy(rectype_str, "PE");
	 
        XmListGetSelectedPos(pri_hsaLI, &poslist2, &cnt2);
        if (cnt2)
        {
          XtVaGetValues(pri_hsaLI, XmNitems, &strlist, NULL);
          XmStringGetLtoR(strlist[poslist2[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
          strcpy(primary, SLdata);
        }
	 
        XmListGetSelectedPos(sec_hsaLI, &poslist2, &cnt2);
        if (cnt2)
        {
          XtVaGetValues(sec_hsaLI, XmNitems, &strlist, NULL);
          XmStringGetLtoR(strlist[poslist2[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
          strcpy(secondary, SLdata);
        }
	 
	 grpinfo_updateFcstPointInfo(ptPtr, atol(buf1), rectype_str, atof(buf2),
                                primary, secondary, atoi(buf3), atoi(buf4), atof(buf5));
	 XtFree(buf1);
	 XtFree(buf2);
	 XtFree(buf3);
	 XtFree(buf4);	
	 XtFree(buf5);
	 
	 ptRec = *ptPtr;
	 grpinfo_show_new_point(&ptRec);  
	 
	 if (cnt > 0)
	    XtFree((char *)poslist);	    
	 
	 return;
      }
      
      XtFree(buf1);
      XtFree(buf2);
      XtFree(buf3); 
      XtFree(buf4);     
      XtFree(buf5);
   }
   
   if (cnt > 0)
      XtFree((char *)poslist);
   
   sprintf(msg, "Unable to update Forecast Point (missing fields)\n");
   ErrorDialog(grpDS, msg);
   
   return;
}


/********************************************************************
   Clear the slate and await the input for the new entry.
   ******************************************************************/

void grpinfo_add(Widget		w, 
		 XtPointer	ptr, 
		 XtPointer	cbs)
{
   clearForm(grpFM);
   
   XmListDeselectAllItems(grpfgLB);
   XmListDeselectAllItems(grpfpLB);
   
   DeSensitize(grp2aLbl);
   DeSensitize(grp2bLbl);
   DeSensitize(grpfpLB);
   DeSensitize(grpnum2Lbl);
   DeSensitize(grpnum2Txt);
   DeSensitize(grp_rectypeLB);   
   DeSensitize(grp_rectype_peTB);
   DeSensitize(grp_rectype_npeTB);   
   DeSensitize(grp_chgwindowLB);
   DeSensitize(grp_chgwindowTX);
   DeSensitize(grp_backhrsLB);
   DeSensitize(grp_backhrsTX); 
   DeSensitize(grp_fwdhrsLB);
   DeSensitize(grp_fwdhrsTX);   
   DeSensitize(grp_ajtendhrsLB);
   DeSensitize(grp_ajtendhrsTX);
   DeSensitize(grpapplyfpPB);
   DeSensitize(grpdelPB);
   
   Sensitize(grpapplyfgPB);
   grpinfo_state = False;
   
   grpinfo_add_new_fcstgroup = True;
   
   return;
}


/********************************************************************
   Confirm the delete.
   ******************************************************************/

void grpinfo_del_conf(Widget	w, 
		      XtPointer ptr, 
		      XtPointer cbs)
{        
   Widget	qstDS, okPB;
   char		buf[MAX_BUF_LEN];
   
   /* verify with the user */
      
   sprintf(buf, "Do you want to delete this Forecast Group?\n"
	   "(This will remove all Forecast Point associations to this Group.)");
   qstDS = QuestionDialog(grpDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   
   /*Get the XmMessageBox ok button, and associate a callback to it. */
   
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, grpinfo_delete, NULL);
   
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


/********************************************************************
   
   ******************************************************************/

void grpinfo_delete(Widget w, 
		    XtPointer ptr, 
		    XtPointer cbs)
{
   char	key[MAX_WHERE_LEN];
   int	pos;
   
   
   /* Delete record from dbms, and reload list and select pos. */
   
   SetCursor(grpFM, XC_watch);
   grpinfo_key(key, &pos);
   if (strlen(key))
   {
      /* key = "WHERE group_id = XXXXX" */
      
      DeleteRpfFcstGroup(key); 
      DeleteRpfFcstPoint(key);   /* delete associated records, if any */
      
      XtUnmapWidget(grpfgLB);
      grpinfo_state = grpinfo_load();
      XmListSelectPos(grpfgLB, pos, True);
      XmListSetPos(grpfgLB, pos);
      XtMapWidget(grpfgLB);
   }
   UnsetCursor(grpFM);
   
   return;
}


/********************************************************************
   Clears out the forecast group text info.
   ******************************************************************/

void grpinfo_clearfg(void)
{
   XmTextSetString(grpidTxt,   "");
   XmTextSetString(grpnameTxt, "");
   XmTextSetString(grpnumTxt,  "");
   
   XmListDeleteAllItems(grpfpLB);   
   
   return;
}


/********************************************************************
   Clears out the forecast point text info.
   ******************************************************************/

void grpinfo_clearfp(void)
{
   XmTextSetString(grpnum2Txt, "");
   XmTextSetString(grp_chgwindowTX, "");
   XmTextSetString(grp_backhrsTX, ""); 
   XmTextSetString(grp_fwdhrsTX, "");
   XmTextSetString(grp_ajtendhrsTX, "");
   
   return;
}


/********************************************************************
   
   ******************************************************************/

void grpinfo_key(char *key, 
		 int *pos)
{
   RpfFcstGroup	*grpinfo;
   RpfFcstGroup *grpPtr;
   char		where[MAX_WHERE_LEN];
   int		*poslist, cnt;
   
   
   XmListGetSelectedPos(grpfgLB, &poslist, &cnt);
   if ((grpinfo = GetRpfFcstGroup(" ORDER BY ordinal, group_id ")) != NULL)
   {
      grpPtr = (RpfFcstGroup *) ListNth(&grpinfo->list, poslist[0]);
      if (grpPtr)
      {
	 memset(where, '\0', sizeof(where));
	 sprintf(where, " WHERE group_id = '%s' ",grpPtr->group_id);
	 
	 *pos = poslist[0];
	 strcpy(key, where);
      }
      FreeRpfFcstGroup(grpinfo);
   }
   
   if (cnt > 0)
      XtFree((char *)poslist);
   
   return;
}


/********************************************************************
   Load the forecast group info.
   ******************************************************************/

int grpinfo_load(void)
{
   XmStringTable	xmStr;
   RpfFcstGroup	*grpinfo, *grpPtr;
   char		buf[MAX_BUF_LEN];
   int		state, cnt, i;
   
   
   /* Set state variable to False, pending retrieval of a record
      from the database.  If a record is retrieved, state variable will
      be set to true. */
   
   clearForm(grpFM);
   XmListDeleteAllItems(grpfgLB);
   state = False;
   
   
   if ((grpinfo = GetRpfFcstGroup(" ORDER BY ordinal, group_id ")) != NULL)
   {
      cnt     = ListCount(&grpinfo->list);
      xmStr   = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
      grpPtr  = (RpfFcstGroup *) ListFirst(&grpinfo->list);
      
      (void) grpinfo_getFcstGroupInfo(grpPtr, False);  /* set static */
      
      for (i=0;  grpPtr;  i++)
      {
	 sprintf(buf, "%-8s    %-32s                %-ld                %-1s",
		 grpPtr->group_id, grpPtr->group_name, 
		 grpPtr->ordinal, grpPtr->rec_all_included);
	 
	 xmStr[i] = XmStringCreateSimple(buf);
	 grpPtr = (RpfFcstGroup *) ListNext(&grpPtr->node);
      }
      
      
      XmListAddItems(grpfgLB, xmStr, cnt, 1);
      XmListSelectPos(grpfgLB, 1, True);
      
      
      /* cleanup */
      
      for (i=0; i<cnt; i++)
      {
	 XmStringFree(xmStr[i]);
      }
      XtFree((char *) xmStr);
      
      
      Sensitize(grpapplyfgPB);
      Sensitize(grpdelPB);
      state = True;
   }
   
   else
   {
      DeSensitize(grpapplyfgPB);
      DeSensitize(grpdelPB);
   }
   
   
   return(state);	
}


/********************************************************************
   Get the info for the forecast group; namely the info for the
   forecast points in the group.
   ******************************************************************/

void grpinfo_importfg(Widget w, 
		      XtPointer ptr, 
		      XtPointer cbs)
{
   RpfFcstGroup		*grpPtr = NULL;
   RpfFcstGroup		*grpinfo = grpinfo_getFcstGroupInfo(NULL, False);   
   RpfFcstPoint		*ptinfo, *ptPtr;  
   Location		*locPtr;   
   char            	where[MAX_WHERE_LEN], buf[MAX_BUF_LEN];   
   XmStringTable	xmStr;
   int			cnt, i;
   int			*poslist;
  
   time_t	        obshrs, fcsthrs, basishrs;
   static int	        first = TRUE;
   char                 token_string[60];
   int                  token_len, string_len;
   static int	        shift_hours = DEFAULT_ENDTIME_SHIFT_HOURS;
   
   
   
   
   grpinfo_add_new_fcstgroup = False;
   
   grpinfo_clearfg();
   
   
   XmListGetSelectedPos(grpfgLB, &poslist, &cnt);
   grpPtr  = (RpfFcstGroup *) ListNth(&grpinfo->list, poslist[0]);
   if (grpPtr)
   {
      /* load the group fields */
      
      XmTextSetString(grpidTxt,   grpPtr->group_id);
      XmTextSetString(grpnameTxt, grpPtr->group_name);
      
      sprintf(buf, "%-ld", grpPtr->ordinal);
      XmTextSetString(grpnumTxt, buf);
      
      if (strcmp(grpPtr->rec_all_included, "Y") == 0)
	 XmToggleButtonSetState(grp_recallTB, 1, 0);
      else
	 XmToggleButtonSetState(grp_recallTB, 0, 0);
      
      
      /* now load the forecast point info for the group */
      
      sprintf(where, " WHERE group_id='%-s' ORDER BY ordinal, lid ",
	      grpPtr->group_id);
      
      if ((ptinfo = GetRpfFcstPoint(where)) != NULL)
      {
	 cnt     = ListCount(&ptinfo->list);
	 xmStr   = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
	 ptPtr   = (RpfFcstPoint *) ListFirst(&ptinfo->list);
	 
	 (void) grpinfo_getFcstPointInfo(ptPtr, False);  /* set static */
	 
	 /* get the default values for backhrs, forwardhrs */
	 
	 get_hrvals(&obshrs, &fcsthrs, &basishrs);  
	 
	 /* get the adjustendhrs from token rpf_endtime_shifthrs */
	 
	 if (first) 
	 {
	   token_len = strlen("rpf_endtime_shifthrs");
	   get_apps_defaults("rpf_endtime_shifthrs", &token_len, token_string, 
                	     &string_len);
	   if (string_len > 0)
	   {
	     shift_hours = atoi(token_string);
	     if (shift_hours < 0 || shift_hours > 48)
	     {
        	 shift_hours = DEFAULT_ENDTIME_SHIFT_HOURS;
		
	     }
	   }

	   first = FALSE;     
	 }  
	 
	 for (i=0;  ptPtr;  i++)
	 {
	    sprintf(where, " WHERE lid='%-s' ", ptPtr->lid);
	    locPtr = GetLocation(where);
	    
	    /* check if backhrs, forwardhrs,adjustendhrs are NULL, if
	    they are use the default values */
	    
	    if (IsNull(INT, &ptPtr->forwardhrs) != NOTNULL)                          
	       ptPtr->forwardhrs = fcsthrs;
      
	    if (IsNull(INT, &ptPtr->backhrs) != NOTNULL)               
	       ptPtr->backhrs = obshrs;
	    
	    if (IsNull(DOUBLE, &ptPtr->adjustendhrs) != NOTNULL)              
	       ptPtr->adjustendhrs = shift_hours;
   	  
	    sprintf(buf, 
		    "%-8s %-30.30s   %3ld   %-3s   %.2f  %-3s   %-3s   %6ld  %6ld  %6.1f",
		    ptPtr->lid, locPtr->name, ptPtr->ordinal,
		    ptPtr->rec_type, ptPtr->chg_threshold,
		    ptPtr->primary_back, ptPtr->secondary_back,
		    ptPtr->backhrs, ptPtr->forwardhrs, ptPtr->adjustendhrs);
	    
	    xmStr[i] = XmStringCreateSimple(buf);
	    ptPtr = (RpfFcstPoint *) ListNext(&ptPtr->node);
	    
	    
	    FreeLocation(locPtr);
	 }
	 
	 /* add all Forecast Points to the list and select the first one */
	 
         XmListAddItems(grpfpLB, xmStr, cnt, 1);
	 XmListSelectPos(grpfpLB, 1, True);

	 Sensitize(grp2aLbl);
	 Sensitize(grp2bLbl);
	 Sensitize(grpfpLB);
	 Sensitize(grpnum2Lbl);
	 Sensitize(grpnum2Txt);
	 
	 Sensitize(grp_rectypeLB);
	 
	 Sensitize(grp_rectype_peTB);
	 Sensitize(grp_rectype_npeTB);
	 
	 Sensitize(grp_chgwindowLB);
	 Sensitize(grp_chgwindowTX);
	 
	 Sensitize(grp_backhrsLB);
	 Sensitize(grp_backhrsTX);
	 
	 Sensitize(grp_fwdhrsLB);
	 Sensitize(grp_fwdhrsTX);
	
	 Sensitize(grp_ajtendhrsLB);
	 Sensitize(grp_ajtendhrsTX);
	 
	 Sensitize(grpapplyfpPB);
	 
	 /* cleanup */
	 
	 for (i=0;  i < cnt;  i++)
	 {
	    XmStringFree(xmStr[i]);
	 }
	 XtFree((char *) xmStr);
      }
      
      else
      {
	 grpinfo_clearfp();
	 
	 DeSensitize(grp2aLbl);
	 DeSensitize(grp2bLbl);
	 DeSensitize(grpfpLB);
	 DeSensitize(grpnum2Lbl);
	 DeSensitize(grpnum2Txt);
	 
	 DeSensitize(grp_rectypeLB);
	 
	 DeSensitize(grp_rectype_peTB);
	 DeSensitize(grp_rectype_npeTB);
	 
	 DeSensitize(grp_chgwindowLB);
	 DeSensitize(grp_chgwindowTX);
	 
	 DeSensitize(grp_backhrsLB);
	 DeSensitize(grp_backhrsTX);
	 
	 DeSensitize(grp_fwdhrsLB);
	 DeSensitize(grp_fwdhrsTX);	 
	 
	 DeSensitize(grp_ajtendhrsLB);
	 DeSensitize(grp_ajtendhrsTX);
	 	 
	 DeSensitize(grpapplyfpPB);
      }
   }
   
   if (cnt > 0)
      XtFree((char *)poslist);
   
   return;
}


/********************************************************************
   Load the forecast point info fields, not the scrolled list.
   ******************************************************************/

void grpinfo_importfp(Widget w, 
		      XtPointer ptr, 
		      XtPointer cbs)
{
   RpfFcstPoint	*ptinfo = grpinfo_getFcstPointInfo(NULL, False);
   RpfFcstPoint	*ptPtr  = NULL;
   
   char		buf[MAX_BUF_LEN];
   
   int		cnt, *poslist;
   XmString select_item;
   int      select_pos = 0;
 
   
   /* clear out the displayed info */
   
   grpinfo_clearfp();
   
   
   /* now load the forecast point info */
   
   XmListGetSelectedPos(grpfpLB, &poslist, &cnt);
   ptPtr = (RpfFcstPoint *) ListNth(&ptinfo->list, poslist[0]);
   if (ptPtr)
   {
      sprintf(buf, "%-ld", ptPtr->ordinal);
      XmTextSetString(grpnum2Txt, buf);
      
      if (strcmp(ptPtr->rec_type, "NPE") == 0)
      {
	 XmToggleButtonSetState(grp_rectype_npeTB, True, False);         
         XmToggleButtonSetState(grp_rectype_peTB, False, False);   
  
      }
      else 
      {
	 XmToggleButtonSetState(grp_rectype_peTB, True, False);         
         XmToggleButtonSetState(grp_rectype_npeTB, False, False); 

      }
      
      sprintf(buf, "%-.2f", ptPtr->chg_threshold);
      XmTextSetString(grp_chgwindowTX, buf);    
      
      sprintf(buf, "%-ld", ptPtr->backhrs);
      XmTextSetString(grp_backhrsTX, buf);  
      
      sprintf(buf, "%-ld", ptPtr->forwardhrs);
      XmTextSetString(grp_fwdhrsTX, buf);      
          
      sprintf(buf, "%-.1f", ptPtr->adjustendhrs);
      XmTextSetString(grp_ajtendhrsTX, buf);      
       
     
      /* set the values in the scrolled lists */
      
      select_item = XmStringCreateSimple(ptPtr->primary_back);
      select_pos = XmListItemPos(pri_hsaLI, select_item);
      XmListSetPos(pri_hsaLI, select_pos);
      XmListSelectPos(pri_hsaLI, select_pos, True);
      select_item = XmStringCreateSimple(ptPtr->secondary_back);
      select_pos = XmListItemPos(sec_hsaLI, select_item);
      XmListSetPos(sec_hsaLI, select_pos);
      XmListSelectPos(sec_hsaLI, select_pos, True);
	 
   } 
   
   if (cnt > 0)
      XtFree((char *)poslist);
   
   return;
}


/********************************************************************
   
   ******************************************************************/

void grpinfo_show_new_group(RpfFcstGroup *newgrpPtr)
{
   RpfFcstGroup*	fginfo = grpinfo_getFcstGroupInfo(NULL, False);   
   char			buf[MAX_BUF_LEN];
   XmString		xmStr;
   
   
   if (fginfo)
   {
      FreeRpfFcstGroup(fginfo);
      fginfo = NULL;
   }
   (void) grpinfo_getFcstGroupInfo(NULL, True);  /* set static to NULL */
   
   
   /* ensure we have the newest XmList.
      (Update the grpinfo_state flag too). */
   
   XtUnmapWidget(grpfgLB);
   grpinfo_state = grpinfo_load();  /* (deletes items from XmList first) */
   sprintf(buf, "%-8s          %-32s                 %-ld",
	   newgrpPtr->group_id, newgrpPtr->group_name, newgrpPtr->ordinal);
   
   xmStr = XmStringCreateSimple(buf);
   XmListSelectItem(grpfgLB, xmStr, True);
   XmStringFree(xmStr);
   XtMapWidget(grpfgLB);
   
   
   return;
}


/********************************************************************
   
   ******************************************************************/

void grpinfo_show_new_point(RpfFcstPoint *newptPtr)
{
   RpfFcstGroup	*grpPtr = NULL;
   RpfFcstGroup	*grpinfo = grpinfo_getFcstGroupInfo(NULL, False);
   
   RpfFcstPoint	*ptinfo = NULL,  /* for reloading XmList */
   *ptPtr  = NULL,  /* for looping / reloading of XmList */
   *fpinfo = grpinfo_getFcstPointInfo(NULL, False);  /* for free */
   
   Location	*locPtr = NULL;
   
   char			buf[MAX_BUF_LEN];
   XmStringTable	xmStr;
   
   char		where[MAX_WHERE_LEN];
   int		cnt, i;
   int		*poslist;
   int		pos = 0;  /* for finding new point */
   
   
   XmListDeleteAllItems(grpfpLB);
   if (fpinfo)
   {
      FreeRpfFcstPoint(fpinfo);
      fpinfo = NULL;
   }
   (void) grpinfo_getFcstPointInfo(NULL, True);  /* set static to NULL */
   
   
   
   XmListGetSelectedPos(grpfgLB, &poslist, &cnt);
   grpPtr  = (RpfFcstGroup *) ListNth(&grpinfo->list, poslist[0]);
   if (grpPtr)
   {
      sprintf(where, " WHERE group_id='%-s' ORDER BY ordinal, lid ",
	      grpPtr->group_id);
      
      if ((ptinfo = GetRpfFcstPoint(where)) != NULL)
      {
	 cnt     = ListCount(&ptinfo->list);
	 xmStr   = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
	 ptPtr   = (RpfFcstPoint *) ListFirst(&ptinfo->list);
	 
	 (void) grpinfo_getFcstPointInfo(ptPtr, False);  /* set static */
	 
	 for (i=0;  ptPtr;  i++)
	 {
	    sprintf(where, " WHERE lid='%-s' ", ptPtr->lid);
	    locPtr = GetLocation(where);
	    
	    /* this same output is generated elsewhere in this file;
	       bad design */
	       	   	    
            sprintf(buf, 
		   "%-8s %-30.30s   %3ld   %-3s   %.2f  %-3s   %-3s   %6ld  %6ld  %6.1f",
		   ptPtr->lid, locPtr->name, ptPtr->ordinal,
		   ptPtr->rec_type, ptPtr->chg_threshold,
		   ptPtr->primary_back, ptPtr->secondary_back,
		   ptPtr->backhrs, ptPtr->forwardhrs, ptPtr->adjustendhrs);		    
		    	    
	    if (strcmp(ptPtr->lid, newptPtr->lid) == 0)
	    {
	       pos = i+1;
	    }
	    
	    xmStr[i] = XmStringCreateSimple(buf);
	    ptPtr = (RpfFcstPoint *) ListNext(&ptPtr->node);
	    
	    
	    FreeLocation(locPtr);
	 }
	 
	 XmListAddItems(grpfpLB, xmStr, cnt, 1);
	 
	 Sensitize(grp2aLbl);
	 Sensitize(grp2bLbl);
	 Sensitize(grpfpLB);
	 Sensitize(grpnum2Lbl);
	 Sensitize(grpnum2Txt);
	 Sensitize(grp_rectypeLB);	 
	 Sensitize(grp_rectype_peTB);
	 Sensitize(grp_rectype_npeTB);	 
	 Sensitize(grp_chgwindowLB);
	 Sensitize(grp_chgwindowTX);
	 Sensitize(grp_backhrsLB);
	 Sensitize(grp_backhrsTX);
	 Sensitize(grp_fwdhrsLB);
	 Sensitize(grp_fwdhrsTX);
	
	 Sensitize(grp_ajtendhrsLB);
	 Sensitize(grp_ajtendhrsTX);
	 Sensitize(grpapplyfpPB);
	 
	 /* cleanup */
	 
	 for (i=0;  i < cnt;  i++)
	 {
	    XmStringFree(xmStr[i]);
	 }
	 XtFree((char *) xmStr);
      }
      
      else
      {
	 grpinfo_clearfp();
	 
	 DeSensitize(grp2aLbl);
	 DeSensitize(grp2bLbl);
	 DeSensitize(grpfpLB);
	 DeSensitize(grpnum2Lbl);
	 DeSensitize(grpnum2Txt);
	 DeSensitize(grp_rectypeLB);	 
	 DeSensitize(grp_rectype_peTB);
	 DeSensitize(grp_rectype_npeTB);	 
	 DeSensitize(grp_chgwindowLB);
	 DeSensitize(grp_chgwindowTX);	
	 DeSensitize(grp_backhrsLB);
	 DeSensitize(grp_backhrsTX);
	 DeSensitize(grp_fwdhrsLB);
	 DeSensitize(grp_fwdhrsTX);
	 	
	 DeSensitize(grp_ajtendhrsLB);
	 DeSensitize(grp_ajtendhrsTX);
	 DeSensitize(grpapplyfpPB);
      }
   }
   
   
   XmListSelectPos(grpfpLB, pos, True);
   XmListSetPos(grpfpLB, pos);
   
   
   return;
}


/********************************************************************
   
   ******************************************************************/

RpfFcstGroup* grpinfo_getFcstGroupInfo(RpfFcstGroup 	*fginfo,
				       Boolean 		init)
{
   static RpfFcstGroup*		infoPtr = NULL;
   
   
   if (init)
   {
      infoPtr = NULL;
   }
   
   else if (fginfo != (RpfFcstGroup *) NULL)  /* pass argument to set infoPtr */
   {
      if (infoPtr != (RpfFcstGroup *) NULL)
      {
	 FreeRpfFcstGroup(infoPtr);
	 infoPtr = NULL;
      }
      
      infoPtr = fginfo;
   }
   
   
   return(infoPtr);
}


/********************************************************************
   
   ******************************************************************/

RpfFcstPoint* grpinfo_getFcstPointInfo(RpfFcstPoint	*fpinfo, 
				       Boolean 		init)
{
   static RpfFcstPoint*		infoPtr = NULL;
   
   
   if (init)
   {
      infoPtr = NULL;
   }
   
   else if (fpinfo != (RpfFcstPoint *) NULL)  /* pass argument to set infoPtr */
   {
      if (infoPtr != (RpfFcstPoint *) NULL)
      {
	 FreeRpfFcstPoint(infoPtr);
	 infoPtr = NULL;
      }
      
      infoPtr = fpinfo;
   }
   
   
   return(infoPtr);
}


/********************************************************************
   
   ******************************************************************/

void grpinfo_updateFcstGroupInfo(RpfFcstGroup	*fginfo,
				 char 		*id, 
				 char 		*name, 
				 long 		ordinal,
				 char		*rec_all_str)
{
   char		where[MAX_WHERE_LEN];
   char		msg[80];
   int		error;
   
   
   sprintf(where, " WHERE group_id='%-s' ", fginfo->group_id);
   
   strcpy(fginfo->group_id,   id);
   strcpy(fginfo->group_name, name);
   fginfo->ordinal = ordinal;
   strcpy(fginfo->rec_all_included, rec_all_str);
   
   
   if ((error = UpdateRpfFcstGroup(fginfo, where)) != 0)
   {
      sprintf(msg, "Unable to update Forecast Group: %d\n", error);
      ErrorDialog(grpDS, msg);
   }
   
   
   return;
}


/********************************************************************
   
   ******************************************************************/

void grpinfo_updateFcstPointInfo(RpfFcstPoint	*fpinfo, 
				 long 	        ordinal,
				 char		*rectype_str,
				 double	        chg_window,
                                 char		*primary,
                                 char		*secondary,
				 int            backhrs,
				 int            fwdhrs,				 
				 double         ajtendhrs)
{
   char		where[MAX_WHERE_LEN];
   char		msg[80];
   int		error;
   
   
   /* build the where clause, load the fields, and do the update */
   
   sprintf(where, " WHERE lid = '%-s' ", fpinfo->lid);
   
   
   fpinfo->ordinal = ordinal;
   strcpy(fpinfo->rec_type, rectype_str);
   fpinfo->chg_threshold = chg_window;
   strcpy(fpinfo->primary_back, primary);
   strcpy(fpinfo->secondary_back, secondary);
   fpinfo->backhrs = backhrs;  
   fpinfo->forwardhrs = fwdhrs;
   fpinfo->adjustendhrs = ajtendhrs;
   
   
   if ((error = UpdateRpfFcstPoint(fpinfo, where)) != 0)
   {
      sprintf(msg, "Unable to update Forecast Point: %d\n", error);
      ErrorDialog(grpDS, msg);
   }
   
   
   return;
}

/********************************************************************
   Close the window.
   ******************************************************************/

void grpinfo_close(Widget w, 
		   XtPointer ptr, 
		   XtPointer cbs)
{
   RpfFcstGroup* fginfo = grpinfo_getFcstGroupInfo(NULL, False);
   RpfFcstPoint* fpinfo = grpinfo_getFcstPointInfo(NULL, False);
   
   if (fginfo)
   {
      FreeRpfFcstGroup(fginfo);
      fginfo = NULL;
   }
   (void) grpinfo_getFcstGroupInfo(NULL, True);  /* set static to NULL */
   
   if (fpinfo)
   {
      FreeRpfFcstPoint(fpinfo);
      fpinfo = NULL;
   }
   (void) grpinfo_getFcstPointInfo(NULL, True);  /* set static to NULL */
   
   
   if(XtIsManaged(grpDS))
   {
      XtDestroyWidget(grpDS);
      grpDS = NULL;
   }
   
   return;
}

/*******************************************
rec_petypeCB
********************************************/
void rec_petypeCB(Widget w, XtPointer xtptr1, XtPointer xtptr2)
{
   load_rec_petype(w); 
   return;
}

/*******************************************
rec_npetypeCB
********************************************/
void rec_npetypeCB(Widget w, XtPointer xtptr1, XtPointer xtptr2)
{
   load_rec_npetype(w); 
   return;
}
/*********************************************
load_rec_petype()
**********************************************/
void load_rec_petype(Widget w)
{         
    XmToggleButtonSetState(grp_rectype_peTB, True, False);           
    XmToggleButtonSetState(grp_rectype_npeTB, False, False);   
         
   return;
}

/*********************************************
load_rec_npetype()
**********************************************/
void load_rec_npetype(Widget w)
{      
   XmToggleButtonSetState(grp_rectype_npeTB, True, False);
   XmToggleButtonSetState(grp_rectype_peTB, False, False);
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}   
