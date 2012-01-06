/*
	File:		stcozo_show.c
	Date:		3/24/97
	Author:		Mark Glaudemans
	
	Purpose:	Provide support for the "States/Countiesy/Zones" DS.
*/


/************************************************************************
   
   Functions to handle the setup control of state, county, and
   zone information.
      
   
   ***********************************************************************/

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/List.h>
#include <Xm/Text.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "State.h"
#include "Counties.h"
#include "Eligzon.h"

#include "DbmsUtils.h"
#include "Xtools.h"

#include "stcozo.h"
#include "stcozo_show.h"


/* variables global to this file */

State		*stateHead;
Counties	*countiesHead;
Eligzon		*eligzonHead;



/************************************************************************
   
   Load the state,county.zone dialog shell.
   
   ***********************************************************************/

void	ShowStCoZoDS(Widget w)
{
   if (! stcozoDS)
   {      
      /* create the dialog shell and add the callbacks */
      
      create_stcozoDS(GetTopShell(w));
      add_stcozo_cbs();      
   }
   
   
   /* manage the windows now before doing the set_selections() */
   
   if (! XtIsManaged(stcozoDS))
   {
      XtManageChild(stcozoFM);
      XtManageChild(stcozoDS);
   }
   
   
   /* initialize data memory */
   
   stateHead    = (State *)(NULL);
   countiesHead = (Counties *)(NULL);
   eligzonHead  = (Eligzon *)(NULL);
      
      
   /* load each of the scrolled lists */
   
   load_statelist();
   load_countylist();
   load_zonelist();
   
   XtUnmanageChild(stcozo_helpPB);

   return;
}


/************************************************************************
    
   Add the stations selection callbacks.
   
   ************************************************************************/

void add_stcozo_cbs(void)
{
   
   Atom	wmAtom;
   
   
   /* callbacks on scrolled list of products */
   
   XtAddCallback(stateLS,  XmNdefaultActionCallback,   load_statetextCB,  NULL);
   XtAddCallback(stateLS,  XmNsingleSelectionCallback, load_statetextCB,  NULL);

   XtAddCallback(countyLS, XmNdefaultActionCallback,   load_countytextCB, NULL);
   XtAddCallback(countyLS, XmNsingleSelectionCallback, load_countytextCB, NULL);
   
   XtAddCallback(zoneLS,   XmNdefaultActionCallback,   load_zonetextCB,   NULL);
   XtAddCallback(zoneLS,   XmNsingleSelectionCallback, load_zonetextCB,   NULL);
   
   
   /* callbacks on state text fields */
   
   XtAddCallback(state_stateTX,  XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter, (XtPointer)UPPERCASE);
   XtAddCallback(county_stateTX, XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter, (XtPointer)UPPERCASE);
   XtAddCallback(zone_stateTX,   XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter, (XtPointer)UPPERCASE);
   
   
   /* callbacks on add, update, delete pushbuttons */
   
   XtAddCallback(state_addPB,    XmNactivateCallback, state_addCB, NULL);
   XtAddCallback(state_updatePB, XmNactivateCallback, state_updateCB, NULL);
   XtAddCallback(state_deletePB, XmNactivateCallback, state_deleteCB, NULL);

   XtAddCallback(county_addPB,    XmNactivateCallback, county_addCB, NULL);
   XtAddCallback(county_updatePB, XmNactivateCallback, county_updateCB, NULL);
   XtAddCallback(county_deletePB, XmNactivateCallback, county_deleteCB, NULL);

   XtAddCallback(zone_addPB,    XmNactivateCallback, zone_addCB, NULL);
   XtAddCallback(zone_updatePB, XmNactivateCallback, zone_updateCB, NULL);
   XtAddCallback(zone_deletePB, XmNactivateCallback, zone_deleteCB, NULL);
   
   
   /* callbacks on atom widget */   
   
   wmAtom = XmInternAtom(XtDisplay(stcozoDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(stcozoDS, wmAtom, ok_stcozoCB, NULL);
   
   XtAddCallback(stcozo_okPB, XmNactivateCallback, ok_stcozoCB, NULL);
   
   
   /* callbacks for TextFilter */   
   
   XtAddCallback(county_fipsTX, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)MIXEDCASE);
   XtAddCallback(zone_numTX,    XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtAddCallback(county_wfoTX,       XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter, (XtPointer)MIXEDCASE);
   XtAddCallback(county_primaryTX,   XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter, (XtPointer)MIXEDCASE);
   XtAddCallback(county_secondaryTX, XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter, (XtPointer)MIXEDCASE);

   
   return;
}


/************************************************************************
   
   Load the scrolled lists for state info.
   
   ***********************************************************************/

void load_statelist(void)
{
   XmStringTable        xmStr;
   Arg                  arg[10];
   int                  ac, i;
   int			cnt;
   char			liststr[80];
   State 		*statePtr;
   char 		where[60];
   
   
   /* free any memory allocated */
   
   if (stateHead != NULL)
   {
      FreeState(stateHead);
      stateHead = (State *)NULL;
   }
   
   
   /* load the list of states from the database */
   
   XmListDeleteAllItems(stateLS);
   sprintf(where, " where state != 'XX' ORDER BY state ");   
   stateHead = GetState(where);
   
   cnt = 0;
   if (stateHead != NULL)
   {
      cnt = ListCount(&stateHead->list);
   }
   
   if (cnt == 0)
   {
      return;
   }
   
   
   /* allocate the Motif list strings */
   
   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
   
   
   /* load the strings into the Motif strings */
   
   statePtr = (State *) ListFirst(&stateHead->list);
   for (i = 0; i < cnt; i++)
   {
      sprintf(liststr, "%-2s     %s", statePtr->state, statePtr->name);
      xmStr[i] = XmStringCreateSimple(liststr);
      
      statePtr = (State *) ListNext(&statePtr->node);
   }
   
   
   /* load the list in the scrolled list */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(stateLS, arg, ac);
   DeSensitize(state_updatePB);
   DeSensitize(state_deletePB);
   
   
   /* free the memory */
   
   for (i = 0; i < cnt; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);
   
     
   
   return;
}


/************************************************************************
   
   Load the scrolled lists for county info.
   
   ***********************************************************************/

void load_countylist(void)
{
   XmStringTable        xmStr;
   Arg                  arg[10];
   int                  ac, i;
   int			cnt;
   char			liststr[80];
   Counties 		*countiesPtr;
   char 		where[60];
   
   
   /* free any memory allocated */
   
   if (countiesHead != NULL)
   {
      FreeCounties(countiesHead);
      countiesHead = (Counties *)NULL;
   }
   
   
   /* load the list of counties from the database */
   
   XmListDeleteAllItems(countyLS);
   sprintf(where, " where state != 'XX' ORDER BY state, county ");   
   countiesHead = GetCounties(where);
   
   cnt = 0;
   if (countiesHead != NULL)
   {
      cnt = ListCount(&countiesHead->list);
   }
   
   if (cnt == 0)
   {
      return;
   }
   
   
   /* allocate the Motif list strings */
   
   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
   
   
   /* load the strings into the Motif strings */
   
   countiesPtr = (Counties *) ListFirst(&countiesHead->list);
   for (i = 0; i < cnt; i++)
   {
      sprintf(liststr, "%-2s    %-20s  %4s   %3s     %3s      %3s",
		countiesPtr->state, countiesPtr->county, countiesPtr->countynum,
		countiesPtr->wfo, countiesPtr->primary_back, countiesPtr->secondary_back);
      xmStr[i] = XmStringCreateSimple(liststr);
      
      countiesPtr = (Counties *) ListNext(&countiesPtr->node);
   }
   
   
   /* load the list in the scrolled list */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(countyLS, arg, ac);
   DeSensitize(county_updatePB);
   DeSensitize(county_deletePB);

   
   /* free the memory */
   
   for (i = 0; i < cnt; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);
   
        
   return;
}


/************************************************************************
   
   Load the scrolled lists for zone info.
   
   ***********************************************************************/

void load_zonelist(void)
{
   XmStringTable        xmStr;
   Arg                  arg[10];
   int                  ac, i;
   int			cnt;
   char			liststr[80];
   Eligzon 		*eligzonPtr;
   char 		where[60];
   
   
   /* free any memory allocated */
   
   if (eligzonHead != NULL)
   {
      FreeEligzon(eligzonHead);
      eligzonHead = (Eligzon *)NULL;
   }
   
   
   /* load the list of eligzon from the database */
   
   XmListDeleteAllItems(zoneLS);
   sprintf(where, " where state != 'XX' ORDER BY state, zonenum ");   
   eligzonHead = GetEligzon(where);

   cnt = 0;
   if (eligzonHead != NULL)
   {
      cnt = ListCount(&eligzonHead->list);
   }
   
   if (cnt == 0)
   {
      return;
   }
   
   
   /* allocate the Motif list strings */
   
   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
   
   
   /* load the strings into the Motif strings */
   
   eligzonPtr = (Eligzon *) ListFirst(&eligzonHead->list);
   for (i = 0; i < cnt; i++)
   {
      sprintf(liststr, "%-2s  %3s  %-20s", eligzonPtr->state,
	      eligzonPtr->zonenum, eligzonPtr->descr);
      xmStr[i] = XmStringCreateSimple(liststr);
      
      eligzonPtr = (Eligzon *) ListNext(&eligzonPtr->node);
   }
   
   
   /* load the list in the scrolled list */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(zoneLS, arg, ac);
   DeSensitize(zone_updatePB);
   DeSensitize(zone_deletePB);


   
   /* free the memory */
   
   for (i = 0; i < cnt; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);
   
   
   return;
}


/************************************************************************
   
   Callback for loading (or attempting) to load text of selected
   item into text fields.
   
   **********************************************************************/

void load_statetextCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   State	*statePtr;
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   
   
   /* determine which item is selected */
   
   XmListGetSelectedPos(stateLS, &listitems, &listcnt);
   
   
   /* get the product text and display it */
   
   if (listcnt > 0)
   {
      itemnum = listitems[0];
      statePtr = (State *) ListNth(&stateHead->list, itemnum);
      if (statePtr != NULL)
      {      
	 XmTextSetString(state_stateTX, statePtr->state);
	 XmTextSetString(state_nameTX,  statePtr->name);
      }
      
      else
	 fprintf(stderr, "Error getting item from State list.\n");
      
      free(listitems);
   }   
   else
   {
      XmTextSetString(state_stateTX, "");
      XmTextSetString(state_nameTX,  "");
   }

   if (ListRsrcGetSelectedCount(stateLS))
   {
      Sensitize(state_updatePB);
      Sensitize(state_deletePB);
   }
   else
   {
      DeSensitize(state_updatePB);
      DeSensitize(state_deletePB);
   }

   
   return;
}


/************************************************************************
   
   Callback for loading (or attempting) to load text of selected
   item into text fields.
   
   **********************************************************************/

void load_countytextCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   Counties	*countiesPtr;
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   
   
   /* determine which item is selected */
   
   XmListGetSelectedPos(countyLS, &listitems, &listcnt);
   
   
   /* get the product text and display it */

   if (listcnt > 0)
   {
      itemnum = listitems[0];
      countiesPtr = (Counties *) ListNth(&countiesHead->list, itemnum);
      if (countiesPtr != NULL)
      {      
	 XmTextSetString(county_stateTX,   countiesPtr->state);
	 XmTextSetString(county_countyTX,  countiesPtr->county);      
	 XmTextSetString(county_fipsTX,    countiesPtr->countynum);

	 XmTextSetString(county_wfoTX,     countiesPtr->wfo);
	 XmTextSetString(county_primaryTX,   countiesPtr->primary_back);
	 XmTextSetString(county_secondaryTX, countiesPtr->secondary_back);
      }
      
      else
	 fprintf(stderr, "Error getting item from Counties list.\n");
      
      free(listitems);
   }
   else
   {
      XmTextSetString(county_stateTX,  "");
      XmTextSetString(county_countyTX, "");      
      XmTextSetString(county_fipsTX,   "");
      XmTextSetString(county_wfoTX,     "");
      XmTextSetString(county_primaryTX,   "");
      XmTextSetString(county_secondaryTX, "");
   }
   
   if (ListRsrcGetSelectedCount(countyLS))
   {
      Sensitize(county_updatePB);
      Sensitize(county_deletePB);
   }
   else
   {
      DeSensitize(county_updatePB);
      DeSensitize(county_deletePB);
   }

   
   return;
}


/************************************************************************
   
   Callback for loading (or attempting) to load text of selected
   item into text fields.
   
   **********************************************************************/

void load_zonetextCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   Eligzon	*eligzonPtr;
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   
   
   /* determine which item is selected */
   
   XmListGetSelectedPos(zoneLS, &listitems, &listcnt);
   
   
   /* get the product text and display it */

   if (listcnt > 0)
   {
      itemnum = listitems[0];
      eligzonPtr = (Eligzon *) ListNth(&eligzonHead->list, itemnum);
      if (eligzonPtr != NULL)
      {      
	 XmTextSetString(zone_stateTX, eligzonPtr->state);
	 XmTextSetString(zone_descrTX, eligzonPtr->descr);
	 XmTextSetString(zone_numTX,   eligzonPtr->zonenum);
      }
      
      else
	 fprintf(stderr, "Error getting item from Eligzon list.\n");
      
      free(listitems);
   }
   else
   {
      XmTextSetString(zone_stateTX, "");
      XmTextSetString(zone_descrTX, "");
      XmTextSetString(zone_numTX,   "");
   }

   if (ListRsrcGetSelectedCount(zoneLS))
   {
      Sensitize(zone_updatePB);
      Sensitize(zone_deletePB);
   }
   else
   {
      DeSensitize(zone_updatePB);
      DeSensitize(zone_deletePB);
   }

   
   return;
}


/************************************************************************
   
   Add an item to the scrolled list.
   
   **********************************************************************/

void state_addCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   State 	stateinfo;
   int 		status;
   char		msgstr[80];
   
   
   /* get the items from the text fields */
   
   read_state_info(&stateinfo);
   
   
   /* now add the info */
   
   status = PutState(&stateinfo);
   
   if (status < 0)
   {
      if ((status == -239) || (status == -268))
	 sprintf(msgstr,
		 "Information not added. \nSpecified STATE already defined.");
      else if (status == -391)
	 sprintf(msgstr,
		 "Information not added. \nNo STATE specified.");
      else
	 sprintf(msgstr, "Add of record failed; err= %d", status);
      
      ErrorDialog(stcozoDS, msgstr);
   }
   
   
   /* update the display of the list */
   
   else
   {
      load_statelist();
      stcozo_findstate(&stateinfo);
   }
   
   
   return;
}


/************************************************************************
   
  Update the entered item into the scrolled list.
   
   **********************************************************************/

void state_updateCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   State 	stateinfo;
   State	*statePtr;
   int 		status;
   char		where[80];
   char		msgstr[80];
   
   
   /* determine which item is selected so as to 
      build the key for the record to update */
   
   XmListGetSelectedPos(stateLS, &listitems, &listcnt);
   
   if (listcnt == 0)
   {
      sprintf(msgstr, "Update failed.  No item selected from list");      
      ErrorDialog(stcozoDS, msgstr);
      return;
   }
   
   else
   {
      itemnum = listitems[0];
      free(listitems);
   }
   
   
   
   statePtr = (State *) ListNth(&stateHead->list, itemnum);
   if (statePtr != NULL)
   {
      /* get the new items from the text fields */
      
      read_state_info(&stateinfo);
      
      
      /* now update the info using the key from the item that
	 was selected and the data below.  note that it may attempt
	 to update the key fileds. */
      
      sprintf(where, " WHERE state = '%s' ", statePtr->state);
      status = UpdateState(&stateinfo, where);
      
      if (status == -692)
      {
	 sprintf(msgstr,
		 "Update prevented.  Previous key %s still "
		 "referenced in other tables.", 
		 statePtr->state);
	 ErrorDialog(stcozoDS, msgstr);
	 return;
      }
      else if (status < 0)
      {
	 sprintf(msgstr, "Error %d updating data into State table\n", status);
	 ErrorDialog(stcozoDS, msgstr);
      }
      
      /* update the display of the list */
      
      else
      {
	 load_statelist();
	 stcozo_findstate(&stateinfo);
      }
   }
   
   else
      fprintf(stderr, "Error getting item to update from State list.\n");
   
    
   
   
   return;
}


/************************************************************************
   
   Delete an item to the scrolled list.
   
   **********************************************************************/

void state_deleteCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   State 	*statePtr;
   int 		status;
   char		where[60];
   char		msgstr[80];
   
   
   /* determine which item is selected */
   
   XmListGetSelectedPos(stateLS, &listitems, &listcnt);
   if (listcnt == 0)
   {
      sprintf(msgstr, "Delete failed.  No item selected from list");      
      ErrorDialog(stcozoDS, msgstr);
      return;
   }
   
   else
   {
      itemnum = listitems[0];
      free(listitems);
   }
   
   
   statePtr = (State *) ListNth(&stateHead->list, itemnum);
   if (statePtr != NULL)
   {      
      sprintf(where, " WHERE state = '%s' ", statePtr->state);
      
      status = DeleteState(where);
      
      if (status < 0)
      {
	 if (status == -692)
	    sprintf(msgstr,
		    "Delete failed.  %s still referenced in other tables.", 
		    statePtr->state); 
	 else
	    sprintf(msgstr,
		    "Delete failed; err= %d", status); 
	 
	 ErrorDialog(stcozoDS, msgstr);
      }
      
      
      /* update the display of the list */
      
      else
      {
	 load_statelist();
      }
   }
   
   else
      fprintf(stderr, "Error getting item to delete from State list.\n");
   
     
   return;
}


/************************************************************************
   
   Add an item to the scrolled list.
   
   **********************************************************************/

void county_addCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   Counties 	countiesinfo;
   int 		status;
   char		msgstr[80];
   
   
   /* get the items from the text fields */
   
   read_county_info(&countiesinfo);
   
   
   /* now add the info */
   
   status = PutCounties(&countiesinfo);
   
   if (status < 0)
   {
      if ((status == -239) || (status == -268))
	 sprintf(msgstr,
		 "Information not added. \nSpecified COUNTY already defined.");
      else
	 sprintf(msgstr, "Add of record failed; err= %d", status);
      
      ErrorDialog(stcozoDS, msgstr);
   }
   
   
   /* update the display of the list */
   
   else
   {
      load_countylist();
      stcozo_findcounty(&countiesinfo);
   }
   
   
   return;
}


/************************************************************************
   
  Update the entered item into the scrolled list.
   
   **********************************************************************/

void county_updateCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   Counties 	countiesinfo;
   Counties	*countiesPtr;
   int 		status;
   char		where[80];
   char		msgstr[80];
   
   
   /* determine which item is selected so as to 
      build the key for the record to update */
   
   XmListGetSelectedPos(countyLS, &listitems, &listcnt);
   
   if (listcnt == 0)
   {
      sprintf(msgstr, "Update failed.  No item selected from list");      
      ErrorDialog(stcozoDS, msgstr);
      return;
   }
   
   else
   {
      itemnum = listitems[0];
      free(listitems);
   }
   
   
   
   countiesPtr = (Counties *) ListNth(&countiesHead->list, itemnum);
   if (countiesPtr != NULL)
   {
      /* get the new items from the text fields  */
      
      read_county_info(&countiesinfo);
      
      
      /* now update the info using the key from the item that
	 was selected and the data below.  note that it may attempt
	 to update the key fileds. */
      
      sprintf(where, " WHERE state = '%s' AND county = '%s' ",
	      countiesPtr->state, countiesPtr->county);
      status = UpdateCounties(&countiesinfo, where);
      
      if (status == -692)
      {
	 sprintf(msgstr,
		 "Update prevented.  Previous key %s:%s still "
		 "referenced in other tables.", 
		 countiesPtr->state, countiesPtr->county);
	 ErrorDialog(stcozoDS, msgstr);
	 return;
      }
      else if (status < 0)
      {
	 sprintf(msgstr, "Error %d updating data into Counties table\n",
		 status);
	 ErrorDialog(stcozoDS, msgstr);
      }
      
      /* update the display of the list */
      
      else
      {
	 load_countylist();
	 stcozo_findcounty(&countiesinfo);
      }
   }
   
   else
      fprintf(stderr, "Error getting item to update from Counties list.\n");
   
   return;
}


/************************************************************************
   
   Delete an item to the scrolled list.
   
   **********************************************************************/

void county_deleteCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   Counties 	*countiesPtr;
   int 		status;
   char		where[60];
   char		msgstr[80];
   
   
   /* determine which item is selected */
   
   XmListGetSelectedPos(countyLS, &listitems, &listcnt);
   if (listcnt == 0)
   {
      sprintf(msgstr, "Delete failed.  No item selected from list");      
      ErrorDialog(stcozoDS, msgstr);
      return;
   }
   
   else
   {
      itemnum = listitems[0];
      free(listitems);
   }
   
   
   countiesPtr = (Counties *) ListNth(&countiesHead->list, itemnum);
   if (countiesPtr != NULL)
   {      
      sprintf(where, " WHERE state = '%s' AND county = '%s' ",
	      countiesPtr->state, countiesPtr->county);
      
      status = DeleteCounties(where);
      
      if (status < 0)
      {
	 if (status == -692)
	    sprintf(msgstr,
		    "Delete failed.  %s:%s still referenced in other tables.", 
		    countiesPtr->state, countiesPtr->county); 
	 else
	    sprintf(msgstr,
		    "Delete failed; err= %d", status); 
	 
	 ErrorDialog(stcozoDS, msgstr);
      }
      
      
      /* update the display of the list */
      
      else
      {
	 load_countylist();
      }
   }
   
   else
      fprintf(stderr, "Error getting item to delete from Counties list.\n");
   
     
   return;
}


/************************************************************************
   
   Add an item to the scrolled list.
   
   **********************************************************************/

void zone_addCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   Eligzon 	eligzoninfo;
   int 		status;
   char		msgstr[80];
   
   
   /* get the items from the text fields */
   
   read_zone_info(&eligzoninfo);
   
   
   /* now add the info */
   
   status = PutEligzon(&eligzoninfo);
   
   if (status < 0)
   {
      if ((status == -239) || (status == -268))
	 sprintf(msgstr,
		 "Information not added. \nSpecified ZONE already defined.");
      else if (status == -391)
	 sprintf(msgstr,
		 "Information not added. \nNo ZONE specified.");
      else
	 sprintf(msgstr, "Add of record failed; err= %d", status);
      
      ErrorDialog(stcozoDS, msgstr);
   }
   
   
   /* update the display of the list */
   
   else
   {
      load_zonelist();
      stcozo_findzone(&eligzoninfo);
   }
   
   
   return;
}


/************************************************************************
   
  Update the entered item into the scrolled list.
   
   **********************************************************************/

void zone_updateCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   Eligzon 	eligzoninfo;
   Eligzon	*eligzonPtr;
   int 		status;
   char		where[80];
   char		msgstr[80];
   
   
   /* determine which item is selected so as to 
      build the key for the record to update */
   
   XmListGetSelectedPos(zoneLS, &listitems, &listcnt);
   
   if (listcnt == 0)
   {
      sprintf(msgstr, "Update failed.  No item selected from list");      
      ErrorDialog(stcozoDS, msgstr);
      return;
   }
   
   else
   {
      itemnum = listitems[0];
      free(listitems);
   }
   
   
   
   eligzonPtr = (Eligzon *) ListNth(&eligzonHead->list, itemnum);
   if (eligzonPtr != NULL)
   {
      /* get the new items from the text fields */
      
      read_zone_info(&eligzoninfo);
      
      
      /* now update the info using the key from the item that
	 was selected and the data below.  note that it may attempt
	 to update the key fileds. */
      
      sprintf(where, " WHERE state = '%s' and zonenum = '%s' ",
	      eligzonPtr->state, eligzonPtr->zonenum);
      status = UpdateEligzon(&eligzoninfo, where);
      
      if (status == -692)
      {
	 sprintf(msgstr,
		 "Update prevented.  Previous key %s:%s still "
		 "referenced in other tables.", 
		 eligzonPtr->state, eligzonPtr->zonenum);
	 ErrorDialog(stcozoDS, msgstr);
	 return;
      }
      else if (status < 0)
      {
	 sprintf(msgstr, "Error %d updating data into Zone table\n", status);
	 ErrorDialog(stcozoDS, msgstr);
      }
      
      /* update the display of the list */
      
      else
      {
	 load_zonelist();
	 stcozo_findzone(&eligzoninfo);
      }
   }
   
   else
      fprintf(stderr, "Error getting item to update from Zone list.\n");
   
   
   return;
}


/************************************************************************
   
   Delete an item to the scrolled list.
   
   **********************************************************************/

void zone_deleteCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   Eligzon 	*eligzonPtr;
   int 		status;
   char		where[60];
   char		msgstr[80];
   
   
   /* determine which item is selected */
   
   XmListGetSelectedPos(zoneLS, &listitems, &listcnt);
   if (listcnt == 0)
   {
      sprintf(msgstr, "Delete failed.  No item selected from list");      
      ErrorDialog(stcozoDS, msgstr);
      return;
   }
   
   else
   {
      itemnum = listitems[0];
      free(listitems);
   }
   
   
   eligzonPtr = (Eligzon *) ListNth(&eligzonHead->list, itemnum);
   if (eligzonPtr != NULL)
   {      
      sprintf(where, " WHERE state = '%s' AND zonenum = '%s' ",
	      eligzonPtr->state, eligzonPtr->zonenum);
      
      status = DeleteEligzon(where);
      
      if (status < 0)
      {
	 if (status == -692)
	    sprintf(msgstr,
		    "Delete failed.  %s:%s still referenced in other tables.", 
		    eligzonPtr->state, eligzonPtr->zonenum); 
	 else
	    sprintf(msgstr,
		    "Delete failed; err= %d", status); 
	 
	 ErrorDialog(stcozoDS, msgstr);
      }
      
      
      /* update the display of the list */
      
      else
      {
	 load_zonelist();
      }
   }
   
   else
      fprintf(stderr, "Error getting item to delete from Eligzon list.\n");
   
     
   return;
}


/************************************************************************
   
   Get the info from the text fields.
   
   **********************************************************************/

void read_state_info(State *stateinfo)
{
   char *valstr;
   
   valstr = XmTextGetString(state_stateTX);
   strcpy(stateinfo->state, valstr);
   XtFree(valstr);
   
   valstr = XmTextGetString(state_nameTX);
   strcpy(stateinfo->name, valstr);
   XtFree(valstr);
   
   return;   
}


/************************************************************************
   
   Get the info from the text fields.
   
   **********************************************************************/

void read_county_info(Counties *countiesinfo)
{
   char *valstr;
   
   valstr = XmTextGetString(county_stateTX);
   strcpy(countiesinfo->state, valstr);
   XtFree(valstr);
   
   valstr = XmTextGetString(county_countyTX);
   strcpy(countiesinfo->county, valstr);
   XtFree(valstr);
   
   valstr = XmTextGetString(county_fipsTX);
   strcpy(countiesinfo->countynum, valstr);
   XtFree(valstr);
  
   valstr = XmTextGetString(county_wfoTX);
   strcpy(countiesinfo->wfo, valstr);
   XtFree(valstr);

   valstr = XmTextGetString(county_primaryTX);
   strcpy(countiesinfo->primary_back, valstr);
   XtFree(valstr);

   valstr = XmTextGetString(county_secondaryTX);
   strcpy(countiesinfo->secondary_back, valstr);
   XtFree(valstr);

   return;   
}


/************************************************************************
   
   Get the info from the text fields.
   
   **********************************************************************/

void read_zone_info(Eligzon *eligzoninfo)
{
   char *valstr;
   
   valstr = XmTextGetString(zone_stateTX);
   strcpy(eligzoninfo->state, valstr);
   XtFree(valstr);
   
   valstr = XmTextGetString(zone_numTX);
   strcpy(eligzoninfo->zonenum, valstr);
   XtFree(valstr);
   
   valstr = XmTextGetString(zone_descrTX);
   strcpy(eligzoninfo->descr, valstr);
   XtFree(valstr);
      
   return;   
}


/************************************************************************
   
   Find function for state.
   
   **********************************************************************/

void stcozo_findstate(State *statePtr)
{
   char			liststr[80];
   XmString		item;
   int			pos;

   
   sprintf(liststr, "%-2s     %s", statePtr->state, statePtr->name);
   item = XmStringCreateSimple(liststr);
   
   if (XmListItemExists(stateLS, item))
   {
      pos = XmListItemPos(stateLS, item);
      XmListSetPos(stateLS, pos);
      XmListSelectPos(stateLS, pos, True);
   }
   else
   {
      XmListSetPos(stateLS, 1);
      XmListSelectPos(stateLS, 1, True);
   }
   XmStringFree(item);

   return;
}


/************************************************************************
   
   Find function for county.
   
   **********************************************************************/

void stcozo_findcounty(Counties *countiesPtr)
{
   char			liststr[80];
   XmString		item;
   int			pos;

   
   sprintf(liststr, "%-2s     %-20s   %-3s", countiesPtr->state,
	   countiesPtr->county, countiesPtr->countynum);
   item = XmStringCreateSimple(liststr);
   
   if (XmListItemExists(countyLS, item))
   {
      pos = XmListItemPos(countyLS, item);
      XmListSetPos(countyLS, pos);
      XmListSelectPos(countyLS, pos, True);
   }
   else
   {
      XmListSetPos(countyLS, 1);
      XmListSelectPos(countyLS, 1, True);
   }
   XmStringFree(item);

   return;
}


/************************************************************************
   
   Find function for zone.
   
   **********************************************************************/

void stcozo_findzone(Eligzon *eligzonPtr)
{
   char			liststr[80];
   XmString		item;
   int			pos;

   
   sprintf(liststr, "%-2s    %-3s       %-20s", eligzonPtr->state,
	   eligzonPtr->zonenum, eligzonPtr->descr);
   item = XmStringCreateSimple(liststr);
   
   if (XmListItemExists(zoneLS, item))
   {
      pos = XmListItemPos(zoneLS, item);
      XmListSetPos(zoneLS, pos);
      XmListSelectPos(zoneLS, pos, True);
   }
   else
   {
      XmListSetPos(zoneLS, 1);
      XmListSelectPos(zoneLS, 1, True);
   }
   XmStringFree(item);

   return;
}


/************************************************************************
   
   Free any memory allocated for the data.
   
   **********************************************************************/

void free_stcozo(void)
{
   
   if (stateHead != NULL)
   {
      FreeState(stateHead);
      stateHead = (State *)NULL;
   }
   
   if (countiesHead == NULL)
   {
      FreeCounties(countiesHead);
      countiesHead = (Counties *)NULL;
   }
   
   if (eligzonHead != NULL)
   {
      FreeEligzon(eligzonHead);
      eligzonHead = (Eligzon *)NULL;
   }
   
   
   return;
}


/************************************************************************
   
   Close the window.
   
   **********************************************************************/

void ok_stcozoCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   /* free any allocated memory */
   free_stcozo();

   
   if (XtIsManaged(stcozoDS))
   {
      XtDestroyWidget(stcozoDS);
      stcozoDS = NULL;
   }
   
   return;
}



