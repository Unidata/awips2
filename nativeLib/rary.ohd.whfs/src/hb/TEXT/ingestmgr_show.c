
/*
	File:		ingestmgr_show.c
	Date:		February 1997
	Author:		Paul Taylor

	Purpose:	Provide support for the Ingest Filter DS.
	
*/


/*
	Standard includes.
*/
#include <time.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>


/*
	Motif includes.
*/
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>


/*
	Local library includes.
*/
#include "Xtools.h"
#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "IngestFilter.h"


/*
	Local includes.
*/
#include "hybase.h"
#include "hbAS.h"
#include "user_prefs.h"
#include "ingestmgr.h"
#include "ingestmgr_show.h"
#include "loc_cbs.h"



void	ShowIngestDs(Widget w)
{
   in_info	*inPtr = getIngestFilterInfo(True);
   
   if (! ingestDS)
   {
      create_ingestDS(GetTopShell(w));
      in_create_shefdur_btns(inPtr);
      in_create_shefts_btns(inPtr);
      in_create_shefex_btns(inPtr);
      ingest_callbacks();
   }
   
   
   if (! XtIsManaged(ingestDS))
   {
      inPtr->shefpePtr = ingest_getshefpes();
      ingest_loadpeLI(infilter_peLI, inPtr->shefpePtr);
      ingest_loadpeLI(initem_peLI, inPtr->shefpePtr);
      
      ingest_loaddisplay(NULL, NULL, NULL);
      
      XtManageChild(ingestFO);
      XtManageChild(ingestDS);
      XtUnmanageChild(ingest_helpPB);
   }
   
   return;
}


/*
	Driver function for main display which sets up default values
	for the display and determines what data is to be loaded using
	the "filter parameters".
	
	The display is then loaded with data.
*/
void	ingest_loaddisplay(Widget w, XtPointer ptr, XtPointer cbs)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   
   strcpy(inPtr->table, "ingestfilter");
   strcpy(inPtr->orderby_phrase, " ORDER BY lid, pe, dur, ts, extremum, ");
   strcat(inPtr->orderby_phrase, "ts_rank, ingest, ofs_input, stg2_input ");
   
   in_Sense_LocTB(NULL, NULL, NULL);	/* Sensitizes for "LocTB". */
   in_Sense_PeTB(NULL, NULL, NULL);	/* Sensitizes for "PeTB". */
   in_Sense_TsTB(NULL, NULL, NULL);	/* Sensitizes for "TsTB". */
   in_Sense_SwTB(NULL, NULL, NULL);	/* Sensitizes for "SwTB". */
      
   
   /*
   	Determine what data to load.
   */
   in_filter_grabLocPeTsSw();
   ingest_loadDataIntoDisplay();
   
   return;
}


/*
	Controls the loading of data into the display.
*/
void	ingest_loadDataIntoDisplay(void)
{
   in_info	*inPtr = getIngestFilterInfo(False);


   /*
   	Build where_phrase & whereorderby_phrase.
   */
/*
printf("PHRASES:\n");
printf("loc_phrase == <%s>\n", inPtr->loc_phrase);
printf("pe_phrase == <%s>\n", inPtr->pe_phrase);
printf("ts_phrase == <%s>\n", inPtr->ts_phrase);
printf("sw_phrase == <%s>\n", inPtr->sw_phrase);
*/

   in_filter_buildPhrases(inPtr->loc_phrase, inPtr->pe_phrase,
			  inPtr->ts_phrase,  inPtr->sw_phrase,
			  inPtr->orderby_phrase);

/*
printf("where_phrase == <%s>\n", inPtr->where_phrase);
printf("whereorderby_phrase == <%s>\n", inPtr->whereorderby_phrase);
*/

   /*
   	Load data into the display.
   */
   ingest_clearDisplay();
   inPtr->item_where_ingestPtr = NULL;	/* No item selected in inlistLI */
   inPtr->ingestPtr = ingest_getingest();
   ingest_loadingest();
   
   return;
}


/*
     	Used to Sensitize and DeSensitize widgets for existence of data.
*/
void 	indata_setSensitivity(Boolean set)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   
   if (set)
   {
      Sensitize(initemFR);
      Sensitize(initem_peLI);

      if (inPtr->data_entry_mode == IN_NORMAL_ENTRY_MODE)
      {
	 Sensitize(ingest_deletePB);
      }
      else if (inPtr->data_entry_mode == IN_NEW_ENTRY_MODE)
      {
	 DeSensitize(ingest_deletePB);
      }
   }
   else
   {
      DeSensitize(initemFR);
      DeSensitize(initem_peLI);
      DeSensitize(ingest_deletePB);
   }
   
   return;   
}


/*
	Returns the correct PB-name by specifying an integer duration.
*/
char*	in_get_shefdur_name(int dur)
{
   static char	name[MAX_BUF_LEN];
   
   ShefDur	*shefdur=NULL,
      		*shefdurPtr=NULL;
   
   char		where[MAX_WHERE_LEN];
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE dur = '%i' ", dur);
   shefdur = GetShefDur(where);
   if (shefdur != (ShefDur *) NULL)
	shefdurPtr = (ShefDur *) ListFirst(&shefdur->list);
   
   if (shefdurPtr != (ShefDur *) NULL)
   {
      memset(&name, '\0', sizeof(name));
      strcpy(name, in_create_shefdur_name(shefdurPtr));
      FreeShefDur(shefdurPtr);
      return(&name[0]);
   }
   else
      return("");
}


/*
	Creates a PB-name for a given shefdur.
*/
char*	in_create_shefdur_name(ShefDur *shefdurPtr)
{
   static char		name[BUFSIZ];
   char			buf[10];
   
   memset(&name, '\0', sizeof(name));
   memset(&buf, '\0', sizeof(buf));

   strcpy(name, shefdurPtr->name);
   sprintf(buf, " (%i)", shefdurPtr->dur);
   strcat(name, buf);
   
   return(&name[0]);
}


/*
	Creates the buttons needed for the Duration OM.
*/
void	in_create_shefdur_btns(in_info *inPtr)
{
   static ShefDur	*shefdur = NULL,
			*shefdurPtr = NULL;	/* for storage */

   char			wname[BUFSIZ];
   
   ShefDur		*sdurPtr = NULL;	/* for loop control only */
   Widget		sdurPB;


   /*
   	Free old memory.
   	Retrieve new record(s) from the dbms.

	If there is at least 1 record in the dbms, set inPtr->shefdurPtr.
	Otherwise, set inPtr->shefdurPtr to NULL.
   */
   ingest_freeshefdur(inPtr);
   shefdur = GetShefDur(" ORDER BY dur ");
   if (shefdur != (ShefDur *) NULL)
   {
      shefdurPtr = (ShefDur *) ListFirst(&shefdur->list);
      inPtr->shefdurPtr = shefdurPtr;  /* keep the list of records around */


      /* LOOPING */
      sdurPtr = shefdurPtr;
      while(sdurPtr != (ShefDur *) NULL)
      {
	 strcpy(wname, in_create_shefdur_name(sdurPtr));
	 
	 sdurPB = XtVaCreateManagedWidget(wname,
					  xmPushButtonWidgetClass,
					  initem_durPDM,
					  NULL);
	 
	 sdurPtr = (ShefDur *) ListNext(&sdurPtr->node);
      }
   }
   else
   {
      inPtr->shefdurPtr = NULL;  /* no records in dbms */
   }
   
   return;
}


/*
	Returns the correct PB-name by specifying an typesource.
*/
char*	in_get_shefts_name(char *ts)
{
   static char	name[MAX_BUF_LEN];
   
   ShefTs	*shefts=NULL,
      		*sheftsPtr=NULL;
   
   char		where[MAX_WHERE_LEN];
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE ts = '%s' ", ts);
   shefts = GetShefTs(where);
   if (shefts != (ShefTs *) NULL)
	sheftsPtr = (ShefTs *) ListFirst(&shefts->list);
   
   if (sheftsPtr != (ShefTs *) NULL)
   {
      memset(&name, '\0', sizeof(name));
      strcpy(name, in_create_shefts_name(sheftsPtr));
      FreeShefTs(sheftsPtr);
      return(&name[0]);
   }
   else
      return("");
}


/*
	Creates a PB-name for a given shefts.
*/
char*	in_create_shefts_name(ShefTs *sheftsPtr)
{
   static char		name[BUFSIZ];
   char			buf[10];
   
   memset(&name, '\0', sizeof(name));
   memset(&buf, '\0', sizeof(buf));

   strcpy(name, sheftsPtr->name);
   sprintf(buf, " (%s)", sheftsPtr->ts);
   strcat(name, buf);
   
   return(&name[0]);
}


/*
	Creates the buttons needed for the TypeSrc OM.
*/
void	in_create_shefts_btns(in_info *inPtr)
{
   static ShefTs	*shefts = NULL,
			*sheftsPtr = NULL;	/* for storage */

   char			wname[BUFSIZ];
   
   ShefTs		*stsPtr = NULL;	/* for loop control only */
   Widget		stsPB, sftsPB;


   /*
   	Free old memory.
   	Retrieve new record(s) from the dbms.

	If there is at least 1 record in the dbms, set inPtr->sheftsPtr.
	Otherwise, set inPtr->sheftsPtr to NULL.
   */
   ingest_freeshefts(inPtr);
   shefts = GetShefTs(" ORDER BY ts ");
   if (shefts != (ShefTs *) NULL)
   {
      sheftsPtr = (ShefTs *) ListFirst(&shefts->list);
      inPtr->sheftsPtr = sheftsPtr;  /* keep the list of records around */


      /* LOOPING */
      stsPtr = sheftsPtr;
      while(stsPtr != (ShefTs *) NULL)
      {
	 strcpy(wname, in_create_shefts_name(stsPtr));
	 
	 stsPB = XtVaCreateManagedWidget(wname,
					 xmPushButtonWidgetClass,
					 initem_tsPDM,
					 NULL);
	 
	 sftsPB = XtVaCreateManagedWidget(wname,
					  xmPushButtonWidgetClass,
					  infilter_tsPDM,
					  NULL);
	 
	 stsPtr = (ShefTs *) ListNext(&stsPtr->node);
      }
   }
   else
   {
      inPtr->sheftsPtr = NULL;  /* no records in dbms */
   }
   
   return;
}


/*
	Returns the correct PB-name by specifying an extremum.
*/
char*	in_get_shefex_name(char *ex)
{
   static char	name[MAX_BUF_LEN];
   
   ShefEx	*shefex=NULL,
      		*shefexPtr=NULL;
   
   char		where[MAX_WHERE_LEN];
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE extremum = '%s' ", ex);
   shefex = GetShefEx(where);
   if (shefex != (ShefEx *) NULL)
	shefexPtr = (ShefEx *) ListFirst(&shefex->list);
   
   if (shefexPtr != (ShefEx *) NULL)
   {
      memset(&name, '\0', sizeof(name));
      strcpy(name, in_create_shefex_name(shefexPtr));
      FreeShefEx(shefexPtr);
      return(&name[0]);
   }
   else
      return("");
}


/*
	Creates a PB-name for a given shefex.
*/
char*	in_create_shefex_name(ShefEx *shefexPtr)
{
   static char		name[BUFSIZ];
   char			buf[10];
   
   memset(&name, '\0', sizeof(name));
   memset(&buf, '\0', sizeof(buf));

   strcpy(name, shefexPtr->name);
   sprintf(buf, " (%s)", shefexPtr->extremum);
   strcat(name, buf);
   
   return(&name[0]);
}


/*
	Creates the buttons needed for the Extremum OM.
*/
void	in_create_shefex_btns(in_info *inPtr)
{
   static ShefEx	*shefex = NULL,
			*shefexPtr = NULL;	/* for storage */

   char			wname[BUFSIZ];
   
   ShefEx		*exPtr = NULL;	/* for loop control only */
   Widget		exPB;


   /*
   	Free old memory.
   	Retrieve new record(s) from the dbms.

	If there is at least 1 record in the dbms, set inPtr->shefexPtr.
	Otherwise, set inPtr->shefexPtr to NULL.
   */
   ingest_freeshefex(inPtr);
   shefex = GetShefEx(" ORDER BY extremum ");
   if (shefex != (ShefEx *) NULL)
   {
      shefexPtr = (ShefEx *) ListFirst(&shefex->list);
      inPtr->shefexPtr = shefexPtr;  /* keep the list of records around */


      /* LOOPING */
      exPtr = shefexPtr;
      while(exPtr != (ShefEx *) NULL)
      {
	 strcpy(wname, in_create_shefex_name(exPtr));
	 
	 exPB = XtVaCreateManagedWidget(wname,
					xmPushButtonWidgetClass,
					initem_extPDM,
					NULL);
	 
	 exPtr = (ShefEx *) ListNext(&exPtr->node);
      }
   }
   else
   {
      inPtr->shefexPtr = NULL;  /* no records in dbms */
   }
   
   return;
}


/*
	This function returns the correct PB-name given a Physical Element.
*/
char*	in_get_shefpe_name(char *pe)
{
   ShefPe	*shefpe=NULL,
      		*shefpePtr=NULL;
   
   char		where[MAX_WHERE_LEN] ;
   static char buf [MAX_BUF_LEN] ;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE pe = '%s' ", pe);
   shefpe = GetShefPe(where);
   if (shefpe != (ShefPe *) NULL)
	shefpePtr = (ShefPe *) ListFirst(&shefpe->list);
   
   if (shefpePtr != (ShefPe *) NULL)
   {
      memset(&buf, '\0', sizeof(buf));
      strcpy(buf, shefpePtr->name);
      FreeShefPe(shefpePtr);
      return(buf);
   }
   else
      return("");
}


/*
	This function gets the 1st pe from a given peLI.
*/
char*	in_getpe_from_peLI(Widget peLI)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   ShefPe	*shefpePtr = NULL;
   
   int		*poslist,
      		cnt,
		pe_nth;

   
   XmListGetSelectedPos(peLI, &poslist, &cnt);
   
   if (cnt > 0)
   {
      pe_nth = poslist[0];
      shefpePtr = (ShefPe *) ListNth(&inPtr->shefpePtr->list, pe_nth);
      
      if (poslist != (int *) NULL)
      {
	 free(poslist);
	 poslist = NULL;
      }
      
      return(shefpePtr->pe);
   }
   
   return(NULL);
}


/*
	This function sets a peLI to a specified pe.
*/
void	in_loadpe_into_peLI(Widget peLI, char *pe)
{
   XmString	item;
   char		buf[MAX_BUF_LEN];
   
   /*
   	Get the name for the pe,
	build the pe to select in the list box,
	and select it.
   */
   strcpy(buf, pe);
   strcat(buf, " ");
   strcat(buf, in_get_shefpe_name(pe));
   while (strlen(buf) < 23)
      strcat(buf, " ");
   
   item = XmStringCreateSimple(buf);
   
   XmListSetItem(peLI, item);
   XmListSelectItem(peLI, item, True);
   
   XmStringFree(item);

   return;
}


/*
	Loads a given peLI with the specified list of shefpes.
*/
void	ingest_loadpeLI(Widget peLI, ShefPe *shefpePtr)
{
   char			buf[MAX_BUF_LEN];
   
   XmStringTable	xmStr;
   
   int			cnt,
      			i;

      
   if (shefpePtr != (ShefPe *) NULL)
   {
      cnt = ListCount(&shefpePtr->list);
      xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
      
      if (cnt > 0)
      {
	 i = 0;
	 while (shefpePtr != (ShefPe *) NULL)
	 {
	    memset(&buf, '\0', sizeof(buf));
	    sprintf(buf, "%s %-20s", shefpePtr->pe, shefpePtr->name);
	    
	    xmStr[i] = XmStringCreateSimple(buf);
	    i++;
	    
	    shefpePtr = (ShefPe *) ListNext(&shefpePtr->node);
	 }
	 
	 XmListAddItems(peLI, xmStr, cnt, 1);
	 
	 /*
	 	Free memory.
	 */
	 for (i=0; i<cnt; i++)
	 {
	    XmStringFree(xmStr[i]);
	 }
	 XtFree((char *) xmStr);
      }
   }
   
   return;
}


void	in_recompute_inlistLI_selpos(in_info *inPtr)
{
   int	selpos = inPtr->inlistLI_selpos;
   int	item_count;

   /*
   	Get the item_count.
	Subtract one from selpos if at end of list, otherwise--no change.
   */
   XtVaGetValues(inlistLI, XmNitemCount, &item_count, NULL);
   if (selpos == item_count)
   {
      selpos = selpos - 1;
   }
   

   /*
   	Boundary check.
   	Assign the new value & return.
   */
   if (selpos <= 0)
   {
      selpos = 1;  /* no items in list, but protect by using a value of 1 */
   }
   inPtr->inlistLI_selpos = selpos;
   inPtr->delete_flag = True;  /* yes, a delete operation was just performed */
   
   return;
}


void	ingest_callbacks(void)
{	
   Atom		atom;
   WidgetList	widgetList;
   int		i, cnt = 0;
   
   /*
   	Window manager callbacks.
   */
   atom = XmInternAtom(XtDisplay(ingestDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(ingestDS, atom, ingest_close, NULL);
   
   
   /*
   	Widget callbacks.
   */
   XtAddCallback(ingest_okPB,    XmNactivateCallback, ingest_ok,      NULL);
   XtAddCallback(ingest_applyPB, XmNactivateCallback, ingest_apply,   NULL);
   XtAddCallback(ingest_cancelPB,XmNactivateCallback, ingest_close,   NULL);
   XtAddCallback(ingest_deletePB,XmNactivateCallback, ingest_del_conf,NULL);
   XtAddCallback(ingest_newPB,   XmNactivateCallback, ingest_new,     NULL);
   XtAddCallback(ingest_helpPB,  XmNactivateCallback, ingest_help,    NULL);
   
   XtAddCallback(inlistLI, XmNbrowseSelectionCallback, in_setItemData, NULL);
   
   XtAddCallback(inlist_setPB, XmNactivateCallback, in_setSwitches_conf, NULL);
   
   XtAddCallback(infilter_locTB, XmNvalueChangedCallback, in_Sense_LocTB, NULL);
   XtAddCallback(infilter_locTB, XmNvalueChangedCallback, in_useFilter,   NULL);
   XtAddCallback(infilter_locTE, XmNvalueChangedCallback, in_useFilter,   NULL);
   XtAddCallback(infilter_locTE, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)MIXEDCASE);

   XtAddCallback(infilter_peTB, XmNvalueChangedCallback,      in_Sense_PeTB, NULL);
   XtAddCallback(infilter_peTB, XmNvalueChangedCallback,      in_useFilter,  NULL);
   XtAddCallback(infilter_peLI, XmNmultipleSelectionCallback, in_useFilter,  NULL);

   XtAddCallback(infilter_tsTB, XmNvalueChangedCallback,      in_Sense_TsTB, NULL);
   XtAddCallback(infilter_tsTB, XmNvalueChangedCallback,      in_useFilter,  NULL);

   XtVaGetValues(infilter_tsPDM,
		 XmNchildren, &widgetList,
		 XmNnumChildren, &cnt,
		 NULL);
   for(i=0; i<cnt; i++)
   {
      XtAddCallback(widgetList[i], XmNactivateCallback, in_useFilter, NULL);
   }
   
   XtAddCallback(infilter_swTB,   XmNvalueChangedCallback,    in_Sense_SwTB, NULL);
   XtAddCallback(infilter_swTB,   XmNvalueChangedCallback,    in_useFilter,  NULL);
   XtAddCallback(infilter_mastTB, XmNvalueChangedCallback,    in_useFilter,  NULL);
   XtAddCallback(infilter_ofsTB,  XmNvalueChangedCallback,    in_useFilter,  NULL);
   XtAddCallback(infilter_stg2TB, XmNvalueChangedCallback,    in_useFilter,  NULL);
   
   
   XtAddCallback(initem_locTE, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)MIXEDCASE);
   
   return;
}


void	ingest_ok(Widget w, XtPointer ptr, XtPointer cbs)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   ingest_apply(NULL, NULL, NULL);

   if (inPtr->apply_successful)
   {
      ingest_close(NULL, NULL, NULL);
   }
   
   return;
}


/*
	Driver function that determines how to apply data to database.
	
	The helper functions below provide the tools necessary
	to apply data to the ingestfilter table.
*/
void	ingest_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   int 		form_element_error = 1;
   int		error = 1;
   
/*
printf("attempting apply...\n");
*/

   /*
   	Obtain data from the initemFO.
	
	Apply changes to the dbms, if possible.
	  Be sure to indicate whether or not the apply was successful.
   */
   form_element_error = in_getItemData(inPtr);
   if (form_element_error == 0)
   {
   	error = in_apply_ingestItemData(inPtr); /* return error, or 0 (no error) */
   }
   else
   {
   	inPtr->apply_successful = False;  /* failure */
	return;
   }
   
/*
printf("error from apply == <%i>\n", error);
*/

   
   /*
   	If there was no error in appling the record to the dbms...
	
   	  Ensure: New entry mode -> normal entry mode,
	  Load data into the display, and
	  Indicate that the apply was successful.
   */
   if (error == 0)
   {
      if (inPtr->data_entry_mode == IN_NEW_ENTRY_MODE)
      {
	 clearForm(initemFO);
	 if (inPtr->inlistLI_selpos > 0)
	 {
	    XmListSelectPos(inlistLI, inPtr->inlistLI_selpos, True);
	    XmListSetPos(inlistLI, inPtr->inlistLI_selpos);
	 }
	 SetLabel(initemLA, "Selected Item");
	 inPtr->data_entry_mode = IN_NORMAL_ENTRY_MODE;
      }
      
      ingest_loadDataIntoDisplay();
      inPtr->apply_successful = True;  /* success */
      return;
   }
   else
   {
      ErrorDialog(ingestDS, DbErrorString(error));
   }

   
   inPtr->apply_successful = False;  /* failure */
   
   return;
}


int	in_apply_ingestItemData(in_info *inPtr)
{
   IngestFilter		*ingestPtr = inPtr->item_ingestPtr;
   char			*ingestPtr_where = inPtr->item_where_ingestPtr;
   
   int			error;
   
   /*
   	Perform Update/Insert operation, as appropriate.
   */
   if (inPtr->data_entry_mode == IN_NEW_ENTRY_MODE)
   {
/*
printf("attempting put ingest (ingestPtr_where == <%s>...\n", ingestPtr_where);
*/
      
      error = PutIngestFilter(ingestPtr);
      if (error != 0)
      {
	 return(error);
      }
   }
   else if (inPtr->data_entry_mode == IN_NORMAL_ENTRY_MODE)
   {
/*
printf("attempting update ingest...where == <%s>\n",
       ingestPtr_where);
*/
      
      error = UpdateIngestFilter(ingestPtr, ingestPtr_where);
      if (error != 0)
      {
	 return(error);
      }
   }
   
   Loc_UpdateStationClassFields(ingestPtr->lid);
   
   return(0);  /* 0 means the "ingestItemData" apply was successful */
}


void	ingest_close(Widget w, XtPointer ptr, XtPointer cbs)
{

   freeIngestFilterInfo();
      
   if (XtIsManaged(ingestDS))
   {
      XtDestroyWidget(ingestDS);
      ingestDS = NULL;
   }
      
   return;
}


void	ingest_new(Widget w, XtPointer ptr, XtPointer cbs)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   int		*poslist, cnt;
   

   /*
   	Deselect the only-selected item from inlistLI.
   */
   XmListGetSelectedPos(inlistLI, &poslist, &cnt);
   if (cnt > 0)
   {
      XmListDeselectPos(inlistLI, poslist[0]);
      
      free(poslist);
      poslist = NULL;
   }
   

   /*
   	Change to new entry mode.
	Set data sensitivity to True.
   */
   if (inPtr->data_entry_mode == IN_NORMAL_ENTRY_MODE)
   {
      clearForm(initemFO);
      SetLabel(initemLA, "NEW Item");
      
      SetMenuPos(initem_durOM, 0);
      SetMenuPos(initem_tsOM, 0);
      SetMenuPos(initem_extOM, 0);
      XmListDeselectAllItems(initem_peLI);
      XmListSetPos(initem_peLI, 1);
      
      inPtr->data_entry_mode = IN_NEW_ENTRY_MODE;
   }
   indata_setSensitivity(True);

   
   return;
}


void	ingest_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   memset(&buf, '\0', sizeof(buf));
   sprintf(buf, "Do you wish to delete this entry?");
   qstDS = QuestionDialog(ingestDS, buf);
   SetTitle(qstDS,"Delete Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, ingest_delete, NULL);
   
   
   if (! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


/*
	Delete the selected item from the database,
	and reload data into the display.
*/
void	ingest_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   int		*poslist, cnt, nth_pos;
   
   IngestFilter	*ingestPtr = NULL;
   
   char		where[1000];
   
   int		error = 0;

   
   SetCursor(ingestFO, XC_watch);

   
   XmListGetSelectedPos(inlistLI, &poslist, &cnt);
   if (cnt > 0)
   {
      nth_pos = poslist[0];
      ingestPtr = (IngestFilter *) ListNth(&inPtr->ingestPtr->list, nth_pos);
      
      memset(&where, '\0', sizeof(where));
      strcpy(where, ingest_build_where(ingestPtr));
      
      error = DeleteIngestFilter((const char *) where);
/*
printf("error == <%i>, where == <%s>\n", error, where);
*/

      if (!error)
      {
	 in_recompute_inlistLI_selpos(inPtr);
	 
	 Loc_UpdateStationClassFields(ingestPtr->lid);
      }
      else
      {
	 UnsetCursor(ingestFO);
   	 ErrorDialog(ingestDS, DbErrorString(error));
	 return;
      }
   }
   
   
   /*
   	Always filter on Loc & Pe.
	Load the display with data.
   */
   in_filter_grabLocPeTsSw();
   ingest_loadDataIntoDisplay();

   
   UnsetCursor(ingestFO);
   
   
   return;
}


void	ingest_help(Widget w, XtPointer ptr, XtPointer cbs)
{
   return;
}


/*
     	Senses the state of the LocTB,
	Sensitizing and DeSensitizing where necessary.
*/
void 	in_Sense_LocTB(Widget w, XtPointer ptr, XtPointer cbs)
{

   if (XmToggleButtonGetState(infilter_locTB))
   {
      Sensitize(infilter_locLA);
      Sensitize(infilter_locTE);
   }
   else
   {
      DeSensitize(infilter_locLA);
      DeSensitize(infilter_locTE);
   }
   
   return;   
}


/*
     	Senses the state of the PeTB,
	Sensitizing and DeSensitizing where necessary.
*/
void 	in_Sense_PeTB(Widget w, XtPointer ptr, XtPointer cbs)
{

   if (XmToggleButtonGetState(infilter_peTB))
   {
      Sensitize(infilter_peLA);
      Sensitize(infilter_peLI);
   }
   else
   {
      DeSensitize(infilter_peLA);
      DeSensitize(infilter_peLI);
   }
   
   return;   
}


/*
     	Senses the state of the TsTB,
	Sensitizing and DeSensitizing where necessary.
*/
void 	in_Sense_TsTB(Widget w, XtPointer ptr, XtPointer cbs)
{

   if (XmToggleButtonGetState(infilter_tsTB))
   {
      Sensitize(infilter_tsOM);
   }
   else
   {
      DeSensitize(infilter_tsOM);
   }
   
   return;   
}


/*
     	Senses the state of the SwTB,
	Sensitizing and DeSensitizing where necessary.
*/
void 	in_Sense_SwTB(Widget w, XtPointer ptr, XtPointer cbs)
{

   if (XmToggleButtonGetState(infilter_swTB))
   {
      Sensitize(infilter_swLA);
      Sensitize(infilter_mastTB);
      Sensitize(infilter_ofsTB);
      Sensitize(infilter_stg2TB);
   }
   else
   {
      DeSensitize(infilter_swLA);
      DeSensitize(infilter_mastTB);
      DeSensitize(infilter_ofsTB);
      DeSensitize(infilter_stg2TB);
   }
   
   return;   
}


void	ingest_clearDisplay(void)
{
   /*
   	Clears out certain widgets.
   */
   XmListDeleteAllItems(inlistLI);
   XmListDeselectAllItems(initem_peLI);
   clearForm(initemFO);
   
   return;
}


/*
	The in_useFilter function calls its helper functions below it.

	There are functions to build phrases, get & set phrases,
	and to select & deselect a stored poslist.
*/
void	in_useFilter(Widget w, XtPointer ptr, XtPointer cbs)
{
   in_info	*inPtr = getIngestFilterInfo(False);

   /*
	If the infilter_peTB was chosen,
	Select or Deselect the infilter_peLI using the filterpe_poslist.
   */
   if (w == infilter_peTB)
   {
      if (XmToggleButtonGetState(infilter_peTB))	/* peTB --> ON */
      {
	 in_select_filterpe();  /* Select (show) the old poslist. */
	 inPtr->filterpe_poslist_okayfree = True;
      }
      else						/* peTB --> OFF */
      {
	 in_deselect_filterpe();  /* Deselect (hide) the new poslist. */
	 inPtr->filterpe_poslist_okayfree = False;
      }
   }
   
   if (w == infilter_swTB)
   {
      if (XmToggleButtonGetState(infilter_swTB))	/* swTB --> ON */
      {
	 in_select_switches();  /* Select (show) the old switches. */
      }
      else						/* swTB --> OFF */
      {
	 in_deselect_switches();  /* Deselect (hide) the new switches. */
      }

   }


   /*
   	Always filter on Loc & Pe.
	Load the display with data.
   */
   in_filter_grabLocPeTsSw();
   ingest_loadDataIntoDisplay();
   
   return;
}


void	in_filter_grabLocPeTsSw(void)
{
   ingest_setLocPhrase(NULL);
   if (XmToggleButtonGetState(infilter_locTB))
   {
      ingest_obtainLocPhrase();
   }

   ingest_setPePhrase(NULL);
   if (XmToggleButtonGetState(infilter_peTB))
   {
      ingest_obtainPePhrase();
   }

   ingest_setTsPhrase(NULL);
   if (XmToggleButtonGetState(infilter_tsTB))
   {
      ingest_obtainTsPhrase();
   }

   ingest_setSwPhrase(NULL);
   if (XmToggleButtonGetState(infilter_swTB))
   {
      ingest_obtainSwPhrase();
   }

   return;
}


void	in_select_filterpe(void)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   int		cnt = inPtr->filterpe_poslist_cnt;
   int		i;

   /*
   	Select (show) the previously-saved filterpe_poslist.
   */
   if (cnt > 0)
   {
      for (i=0; i<cnt; i++)
      {
	 XmListSelectPos(infilter_peLI, inPtr->filterpe_poslist[i], False);
      }
   }
   
   return;
}


void	in_deselect_filterpe(void)
{
   in_info	*inPtr = getIngestFilterInfo(False);

   int		cnt = inPtr->filterpe_poslist_cnt;
   int		i;
   
   /*
   	Deselect (hide) the filterpe_poslist.
   */
   if (cnt > 0)
   {
      for (i=0; i<cnt; i++)
      {
	 XmListDeselectPos(infilter_peLI, inPtr->filterpe_poslist[i]);
      }
   }
   
   return;
}


void	in_select_switches(void)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   

   /*
   	Select (show) the previously-saved switches TBs.
   */
   if (inPtr->switches_master)
      XmToggleButtonSetState(infilter_mastTB, True, False);
   
   if (inPtr->switches_ofs)
      XmToggleButtonSetState(infilter_ofsTB,  True, False);
   
   if (inPtr->switches_stg2)
      XmToggleButtonSetState(infilter_stg2TB, True, False);
   

   return;
}


void	in_deselect_switches(void)
{
   in_info	*inPtr = getIngestFilterInfo(False);

   
   /*
   	Deselect (hide) the switches TBs.
   */
   inPtr->switches_master = XmToggleButtonGetState(infilter_mastTB);
   inPtr->switches_ofs    = XmToggleButtonGetState(infilter_ofsTB);
   inPtr->switches_stg2   = XmToggleButtonGetState(infilter_stg2TB);

   XmToggleButtonSetState(infilter_mastTB, False, False);
   XmToggleButtonSetState(infilter_ofsTB,  False, False);
   XmToggleButtonSetState(infilter_stg2TB, False, False);
   
   
   return;
}


void	in_filter_buildPhrases(char *loc_phrase, char *pe_phrase,
			       char *ts_phrase,  char *sw_phrase,
			       char *orderby_phrase)
{
   static char		where_phrase[IN_MAX_WHERE_PHRASE],
      			whereorderby_phrase[IN_MAX_WHERE_PHRASE];
   
   in_info		*inPtr = getIngestFilterInfo(False);
   
   int			i, value;

   
   /*
   	Empty out the phrases.
   */
   memset(&where_phrase, '\0', sizeof(where_phrase));
   memset(&whereorderby_phrase, '\0', sizeof(whereorderby_phrase));
   inPtr->where_phrase = NULL;
   inPtr->whereorderby_phrase = NULL;
   

   /*
   	Create the where_phrase (if possible).
   */
   if (loc_phrase || pe_phrase || ts_phrase || sw_phrase)
   {
      sprintf(where_phrase, " WHERE ");
   }

   value = 0;
   if (loc_phrase && (value+=8))	/* add 8 to value */
   {
      i = strlen(where_phrase);
      sprintf(&where_phrase[i], " (%s) ", loc_phrase);
      
   }
   if (pe_phrase && (value+=4))		/* add 4 to value */
   {
      if (value > 4)
      {
	 i = strlen(where_phrase);
	 sprintf(&where_phrase[i], " AND");
      }
      
      i = strlen(where_phrase);
      sprintf(&where_phrase[i], " (%s) ", pe_phrase);
      
   }
   if (ts_phrase && (value+=2))		/* add 2 to value */
   {
      if (value > 2)
      {
	 i = strlen(where_phrase);
	 sprintf(&where_phrase[i], " AND");
      }
      
      i = strlen(where_phrase);
      sprintf(&where_phrase[i], " (%s) ", ts_phrase);
      
   }
   if (sw_phrase && (value+=1))		/* add 1 to value */
   {
      if (value > 1)
      {
	 i = strlen(where_phrase);
	 sprintf(&where_phrase[i], " AND");
      }
      
      i = strlen(where_phrase);
      sprintf(&where_phrase[i], " (%s) ", sw_phrase);
      
   }
/*
printf("value == <%i>\n", value);
*/

   /*
   	Create the whereorderby_phrase.
   */
   if (strlen(where_phrase) > 0)
   {
      strcpy(whereorderby_phrase, where_phrase);
      inPtr->where_phrase = &where_phrase[0];
   }

   strcat(whereorderby_phrase, orderby_phrase);   
   inPtr->whereorderby_phrase = &whereorderby_phrase[0];

   
   return;
}


void	ingest_setLocPhrase(char *loc_phrase)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   inPtr->loc_phrase = loc_phrase;
   return;
}


void	ingest_setPePhrase(char *pe_phrase)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   inPtr->pe_phrase = pe_phrase;
   return;
}


void	ingest_setTsPhrase(char *ts_phrase)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   inPtr->ts_phrase = ts_phrase;
   return;
}


void	ingest_setSwPhrase(char *sw_phrase)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   inPtr->sw_phrase = sw_phrase;
   return;
}


void	ingest_obtainLocPhrase(void)
{
   static char		loc_phrase[IN_MAX_LOC_PHRASE];
   char			*text;
   

   text = XmTextGetString(infilter_locTE);
   memset(&loc_phrase, '\0', sizeof(loc_phrase));
   sprintf(loc_phrase, "lid = '%s'", text);

   /*
   	Set the Loc Phrase, free text,
	and build a new where clause.
	Return.
   */
   ingest_setLocPhrase(loc_phrase);
   if (text != NULL)
   {
      XtFree(text);
   }
   
   return;
}


void	ingest_obtainPePhrase(void)
{
   in_info		*inPtr = getIngestFilterInfo(False);
   ShefPe		*shefpePtr = NULL;

   static int		*poslist = NULL;
   int			cnt, i;

   static char		pe_phrase[IN_MAX_PE_PHRASE];
   static char		pe_header[] = "pe in (";
   static char		pe_trailer[] = ")";
   char			newpe[5];
   

   /*
   	Work to maintain inPtr->filterpe_poslist.
   */
   if (inPtr->filterpe_poslist_okayfree)
   {
      if (inPtr->filterpe_poslist != (int *) NULL)
      {
	 ingest_freeposlist(inPtr);	/* (also sets poslist to NULL) */
      }
      
      
      XmListGetSelectedPos(infilter_peLI, &poslist, &cnt);
      
      
      if (cnt > 0)
      {
	 inPtr->filterpe_poslist = poslist;	/* ensures poslist is saved */
	 inPtr->filterpe_poslist_cnt = cnt;
      }
      else
      {
	 poslist = NULL;			/* ensures poslist is NULL */
	 inPtr->filterpe_poslist_cnt = 0;
      }
   }
   else
   {
      cnt = 0;	/* We are not filtering on Pe right now... */
   }
/*
printf("poslist == <%p>, cnt == <%i>\n",
       inPtr->filterpe_poslist, inPtr->filterpe_poslist_cnt);
*/
   
   /*
   	Get all items from the filter_peLI that are to be displayed
	in the mainLI on the left-hand part of the dialog.
   */
   memset(&pe_phrase, '\0', sizeof(pe_phrase));
   strcpy(pe_phrase, pe_header);
   
   if (cnt > 0)
   {
      for(i=0; i<cnt; i++)
      {
	 shefpePtr = (ShefPe *) ListNth(&inPtr->shefpePtr->list, poslist[i]);
	 if (shefpePtr != (ShefPe *) NULL)
	 {
	    memset(&newpe, '\0', sizeof(newpe));
	    if (i == cnt-1)
	    {
	       sprintf(newpe, "'%s'" , shefpePtr->pe);
	    }
	    else
	    {
	       sprintf(newpe, "'%s',", shefpePtr->pe);
	    }
	 }
	 
	 strcat(pe_phrase, newpe);
      }
   }
   strcat(pe_phrase, pe_trailer);
   
   
   /*
   	Set the Pe Phrase (or use NULL).
	Load the display with new data.
	Return.
   */
   if (cnt > 0)
      ingest_setPePhrase(pe_phrase);
   else
      ingest_setPePhrase("pe in (NULL)");

   return;
}


void	ingest_obtainTsPhrase(void)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   static char	ts_phrase[IN_MAX_TS_PHRASE];

   int		menu_pos,
      		menu_nth;

   ShefTs	*menu_shefts;
   
   /*
   	Get the position of the TypeSrc OM,
	and use it to lookup in the static list of typesources
	to get the real ts.
	
	Build the Ts phrase and set it.
   */
   menu_pos = GetMenuPos(infilter_tsOM);
   menu_nth = menu_pos + 1;
   menu_shefts = (ShefTs *) ListNth(&inPtr->sheftsPtr->list, menu_nth);

   memset(&ts_phrase, '\0', sizeof(ts_phrase));
   sprintf(ts_phrase, "ts = '%s'", menu_shefts->ts);

   if (menu_shefts != (ShefTs *) NULL)
   {
      ingest_setTsPhrase(ts_phrase);
   }
   else
      ingest_setTsPhrase("ts in (NULL)");
   
   return;
}


void	ingest_obtainSwPhrase(void)
{
   static char	sw_phrase[IN_MAX_SW_PHRASE];

   char		mast[] = "F";
   char		ofs[] = "F";
   char		stg2[] = "F";
   
   int		i;

   
   /*
	Get the switch values from the filter area.
   */
   if (XmToggleButtonGetState(infilter_mastTB))
   {
      strcpy(mast, "T");
   }
   if (XmToggleButtonGetState(infilter_ofsTB))
   {
      strcpy(ofs, "T");
   }
   if (XmToggleButtonGetState(infilter_stg2TB))
   {
      strcpy(stg2, "T");
   }

   
   /*
	Build the Sw phrase and set it.
   */
   memset(&sw_phrase, '\0', sizeof(sw_phrase));
   i = 0;
   sprintf(&sw_phrase[i], "(ingest = '%s') AND ", mast);

   i = strlen(sw_phrase);
   sprintf(&sw_phrase[i], "(ofs_input = '%s') AND ", ofs);
   
   i = strlen(sw_phrase);
   sprintf(&sw_phrase[i], "(stg2_input = '%s')", stg2);

   ingest_setSwPhrase(sw_phrase);
   
   return;
}


/*
	Driver function that determines how to get the data from the
	Item fields on the display.
*/
int	in_getItemData(in_info *inPtr)
{
   static IngestFilter		ingest;
   int form_element_error = 1;
   
   memset(&ingest, '\0', sizeof(ingest));
   form_element_error = in_getIngestItemData(&ingest);
   if(form_element_error == 0)
   { 
   	inPtr->item_ingestPtr = &ingest;
   }
   else
   {
   	return(1);
   }
   
   return(0);
}


/*
	Obtains Item fields from the display and places data into
	the structure of a passed-in pointer variable (ingestPtr).
*/
int	in_getIngestItemData(IngestFilter *ingestPtr)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   int		menu_pos,
      		menu_nth;

   
   
   ShefDur	*menu_shefdur = NULL;
   ShefTs	*menu_shefts = NULL;
   ShefEx	*menu_shefex = NULL;
   
   char		*buf = NULL;

/*   int          rv = 0;*/
   
   /*
   	Obtain lid.
   */

   if ( (buf = XmTextGetString(initem_locTE)) )
   {
   	/* 
		Checking buf for empty string, user didn't enter Location Name,
		and arises ErrorDialog Window
	*/
	if ( strncmp(buf, "", 1) == 0)
   	{
      		ErrorDialog(ingestDS, "You must enter a Location Name. \n");
      		return(1);
	}
	strcpy(ingestPtr->lid, buf);
      	XtFree(buf);
      	buf = NULL;
   }
   else
   {
      	ErrorDialog(ingestDS, "You must enter a Location Name. \n");
      	return(1);           
 
   }
   
   
   /*
   	Obtain duration.
   */
   menu_pos = GetMenuPos(initem_durOM);
   menu_nth = menu_pos + 1;
   menu_shefdur = (ShefDur *) ListNth(&inPtr->shefdurPtr->list, menu_nth);
   ingestPtr->dur = menu_shefdur->dur;
   
   
   /*
   	Obtain typesource.
   */
   menu_pos = GetMenuPos(initem_tsOM);
   menu_nth = menu_pos + 1;
   menu_shefts = (ShefTs *) ListNth(&inPtr->sheftsPtr->list, menu_nth);
   strcpy(ingestPtr->ts, menu_shefts->ts);
   

   /*
   	Obtain extremum.
   */
   menu_pos = GetMenuPos(initem_extOM);
   menu_nth = menu_pos + 1;
   menu_shefex = (ShefEx *) ListNth(&inPtr->shefexPtr->list, menu_nth);
   strcpy(ingestPtr->extremum, menu_shefex->extremum);

   
   /*
   	Obtain pe.
   */
   /* 
	Checking Physical Element for NULL, user didn't choose,
	and arise ErrorDialog Window
   */

   if (in_getpe_from_peLI(initem_peLI) != NULL)
   {
     (void) strcpy(ingestPtr->pe, in_getpe_from_peLI(initem_peLI));
   }
   else
   {
      ErrorDialog(ingestDS, "You must select a Physical Element. \n");
      return(1);
   }   
   
   /*
   	Obtain ts_rank.
   */
   menu_pos = GetMenuPos(initem_rankOM);
   menu_nth = menu_pos + 1;
   ingestPtr->ts_rank = menu_nth;
   
   
   /*
   	Obtain switches (assume False).
   */
   strcpy(ingestPtr->ingest, "F");
   strcpy(ingestPtr->ofs_input, "F");
   strcpy(ingestPtr->stg2_input, "F");

   if (XmToggleButtonGetState(initem_mastTB))
      strcpy(ingestPtr->ingest, "T");

   if (XmToggleButtonGetState(initem_ofsTB))
      strcpy(ingestPtr->ofs_input, "T");

   if (XmToggleButtonGetState(initem_stg2TB))
      strcpy(ingestPtr->stg2_input, "T");
   
   
   return(0);
}


/*
	Driver function that determines how to populate the
	Item fields on the display.
*/
void	in_setItemData(Widget w, XtPointer ptr, XtPointer cbs)
{
   in_info		*inPtr = getIngestFilterInfo(False);
   IngestFilter		*ingestPtr;
   
   int			*poslist,
      			cnt;
   
   
   /*
   	Use the correct mode (normal entry mode).
   	Use the correct label for the "item" section.

   	Set the item data for the selected position.
   */
   inPtr->data_entry_mode = IN_NORMAL_ENTRY_MODE;
   SetLabel(initemLA, "Selected Item");
   Sensitize(ingest_deletePB);

   XmListGetSelectedPos(inlistLI, &poslist, &cnt);

   if (cnt > 0)
   {
      inPtr->inlistLI_selpos = poslist[0];	/* save selected position */
/*
printf("inPtr->inlistLI_selpos == <%i>\n",
       inPtr->inlistLI_selpos);
*/
      
      ingestPtr = (IngestFilter *) ListNth(&inPtr->ingestPtr->list, poslist[0]);
      if (ingestPtr != (IngestFilter *) NULL)
      {
	 in_setIngestItemData(ingestPtr);
      }
      
      if (poslist != (int *) NULL)
      {
	 free(poslist);
	 poslist = NULL;
      }
   }   
   
   return;
}


/*
	Sets the Item fields on the display,
	using the specified drcPtr.
*/
void	in_setIngestItemData(IngestFilter *ingestPtr)
{
   in_info	*inPtr = getIngestFilterInfo(False);

   
   XmTextSetString(initem_locTE, ingestPtr->lid);
   
   SetMenuHistory(initem_durOM, in_get_shefdur_name(ingestPtr->dur));
   SetMenuHistory(initem_tsOM, in_get_shefts_name(ingestPtr->ts));
   SetMenuHistory(initem_extOM, in_get_shefex_name(ingestPtr->extremum));
   
   in_loadpe_into_peLI(initem_peLI, ingestPtr->pe);
   
   switch (ingestPtr->ts_rank)
   {
      case 1:	SetMenuHistory(initem_rankOM, "1st");	break;
      case 2:	SetMenuHistory(initem_rankOM, "2nd");	break;
      case 3:	SetMenuHistory(initem_rankOM, "3rd");	break;
      case 4:	SetMenuHistory(initem_rankOM, "4th");	break;
      case 5:	SetMenuHistory(initem_rankOM, "5th");	break;
	 
      default:	fprintf(stderr, "ERROR: Incorrect ts_rank specified.\n");
   }   

   /*
   	Set switches (assume off).
   */
   XmToggleButtonSetState(initem_mastTB, False, False);
   XmToggleButtonSetState(initem_ofsTB,  False, False);
   XmToggleButtonSetState(initem_stg2TB, False, False);
   
   if (strcmp(ingestPtr->ingest, "T") == 0)
      XmToggleButtonSetState(initem_mastTB, True, False);
   
   if (strcmp(ingestPtr->ofs_input, "T") == 0)
      XmToggleButtonSetState(initem_ofsTB,  True, False);
   
   if (strcmp(ingestPtr->stg2_input, "T") == 0)
      XmToggleButtonSetState(initem_stg2TB, True, False);
   

   inPtr->item_where_ingestPtr = ingest_build_where(ingestPtr);

/*
printf("inPtr->item_where_ingestPtr == <%s>\n",
       inPtr->item_where_ingestPtr);
*/

   return;
}


void	in_setSwitches_conf(Widget w, XtPointer ptr, XtPointer cbs)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   char		mast[] = "F",
      		ofs[] = "F",
		stg2[] = "F";
   
   /*
   	Save the state of the switches to be used
	if the user confirms the setSwitches action.
   */
   if ( (inPtr->setswitches_master = XmToggleButtonGetState(inlist_masterTB)) )
      strcpy(mast, "T");

   if ( (inPtr->setswitches_ofs = XmToggleButtonGetState(inlist_ofsTB)) )
      strcpy(ofs, "T");
   
   if ( (inPtr->setswitches_stg2 = XmToggleButtonGetState(inlist_stg2TB)) )
      strcpy(stg2, "T");
   
   
   memset(&buf, '\0', sizeof(buf));
   sprintf(buf, "Do you wish to update all displayed entries to\n\n"
	        "Master = %s, OFS = %s, and MPE = %s ?", mast, ofs, stg2);

   qstDS = QuestionDialog(ingestDS, buf);
   SetTitle(qstDS,"Set Switches Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, in_setSwitches, NULL);
   
   
   if (! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   
   return;
}


void	in_setSwitches(Widget w, XtPointer ptr, XtPointer cbs)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   IngestFilter	*ingestPtr = inPtr->ingestPtr;
   char		*ingestPtr_where;
   
   char		mast[] = "F",	/* assigning "F" for memory allocation */
      		ofs[]  = "F",
		stg2[] = "F";
   
   int		error = 1;  /* assume possible error */
   

   /*
   	Init.
   */
   if (inPtr->setswitches_master)
      strcpy(mast, "T");
   if (inPtr->setswitches_ofs)
      strcpy(ofs,  "T");
   if (inPtr->setswitches_stg2)
      strcpy(stg2, "T");

   
   /*
   	For every record in the currently-displayed list,
	update each record to reflect the newly-confirmed switches.
   */
   while (ingestPtr != (IngestFilter *) NULL)
   {
      ingestPtr_where = ingest_build_where(ingestPtr);
/*
printf("attempting update ingest...where == <%s>\n",
       ingestPtr_where);
*/

      strcpy(ingestPtr->ingest,     mast);
      strcpy(ingestPtr->ofs_input,  ofs);
      strcpy(ingestPtr->stg2_input, stg2);
      
            
      error = UpdateIngestFilter(ingestPtr, ingestPtr_where);
      if (error != 0)
      {
	 ErrorDialog(ingestDS, "Could not update 1 or more records.\n");
	 break;
      }
      ingestPtr = (IngestFilter *) ListNext(&ingestPtr->node);
   }
   
   
   /*
   	Determine what data to load.
   */
   in_filter_grabLocPeTsSw();
   ingest_loadDataIntoDisplay();

   return;
}


/*
	Creates a "ingestfilter" where clause for the specified ingestPtr.
*/
char*	ingest_build_where(IngestFilter *ingestPtr)
{
   static char	where[1000];
   int		i;
   

   /*
   	Build the ingestfilter where.
   */
   memset(&where, '\0', sizeof(where));
   
   i = 0;
   sprintf(&where[i], " WHERE (lid = '%s') AND (pe = '%s') AND (dur = %d)",
	   ingestPtr->lid, ingestPtr->pe, ingestPtr->dur);

   i = strlen(where);
   sprintf(&where[i], " AND (ts = '%s') AND (extremum = '%s')",
	   ingestPtr->ts, ingestPtr->extremum);
   
   i = strlen(where);
   sprintf(&where[i], " AND (ts_rank = %i)", ingestPtr->ts_rank);

   i = strlen(where);
   sprintf(&where[i], " AND (ingest = '%s') AND (ofs_input = '%s')",
	   ingestPtr->ingest, ingestPtr->ofs_input);

   i = strlen(where);
   sprintf(&where[i], " AND (stg2_input = '%s') ", ingestPtr->stg2_input);
   
   
   return(where);
}


/*
	Loads all possible "Ingest Filter data" into the display.
	(NOTE: Assumes that inPtr->ingestPtr has already been set.)
*/
void	ingest_loadingest(void)
{
   in_info		*inPtr = getIngestFilterInfo(False);
   
   IngestFilter*	ingestPtr;
   
   char			dur[10],
			buf[MAX_BUF_LEN];
   
   XmStringTable	xmStr;
   
   int			cnt,
      			i;

   int			rcdCount;
   
   int			pos = 1; /* by default, show 1st record in list */
   

   /*
   	Sensitize widgets based on availability of data.
   */
   rcdCount = recordCount(inPtr->table, inPtr->where_phrase);
   if (rcdCount > 0)
   {
      indata_setSensitivity(True);
   }
   else
   {
      indata_setSensitivity(False);
   }
   

   /*
   	Acquire access to the IngestFilter data.
	Loop over all of the appropriate IngestFilter data and
	create XmStrings, and load them into the XmList.
	
	(Validation for inclusion into the XmList is as follows:
	 A data record is considered valid, if and only if
	 it meets the criteria specified by the Filter Parameters.)
   */
   if ((ingestPtr = inPtr->ingestPtr) != (IngestFilter *) NULL)
   {
      cnt = ListCount(&ingestPtr->list);
      xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
      
      if (cnt > 0)
      {
	 i = 0;
	 while (ingestPtr != (IngestFilter *) NULL)
	 {
	    memset(&dur, '\0', sizeof(dur));
	    memset(&buf, '\0', sizeof(buf));
	    
	    sprintf(dur, "%i", ingestPtr->dur);
	    sprintf(buf, INGEST_FMT_STR,
		    ingestPtr->lid, " ", ingestPtr->pe, " ", dur, " ",
		    ingestPtr->ts, " ", ingestPtr->extremum, " ",
		    ingestPtr->ts_rank, " ", ingestPtr->ingest, " ",
		    ingestPtr->ofs_input, " ", ingestPtr->stg2_input);
	    
	    xmStr[i] = XmStringCreateSimple(buf);
	    i++;
	    
	    ingestPtr = (IngestFilter *) ListNext(&ingestPtr->node);
	    
	 }
	 	 
	 XmListAddItems(inlistLI, xmStr, cnt, 1);
/*
printf("i==%i, cnt==%i\n", i, cnt);
*/

	 /*
	 	Select the appropriate position in the rcmainLI.
	 */
	 if (inPtr->delete_flag == True)  /* DELETE just performed */
	 {
	    pos = inPtr->inlistLI_selpos;  /* (use recomputed position) */
	    inPtr->delete_flag = False;
	    XmListSetPos(inlistLI, pos);
	    XmListSelectPos(inlistLI, pos, True);
	 }
	 else if (inPtr->delete_flag == False)  /* either NEW or NORMAL */
	 {
	    if (inPtr->item_ingestPtr)
	    {
	       in_select_new_ingest_item(inPtr);
	    }
	    else
	    {
	       XmListSetPos(inlistLI, pos);
	       XmListSelectPos(inlistLI, pos, True);
	    }
	 }
	 
	 
	 /*
	 	Free memory.
	 */
	 for (i=0; i<cnt; i++)
	 {
	    XmStringFree(xmStr[i]);
	 }
	 XtFree((char *) xmStr);
      }
   }
   
   return;
}


/*
	rc_select_new_ingest_item:
	Selects the "Ingest Filter item" in the inlistLI which was just
	applied to the database via a New-Apply operation.
*/	
void	in_select_new_ingest_item(in_info *inPtr)
{
   IngestFilter		*ingestPtr = inPtr->item_ingestPtr;
   
   char		dur[10],
      		buf[MAX_BUF_LEN];
   
   XmString	item;
   
   int		pos;
   
   
   /*
   	Find the record (as indicated by ingestPtr) in the
   	inlistLI if possible.  If present, select it and
   	make sure that it is visible.
   */
   memset(&dur, '\0', sizeof(dur));
   memset(&buf, '\0', sizeof(buf));
   
   sprintf(dur, "%i", ingestPtr->dur);
   
   sprintf(buf, INGEST_FMT_STR,
	   ingestPtr->lid, " ", ingestPtr->pe, " ", dur, " ",
	   ingestPtr->ts, " ", ingestPtr->extremum, " ",
	   ingestPtr->ts_rank, " ", ingestPtr->ingest, " ",
	   ingestPtr->ofs_input, " ", ingestPtr->stg2_input);

   item = XmStringCreateSimple(buf);

   
   /*
   	Select the position (if present).
   	Free memory.
   */
   if (XmListItemExists(inlistLI, item))
   {
      pos = XmListItemPos(inlistLI, item);
      XmListSetPos(inlistLI, pos);
      XmListSelectPos(inlistLI, pos, True);
   }
   else
   {
      XmListSetPos(inlistLI, 1);
      XmListSelectPos(inlistLI, 1, True);
   }
   XmStringFree(item);
   
   
   return;
}


in_info*	getIngestFilterInfo(int init_flag)
{
   static in_info	*inPtr = NULL;
   
   if (init_flag)
   {
      inPtr = NULL;
   }
   
   if (inPtr == (in_info *) NULL)
   {
      inPtr = (in_info *) malloc (sizeof(in_info));
      memset(&(*inPtr), '\0', sizeof(in_info));
      
      inPtr->ingestPtr = (IngestFilter *) NULL;
      inPtr->item_ingestPtr = (IngestFilter *) NULL;
      inPtr->item_where_ingestPtr = (char *) NULL;
      inPtr->shefpePtr = (ShefPe *) NULL;
      inPtr->filterpe_poslist = (int *) NULL;
      inPtr->filterpe_poslist_okayfree = False;
      
      inPtr->switches_master = False;
      inPtr->switches_ofs = False;
      inPtr->switches_stg2 = False;
      
      inPtr->setswitches_master = False;
      inPtr->setswitches_ofs = False;
      inPtr->setswitches_stg2 = False;
      
      inPtr->apply_successful = False;
      inPtr->data_entry_mode = IN_NORMAL_ENTRY_MODE;
      inPtr->delete_flag = False;
   }
   
   return(inPtr);
}


IngestFilter*	ingest_getingest(void)
{
   in_info		*inPtr = getIngestFilterInfo(False);

   IngestFilter		*ingest=NULL,
      			*ingestPtr=NULL;

   /*
   	Free old memory.
	Retrieve new record(s) from the dbms.

	If there is at least 1 record in the dbms, return ingestPtr.
	Otherwise, return NULL.
   */
   ingest_freeingest(inPtr);

   ingest = GetIngestFilter(inPtr->whereorderby_phrase);
   if (ingest != (IngestFilter *) NULL)
   {
      ingestPtr = (IngestFilter *) ListFirst(&ingest->list);
      return(ingestPtr);
   }
      
   return(NULL);
}


ShefPe*		ingest_getshefpes(void)
{
   in_info		*inPtr = getIngestFilterInfo(False);
   
   static ShefPe	*shefpe = NULL,
      			*shefpePtr = NULL;

   /*
   	Free old memory.
   	Retrieve new record(s) from the dbms.

	If there is at least 1 record in the dbms, return shefpePtr.
	Otherwise, return NULL.
   */
   ingest_freeshefpes(inPtr);
   shefpe = GetShefPe(" ORDER BY pe ");
   if (shefpe != (ShefPe *) NULL)
   {
      shefpePtr = (ShefPe *) ListFirst(&shefpe->list);
      return(shefpePtr);
   }
   
   return(NULL);
}


void		freeIngestFilterInfo(void)
{
   in_info	*inPtr = getIngestFilterInfo(False);
   
   if (inPtr != (in_info *) NULL)
   {
      ingest_freeingest (inPtr);
      ingest_freeshefdur(inPtr);
      ingest_freeshefts (inPtr);
      ingest_freeshefex (inPtr);

      ingest_freeshefpes(inPtr);
      ingest_freeposlist(inPtr);  /* frees filterpe_poslist */
   
      free(inPtr);
      inPtr = NULL;
   }
   
   return;
}


void	ingest_freeingest(in_info *inPtr)
{
   if (inPtr->ingestPtr != (IngestFilter *) NULL)
   {
      FreeIngestFilter(inPtr->ingestPtr);
      inPtr->ingestPtr = NULL;
   }
   
   return;
}


void	ingest_freeshefdur(in_info *inPtr)
{
   if (inPtr->shefdurPtr != (ShefDur *) NULL)
   {
      FreeShefDur(inPtr->shefdurPtr);
      inPtr->shefdurPtr = NULL;
   }
   
   return;
}


void	ingest_freeshefpes(in_info *inPtr)
{
   if (inPtr->shefpePtr != (ShefPe *) NULL)
   {
      FreeShefPe(inPtr->shefpePtr);
      inPtr->shefpePtr = NULL;
   }
   
   return;
}


void	ingest_freeshefts(in_info *inPtr)
{
   if (inPtr->sheftsPtr != (ShefTs *) NULL)
   {
      FreeShefTs(inPtr->sheftsPtr);
      inPtr->sheftsPtr = NULL;
   }
   
   return;
}


void	ingest_freeshefex(in_info *inPtr)
{
   if (inPtr->shefexPtr != (ShefEx *) NULL)
   {
      FreeShefEx(inPtr->shefexPtr);
      inPtr->shefexPtr = NULL;
   }
   
   return;
}


void	ingest_freeposlist(in_info *inPtr)  /* free the filterpe_poslist */
{
   if (inPtr->filterpe_poslist != (int *) NULL)
   {
      free(inPtr->filterpe_poslist);
      inPtr->filterpe_poslist = NULL;
   }
   
   return;
}



