/*
	File:		adjustfactor_show.c
	Date:		October 2002
	Author:		Russell Erb

	Purpose:	Provide support for the Adjust Factor DS.
	
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
#include "AdjustFactor.h"
#include "GeneralUtil.h"


/*
	Local includes.
*/
#include "hybase.h"
#include "hbAS.h"
#include "user_prefs.h"
#include "adjustfactor.h"
#include "adjustfactor_show.h"
#include "loc_cbs.h"



void	ShowAdjustDs(Widget w)
{
   adj_info	*adjPtr = getAdjustFactorInfo(True);
   
   if (! adjustDS)
   {
      create_adjustDS(GetTopShell(w));
      af_create_shefdur_btns(adjPtr);
      af_create_shefts_btns(adjPtr);
      af_create_shefex_btns(adjPtr);
      adjust_callbacks();
   }
   
   
   if (! XtIsManaged(adjustDS))
   {
      adjPtr->shefpePtr = adjust_getshefpes();
      adjust_loadpeLI(afitem_peLI, adjPtr->shefpePtr);
      
      adjust_loaddisplay(NULL, NULL, NULL);
      
      XtManageChild(adjustFO);
      XtManageChild(adjustDS);
   }
   
   return;
}


/*
	Driver function for main display which sets up default values
	for the display and determines what data is to be loaded using
	the "filter parameters".
	
	The display is then loaded with data.
*/
void	adjust_loaddisplay(Widget w, XtPointer ptr, XtPointer cbs)
{
   adj_info	*adjPtr = getAdjustFactorInfo(False);
   
   
   strcpy(adjPtr->table, "adjustfactor");
   strcpy(adjPtr->orderby_phrase, " ORDER BY lid, pe, dur, ts, extremum ");
   
   
   /*
   	Determine what data to load.
   */
   adjust_loadDataIntoDisplay();
   
   return;
}


/*
	Controls the loading of data into the display.
*/
void	adjust_loadDataIntoDisplay(void)
{
   adj_info	*adjPtr = getAdjustFactorInfo(False);


   /*
   	Load data into the display.
   */
   adjust_clearDisplay();
   adjPtr->item_where_adjustPtr = NULL;	/* No item selected in aflistLI */
   adjPtr->adjustPtr = adjust_getadjust();
   adjust_loadadjust();
   
   return;
}


/*
	Returns the correct PB-name by specifying an integer duration.
*/
char*	af_get_shefdur_name(int dur)
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
      strcpy(name, af_create_shefdur_name(shefdurPtr));
      FreeShefDur(shefdurPtr);
      return(&name[0]);
   }
   else
      return("");
}


/*
	Creates a PB-name for a given shefdur.
*/
char*	af_create_shefdur_name(ShefDur *shefdurPtr)
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
void	af_create_shefdur_btns(adj_info *adjPtr)
{
   static ShefDur	*shefdur = NULL,
			*shefdurPtr = NULL;	/* for storage */

   char			wname[BUFSIZ];
   
   ShefDur		*sdurPtr = NULL;	/* for loop control only */
   Widget		sdurPB;


   /*
   	Free old memory.
   	Retrieve new record(s) from the dbms.

	If there is at least 1 record in the dbms, set adjPtr->shefdurPtr.
	Otherwise, set adjPtr->shefdurPtr to NULL.
   */
   adjust_freeshefdur(adjPtr);
   shefdur = GetShefDur(" ORDER BY dur ");
   if (shefdur != (ShefDur *) NULL)
   {
      shefdurPtr = (ShefDur *) ListFirst(&shefdur->list);
      adjPtr->shefdurPtr = shefdurPtr;  /* keep the list of records around */


      /* LOOPING */
      sdurPtr = shefdurPtr;
      while(sdurPtr != (ShefDur *) NULL)
      {
	 strcpy(wname, af_create_shefdur_name(sdurPtr));
	 
	 sdurPB = XtVaCreateManagedWidget(wname,
					  xmPushButtonWidgetClass,
					  afitem_durPDM,
					  NULL);
	 
	 sdurPtr = (ShefDur *) ListNext(&sdurPtr->node);
      }
   }
   else
   {
      adjPtr->shefdurPtr = NULL;  /* no records in dbms */
   }
   
   return;
}


/*
	Returns the correct PB-name by specifying an typesource.
*/
char*	af_get_shefts_name(char *ts)
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
      strcpy(name, af_create_shefts_name(sheftsPtr));
      FreeShefTs(sheftsPtr);
      return(&name[0]);
   }
   else
      return("");
}


/*
	Creates a PB-name for a given shefts.
*/
char*	af_create_shefts_name(ShefTs *sheftsPtr)
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
void	af_create_shefts_btns(adj_info *adjPtr)
{
   static ShefTs	*shefts = NULL,
			*sheftsPtr = NULL;	/* for storage */

   char			wname[BUFSIZ];
   
   ShefTs		*stsPtr = NULL;	/* for loop control only */
   Widget		stsPB;


   /*
   	Free old memory.
   	Retrieve new record(s) from the dbms.

	If there is at least 1 record in the dbms, set adjPtr->sheftsPtr.
	Otherwise, set adjPtr->sheftsPtr to NULL.
   */
   adjust_freeshefts(adjPtr);
   shefts = GetShefTs(" ORDER BY ts ");
   if (shefts != (ShefTs *) NULL)
   {
      sheftsPtr = (ShefTs *) ListFirst(&shefts->list);
      adjPtr->sheftsPtr = sheftsPtr;  /* keep the list of records around */


      /* LOOPING */
      stsPtr = sheftsPtr;
      while(stsPtr != (ShefTs *) NULL)
      {
	 strcpy(wname, af_create_shefts_name(stsPtr));
	 
	 stsPB = XtVaCreateManagedWidget(wname,
					 xmPushButtonWidgetClass,
					 afitem_tsPDM,
					 NULL);
	 
	 stsPtr = (ShefTs *) ListNext(&stsPtr->node);
      }
   }
   else
   {
      adjPtr->sheftsPtr = NULL;  /* no records in dbms */
   }
   
   return;
}


/*
	Returns the correct PB-name by specifying an extremum.
*/
char*	af_get_shefex_name(char *ex)
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
      strcpy(name, af_create_shefex_name(shefexPtr));
      FreeShefEx(shefexPtr);
      return(&name[0]);
   }
   else
      return("");
}


/*
	Creates a PB-name for a given shefex.
*/
char*	af_create_shefex_name(ShefEx *shefexPtr)
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
void	af_create_shefex_btns(adj_info *adjPtr)
{
   static ShefEx	*shefex = NULL,
			*shefexPtr = NULL;	/* for storage */

   char			wname[BUFSIZ];
   
   ShefEx		*exPtr = NULL;	/* for loop control only */
   Widget		exPB;


   /*
   	Free old memory.
   	Retrieve new record(s) from the dbms.

	If there is at least 1 record in the dbms, set adjPtr->shefexPtr.
	Otherwise, set adjPtr->shefexPtr to NULL.
   */
   adjust_freeshefex(adjPtr);
   shefex = GetShefEx(" ORDER BY extremum ");
   if (shefex != (ShefEx *) NULL)
   {
      shefexPtr = (ShefEx *) ListFirst(&shefex->list);
      adjPtr->shefexPtr = shefexPtr;  /* keep the list of records around */


      /* LOOPING */
      exPtr = shefexPtr;
      while(exPtr != (ShefEx *) NULL)
      {
	 strcpy(wname, af_create_shefex_name(exPtr));
	 
	 exPB = XtVaCreateManagedWidget(wname,
					xmPushButtonWidgetClass,
					afitem_extPDM,
					NULL);
	 
	 exPtr = (ShefEx *) ListNext(&exPtr->node);
      }
   }
   else
   {
      adjPtr->shefexPtr = NULL;  /* no records in dbms */
   }
   
   return;
}


/*
	This function returns the correct PB-name given a Physical Element.
*/
char*	af_get_shefpe_name(char *pe)
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
char*	af_getpe_from_peLI(Widget peLI)
{
   adj_info	*adjPtr = getAdjustFactorInfo(False);
   
   ShefPe	*shefpePtr = NULL;
   
   int		*poslist,
      		cnt,
		pe_nth;

   
   XmListGetSelectedPos(peLI, &poslist, &cnt);
   
   if (cnt > 0)
   {
      pe_nth = poslist[0];
      shefpePtr = (ShefPe *) ListNth(&adjPtr->shefpePtr->list, pe_nth);
      
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
void	af_loadpe_into_peLI(Widget peLI, char *pe)
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
   strcat(buf, af_get_shefpe_name(pe));
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
void	adjust_loadpeLI(Widget peLI, ShefPe *shefpePtr)
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


void	in_recompute_aflistLI_selpos(adj_info *adjPtr)
{
   int	selpos = adjPtr->aflistLI_selpos;
   int	item_count;

   /*
   	Get the item_count.
	Subtract one from selpos if at end of list, otherwise--no change.
   */
   XtVaGetValues(aflistLI, XmNitemCount, &item_count, NULL);
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
   adjPtr->aflistLI_selpos = selpos;
   adjPtr->delete_flag = True;  /* yes, a delete operation was just performed */
   
   return;
}


void	adjust_callbacks(void)
{	
   Atom		atom;
  
   /*
   	Window manager callbacks.
   */
   atom = XmInternAtom(XtDisplay(adjustDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(adjustDS, atom, adjust_close, NULL);
   
   
   /*
   	Widget callbacks.
   */
   XtAddCallback(adjust_applyPB, XmNactivateCallback, adjust_apply,   NULL);
   XtAddCallback(adjust_cancelPB,XmNactivateCallback, adjust_close,   NULL);
   XtAddCallback(adjust_deletePB,XmNactivateCallback, adjust_del_conf,NULL);
   
   XtAddCallback(aflistLI, XmNbrowseSelectionCallback, af_setItemData, NULL);
   
   XtAddCallback(afitem_locTE, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
   XtAddCallback(afdivisorTE,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(afbaseTE,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(afmultiplierTE,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(afadderTE,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   
   return;
}


/*
	Driver function that determines how to apply data to database.
	
	The helper functions below provide the tools necessary
	to apply data to the adjustfactor table.
*/
void	adjust_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
   adj_info	*adjPtr = getAdjustFactorInfo(False);
   int		error = 1;
   
   /*
   	Obtain data from the afitemFO.
	
	Apply changes to the dbms, if possible.
	  Be sure to indicate whether or not the apply was successful.
   */
   af_getItemData(adjPtr);
   error = in_apply_adjustItemData(adjPtr); /* return error, or 0 (no error) */
   
   /*
   	If there was no error in appling the record to the dbms...
	
   	  Ensure: New entry mode -> normal entry mode,
	  Load data into the display, and
	  Indicate that the apply was successful.
   */
   if (error == 0)
   {
	clearForm(afitemFO);
	if (adjPtr->aflistLI_selpos > 0)
	{
	    XmListSelectPos(aflistLI, adjPtr->aflistLI_selpos, True);
	    XmListSetPos(aflistLI, adjPtr->aflistLI_selpos);
	}
	SetLabel(afitemLA, "Selected Item");
      
	adjust_loadDataIntoDisplay();
	adjPtr->apply_successful = True;  /* success */
	return;
   }
   else
   {
      ErrorDialog(adjustDS, DbErrorString(error));
   }

   
   adjPtr->apply_successful = False;  /* failure */
   
   return;
}


int	in_apply_adjustItemData(adj_info *adjPtr)
{
   AdjustFactor		*adjustPtr = adjPtr->item_adjustPtr;
   char			*adjustPtr_where = adjPtr->item_where_adjustPtr;
   
   int			error;
   
   /*
   	Perform Update/Insert operation, as appropriate.
   */
   error = PutAdjustFactor(adjustPtr);
   if (error != 0)
   {
      error = UpdateAdjustFactor(adjustPtr, adjustPtr_where);
      if (error != 0)
      {
	 return(error);
      }
   }
   
   return(0);  /* 0 means the "adjustItemData" apply was successful */
}


void	adjust_close(Widget w, XtPointer ptr, XtPointer cbs)
{

   freeAdjustFactorInfo();
      
   if (XtIsManaged(adjustDS))
   {
      XtDestroyWidget(adjustDS);
      adjustDS = NULL;
   }
      
   return;
}


void	adjust_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   memset(&buf, '\0', sizeof(buf));
   sprintf(buf, "Do you wish to delete this entry?");
   qstDS = QuestionDialog(adjustDS, buf);
   SetTitle(qstDS,"Delete Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, adjust_delete, NULL);
   
   
   if (! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


/*
	Delete the selected item from the database,
	and reload data into the display.
*/
void	adjust_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
   adj_info	*adjPtr = getAdjustFactorInfo(False);
   
   int		*poslist, cnt, nth_pos;
   
   AdjustFactor	*adjustPtr = NULL;
   
   char		where[1000];
   
   int		error = 0;

   
   SetCursor(adjustFO, XC_watch);

   
   XmListGetSelectedPos(aflistLI, &poslist, &cnt);
   if (cnt > 0)
   {
      nth_pos = poslist[0];
      adjustPtr = (AdjustFactor *) ListNth(&adjPtr->adjustPtr->list, nth_pos);
      
      memset(&where, '\0', sizeof(where));
      strcpy(where, adjust_build_where(adjustPtr));
      
      error = DeleteAdjustFactor((const char *) where);

      if (!error)
      {
	 in_recompute_aflistLI_selpos(adjPtr);
      }
      else
      {
	 UnsetCursor(adjustFO);
   	 ErrorDialog(adjustDS, DbErrorString(error));
	 return;
      }
   }
   
   
   /*
   	Always filter on Loc & Pe.
	Load the display with data.
   */
   adjust_loadDataIntoDisplay();

   
   UnsetCursor(adjustFO);
   
   
   return;
}


void	adjust_clearDisplay(void)
{
   /*
   	Clears out certain widgets.
   */
   XmListDeleteAllItems(aflistLI);
   XmListDeselectAllItems(afitem_peLI);
   clearForm(afitemFO);
   
   return;
}


/*
	Driver function that determines how to get the data from the
	Item fields on the display.
*/
void	af_getItemData(adj_info *adjPtr)
{
   static AdjustFactor		adjust;
   
   
   memset(&adjust, '\0', sizeof(adjust));
   af_getAdjustItemData(&adjust);
   adjPtr->item_adjustPtr = &adjust;
   
   
   return;
}


/*
	Obtains Item fields from the display and places data into
	the structure of a passed-in pointer variable (adjustPtr).
*/
void	af_getAdjustItemData(AdjustFactor *adjustPtr)
{
   adj_info	*adjPtr = getAdjustFactorInfo(False);
   
   int		menu_pos,
      		menu_nth;
   
   ShefDur	*menu_shefdur = NULL;
   ShefTs	*menu_shefts = NULL;
   ShefEx	*menu_shefex = NULL;
   
   char		*buf = NULL;
   
   /*
   	Obtain lid.
   */
   if ( (buf = XmTextGetString(afitem_locTE)))
   {
      strcpy(adjustPtr->lid, buf);
      XtFree(buf);
      buf = NULL;  
   }
   
   
   /*
   	Obtain Divisor.
   */
   if ( (buf = XmTextGetString(afdivisorTE)))
   {
      if (!IsBlankStr(buf))
	adjustPtr->divisor = atof(buf);
      else
	adjustPtr->divisor = 1.0;
      
      XtFree(buf);
      buf = NULL;
      
      if (adjustPtr->divisor == 0.0)
	adjustPtr->divisor = 1.0;
   }
   else
      adjustPtr->divisor = 1.0;


   /*
   	Obtain Base.
   */
   if ( (buf = XmTextGetString(afbaseTE)))
   {
      if (!IsBlankStr(buf))
	adjustPtr->base = atof(buf);
      else
	adjustPtr->base = 0.0;
      XtFree(buf);
      buf = NULL;  
   }
   else
      adjustPtr->base = 0.0;


   /*
   	Obtain Multiplier.
   */
   if ( (buf = XmTextGetString(afmultiplierTE)))
   {
      if (!IsBlankStr(buf))
	adjustPtr->multiplier = atof(buf);
      else
	adjustPtr->multiplier = 1.0;
      XtFree(buf);
      buf = NULL;  
   }
   else
      adjustPtr->multiplier = 1.0;


   /*
   	Obtain Adder.
   */
   if ( (buf = XmTextGetString(afadderTE)))
   {
      if (!IsBlankStr(buf))
	adjustPtr->adder = atof(buf);
      else
	adjustPtr->adder = 0.0;
      XtFree(buf);
      buf = NULL;  
   }
   else
      adjustPtr->adder = 0.0;


   /*
   	Obtain duration.
   */
   menu_pos = GetMenuPos(afitem_durOM);
   menu_nth = menu_pos + 1;
   menu_shefdur = (ShefDur *) ListNth(&adjPtr->shefdurPtr->list, menu_nth);
   adjustPtr->dur = menu_shefdur->dur;
   
   
   /*
   	Obtain typesource.
   */
   menu_pos = GetMenuPos(afitem_tsOM);
   menu_nth = menu_pos + 1;
   menu_shefts = (ShefTs *) ListNth(&adjPtr->sheftsPtr->list, menu_nth);
   strcpy(adjustPtr->ts, menu_shefts->ts);
   

   /*
   	Obtain extremum.
   */
   menu_pos = GetMenuPos(afitem_extOM);
   menu_nth = menu_pos + 1;
   menu_shefex = (ShefEx *) ListNth(&adjPtr->shefexPtr->list, menu_nth);
   strcpy(adjustPtr->extremum, menu_shefex->extremum);

   
   /*
   	Obtain pe.
   */
   if (af_getpe_from_peLI(afitem_peLI) != NULL)
   {
     (void) strcpy(adjustPtr->pe, af_getpe_from_peLI(afitem_peLI));
   }
   else
   {
      ErrorDialog(adjustDS, "You must select a Physical Element. \n");
      return;
   }   
   
   return;
}


/*
	Driver function that determines how to populate the
	Item fields on the display.
*/
void	af_setItemData(Widget w, XtPointer ptr, XtPointer cbs)
{
   adj_info		*adjPtr = getAdjustFactorInfo(False);
   AdjustFactor		*adjustPtr;
   
   int			*poslist,
      			cnt;
   
   
   /*
   	Use the correct mode (normal entry mode).
   	Use the correct label for the "item" section.

   	Set the item data for the selected position.
   */
   SetLabel(afitemLA, "Selected Item");
   Sensitize(adjust_deletePB);

   XmListGetSelectedPos(aflistLI, &poslist, &cnt);

   if (cnt > 0)
   {
      adjPtr->aflistLI_selpos = poslist[0];	/* save selected position */
      
      adjustPtr = (AdjustFactor *) ListNth(&adjPtr->adjustPtr->list, poslist[0]);
      if (adjustPtr != (AdjustFactor *) NULL)
      {
	 in_setAdjustItemData(adjustPtr);
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
void	in_setAdjustItemData(AdjustFactor *adjustPtr)
{
   char		divisor[15],base[15],multiplier[15],adder[15];

   adj_info	*adjPtr = getAdjustFactorInfo(False);

   
   XmTextSetString(afitem_locTE, adjustPtr->lid);
   
   SetMenuHistory(afitem_durOM, af_get_shefdur_name(adjustPtr->dur));
   SetMenuHistory(afitem_tsOM, af_get_shefts_name(adjustPtr->ts));
   SetMenuHistory(afitem_extOM, af_get_shefex_name(adjustPtr->extremum));
   
   af_loadpe_into_peLI(afitem_peLI, adjustPtr->pe);
   
   memset(&divisor, '\0', sizeof(divisor));
   if (! IsNull(DOUBLE, (void*) &adjustPtr->divisor))
	sprintf(divisor, "%.3f", adjustPtr->divisor);
   else
	sprintf(divisor, "%.3f", 1.0);
   XmTextSetString(afdivisorTE, divisor);

   memset(&base, '\0', sizeof(base));
   if (! IsNull(DOUBLE, (void*) &adjustPtr->base))
	sprintf(base, "%.3f", adjustPtr->base);
   else
	sprintf(base, "%.3f", 0.0);
   XmTextSetString(afbaseTE, base);

   memset(&multiplier, '\0', sizeof(multiplier));
   if (! IsNull(DOUBLE, (void*) &adjustPtr->multiplier))
	sprintf(multiplier, "%.3f", adjustPtr->multiplier);
   else
	sprintf(multiplier, "%.3f", 1.0);
   XmTextSetString(afmultiplierTE, multiplier);

   memset(&adder, '\0', sizeof(adder));
   if (! IsNull(DOUBLE, (void*) &adjustPtr->adder))
	sprintf(adder, "%.3f", adjustPtr->adder);
   else
	sprintf(adder, "%.3f", 0.0);
   XmTextSetString(afadderTE, adder);
   
   adjPtr->item_where_adjustPtr = adjust_build_where(adjustPtr);


   return;
}


void	af_setSwitches(Widget w, XtPointer ptr, XtPointer cbs)
{
   adj_info	*adjPtr = getAdjustFactorInfo(False);
   
   AdjustFactor	*adjustPtr = adjPtr->adjustPtr;
   char		*adjustPtr_where;
   
   int		error = 1;  /* assume possible error */
   
   
   /*
   	For every record in the currently-displayed list,
	update each record to reflect the newly-confirmed switches.
   */
   while (adjustPtr != (AdjustFactor *) NULL)
   {
      adjustPtr_where = adjust_build_where(adjustPtr);

      error = UpdateAdjustFactor(adjustPtr, adjustPtr_where);
      if (error != 0)
      {
	 ErrorDialog(adjustDS, "Could not update 1 or more records.\n");
	 break;
      }
      adjustPtr = (AdjustFactor *) ListNext(&adjustPtr->node);
   }
   
   
   /*
   	Determine what data to load.
   */
   adjust_loadDataIntoDisplay();

   return;
}


/*
	Creates a "adjustfactor" where clause for the specified adjustPtr.
*/
char*	adjust_build_where(AdjustFactor *adjustPtr)
{
   static char	where[1000];
   int		i;
   

   /*
   	Build the adjustfactor where.
   */
   memset(&where, '\0', sizeof(where));
   
   i = 0;
   sprintf(&where[i], " WHERE (lid = '%s') AND (pe = '%s') AND (dur = %d)",
	   adjustPtr->lid, adjustPtr->pe, adjustPtr->dur);

   i = strlen(where);
   sprintf(&where[i], " AND (ts = '%s') AND (extremum = '%s')",
	   adjustPtr->ts, adjustPtr->extremum);
   
   return(where);
}


/*
	Loads all possible "Adjust Factor data" into the display.
	(NOTE: Assumes that adjPtr->adjustPtr has already been set.)
*/
void	adjust_loadadjust(void)
{
   adj_info		*adjPtr = getAdjustFactorInfo(False);
   
   AdjustFactor*	adjustPtr;
   
   char			dur[10],divisor[15],base[15],multiplier[15],adder[15],
			buf[MAX_BUF_LEN];
   
   XmStringTable	xmStr;
   
   int			cnt,
      			i;

   int			pos = 1; /* by default, show 1st record in list */
   

   /*
   	Acquire access to the AdjustFactor data.
	Loop over all of the appropriate AdjustFactor data and
	create XmStrings, and load them into the XmList.
	
	(Validation for inclusion into the XmList is as follows:
	 A data record is considered valid, if and only if
	 it meets the criteria specified by the Factor Parameters.)
   */
   if ((adjustPtr = adjPtr->adjustPtr) != (AdjustFactor *) NULL)
   {
      cnt = ListCount(&adjustPtr->list);
      xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
      
      if (cnt > 0)
      {
	 i = 0;
	 while (adjustPtr != (AdjustFactor *) NULL)
	 {
	    memset(&buf, '\0', sizeof(buf));
	    
	    memset(&dur, '\0', sizeof(dur));
	    sprintf(dur, "%i", adjustPtr->dur);

	    memset(&divisor, '\0', sizeof(divisor));
	    if (! IsNull(DOUBLE, (void*) &adjustPtr->divisor))
		sprintf(divisor, "%.3f", adjustPtr->divisor);
	    else
		sprintf(divisor, "%.3f", 1.0);

	    memset(&base, '\0', sizeof(base));
	    if (! IsNull(DOUBLE, (void*) &adjustPtr->base))
		sprintf(base, "%.3f", adjustPtr->base);
	    else
		sprintf(base, "%.3f", 0.0);

	    memset(&multiplier, '\0', sizeof(multiplier));
	    if (! IsNull(DOUBLE, (void*) &adjustPtr->multiplier))
		sprintf(multiplier, "%.3f", adjustPtr->multiplier);
	    else
		sprintf(multiplier, "%.3f", 1.0);

	    memset(&adder, '\0', sizeof(adder));
	    if (! IsNull(DOUBLE, (void*) &adjustPtr->adder))
		sprintf(adder, "%.3f", adjustPtr->adder);
	    else
		sprintf(adder, "%.3f", 0.0);

	    sprintf(buf, "%-8s %2s %4s  %2s %2s   %11s %11s %11s %11s",
		    adjustPtr->lid, adjustPtr->pe, dur, adjustPtr->ts,
		    adjustPtr->extremum, divisor, base, multiplier, adder);
	    
	    xmStr[i] = XmStringCreateSimple(buf);
	    i++;
	    
	    adjustPtr = (AdjustFactor *) ListNext(&adjustPtr->node);
	    
	 }
	 	 
	 XmListAddItems(aflistLI, xmStr, cnt, 1);

	 /*
	 	Select the appropriate position in the rcmainLI.
	 */
	 if (adjPtr->delete_flag == True)  /* DELETE just performed */
	 {
	    pos = adjPtr->aflistLI_selpos;  /* (use recomputed position) */
	    adjPtr->delete_flag = False;
	    XmListSetPos(aflistLI, pos);
	    XmListSelectPos(aflistLI, pos, True);
	 }
	 else if (adjPtr->delete_flag == False)  /* either NEW or NORMAL */
	 {
	    if (adjPtr->item_adjustPtr)
	    {
	       in_select_new_adjust_item(adjPtr);
	    }
	    else
	    {
	       XmListSetPos(aflistLI, pos);
	       XmListSelectPos(aflistLI, pos, True);
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
	rc_select_new_adjust_item:
	Selects the "Adjust Factor item" in the aflistLI which was just
	applied to the database via a New-Apply operation.
*/	
void	in_select_new_adjust_item(adj_info *adjPtr)
{
   AdjustFactor		*adjustPtr = adjPtr->item_adjustPtr;
   
   char		dur[10],
      		buf[MAX_BUF_LEN];
   
   XmString	item;
   
   int		pos;
   
   
   /*
   	Find the record (as indicated by adjustPtr) in the
   	aflistLI if possible.  If present, select it and
   	make sure that it is visible.
   */
   memset(&dur, '\0', sizeof(dur));
   memset(&buf, '\0', sizeof(buf));
   
   sprintf(dur, "%i", adjustPtr->dur);
   
   sprintf(buf, "%-5s %3s %-s %2s %4s %4s %s %4s %s",
	   adjustPtr->lid, " ", adjustPtr->pe, " ", dur, " ",
	   adjustPtr->ts, " ", adjustPtr->extremum);

   item = XmStringCreateSimple(buf);

   
   /*
   	Select the position (if present).
   	Free memory.
   */
   if (XmListItemExists(aflistLI, item))
   {
      pos = XmListItemPos(aflistLI, item);
      XmListSetPos(aflistLI, pos);
      XmListSelectPos(aflistLI, pos, True);
   }
   else
   {
      XmListSetPos(aflistLI, 1);
      XmListSelectPos(aflistLI, 1, True);
   }
   XmStringFree(item);
   
   
   return;
}


adj_info*	getAdjustFactorInfo(int init_flag)
{
   static adj_info	*adjPtr = NULL;
   
   if (init_flag)
   {
      adjPtr = NULL;
   }
   
   if (adjPtr == (adj_info *) NULL)
   {
      adjPtr = (adj_info *) malloc (sizeof(adj_info));
      memset(&(*adjPtr), '\0', sizeof(adj_info));
      
      adjPtr->adjustPtr = (AdjustFactor *) NULL;
      adjPtr->item_adjustPtr = (AdjustFactor *) NULL;
      adjPtr->item_where_adjustPtr = (char *) NULL;
      adjPtr->shefpePtr = (ShefPe *) NULL;
      
      adjPtr->apply_successful = False;
      adjPtr->delete_flag = False;
   }
   
   return(adjPtr);
}


AdjustFactor*	adjust_getadjust(void)
{
   adj_info		*adjPtr = getAdjustFactorInfo(False);

   AdjustFactor		*adjust=NULL,
      			*adjustPtr=NULL;

   /*
   	Free old memory.
	Retrieve new record(s) from the dbms.

	If there is at least 1 record in the dbms, return adjustPtr.
	Otherwise, return NULL.
   */
   adjust_freeadjust(adjPtr);

   adjust = GetAdjustFactor(adjPtr->orderby_phrase);
   if (adjust != (AdjustFactor *) NULL)
   {
      adjustPtr = (AdjustFactor *) ListFirst(&adjust->list);
      return(adjustPtr);
   }
      
   return(NULL);
}


ShefPe*		adjust_getshefpes(void)
{
   adj_info		*adjPtr = getAdjustFactorInfo(False);
   
   static ShefPe	*shefpe = NULL,
      			*shefpePtr = NULL;

   /*
   	Free old memory.
   	Retrieve new record(s) from the dbms.

	If there is at least 1 record in the dbms, return shefpePtr.
	Otherwise, return NULL.
   */
   adjust_freeshefpes(adjPtr);
   shefpe = GetShefPe(" ORDER BY pe ");
   if (shefpe != (ShefPe *) NULL)
   {
      shefpePtr = (ShefPe *) ListFirst(&shefpe->list);
      return(shefpePtr);
   }
   
   return(NULL);
}


void		freeAdjustFactorInfo(void)
{
   adj_info	*adjPtr = getAdjustFactorInfo(False);
   
   if (adjPtr != (adj_info *) NULL)
   {
      adjust_freeadjust (adjPtr);
      adjust_freeshefdur(adjPtr);
      adjust_freeshefts (adjPtr);
      adjust_freeshefex (adjPtr);

      adjust_freeshefpes(adjPtr);
   
      free(adjPtr);
      adjPtr = NULL;
   }
   
   return;
}


void	adjust_freeadjust(adj_info *adjPtr)
{
   if (adjPtr->adjustPtr != (AdjustFactor *) NULL)
   {
      FreeAdjustFactor(adjPtr->adjustPtr);
      adjPtr->adjustPtr = NULL;
   }
   
   return;
}


void	adjust_freeshefdur(adj_info *adjPtr)
{
   if (adjPtr->shefdurPtr != (ShefDur *) NULL)
   {
      FreeShefDur(adjPtr->shefdurPtr);
      adjPtr->shefdurPtr = NULL;
   }
   
   return;
}


void	adjust_freeshefpes(adj_info *adjPtr)
{
   if (adjPtr->shefpePtr != (ShefPe *) NULL)
   {
      FreeShefPe(adjPtr->shefpePtr);
      adjPtr->shefpePtr = NULL;
   }
   
   return;
}


void	adjust_freeshefts(adj_info *adjPtr)
{
   if (adjPtr->sheftsPtr != (ShefTs *) NULL)
   {
      FreeShefTs(adjPtr->sheftsPtr);
      adjPtr->sheftsPtr = NULL;
   }
   
   return;
}


void	adjust_freeshefex(adj_info *adjPtr)
{
   if (adjPtr->shefexPtr != (ShefEx *) NULL)
   {
      FreeShefEx(adjPtr->shefexPtr);
      adjPtr->shefexPtr = NULL;
   }
   
   return;
}
