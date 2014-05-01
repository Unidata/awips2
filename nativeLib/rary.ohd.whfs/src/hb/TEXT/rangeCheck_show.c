/*
	File:		rangeCheck_show.c
	Date:		February 1997
	Author:		Paul Taylor

	Purpose:	Provide support for the Range Check DS.
*/


/*
	Standard includes.
*/
#include <time.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <datetime.h>

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
#include "DataLimits.h"
#include "Filter.h"
#include "GeneralUtil.h"
#include "LocDataLimits.h"
#include "ShefDur.h"
#include "ShefPe.h"
#include "sqltypes.h"
#include "time_convert.h"

/*
	Local includes.
*/
#include "hybase.h"
#include "hbAS.h"
#include "user_prefs.h"
#include "rangeCheck.h"
#include "rangeCheck_show.h"



void	ShowRangeCheckDs(Widget w)
{
   rc_info	*rcPtr = GetDataLimitsInfo(True);
   
   if (! rcheckDS)
   {
      create_rcheckDS(GetTopShell(w));
      rc_create_shefdur_btns(rcPtr);
      rcheck_callbacks();
   }
   
   
   if (! XtIsManaged(rcheckDS))
   {
      rcPtr->shefpePtr = rcheck_getshefpes();
      rcheck_loadpeLI(rcfilter_peLI, rcPtr->shefpePtr);
      rcheck_loadpeLI(rcitem_peLI, rcPtr->shefpePtr);
      
      SetMenuPos(rclistOM, RC_DEFAULT_RANGES);
      rcheck_loaddisplay(NULL, NULL, NULL);
      
      XtManageChild(rcheckFO);
      XtManageChild(rcheckDS);
   }
   
   return;
}


/*
	Driver function for main display which sets up default values
	for the display and determines what data is to be loaded using
	the "filter parameters".
	
	The display is then loaded with data.
*/
void	rcheck_loaddisplay(Widget w, XtPointer ptr, XtPointer cbs)
{
   rc_info	*rcPtr   = GetDataLimitsInfo(False);
   int		menu_pos = GetMenuPos(rclistOM);
      

   /*
   	Performs PB-specific work,
	then load data into display.
   */
   if (menu_pos == RC_DEFAULT_RANGES)
   {
      strcpy(rcPtr->table, "DataLimits");
      strcpy(rcPtr->orderby_phrase, " ORDER BY pe, dur, monthdaystart, ");
      strcat(rcPtr->orderby_phrase, "monthdayend, gross_range_min, gross_range_max ");
      
      rcloc_setSensitivity(False);	/* Turns off location parts of dialog */
      rc_Sense_PeTB(NULL, NULL, NULL);	/* Sensitizes for "PeTB". */
      XmToggleButtonSetState(rcfilter_locTB, False, False); /* button OFF */
   }
   else if (menu_pos == RC_LOCATION_RANGES)
   {
      strcpy(rcPtr->table, "LocDataLimits");
      strcpy(rcPtr->orderby_phrase, " ORDER BY lid, pe, dur, monthdaystart, ");
      strcat(rcPtr->orderby_phrase, "monthdayend, gross_range_min, gross_range_max ");
      
      rcloc_setSensitivity(True);	/* Turns on location parts of dialog */
      rc_Sense_LocTB(NULL, NULL, NULL);	/* Sensitizes for "LocTB". */
      rc_Sense_PeTB(NULL, NULL, NULL);	/* Sensitizes for "PeTB". */
   }
   
   /*
	Determine what data to load.
   */
   rc_filter_grabLocPe();
   rcheck_loadDataIntoDisplay(menu_pos);

   return;
}


/*
	Controls the loading of data into the display.
*/
void	rcheck_loadDataIntoDisplay(int menu_pos)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);


   /*
   	Build where_phrase & whereorderby_phrase.
   */
   
   if (rcPtr != NULL)
   {
     rc_filter_buildPhrases(rcPtr->loc_phrase, rcPtr->pe_phrase,
			  rcPtr->orderby_phrase);

   /*
   	Load data into the display.
   */
     rcheck_clearDisplay();
     if (menu_pos == RC_DEFAULT_RANGES)
     {
       rcPtr->item_where_drcPtr = NULL;	/* No drc item selected in rcmainLI */
      
       rcPtr->drcPtr = rcheck_getdefault();
       rcheck_loaddefault();
     }
     else if (menu_pos == RC_LOCATION_RANGES)
     {
       rcPtr->item_where_lrcPtr = NULL;	/* No lrc item selected in rcmainLI */
      
       rcPtr->lrcPtr = rcheck_getlocation();
       rcheck_loadlocation();
     }
    }
   return;
}


/*
     	Used to Sensitize and DeSensitize the location parts of dialog.
*/
void 	rcloc_setSensitivity(Boolean set)
{
   if (set)
   {
      Sensitize(rcfilter_locTB);
      Sensitize(rcfilterlocTE);
      Sensitize(rcitem_locLA);
      Sensitize(rcitem_locTE);
   }	
   else
   {
      DeSensitize(rcfilter_locTB);
      DeSensitize(rcfilterlocTE);      
      DeSensitize(rcitem_locLA);
      DeSensitize(rcitem_locTE);
   }
   
   return; 
}


/*
     	Used to Sensitize and DeSensitize widgets for existence of data.
*/
void 	rcdata_setSensitivity(Boolean set)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);
   
   if (set)
   {
      Sensitize(rcitemFR);
      Sensitize(rcitem_peLI);
      
      if (rcPtr != NULL)
      {
       if (rcPtr->data_entry_mode == RC_NORMAL_ENTRY_MODE)
       {
	 Sensitize(rcheck_deletePB);
       }
       else if (rcPtr->data_entry_mode == RC_NEW_ENTRY_MODE)
       {
	 DeSensitize(rcheck_deletePB);
       }
     }  
   }
   else
   {
      DeSensitize(rcitemFR);
      DeSensitize(rcitem_peLI);
      DeSensitize(rcheck_deletePB);
   }
   
   return;   
}


/*
	Converts all slashes in the input string to hyphens.
*/
char*	rc_cvttext_slash_to_hyphen(char *input)
{
   static char	output[ANSI_TIME_LEN];
   int		i;

   
   memset(output, '\0', sizeof(output));
   for (i=0; i<strlen(input); i++)
   {
      if (input[i] == '/')
      {
	 output[i] = '-';
      }
      else
      {
	 output[i] = input[i];
      }
   }
   
   
   return(output);
}


/*
	Converts a string in a text widget to uppercase (as you type it).
	All non-alphabetic characters are ignored.
*/
void	rc_cvttext_lwr_to_upr(Widget w, XtPointer ptr, XtPointer cbs)
{
   XmTextVerifyCallbackStruct *tvcbs = (XmTextVerifyCallbackStruct *) cbs;
   
   char		buf[MAX_BUF_LEN],
      		*string;
   
   int		i;
   
   /*
   	Null string entered.
   */
   if (tvcbs->text->length == 0)
      return;
   
   
   for (i = 0; i < tvcbs->text->length; i++)
   {
      /*
      		Verify input text is alphanumeric,
      		and convert to uppercase, if
      		necessary.
      */
      
      if (! isAlphaNum(tvcbs->text->ptr[i]))
      {
	 tvcbs->doit = False;
	 break;
      }
      
      if (islower(tvcbs->text->ptr[i]))
	 tvcbs->text->ptr[i] = toupper(tvcbs->text->ptr[i]);
   }
   
   /*
   	Build final string.
   */
   if ( ( string = XmTextGetString(w) ) )
   {
      memset(buf, '\0', sizeof(buf));
      strcpy(buf, string);
      strcat(buf, tvcbs->text->ptr);
      XtFree(string);
   }
   
   
   return;
}


/*
	Returns the correct PB-name by specifying an integer duration.
*/
char*	rc_get_shefdur_name(int dur)
{
   static char	name[MAX_BUF_LEN];
   
   ShefDur	*shefdur,
      		*shefdurPtr;
   
   char		where[MAX_WHERE_LEN];
   
   
   memset(where, '\0', sizeof(where));
   sprintf(where, " WHERE dur = '%i' ", dur);
   shefdur = GetShefDur(where);
   shefdurPtr = (ShefDur *) ListFirst(&shefdur->list);
   
   if (shefdurPtr != (ShefDur *) NULL)
   {
      memset(name, '\0', sizeof(name));
      strcpy(name, rc_create_shefdur_name(shefdurPtr));
      FreeShefDur(shefdurPtr);
      return(&name[0]);
   }
   else
      return("");
}


/*
	Creates a PB-name for a given shefdur.
*/
char*	rc_create_shefdur_name(ShefDur *shefdurPtr)
{
   static char		name[BUFSIZ];
   char			buf[10];
   
   memset(name, '\0', sizeof(name));
   memset(buf, '\0', sizeof(buf));

   strcpy(name, shefdurPtr->name);
   sprintf(buf, " (%i)", shefdurPtr->dur);
   strcat(name, buf);
   
   return(&name[0]);
}


/*
	Creates the buttons needed for the Duration OM.
*/
void	rc_create_shefdur_btns(rc_info *rcPtr)
{
   static ShefDur	*shefdur = NULL,
			*shefdurPtr = NULL;	/* for storage */

   char			wname[BUFSIZ];
   
   ShefDur		*sdurPtr = NULL;	/* for loop control only */
   Widget		sdurPB;


   /*
   	Free old memory.
   	Retrieve new record(s) from the dbms.

	If there is at least 1 record in the dbms, set rcPtr->shefdurPtr.
	Otherwise, set rcPtr->shefdurPtr to NULL.
   */
   rcheck_freeshefdur(rcPtr);
   shefdur = GetShefDur(" ORDER BY dur ");
   if (shefdur != (ShefDur *) NULL)
   {
      shefdurPtr = (ShefDur *) ListFirst(&shefdur->list);
      rcPtr->shefdurPtr = shefdurPtr;  /* keep the list of records around */


      /* LOOPING */
      sdurPtr = shefdurPtr;
      while(sdurPtr != (ShefDur *) NULL)
      {
	 strcpy(wname, rc_create_shefdur_name(sdurPtr));
	 
	 sdurPB = XtVaCreateManagedWidget(wname,
					  xmPushButtonWidgetClass,
					  rcitem_durPDM,
					  NULL);
	 
	 sdurPtr = (ShefDur *) ListNext(&sdurPtr->node);
      }
   }
   else
   {
      rcPtr->shefdurPtr = NULL;  /* no records in dbms */
   }
   
   return;
}


/*
	This function returns the correct PB-name given a Physical Element.
*/
char*	rc_get_shefpe_name(char *pe)
{
   ShefPe	*shefpe,
      		*shefpePtr;
   
   char where [ MAX_WHERE_LEN ] ;
   static char buf [ MAX_BUF_LEN ] ;
   
   
   memset(where, '\0', sizeof(where));
   sprintf(where, " WHERE pe = '%s' ", pe);
   shefpe = GetShefPe(where);
   shefpePtr = (ShefPe *) ListFirst(&shefpe->list);
   
   if (shefpePtr != (ShefPe *) NULL)
   {
      memset(buf, '\0', sizeof(buf));
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
char*	rc_getpe_from_peLI(Widget peLI)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);
   
   ShefPe	*shefpePtr = NULL;
   
   int		*poslist,
      		cnt,
		pe_nth;

   
   XmListGetSelectedPos(peLI, &poslist, &cnt);
   
   if (cnt > 0)
   {
      pe_nth = poslist[0];
      shefpePtr = (ShefPe *) ListNth(&rcPtr->shefpePtr->list, pe_nth);
      
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
void	rc_loadpe_into_peLI(Widget peLI, char *pe)
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
   strcat(buf, rc_get_shefpe_name(pe));
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
void	rcheck_loadpeLI(Widget peLI, ShefPe *shefpePtr)
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
	    memset(buf, '\0', sizeof(buf));
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


void	rc_recompute_rcmainLI_selpos(rc_info *rcPtr)
{
   int	selpos = rcPtr->rcmainLI_selpos;
   int	item_count;

   /*
   	Get the item_count.
	Subtract one from selpos if at end of list, otherwise--no change.
   */
   XtVaGetValues(rcmainLI, XmNitemCount, &item_count, NULL);
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
   rcPtr->rcmainLI_selpos = selpos;
   rcPtr->delete_flag = True;  /* yes, a delete operation was just performed */
   
   return;
}


void	rcheck_callbacks(void)
{	
   Atom		atom;
   
   
   /*
   	Window manager callbacks.
   */
   atom = XmInternAtom(XtDisplay(rcheckDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(rcheckDS, atom, rcheck_close, NULL);
   
   
   /*
   	Widget callbacks.
   */
   XtAddCallback(rcdefaultPB,    XmNactivateCallback, rcheck_loaddisplay, NULL);
   XtAddCallback(rclocationPB,   XmNactivateCallback, rcheck_loaddisplay, NULL);
   
   XtAddCallback(rcheck_okPB,    XmNactivateCallback, rcheck_ok,      NULL);
   XtAddCallback(rcheck_applyPB, XmNactivateCallback, rcheck_apply,   NULL);
   XtAddCallback(rcheck_cancelPB,XmNactivateCallback, rcheck_close,   NULL);
   XtAddCallback(rcheck_deletePB,XmNactivateCallback, rcheck_del_conf,NULL);
   XtAddCallback(rcheck_newPB,   XmNactivateCallback, rcheck_new,     NULL);
   
   XtAddCallback(rcmainLI, XmNbrowseSelectionCallback, rc_setItemData, NULL);
   
   XtAddCallback(rcfilter_locTB, XmNvalueChangedCallback, rc_Sense_LocTB, NULL);
   XtAddCallback(rcfilter_locTB, XmNvalueChangedCallback, rc_useFilter,   NULL);
   XtAddCallback(rcfilterlocTE,  XmNvalueChangedCallback, rc_useFilter,   NULL);

   XtAddCallback(rcfilter_peTB, XmNvalueChangedCallback,      rc_Sense_PeTB, NULL);
   XtAddCallback(rcfilter_peTB, XmNvalueChangedCallback,      rc_useFilter,  NULL);
   XtAddCallback(rcfilter_peLI, XmNmultipleSelectionCallback, rc_useFilter,  NULL);
   
   XtAddCallback(rcfilterlocTE, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)MIXEDCASE);
   XtAddCallback(rcitem_locTE,  XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)MIXEDCASE);
   
   XtAddCallback(rcitem_startTE,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
   XtAddCallback(rcitem_endTE,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);

   XtAddCallback(gross_minTE,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(gross_maxTE,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(reason_minTE, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(reason_maxTE, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(roc_maxTE,    XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(alert_upper_limitTE,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(alarm_upper_limitTE,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(alert_lower_limitTE,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(alarm_lower_limitTE,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(alert_diff_limitTE,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(alarm_diff_limitTE,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(alert_roc_limitTE,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(alarm_roc_limitTE,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);

   
   return;
} 


void	rcheck_ok(Widget w, XtPointer ptr, XtPointer cbs)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);
   

   rcheck_apply(NULL, NULL, NULL);
   
   if (rcPtr->apply_successful)
   {
      rcheck_close(NULL, NULL, NULL);
   }
   
   return;
}


/*
	Driver function that determines how to apply data to database.
	
	The 2 helper functions below provide the tools necessary
	to apply data to the defaultrangecheck & locrangecheck tables.
*/
void	rcheck_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
   rc_info		*rcPtr = GetDataLimitsInfo(False);
   
   int			menu_pos = GetMenuPos(rclistOM);
   int			error = 1;

   int			okay = 0;

   /*
   	Obtain data from the rcitemFO.
	
	Apply changes to the dbms, if possible.
	  Be sure to indicate whether or not the apply was successful.
   */
   if ( ( okay = rc_getItemData ( rcPtr ) ) )
   {
      if (menu_pos == RC_DEFAULT_RANGES)
      {
	 error = rc_apply_defItemData(rcPtr); /* return error, or 0 (no error) */
      }
      else if (menu_pos == RC_LOCATION_RANGES)
      {
	 error = rc_apply_locItemData(rcPtr); /* return error, or 0 (no error) */
      }
   }
   else
   {
      rcPtr->apply_successful = False;  /* failure */

      return;
   }

/*
printf("stvo error from apply == <%i>\n", error);
*/
   
   /*
   	If there was no error in appling the record to the dbms...
	
   	  Ensure: New entry mode -> normal entry mode,
	  Load data into the display, and
	  Indicate that the apply was successful.
   */
   if (error == 0)
   {
      if (rcPtr->data_entry_mode == RC_NEW_ENTRY_MODE)
      {
	 clearForm(rcitemFO);
	 if (rcPtr->rcmainLI_selpos > 0)
	 {
	    XmListSelectPos(rcmainLI, rcPtr->rcmainLI_selpos, True);
	    XmListSetPos(rcmainLI, rcPtr->rcmainLI_selpos);
	 }
	 SetLabel(rcitemLA, "Selected Item");
	 rcPtr->data_entry_mode = RC_NORMAL_ENTRY_MODE;
      }
      
      rcheck_loadDataIntoDisplay(menu_pos);
      rcPtr->apply_successful = True;  /* success */
      return;
   }
   else
   {
      ErrorDialog(rcheckDS, DbErrorString(error));
   }

   
   rcPtr->apply_successful = False;  /* failure */
   
   return;
}


int	rc_apply_defItemData(rc_info *rcPtr)
{
   DataLimits		*drcPtr = rcPtr->item_drcPtr;
   char			*drcPtr_where = rcPtr->item_where_drcPtr;
   
   int			error;
   

   /*
   	Perform Update/Insert operation, as appropriate.
   */
   if (rcPtr->data_entry_mode == RC_NEW_ENTRY_MODE)
   {

/*
printf("stvo attempting put default (drcPtr_where == <%s>...\n", drcPtr_where);
*/
      
      error =PutDataLimits(drcPtr);
      if (error != 0)
      {
	 return(error);
      }
   }
   else if (rcPtr->data_entry_mode == RC_NORMAL_ENTRY_MODE)
   {

/*
printf("stvo attempting update default...where == <%s>\n", drcPtr_where);
*/
      
      error = UpdateDataLimits(drcPtr, drcPtr_where);
      if (error != 0)
      {
	 return(error);
      }
   }
   
   return(0);	/* 0 means the "defItemData" apply was successful */
}


int	rc_apply_locItemData(rc_info *rcPtr)
{
   LocDataLimits	*lrcPtr = rcPtr->item_lrcPtr;
   char			*lrcPtr_where = rcPtr->item_where_lrcPtr;
   
   int			error;
   

   /*
   	Perform Update/Insert operation, as appropriate.
   */
   if (rcPtr->data_entry_mode == RC_NEW_ENTRY_MODE)
   {
/*
printf("attempting put loc...\n");
*/
      
      error = PutLocDataLimits(lrcPtr);
      if (error != 0)
      {
	 return(error);
      }
   }
   else if (rcPtr->data_entry_mode == RC_NORMAL_ENTRY_MODE)
   {
/*
printf("attempting update loc...where == <%s>\n",
       lrcPtr_where);
*/
      
      error = UpdateLocDataLimits(lrcPtr, lrcPtr_where);
      if (error != 0)
      {
	 return(error);
      }
   }
   
   return(0);	/* 0 means the "defItemData" apply was successful */
}


void	rcheck_close(Widget w, XtPointer ptr, XtPointer cbs)
{

   freeDataLimitsInfo();
      
   if (XtIsManaged(rcheckDS))
   {
      XtDestroyWidget(rcheckDS);
      rcheckDS = NULL;
   }
      
   return;
}


void	rcheck_new(Widget w, XtPointer ptr, XtPointer cbs)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);
   
   int		*poslist, cnt;
   

   /*
   	Deselect the only-selected item from rcmainLI.
   */
   XmListGetSelectedPos(rcmainLI, &poslist, &cnt);
   if (cnt > 0)
   {
      XmListDeselectPos(rcmainLI, poslist[0]);

      free(poslist);
      poslist = NULL;
   }
      

   /*
   	Change to new entry mode.
	Set data sensitivity to True.
   */
   if (rcPtr->data_entry_mode == RC_NORMAL_ENTRY_MODE)
   {
      clearForm(rcitemFO);
      SetLabel(rcitemLA, "NEW Item");
      
      SetMenuPos(rcitem_durOM, 0);
      XmListDeselectAllItems(rcitem_peLI);
      XmListSetPos(rcitem_peLI, 1);
      
      rcPtr->data_entry_mode = RC_NEW_ENTRY_MODE;
   }
   rcdata_setSensitivity(True);
   
   return;
}


void	rcheck_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{        
   QuestionDialogWithCallbacks(rcheckDS, "Do you wish to delete this entry?",
			       "Delete Confirmation", rcheck_delete, NULL);
   
   return;
}


/*
	Delete the selected item from the database,
	and reload data into the display.
*/
void	rcheck_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);
   
   int		menu_pos = GetMenuPos(rclistOM);
   
   int		*poslist, cnt, nth_pos;
   
   DataLimits	*drcPtr = NULL;
   LocDataLimits	*lrcPtr = NULL;
   
   char		where[1000];
   
   int		error = 0;
   

   SetCursor(rcheckFO, XC_watch);
   
   
   XmListGetSelectedPos(rcmainLI, &poslist, &cnt);
   if (cnt > 0)
   {
      nth_pos = poslist[0];

      if (menu_pos == RC_DEFAULT_RANGES)
      {
	 drcPtr = (DataLimits *) ListNth(&rcPtr->drcPtr->list, nth_pos);
	 
	 memset(where, '\0', sizeof(where));
	 strcpy(where, rc_build_def_where(drcPtr));
	 
	 error = DeleteDataLimits((const char *) where);
/*
printf("error == <%i>, where == <%s>\n", error, where);
*/
	 
	 if (!error)
	 {
	    rc_recompute_rcmainLI_selpos(rcPtr);
	 }
	 else
	 {
	    UnsetCursor(rcheckFO);
	    ErrorDialog(rcheckDS, DbErrorString(error));
	    return;
	 }
      }
      else if (menu_pos == RC_LOCATION_RANGES)
      {
	 lrcPtr = (LocDataLimits *) ListNth(&rcPtr->lrcPtr->list, nth_pos);
	 
	 memset(where, '\0', sizeof(where));
	 strcpy(where, rc_build_loc_where(lrcPtr));
	 
	 error = DeleteLocDataLimits((const char *) where);
	 if (!error)
	 {
	    rc_recompute_rcmainLI_selpos(rcPtr);
	 }
	 else
	 {
	    UnsetCursor(rcheckFO);
	    ErrorDialog(rcheckDS, DbErrorString(error));
	    return;
	 }
      }
   }
   
   
   /*
   	Always filter on Loc & Pe.
	Load the display with data.
   */
   rc_filter_grabLocPe();
   rcheck_loadDataIntoDisplay(menu_pos);
   
   UnsetCursor(rcheckFO);
   
   
   return;
}


void	rcheck_help(Widget w, XtPointer ptr, XtPointer cbs)
{
   return;
}


/*
     	Senses the state of the LocTB,
	Sensitizing and DeSensitizing where necessary.
*/
void 	rc_Sense_LocTB(Widget w, XtPointer ptr, XtPointer cbs)
{

   if (XmToggleButtonGetState(rcfilter_locTB))
   {
      Sensitize(rcfilterlocTE);
   }
   else
   {
      DeSensitize(rcfilterlocTE);
   }
   
   return;   
}


/*
     	Senses the state of the PeTB,
	Sensitizing and DeSensitizing where necessary.
*/
void 	rc_Sense_PeTB(Widget w, XtPointer ptr, XtPointer cbs)
{

   if (XmToggleButtonGetState(rcfilter_peTB))
   {
      Sensitize(rcfilter_peLI);
   }
   else
   {
      DeSensitize(rcfilter_peLI);
   }
   
   return;   
}


void	rcheck_clearDisplay(void)
{
   /*
   	Clear out certain widgets.
   */
   XmListDeleteAllItems(rcmainLI);
   XmListDeselectAllItems(rcitem_peLI);
   clearForm(rcitemFO);
   
   return;
}


/*
	The rc_useFilter function calls its helper functions below it.

	There are functions to build phrases, get & set phrases,
	and to select & deselect a stored poslist.
*/
void	rc_useFilter(Widget w, XtPointer ptr, XtPointer cbs)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);


   /*
	If the rcfilter_peTB was chosen,
	Select or Deselect the rcfilter_peLI using the filterpe_poslist.
   */
   if (w == rcfilter_peTB)
   {
      if (XmToggleButtonGetState(rcfilter_peTB))	/* peTB --> ON */
      {
	 rc_selectposlist();	/* Select (show) the old poslist. */
	 rcPtr->filterpe_poslist_okayfree = True;
      }
      else						/* peTB --> OFF */
      {
	 rc_deselectposlist();	/* Deselect (hide) the new poslist. */
	 rcPtr->filterpe_poslist_okayfree = False;
      }
   }


   /*
   	Always filter on Loc & Pe.
	Load the display with data.
   */
   rc_filter_grabLocPe();
   rcheck_loadDataIntoDisplay(GetMenuPos(rclistOM));
   
   return;
}


void	rc_filter_grabLocPe(void)
{
   rcheck_setLocPhrase(NULL);
   if (XmToggleButtonGetState(rcfilter_locTB))
   {
      rcheck_obtainLocPhrase();
   }

   rcheck_setPePhrase(NULL);
   if (XmToggleButtonGetState(rcfilter_peTB))
   {
      rcheck_obtainPePhrase();
   }
   
   return;
}


void	rc_selectposlist(void)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);
   
   int		cnt = rcPtr->filterpe_poslist_cnt;
   int		i;

   /*
   	Select the previously-saved filterpe_poslist.
   */
   if (cnt > 0)
   {
      for (i=0; i<cnt; i++)
      {
	 XmListSelectPos(rcfilter_peLI, rcPtr->filterpe_poslist[i], False);
      }
   }
   
   return;
}


void	rc_deselectposlist(void)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);

   int		cnt = rcPtr->filterpe_poslist_cnt;
   int		i;
   
   /*
   	Deselect very-recently-saved filterpe_poslist.
   */
   if (cnt > 0)
   {
      for (i=0; i<cnt; i++)
      {
	 XmListDeselectPos(rcfilter_peLI, rcPtr->filterpe_poslist[i]);
      }
   }
   
   return;
}


void	rc_filter_buildPhrases(char *loc_phrase, char *pe_phrase,
			       char *orderby_phrase)
{
   static char		where_phrase[RC_MAX_WHERE_PHRASE],
      			whereorderby_phrase[RC_MAX_WHERE_PHRASE];
   
   rc_info		*rcPtr = GetDataLimitsInfo(False);
   
   
   /*
   	Empty out the phrases.
   */
   memset(where_phrase, '\0', sizeof(where_phrase));
   memset(whereorderby_phrase, '\0', sizeof(whereorderby_phrase));
   rcPtr->where_phrase = NULL;
   rcPtr->whereorderby_phrase = NULL;
   

   /*
   	Create the where_phrase (if possible).
   */
   if (loc_phrase && pe_phrase)
   {
      sprintf(where_phrase, " WHERE (%s) AND (%s) ", loc_phrase, pe_phrase);
   }
   else if (loc_phrase && !pe_phrase)
   {
      sprintf(where_phrase, " WHERE (%s) ", loc_phrase);
   }
   else if (!loc_phrase && pe_phrase)
   {
      sprintf(where_phrase, " WHERE (%s) ", pe_phrase);
   }
   

   /*
   	Create the whereorderby_phrase.
   */
   if (strlen(where_phrase) > 0)
   {
      strcpy(whereorderby_phrase, where_phrase);
      rcPtr->where_phrase = &where_phrase[0];
   }

   strcat(whereorderby_phrase, orderby_phrase);   
   rcPtr->whereorderby_phrase = &whereorderby_phrase[0];

   
   return;
}


void	rcheck_setLocPhrase(char *loc_phrase)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);
   
   rcPtr->loc_phrase = loc_phrase;
   return;
}


void	rcheck_setPePhrase(char *pe_phrase)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);
   
   rcPtr->pe_phrase = pe_phrase;
   return;
}


void	rcheck_obtainLocPhrase(void)
{
   static char		loc_phrase[RC_MAX_LOC_PHRASE];
   char			*text;
   

   text = XmTextGetString(rcfilterlocTE);
   memset(loc_phrase, '\0', sizeof(loc_phrase));
   sprintf(loc_phrase, "lid = '%s'", text);

   /*
   	Set the Loc Phrase, free text,
	and build a new where clause.
	Return.
   */
   rcheck_setLocPhrase(loc_phrase);
   if (text != NULL)
   {
      XtFree(text);
   }
   
   return;
}


void	rcheck_obtainPePhrase(void)
{
   rc_info		*rcPtr = GetDataLimitsInfo(False);
   ShefPe		*shefpePtr = NULL;

   static int		*poslist = NULL;
   int			cnt, i;

   static char		pe_phrase[RC_MAX_PE_PHRASE];
   static char		pe_header[] = "pe in (";
   static char		pe_trailer[] = ")";
   char			newpe[5];
   

   /*
   	Work to maintain rcPtr->filterpe_poslist.
   */
   if (rcPtr->filterpe_poslist_okayfree)
   {
      if (rcPtr->filterpe_poslist != (int *) NULL)
      {
	 rcheck_freeposlist(rcPtr);	/* (also sets poslist to NULL) */
      }
      
      
      XmListGetSelectedPos(rcfilter_peLI, &poslist, &cnt);
      
      
      if (cnt > 0)
      {
	 rcPtr->filterpe_poslist = poslist;	/* ensures poslist is saved */
	 rcPtr->filterpe_poslist_cnt = cnt;
      }
      else
      {
	 poslist = NULL;			/* ensures poslist is NULL */
	 rcPtr->filterpe_poslist_cnt = 0;
      }
   }
   else
   {
      cnt = 0;	/* We are not filtering on Pe right now... */
   }
/*
printf("poslist == <%p>, cnt == <%i>\n",
       rcPtr->filterpe_poslist, rcPtr->filterpe_poslist_cnt);
*/
   
   /*
   	Get all items from the filter_peLI that are to be displayed
	in the mainLI on the left-hand part of the dialog.
   */
   memset(pe_phrase, '\0', sizeof(pe_phrase));
   strcpy(pe_phrase, pe_header);
   
   if (cnt > 0)
   {
      for(i=0; i<cnt; i++)
      {
	 shefpePtr = (ShefPe *) ListNth(&rcPtr->shefpePtr->list, poslist[i]);
	 if (shefpePtr != (ShefPe *) NULL)
	 {
	    memset(newpe, '\0', sizeof(newpe));
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
      rcheck_setPePhrase(pe_phrase);
   else
      rcheck_setPePhrase("pe in (NULL)");

   return;
}


/*
	Driver function that determines how to get the data from the
	Item fields on the display.
*/
int	rc_getItemData(rc_info *rcPtr)
{
   static DataLimits	drc;
   static LocDataLimits		lrc;
   
   int		menu_pos = GetMenuPos(rclistOM);

   int		okay = 0;
   
   
   if (menu_pos == RC_DEFAULT_RANGES)
   {
      memset(&drc, '\0', sizeof(drc));
      okay = rc_getDefItemData(&drc);
      rcPtr->item_drcPtr = &drc;
   }
   else if (menu_pos == RC_LOCATION_RANGES)
   {
      memset(&lrc, '\0', sizeof(lrc));
      okay = rc_getLocItemData(&lrc);
      rcPtr->item_lrcPtr = &lrc;
   }
   
   return(okay);
}
	 
	 
/*
	Obtains Item fields from the display and places data into
	the structure of a passed-in pointer variable (drcPtr).
*/
int	rc_getDefItemData(DataLimits *drcPtr)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);
   
   int		menu_pos,
      		menu_nth;
   
   ShefDur	*menu_shefdur = NULL ;
   
   char		*buf = NULL ;

   int		rv = 0;

   monthday_t start_date = 0,
              end_date = 0;
   
   /*
   	Obtain duration.
   */
   menu_pos = GetMenuPos(rcitem_durOM);
   menu_nth = menu_pos + 1;
   menu_shefdur = (ShefDur *) ListNth(&rcPtr->shefdurPtr->list, menu_nth);
   drcPtr->dur = menu_shefdur->dur;
   

   /*
   	Obtain pe.
   */
   if (rc_getpe_from_peLI(rcitem_peLI) != (char *) NULL)
   {
      (void) strcpy(drcPtr->pe, rc_getpe_from_peLI(rcitem_peLI));
   }
   else
   {
      ErrorDialog(rcheckDS, "You must select a Physical Element.\n");
      return(rv);
   }

   /*
   	Obtain minval/maxval.
   */
   if ( ( buf = XmTextGetString ( gross_minTE ) ) )
   {
      if (!IsBlankStr(buf))
      	drcPtr->gross_range_min = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &drcPtr->gross_range_min);
      XtFree(buf);
      buf = NULL ;
   }
   if ( ( buf = XmTextGetString ( gross_maxTE ) ) )
   {
      if (!IsBlankStr(buf))
      	drcPtr->gross_range_max = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &drcPtr->gross_range_max);
      XtFree(buf);
      buf = NULL ;
   }

   if (drcPtr->gross_range_max <= drcPtr->gross_range_min)
   {
      ErrorDialog(rcheckDS, "The min value must be < the max value.\n"
		  "Please make the required changes and re-Apply them.");
      return(rv);
   }


   /* stvo Added */

   if ( (buf = XmTextGetString(reason_minTE)) )
   {
      if (!IsBlankStr(buf))
      	drcPtr->reason_range_min = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &drcPtr->reason_range_min);
      XtFree(buf);
      buf = NULL ;
   }

   if ( (buf = XmTextGetString(reason_maxTE)) )
   {
      if (!IsBlankStr(buf))
      	drcPtr->reason_range_max = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &drcPtr->reason_range_max);
      XtFree(buf);
      buf = NULL ;
   }

   if ( (buf = XmTextGetString(roc_maxTE)) )
   {
      if (!IsBlankStr(buf))
      	drcPtr->roc_max = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &drcPtr->roc_max);
      XtFree(buf);
      buf = NULL ;
   }

   if ( (buf = XmTextGetString(alert_upper_limitTE)) )
   {
      if (!IsBlankStr(buf))
      	drcPtr->alert_upper_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &drcPtr->alert_upper_limit);
      XtFree(buf);
      buf = NULL ;
   }

   if ( (buf = XmTextGetString(alarm_upper_limitTE)) )
   {
      if (!IsBlankStr(buf))
      	drcPtr->alarm_upper_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &drcPtr->alarm_upper_limit);
      XtFree(buf);
      buf = NULL ;
   }

   if ( (buf = XmTextGetString(alert_lower_limitTE)) )
   {
      if (!IsBlankStr(buf))
      	drcPtr->alert_lower_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &drcPtr->alert_lower_limit);
      XtFree(buf);
      buf = NULL ;
   }

   if ( (buf = XmTextGetString(alarm_lower_limitTE)) )
   {
      if (!IsBlankStr(buf))
      	drcPtr->alarm_lower_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &drcPtr->alarm_lower_limit);
      XtFree(buf);
      buf = NULL ;
   }

   if ( (buf = XmTextGetString(alert_diff_limitTE)) )
   {
      if (!IsBlankStr(buf))
      	drcPtr->alert_diff_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &drcPtr->alert_diff_limit);
      XtFree(buf);
      buf = NULL ;
   }

   if ( (buf = XmTextGetString(alarm_diff_limitTE)) )
   {
      if (!IsBlankStr(buf))
      	drcPtr->alarm_diff_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &drcPtr->alarm_diff_limit);
      XtFree(buf);
      buf = NULL ;
   }

   if ( (buf = XmTextGetString(alert_roc_limitTE)) )
   {
      if (!IsBlankStr(buf))
      	drcPtr->alert_roc_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &drcPtr->alert_roc_limit);
      XtFree(buf);
      buf = NULL ;
   }
   
   if ( (buf = XmTextGetString(alarm_roc_limitTE)) )
   {
      if (!IsBlankStr(buf))
      	drcPtr->alarm_roc_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &drcPtr->alarm_roc_limit);
      XtFree(buf);
      buf = NULL ;
   }

   /*
   	Obtain start/end.
   */
   if ( ( buf = XmTextGetString(rcitem_startTE) ) ) 
   {
      if ( (rv = USA_monthday_to_monthday_t (buf, &start_date)) != 0 )
      {
         ErrorDialog(rcheckDS, "Invalid start date...Please try again.\n");
	      return(0);
      }

      buf = NULL ;
   }

   if ( ( buf = XmTextGetString(rcitem_endTE) ) )
   {
      if ( (rv = USA_monthday_to_monthday_t (buf, &end_date)) != 0 )
      {
         ErrorDialog(rcheckDS, "Invalid end date...Please try again.\n");
	      return(0);
      }

      buf = NULL ;
   }

   if (end_date < start_date)
   {
      ErrorDialog(rcheckDS, "The end date must be >= the start date, up to\n"
                            "a maximum end date of 12/31.  Please make the\n"
                            "required changes and re-Apply them.");
	   return(rv);
    }
    else
    {
       monthday_t_to_ansi_monthday ( start_date, drcPtr->monthdaystart );
       monthday_t_to_ansi_monthday ( end_date, drcPtr->monthdayend );
    }


   rv = 1;
   return(rv);
}


/*
	Obtains Item fields from the display and places data into
	the structure of a passed-in pointer variable (lrcPtr).
*/
int	rc_getLocItemData(LocDataLimits *lrcPtr)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);
   
   int		menu_pos,
      		menu_nth;
   
   ShefDur	*menu_shefdur = NULL;

   char		*buf = NULL ;

   monthday_t start_date = 0,
              end_date = 0;

   int		rv = 0;
   

   /*
   	Obtain lid.
   */
   if ( (buf = XmTextGetString(rcitem_locTE)) )
   {
      strcpy(lrcPtr->lid, buf);
      XtFree(buf);
      buf = NULL ;
   }
   
   
   /*
   	Obtain duration.
   */
   menu_pos = GetMenuPos(rcitem_durOM);
   menu_nth = menu_pos + 1;
   menu_shefdur = (ShefDur *) ListNth(&rcPtr->shefdurPtr->list, menu_nth);
   lrcPtr->dur = menu_shefdur->dur;
   

   /*
   	Obtain pe.
   */
   if (rc_getpe_from_peLI(rcitem_peLI) != (char *) NULL)
   {
      (void) strcpy(lrcPtr->pe, rc_getpe_from_peLI(rcitem_peLI));
   }
   else
   {
      ErrorDialog(rcheckDS, "You must select a Physical Element.\n");
      return(rv);
   }
   

   /*
   	Obtain minval/maxval.
   */

   if ( (buf = XmTextGetString(gross_minTE)) )
   {
      if (!IsBlankStr(buf))
      	lrcPtr->gross_range_min = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &lrcPtr->gross_range_min);
      XtFree(buf);
      buf = NULL ;
   }
   if ( (buf = XmTextGetString(gross_maxTE)) )
   {
      if (!IsBlankStr(buf))
      	lrcPtr->gross_range_max = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &lrcPtr->gross_range_max);
      XtFree(buf);
      buf = NULL ;
   }

   if (lrcPtr->gross_range_max <= lrcPtr->gross_range_min)
   {
      ErrorDialog(rcheckDS, "The min value must be < the max value.\n"
		  "Please make the required changes and re-Apply them.");
      return(rv);
   }

   
   /* stvo Added */

   if ( ( buf = XmTextGetString ( reason_minTE ) ) )
   {
      if (!IsBlankStr(buf))
      	lrcPtr->reason_range_min = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &lrcPtr->reason_range_min);
      XtFree(buf);
      buf = NULL ;
   }

   if ( ( buf = XmTextGetString ( reason_maxTE ) ) )
   {
      if (!IsBlankStr(buf))
      	lrcPtr->reason_range_max = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &lrcPtr->reason_range_max);
      XtFree(buf);
      buf = NULL ;
   }

   if ( ( buf = XmTextGetString (roc_maxTE ) ) )
   {
      if (!IsBlankStr(buf))
      	lrcPtr->roc_max = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &lrcPtr->roc_max);
      XtFree(buf);
      buf = NULL ;
   }

   if ( ( buf = XmTextGetString ( alert_upper_limitTE ) ) )
   {
      if (!IsBlankStr(buf))
      	lrcPtr->alert_upper_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &lrcPtr->alert_upper_limit);
      XtFree(buf);
      buf = NULL ;
   }

   if ( ( buf = XmTextGetString ( alert_lower_limitTE ) ) )
   {
      if (!IsBlankStr(buf))
      	lrcPtr->alert_lower_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &lrcPtr->alert_lower_limit);
      XtFree(buf);
      buf = NULL ;
   }

   if ( ( buf = XmTextGetString ( alarm_upper_limitTE ) ) )
   {
      if (!IsBlankStr(buf))
      	lrcPtr->alarm_upper_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &lrcPtr->alarm_upper_limit);
      XtFree(buf);
      buf = NULL ;
   }

   if ( ( buf = XmTextGetString ( alarm_lower_limitTE ) ) )
   {
      if (!IsBlankStr(buf))
      	lrcPtr->alarm_lower_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &lrcPtr->alarm_lower_limit);
      XtFree(buf);
      buf = NULL ;
   }

   if ( ( buf = XmTextGetString ( alert_diff_limitTE ) ) )
   {
      if (!IsBlankStr(buf))
      	lrcPtr->alert_diff_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &lrcPtr->alert_diff_limit);
      XtFree(buf);
      buf = NULL ;
   }

   if ( ( buf = XmTextGetString ( alarm_diff_limitTE ) ) )
   {
      if (!IsBlankStr(buf))
      	lrcPtr->alarm_diff_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &lrcPtr->alarm_diff_limit);
      XtFree(buf);
      buf = NULL ;
   }

   if ( ( buf = XmTextGetString ( alert_roc_limitTE ) ) )
   {
      if (!IsBlankStr(buf))
      	lrcPtr->alert_roc_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &lrcPtr->alert_roc_limit);
      XtFree(buf);
      buf = NULL ;
   }
   
   if ( ( buf = XmTextGetString (alarm_roc_limitTE ) ) )
   {
      if (!IsBlankStr(buf))
      	lrcPtr->alarm_roc_limit = atof(buf);
      else
        (void) SetNull(DOUBLE, (void *) &lrcPtr->alarm_roc_limit);
      XtFree(buf);
      buf = NULL ;
   }


   /*
   	Obtain start/end.
   */
   if ( ( buf = XmTextGetString(rcitem_startTE) ) ) 
   {
      if ( (rv = USA_monthday_to_monthday_t (buf, &start_date)) != 0 )
      {
         ErrorDialog(rcheckDS, "Invalid start date...Please try again.\n");
	      return(0);
      }

      buf = NULL ;
   }
   if ( ( buf = XmTextGetString(rcitem_endTE) ) )
   {
      if ( (rv = USA_monthday_to_monthday_t (buf, &end_date)) != 0 )
      {
         ErrorDialog(rcheckDS, "Invalid end date...Please try again.\n");
	      return(0);
      }

      buf = NULL ;
   }

   if (end_date < start_date)
   {
      ErrorDialog(rcheckDS, "The end date must be >= the start date, up to\n"
                            "a maximum end date of 12/31.  Please make the\n"
                            "required changes and re-Apply them.");
	   return(rv);
    }
    else
    {
       monthday_t_to_ansi_monthday ( start_date, lrcPtr->monthdaystart );
       monthday_t_to_ansi_monthday ( end_date, lrcPtr->monthdayend );
    }


   rv = 1;
   return(rv);
}


/*
	Driver function that determines how to populate the
	Item fields on the display.
*/
void	rc_setItemData(Widget w, XtPointer ptr, XtPointer cbs)
{
   rc_info		*rcPtr = GetDataLimitsInfo(False);
   DataLimits	*drcPtr = NULL ;
   LocDataLimits	*lrcPtr = NULL ;
   
   int			*poslist = NULL ,
      			cnt;
   
   int			menu_pos = GetMenuPos(rclistOM);

   

   /*
   	Use the correct mode (normal entry mode).
   	Use the correct label for the "item" section.

   	Set the item data for the selected position.
   */
   rcPtr->data_entry_mode = RC_NORMAL_ENTRY_MODE;
   SetLabel(rcitemLA, "Limits For Selected Item");
   Sensitize(rcheck_deletePB);

   XmListGetSelectedPos(rcmainLI, &poslist, &cnt);

   if (cnt > 0)
   {
      rcPtr->rcmainLI_selpos = poslist[0];	/* save selected position */
      
      if (menu_pos == RC_DEFAULT_RANGES)
      {
	 drcPtr = (DataLimits *) ListNth(&rcPtr->drcPtr->list, poslist[0]);
	 if (drcPtr != (DataLimits *) NULL)
	 {
	    rc_setDefItemData(drcPtr);
	 }
      }
      else if (menu_pos == RC_LOCATION_RANGES)
      {
	 lrcPtr = (LocDataLimits *) ListNth(&rcPtr->lrcPtr->list, poslist[0]);
	 if (lrcPtr != (LocDataLimits *) NULL)
	 {
	    rc_setLocItemData(lrcPtr);
	 }
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
void	rc_setDefItemData(DataLimits *drcPtr)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);
   
		char 	start[ANSI_TIME_LEN],
      		end[ANSI_TIME_LEN],
		gross_minval[15],
		gross_maxval[15],
		reason_minval[15],
		reason_maxval[15],
		roc_maxval[15],
		alert_upper_limit[15],
		alarm_upper_limit[15],
		alert_lower_limit[15],
		alarm_lower_limit[15],
		alert_diff_limit[15],
		alarm_diff_limit[15],
		alert_roc_limit[15],
		alarm_roc_limit[15];

   
   SetMenuHistory(rcitem_durOM, rc_get_shefdur_name(drcPtr->dur));
   
   memset(start, '\0', sizeof(start));
   strcpy(start, drcPtr->monthdaystart);
   start[2] = '/';
   XmTextSetString(rcitem_startTE, start);

   memset(end, '\0', sizeof(end));
   strcpy(end, drcPtr->monthdayend);
   end[2] = '/';
   XmTextSetString(rcitem_endTE, end);
   
   rc_loadpe_into_peLI(rcitem_peLI, drcPtr->pe);

   memset ( gross_minval, '\0', sizeof ( gross_minval ) ) ;
   if ( ! ( IsNull ( DOUBLE, ( void* ) &drcPtr->gross_range_min ) ) )
   {
   	sprintf(gross_minval, "%.2f", drcPtr->gross_range_min);
   }
   XmTextSetString ( gross_minTE , gross_minval ) ;
   
   memset(gross_maxval, '\0', sizeof(gross_maxval));
   if ( ! ( IsNull(DOUBLE, ( void* ) &drcPtr->gross_range_max ) ) )
   {
   	sprintf(gross_maxval, "%.2f", drcPtr->gross_range_max);
   }
   XmTextSetString(gross_maxTE, gross_maxval);

   memset(reason_minval, '\0', sizeof(reason_minval));
   if ( ! ( IsNull ( DOUBLE, ( void* ) &drcPtr->reason_range_min ) ) )
   {
   	sprintf(reason_minval, "%.2f", drcPtr->reason_range_min);
   }
   XmTextSetString(reason_minTE, reason_minval);

   memset(reason_maxval, '\0', sizeof(reason_maxval));
   if (  ! ( IsNull ( DOUBLE, ( void * ) &drcPtr->reason_range_max ) ) )
   {
   	sprintf(reason_maxval, "%.2f", drcPtr->reason_range_max);
   }
   XmTextSetString(reason_maxTE, reason_maxval);
   
   memset(roc_maxval, '\0', sizeof(roc_maxval));
   if ( ! ( IsNull ( DOUBLE , ( void * ) &drcPtr->roc_max ) ) )
   {
      sprintf(roc_maxval, "%.2f", drcPtr->roc_max);
   }
   XmTextSetString(roc_maxTE, roc_maxval);

   memset(alert_upper_limit, '\0', sizeof(alert_upper_limit));
   if ( ! ( IsNull(DOUBLE, (void*) &drcPtr->alert_upper_limit ) ) )
   {
      sprintf(alert_upper_limit, "%.2f", drcPtr->alert_upper_limit);
   }
   XmTextSetString(alert_upper_limitTE, alert_upper_limit);

   memset(alarm_upper_limit, '\0', sizeof(alarm_upper_limit));
   if ( ! ( IsNull(DOUBLE, (void*) &drcPtr->alarm_upper_limit ) ) )
   {
      sprintf(alarm_upper_limit, "%.2f", drcPtr->alarm_upper_limit);
   }
   XmTextSetString(alarm_upper_limitTE, alarm_upper_limit);

   memset(alert_lower_limit, '\0', sizeof(alert_lower_limit));
   if ( ! ( IsNull(DOUBLE, (void*) &drcPtr->alert_lower_limit ) ) )
   {
      sprintf(alert_lower_limit, "%.2f", drcPtr->alert_lower_limit);
   }
   XmTextSetString(alert_lower_limitTE, alert_lower_limit);

   memset(alarm_lower_limit, '\0', sizeof(alarm_lower_limit));
   if ( ! ( IsNull(DOUBLE, (void*) &drcPtr->alarm_lower_limit ) ) )
   {
      sprintf(alarm_lower_limit, "%.2f", drcPtr->alarm_lower_limit);
   }
   XmTextSetString(alarm_lower_limitTE, alarm_lower_limit);

   memset(alert_diff_limit, '\0', sizeof(alert_diff_limit));
   if ( ! ( IsNull(DOUBLE, (void*) &drcPtr->alert_diff_limit ) ) )
   {
      sprintf(alert_diff_limit, "%.2f", drcPtr->alert_diff_limit);
   }
   XmTextSetString(alert_diff_limitTE, alert_diff_limit);

   memset(alarm_diff_limit, '\0', sizeof(alarm_diff_limit));
   if ( ! ( IsNull(DOUBLE, (void*) &drcPtr->alarm_diff_limit ) ) )
   {
      sprintf(alarm_diff_limit, "%.2f", drcPtr->alarm_diff_limit);
   }
   XmTextSetString(alarm_diff_limitTE, alarm_diff_limit);

   memset(alert_roc_limit, '\0', sizeof(alert_roc_limit));
   if ( ! ( IsNull(DOUBLE, (void*) &drcPtr->alert_roc_limit ) ) )
   {
      sprintf(alert_roc_limit, "%.2f", drcPtr->alert_roc_limit);
   }
   XmTextSetString(alert_roc_limitTE, alert_roc_limit);

   memset(alarm_roc_limit, '\0', sizeof(alarm_roc_limit));
   if ( ! ( IsNull ( DOUBLE , ( void * ) &drcPtr->alarm_roc_limit ) ) )
   {
      sprintf(alarm_roc_limit, "%.2f", drcPtr->alarm_roc_limit);
   }
   XmTextSetString(alarm_roc_limitTE, alarm_roc_limit);
		       
   rcPtr->item_where_drcPtr = rc_build_def_where(drcPtr);

   return;
}


/*
	Sets the Item fields on the display,
	using the specified lrcPtr.
*/
void	rc_setLocItemData(LocDataLimits *lrcPtr)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);

 char		start[ANSI_TIME_LEN],
      	end[ANSI_TIME_LEN],
		gross_minval[15],
		gross_maxval[15],
		reason_minval[15],
		reason_maxval[15],
		roc_maxval[15],
		alert_upper_limit[15],
		alarm_upper_limit[15],
		alert_lower_limit[15],
		alarm_lower_limit[15],
		alert_diff_limit[15],
		alarm_diff_limit[15],
		alert_roc_limit[15],
		alarm_roc_limit[15];
   
   
   XmTextSetString(rcitem_locTE, lrcPtr->lid);

   SetMenuHistory(rcitem_durOM, rc_get_shefdur_name(lrcPtr->dur));
   
   memset(start, '\0', sizeof(start));
   strcpy(start, lrcPtr->monthdaystart);
   start[2] = '/';
   XmTextSetString(rcitem_startTE, start);

   memset(end, '\0', sizeof(end));
   strcpy(end, lrcPtr->monthdayend);
   end[2] = '/';
   XmTextSetString(rcitem_endTE, end);
   
   rc_loadpe_into_peLI(rcitem_peLI, lrcPtr->pe);
		       
   memset(gross_minval, '\0', sizeof(gross_minval));
   if ( ! ( IsNull ( DOUBLE, ( void * ) &lrcPtr->gross_range_min ) ) )
   {
      sprintf(gross_minval, "%.2f", lrcPtr->gross_range_min);
   }
   XmTextSetString(gross_minTE, gross_minval);
   
   memset(gross_maxval, '\0', sizeof(gross_maxval));
   if ( ! ( IsNull(DOUBLE, ( void * ) &lrcPtr->gross_range_max ) ) )
   {
      sprintf(gross_maxval, "%.2f", lrcPtr->gross_range_max);
   }
   XmTextSetString(gross_maxTE, gross_maxval);

   memset(reason_minval, '\0', sizeof(reason_minval));
   if ( ! ( IsNull ( DOUBLE , ( void * ) &lrcPtr->reason_range_min ) ) )
   {
      sprintf(reason_minval, "%.2f", lrcPtr->reason_range_min);
   }
   XmTextSetString(reason_minTE, reason_minval);

   memset(reason_maxval, '\0', sizeof(reason_maxval));
   if ( ! ( IsNull ( DOUBLE , ( void * ) &lrcPtr->reason_range_max ) ) )
   {
      sprintf(reason_maxval, "%.2f", lrcPtr->reason_range_max);
   }
   XmTextSetString(reason_maxTE, reason_maxval);
   
   memset(roc_maxval, '\0', sizeof(roc_maxval));
   if ( ! ( IsNull ( DOUBLE , ( void * ) &lrcPtr->roc_max ) ) )
   {
      sprintf(roc_maxval, "%.2f", lrcPtr->roc_max);
   }
   XmTextSetString(roc_maxTE, roc_maxval);

   memset(alert_upper_limit, '\0', sizeof(alert_upper_limit));
   if ( ! ( IsNull ( DOUBLE , ( void * ) &lrcPtr->alert_upper_limit ) ) )
   {
      sprintf(alert_upper_limit, "%.2f", lrcPtr->alert_upper_limit);
   }
   XmTextSetString(alert_upper_limitTE, alert_upper_limit);

   memset(alarm_upper_limit, '\0', sizeof(alarm_upper_limit));
   if ( ! ( IsNull ( DOUBLE , ( void * ) &lrcPtr->alarm_upper_limit ) ) )
   {
      sprintf(alarm_upper_limit, "%.2f", lrcPtr->alarm_upper_limit);
   }
   XmTextSetString(alarm_upper_limitTE, alarm_upper_limit);

   memset(alert_lower_limit, '\0', sizeof(alert_lower_limit));
   if ( ! ( IsNull ( DOUBLE , ( void * ) &lrcPtr->alert_lower_limit ) ) )
   {
      sprintf(alert_lower_limit, "%.2f", lrcPtr->alert_lower_limit);
   }
   XmTextSetString(alert_lower_limitTE, alert_lower_limit);

   memset(alarm_lower_limit, '\0', sizeof(alarm_lower_limit));
   if ( ! ( IsNull ( DOUBLE , ( void * ) &lrcPtr->alarm_lower_limit ) ) )
   {
      sprintf(alarm_lower_limit, "%.2f", lrcPtr->alarm_lower_limit);
   }
   XmTextSetString(alarm_lower_limitTE, alarm_lower_limit);

   memset(alert_diff_limit, '\0', sizeof(alert_diff_limit));
   if ( ! ( IsNull ( DOUBLE , ( void * ) &lrcPtr->alert_diff_limit ) ) )
   {
      sprintf(alert_diff_limit, "%.2f", lrcPtr->alert_diff_limit);
   }
   XmTextSetString(alert_diff_limitTE, alert_diff_limit);

   memset(alarm_diff_limit, '\0', sizeof(alarm_diff_limit));
   if ( ! ( IsNull ( DOUBLE , ( void * ) &lrcPtr->alarm_diff_limit ) ) )
   {
      sprintf(alarm_diff_limit, "%.2f", lrcPtr->alarm_diff_limit);
   }
   XmTextSetString(alarm_diff_limitTE, alarm_diff_limit);

   memset(alert_roc_limit, '\0', sizeof(alert_roc_limit));
   if ( ! ( IsNull ( DOUBLE, ( void * ) &lrcPtr->alert_roc_limit ) ) )
   {
      sprintf(alert_roc_limit, "%.2f", lrcPtr->alert_roc_limit);
   }
   XmTextSetString(alert_roc_limitTE, alert_roc_limit);

   memset(alarm_roc_limit, '\0', sizeof(alarm_roc_limit));
   if ( ! ( IsNull ( DOUBLE, ( void * ) &lrcPtr->alarm_roc_limit ) ) )
   {
      sprintf(alarm_roc_limit, "%.2f", lrcPtr->alarm_roc_limit);
   }
   XmTextSetString(alarm_roc_limitTE, alarm_roc_limit);

   rcPtr->item_where_lrcPtr = rc_build_loc_where(lrcPtr);


/*
printf("stvo rcPtr->item_where_lrcPtr == <%s>\n", rcPtr->item_where_lrcPtr);
*/
   
   return;
}


/*
	Creates a "defaultrangecheck" where clause for the specified drcPtr.
*/
char*	rc_build_def_where(DataLimits *drcPtr)
{
   static char	where[200];
   
   /*
   	Build the default where.
   */

   memset(where, '\0', sizeof(where));
   sprintf(where, " WHERE (pe = '%s') AND (dur = %d) AND (monthdaystart = '%s')",
	   drcPtr->pe, drcPtr->dur,  drcPtr->monthdaystart);

   return(where);
}


/*
	Creates a "locrangecheck" where clause for the specified lrcPtr.
*/
char*	rc_build_loc_where(LocDataLimits *lrcPtr)
{
   static char	where[BUFSIZ];
   
   /*
   	Build the location where.
   */
   
   memset(where, '\0', sizeof(where));
   sprintf(where, " WHERE (lid = '%s') AND (pe = '%s') AND (dur = %d) AND (monthdaystart = '%s')",
	   lrcPtr->lid, lrcPtr->pe, lrcPtr->dur,  lrcPtr->monthdaystart);

   return(where);
}


/*
	Loads all possible "Default Ranges data" into the display.
	(NOTE: Assumes that rcPtr->drcPtr has already been set.)
*/

void	rcheck_loaddefault(void)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);
   
   DataLimits*	drcPtr;
   
   char		buf[MAX_BUF_LEN];

   
   XmStringTable	xmStr;
   
   int			cnt,
      			i;

   int			rcdCount;
   
   int			pos = 1; /* by default, show 1st record in list */
   

   /*
   	Sensitize widgets based on availability of data.
   */
   
     rcdCount = recordCount(rcPtr->table, rcPtr->where_phrase);
    
   if (rcdCount > 0)
   {
      rcdata_setSensitivity(True);
   }
   else
   {
      rcdata_setSensitivity(False);
   }
   

   /*
   	Acquire access to the RangeCheck data.
	Loop over all of the appropriate RangeCheck data and
	create XmStrings, and load them into the XmList.
	
	(Validation for inclusion into the XmList is as follows:
	 A data record is considered valid, if and only if
	 it meets the criteria specified by the Filter Parameters.)
   */
   if ((drcPtr = rcPtr->drcPtr) != (DataLimits *) NULL)
   {
      cnt = ListCount(&drcPtr->list);
      xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
      
      if (cnt > 0)
      {
	 i = 0;
	 while (drcPtr != (DataLimits *) NULL)
	 {
	    
	    decode_datalimit_str(drcPtr, buf);
	    xmStr[i] = XmStringCreateSimple(buf);
	    i++;
	    
	    drcPtr = (DataLimits *) ListNext(&drcPtr->node);
	    
	 }
	 	 
	 XmListAddItems(rcmainLI, xmStr, cnt, 1);

	 /*
	 	Select the appropriate position in the rcmainLI.
	 */
	 if (rcPtr->delete_flag == True)  /* DELETE just performed */
	 {
	    pos = rcPtr->rcmainLI_selpos;  /* (use recomputed position) */
	    rcPtr->delete_flag = False;
	    XmListSetPos(rcmainLI, pos);
	    XmListSelectPos(rcmainLI, pos, True);
	 }
	 else if (rcPtr->delete_flag == False)  /* either NEW or NORMAL */
	 {
	    if (rcPtr->item_drcPtr)
	    {
	       rc_select_new_def_item(rcPtr);
	    }
	    else
	    {
	       XmListSetPos(rcmainLI, pos);
	       XmListSelectPos(rcmainLI, pos, True);
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
	Loads all possible "Location Ranges data" into the display.
	(NOTE: Assumes that rcPtr->lrcPtr has already been set.)
*/
void	rcheck_loadlocation(void)
{
   rc_info		*rcPtr = GetDataLimitsInfo(False);
   
   LocDataLimits*	lrcPtr;
   
   char			buf[MAX_BUF_LEN];
   
   XmStringTable	xmStr;
   
   int			cnt = 0,
      			i;
   
   int			rcdCount;
   
   int			pos = 1; /* by default, show 1st record in list */
   
   
   /*
   	Sensitize widgets based on availability of data.
   */
   rcdCount = recordCount(rcPtr->table, rcPtr->where_phrase);
   if (rcdCount > 0)
   {
      rcdata_setSensitivity(True);
   }
   else
   {
      rcdata_setSensitivity(False);
   }
   

   /*
   	Acquire access to the RangeCheck data.
	Loop over all of the appropriate RangeCheck data and
	create XmStrings, and load them into the XmList.
	
	(Validation for inclusion into the XmList is as follows:
	 A data record is considered valid, if and only if
	 it meets the criteria specified by the Filter Parameters.)
   */
   
   if ((lrcPtr = rcPtr->lrcPtr) != (LocDataLimits *) NULL)
   {   
      cnt = ListCount(&lrcPtr->list);
      xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
      
      if (cnt > 0)
      {
	 i = 0;
	 while (lrcPtr != (LocDataLimits *) NULL)
	 {
	    decode_locdatalimit_str(lrcPtr, buf);
	    xmStr[i] = XmStringCreateSimple(buf);
	    i++;
	    
	    lrcPtr = (LocDataLimits *) ListNext(&lrcPtr->node);
	 }
	 
	 XmListAddItems(rcmainLI, xmStr, cnt, 1);

	 
	 /*
	 	Select the appropriate position in the rcmainLI.
	 */
	 if (rcPtr->delete_flag == True)  /* DELETE just performed */
	 {
	    pos = rcPtr->rcmainLI_selpos;  /* (use recomputed position) */
	    rcPtr->delete_flag = False;
	    XmListSetPos(rcmainLI, pos);
	    XmListSelectPos(rcmainLI, pos, True);
	 }
	 else if (rcPtr->delete_flag == False)  /* either NEW or NORMAL */
	 {
	    if (rcPtr->item_lrcPtr)
	    {
	       rc_select_new_loc_item(rcPtr);
	    }
	    else
	    {
	       XmListSetPos(rcmainLI, pos);
	       XmListSelectPos(rcmainLI, pos, True);
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
	rc_select_new_def_item:
	Selects the "Default Ranges item" in the rcmainLI which was just
	applied to the database via a New-Apply operation.
*/	
void	rc_select_new_def_item(rc_info *rcPtr)
{
   DataLimits	*drcPtr = rcPtr->item_drcPtr;
   
   char		dur[10],
      		buf[MAX_BUF_LEN];
   
   XmString	item;
   
   int		pos;
   

   /*
   	Find the record (as indicated by drcPtr) in the
   	rcmainLI if possible.  If present, select it and
   	make sure that it is visible.
   */
   memset(dur, '\0', sizeof(dur));
   memset(buf, '\0', sizeof(buf));
   
   sprintf(dur, "%i", drcPtr->dur);
   
   decode_datalimit_str(drcPtr, buf);

   item = XmStringCreateSimple(buf);

   
   /*
   	Select the position (if present).
   	Free memory.
   */
   if (XmListItemExists(rcmainLI, item))
   {
      pos = XmListItemPos(rcmainLI, item);
      XmListSetPos(rcmainLI, pos);
      XmListSelectPos(rcmainLI, pos, True);
   }
   else
   {
      XmListSetPos(rcmainLI, 1);
      XmListSelectPos(rcmainLI, 1, True);
   }
   XmStringFree(item);
   
   
   return;
}


/*
	rc_select_new_loc_item:
	Selects the "Location Ranges item" in the rcmainLI which was just
	applied to the database via a New-Apply operation.
*/	
void	rc_select_new_loc_item(rc_info *rcPtr)
{
   LocDataLimits	*lrcPtr = rcPtr->item_lrcPtr;
   
   char		dur[10],
      		buf[MAX_BUF_LEN];
   
   XmString	item;
   
   int		pos;
   
   
   /*
   	Find the record (as indicated by lrcPtr) in the
   	rcmainLI if possible.  If present, select it and
   	make sure that it is visible.
   */
   memset(dur, '\0', sizeof(dur));
   memset(buf, '\0', sizeof(buf));
   
   sprintf(dur, "%i", lrcPtr->dur);
   
   decode_locdatalimit_str(lrcPtr, buf);

   item = XmStringCreateSimple(buf);

   
   /*
   	Select the position (if present).
   	Free memory.
   */
   if (XmListItemExists(rcmainLI, item))
   {
      pos = XmListItemPos(rcmainLI, item);
      XmListSetPos(rcmainLI, pos);
      XmListSelectPos(rcmainLI, pos, True);
   }
   else
   {
      XmListSetPos(rcmainLI, 1);
      XmListSelectPos(rcmainLI, 1, True);
   }
   XmStringFree(item);
   
   
   return;
}


rc_info*	GetDataLimitsInfo(int init_flag)
{
   static rc_info	*rcPtr = NULL;
   
   if (init_flag)
   {
      rcPtr = NULL;
   }
   
   if (rcPtr == (rc_info *) NULL)
   {
      rcPtr = (rc_info *) malloc (sizeof(rc_info));
      memset((rcPtr), '\0', sizeof(rc_info));
      
      rcPtr->drcPtr = (DataLimits *) NULL;
      rcPtr->lrcPtr = (LocDataLimits *) NULL;
      rcPtr->item_drcPtr = (DataLimits *) NULL;
      rcPtr->item_lrcPtr = (LocDataLimits *) NULL;
      
      rcPtr->item_where_drcPtr = (char *) NULL;
      rcPtr->item_where_lrcPtr = (char *) NULL;
      
      rcPtr->shefpePtr = (ShefPe *) NULL;
      rcPtr->filterpe_poslist = (int *) NULL;
      rcPtr->filterpe_poslist_okayfree = False;

      rcPtr->data_entry_mode = RC_NORMAL_ENTRY_MODE;
      rcPtr->delete_flag = False;
   }
   return(rcPtr);
}


DataLimits*	rcheck_getdefault(void)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);

   DataLimits	*defrange,
      		*drcPtr;

   /*
   	Free old memory.
	Retrieve new record(s) from the dbms.

	If there is at least 1 record in the dbms, return drcPtr.
	Otherwise, return NULL.
   */
   rcheck_freedefault(rcPtr);

   defrange = GetDataLimits(rcPtr->whereorderby_phrase);
   if (defrange != (DataLimits *) NULL)
   {
      drcPtr = (DataLimits *) ListFirst(&defrange->list);
      return(drcPtr);
   }
      
   return(NULL);
}


LocDataLimits*		rcheck_getlocation(void)
{
   rc_info		*rcPtr = GetDataLimitsInfo(False);
   
   LocDataLimits	*locrange = NULL,
      			*lrcPtr   = NULL;


   /*
   	Free old memory.
   	Retrieve new record(s) from the dbms.

	If there is at least 1 record in the dbms, return lrcPtr.
	Otherwise, return NULL.
   */
   rcheck_freelocation(rcPtr);
   locrange = GetLocDataLimits(rcPtr->whereorderby_phrase);
   if (locrange != (LocDataLimits *) NULL)
   {
      lrcPtr = (LocDataLimits *) ListFirst(&locrange->list);
      return(lrcPtr);
   }
   
   return(NULL);
}


ShefPe*		rcheck_getshefpes(void)
{
   rc_info		*rcPtr = GetDataLimitsInfo(False);
   
   static ShefPe	*shefpe = NULL,
      			*shefpePtr = NULL;

   /*
   	Free old memory.
   	Retrieve new record(s) from the dbms.

	If there is at least 1 record in the dbms, return shefpePtr.
	Otherwise, return NULL.
   */

   rcheck_freeshefpes(rcPtr);
   shefpe = GetShefPe(" ORDER BY pe ");
   if (shefpe != (ShefPe *) NULL)
   {
      shefpePtr = (ShefPe *) ListFirst(&shefpe->list);
      return(shefpePtr);
   }
   
   return(NULL);
}


void		freeDataLimitsInfo(void)
{
   rc_info	*rcPtr = GetDataLimitsInfo(False);
   
   if (rcPtr != (rc_info *) NULL)
   {
      rcheck_freedefault(rcPtr);
      rcheck_freelocation(rcPtr);
      rcheck_freeshefdur(rcPtr);

      rcheck_freeshefpes(rcPtr);
      rcheck_freeposlist(rcPtr);  /* frees filterpe_poslist */
   
      free(rcPtr);
      rcPtr = NULL;
   }
   
   return;
}


void	rcheck_freedefault(rc_info *rcPtr)
{
   if (rcPtr->drcPtr != (DataLimits *) NULL)
   {
      FreeDataLimits(rcPtr->drcPtr);
      rcPtr->drcPtr = NULL;
   }
   
   return;
}


void	rcheck_freelocation(rc_info *rcPtr)
{
   if (rcPtr->lrcPtr != (LocDataLimits *) NULL)
   {
      FreeLocDataLimits(rcPtr->lrcPtr);
      rcPtr->lrcPtr = NULL;
   }
   
   return;
}


void	rcheck_freeshefdur(rc_info *rcPtr)
{
   if (rcPtr->shefdurPtr != (ShefDur *) NULL)
   {
      FreeShefDur(rcPtr->shefdurPtr);
      rcPtr->shefdurPtr = NULL;
   }
   
   return;
}


void	rcheck_freeshefpes(rc_info *rcPtr)
{
   if (rcPtr->shefpePtr != (ShefPe *) NULL)
   {
      FreeShefPe(rcPtr->shefpePtr);
      rcPtr->shefpePtr = NULL;
   }
   
   return;
}


void	rcheck_freeposlist(rc_info *rcPtr)  /* free the filterpe_poslist */
{
   if (rcPtr->filterpe_poslist != (int *) NULL)
   {
      free(rcPtr->filterpe_poslist);
      rcPtr->filterpe_poslist = NULL;
   }
   
   return;
}



void decode_datalimit_str( DataLimits  *drcPtr, char buf[] )
{
   char	dur[10],
      	start[ANSI_TIME_LEN],
      	end[ANSI_TIME_LEN],
	tbuf[20];

	memset(dur,   '\0', sizeof(dur));
	memset(start, '\0', sizeof(start));
	memset(end,   '\0', sizeof(end));
	memset(buf,   '\0', sizeof(buf));
	    
	sprintf(dur, "%i", drcPtr->dur);
	strcpy(start, drcPtr->monthdaystart);
	start[2] = '/';
	strcpy(end, drcPtr->monthdayend);
	end[2] = '/';


	    sprintf(buf, "%13s%7s%7s%8s  ", drcPtr->pe, dur, start, end);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &drcPtr->gross_range_min))
	    	strcat(tbuf,"         ");
	    else
	    	sprintf(tbuf,"%9.1f",drcPtr->gross_range_min);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &drcPtr->gross_range_max))
	    	strcat(tbuf,"          ");
	    else
	    	sprintf(tbuf,"%10.1f",drcPtr->gross_range_max);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &drcPtr->reason_range_min))
	    	strcat(tbuf,"        ");
	    else
	    	sprintf(tbuf,"%8.1f",drcPtr->reason_range_min);
	    	/*sprintf(tbuf,"%10.1f",drcPtr->reason_range_min);*OLD*/
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &drcPtr->reason_range_max))
	    	strcat(tbuf,"        ");
	    else
	    	sprintf(tbuf,"%8.1f",drcPtr->reason_range_max);
	    	/*sprintf(tbuf,"%10.1f",drcPtr->reason_range_max);*OLD*/
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &drcPtr->roc_max))
	    	strcat(tbuf,"      ");
	    else
	    	sprintf(tbuf,"%6.1f",drcPtr->roc_max);
	    	/*sprintf(tbuf,"%11.1f",drcPtr->roc_max);*OLD*/
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &drcPtr->alert_upper_limit))
	    	strcat(tbuf,"       ");
	    else
	    	sprintf(tbuf,"%7.1f",drcPtr->alert_upper_limit);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &drcPtr->alert_lower_limit))
	    	strcat(tbuf,"       ");
	    else
	    	sprintf(tbuf,"%7.1f",drcPtr->alert_lower_limit);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &drcPtr->alert_roc_limit))
	    	strcat(tbuf,"      ");
	    else
	    	sprintf(tbuf,"%6.1f",drcPtr->alert_roc_limit);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &drcPtr->alert_diff_limit))
	    	strcat(tbuf,"     ");
	    else
	    	sprintf(tbuf,"%5.1f",drcPtr->alert_diff_limit);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &drcPtr->alarm_upper_limit))
	    	strcat(tbuf,"      ");
	    else
	    	sprintf(tbuf,"%6.1f",drcPtr->alarm_upper_limit);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &drcPtr->alarm_lower_limit))
	    	strcat(tbuf,"     ");
	    else
	    	sprintf(tbuf,"%5.1f",drcPtr->alarm_lower_limit);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &drcPtr->alert_roc_limit))
	    	strcat(tbuf,"      ");
	    else
	    	sprintf(tbuf,"%6.1f",drcPtr->alarm_roc_limit);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &drcPtr->alarm_diff_limit))
	    	strcat(tbuf,"      ");
	    else
	    	sprintf(tbuf,"%6.1f",drcPtr->alarm_diff_limit);
	    strcat(buf,tbuf);

}



void decode_locdatalimit_str( LocDataLimits  *lrcPtr, char buf[] )
{
   char	dur[10],
      	start[ANSI_TIME_LEN],
      	end[ANSI_TIME_LEN],
	tbuf[20];

	memset(dur,   '\0', sizeof(dur));
	memset(start, '\0', sizeof(start));
	memset(end,   '\0', sizeof(end));
	memset(buf,   '\0', sizeof(buf));
	    
	sprintf(dur, "%i", lrcPtr->dur);
	strcpy(start, lrcPtr->monthdaystart);
	start[2] = '/';
	strcpy(end, lrcPtr->monthdayend);
	end[2] = '/';


	    sprintf(buf, "%-10s%3s%7s%7s%8s  ", lrcPtr->lid,lrcPtr->pe, dur, start, end);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &lrcPtr->gross_range_min))
	    	strcat(tbuf,"         ");
	    else
	    	sprintf(tbuf,"%9.1f",lrcPtr->gross_range_min);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &lrcPtr->gross_range_max))
	    	strcat(tbuf,"          ");
	    else
	    	sprintf(tbuf,"%10.1f",lrcPtr->gross_range_max);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &lrcPtr->reason_range_min))
	    	strcat(tbuf,"        ");
	    else
	    	sprintf(tbuf,"%8.1f",lrcPtr->reason_range_min);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &lrcPtr->reason_range_max))
	    	strcat(tbuf,"        ");
	    else
	    	sprintf(tbuf,"%8.1f",lrcPtr->reason_range_max);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &lrcPtr->roc_max))
	    	strcat(tbuf,"      ");
	    else
	    	sprintf(tbuf,"%6.1f",lrcPtr->roc_max);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &lrcPtr->alert_upper_limit))
	    	strcat(tbuf,"       ");
	    else
	    	sprintf(tbuf,"%7.1f",lrcPtr->alert_upper_limit);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &lrcPtr->alert_lower_limit))
	    	strcat(tbuf,"       ");
	    else
	    	sprintf(tbuf,"%7.1f",lrcPtr->alert_lower_limit);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &lrcPtr->alert_roc_limit))
	    	strcat(tbuf,"      ");
	    else
	    	sprintf(tbuf,"%6.1f",lrcPtr->alert_roc_limit);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &lrcPtr->alert_diff_limit))
	    	strcat(tbuf,"     ");
	    else
	    	sprintf(tbuf,"%5.1f",lrcPtr->alert_diff_limit);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &lrcPtr->alarm_upper_limit))
	    	strcat(tbuf,"      ");
	    else
	    	sprintf(tbuf,"%6.1f",lrcPtr->alarm_upper_limit);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &lrcPtr->alarm_lower_limit))
	    	strcat(tbuf,"     ");
	    else
	    	sprintf(tbuf,"%5.1f",lrcPtr->alarm_lower_limit);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &lrcPtr->alarm_roc_limit))
	    	strcat(tbuf,"      ");
	    else
	    	sprintf(tbuf,"%6.1f",lrcPtr->alarm_roc_limit);
	    strcat(buf,tbuf);

	    memset(tbuf,   '\0',sizeof(tbuf));
	    if( IsNull(DOUBLE, (void*) &lrcPtr->alarm_diff_limit))
	    	strcat(tbuf,"      ");
	    else
	    	sprintf(tbuf,"%6.1f",lrcPtr->alarm_diff_limit);
	    strcat(buf,tbuf);

}


