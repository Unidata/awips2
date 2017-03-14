/*
	File:		purgedata_show.c
	Purpose:	Provide support for PurgeParameters DS.
*/


/************************************************************************
   
   Functions to handle the setup control of purge data information.
      
   
   ***********************************************************************/

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/List.h>
#include <Xm/Text.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "time_convert.h"

#include "PurgeDynData.h"
#include "PurgeProduct.h"

#include "purgedata.h"
#include "purgedata_show.h"

#include "DbmsUtils.h"
#include "Xtools.h"



/* variables global to this file */

PurgeDynData	*peHead;
PurgeProduct	*prodHead;

#define purgelist_formatstr "%-18s %-8s (%5ld)  %-8s (%5ld)  %-s"



/************************************************************************
   
   Load the purge data dialog shell.
   
   ***********************************************************************/

void purgedata_show(Widget 	w)
{
   if (! purgedataDS)
   {      
      /* create the dialog shell and add the callbacks */
      
      create_purgedataDS(GetTopShell(w));
      add_purgedata_cbs();      
   }
   
   
   /* manage the windows now before doing the selections */   

   if (! XtIsManaged(purgedataDS))
   {
      XtManageChild(purgedataFO);
      XtManageChild(purgedataDS);
      
      
      /* initialize data memory */
      
      peHead    = (PurgeDynData *)(NULL);
      prodHead  = (PurgeProduct *)(NULL);
      
      
      /* load each of the scrolled lists */
      
      load_purgepe_list();
      load_purgeprod_list();
      
      
      /* select the first item in the lists with the callback */
      
      XmListSetPos(purgepeLS, 1);
      XmListSelectPos(purgepeLS,   1, 1);
      
      XmListSetPos(purgeprodLS, 1);
      XmListSelectPos(purgeprodLS, 1, 1);
   }
   
   
   return;
}


/************************************************************************
    
   Add the stations selection callbacks.
   
   ************************************************************************/

void add_purgedata_cbs()
{
   
   Atom	wmAtom;
   
   
   /* callbacks on scrolled list of products */
   
   XtAddCallback(purgepeLS, XmNdefaultActionCallback,   load_purgepe_textCB,
		 NULL);
   XtAddCallback(purgepeLS, XmNbrowseSelectionCallback, load_purgepe_textCB,
		 NULL);

   XtAddCallback(purgeprodLS, XmNdefaultActionCallback,   load_purgeprod_textCB,
		 NULL);
   XtAddCallback(purgeprodLS, XmNbrowseSelectionCallback, load_purgeprod_textCB,
		 NULL);

     
   /* callbacks on add, update, delete pushbuttons */
   
   XtAddCallback(purgepe_updatePB, XmNactivateCallback, purgepe_updateCB, NULL);   

   XtAddCallback(purgeprod_addPB,    XmNactivateCallback, purgeprod_addCB, NULL);
   XtAddCallback(purgeprod_updatePB, XmNactivateCallback, purgeprod_updateCB,
		 NULL);
   XtAddCallback(purgeprod_deletePB, XmNactivateCallback, purgeprod_deleteCB,
		 NULL);

      
   /* callbacks on atom widget */   
   
   wmAtom = XmInternAtom(XtDisplay(purgedataDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(purgedataDS, wmAtom, ok_purgedataCB, NULL);
   
   XtAddCallback(purge_okPB, XmNactivateCallback, ok_purgedataCB, NULL);

   
   /* add TextFilter callbacks */
   XtAddCallback(purgepe_hrsTX,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtAddCallback(purgeprod_idTX, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
   XtAddCallback(purgeprod_verTX,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   
   
   return;
}


/************************************************************************
   
   Load the scrolled lists for purge pe table info.
   
   ***********************************************************************/

void load_purgepe_list()
{
   XmStringTable        xmStr;
   Arg                  arg[10];
   int                  ac, i;
   int			cnt;
   char			liststr[80];
   PurgeDynData		*pePtr = NULL;
   char 		where[60];
   char 		dayshost_str[10], daysbackup_str[10];
   int			num_days, num_hours;
   
   
   /* free any memory allocated */
   
   if (peHead != NULL)
   {
      FreePurgeDynData(peHead);
      peHead = (PurgeDynData *)NULL;
   }
   
   
   /* load the list of tables to purge data from the database */
   
   sprintf(where, " ORDER BY time_column_name, table_name ");   
   peHead = GetPurgeDynData(where);
   
   if (peHead != NULL)
      cnt = ListCount(&peHead->list);
   else
      cnt = 0;
   
   
   /* allocate the Motif list strings */
   
   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
   
   
   /* load the strings into the Motif strings */
   
   if (peHead != NULL) pePtr = (PurgeDynData *) ListFirst(&peHead->list);
   
   for (i = 0; i < cnt; i++)
   {
      num_days   = pePtr->num_hours_host / 24;
      num_hours  = pePtr->num_hours_host % 24;
      sprintf(dayshost_str, "%5d/%2d", num_days, num_hours);
      
      num_days   = pePtr->num_hours_backup / 24;
      num_hours  = pePtr->num_hours_backup % 24;
      sprintf(daysbackup_str, "%5d/%2d", num_days, num_hours);
      
      
      /* load the formatted list item.  note that the list is also loaded 
         in purgedata_selectpe(), and the format used for both should agree */
      
      sprintf(liststr, purgelist_formatstr,
	      pePtr->table_name,  
	      dayshost_str,   pePtr->num_hours_host,
	      daysbackup_str, pePtr->num_hours_backup,
	      pePtr->time_column_name);
      xmStr[i] = XmStringCreateSimple(liststr);
      
      pePtr = (PurgeDynData *) ListNext(&pePtr->node);
   }
   
   
   /* load the list in the scrolled list */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(purgepeLS, arg, ac);
   
      
   /* free the memory */
   
   for (i = 0; i < cnt; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);
        
   
   return;
}


/************************************************************************
   
   Load the scrolled lists for purge of products.
   
   ***********************************************************************/

void load_purgeprod_list()
{
   XmStringTable        xmStr;
   Arg                  arg[10];
   int                  ac, i;
   int			cnt;
   char			liststr[80];
   PurgeProduct 	*prodPtr;
   char 		where[60];
   char			post_ansitime[ANSI_TIME_LEN];
   char			prod_ansitime[ANSI_TIME_LEN];
   time_t		post_timet, prod_timet;
   int			status;
   
   
   /* free any memory allocated */
   
   if (prodHead != NULL)
   {
      FreePurgeProduct(prodHead);
      prodHead = (PurgeProduct *)NULL;
   }
   
   
   /* load the list of products from the database */
   
   sprintf(where, " ORDER BY product_id ");   
   prodHead = GetPurgeProduct(where);
   
   if (prodHead != NULL)
      cnt = ListCount(&prodHead->list);
   else
      cnt = 0;
   
   
   /* allocate the Motif list strings */
   
   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
   
   
   /* load the strings into the Motif strings.
      use the same string format for the select function */
   
   prodPtr = (PurgeProduct *) ListFirst(&prodHead->list);
   for (i = 0; i < cnt; i++)
   {
      status = yearsec_dt_to_timet(prodPtr->producttime, &prod_timet);      
      if (prod_timet == 0)
	 strcpy(prod_ansitime, "N/A");
      else
	 status = yearsec_dt_to_ansi(prodPtr->producttime, prod_ansitime);
      
      status = yearsec_dt_to_timet(prodPtr->postingtime, &post_timet);
      if (post_timet == 0)
	 strcpy(post_ansitime, "N/A");
      else
	 status = yearsec_dt_to_ansi(prodPtr->postingtime, post_ansitime);
	 
	 	 
      sprintf(liststr, "%-10s      %6ld   %-19s  %-19s",
	      prodPtr->product_id, prodPtr->num_versions,
	      prod_ansitime, post_ansitime);
      xmStr[i] = XmStringCreateSimple(liststr);
      
      prodPtr = (PurgeProduct *) ListNext(&prodPtr->node);
   }
   
   
   /* load the list in the scrolled list */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(purgeprodLS, arg, ac);
  
   
   
   
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

void load_purgepe_textCB()
{
   PurgeDynData	*pePtr;
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   char		numstr[10];
   
   
   /* determine which item is selected */
   
   XmListGetSelectedPos(purgepeLS, &listitems, &listcnt);
   itemnum = listitems[0];
   free(listitems);
   
   
   /* get the data table info and display it */
   
   pePtr = (PurgeDynData *) ListNth(&peHead->list, itemnum);
   if (pePtr != NULL)
   {      
      XmTextSetString(purgepe_datasetTX, pePtr->table_name);

      XmTextSetString(purgepe_timefldTX, pePtr->time_column_name);
      
      sprintf(numstr, "%ld",  pePtr->num_hours_host);
      XmTextSetString(purgepe_hrsTX, numstr);
      
      sprintf(numstr, "%ld",  pePtr->num_hours_backup);
      XmTextSetString(purgepe_hrsbackupTX, numstr);
  }
   
   else
      fprintf(stderr, "Error getting item from PurgeDynData list.\n");
   
   
   return;
}


/************************************************************************
   
   Callback for loading (or attempting) to load text of selected
   item into text fields.
   
   **********************************************************************/

void load_purgeprod_textCB()
{
   PurgeProduct	*prodPtr;
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   char		numstr[10];
   
   
   /* determine which item is selected */
   
   XmListGetSelectedPos(purgeprodLS, &listitems, &listcnt);
   itemnum = listitems[0];
   free(listitems);
   
   
   /* get the product text and display it */
   
   prodPtr = (PurgeProduct *) ListNth(&prodHead->list, itemnum);
   if (prodPtr != NULL)
   {      
      XmTextSetString(purgeprod_idTX,      prodPtr->product_id);
      
      sprintf(numstr, "%ld",  prodPtr->num_versions);
      XmTextSetString(purgeprod_verTX, numstr);
   }
   
   else
      fprintf(stderr, "Error getting item from PurgeProduct list.\n");
   
   
   
   return;
}


/************************************************************************
   
  Update the entered item into the scrolled list.
   
   **********************************************************************/

void purgepe_updateCB()
{
   
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   PurgeDynData peinfo;
   PurgeDynData	*pePtr;
   int 		status;
   char		where[80];
   char		msgstr[80];
   
   
   /* determine which item is selected so as to 
      build the key for the record to update */
   
   XmListGetSelectedPos(purgepeLS, &listitems, &listcnt);
   
   if (listcnt == 0)
   {
      sprintf(msgstr, "Update failed.  No item selected from list");      
      ErrorDialog(purgedataDS, msgstr);
      return;
   }
   
   else
   {
      itemnum = listitems[0];
      free(listitems);
   }
      
   
   pePtr = (PurgeDynData *) ListNth(&peHead->list, itemnum);
   if (pePtr != NULL)
   {
      /* get the new items from the text fields */
      
      read_pe_info(&peinfo);
      
      
      /* now update the info using the key from the item that
	 was selected and the data below.  note that it may attempt
	 to update the key fileds. */
      
      sprintf(where, " WHERE table_name = '%s' ", pePtr->table_name);
      status = UpdatePurgeDynData(&peinfo, where);
      
      if (status < 0)
	 fprintf(stderr, "Error %d updating data into PurgeDynData table\n",
		 status);
      
      
      /* update the display of the list */
      
      else
      {
	 load_purgepe_list();
	 purgedata_selectpe(&peinfo);
      }
   }
   
   else
      fprintf(stderr, "Error getting item to update from PurgeDynData list.\n");
   
    
   
   
   return;
}


/************************************************************************
   
   Add an item to the scrolled list.
   
   **********************************************************************/

void purgeprod_addCB()
{
   
   PurgeProduct 	prodinfo;
   int 			status;
   char			msgstr[80];
   
   
   /* get the items from the text fields */
   
   read_prod_info(&prodinfo);
   
   
   /* now add the info */
   
   status = PutPurgeProduct(&prodinfo);
   
   if (status < 0)
   {
      if ((status == -239) || (status == -268))
	 sprintf(msgstr,
		 "Information not added. \nSpecified product already defined.");
      else if (status == -391)
	 sprintf(msgstr,
		 "Information not added. \nNo product specified.");
      else
	 sprintf(msgstr, "Add of record failed; err= %d", status);
      
      ErrorDialog(purgedataDS, msgstr);
   }
   
   
   /* update the display of the list */
   
   else
   {
      load_purgeprod_list();
      purgedata_selectprod(&prodinfo);
   }
   
   
   return;
}


/************************************************************************
   
  Update the entered item into the scrolled list.
   
   **********************************************************************/

void purgeprod_updateCB()
{
   
   int 			*listitems;
   int 			listcnt;
   int			itemnum;
   PurgeProduct 	prodinfo;
   PurgeProduct		*prodPtr;
   int 			status;
   char			where[80];
   char			msgstr[80];
   
   
   /* determine which item is selected so as to 
      build the key for the record to update */
   
   XmListGetSelectedPos(purgeprodLS, &listitems, &listcnt);
   
   if (listcnt == 0)
   {
      sprintf(msgstr, "Update failed.  No item selected from list");      
      ErrorDialog(purgedataDS, msgstr);
      return;
   }
   
   else
   {
      itemnum = listitems[0];
      free(listitems);
   }
   
   
   
   prodPtr = (PurgeProduct *) ListNth(&prodHead->list, itemnum);
   if (prodPtr != NULL)
   {
      /* get the new items from the text fields  */
      
      read_prod_info(&prodinfo);
      
      
      /* now update the info using the key from the item that
	 was selected and the data below.  note that it may attempt
	 to update the key fileds. */
      
      sprintf(where, " WHERE product_id = '%s' ",
	      prodPtr->product_id);
      status = UpdatePurgeProduct(&prodinfo, where);
      
      if (status < 0)
	 fprintf(stderr, "Error %d updating data into PurgeProduct table\n",
		 status);
      
      
      /* update the display of the list */
      
      else
      {
	 load_purgeprod_list();
	 purgedata_selectprod(&prodinfo);
      }
   }
   
   else
      fprintf(stderr, "Error getting item to update from PurgeProduct list.\n");
   
   return;
}


/************************************************************************
   
   Delete an item from the scrolled list.
   
   **********************************************************************/

void purgeprod_deleteCB()
{
   
   int 			*listitems;
   int 			listcnt;
   int			itemnum;
   PurgeProduct 	*prodPtr;
   int 			status;
   char			where[60];
   char			msgstr[80];
   
   
   SetCursor(purgedataFO, XC_watch);
   
   
   /* determine which item is selected */
   
   XmListGetSelectedPos(purgeprodLS, &listitems, &listcnt);
   if (listcnt == 0)
   {
      UnsetCursor(purgedataFO);
      sprintf(msgstr, "Delete failed.  No item selected from list");      
      ErrorDialog(purgedataDS, msgstr);
      return;
   }
   
   else
   {
      itemnum = listitems[0];
      free(listitems);
   }
   
   
   prodPtr = (PurgeProduct *) ListNth(&prodHead->list, itemnum);
   if (prodPtr != NULL)
   {      
      sprintf(where, " WHERE product_id = '%s' ", prodPtr->product_id);
      
      status = DeletePurgeProduct(where);
      
      if (status < 0)
      {	 
	 sprintf(msgstr,
		 "Delete failed; err= %d", status); 	 
	 ErrorDialog(purgedataDS, msgstr);
      }
      
      
      /* update the display of the list */
      
      else
      {
	 load_purgeprod_list();
	 XmListSetPos(purgeprodLS, itemnum);
	 XmListSelectPos(purgeprodLS, itemnum, 1);
      }
   }
   
   else
      fprintf(stderr, "Error getting item to delete from PurgeProduct list.\n");
   

   
   UnsetCursor(purgedataFO);
   
   
   return;
}



/************************************************************************
   
   Get the info from the text fields.
   
   **********************************************************************/

void read_pe_info(PurgeDynData *peinfo)
{
   char 		*valstr;
   
   
   /* get the value of the displayed text. */
   
   valstr = XmTextGetString(purgepe_datasetTX);
   strcpy(peinfo->table_name, valstr);
   XtFree(valstr);
   
   valstr = XmTextGetString(purgepe_hrsTX);
   sscanf(valstr, "%ld", &peinfo->num_hours_host);
   XtFree(valstr);
   
   valstr = XmTextGetString(purgepe_hrsbackupTX);
   sscanf(valstr, "%ld", &peinfo->num_hours_backup);
   XtFree(valstr);
   
   valstr = XmTextGetString(purgepe_timefldTX);
   strcpy(peinfo->time_column_name, valstr);
   XtFree(valstr);

   return;   
}


/************************************************************************
   
   Get the info from the text fields.
   
   **********************************************************************/

void read_prod_info(PurgeProduct *prodinfo)
{
   char 		*valstr;
   int 			status;
   PurgeProduct		*prodPtr;
   char			where[80];
   
   
   /* get the values from the window */
   
   valstr = XmTextGetString(purgeprod_idTX);
   strcpy(prodinfo->product_id, valstr);
   XtFree(valstr);
   
   valstr = XmTextGetString(purgeprod_verTX);
   sscanf(valstr, "%ld", &prodinfo->num_versions);
   XtFree(valstr);
     
   
   /* get the value of the undisplayed producttime, postingtime fields.
      if new record, then initialize */
   
   sprintf(where, " WHERE product_id = '%s' ", prodinfo->product_id);
   
   prodPtr = GetPurgeProduct(where);
   
   if (prodPtr != NULL)
   {
      prodinfo->producttime = prodPtr->producttime;
      prodinfo->postingtime = prodPtr->postingtime;
      
      FreePurgeProduct(prodPtr);
   }
   
   else
   {
      status = timet_to_yearsec_dt(0, &prodinfo->producttime);
      status = timet_to_yearsec_dt(0, &prodinfo->postingtime);
   }
   
   
   return;   
}


/************************************************************************
   
   Select the entry currently being considered.
   The format of this string must agree with how the list
   was loaded.  sloppy, inherited program style.
   
   **********************************************************************/

void purgedata_selectpe(PurgeDynData *pePtr)
{
   char			liststr[80];
   XmString		item;
   int			pos;
   char 		days_str[10], daysbackup_str[10];
   int			num_days, num_hours;
   
   
   /* only need to update two hours fields, since that is all
      the interface support for updating. */
   
   num_days   = pePtr->num_hours_host / 24;
   num_hours  = pePtr->num_hours_host % 24;
   sprintf(days_str, "%5d/%2d", num_days, num_hours);\
   
   num_days   = pePtr->num_hours_backup / 24;
   num_hours  = pePtr->num_hours_backup % 24;
   sprintf(daysbackup_str, "%5d/%2d", num_days, num_hours);


   /* load string in same way as formatted originally */
   
   sprintf(liststr, purgelist_formatstr,
	   pePtr->table_name,  
	   days_str, pePtr->num_hours_host,
	   daysbackup_str, pePtr->num_hours_backup,
	   pePtr->time_column_name);
   item = XmStringCreateSimple(liststr);
   
   
   /* update and select the string */
   
   if (XmListItemExists(purgepeLS, item))
   {
      pos = XmListItemPos(purgepeLS, item);
      XmListSetPos(purgepeLS, pos);
      XmListSelectPos(purgepeLS, pos, True);
   }
   else
   {
      XmListSetPos(purgepeLS, 1);
      XmListSelectPos(purgepeLS, 1, True);
   }
   XmStringFree(item);

   return;
}


/************************************************************************
   
   Select the entry currently being considered.
   The format of this string must agree with how the list
   was loaded.
   
   **********************************************************************/

void purgedata_selectprod(PurgeProduct *prodPtr)
{
   char			liststr[80];
   XmString		item;
   int			pos;
   char			post_ansitime[ANSI_TIME_LEN];
   char			prod_ansitime[ANSI_TIME_LEN];
   int 			status;
   
   
   status = yearsec_dt_to_ansi(prodPtr->producttime, prod_ansitime);
   status = yearsec_dt_to_ansi(prodPtr->postingtime, post_ansitime);
   
   sprintf(liststr, "%-10s      %6ld   %-19s  %-19s",
	   prodPtr->product_id, prodPtr->num_versions,
	   prod_ansitime, post_ansitime);
   
   item = XmStringCreateSimple(liststr);
   
   if (XmListItemExists(purgeprodLS, item))
   {
      pos = XmListItemPos(purgeprodLS, item);
      XmListSetPos(purgeprodLS, pos);
      XmListSelectPos(purgeprodLS, pos, True);
   }
   else
   {
      XmListSetPos(purgeprodLS, 1);
      XmListSelectPos(purgeprodLS, 1, True);
   }
   XmStringFree(item);
   
   return;
}


/************************************************************************
   
   Free any memory allocated for the data.
   
   **********************************************************************/

void free_purgedata()
{
   
   if (peHead != NULL)
   {
      FreePurgeDynData(peHead);
      peHead = (PurgeDynData *)NULL;
   }
   
   if (prodHead != NULL)
   {
      FreePurgeProduct(prodHead);
      prodHead = (PurgeProduct *)NULL;
   }
   
   
   
   return;
}


/************************************************************************
   
   Close the window.
   
   **********************************************************************/

void ok_purgedataCB()
{
   if (XtIsManaged(purgedataDS))
   {
      XtDestroyWidget(purgedataDS);
      purgedataDS = NULL;
   }

   
   /* free any allocated memory */
   
   free_purgedata();
   
   return;
}




