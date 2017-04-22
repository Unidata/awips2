/*
	File:		radarloc_show.c
	
	Purpose:	Provides support for the RadarLocations DS.
*/


/************************************************************************
   
   Functions to handle the setup control of Radar Location information.
      
   
   ***********************************************************************/

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "RadarLoc.h"

#include "radarloc.h"
#include "radarloc_show.h"

#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "Xtools.h"

#include "hybase.h"
#include "hybase_utils.h"
#include "cvt_latlon.h"


/* variables global to this file */

RadarLoc	*rlocHead;

char		rloc_format_string[] =
"%-3s %3.3d  %-1s    %-20s    %-2s   %9s   %10s  %10s  %6s   %c      %-3s";




/************************************************************************
   
   Load the radar locations dialog shell.
   
   ***********************************************************************/

void	radarloc_show(Widget w)
{
   if (! radarlocDS)
   {
      /* create the dialog shell and add the callbacks */
      
      create_radarlocDS(GetTopShell(w));
      add_radarloc_cbs();      
   }
   
   
   /* manage the windows now before doing the selections */   
   
   if (! XtIsManaged(radarlocDS))
   {
      XtManageChild(radarlocFO);
      XtManageChild(radarlocDS);
      
      
      /* initialize data memory */

      rlocHead  = (RadarLoc *)(NULL);
      
      
      /* load the scrolled list */

      load_radarloc_list();
   }
   
   
   return;
}


/************************************************************************
    
   Add the stations selection callbacks.
   
   ************************************************************************/

void	add_radarloc_cbs()
{
   
   Atom	wmAtom;
   
   
   /* callbacks on scrolled list of products */
   
   XtAddCallback(radarlocLS, XmNdefaultActionCallback,   load_radarloc_textCB, NULL);
   XtAddCallback(radarlocLS, XmNsingleSelectionCallback, load_radarloc_textCB, NULL);
   
   
   /* callbacks on add, update, delete pushbuttons */
   
   XtAddCallback(radaddPB,    XmNactivateCallback, radarloc_addCB,    NULL);
   XtAddCallback(radupdatePB, XmNactivateCallback, radarloc_updateCB, NULL);
   XtAddCallback(raddeletePB, XmNactivateCallback, radarloc_deleteCB, NULL);

      
   /* callbacks on atom widget */   
   
   wmAtom = XmInternAtom(XtDisplay(radarlocDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(radarlocDS, wmAtom, ok_radarlocCB, NULL);
   
   XtAddCallback(radarloc_okPB, XmNactivateCallback, ok_radarlocCB, NULL);

   
   /* callbacks for TextFilter */   

   add_radarloc_textfilter_cbs();
   
   
   return;
}


void	add_radarloc_textfilter_cbs()
{
   XtAddCallback(radidTX,    XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
   XtAddCallback(radnumTX,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtAddCallback(radstateTX, XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter, (XtPointer)UPPERCASE);
   
   XtAddCallback(radlatTX, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
				(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));
   XtAddCallback(radlonTX, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
				(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));

   XtAddCallback(radelevTX, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(radheightTX, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtAddCallback(radbiasTX, XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter, (XtPointer)UPPERCASE);
   
   return;
}


void	remove_radarloc_textfilter_cbs()
{
   XtRemoveCallback(radidTX,    XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
   XtRemoveCallback(radnumTX,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtRemoveCallback(radstateTX, XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter, (XtPointer)UPPERCASE);
   
   XtRemoveCallback(radlatTX, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
				(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));
   XtRemoveCallback(radlonTX, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
				(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));

   XtRemoveCallback(radelevTX, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtRemoveCallback(radheightTX, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
   XtRemoveCallback(radbiasTX, XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter, (XtPointer)UPPERCASE);
   
   return;
}


/************************************************************************
   
   Load the scrolled lists for radarloc table info.
   
   ***********************************************************************/

void	load_radarloc_list()
{
   XmStringTable        xmStr;
   Arg                  arg[10];
   int                  ac, i;
   int			cnt;
   char			liststr[200];
   RadarLoc		*rlocPtr;
   char 		where[80];
   char			lat[11];
   char			lon[11];
   char			elev[11];
   char			height[9];
   
   
   /* free any memory allocated */
   
   free_radarloc();
   
   
   /* clear out everything & load the list from the database */
   
   XmListDeleteAllItems(radarlocLS);
   sprintf(where, " WHERE radid != 'ZZZ' ORDER BY radid ");   
   rlocHead = GetRadarLoc(where);

   
   cnt = 0;
   if (rlocHead != NULL)
   {
      cnt = ListCount(&rlocHead->list);
   }
   
   if (cnt == 0)
   {
      return;
   }   



   /* allocate the Motif list strings */
   
   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
   
   
   /* load the strings into the Motif strings */
   
   rlocPtr = (RadarLoc *) ListFirst(&rlocHead->list);
   for (i = 0; i < cnt; i++)
   {
      memset(&lat, '\0', sizeof(lat));
      memset(&lon, '\0', sizeof(lon));
      if ((! IsNull(DOUBLE, (void*) &rlocPtr->lat)) &&
	  (! IsNull(DOUBLE, (void*) &rlocPtr->lon)))
      {
	 strcpy(lat, cvt_latlon_from_double(rlocPtr->lat));
	 strcpy(lon, cvt_latlon_from_double(rlocPtr->lon));
      }
      else
      {
	 strcpy(lat, cvt_latlon_from_double(0.0));
	 strcpy(lon, cvt_latlon_from_double(0.0));
      }
      
      DataToString(&rlocPtr->elev, DOUBLE, elev, "%.1lf", "");

      DataToString(&rlocPtr->tower_ht, DOUBLE, height, "%.1lf", "");


      if(IsNull(SHORT, (void*) &rlocPtr->radar_num)) rlocPtr->radar_num = 0;

      sprintf(liststr, rloc_format_string,
	      rlocPtr->radid, rlocPtr->radar_num, rlocPtr->radid_prefix,
	      rlocPtr->name, rlocPtr->state,
	      lat, lon, elev, height, rlocPtr->use_radar[0],
              rlocPtr->office_id);
      
      xmStr[i] = XmStringCreateSimple(liststr);
      
      rlocPtr = (RadarLoc *) ListNext(&rlocPtr->node);
   }
   
   
   /* load the list in the scrolled list */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(radarlocLS, arg, ac);
   DeSensitize(radupdatePB);
   DeSensitize(raddeletePB);
   
   
   /* free the memory */
   
   for (i = 0; i < cnt; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);

   
   return;
}


/************************************************************************
   
   Callback for loading (or attempting) to load text of selected
   item into data fields.
   
   **********************************************************************/

void	load_radarloc_textCB()
{
   RadarLoc	*rlocPtr;
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   char		elev[11];
   char		height[9];
   char		rad_num[4];
   
   
   remove_radarloc_textfilter_cbs();
   
   
   /* determine which item is selected */
   
   XmListGetSelectedPos(radarlocLS, &listitems, &listcnt);
   
   
   /* get the data table info and display it */
   
   if (listcnt > 0)
   {
      itemnum = listitems[0];
      rlocPtr = (RadarLoc *) ListNth(&rlocHead->list, itemnum);
      if (rlocPtr != NULL)
      {
	 /* sets a pointer to the newly-selected radar */
	 
	 (void) radarloc_current_getRadarPtr(rlocPtr);

	 
	 if (rlocPtr->use_radar[0] == 'T')
	    XmToggleButtonSetState(radactiveTB, True, False);
	 else
	    XmToggleButtonSetState(radactiveTB, False, False);

	 
	 XmTextSetString(radidTX,       rlocPtr->radid);
	 DataToString(&rlocPtr->radar_num, SHORT, rad_num, "%3d", "");
	 XmTextSetString(radnumTX,        rad_num);
	 XmTextSetString(radidprefixTX, rlocPtr->radid_prefix);
	 XmTextSetString(radnameTX,     rlocPtr->name);
	 XmTextSetString(radstateTX,    rlocPtr->state);
         XmTextSetString(radbiasTX, rlocPtr->office_id);
	 
	 if ((! IsNull(DOUBLE, (void*) &rlocPtr->lat)) &&
	     (! IsNull(DOUBLE, (void*) &rlocPtr->lon)))
	 {
	    XmTextSetString(radlatTX, cvt_latlon_from_double(rlocPtr->lat));
	    XmTextSetString(radlonTX, cvt_latlon_from_double(rlocPtr->lon));
	 }
	 else
	 {
	    XmTextSetString(radlatTX, cvt_latlon_from_double(0.0));
	    XmTextSetString(radlonTX, cvt_latlon_from_double(0.0));
	 }
	 
	 DataToString(&rlocPtr->elev, DOUBLE, elev, "%5.1lf", "");
	 XmTextSetString(radelevTX, elev);

	 DataToString(&rlocPtr->tower_ht, DOUBLE, height, "%5.1lf", "");
	 XmTextSetString(radheightTX, height);
      }
      
      else
	 fprintf(stderr, "Error getting item from RadarLoc list.\n");
      
      
      free(listitems);
   }
   
   else
   {
      clearForm(radarlocradFO);
   }

   
   if (ListRsrcGetSelectedCount(radarlocLS))
   {
      Sensitize(radupdatePB);
      Sensitize(raddeletePB);
   }
   else
   {
      DeSensitize(radupdatePB);
      DeSensitize(raddeletePB);
   }
   

   add_radarloc_textfilter_cbs();


   return;
}


/************************************************************************
   
   Add an item to the scrolled list.
   
   **********************************************************************/

void	radarloc_addCB()
{
   RadarLoc 	rlocInfo;
   int 		status;
   char		msgstr[80];
   int		rv = 0;
   
   
   SetCursor(radarlocFO, XC_watch);
   
   
   /* get the items from the text fields */
   
   rv = read_radarloc_info(&rlocInfo);  /* 0==failure, 1==success */
   if (rv == 0)
   {
      UnsetCursor(radarlocFO);
      return;

   }
   
   
   /* now add the info */
   
   status = PutRadarLoc(&rlocInfo);
   
   if (status < 0)
   {
      if ((status == -239) || (status == -268))
	 sprintf(msgstr,
		 "Information not added. \nSpecified record already defined.");
      else if (status == -391)
	 sprintf(msgstr,
		 "Information not added. \nMissing one or more fields.");
      else
	 sprintf(msgstr, "Add of record failed; err= %d", status);
      
      ErrorDialog(radarlocDS, msgstr);
   }
   
   
   /* add entries to several tables & update the display of the list */
   
   else
   {
	 load_radarloc_list();
	 radarloc_selectItem(&rlocInfo);
   }
   
   
   
   UnsetCursor(radarlocFO);
   
   
   return;
}

/************************************************************************
   
  Update the entered item into the scrolled list.
   
   **********************************************************************/

void	radarloc_updateCB()
{
   
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   RadarLoc 	rlocInfo;
   RadarLoc	*rlocPtr;
   int 		status;
   char		where[80];
   char		msgstr[80];
   int		rv = 0;
   
   
   SetCursor(radarlocFO, XC_watch);
   
   
   /* determine which item is selected so as to 
      build the key for the record to update */
   
   XmListGetSelectedPos(radarlocLS, &listitems, &listcnt);
   
   if (listcnt == 0)
   {
      UnsetCursor(radarlocFO);
      sprintf(msgstr, "Update failed.  No item selected from list");      
      ErrorDialog(radarlocDS, msgstr);
      return;
   }
   
   else
   {
      itemnum = listitems[0];
      free(listitems);
   }
   
   
   
   rlocPtr = (RadarLoc *) ListNth(&rlocHead->list, itemnum);
   if (rlocPtr != NULL)
   {
      /* get the new items from the text fields  */
      
      rv = read_radarloc_info(&rlocInfo);  /* 0==failure, 1==success */
      if (rv == 0) 
      {
	 /* Added by stvo 05/19/99 */
   	 UnsetCursor(radarlocFO);
	 return;
      }
      
      
      if (strcmp(rlocPtr->radid, rlocInfo.radid) != 0)
      {
	 /* prevent update (user must add a new radar & delete old one) */
	 
	 UnsetCursor(radarlocFO);
	 sprintf(msgstr, "Updating a RadarId is not allowed...\n"
		 "To add a new radar, click \"Add\" instead.");
	 ErrorDialog(radarlocDS, msgstr);
	 return;
      }
      
      
      /* now update the info using the key from the item that
	 was selected and the data below.  note that it may attempt
	 to update the key fileds. */
      
      sprintf(where, " WHERE radid = '%s' ",
	      rlocPtr->radid);
      status = UpdateRadarLoc(&rlocInfo, where);
      
      if (status < 0)
	 fprintf(stderr, "Error %d updating data into RadarLoc table\n",
		 status);
      
      
      /* update the display of the list */
      
      else
      {
	 load_radarloc_list();
	 radarloc_selectItem(&rlocInfo);
      }
   }
   
   else
      fprintf(stderr, "Error getting item to update from RadarLoc list.\n");

   
   UnsetCursor(radarlocFO);

   
   return;
}


/************************************************************************
   
   Delete an item from the scrolled list.
   
   **********************************************************************/

void	radarloc_deleteCB()
{
   
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   RadarLoc 	*rlocPtr;
   char		msgstr[1024];
   
   
   /* determine which item is selected */
   
   XmListGetSelectedPos(radarlocLS, &listitems, &listcnt);
   if (listcnt == 0)
   {
      sprintf(msgstr, "Delete failed.  No item selected from list");
      ErrorDialog(radarlocDS, msgstr);
      return;
   }
   
   else
   {
      itemnum = listitems[0];
      free(listitems);
   }
   
   
   rlocPtr = (RadarLoc *) ListNth(&rlocHead->list, itemnum);
   if (rlocPtr != NULL)
   {
      memset(&msgstr, '\0', sizeof(msgstr));
      sprintf(msgstr, "WARNING:  You are about to delete radar information \n"
	      "from multiple tables.  Are you sure you want to do this? \n\n"
	      "(Note: If you delete this radar now, you will NOT be able to \n"
	      "recover the non-default values for the affected tables.) \n\n"
              "If you do not wish to delete at this time, click \"Cancel\".");
      
      QuestionDialogWithCallbacks(radarlocDS, msgstr, "Delete Confirmation",
				  radarloc_delete, NULL);
   }
   
   else
      fprintf(stderr, "Error getting item to delete from RadarLoc list.\n");

   
   return;
}



/************************************************************************
   
   Perform the actual physical delete of information from the database.
   
   **********************************************************************/

void	radarloc_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
   RadarLoc	*rlocPtr = radarloc_current_getRadarPtr(NULL);

   int 		status;
   char		buf[80];
   char		msgstr[80];
   
   
   SetCursor(radarlocFO, XC_watch);

   
   if (rlocPtr)
   {
      sprintf(buf, " delete_radar ( '%s' ) ", rlocPtr->radid);
      if ((status = execFunction(buf)) != 0)
      {
	 sprintf(msgstr, "Delete failed; err= %d", status);
	 ErrorDialog(radarlocDS, msgstr);
      }
      
      load_radarloc_list();
   }
   else
      fprintf(stderr, "ERROR: Unable to delete radar information...\n");
   
   
   UnsetCursor(radarlocFO);
   
   
   return;
}



/************************************************************************

   Retrieves the previously stored radarPtr (pass rlocPtr as NULL), or
   stores a new radarPtr (pass rlocPtr as non-NULL).
   
   **********************************************************************/

RadarLoc*	radarloc_current_getRadarPtr(RadarLoc *rlocPtr)
{
   static RadarLoc*	radarPtr = NULL;
   
   
   if (rlocPtr != NULL)
   {
      radarPtr = rlocPtr;
   }

   
   return(radarPtr);
}



/************************************************************************
   
   Get the info from the data fields.
   
   **********************************************************************/

int	read_radarloc_info(RadarLoc *rlocInfo)
{
   char 	*valstr;
   int          length;
   
   
   /* get the values of the displayed data */
   
   valstr = XmTextGetString(radidTX);
   strcpy(rlocInfo->radid, valstr);
   XtFree(valstr);
   
   valstr = XmTextGetString(radnumTX);
   rlocInfo->radar_num = atoi(valstr);
   XtFree(valstr);
   
   valstr = XmTextGetString(radidprefixTX);
   strcpy(rlocInfo->radid_prefix, valstr);
   XtFree(valstr);

   valstr = XmTextGetString(radnameTX);
   strcpy(rlocInfo->name, valstr);
   XtFree(valstr);
   
   valstr = XmTextGetString(radstateTX);
   strcpy(rlocInfo->state, valstr);
   XtFree(valstr);

   
   valstr = XmTextGetString(radlatTX);

   if (hb_check_lat_bounds(valstr))
   {
      rlocInfo->lat = cvt_spaced_format(valstr, 0);
   }
   else
   {
      ErrorDialog(radarlocDS, "Please enter a VALID (-90 to 90) Latitude.");
      XtFree(valstr);
      return(0);  /* failure */
   }
   XtFree(valstr);
    
   
   valstr = XmTextGetString(radlonTX);
   if (hb_check_lon_bounds(valstr))
   {
      rlocInfo->lon = cvt_spaced_format(valstr, 0);
   }
   else
   {
      ErrorDialog(radarlocDS, "Please enter a VALID (-180 to 180) Longitude.");
      XtFree(valstr);
      return(0);  /* failure */
   }
   XtFree(valstr);
   
   
   valstr = XmTextGetString(radelevTX);
   rlocInfo->elev = atof(valstr);
   XtFree(valstr);

   valstr = XmTextGetString(radheightTX);
   rlocInfo->tower_ht = atof(valstr);
   XtFree(valstr);
 
   if (XmToggleButtonGetState(radactiveTB))
   {
      strcpy ( rlocInfo->use_radar , "T" ) ;
   }
   else
   {
      strcpy ( rlocInfo->use_radar , "F" ) ;
   }

   valstr = XmTextGetString(radbiasTX);

   length = strlen ( valstr );

   if ( length != WFO_LEN )
   {
      ErrorDialog(radarlocDS, "Please enter three character bias source id.");
      XtFree(valstr);
      return ( 0 );
   }

   strcpy ( rlocInfo->office_id, valstr );
   XtFree (valstr );
   
   return(1);  /* success */
}


/************************************************************************
   
   Select the entry currently being considered.
   The format of this string must agree with how the list
   was loaded.
   
   **********************************************************************/

void	radarloc_selectItem(RadarLoc *rlocPtr)
{
   char		liststr[200];
   XmString	item;
   int		pos;
   char		lat[11], lon[11];
   char		elev[11];
   char		height[9];
   
   RadarLoc*	radarPtr = NULL;  /* will point to newly-selected radar */
   
   
   /* find the position in the list & highlight it */
   
   memset(&lat, '\0', sizeof(lat));
   memset(&lon, '\0', sizeof(lon));
   if ((! IsNull(DOUBLE, (void*) &rlocPtr->lat)) &&
       (! IsNull(DOUBLE, (void*) &rlocPtr->lon)))
   {
      strcpy(lat, cvt_latlon_from_double(rlocPtr->lat));
      strcpy(lon, cvt_latlon_from_double(rlocPtr->lon));
   }
   else
   {
      strcpy(lat, cvt_latlon_from_double(0.0));
      strcpy(lon, cvt_latlon_from_double(0.0));
   }
	 
   
   DataToString(&rlocPtr->elev, DOUBLE, elev, "%.1lf", "");
   DataToString(&rlocPtr->tower_ht, DOUBLE, height, "%.1lf", "");

      
   sprintf(liststr, rloc_format_string,
	      rlocPtr->radid, rlocPtr->radar_num, rlocPtr->radid_prefix,
	      rlocPtr->name, rlocPtr->state,
	      lat, lon, elev, height, rlocPtr->use_radar[0],
              rlocPtr->office_id);
   
   item = XmStringCreateSimple(liststr);
   
   if (XmListItemExists(radarlocLS, item))
   {
      pos = XmListItemPos(radarlocLS, item);
   }
   else
   {
      pos = 1;
   }
   XmListSetPos(radarlocLS, pos);
   XmListSelectPos(radarlocLS, pos, True);
   XmStringFree(item);

   
   /* sets a pointer to the newly-selected radar (given the pos in the list) */
   
   radarPtr = (RadarLoc *) ListNth(&rlocHead->list, pos);
   (void) radarloc_current_getRadarPtr(radarPtr);

   
   return;
}


/************************************************************************
   
   Free any memory allocated for the data.
   
   **********************************************************************/

void	free_radarloc()
{
   if (rlocHead != NULL)
   {
      FreeRadarLoc(rlocHead);
      rlocHead = (RadarLoc *) NULL;
   }
   
   
   return;
}


/************************************************************************
   
   Close the window.
   
   **********************************************************************/

void	ok_radarlocCB()
{
   if (XtIsManaged(radarlocDS))
   {
      XtDestroyWidget(radarlocDS);
      radarlocDS = NULL;
   }
   
   
   /* free any allocated memory */
   
   free_radarloc();
   
   
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}




