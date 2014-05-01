/*
	File:		floodrept_show.c
	Date:		May 1997
	Author:		Paul Taylor
	
	Purpose:	Provide support for Flood Report DS.
	
	Chip: 5/19/05
	need to add the code that loads the available hsa array
	need to connect the callback to the widget
	need to alter the loadXmList function to include usage of hsa filtering
	
        Cham: 9/5/07
	Added the logic to check if there is no flood stage (i.e NULL or <= 0),
	then should not show it in either the scrolled list or text report.
	
*/


#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
/**** POSTGRES
#include <sqlhdr.h>
*****/

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <X11/cursorfont.h>
#include <Xm/FileSB.h>
#include <Xm/List.h>

#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "ParamDefs.h"

#include "Location.h"
#include "Riverstat.h"
#include "FloodTs.h"
#include "Hsa.h"
#include "LoadUnique.h"
#include "Crest.h"
#include "LocView.h"
#include "rating_util.h"

#include "time_series.h"
#include "time_convert.h"
#include "user_prefs.h"

#include "floodrept.h"
#include "floodrept_draw.h"
#include "floodrept_hairs.h"
#include "floodrept_interp.h"
#include "floodrept_print.h"
#include "floodrept_show.h"
#include "GeneralUtil.h"


#define	DEBUG 0

#define ALL_HSA_TEXT "ALL HSAs"

/* globals */

Widget		fr_fileSB = (Widget) NULL;

MonthDays  monthdays[] = {{ 1, 31 }, { 2,  29 }, { 3,  31 }, { 4,  30 },
   			  { 5, 31 }, { 6,  30 }, { 7,  31 }, { 8,  31 },
			  { 9, 30 }, { 10, 31 }, { 11, 30 }, { 12, 31 }};

/* ------------------------------------------------------------------------ */

void floodrept_InitHsaArray(int * hsaCount)
{

        FloodReportWorkArea	*frwa = getFrWorkArea();
	
	Hsa * hsaHead = NULL;
	Hsa * hsaPtr = NULL;
	*hsaCount = 0;
	
			
	/* init the default HSA */
	
	int size = (MAX_HSA_COUNT) * (HYD_SERV_LEN+1);
	memset(frwa->availableHsaArray, size, '\0');
		
	hsaHead = GetHsa("ORDER BY 1");
	
	if (hsaHead != NULL)
	{
	    hsaPtr = (Hsa *) ListFirst(&hsaHead->list);
	    
	    while ((hsaPtr != NULL) && (*hsaCount <= MAX_HSA_COUNT-1) )
	    {
	        strncpy(frwa->availableHsaArray[*hsaCount], hsaPtr->hsa, HYD_SERV_LEN);
		    (*hsaCount)++;
		    
		
		hsaPtr = (Hsa *) ListNext(&hsaPtr->node);
	    }
		    
	    FreeHsa(hsaHead);
	    hsaHead = NULL;
	}
	
	

	return;
}

/* ------------------------------------------------------------------------ */

void	floodrept_hsa_cbxCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   XmComboBoxCallbackStruct *cbs_struct = (XmComboBoxCallbackStruct *)cbs;
   
   int cbsPosition = cbs_struct->item_position;
   int position = 0;
  
   /* set the HSA, load the XmList,
      and init the drawing area */
   
   position = cbsPosition;
    
   floodrept_setHsa(position, frwa->hsa); 
   floodrept_omCB(w, ptr, cbs);
   
 


   return;
}


/* ------------------------------------------------------------------------ */

void floodrept_setHsa(int selectedPosition, char * hsa)
{

    FloodReportWorkArea	*frwa = getFrWorkArea();
       
     
    if (selectedPosition == 1)
    {
        strcpy(hsa, "");  //special ALL_HSAs item
    } 
    else
    {
    	  
        strcpy(hsa, frwa->availableHsaArray[selectedPosition-2]); 
    }
     
    return;  
}

/* ------------------------------------------------------------------------ */


void floodrept_fill_hsa_CBX()
{
    XmString   xmStr;
    FloodReportWorkArea	*frwa = getFrWorkArea();
    int i = 0;
    int hsaCount = 0;
    int tableEntryCount = 0;
    
    
    /* add the callback for the always-there button */   
    XtAddCallback(fr_hsaCBX, XmNselectionCallback, (XtCallbackProc) floodrept_hsa_cbxCB, NULL);
    
    /*  create the dynamic HSA buttons from what is in the database */  
    hsaCount = frwa->hsaCount;
    tableEntryCount = hsaCount + 1;
   
   
    /* add the special ALL HSAs item*/ 
    xmStr = XmStringCreateSimple(ALL_HSA_TEXT);
    XmListAddItem(fr_hsaLI, xmStr, 0); 
    XmStringFree(xmStr);
    xmStr = NULL;
    
       
    /*  create the XM Strings from the actual db-read HSAs */ 
    for (i = 0; i < hsaCount; i++)
    {
         xmStr = XmStringCreateSimple(frwa->availableHsaArray[i]);
	
	 XmListAddItem(fr_hsaLI, xmStr, 0);
	 
	 XmStringFree(xmStr);
	 xmStr = NULL;
    }
        	
     
    // select the first position and thereby invoke the callback
    
    XmListSelectPos(fr_hsaLI, 1, True);    
    XmProcessTraversal(fr_hsaLI, XmTRAVERSE_CURRENT);
      
    return;  
}

/* ------------------------------------------------------------------------ */

void	floodrept_show(Widget w)
{
   if (! floodreptDS)
   {	
      create_floodreptDS(GetTopShell(w));
      
      floodrept_callbacks();
   }

   
   if (! XtIsManaged(floodreptDS))
   {
      XtManageChild(floodreptFO);
      XtManageChild(floodreptDS);
      
      (void) createFrWorkArea();
      floodrept_fill_hsa_CBX();  //creates HSA buttons
 
      floodrept_omCB(NULL, NULL, NULL);
      
   }
   
   return;
}

/* ------------------------------------------------------------------------ */
void	floodrept_callbacks(void)
{
   Atom		atom;
   
   
   /*
   	Window manager callbacks.
   */
   atom = XmInternAtom(XtDisplay(floodreptDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(floodreptDS, atom, floodrept_closeCB, NULL);
   
   
   /*
   	OM callbacks (other callsbacks exist in createFrWorkArea()).
   */
   XtAddCallback(fr_monPB,   XmNactivateCallback, floodrept_omCB, NULL);
   XtAddCallback(fr_yearPB,  XmNactivateCallback, floodrept_omCB, NULL);
   XtAddCallback(fr_12monPB, XmNactivateCallback, floodrept_omCB, NULL);

   XtAddCallback(fr_janPB,   XmNactivateCallback, floodrept_omCB, NULL);
   XtAddCallback(fr_febPB,   XmNactivateCallback, floodrept_omCB, NULL);
   XtAddCallback(fr_marPB,   XmNactivateCallback, floodrept_omCB, NULL);
   XtAddCallback(fr_aprPB,   XmNactivateCallback, floodrept_omCB, NULL);
   XtAddCallback(fr_mayPB,   XmNactivateCallback, floodrept_omCB, NULL);
   XtAddCallback(fr_junPB,   XmNactivateCallback, floodrept_omCB, NULL);
   XtAddCallback(fr_julPB,   XmNactivateCallback, floodrept_omCB, NULL);
   XtAddCallback(fr_augPB,   XmNactivateCallback, floodrept_omCB, NULL);
   XtAddCallback(fr_sepPB,   XmNactivateCallback, floodrept_omCB, NULL);
   XtAddCallback(fr_octPB,   XmNactivateCallback, floodrept_omCB, NULL);
   XtAddCallback(fr_novPB,   XmNactivateCallback, floodrept_omCB, NULL);
   XtAddCallback(fr_decPB,   XmNactivateCallback, floodrept_omCB, NULL);
   
   XtAddCallback(fr_okPB,    XmNactivateCallback, floodrept_closeCB, NULL);
   
   
   XtAddCallback(fr_exportPB, XmNactivateCallback, export_floodts_file, NULL);

   XtAddCallback(fr_insertPB,XmNactivateCallback, insert_cresttable, NULL);
   
   XtAddCallback(fr_okPB,    XmNactivateCallback, floodrept_closeCB, NULL);
   
   
   return;
}


/* ------------------------------------------------------------------------ */

FloodReportWorkArea*	getFrWorkArea(void)
{
   static FloodReportWorkArea	frwa;

   
   return ((FloodReportWorkArea *) &frwa);
}   
   
   
/* ------------------------------------------------------------------------ */

FloodReportWorkArea*	createFrWorkArea(void)
{	
   FloodReportWorkArea	*frwa = getFrWorkArea();
   
   
   /*
   	Set attributes.
   */
   setWidgetWidth(floodrept_mainDA, 9000);
   
   frwa->axisDA = floodrept_axisDA;		/* XDesigner widgetname */
   frwa->mainDA = floodrept_mainDA;		/* XDesigner widgetname */
   frwa->display = XtDisplay(frwa->mainDA);
   
   frwa->axisWindow = XtWindow(frwa->axisDA);
   frwa->axisHeight = getWidgetHeight(frwa->axisDA);
   frwa->axisWidth  = getWidgetWidth(frwa->axisDA);
   frwa->interval = 5;
   
   frwa->mainWindow = XtWindow(frwa->mainDA);
   frwa->mainHeight = getWidgetHeight(frwa->mainDA);
   frwa->mainWidth  = getWidgetWidth(frwa->mainDA);
   
   
   /*
   	Add other callbacks.
   */
   XtAddCallback(frwa->axisDA, XmNexposeCallback, (XtCallbackProc) floodrept_redrawAxis, NULL);
   XtAddCallback(frwa->mainDA, XmNexposeCallback, (XtCallbackProc) floodrept_redrawMain, NULL);

   XtAddCallback(floodreptLI,  XmNbrowseSelectionCallback, (XtCallbackProc) floodrept_selectCB, NULL);
   XtAddCallback(fr_refreshPB, XmNactivateCallback, (XtCallbackProc) floodrept_refreshCB, NULL);
   XtAddCallback(fr_deletePB,  XmNactivateCallback, (XtCallbackProc) floodrept_deleteCB, NULL);
   
   
   /*
   	Add event handlers.
   */
   floodrept_xhairAddHandlers();
   
   
   /*
   	Create GCs and Pixmaps.
   */
   frwa->axisGC = XCreateGC(frwa->display, frwa->axisWindow, 0, NULL);
   frwa->mainGC = XCreateGC(frwa->display, frwa->mainWindow, 0, NULL);
   
   frwa->axisPM = XCreatePixmap(frwa->display, frwa->axisWindow,
				frwa->axisWidth, frwa->axisHeight,
				DefaultDepthOfScreen(XtScreen(frwa->axisDA)));
   
   frwa->mainPM = XCreatePixmap(frwa->display, frwa->mainWindow,
				frwa->mainWidth, frwa->mainHeight,
				DefaultDepthOfScreen(XtScreen(frwa->mainDA)));

   frwa->crosshairs.gc = floodrept_xhairSetup();
   
   
   /*
   	Load fonts for drawing.
   */
   if ((frwa->finfo = XLoadQueryFont(frwa->display, "7x13")) != NULL)
   {
      XSetFont(frwa->display, frwa->axisGC, frwa->finfo->fid);
      XSetFont(frwa->display, frwa->mainGC, frwa->finfo->fid);
   }
   
   
   /*
       initialize array of HSAs, hsa count
       selected HSA filter, and
   */
   floodrept_InitHsaArray(&frwa->hsaCount);
   strcpy(frwa->hsa,"");
   
   return(frwa);
}

/* ------------------------------------------------------------------------ */

void	floodrept_loadXmList(void)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   
   time_t		beginEventsTime,
      			endEventsTime,
			crestTime;
   
   dtime_t		begin_obs_dt;
   dtime_t		end_obs_dt;
   dtime_t		crest_dt;
   
   struct tm		*tm_structPtr;
   
   
   char			begin_obs_ansi[ANSI_TIME_LEN+1];
   char			end_obs_ansi[ANSI_TIME_LEN+1];
   char			crest_obs_ansi[ANSI_TIME_LEN+1];
   
   int			cnt = 0;
   int			i = 0;
   
   char 		hsaWhere[MAX_WHERE_LEN/2];
   char			where[MAX_WHERE_LEN];
   char			field[BUFSIZ];
   
   UniqueList		*ulPtr = NULL;
   XmStringTable	stringTable;
   
   char			text[BUFSIZ];
   XmString		xmStr;
   
   char			lname[LOC_NAME_LEN + 1];
   char			lid[LOC_ID_LEN + 1];
   double		crest;
   double		fldstg;
   
   char			prev_lid[LOC_ID_LEN + 1];
   int 			maxLenOfLocationName = 13;
   
   
   SetCursor(floodreptFO, XC_watch);
	  
   
   if (maxLenOfLocationName > LOC_NAME_LEN)
   {
       maxLenOfLocationName = LOC_NAME_LEN;
   }
   
   /* initialize */
   
   XmListDeleteAllItems(floodreptLI);
   frwa->fs = 0.0;  /* don't display old flood stage value */
   xmStr = XmStringCreateSimple("Location:  (n/a)");
   XtVaSetValues(floodrept_lidLA, XmNlabelString, xmStr, NULL);
   XmStringFree(xmStr);

   
   /* get time values, and convert them to dt values */
   
   beginEventsTime = frwa->beginEventsTime;
   endEventsTime   = frwa->endEventsTime;

   timet_to_yearsec_dt(beginEventsTime, &begin_obs_dt);
   yearsec_dt_to_ansi(begin_obs_dt, begin_obs_ansi);
   
   timet_to_yearsec_dt(endEventsTime, &end_obs_dt);
   yearsec_dt_to_ansi(end_obs_dt, end_obs_ansi);
   
   /* determine what events to load */
   memset(&where, '\0', sizeof(where));
   memset(&field, '\0', sizeof(field));


   /* HSA filtering section */
   
   if (strlen(frwa->hsa) > 0)  /* a specific HSA has been chosen */
   {
       sprintf( hsaWhere, " lid in (SELECT lid from location where hsa = '%s') AND", frwa->hsa);
   }
   else /* no specific HSA has been chosen, so don't filter on this */
   {
       strcpy(hsaWhere, ""); // use all HSAs, don't filter
   }


   /* create the where clause */
   sprintf(where, "WHERE %s obstime >= '%-s' AND obstime <= '%-s' "
	   "AND flood_event_id > 0   ORDER BY 1 ",
	   hsaWhere, begin_obs_ansi, end_obs_ansi);
   
  
   sprintf(field, "lid||(flood_event_id+%d)", MAX_NUM_EVENTS );
    
   
   ulPtr = (UniqueList *) LoadUnique(field, "FloodTs", where, &cnt);
   if (cnt > 0)
   {
   
      /*
         frees old UniqueList, not the one just retrieved
      */	
   
      floodrept_FreeUnique();
      
      /*
          set the workspace area's holder for unique list to the one just retrieved
      */
      frwa->ulPtr = ulPtr;
   
    
      stringTable = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));

      /* Load Events into XmList.	
	 (Later, when an event is selected from the XmList,
	 the drawing area and Time Above/Crest/Below text fields can be
	 set to their proper values).
      */
      memset(&prev_lid, '\0', sizeof(prev_lid));
   
      i = 0;
      while (ulPtr)
      {
          memset(&lname, '\0', sizeof(lname));
          memset(&lid,   '\0', sizeof(lid));
      
          if (floodrept_ParseUnique(ulPtr, prev_lid,
				&lname[0], &lid[0], &crest, &crest_dt, &fldstg))
          {
	     if ( !IsNull(DOUBLE, (void *)&fldstg) && fldstg > 0.0 )
	     {
      
                yearsec_dt_to_timet(crest_dt, &crestTime); 
	        tm_structPtr =  gmtime(&crestTime);
	 
	        timet_to_yearhour_ansi(crestTime, crest_obs_ansi);
	        strftime(crest_obs_ansi, ANSI_TIME_LEN, "%m/%d/%Y %H:%M", tm_structPtr);
	  
	  
                lname[maxLenOfLocationName+1] = '\0';  /* trunc name if needed */
	        sprintf(text, "%-15s %-8s %7.2f  %-13s %6.1f ",
		          lname, lid, crest, crest_obs_ansi, fldstg);


	        stringTable[i] = XmStringCreateSimple(text);
	        i++;
		cnt = i;
	     }
          }
      
          ulPtr = (UniqueList *) ListNext(&ulPtr->node);
      }
   
      //add all the items to the list at once
      XmListAddItems(floodreptLI, stringTable, cnt, 1);
 
      //free up the string table
      for (i = 0; i < cnt; i++)
      {
         XmStringFree(stringTable[i]);
      }
      XtFree((char *) stringTable);
   
   
    
      if ( frwa->ulPtr != NULL )
      { 
  
          if (ListCount(&frwa->ulPtr->list) > 0)
          {
      	 
              XmListSelectPos(floodreptLI, 1, True);
	    
              XmProcessTraversal(floodreptLI, XmTRAVERSE_CURRENT);
	 
          }
      
      }
   
   }
   else
   {
       /* there are no records */
       frwa->endEventsTime = frwa->beginEventsTime + SECONDS_PER_DAY * 5;
   }
   
   UnsetCursor(floodreptFO);
   
   return;
}

/* ------------------------------------------------------------------------ */

int	floodrept_ParseUnique(UniqueList 	*ulPtr, 
			      char 		*prev_lid,
			      char 		*lname, 
			      char 		*lid,
			      double 		*crest, 
			      dtime_t		*crest_dt,
			      double 		*fldstg)
{
   Location	*loc = NULL;
   Riverstat	*riv = NULL;
   FloodTs	*floodts = NULL;
   FloodTs	tmp_crest;
   FloodTs	dummy_ts;
   
   char		tmp_lid[LOC_ID_LEN + 1];
      /* Failed to parse. */
   char **      values = NULL;
   char		where[MAX_WHERE_LEN];
   char		tmp1[BUFSIZ];
   
   int          count;
   int		rv;
   
   memset(&tmp_lid, '\0', sizeof(tmp_lid));
   memset(&where, '\0', sizeof(where));

   /* Parse the LoadUnique values. */
   values = ParseUnique ( ulPtr, &count );

   if ( ( values == NULL ) || ( count < 2 ) )
   {
      /* Failed to parse. */
      fprintf ( stderr, "In 'floodrept_ParseUnique':\n"
                        "The call to ParseUnique failed.\n" );
      return 0 ;
   }  

   /* get Location & Riverstat info */
   strncpy(tmp_lid, values[0], LOC_ID_LEN);
   sprintf(where, " WHERE lid = '%s' ", tmp_lid);
   loc = (Location *)  GetLocation(where);
   riv = (Riverstat *) GetRiverstat(where);

   
   /* get FloodTs info and use this in determining the crest */
   
   memset(&tmp1, '\0', sizeof(tmp1));
   sprintf(tmp1, " AND flood_event_id = %ld ",
	   atol(values[1]) - MAX_NUM_EVENTS);

   strcat(where, tmp1);
   strcat(where, " ORDER BY obstime ");
   
   floodts = (FloodTs *) GetFloodTs(where);
   if (loc && riv && floodts)
   {
      if (strcmp(tmp_lid, prev_lid) != 0)  /* (different lids) */
      {
	 strcpy(lname, loc->name);
	 strcpy(lid,   tmp_lid);
	 
	 strcpy(prev_lid, tmp_lid);
      }
      else  /* (same lids) suppress printing of lname & lid in XmList... */
      {
	 strcpy(lname, "");
	 strcpy(lid, "");
      }

      tmp_crest = floodrept_findCrest(floodts, riv->fs, &dummy_ts);
      *crest  = tmp_crest.value;
      *crest_dt = tmp_crest.obstime;
      *fldstg = riv->fs;
      
      rv = 1;	/* parsing was successful */
   }
   else
   {
      rv = 0;	/* failed to parse */
   }
   

   /*
   	Free memory & return.
   */
   if (loc)
      FreeLocation(loc);
   if (riv)
      FreeRiverstat(riv);
   if (floodts)
      FreeFloodTs(floodts);

   if ( values != NULL )
   {
      FreeParseUnique ( values );
   } 
   
   
   return(rv);
}


/* ------------------------------------------------------------------------ */
void	floodrept_FreeUnique(void)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();
   
   if (frwa->ulPtr != NULL)
   {
      FreeUnique(frwa->ulPtr);
      frwa->ulPtr = NULL;
   }
   
   return;
}


/* ------------------------------------------------------------------------ */
FloodTs	floodrept_findCrest(FloodTs 	*floodtsPtr,
			    double 	fs,
			    FloodTs 	*last_crestPtr)
{
   FloodTs	crest;
   int		number_since_crest;
   
   
   /* initialize */
   
   *last_crestPtr = *floodtsPtr;
   (*last_crestPtr).value = MSG;
   crest.value = MSG;
   number_since_crest  = 0;
   
   
   /* loop on stages in the event and find the crest(s) */
   
   while (floodtsPtr)
   {    
      /* if a crest is already defined and the value immediately 
	 after the crest is identical, consider it a sustained crest */
      
      if (crest.value != MSG && floodtsPtr->value == crest.value &&
	  number_since_crest == 0)
      {
	 *last_crestPtr = *floodtsPtr;
      }
      
      
      /* check for a higher crest. if new crest found, then 
	 clear out the sustained crest */
      
      else if (floodtsPtr->value > crest.value || crest.value == MSG)
      {
	 crest = *floodtsPtr;
	 (*last_crestPtr).value = MSG;
	 
	 number_since_crest = 0;
      }
      
      else
	 number_since_crest++;
      
      
      /* get the next item */
      
      floodtsPtr = (FloodTs *) ListNext(&floodtsPtr->node);
   }
   
   return crest;
}   


/* ------------------------------------------------------------------------ */
void	floodrept_setReportingPeriod(int 	menu_pos,
				     time_t 	*beginEventsTime,
				     time_t 	*endEventsTime)
{
   int		month;
   struct tm	*tmPtr;
   struct tm	tmStruct;
   time_t	beginTime;
   time_t	endTime;   
   char		orig_ansi[ANSI_TIME_LEN];
   char		ansi[ANSI_TIME_LEN];   
   float	value;

   
   /* use current time as the endTime for the moment */
   
   time(&endTime);
   tmPtr = gmtime(&endTime);
   
   strftime(orig_ansi, ANSI_TIME_LEN, "%Y-%m-%d %H:%M:%S", tmPtr);
   tmStruct = *tmPtr;

   
   /* check menu_pos to see if we need to use a different endTime */
   
   if (menu_pos > 3)	/* January -> December */
   {
      month = menu_pos - 3;

      /* for endTime */
      
      tmStruct.tm_mon = month - 1;
      tmStruct.tm_mday = monthdays[month - 1].days;

      if ((tmStruct.tm_mon == 1)	/* adjust for non-leap years */
	  && ((value = tmStruct.tm_year / 4.0) &&
	      (value != (float) (int) value)))
      {
	 tmStruct.tm_mday -= 1;
      }
      
      tmStruct.tm_hour= 23;
      tmStruct.tm_min = 59;
      tmStruct.tm_sec = 59;
      
      memset(&ansi, '\0', sizeof(ansi));
      strftime(ansi, ANSI_TIME_LEN, "%Y-%m-%d %H:%M:%S",
	       &tmStruct);

      if ((month > tmPtr->tm_mon) &&   /* (looking for month > currentmonth) */
	  (strcmp(ansi, orig_ansi) > 0))
      {
	 tmStruct.tm_year -= 1;
	 memset(&ansi, '\0', sizeof(ansi));
	 strftime(ansi, ANSI_TIME_LEN, "%Y-%m-%d %H:%M:%S",
		  &tmStruct);
      }
      if (isTimeError(yearsec_ansi_to_timet(ansi, &endTime)))
	 fprintf(stderr, "ERROR converting endTime...\n");

      /* for beginTime */
      
      tmStruct.tm_mon  = month - 1;
      tmStruct.tm_mday = 1;
      tmStruct.tm_hour = 0;
      tmStruct.tm_min  = 0;
      tmStruct.tm_sec  = 0;
      
      memset(&ansi, '\0', sizeof(ansi));
      strftime(ansi, ANSI_TIME_LEN, "%Y-%m-%d %H:%M:%S", &tmStruct);
      
      if (isTimeError(yearsec_ansi_to_timet(ansi, &beginTime)))
	 fprintf(stderr, "ERROR converting beginTime...\n");
   }
   
   else if (menu_pos == 0)	/* Month to Date (keep existing endTime) */
   {
      /*
      		for beginTime
      */
      tmStruct.tm_mday = 1;
      tmStruct.tm_hour= 0;
      tmStruct.tm_min = 0;
      tmStruct.tm_sec = 0;
      
      memset(&ansi, '\0', sizeof(ansi));
      strftime(ansi, ANSI_TIME_LEN, "%Y-%m-%d %H:%M:%S", &tmStruct);
      
      if (isTimeError(yearsec_ansi_to_timet(ansi, &beginTime)))
	 fprintf(stderr, "ERROR converting beginTime...\n");
   }
   
   else if (menu_pos == 1)	/* Year to Date (keep existing endTime) */
   {
      /* for beginTime */
      
      tmStruct.tm_mon = 0;
      tmStruct.tm_mday = 1;
      tmStruct.tm_hour= 0;
      tmStruct.tm_min = 0;
      tmStruct.tm_sec = 0;
      
      memset(&ansi, '\0', sizeof(ansi));
      strftime(ansi, ANSI_TIME_LEN, "%Y-%m-%d %H:%M:%S",
	       &tmStruct);
      
      if (isTimeError(yearsec_ansi_to_timet(ansi, &beginTime)))
	 fprintf(stderr, "ERROR converting beginTime...\n");
   }

   else if (menu_pos == 2)	/* Last 12 Months (keep existing endTime) */
   {
      /* for beginTime */
      
      tmStruct.tm_year -= 1;
      
      memset(&ansi, '\0', sizeof(ansi));
      strftime(ansi, ANSI_TIME_LEN, "%Y-%m-%d %H:%M:%S",
	       &tmStruct);
      
      if (isTimeError(yearsec_ansi_to_timet(ansi, &beginTime)))
	 fprintf(stderr, "ERROR converting beginTime...\n");
   }

   
   /* assign values & return */
   
   *beginEventsTime = beginTime;
   *endEventsTime = endTime;
   
   return;
}


/* ------------------------------------------------------------------------ */
void	floodrept_omCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   FloodReportWorkArea	*frwa = getFrWorkArea();

  
   /* set the Reporting Period, load the XmList,
      and init the drawing area */
   
   floodrept_setReportingPeriod(GetMenuPos(floodreptOM),
				&frwa->beginEventsTime,
				&frwa->endEventsTime);
				
	
   floodrept_loadXmList();
   
   floodrept_drawFloodEvent();  /* inits the main drawing area */
  
 
   return;
}


/* ------------------------------------------------------------------------ */
void	floodrept_selectCB(Widget w, XtPointer ptr, XtPointer cbs)
{

   floodrept_drawFloodEvent();
   
   
   floodrept_xhairErase();
   floodrept_xhairErase();
   

   return;
}


/* ------------------------------------------------------------------------ */
void export_floodts_file(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	list;
   Arg		args[10];
   int		ac;
   XmString	str;
   char		report_dir[128];
   int		gad_token_len=0, gad_value_len=0;
   
   
   if(! fr_fileSB)
   {
      fr_fileSB = XmCreateFileSelectionDialog(GetTopShell(w), "fr_fileSB", NULL, 0);
      
      
      /* set XmNpattern & XmNdialogStyle */
      
      str = XmStringCreateSimple("*.e3info");
      ac = 0;
      XtSetArg(args[ac], XmNpattern, str); ac++;
      XtSetArg(args[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;
      
      gad_token_len = strlen("whfs_report_dir");
      get_apps_defaults("whfs_report_dir", &gad_token_len, report_dir, &gad_value_len);
      str = XmStringCreateSimple(report_dir);
      XtSetArg(args[ac], XmNdirectory, str); ac++;
      
      XtSetValues(fr_fileSB, args, ac);
      XmStringFree(str);
      
      
      /* set callbacks for cancelling and ok'ing on file selection */
      
      XtAddCallback(fr_fileSB, XmNcancelCallback, cancel_floodts_file, NULL);
      XtAddCallback(fr_fileSB, XmNokCallback, select_floodts_file, NULL);
      
      list = XmFileSelectionBoxGetChild(fr_fileSB, XmDIALOG_LIST);
      XtAddCallback(list, XmNdefaultActionCallback, select_floodts_file, NULL);
   }   
   
   SetTitle(fr_fileSB,"Flood Stage Report Info");
   
   if(! XtIsManaged(fr_fileSB))
      XtManageChild(fr_fileSB);
   
   return;
}


   
/* ------------------------------------------------------------------------ */
void list_event_vals(FloodTs *floodtsEvent)
{
   char			text[BUFSIZ], timestr[60];
   FloodTs		*floodtsPtr = NULL ;
   XmString		*xmStr = NULL ;
   int			i, cnt = 0 , status;
   int			ac;
   Arg			arg[40];
   time_t		tmp_timet;
   struct tm 		*tm_struct;
   
   
   if ( floodtsEvent != NULL )
   {
      cnt = ListCount ( & floodtsEvent->list ) ; 
   }

   xmStr  = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
   
   if ( floodtsEvent != NULL )
   {
      floodtsPtr = (FloodTs *) ListFirst(&floodtsEvent->list);
   }

   i = 0;
   while (floodtsPtr)
   {
      status = yearsec_dt_to_timet(floodtsPtr->obstime, &tmp_timet);
      tm_struct = gmtime(&tmp_timet);
      strftime(timestr, 60, "%a %m/%d/%Y  %H:%M", tm_struct);
      sprintf(text, " %12.2f    %s", floodtsPtr->value, timestr);
      
      xmStr[i] = XmStringCreateSimple(text);
      
      floodtsPtr = (FloodTs *) ListNext(&floodtsPtr->node);
      i++;
   }
   
   
   /* load the list into the scrolled list */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(fr_stagesLI, arg, ac);
   
   
   /* free the memory */
   
   for (i = 0; i < cnt; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *) xmStr);
   
   return;
}


/* ------------------------------------------------------------------------ */
void select_floodts_file(Widget w,
			 XtPointer ptr,
			 XmFileSelectionBoxCallbackStruct *cbs)
{
   XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
   FILE		*fp;
   char		*name;
   char        **values = NULL ;
   
   FloodReportWorkArea	*frwa = getFrWorkArea();
   UniqueList	*ulPtr;
   int          count;
   int 		ul_count;
   int		i;
   int		status;
   FloodTs	*floodtsHead = NULL;
   FloodTs	crest, last_crest;
   char     	where[MAX_WHERE_LEN];
   char		tmp_lid[LOC_ID_LEN + 1], prev_lid[LOC_ID_LEN + 1];
   char		prev_stream[STREAM_NAME_LEN + 1], prev_rb[RIV_BAS_LEN + 1];
   int		event_id;
   char		above_txt[60], below_txt[60], crest_txt[60];
   time_t	curtime, begin_time, end_time;
   Riverstat	*riv;
   Location	*loc;
   float	crest_stage;
   time_t	above_timet, below_timet, crest_timet;
   struct tm 	*tm_struct;
   int		*sort_order;
   char		below_flagstr[2], crest_flagstr[2];
   double	roc;
   
   
   if(XtIsManaged(fr_fileSB))
   {
      XtDestroyWidget(fr_fileSB);
      fr_fileSB = NULL;
   }
   
   if (! XmStringGetLtoR(cbs->value, charset, &name))
      return;
   
   if (! *name)
   {
      XtFree(name);
      return;
   }
   
   if ((fp = fopen(name, "w")) == (FILE *) NULL)
   {
      printf("Unable to open file: %s\n", name);
      return;
   }	
		
		
   /* get and write the data to the file ------------------------------ */

   fprintf(fp, "Flood report information listing generated by WHFS HydroBase.\n");
   time(&curtime);
   fprintf(fp, "File created on: %s", asctime(gmtime(&curtime)));
   
   
   /* get and write the time window */
   
   floodrept_setReportingPeriod(GetMenuPos(floodreptOM),
				&begin_time, &end_time);
   fprintf(fp, "\nFor period beginning: %s", asctime(gmtime(&begin_time)));
   fprintf(fp, "          and ending: %s", asctime(gmtime(&end_time)));
   fprintf(fp, "All times given in GMT.\n");
   fprintf(fp, "N/A= Not Available   q= questionable\n\n");
   
   fprintf(fp, "Flood events are grouped by location, river, and basin.\n");
   fprintf(fp, "For each event, the following information is given:\n\n");  
   fprintf(fp, "    FLD STG     ABOVE FLOOD  - BELOW FLOOD    CREST    TIME\n"); 
   
   fprintf(fp, "\n------------------------------------------------------------------\n");
   
   ul_count = ListCount(&frwa->ulPtr->list);
   if (ul_count == 0)
   {
      fprintf(fp, "No data for reported period.\n");
      return;
   }
   
   /* sort the events.  use this information below when looping */
   
   sort_order = (int *)malloc(sizeof(int) * ul_count);
   sort_events(frwa->ulPtr, sort_order);

   
   /* process the info that is currently in the event list */
   
   memset(&prev_lid,    '\0', sizeof(prev_lid));      
   memset(&prev_stream, '\0', sizeof(prev_stream));      
   memset(&prev_rb,     '\0', sizeof(prev_rb));      
   
   for (i = 0;  i < ul_count;  i++)
   {
      
      /* list the events in the sorted order. */
      
      ulPtr = (UniqueList *) ListNth(&frwa->ulPtr->list, sort_order[i] + 1);
      
      if ( ulPtr )
      { 
              memset(&tmp_lid, '\0', sizeof(tmp_lid));      

	      values = ParseUnique ( ulPtr, & count );
	      strncpy ( tmp_lid, values[0], LOC_ID_LEN);
	      event_id = atol ( values[1] ) - MAX_NUM_EVENTS;
              FreeParseUnique ( values );
	      
	      sprintf(where, " WHERE lid = '%-s' and flood_event_id = %d ",
		      tmp_lid, event_id);
	      
	      floodtsHead = (FloodTs *) GetFloodTs(where);
	      if (floodtsHead)
	      {
		 sprintf(where, " WHERE lid = '%s' ", tmp_lid);
		 loc = (Location *)  GetLocation(where);
		 riv = (Riverstat *) GetRiverstat(where);
		 
		 
		 /* get the crest info.  Any missing crest info is indicated
		    by a missing indicator for the value. */
		 
		 crest = floodrept_findCrest(floodtsHead, riv->fs, &last_crest);
		 crest_stage = crest.value;
		 if (crest.value != MSG)
		 {
		    status = yearsec_dt_to_timet(crest.obstime, &crest_timet);
		    tm_struct = gmtime(&crest_timet);
		    strftime(crest_txt, sizeof(crest_txt), "  %d %b %H:%M",
			     tm_struct);
		 }
		 else
		    strcpy(crest_txt, "   N/A");
		    
		 
		 
		 /* get the passthru flood stage info. */
		 
		 if (get_passthru_times(floodtsHead, riv->fs, 
					&above_timet, &below_timet))
		 {
		    /* convert above and below fld stage times to strings */
		    
		    if (above_timet != MSG)
		    {
		       tm_struct = gmtime(&above_timet);	    
		       strftime(above_txt, sizeof(above_txt),  "%d %b %H:%M",
				tm_struct);
		    }
		    else
		       strcpy(above_txt, "    N/A");
		    
			    
		    if (below_timet != MSG)
		    {
		       tm_struct = gmtime(&below_timet);	    
		       strftime(below_txt, sizeof(below_txt),  "%d %b %H:%M", 
				tm_struct);
		    }
		    else
		       strcpy(below_txt, "    N/A");
		 }  
		 
		 
		 /* check for some obvious error conditions, i.e.
		    if fall below fld before crest, or if crest much
		    higher than fld stage, or if rate-of-change too high */
		 
		 sprintf(below_flagstr, " ");
		 sprintf(crest_flagstr, " "); 
		 
		 if (below_timet < crest_timet && 
		     (crest.value != MSG && below_timet != MSG))
		    sprintf(below_flagstr, "q");
		 
		 if (crest.value == MSG || abs(crest.value - riv->fs) > 50.)
		    sprintf(crest_flagstr, "q");
		 
		 if (strcmp(loc->lid, "EAKO2") == 0)
		 {
		    roc = 0.;
		 }
		 
		 if (crest.value != MSG && crest_timet != MSG && above_timet != MSG &&
		     crest_timet != above_timet)
		 {
		    roc = 3600. * ((crest.value - riv->fs) / (double )(crest_timet - above_timet));    
		    if (abs(roc) > 10) sprintf(crest_flagstr, "q");
		 }
		 
		 if (crest.value != MSG && crest_timet != MSG && below_timet != MSG &&
		     crest_timet != below_timet)
		 {
		    roc = 3600. * ((crest.value - riv->fs) / (double )(crest_timet - below_timet));
		    if (abs(roc) > 10) sprintf(crest_flagstr, "q");
		 }
		 
		 
		 /* now output the info for the event */
		 
		 if (strcmp(loc->rb, prev_rb) != 0)
		    fprintf(fp, "\n\nBASIN: %s\n", loc->rb);
		 
		 if (strcmp(riv->stream, prev_stream) != 0)
		    fprintf(fp, "\n  RIVER: %s\n", riv->stream);
		 
		 if (strcmp(tmp_lid, prev_lid) != 0)
		    fprintf(fp, "\n    %s, %s (%s)\n",
			    loc->name, loc->state, tmp_lid);
		 
		 fprintf(fp, "    %7.1f    %-12s - %-12s%s  %8.2f%s  %-12s\n", 
			 riv->fs, above_txt, below_txt, below_flagstr,
			 crest_stage, crest_flagstr, crest_txt);
		 
		 
		 /* preserve these for the next pass */
		 
		 strcpy(prev_lid,    tmp_lid);
		 strcpy(prev_stream, riv->stream);
		 strcpy(prev_rb,     loc->rb);
		 
		 
		 /* free memory for the event */
			 
		 FreeLocation(loc);
		 FreeRiverstat(riv);
		 FreeFloodTs(floodtsHead);
	      }
	      
	      else
	      {
		 fprintf(fp, "Did NOT find data for:%s:%d:%s\n",
			 tmp_lid, event_id, ulPtr->uchar);
	      }
      } /* end if (ulPtr) */
   } /* end i loop */
	   
	   
	   /* free memory allocated above */
	   
	   free(sort_order);
		     
	   XtFree(name);	
	   fclose(fp);
	   
	   return;
	}


	/* ------------------------------------------------------------------------ */
	void cancel_floodts_file(Widget w, XtPointer ptr, XtPointer cbs)
	{
	   if (XtIsManaged(fr_fileSB))
	   {
	      XtDestroyWidget(fr_fileSB);
	      fr_fileSB = NULL;
	   }
	   
	   return;
	}


	/* ------------------------------------------------------------------------ */
	void	floodrept_closeCB(Widget w, XtPointer ptr, XtPointer cbs)
	{
	   FloodReportWorkArea	*frwa = getFrWorkArea();
	   
	   
	   /* free Data */
	   
	   if (frwa->floodtsPtr)
	      FreeFloodTs(frwa->floodtsPtr);
	   
	   if (frwa->mainPM)
	      XFreePixmap(frwa->display, frwa->mainPM);
	   
	   if (frwa->mainGC)
	      XFreeGC(frwa->display, frwa->mainGC);
	   
	   if (frwa->axisPM)
	      XFreePixmap(frwa->display, frwa->axisPM);
	   
	   if (frwa->axisGC)
	      XFreeGC(frwa->display, frwa->axisGC);

	   floodrept_FreeUnique();
	   

	   /*
		Set Values to NULL.
	   */
	   frwa->floodtsPtr = NULL;
	   frwa->mainPM =  0 ;
	   frwa->mainGC = NULL;
	   frwa->axisPM =  0 ;
	   frwa->axisGC = NULL;
	   
	   frwa = NULL;
	   
	   if ( floodreptDS != NULL )
	   {
	      if(XtIsManaged(floodreptDS))
	      {
		 XtDestroyWidget(floodreptDS);
		 floodreptDS = NULL;
	      }
	   }
	   
	   return;
	}


	/* ------------------------------------------------------------------------ */
	void	floodrept_refreshCB(Widget w, XtPointer ptr, XtPointer cbs)
	{
	   char		bin_dir[128];
	   char		buf[MAX_BUF_LEN];
	   int		gad_token_len=0, gad_value_len=0;
	   
	   
	   memset(&bin_dir, '\0', sizeof(bin_dir));
	   memset(&buf, '\0', sizeof(buf));
	   
	   gad_token_len = strlen("whfs_bin_dir");
	   get_apps_defaults("whfs_bin_dir", &gad_token_len, bin_dir, &gad_value_len);
	   if (strlen(bin_dir) > 0)
	   {
	      sprintf(buf, "%-s/run_floodseq_interactive &", bin_dir);
	      system(buf);
	   }
	   else
	   {
	      fprintf(stderr, "ERROR: Could not Refresh data (check environment variables).\n");
	   }
	   
	   return;
	}


	/* ------------------------------------------------------------------------ */
	void	floodrept_deleteCB(Widget w, XtPointer ptr, XtPointer cbs)
	{
	   Widget		qstDS,
				okPB;
	   char			buf[MAX_BUF_LEN];
	   
	   
	   sprintf(buf, "Do you wish to delete this entry?");
	   qstDS = QuestionDialog(floodreptDS, buf);
	   SetTitle(qstDS, "Delete Confirmation");
	   
	   
	   /* get the XmMessageBox ok button,
		and associate a callback to it */
	   
	   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
	   XtAddCallback(okPB, XmNactivateCallback, (XtCallbackProc) floodrept_delete, NULL);
	   
	   
	   if(! XtIsManaged(qstDS))
	      XtManageChild(qstDS);
	   
	   return;
	}


	/* ------------------------------------------------------------------------ */
	void	floodrept_delete(void)
	{
	   FloodReportWorkArea	*frwa = getFrWorkArea();
	   
	   UniqueList		*ulPtr;
	   
	   FloodTs		*floodts = NULL,
				*floodtsPtr = NULL;
	   
	   char     		where[MAX_WHERE_LEN];
	   char			tmp_lid[LOC_ID_LEN + 1];
	   char			obstime[ANSI_TIME_LEN + 1];
           char                 ** values = NULL;
	   
	   int      		*poslist = NULL;
	   int      		i, cnt = 0;
           int                  count;
	   int			error;
	   
	   
	   SetCursor(floodreptFO, XC_watch);
	   

	   /* For all positions selected,  obtain the unique list which will
	      indicate what record(s) from the FloodTs table need to be deleted. */
	   
	   XmListGetSelectedPos(floodreptLI, &poslist, &cnt);
	   if (poslist && (cnt > 0))
	   {
	      for (i = 0;  i < cnt;  i++)
	      {
		 ulPtr = (UniqueList *) ListNth(&frwa->ulPtr->list, poslist[i]);
		 if (ulPtr)
		 {
		    memset(&where, '\0', sizeof(where));
		    memset(&tmp_lid, '\0', sizeof(tmp_lid));

                    values = ParseUnique ( ulPtr, &count ); 
		    
		    strncpy(tmp_lid, values[0], LOC_ID_LEN);
		    sprintf(where, " WHERE lid = '%-s' and flood_event_id = %ld ",
			    tmp_lid, atol(values[1]) - MAX_NUM_EVENTS);
                    FreeParseUnique ( values );
		    
		    floodts = (FloodTs *) GetFloodTs(where);
		    if (floodts)
		    {
		       floodtsPtr = (FloodTs *) ListFirst(&floodts->list);
		       while (floodtsPtr)
		       {
			  memset(&where, '\0', sizeof(where));
			  memset(&obstime, '\0', sizeof(obstime));
			  if (! isTimeError(yearsec_dt_to_ansi(floodtsPtr->obstime,
							       obstime)))
			  {
			     sprintf(where, " WHERE lid = '%-s' AND "
				     "obstime = '%-s' AND flood_event_id = %ld AND "
				     "value = %f",
				     floodtsPtr->lid, obstime, floodtsPtr->flood_event_id,
				     floodtsPtr->value);
			     error = DeleteFloodTs(where);
			     if (error != 0)
			     {
				ErrorDialog(floodreptDS, DbErrorString(error));
				break;
			     }
			  }
			  else
			  {
			     fprintf(stderr, "ERROR while attempting to delete flood event...\n");
			  }
			  
			  floodtsPtr = (FloodTs *) ListNext(&floodtsPtr->node);
		       }
		       
		       FreeFloodTs(floodts);
		    }
		 }
	      }
	      
	      
	      /*
			Free, update, and return.
	      */
	      if (poslist)
		 free(poslist);
	      
	      floodrept_omCB(NULL, NULL, NULL);
	   }

	   
	   UnsetCursor(floodreptFO);
	   
	   
	   return;
	}


	/* ------------------------------------------------------------------------ */
	void	floodrept_loadSignificantTimes(void)
	{
	   FloodReportWorkArea	*frwa = getFrWorkArea();
	   
	   FloodTs		*floodtsEvent;
	   FloodTs		crest;
	   double 		fs;
	   FloodTs		last_crest;
	   float		crest_stage;
	   time_t		above_timet, below_timet;
	   time_t		tmp_timet;
	   char			crest_txt[MAX_TEXT_LEN], crest2_txt[MAX_TEXT_LEN];
	   char			above_txt[MAX_TEXT_LEN], below_txt[MAX_TEXT_LEN];
	   struct tm		tmp_tm;
	   int			status;
	      
	   
	   /* define convenience variables */
	   
	   floodtsEvent = frwa->floodtsPtr;
	   fs = frwa->fs;
	   
	   
	   /* initialize */
	   
	   strcpy(crest_txt,  " ");
	   strcpy(crest2_txt, " ");
	   strcpy(above_txt,  " ");
	   strcpy(below_txt,  " ");
	   
	   
	   /* if flood event info exists */
	   
	   if (floodtsEvent)
	   {      
	      /* get the crest info */
	      
	      crest = floodrept_findCrest(floodtsEvent, fs, &last_crest);
	      crest_stage = crest.value;
	      
	      
	      /* store crest time info; check for a sustained crest. */
	      
	      if (crest.value != MSG)
	      {
		 status = yearsec_dt_to_timet(crest.obstime, &tmp_timet);
		 tmp_tm = *gmtime(&tmp_timet);
		 strftime(crest_txt, sizeof(crest_txt), "  %d %b %Y %H:%M Z", &tmp_tm);
		 
		 
		 if (last_crest.value != MSG)
		 {
		    status = yearsec_dt_to_timet(last_crest.obstime, &tmp_timet);
		    tmp_tm = *gmtime(&tmp_timet);
		    strftime(crest2_txt, sizeof(crest2_txt), "  %d %b %Y %H:%M Z", &tmp_tm);
		    strcat(crest_txt, "\n");
		    strcat(crest_txt, crest2_txt);
		 }
	      }
	      
	      
	      /* get the passthru flood stage times */
	      
	      if (get_passthru_times(floodtsEvent, fs, 
				     &above_timet, &below_timet))
	      {
		 /* convert above fld stage time */
		 
		 if (above_timet != MSG)
		 {
		    tmp_tm = *gmtime(&above_timet);	    
		    strftime(above_txt, sizeof(above_txt), "  %d %b %Y %H:%M Z", &tmp_tm);
		 }
		 
		 
		 /* convert below fld stage time */
		 
		 if (below_timet != MSG)
		 {
		    tmp_tm = *gmtime(&below_timet);	    
		    strftime(below_txt, sizeof(below_txt), "  %d %b %Y %H:%M Z", &tmp_tm);
		 }
	      }     
	   }
	   
	   
	   /* place the text in the display */
	   
	   XmTextSetString(fr_aboveTE, above_txt);
	   XmTextSetString(fr_crestTE, crest_txt);
	   XmTextSetString(fr_belowTE, below_txt);
	   
	   
	   /* ALSO load the data for the flood time series into the scrolled list */
	   
	   list_event_vals(floodtsEvent);
	   
	   return;
	}
	  

	/* ------------------------------------------------------------------------ */
	int  get_passthru_times(FloodTs 	*floodtsPtr,
				float		fs,
				time_t		*above_timet,
				time_t		*below_timet)
	{
	   int		find_first_anchor = True;
	   int		find_last_anchor = True;   
	   FloodTs	ptA, ptB, ptC, ptD, anchor1, anchor2;
	   FloodTs  	*nextPtr;   
	   time_t	tmp_timet;
	   PtsMissing	pts_missing[] = {{INDEX_PT_A, True}, {INDEX_PT_B, True},
					 {INDEX_PT_C, True}, {INDEX_PT_D, True},
					 {INDEX_ANCHOR1, True},
					 {INDEX_ANCHOR2, True}};
	   int		status;
	      

	   /* initialize */
	   
	   ptA.value     = MSG;
	   ptB.value     = MSG;
	   ptC.value     = MSG;
	   ptD.value     = MSG;
	   anchor1.value = MSG;
	   anchor2.value = MSG;
	     
	   
	   /* initialize return values */
	   
	   *above_timet = *below_timet = MSG;
	   
	   if (!floodtsPtr)
	      return 0 ;
	    
	   
	   /* get first Anchor point */
	   
	   while (floodtsPtr && find_first_anchor)
	   {
	      nextPtr = (FloodTs *) ListNext(&floodtsPtr->node);  
	      
	      if (floodtsPtr->value < fs)
	      {
		 ptA = *floodtsPtr;
		 pts_missing[INDEX_PT_A].state = False;
		 
		 if (nextPtr)
		 {
		    if (nextPtr->value >= fs)
		    {
		       ptB = *nextPtr;
		       pts_missing[INDEX_PT_B].state = False;
		       
		       find_first_anchor = False;
		    }
		 }
	      }
	      else  /* missing ptA */
	      {
		 ptB = *floodtsPtr;
		 pts_missing[INDEX_PT_B].state = False;
		 
		 find_first_anchor = False;
	      }
	      
	      floodtsPtr = (FloodTs *) ListNext(&floodtsPtr->node);  
	   }   
	   
	   
	   /* interpolate to find time of progression above flood stage */
	   
	   if ((pts_missing[INDEX_PT_A].state == False) &&
	       (pts_missing[INDEX_PT_B].state == False))
	   {
	      anchor1 = floodrept_interp(ptA, ptB, fs);
	      pts_missing[INDEX_ANCHOR1].state = False;
	   }
	   else
	   {   
	      if (ptA.value == fs)
	      {
		 anchor1 = ptA;
		 pts_missing[INDEX_ANCHOR1].state = False;
	      }
	      
	      else if (ptB.value == fs)
	      {
		 anchor1 = ptB;
		 pts_missing[INDEX_ANCHOR1].state = False;
	      }
	   } 
	      
	   
	   /* get last Anchor point */
	   
	   while (floodtsPtr && find_last_anchor)
	   { 	   
	      nextPtr = (FloodTs *) ListNext(&floodtsPtr->node);
	      
	      if (floodtsPtr->value >= fs)
	      {
		 if (nextPtr)
		 {   
		    if (nextPtr->value < fs)
		    {
		       ptC = *floodtsPtr;
		       pts_missing[INDEX_PT_C].state = False;
		       
		       ptD = *nextPtr;
		       pts_missing[INDEX_PT_D].state = False;
		       
		       find_last_anchor = False;
		       
		       /* advance ptr one extra since nextPtr is the
			  end of this event */
		       
		       floodtsPtr = (FloodTs *) ListNext(&floodtsPtr->node);
		    }   
		 }
		 else
		 {
		    ptC = *floodtsPtr;
		    pts_missing[INDEX_PT_C].state = False;
		    
		    find_last_anchor = False;
		 }
	      }
	      else  /* point C was skipped or MSG. */
	      {
		 ptC = ptB; /* ptB is MSG or above fs */
		 pts_missing[INDEX_PT_C].state = False;

		 ptD = *floodtsPtr;
		 pts_missing[INDEX_PT_D].state = False;
		 
		 find_last_anchor = False;
	      }   
	      
	      floodtsPtr = (FloodTs *)  ListNext(&floodtsPtr->node);  
	   }   
	   
	   
	   /* interpolate to find time of recession below flood stage */
	   
	   if ((pts_missing[INDEX_PT_C].state == False) &&
	       (pts_missing[INDEX_PT_D].state == False))
	   {
	      anchor2 = floodrept_interp(ptC, ptD, fs);
	      pts_missing[INDEX_ANCHOR2].state = False;
	   }
	   else
	   {
	      if (ptC.value == fs)
	      {
		 anchor2 = ptC;
		 pts_missing[INDEX_ANCHOR2].state = False;
	      }
	      
	      else if (ptD.value == fs)
	      {
		 anchor2 = ptD;
		 pts_missing[INDEX_ANCHOR2].state = False;
	      }
	   } 
	   
		
	   
	   /* return with success */
	   
	   if (anchor1.value != MSG)
	   {
	      status = yearsec_dt_to_timet(anchor1.obstime, &tmp_timet);   
	      *above_timet = tmp_timet;
	   }
	   
	   
	   if (anchor2.value != MSG)
	   {
	      status = yearsec_dt_to_timet(anchor2.obstime, &tmp_timet);   
	      *below_timet = tmp_timet;
	   }
	   
	   return(1); 
	}   


	/*********************************************************************
	   load_passthru_stages()
	    
	   PURPOSE
	   NOT COMPLETED/NOT TESTED!!!!!!!!!
	   
	   ********************************************************************/
	void load_passthru_stages(FloodTs       *floodtsHead,
				  double	fldstage,
				  time_t	*above_time,
				  time_t	*below_time)
	{
	   FloodTs	*floodtsPtr = NULL ;     
	   int		numvals = 0 ;
	   double 	stage, prev_stage = 0. ;
	   time_t	stage_timet, prevstage_timet;
	   int		status = 0 ;
	   time_t	riseabove_timet, fallbelow_timet;
	      
	   
	   /* initialize */
	   
	   *above_time     = *below_time = 0;
	   riseabove_timet = fallbelow_timet = 0;
	      
	   
	   /* if flood stage not defined, don't continue. */
	   
	   if (fldstage < 0)
	      return;
	   
	   if ( floodtsHead != NULL )
	   {
	      numvals = ListCount(&floodtsHead->list);
	   }

	   if (numvals <= 1) return;
	   
	   /* set the first value in the time series to be the
	      previous value, for use later. */
	   
	   if ( floodtsHead != NULL )
	   {
	      floodtsPtr    = (FloodTs *) ListFirst(&floodtsHead->list);
	      prev_stage    = floodtsPtr->value;
	      status = yearsec_dt_to_timet(floodtsPtr->obstime, &prevstage_timet);     
	      floodtsPtr = (FloodTs *)  ListNext(&floodtsPtr->node);  
	   }
	   
	   /* loop on the number of stage values in chronological order.
	      note that because of the order of the looping and because of the
	      if checks included below, if there are multiple pass thru
	      stages, the last one will be represented. */
	   
	   while(floodtsPtr)
	   {
	      /* set the current stages  */
	      
	      stage  = floodtsPtr->value;
	      status = yearsec_dt_to_timet(floodtsPtr->obstime, &stage_timet);     
		    
	      
	      /* check if the stage value has risen above the flood stage; 
		 compute the time of stage via interpolation */
	      
	      if (prev_stage <= fldstage && fldstage <= stage)
	      {
		 /* set the rise above value if appropriate. it is considered
		    a rise above if it hits the flood stage precisely; i.e. it does
		    not have to actually be greater than fld stage, just equal to it */
		 
		 if (prev_stage == stage)
		    riseabove_timet = prevstage_timet;
		 else
		    riseabove_timet = stage_timet - 
		       ((stage - fldstage) / (stage - prev_stage)) *
		       (stage_timet - prevstage_timet);
	      }
	      
	      
	      /* check if the stage value has passed below the flood stage.
		 for the fall below check, it is considered a fall
		 below if it hits the flood stage precisely; i.e. it does
		 not have to actually be greater than fld stage, just equal to it */
	      
	      if (prev_stage >= fldstage && fldstage > stage)
	      {
		 if (prev_stage == stage)
		    fallbelow_timet = prevstage_timet;
		 else
		    fallbelow_timet = stage_timet - 
		       ((stage - fldstage) / (stage - prev_stage)) *
		       (stage_timet - prevstage_timet);
	      }
	      
	      
	      /* shift the current data to be the previous and get the next entry */
	      
	      prev_stage      = stage;
	      prevstage_timet = stage_timet;  
	      
	      floodtsPtr = (FloodTs *)  ListNext(&floodtsPtr->node);  
	   } 
	 
	   
	   /* return with the values */
	      
	   *above_time = riseabove_timet;
	   *below_time = fallbelow_timet;
	   
	   
	   return;
	}


	/*********************************************************************

	   sort_events()   
	   sort flood events by riverbasin, then stream, then location().
	   assumes that sort array is already malloced in calling function.
	   
	   ********************************************************************/
	void sort_events(UniqueList	*ulHead,
			 int		*sort_order)
	{
	   int		ul_count = 0 ;
   UniqueList	*ulPtr = NULL ; 
   LocView	*lvHead = NULL , *lvPtr = NULL ;
   char		where[200];
   char		tmp_lid[LOC_ID_LEN + 1];
   int		sort_index, match_found, i;
   char        **values = NULL ;
   int          count;

   
   /* make sure there are data to process */
  
   if ( ulHead != NULL )
   {
      ul_count = ListCount(&ulHead->list);
   }

   if (ul_count == 0)
      return;
   
   
   /* initialize */
   
   for (i = 0; i < ul_count; i++)
      sort_order[i] = MSG;
   
   
   /* get a list of locations that are candidates for the list -
      i.e. locations with a valid flood stage.  The sorting is done
      by river basin and stream so use the LocView database view
      since it already exists. */
   
   sprintf(where, "WHERE lid IN (SELECT lid FROM riverstat WHERE "
	   " fs IS NOT NULL AND fs > 0.0) ORDER BY rb, stream ");
   lvHead = GetLocView(where);
   if (lvHead == NULL)
   {
      printf("Sort not performed since no matching stations in LocView.\n");
      return;
   }
   
   /* loop on the list of candidates */
   
   sort_index = 0; 
   lvPtr = (LocView *) ListFirst(&lvHead->list);

   while (lvPtr)
   {      	 
      
      /* loop on the actual entries in the event list */
      
      match_found = 0;
      ulPtr = (UniqueList *) ListFirst(&ulHead->list);
      for (i = 0;  i < ul_count;  i++)
      {
         memset(&tmp_lid, '\0', sizeof(tmp_lid));      
         values = ParseUnique ( ulPtr, & count );
         strncpy(tmp_lid, values[0], LOC_ID_LEN);
         FreeParseUnique ( values );
         values = NULL;
	 
         /* check if the lid matches those actually in the list.
         if match found, note it in the sort array but
         don't break out of for loop in the event that 
         there are more events for this lid. */


         if (strcmp(lvPtr->lid, tmp_lid) == 0)
         {
            sort_order[sort_index] = i;
            sort_index++; 
            match_found = 1;
         }


         /* break out of for loop to save processing in the event 
         that there are no more entries for a station we had
         just had a match for. */

         else if (match_found)
         {
            break;
         }

         ulPtr = (UniqueList *) ListNext(&ulPtr->node);
      } /* for loop */
      
      lvPtr = (LocView *)  ListNext(&lvPtr->node);        

   } /* while */


   return;
}

/************************************************************************************************
*   Function Name: insert_cresttable
*   Description: Takes the selected flood entry and prompts the user before inserting the entry into the crest table.
*************************************************************************************************/

void insert_cresttable(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget               qstDS,
                        okPB;
   char                 buf[MAX_BUF_LEN], msg[MAX_BUF_LEN];
   int                  *poslist = NULL, cnt = 0;
  


   XmListGetSelectedPos(fr_stagesLI, &poslist, &cnt);       //Get's the position for the currently selected item in the Stages list

   if (cnt)
   {
      sprintf(buf, "Do you wish to insert this entry into the Crest table?");
      qstDS = QuestionDialog(floodreptDS, buf);
      SetTitle(qstDS, "Insert Confirmation");
  

      /* get the XmMessageBox ok button,
           and associate a callback to it */
  
      okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
      XtAddCallback(okPB, XmNactivateCallback, (XtCallbackProc) floodrept_insert, NULL);
  
  
      if(! XtIsManaged(qstDS))
         XtManageChild(qstDS);
   }
   else
   {
      memset(msg, '\0', sizeof(msg));
      sprintf(msg, "You must select a flood event!");
      ErrorDialog(floodreptDS, msg);
   }   
   if (poslist)
      free(poslist);
   return;
}


/*************************************************************************************************
*  Function Name : floodrept_insert
*  Description   : Retrieves the currently selected item and inserts it into the crest table
**************************************************************************************************/
void    floodrept_insert(Widget w, XtPointer ptr, XtPointer cbs) 
{
   FloodReportWorkArea  *frwa = getFrWorkArea();

   UniqueList           *ulPtr = NULL;


   int   		*poslist = NULL;
   int 			cnt = 0;
   int                  counter = 0;
   int                  rtn = 0;
   int                  error = 0;
   int                  discharge = 0;
   int                  status;
   char                 lid[LOC_ID_LEN + 1];
   char                 msg[MAX_BUF_LEN];
   char                 *stagesstring = NULL;
   char                 *tokenPtr = NULL;
   char                 *flooddate = NULL;
   char                 *floodtime = NULL;
   char                 *tokenizedstring[4];
   double               value;
   date_t               date;
   Crest                crest;
   XmString             *strlist;
   char         ** values = NULL;
   int          count;
  

  
   if (frwa->floodtsPtr)
   {
      FreeFloodTs(frwa->floodtsPtr);
      frwa->floodtsPtr = NULL;
   }
   XmListGetSelectedPos(floodreptLI, &poslist, &cnt);

   if (poslist && (cnt > 0))
   {
      ulPtr = (UniqueList *) ListNth(&frwa->ulPtr->list, poslist[0]);
      if (ulPtr)
      {
         memset(lid, '\0', sizeof(lid));
         
         values = ParseUnique ( ulPtr, &count ); 
         strncpy(lid, values[0], LOC_ID_LEN);

         FreeParseUnique ( values );
         values = NULL;
      }
   }
   
   if (poslist)
      free(poslist);

   XmListGetSelectedPos(fr_stagesLI, &poslist, &cnt);       //Get's the position for the currently selected item in the Stages list

   if (cnt)                                                 //Check's to make sure something was selected   
   {
     XtVaGetValues(fr_stagesLI, XmNitems, &strlist, NULL);  //Get's the currently selected item from the list and puts it in strlist

     XmStringGetLtoR (strlist[poslist[0]-1], XmFONTLIST_DEFAULT_TAG, &stagesstring);   //converts strlist into a string 

     tokenPtr = strtok(stagesstring, " ");
     if (poslist)                                          // Cleanup
        free(poslist);
     if (! *stagesstring)
       XtFree(stagesstring);
     
     while (tokenPtr != NULL) 
     {
        tokenizedstring[counter] = tokenPtr;
        tokenPtr = strtok(NULL, " ");
        counter++;
     }

     value = atof(tokenizedstring[0]);

     flooddate = tokenizedstring[2];
     status = USA_date_to_date_t ( flooddate, &date );

     if ( status == 0 )
     {
        crest.datcrst = date;
     }

     floodtime = tokenizedstring[3];
     strcpy(crest.timcrst, floodtime);
   
     discharge = stage2discharge(lid, value);
 
     strcpy(crest.lid,lid);
     strcpy(crest.cremark, "Inserted from the FloodTS table via Hydrobase");
     (void) SetNull(CHAR, (void *) &crest.hw);
     (void) SetNull(CHAR, (void *) &crest.jam);
     (void) SetNull(CHAR, (void *) &crest.olddatum);
     if (discharge == RATING_CONVERT_FAILED)
        (void) SetNull(INT, (void *) &crest.q);  
     else 
        crest.q = discharge; 
     crest.stage = value;
     (void) SetNull(CHAR, (void *) &crest.suppress);
     strcpy(crest.timcrst, floodtime);
     (void) SetNull(CHAR, (void *) &crest.prelim);
     
     if ((error = PutCrest(&crest)) != 0)
        if (error == -268)
        {
           memset(msg, '\0', sizeof(msg));
           sprintf(msg, "Record already exists in the Crest table!");
           ErrorDialog(floodreptDS, msg);
           rtn = False;
        }
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/whfs/src/hb/RCS/floodrept_show.c,v $";
 static char rcs_id2[] = "$Id: floodrept_show.c,v 1.13 2006/11/08 19:05:55 gsood Exp champ $";}
/*  ===================================================  */

}
