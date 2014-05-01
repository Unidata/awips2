/*
	File:		cities_show.c
	Date:		4/1/97
	Author:		Paul Taylor
	
	Purpose:	Provide support for the Cities DS.
*/


/************************************************************************
   
   Functions to handle the setup control of cities information.
   
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

#include "City.h"

#include "DbmsUtils.h"
#include "Xtools.h"

#include "cvt_latlon.h"

#include "cities.h"
#include "cities_show.h"
#include "hybase_utils.h"


/* variables global to this file */

City		*cityHead;

char		cities_format_string[] =
"%-20s  %-2s    %9s  %9s     %ld     %9ld";

   
/************************************************************************
   
   Display the cities dialog shell.
   
   ***********************************************************************/

void	ShowCitiesDs(Widget w)
{
   if (! citiesDS)
   {      
      /* create the dialog shell and add the callbacks */
      
      create_citiesDS(GetTopShell(w));
      add_cities_cbs();
      XmListDeleteAllItems(citiesLI);  /* remove XDesigner test items */
   }
   
   
   /* manage the windows now before doing the set_selections() */
   
   if (! XtIsManaged(citiesDS))
   {
      XtManageChild(citiesFM);
      XtManageChild(citiesDS);
      XtUnmanageChild(cities_helpPB);
   }
   
   
   /* load each of the scrolled lists */

   load_citylist();
   
   return;
}


/************************************************************************
    
   Add the cities selection callbacks.
   
   ************************************************************************/

void	add_cities_cbs(void)
{
   
   Atom	wmAtom;
   
   
   /* callbacks on scrolled list of products */
   
   XtAddCallback(citiesLI, XmNdefaultActionCallback,   load_citytextCB, NULL);
   XtAddCallback(citiesLI, XmNsingleSelectionCallback, load_citytextCB, NULL);
   
   
   /* callbacks on add, update, delete pushbuttons */
   
   XtAddCallback(cities_okPB,     XmNactivateCallback, cities_closeCB,  NULL);
   XtAddCallback(cities_addPB,    XmNactivateCallback, cities_addCB,    NULL);
   XtAddCallback(cities_updatePB, XmNactivateCallback, cities_updateCB, NULL);
   XtAddCallback(cities_deletePB, XmNactivateCallback, cities_deleteCB, NULL);
   
   
   /* callbacks on atom widget */   
   
   wmAtom = XmInternAtom(XtDisplay(citiesDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(citiesDS, wmAtom, cities_closeCB, NULL);

   
   /* callbacks for TextFilter */   
   
   add_cities_textfilter_cbs();
   
   
   return;
}


void	add_cities_textfilter_cbs(void)
{
   XtAddCallback(cityitem_stateTE, XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter, (XtPointer)UPPERCASE);
   
   XtAddCallback(cityitem_latTE, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
				(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));
   XtAddCallback(cityitem_lonTE, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
				(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));

   XtAddCallback(cityitem_popTE, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);

   return;
}


void	remove_cities_textfilter_cbs(void)
{
   XtRemoveCallback(cityitem_stateTE, XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter, (XtPointer)UPPERCASE);
   
   XtRemoveCallback(cityitem_latTE, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
					(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));
   XtRemoveCallback(cityitem_lonTE, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
					(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));
   XtRemoveCallback(cityitem_popTE, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);

   return;
}


/************************************************************************
   
   Load the scrolled lists for cities info.
   
   ***********************************************************************/

void load_citylist(void)
{
   XmStringTable        xmStr;
   Arg                  arg[10];
   int                  ac, i;
   int			cnt;
   char			liststr[80];

   char			lat[10];
   char			lon[10];
   
   City 		*cityPtr;
   char 		where[60];
   
   
   /* free any memory allocated */
   
   free_cities();
   
   
   /* load the list of cities from the database */
   
   XmListDeleteAllItems(citiesLI);
   sprintf(where, " ORDER BY state, name ");   
   cityHead = GetCity(where);

   cnt = 0;
   if (cityHead != NULL)
   {
      cnt = ListCount(&cityHead->list);
   }
   
   if (cnt == 0)
   {
      return;
   }   

   
   /* allocate the Motif list strings */
   
   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
   
   
   /* load the strings into the Motif strings */
   
   cityPtr = (City *) ListFirst(&cityHead->list);
   for (i = 0; i < cnt; i++)
   {
      memset(&lat, '\0', sizeof(lat));
      memset(&lon, '\0', sizeof(lon));

      strcpy(lat, cvt_latlon_from_double(cityPtr->lat));
      strcpy(lon, cvt_latlon_from_double(cityPtr->lon));
      
      if (cityPtr->population < 0) cityPtr->population = 0;
         
      sprintf(liststr, cities_format_string,
	      cityPtr->name, cityPtr->state, lat, lon,
	      cityPtr->disp_precedence, cityPtr->population);
      
      xmStr[i] = XmStringCreateSimple(liststr);
      
      cityPtr = (City *) ListNext(&cityPtr->node);
   }
   
   
   /* load the list in the scrolled list */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(citiesLI, arg, ac);
   DeSensitize(cities_updatePB);
   DeSensitize(cities_deletePB);
   
   
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

void load_citytextCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   City		*cityPtr;
   int 		*listitems;
   int 		listcnt;
   int		itemnum;

   char		lat[10];
   char		lon[10];
   char		pop[20];
   
   
   remove_cities_textfilter_cbs();

   
   /* determine which item is selected */
   
   XmListGetSelectedPos(citiesLI, &listitems, &listcnt);
   
   
   /* get the product text and display it */
   
   if (listcnt > 0)
   {
      itemnum = listitems[0];
      cityPtr = (City *) ListNth(&cityHead->list, itemnum);
      if (cityPtr != NULL)
      {      
	 XmTextSetString(cityitem_cityTE,  cityPtr->name);
	 XmTextSetString(cityitem_stateTE, cityPtr->state);
	 
	 memset(&lat, '\0', sizeof(lat));
	 memset(&lon, '\0', sizeof(lon));
#ifdef DEBUG
printf("lat == <%4.5lf>, lon == <%4.5lf>\n",
       cityPtr->lat, cityPtr->lon);
#endif
	 strcpy(lat, cvt_latlon_from_double(cityPtr->lat));
	 strcpy(lon, cvt_latlon_from_double(cityPtr->lon));
	 XmTextSetString(cityitem_latTE, lat);
	 XmTextSetString(cityitem_lonTE, lon);
	 
	 SetMenuPos(cityitem_precOM, cityPtr->disp_precedence - 1);
	 
	 memset(&pop, '\0', sizeof(pop));
	 sprintf(pop, "%ld", cityPtr->population);
	 XmTextSetString(cityitem_popTE, pop);
      }
      
      else
	 fprintf(stderr, "Error getting item from City list.\n");
      
      free(listitems);
   }
   else
   {
      clearForm(cityitem_FM);
   }

   
   if (ListRsrcGetSelectedCount(citiesLI))
   {
      Sensitize(cities_updatePB);
      Sensitize(cities_deletePB);
   }
   else
   {
      DeSensitize(cities_updatePB);
      DeSensitize(cities_deletePB);
   }

   
   add_cities_textfilter_cbs();

   
   return;
}


/************************************************************************
   
   Add an item to the scrolled list (and thus to the database).
   
   **********************************************************************/

void cities_addCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   City 	cityinfo;
   int 		status;
   char		msgstr[80];
   
   int		rv = False;
   
   
   /* get the items from the text fields */
   
   rv = read_city_info(&cityinfo);
   if (rv == False)
   {
      return;
   }

   
   /* now add the info */
   
   status = PutCity(&cityinfo);
   
   if (status < 0)
   {
      if ((status == -239) || (status == -268))
	 sprintf(msgstr,
		 "Could not add record because duplicate "
		 "information was provided.");
      else if (status == -391)
	 sprintf(msgstr,
		 "Could not add record.  CITY and STATE must be specified.");
      else
	 sprintf(msgstr, "Could not add record (error %d)", status);
      
      ErrorDialog(citiesDS, msgstr);
   }
   
   
   /* update the display of the list */
   
   else
   {
      load_citylist();
      cities_findcity(&cityinfo);
   }
   
   
   return;
}


/************************************************************************
   
  Update the database using the most-recently-entered user-data.
   
   **********************************************************************/

void cities_updateCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   City 	cityinfo;
   City		*cityPtr;
   int 		status;
   char		where[80];
   char		msgstr[80];
   
   int		rv = False;
   
   
   /* determine which item is selected so as to 
      build the key for the record to update */
   
   XmListGetSelectedPos(citiesLI, &listitems, &listcnt);
   
   if (listcnt == 0)
   {
      sprintf(msgstr, "Could not perform update since no item was selected.");
      ErrorDialog(citiesDS, msgstr);
      return;
   }
   else
   {
      itemnum = listitems[0];
      free(listitems);
   }
   
   
   cityPtr = (City *) ListNth(&cityHead->list, itemnum);
   if (cityPtr != NULL)
   {
      /* get the new items from the text fields */
      
      rv = read_city_info(&cityinfo);
      if (rv == False)
      {
	 return;
      }

      
      /* now update the info using the key from the item that
	 was selected and the data below.  note that it may attempt
	 to update the key fileds. */
      
      sprintf(where, " WHERE name = '%s' and state = '%s' ",
	      cityPtr->name, cityPtr->state);
      status = UpdateCity(&cityinfo, where);
      
      if (status == -692)
      {
	 sprintf(msgstr,
		 "Could not perform update.  Previous key %s still "
		 "referenced in other tables.", 
		 cityPtr->name);
	 ErrorDialog(citiesDS, msgstr);
	 return;
      }
      else if (status < 0)
	    ErrorDialog(citiesDS, DbErrorString(status));
      
      /* update the display of the list */
      
      else
      {
	 load_citylist();
	 cities_findcity(&cityinfo);
      }
   }
   
   else
      fprintf(stderr, "Error getting item to update from City list.\n");
   
   
   return;
}


/************************************************************************
   
   Delete an item from the scrolled list (and thus the database).
   
   **********************************************************************/

void cities_deleteCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   City 	*cityPtr;
   int 		status;
   char		where[60];
   char		msgstr[80];
   
   
   
   SetCursor(citiesFM, XC_watch);
   
   
   /* determine which item is selected */
   XmListGetSelectedPos(citiesLI, &listitems, &listcnt);

   if (listcnt == 0)
   {
      UnsetCursor(citiesFM);
      sprintf(msgstr, "Could not perform delete since no item was selected.");
      ErrorDialog(citiesDS, msgstr);
      return;
   }
   else
   {
      itemnum = listitems[0];
      free(listitems);
   }
   
   
   cityPtr = (City *) ListNth(&cityHead->list, itemnum);
   if (cityPtr != NULL)
   {      
      sprintf(where, " WHERE name = '%s' and state = '%s' ",
	      cityPtr->name, cityPtr->state);
      status = DeleteCity(where);
      
      if (status < 0)
      {
	 if (status == -692)
	    sprintf(msgstr,
		    "Unable to perform delete.  %s is still being "
		    "referenced in other tables.", 
		    cityPtr->name); 
	 else
	    sprintf(msgstr,
		    "Unable to perform delete (error %d)", status); 
	 
	 ErrorDialog(citiesDS, msgstr);
      }
      
      
      /* update the display of the list */
      
      else
      {
	 load_citylist();
      }
   }
   
   else
      fprintf(stderr, "Error getting item to delete from City list.\n");

   
   UnsetCursor(citiesFM);
   
   
   return;
}


/************************************************************************
   
   Get the info from the text fields.
   
   **********************************************************************/

int	read_city_info(City *cityinfo)
{
   char		*valstr;
   
   int		rv = False;


   valstr = XmTextGetString(cityitem_cityTE);
   strcpy(cityinfo->name, valstr);
   XtFree(valstr);
   
   
   valstr = XmTextGetString(cityitem_stateTE);
   if (strncmp(valstr,"", 2) == 0)
   {
      ErrorDialog(citiesDS, "Please enter a State ID.");
      XtFree(valstr);
      return(rv);
   }
   else
   {
     strcpy(cityinfo->state, valstr);
     XtFree(valstr);
   }
   
   valstr = XmTextGetString(cityitem_latTE);
   if (hb_check_lat_bounds(valstr))
   {
      cityinfo->lat = cvt_spaced_format(valstr, 0);
   }
   else
   {
      ErrorDialog(citiesDS, "Please enter a VALID (-90 to 90) Latitude.");
      XtFree(valstr);
      return(rv);
   }
   XtFree(valstr);

   
   valstr = XmTextGetString(cityitem_lonTE);
   if (hb_check_lon_bounds(valstr))
   {
      cityinfo->lon = cvt_spaced_format(valstr, 0);
   }
   else
   {
      ErrorDialog(citiesDS, "Please enter a VALID (-180 to 180) Longitude.");
      XtFree(valstr);
      return(rv);
   }
   XtFree(valstr);

   
   cityinfo->disp_precedence = GetMenuPos(cityitem_precOM) + 1;

   
   valstr = XmTextGetString(cityitem_popTE);
   cityinfo->population = atol(valstr);
   

   rv = True; /* success */
   return(rv);
}


/************************************************************************
   
   Find function for city.
   
   **********************************************************************/

void cities_findcity(City *cityPtr)
{
   char		liststr[80];
   XmString	item;
   int		pos;
   
   char		lat[11];
   char		lon[11];


   /*
   	Get Lat/Lon.
	Build search string,
	and attempt to locate in list.
   */
   memset(&lat, '\0', sizeof(lat));
   memset(&lon, '\0', sizeof(lon));
   
   strcpy(lat, cvt_latlon_from_double(cityPtr->lat));
   strcpy(lon, cvt_latlon_from_double(cityPtr->lon));

   if (cityPtr->population < 0) cityPtr->population = 0;

   sprintf(liststr, cities_format_string,
	   cityPtr->name, cityPtr->state, lat, lon,
	   cityPtr->disp_precedence, cityPtr->population);
   
   item = XmStringCreateSimple(liststr);
   
   if (XmListItemExists(citiesLI, item))
   {
      pos = XmListItemPos(citiesLI, item);
      XmListSetPos(citiesLI, pos);
      XmListSelectPos(citiesLI, pos, True);
   }
   else
   {
      XmListSetPos(citiesLI, 1);
      XmListSelectPos(citiesLI, 1, True);
   }
   XmStringFree(item);

   
   return;
}


/************************************************************************
   
   Free any memory allocated for the data.
   
   **********************************************************************/

void free_cities(void)
{
   if (cityHead != NULL)
   {
      FreeCity(cityHead);
      cityHead = (City *)NULL;
   }
   
   return;
}


/************************************************************************
   
   Close the window.
   
   **********************************************************************/

void cities_closeCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   /* free any allocated memory */
   free_cities();

   
   if (XtIsManaged(citiesDS))
   {
      XtDestroyWidget(citiesDS);
      citiesDS = NULL;
   }
   
   return;
}


