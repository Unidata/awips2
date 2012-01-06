/*
	File:		load_stats.c
	Purpose:	Provide support for loading the main HydroBase
			station list.
	
*/


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/Text.h>

#include "DbmsUtils.h"  /* utilities */
#include "DbmsDefs.h"
#include "Xtools.h"
#include "cvt_latlon.h"

#include "LocView.h"    /* database view */

#include "callbacks.h"  /* hydrobase includes */
#include "user_prefs.h"
#include "hybase.h"
#include "hbAS.h"

/**********************************************************************/
/*
	Global variable to allow subsequent searches on	the lid field.
*/
LocView		*locview;


int	load_stats(const char *where, int pos)
{
	XmStringTable	xmStr;
	LocView		*lvPtr = NULL;
	
	char		buf[MAX_BUF_LEN],
			tmpbuf[MAX_BUF_LEN],
			tmpstr[MAX_BUF_LEN];	
	int		code,
	   		cnt = 0,
			i;	
	char		lat[11];
	char		lon[11];
        time_t 		checktime;
   
   
        time(&checktime);
        printf("Load station list begin = %s", asctime(gmtime(&checktime)));
	
					
	/*
		Free any previously allocated memory.
	*/
	if (locview)
		FreeLocView(locview);
		
	
	/*
		Retrieve the appropriate code indicating user
		preferences for main window field display.
	*/
	code = get_field_preference();
	set_list_label(code);

	
	/*
		Get the list of valid locations from the
		hydrologic database.
	*/
	if ((locview = GetLocView((char *) where)) != NULL)
	{
		cnt = ListCount(&locview->list);
	        xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
   		
        	lvPtr = (LocView *) ListFirst(&locview->list);
		if (lvPtr != NULL)
		{
        	  for (i = 0; lvPtr; i++)
        	 {
        		/* ONLY display first 25 charaters for station name */
			
			memset(&tmpstr, '\0', sizeof(tmpstr));
			strncpy(tmpstr, lvPtr->name, 25);
        		sprintf(buf, "%-8s %-25s", lvPtr->lid, tmpstr);
			
			
			/*
				If county desired, add to buffer.
			*/
			if (code & FLD_COUNTY)
			{
				sprintf(tmpbuf, " %2s,%-20s", 
				        lvPtr->state,lvPtr->county);
				strcat(buf, tmpbuf);
			}
			
			
			/*
				If basin desired, add to buffer.
			*/
			if (code & FLD_BASIN)
			{
				memset(&tmpstr, '\0', sizeof(tmpstr));
			        strncpy(tmpstr, lvPtr->rb, 25);
			  	sprintf(tmpbuf, " %-25s", tmpstr);
				strcat(buf, tmpbuf);
			}
			
			
			/*
				If stream desired, add to buffer.
			*/
			if (code & FLD_STREAM)
			{
				memset(&tmpstr, '\0', sizeof(tmpstr));
			        strncpy(tmpstr, lvPtr->stream, 25);   
			   	sprintf(tmpbuf, " %-25s", tmpstr);
				strcat(buf, tmpbuf);
			}
			
			
			/*
				If latitude/longitude desired, add to buffer.
			*/
			if (code & FLD_LATLON)
			{
				memset(&lat, '\0', sizeof(lat));
				memset(&lon, '\0', sizeof(lon));
				strcpy(lat, cvt_latlon_from_double(lvPtr->lat));
				strcpy(lon, cvt_latlon_from_double(lvPtr->lon));
				sprintf(tmpbuf, " %9s  %9s", lat, lon);
				strcat(buf, tmpbuf);
			}
			
			
                  	xmStr[i] = XmStringCreateSimple(buf);
        		lvPtr = (LocView *) ListNext(&lvPtr->node);
        	    }
                 }
		
		/*
			Delete old items
		*/
		XmListDeleteAllItems(hbmainLI);
		
		
		/*
                	Load the list box with the selected items.
        	*/
  		XmListAddItems(hbmainLI, xmStr, cnt, 1);
     		XmListSelectPos(hbmainLI, pos, True);
		clear_search_window();	


        	/*
                	cleanup and return.
        	*/		
        	for (i = 0; i < cnt; i++)
                	XmStringFree(xmStr[i]);
		XtFree((char *) xmStr);	
	}
	
	else
	{
		XmListDeleteAllItems(hbmainLI);
	}

        time(&checktime);
        printf("Load station list done  = %s", asctime(gmtime(&checktime)));
	
	return(cnt);
}


/**********************************************************************/

int	search_stats(const char *str)
{
	LocView		*lvPtr = NULL ;
	int		len,
			i;
	
        if ( str != NULL )	
        {
	   len    = strlen(str);
	   lvPtr  = (LocView *) ListFirst(&locview->list);
           
	   if (lvPtr != NULL)
	   {
   	     for (i = 0; lvPtr; i++)
	     {
	      if (strncmp(lvPtr->lid, str, len) == 0)
		  return(++i);
			
	      lvPtr = (LocView *) ListNext(&lvPtr->node);
	     }
           }
        }
	
	/*
		Never got a match or the input string "str" contained a 
                NULL value. Return a negative value to indicate 
		failure.
	*/
	return -1 ;
}


/**********************************************************************/
/* sets the availability of the river station menu options based on
   whether the location is a river station */
   
void	list_state(Widget w, XtPointer ptr, XmListCallbackStruct *cbs)
{
	LocView		*lvPtr = NULL;
	char		where[MAX_WHERE_LEN];
	
	
	lvPtr = (LocView *) ListNth(&locview->list, cbs->item_position);
	
	memset(&where, '\0', sizeof(where));
	
	sprintf(where, " WHERE lid = '%s' ", lvPtr->lid);
        
	if (recordCount("riverstat", where) > 0)
	{
	   Sensitize(hbrvr_fcatPB);
	   Sensitize(hbrvr_fstmtPB);
	   Sensitize(hbrvr_lwstmtPB);
	   Sensitize(hbrvr_floodPB);
	   Sensitize(hbrvr_ratingPB);
	   Sensitize(hbrvr_uhgPB);
	   Sensitize(hbrvr_crestPB);
	   Sensitize(hbrvr_lwPB);
	   Sensitize(hbrvr_bmPB);
	   Sensitize(hbrvr_datumPB);
	   Sensitize(hbrvr_descrPB);	 
	   Sensitize(hbrvr_pubPB);
	   Sensitize(hbrvr_refPB);   
	}
	  
	else
	{
	   DeSensitize(hbrvr_fcatPB);
	   DeSensitize(hbrvr_fstmtPB);
	   DeSensitize(hbrvr_lwstmtPB);
	   DeSensitize(hbrvr_floodPB);
	   DeSensitize(hbrvr_ratingPB);
	   DeSensitize(hbrvr_uhgPB);
	   DeSensitize(hbrvr_crestPB);
	   DeSensitize(hbrvr_lwPB);
	   DeSensitize(hbrvr_bmPB);
	   DeSensitize(hbrvr_datumPB);
	   DeSensitize(hbrvr_descrPB);	
	   DeSensitize(hbrvr_pubPB);
	   DeSensitize(hbrvr_refPB);	   
	}
	  
	
	memset(&where, '\0', sizeof(where));
	sprintf(where, " WHERE lid = '%s' ", lvPtr->lid);
	
	list_lid_match(lvPtr->lid);
      	
	
	return;
}


/**********************************************************************/
/*
	Determine if the currently selected item
	in the main location list matches the value,
	if any, in the lid search window. If it does, it clears
	the search window.
*/

void	list_lid_match(const char *lid)
{
	char		*buf;
	int		len;
	
	
	fflush(stdout);
	len = 0;
	buf = (char *) NULL;
	if ( (buf = XmTextGetString(hblidTE)) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
		{
			len = strlen(buf);
			if (strncmp(lid, buf, len) != 0)
				clear_search_window();
		}
		XtFree(buf);
	}
}


/**********************************************************************/

void	set_list_label(const long code)
{
   	char		buf[MAX_BUF_LEN],
	   		tmp[MAX_BUF_LEN];
	
	
	/*
		Set up permanent fields.
	*/
	sprintf(buf, "Station     Name                              ");
	
	
	/*
		Compare fields, including selected items in
		the main window list label.
	*/
	if (code & FLD_COUNTY)
	{
	   	sprintf(tmp, "State,County                 ");
		strcat(buf, tmp);
	}
	
	
	if (code & FLD_BASIN)
	{
	   	sprintf(tmp, "Basin                               ");
		strcat(buf, tmp);
	}
	
	
	if (code & FLD_STREAM)
	{
		sprintf(tmp, "Stream                               ");
		strcat(buf, tmp);
	}
	
	
	if (code & FLD_LATLON)
	{
	   	sprintf(tmp, "  Latitude/Longitude");
		strcat(buf, tmp);
	}
	
	
	/*
		Set widget label.
	*/
	SetLabel(hbmainLA, buf);
	
   	return;
}

