
/*
	File:		loc_show.c
	Date:		August 1994
	Author:		Dale Shelton / Chip Gobs
			Paul Taylor

	Purpose:	Provides support for the Location DS.

	Modified:	02/28/2002 - added TimeZome widget logic
*/


/*
	Standard includes.
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>
/***** POSTGRES
#include <sqlhdr.h>
*****/

/*
	Motif/X11 includes.
*/
#include <Xm/Xm.h>
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>


/*
	Local library includes.
*/
#include "agencyoffice_show.h"
#include "Xtools.h"
#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "Location.h"
#include "ParamDefs.h"
#include "Admin.h"
#include "StnClass.h"
#include "set_stnclass.h"
#include "TimeZone.h"

/*
	Local includes.
*/
#include "callbacks.h"
#include "user_prefs.h"
#include "loc_cbs.h"
#include "loc.h"
#include "hbAS.h"
#include "admin.h"
#include "cnty.h"
#include "cnty_cbs.h"
#include "stachar.h"
#include "stachar_show.h"
#include "hybase_utils.h"
#include "cvt_latlon.h"
#include "copy_lid.h"
#include "agencyoffice.h"
#include "time_convert.h"
#include "GeneralUtil.h"

#define LOC_AO_INITCOUNT   "( 0 Cooperators )"


/*
	Global widgets.
*/
Widget  	wsfoPB;
Widget  	hsaPB;
Widget  	coePB;
Widget  	cpmPB;


char		loc_lid[LOC_ID_LEN + 1];


void	ShowLocDs(Widget w, char *lid)
{
	if (! locDS)
	{
	   create_locDS(GetTopShell(w));
	   loc_create_btns();
	   loc_callbacks();
	}
	

	if (! XtIsManaged(locDS))
	{
	   locSetSensitivity(False);
	   memset(loc_lid, '\0', sizeof(loc_lid));	
	   if ( lid != NULL )
	   {
	      strcpy(loc_lid, lid);
	      locSetSensitivity(True);
	   }
	   
	   if(w == hbloc_addPB)
	   {
	      set_window_title(locDS, "Add Location", (char *) loc_lid);
	      XmTextSetEditable(lidTxt, True);
	      DeSensitize(loccopyPB);
	      DeSensitize(locagcyofficePB);
	   }
	   else
	   {
	      set_window_title(locDS, "Modify Location", (char *) loc_lid);
	      XmTextSetEditable(lidTxt, False);
	      Sensitize(loccopyPB);
	      Sensitize(locagcyofficePB);
	   }
	   SetLabel(locagcyoffice_countLA, LOC_AO_INITCOUNT);
	   
	   SetMenuPos(lmainOM, 0);
	   loc_checkPB(locgeoPB, NULL, NULL);
	   LocImport(loc_lid);
	   
	   if (w == hbloc_addPB)
	   {
	      loc_removeTextFilterCallbacks();
	      XmTextSetString(lidTxt, "XXXXX");
	      XmTextSetString(cntystTxt, "XXXXXXXXXXXXXXXXXXXX, XX");
	      loc_addTextFilterCallbacks();
	      
	      
	      /* default to always post when adding a station */
	      
	      XmToggleButtonSetState(lspostTB, True, False);
              init_locPBs();
	   }
	   
	   XtManageChild(lmainFM);
	   XtManageChild(locDS);
	}


	return;
}


void	ShowLocCopyDs(Widget w)
{
	if (! loccopyDS)
	{
	   create_loccopyDS(GetTopShell(w));
	   loccopy_callbacks();
	   
	   XmTextSetString(loccopysrcTE, loc_lid);
	   XmToggleButtonSetState(loccopyinclallTB, True, False);
	}
	
	if (! XtIsManaged(loccopyDS))
	{
	   XtManageChild(loccopyFO);
	   XtManageChild(loccopyDS);
	}
	
   	return;
}


void	loc_checkPB(Widget w, XtPointer ptr, XtPointer cbs)
{
	if (w == locgeoPB)
	{
		XtSetMappedWhenManaged(lsubFrm, False);

	   	XtSetMappedWhenManaged(lgeoFrm, True);
		XtSetMappedWhenManaged(rmkFrm,  True);
		XtSetMappedWhenManaged(lviewonlyFR, True);
	}
	else if(w == locaddPB)
	{
		XtSetMappedWhenManaged(lgeoFrm, False);
		XtSetMappedWhenManaged(rmkFrm,  False);
		XtSetMappedWhenManaged(lviewonlyFR, False);

	   	XtSetMappedWhenManaged(lsubFrm, True);
	}
	return;
}


/*
     	Used to Sensitize and DeSensitize PB's that
	require the current lid to be in the database.
*/
void 	locSetSensitivity(Boolean set)
{
      	if (set)
	{     
	     	Sensitize(ldeletePB);
	}	
	else
	{
	     	DeSensitize(ldeletePB);
	}     
	return;
}


void	close_loc(Widget w, XtPointer ptr, XtPointer cbs)
{
  	/*
   		Determine if any of the subwindows
		are managed, and if so unmanage them.
	*/
        if ( cntyDS != NULL )
        {
	   if (XtIsManaged(cntyDS))
	      close_cnty(NULL, NULL, NULL);
        }
	
        if ( loccopyDS != NULL )
        {
	   if (XtIsManaged(loccopyDS))
	      close_loccopy(NULL, NULL, NULL);
        }
	
	/*
		Unmanage the TopLevel shell for 
		this entry form.
	*/
        if ( locDS != NULL )
        {
	   if (XtIsManaged(locDS))
	   {
	      XtDestroyWidget(locDS);
	      locDS = NULL;
	   }
        }
	
	return;
}


void	loc_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   strcpy(buf, "Do you wish to delete all entries for this location?");
   qstDS = QuestionDialog(locDS, buf);			
   SetTitle(qstDS, "Delete Confirmation");

   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, delete_loc, NULL);
   
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}



void    delete_loc(Widget w, XtPointer ptr, XtPointer cbs)
{
   	char		buf[MAX_BUF_LEN],
	   		msg[MAX_BUF_LEN];

	int		pos,
	   		error,
	   		found, 
                        i;
	

	SetCursor(lmainFM, XC_watch);

	
	/*
		Save the position of the item to be deleted.
	*/
	found=False;
	if((pos=search_stats(loc_lid)) > 0)
	   found=True;
	
	
	/*
		Delete all entries from the database
		with the associated location id.
	*/
	sprintf(buf, " delete_location ( '%s' ) ", loc_lid);
	if ((error = execFunction(buf)) != 0)
	{
	   	sprintf(msg, "Unable to delete location - %s: %d\n", 
			loc_lid, error);
		ErrorDialog(locDS, msg);
	}
	
	
	/*
		Update the main window if the item to be deleted
		was previously present.
	*/
	if(found == True)
	{
	   XtUnmapWidget(hbmainLI);
           set_stat_list(NULL, NULL, NULL);
           if((i=search_stats(loc_lid)) > 0)
           {
               XmListSelectPos(hbmainLI, i, True);
               XmListSetPos(hbmainLI, i);
           }
            
	   XmListSelectPos(hbmainLI, pos, True);
	   XmListSetPos(hbmainLI, pos);
	   XtMapWidget(hbmainLI);
	}	

	
	/*
		Close the window and return.
	*/
	UnsetCursor(lmainFM);
	close_loc(w, NULL, NULL);

	return;
}


void	init_locPBs(void)
{	
	Admin		*admin;
        XmString        item;
        int             pos = 0;


	if ((admin = GetAdmin("")) != NULL)
	{
                item = XmStringCreateSimple(admin->hsa);
                pos = XmListItemPos(wfoLI, item);
                XmListSetPos(wfoLI, pos);
                XmListSelectPos(wfoLI, pos, True);
 
                item = XmStringCreateSimple(admin->hsa);
                pos = XmListItemPos(hsaLI, item);
                XmListSetPos(hsaLI, pos);
                XmListSelectPos(hsaLI, pos, True);
		FreeAdmin(admin);
	}
	else
	{
                XmListSelectPos(wfoLI, 1, True);
                XmListSelectPos(hsaLI, 1, True);
	}
	return;
}


int	save_loc(void)
{   
   	Location	loc;
	char 		*buf,
	   		*tok,
			where[MAX_WHERE_LEN],
			msg[MAX_WHERE_LEN],
                        *SLdata = NULL;
   date_t   datet;
	int		rcdCount,
	   		error,
	   		rtn, 
                        *pos = NULL, 
                        cnt = 0,
                        i;
        XmString        *strlist;
       
	

	/*
		Initialize local variables.
	*/
	memset(&loc, '\0', sizeof(loc));

		
	/*
		Get a new copy of loc_lid.
		Read data from all the widgets & copy into structure.	
	*/
	strcpy(loc_lid, XmTextGetString(lidTxt));
	sprintf(where, " WHERE lid = '%s' ", loc_lid);
	rcdCount = recordCount("location", where);
	strcpy(loc.lid, loc_lid);


	/*
		Retrieve data from widgets.
	*/
	if ( (buf = XmTextGetString(cntystTxt)) )
	{
		tok = strtok(buf, ",");
		strcpy(loc.county, buf);
		
		tok = strtok(NULL, " \t\n\0");
		strcpy(loc.state, tok);
		
		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(detailTxt)) )
	{
		strcpy(loc.detail, buf);
		XtFree(buf);
	}
	
	
	(void) strcat(loc.detail, " ");
	(void) strcat(loc.detail, GetLabel(GetMenuHistory(dtlPDM)));

	
	if ( (buf = XmTextGetString(elevTxt)) )
	{
		loc.elev = atof(buf);
		XtFree(buf);
	}
	
	buf = XmTextGetString ( latTxt ) ;

	if ( buf != NULL )
	{
		if ( hb_check_lat_bounds ( buf ) )
		{
		   loc.lat = cvt_spaced_format(buf, 0);
		}
		else
		{
		   ErrorDialog(locDS, "Please enter a VALID (-90 to 90) "
                                      "Latitude.");
		   XtFree(buf);
		   return(False);
		}
		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(lonTxt)) )
	{
		if (hb_check_lon_bounds(buf))
		{
		   loc.lon = cvt_spaced_format(buf, 0);
		}
		else
		{
		   ErrorDialog(locDS, "Please enter a VALID (-180 to 180) Longitude.");
		   XtFree(buf);
		   return(False);
		}
		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(rmkTxt)) )
	{
		strcpy(loc.lremark, buf);
		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(rvsTxt)) )
	{
      if ( strlen(buf) == 0)
      {
         SetNull(LONG, (void *) &loc.lrevise);
      }
		else if (four_digit_year(buf) != 0)
		{
		  sprintf(msg, "Invalid date '%s' entered, 4 digit year required.\n", 
			  buf);
		  ErrorDialog(locDS, msg);
		  /* leave function, do not save data */
		  return(False);
		}
		else
		{
        if ( USA_date_to_date_t ( buf, &datet ) != 0 )
		  {
		    sprintf(msg, "Invalid date '%s' entered, check month and day.\n", 
			    buf);
		    ErrorDialog(locDS, msg);
		    /* leave function, do not save data */
		    return(False);
		  }
		  else
		    loc.lrevise = datet;
		}
			
		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(nameTxt)) )
	{
		strcpy(loc.name, buf);
		XtFree(buf);
	}
	
		
	if ( (buf = XmTextGetString(rbTxt)) )
	{
		strcpy(loc.rb, buf);
		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(lsnTxt)) )
	{
		strcpy(loc.sn, buf);
		XtFree(buf);
	}

	
	if ( (buf = XmTextGetString(lsdesTxt)) )
	{
	 	strcpy(loc.des, buf);
		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(lsdetTxt)) )
	{
	 	strcpy(loc.det, buf);
		XtFree(buf);
	}
	 
	
	if ( (buf = XmTextGetString(lshdatumTxt)) )
	{
	 	strcpy(loc.hdatum, buf);
		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(lshuTxt)) )
	{
	 	strcpy(loc.hu, buf);
		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(stntypeTxt)) )
	{
		strcpy(loc.stntype, buf);
		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(lssbdTxt)) )
	{
      if ( strlen(buf) == 0)
      {
         SetNull(LONG, (void *) &loc.sbd);
      }
		else if (four_digit_year(buf) != 0)
		{
		  sprintf(msg, "Invalid date '%s' entered, 4 digit year required.\n", 
			  buf);
		  ErrorDialog(locDS, msg);
		  /* leave function, do not save data */
		  return(False);
		}
		else
		{
        if ( USA_date_to_date_t ( buf, &datet ) != 0 )
		  {
		    sprintf(msg, "Invalid date '%s' entered, check month and day.\n", 
			    buf);
		    ErrorDialog(locDS, msg);
		    /* leave function, do not save data */
		    return(False);
		  }
		  else
		    loc.sbd = datet;
		}
			
		XtFree(buf);
	}
	

	loc.post = 0;
	if (XmToggleButtonGetState(lspostTB))
		loc.post = 1;
	
	
	/*
		Get values from XmOptionMenu(s).
	*/
	(void) strcpy(loc.network, GetLabel(GetMenuHistory(netPDM)));	
	(void) strcpy(loc.rfc,     GetLabel(GetMenuHistory(rfcPDM)));	
	(void) strncpy(loc.tzone,  GetLabel(GetMenuHistory(tzPDM)), 8);
   /* strip off trailing blanks from timezone */
   strip_tblanks(loc.tzone);

        /* 
                Get values from the Scrolled lists (WFO/HSA)
        */
 
        XmListGetSelectedPos(wfoLI, &pos, &cnt);
        if (cnt)
        {
          XtVaGetValues(wfoLI, XmNitems, &strlist, NULL);
          XmStringGetLtoR(strlist[pos[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
          strcpy(loc.wfo, SLdata);
        }

        XmListGetSelectedPos(hsaLI, &pos, &cnt);
        if (cnt)
        {
          XtVaGetValues(hsaLI, XmNitems, &strlist, NULL);
          XmStringGetLtoR(strlist[pos[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
          strcpy(loc.hsa, SLdata);
        }

	/*
		If station is "Inactive",
		store an 'I' in the loc.type field.
	*/
	strcpy(loc.type, "");
	if (XmToggleButtonGetState(loc_inactiveTB))
	   strcat(loc.type, "I");
	

	/*
		Save to the database.
	*/
	
	rtn = True;
	if (rcdCount)
	{
	   if(XmTextGetEditable(lidTxt)) /* editable == Add New Lid operation */
	   {
	      error = True;
	      sprintf(msg, "Location exists already...please try another.");
	      ErrorDialog(locDS, msg);
	      rtn = False;	      
	   }
	   else /* not editable == Modify Lid operation */
	   {
	      if ((error = UpdateLocation(&loc, where)) != 0)
	      {
		 sprintf(msg, "Unable to update record: %d\n", error);
		 ErrorDialog(locDS, msg);
		 rtn = False;
	      }
	   }
	}
	else
	{
		if ((error = PutLocation(&loc)) != 0)
		{
			sprintf(msg, "Unable to insert record: %d\n", error);
			ErrorDialog(locDS, msg);
			rtn = False;
		}
                else
                {
                        set_stat_list(NULL, NULL, NULL);

                        if((i=search_stats(loc_lid)) > 0)
                        {
                               XmListSelectPos(hbmainLI, i, True);
                               XmListSetPos(hbmainLI, i);
                        }
                }

	}
	

	/*
		Set the window sensitivity to allow subsequent
		delete operations.
	*/
	if (error == False)
	{
		XmTextSetEditable(lidTxt, False);
		locSetSensitivity(True);
		Sensitize(loccopyPB);
		Sensitize(locagcyofficePB);
		set_window_title(locDS, "Modify Location", loc_lid);
		set_stat_pos(loc_lid);
		LocImport(loc_lid);
	}
	return(rtn);
}
   


void	ok_loc(Widget w, XtPointer ptr, XtPointer cbs)
{
	if (save_loc())
		close_loc(w, NULL, NULL);
	return;
}


void	apply_loc(Widget w, XtPointer ptr, XtPointer cbs)
{
	save_loc();
	return;
}



void	ok_loccopy(Widget w, XtPointer ptr, XtPointer cbs)
{
   char		*new_lid = NULL;
   Location	*locPtr = NULL;
   char		where[MAX_WHERE_LEN];
   
   
   new_lid = XmTextGetString(loccopydestTE);
   if (strlen(new_lid) == 0)
   {
      ErrorDialog(loccopyDS, "Please enter the new Location Id...\n");
      return;
   }

   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", new_lid);
   locPtr = (Location *) GetLocation(where);
   if (locPtr != NULL)
   {
      FreeLocation(locPtr);
      ErrorDialog(loccopyDS, "That Location exists already.\n"
		  "Please try another...\n");
      return;
   }   

   
   SetCursor(loccopyFO, XC_watch);
   SetCursor(lmainFM, XC_watch);
   SetCursor(hbFO, XC_watch);

   if (XmToggleButtonGetState(loccopyinclallTB))
   {
      CopyLid_All(loc_lid, new_lid);
   }
   else
   {
      CopyLid_Reference(loc_lid, new_lid);
   }
   
   UnsetCursor(loccopyFO);
   UnsetCursor(lmainFM);
   UnsetCursor(hbFO);
   
   
   close_loccopy(NULL, NULL, NULL);
   set_stat_list(NULL, NULL, NULL);
   if (new_lid)
      XtFree(new_lid);
   
   
   return;
}


void	close_loccopy(Widget w, XtPointer ptr, XtPointer cbs)
{
   if(XtIsManaged(loccopyDS))
   {
      XtDestroyWidget(loccopyDS);
      loccopyDS = NULL;
   }
   
   return;
}


void	LocImport(char *lid)
{

   Location	*loc=NULL;
   
   char		where[255],
      		buf[MAX_BUF_LEN],
      		datestring[DATE_LEN+1],
      		*dir=NULL,
      		*txt=NULL;
   
   int		i, pos = 0;
   
   TimeZone	*tzPtr=NULL;
   XmString     item;
   char		timeZoneString[TZ_LEN + TZ_NAME_LEN + 5];
   
   /*
   	Temporarily remove Text callbacks (add them back later).
   	Clear the application form widgets.
   */
   loc_removeTextFilterCallbacks();
   clearForm(locFM);
   clearForm(rmkFM);
   clearForm(lsubFM);
   
   
   /*
   	Retrieve the location record from the DBMS.
   */	
   sprintf(where, " WHERE lid = '%s' ", lid);
   if ((loc = GetLocation(where)) != (Location *) NULL)
   {
      /*
      		Load the text strings.
      */
      XmTextSetString(lidTxt,  loc->lid);
      XmTextSetString(nameTxt, loc->name);
      XmTextSetString(rbTxt,   loc->rb);
      
      
      strcpy(buf, loc->county);
      strcat(buf, ", ");
      strcat(buf, loc->state);
      XmTextSetString(cntystTxt, buf);
      
      
      XmTextSetString(lsnTxt,  loc->sn);
      XmTextSetString(rmkTxt,  loc->lremark);
      if ((! IsNull(DOUBLE, (void*) &loc->lat)) &&
	  (! IsNull(DOUBLE, (void*) &loc->lon)))
      {
	 XmTextSetString(latTxt, cvt_latlon_from_double(loc->lat));
	 XmTextSetString(lonTxt, cvt_latlon_from_double(loc->lon));
      }
      else
      {
	 XmTextSetString(latTxt, cvt_latlon_from_double(0.0));
	 XmTextSetString(lonTxt, cvt_latlon_from_double(0.0));
      }
      
      
      /*
      		Indicate that a station is Inactive,
      		if there is an 'I' in the loc->type field.
      */
      for(i=0; i<strlen(loc->type); i++)
      {
	 if (loc->type[i] == 'I')
	    XmToggleButtonSetState(loc_inactiveTB, True, False);
      }
      
      
      /*
      		Set office option menus to the 
      		appropriate value.
      */
      SetMenuHistory(netOM,   loc->network);
      SetMenuHistory(rfcOM,   loc->rfc);

      item = XmStringCreateSimple(loc->wfo);
      pos = XmListItemPos(wfoLI, item);
      XmListSetPos(wfoLI, pos);
      XmListSelectPos(wfoLI, pos, True);

      item = XmStringCreateSimple(loc->hsa);
      pos = XmListItemPos(hsaLI, item);
      XmListSetPos(hsaLI, pos);
      XmListSelectPos(hsaLI, pos, True);

      /*  need to get name of tzone from TimeZone table for option menu to be set correctly */
      sprintf(where, " WHERE tzone='%s' ", loc->tzone);
      tzPtr = GetTimeZone(where);
      if (tzPtr != NULL)
	sprintf(timeZoneString, "%-8s (%s)", tzPtr->tzone, tzPtr->name);
      else
        strcpy(timeZoneString, "GMT0GMT (Greenwich Mean, UTC, Z-time)");

      SetMenuHistory(tzOM,    timeZoneString);
	 

      /*
      		Set values for the detail field if it is defined.
			
			Modify the check logic by guoxian zhou 07-2004
			Need capability to handle unnormal cases such as "4M", "M M"
			without crash.
      */

      if (strlen(loc->detail) > 0)
      {
		txt = strtok(loc->detail, " ");

/*
		if ( isdigit(txt[0]) )
		{
			XmTextSetString(detailTxt, txt);
			dir = strtok(NULL, " \t\n\0");
		}
		else
		{
			dir  = txt; 
		}		
*/
		dir = strtok(NULL, " \t\n\0");
		
		/**
		* Unnormal case:
		* only one data available, need check if it is for direction,
		* or combination of the mileage and the direction.
		* if data is like "WNW", set the mileage field to blank.
		* if data is like "4W", split the data
		* for mileage data and direction data.
		* 
		*/
		

		if(dir == NULL)
		{

			/** Split the data */			
			char *mileData, *dirData;

			mileData = (char*)malloc(strlen(txt)*sizeof(char));
			memset(mileData, '\0', sizeof(mileData));
			
			int index;

			for(index = strlen(txt) - 1; index >= 0; index --)
			{
				if(isdigit(txt[index]) || txt[index] == '.')
					break;	
			}
			
			strncpy(mileData, txt, index+1);
			mileData[index+1] = '\0';

			dirData = txt + index + 1;

			XmTextSetString(detailTxt, mileData);
			SetMenuHistory(dtlOM, dirData);
		}
		else
		{
			if ( atof(txt) == 0.0 && !(isdigit(txt[0])) )
			{
				/**
				* Unnormal case:
				* if data is like "M M", set the mileage
				* field to blank.
				*/

				XmTextSetString(detailTxt, " ");
			}
			else
				XmTextSetString(detailTxt, txt);

			SetMenuHistory(dtlOM, dir);
		}
      }
      
      
      /*
      		Set the dates.
      */
      date_t_to_USA_date ( loc->lrevise, datestring );
      XmTextSetString(rvsTxt, datestring);
      
      
      /*
      		Set elevation
      */
      DataToString(&loc->elev, DOUBLE, buf, "%5.1lf", "");
      XmTextSetString(elevTxt, buf);
      
      
      /*
      		Set sub field widgets.
      */
      XmTextSetString(lsdesTxt,    loc->des);
      XmTextSetString(lsdetTxt,    loc->det);
      XmTextSetString(lshdatumTxt, loc->hdatum);	
      XmTextSetString(lshuTxt,     loc->hu);
      XmTextSetString(stntypeTxt,  loc->stntype);
      
      
      date_t_to_USA_date ( loc->sbd, datestring );
      XmTextSetString(lssbdTxt, datestring);
      
      
      if (loc->post)
	 XmToggleButtonSetState(lspostTB, True, False);
      
      
      Loc_LoadCooperators(lid);
      Loc_UpdateStationClassFields(lid);
      
      
      /*
      		Cleanup and return.
      */
      FreeLocation(loc);
      if (tzPtr != NULL)
      	FreeTimeZone(tzPtr);
  }
   else
   {
      XmToggleButtonSetState(lspostTB, True, False);	   
   }
   
   
   loc_addTextFilterCallbacks();
   
   
   return;
}



void	Loc_LoadCooperators(char *lid)
{
   AgencyOfficeRec 	*aor = (AgencyOfficeRec *) getAgencyOfficeRec();
   int			cnt;
   char			buf[MAX_BUF_LEN];

   
   /*
   	init aor
   */
   strcpy(aor->lid, lid);
   aor->availableHead = NULL;	  
   aor->selectedHead = NULL;
   
   cnt = Loc_LoadSelectedList(aor);
   sprintf(buf, "( %-i Cooperators )", cnt);
   SetLabel(locagcyoffice_countLA, buf);
   
   return;
}



int	Loc_LoadSelectedList(AgencyOfficeRec *aor)
{
     
     LocExtAgency *aPtr = NULL;
     char where[BUFSIZ];
     LongText *text;
     int count = 0;
     int i;
     
     
     SetLabel(locagcyoffice_countLA, LOC_AO_INITCOUNT);
	   
	   
     if (aor->selectedHead)
     {
          FreeLocExtAgency(aor->selectedHead);
	  aor->selectedHead = NULL;
     }
     
     XmListDeleteAllItems(locagcyofficeLB);
     
     
     sprintf(where, "WHERE lid ='%s' order by agency_code, office", aor->lid);
     
     
     
     if ( (aor->selectedHead = GetLocExtAgency(where)) )
     {
          count= ListCount(&aor->selectedHead->list);
	  if (count)
	  {
	       text = (LongText *) malloc(sizeof(LongText) * count);
	       if (text == NULL)
	       {
		    aor->selectedHead = NULL;
	            return 0 ;     	    
	       }
	       else
	       {
		    i = 0;
		    aPtr = (LocExtAgency *) ListFirst(&aor->selectedHead->list);
	            while (aPtr)
		    {
			 sprintf(text[i], "%-10s  %-30s",
				 aPtr->agency_code,
				 aPtr->office);
			 
			 i++;
		         aPtr = (LocExtAgency *) ListNext(&aPtr->node);	 
		    }
		    
		   
		    loadXmList(locagcyofficeLB, text, i);
		    free(text);
		    text = NULL;
	       }
		   
	  }
     }

     
     return(count);
}
     


void	Loc_UpdateStationClassFields(char *lid)
{
   StnClass	*stnclass = NULL;
   char		where[MAX_WHERE_LEN];
   int		i;
   
   set_stnclass(lid);

   if (locDS != NULL)
   {
      memset(&where, '\0', sizeof(where));
      sprintf(where, " WHERE lid = '%s' ", lid);
      
      if ((stnclass = (StnClass *) GetStnClass(where)) != NULL)
      {
	 /* Set StationClass TBs (assume OFF at first). */
	 
	 XmToggleButtonSetState(lfcstptTB, False, False);
	 XmToggleButtonSetState(lrvrTB,    False, False);
	 XmToggleButtonSetState(lresvrTB,  False, False);
	 XmToggleButtonSetState(lprecipTB, False, False);
	 XmToggleButtonSetState(lsnowTB,   False, False);
	 XmToggleButtonSetState(ltempTB,   False, False);
	 XmToggleButtonSetState(lotherTB,  False, False);
	 XmToggleButtonSetState(lundefTB,  False, False);
	 
	 for(i=0; i<strlen(stnclass->disp_class); i++)
	 {
	    switch(stnclass->disp_class[i])
	    {
	       case 'F':	XmToggleButtonSetState(lfcstptTB, True, False);  break;
	       case 'R':	XmToggleButtonSetState(lrvrTB,    True, False);  break;
	       case 'D':	XmToggleButtonSetState(lresvrTB,  True, False);  break;
	       case 'P':	XmToggleButtonSetState(lprecipTB, True, False);  break;
	       case 'S':	XmToggleButtonSetState(lsnowTB,   True, False);  break;
	       case 'T':	XmToggleButtonSetState(ltempTB,   True, False);  break;
	       case 'O':	XmToggleButtonSetState(lotherTB,  True, False);  break;
	       case 'U':	XmToggleButtonSetState(lundefTB,  True, False);  break;
	       default:	fprintf(stderr, "WARNING: Station Class option '%c' not supported.\n", stnclass->disp_class[i]);
		  break;
	    }
	 }
	 
	 
	 /* Set DataSources TBs (assume OFF at first). */
	 
	 XmToggleButtonSetState(loc_dcpTB, False, False);
	 XmToggleButtonSetState(loc_obsTB, False, False);
	 XmToggleButtonSetState(loc_telemTB, False, False);
	 
	 if (stnclass->dcp[0] == 'T')
	    XmToggleButtonSetState(loc_dcpTB, True, False);
	 if (stnclass->observer[0] == 'T')
	    XmToggleButtonSetState(loc_obsTB, True, False);
	 if (strlen(stnclass->telem_type) > 0)
	    XmToggleButtonSetState(loc_telemTB, True, False);
	 
	 
	 FreeStnClass(stnclass);
      }
   }   

   return;
}



