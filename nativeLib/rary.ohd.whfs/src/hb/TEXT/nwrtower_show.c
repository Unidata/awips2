/*
	File:		nwrtower_show.c
	Date:		October 1998
	Author:		Russ Erb
	
	Purpose:	Provide support for the NWR DS.
	
*/


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <X11/cursorfont.h>
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "NWRTransmitter.h"
#include "CountyTransmit.h"
#include "Counties.h"
#include "user_prefs.h"
#include "nwrtower_show.h"
#include "nwrtower.h"
#include "hybase.h"
#include "Wfo.h"
#include "cnty_cbs.h"
#include "cvt_latlon.h"
#include "hybase_utils.h"

int		nwrtower_state;
char		nwrtower_call_sign[LONG_CODE_LEN + 1];

void	nwrtower_show(Widget w)
{
        Wfo     *wfo, *wfoPtr;
        XmStringTable   xmStr;
        int             cnt = 0,
                        i = 0;

	if (! nwrDS)
	{	
		create_nwrDS(GetTopShell(w));
		
		wfo = GetWfo(" ORDER BY wfo ");
		wfoPtr = (Wfo *) ListFirst(&wfo->list);
                cnt = ListCount(&wfoPtr->list);
                xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
		while (wfoPtr)
		{
                    xmStr[i] = XmStringCreateSimple(wfoPtr->wfo);
                    i++;
                    wfoPtr = (Wfo *) ListNext(&wfoPtr->node);
        	}
                XmListAddItems(nwrconsoleLI, xmStr, cnt, 1);
                for (i = 0; i < cnt; i++) 
                  XmStringFree (xmStr[i]);
                XtFree((char *) xmStr);

		nwrtower_callbacks();
	}
	

	if (! XtIsManaged(nwrDS))
	{
		nwrtower_state = nwrtower_load();
		nwravailcty_load();
		XtManageChild(nwrFM);
		XtManageChild(nwrDS);
	}
	
	return;
}


void	nwrtower_callbacks(void)
{
	Atom		atom;
	
	/*
		Window manager callbacks.
	*/
	atom = XmInternAtom(XtDisplay(nwrDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(nwrDS, atom, nwr_close, NULL);
	
	
	/*
		Widget callbacks.
	*/
        XtAddCallback(nwrtowerLI,     XmNdefaultActionCallback,   nwrtower_import, NULL);
        XtAddCallback(nwrtowerLI,     XmNbrowseSelectionCallback, nwrtower_import, NULL);

	XtAddCallback(nwrcntystPB,    XmNactivateCallback, nwrtower_show_cntyst, NULL);
	XtAddCallback(nwrtwrclearPB,  XmNactivateCallback, nwrtower_clear, NULL);
	XtAddCallback(nwrtwrapplyPB,  XmNactivateCallback, nwrtower_apply, NULL);
	XtAddCallback(nwrtwrdeletePB, XmNactivateCallback, nwrtower_del_conf, NULL);

	XtAddCallback(nwrctyaddPB,    XmNactivateCallback, nwrcounty_add, NULL);
	XtAddCallback(nwrctydeletePB, XmNactivateCallback, nwrcounty_del_conf, NULL);

	XtAddCallback(nwrclosePB,     XmNactivateCallback, nwr_close, NULL);

	
	/*
		Add TextFilter callbacks.
	*/
	nwrtower_add_callbacks();
	
	
	return;
}


void	nwrtower_add_callbacks(void)
{
   XtAddCallback(nwrcall_signTE,XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
   
   return;
}


void	nwrtower_remove_callbacks(void)
{
   XtAddCallback(nwrcall_signTE, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
   
   return;
}


void	nwrtower_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
   	(void) nwrtower_save();
	return;
}


int	nwrtower_save(void)
{
	NWRTransmitter	nwrtransmitter;
	char		*buf,
			*tok,
			key[MAX_BUF_LEN],
			msg[MAX_BUF_LEN],
                        *SLdata = NULL;
	int		error,
	   		pos = 0,
			rtn,
                        cnt = 0, 
                        *position = NULL;
        XmString        *strlist;


	/*
		Associate lid to field entry.
	*/
	memset(&nwrtransmitter, '\0', sizeof(nwrtransmitter));	
	
	/*
		Get values from XmText widgets.
	*/
	if ( (buf = XmTextGetString(nwrpowerTE)) )
	{
                nwrtransmitter.transmit_power = atoi(buf);
		XtFree(buf);
	}

	if ( (buf = XmTextGetString(nwrfreqTE)) )
	{
                nwrtransmitter.transmit_freq = atof(buf);
		XtFree(buf);
	}

	if ( (buf = XmTextGetString(nwrlatTE)) )
	{
		if (hb_check_lat_bounds(buf))
		{
		   nwrtransmitter.lat = cvt_spaced_format(buf, 0);
		}
		else
		{
		   ErrorDialog(nwrDS, "Please enter a VALID (-90 to 90) Latitude.");
		   XtFree(buf);
		   return(False);
		}
		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(nwrlonTE)) )
	{
		if ( (hb_check_lon_bounds(buf)) )
		{
		   nwrtransmitter.lon = cvt_spaced_format(buf, 0);
		}
		else
		{
		   ErrorDialog(nwrDS, "Please enter a VALID (-180 to 180) Longitude.");
		   XtFree(buf);
		   return(False);
		}
		XtFree(buf);
	}
	
		
	if ( (buf = XmTextGetString(nwrcall_signTE)) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(nwrtransmitter.call_sign, buf);
		else
		{
		   ErrorDialog(nwrDS, "Call Sign is a required field.");
		   return(False);
		}
		
		XtFree(buf);
	}
		

	if ( (buf = XmTextGetString(nwrcityTE)) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(nwrtransmitter.city, buf);
		XtFree(buf);
	}


	if ( (buf = XmTextGetString(nwrprodcodeTE)) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(nwrtransmitter.transmit_prod_code, buf);
		XtFree(buf);
	}

	if ( (buf = XmTextGetString(nwrpseudoctyTE)) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(nwrtransmitter.transmit_countynum, buf);
		XtFree(buf);
	}

	if ( (buf = XmTextGetString(nwrcntystTE)) )
	{
		tok = strtok(buf, ",");
		strcpy(nwrtransmitter.county, buf);
		
		tok = strtok(NULL, " \t\n\0");
                if (tok != NULL)
                {
		  strcpy(nwrtransmitter.state, tok);
	        }	
		XtFree(buf);
	}


	if ( (buf = XmTextGetString(nwrareaTE)) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(nwrtransmitter.coverage_area, buf);
		XtFree(buf);
	}
        
        XmListGetSelectedPos(nwrconsoleLI, &position, &cnt);
        if (cnt)
        {
                XtVaGetValues(nwrconsoleLI, XmNitems, &strlist, NULL);
                XmStringGetLtoR(strlist[position[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
                strcpy(nwrtransmitter.wfo, SLdata);
        } 

	if (XmToggleButtonGetState(nwrtwractiveTB))
	   nwrtransmitter.use_transmitter[0] = 'T';
	else
	   nwrtransmitter.use_transmitter[0] = 'F';

	/*
                Insert/Update the entered dbms
                record, based on the state of
                the state variable.
        */
	rtn = True;
        if (nwrtower_state)
       	{
	   nwrtower_key(key, &pos);
	   if (strlen(key))
	   {
	      if ((error = UpdateNWRTransmitter(&nwrtransmitter, key)) != 0)
	      {
	 	 if (error == -692)
	 	 {
	 	    sprintf(msg, "Unable to change Call Sign for NWR Transmitter %s.\n"
	 	                 "It currently has at least one county assigned to it.", nwrtower_call_sign );
		    ErrorDialog(nwrDS, msg);
		    rtn = False;
		 }
	 	 else
	 	 {
	 	    sprintf(msg, "Unable to update NWR Transmitter %s, Informix error: %d",
	 	            nwrtower_call_sign, error);
		    ErrorDialog(nwrDS, msg);
		    rtn = False;
		 }
	      }
	   }
       	}
        else
	{
           if ((error = PutNWRTransmitter(&nwrtransmitter)) != 0)
	   {
	      if (error == -268)
	      {
	         sprintf(msg, "Unable to insert NWR Transmitter %s.\n This Call Sign already exists.", nwrtower_call_sign);
	         ErrorDialog(nwrDS, msg);
	         rtn = False;
	      }
	      else
	      {
	         sprintf(msg, "Unable to insert NWR Transmitter %s, Informix error: %d",
	                 nwrtower_call_sign, error);
	         ErrorDialog(nwrDS, msg);
	         rtn = False;
	      }
	   }
        }
	
        	
        /*
        	Reset Insert/Update state to True.
		
		Find the newly added/modified entry & select the position.
		Return.
        */
        nwrtower_state = nwrtower_load();
	
	find_nwrtower_pos(&nwrtransmitter, &pos);
	XmListSetPos(nwrtowerLI, pos);
	XmListSelectPos(nwrtowerLI, pos, True);
	    
	return(rtn);
}


void	nwr_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if(XtIsManaged(nwrDS))
   {
      XtDestroyWidget(nwrDS);
      nwrDS = NULL;
   }

   return;
}


void	nwrtower_clear(Widget w, XtPointer ptr, XtPointer cbs)
{
   	clearForm(nwrtowerFM);
	Sensitize(nwrtwrapplyPB);
	nwrtower_state = False;
	return;
}


void	nwrtower_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{        
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete Transmitter %s?", nwrtower_call_sign);
   qstDS = QuestionDialog(nwrDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, nwrtower_delete, NULL);
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	nwrtower_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
   char	key[MAX_WHERE_LEN],
        msg[MAX_BUF_LEN];
   int	error = 0,
        pos = 0;
   
   
   /*
   	Delete record from dbms,
   	and reload list and select pos.
   */
   SetCursor(nwrFM, XC_watch);
   nwrtower_key(key, &pos);
   if (strlen(key))
   {
      if ((error = DeleteNWRTransmitter(key)) != 0)
      {
         if (error == -692)
         {
            sprintf(msg, "Can not delete NWR Transmitter %s.\nThere is at least one county associated with it.",
                    nwrtower_call_sign);
            ErrorDialog(nwrDS, msg); 
         }
         else
         {
            sprintf(msg, "Unable to delete NWR Transmitter %s, Informix error: %d",
                    nwrtower_call_sign, error);
            ErrorDialog(nwrDS, msg);
         }
      }
      (void) nwrtower_load();
      XmListSelectPos(nwrtowerLI, pos, True);
   }
   UnsetCursor(nwrFM);
   
   return;
}


void	nwrtower_key(char *key, int *pos)
{
   NWRTransmitter	*nwrtransmitter,
      		        *nwrtPtr;
   
   char		where[MAX_WHERE_LEN];
   
   int		*poslist,
      		cnt;
   
   
   XmListGetSelectedPos(nwrtowerLI, &poslist, &cnt);
   sprintf(where, " ORDER BY call_sign ");
   if ((nwrtransmitter = GetNWRTransmitter(where)) != NULL)
   {
      nwrtPtr = (NWRTransmitter *) ListNth(&nwrtransmitter->list, poslist[0]);
      if (nwrtPtr)
      {
	 memset(where, '\0', sizeof(where));
	 sprintf(where, " WHERE call_sign = '%s' ", nwrtPtr->call_sign);
	 
	 *pos = poslist[0];
	 strcpy(key, where);
	 XtFree((char *)poslist);
      }
      
      FreeNWRTransmitter(nwrtransmitter);
   }
   
   return;
}



int	nwrtower_load(void)
{
	XmStringTable	xmStr;
	NWRTransmitter	*nwrtransmitter,
			*nwrtPtr;
	char		where[MAX_WHERE_LEN],
			buf[MAX_BUF_LEN],
			lat[11],
			lon[11];
	int		state,
			cnt,
			i;

	/*
		Initialize state of display.
	*/
/*
	clearForm(nwrtowerFM);
*/
 	XmListDeleteAllItems(nwrtowerLI);
	state = False;
                
        sprintf(where, " ORDER BY call_sign ");
        if ((nwrtransmitter = GetNWRTransmitter(where)) != NULL)
        {
                cnt     = ListCount(&nwrtransmitter->list);
                xmStr   = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
                nwrtPtr = (NWRTransmitter *) ListFirst(&nwrtransmitter->list);
                for (i = 0; nwrtPtr; i++)
                {
                	if (IsNull(FLOAT, (void*) &nwrtPtr->transmit_freq)) nwrtPtr->transmit_freq = 0.0;
                        if (nwrtPtr->transmit_power < 0) nwrtPtr->transmit_power = 0;
			strcpy(lat, cvt_latlon_from_double(nwrtPtr->lat));
			strcpy(lon, cvt_latlon_from_double(nwrtPtr->lon));
                        
                        sprintf(buf, "%-6s %-3s %-20s %-20s %2s %-25s %8s %9s %7.3f  %4ld  %3s  %4s  %s",
                                nwrtPtr->call_sign, nwrtPtr->wfo, nwrtPtr->city, nwrtPtr->county,
                                nwrtPtr->state, nwrtPtr->coverage_area, lat, lon,
                                nwrtPtr->transmit_freq, nwrtPtr->transmit_power,
                                nwrtPtr->transmit_prod_code, nwrtPtr->transmit_countynum,
                                nwrtPtr->use_transmitter);
                        xmStr[i] = XmStringCreateSimple(buf);
                        nwrtPtr = (NWRTransmitter *) ListNext(&nwrtPtr->node);
                }


		XmListAddItems(nwrtowerLI, xmStr, cnt, 1);
                XmListSelectPos(nwrtowerLI, 1, True);
 
        	/*
                	cleanup.
        	*/
        	for (i = 0; i < cnt; i++)
                	XmStringFree(xmStr[i]);
		XtFree((char *) xmStr);
                FreeNWRTransmitter(nwrtransmitter);
 
                
		Sensitize(nwrtwrclearPB);
		Sensitize(nwrtwrapplyPB);
                Sensitize(nwrtwrdeletePB);
                state = True;
        }
	else
	{
		DeSensitize(nwrtwrclearPB);
	   	DeSensitize(nwrtwrdeletePB);
	}
	
	return(state);	
}


void    nwrtower_import(Widget w, XtPointer ptr, XmListCallbackStruct *cbs)
{
        NWRTransmitter          *nwrtransmitter,
                        	*nwrtPtr;
        char            	where[MAX_WHERE_LEN],
                        	buf[MAX_BUF_LEN];
        XmString                item;
        int                     pos = 0;

	nwrtower_remove_callbacks();
	
	
        /*
                Retrieve data for ALL NWR Towers from the
                dbms, and search for the currently selected
                offset in the XmList widget.  Update
                Xm widgets on XmForm work area with
                appropriate values.
        */
        sprintf(where, " ORDER BY call_sign ");
        if ((nwrtransmitter = GetNWRTransmitter(where)) != NULL)
	{
		nwrtPtr = (NWRTransmitter *) ListNth(&nwrtransmitter->list, cbs->item_position);
        	if (nwrtPtr)
        	{
                	XmTextSetString(nwrcall_signTE, nwrtPtr->call_sign);
                	XmTextSetString(nwrcityTE,      nwrtPtr->city);
                	XmTextSetString(nwrprodcodeTE,  nwrtPtr->transmit_prod_code);
                	XmTextSetString(nwrpseudoctyTE, nwrtPtr->transmit_countynum);
                	XmTextSetString(nwrareaTE,      nwrtPtr->coverage_area);

			if (nwrtPtr->use_transmitter[0] == 'T')
		            XmToggleButtonSetState(nwrtwractiveTB, True, False);
		         else
		            XmToggleButtonSetState(nwrtwractiveTB, False, False);

                	strcpy(buf, nwrtPtr->county);
                	strcat(buf, ", ");
                	strcat(buf, nwrtPtr->state);
                 	XmTextSetString(nwrcntystTE, buf);

			if ((! IsNull(DOUBLE, (void*) &nwrtPtr->lat)) &&
			    (! IsNull(DOUBLE, (void*) &nwrtPtr->lon)))
			{
			   XmTextSetString(nwrlatTE, cvt_latlon_from_double(nwrtPtr->lat));
			   XmTextSetString(nwrlonTE, cvt_latlon_from_double(nwrtPtr->lon));
			}
			else
			{
			   XmTextSetString(nwrlatTE, cvt_latlon_from_double(0.0));
			   XmTextSetString(nwrlonTE, cvt_latlon_from_double(0.0));
			}
      
                	if (IsNull(FLOAT, (void*) &nwrtPtr->transmit_freq)) nwrtPtr->transmit_freq = 0.0;
                	sprintf(buf, "%7.3f", nwrtPtr->transmit_freq);
                	XmTextSetString(nwrfreqTE,  buf);

                	if (nwrtPtr->transmit_power < 0) nwrtPtr->transmit_power = 0;
                	sprintf(buf, "%4ld", nwrtPtr->transmit_power);
                	XmTextSetString(nwrpowerTE, buf);
                        
                        item = XmStringCreateSimple(nwrtPtr->wfo);
                        pos = XmListItemPos(nwrconsoleLI, item);
                        XmListSetPos(nwrconsoleLI, pos);
                        XmListSelectPos(nwrconsoleLI, pos, True);

                        /* Modified ... Changed XtFree to XtFreeString. */
                        XmStringFree ( item );
                	
			strcpy(nwrtower_call_sign, nwrtPtr->call_sign);
			nwrcounty_load(nwrtower_call_sign);

        	}


        	/*
                	Cleanup.        
        	*/
        	FreeNWRTransmitter(nwrtransmitter);     
        }
        
        
	nwrtower_add_callbacks();
	
	
        return;
}


void	find_nwrtower_pos(NWRTransmitter *nwrtPtr, int *pos)
{
   char		buf[MAX_BUF_LEN],
		lat[11],
		lon[11];
   XmString	xmStr;

   strcpy(lat, cvt_latlon_from_double(nwrtPtr->lat));
   strcpy(lon, cvt_latlon_from_double(nwrtPtr->lon));
   
   sprintf(buf, "%-6s %-3s %-20s %-20s %2s %-25s %8s %9s %7.3f  %4ld  %3s  %4s  %s",
           nwrtPtr->call_sign, nwrtPtr->wfo, nwrtPtr->city, nwrtPtr->county,
           nwrtPtr->state, nwrtPtr->coverage_area, lat, lon,
           nwrtPtr->transmit_freq, nwrtPtr->transmit_power,
           nwrtPtr->transmit_prod_code, nwrtPtr->transmit_countynum,
           nwrtPtr->use_transmitter);
   xmStr = XmStringCreateSimple(buf);
   *pos = XmListItemPos(nwrtowerLI, xmStr);
   XmStringFree(xmStr);
   
   
   return;
}


void	nwravailcty_load(void)
{
	XmStringTable	xmStr;
	Counties	*cntyPtr;
	Counties	*all_counties;
	char		where[MAX_WHERE_LEN],
			buf[MAX_BUF_LEN];
	int		cnt,
			ctr;

	XmListDeleteAllItems(nwravailctyLI);
               
	sprintf(where, " ORDER BY state, county ");
	if ((all_counties = GetCounties(where)) != NULL)
	{
            cnt     = ListCount(&all_counties->list);
            xmStr   = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
            cntyPtr = (Counties *) ListFirst(&all_counties->list);
	    for (ctr = 0; cntyPtr; ctr++)
	    {
	         sprintf(buf, "%-20s %-2s", cntyPtr->county, cntyPtr->state);
	         xmStr[ctr] = XmStringCreateSimple(buf);
	         cntyPtr = (Counties *) ListNext(&cntyPtr->node);
	    }

	    XmListAddItems(nwravailctyLI, xmStr, cnt, 1);
            XmListSelectPos(nwravailctyLI, 1, True);
 
	   /*
	       	cleanup.
	   */
	   for (ctr = 0; ctr < cnt; ctr++)
	      	XmStringFree(xmStr[ctr]);
	   XtFree((char *) xmStr);
	   FreeCounties(all_counties);
 
           Sensitize(nwrctyaddPB);
	}
	else
	{
	   DeSensitize(nwrctyaddPB);
	}

}


void	nwrcounty_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{        
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete this County from Transmitter %s?", nwrtower_call_sign);
   qstDS = QuestionDialog(nwrDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, nwrcounty_delete, NULL);
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	nwrcounty_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
   char	key[MAX_WHERE_LEN];
   int	pos;
   
   
   /*
   	Delete record from dbms,
   	and reload list and select pos.
   */
   SetCursor(nwrFM, XC_watch);
   nwrcounty_key(key, &pos);
   if (strlen(key))
   {
      DeleteCountyTransmit(key);
      nwrcounty_load(nwrtower_call_sign);
      XmListSetPos(nwrctytwrLI, pos);
      XmListSelectPos(nwrctytwrLI, pos, True);
   }
   UnsetCursor(nwrFM);
   
   return;
}


void	nwrcounty_key(char *key, int *pos)
{
   CountyTransmit	*nwrcPtr,
			*countytransmit;
   
   char		where[MAX_WHERE_LEN];
   
   int		*poslist,
      		cnt;
   
   
   XmListGetSelectedPos(nwrctytwrLI, &poslist, &cnt);
   sprintf(where, " WHERE call_sign='%s' ORDER BY state, county ", nwrtower_call_sign);
   if ((countytransmit = GetCountyTransmit(where)) != NULL)
   {
      nwrcPtr = (CountyTransmit *) ListNth(&countytransmit->list, poslist[0]);
      if (nwrcPtr)
      {
	 memset(where, '\0', sizeof(where));
	 sprintf(where, " WHERE call_sign='%s' AND county = '%s' AND  state = '%s' ",
	                  nwrcPtr->call_sign, nwrcPtr->county, nwrcPtr->state);
	 
	 *pos = poslist[0];
	 strcpy(key, where);
	 XtFree((char *)poslist);
      }
      
      FreeCountyTransmit(countytransmit);
   }
   
   return;
}

void	nwrcounty_load(const char *call_sign)
{
	XmStringTable	xmStr;
        CountyTransmit          *nwrcPtr,
        			*countytransmit;
        char            	where[MAX_WHERE_LEN],
                        	buf[MAX_BUF_LEN];
	int		cnt,
			ctr;

	XmListDeleteAllItems(nwrctytwrLI);
	sprintf(buf, "Counties covered by Transmitter: %-6s", call_sign);
        SetLabel(nwrctytwrLA, buf);     
                   
        sprintf(where, " WHERE call_sign='%s' ORDER BY state, county ", call_sign);
        if ((countytransmit = GetCountyTransmit(where)) != NULL)
        {
	   cnt     = ListCount(&countytransmit->list);
	   xmStr   = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
	   nwrcPtr = (CountyTransmit *) ListFirst(&countytransmit->list);
	   for (ctr = 0; nwrcPtr; ctr++)
	   {
	      sprintf(buf, "%-20s %-2s", nwrcPtr->county, nwrcPtr->state);
	      xmStr[ctr] = XmStringCreateSimple(buf);
	      nwrcPtr = (CountyTransmit *) ListNext(&nwrcPtr->node);
	   }


	   XmListAddItems(nwrctytwrLI, xmStr, cnt, 1);
	   XmListSelectPos(nwrctytwrLI, 1, True);
 
       	   /*
               	cleanup.
       	   */
       	   for (ctr = 0; ctr < cnt; ctr++)
               	XmStringFree(xmStr[ctr]);
	   XtFree((char *) xmStr);
           FreeCountyTransmit(countytransmit);
 
           Sensitize(nwrctydeletePB);
       	}
	else
	{
   	   DeSensitize(nwrctydeletePB);
	}

}


void	nwrtower_show_cntyst(Widget w, XtPointer ptr, XtPointer cbs)
{
	ShowCntyDs(GetTopShell(w), FROM_NWR);
	return;
}

void    nwrcounty_add(Widget w, XtPointer ptr, XtPointer cbs)
{
   char			where[MAX_WHERE_LEN],
			msg[MAX_BUF_LEN];
   
   int			*pos_list = NULL,
   			pos_cnt = 0,
      			error = 0,
			nth;
   
   Counties		*nthPtr,
			*all_counties;

   CountyTransmit	newcounty;


   XmListGetSelectedPos(nwravailctyLI, &pos_list, &pos_cnt);
   if (pos_cnt == 1)
   {
	nth = pos_list[0];

	XtUnmapWidget(nwrctytwrLI);
      
	/*
		Get the nth item from the available list and
		Add it to selected list.
	*/

	sprintf(where, " ORDER BY state, county ");
	if ((all_counties = GetCounties(where)) != NULL)
	{
	    nthPtr = (Counties *) ListNth(&all_counties->list, nth);
	    memset(&newcounty, '\0', sizeof(newcounty));	
	    strcpy(newcounty.state,      nthPtr->state);
	    strcpy(newcounty.county,     nthPtr->county);	    
	    FreeCounties(all_counties);
	}
	
	strcpy(newcounty.call_sign,  nwrtower_call_sign);

	if( (error = PutCountyTransmit(&newcounty)) !=0 )
	{
	   if (error == -268)
	   {
	       sprintf(msg, "Unable to Add County %s, %s because it is\n"
	                    " already assigned to NWR Transmitter %s.",
	                    newcounty.county, newcounty.state, newcounty.call_sign);
	       ErrorDialog(nwrDS, msg);
	   }
	}

	XtMapWidget(nwrctytwrLI);
	nwrcounty_load(nwrtower_call_sign);
      
	/*
		Cleanup.
	*/      
	   free(pos_list);
	   pos_list = NULL;

   } /* end of if pos_cnt == 1 */
   
   return;
}
