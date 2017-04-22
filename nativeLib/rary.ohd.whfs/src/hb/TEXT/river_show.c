/*
	File:		river_show.c
	Date:		September 1995
	Author:		Dale Shelton, Paul Taylor (4/4/97)

	Purpose:	Provide support for the River Gage DS.
	
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
        Informix includes.
*/
/**** POSTGRES
#include <sqlhdr.h>
****/


/*
	Local library includes.
*/
#include "Riverstat.h"
#include "Xtools.h"
#include "callbacks.h"
#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "GeneralUtil.h"
#include "Location.h"
#include "loc_cbs.h"
#include "RpfFcstPoint.h"
#include "ShefPe.h"


/*
	Local includes.
*/
#include "user_prefs.h"
#include "mgage_cbs.h"
#include "mgage.h"
#include "river_cbs.h"
#include "grp_cbs.h"
#include "grp.h"
#include "river.h"
#include "crest_show.h"
#include "crest.h"
#include "hybase.h"
#include "hybase_utils.h"
#include "cvt_latlon.h"
#include "hbAS.h"
#include "ugc.h"
#include "ugc_show.h"
#include "time_convert.h"



/*
	Globals.
*/
extern	char	*rgroup_none;
char		rv_lid[LOC_ID_LEN + 1];
int   	rlatestfcst;


void	ShowRiverDs(Widget w, char *lid)
{
	char 	where[MAX_WHERE_LEN];
	int	rcdCount;

	if (! riverDS)
	{
		rlatestfcst = 0;
		create_riverDS(GetTopShell(w));
		river_callbacks();
	}

	
	if ( ! XtIsManaged(riverDS) )
	{
	     	riverSetSensitivity(False);
		memset(rv_lid, '\0', sizeof(rv_lid));	
		if (lid || *lid)
		{
			(void) strcpy(rv_lid, lid);
			sprintf(where, " WHERE lid = '%s' ", rv_lid);
			if ( ( rcdCount = recordCount("riverstat", where) ) )
			   riverSetSensitivity(True);
		}

		set_window_title(riverDS, "River Gage", rv_lid);
		SetMenuPos(rvrOM, 0);
		rvr_checkPB(rvrgeoPB, NULL, NULL);
		river_load(rv_lid);
		grpinfo_loadpeLI(riverinfo_peLI, grpinfo_getshefpes(False));
		grpinfo_highlightpeLI(riverinfo_peLI, rv_lid);
		XtManageChild(riverFM);
		XtManageChild(riverDS);
		XtUnmanageChild(rvhelpPB);
	}
	
	return;
}



void	rvr_checkPB(Widget w, XtPointer ptr, XtPointer cbs)
{
	if (w == rvrgeoPB)
	{
		XtSetMappedWhenManaged(rsFrm, False);
	   	XtSetMappedWhenManaged(rmainFrm, True);
		XtSetMappedWhenManaged(rrmkFrm, True);
	}
	else
	{
		XtSetMappedWhenManaged(rmainFrm, False);
		XtSetMappedWhenManaged(rrmkFrm, False);
	   	XtSetMappedWhenManaged(rsFrm, True);
	}

	return;
}


/*
     	Used to Sensitize and DeSensitize PB's that
	require the current lid to be in the database.
*/
void 	riverSetSensitivity(Boolean set)
{
      	if (set)
	{     
	     	Sensitize(rvdelPB);
	}	
	else
	{
	     	DeSensitize(rvdelPB);
	}     
	return;   
}


void	river_callbacks(void)
{	
	Atom		atom;
		
	
	/*
		Window manager callbacks.
	*/
	atom = XmInternAtom(XtDisplay(riverDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(riverDS, atom, close_river, NULL);
	
	
	/*
		General widget callbacks.
	*/

	XtAddCallback(rvrgeoPB,   XmNactivateCallback,	   rvr_checkPB,   NULL);
	XtAddCallback(rvraddPB,   XmNactivateCallback,     rvr_checkPB,   NULL);
	
/*
	XtAddCallback(fcstsvcPB,  XmNactivateCallback,     show_fcstsvc,  NULL);
	XtAddCallback(hwsvcPB,    XmNactivateCallback,     show_hwsvc,    NULL);
*/
	XtAddCallback(rgroupPB,   XmNactivateCallback,     ShowGrpDs,     NULL);

	XtAddCallback(rrvsTB,     XmNvalueChangedCallback, river_settime, NULL);

	XtAddCallback(rvokPB,     XmNactivateCallback,     ok_river,      NULL);
	XtAddCallback(rvapplyPB,  XmNactivateCallback,     apply_river,   NULL);
	XtAddCallback(rvclosePB,  XmNactivateCallback,     close_river,   NULL);
	XtAddCallback(rvdelPB,    XmNactivateCallback,     river_del_conf,NULL);
	XtAddCallback(rvhelpPB,   XmNactivateCallback,     help_river,    NULL);
	XtAddCallback(riverFcstTB,XmNvalueChangedCallback, river_fcst_TB, (XtPointer) NULL);

	
	/*
		TextFilter callbacks.
	*/
	river_addTextFilterCallbacks();
	
	
	return;
}

void river_fcst_TB ( Widget w, XtPointer ptr, XtPointer cbs )
{
	XmToggleButtonCallbackStruct *tb_state = (XmToggleButtonCallbackStruct *)cbs;
	rlatestfcst = tb_state->set;

}

void	river_addTextFilterCallbacks(void)
{
	XtAddCallback(rlatTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
					(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));
	XtAddCallback(rlonTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
					(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));

	XtAddCallback(rdaTxt,     XmNmodifyVerifyCallback,  (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(rmileTxt,   XmNmodifyVerifyCallback,  (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(rfflowTxt,  XmNmodifyVerifyCallback,  (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(rfloodsTxt, XmNmodifyVerifyCallback,  (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(ractionsTxt,XmNmodifyVerifyCallback,  (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(ractionfTxt,XmNmodifyVerifyCallback,  (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(rzdTxt,     XmNmodifyVerifyCallback,  (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
	XtAddCallback(rtrTxt,     XmNmodifyVerifyCallback,  (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS); 
	XtAddCallback(rrvsTxt,    XmNmodifyVerifyCallback,  (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);

	XtAddCallback(rsrdateTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
	XtAddCallback(rsgsnoTxt, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
	XtAddCallback(rsbfTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(rscbTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(rspoolTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	
	return;
}


void	river_removeTextFilterCallbacks(void)
{
	XtRemoveCallback(rlatTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
					(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));
	XtRemoveCallback(rlonTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, 
					(XtPointer)(INTEGERS_AND_DECIMALS_SIGN+SPACES));
	XtRemoveCallback(rdaTxt,      XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtRemoveCallback(rmileTxt,    XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtRemoveCallback(rfflowTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtRemoveCallback(rfloodsTxt,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtRemoveCallback(ractionsTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtRemoveCallback(ractionfTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtRemoveCallback(rzdTxt,      XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
	XtRemoveCallback(rtrTxt,      XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtRemoveCallback(rrvsTxt,     XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);

	XtRemoveCallback(rsrdateTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
	XtRemoveCallback(rsgsnoTxt, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
	XtRemoveCallback(rsbfTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtRemoveCallback(rscbTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtRemoveCallback(rspoolTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	
	return;
}


void	show_rmcrest(Widget w, XtPointer ptr, XtPointer cbs)
{
	crest_show(GetTopShell(w), rv_lid, True);
	return;
}

void	show_rmgage(Widget w, XtPointer ptr, XtPointer cbs)
{
	ShowMgageDs(GetTopShell(w), rv_lid);
	return;
}


void	show_fcstsvc(Widget w, XtPointer ptr, XtPointer cbs)
{
/*
	ShowFcstSvcDs(GetTopShell(w), rv_lid);
*/
	return;
}

void	show_hwsvc(Widget w, XtPointer ptr, XtPointer cbs)
{
/*
	ShowHwSvcDs(GetTopShell(w), rv_lid);
*/
 	return;
}




void	river_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{        
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete this entry?");
   qstDS = QuestionDialog(riverDS, buf);
   SetTitle(qstDS,"Delete Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, delete_river, NULL);
   
   
   if( ! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	ok_river(Widget w, XtPointer ptr, XtPointer cbs)
{
	if (save_river())
		close_river(w, NULL, NULL);

	rlatestfcst = 0;
	return;
}


void	close_river(Widget w, XtPointer ptr, XtPointer cbs)
{
   if ( ( glDS != NULL ) &&  ( XtIsManaged(glDS) ) )
      close_grp(glDS, NULL, NULL);	/* Forecast Groups */
   
   
   if (XtIsManaged(riverDS))
   {
      XtDestroyWidget(riverDS);
      riverDS = NULL;
   }
	
   return;
}


/*
	Delete all entries from the database
	with the associated location id.
*/
void	delete_river(Widget w, XtPointer ptr, XtPointer cbs)
{
   	char		buf[MAX_BUF_LEN],
	   		msg[MAX_BUF_LEN];
	int		error;
	

	SetCursor(riverFM, XC_watch);

	
	/*
		Build the SQL procedure statement.
	*/
	sprintf(buf, " delete_riverstat ( '%s' ) ", rv_lid);
	if ((error = execFunction(buf)) != 0)
	{
	   	sprintf(msg, "Unable to delete rivergage - %s: %d\n", 
			rv_lid, error);
		ErrorDialog(riverDS, msg);
	}

	Loc_UpdateStationClassFields(rv_lid);

	close_river(w, NULL, NULL);
	set_stat_pos(rv_lid);
	
	
	UnsetCursor(riverFM);
	
	
	return;
}


void	help_river(Widget w, XtPointer ptr, XtPointer cbs)
{
	return;
}


void	river_settime(Widget w, XtPointer ptr, XtPointer cbs)
{
	struct tm       *tmptr;
        time_t          starttime;
       	char    	sdate[DATE_LEN + 2],
                	where[MAX_WHERE_LEN];
	Riverstat       *river;
 

	/*
		Check XmToggle state and
		perform appropriate action.
	*/
        if (XmToggleButtonGetState(rrvsTB))
        {
                time(&starttime);
                tmptr = localtime(&starttime);
                strftime(sdate, DATE_LEN + 2, "%m/%d/%Y", tmptr);
        }
        else
        {      	
        	memset(sdate, '\0', sizeof(sdate));
                sprintf(where, " WHERE lid = '%s' ", rv_lid);
                river = GetRiverstat(where);
                if (ListCount(&river->list))
                {
                        date_t_to_USA_date ( river->rrevise, sdate );
	                FreeRiverstat(river);
	        }
        }
                  
        XmTextSetString(rrvsTxt, sdate);  
        return;
}


void	river_load(const char *lid)
{
	char            where[MAX_WHERE_LEN],
			buf[12];
	
	Riverstat	*rs;
	Location	*loc;
	
	
	
	river_removeTextFilterCallbacks();

	
	/*
		Clear the window widgets.
	*/
	clearForm(rmainFM);
	clearForm(rrmkFM);
	clearForm(rsFM);
	
	
	/*
		Load Group.
	*/
	river_load_group(lid);


	/*
		load lat and lon from Location table
	
	*/
	sprintf(where, " WHERE lid = '%s' ", lid);
	if ((loc = GetLocation(where)) != NULL)
	{
	   if (! IsNull(DOUBLE, (void*) &loc->lat) && ! IsNull(DOUBLE, (void*) &loc->lon))
	   {
	      XmTextSetString(rlatTxt, cvt_latlon_from_double(loc->lat));
	      XmTextSetString(rlonTxt, cvt_latlon_from_double(loc->lon));
	   }
	   else
	   {
	      XmTextSetString(rlatTxt, cvt_latlon_from_double(0.0));
	      XmTextSetString(rlonTxt, cvt_latlon_from_double(0.0));
	   }
	   FreeLocation(loc);
	}
	
	
	/*
		Retrieve appropriate record from the 
		dbms, and populate the related fields.
	*/
	sprintf(where, " WHERE lid = '%s' ", lid);
	if ((rs = GetRiverstat(where)) != NULL)
	{
	        /*
			Set text entries.
		*/
		XmTextSetString(rstrmTxt, rs->stream);
		if ((! IsNull(DOUBLE, (void*) &rs->lat)) &&
		    (! IsNull(DOUBLE, (void*) &rs->lon)))
		{
		   XmTextSetString(rlatTxt, cvt_latlon_from_double(rs->lat));
		   XmTextSetString(rlonTxt, cvt_latlon_from_double(rs->lon));
		}
		else
		{
		   XmTextSetString(rlatTxt, cvt_latlon_from_double(0.0));
		   XmTextSetString(rlonTxt, cvt_latlon_from_double(0.0));
		}
		XmTextSetString(rrmkTxt,    rs->remark);
		XmTextSetString(rsgsnoTxt,  rs->gsno);
		XmTextSetString(rssrcTxt,   rs->rsource);
		XmTextSetString(rslevelTxt, rs->level);
		XmTextSetString(rsporTxt,   rs->por);
		XmTextSetString(rsratedTxt, rs->rated);
		XmTextSetString(rsvdatumTxt,rs->vdatum);
		XmTextSetString(rsratenumTxt,rs->usgs_ratenum);

		
		/*
			Fill in date-related values.
		*/
                date_t_to_USA_date ( rs->rrevise, buf );
		XmTextSetString(rrvsTxt, buf);

                date_t_to_USA_date ( rs->ratedat, buf );
		XmTextSetString(rsrdateTxt, buf);
		
		
		/*
			Perform conversions and set values
			for floating values.
		*/

		if(! IsNull(DOUBLE, (void*)&rs->bf))
		   DataToString(&rs->bf, DOUBLE, buf, "%8.1lf", "");
		else
		   strcpy(buf, "");
		XmTextSetString(rsbfTxt, buf);
		
		if(! IsNull(DOUBLE, (void*)&rs->cb))
		   DataToString(&rs->cb, DOUBLE, buf, "%8.3lf", "");
		else
		   strcpy(buf, "");
		XmTextSetString(rscbTxt, buf);
		
		if(! IsNull(DOUBLE, (void*)&rs->pool))
		   DataToString(&rs->pool, DOUBLE, buf, "%8.1lf", "");
		else
		   strcpy(buf, "");
		XmTextSetString(rspoolTxt, buf);
	
		if(! IsNull(DOUBLE, (void*)&rs->da))
		   DataToString(&rs->da, DOUBLE, buf, "%-8.1lf", "");
		else
		   strcpy(buf, "");
		XmTextSetString(rdaTxt, buf);
	
		if(! IsNull(DOUBLE, (void*)&rs->mile))
		   DataToString(&rs->mile, DOUBLE, buf, "%-8.1f", "");
		else
		   strcpy(buf, "");
		XmTextSetString(rmileTxt, buf);
	
		/* set flood stage to text widget */
		if(! IsNull(DOUBLE, (void*)&rs->fs))
		   DataToString(&rs->fs, DOUBLE, buf, "%-8.2lf", "");
		else
		   strcpy(buf, "");
		XmTextSetString(rfloodsTxt, buf);
	
		/* set flood flow to text widget */
		if(! IsNull(DOUBLE, (void*)&rs->fq))
		   DataToString(&rs->fq, DOUBLE , buf, "%-8.0lf", "");
		else
		   strcpy(buf, "");
		XmTextSetString(rfflowTxt, buf);

		/* set action stage to text widget */
		if(! IsNull(DOUBLE, (void*)&rs->wstg))
		   DataToString(&rs->wstg, DOUBLE, buf, "%-8.2lf", "");
		else
		   strcpy(buf, "");
		XmTextSetString(ractionsTxt, buf);
		
		/* set action flow to text widget */
		if(! IsNull(DOUBLE, (void*)&rs->action_flow))
		   DataToString(&rs->action_flow, DOUBLE, buf, "%-8.0lf", "");
		else
		   strcpy(buf, "");
		XmTextSetString(ractionfTxt, buf);
		
		/* set zero datum to text widget */
		if(! IsNull(DOUBLE, (void*)&rs->zd))
		   DataToString(&rs->zd, DOUBLE, buf, "%-8.3f", "");
		else
		   strcpy(buf, "");
		XmTextSetString(rzdTxt, buf);
		
		/* set threshold runoff to text widget */
		if(! IsNull(DOUBLE, (void*)&rs->threshold_runoff))
		   DataToString(&rs->threshold_runoff, DOUBLE, buf, "%-8.3f", "");
		else
		   strcpy(buf, "");
		XmTextSetString(rtrTxt, buf);

		if ( strncmp(rs->use_latest_fcst,"T",1) == 0)
		{
			rlatestfcst = 1;
	 		XmToggleButtonSetState(riverFcstTB, True, False); 
		}
		
		/*
			Setup XmOptionMenu value.
		*/
		SetMenuHistory(rtidalOM, rs->tide);
		SetMenuHistory(rbckwtrOM, rs->backwater);
	
		
		/*
			Load data buffers.
		*/
		ratedat = rs->ratedat;
		uhgdur  = rs->uhgdur;
		
		
		/*
			Deallocate memory.
		*/
		FreeRiverstat(rs);
	}

	
	river_addTextFilterCallbacks();

	
	return;
}


void	river_load_group(const char *lid)
{
	
   	RpfFcstPoint	*gHead;
	char		where[MAX_WHERE_LEN];


	sprintf(where," WHERE lid = '%s' ",lid);	
	gHead = GetRpfFcstPoint(where);
	if (gHead)
	{
           XmTextSetString(rgroupTxt, gHead->group_id);
	   FreeRpfFcstPoint(gHead);
	
	}
	else
	{
	   XmTextSetString(rgroupTxt, rgroup_none);
	}
}



void	apply_river(Widget w, XtPointer ptr, XtPointer cbs)
{
	(void) save_river();
	return;
}


int	save_river(void)
{
	Riverstat	riverstat;
	char		where[MAX_WHERE_LEN],
	   		msg[MAX_BUF_LEN],
			*pe = NULL ,
			*buf = NULL ;
	long		date;
	int		rcdCount,
	   		error,
                        status;
	
	
	/*
		Associate the location identifier for the
		specified station.
	*/
	memset(&riverstat, '\0', sizeof(riverstat));
	(void) strcpy(riverstat.lid, rv_lid);
	
	
	/*
		Associate XmText widget values with
		the appropriate field in the 
		Riverstat struct.
	*/	
	if ( ( buf = XmTextGetString(rstrmTxt) ) != NULL )
	{
	   if (IsNull(CHAR, buf) == NOTNULL)
	      strcpy(riverstat.stream, buf);		
	   XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(rlatTxt) ) != NULL )
	{
	   if (hb_check_lat_bounds(buf))
	   {
	      riverstat.lat = cvt_spaced_format(buf, 0);
	   }
	   else
	   {
	      ErrorDialog(riverDS, "Please enter a VALID (-90 to 90) Latitude.");
	      XtFree(buf);
	      return(False);
	   }
	   
	   XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(rlonTxt) ) != NULL )
	{
	   if (hb_check_lon_bounds(buf))
	   {
	      riverstat.lon = cvt_spaced_format(buf, 0);
	   }
	   else
	   {
	      ErrorDialog(riverDS, "Please enter a VALID (-180 to 180) Longitude.");
	      XtFree(buf);
	      return(False);
	   }
	   
	   XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(rrmkTxt) ) != NULL )
	{
	   if (IsNull(CHAR, buf) == NOTNULL)
	      strcpy(riverstat.remark, buf);
	   XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(rdaTxt) ) != NULL )
	{
	   if (!IsBlankStr(buf))
	      riverstat.da = atof(buf);
	   else
	      (void) SetNull(DOUBLE, (void *) &riverstat.da);
	   XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(rmileTxt) ) != NULL )
	{
	   if (!IsBlankStr(buf))
	      riverstat.mile = atof(buf);
	   else
	      (void) SetNull(DOUBLE, (void *) &riverstat.mile);
	   XtFree(buf);
	}
	
	
	/* Get flood stage from text widget */
	if ( ( buf = XmTextGetString(rfloodsTxt) ) != NULL )
	{
	   if (!IsBlankStr(buf))
	      riverstat.fs = atof(buf);
	   else
	      (void) SetNull(DOUBLE , (void *) &riverstat.fs);
	   XtFree(buf);
	}
	
	/* Get flood flow  from text widget */
	if ( ( buf = XmTextGetString(rfflowTxt) ) != NULL )
	{
	   if (!IsBlankStr(buf))
	      riverstat.fq = atof(buf);
	   else
	      (void) SetNull(DOUBLE, (void *) &riverstat.fq);
	   XtFree(buf);
	}
	
	/* Get action stage from text widget */
	if ( ( buf = XmTextGetString(ractionsTxt) ) != NULL )
	{
	   if (!IsBlankStr(buf))
	      riverstat.wstg = atof(buf);
	   else
	      (void) SetNull(DOUBLE, (void *) &riverstat.wstg);
	   XtFree(buf);
	}

	/* Get action flow from text widget */
	if ( ( buf = XmTextGetString(ractionfTxt) ) != NULL )
	{
	   if (!IsBlankStr(buf))
	      riverstat.action_flow = atof(buf);
	   else
	      (void) SetNull(DOUBLE , (void *) &riverstat.action_flow);
	   XtFree(buf);
	}
	

	pe = grpinfo_getpe_from_peLI(riverinfo_peLI);

        if ( pe != NULL )
        {
	   strcpy(riverstat.primary_pe,pe);
        }

	if ( rlatestfcst )
		strcpy(riverstat.use_latest_fcst,"T");
	else
		strcpy(riverstat.use_latest_fcst,"F");


	rlatestfcst  = 0;
   buf = XmTextGetString(rrvsTxt);
	if ( (buf != NULL) && (*buf != '\0') )
	{
		if (four_digit_year(buf) != 0)
		{
		  sprintf(msg, "Invalid Revise Date '%s' entered, 4 digit year required.\n", 
			  buf);
		  ErrorDialog(riverDS, msg);
		  /* leave function, do not save data */
		  return(False);
		}
		else
		{
                  status = USA_date_to_date_t ( buf, & date );

		  if ( status != 0 )
		  {
		    sprintf(msg, "Invalid Revise Date '%s' entered, check month\n"
                                 "and day.\n", buf);
		    ErrorDialog(riverDS, msg);

		    /* leave function, do not save data */
		    return(False);
		  }
		  else
		    riverstat.rrevise = date;
		}
			
	        XtFree(buf);
	}

	/* Get zero datum from text widget */
	if ( ( buf = XmTextGetString(rzdTxt) ) != NULL )
	{
	   if (!IsBlankStr(buf))
	      riverstat.zd = atof(buf);
	   else
	      (void) SetNull(DOUBLE, (void *) &riverstat.zd);
	   XtFree(buf);
	}
	
	/* Get threshold runoff from text widget */
	if ( ( buf = XmTextGetString(rtrTxt) ) != NULL )
	{
	   if (!IsBlankStr(buf))
	      riverstat.threshold_runoff = atof(buf);
	   else
	      (void) SetNull(DOUBLE, (void *) &riverstat.threshold_runoff);
	   XtFree(buf);
	}
	
	if ( ( buf = XmTextGetString(rsvdatumTxt) ) != NULL )
	{
	   if (IsNull(CHAR, buf) == NOTNULL)
	      strcpy(riverstat.vdatum, buf);
	   XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(rssrcTxt) ) != NULL )
	{
	   if (IsNull(CHAR, buf) == NOTNULL)
	      strcpy(riverstat.rsource, buf);
	   XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(rsgsnoTxt) ) != NULL )
	{
	   if (IsNull(CHAR, buf) == NOTNULL)
	      strcpy(riverstat.gsno, buf);
	   XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(rsratenumTxt) ) != NULL )
	{
	   if (IsNull(CHAR, buf) == NOTNULL)
	      strcpy(riverstat.usgs_ratenum, buf);
	   XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(rsporTxt) ) != NULL )
	{
	   if (IsNull(CHAR, buf) == NOTNULL)
	      strcpy(riverstat.por, buf);
	   XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(rslevelTxt) ) != NULL )
	{
	   if (IsNull(CHAR, buf) == NOTNULL)
	      strcpy(riverstat.level, buf);
	   XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(rsratedTxt) ) != NULL )
	{
	   if (IsNull(CHAR, buf) == NOTNULL)
	      strcpy(riverstat.rated, buf);
	   XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(rspoolTxt) ) != NULL )
	{
	   if (!IsBlankStr(buf))
	      riverstat.pool = atof(buf);
	   else
	      (void) SetNull(DOUBLE, (void *) &riverstat.pool);
	   XtFree(buf);
	}

	
	if ( ( buf = XmTextGetString(rscbTxt) ) != NULL )
	{
	   if (!IsBlankStr(buf))
	      riverstat.cb = atof(buf);
	   else
	      (void) SetNull(DOUBLE, (void *) &riverstat.cb);
	   XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(rsbfTxt) ) != NULL )
	{
	   if (!IsBlankStr(buf))
	      riverstat.bf = atof(buf);
	   else
	      (void) SetNull(DOUBLE, (void *) &riverstat.bf);
	   XtFree(buf);
	}
	
	
	buf = XmTextGetString(rsrdateTxt);
  	if ( (buf != NULL) && (*buf != '\0') )
	{
		if (four_digit_year(buf) != 0)
		{
		  sprintf(msg, "Invalid Date of Rating '%s' entered, 4 digit year required.\n", 
			  buf);
		  ErrorDialog(riverDS, msg);
		  /* leave function, do not save data */
		  return(False);
		}
		else
		{
                  status = USA_date_to_date_t ( buf, &date );

		  if ( status != 0 )
		  {
		    sprintf(msg, "Invalid Date of Rating '%s' entered, check month and\n"
                                 "day.\n", buf);
		    ErrorDialog(riverDS, msg);
		    /* leave function, do not save data */
		    return(False);
		  }
		  else
		    riverstat.ratedat = date;
		}
			
		XtFree(buf);
	}
	
	
	/*
		Get XmOptionMenu labels.
	*/
	buf = GetLabel(GetMenuHistory(rtidalOM));
	(void) strcpy(riverstat.tide, buf);
	buf = GetLabel(GetMenuHistory(rbckwtrOM));
	(void) strcpy(riverstat.backwater, buf);
	

	/*
		Perform Update/Insert operation, as
		appropriate.
	*/
	sprintf(where, " WHERE lid = '%s' ", rv_lid);	
	if ( ( rcdCount = recordCount("riverstat", where) ) != 0 )
	{
	   if ((error = UpdateRiverstat(&riverstat, where)) != 0)
	   {
	      sprintf(msg, "Unable to update record: %d\n", error); 
	      ErrorDialog(riverDS, msg);
	   }
	}
	else
	{
	   if ((error = PutRiverstat(&riverstat)) != 0)
	   {
	      sprintf(msg, "Unable to insert record: %d\n", error);
	      ErrorDialog(riverDS, msg);
	   }
	   
	   set_window_title(riverDS, "River Gage", rv_lid);
	   set_stat_pos(rv_lid);
	}

	
	/*
		Set the window sensitivity to allow subsequent
		delete operations.
	*/
	if (error == 0)
	{
	   river_save_group(rv_lid);
	   
	   riverSetSensitivity(True);
	   river_load(rv_lid);
	   Loc_UpdateStationClassFields(rv_lid);
	}
	
	
	return(True);
}


void	river_save_group(const char *lid)
{
   	RpfFcstPoint 	*gHead,
			*gPtr;
	
	char		where[MAX_WHERE_LEN],
			buf[BUFSIZ],
			*str;

	
	sprintf(where," WHERE lid = '%s' ", lid);
	if ( (gHead = GetRpfFcstPoint(where)) != NULL)
		gPtr = (RpfFcstPoint *) ListFirst(&gHead->list);
	else
		gPtr = NULL;
		
		
	str = XmTextGetString(rgroupTxt);
	if ((strlen(str) > 0) &&
	    (strcmp(str, rgroup_none) != 0))  /* Forecast Group selected !! */
	{
	   strcpy(buf, str);
	   if (gPtr)  /* record is in database already */
	   {
	      strcpy(gPtr->group_id, buf);
	      UpdateRpfFcstPoint(gPtr, where);
	      FreeRpfFcstPoint(gHead);
	   }
	   else  /* record is NOT in database */
	   {
	      gPtr = (RpfFcstPoint *) malloc(sizeof(RpfFcstPoint));
	      if (gPtr)
	      {
		 strcpy(gPtr->lid, lid);
		 strcpy(gPtr->group_id, buf);
		 strcpy(gPtr->rec_type, "PE");
		 strcpy(gPtr->primary_back, "XXX");
		 strcpy(gPtr->secondary_back, "XXX");
		 gPtr->chg_threshold = 0.5;
		 gPtr->ordinal = 1;
		 gPtr->backhrs = 24;
		 gPtr->forwardhrs = 240;
		 gPtr->adjustendhrs = 6;
		 PutRpfFcstPoint(gPtr);
		 free(gPtr);
	      }
	   }
	   XtFree(str);
	}
	else /* NO Forecast Group selected (remove group_id from database) */
	{
           if ( gPtr != NULL )
           {
	      sprintf(where," WHERE lid = '%s' and group_id = '%s' ",
		      lid, gPtr->group_id);
           }
           else
           {
	      sprintf(where," WHERE lid = '%s' and group_id = '%s' ",
		      lid, "" ) ;
           }

	   DeleteRpfFcstPoint(where);
	}
	
	return;
}



ShefPe*		grpinfo_getshefpes(Boolean reset_statics)
{
   static ShefPe	*shefpe = NULL,
      			*shefpePtr = NULL;

   if (reset_statics)
   {
      shefpe = NULL;
      shefpePtr = NULL;
   }
   else
   {      
      /*
      		Retrieve record(s) from the dbms (first time ONLY).
      		Always return the shefpePtr.
      */
      if (shefpe == NULL)
      {
	 shefpe = GetShefPe(" WHERE pe LIKE 'H%' OR pe LIKE 'Q%' ORDER BY pe ");
	 if (shefpe != (ShefPe *) NULL)
	 {
	    shefpePtr = (ShefPe *) ListFirst(&shefpe->list);
	 }
      }
   }
   
   return(shefpePtr);
}


void	grpinfo_freeshefpes(void)
{
   ShefPe 	*shefpePtr = grpinfo_getshefpes(False);
   
   
   if (shefpePtr != (ShefPe *) NULL)
   {
      FreeShefPe(shefpePtr);
      shefpePtr = NULL;
   }
   
   return;
}



/*
	This function gets the 1st pe from a given peLI.
*/
char*	grpinfo_getpe_from_peLI(Widget peLI)
{
   ShefPe	*shefpePtr = grpinfo_getshefpes(False);
   
   int		*poslist,
      		cnt,
		pe_nth;

   
   XmListGetSelectedPos(peLI, &poslist, &cnt);
   
   if (cnt > 0)
   {
      pe_nth = poslist[0];
      shefpePtr = (ShefPe *) ListNth(&shefpePtr->list, pe_nth);
      
      if (poslist != (int *) NULL)
      {
	 free(poslist);
	 poslist = NULL;
      }
      
      XtFree((char *)poslist);
      return(shefpePtr->pe);
   }
      
   return(NULL);
}



/*
	Loads a given peLI with the specified list of shefpes.
*/
void	grpinfo_loadpeLI(Widget peLI, ShefPe *shefpePtr)
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

/*
	This function hightlight the Primary_pe from the PeList
*/

void grpinfo_highlightpeLI(Widget peLI, char *rv_lid)
{
   int		i,
		cnt,
		pos = 0;

   ShefPe 	*shefpePtr;

   Riverstat 	*rs;

   char		where[80],
		primary_pe[SHEF_PE_LEN+1];


   sprintf(where, " WHERE lid = '%s' ", rv_lid);
   if ((rs = GetRiverstat(where)) != NULL)
   {
		memset(primary_pe,'\0',strlen(primary_pe));
		strcat(primary_pe,rs->primary_pe);
	        FreeRiverstat(rs);
   }

   shefpePtr = grpinfo_getshefpes(False);
   if (shefpePtr != (ShefPe *) NULL)
   {
      cnt = ListCount(&shefpePtr->list);
      if (cnt > 0)
      {
	 i = 0;
	 while (shefpePtr != (ShefPe *) NULL)
	 {
	    i++;
	    if(strncmp(shefpePtr->pe,primary_pe,2) == 0)
	    {
	    	pos=i;
		break;
	    }
	    shefpePtr = (ShefPe *) ListNext(&shefpePtr->node);
	 }
      } 
   }

   if(pos ) 
   {
  	XmListSelectPos(peLI, pos, True);
  	XmListSetPos(peLI, pos);
   }

}

/*
	This function returns the correct PB-name given a Physical Element.
*/
char*	grpinfo_get_shefpe_name(char *pe)
{
   ShefPe	*shefpe,
      		*shefpePtr;
   
   char		where[MAX_WHERE_LEN] ;
   static char	buf[MAX_BUF_LEN];
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE pe = '%s' ", pe);
   shefpe = GetShefPe(where);
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
	This function sets a peLI to a specified pe.
*/
void	grpinfo_loadpe_into_peLI(Widget peLI, char *pe)
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
   strcat(buf, grpinfo_get_shefpe_name(pe));
   while (strlen(buf) < 23)
      strcat(buf, " ");
   
   item = XmStringCreateSimple(buf);
   
   XmListSetItem(peLI, item);
   XmListSelectItem(peLI, item, True);
   
   XmStringFree(item);

   
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}





