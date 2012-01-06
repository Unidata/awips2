/*
	File:		callbacks.c
	Date:		7/21/94
	Author:		Dale Shelton, Paul Taylor
	
	Purpose:	Sets up and implements the callback 
			functions for the hydrobase main
			window widgets.  Subsidiary windows
			implement callbacks in their own
			source files.
			
        Modified:       Jingtao Deng
        Notes:          Modify function show_serv_bkup, get ride of 
			primary and secondary offices.			
        
        Modified:       Gautam Sood
        Revision Date:  4/03/2003 
        Notes:          Combined Service backup filter, lat-lon filter, and
                        show shef post/no post into one filter window

        Modified:       Bryon Lawrence
        Revision Date:  January/February 2004
        Notes:          Modified service backup filtering to be based on
                        the hsa column in the location table, not the wfo
                        column.  The service backup filter affects which
                        stations are displayed in the Hydrobase station
                        listing.

                        Also, removed the callbacks which displayed the
                        damcat catalog.

        Modified:       Bryon Lawrence
        Revision Date:  January 24, 2005
        Notes:          Modified to use ParseUnique to parse output from 
                        LoadUnique when it is used to retrieve multiple
                        unique fields using the '||' concatenation operator.


        Modified:       Gautam Sood	
        Revision Date:  January 27, 2005
        Notes:          Modified to use the Java Unithydrograph editor.
	
	Modified:       Jingtao Deng
	Revision Date:  Jan. 18, 2006
	Notes:          Change hbrvr_gagePB to hbloc_gagePB. Move Gage History
	                from "River Gage" menu to "Location" menu.
			
	Modified:       Jingtao
	Revision Date:  03/16/2006
	Notes:          Add "River Station Location Info" in the "Setup" menu
	                to show information from HgStation table.			
           		

*/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>

#include "DbmsAccess.h"   /* utilities */
#include "Xtools.h"
#include "Filter.h"
#include "GeneralUtil.h"

#include "bench_cbs.h"    /* hydrobase defs */
#include "callbacks.h"
#include "cities_show.h"
#include "datum_cbs.h"
#include "fcst_show.h"
#include "geoarea_show.h"
#include "geoline_show.h"
#include "TechInfo.h"
#include "hybase.h"
#include "hybase_utils.h"
#include "loc_cbs.h"
#include "mgage_cbs.h"
#include "pubds_cbs.h"
#include "purgedata_show.h"
#include "rangeCheck_show.h"
#include "refer_cbs.h"
#include "res_cbs.h"
#include "rpfparams_show.h"
#include "contact_show.h"
#include "crest_show.h"
#include "radarloc_show.h"
#include "river_cbs.h"
#include "fcst_cbs.h"
#include "fldcat_show.h"
#include "hbAS.h"
#include "admin_cbs.h"
#include "lwds_cbs.h"
#include "prefer_cbs.h"
#include "rate_cbs.h"
#include "uhg_cbs.h"
#include "rateds.h"
#include "grpinfo_cbs.h"
#include "flood_cbs.h"
#include "fstmt_cbs.h"
#include "setup_cbs.h"
#include "e19_show.h"
#include "ingestmgr.h"
#include "ingestmgr_show.h"
#include "stcozo.h"
#include "stcozo_show.h"
#include "textrept_show.h"
#include "nwrtower.h"
#include "nwrtower_show.h"
#include "river_show.h"
#include "filteroptions.h"
#include "stachar_show.h"
#include "ugc_show.h"
#include "adjustfactor_show.h"
#include "cvt_latlon.h"
#include "user_prefs.h"
#include "lwstmt_show.h"
#include "floodrept_show.h"
#include "hgstation_show.h"

#include "LoadUnique.h"    /* database utilities */

#include "LocView.h"       /* database defs */
#include "shefdecoder_show.h"

/* global variable indicating currently selected station */

char	cur_lid[ LOC_ID_LEN + 1 ];

/* variables global to this file, used for the station list filter */

UniqueList	*ulhsaHead = NULL;

char	hsa_filter [ 200 ] = "";

double  glat = 0.0;
double  glon = 0.0;
double  glat_offset = 1.0;
double  glon_offset = 1.0;
int     glatlon_enable = 0;

int     gshow_post = 1;
int     gshow_nopost = 0; 

/***********************************************************************************/

void	AddCallbacks(void)
{
	Atom	wmAtom;
	
	/*
		Add window manager callbacks.
	*/
	wmAtom = XmInternAtom(XtDisplay(hbAS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(hbAS, wmAtom, close_hb, NULL);
	
	
	/*
		Add widget callbacks.
	*/
	XtAddCallback(hbmainLI, XmNdefaultActionCallback,   show_loc,   NULL);
	XtAddCallback(hbmainLI, XmNbrowseSelectionCallback, list_state, NULL);
	
	
	XtAddCallback(hbfile_prefPB, XmNactivateCallback, show_prefs, NULL);
	XtAddCallback(hbfile_exitPB, XmNactivateCallback, close_hb, NULL);
	
	
	XtAddCallback(hbloc_addPB,     XmNactivateCallback, show_loc,     NULL);
	XtAddCallback(hbloc_modPB,     XmNactivateCallback, show_loc,     NULL);
	XtAddCallback(hbloc_dsrcsPB,   XmNactivateCallback, show_schar,   NULL);
	XtAddCallback(hbloc_czugcPB,   XmNactivateCallback, show_czone,   NULL);
	XtAddCallback(hbloc_contactPB, XmNactivateCallback, show_contact, NULL);
	XtAddCallback(hbloc_gagePB,    XmNactivateCallback, show_gage, NULL);
	
	XtAddCallback(hbrvr_riverPB,  XmNactivateCallback, show_river, NULL);
	XtAddCallback(hbrvr_fcatPB,   XmNactivateCallback, show_fcat, NULL);
	XtAddCallback(hbrvr_fstmtPB,  XmNactivateCallback, show_fstmt, NULL);
	XtAddCallback(hbrvr_lwstmtPB, XmNactivateCallback, show_lwstmt, NULL);
	XtAddCallback(hbrvr_floodPB,  XmNactivateCallback, show_fdam, NULL);
	XtAddCallback(hbrvr_ratingPB, XmNactivateCallback, show_rate, NULL);
	XtAddCallback(hbrvr_uhgPB,    XmNactivateCallback, show_uhg, NULL);
	XtAddCallback(hbrvr_crestPB,  XmNactivateCallback, show_crest, NULL);
	XtAddCallback(hbrvr_lwPB,     XmNactivateCallback, show_lowwater, NULL);
	XtAddCallback(hbrvr_bmPB,     XmNactivateCallback, show_benchmark, NULL);
	XtAddCallback(hbrvr_datumPB,  XmNactivateCallback, show_datum, NULL);
	XtAddCallback(hbrvr_descrPB,  XmNactivateCallback, show_descr, NULL);
	XtAddCallback(hbrvr_pubPB,    XmNactivateCallback, show_pub, NULL);
	XtAddCallback(hbrvr_refPB,    XmNactivateCallback, show_refer, NULL);
	
	XtAddCallback(hbresvr_resvrPB,   XmNactivateCallback, show_res, NULL);
	
	XtAddCallback(hbingest_ingestPB, XmNactivateCallback, show_filter, NULL);
	XtAddCallback(hbingest_adjustPB, XmNactivateCallback, show_adjust, NULL);
	XtAddCallback(hbingest_rcheckPB, XmNactivateCallback, show_rcheck, NULL);
	XtAddCallback(hbingest_purgePB,  XmNactivateCallback, show_purge,  NULL);
	XtAddCallback(hbingest_shefPB , XmNactivateCallback , 
                      show_shef , NULL ) ;


	
	XtAddCallback(hbreports_floodPB, XmNactivateCallback, show_fsrept, NULL);
	XtAddCallback(hbreports_textPB,  XmNactivateCallback, show_textrept, NULL);
	
	
	XtAddCallback(hbsetup_adminPB,    XmNactivateCallback, show_admin,   NULL);
	XtAddCallback(hbsetup_citiesPB,   XmNactivateCallback, show_cities,  NULL);
	XtAddCallback(hbsetup_rfPB,       XmNactivateCallback, show_rfields, NULL);
	XtAddCallback(hbsetup_stcozoPB,   XmNactivateCallback, show_stcozo,  NULL);
	XtAddCallback(hbsetup_rpfparamsPB,XmNactivateCallback, show_rpfparams, NULL);
	XtAddCallback(hbsetup_fgPB,       XmNactivateCallback, show_fgrp,    NULL);
	XtAddCallback(hbsetup_rlocPB,     XmNactivateCallback, show_radarloc,NULL);
	XtAddCallback(hbsetup_adefPB,     XmNactivateCallback, show_geoarea, NULL);
	XtAddCallback(hbsetup_vdefPB,     XmNactivateCallback, show_geoline, NULL);
	XtAddCallback(nwrtowerPB,         XmNactivateCallback, show_nwrtower, NULL);
	XtAddCallback(tscfgPB,            XmNactivateCallback, show_tscfg, NULL);
	XtAddCallback(hgstationPB,        XmNactivateCallback, show_hgs, NULL);
	
	XtAddCallback(hbhelp_aboutPB, XmNactivateCallback, show_techinfo, NULL);
	
	
	/*
		Sort XmOptionMenu children callbacks.
	*/
	XtAddCallback(hbsort_stationPB, XmNactivateCallback, set_stat_list, NULL);
	XtAddCallback(hbsort_namePB,    XmNactivateCallback, set_stat_list, NULL);
	XtAddCallback(hbsort_countyPB,  XmNactivateCallback, set_stat_list, NULL);
	
	
	/*
		Text field callbacks.
	*/
	XtAddCallback(hblidTE, XmNmodifyVerifyCallback, lookup_lid, NULL);
	
	/*
		Service Backup filter push button callbacks.
	*/
	XtAddCallback(hbbackupPB, XmNactivateCallback, show_filter_options, NULL);

	return;
}


void	show_prefs(Widget w, XtPointer ptr, XtPointer cbs)
{
	ShowPrefDs(w);
	return;
}


void	close_hb(Widget w, XtPointer ptr, XtPointer cbs)
{
   	CloseDbms();
	exit(0);
}


void	show_loc(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 	
	if (w == hbloc_addPB)
		ShowLocDs(w, (char *) NULL);
	else
		ShowLocDs(w, cur_lid);
	return;
}


void	show_schar(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	ShowStaCharDs(GetTopShell(w), cur_lid, True);
	
        return;
}


void	show_czone(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	ShowUgcDs(w, cur_lid);
	return;
}


void	show_contact(Widget w, XtPointer ptr, XtPointer cbs)
{
	strcpy(cur_lid, CurrentLid());
	contact_show(w, cur_lid, True);
	return;
}


void	show_river(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	ShowRiverDs(w, cur_lid);
	return;
}


void	show_fcat(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	ShowFdatDs(w, cur_lid);
	return;
}


void	show_fstmt(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	ShowFstmtDs(w, cur_lid, True);
	return;
}

void	show_lwstmt(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	ShowLwstmtDs(w, cur_lid, True);
	return;
}

void	show_fdam(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	flood_show(w, cur_lid);
	return;
}


void	show_rate(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	ShowRateDs(w, cur_lid, True);
	return;
}


void	show_uhg(Widget w, XtPointer ptr, XtPointer cbs)
{
//	ShowUhgDs(w, cur_lid);

        char command[BUFSIZ];
        char       gad_value[128];
        int        gad_token_len=0, gad_value_len=0;
        int rv = 1;

        strcpy(cur_lid, CurrentLid());
        gad_token_len = strlen("whfs_bin_dir");
        get_apps_defaults("whfs_bin_dir", &gad_token_len, gad_value, &gad_value_len);
        if (gad_value_len > 0)
        {
            sprintf(command, "%s/run_UnitHydrographEditor %s &",gad_value, cur_lid);
            rv = system(command);
        }
        else
        {
            fprintf(stderr,"whfs_bin_dir env variable not available\n");
        }

        return;
}


void	show_crest(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	crest_show(w, cur_lid, True);
	return;
}


void	show_lowwater(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	lwds_show(w, cur_lid);
	return;
}


void	show_benchmark(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	bench_show(w, cur_lid);
	return;
}


void	show_datum(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	datum_show(w, cur_lid);
	return;
}


void	show_descr(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	fcst_show(w, cur_lid);
	return;
}


void	show_gage(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	ShowMgageDs(w, cur_lid);
	return;
}


void	show_pub(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	pubds_show(w, cur_lid);
	return;
}


void	show_refer(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	refer_show(w, cur_lid);
	return;
}


void	show_res(Widget w, XtPointer ptr, XtPointer cbs)
{
   	strcpy(cur_lid, CurrentLid()); 
	ShowResDs(w, cur_lid);
	return;
}


void	show_damcat(Widget w, XtPointer ptr, XtPointer cbs)
{
     
     char command[BUFSIZ];
     char	gad_value[128];
     int	gad_token_len=0, gad_value_len=0;
     int rv = 1;
     
	  
     SetCursor(hbFO, XC_watch);
     
     gad_token_len = strlen("whfs_bin_dir");
     get_apps_defaults("whfs_bin_dir", &gad_token_len, gad_value, &gad_value_len);
     if (gad_value_len > 0)
     {
	  sprintf(command, "%s/run_damcat_hb &",gad_value);
	  rv = system(command);
     }
     else
     {
	  fprintf(stderr,"whfs_bin_dir env variable not available\n");   
     }
     
     
     UnsetCursor(hbFO);
     
     
     
     return;
}


void	show_filter(Widget w, XtPointer ptr, XtPointer cbs)
{
	ShowIngestDs(w);
	return;
}



void	show_adjust(Widget w, XtPointer ptr, XtPointer cbs)
{
	ShowAdjustDs(w);
	return;
}


void	show_rcheck(Widget w, XtPointer ptr, XtPointer cbs)
{
   	ShowRangeCheckDs(w);
	return;
}


void	show_purge(Widget w, XtPointer ptr, XtPointer cbs)
{
   	purgedata_show(w);
	return;
}


void	show_fsrept(Widget w, XtPointer ptr, XtPointer cbs)
{
 	floodrept_show(w);
	return;
}


void	show_textrept(Widget w, XtPointer ptr, XtPointer cbs)
{
	strcpy(cur_lid, CurrentLid());
	ShowTextReportsDs(w, cur_lid);
	return;
}


void	show_admin(Widget w, XtPointer ptr, XtPointer cbs)
{
	ShowAdmDs(w);
	return;
}


void	show_cities(Widget w, XtPointer ptr, XtPointer cbs)
{
	ShowCitiesDs(w);
	return;
}


void	show_rfields(Widget w, XtPointer ptr, XtPointer cbs)
{
	setup_show(w);
	return;
}


void	show_stcozo(Widget w, XtPointer ptr, XtPointer cbs)
{
	ShowStCoZoDS(w);
	return;
}


void	show_rpfparams(Widget w, XtPointer ptr, XtPointer cbs)
{
	rpfparams_show(w);
	return;
}


void	show_fgrp(Widget w, XtPointer ptr, XtPointer cbs)
{
	grpinfo_show(w);
	return;
}


void	show_radarloc(Widget w, XtPointer ptr, XtPointer cbs)
{
	radarloc_show(w);
	return;
}


void	show_geoarea(Widget w, XtPointer ptr, XtPointer cbs)
{
	geoarea_show(w);
	return;
}


void	show_geoline(Widget w, XtPointer ptr, XtPointer cbs)
{
	geoline_show(w);
	return;
}

void	show_hgs(Widget w, XtPointer ptr, XtPointer cbs)
{
	hgs_show(w);
	return;
}

void	show_nwrtower(Widget w, XtPointer ptr, XtPointer cbs)
{
	nwrtower_show(w);
	return;
}

void	show_filter_options(Widget w, XtPointer ptr, XtPointer cbs)
{

     int		count=0, ctr=0, ac=0;
     XmStringTable	xmStr;
     Arg		arg[10];
     UniqueList		*ulPtr=NULL;
	
     char lat_str[20] = {'\0'}; 
     char lon_str[20] = {'\0'};
     char offset_str[12]; 
     char ** values = NULL;

     int cnt;
     int gad_token_len = 0; 
     int gad_value_len = 0;
     
     
     /* only create the window the first time since it is
        unmanaged, not destroyed, when it is closed */

     if (! hbsbfilterDS)
     {
	create_hbsbfilterDS(GetTopShell(w));
	  
	/*  Set up the callbacks  */

	XtAddCallback(hbsbfapplyPB,  XmNactivateCallback, filter_option_save,   NULL);
	XtAddCallback(hbsbfcancelPB, XmNactivateCallback, filter_option_cancel, NULL);


        /* load the service backup filter info */
	
	if (ulhsaHead != NULL)
	{
	      FreeUnique (ulhsaHead);
	      ulhsaHead=NULL;
	}
	

	ulhsaHead = (UniqueList *) LoadUnique("hsa", "LocView", " ", &count);
	
	if (ulhsaHead != NULL)
	{
		ulPtr = (UniqueList *) ListFirst(&ulhsaHead->list);
		count = ListCount(&ulhsaHead->list);
		xmStr = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		for (ctr=0; ctr < count; ctr++)
		{
                        values = ParseUnique ( ulPtr, & cnt );
			xmStr[ctr] = XmStringCreateSimple(values[0]);
                        FreeParseUnique ( values ); 
			ulPtr = (UniqueList *) ListNext(&ulPtr->node);
		}

		/* now, we load the list in the scrolled lists */
		ac=0;
		XtSetArg(arg[ac], XmNitemCount, count);ac++;
		XtSetArg(arg[ac], XmNitems, xmStr); ac++;
		XtSetValues(hbsbfhsaLI, arg, ac);

		/* need to free the memory */
		for (ctr=0; ctr < count; ctr++)
		    XmStringFree(xmStr[ctr]);
		XtFree((char *)xmStr);

	} /* ulhsaHead exists */
	
	/*DeSensitize(hbsbfhsaLI);*/
	
        /* load the lat-lon filter info.  this code is only accessed
           the first time in, when the fields are undefined, it will use the
	   token to set the initial value. */

        gad_token_len = strlen("hv_center_lat");
        get_apps_defaults("hv_center_lat", &gad_token_len, lat_str, &gad_value_len);
        glat = atof(lat_str);
     
        strcpy(lat_str, cvt_latlon_from_double(glat));
        XmTextSetString(latfilterTX, lat_str);
     

        gad_token_len = strlen("hv_center_lon");
        get_apps_defaults("hv_center_lon", &gad_token_len, lon_str, &gad_value_len);
        glon = abs(atof(lon_str));
      
        strcpy(lon_str, cvt_latlon_from_double(glon));
        XmTextSetString(lonfilterTX, lon_str);
     
        /* set the lat, lon offsets */
     
        sprintf(offset_str, "%.2f", glat_offset);
        XmTextSetString(offsetlatTX, offset_str);
     
        sprintf(offset_str, "%.2f", glon_offset);
        XmTextSetString(offsetlonTX, offset_str);
   
        /* Add text filter callbacks for the lat/lon and offset text boxes */

        XtAddCallback(latfilterTX, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter,
                                (XtPointer)(INTEGERS+SPACES));
        XtAddCallback(lonfilterTX, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter,
                                (XtPointer)(INTEGERS+SPACES));
        XtAddCallback(offsetlatTX,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
        XtAddCallback(offsetlonTX,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);

    
        /* load the show shef post/nopost info */
	
	XmToggleButtonSetState(hbpostTB,True,True);
	

     } /* hbsbfilterDS exists */
     
     
     if (! XtIsManaged(hbsbfilterDS))
     {
	/*  Manage dialog shell and form  */
	XtManageChild(hbfiltermainFO);
	XtManageChild(hbsbfilterDS);
     }

     XRaiseWindow(XtDisplay(hbsbfilterDS), XtWindow(hbsbfilterDS));
     
     return;
}


void	filter_option_save(Widget w, XtPointer ptr, XtPointer cbs)
{
	int * poslist = NULL , count = 0 , ctr = 0 , rtn = 0 ;
	UniqueList * ulPtr=NULL;
	char	temp_dude[4]="", one_hsa[8]="", *buf = NULL ;

	memset(temp_dude,        '\0', sizeof(temp_dude));
	memset(one_hsa,          '\0', sizeof(one_hsa));
	memset(hsa_filter,  '\0', sizeof(hsa_filter));
	
	strcpy(hsa_filter, "");
        
	/* find out which WFOs where selected from the WFO scrolled list */
	
	XmListGetSelectedPos(hbsbfhsaLI, &poslist, &count);
	if (count > 0)
	{
	     for (ctr = 0; ctr<count; ctr++)
	     {
		     ulPtr = (UniqueList *) ListNth(&ulhsaHead->list, poslist[ctr]);
		     strncpy(temp_dude, ulPtr->uchar, 3);
		     if (ctr < (count - 1))
                     {
			     sprintf(one_hsa, "'%s', ", temp_dude);
                     }
		     else
                     {
			     sprintf(one_hsa, "'%s'", temp_dude);
                     }
                      
		     strcat( hsa_filter , one_hsa ) ;
	     }
	}
	
	
	/* get the lat-lon filter info */
        if ( XmToggleButtonGetState(latlonTB) )
        {
        	glatlon_enable = 1;
        	buf = XmTextGetString(latfilterTX);
	        if ( hb_check_lat_bounds(buf) )
       	 		glat = cvt_to_latlon(XmTextGetString(latfilterTX), 0);
        	else
        	{
               		ErrorDialog(hbsbfilterDS, "Please enter a VALID (-90 to 90) Latitude.");
                	XtFree(buf);
                        rtn = 1;
        	}
        
        	buf = XmTextGetString(lonfilterTX);
        	if ( hb_check_lon_bounds(buf) )
        		glon = cvt_to_latlon(XmTextGetString(lonfilterTX), 0);
        	else
        	{
			ErrorDialog(hbsbfilterDS, "Please enter a VALID (-180 to 180) Longitude.");
                	XtFree(buf);
                        rtn = 1;
        	}

        	glat_offset = atof(XmTextGetString(offsetlatTX));
        	glon_offset = atof(XmTextGetString(offsetlonTX));
                if (rtn)
                	return;
        }
        else
        {
                glatlon_enable = 0;
        }
	
	/* get the show shef post/nopost info */
			
	if (XmToggleButtonGetState(hbpostTB))
 	   gshow_post = 1;
	else
	   gshow_post = 0;
	
	if (XmToggleButtonGetState(hbnopostTB))
	   gshow_nopost = 1;
	else
	   gshow_nopost = 0;

	set_stat_list(NULL, NULL, NULL);

   	return;
}

void	filter_option_cancel(Widget w, XtPointer ptr, XtPointer cbs)
{
     XtUnmanageChild(hbsbfilterDS);
     return;
}

void	show_tscfg(Widget w, XtPointer ptr, XtPointer cbs)
{
	char	app_dir [ 128 ] , bin_dir [ 128 ] , editor_name [ 128 ] ;
	int	gad_token_len=0, gad_value_len=0;

	char	path_name[200];
	char	command[300];

	gad_token_len = strlen("whfs_config_dir");
	get_apps_defaults( "whfs_config_dir" , & gad_token_len , app_dir , 
                          & gad_value_len ) ;
	if (gad_value_len <= 0)
	{
		fprintf(stderr, "whfs_config_dir undefined.  Edit "
                                "not performed.\n");
		return;
	}

        /* Retrieve the path to the whfs editor script. */
	gad_token_len = strlen("whfs_local_bin_dir") ;
	get_apps_defaults( "whfs_local_bin_dir" , & gad_token_len , bin_dir ,
                           & gad_value_len ) ;
	if (gad_value_len <= 0)
	{
		fprintf(stderr, "whfs_bin_dir undefined.  Edit not "
                                "performed.\n");
		return;
	}

        /* Retrieve the name of the whfs editor script. */
	gad_token_len = strlen("whfs_editor") ;
	get_apps_defaults( "whfs_editor" , & gad_token_len , editor_name ,
                           & gad_value_len ) ;
	if (gad_value_len <= 0)
	{
		fprintf(stderr, "whfs_editor undefined.  Edit not "
                                "performed.\n");
		return;
	}

	sprintf(path_name, "%s/timeseries/group_definition.cfg", app_dir);

	sprintf( command , "%s/%s %s %s" ,
			   bin_dir , editor_name , "GroupDefinitionEditor" ,
                           path_name ) ;
	system(command);

	return;
}


void	show_techinfo(Widget w, XtPointer ptr, XtPointer cbs)
{
	techinfo_show(w, "HydroBase", "OB-81", "02/28/2007");
	return;
}

void    show_shef ( Widget w , XtPointer ptr , XtPointer cbs )
{
        ShowShefdecoderDS ( w ) ;
}



void	lookup_lid(Widget w, XtPointer ptr, XmTextVerifyCallbackStruct *cbs)
{    
	char		buf[9],
			*lid;    
	int		pos,
			i;
	
	/* Null string entered.*/
	
	if (cbs->text->length == 0)
	   return;
		
		
	for (i = 0; i < cbs->text->length; i++)
	{
		/*
			Verify input text is alphanumeric,
			and convert to uppercase, if
			necessary.
		*/

		if (! isAlphaNum(cbs->text->ptr[i]))
		{
			cbs->doit = False;
			break;
		}
		
		if (islower(cbs->text->ptr[i]))
			cbs->text->ptr[i] = toupper(cbs->text->ptr[i]);
	}


	/*
		Build search pattern.
	*/
	if ( ( lid = XmTextGetString(w) ) != NULL )
	{
		strcpy(buf, lid);
		strcat(buf, cbs->text->ptr);
		XtFree(lid);
	}


	/*
		Search through the list and find 
		the list position that matches
		the specified input string.
	*/
	if ((pos = search_stats(buf)) < 0)
		cbs->doit = False;
		

	/*
		Reset the currently selected position
		in the location list.
	*/
	XmListSelectPos(hbmainLI, pos, True);
	XmListSetPos(hbmainLI, pos);
	return;
}


void	set_stat_list(Widget w, XtPointer ptr, XtPointer cbs)
{
	char	where[MAX_BUF_LEN];
	char	buf[MAX_BUF_LEN];
	char	temp_str[200];
	int	pos;
	int	cnt = 0;
	
	/*
		Nullify where clause.
	*/
	memset(where, '\0', sizeof(where));
	
	/* add SHEF post to where clause based on toggle button values */
	
	if (gshow_post)
	{
		if (gshow_nopost)
			strcat(where, " WHERE post IS NOT NULL ");	
		else
			strcat(where, " WHERE post = 1 ");
	}
	else if (gshow_nopost)
		strcat(where, " WHERE post = 0 ");
	else
		strcat(where, " WHERE post IS NULL ");
			

	/* add Service Backup to where clause.  Service Backup operations
	   were altered to use the LocView hsa column instead of the 
	   wfo column on February 9, 2004. */
	strcat(where, " AND lid IN (SELECT lid FROM LocView WHERE ");

	if(strlen(hsa_filter) == 0)
        {
	   strcat(where, " hsa IS NOT NULL ");
        }
	else
	{
		sprintf(temp_str, " hsa IN (%s) ", hsa_filter);
		strcat(where, temp_str);
	}
	
	strcat(where, " )");
	
	/* specify the lat-lon filter if it is enabled */
	
	if (glatlon_enable)
	{
	        sprintf(temp_str, " AND ((lat > %f) AND (lat < %f)) AND ((lon > %f) AND (lon < %f)) ",glat-glat_offset, 
                        glat+glat_offset, glon-glon_offset, glon+glon_offset);
                strcat(where, temp_str);
	}
	
	/*
		Determine sort selection criteria.
	*/
	pos = GetMenuPos(hbsortOM);
	switch (pos)
	{
	   	case SORT_LID:  
		   strcat(where, " ORDER BY lid ");
		   break;
		
		case SORT_NAME:
		   strcat(where, " ORDER BY name ");
		   break;
		   
		case SORT_CNTY:
		   strcat(where, " ORDER BY state, county ASC, lid ");
		   break;
		   
		default:
		   strcat(where, " ORDER BY lid ");
		   break;
	}
      
        printf("The WHERE clause is %s\n", where);
        

	cnt = load_stats(where, 1);	
	sprintf(buf, "( %-i Stations )", cnt);
	SetLabel(loc_countLA, buf);
	
	return;
}


void	set_stat_pos(char *lid)
{
   LocView           *lvPtr = NULL,
                     *locview = NULL;

   XmString          xmStr;

   char              where[MAX_WHERE_LEN],
                     buf[MAX_BUF_LEN],
                     tmpbuf[MAX_BUF_LEN],
                     tmpstr[MAX_BUF_LEN],
                     lat[11], lon[11];

   int               i,
                     cnt, 
                     code,
                     *pos = NULL;
   
   /*
   	First ensure we have the newest XmList.
   */
   
   if (locview)
      FreeLocView(locview);
  
   code = get_field_preference();

   sprintf(where, " WHERE lid = '%s' ", lid);
   if ((locview = GetLocView((char *) where)) != NULL)
   {
      xmStr = (XmString) XtMalloc(sizeof(XmString *)); 
      
      lvPtr = (LocView *) ListFirst(&locview->list);
      if (lvPtr != NULL)
      {
         memset(&tmpstr, '\0', sizeof(tmpstr));
         strncpy(tmpstr, lvPtr->name, 25);
         sprintf(buf, "%-8s %-25s", lvPtr->lid, tmpstr);
      }
      
      if (code & FLD_COUNTY)
      { 
          sprintf(tmpbuf, " %2s, %-20s", lvPtr->state, lvPtr->county);
          strcat(buf,tmpbuf);
       }
       
       if (code & FLD_BASIN)
       {
          memset(&tmpstr, '\0', sizeof(tmpstr));
          strncpy(tmpstr, lvPtr->rb, 25);
          sprintf(tmpbuf, " %-25s", tmpstr);
          strcat(buf, tmpbuf);
       }
 
       if (code & FLD_STREAM)
       {
          memset(&tmpstr, '\0', sizeof(tmpstr));
          strncpy(tmpstr, lvPtr->stream, 25);
          sprintf(tmpbuf, " %-25s", tmpstr);
          strcat(buf, tmpbuf);
       } 
 
       if (code & FLD_LATLON)
       {
          memset(&lat, '\0', sizeof(lat));
          memset(&lon, '\0', sizeof(lon));
          strcpy(lat, cvt_latlon_from_double(lvPtr->lat));
          strcpy(lon, cvt_latlon_from_double(lvPtr->lon));
          sprintf(tmpbuf, " %9s  %9s", lat, lon);
          strcat(buf, tmpbuf);
       }

       xmStr = XmStringCreateSimple(buf);

       /*
             Get's the position of the currently selected item in the list and replaces it with the new information.   
       */
       XmListGetSelectedPos(hbmainLI, &pos, &cnt);    
       XmListReplacePositions(hbmainLI, pos, &xmStr, 1); 
   }

   if((i=search_stats(lid)) > 0)
   {
      XmListSelectPos(hbmainLI, i, True);
      XmListSetPos(hbmainLI, i);                  
   }
}


void	clear_search_window(void)
{
   XmTextSetString(hblidTE, "");
   
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
