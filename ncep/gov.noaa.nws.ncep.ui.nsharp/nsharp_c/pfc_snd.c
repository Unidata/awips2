#include "gui.h"
#include "sharp95.h"

extern char sta_id[5];

/* private functions */
void pfc_select_cb	 (Widget wdgt, XtPointer clnt, XtPointer call);
void pfc_snd_load_gemfile(Widget wdgt, XtPointer clnt, XtPointer call);
void pfc_time_select_cb  (Widget wdgt, XtPointer clnt, XtPointer call);
void mapprojpfc_cb	 (Widget wdgt, XtPointer clnt, XtPointer call);
void mapunzoompfc_cb	 (Widget wdgt, XtPointer clnt, XtPointer call);
void mapzoompfc_cb	 (Widget wdgt, XtPointer clnt, XtPointer call);


Widget pfc_snd_dialog = NULL;
Widget pfc_snd_timelist, pfc_snd_text, pfc_list;
char pfcsoundfile[200], pfcsoundtime[20];

extern mapstruct pfc_map;
extern Widget toplevel;
extern char time_list[500][20];


void
pfc_sounding_cb (Widget wdgt)
/************************************************************************
 * PFC_SOUNDING_CB                                                      *
 *                                                                      *
 * Display Observed Sounding Map and get user selection.                *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC        2/02    freed mapmenu.label                     *
 * T. Piper/SAIC        01/04   removed NAWIPS_TABLES                   *
 * T. Piper/SAIC        01/04   added NxmWarn_show                      *
 ***********************************************************************/
{
  static Widget pfc_form, pfc_form2, pfc_form3, pfc_pane,
    pfc_snd_cancel, pfc_snd_help,
    pfc_lbl_time, pfc_lbl_station;
  static Widget menubar, maparea_zoom, maparea_unzoom, button, obssel_opt,
    obs_00_12, pfc_selW;
  XmString str, title_str;
  char gemdevice[72];

  int ii, iret = 0, ier, nmap;
  Arg args[10];
  Cardinal argcnt;

  static _NXMmenuItem mapmenu[30], *pfcmenu;

  static char map_winname[] = "mappfc";

  if (!pfc_snd_dialog)
    {
      title_str =
	XmStringCreateLocalized ("Point Forecast Sounding Selection");
      pfc_snd_dialog =
	XmCreateBulletinBoardDialog (toplevel, "pfc_panel", NULL, 0);
      XtVaSetValues (pfc_snd_dialog, XmNdialogTitle, title_str, NULL);
      XmStringFree (title_str);

      pfc_pane = XtVaCreateManagedWidget ("pfc_snd_pane",
					  xmPanedWindowWidgetClass,
					  pfc_snd_dialog,
					  XmNsashWidth, 1,
					  XmNsashHeight, 1, NULL);

      pfc_form2 = XtVaCreateWidget ("form", xmFormWidgetClass,
				    pfc_pane, NULL);

      pfc_form3 = XtVaCreateWidget ("form", xmFormWidgetClass,
				    pfc_pane, NULL);

      pfc_form = XtVaCreateWidget ("form", xmFormWidgetClass,
				   pfc_pane, XmNfractionBase, 7, NULL);

      str = XmStringCreateLocalized ("Available PFC files:");
      pfc_selW = XtVaCreateManagedWidget ("pfcfile_label",
					  xmLabelWidgetClass, pfc_form3,
					  XmNlabelString, str,
					  XmNleftAttachment,
					  XmATTACH_POSITION, XmNleftPosition,
					  0, XmNtopAttachment, XmATTACH_FORM,
					  NULL);
      XmStringFree (str);

      pfc_list = XmCreateScrolledList (pfc_form3, "pfc_files", NULL, 0);
      XtVaSetValues (pfc_list, XmNvisibleItemCount, 3,
		     XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
      XtVaSetValues (XtParent (pfc_list),
		     XmNleftAttachment, XmATTACH_POSITION,
		     XmNleftPosition, 0,
		     XmNtopAttachment, XmATTACH_WIDGET,
		     XmNtopWidget, pfc_selW,
		     /*XmNrightAttachment, XmATTACH_POSITION,
		        XmNrightPosition, 3, */
		     XmNwidth, 300, XmNbottomAttachment, XmATTACH_FORM, NULL);
      XtAddCallback (pfc_list,
		     XmNextendedSelectionCallback, pfc_snd_load_gemfile,
		     NULL);

      XtManageChild (pfc_list);

      XtManageChild (pfc_form3);

      title_str = XmStringCreateLocalized ("00Z and 12Z only:");
      obs_00_12 = XtVaCreateManagedWidget ("obs_00_12",
					   xmToggleButtonWidgetClass,
					   pfc_form, XmNindicatorType,
					   XmONE_OF_MANY, XmNlabelString,
					   title_str, XmNleftAttachment,
					   XmATTACH_POSITION, XmNleftPosition,
					   0, XmNtopAttachment, XmATTACH_FORM,
					   NULL);
      XmStringFree (title_str);
      XtAddCallback (obs_00_12, XmNvalueChangedCallback, pfc_snd_load_gemfile,
		     NULL);

      title_str = XmStringCreateLocalized ("Sounding times:");
      pfc_lbl_time = XtVaCreateManagedWidget ("obsfile_time",
					      xmLabelWidgetClass, pfc_form,
					      XmNlabelString, title_str,
					      XmNleftAttachment,
					      XmATTACH_POSITION,
					      XmNleftPosition, 0,
					      XmNtopAttachment,
					      XmATTACH_WIDGET, XmNtopWidget,
					      obs_00_12, XmNalignment,
					      XmALIGNMENT_BEGINNING, NULL);
      XmStringFree (title_str);

      pfc_snd_timelist = XmCreateScrolledList (pfc_form, "times", NULL, 0);

      XtVaSetValues (pfc_snd_timelist,
		     XmNvisibleItemCount, 20,
		     XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
      XtVaSetValues (XtParent (pfc_snd_timelist),
		     XmNleftAttachment, XmATTACH_POSITION,
		     XmNleftPosition, 0,
		     XmNtopAttachment, XmATTACH_WIDGET,
		     XmNtopWidget, pfc_lbl_time,
		     XmNbottomAttachment, XmATTACH_FORM,
		     XmNrightAttachment, XmATTACH_POSITION,
		     XmNrightPosition, 2, NULL);

      XtAddCallback (pfc_snd_timelist,
		     XmNextendedSelectionCallback, pfc_time_select_cb, NULL);

      XtManageChild (pfc_snd_timelist);

      pfc_lbl_station = XtVaCreateManagedWidget ("obsmap",
						 xmDrawingAreaWidgetClass,
						 pfc_form, XmNtopAttachment,
						 XmATTACH_POSITION,
						 XmNtopPosition, 1,
						 XmNleftAttachment,
						 XmATTACH_POSITION,
						 XmNleftPosition, 2,
						 XmNrightAttachment,
						 XmATTACH_FORM,
						 XmNbottomAttachment,
						 XmATTACH_FORM, XmNwidth, 560,
						 XmNheight, 560,
						 XmNbackground, pixels[0],
						 NULL);

      XtAddCallback (pfc_lbl_station, XmNexposeCallback,
		     (XtCallbackProc) mapw_exposeCb, NULL);

      plotData.mode = STNSELECT;
      XtAddEventHandler (pfc_lbl_station, ButtonPressMask,
			 FALSE, (XtEventHandler) mapw_pickstnCb_pfc, NULL);


      XtManageChild (pfc_form);

      /*
       * create the menubar
       */
      argcnt = 0;
      XtSetArg (args[argcnt], XmNtopAttachment, XmATTACH_FORM);
      argcnt++;
      XtSetArg (args[argcnt], XmNleftAttachment, XmATTACH_FORM);
      argcnt++;
      XtSetArg (args[argcnt], XmNrightAttachment, XmATTACH_FORM);
      argcnt++;

      menubar = XmCreateMenuBar (pfc_form2, "menubar", args, argcnt);

      argcnt = 0;
      XtSetArg (args[argcnt], XmNborderWidth, 1);
      argcnt++;

      pfcmenu = NULL;
      file_selection_menu (PFC_SND_TABLE, "nsharp", &pfcmenu,
			   (XtCallbackProc)pfc_select_cb, &ier);

      if (ier != 0)
	{
	  NxmWarn_show (pfc_form,
			"NSHARP:  ERROR opening $GEMTBL/nsharp/nsharp_observed.tbl\nSee system administrator.");
	  pfc_snd_dialog = NULL;
	  return;
	}

      obssel_opt = NxmMenuPulldownBuild (menubar, NULL, "File",
					 0, pfcmenu);
      XtSetValues (obssel_opt, args, argcnt);

      free (pfcmenu);


      /*
       * Create Map pulldown menu
       *
       * Set the items for the menu bar for the map areas.
       */
      nmap = nwxTable->nmap;
      for (ii = 0; ii < nmap; ii++)
	{
	  mapmenu[ii].label = (char *)
	    malloc (strlen (nwxTable->map_info[ii].name) + 1);
	  strcpy (mapmenu[ii].label, nwxTable->map_info[ii].name);
	  mapmenu[ii].class = &xmCascadeButtonGadgetClass;
	  mapmenu[ii].mnemonic = 0;
	  mapmenu[ii].accelerator = NULL;
	  mapmenu[ii].accel_text = NULL;
	  mapmenu[ii].callback = mapprojpfc_cb;
	  mapmenu[ii].which_widget = (long)ii;
	  mapmenu[ii].subitems = NULL;
	  mapmenu[ii].sub_buttons = NULL;
	}
      mapmenu[nmap].label = NULL;
      mapmenu[nmap].class = &xmCascadeButtonGadgetClass;
      mapmenu[nmap].mnemonic = 0;
      mapmenu[nmap].accelerator = NULL;
      mapmenu[nmap].accel_text = NULL;
      mapmenu[nmap].callback = NULL;
      mapmenu[nmap].which_widget = (long)nmap;
      mapmenu[nmap].subitems = NULL;
      mapmenu[nmap].sub_buttons = NULL;

      NxmMenuPulldownBuild (menubar, NULL, "Area", 0, mapmenu);
      for (ii = 0; ii < nmap; ii++)
	free (mapmenu[ii].label);

      if ((button = XtNameToWidget (menubar, "Area")))
	XtSetValues (button, args, argcnt);

      maparea_zoom = XmCreateCascadeButton (menubar, "Zoom", NULL, 0);
      XtAddCallback (maparea_zoom, XmNactivateCallback,
		     (XtCallbackProc) mapzoompfc_cb, pfc_lbl_station);
      XtSetValues (maparea_zoom, args, argcnt);
      XtManageChild (maparea_zoom);


      maparea_unzoom = XmCreateCascadeButton (menubar, "UnZoom", NULL, 0);
      XtAddCallback (maparea_unzoom, XmNactivateCallback,
		     (XtCallbackProc) mapunzoompfc_cb, NULL);
      XtSetValues (maparea_unzoom, args, argcnt);
      XtManageChild (maparea_unzoom);


      pfc_snd_cancel = XmCreateCascadeButton (menubar, "Cancel", NULL, 0);
      XtAddCallback (pfc_snd_cancel, XmNactivateCallback,
		     (XtCallbackProc) popdown_cb, pfc_snd_dialog);
      XtSetValues (pfc_snd_cancel, args, argcnt);
      XtManageChild (pfc_snd_cancel);

      pfc_snd_help = XmCreateCascadeButton (menubar, "Help", NULL, 0);
      XtSetValues (pfc_snd_help, args, argcnt);
      XtAddCallback (pfc_snd_help, XmNactivateCallback,
		     (XtCallbackProc) NxmHelp_helpBtnCb, (XtPointer) 3);
      XtManageChild (pfc_snd_help);

      XtVaSetValues (menubar, XmNmenuHelpWidget, pfc_snd_help, NULL);
      XtManageChild (menubar);

      XtManageChild (pfc_form2);
      XtManageChild (pfc_pane);
      XtManageChild (pfc_snd_dialog);

      pfc_map.mapindx = 0;
      pfc_map.zoomflg = 0;
      ii = nsharp_mapw_rgstr (pfc_lbl_station, map_winname);

      XtAddCallback (pfc_lbl_station, XmNresizeCallback,
		     (XtCallbackProc) mapw_resizeCb, NULL);

    }


  /* draw the map accoring to mapw call back */
  XtManageChild (pfc_snd_dialog);
  strcpy (gemdevice, map_winname);
  gslwin (gemdevice, &iret, strlen (gemdevice));
  Load_stationlist_pfc ();
}

/*========================================================================*/
/* ARGSUSED */
void
pfc_snd_load_gemfile (Widget wdgt, XtPointer clnt, XtPointer call)
/*************************************************************/
/* pfc_snd_load_gemfile                                      */
/*************************************************************/
{
  static int only_00_12 = 0;
  char *choice;
  int ii, sumtim, nlist;
  struct obs_file_times *olist;
  XmStringTable item_list;
  XmStringTable str_list;
  XtPointer userdata;


  if (XtIsSubclass (wdgt, xmToggleButtonWidgetClass))
    {
      /* toggling 0Z and 12Z on/off */
      Boolean is_set;

      XtVaGetValues (wdgt, XmNset, &is_set, NULL);
      if (is_set)
	only_00_12 = 1;
      else
	only_00_12 = 0;
    }



/* selected files in pfc_list */

  XtVaGetValues (pfc_list, XmNselectedItemCount, &nlist,
		 XmNselectedItems, &item_list, NULL);

  if (nlist == 0) return;

  XtVaGetValues (pfc_snd_timelist, XmNuserData, &userdata, NULL);
  if (userdata == NULL)
    {
      printf ("error getting user data\n");
      return;
    }
  olist = (struct obs_file_times *) userdata;

  sumtim = 0;
  for (ii = 0; ii < nlist; ii++)
    {
      int jj, ntimes, ier;

    choice = XmStringUnparse (item_list[ii], NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
      sprintf (pfcsoundfile, "%s/%s", olist->path, choice);
      XtFree (choice);
      get_gem_times (pfcsoundfile, &only_00_12, &time_list[sumtim], &ntimes,
		     &ier, strlen (pfcsoundfile), 20 /*sizeof(time_list) */ );
      if (ier == 0)
	{
	  for (jj = 0; jj < ntimes; jj++)
	    olist->posindex[sumtim + jj] = ii;
	  sumtim += ntimes;
	}
    }

  str_list = (XmStringTable) XtMalloc ((size_t) sumtim * sizeof (XmString *));
  for (ii = 0; ii < sumtim; ii++)
    {
      time_list[ii][11] = '\0';
      str_list[ii] = XmStringCreateLocalized (time_list[ii]);
    }
  XtVaSetValues (pfc_snd_timelist,
		 XmNitemCount, sumtim, XmNitems, str_list, NULL);
  XmListDeselectAllItems (pfc_snd_timelist);
  if (olist->selected_items != NULL)
    {
      free (olist->selected_items);
      olist->nitems = 0;
      olist->selected_items = NULL;
    }
  for (ii = 0; ii < sumtim; ii++)
    XmStringFree ((XmString) str_list[ii]);
  XtFree ((XtPointer) str_list);
}


/*========================================================================*/
/* ARGSUSED */
void
pfc_select_cb (Widget wdgt, XtPointer clnt, XtPointer call)
/*************************************************************/
/* pfc_snd_LOAD_GEMFILE                                      */
/*************************************************************/
{
  int ier;
  char path[256], tmpl[256];


  if (!XtIsSubclass (wdgt, xmFileSelectionBoxWidgetClass))
    {
      /* Cascade menu entry */
      char *choice;
      XmString label_str;

      XtVaGetValues (wdgt, XmNlabelString, &label_str, NULL);
      choice = XmStringUnparse (label_str, NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
      XmStringFree (label_str);

      if ((choice[0] == '\0') || (strcmp (choice, "BROWSE") == 0))
	{
	  sprintf (tmpl, "*");
	  sprintf (path, ".");
	}
      else
	{
	  get_file_alias (choice, path, tmpl, &ier,
			  strlen (choice), sizeof (path), sizeof (tmpl));
	  if (ier != 0)
	    {
	      sprintf (tmpl, "*");
	      sprintf (path, ".");
	    }
	}
      XtFree (choice);
      if (strcmp (tmpl, "*") == 0)
	{
	  file_browse_popup (path, tmpl, toplevel, pfc_select_cb);
	  return;
	}
    }
  else
    {
      /* file selection dialog entry of a single file */
      char *choice;
      XmFileSelectionBoxCallbackStruct *cbs =
	(XmFileSelectionBoxCallbackStruct *)call;

      if (cbs)
	{
	  char *cpos;
	  choice = XmStringUnparse (cbs->value, NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	  cpos = strrchr (choice, '/');
	  path[0] = '\0';
	  if (cpos != NULL)
	    {
	      strcpy (tmpl, cpos + 1);
	      strncat (path, choice, cpos - choice);
	    }
	  else
	    {
	      strncat (path, ".", 1);
	      strcpy (tmpl, choice);
	    }
	  XtFree (choice);
	}
    }

  {
    char sep = ';';
    int plen, tlen, rorder = (-1), flen, nfil;
    int maxlen = MAX_LIST_LEN;
    XtPointer userdata;
    struct obs_file_times *olist;
    char *filstr;

    XtVaGetValues (pfc_snd_timelist, XmNuserData, &userdata, NULL);

    if (userdata == NULL)
      {
	olist =
	  (struct obs_file_times *) malloc (sizeof (struct obs_file_times));
	olist->filstr = (char *) malloc (maxlen);
	olist->filstr[0] = '\0';
	olist->posindex = (int *) malloc (MAX_TIME_LIST * sizeof (int));
	olist->selected_items = NULL;
	olist->nitems = 0;
	olist->path[0] = '\0';
	XtVaSetValues (pfc_snd_timelist, XmNuserData, (XtPointer) olist,
		       NULL);
      }
    else
      {
	olist = (struct obs_file_times *) userdata;
      }
    filstr = olist->filstr;

    if (!XtIsSubclass (wdgt, xmFileSelectionBoxWidgetClass))
      {
	plen = strlen (path);
	tlen = strlen (tmpl);
	strcpy (olist->path, path);
	cfl_scnd (path, &plen, tmpl, &tlen, &sep, &maxlen, &rorder, filstr,
		  &flen, &nfil, &ier);
      }
    else
      {
	ier = 0;
	strcpy (filstr, tmpl);
	strcpy (olist->path, path);
      }

    if (ier == -1)
      {
	printf ("too many files in directory\n");
      }
    else
      {
	char *spos, *epos, tstr[256];
	int posnum = 0;
	XmStringTable str_list;
	int nlist = 0;

	spos = filstr;
	while (spos != NULL)
	  {
	    str_list = (XmStringTable) XtMalloc ((size_t) 1 *
						 sizeof (XmString *));
	    tstr[0] = '\0';
	    epos = strchr (spos, sep);
	    if (epos != NULL)
		strncat (tstr, spos, epos - spos);
	    else
		strcat (tstr, spos);
	    str_list[0] = XmStringCreateLocalized (tstr);

	    if (nlist == 0)
	      XtVaSetValues (pfc_list, XmNitemCount, 1,
			     XmNitems, str_list, NULL);
	    else
	      XmListAddItems (pfc_list, str_list, 1, nlist + 1);

	    XmStringFree (str_list[0]);
	    XtFree ((XtPointer) str_list);
	    nlist++;

	    spos = epos;
	    if (spos != NULL)
	      spos++;
	    posnum++;
	  }
	XmListDeselectAllItems (pfc_list);
	XmListDeleteAllItems (pfc_snd_timelist);

	if (XtIsSubclass (wdgt, xmFileSelectionBoxWidgetClass))
	    XmListSelectPos (pfc_list, 1, True);


      }
  }

}



/*========================================================================*/
/* ARGSUSED */
void
pfc_time_select_cb (Widget wdgt, XtPointer clnt, XtPointer call)
       /*************************************************************/
       /* PFC_TIME_SELECT_CB                                        */
       /*************************************************************/
{
  XmListCallbackStruct *cbs = (XmListCallbackStruct *) call;
  struct obs_file_times *olist;
  XtPointer userdata;
  char *choice;
  int top_item, fileindex;

  if (cbs->reason == XmCR_EXTENDED_SELECT)
    {
      if (cbs->selected_item_count == 0)
	return;
      top_item = cbs->selected_item_positions[0];
      XtVaGetValues (pfc_snd_timelist, XmNuserData, &userdata, NULL);
      if (userdata != NULL)
	{
	  int ier;
	  char sep = ';';
	  olist = (struct obs_file_times *) userdata;
	  fileindex = olist->posindex[top_item - 1];

	  sprintf (pfcsoundfile, "%s/", olist->path);
	  get_listitem (olist->filstr, fileindex, sep,
			pfcsoundfile + strlen (pfcsoundfile), &ier);

	  olist->nitems = cbs->selected_item_count;
	  if (olist->selected_items != NULL)
	    free (olist->selected_items);
	  olist->selected_items =
	    (int *) malloc (olist->nitems * sizeof (int));
	  memcpy (olist->selected_items, cbs->selected_item_positions,
		  olist->nitems * sizeof (int));
	}
      choice = XmStringUnparse (cbs->selected_items[0], NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
    }
  else
    {
     choice = XmStringUnparse (cbs->item, NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
    }

  sprintf (pfcsoundtime, "%s", choice);
  XtFree (choice);

  Load_stationlist_pfc ();

}

void
sta_select_cb_pfc (int which_sta)
       /*************************************************************/
       /* STA_SELECT_CB                                             */
       /*************************************************************/
{
  char stanum[8], sta_tmp[12], snd_station[9] = "";
  int ival;

  ival = sscanf (stnList.stnName[which_sta],
		 "%s %s %s", sta_id, stanum, sta_tmp);

  if (ival == 2)
    sprintf (snd_station, "@%s", stanum);
  else if (ival == 1)
    sprintf (snd_station, "@%s", sta_id);

  Load_gem_sounding (pfc_snd_dialog, pfc_snd_timelist, snd_station, sta_id);
}

/*========================================================================*/
/* ARGSUSED */
void
mapprojpfc_cb (Widget wdgt, XtPointer clnt, XtPointer call)
{
  int item_no = (long)clnt;

  pfc_map.mapindx = item_no;
  pfc_map.zoomflg = 0;
  Load_stationlist_pfc ();
}


/*========================================================================*/
/* ARGSUSED */
void
mapunzoompfc_cb (Widget wdgt, XtPointer clnt, XtPointer call)
{
  pfc_map.zoomflg = 0;
  Load_stationlist_pfc ();
}

/*========================================================================*/
/* ARGSUSED */
void
mapzoompfc_cb (Widget wdgt, XtPointer clnt, XtPointer call)
{
  Widget mapCanvW = (Widget) clnt;

  _mapzoom_cb (mapCanvW, mapw_pickstnCb_pfc, &pfc_map, Load_stationlist_pfc);
}
