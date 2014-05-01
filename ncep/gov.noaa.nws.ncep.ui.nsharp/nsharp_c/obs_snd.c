#include "gui.h"
#include "sharp95.h"


/* 
 * private rouines
 */
void obs_snd_load_gemfile (Widget, XtPointer, XtPointer);
void time_select_cb	  (Widget, XtPointer, XtPointer);
void mapproj_cb		  (Widget, XtPointer, XtPointer);
void mapunzoom_cb	  (Widget, XtPointer, XtPointer);
void mapzoom_cb		  (Widget, XtPointer, XtPointer);

Widget obs_snd_dialog = NULL;
Widget obs_snd_timelist, obs_snd_text;

extern mapstruct obs_map;
extern Widget toplevel;
extern char time_list[500][20];


void observed_sounding_cb (Widget wdgt)
/************************************************************************
 * OBSERVED_SOUNDING_CB                                                 *
 *                                                                      *
 * Display Observed Sounding Map and get user selection.                *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC        2/02    freed mapmenu.label                     *
 * T. Piper/SAIC        01/04   removed NAWIPS_TABLES                   *
 * T. Piper/SAIC        01/04   added NxmWarn_show                      *
 ***********************************************************************/
{
  static Widget obs_form, obs_form2, obs_pane,
			obs_snd_cancel, obs_snd_help,
			obs_lbl_time, obs_lbl_station;
  static Widget menubar, maparea_zoom, maparea_unzoom, button, obssel_opt,
    obs_00_12;
  XmString title_str;
  char gemdevice[72];

  int ii, iret = 0, ier, nmap;
  Arg args[10];
  Cardinal argcnt;

  static _NXMmenuItem mapmenu[30], *obsmenu;

  static char map_winname[] = "mapobs";

  if (!obs_snd_dialog)
    {
      title_str = XmStringCreateLocalized ("Observed Sounding Selection");
      obs_snd_dialog = XmCreateBulletinBoardDialog (toplevel, "obs_panel",
						    NULL, 0);
      XtVaSetValues (obs_snd_dialog, XmNdialogTitle, title_str, NULL);
      XmStringFree (title_str);

      obs_pane = XtVaCreateManagedWidget ("obs_snd_pane",
					  xmPanedWindowWidgetClass,
					  obs_snd_dialog,
					  XmNsashWidth, 1,
					  XmNsashHeight, 1, NULL);

      obs_form2 = XtVaCreateWidget ("form", xmFormWidgetClass,
				    obs_pane, NULL);

      obs_form = XtVaCreateWidget ("form", xmFormWidgetClass,
				   obs_pane, XmNfractionBase, 7, NULL);

      title_str = XmStringCreateLocalized ("00Z and 12Z only:");
      obs_00_12 = XtVaCreateManagedWidget ("obs_00_12",
					   xmToggleButtonWidgetClass,
					   obs_form, XmNindicatorType,
					   XmN_OF_MANY, XmNlabelString,
					   title_str, NULL);
      XtAddCallback (obs_00_12, XmNvalueChangedCallback, obs_snd_load_gemfile,
		     NULL);
      XmStringFree (title_str);

      title_str = XmStringCreateLocalized ("Sounding times:");
      obs_lbl_time = XtVaCreateManagedWidget ("obsfile_time",
					      xmLabelWidgetClass, obs_form,
					      XmNlabelString, title_str,
					      XmNleftAttachment,
					      XmATTACH_POSITION,
					      XmNleftPosition, 0,
					      XmNtopAttachment,
					      XmATTACH_POSITION,
					      XmNtopPosition, 1, XmNalignment,
					      XmALIGNMENT_BEGINNING, NULL);
      XmStringFree (title_str);

      obs_snd_timelist = XmCreateScrolledList (obs_form, "times", NULL, 0);

      XtVaSetValues (obs_snd_timelist,
		     XmNvisibleItemCount, 20,
		     XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
      XtVaSetValues (XtParent (obs_snd_timelist),
		     XmNleftAttachment, XmATTACH_POSITION,
		     XmNleftPosition, 0,
		     XmNtopAttachment, XmATTACH_WIDGET,
		     XmNtopWidget, obs_lbl_time,
		     XmNbottomAttachment, XmATTACH_FORM,
		     XmNrightAttachment, XmATTACH_POSITION,
		     XmNrightPosition, 2, NULL);

      XtAddCallback (obs_snd_timelist,
		     XmNextendedSelectionCallback, time_select_cb, NULL);

      XtManageChild (obs_snd_timelist);

      obs_lbl_station = XtVaCreateManagedWidget ("obsmap",
						 xmDrawingAreaWidgetClass,
						 obs_form, XmNtopAttachment,
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

      XtAddCallback (obs_lbl_station, XmNexposeCallback, 
		     (XtCallbackProc)mapw_exposeCb, NULL);

      plotData.mode = STNSELECT;
      XtAddEventHandler (obs_lbl_station, ButtonPressMask,
			 FALSE, (XtEventHandler) mapw_pickstnCb, NULL);


      XtManageChild (obs_form);

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

      menubar = XmCreateMenuBar (obs_form2, "menubar", args, argcnt);

      argcnt = 0;
      XtSetArg (args[argcnt], XmNborderWidth, 1);
      argcnt++;

      obsmenu = NULL;
      file_selection_menu (OBS_SND_TABLE, "nsharp", &obsmenu,
			   (XtCallbackProc)obs_snd_load_gemfile, &ier);

      if (ier != 0)
	{
	  NxmWarn_show (obs_form,
			"NSHARP:  ERROR opening $GEMTBL/nsharp/nsharp_observed.tbl\nSee system administrator.");
	  obs_snd_dialog = NULL;
	  return;
	}

      obssel_opt = NxmMenuPulldownBuild (menubar, NULL, "File",
					 0, obsmenu);
      XtSetValues (obssel_opt, args, argcnt);

      free (obsmenu);

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
	  mapmenu[ii].callback = mapproj_cb;
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
		     (XtCallbackProc) mapzoom_cb, obs_lbl_station);
      XtSetValues (maparea_zoom, args, argcnt);
      XtManageChild (maparea_zoom);


      maparea_unzoom = XmCreateCascadeButton (menubar, "UnZoom", NULL, 0);
      XtAddCallback (maparea_unzoom, XmNactivateCallback,
		     (XtCallbackProc) mapunzoom_cb, NULL);
      XtSetValues (maparea_unzoom, args, argcnt);
      XtManageChild (maparea_unzoom);


      obs_snd_cancel = XmCreateCascadeButton (menubar, "Cancel", NULL, 0);
      XtAddCallback (obs_snd_cancel, XmNactivateCallback,
		     (XtCallbackProc) popdown_cb, obs_snd_dialog);
      XtSetValues (obs_snd_cancel, args, argcnt);
      XtManageChild (obs_snd_cancel);

      obs_snd_help = XmCreateCascadeButton (menubar, "Help", NULL, 0);
      XtSetValues (obs_snd_help, args, argcnt);
      XtAddCallback (obs_snd_help, XmNactivateCallback,
		     (XtCallbackProc) NxmHelp_helpBtnCb, (XtPointer) 2);
      XtManageChild (obs_snd_help);

      XtVaSetValues (menubar, XmNmenuHelpWidget, obs_snd_help, NULL);
      XtManageChild (menubar);

      XtManageChild (obs_form2);
      XtManageChild (obs_pane);
      XtManageChild (obs_snd_dialog);

      obs_map.mapindx = 0;
      obs_map.zoomflg = 0;
      ii = nsharp_mapw_rgstr (obs_lbl_station, map_winname);

      XtAddCallback (obs_lbl_station, XmNresizeCallback,
		     (XtCallbackProc) mapw_resizeCb, NULL);

    }


/* draw the map accoring to mapw call back */
  XtManageChild (obs_snd_dialog);
  strcpy (gemdevice, map_winname);
  gslwin (gemdevice, &iret, strlen (gemdevice));
  Load_stationlist ();
}

/*========================================================================*/
/* ARGSUSED */
void obs_snd_load_gemfile (Widget wdgt, XtPointer clnt, XtPointer call)
/*************************************************************/
/* OBS_SND_LOAD_GEMFILE                                              */
/*************************************************************/
{
  int ier;
  char path[256], tmpl[256];
  static int only_00_12 = 0;

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
  else if (!XtIsSubclass (wdgt, xmFileSelectionBoxWidgetClass))
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
	  file_browse_popup (path, tmpl, toplevel, obs_snd_load_gemfile);
	  return;
	}
    }
  else
    {
/* file selection dialog entry of a single file */
      char *choice;
      XmFileSelectionBoxCallbackStruct *cbs =
	(XmFileSelectionBoxCallbackStruct *) call;

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

    XtVaGetValues (obs_snd_timelist, XmNuserData, &userdata, NULL);

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
	XtVaSetValues (obs_snd_timelist, XmNuserData, (XtPointer) olist,
		       NULL);
      }
    else
      {
	olist = (struct obs_file_times *) userdata;
      }
    filstr = olist->filstr;

    if (XtIsSubclass (wdgt, xmToggleButtonWidgetClass))
      {
	/* toggled */
	ier = 0;
	strcpy (path, olist->path);
	}
    else if (!XtIsSubclass (wdgt, xmFileSelectionBoxWidgetClass))
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
	char *spos, *epos;
	int ii, posnum = 0, ntimes, sumtim = 0;

	spos = filstr;
	while (spos != NULL)
	  {
	    sprintf (gemsoundfile, "%s/", path);
	    epos = strchr (spos, sep);
	    if (epos != NULL)
	      strncat (gemsoundfile, spos, epos - spos);
	    else
	      strcat (gemsoundfile, spos);

	    get_gem_times (gemsoundfile, &only_00_12, &time_list[sumtim],
			   &ntimes, &ier, strlen (gemsoundfile),
			   20 /*sizeof(time_list) */ );
	    if (ier == 0)
	      {
		for (ii = 0; ii < ntimes; ii++)
		  olist->posindex[sumtim + ii] = posnum;
		sumtim += ntimes;
	      }
	    spos = epos;
	    if (spos != NULL)
	      spos++;
	    posnum++;
	  }
	{
	  int i;
	  XmStringTable str_list;

	  str_list = (XmStringTable) XtMalloc ((size_t) sumtim *
					       sizeof (XmString *));
	  for (i = 0; i < sumtim; i++)
	    {
	      time_list[i][11] = '\0';
	      str_list[i] = XmStringCreateLocalized (time_list[i]);
	    }
	  XtVaSetValues (obs_snd_timelist,
			 XmNitemCount, sumtim, XmNitems, str_list, NULL);
	  XmListDeselectAllItems (obs_snd_timelist);
	  if (olist->selected_items != NULL)
	    {
	      free (olist->selected_items);
	      olist->nitems = 0;
	      olist->selected_items = NULL;
	    }
	  for (i = 0; i < sumtim; i++)
	    XmStringFree ((XmString) str_list[i]);
	  XtFree ((XtPointer) str_list);
	}
      }
  }
}

/*========================================================================*/
/* ARGSUSED */
void time_select_cb (Widget wdgt, XtPointer clnt, XtPointer call)
/*************************************************************/
/* TIME_SELECT_CB                                            */
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
      XtVaGetValues (obs_snd_timelist, XmNuserData, &userdata, NULL);
      if (userdata != NULL)
	{
	  int ier;
	  char sep = ';';
	  olist = (struct obs_file_times *) userdata;
	  fileindex = olist->posindex[top_item - 1];

	  sprintf (gemsoundfile, "%s/", olist->path);
	  get_listitem (olist->filstr, fileindex, sep,
			gemsoundfile + strlen (gemsoundfile), &ier);

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

  sprintf (gemsoundtime, "%s", choice);
  XtFree (choice);

  Load_stationlist ();

}

/*=====================================================================*/

void sta_select_cb (int which_sta)
/*************************************************************/
/* STA_SELECT_CB                                             */
/*************************************************************/
{
  char stanum[8], sta_tmp[12], snd_station[9] = "", sta_id[5] = "";
  int ival;

  ival = sscanf (stnList.stnName[which_sta],
		 "%s %s %s", sta_id, stanum, sta_tmp);

  if (ival == 2)
    sprintf (snd_station, "@%s", stanum);
  else if (ival == 1)
    sprintf (snd_station, "@%s", sta_id);

  Load_gem_sounding (obs_snd_dialog, obs_snd_timelist, snd_station, sta_id);
}

/*========================================================================*/
/* ARGSUSED */
void mapproj_cb (Widget wdgt, XtPointer clnt, XtPointer call)
{
  int item_no = (long)clnt;
/*-------------------------------------------------------------------------*/
  obs_map.mapindx = item_no;
  obs_map.zoomflg = 0;
  Load_stationlist ();
}

/*========================================================================*/
/* ARGSUSED */
void mapunzoom_cb (Widget wdgt, XtPointer clnt, XtPointer call)
{
  obs_map.zoomflg = 0;
  Load_stationlist ();
}

/*========================================================================*/
/* ARGSUSED */
void mapzoom_cb (Widget wdgt, XtPointer clnt, XtPointer call)
{
  Widget mapCanvW = (Widget) clnt;
  _mapzoom_cb (mapCanvW, mapw_pickstnCb, &obs_map, Load_stationlist);
}
