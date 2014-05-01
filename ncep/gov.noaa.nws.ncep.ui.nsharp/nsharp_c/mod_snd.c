#include "gui.h"
#include "sharp95.h"

/*
 *  private functions
 */
void _mapzoommdl ( void );
void model_list_cb	(Widget wdgt, XtPointer clnt, XtPointer call);
void model_select_cb	(Widget wdgt, XtPointer clnt, XtPointer call);
void mtime_select_cb	(Widget wdgt, XtPointer clnt, XtPointer call);
void mod_time_select_cb	(Widget wdgt, XtPointer clnt, XtPointer call);
void mapprojmdl_cb	(Widget wdgt, XtPointer clnt, XtPointer call);
void mapzoommdl_cb	(Widget wdgt, XtPointer clnt, XtPointer call);
void mapunzoommdl_cb	(Widget wdgt, XtPointer clnt, XtPointer call);
void set_mdlsta		(Widget wdgt, XtPointer clnt, XtPointer call);


extern char mtime_list[500][20], mdlsoundtime[20], mdlsoundsta[12],
  mdl_selected[12];
extern char mdlsoundfile[200];
extern mapstruct mod_map;

extern Widget mdl_statext;
extern Widget mdl_cursor_text;
extern Widget mdl_map;
extern Widget toplevel;


static _NXMmenuItem *modmenu;

Widget mdl_list = NULL;
Widget mdlfile_timelist;

static char map_winname[]="grid_map";

void model_sounding_cb (Widget wid)
/************************************************************************
 * SHOW_MODEL_INFO                                                      *
 **                                                                     *
 * log:                                                                 *
 * R. Tian/SAIC         02/03   Added CPF ratio button                  *
 * T. Piper/SAIC        01/04   Removed NAWIPS_TABLES                   *
 * T. Piper/SAIC        01/04   NxmWarn_show                            *
 ***********************************************************************/
{
  static Widget mdl_dialog = NULL, mdlpane, mdlform, mdlmenuform, mdlform2, mdlform3,
    mdlfile_ok, mdlfile_cancel, mdlfile_help,
    mdllbl_time, mdlstn_opt, mdltxtframe;
  static Widget mdl_selW;
  XmString one, two, thr, str, mdl_title;
  int ii, nmap;
  int ier;
  Arg args[10];
  Cardinal argcnt;
  static Widget menubar, maparea_zoom, maparea_unzoom, button;
  static Widget rowcol, radio_box;


  static _NXMmenuItem mapmenu[30];

  if (!mdl_dialog)
    {
      mdl_title = XmStringCreateLocalized ("Model Sounding Selection");
      mdl_dialog = XmCreateBulletinBoardDialog (toplevel, "mdl_panel",
						NULL, 0);
      XtVaSetValues (mdl_dialog, XmNdialogTitle, mdl_title, NULL);
      XmStringFree (mdl_title);

      mdlpane = XtVaCreateManagedWidget ("parcel_pane",
					 xmPanedWindowWidgetClass,
					 mdl_dialog,
					 XmNsashWidth, 1,
					 XmNsashHeight, 1, NULL);

      /*
       * Create forms for areas of popup dialog
       */

      /* Menu bar form */
      mdlmenuform = XtVaCreateWidget ("form", xmFormWidgetClass, mdlpane, NULL);

      /* data contols */
      mdlform2 = XtVaCreateWidget ("form", xmFormWidgetClass, mdlpane, 
			XmNwidth, 800, XmNfractionBase, 7, NULL);

      /* File and map selection form */
      mdlform = XtVaCreateWidget ("form", xmFormWidgetClass,
				  mdlpane, XmNwidth, 800, XmNfractionBase, 7,
				  NULL);

      /* lat/lon text box form */
      mdlform3 = XtVaCreateWidget ("form", xmFormWidgetClass, mdlpane, NULL);

      str = XmStringCreateLocalized ("Available Grid files:");
      mdl_selW = XtVaCreateManagedWidget ("mdlfile_label",
					  xmLabelWidgetClass, mdlform2,
					  XmNlabelString, str,
					  XmNleftAttachment,
					  XmATTACH_POSITION, XmNleftPosition,
					  0, XmNtopAttachment, XmATTACH_FORM,
					  NULL);
      XmStringFree (str);


      mdl_list = XmCreateScrolledList (mdlform2, "mdl_times", NULL, 0);
      XtVaSetValues (mdl_list, XmNvisibleItemCount, 3,
		     XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
      XtVaSetValues (XtParent (mdl_list),
		     XmNleftAttachment, XmATTACH_POSITION,
		     XmNleftPosition, 0,
		     XmNtopAttachment, XmATTACH_WIDGET,
		     XmNtopWidget, mdl_selW,
		     XmNrightAttachment, XmATTACH_POSITION,
		     XmNrightPosition, 3, NULL);
      XtAddCallback (mdl_list,
		     XmNextendedSelectionCallback, model_select_cb, NULL);

      XtManageChild (mdl_list);
      /*
       * Create file and map selection widgets first. The
       * map canvas events will be set for use by menubar later.
       */
      mdl_statext = XtVaCreateManagedWidget ("mdl_statext",
					     xmTextWidgetClass, mdlform2,
					     XmNrightAttachment, XmATTACH_FORM, 
					     XmNtopAttachment, XmATTACH_FORM,
					     XmNwidth, 250, NULL);

      XtAddCallback (mdl_statext, XmNvalueChangedCallback, set_mdlsta, NULL);
      XtAddCallback (mdl_statext, XmNactivateCallback, (XtCallbackProc) Load_mdl_sounding, mdl_list);


      str = XmStringCreateLocalized ("Location:");
      XtVaCreateManagedWidget ("mdlsta_label",
					      xmLabelWidgetClass, mdlform2,
					      XmNlabelString, str,
					      XmNrightAttachment, XmATTACH_WIDGET,
					      XmNrightWidget, mdl_statext,
					      XmNtopAttachment, XmATTACH_FORM,
					      NULL);
      XmStringFree (str);


      /*
       * Create a RowColumn Widget for radio buttons
       */
      rowcol = XtVaCreateManagedWidget ("Rc",
					xmRowColumnWidgetClass, mdlform2,
					XmNorientation, XmHORIZONTAL,
					XmNnumColumns, 2,
					XmNrightAttachment, XmATTACH_FORM,
					XmNbottomAttachment, XmATTACH_FORM,
					NULL);

      one = XmStringCreateLocalized ("Lat/Lon");
      two = XmStringCreateLocalized ("Station");
      thr = XmStringCreateLocalized ("CPF");

      radio_box = XmVaCreateSimpleRadioBox (rowcol,
					    "radio", 0,
					    (XtCallbackProc) mdl_cursor_fmt,
					    XmVaRADIOBUTTON, one, NULL, NULL,
					    NULL, XmVaRADIOBUTTON, two, NULL,
					    NULL, NULL, XmVaRADIOBUTTON, thr,
					    NULL, NULL, NULL, XmNorientation,
					    XmHORIZONTAL, NULL);

      XmStringFree (one);
      XmStringFree (two);
      XmStringFree (thr);

      XtManageChild (radio_box);

      XtManageChild (mdlform2);

      str = XmStringCreateLocalized ("Model times:");
      mdllbl_time = XtVaCreateManagedWidget ("mdlfile_time",
					     xmLabelWidgetClass, mdlform,
					     XmNlabelString, str,
					     XmNleftAttachment,
					     XmATTACH_POSITION,
					     XmNleftPosition, 0,
					     XmNtopAttachment, XmATTACH_FORM, 
					     XmNrightAttachment, XmATTACH_POSITION,
					     XmNrightPosition, 2,
					     XmNalignment,
					     XmALIGNMENT_BEGINNING, NULL);
      XmStringFree (str);

      mdl_map = XtVaCreateManagedWidget ("map",
					 xmDrawingAreaWidgetClass, mdlform,
					 XmNtopAttachment, XmATTACH_WIDGET,
					 XmNtopWidget, mdllbl_time,
					 XmNrightAttachment, XmATTACH_FORM,
					 XmNwidth, 560,
					 XmNheight, 560,
					 XmNbackground, pixels[0], NULL);

      argcnt = 0;
      XtSetArg (args[argcnt], XmNlistSizePolicy, XmCONSTANT);
      XtSetArg (args[argcnt], XmNselectionPolicy, XmEXTENDED_SELECT);
      argcnt++;

      mdlfile_timelist =
	XmCreateScrolledList (mdlform, "times", args, argcnt);

      /*XtVaSetValues (mdlfile_timelist, XmNvisibleItemCount, 10, NULL); */


      XtVaSetValues (XtParent (mdlfile_timelist),
		     XmNleftAttachment, XmATTACH_POSITION,
		     XmNleftPosition, 0,
		     XmNtopAttachment, XmATTACH_WIDGET,
		     XmNtopWidget, mdllbl_time,
		     XmNbottomAttachment, XmATTACH_FORM,
		     XmNrightAttachment, XmATTACH_WIDGET,
		     XmNrightWidget, mdl_map, NULL);

      XtAddCallback (mdlfile_timelist,
		     XmNextendedSelectionCallback,
		     mod_time_select_cb, (XtPointer) mdl_list);

      XtManageChild (mdlfile_timelist);


      /* ----- "mouse moved" event handler for cursor position----- */
      XtAddEventHandler (mdl_map, PointerMotionMask, FALSE,
			 (XtEventHandler) mdl_pointer, (XtPointer) NULL);

      XtAddCallback (mdl_map, XmNexposeCallback,
		     (XtCallbackProc) mapw_exposeCb, NULL);
      plotData.mode = STNSELECT;

      /* ----- "button pressed" event handler for cursor position----- */
      XtAddEventHandler (mdl_map, ButtonPressMask,
			 FALSE, (XtEventHandler) modmap_selCb, NULL);

      XtManageChild (mdlform);

      /*
       * create the menubar for top form
       */
      argcnt = 0;
      XtSetArg (args[argcnt], XmNtopAttachment, XmATTACH_FORM);
      argcnt++;
      XtSetArg (args[argcnt], XmNleftAttachment, XmATTACH_POSITION);
      argcnt++;
      XtSetArg (args[argcnt], XmNleftPosition, 0);
      argcnt++;
      XtSetArg (args[argcnt], XmNrightAttachment, XmATTACH_FORM);
      argcnt++;

      menubar = XmCreateMenuBar (mdlmenuform, "menubar", args, argcnt);

      argcnt = 0;
      XtSetArg (args[argcnt], XmNborderWidth, 1);
      argcnt++;

      modmenu = NULL;
      file_selection_menu ("nsharp_models.tbl", "nsharp", &modmenu,
			   (XtCallbackProc)model_list_cb, &ier);
      if (ier != 0)
	{
	  NxmWarn_show (mdlform,
			"NSHARP:  ERROR opening $GEMTBL/nsharp/nsharp_models.tbl\nSee system admin istrator.");
	  mdl_dialog = NULL;
	  return;
	}

      mdlstn_opt = NxmMenuPulldownBuild (menubar, NULL, "Select Model",
					 0, modmenu);
      XtSetValues (mdlstn_opt, args, argcnt);

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
	  mapmenu[ii].callback = mapprojmdl_cb;
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
		     (XtCallbackProc) mapzoommdl_cb, mdl_map);
      XtSetValues (maparea_zoom, args, argcnt);
      XtManageChild (maparea_zoom);


      maparea_unzoom = XmCreateCascadeButton (menubar, "UnZoom", NULL, 0);
      XtAddCallback (maparea_unzoom, XmNactivateCallback,
		     (XtCallbackProc) mapunzoommdl_cb, mdl_map);
      XtSetValues (maparea_unzoom, args, argcnt);
      XtManageChild (maparea_unzoom);


      mdlfile_ok = XmCreateCascadeButton (menubar, "OK", NULL, 0);
      XtAddCallback (mdlfile_ok, XmNactivateCallback,
		     (XtCallbackProc) popdown_cb, mdl_dialog);
      XtAddCallback (mdlfile_ok, XmNactivateCallback,
		     (XtCallbackProc) Load_mdl_sounding, mdl_list);
      XtSetValues (mdlfile_ok, args, argcnt);
      XtManageChild (mdlfile_ok);

      mdlfile_cancel = XmCreateCascadeButton (menubar, "Cancel", NULL, 0);
      XtAddCallback (mdlfile_cancel, XmNactivateCallback,
		     (XtCallbackProc) popdown_cb, mdl_dialog);
      XtSetValues (mdlfile_cancel, args, argcnt);
      XtManageChild (mdlfile_cancel);
      mdlfile_help = XmCreateCascadeButton (menubar, "Help", NULL, 0);
      XtSetValues (mdlfile_help, args, argcnt);
      XtAddCallback (mdlfile_help, XmNactivateCallback,
		     (XtCallbackProc) NxmHelp_helpBtnCb, (XtPointer) 4);
      XtManageChild (mdlfile_help);

      XtVaSetValues (menubar, XmNmenuHelpWidget, mdlfile_help, NULL);
      XtManageChild (menubar);

      XtManageChild (mdlmenuform);

      /*
       * create lat/lon cursor info widget
       */
      mdltxtframe = XtVaCreateManagedWidget ("mdl_latlonFrame",
					     xmFrameWidgetClass, mdlform3,
					     XmNrightAttachment,
					     XmATTACH_FORM,
					     XmNbottomAttachment,
					     XmATTACH_FORM, XmNtopAttachment,
					     XmATTACH_FORM, XmNshadowType,
					     XmSHADOW_IN, NULL);

      str = XmStringCreateLocalized ("------;-------");
      mdl_cursor_text = XtVaCreateManagedWidget ("cursor_text",
						 xmLabelWidgetClass,
						 mdltxtframe, XmNlabelString,
						 str, XmNrecomputeSize, False,
						 XmNwidth, 85, NULL);
      XmStringFree (str);

      XtManageChild (mdlform3);
      XtManageChild (mdlpane);
      XtManageChild (mdl_dialog);

      mod_map.mapindx = 0;
      mod_map.zoomflg = 0;
      ii = nsharp_mapw_rgstr (mdl_map, map_winname);

      XtAddCallback (mdl_map, XmNresizeCallback,
		     (XtCallbackProc) mapw_resizeCb, NULL);

    }
  XtManageChild (mdl_dialog);

  gslwin (map_winname, &ier, strlen (map_winname));

  nsharp_draw_map (map_winname, &mod_map, &ier);

}

/*========================================================================*/
/* ARGSUSED */
void model_list_cb (Widget wdgt, XtPointer clnt, XtPointer call)
/*************************************************************
 * MODELLIST_CB                                              *
 ************************************************************/
{
  int ii, ier, lens;
  char tmp_str[80];
  char path[256], tmpl[256];
/*  static int only_f000 = 0;  */

  if (wdgt == NULL)
    {
      printf ("look wdgt is null\n");
      return;
    }

  if (XtIsSubclass (wdgt, xmToggleButtonWidgetClass))
    {
      /* toggling f000 only on/of */
      Boolean is_set;

      XtVaGetValues (wdgt, XmNset, &is_set, NULL);
/*      if (is_set)
	only_f000 = 1;
      else
	only_f000 = 0;  NOT used */
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
	  int ncyc;
	  char sep[] = ";";
	  char *flist, *spos, *epos;
	  XmStringTable str_list;
#define SOMESIZE 200000

	  flist = (char *) malloc (SOMESIZE);
	  gd_gcyc (choice, sep, &ncyc, flist, &ier,
		   strlen(choice), strlen(sep), SOMESIZE);
	  if  (ier == 0 && ncyc > 0)  {
	      st_null(flist, flist, &lens, &ier, SOMESIZE, SOMESIZE);

	      str_list = (XmStringTable) XtMalloc ((size_t) ncyc *
						   sizeof (XmString *));

	      epos = flist;
	      ii = 0;
	      while (epos != NULL)
		{
		  epos = strrchr (flist, sep[0]);
		  if (epos != NULL)
		    spos = epos + 1;
		  else
		    spos = flist;
		  if (epos != NULL)
		    epos[0] = '\0';
		  sprintf (tmp_str, "%s %s", choice, spos);
		  str_list[ii] = XmStringCreateLocalized (tmp_str);
		  ii++;
		}
	      ncyc = ii;
	      XtVaSetValues (mdl_list, XmNitemCount, ncyc, XmNitems, str_list,
			     NULL);
	      XmListDeselectAllItems (mdl_list);

	      for (ii = 0; ii < ncyc; ii++)
		XmStringFree ((XmString) str_list[ii]);
	      XtFree ((XtPointer) str_list);
	      XmListDeleteAllItems (mdlfile_timelist);	/* clear all times in list */
	    }

	  free (flist);

	  strcpy (tmpl, choice);
	}
      XtFree (choice);
      if (strcmp (tmpl, "*") == 0)
	{
	  file_browse_popup (path, tmpl, toplevel, model_list_cb);
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
	  XmStringTable str_list;

	  choice = XmStringUnparse (cbs->value, NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	  str_list = (XmStringTable) XtMalloc (1 * sizeof (XmString *));
	  str_list[0] = cbs->value;
	  XtVaSetValues (mdl_list, XmNitemCount, 1, XmNitems, str_list, NULL);
	  XmListDeselectAllItems (mdl_list);
	  XmListDeleteAllItems (mdlfile_timelist);	/* clear all times in list */
	  XmListSelectPos (mdl_list, 1, True);
	  XtFree ((XtPointer) str_list);
	  XtFree (choice);
	}
    }

}

/*=====================================================================*/
/* ARGSUSED */
void model_select_cb (Widget wdgt, XtPointer clnt, XtPointer call)
/*  Log:									*
 * T. Piper/SAIC	12/04	Added free(tarr)				*
 *******************************************************************************/
 {
  int ii, jj, ier, sumpos = 0;
  char *choice;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *) call;
  XmStringTable str_list;
  XtPointer userdata;
  struct mlist_struct *mlist;
  int *tarr;

  if (cbs->reason == XmCR_EXTENDED_SELECT)
    {
      if (cbs->selected_item_count == 0)
	return;
      XtVaGetValues (wdgt, XmNuserData, &userdata, NULL);
      if (userdata == NULL)
	{
	  mlist =
	    (struct mlist_struct *) malloc (sizeof (struct mlist_struct));
	  mlist->ftim = 0;
	  mlist->posindex = NULL;
	  mlist->nselect = 0;
	  XtVaSetValues (wdgt, XmNuserData, (XtPointer) mlist, NULL);
	}
      else
	mlist = (struct mlist_struct *) userdata;

      if (mlist->posindex != NULL)
	{
	  free (mlist->posindex);
	  mlist->posindex = NULL;
	  mlist->ftim = 0;
	}

      tarr = (int *) malloc (cbs->selected_item_count * sizeof (int));
      for (ii = cbs->selected_item_count - 1; ii >= 0; ii--)
	{
	  int ngdftm, maxt = 500;	/* maxt size of mtime_list */
	  char cycle[] = " ";
	  char gdfile[256], gdatim[256];
	  char *spos;

	  choice = XmStringUnparse (cbs->selected_items[ii], NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	  spos = strchr (choice, ' ');
	  if (spos != NULL)
	    {
	      gdfile[0] = '\0';
	      strncat (gdfile, choice, spos - choice);
	      spos++;
	      strcpy (gdatim, spos);
	      gd_gtmf (gdfile, gdatim, cycle, &maxt, &ngdftm, mtime_list,
		       &ier, strlen (gdfile), strlen (gdatim), strlen (cycle),
		       20 /* mtime_list size */ );
	    }
	  else
	    {
	      int nfiles = 1;
	      gd_fltm (choice, &nfiles, &maxt, &ngdftm, mtime_list, &ier,
		       strlen (choice), 20 /* mtime_list size */ );
	    }
	  if (ier == 0 && ngdftm > 0)
	    {
	      int jj;
	      str_list = (XmStringTable) XtMalloc ((size_t) ngdftm *
						   sizeof (XmString *));
	      for (jj = 0; jj < ngdftm; jj++)
		{
		  int lens;
		  mtime_list[jj][19] = '\0';
		  lens = strcspn (mtime_list[jj], "\t \0");
		  mtime_list[jj][lens] = '\0';
		  str_list[jj] = XmStringCreateLocalized (mtime_list[jj]);
		}
	      if (sumpos == 0)
		XtVaSetValues (mdlfile_timelist, XmNitemCount, ngdftm,
			       XmNitems, str_list, NULL);
	      else
		XmListAddItems (mdlfile_timelist, str_list, ngdftm,
				sumpos + 1);

	      for (jj = 0; jj < ngdftm; jj++)
		XmStringFree ((XmString) str_list[jj]);
	      XtFree ((XtPointer) str_list);
	      sumpos = sumpos + ngdftm;
	      tarr[ii] = sumpos;
	    }
	  else
	    tarr[ii] = -1;

	  XtFree (choice);
	}
      XmListDeselectAllItems (mdlfile_timelist);

      mlist->posindex = (int *) malloc (sumpos * sizeof (int));
      mlist->ftim = sumpos;

      jj = 0;
      ii = cbs->selected_item_count - 1;
      while (jj < sumpos)
	{
	  if (jj < tarr[ii])
	    {
	      mlist->posindex[jj] = cbs->selected_item_positions[ii];
	      jj++;
	    }
	  else
	    ii--;
	}
      free(tarr);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void mod_time_select_cb (Widget wdgt, XtPointer clnt, XtPointer call)
/*************************************************************/
/* MOD_TIME_SELECT_CB                                        */
/*Log:									*
 * T. Piper/SAIC	12/04	Corrected free for model, cycle, & fhour*
 * T. Piper/SAIC	12/04	Corrected malloc for cycle		*
 ***********************************************************************/
{
  XmListCallbackStruct *cbs = (XmListCallbackStruct *) call;
  Widget cycles = (Widget)clnt;
  char *choice;
  XtPointer userdata;
  XmStringTable item_list;
  int ii, jj;
  struct mlist_struct *mlist;

  if (cbs->reason == XmCR_EXTENDED_SELECT)
    {
      if (cbs->selected_item_count == 0)
	return;
	
      XtVaGetValues (cycles, XmNuserData, &userdata, NULL);
      if (userdata == NULL)
	{
	  printf ("userdata is null\n");
	  return;
	}
      mlist = (struct mlist_struct *) userdata;

      XtVaGetValues (cycles, XmNitems, &item_list, NULL);
      if ( mlist->nselect > 0 )
        {
        for (ii = 0; ii < mlist->nselect; ii++)
	  {
	    free (mlist->modlist[ii].model);
	    free (mlist->modlist[ii].cycle);
	    free (mlist->modlist[ii].fhour);
	  }
        free (mlist->modlist);
	}

      mlist->nselect = cbs->selected_item_count;
      mlist->modlist =
	(struct modfile_struct *) malloc (mlist->nselect *
					  sizeof (struct modfile_struct));

      for (ii = 0; ii < cbs->selected_item_count; ii++)
	{
	  char *spos;
	  choice = XmStringUnparse (cbs->selected_items[ii], NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	  mlist->modlist[ii].fhour = (char *) malloc (strlen (choice) + 1);
	  strcpy (mlist->modlist[ii].fhour, choice);
	  XtFree (choice);


	  jj = cbs->selected_item_positions[ii];
	  choice = XmStringUnparse (item_list[mlist->posindex[jj - 1] - 1],
				NULL, XmCHARSET_TEXT, XmCHARSET_TEXT,
				NULL, 0, XmOUTPUT_ALL);

	if ((spos = strchr (choice, ' ')) != NULL)
	    {
	      mlist->modlist[ii].model = (char *) malloc ((spos - choice) + 1);
	      mlist->modlist[ii].model[0] = '\0';
	      strncat (mlist->modlist[ii].model, choice, spos - choice);
	      spos++;
	      mlist->modlist[ii].cycle = (char *) malloc (strlen (spos) + 1);
	      strcpy (mlist->modlist[ii].cycle, spos);
	}
	  else
	    {
	      mlist->modlist[ii].model = (char *) malloc (strlen (choice) + 1);
	      strcpy (mlist->modlist[ii].model, choice);
	      mlist->modlist[ii].cycle = (char *) malloc (2);
	      strcpy (mlist->modlist[ii].cycle, "*");
	    }
	  XtFree (choice);

	}

    }
  else
    {
      choice = XmStringUnparse (cbs->item, NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
      strcpy (mdlsoundtime, choice);
      XtFree (choice);
    }

}

/*========================================================================*/
/* ARGSUSED */
void mtime_select_cb ( Widget wdgt, XtPointer clnt, XtPointer call )
       /*************************************************************/
       /* MTIME_SELECT_CB                                           */
       /*************************************************************/
          {
          XmListCallbackStruct *cbs = (XmListCallbackStruct *)call;
          char *choice;

	  choice = XmStringUnparse (cbs->item, NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
          strcpy ( mdlsoundtime, choice );
          XtFree (choice);

          }

/*========================================================================*/
/* ARGSUSED */
void set_mdlsta ( Widget wdgt, XtPointer clnt, XtPointer call )
       /*************************************************************/
       /* SET_MDLSTA                                                */
       /*************************************************************/
             {
             char       *text_sta;

             text_sta = XmTextGetString (wdgt);

             if ( text_sta != NULL )
                 strcpy ( mdlsoundsta, text_sta );

             XtFree(text_sta);
             }


/*========================================================================*/
/* ARGSUSED */
void mapprojmdl_cb ( Widget wdgt, XtPointer clnt, XtPointer call )
        {
        int item_no = (long)clnt;
        int ier;

        mod_map.mapindx = item_no;
        mod_map.zoomflg = 0;
        nsharp_draw_map ( map_winname, &mod_map, &ier);
        }

/*========================================================================*/
/* ARGSUSED */
void mapunzoommdl_cb ( Widget wdgt, XtPointer clnt, XtPointer call )
{
        int ier;

        mod_map.zoomflg = 0;

        nsharp_draw_map ( map_winname, &mod_map, &ier);
}

/*========================================================================*/

void _mapzoommdl ( void )
{
        int ier;
        nsharp_draw_map ( map_winname, &mod_map, &ier);
}

/*========================================================================*/
/* ARGSUSED */
void mapzoommdl_cb ( Widget wdgt, XtPointer clnt, XtPointer call )
{
        Widget mapCanvW = (Widget) clnt;

        XtRemoveEventHandler( mapCanvW, PointerMotionMask, FALSE,
                        (XtEventHandler)mdl_pointer, NULL );

        _mapzoom_cb ( mapCanvW, NULL, &mod_map, _mapzoommdl );

        XtAddEventHandler( mapCanvW, PointerMotionMask, FALSE,
                        (XtEventHandler)mdl_pointer, (XtPointer)NULL );
}
