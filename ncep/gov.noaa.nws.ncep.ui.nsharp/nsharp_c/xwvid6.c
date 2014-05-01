#define GLOBAL
#define VIDEO
#include "gui.h"
#include "sharp95.h"
/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  XW Video Graphics Routines (Part #6)                       */
/*  These routines have been used in the porting of SHARP      */
/*  to X/Xt/Motif.                                             */
/*                                                             */
/*  John Hart & Jim Whistler                                   */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*                                                             */
/*      --------------------------------------------------     */
/*  List of Routines in this module:                           */
/*                                                             */
/*  X_Init                                                     */
/*  setcliprgn                                                 */
/*  setlinestyle                                               */
/*  getgtextextent                                             */
/*  moveto                                                     */
/*  lineto                                                     */
/*  rectangle                                                  */
/*  setcolor                                                   */
/*  set_font                                                   */
/*  reset_graphic                                              */
/*  parcel_popup                                               */
/*                                                             */
/***************************************************************/
/*  OPC MODIFICATION - J. Morgan 4/27/05            	        */
/*  page_next() - Increased page number to 5        	        */
/***************************************************************/

/*
 *	Private functions
 */
void expose_overlays	(Widget, XtPointer,
				   XmDrawingAreaCallbackStruct *call);
void file_cb		(Widget, XtPointer, XtPointer);
void gem_loop (void);
void inset_graphic	(Widget, XtPointer, XtPointer);
void interp_data	(Widget, XtPointer, XtPointer);
void load_cb		(Widget, XtPointer, XtPointer);
void mdl_cursor_fmt_val (int getset, int fmt, int *curval);
void mdl_cursor_mkstr	(float lat, float lon, char *cpos);
void option_cb		(Widget, XtPointer, XtPointer);
void option_graphic	(Widget, XtPointer, XtPointer);
void page_next		(Widget, XtPointer, XtPointer);
void parcel_popup	(Widget );
void pointer_update	(Widget, XtPointer, XEvent *event);
void position_cursor	(Widget, XtPointer, XEvent *event);
void redraw_sounding	(Widget, XtPointer, XEvent *event);
void reset_graphic	(Widget, XtPointer, XtPointer);
void resize_callback	(Widget, XtPointer, XtPointer);
void set_user_level	(Widget, XtPointer, XtPointer);
void sharp_load		(Widget, XtPointer, XtPointer);
void show_acars_info	(Widget );
void show_profiler_info	(Widget );
void tog_graph		(Widget, XtPointer, XtPointer);
void Toggle_Callback	(Widget, long, XtPointer);
void update_pointer	(Widget, XtPointer, XEvent *event);
GC xs_create_xor_gc	(Widget, char *color);

void load_cpf (void);

#define RESOURCE_FILE "Nsharp"

typedef struct
{				/* for drawing moving crosshairs or zoom boxes */
  int start_x, start_y, last_x, last_y;
  GC gc;
  XPoint points[4];
  int itype;			/* 0 = temp 1 = dwpt */
  int ilev;			/* 0 = bottom 1 = top 2 = inbetween */
  short yy, i;
} rubber_band_data;


stnlist_t stnList;
plotdata_t plotData;
usrslct_t usrSelect;
srchinfo_t srchInfo;

mapstruct obs_map, pfc_map, mod_map;

XtAppContext app;
XFontStruct *font_struct = NULL;
XPoint lpoints[2];
Cardinal current_parcel = 4;
rubber_band_data rbdata;
Widget toplevel, gemfile_timelist, gemfile_text, gemfile_stationlist,
	mdlfile_text, mdl_statext, user_defined_text, load_sharp,
	menubar_form, dwell_panel, parcel_button;

Widget mdl_map, mdl_cursor_text;
char time_list[500][20], sta_id[5], mdlsoundfile[200],
  mdlsoundtime[20], mtime_list[500][20], mdlsoundsta[12], mdl_selected[12];
int ntimes, sounding_type = 999;
float user_level = 850.;
char _transferInterrupt = 0;

extern int cursor_xwdth, cursor_xhght;
extern char pfcsoundfile[200], pfcsoundtime[20];
extern _NXMpixmapData NXMpixmapData;
extern _NXManimationFlags NXManimationFlags;
extern Widget print_dialog;

/*=====================================================================*/

void X_Init (void)
/************************************************************************
 *  X_INIT								*
 *  John Hart  NSSFC KCMO                                    		*
 *                                                           		*
 *  Draws basic SHARP graphic template on screen, including  		*
 *  areas, buttons, and tables.                              		*
 *                                                           		*
 * T. Piper/SAIC	07/03	removed *Cid initialization		*
 * T. Piper/SAIC	02/04	Changed name and location of help index *
 * T. Piper/SAIC	12/04	Changed about to Help			*
 * T. Piper/SAIC        01/05   Added check on NxmGmpkInit              *
 ***********************************************************************/
{
  int ret, xloc, yloc;
  unsigned int wd, ht, bw, dpth;
  XmString load_st, sharp_st, sounding_st, model_st,
    help_st, pfc_st, acars_st, print_st, file_st, exit_st, xstr;

  Widget topform, inset_button, reset_button,
    interp_button, option_button, next_page,
    graph_tog, menubar, help_menu, show_text, buttonrc, stop_rc;
/*---------------------------------------------------------------------*/

  int argc = 0;

/*
 * Get the NWX map info table for AREA selection widget
 */
  ret = nwxtbl_init ();


/* ----- Initialize X Toolkit and Load Resources ----- */
  toplevel = XtVaAppInitialize (&app, RESOURCE_FILE, NULL, 0,
				&argc, NULL, NULL,
				XmNbaseWidth, 780,
				XmNbaseHeight, 680,
				XmNminWidth, 860, XmNminHeight, 750, NULL);

  if (toplevel == NULL)
    printf ("toplevel is null\n");

/*
 * check resource file
 */
  NxmInitialize (toplevel);
  NxmRes_check (XtDisplay (toplevel), RESOURCE_FILE, NULL);
/*
 * display version in title string
 */
  NxmVers_showTitle (toplevel);


/* ----- Create Graphics Form (window) ----- */
  topform = XtVaCreateManagedWidget ("graphic_form",
				     xmFormWidgetClass, toplevel,
				     XmNfractionBase, 28, NULL);

/*
 * initialize GEMPAK variables
 */
  if ( NxmGmpkInit (topform, 2, NULL) != 0 ) {
    exit(1);
  }

/* ----- Initialize GEMPAK color palette ----- */
  {
    int color_bank = GraphCid, ncolors;
    xqclrs (&color_bank, &ncolors, pixels, &ret);
  }

  menubar_form = XtVaCreateManagedWidget ("menubar_form",
					  xmFormWidgetClass, topform,
					  XmNtopAttachment, XmATTACH_FORM,
					  XmNrightAttachment, XmATTACH_FORM,
					  XmNleftAttachment, XmATTACH_FORM,
					  NULL);



  /* ----- Create Menu bar across top of "topform" ----- */
  load_st = XmStringCreateLocalized ("Load");
  help_st = XmStringCreateLocalized ("Help");
  file_st = XmStringCreateLocalized ("File");
  xstr = XmStringCreateLocalized ("Options");
  menubar = XmVaCreateSimpleMenuBar (menubar_form, "menubar",
				     XmVaCASCADEBUTTON, file_st, (KeySym)'F',
				     XmVaCASCADEBUTTON, load_st, (KeySym)'L',
				     XmVaCASCADEBUTTON, xstr, (KeySym)'t',
				     XmVaCASCADEBUTTON, help_st, (KeySym)'H',
				     XmNtopAttachment, XmATTACH_FORM,
				     XmNleftAttachment, XmATTACH_FORM,
				     /*XmNbottomAttachment, XmATTACH_FORM, */
				     XmNrightAttachment, XmATTACH_FORM, NULL);

  XmStringFree (load_st);
  XmStringFree (help_st);
  XmStringFree (file_st);
  XmStringFree (xstr);
  XtManageChild (menubar);

  /* ----- Create Help Menu Option ----- */
  help_menu = XtNameToWidget (menubar, "button_3");
  XtVaSetValues (menubar, XmNmenuHelpWidget, help_menu, NULL);
  XtAddCallback (help_menu, XmNactivateCallback,
		 (XtCallbackProc) NxmHelp_helpBtnCb, (XtPointer) 1);

  /* ----- Create Pulldown Menu for Loading Soundings ----- */
  sharp_st = XmStringCreateLocalized ("Archive Files");
  sounding_st = XmStringCreateLocalized ("Observed Soundings");
  model_st = XmStringCreateLocalized ("Model Soundings");
  pfc_st = XmStringCreateLocalized ("PFC Soundings");
  acars_st = XmStringCreateLocalized ("ACARS Soundings");
  XmVaCreateSimplePulldownMenu (menubar, "load_menu",
				1, load_cb,
				XmVaPUSHBUTTON, sharp_st, (KeySym)'r', NULL, NULL,
				XmVaPUSHBUTTON, sounding_st, (KeySym)'O', NULL, NULL,
				XmVaPUSHBUTTON, model_st, (KeySym)'M', NULL, NULL,
				XmVaPUSHBUTTON, pfc_st, (KeySym)'P', NULL, NULL,
				XmVaPUSHBUTTON, acars_st, (KeySym)'A', NULL, NULL,
				NULL);
  XmStringFree (sharp_st);
  XmStringFree (sounding_st);
  XmStringFree (model_st);
  XmStringFree (pfc_st);
  XmStringFree (acars_st);
  print_st = XmStringCreateLocalized ("Print");
  exit_st = XmStringCreateLocalized ("Exit");
  XmVaCreateSimplePulldownMenu (menubar, "file_menu",
				0, file_cb,
				XmVaPUSHBUTTON, print_st, (KeySym)'P', NULL, NULL,
				XmVaPUSHBUTTON, exit_st, (KeySym)'x', NULL, NULL,
				NULL);
  XmStringFree (print_st);
  XmStringFree (exit_st);

  xstr = XmStringCreateLocalized ("Dwell");
  XmVaCreateSimplePulldownMenu (menubar, "file_menu",
				2, option_cb,
				XmVaPUSHBUTTON, xstr, (KeySym)'D', NULL, NULL, NULL);
  XmStringFree (xstr);

  dwell_panel = NxmDwell_popupCreate (menubar_form, "DwellPanel");

  stop_rc = XtVaCreateWidget ("stop_rc",
			      xmRowColumnWidgetClass, menubar_form,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, menubar,
			      XmNrightAttachment, XmATTACH_FORM,
			      XmNorientation, XmHORIZONTAL, NULL);

  /*
   * create stop/busy buttons
   */
  NxmBusy_createBtns (stop_rc);

  /*
   * initialize STOP button as insensitive
   */
  NxmBusy_setStopBtn (1);

  XtManageChild (stop_rc);


  print_dialog = NxmPrt_create ("Print dialog", topform, printdialog_ok_cb);

  /*
   * create HELP popup
   */
  NxmHelp_create (topform, "HelpDialog", "Help",
		  "$GEMHLP/hlp/nsharpIndex.hlp", 20, 80);


  /* ----- Create main area for soundings/parameters ----- */
  draw_reg = XtVaCreateManagedWidget ("canvas",
				      xmDrawingAreaWidgetClass, topform,
				      XmNtopAttachment, XmATTACH_WIDGET,
				      XmNtopWidget, menubar_form,
				      XmNleftAttachment, XmATTACH_FORM,
				      XmNrightAttachment, XmATTACH_FORM,
				      XmNbottomAttachment, XmATTACH_POSITION,
				      XmNbottomPosition, 26,
				      XmNbackground, pixels[0],
				      XmNwidth, 900, XmNheight, 620, NULL);

  /* ----- Create SKEWT/HODOGRAPH button ----- */
  graph_tog = XtVaCreateManagedWidget ("Hodograph",
				       xmPushButtonWidgetClass, topform,
				       XmNleftAttachment, XmATTACH_POSITION,
				       XmNleftPosition, 0,
				       XmNtopAttachment, XmATTACH_POSITION,
				       XmNtopPosition, 26,
				       XmNbottomAttachment, XmATTACH_POSITION,
				       XmNbottomPosition, 27,
				       XmNrightAttachment, XmATTACH_POSITION,
				       XmNrightPosition, 4, NULL);
  XtAddCallback (graph_tog, XmNactivateCallback,
		 (XtCallbackProc) tog_graph, NULL);

  /* ----- Create RESET button ----- */
  reset_button = XtVaCreateManagedWidget ("RESET",
					  xmPushButtonWidgetClass, topform,
					  XmNleftAttachment,
					  XmATTACH_POSITION, XmNleftPosition,
					  4, XmNtopAttachment,
					  XmATTACH_POSITION, XmNtopPosition,
					  26, XmNbottomAttachment,
					  XmATTACH_POSITION,
					  XmNbottomPosition, 27,
					  XmNrightAttachment,
					  XmATTACH_POSITION, XmNrightPosition,
					  8, NULL);
  XtAddCallback (reset_button, XmNactivateCallback,
		 (XtCallbackProc)reset_graphic, NULL);

  /* ----- Create PARCEL button ----- */
  parcel_button = XtVaCreateManagedWidget ("PARCEL",
					   xmPushButtonWidgetClass, topform,
					   XmNleftAttachment,
					   XmATTACH_POSITION, XmNleftPosition,
					   8, XmNtopAttachment,
					   XmATTACH_POSITION, XmNtopPosition,
					   26, XmNbottomAttachment,
					   XmATTACH_POSITION,
					   XmNbottomPosition, 27,
					   XmNrightAttachment,
					   XmATTACH_POSITION,
					   XmNrightPosition, 12, NULL);
  XtAddCallback (parcel_button, XmNactivateCallback,
		 (XtCallbackProc)parcel_popup, NULL);

  /* ----- Create INSET button ----- */
  inset_button = XtVaCreateManagedWidget ("INSET",
					  xmPushButtonWidgetClass, topform,
					  XmNleftAttachment,
					  XmATTACH_POSITION, XmNleftPosition,
					  0, XmNtopAttachment,
					  XmATTACH_POSITION, XmNtopPosition,
					  27, XmNbottomAttachment,
					  XmATTACH_FORM, XmNrightAttachment,
					  XmATTACH_POSITION, XmNrightPosition,
					  4, NULL);
  XtAddCallback (inset_button, XmNactivateCallback,
		 (XtCallbackProc)inset_graphic, NULL);

  /* ----- Create INTERP button ----- */
  interp_button = XtVaCreateManagedWidget ("INTERP",
					   xmPushButtonWidgetClass, topform,
					   XmNleftAttachment,
					   XmATTACH_POSITION, XmNleftPosition,
					   4, XmNtopAttachment,
					   XmATTACH_POSITION, XmNtopPosition,
					   27, XmNbottomAttachment,
					   XmATTACH_FORM, XmNrightAttachment,
					   XmATTACH_POSITION,
					   XmNrightPosition, 8, NULL);
  XtAddCallback (interp_button, XmNactivateCallback,
		 (XtCallbackProc)interp_data, NULL);

  /* ----- Create OPTIONS button ----- */
  option_button = XtVaCreateManagedWidget ("Overlay: OFF",
					   xmPushButtonWidgetClass, topform,
					   XmNleftAttachment,
					   XmATTACH_POSITION, XmNleftPosition,
					   8, XmNtopAttachment,
					   XmATTACH_POSITION, XmNtopPosition,
					   27, XmNbottomAttachment,
					   XmATTACH_FORM, XmNrightAttachment,
					   XmATTACH_POSITION,
					   XmNrightPosition, 12, NULL);
  XtAddCallback (option_button, XmNactivateCallback,
		 (XtCallbackProc)option_graphic, NULL);

  /* ----- Create NEXT PAGE button ----- */
  next_page = XtVaCreateManagedWidget ("NEXT PAGE",
				       xmPushButtonWidgetClass, topform,
				       XmNleftAttachment, XmATTACH_POSITION,
				       XmNleftPosition, 12,
				       XmNtopAttachment, XmATTACH_POSITION,
				       XmNtopPosition, 26,
				       XmNbottomAttachment, XmATTACH_POSITION,
				       XmNbottomPosition, 27,
				       XmNrightAttachment, XmATTACH_POSITION,
				       XmNrightPosition, 16, NULL);
  XtAddCallback (next_page, XmNactivateCallback,
		 (XtCallbackProc)page_next, NULL);

  show_text = XtVaCreateManagedWidget ("SHOW TEXT",
				       xmPushButtonWidgetClass, topform,
				       XmNleftAttachment, XmATTACH_POSITION,
				       XmNleftPosition, 12,
				       XmNtopAttachment, XmATTACH_POSITION,
				       XmNtopPosition, 27,
				       XmNbottomAttachment, XmATTACH_FORM,
				       XmNrightAttachment, XmATTACH_POSITION,
				       XmNrightPosition, 16, NULL);
  XtAddCallback (show_text, XmNactivateCallback,
		 (XtCallbackProc)show_textCb, NULL);

  /* ---- Create Cursor Readout display ---- */
  cursor_out = XtVaCreateManagedWidget ("cursor_out",
					xmDrawingAreaWidgetClass, topform,
					XmNleftAttachment, XmATTACH_POSITION,
					XmNleftPosition, 17,
					XmNrightAttachment, XmATTACH_FORM,
					XmNbottomAttachment, XmATTACH_FORM,
					XmNtopAttachment, XmATTACH_POSITION,
					XmNtopPosition, 26,
					XmNbackground, pixels[0], NULL);

  /* ----- Manage Menubar and loop controls ----- */
  {
    int nloop_insensitive = 4;
    Pixel background;
    WidgetList loopsensitive;

    loopsensitive = (WidgetList) XtMalloc ((size_t) nloop_insensitive *
					   sizeof (Widget));
    loopsensitive[0] = option_button;
    loopsensitive[1] = show_text;
    loopsensitive[2] = reset_button;
    loopsensitive[3] = interp_button;
    buttonrc = NxmAnimationPanelCreate (menubar_form, "AnimationButtons",
					"yellow", "black", loopsensitive,
					nloop_insensitive,
					(XtCallbackProc)
					NxmLoopButtonCallback, gem_loop);

    XtVaGetValues (toplevel, XmNbackground, &background, NULL);
    XtVaSetValues (buttonrc,
		   XmNbackground, background,
		   XmNtopAttachment, XmATTACH_WIDGET,
		   XmNtopWidget, menubar,
		   XmNrightAttachment, XmATTACH_WIDGET,
		   XmNrightWidget, stop_rc,
		   XmNrightOffset, 30, XmNorientation, XmHORIZONTAL, NULL);

  }

  XtManageChild (menubar_form);
  NxmLoopbuttonSensitive (False);


  /* ----- Create Window and Map Screen ----- */
  XtRealizeWidget (toplevel);
  {
    char wname[] = "skewt_canvas";
    NxmGmpkRgstr (draw_reg, wname, NULL);
  }


  /* ----- Query "Graphic Context" ----- */
  gc = xqgemgc ();

  /* ----- callback for expose ----- */
  XtAddCallback (draw_reg, XmNexposeCallback,
		 (XtCallbackProc)expose_overlays, NULL);

  /* ----- callback for resize ----- */
  XtAddCallback (draw_reg, XmNresizeCallback,
		 (XtCallbackProc)resize_callback, NULL);

  /* ----- "button pressed" event ----- */
  XtAddEventHandler (draw_reg, ButtonPressMask, FALSE,
		     (XtEventHandler)position_cursor, (XtPointer)NULL);

  /* ----- "mouse moved with button #1 depressed" event ----- */
  XtAddEventHandler (draw_reg, Button1MotionMask, FALSE,
		     (XtEventHandler)update_pointer, (XtPointer)NULL);

  /* ----- "button released" event ----- */
  XtAddEventHandler (draw_reg, ButtonReleaseMask, FALSE,
		     (XtEventHandler)redraw_sounding, (XtPointer)NULL);

  /* ----- "mouse moved" event ----- */
  XtAddEventHandler (draw_reg, PointerMotionMask, FALSE,
		     (XtEventHandler)pointer_update, (XtPointer)NULL);

  XGrabButton (XtDisplay (draw_reg), AnyButton, AnyModifier,
	       XtWindow (draw_reg), TRUE,
	       ButtonPressMask | Button1MotionMask |
	       ButtonReleaseMask,
	       GrabModeAsync, GrabModeAsync, XtWindow (draw_reg), 0);

  {
    /* ----- Create Cursor Graphic Context ----- */
    Window root;
/*---------------------------------------------------------------------*/
    gc_cursor =
      XCreateGC (XtDisplay(cursor_out), XtWindow(cursor_out), 0, 0);
    /* ----- Set background color to Black (pixels[0]) ----- */
    XSetBackground (XtDisplay(cursor_out), gc_cursor, pixels[0]);
    XGetGeometry (XtDisplay(cursor_out), XtWindow(cursor_out), &root, &xloc,
		  &yloc, &wd, &ht, &bw, &dpth);
    cursor_xwdth = (int) wd;
    cursor_xhght = (int) ht;
    canvas_cursor =
      XCreatePixmap (XtDisplay (cursor_out), root, wd, ht, dpth);
  }
  XtVaGetValues ( draw_reg,
		XmNwidth,	&wd,
		XmNheight,	&ht,
		NULL );
  xwdth = (int)wd;
  xhght = (int)ht;
  /* initialize canvas to first pixmap */
  canvas = xqpxms (0, 0);

  /* ----- Set background color to Black (pixels[0]) ----- */
  XSetBackground (XtDisplay (draw_reg), gc, pixels[0]);
  XFillRectangle (XtDisplay (draw_reg), canvas, gc, 0, 0, wd, ht);

  /* ----- Initialize "RubberBand" struct for skewt edit ----- */
  rbdata.gc = xs_create_xor_gc (draw_reg, "white");
  rbdata.start_x = 0;
  rbdata.start_y = 0;
  rbdata.last_x = 0;
  rbdata.last_y = 0;
  rbdata.itype = 0;
  rbdata.ilev = 2;
  rbdata.yy = 0;
  rbdata.i = 0;
  rbdata.points[0].x = rbdata.points[1].x = rbdata.points[2].x = 0;
  rbdata.points[0].y = rbdata.points[1].y = rbdata.points[2].y = 0;

  set_font (2);


  resize_callback (draw_reg, (XtPointer) NULL, (XtPointer)NULL);
}

/*=====================================================================*/

void setcliprgn (short tlx, short tly, short brx, short bry, Widget _canvas,
	    GC _gc)
/*************************************************************/
/*  SETCLIPRGN                                               */
/*************************************************************/
{
  Region regn;
  XRectangle rect;
/*---------------------------------------------------------------------*/
  regn = XCreateRegion ();
  rect.x = (short) tlx - (short) 1;
  rect.y = tly - (short) 1;
  rect.width = (unsigned short) (brx - tlx + 5);
  rect.height = (unsigned short) (bry - tly + 5);
  XUnionRectWithRegion (&rect, regn, regn);

  XSetRegion (XtDisplay (_canvas), _gc, regn);

  XDestroyRegion (regn);
}

/*=====================================================================*/

void setlinestyle (short style, short width)
/*************************************************************/
/*  SETLINESTYLE                                             */
/*     style = 1 = solid                                     */
/*     style = 2 = dash                                      */
/*     style = 3 = dash dot dash                             */
/*     style = 4 = short dash long dash                      */
/*************************************************************/
{
  int dash_offset = 1;
  static char dash_dash[] = { 3, 3 };
  static char dash_dot[] = { 4, 4, 2, 4 };
  static char dash_long[] = { 3, 3, 6, 3 };
  Cardinal line_width;
/*---------------------------------------------------------------------*/
  if (width < 0 || width > 10)
    line_width = 1;
  else
    line_width = (Cardinal) width;

  if (style == 1)
    {
      XSetLineAttributes (XtDisplay (draw_reg), gc, line_width,
			  LineSolid, CapButt, JoinRound);
    }
  else
    {
      XSetLineAttributes (XtDisplay (draw_reg), gc, line_width,
			  LineOnOffDash, CapButt, JoinRound);
    }

  if (style == 2)
    {
      XSetDashes (XtDisplay (draw_reg), gc, dash_offset, dash_dash, 2);
    }
  else if (style == 3)
    {
      XSetDashes (XtDisplay (draw_reg), gc, dash_offset, dash_dot, 4);
    }
  else if (style == 4)
    {
      XSetDashes (XtDisplay (draw_reg), gc, dash_offset, dash_long, 4);
    }
}

/*=====================================================================*/

int getgtextextent (char *st)
/*************************************************************/
/*  GETGTEXTEXTENT                                           */
/*************************************************************/
{
    return XTextWidth (font_struct, st, (int) strlen (st));
}

/*=====================================================================*/

void moveto (short x, short y)
/*************************************************************/
/*  MOVETO                                                   */
/*************************************************************/
{
  lpoints[0].x = x;
  lpoints[0].y = y;
}

/*=====================================================================*/

void lineto (short x, short y)
/*************************************************************/
/*  LINETO                                                   */
/*************************************************************/
{
  lpoints[1].x = x;
  lpoints[1].y = y;

  XDrawLine (XtDisplay (draw_reg), canvas, gc,
	     (int) lpoints[0].x, (int) lpoints[0].y,
	     (int) lpoints[1].x, (int) lpoints[1].y);
  lpoints[0].x = x;
  lpoints[0].y = y;
}

/*=====================================================================*/

void rectangle (int type, short x, short y, short width, short height)
/*************************************************************/
/*  RECTANGLE                                                */
/*************************************************************/
{

  if (type == 0)
    {
      XDrawRectangle (XtDisplay (draw_reg), (canvas), gc,
		      (int) x, (int) y, (Cardinal) (width - x),
		      (Cardinal) (height - y));
    }
  else if (type == 1)
    {
      XFillRectangle (XtDisplay (draw_reg), (canvas), gc, (int) x,
		      (int) y, (Cardinal) (width - x + 1),
		      (Cardinal) (height - y + 1));
    }
}

/*=====================================================================*/

void rectangle_cursor (int type, short x, short y, short width, short height)
/*************************************************************/
/*  RECTANGLE_CURSOR                                         */
/*************************************************************/
{

  if (type == 0)
    {
      XDrawRectangle (XtDisplay (cursor_out), XtWindow (cursor_out),
		      gc_cursor, (int) x, (int) y, (Cardinal) (width - x),
		      (Cardinal) (height - y));
    }
  else if (type == 1)
    {
      XFillRectangle (XtDisplay (cursor_out), XtWindow (cursor_out),
		      gc_cursor, (int) x, (int) y, (Cardinal) (width - x),
		      (Cardinal) (height - y));
    }
}

/*=====================================================================*/

void setcolor (int color, Widget canvas, GC _gc)
/*************************************************************/
/*  SETCOLOR                                                 */
/*************************************************************/
{
    XSetForeground (XtDisplay (canvas), _gc, pixels[color]);
}

/*=====================================================================*/

static char font_1[] =
  { "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-1" };

static char font_2[] =
  { "-adobe-courier-bold-r-normal--14-100-100-100-m-90-iso8859-1" };

static char font_3[] =
  { "-adobe-times-bold-r-normal--17-120-100-100-p-88-iso8859-1" };

static char font_4[] =
  { "-adobe-new century schoolbook-bold-*-*-*-34-*-*-*-*-*-*-*" };

static char font_5[] = { "-*-symbol-medium-*-*-*-14-140-75-75-p-85-*-*" };

void set_font (short font)
/*************************************************************/
/*  SET_FONT                                                 */
/*************************************************************/
{
  if (font_struct)
    XFreeFont (XtDisplay (draw_reg), font_struct);

  if (font == 1)
    {
      font_struct = XLoadQueryFont (XtDisplay (draw_reg), font_1);
    }
  else if (font == 2)
    {
      font_struct = XLoadQueryFont (XtDisplay (draw_reg), font_2);
    }
  else if (font == 3)
    {
      font_struct = XLoadQueryFont (XtDisplay (draw_reg), font_3);
    }
  else if (font == 4)
    {
      font_struct = XLoadQueryFont (XtDisplay (draw_reg), font_4);
    }
  else if (font == 5)
    {
      font_struct = XLoadQueryFont (XtDisplay (draw_reg), font_5);
    }

  if (font_struct)
    XSetFont (XtDisplay (draw_reg), gc, font_struct->fid);
}

void
set_font_cursor (short font, Widget _canvas, GC _gc, XFontStruct ** _fs)
/*************************************************************/
/*  SET_FONT_CURSOR                                          */
/*************************************************************/
{

  if (*_fs)
    XFreeFont (XtDisplay (_canvas), *_fs);

  switch ((int) font)
    {
    case 1:
      *_fs = XLoadQueryFont (XtDisplay (_canvas), font_1);
      break;
    case 2:
      *_fs = XLoadQueryFont (XtDisplay (_canvas), font_2);
      break;
    case 3:
      *_fs = XLoadQueryFont (XtDisplay (_canvas), font_3);
      break;
    case 4:
      *_fs = XLoadQueryFont (XtDisplay (_canvas), font_4);
      break;
    case 5:
      *_fs = XLoadQueryFont (XtDisplay (_canvas), font_5);
      break;
    default:
      printf ("Unknown font number %d\n", font);
      *_fs = XLoadQueryFont (XtDisplay (_canvas), font_2);
    }

  if (*_fs)
    XSetFont (XtDisplay (_canvas), _gc, (*_fs)->fid);
}

/*=====================================================================*/
/* ARGSUSED */
void reset_graphic (Widget wdgt, XtPointer clnt, XtPointer call) 
/*************************************************************/
/*  RESET_GRAPHIC                                            */
/*************************************************************/
{

  if (!qc (i_temp (700.0F)))
    return;

  restore_origsndg ();
  reset_options (mode, pagenum);

  XCopyArea (XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
	     gc, 0, 0, (Cardinal)xwdth, (Cardinal)xhght, 0, 0);
}

/*=====================================================================*/
/* ARGSUSED */
static Widget rbutton[5];
void parcel_popup (Widget wdgt)
/*************************************************************/
/*  PARCEL_POPUP                                             */
/*                                                           */
/*  Creates pop-up window to choose parcel.                  */
/*************************************************************/
{
  static Widget parcel_pane, form, prcl_top = 0;
  static Widget rowcol, form2, parcel_cancel;
  long ii;
  char *labels[] = { "Current Surface", "Forecast Surface",
    "Mean mixing layer",
    "Most unstable parcel",
    "User Defined level"
  };
  XmString pop_title;
/*---------------------------------------------------------------------*/
  if (!qc (i_temp (700.0F)))
    return;

  if (!prcl_top)
    {

      /* ----- Create a Dialog for parcel selection ----- */
      pop_title = XmStringCreateLocalized ("Select Thermodynamic Parcel");
      prcl_top = XmCreateBulletinBoardDialog (wdgt, "parcel_panel", NULL, 0);
      XtVaSetValues (prcl_top, XmNdialogTitle, pop_title, NULL);
      XmStringFree (pop_title);

      parcel_pane = XtVaCreateWidget ("parcel_pane",
				      xmPanedWindowWidgetClass,
				      prcl_top,
				      XmNsashWidth, 1,
				      XmNsashHeight, 1, NULL);

      form = XtCreateManagedWidget ("content_form",
				    xmFormWidgetClass, parcel_pane, NULL, 0);

      /* Create a rowcol */

      rowcol = XtVaCreateManagedWidget ("Parcel",
					xmRowColumnWidgetClass, form,
					XmNnumColumns, 2,
					XmNorientation, XmVERTICAL,
					XmNspacing, 4,
					XmNtopAttachment, XmATTACH_POSITION,
					XmNtopPosition, 2,
					XmNleftAttachment, XmATTACH_POSITION,
					XmNleftPosition, 2, NULL);

      for (ii = 0; ii < (long)XtNumber (labels); ii++)
	{
	  rbutton[ii] = XtVaCreateManagedWidget (labels[ii],
						xmToggleButtonWidgetClass,
						rowcol, XmNindicatorType,
						XmONE_OF_MANY, NULL);

	  XtAddCallback (rbutton[ii], XmNvalueChangedCallback,
			 (XtCallbackProc)Toggle_Callback, (XtPointer)ii);

	  if (ii == (long)(current_parcel - 1))
	    XmToggleButtonSetState (rbutton[ii], True, True);
	}

      for (ii = 0; ii < (long)XtNumber (labels); ii++)
	{
	  if (ii == 4)
	    {
	      user_defined_text = XtVaCreateManagedWidget ("user_text",
							   xmTextWidgetClass,
							   rowcol, XmNcolumns,
							   16, XmNvalue,
							   "850", NULL);

	      XtAddCallback (user_defined_text, XmNvalueChangedCallback,
			     set_user_level, NULL);
	    }
	  else
	    {
	      XtVaCreateManagedWidget (labels[ii],
				       xmSeparatorWidgetClass, rowcol,
				       XmNseparatorType, XmNO_LINE, NULL);
	    }
	}


      form2 = XtVaCreateWidget ("form", xmFormWidgetClass,
				parcel_pane, XmNfractionBase, 7, NULL);


      parcel_cancel = XtVaCreateManagedWidget ("CANCEL",
					       xmPushButtonWidgetClass, form2,
					       XmNleftAttachment,
					       XmATTACH_POSITION,
					       XmNleftPosition, 2,
					       XmNtopAttachment,
					       XmATTACH_FORM,
					       XmNbottomAttachment,
					       XmATTACH_FORM,
					       XmNrightAttachment,
					       XmATTACH_POSITION,
					       XmNrightPosition, 5, NULL);

      XtAddCallback (parcel_cancel, XmNactivateCallback,
		     (XtCallbackProc) popdown_cb, prcl_top);


      XtManageChild (form2);
      XtManageChild (parcel_pane);


    }
  XtManageChild (prcl_top);

}

/*=====================================================================*/
/* ARGSUSED */
void inset_graphic (Widget wdgt, XtPointer clnt, XtPointer call )
/*************************************************************/
/* INSET_GRAPHIC					     */
/*************************************************************/
{
  char winname[] = "skewt_canvas";
  int i, ier, len =
    strlen (winname), ixsize, iysize, isxsiz, isysiz, ixo, iyo, ncurwn;
  int npxms, curpxm;


  inset_options (mode);

  xslwin (winname, &len, &ixsize, &iysize,
	  &isxsiz, &isysiz, &ixo, &iyo, &ncurwn, &ier);

  xqcpxm (&npxms, &curpxm);

  for (i = 0; i < npxms; i++)
    {
      if ((sndgs[i] != NULL) && (sndgs[i]->numlev > 0))
	{
	  xscpxm (i, &ier);
	  canvas = xqpxms (0, i);
	  sndgp = sndgs[i];
	  redraw_graph (mode);
	}
    }
  xscpxm (curpxm, &ier);
  sndgp = sndgs[curpxm];
  canvas = xqpxms (0, curpxm);

  XCopyArea (XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
	     gc, 0, 0, (Cardinal)xwdth, (Cardinal)xhght, 0, 0);
}

/*=====================================================================*/
/* ARGSUSED */
void interp_data (Widget wdgt, XtPointer clnt, XtPointer call)
/*************************************************************/
/* INTERP_DATA						     */
/*************************************************************/
{
  if (!qc (i_temp (700.0F)))
    return;

  interp_sndg ();
  redraw_graph (mode);
  define_parcel ((short)current_parcel, user_level);
  show_parcel ();
  show_page (pagenum);
}

/*=====================================================================*/
/* ARGSUSED */
void option_graphic (Widget wdgt, XtPointer clnt, XtPointer call)
/****************************************************************
 * OPTION_GRAPHIC						*
 **								*
 * T. Piper/SAIC	2/02	Freed XmStringCreateLocalized	*
 ***************************************************************/
{
  XmString tmp;

  if (!qc (i_temp (700.0F)))
    return;
  overlay_previous += 1;
  if (overlay_previous == 2)
    overlay_previous = 0;
  if (overlay_previous == 0)
    {
      XtVaSetValues (wdgt, XmNlabelString,
		     tmp = XmStringCreateLocalized ("Overlay: OFF"), NULL);
      XmStringFree (tmp);
    }
  if (overlay_previous == 1)
    {
      XtVaSetValues (wdgt, XmNlabelString,
		     tmp = XmStringCreateLocalized ("Overlay: ON"), NULL);
      XmStringFree (tmp);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void expose_overlays (Widget wdgt, XtPointer clnt, 
				XmDrawingAreaCallbackStruct *call)
/*************************************************************/
/* EXPOSE_OVERLAYS					     */
/*Log:									*
 * T. Piper/SAIC	12/04	Added check on count			*
 ***********************************************************************/
{
XEvent *event;
/*---------------------------------------------------------------------*/
  event = call->event;
  if ( event->xexpose.count == 0 )
    XCopyArea (XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
	       gc, 0, 0, (Cardinal)xwdth, (Cardinal)xhght, 0, 0);

}

/*=====================================================================*/
/* ARGSUSED */
void
resize_callback (Widget wdgt, XtPointer clnt, XtPointer call)
/*************************************************************/
/* RESIZE_CALLBACK					     */
/*************************************************************/
{
  Dimension nwdth, nhght, xdim, ydim;
/*Dimension ndist;
  char st[80]; */
  int i, ier, npxms, curpxm;
  char winname[] = "skewt_canvas";
  int len =
    strlen (winname), ixsize, iysize, isxsiz, isysiz, ixo, iyo, ncurwn;


  XtVaGetValues (draw_reg, XmNwidth, &nwdth, XmNheight, &nhght, NULL);

  xdim = (Dimension) (nwdth - 355 - skv.tlx - 20);	/* Added 15 to 340 for better spacing */
  ydim = (Dimension) (nhght - skv.tly);

  /*if ( xdim < ydim ) used to constrain x/y for window
     ndist = xdim;
     else
     ndist = ydim;
     skv.brx = skv.bry = ndist;
     hov.brx = hov.bry = ndist; */


  skv.brx = xdim;
  skv.bry = ydim;
  hov.brx = xdim;
  hov.bry = ydim;

  xwdth = nwdth;
  xhght = nhght;


  xslwin (winname, &len, &ixsize, &iysize,
	  &isxsiz, &isysiz, &ixo, &iyo, &ncurwn, &ier);
  xqcpxm (&npxms, &curpxm);
  xscpxm (0, &ier);
  xclear (&ixsize, &iysize, &isxsiz, &isysiz, &ier);

  for (i = 0; i < npxms; i++)
    {
      if (i > 0)
	xsplot (&ier);		/* first pixmap already created in xclear */

      canvas = xqpxms (0, i);

      setcliprgn (1, 1, xwdth, xhght, draw_reg, gc);
      setcolor (0, draw_reg, gc);
      XFillRectangle (XtDisplay (draw_reg), canvas, gc, 0, 0, xwdth, xhght);

      /* ----- Parameter Area ----- */
      setcolor (4, draw_reg, gc);
      rectangle (1, skv.brx + 20, skv.tly, xwdth - 5, xhght - 5);
      setcolor (1, draw_reg, gc);
      rectangle (0, skv.brx + 20, skv.tly, xwdth - 5, xhght - 5);

      /* ----- Cursor Data Area ----- *--
         setcolor (3, draw_reg, gc);
         strcpy (st, "CURSOR DATA");
         outgtext (st,
         (((skv.brx + 20 + xwdth) / 2) - (getgtextextent (st) / 2)),
         skv.tly + 5);
         setcolor (0, draw_reg, gc);
         rectangle (1, skv.brx + 30, skv.tly + 22, xwdth - 15, 100);
         setcolor (1, draw_reg, gc);
         rectangle (0, skv.brx + 30, skv.tly + 22, xwdth - 15, 100); */

      sndgp = sndgs[i];
      if (mode == 1)
	{
	  draw_skewt ();	/* will draw graph background if no data yet */
	  if ((sndgp != NULL) && (sndgp->numlev > 0))
	    {
	      define_parcel ((short)current_parcel, user_level);
	      show_parcel ();
	    }
	}
      else
	draw_hodo ();

      if ((sndgp != NULL) && (sndgp->numlev > 0))
	show_page (pagenum);
    }

  xscpxm (curpxm, &ier);
  if (curpxm > 0)
    NxmChangePixmapData (curpxm, npxms);

}

/*=====================================================================*/
/* ARGSUSED */
void
position_cursor (Widget w, XtPointer clnt, XEvent *event)
/************************************************************************/
/* POSITION_CURSOR					    		*/
/*									*/
/* Handles "mouse button depressed" event				*/
/* S. Chiswell/Unidata	06/06	raob_mod is set as check for cases 	*/
/*				where button release occurs after	*/
/*				data selection window pops down.	*/
/************************************************************************/
{
  short i, yy;
  float pres, temp, dwpt, tcur, d1, d2;

/*---------------------------------------------------------------------*/

  /* ----- Determine if a button is pushed ----- */
  if (event->xbutton.button == 1)
    {
      if (!qc (i_temp (700.0F)))
	return;
      
      raob_mod = 1;

      if (mode == 1) 
        {
	  if (event->xbutton.y > skv.bry || event->xbutton.x > skv.brx ||
	      event->xbutton.y < skv.tly || event->xbutton.x < skv.tlx)
		return;

	  XDrawLine (XtDisplay (w), XtWindow (w), rbdata.gc,
		     rbdata.points[0].x,
		     rbdata.points[0].y,
		     (int) rbdata.points[1].x, (int) rbdata.points[1].y);
	  XDrawLine (XtDisplay (w), XtWindow (w), rbdata.gc,
		     rbdata.points[1].x,
		     rbdata.points[1].y,
		     (int) rbdata.points[2].x, (int) rbdata.points[2].y);


	  /* ----- Determine pres/temp coords of cursor ----- */
	  pres = pix_to_pres ((short) event->xbutton.y);
	  tcur = pix_to_temp ((short) event->xbutton.x,
			      (short) event->xbutton.y);

	  if (pres > sndgp->sndg[sfc ()].pres)
	    {
	      pres = sndgp->sndg[sfc ()].pres;
	    }

	  temp = i_temp (pres);
	  dwpt = i_dwpt (pres);
	  d1 = (float) fabs ((double) (tcur - temp));
	  d2 = (float) fabs ((double) (tcur - dwpt));

	  if (d1 < d2)
	    {

	      /* ----- Edit Temperatures ----- */
	      rbdata.itype = 0;
	      rbdata.i = i = grab_level (pres);
	      if (!qc (sndgp->sndg[i].temp))
		{
		  sndgp->sndg[i].temp = i_temp (sndgp->sndg[i].pres);
		}

	      /* ----- Move mouse to starting spot ----- */
	      rbdata.yy = yy = pres_to_pix (sndgp->sndg[i].pres);
	      XWarpPointer (XtDisplay (draw_reg), XtWindow (draw_reg),
			    XtWindow (draw_reg), (int) skv.tlx, (int) skv.tly,
			    (Cardinal) skv.brx + skv.tlx,
			    (Cardinal) skv.bry + skv.tly,
			    temp_to_pix (sndgp->sndg[i].temp,
					 sndgp->sndg[i].pres), yy);
	      rbdata.last_x = rbdata.start_x = event->xbutton.x;
	      rbdata.last_y = rbdata.start_y = event->xbutton.y;
	      rbdata.points[1].x =
		temp_to_pix (sndgp->sndg[i].temp, sndgp->sndg[i].pres);
	      rbdata.points[1].y = yy;

	      if (i == 0)
		{
		  rbdata.points[0].x = rbdata.points[1].x;
		  rbdata.points[0].y = rbdata.points[1].y;
		  rbdata.points[2].x =
		    temp_to_pix (sndgp->sndg[i + 1].temp,
				 sndgp->sndg[i + 1].pres);
		  rbdata.points[2].y = pres_to_pix (sndgp->sndg[i + 1].pres);
		  rbdata.ilev = 0;
		}
	      else if (i == sndgp->numlev - 1)
		{
		  rbdata.points[2].x = rbdata.points[1].x;
		  rbdata.points[2].y = rbdata.points[1].y;
		  rbdata.points[0].x =
		    temp_to_pix (sndgp->sndg[i - 1].temp,
				 sndgp->sndg[i - 1].pres);
		  rbdata.points[0].y = pres_to_pix (sndgp->sndg[i - 1].pres);
		  rbdata.ilev = 1;
		}
	      else
		{
		  rbdata.points[0].x =
		    temp_to_pix (sndgp->sndg[i - 1].temp,
				 sndgp->sndg[i - 1].pres);
		  rbdata.points[0].y = pres_to_pix (sndgp->sndg[i - 1].pres);
		  rbdata.points[2].x =
		    temp_to_pix (sndgp->sndg[i + 1].temp,
				 sndgp->sndg[i + 1].pres);
		  rbdata.points[2].y = pres_to_pix (sndgp->sndg[i + 1].pres);
		  rbdata.ilev = 2;
		}
	    }
	  else
	    {
	      /* ----- Edit Dew Points ----- */
	      rbdata.itype = 1;
	      rbdata.i = i = grab_level (pres);
	      if (!qc (sndgp->sndg[i].dwpt))
		{
		  sndgp->sndg[i].dwpt = i_dwpt (sndgp->sndg[i].pres);
		}

	      /* ----- Move mouse to starting spot ----- */
	      rbdata.yy = yy = pres_to_pix (sndgp->sndg[i].pres);
	      XWarpPointer (XtDisplay (draw_reg), XtWindow (draw_reg),
			    XtWindow (draw_reg), (int) skv.tlx, (int) skv.tly,
			    (Cardinal) skv.brx + skv.tlx,
			    (Cardinal) skv.bry + skv.tly,
			    temp_to_pix (sndgp->sndg[i].dwpt,
					 sndgp->sndg[i].pres), yy);
	      rbdata.last_x = rbdata.start_x = event->xbutton.x;
	      rbdata.last_y = rbdata.start_y = event->xbutton.y;
	      rbdata.points[1].x =
		temp_to_pix (sndgp->sndg[i].dwpt, sndgp->sndg[i].pres);
	      rbdata.points[1].y = yy;
	      if (i == 0)
		{
		  rbdata.points[0].x = rbdata.points[1].x;
		  rbdata.points[0].y = rbdata.points[1].y;
		  rbdata.points[2].x =
		    temp_to_pix (sndgp->sndg[i + 1].dwpt,
				 sndgp->sndg[i + 1].pres);
		  rbdata.points[2].y = pres_to_pix (sndgp->sndg[i + 1].pres);
		  rbdata.ilev = 0;
		}
	      else if (i == sndgp->numlev - 1)
		{
		  rbdata.points[2].x = rbdata.points[1].x;
		  rbdata.points[2].y = rbdata.points[1].y;
		  rbdata.points[0].x =
		    temp_to_pix (sndgp->sndg[i - 1].dwpt,
				 sndgp->sndg[i - 1].pres);
		  rbdata.points[0].y = pres_to_pix (sndgp->sndg[i - 1].pres);
		  rbdata.ilev = 1;
		}
	      else
		{
		  rbdata.points[0].x =
		    temp_to_pix (sndgp->sndg[i - 1].dwpt,
				 sndgp->sndg[i - 1].pres);
		  rbdata.points[0].y = pres_to_pix (sndgp->sndg[i - 1].pres);
		  rbdata.points[2].x =
		    temp_to_pix (sndgp->sndg[i + 1].dwpt,
				 sndgp->sndg[i + 1].pres);
		  rbdata.points[2].y = pres_to_pix (sndgp->sndg[i + 1].pres);
		  rbdata.ilev = 2;
		}
	    }

	}
      else
	{
	  pix_to_hodo ((short) event->xbutton.x, (short) event->xbutton.y,
		       &sndgp->st_dir, &sndgp->st_spd);
	  redraw_graph (mode);
	  show_page (pagenum);
	  XCopyArea (XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
		     gc, 0, 0, (Cardinal)xwdth, (Cardinal)xhght, 0, 0);
	  hodo_cursor_data ((short) event->xbutton.x,
			    (short) event->xbutton.y);
	}

      XFlush (XtDisplay (draw_reg));

    }
  else if (event->xbutton.button == 3)
    {
      switch (sounding_type)
	{
	case 0:
	  XtManageChild (load_sharp);
	  break;

	case 1:
	  observed_sounding_cb (w);
	  break;

	case 2:
	  model_sounding_cb (w);
	  break;

	case 3:
	  pfc_sounding_cb (w);
	  break;

	case 4:
	  show_acars_info (w);
	  break;
	}
    }
}

/*=====================================================================*/
/* ARGSUSED */
void
pointer_update (Widget w, XtPointer clnt, XEvent *event)
/*************************************************************/
/* POINTER_UPDATE					     */
/* 							     */
/* Handles "mouse moved w/ no buttons" event		     */
/*************************************************************/
{

  /* ----- Update Cursor Data when mouse is moved ----- */
  if (mode == 1)
    {
      if ((event->xbutton.x < skv.brx) && (event->xbutton.y < skv.bry))
	skewt_cursor_data ((short) event->xbutton.x,
			   (short) event->xbutton.y);
    }
  else
    {
      if ((event->xbutton.x < hov.brx) && (event->xbutton.y < hov.bry))
	hodo_cursor_data ((short) event->xbutton.x, (short) event->xbutton.y);
    }
  XFlush (XtDisplay (cursor_out));
}

/*=====================================================================*/
/* ARGSUSED */
void
update_pointer (Widget w, XtPointer clnt, XEvent *event)
/************************************************************************
 * UPDATE_POINTER					     		*
 *							     		*
 * Handles "mouse moved while button #1 was depressed" event 		*
 *                                                                      *
 **                                                                     *                                      * Log:                                                                 * 
 * T. Piper/SAIC        03/05   Added check of button location          * 
 ***********************************************************************/ 
{
  if (!qc (i_temp (700.0F)))
    return;

  if (mode == 1)
    {
      if ( event->xbutton.y > skv.bry || event->xbutton.x > skv.brx ||
  	   event->xbutton.y < skv.tly || event->xbutton.x < skv.tlx) {
	    return;
    }
      XDrawLine (XtDisplay (w), XtWindow (w), rbdata.gc,
		 rbdata.points[0].x, rbdata.points[0].y,
		 (int) rbdata.points[1].x, (int) rbdata.points[1].y);
      XDrawLine (XtDisplay (w), XtWindow (w), rbdata.gc,
		 rbdata.points[1].x, rbdata.points[1].y,
		 (int) rbdata.points[2].x, (int) rbdata.points[2].y);

      rbdata.last_x = event->xbutton.x;
      rbdata.last_y = event->xbutton.y;
      rbdata.points[1].x = event->xbutton.x;

      XDrawLine (XtDisplay (w), XtWindow (w), rbdata.gc,
		 rbdata.points[0].x, rbdata.points[0].y,
		 (int) rbdata.points[1].x, (int) rbdata.points[1].y);
      XDrawLine (XtDisplay (w), XtWindow (w), rbdata.gc,
		 rbdata.points[1].x, rbdata.points[1].y,
		 (int) rbdata.points[2].x, (int) rbdata.points[2].y);

      set_font (2);
      skewt_cursor_data ((short) event->xbutton.x,
			 (short) rbdata.points[1].y);

    }
  else
    {
      pix_to_hodo ((short) event->xbutton.x, (short) event->xbutton.y,
		   &sndgp->st_dir, &sndgp->st_spd);

      /* ----- Display Hodograph Inset ----- */
      draw_hoinset ();

      set_font (2);
      hodo_cursor_data ((short) event->xbutton.x, (short) event->xbutton.y);
    }

  XFlush (XtDisplay (draw_reg));
}

/*=====================================================================*/
/* ARGSUSED */
void redraw_sounding (Widget w, XtPointer clnt, XEvent *event)
/************************************************************************
 * REDRAW_SOUNDING					    		*
 *									*
 **									*
 * Log:									*
 * T. Piper/SAIC	03/05	Added check of button location		*
 * S. Chiswell/Unidata	06/06	Added check for button to have been	*
 *				depressed in position_cursor		*
 ***********************************************************************/
{
  float chg1;
/*---------------------------------------------------------------------*/

  if (!qc (i_temp (700.0F)))
    return;

  if ( ( event->xbutton.button != 1) || ( ! raob_mod ) )
    return;

  raob_mod = 0;

  /* Redraw previous line */

  if (mode == 1)
    {
      if (event->xbutton.y > skv.bry || event->xbutton.x > skv.brx ||
	  event->xbutton.y < skv.tly || event->xbutton.x < skv.tlx) {
	    return;
      }

      XDrawLine (XtDisplay (w), XtWindow (w), rbdata.gc,
		 rbdata.points[0].x,
		 rbdata.points[0].y,
		 (int) rbdata.points[1].x, (int) rbdata.points[1].y);
      XDrawLine (XtDisplay (w), XtWindow (w), rbdata.gc,
		 rbdata.points[1].x,
		 rbdata.points[1].y,
		 (int) rbdata.points[2].x, (int) rbdata.points[2].y);

      if (!rbdata.itype)
	{
	  chg1 = pix_to_temp ((short) event->xbutton.x, rbdata.yy);
	  if (chg1 < i_dwpt (sndgp->sndg[rbdata.i].pres))
	    {
	      chg1 = i_dwpt (sndgp->sndg[rbdata.i].pres);
	    }
	  sndgp->sndg[rbdata.i].temp = chg1;
	  redraw_graph (mode);
	  define_parcel ((short)current_parcel, user_level);
	  show_parcel ();
	  show_page (pagenum);
	}
      else
	{
	  chg1 = pix_to_temp ((short) event->xbutton.x, rbdata.yy);
	  if (chg1 > i_temp (sndgp->sndg[rbdata.i].pres))
	    {
	      chg1 = i_temp (sndgp->sndg[rbdata.i].pres);
	    }
	  sndgp->sndg[rbdata.i].dwpt = chg1;
	  redraw_graph (mode);
	  define_parcel ((short)current_parcel, user_level);
	  show_parcel ();
	  show_page (pagenum);
	}
      rbdata.points[0].x = rbdata.points[1].x = rbdata.points[2].x = 0;
      rbdata.points[0].y = rbdata.points[1].y = rbdata.points[2].y = 0;
      XCopyArea (XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
		 gc, 0, 0, (Cardinal)xwdth, (Cardinal)xhght, 0, 0);
    }
  else
    {
      pix_to_hodo ((short) event->xbutton.x, (short) event->xbutton.y,
		   &sndgp->st_dir, &sndgp->st_spd);
      redraw_graph (mode);
      show_page (pagenum);
      XCopyArea (XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
		 gc, 0, 0, (Cardinal)xwdth, (Cardinal)xhght, 0, 0);
      hodo_cursor_data ((short) event->xbutton.x, (short) event->xbutton.y);
      XFlush (XtDisplay (draw_reg));
    }

}

/*=====================================================================*/
/* ARGSUSED */
void tog_graph (Widget wdgt, XtPointer clnt, XtPointer call)
/************************************************************************
* TOG_GRAPH								*
**									*
* T. Piper/SAIC		 1/02	Freed XmStringCreateLocalized		*
* T. Piper/SAIC		11/06	Changed winname from char* to char array*
************************************************************************/
{
  XmString tmp;

  int ii, ier, npxms, curpxm;
  char winname[] = "skewt_canvas";
  int len =
    strlen (winname), ixsize, iysize, isxsiz, isysiz, ixo, iyo, ncurwn;

/*---------------------------------------------------------------------*/

  if (sndgp == NULL)
    return;

  xslwin (winname, &len, &ixsize, &iysize,
	  &isxsiz, &isysiz, &ixo, &iyo, &ncurwn, &ier);
  xqcpxm (&npxms, &curpxm);
  xscpxm (0, &ier);
  xclear (&ixsize, &iysize, &isxsiz, &isysiz, &ier);

  xstanm (&ier);

  for (ii = 0; ii < npxms; ii++)
    {
      if (ii > 0)
	xsplot (&ier);		/* first pixmap already created in xclear */

      canvas = xqpxms (0, ii);

      sndgp = sndgs[ii];

      if (sndgp != NULL)
	switch_modes (mode);
    }
  xenanm (&ier);

  if (mode == 2)
    mode = 1;
  else
    mode = 2;

  xscpxm (curpxm, &ier);
  if (curpxm > 0)
    NxmChangePixmapData (curpxm, npxms);

  if (mode == 1)
    {
      tmp = XmStringCreateLocalized ("Hodograph");
      XtVaSetValues (wdgt, XmNlabelString, tmp, NULL);
      XmStringFree (tmp);
      XtVaSetValues (parcel_button, XmNsensitive, True, NULL);
    }
  else
    {
      tmp = XmStringCreateLocalized ("Skewt");
      XtVaSetValues (wdgt, XmNlabelString, tmp, NULL);
      XmStringFree (tmp);
      XtVaSetValues (parcel_button, XmNsensitive, False, NULL);
    }
  XCopyArea (XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
	     gc, 0, 0, (Cardinal)xwdth, (Cardinal)xhght, 0, 0);
}

/*=====================================================================*/
/* ARGSUSED */
void page_next (Widget wdgt, XtPointer clnt, XtPointer call)
/************************************************************************
 * PAGE_NEXT						    		*
 *                                                           		*
 * Log:									*
 * J. Morgan/OPC	 4/05	Increased page number to 5		*
 * T. Piper/SAIC	11/06	Changed winname from char* to char array*
 ***********************************************************************/
{
  char winname[] = "skewt_canvas";
  int curpxm, ier, ii, isxsiz, isysiz, ixo, ixsize, iyo, iysize,
			 len = strlen (winname), ncurwn, npxms;
/*---------------------------------------------------------------------*/
  if (!qc(i_temp(700.0F))) return;
  pagenum += 1;
  if (pagenum == 6)
    pagenum = mode;

  xslwin (winname, &len, &ixsize, &iysize,
	  &isxsiz, &isysiz, &ixo, &iyo, &ncurwn, &ier);

  xqcpxm (&npxms, &curpxm);

  for (ii = 0; ii < npxms; ii++) {
      if ((sndgs[ii] != NULL) && (sndgs[ii]->numlev > 0) && ii != curpxm) {
	  xscpxm (ii, &ier);
	  sndgp = sndgs[ii];
	  canvas = xqpxms (0, ii);
	  show_page (pagenum);
      }
  }

  xscpxm (curpxm, &ier);
  sndgp = sndgs[curpxm];
  canvas = xqpxms (0, curpxm);
  show_page (pagenum);

  XCopyArea (XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
	     gc, 0, 0, (Cardinal)xwdth, (Cardinal)xhght, 0, 0);
}

/*=====================================================================*/

char * itoa (int value, char *st, int radx)
/*************************************************************/
/* ITOA                                                      */
/*************************************************************/
{
  sprintf (st, "%d", value);
  return st;
}

/*=====================================================================*/

void outcursor (char *st, int x, int y)
/*************************************************************/
/* OUTCURSOR                                                 */
/*************************************************************/
{
  y = y + font_struct->ascent;
  XDrawImageString (XtDisplay (cursor_out), XtWindow (cursor_out), gc_cursor,
		    x, y, st, (int) strlen (st));
}
/*=====================================================================*/

void outtext (char *st, int x, int y)
/*************************************************************/
/* OUTTEXT                                                   */
/*************************************************************/
{
  y = y + font_struct->ascent;
  XDrawImageString (XtDisplay (draw_reg), XtWindow (draw_reg), gc,
		    x, y, st, (int) strlen (st));
}

/*=====================================================================*/

void outgtext (char *st, int x, int y)
/*************************************************************/
/* OUTGTEXT                                                  */
/*************************************************************/
{
  y = y + font_struct->ascent;
  XDrawString (XtDisplay (draw_reg), (canvas), gc, x, y, st,
	       (int) strlen (st));
}

/*=====================================================================*/
/* ARGSUSED */
void Toggle_Callback (Widget w, long which, XtPointer call)
/*************************************************************/
/* TOGGLE_CALLBACK					    */
/*									*
 * Log:									*
 * T. Piper/SAIC	12/04	Toggle for all pixmaps			*
 ***********************************************************************/
{
  char winname[] = "skewt_canvas";
  int ii, ier, npxms, curpxm, ixsize, iysize, isxsiz, isysiz, ixo, iyo, ncurwn;
  int len = strlen (winname);
  static int lastpick = (-1);

/*---------------------------------------------------------------------*/

  XtUnmanageChild (XtParent (XtParent (XtParent (XtParent (w)))));
  if (which == 5)
    return;
  if ((lastpick >= 0) && (lastpick != which))
    XtVaSetValues (rbutton[lastpick], XmNset, False, NULL);
  else
    XtVaSetValues (rbutton[which], XmNset, True, NULL);

  pagenum = 1;
  current_parcel = which + 1;
  xslwin (winname, &len, &ixsize, &iysize,
		&isxsiz, &isysiz, &ixo, &iyo, &ncurwn, &ier);
  xqcpxm (&npxms, &curpxm);
  for (ii = 0; ii < npxms; ii++) {
    if ((sndgs[ii] != NULL) && (sndgs[ii]->numlev > 0) && ii != curpxm) {
	xscpxm (ii, &ier);
        sndgp = sndgs[ii];
	canvas = xqpxms (0, ii);
        define_parcel ((short)current_parcel, user_level);
	show_parcel ();
	show_page (pagenum);
	}
  }
  xscpxm (curpxm, &ier);
  sndgp = sndgs[curpxm];
  canvas = xqpxms (0, curpxm);
  define_parcel ((short)current_parcel, user_level);
  show_parcel ();
  show_page (pagenum);
  XCopyArea (XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
             gc, 0, 0, (Cardinal)xwdth, (Cardinal)xhght, 0, 0);
  lastpick = which;
}

/*=====================================================================*/

GC xs_create_xor_gc (Widget w, char *color)
/*************************************************************/
/* XS_CREATE_XOR_GC					    */
/*************************************************************/
{
  XGCValues values;
  GC gc;
/*---------------------------------------------------------------------*/

  XtVaGetValues (w, XtNbackground, &values.background, NULL);

/*
 * Set the fg to the XOR of the fg and bg, so if it is
 * XOR'ed with the bg, the result will be fg and vice-versa.
 * This effectively achieves inverse video for the line.
 */
  values.foreground = pixels[1];
  values.background = pixels[0];
  values.foreground = values.foreground ^ values.background;

/* Set rubber band gc to use XOR mode and draw solid line */
  values.line_style = LineSolid;
  values.line_width = 4;
  values.function = GXxor;

  gc = XtGetGC (w, GCForeground | GCBackground | GCFunction | GCLineStyle,
		&values);

  return gc;
}

/*=====================================================================*/
/* ARGSUSED */
void file_cb (Widget wdgt, XtPointer clnt, XtPointer call)
{
  int item_no = (long)clnt;
/*---------------------------------------------------------------------*/

  switch (item_no)
    {
    case 0:
      if (sndgp == NULL)
	return;
      if (sndgp->numlev > 0)
	print_selection (wdgt);
      break;
    case 1:
      exit (0);
      break;
    default:
      printf ("Unknown callback\n");
    }
}

/*=====================================================================*/
/* ARGSUSED */
void option_cb (Widget wdgt, XtPointer clnt, XtPointer call)
{
  int item_no = (long)clnt;
/*---------------------------------------------------------------------*/

  switch (item_no)
    {
    case 0:
      XtManageChild (dwell_panel);
      break;
      /*case 1:
         NxmBusy_setStopBtn(1);
         break;
         case 2:
         NxmBusy_setStopBtn(0);
         break; */
    }
}

/*=====================================================================*/
/* ARGSUSED */
void _mapzoom_cb (Widget mapCanvW, XtEventHandler pickstn_func,
			 mapstruct *mapdata, void (*cb_func)(void))
{
  int ityp, np, i;
  float xpts[2], ypts[2], xdev[2], ydev[2];
  int iret;
/*---------------------------------------------------------------------*/
/*
 * Change mouse event handling for zooming
 */
  if (pickstn_func != NULL)
    XtRemoveEventHandler (mapCanvW, ButtonPressMask, FALSE,
			  (XtEventHandler) pickstn_func, NULL);

/*
 * change to the zoom cursor
 */
  NxmCursor_setCursor (mapCanvW, CURS_POINT_SELECT);
  ityp = 13;
  np = 2;
  for (i = 0; i < np; i++)
    xpts[i] = ypts[i] = 0.0F;
  ggtpnt (sys_D, &ityp, xdev, ydev, &iret, strlen (sys_D));

  NxmCursor_setCursor (mapCanvW, CURS_DEFAULT);

/*
 * Restore mouse to be selection status
 */
  XSelectInput (XtDisplay(mapCanvW), XtWindow (mapCanvW),
		ButtonPressMask | ButtonReleaseMask | ExposureMask);

  if (pickstn_func != NULL)
    XtAddEventHandler (mapCanvW, ButtonPressMask, FALSE,
		       (XtEventHandler) pickstn_func, NULL);

/*
 * check if the box is big enough
 */
  if (fabs ((double) (xdev[0] - xdev[1])) > 20. &&
      fabs ((double) (ydev[0] - ydev[1])) > 20.)
    {

      gtrans (sys_D, sys_M, &np, xdev, ydev, xpts, ypts,
	      &iret, strlen (sys_D), strlen (sys_M));

      if (iret == 0)
	{
	  for (i = 0; i < np; i++)
	    {
	      mapdata->mapb.x[i] = xpts[i];
	      mapdata->mapb.y[i] = ypts[i];
	    };
	  mapdata->zoomflg = 1;
	}
    }
  cb_func ();
}

/*=====================================================================*/
/* ARGSUSED */
void load_cb (Widget wdgt, XtPointer clnt, XtPointer call)
/****************************************************************
 * LOAD_CB	 						*
 **								*
 * T. Piper/SAIC	2/02	Freed title			*
 * T. Piper/SAIC	12/04	Stop looping during load	*
 ***************************************************************/
{
  int item_no = (long)clnt;
  XmString title;

/*---------------------------------------------------------------------*/

  if ( NxmQueryAnimationStatus() ) NxmStopAnimation();
  sounding_type = item_no;
  if (item_no == 0)
    {
      if (!load_sharp)
	{
	  title = XmStringCreateLocalized ("Archive File Selection");
	  load_sharp = XmCreateFileSelectionDialog (toplevel,
						    "sharp_sel", NULL, 0);
	  XtAddCallback (load_sharp, XmNokCallback, sharp_load, NULL);
	  XtAddCallback (load_sharp, XmNcancelCallback,
			 (XtCallbackProc) XtUnmanageChild, NULL);
	  XtAddCallback (load_sharp, XmNokCallback,
			 (XtCallbackProc) XtUnmanageChild, NULL);
	  XtVaSetValues (load_sharp, XmNdialogTitle, title, NULL);
	  XmStringFree (title);
	}
      XtManageChild (load_sharp);
    }
  else if (item_no == 1)
    {
      observed_sounding_cb (wdgt);
    }
  else if (item_no == 2)
    {
      model_sounding_cb (wdgt);
    }
  else if (item_no == 3)
    {
      pfc_sounding_cb (wdgt);
    }
  else if (item_no == 4)
    {
      show_acars_info (wdgt);
    }
  else
    {
      show_profiler_info (wdgt);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void sharp_load (Widget wdgt, XtPointer clnt, XtPointer call)
/*************************************************************/
/* SHARP_LOAD                                                */
/*************************************************************/
{
  float ix1, ix2;

  char *file = NULL, filename[200], winname[] = "skewt_canvas";
  int i, npxms, curpxm;
  int ier, len =
    strlen (winname), ixsize, iysize, isxsiz, isysiz, ixo, iyo, ncurwn;
  XmFileSelectionBoxCallbackStruct *cbs =
    (XmFileSelectionBoxCallbackStruct *) call;

  if (cbs)
    {
      file = XmStringUnparse (cbs->value, NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
      strcpy (filename, config.filename);
      strcpy (config.filename, file);
      XtFree (file);

      xslwin (winname, &len, &ixsize, &iysize, &isxsiz, &isysiz, &ixo, &iyo,
	      &ncurwn, &ier);
      xstanm (&ier);
      xsplot (&ier);
      xqcpxm (&npxms, &curpxm);
      canvas = xqpxms (0, curpxm);
      NxmChangePixmapData (curpxm, npxms);
      if (sndgs[curpxm] == NULL)
	{
	  sndgs[curpxm] =
	    (struct sndg_struct *) malloc (sizeof (struct sndg_struct));
	  sndgs[curpxm]->numlev = 0;
	}

      sndgp = sndgs[curpxm];

      if (sndgp->numlev > 0)
	copy_sndg ();
      else
	sndgp->ovrlev = 0;

      if (get_sndg ())
	{
	  printf ("\n Error reading file %s", config.filename);
	  strcpy (config.filename, filename);
	  return;
	}

      save_origsndg ();

      mode = 1;
      draw_skewt ();
      define_parcel ((short)current_parcel, user_level);
      show_parcel ();
      pagenum = 1;
      /*write_file(); */
      mean_wind (sndgp->sndg[sfc ()].pres, i_pres (msl (6000.0F)), &ix1, &ix2,
		 &sndgp->st_dir, &sndgp->st_spd);
      sndgp->st_spd *= .75F;
      sndgp->st_dir += 30.0F;
      if (sndgp->st_dir > 360.0F)
	sndgp->st_dir -= 360.0F;
      printf ("storm motion = %f/%f\n", sndgp->st_dir, sndgp->st_spd);

      show_page (pagenum);
      xenanm (&ier);
      i = curpxm + 1;
      while ((i < MAX_PIXMAP) && (sndgs[i] != NULL))
	{
	  sndgs[i]->numlev = 0;
	  i++;
	}
      NxmLoopbuttonSensitive (False);
    }
}

/*=====================================================================*/

void Load_stationlist (void)
/*************************************************************/
/* LOAD_STATIONLIST                                          */
/*                                                           */
/* T. Lee/SAIC   10/02   Used LLSTFL in station dimension    */
/* R. Tian/SAIC  02/03   Added Cursor Points mark            */
/* D.W.Plummer/NCEP 3/03 modify ctb_rdcpf calling sequence   */
/*************************************************************/
{
  char station_tbl[12], statlist[LLSTFL][18];
  int nsta, ncp, i;
  int ncolor, mrktyp, mrkwid, pltval, iposn, jcolr;
  int iret;
  float sta_lat[LLSTFL], sta_lon[LLSTFL], sizmrk;
  float cpf_lat[LLSTFL], cpf_lon[LLSTFL];
  static char map_winname[] = "mapobs";

  switch (obs_map.mapindx)
    {
    case 0:
      sprintf (station_tbl, "US");
      break;

    case 1:
      sprintf (station_tbl, "CN");
      break;

    case 2:
      sprintf (station_tbl, "MX");
      break;

    default:
      sprintf (station_tbl, "DSET");
    }


  nsharp_draw_map (map_winname, &obs_map, &iret);

  if (gemsoundfile[0] != '\0' && gemsoundtime[0] != '\0' &&
      station_tbl[0] != '\0')
    {

      ncolor = 1;
      mrktyp = 6;
      sizmrk = 1.0F;
      mrkwid = 2;
      pltval = G_FALSE;
      iposn = 0;
      jcolr = 2;


      get_gem_stns (gemsoundfile, station_tbl, gemsoundtime,
		    statlist, &nsta, sta_lat, sta_lon,
		    strlen (gemsoundfile), strlen (station_tbl),
		    strlen (gemsoundtime), 18);

      map_mark (&nsta, sta_lat, sta_lon, NULL, &ncolor, NULL,
		&jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval, &iposn, &iret);

      ctb_rdcpf ("nmap2.cpf", &ncp, cpf_lat, cpf_lon, &iret);
      if (iret == 0 && ncp > 0)
	{
	  ncolor = 1;
	  mrktyp = 5;
	  sizmrk = 2.0F;
	  mrkwid = 2;
	  pltval = G_FALSE;
	  iposn = 0;
	  jcolr = 5;

	  map_mark (&ncp, cpf_lat, cpf_lon, NULL, &ncolor, NULL,
		    &jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval,
		    &iposn, &iret);
	}

      stnList.nstn = nsta;
      for (i = 0; i < nsta; i++)
	{
	  statlist[i][17] = 0;
	  stnList.lat[i] = sta_lat[i];
	  stnList.lon[i] = sta_lon[i];
	  strcpy (stnList.stnName[i], statlist[i]);
	}
    }
  else
    {
      stnList.nstn = 0;
    }
}

/*=====================================================================*/

void get_listitem (char *srchstr, int findex, char sep, char *outstr, int *iret)
{
  int ipos;
  char *spos, *epos;

  outstr[0] = '\0';
  *iret = 0;

  if (findex > 0)
    cst_nocc (srchstr, sep, findex, 0, &ipos, iret);
  else
    ipos = 0;

  if (*iret != 0)
    return;

  if (ipos != 0)
    ipos++;
  spos = srchstr + ipos;
  epos = strchr (spos, sep);
  if (epos == NULL)
    strcpy (outstr, spos);
  else
    strncat (outstr, spos, epos - spos);
}

/*=====================================================================*/

void Load_gem_sounding (Widget _mypane, Widget wid, char *snd_station, char *stid)
/*************************************************************/
/* LOAD_GEM_SOUNDING                                         */
/*************************************************************/
{
  int ier;
  float ix1, ix2;
  XtPointer userdata;
  char sndtime[20], sndfile[200];

  if ((wid == NULL) || (snd_station == NULL) || (snd_station[0] == '\0'))
    return;

  XtVaGetValues (wid, XmNuserData, &userdata, NULL);

  if (userdata != NULL)
    {
      int i, npxms, curpxm;
      char winname[] = "skewt_canvas";
      int len =
	strlen (winname), ixsize, iysize, isxsiz, isysiz, ixo, iyo, ncurwn;
      char *choice;
      struct obs_file_times *olist = (struct obs_file_times *) userdata;
      XmStringTable item_list;

      XtVaGetValues (wid, XmNselectedItems, &item_list, NULL);

      xslwin (winname, &len, &ixsize, &iysize,
	      &isxsiz, &isysiz, &ixo, &iyo, &ncurwn, &ier);
      xstanm (&ier);

      _transferInterrupt = 0;
      NxmBusy_invoke (draw_reg, &_transferInterrupt);
      for (i = olist->nitems - 1; i >= 0 && !_transferInterrupt; i--)
	{
	  int findex;
	  static char sep = ';';

	  sprintf (sndfile, "%s/", olist->path);
	  findex = olist->posindex[olist->selected_items[i] - 1];
	  get_listitem (olist->filstr, findex, sep,
			sndfile + strlen (sndfile), &ier);

	  choice = XmStringUnparse (item_list[i], NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	  strcpy (sndtime, choice);
	  XtFree (choice);

	  xsplot (&ier);
	  xqcpxm (&npxms, &curpxm);
	  canvas = xqpxms (0, curpxm);
	  NxmChangePixmapData (curpxm, npxms);

	  if (sndgs[curpxm] == NULL)
	    {
	      sndgs[curpxm] =
		(struct sndg_struct *) malloc (sizeof (struct sndg_struct));
	      sndgs[curpxm]->numlev = 0;
	    }

	  sndgp = sndgs[curpxm];

	  if (sndgp->numlev > 0)
	    copy_sndg ();
	  else
	    sndgp->ovrlev = 0;


	  get_gem_snd (sndfile, sndtime, snd_station,
		       (float **) sndgs[curpxm]->sndg, &sndgp->numlev,
		       strlen (sndfile), strlen (sndtime),
		       strlen (snd_station));

	  if (strlen (stid) > (size_t)2)
	    sprintf (raobtitle, " %s   %s ", stid, sndtime);
	  else
	    sprintf (raobtitle, " %s   %s ", snd_station, sndtime);
	  strcpy (sndgs[curpxm]->title, raobtitle);

	  strcpy (raob_type, "RAOB");
	  /*write_file();                */
	  xtnd_sndg ();
	  save_origsndg ();
	  if (sndgp->numlev > 2 && sndgp->sndg[0].pres > 100.0F)
	    {
	      pagenum = 1;
	      define_parcel ((short)current_parcel, user_level);
	      mean_wind (sndgp->sndg[sfc ()].pres, i_pres (msl (6000.0F)),
			 &ix1, &ix2, &sndgp->st_dir, &sndgp->st_spd);
	      sndgp->st_spd *= .75F;
	      sndgp->st_dir += 30.0F;
	      if (sndgp->st_dir > 360.0F)
		sndgp->st_dir -= 360.0F;
	      show_page (pagenum);
	    }
	  mode = 1;

/*
 *  clean_uvvs( sndgp ); this is done ins the data reading routine
 */
	  draw_skewt ();
	  show_parcel ();
	  NxmBusy_checkStopBtn ();
	}
      xenanm (&ier);

      if (!_transferInterrupt)
	NxmBusy_animateFinish ();
      _transferInterrupt = 0;

      i = curpxm + 1;
      while ((i < MAX_PIXMAP) && (sndgs[i] != NULL))
	{
	  sndgs[i]->numlev = 0;
	  i++;
	}
      if (olist->nitems > 1)
	NxmLoopbuttonSensitive (True);
      else
	NxmLoopbuttonSensitive (False);
      XtUnmanageChild (_mypane);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void popdown_cb (Widget wdgt, XtPointer clnt, XtPointer call)
/*************************************************************/
/* POPDOWN_CB                                                */
/*************************************************************/
{
    Widget wid = (Widget)clnt;
    XtUnmanageChild (wid);
}

/*=====================================================================*/

void gem_loop ( void )
/************************************************************************
 **									*
 * Log:									*
 * T. Piper/SAIC	03/05	Added update_text_values()		*
 ***********************************************************************/
{
  int ier;
  char winname[] = "skewt_canvas";
  int len =
    strlen (winname), ixsize, iysize, isxsiz, isysiz, ixo, iyo, ncurwn;
  int npxms, curpxm;

/*---------------------------------------------------------------------*/

  xslwin (winname, &len, &ixsize, &iysize,
	  &isxsiz, &isysiz, &ixo, &iyo, &ncurwn, &ier);

  xqcpxm (&npxms, &curpxm);

  if (NXMpixmapData.current == 0)
    {
      if (NXManimationFlags.loopfrwdbkwd)
	{
	  if (npxms > 2)
	    NXMpixmapData.current = 2;
	  else
	    NXMpixmapData.current = 1;
	}
      else if (curpxm == 1)
	NXMpixmapData.current = npxms - 1;
      else
	NXMpixmapData.current = 1;
    }

  xscpxm (NXMpixmapData.current, &ier);
  canvas = xqpxms (0, NXMpixmapData.current);	/* update global canvas pointer to current pixmap */

  XCopyArea (XtDisplay(draw_reg), canvas, XtWindow(draw_reg), 
	     gc, 0, 0, (Cardinal)xwdth, (Cardinal)xhght, 0, 0);

/* set current data structure pointer */
  if (sndgs[NXMpixmapData.current] == NULL)
    printf ("Hmmm Data pointer is null %d\n", NXMpixmapData.current);
  else
    sndgp = sndgs[NXMpixmapData.current];

  update_text_values();

  XFlush (XtDisplay(draw_reg));

}

/*=====================================================================*/
/* ARGSUSED */
void mdl_pointer (Widget w, XtPointer clnt, XEvent *event)
/*************************************************************/
/* MDL_POINTER                                               */
/*                                                           */
/* Handles "mouse moved w/ no buttons" event                 */
/* R. Tian/SAIC		02/03	Added CPF check		     */
/*************************************************************/
{
  char cursor_pos[20];
  float xdev, ydev, lat, lon;
  int iret, np = 1;
  static char sysin[] = "D";
  static char sysout[] = "M";
  XmString str;
/*---------------------------------------------------------------------*/

  xdev = (float) event->xbutton.x;
  ydev = (float) event->xbutton.y;

  gtrans (sysin, sysout, &np, &xdev, &ydev, &lat, &lon,
	  &iret, strlen (sysin), strlen (sysout));

  if (iret == 0)
    {
      mdl_cursor_mkstr (lat, lon, cursor_pos);
      if (strncmp (cursor_pos, "CPF", 3) != 0)
	{
	  str = XmStringCreateLocalized (cursor_pos);
	  XtVaSetValues (mdl_cursor_text, XmNlabelString, str, NULL);
	  XmStringFree (str);
	}
    }
  else
    {
      sprintf (cursor_pos, "------;-------");
      str = XmStringCreateLocalized (cursor_pos);
      XtVaSetValues (mdl_cursor_text, XmNlabelString, str, NULL);
      XmStringFree (str);
    }

  XFlush (XtDisplay (mdl_map));
}

/*=====================================================================*/

void mdl_cursor_mkstr (float lat, float lon, char *cpos)
/*************************************************************/
/* R. Tian/SAIC		02/03	Added CPF check		     */
/*************************************************************/
{
  int cfmt;
  char stn[20];
  float dist, dir;
  int iret;
/*---------------------------------------------------------------------*/
  mdl_cursor_fmt_val (0, 0, &cfmt);

  switch (cfmt)
    {
    case 0:
      sprintf (cpos, "%4.2f;%4.2f", lat, lon);
      break;
    case 1:
      clo_tdirect ("SFSTNS", lat, lon, stn, &dist, &dir, &iret);
      sprintf (cpos, "%s", stn);
      break;
    case 2:
      sprintf (cpos, "%s", "CPF");
      break;
    }
}

/*=====================================================================*/

void load_cpf (void)
/*************************************************************/
/* LOAD_CPF                                                  */
/*                                                           */
/* Load Cursor Point lat/lon from a file for model sounding  */
/*                                                           */
/* R. Tian/SAIC		Coding                		     */
/* D.W.Plummer/NCEP 3/03 modify ctb_rdcpf calling sequence   */
/*************************************************************/
{
  int ncp, len, iret;
  char buffer[18];
  float cpf_lat[LLSTFL], cpf_lon[LLSTFL];
  XmString str;
/*---------------------------------------------------------------------*/

  ctb_rdcpf ("nmap2.cpf", &ncp, cpf_lat, cpf_lon, &iret);
  if (iret == 0 && ncp > 0)
    {
      sprintf (buffer, "%7.2f;%7.2f", cpf_lat[0], cpf_lon[0]);
      cst_rmbl (buffer, buffer, &len, &iret);
    }
  else
    {
      sprintf (buffer, "%s", "No Cursor Point");
    }

  XtVaSetValues (mdl_statext, XmNvalue, buffer, NULL);

  sprintf (buffer, "%s", "------;-------");
  str = XmStringCreateLocalized (buffer);
  XtVaSetValues (mdl_cursor_text, XmNlabelString, str, NULL);
  XmStringFree (str);
}

/*=====================================================================*/

void mdl_cursor_fmt_val (int getset, int fmt, int *curval)
/*************************************************************/
/* R. Tian/SAIC		02/03	Added CPF check		     */
/*************************************************************/
{
  int iret;
  static int cursorFORMAT = 0;
/*---------------------------------------------------------------------*/
  if (getset == 1)
    {
      switch (fmt)
	{
	case 0:
	case 1:
	  clo_init (&iret);
	  cursorFORMAT = fmt;
	  break;
	case 2:
	  cursorFORMAT = fmt;
	  load_cpf ();
	  break;
	default:
	  printf ("unknown format use latlon %d\n", fmt);
	  cursorFORMAT = 0;
	}
    }
  else
    *curval = cursorFORMAT;

}

/*=====================================================================*/
/* ARGSUSED */
void mdl_cursor_fmt (Widget wdgt, long which, XtPointer call)
{
  XmToggleButtonCallbackStruct *cbs;
  int iret;

  cbs = (XmToggleButtonCallbackStruct *)call;

  if (cbs->set == 0)
    return;

  mdl_cursor_fmt_val (1, (int)which, &iret);

}

/*=====================================================================*/
/* ARGSUSED */
void modmap_selCb (Widget w, XtPointer clnt, XEvent *event)
/*************************************************************/
/* R. Tian/SAIC		02/03	Added CPF check		     */
/*************************************************************/
{
  float xloc, yloc;
  float lat, lon;
  int iret, np = 1;

  char locstr[20];
  static char sysin[] = "D";
  static char sysout[] = "M";
/*---------------------------------------------------------------------*/
/*
 * Check if it is a left mouse button.
 */

  if (event->xbutton.button == 1)
    {
      xloc = (float) event->xbutton.x;
      yloc = (float) event->xbutton.y;

      gtrans (sysin, sysout, &np, &xloc, &yloc,
	      &lat, &lon, &iret, strlen (sysin), strlen (sysout));

      if (iret == 0)
	{
	  mdl_cursor_mkstr (lat, lon, locstr);
	  if (strncmp (locstr, "CPF", 3) != 0)
	    {
	      XtVaSetValues (mdl_statext, XmNvalue, locstr, NULL);
	      strcpy (mdlsoundsta, locstr);
	    }
	}
    }
}

/*=====================================================================*/
/* ARGSUSED */
void set_user_level (Widget wdgt, XtPointer clnt, XtPointer call)
/*************************************************************/
/* SET_USER_LEVEL                                            */
/*************************************************************/
{
  char *text_sta;

  text_sta = XmTextGetString (wdgt);

  if (text_sta != NULL)
    sscanf (text_sta, "%f", &user_level);
  else
    user_level = 850.0F;

  XtFree (text_sta);
}

/*=====================================================================*/
/* ARGSUSED */
void Load_mdl_sounding (Widget wdgt, XtPointer clnt, XtPointer call)
/*************************************************************/
/* LOAD_MDL_SOUNDING                                         */
/*************************************************************/
{
  XtPointer userdata;
  struct mlist_struct *mlist;
  float ix1, ix2;
  int maxfls, nfiles, ii, ier, len;
  char *cpos, tmplt[4], filnms[256];
  int npxms, curpxm;
  char winname[] = "skewt_canvas";
  int ixsize, iysize, isxsiz, isysiz, ixo, iyo, ncurwn;

  XtVaGetValues ((Widget)clnt, XmNuserData, &userdata, NULL);
  if (mdlsoundsta[0] != '\0' && mdlsoundsta[1] != 'o' && userdata != NULL)
    mlist = (struct mlist_struct *) userdata;
  else
    return;

  len = strlen (winname);
  xslwin (winname, &len, &ixsize, &iysize,
	  &isxsiz, &isysiz, &ixo, &iyo, &ncurwn, &ier);
  xstanm (&ier);

  _transferInterrupt = 0;
  NxmBusy_invoke (draw_reg, &_transferInterrupt);

  for (ii = 0; ii < mlist->nselect && !_transferInterrupt; ii++)
    {
      maxfls = 1;
      fl_mfls (mlist->modlist[ii].model,
	       mlist->modlist[ii].fhour,
	       mlist->modlist[ii].cycle,
	       &maxfls,
	       filnms, &nfiles, tmplt, &ier,
	       strlen (mlist->modlist[ii].model),
	       strlen (mlist->modlist[ii].fhour),
	       strlen (mlist->modlist[ii].cycle),
	       sizeof (filnms), sizeof (tmplt));

      if (ier != 0)
	{
	  printf ("failed to get model sounding file name for %s %s %s\n",
		  mlist->modlist[ii].model, mlist->modlist[ii].fhour,
		  mlist->modlist[ii].cycle);
	  continue;
	}

      filnms[255] = '\0';
      len = strcspn (filnms, "\t \0");
      filnms[len] = '\0';

      xsplot (&ier);
      xqcpxm (&npxms, &curpxm);
      canvas = xqpxms (0, curpxm);
      NxmChangePixmapData (curpxm, npxms);

      if (sndgs[curpxm] == NULL)
	{
	  sndgs[curpxm] =
	    (struct sndg_struct *) malloc (sizeof (struct sndg_struct));
	  sndgs[curpxm]->numlev = 0;
	}

      sndgp = sndgs[curpxm];

      if (sndgp->numlev > 0)
	copy_sndg ();
      else
	sndgp->ovrlev = 0;

      get_mdl_snd (filnms, mlist->modlist[ii].fhour, mdlsoundsta,
		   (float **) sndgs[curpxm]->sndg, &sndgp->numlev,
		   strlen (filnms), strlen (mlist->modlist[ii].fhour),
		   strlen (mdlsoundsta));

      xtnd_sndg ();
      save_origsndg ();
      if ((cpos = strrchr (mlist->modlist[ii].model, '/')) == NULL)
	sprintf (raobtitle, " %s %s %s ", mlist->modlist[ii].model,
		 mlist->modlist[ii].fhour, mdlsoundsta);
      else
	sprintf (raobtitle, " %s %s %s ", cpos + 1,
		 mlist->modlist[ii].fhour, mdlsoundsta);
      strcpy (sndgs[curpxm]->title, raobtitle);

      sprintf (raob_type, "Model Forecast");
      mode = 1;
      if (sndgp->numlev > 0)
	{
	  define_parcel ((short)current_parcel, user_level);
	  mean_wind (sndgp->sndg[sfc ()].pres, (float) i_pres (msl (6000.0F)),
		     &ix1, &ix2, &sndgp->st_dir, &sndgp->st_spd);
	  sndgp->st_spd *= .75F;
	  sndgp->st_dir += 30.0F;
	  if (sndgp->st_dir > 360.0F)
	    sndgp->st_dir -= 360.0F;
	  pagenum = 1;
	  show_page (pagenum);
	}
      show_parcel();
      draw_skewt();
      NxmBusy_checkStopBtn();
    }
  xenanm (&ier);

  if (!_transferInterrupt)
    NxmBusy_animateFinish();
  _transferInterrupt = 0;

  ii = curpxm + 1;
  while ((ii < MAX_PIXMAP) && (sndgs[ii] != NULL))
    {
      sndgs[ii]->numlev = 0;
      ii++;
    }
  if (mlist->nselect > 1)
    NxmLoopbuttonSensitive (True);
  else
    NxmLoopbuttonSensitive (False);
}

/*=====================================================================*/

void ellipse (int type, short x, short y, short width, short height)
/*************************************************************/
/*  ELLIPSE                                                  */
/*************************************************************/
{

  if (type == 0)
    {
      XDrawArc (XtDisplay (draw_reg), (canvas), gc, (int) x, (int) y,
		(Cardinal) (width - x), (Cardinal) (height - y), 0, 360 * 64);
    }
  else if (type == 1)
    {
      XFillArc (XtDisplay (draw_reg), (canvas), gc, (int) x, (int) y,
		(Cardinal) (width - x), (Cardinal) (height - y), 0, 360 * 64);
    }
}

/*=====================================================================*/

void StartLoop (void)
/*************************************************************/
/*  STARTLOOP                                                */
/*************************************************************/
{
  set_font (2);
  XtAppMainLoop (app);
}

/*=====================================================================*/
void Load_stationlist_pfc (void)
/*************************************************************/
/* LOAD_STATIONLIST_PFC                                      */
/*                                                           */
/* T. Lee/SAIC   10/02   Used LLSTFL in station dimension    */
/* R. Tian/SAIC  02/03   Added Cursor Points mark            */
/* D.W.Plummer/NCEP 3/03 modify ctb_rdcpf calling sequence   */
/*************************************************************/
{
  char station_tbl[12], statlist[LLSTFL][18];
  int nsta, ncp, i;
  int ncolor, mrktyp, mrkwid, pltval, iposn, jcolr;
  int iret;
  float sta_lat[LLSTFL], sta_lon[LLSTFL], sizmrk;
  float cpf_lat[LLSTFL], cpf_lon[LLSTFL];
  static char map_winname[] = "mappfc";


  switch (pfc_map.mapindx)
    {
    case 0:
      sprintf (station_tbl, "US");
      break;

    case 1:
      sprintf (station_tbl, "CN");
      break;

    case 2:
      sprintf (station_tbl, "MX");
      break;

    default:
      sprintf (station_tbl, "DSET");

    }


  nsharp_draw_map (map_winname, &pfc_map, &iret);

  if (pfcsoundfile[0] != '\0' && pfcsoundtime[0] != '\0' &&
      station_tbl[0] != '\0')
    {
      ncolor = 1;
      mrktyp = 6;
      sizmrk = 1.0F;
      mrkwid = 2;
      pltval = G_FALSE;
      iposn = 0;
      jcolr = 2;


      /*printf( "Beginning GET_GEM_STNS %s\n", station_tbl ); */
      get_gem_stns (pfcsoundfile, station_tbl, pfcsoundtime,
		    statlist, &nsta, sta_lat, sta_lon,
		    strlen (pfcsoundfile), strlen (station_tbl),
		    strlen (pfcsoundtime), 18);
      /*printf( "Ending GET_GEM_STNS\n" ); */

      map_mark (&nsta, sta_lat, sta_lon, NULL, &ncolor, NULL,
		&jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval, &iposn, &iret);

      ctb_rdcpf ("nmap2.cpf", &ncp, cpf_lat, cpf_lon, &iret);
      if (iret == 0 && ncp > 0)
	{
	  ncolor = 1;
	  mrktyp = 5;
	  sizmrk = 2.0F;
	  mrkwid = 2;
	  pltval = G_FALSE;
	  iposn = 0;
	  jcolr = 5;

	  map_mark (&ncp, cpf_lat, cpf_lon, NULL, &ncolor, NULL,
		    &jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval,
		    &iposn, &iret);
	}

      stnList.nstn = nsta;
      for (i = 0; i < nsta; i++)
	{
	  statlist[i][17] = 0;
	  stnList.lat[i] = sta_lat[i];
	  stnList.lon[i] = sta_lon[i];
	  strcpy (stnList.stnName[i], statlist[i]);
	}
    }
  else
    {
      stnList.nstn = 0;
    }
}

/*=====================================================================*/

void clean_uvvs (struct sndg_struct *sp)
{
  int i;
  for (i = 0; i < sp->numlev; i++)
    sp->sndg[i].omega = (-999.0F);
}

/*=====================================================================*/

void show_profiler_info (Widget wid)
/*************************************************************/
/* SHOW_PROFILER                                             */
/*                                                           */
/* Display Profiler Map and get user selection.              */
/*************************************************************/
{
  printf ("Profiler data not implemented yet\n");
}

/*=====================================================================*/

void show_acars_info (Widget wid)
/*************************************************************/
/* SHOW_PROFILER                                             */
/*                                                           */
/* Display ACARS Map and get user selection.                 */
/*************************************************************/
{
    acars_selection ();
}
