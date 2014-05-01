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
/*  Modified 11/10/1999 to handle WORLD Soundings              */
/*  Modified 12/7/1999 to: Display Icing/Turbulence/Clouds     */
/*                         Allow Point and Click for Model Data*/
/*  Modified 1/4/2000 for Port of McIdas ICEMAP functionality  */
/*  Modified 8/18/2000 to:  Display Plane Profiler/Vad Data    */
/*                          Fix Sfc Temp=Dwpt Bug              */
/*  Modified 7/2001 to: Add Sampling functionality to planview */
/*                      Profiler/Vad data display with         */
/*                      vertical profile/hodograph represent-  */
/*                      ative of data at cursor location.      */
/*     Larry J. Hinson, Aviation Weather Center KCMO           */
/*  Modified 11/2002 to:  correct dimension of time_list,      */
/*                      time_list2, and mtime_list             */
/*      T. Piper/SAIC                                          */
/*  Modified 11/2002 to: correct calls to draw_map with        */
/*                       regards to map_info variable          */
/*      L. Hinson/AWC                                          */
/*  L. Hinson: 10/03: Removed Assignment to GraphCid, SatCid,  */
/*      RadCid as these are now constants in xwprm.h           */
/*  L. Hinson: 10/03: Added idle routine to eliminate problem  */
/*      with pressures being clipped off left side.            */
/*  L. Hinson: 05/06: Add Stability Map 3                      */
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
/*  print_graphic                                              */
/*  reset_graphic                                              */
/*  parcel_popup                                               */
/*                                                             */
/***************************************************************/
#define GLOBAL
#define VIDEO
#include <xwcmn.h>
#include <sharp95.h>
#include <gui.h>
#include "expdraw.h"
#include "Xm/Scale.h"
#include "Xm/SelectioB.h"
#include "Xm/RowColumn.h"
#include "Xm/ToggleB.h"
#include "Xm/Scale.h"
#include "vertprof.h"
/***********/
/* LJH added includes */
#include <dirent.h>
#include <regex.h>
#include <sys/time.h>

/***********/

#define RESOURCE_FILE "Sharp95"

void gemfile_times ();
void mdlmap_pointer();

typedef struct {       /* for drawing moving crosshairs or zoom boxes */
  int start_x, start_y,
      last_x,  last_y;
  GC  gc;
  XPoint   points[4];
  int itype; /* 0 = temp 1 = dwpt */
  int ilev; /* 0 = bottom 1 = top 2 = inbetween */
  short yy, i;
} rubber_band_data;

#ifdef UNDERSCORE
#define get_gem_times   get_gem_times_
#define get_gem_stns    get_gem_stns_
#define get_gem_snd     get_gem_snd_
#define get_icing_fm_snd get_icing_fm_snd_
#define get_mdl_times   get_mdl_times_
#define get_mdl_snd     get_mdl_snd_
#define sncross         sncross_
#define get_pvsoundings get_pvsoundings_
#define gqdev gqdev_
#define plot_indices    plot_indices_
#endif


/*-LJH added functions--*/

extern int file_select();
extern int alphasort();
void lf_loadmdlfiles_cb (Widget w, XtPointer client_data,XtPointer call_data );
void lf_cancel_cb (Widget w, XtPointer client_data,XtPointer call_data );
void AppendMdlFileToList ( Widget List, char* item);
void getlist_mdlfilesCB (Widget List, char* item);
void mdlmap_pointer();
void GetSoundingForLatLon();
static void getTimeOper (Widget w, XtPointer *call_data, XEvent *event);
void getModelForTime (Widget w, XtPointer *call_data, XEvent *event);
static void LoopBackward (XtPointer w, XtIntervalId* timer);
static void LoopForward (XtPointer w, XtIntervalId* timer);
void LoadSingleModelSounding(int g_act_sndg);
/* End add */
void GetInterpPVSounding();
int UpdateTimeList();
int SGN(float x);
void draw_model_map();
void GetSoundingForLatLon();
void       mapw_pickstnCb();
void       mapw_exposeCb();
void       mapw_resizeCb();
void       file_cb();
void       show_textCb();
void print_gem_snd(float sndg[][7],int nlvl);
void idle(double duration);
void TvParcel_toggleCB(Widget widget, XtPointer client_data, XtPointer call_data);
void parcelLevelactivateCB(Widget w, XtPointer client_data, XtPointer call_data);
void scaleCB(Widget widget, XtPointer client_data, XtPointer call_data);
void load_pfs_times();
void load_mdl_times();

nwxtbl_t   *nwxTable;
stnlist_t  stnList;
plotdata_t plotData;
usrslct_t  usrSelect;
srchinfo_t srchInfo;
struct     maptype_list     map_info[2];
mapbnd_t   mapb;

XtAppContext app;
XFontStruct *font_struct=NULL;
XPoint lpoints[2];
short  pagenum = 1;
int    current_parcel = 4;
rubber_band_data rbdata;
Widget toplevel, gemfile_timelist, gemfile_text, gemfile_stationlist,
       gem_dialog, mdl_dialog, pfs_mdl_dialog,icemap_dialog,stabmap_dialog, mdlfile_timelist,
       pfsmdlfile_timelist, gcolrbar_frame,
       mdlfile_text, mdl_statext, sounding_text, user_defined_text, load_sharp,
       draw_reg, raob_btn, tamdar_btn, prof_btn, vad_btn, xsection_hght, gemlbl_station,
       gemlbl_station2, gemlbl_station3, icemap_station, stabmap_station, pv_station,
           pv_vertprof, pv_hodo, time_reg, time_oper_reg,
       pv_dialog,pvfile_timelist,psection_hght,pvpane,mdl_cursor_text;
/* LJH added Widgets */
Widget lf_dialog;
/*********************/
char   gemsoundfile[200], time_list [500][20], time_list2[500][20], gemsoundtime[20],
       gemsoundsta[70], sta_id[5], mdlsoundfile[200], mdlsoundtime[200],
       mtime_list[500][24], mtimeac_list[500][24], mdlsoundsta[24], mdl_selected[5], x_hght[6],
       gemsoundraob[200], gemsoundtamdar[200], gemsoundprof[200], gemsoundvad[200];
int    ntimes, mtimes, station_list, sounding_type = 999, item_type;
int    redisplay = 0;
float  user_level = 850.;
char   *levellist[] = { "3000",  "5000",  "8000",  "10000", "12000",
                        "15000", "18000", "20000", "25000", "30000",
                        "40000", "50000", "60000" };

/* LJH Made Global */
char            g_pattern[20];
char            g_pathname[400];
char            g_pfs_pathname[400];
char            g_pfs_mdlsoundfile[200];
struct dirent **g_mdlfiles;
int             g_mfcountall=0;
XrmDatabase     g_applicationDB;
char            g_mdllist[7][10];
char            g_pfsmdllist[7][10];
int             g_nummdls=0;
int             g_numpfsmdls=0;
int             g_twdth=60;
int             nsta, g_nprofstns,g_nvadstns, ncolor, mrktyp, mrkwid, pltval, iposn, jcolr;
float           sta_lat[4000], sta_lon[4000], sta_elev[4000], sta_lat2[4000],
                sta_lon2[4000], sizmrk;
char            station_table[200], staid[4000][5],
                        staname[30], idsta[5], station_tbl[12],
                        sta_st[3], sta_coun[3], statlist[4000][18];
int             markSW=0;
int             g_allModelParamsSW=0;
int             g_mlocationSelected=0;
int             g_raobModelSW=0;
int             g_pixcreated=0;
int             g_icemappixcreated=0;
int             g_pvmappixcreated=0;
int             g_WorldProj=0;
int             g_Proj=0;
int             g_nmtimes;
int             g_act_sndg=0;
int             g_newlev[49]; /* Extend from 20 Soundings to 49 */
int             g_looping=0;
int             g_looponsw=0;
char            g_mdlsoundstawrk[80];
XtAppContext    g_appcontext;
int             g_hghtno=2;
char            g_pvsound[80] = "\0";
int             g_LNGC;
/* int             g_pfs_model_sel_sw=0; */
int             g_lastmodelPFS=0;
int             g_pfs_mapset=0;
float pvsndg[14][3][200];
float pvsndghodo[20][3][200];
float staxywrk[2][200];
float vawndwrk[200][39];
int wnddirs[20],wndspds[20],wndhts[20],wndcnt;
/* int pvs_WDTH=949; */
/* int pvs_HGHT=800; */
int pvs_WDTH=830;
int pvs_HGHT=750;
unsigned levelshodo[20]={0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,12000,
                        14000,16000,18000,20000,25000,30000,35000,40000};



static XtIntervalId g_timer,g_pv_timer;
static int g_pv_timersw=0;

char g_msoundtimes[500][20];
char g_msoundfilenm[500][25];
float g_lng,g_lat,g_elev;
char g_stabmap[]="map1";
float g_printctrl_l=17.0;
float g_printctrl_w=11.0;
Pixmap time_window,time_oper_window, icemap_window,pv_window;
GC gc2, gc3;
char g_proffile[200];
char g_vadfile[200];
int g_pv_timechange=0;
char g_stnfilter[8];

       /*NP*/
       void X_Init( int argc, char *argv[])
       /*************************************************************/
       /*  X_INIT                                                   */
       /*  John Hart  NSSFC KCMO                                    */
       /*                                                           */
       /*  Draws basic SHARP graphic template on screen, including  */
       /*  areas, buttons, and tables.                              */
       /*                                                           */
       /*************************************************************/
       {
       Window   gwin;
       int ret, i, xloc, yloc, iret;
       unsigned int wd, ht, bw, dpth;
       XrmOptionDescRec _nsharpOpts[] = {
               {"-v", "verbose",  XrmoptionNoArg,  "True"},
               };
       XmString     load_st, sharp_st, sounding_st, pfs_st, model_st,
                    about_st, crossx_st, plane_st, acars_st;

       Widget       topform,  print_button,
                    inset_button, reset_button, parcel_button,
                    GIF_button,
                    interp_button, option_button, help_button, next_page,
                    main_menu, graph_tog, graph_SKEWT, graph_HODO, graph_ICG,
                    graph_TURB, graph_Clouds, menubar, load_menu,
                    about_menu, show_text;
       /* LJH add */
       char *str_type[20];
       XrmValue value;
       char text1[80];
       char resourcename[256];
       /************/

       /* ----- Initialize X Toolkit and Load Resources ----- */
       toplevel = XtVaAppInitialize(&app, "Sharp95",
                                   _nsharpOpts, XtNumber(_nsharpOpts),
                                   &argc, argv,
                                   NULL, NULL,
                                   /*
                                   XmNbaseWidth, 780,
                                   XmNbaseHeight, 680,
                                   XmNminWidth, 860,
                                   XmNminHeight, 750,
                                   */
                                   NULL);
       XtVaSetValues(toplevel, XmNbaseWidth, 780,
                               XmNbaseHeight, 680,
                               XmNminWidth, 860,
                               XmNminHeight, 750,
                               NULL);

       /* check resource file */
       /*  LJH alter to grab actual name of resource file */
       /* NxmRes_check(XtDisplay(toplevel), RESOURCE_FILE, NULL); */
       NxmRes_check(XtDisplay(toplevel), RESOURCE_FILE, resourcename);
       /* LJH addin to pull additional variables from Resource file */
       if ((g_applicationDB=XrmGetFileDatabase(resourcename))==NULL)
         printf("Resource File %s not found\n", resourcename);

       /**LJH end */
       NxmVers_showTitle(toplevel);

       /* ----- Create Graphics Form (window) ----- */
       topform = XtVaCreateManagedWidget("graphic_form",
                     xmFormWidgetClass, toplevel,
                     XmNfractionBase, 56, NULL );  /* 28 to 56 */

       /* ----- Create Menu bar across top of "topform" ----- */
       load_st = XmStringCreateLocalized ("Load");
       about_st = XmStringCreateLocalized ("About" );
       menubar = XmVaCreateSimpleMenuBar ( topform, "menubar",
                     XmVaCASCADEBUTTON, load_st, 'L',
                     XmVaCASCADEBUTTON, about_st, 'A',
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNrightAttachment, XmATTACH_FORM, NULL );
       /* XmStringFree (load_st); */
       /* XmStringFree (about_st); */

       /* ----- Create About Menu Option ----- */
       about_menu = XtNameToWidget(menubar, "button_1");
       XtVaSetValues(menubar, XmNmenuHelpWidget, about_menu, NULL);
       XtAddCallback(about_menu, XmNactivateCallback,
                     (XtCallbackProc)about_cb, NULL);

       /* ----- Create Pulldown Menu for Loading Soundings ----- */
       sharp_st = XmStringCreateLocalized ("Archive Files");
       sounding_st = XmStringCreateLocalized ("Observed Raobs");
       model_st = XmStringCreateLocalized ("Model Soundings");
       pfs_st = XmStringCreateLocalized("Point Fcst Soundings");
       crossx_st = XmStringCreateLocalized ("Timesections");
       plane_st = XmStringCreateLocalized ("Plane Profiler+Vad");
       acars_st = XmStringCreateLocalized ("ACARS Soundings");
       load_menu = XmVaCreateSimplePulldownMenu ( menubar, "load_menu",
                     0, load_cb,
                     XmVaPUSHBUTTON, sharp_st, 'F', NULL, NULL,
                     XmVaPUSHBUTTON, sounding_st, 'O', NULL, NULL,
                     XmVaPUSHBUTTON, model_st, 'M', NULL, NULL,
                     XmVaPUSHBUTTON, pfs_st,'P', NULL, NULL,
                     XmVaPUSHBUTTON, crossx_st, 'T', NULL, NULL,
                     XmVaPUSHBUTTON, plane_st, 'l', NULL, NULL,
                     XmVaPUSHBUTTON, acars_st, 'A', NULL, NULL,
                     NULL );
       /*
       XmStringFree (sharp_st);
       XmStringFree (sounding_st);
       XmStringFree (model_st);
       XmStringFree (crossx_st);
       XmStringFree (plane_st);
       XmStringFree (acars_st);
       */
       XtManageChild(menubar);

       /* ----- Initialize GEMPAK color palette ----- */
       gemdisplay = XtDisplay(topform);
       gemmap  = DefaultColormap(gemdisplay,DefaultScreen(gemdisplay));
       gemvis  = DefaultVisual(gemdisplay, DefaultScreen(gemdisplay));
       xgbank(XtDisplay(topform), &ret);
       xcaloc(GraphCid, &ret);
       for ( i = 0; i < ColorBanks.banks[GraphCid]; i++)
          pixels[i] = ColorBanks.colrs[GraphCid][i];

       /* ----- Create NAWIPS color bar ----- */
       gcolrbar_frame = XtVaCreateManagedWidget("gcolrbar",
                xmFrameWidgetClass, topform,
                XmNtopAttachment,XmATTACH_POSITION,
                XmNtopPosition, 54,
                XmNbottomAttachment,XmATTACH_POSITION,
                XmNbottomPosition, 56,
                XmNleftAttachment,  XmATTACH_FORM,
                XmNrightAttachment, XmATTACH_FORM,
                NULL);

       /* ----- Create main area for soundings/parameters ----- */
       draw_reg = XtVaCreateManagedWidget("canvas",
              xmDrawingAreaWidgetClass, topform,
              XmNtopAttachment, XmATTACH_WIDGET,
              XmNtopWidget, menubar,
              XmNleftAttachment, XmATTACH_FORM,
              XmNrightAttachment, XmATTACH_FORM,
              XmNbottomAttachment, XmATTACH_POSITION,
              XmNbottomPosition, 45,
              XmNbackground, pixels[0],
              XmNwidth, 900,
              XmNheight, 620,
              NULL);
       /* ----- Create Area for plotting/Selecting Times */
       time_reg = XtVaCreateManagedWidget("times",
              xmDrawingAreaWidgetClass, topform,
              XmNtopAttachment, XmATTACH_WIDGET,
              XmNtopWidget, draw_reg,
              XmNleftAttachment, XmATTACH_FORM,
              XmNrightAttachment, XmATTACH_POSITION,
              XmNrightPosition, 34,
              XmNbottomAttachment, XmATTACH_POSITION,
              XmNbottomPosition, 48,
              XmNbackground, pixels[0],
              XmNwidth, 600,
              XmNheight, g_twdth,
              NULL);
       /* ----- Create Area for Time Operations */
       time_oper_reg =XtVaCreateManagedWidget("timesoper",
              xmDrawingAreaWidgetClass, topform,
              XmNtopAttachment, XmATTACH_WIDGET,
              XmNtopWidget, draw_reg,
              XmNleftAttachment, XmATTACH_POSITION,
              XmNleftPosition, 34,
              XmNrightAttachment, XmATTACH_FORM,
              XmNbottomAttachment, XmATTACH_POSITION,
              XmNbottomPosition, 48,
              XmNbackground, pixels[0],
              XmNwidth, 100,
              XmNheight,40,
              NULL);
       XtAddEventHandler(time_oper_reg, ButtonReleaseMask, FALSE,
            (XtEventHandler) getTimeOper,(XtPointer)NULL);
       XtAddEventHandler(time_reg, ButtonReleaseMask, FALSE,
            (XtEventHandler) getModelForTime,(XtPointer)NULL);
       /* ----- Create SKEWT/HODOGRAPH button ----- */
       /*graph_tog = XtVaCreateManagedWidget("Hodograph",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 24,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 25,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 4,
                     NULL);
       XtAddCallback(graph_tog, XmNactivateCallback,
                     (XtCallbackProc)tog_graph, NULL);*/
       graph_SKEWT= XtVaCreateManagedWidget("SKEWT",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 48,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 50,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 4,
                     NULL);
       XtAddCallback(graph_SKEWT, XmNactivateCallback,
                    (XtCallbackProc)display_SKEWT, NULL);
       graph_HODO= XtVaCreateManagedWidget("HODO",
                     xmPushButtonWidgetClass, topform,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 48,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 50,
                     XmNleftAttachment, XmATTACH_POSITION,
                     XmNleftPosition, 4,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 8,
                     NULL);
       XtAddCallback(graph_HODO, XmNactivateCallback,
                    (XtCallbackProc)display_HODO, NULL);

       graph_ICG = XtVaCreateManagedWidget("Icing",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 50,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 52,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 4,
                     NULL);
       XtAddCallback(graph_ICG, XmNactivateCallback,
                    (XtCallbackProc)display_ICG, NULL);
       graph_TURB = XtVaCreateManagedWidget("TURB",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_WIDGET,
                     XmNleftWidget, graph_ICG,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 50,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 52,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 8,
                     NULL);
       XtAddCallback(graph_TURB, XmNactivateCallback,
                    (XtCallbackProc)display_TURB, NULL);
       graph_Clouds = XtVaCreateManagedWidget("Clouds",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 52,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 54,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 8,
                     NULL);
       XtAddCallback(graph_Clouds, XmNactivateCallback,
                    (XtCallbackProc)display_Clouds, NULL);

       /* ----- Create EXIT button ----- */
       main_menu = XtVaCreateManagedWidget("Exit",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_POSITION,
                     XmNleftPosition, 8,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 48,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 54,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 16,
                     NULL);
       XtAddCallback(main_menu, XmNactivateCallback,
                      (XtCallbackProc)menu_main, NULL);

       /* ----- Create PRINT button ----- */
       print_button = XtVaCreateManagedWidget("PRINT",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_POSITION,
                     XmNleftPosition, 16,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 50,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 48,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 24,
                     NULL);
       XtAddCallback(print_button, XmNactivateCallback,
                     (XtCallbackProc)print_graphic, NULL);
       GIF_button = XtVaCreateManagedWidget("SAVE GIF",
                    xmPushButtonWidgetClass, topform,
                    XmNleftAttachment, XmATTACH_POSITION,
                    XmNleftPosition, 16,
                    XmNbottomAttachment, XmATTACH_POSITION,
                    XmNbottomPosition, 52,
                    XmNtopAttachment, XmATTACH_POSITION,
                    XmNtopPosition, 50,
                    XmNrightAttachment, XmATTACH_POSITION,
                    XmNrightPosition, 24,
                    NULL);
       XtAddCallback(GIF_button, XmNactivateCallback,
                   (XtCallbackProc)Make_Gif, NULL);


       /* ----- Create RESET button ----- */
       reset_button = XtVaCreateManagedWidget("RESET",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_POSITION,
                     XmNleftPosition, 24,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 50,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 48,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 32,
                     NULL);
       XtAddCallback(reset_button, XmNactivateCallback,
                     (XtCallbackProc)reset_graphic, NULL);

       /* ----- Create PARCEL button ----- */
       parcel_button = XtVaCreateManagedWidget("PARCEL",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_POSITION,
                     XmNleftPosition, 32,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 50,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 48,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 40,
                     NULL);
       XtAddCallback(parcel_button, XmNactivateCallback,
                      (XtCallbackProc)parcel_popup, NULL);

       /* ----- Create INSET button ----- */
       inset_button = XtVaCreateManagedWidget("INSET",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_POSITION,
                     XmNleftPosition, 16,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 54,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 52,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 24,
                     NULL);
       XtAddCallback(inset_button, XmNactivateCallback,
                      (XtCallbackProc)inset_graphic, NULL);

       /* ----- Create INTERP button ----- */
       interp_button = XtVaCreateManagedWidget("INTERP",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_POSITION,
                     XmNleftPosition, 24,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 54,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 52,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 32,
                     NULL);
       XtAddCallback(interp_button, XmNactivateCallback,
                      (XtCallbackProc)interp_data, NULL);

       /* ----- Create OPTIONS button ----- */
       option_button = XtVaCreateManagedWidget("OPTIONS",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_POSITION,
                     XmNleftPosition, 32,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 54,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 52,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 40,
                     NULL);
       XtAddCallback(option_button, XmNactivateCallback,
                      (XtCallbackProc)option_graphic, NULL);

       /* ----- Create NEXT PAGE button ----- */
       next_page = XtVaCreateManagedWidget("NEXT PAGE",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_POSITION,
                     XmNleftPosition, 40,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 48,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 54,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 48,
                     NULL);
       XtAddCallback(next_page, XmNactivateCallback,
                      (XtCallbackProc)page_next, NULL);

       /* ----- Create HELP button ----- */
       /* help_button = XtVaCreateManagedWidget("HELP",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_POSITION,
                     XmNleftPosition, 24,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 24,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 27,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 28,
                     NULL); */

       /* ----- Create Show Text button ----- */
       show_text = XtVaCreateManagedWidget("Show Text",
                     xmPushButtonWidgetClass, topform,
                     XmNleftAttachment, XmATTACH_POSITION,
                     XmNleftPosition, 48,
                     XmNtopAttachment, XmATTACH_POSITION,
                     XmNtopPosition, 48,
                     XmNbottomAttachment, XmATTACH_POSITION,
                     XmNbottomPosition, 54,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 56,
                     NULL);
        XtAddCallback(show_text, XmNactivateCallback,
                       (XtCallbackProc)show_textCb, NULL);

       /*
        * create print popup window
        */

       NxmPrt_create( "datatop", topform, print_soundings);

       /* ----- Create Window and Map Screen ----- */
       XtRealizeWidget( toplevel );

       /* ----- Create "Graphic Context" ----- */
       gc = XCreateGC(XtDisplay(draw_reg), XtWindow(draw_reg), 0, 0);

       /* ----- Create "Graphic Context" for time_reg Widget ----- */
       gc2 = XCreateGC(XtDisplay(time_reg), XtWindow(time_reg), 0, 0);
       /* ----- Create "Graphic Context" for time_oper_reg Widget ---- */
       gc3 = XCreateGC(XtDisplay(time_oper_reg), XtWindow(time_oper_reg), 0, 0);
       /* ----- callback for expose ----- */
       XtAddCallback(draw_reg, XmNexposeCallback,
                     (XtCallbackProc)expose_overlays, NULL);

       /* ----- callback for resize ----- */
       XtAddCallback(draw_reg, XmNresizeCallback,
                     (XtCallbackProc)resize_callback, NULL);
       /* ----- callback for time_reg expose ----- */
       XtAddCallback( time_reg, XmNexposeCallback,
                (XtCallbackProc)expose_time_reg_overlays, NULL );

       /* ----- callback for time_reg resize ----- */
       XtAddCallback(time_reg, XmNresizeCallback,
                (XtCallbackProc)resize_time_reg_overlays, NULL);
       XtAddCallback(time_oper_reg, XmNexposeCallback,
                (XtCallbackProc)expose_time_oper_reg_overlays, NULL );
       XtAddCallback(time_reg, XmNresizeCallback,
                (XtCallbackProc)resize_time_oper_reg_overlays, NULL);




       /* ----- "button pressed" event ----- */
       XtAddEventHandler(draw_reg, ButtonPressMask, FALSE,
                     (XtEventHandler)position_cursor,
                     (XtPointer)NULL);

       /* ----- "mouse moved with button #1 depressed" event ----- */
       XtAddEventHandler(draw_reg, Button1MotionMask, FALSE,
                     (XtEventHandler)update_pointer,
                     (XtPointer)NULL);

       /* ----- "button released" event ----- */
       XtAddEventHandler(draw_reg, ButtonReleaseMask, FALSE,
                     (XtEventHandler)redraw_sounding,
                     (XtPointer)NULL);

       /* ----- "mouse moved" event ----- */
       XtAddEventHandler(draw_reg, PointerMotionMask, FALSE,
                     (XtEventHandler)pointer_update,
                     (XtPointer)NULL);

       XGrabButton(XtDisplay(draw_reg), AnyButton, AnyModifier,
              XtWindow(draw_reg), TRUE,
              ButtonPressMask | Button1MotionMask |
              ButtonReleaseMask,
              GrabModeAsync, GrabModeAsync,
              XtWindow(draw_reg),
              0 );

       root        =  DefaultRootWindow ( XtDisplay(draw_reg) );
       XGetGeometry  ( XtDisplay(draw_reg), XtWindow(draw_reg),
                       &root, &xloc, &yloc, &wd, &ht, &bw, &dpth );

       xwdth = wd;
       xhght = ht;
       xdpth = dpth;
       canvas = XCreatePixmap( XtDisplay(draw_reg), root, wd, ht, dpth );

       strcpy ( x_hght , "25000" );
       gwin = XtWindow(draw_reg);
       xmotifw(gwin, "datatop", gc, xwdth, xhght, xdpth, &iret);

       XFlush ( gemdisplay );

       if( iret != 0 )
         return;

       /* ----- Set background color to Black (pixels[0]) ----- */
       XSetBackground ( XtDisplay(draw_reg), gc, pixels[0] );
       XFillRectangle ( XtDisplay(draw_reg), canvas, gc, 0, 0,
                                xwdth, xhght );

       /* ----- Display NAWIPS color bar ----- */
       NuiColorBarCreate( gcolrbar_frame, False);
       NuiColorEditPopup( topform );

       /* ----- Initialize "RubberBand" struct for skewt edit ----- */
       rbdata.gc      = xs_create_xor_gc(draw_reg, "white");
       rbdata.start_x = 0;
       rbdata.start_y = 0;
       rbdata.last_x  = 0;
       rbdata.last_y  = 0;
       rbdata.itype  = 0;
       rbdata.ilev  = 2;
       rbdata.yy  = 0;
       rbdata.i  = 0;
       rbdata.points[0].x = rbdata.points[1].x = rbdata.points[2].x = 0;
       rbdata.points[0].y = rbdata.points[1].y = rbdata.points[2].y = 0;

       set_font(2);

       if ( ! nobanner )
            about_cb ( toplevel, NULL, NULL );

       resize_callback ( draw_reg, (XtPointer)NULL, (XtPointer)NULL );
       }


       /*NP*/
       void setcliprgn(short tlx, short tly, short brx, short bry)
       /*************************************************************/
       /*  SETCLIPRGN                                               */
       /*************************************************************/
       {
       Region        regn;
       XRectangle    rect;
       regn = XCreateRegion ();
       rect.x = tlx-1;
       rect.y = tly-1;
       rect.width = brx - tlx + 5;
       rect.height = bry - tly + 5;
       XUnionRectWithRegion ( &rect, regn, regn );

       XSetRegion ( XtDisplay (draw_reg), gc, regn );

       XDestroyRegion ( regn );
       }


       /*NP*/
       void setlinestyle(short style, short width )
       /*************************************************************/
       /*  SETLINESTYLE                                             */
       /*     style = 1 = solid                                     */
       /*     style = 2 = dash                                      */
       /*     style = 3 = dash dot dash                             */
       /*     style = 4 = short dash long dash                      */
       /*************************************************************/
       {
       int           dash_offset = 1, n;
       static char   dash_dash[]  = {3,3};
       static char   dash_dot[]  = {4,4,2,4};
       static char   dash_long[]  = {3,3,6,3};
       short         line_width;

       if ( width < 0 || width > 10 )
              line_width = 1;
       else
              line_width = width;

       if ( style == 1 )
          {
          XSetLineAttributes ( XtDisplay(draw_reg), gc, line_width,
                     LineSolid, CapButt, JoinRound );
          }
       else
          {
          XSetLineAttributes ( XtDisplay(draw_reg), gc, line_width,
                     LineOnOffDash, CapButt, JoinRound );
          }

          if ( style == 2 )
             { XSetDashes ( XtDisplay(draw_reg), gc, dash_offset, dash_dash, 2 ); }
          else if ( style == 3 )
             { XSetDashes ( XtDisplay(draw_reg), gc, dash_offset, dash_dot, 4 ); }
          else if ( style == 4 )
             { XSetDashes ( XtDisplay(draw_reg), gc, dash_offset, dash_long,4 ); }
       }


       /*NP*/
       int getgtextextent(char *st )
       /*************************************************************/
       /*  GETGTEXTEXTENT                                           */
       /*************************************************************/
       {
       return XTextWidth (font_struct, st, strlen(st) );
       }

       /*NP*/
       int getfontheight()
       {
       return font_struct->ascent+font_struct->descent;
       }
       /*NP*/
       void moveto(short x, short y )
       /*************************************************************/
       /*  MOVETO                                                   */
       /*************************************************************/
       {
       lpoints[0].x = x;
       lpoints[0].y = y;
       }


       /*NP*/
       void lineto(short x, short y )
       /*************************************************************/
       /*  LINETO                                                   */
       /*************************************************************/
       {
       lpoints[1].x = x;
       lpoints[1].y = y;

       XDrawLine(XtDisplay(draw_reg), canvas, gc,
              lpoints[0].x, lpoints[0].y,
              lpoints[1].x, lpoints[1].y );
       lpoints[0].x = x;
       lpoints[0].y = y;
       }


       /*NP*/
       void rectangle (int type, short x, short y, short width,
                     short height )
       /*************************************************************/
       /*  RECTANGLE                                                */
       /*************************************************************/
       {

       if ( type == 0 )
          {
          XDrawRectangle ( XtDisplay(draw_reg), canvas, gc,
                            x, y, width - x, height- y );

          }
       else if ( type == 1 )
          {
          XFillRectangle ( XtDisplay(draw_reg), canvas, gc, x,
                            y, width - x, height - y );

          }
       }


       /*NP*/
       void setcolor (int color )
       /*************************************************************/
       /*  SETCOLOR                                                 */
       /*************************************************************/
       {
       XSetForeground(XtDisplay(draw_reg), gc, pixels[color] );
       }


       /*NP*/
       short set_font (short font )
       /*************************************************************/
       /*  SET_FONT                                                 */
       /*************************************************************/
       {
       Font          font_info;
       static char   font_1 [] =
       { "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-1"};

       static char   font_2 [] =
       { "-adobe-courier-bold-r-normal--14-100-100-100-m-90-iso8859-1" };

       static char   font_3 [] =
       { "-adobe-times-bold-r-normal--17-120-100-100-p-88-iso8859-1" };

       static char   font_4 [] =
       { "-adobe-new century schoolbook-bold-*-*-*-34-*-*-*-*-*-*-*" };

       static char   font_5 [] =
       { "-*-symbol-medium-*-*-*-14-140-75-75-p-85-*-*" };

       static char   font_6 [] =
       { "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"};


       if ( font == 1 )
          {
          font_info = XLoadFont(XtDisplay(draw_reg), font_1 );
          }
       else if ( font == 2 )
          {
          font_info = XLoadFont(XtDisplay(draw_reg), font_2 );
          }
       else if ( font == 3 )
          {
          font_info = XLoadFont(XtDisplay(draw_reg), font_3 );
          }
       else if ( font == 4 )
          {
          font_info = XLoadFont(XtDisplay(draw_reg), font_4 );
          }
       else if ( font == 5 )
          {
          font_info = XLoadFont(XtDisplay(draw_reg), font_5 );
          }
       else if ( font == 6 )
          {
          font_info = XLoadFont(XtDisplay(draw_reg), font_6 );
          }

       XSetFont(XtDisplay(draw_reg), gc, font_info );
       if(font_struct != NULL) XFreeFontInfo(NULL,font_struct,0);
       font_struct = XQueryFont ( XtDisplay(draw_reg), font_info);
       }


       /*NP*/
       void print_graphic (Widget w, XmDrawingAreaCallbackStruct *call_data)
       /*************************************************************/
       /*  PRINT_GRAPHIC                                            */
       /*************************************************************/
       {
       if ( mode != 3 )
         {
         /* if ( numlvl > 0 ) */  /* LJH Removed 9-14-2001 */
         /* print_sounding_hpgl (); */
                 print_sounding_ps(1);

         }
       else
         {
         NxmPrt_prtWPopup();
         }
       }


       /*NP*/
       void reset_graphic (Widget w, XmDrawingAreaCallbackStruct *call_data)
       /*************************************************************/
       /*  RESET_GRAPHIC                                            */
       /*************************************************************/
       {
       if (!qc(i_temp(700))) return;
       if ( mode != 3 )
         {
         restore_origsndg ();
         reset_options ( mode, pagenum );
         XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
                    gc, 0, 0, xwdth, xhght, 0, 0 );
         }
       }


       /*NP*/
       static Widget rbutton[5];
       void parcel_popup (Widget w)
       /*************************************************************/
       /*  PARCEL_POPUP                                             */
       /*                                                           */
       /*  Creates pop-up window to choose parcel.                  */
       /*************************************************************/
       {
       Arg                     args[5];
       Cardinal                argcnt;
       XmStringCharSet         def_charset;
       static Widget           parcel_pane, form, prcl_top=0;
       static Widget           rowcol, form2, sep_wid, parcel_cancel;
       static Widget           TvParcelCorToggle,ParcelToggle;
       int                     i;
       char     *labels[] = { "Current Surface", "Forecast Surface",
                              "Mean mixing layer",
                              "Most unstable parcel",
                              "Smallest CINH",
                              "User Defined level" };
       XmString                 pop_title;

       if (!qc(i_temp(700))) return;

       if ( mode != 3 )
       {
       if ( ! prcl_top )
          {
          def_charset = (XmStringCharSet) XmSTRING_DEFAULT_CHARSET;

        /* ----- Create a Dialog for parcel selection ----- */
        pop_title = XmStringCreateLocalized( "Select Thermodynamic Parcel" );
        prcl_top = XmCreateBulletinBoardDialog(w, "parcel_panel", NULL, 0);
        XtVaSetValues( prcl_top, XmNdialogTitle, pop_title, NULL);
        /* XmStringFree(pop_title); */

        parcel_pane = XtVaCreateWidget("parcel_pane",
                                xmPanedWindowWidgetClass,
                                prcl_top,
                                XmNsashWidth,   1,
                                XmNsashHeight,  1,
                                NULL);

        form = XtCreateManagedWidget("content_form",
                                         xmFormWidgetClass,
                                         parcel_pane,
                                         NULL,
                                         0);

        /* Create a rowcol */

        rowcol = XtVaCreateWidget("Parcel",
                                  xmRowColumnWidgetClass, form,
                                  XmNnumColumns,          2,
                                  XmNorientation,         XmVERTICAL,
                                  XmNspacing,             4,
                                  XmNtopAttachment,       XmATTACH_POSITION,
                                  XmNtopPosition,         2,
                                  XmNleftAttachment,      XmATTACH_POSITION,
                                  XmNleftPosition,        2,
                                  NULL );


       for (i=0; i<XtNumber(labels); i++)
          {
          rbutton[i] = XtVaCreateManagedWidget(labels[i],
                     xmToggleButtonWidgetClass, rowcol,
                     XmNindicatorType, XmONE_OF_MANY,
                     NULL );

           XtAddCallback(rbutton[i], XmNvalueChangedCallback,
                     (XtCallbackProc)Toggle_Callback,
                     (XtPointer)i);

          if (i == current_parcel-1 )
             XmToggleButtonSetState(rbutton[i],True,True);
          }

       for (i=0; i<XtNumber(labels); i++)
          {
          /* Change i comparison from 4 to 5 */
          if ( i == 5 )
             {
             user_defined_text = XtVaCreateManagedWidget ("user_text",
                     xmTextWidgetClass, rowcol,
                     XmNcolumns, 16,
                     XmNvalue, "850",
                     NULL );

             XtAddCallback ( user_defined_text, XmNvalueChangedCallback,
                     set_user_level, NULL );
             XtAddCallback ( user_defined_text, XmNactivateCallback,
               (XtCallbackProc) parcelLevelactivateCB, (XtPointer) NULL);
             /* Add One more */

             TvParcelCorToggle = CreateRadioButton(rowcol, "Parcel Traj - Tv Corrected",
               (XtCallbackProc) TvParcel_toggleCB, (XtPointer) "TvParcelCor");
             ParcelToggle = CreateRadioButton(rowcol,"Parcel Traj - Not Corrected",
               (XtCallbackProc) TvParcel_toggleCB, (XtPointer) "Parcel");
             XmToggleButtonSetState(TvParcelCorToggle,True,True);
             XmToggleButtonSetState(ParcelToggle,True,True);
             }
       else
             {
             sep_wid = XtVaCreateManagedWidget(labels[i],
                     xmSeparatorWidgetClass, rowcol,
                     XmNseparatorType, XmNO_LINE,
                     NULL );
             }
          }

       form2 = XtVaCreateWidget("form", xmFormWidgetClass,
                     parcel_pane, XmNfractionBase, 7,
                     NULL );


       parcel_cancel = XtVaCreateManagedWidget ("CANCEL",
                     xmPushButtonWidgetClass, form2,
                     XmNleftAttachment, XmATTACH_POSITION,
                     XmNleftPosition, 2,
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNrightAttachment, XmATTACH_POSITION,
                     XmNrightPosition, 5,
                     NULL );

       XtAddCallback(parcel_cancel, XmNactivateCallback,
                     (XtCallbackProc)parcel_cancel_cb, prcl_top);


       XtManageChild(rowcol);
       XtManageChild(form2);
       XtManageChild(parcel_pane);


       }
       XtManageChild(prcl_top);

       }
       }


void TvParcel_toggleCB(Widget widget, XtPointer client_data, XtPointer call_data) {
            char gemdevice[72];
            int iret;
            XmToggleButtonCallbackStruct* ptr;
            ptr=(XmToggleButtonCallbackStruct*) call_data;
            if (ptr != NULL) {
            if (strcmp((char *) client_data,"TvParcelCor")==0)
              set_g_TvParcelCor((int)ptr->set);
            if (strcmp((char *) client_data,"Parcel")==0)
              set_g_Parcel((int)ptr->set);
            redraw_graph(1);
            show_parcel();
            XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
              gc, 0, 0, xwdth, xhght, 0, 0 );
            XFlush ( XtDisplay(draw_reg) );
          }
}

        /*NP*/
        void inset_graphic (Widget w, XmDrawingAreaCallbackStruct *call_data)
       /*************************************************************/
       /* INSET_GRAPHIC                                             */
       /*************************************************************/
        {
        if (!qc(i_temp(700))) return;

        if ( mode != 3 )
            inset_options( mode );
        }



        /*NP*/
        void interp_data (Widget w, XmDrawingAreaCallbackStruct *call_data)
       /*************************************************************/
       /* INTERP_DATA                                               */
       /*************************************************************/
        {
        if (!qc(i_temp(700))) return;

        if ( mode != 3 )
          {
          interp_sndg();
          redraw_graph( mode );
          define_parcel( current_parcel, user_level );
          show_parcel ();
          show_page( pagenum );
          }
        }


        /*NP*/
        void option_graphic (Widget w, XmDrawingAreaCallbackStruct *call_data)
       /*************************************************************/
       /* OPTION_GRAPHIC                                            */
       /*************************************************************/
        {
        if (!qc(i_temp(700))) return;
        overlay_previous += 1;
        if (overlay_previous == 2) overlay_previous = 0;
        if (overlay_previous == 0)
           {
           XtVaSetValues(w, XmNlabelString,
                        XmStringCreateLocalized("Overlay: OFF"), NULL );
           }
        if (overlay_previous == 1)
           {
           XtVaSetValues(w, XmNlabelString,
                        XmStringCreateLocalized("Overlay: ON"), NULL );
           }

        if ( mode != 3 )
          general_options();
        }


        /*NP*/
        void expose_overlays (Widget w, XmDrawingAreaCallbackStruct *call_data)
        {
       /*************************************************************/
       /* EXPOSE_OVERLAYS                                           */
       /*************************************************************/
        XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
                    gc, 0, 0, xwdth, xhght, 0, 0 );

        }


        /*NP*/
        void resize_callback (Widget w, XtPointer *data,
                              XmDrawingAreaCallbackStruct *call_data)
       /*************************************************************/
       /*RESIZE_CALLBACK                                            */
       /*************************************************************/
        {
        Dimension nwdth, nhght, xdim, ydim, ndist;
        char st[80];
        int  idwdth, idhght, iswdth, ishght, iret;

        XtVaGetValues ( draw_reg, XmNwidth, &nwdth,
                        XmNheight, &nhght, NULL );
        xdim = nwdth - 340 - skv.tlx - 20;
        ydim = nhght - skv.tly;

        if ( xdim < ydim )
            ndist = xdim;
        else
            ndist = ydim;


        skv.brx = skv.bry = ndist;
        hov.brx = hov.bry = ndist;
        xwdth = nwdth;
        xhght = nhght;

        /****  Re-activated 11/3 for Resize events of the Window */
        XFreePixmap ( XtDisplay(draw_reg), canvas );
        root        =  DefaultRootWindow ( XtDisplay(draw_reg) );
        canvas = XCreatePixmap ( XtDisplay(draw_reg),
                                root, xwdth, xhght, 8 );
        /****/

        xclear(&idwdth, &idhght, &iswdth, &ishght, &iret);


        setcliprgn ( 1, 1, xwdth, xhght );
        setcolor(0);
        XFillRectangle (XtDisplay(draw_reg), canvas, gc, 0, 0,
                        xwdth, xhght );

        if ( mode != 3 )
        {

          /* ----- Parameter Area ----- */
           setcolor(4);
           rectangle( 1, skv.brx + 20, skv.tly, xwdth-5, xhght-5);
           setcolor(1);
           rectangle( 0, skv.brx + 20, skv.tly, xwdth-5, xhght-5);

          /* ----- Cursor Data Area ----- */
           setcolor(3);
           strcpy( st, "CURSOR DATA" );
           outgtext ( st,
            (((skv.brx + 20 + xwdth) / 2) - (getgtextextent( st ) / 2)),
            skv.tly + 5 );
           setcolor(0);
           rectangle( 1, skv.brx + 30, skv.tly + 22, xwdth-15, 100);
           setcolor(1);
           rectangle( 0, skv.brx + 30, skv.tly + 22, xwdth-15, 100);

        if ( mode == 1 )
          {
          draw_skewt();
          if ( numlvl > 0 )
            {
            define_parcel( current_parcel, user_level);
            show_parcel ();
            }
          }
        else if ( mode == 2 )
          draw_hodo();
        else if ( mode == 4 )
          draw_ICG();
        else if (mode == 5)
          draw_TURB();
        else if (mode == 6)
          draw_Clouds();
        if (numlvl>0 && !(mode ==4 || mode == 5  || mode == 6 || g_looping)) show_page(pagenum);
        }
        else
            if (!(mode==4 || mode==5)) {
              Load_gem_sounding ();
            }
        if (g_pfs_model_sel_sw) {
          setcliprgn ( 1, 1, xwdth, xhght );
          setcolor(5);
          set_font(2);
          strcpy(st,"Forecast Times:");
          outgtext(st,0,xhght-12);
        }
        setcliprgn(1,1,xwdth,xhght);
        XCopyArea ( XtDisplay(draw_reg), canvas,
                      XtWindow(draw_reg), gc, 0, 0, xwdth, xhght, 0, 0 );
        XFlush(XtDisplay(draw_reg));
        }


        /*NP*/
        void position_cursor (Widget w, XtPointer *data, XEvent *event)
       /*************************************************************/
       /* POSITION_CURSOR                                           */
       /*                                                           */
       /* Handles "mouse button depressed" event                    */
       /*************************************************************/
        {
        short x, y, i, yy;
        float pres, temp, dwpt, tcur, d1, d2, chg1;
        char st[20], gemdevice[72];
        int iret;
        /* LJH Added to correct temp=dwpt problem */
        /* ----- "button released" event ----- */
        XtAddEventHandler(draw_reg, ButtonReleaseMask, FALSE,
                     (XtEventHandler)redraw_sounding,
                     (XtPointer)NULL);
        /* ----- Determine if a button is pushed ----- */
        if(event->xbutton.button == 1)
          {
          if (!qc(i_temp(700))) return;

        raob_mod = 1;
        if ( mode == 1 )
        {
          if ( event->xbutton.y > skv.bry || event->xbutton.x > skv.brx ||
               event->xbutton.y < skv.tly || event->xbutton.x < skv.tlx )
               return;


          XDrawLine( XtDisplay(w), XtWindow(w), rbdata.gc,
                rbdata.points[0].x,
                rbdata.points[0].y,
                rbdata.points[1].x,
                rbdata.points[1].y );
          XDrawLine( XtDisplay(w), XtWindow(w), rbdata.gc,
                rbdata.points[1].x,
                rbdata.points[1].y,
                rbdata.points[2].x,
                rbdata.points[2].y );


          /* ----- Determine pres/temp coords of cursor ----- */
          pres = pix_to_pres( (short)event->xbutton.y);
          tcur = pix_to_temp( (short)event->xbutton.x,
                              (short)event->xbutton.y);

          if (pres > sndg[sfc()][1]) { pres = sndg[sfc()][1]; }

          temp = i_temp( pres );
          dwpt = i_dwpt( pres );
          d1 = (float)fabs(tcur - temp);
          d2 = (float)fabs(tcur - dwpt);

          if( d1 < d2 )
             {

             /* ----- Edit Temperatures ----- */
             rbdata.itype = 0;
             rbdata.i = i = grab_level( pres );
             if(!qc(sndg[i][3])) { sndg[i][3] = i_temp(sndg[i][1]); }

             /* ----- Move mouse to starting spot ----- */
             rbdata.yy = yy = pres_to_pix(sndg[i][1]);
             XWarpPointer ( XtDisplay(draw_reg), XtWindow(draw_reg),
                             XtWindow(draw_reg), skv.tlx, skv.tly,
                             skv.brx+skv.tlx, skv.bry+skv.tly,
                             temp_to_pix(sndg[i][3], sndg[i][1]), yy );
             rbdata.last_x = rbdata.start_x = event->xbutton.x;
             rbdata.last_y = rbdata.start_y = event->xbutton.y;
             rbdata.points[1].x = temp_to_pix(sndg[i][3], sndg[i][1]);
             rbdata.points[1].y = yy;

             if ( i == 0 )
               {
               rbdata.points[0].x = rbdata.points[1].x;
               rbdata.points[0].y = rbdata.points[1].y;
               rbdata.points[2].x = temp_to_pix(sndg[i+1][3],sndg[i+1][1]);
               rbdata.points[2].y =  pres_to_pix(sndg[i+1][1]);
               rbdata.ilev = 0;
               }
             else if ( i == numlvl -1 )
               {
               rbdata.points[2].x = rbdata.points[1].x;
               rbdata.points[2].y = rbdata.points[1].y;
               rbdata.points[0].x = temp_to_pix(sndg[i-1][3],sndg[i-1][1]);
               rbdata.points[0].y =  pres_to_pix(sndg[i-1][1]);
               rbdata.ilev = 1;
               }
             else
               {
               rbdata.points[0].x = temp_to_pix(sndg[i-1][3],sndg[i-1][1]);
               rbdata.points[0].y =  pres_to_pix(sndg[i-1][1]);
               rbdata.points[2].x = temp_to_pix(sndg[i+1][3],sndg[i+1][1]);
               rbdata.points[2].y =  pres_to_pix(sndg[i+1][1]);
               rbdata.ilev = 2;
               }
             }
          else
             {
             /* ----- Edit Dew Points ----- */
             rbdata.itype = 1;
             rbdata.i = i = grab_level( pres );
             if(!qc(sndg[i][4])) { sndg[i][4] = i_dwpt(sndg[i][1]); }

             /* ----- Move mouse to starting spot ----- */
             rbdata.yy = yy = pres_to_pix(sndg[i][1]);
             XWarpPointer ( XtDisplay(draw_reg), XtWindow(draw_reg),
                             XtWindow(draw_reg), skv.tlx, skv.tly,
                             skv.brx+skv.tlx, skv.bry+skv.tly,
                             temp_to_pix(sndg[i][4], sndg[i][1]), yy );
             rbdata.last_x = rbdata.start_x = event->xbutton.x;
             rbdata.last_y = rbdata.start_y = event->xbutton.y;
             rbdata.points[1].x = temp_to_pix(sndg[i][4],sndg[i][1]);
             rbdata.points[1].y = yy;
             if ( i == 0 )
               {
               rbdata.points[0].x = rbdata.points[1].x;
               rbdata.points[0].y = rbdata.points[1].y;
               rbdata.points[2].x = temp_to_pix(sndg[i+1][4],sndg[i+1][1]);
               rbdata.points[2].y =  pres_to_pix(sndg[i+1][1]);
               rbdata.ilev = 0;
               }
             else if ( i == numlvl -1 )
               {
               rbdata.points[2].x = rbdata.points[1].x;
               rbdata.points[2].y = rbdata.points[1].y;
               rbdata.points[0].x = temp_to_pix(sndg[i-1][4],sndg[i-1][1]);
               rbdata.points[0].y =  pres_to_pix(sndg[i-1][1]);
               rbdata.ilev = 1;
               }
             else
               {
               rbdata.points[0].x = temp_to_pix(sndg[i-1][4],sndg[i-1][1]);
               rbdata.points[0].y =  pres_to_pix(sndg[i-1][1]);
               rbdata.points[2].x = temp_to_pix(sndg[i+1][4],sndg[i+1][1]);
               rbdata.points[2].y =  pres_to_pix(sndg[i+1][1]);
               rbdata.ilev = 2;
               }
             }
           }
           else if ( mode == 2 )
           {
           pix_to_hodo( (short)event->xbutton.x,(short)event->xbutton.y,
                        &st_dir, &st_spd );
           redraw_graph( mode );
           show_page( pagenum );
           XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
                       gc, 0, 0, xwdth, xhght, 0, 0 );
           hodo_cursor_data( (short) event->xbutton.x,
                             (short) event->xbutton.y );
           }

           XFlush ( XtDisplay ( draw_reg ) );

           }
         else if(event->xbutton.button == 3)
           {
           switch ( sounding_type )
             {
             case 0:
               XtManageChild(load_sharp);
             break;

             case 1:
                 redisplay = 1;
                 strcpy ( gemdevice, "maptop" );
                 gslwin(gemdevice, &iret, strlen(gemdevice));
                 show_gem_info (w);
             break;

             case 2:
                 redisplay = 1;
                 strcpy( gemdevice, "maptop2");
                 gslwin(gemdevice, &iret, strlen(gemdevice));
                 show_model_info (w);
             break;

             case 3:
                 redisplay = 1;
                 strcpy( gemdevice, "maptop3");
                 gslwin(gemdevice, &iret, strlen(gemdevice));
                 show_pfs_model_info (w);
             break;

             case 4:
                 redisplay = 1;
                 show_gem_info (w);
             break;

             case 5:
                  show_profiler_vad_info(w);
             break;

             case 6:
                 redisplay = 1;
                 show_acars_info (w);
             break;


             }
           }
        }

        /*NP*/
        void pointer_update (Widget w, XtPointer *call_data, XEvent *event)
        /*************************************************************/
        /* POINTER_UPDATE                                            */
        /*                                                           */
        /* Handles "mouse moved w/ no buttons" event                 */
        /*************************************************************/
        {

        /* ----- Update Cursor Data when mouse is moved ----- */
        set_font(2);
        if (mode == 1 || mode == 6)
           {
           if ((event->xbutton.x < skv.brx) && (event->xbutton.y < skv.bry))
              skewt_cursor_data((short)event->xbutton.x,
                                (short)event->xbutton.y);
           }
        else if ( mode == 2 )
           {
           if ((event->xbutton.x < hov.brx) && (event->xbutton.y < hov.bry))
              hodo_cursor_data((short)event->xbutton.x,
                               (short)event->xbutton.y);
           }
        else if ( mode == 4 || mode == 5)
           {
           if ((event->xbutton.x < skv.brx) && (event->xbutton.y < skv.bry))
              icing_turb_cursor_data((short)event->xbutton.x,
                                     (short)event->xbutton.y);
           }
        XFlush (XtDisplay(draw_reg));
        }

        /*NP*/
        void update_pointer (Widget w, XtPointer *call_data, XEvent *event)
        /*************************************************************/
        /* UPDATE_POINTER                                            */
        /*                                                           */
        /* Handles "mouse moved while button #1 was depressed" event */
        /*************************************************************/
        {
        if (!qc(i_temp(700))) return;

        if ( mode == 1 )
           {
           XDrawLine( XtDisplay(w), XtWindow(w), rbdata.gc,
              rbdata.points[0].x, rbdata.points[0].y,
              rbdata.points[1].x, rbdata.points[1].y );
           XDrawLine( XtDisplay(w), XtWindow(w), rbdata.gc,
              rbdata.points[1].x, rbdata.points[1].y,
              rbdata.points[2].x, rbdata.points[2].y );

           rbdata.last_x  = event->xbutton.x;
           rbdata.last_y  = event->xbutton.y;
           rbdata.points[1].x = event->xbutton.x;

           XDrawLine( XtDisplay(w), XtWindow(w), rbdata.gc,
              rbdata.points[0].x, rbdata.points[0].y,
              rbdata.points[1].x, rbdata.points[1].y );
           XDrawLine( XtDisplay(w), XtWindow(w), rbdata.gc,
              rbdata.points[1].x, rbdata.points[1].y,
              rbdata.points[2].x, rbdata.points[2].y );

           set_font( 2 );
           skewt_cursor_data((short)event->xbutton.x,
                             (short)rbdata.points[1].y );

           }
        else if ( mode == 2 )
           {
           pix_to_hodo( (short)event->xbutton.x,(short)event->xbutton.y,
                         &st_dir, &st_spd );

           /* ----- Display Hodograph Inset ----- */
           draw_hoinset();

           set_font( 2 );
           hodo_cursor_data( (short) event->xbutton.x,
                             (short) event->xbutton.y );
           }

        XFlush ( XtDisplay ( draw_reg ) );
        }



        /*NP*/
        void redraw_sounding (Widget w, XtPointer *call_data, XEvent *event )
       /*************************************************************/
       /* REDRAW_SOUNDING                                           */
       /*************************************************************/
        {
        float   chg1;
        short   i;

        if (!qc(i_temp(700))) return;

        if(event->xbutton.button != 1 ) return;

        /* Redraw previous line */

        if ( mode == 1 )
          {
          XDrawLine( XtDisplay(w), XtWindow(w), rbdata.gc,
              rbdata.points[0].x,
              rbdata.points[0].y,
              rbdata.points[1].x,
              rbdata.points[1].y );
          XDrawLine( XtDisplay(w), XtWindow(w), rbdata.gc,
              rbdata.points[1].x,
              rbdata.points[1].y,
              rbdata.points[2].x,
              rbdata.points[2].y );

          if ( ! rbdata.itype )
            {
            chg1 = pix_to_temp( (short)event->xbutton.x, rbdata.yy );
            if(chg1 < i_dwpt(sndg[rbdata.i][1]))
              {
              chg1 = i_dwpt(sndg[rbdata.i][1]);
              }
            sndg[rbdata.i][3] = chg1;
            redraw_graph( mode );
            define_parcel( current_parcel, user_level);
            show_parcel ();
            show_page( pagenum );
            }
          else
            {
            chg1 = pix_to_temp( (short)event->xbutton.x, rbdata.yy );
            if(chg1 > i_temp(sndg[rbdata.i][1]))
              {
              chg1 = i_temp(sndg[rbdata.i][1]);
              }
            sndg[rbdata.i][4] = chg1;
            redraw_graph( mode );
            define_parcel( current_parcel, user_level);
            show_parcel ();
            show_page( pagenum );
            }
          rbdata.points[0].x = rbdata.points[1].x =
                                rbdata.points[2].x = 0;
          rbdata.points[0].y = rbdata.points[1].y =
                                rbdata.points[2].y = 0;
          XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
                      gc, 0, 0, xwdth, xhght, 0, 0 );
          }
        else if ( mode == 2 )
          {
          pix_to_hodo( (short)event->xbutton.x,(short)event->xbutton.y,
                        &st_dir, &st_spd );
          redraw_graph( mode );
          show_page( pagenum );
          XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
                      gc, 0, 0, xwdth, xhght, 0, 0 );
          hodo_cursor_data( (short) event->xbutton.x,
                            (short) event->xbutton.y );
          XFlush ( XtDisplay ( draw_reg ) );
          }

        }

        /*NP*/
        void tog_graph (Widget w, XmDrawingAreaCallbackStruct *call_data)
       /*************************************************************/
       /* TOG_GRAPH                                                 */
       /*************************************************************/
        {
          if ( mode != 3 )
          {
          mode = switch_modes( mode );
          if ( mode == 1 )
            {
            XtVaSetValues(w, XmNlabelString,
                          XmStringCreateLocalized("Hodograph"), NULL );
            }
          else if ( mode == 2 )
            {
            XtVaSetValues(w, XmNlabelString,
                          XmStringCreateLocalized("Skewt"), NULL );
            }
          XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
                      gc, 0, 0, xwdth, xhght, 0, 0 );
          }
        }

        /*NP*/
        void display_SKEWT (Widget w, XmDrawingAreaCallbackStruct *call_data)
        /*******************************************************************/
        /* DISPLAY_SKEWT                                                   */
        /*******************************************************************/
        {
           pagenum = 1;
           clear_paramarea();
           draw_skewt();
           show_page( pagenum );
           mode = 1;
           XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
                    gc, 0, 0, xwdth, xhght, 0, 0 );
        }

        /*NP*/
        void display_HODO (Widget w, XmDrawingAreaCallbackStruct *call_data)
        /*******************************************************************/
        /* DISPLAY_HODO                                                    */
        /*******************************************************************/
        {
           pagenum = 2;
           clear_paramarea();
           draw_hodo();
           show_page( pagenum );
           mode = 2;
           XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
                    gc, 0, 0, xwdth, xhght, 0, 0 );
        }

        /*NP*/

        void display_ICG(Widget w, XmDrawingAreaCallbackStruct *call_data)
        /****************************************************************/
        /* DISPLAY_ICG                                                  */
        /****************************************************************/
        {
          draw_ICG();
          mode=4;

          XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
            gc, 0, 0, xwdth, xhght, 0, 0 );

        }

        /*NP*/
        void display_TURB(Widget w, XmDrawingAreaCallbackStruct *call_data)
        /*****************************************************************/
        /* DISPLAY_TURB                                                  */
        /*****************************************************************/
        {
          draw_TURB();
          mode=5;

          XCopyArea (XtDisplay(draw_reg),canvas, XtWindow(draw_reg),
            gc, 0, 0, xwdth, xhght, 0, 0 );
        }

        /*NP*/
        void display_Clouds(Widget w, XmDrawingAreaCallbackStruct *call_data)
        /*******************************************************************/
        /* DISPLAY_CLOUDS                                                  */
        /*******************************************************************/
        {
          draw_Clouds();
          mode=6;
          XCopyArea (XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
            gc, 0, 0, xwdth, xhght, 0, 0 );

        }

        /*NP*/
        void menu_main (Widget w, XmDrawingAreaCallbackStruct *call_data)
       /*************************************************************/
       /* MAIN_MENU                                                 */
       /*************************************************************/
        {
        exit(0);
        }


        /*NP*/
        void page_next (Widget w, XmDrawingAreaCallbackStruct *call_data)
       /*************************************************************/
       /* PAGE_NEXT                                                 */
       /*************************************************************/
        {
        if (!qc(i_temp(700))) return;

        if ( mode != 3 )
        {
          pagenum += 1;
          if(pagenum == 5) pagenum = mode;
          show_page( pagenum );
          XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
                      gc, 0, 0, xwdth, xhght, 0, 0 );
        }
        }


        /*NP*/
        char * itoa ( int value, char *st, int radx )
       /*************************************************************/
       /* ITOA                                                      */
       /*************************************************************/
        {
        sprintf ( st, "%d", value );
        return st;
        }


        /*NP*/
        void outtext ( char *st, int x, int y )
       /*************************************************************/
       /* OUTTEXT                                                   */
       /*************************************************************/
          {
          y = y + font_struct->ascent;
          XDrawImageString( XtDisplay(draw_reg),XtWindow(draw_reg), gc,
                            x, y, st, strlen (st) );
          }


        /*NP*/
        void outgtext ( char *st, int x, int y )
       /*************************************************************/
       /* OUTGTEXT                                                  */
       /*************************************************************/
          {
           y = y + font_struct->ascent;
          XDrawString( XtDisplay(draw_reg), canvas, gc, x, y, st,
                       strlen (st) );
          }


        /*NP*/
        void Toggle_Callback( Widget w, int which, caddr_t call)
       /*************************************************************/
       /* TOGGLE_CALLBACK                                           */
       /*************************************************************/
        {
          static int lastpick=-1;

          XmToggleButtonCallbackStruct    *state =
                                (XmToggleButtonCallbackStruct *) call;

          XtUnmanageChild( XtParent(XtParent(XtParent(XtParent(w)))));
          if ( which == 6 )
            return;
          if((lastpick >= 0)&&(lastpick != which))
             XtVaSetValues(rbutton[lastpick],XmNset,False,NULL);
          else
             XtVaSetValues(rbutton[which],XmNset,True,NULL);

          current_parcel = which+1;
          define_parcel( (short)(which+1), user_level );
          redraw_graph(1);
          show_parcel();
          XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
              gc, 0, 0, xwdth, xhght, 0, 0 );
          XFlush ( XtDisplay(draw_reg) );
          lastpick = which;
        }


        /*NP*/
        GC xs_create_xor_gc(Widget w, char *color)
       /*************************************************************/
       /* XS_CREATE_XOR_GC                                          */
       /*************************************************************/
        {
        XGCValues     values;
        GC            gc;

        XtVaGetValues(w, XtNbackground, &values.background, NULL);

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
        values.function   = GXxor;

        gc = XtGetGC(w, GCForeground | GCBackground | GCFunction | GCLineStyle,
                &values);

        return gc;
        }



        /*NP*/
        void load_cb ( Widget w, XtPointer client_data,
                         XtPointer call_data )
       /*************************************************************/
       /* LOAD_CB                                                   */
       /*************************************************************/
        {
        int             i, newlev, item_no = (int) client_data;
        XmString        title;
        XmStringTable   str_list = NULL;

        sounding_type = item_no;
        /* Blacken Out Widget at Bottom of Screen if necessary*/
        if (g_raobModelSW != 0) {
          XSetForeground(XtDisplay(time_reg), gc2, pixels[0] );
          XFillRectangle (XtDisplay(time_reg), time_window, gc2, 0, 0,
                        600, g_twdth );
          XCopyArea(XtDisplay(time_reg), time_window, XtWindow(time_reg),
                 gc2, 0, 0, 600, g_twdth, 0, 0);
          XSetForeground(XtDisplay(time_oper_reg), gc3, pixels[0] );
          XFillRectangle(XtDisplay(time_oper_reg),time_oper_window, gc3, 0, 0,
                        100, 40);
          XCopyArea(XtDisplay(time_oper_reg), time_oper_window, XtWindow(time_oper_reg),
                 gc3, 0, 0, 100, 40, 0, 0);
          g_raobModelSW=0;
        }
        plotData.mode = EMPTY;
        g_pfs_model_sel_sw=0;

        if ( item_no == 0 )
           {
           if ( ! load_sharp )
             {
               title = XmStringCreateLocalized( "Archive File Selection" );
               load_sharp = XmCreateFileSelectionDialog ( toplevel,
                           "sharp_sel", NULL, 0 );
               XtAddCallback ( load_sharp, XmNokCallback, sharp_load,
                            NULL );
               XtAddCallback ( load_sharp, XmNcancelCallback,
                            (XtCallbackProc)XtUnmanageChild, NULL );
               XtAddCallback ( load_sharp, XmNokCallback,
                            (XtCallbackProc)XtUnmanageChild, NULL );
               XtVaSetValues( load_sharp, XmNdialogTitle, title, NULL);
             }
           XtManageChild(load_sharp);
           }
        else if (item_no == 1)
           {
           mode = 1;
           redisplay = 0;
           show_gem_info(w);
           XtSetSensitive( raob_btn, True );
           XtSetSensitive( tamdar_btn, True );
           XtSetSensitive( prof_btn, False );
           XtSetSensitive( vad_btn, False );
           XtSetSensitive( xsection_hght, False );
           item_type = 1;

           gemfile_times ();

           gemsoundtime[0] = '\0';

           Load_stationlist ( station_list );
           }
        else if (item_no == 2 )
           {
           mode = 1;
           redisplay = 0;
           if (mdlfile_timelist != NULL)
             XmListDeselectAllItems(mdlfile_timelist);
           if (pfsmdlfile_timelist != NULL) {
             /* Deselect and erase all items to fix bug with xfr between
             model soundings and point forecast soundings dialogue boxes.
             This forces forecaster to have to reload time list on switch,
             but this fixes problem with possible labelling problems
             showing up on switch over to point forecast soundings */
             XmListDeselectAllItems(pfsmdlfile_timelist);
             XmListDeleteAllItems(pfsmdlfile_timelist);
           }
           show_model_info (w);
           draw_model_map();
           }
        else if (item_no == 3 )
           {
           mode = 1;
           redisplay = 0;
           if (pfsmdlfile_timelist != NULL) {
             XmListDeselectAllItems(pfsmdlfile_timelist);
           }
           if (mdlfile_timelist != NULL) {
             /* Deselect and erase all items to fix bug with xfr between
             point forecast soundings and model soundings dialogue boxes.
             This forces forecaster to have to reload time list on switch,
             but this fixes problem with no soundings showing up on switch
             over to regular model data */
             XmListDeselectAllItems(mdlfile_timelist);
             XmListDeleteAllItems(mdlfile_timelist);
           }
           show_pfs_model_info (w);
           draw_model_map();
           }
        else if (item_no == 4 )
           {
           mode = 3;
           redisplay = 0;
           show_gem_info (w);
           XtSetSensitive( raob_btn, False );
           XtSetSensitive( tamdar_btn, False );
           XtSetSensitive( prof_btn, True );
           XtSetSensitive( vad_btn, True );
           XtSetSensitive( xsection_hght, True );
           item_type = 2;

           XmListDeselectAllItems (gemfile_timelist);
           XtVaSetValues ( gemfile_timelist,
                                 XmNitemCount, 0,
                                 XmNitems, str_list,
                                 NULL );
           gemfile_times ();

           gemsoundtime[0] = '\0';

           Load_stationlist ( station_list );
           }
        else if (item_no == 5 )
          {
          mode=4;
          show_profiler_vad_info(w);
          }
        else if (item_no == 6)
           {
           mode = 1;
           redisplay = 0;
           show_acars_info (w);

           }
        }


        /*NP*/
        void about_cb (Widget w, XtPointer client_data, XtPointer call_data)
       /*************************************************************/
       /* ABOUT_CB                                                  */
       /*                                                           */
       /* Displays ABOUT message                                    */
       /*************************************************************/
        {
        static Widget   aboutBox, temp;
        XmString        msg, title;

        if (!aboutBox)
           {
           aboutBox = XmCreateMessageDialog( toplevel, "aboutBox", NULL, 0);
           title = XmStringCreateLocalized( "About SHARP version 3.00" );
           msg   = XmStringCreateLocalized( "Welcome to:" );
           XtVaSetValues( aboutBox,
                        XmNdialogTitle, title,
                        NULL);
           /*
           XmStringFree( title );
           XmStringFree( msg );
           */

           /* ----- Turn off CANCEL and HELP buttons ----- */
           temp = XmMessageBoxGetChild( aboutBox, XmDIALOG_CANCEL_BUTTON);
           XtUnmanageChild(temp);
           temp = XmMessageBoxGetChild( aboutBox, XmDIALOG_HELP_BUTTON);
           XtUnmanageChild(temp);
           }

        XtManageChild(aboutBox);
        }




        /*NP*/
        void sharp_load ( Widget w, XtPointer client_data,
                               XtPointer call_data )
       /*************************************************************/
       /* SHARP_LOAD                                                */
       /*************************************************************/
             {
             char *file = NULL, filename[200];
             XmFileSelectionBoxCallbackStruct *cbs =
              (XmFileSelectionBoxCallbackStruct *) call_data;

             if (cbs)
               {
               if ( !XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG,
                                      &file) )
                  return;

                strcpy ( filename, config.filename);
                strcpy ( config.filename, file );
                XtFree (file);
                if ( get_sndg() )
                  {
                  printf("\n Error reading file %s", config.filename );
                  strcpy ( config.filename, filename );
                  return;
                  }
               mode = 1;

               resize_callback ( draw_reg, (XtPointer)NULL,
                                 (XtPointer)NULL );

               define_parcel( current_parcel, user_level );
               show_parcel ();
               pagenum = 1;
               show_page( pagenum );
               }
             }


        /*NP*/
        void show_gem_info ( Widget w )
       /*************************************************************/
       /* SHOW_GEM_INFO                                             */
       /*                                                           */
       /* Display Observed Sounding Map and get user selection.     */
       /*************************************************************/
        {

        static Widget   gemform, gemform2, gempane,
                        gemfile_label,
                        gemfile_load, gemfile_ok, gemfile_cancel,
                        gemfile_world, gemfile_US, gemfile_TROP_SFC,
                        gemfile_OFAGX, gemfile_ATL, gemfile_PAC,
                        gemfile_icemap, gemfile_stabmap,
                        gemfile_zoom, gemfile_unzoom,
                        gemfile_help,
                        gemlbl_time,

                        /*gemlbl_station, gemstn_opt, rc;*/
                        gemstn_opt,rc;
        XmString        str, gem_title, *levels_str, xsection_lbl;
        XmStringTable   str_list = NULL;
        char            *gemouttext, gemdevice[72];
        char            *strp;
        int             i, num, iret=0;
        float           fraction=0.9;

        if ( ! gem_dialog )
           {
           gem_title = XmStringCreateLocalized( "Observed Sounding Selection" );
           gem_dialog = XmCreateBulletinBoardDialog(toplevel, "gem_panel",
                                        NULL, 0);
           XtVaSetValues( gem_dialog, XmNdialogTitle, gem_title, NULL);
           /* XmStringFree(gem_title); */

           gempane = XtVaCreateManagedWidget("parcel_pane",
                                xmPanedWindowWidgetClass,
                                gem_dialog,
                                XmNsashWidth, 1,
                                XmNsashHeight, 1,
                                NULL);

           gemform = XtVaCreateWidget("form", xmFormWidgetClass,
                                gempane, XmNfractionBase, 8,
                                NULL );

           str = XmStringCreateLocalized ("GEMPAK sounding file:");
           gemfile_label = XtVaCreateManagedWidget ("gemfile_label",
                                xmLabelWidgetClass, gemform,
                                XmNlabelString, str,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 0,
                                XmNtopAttachment, XmATTACH_FORM,
                                XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 2,
                                NULL );
           /* XmStringFree (str); */

         rc = XtVaCreateWidget ("rowcol", xmRowColumnWidgetClass, gemform,
                                XmNtopAttachment,       XmATTACH_FORM,
                                XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 8,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 4,
                                XmNentryAlignment, XmALIGNMENT_END,
                                XmNpacking, XmPACK_COLUMN,
                                XmNorientation, XmHORIZONTAL,
                                NULL );

        xsection_lbl  = XmStringCreateLocalized ("Timesection Height(Ft):");

        num = XtNumber(levellist);
        levels_str = (XmString *)XtMalloc( num * sizeof(XmString));
        for (i=0; i < num; i++)
           levels_str[i] = XmStringCreateLocalized(levellist[i]);

        xsection_hght = XmVaCreateSimpleOptionMenu (rc, "option_menu",
                        xsection_lbl, 'S', 8, xsection_hght_cb,
                        XmVaPUSHBUTTON, levels_str[0],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[1],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[2],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[3],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[4],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[5],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[6],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[7],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[8],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[9],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[10], NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[11], NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[12], NULL, NULL, NULL,
                        NULL );

        XtManageChild(xsection_hght);

/*
        for (i=0; i < num; i++)
           XmStringFree ( levels_str[i] );
*/

        XtManageChild(rc);

           gemfile_text = XtVaCreateManagedWidget ("gemtext",
                                xmTextFieldWidgetClass, gemform,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 0,
                                XmNtopAttachment, XmATTACH_WIDGET,
                                XmNtopWidget, rc,
                                XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 3,
                                NULL );

            XtAddCallback ( gemfile_text, XmNactivateCallback,
                              get_gemfile_text, NULL );

            gemfile_load = XtVaCreateManagedWidget ("Change File",
                                xmPushButtonWidgetClass, gemform,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 3,
                                XmNtopAttachment, XmATTACH_WIDGET,
                                XmNtopWidget, rc,
                                XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 4,
                                NULL );

            XtAddCallback ( gemfile_load, XmNactivateCallback,
                              load_gemfile, NULL );

            raob_btn = XtVaCreateManagedWidget ("RAOB",
                                xmPushButtonWidgetClass, gemform,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 4,
                                XmNtopAttachment, XmATTACH_WIDGET,
                                XmNtopWidget, rc,
                                XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 5,
                                NULL );

            XtAddCallback ( raob_btn, XmNactivateCallback,
                              set_gempak_file, (XtPointer)0 );
            tamdar_btn = XtVaCreateManagedWidget ("TAMDAR",
                                xmPushButtonWidgetClass, gemform,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 5,
                                XmNtopAttachment, XmATTACH_WIDGET,
                                XmNtopWidget, rc,
                                XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 6,
                                NULL );
            
            XtAddCallback ( tamdar_btn, XmNactivateCallback,
                              set_gempak_file, (XtPointer)1 );

            prof_btn = XtVaCreateManagedWidget ("PROFILER",
                                xmPushButtonWidgetClass, gemform,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 6,
                                XmNtopAttachment, XmATTACH_WIDGET,
                                XmNtopWidget, rc,
                                XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 7,
                                NULL );

            XtAddCallback ( prof_btn, XmNactivateCallback,
                              set_gempak_file, (XtPointer)2 );

            vad_btn = XtVaCreateManagedWidget ("VADWIND",
                                xmPushButtonWidgetClass, gemform,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 7,
                                XmNtopAttachment, XmATTACH_WIDGET,
                                XmNtopWidget, rc,
                                XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 8,
                                NULL );

            XtAddCallback ( vad_btn, XmNactivateCallback,
                              set_gempak_file, (XtPointer)3 );

            str = XmStringCreateLocalized ("Sounding times:");
            gemlbl_time = XtVaCreateManagedWidget ("gemfile_time",
                                xmLabelWidgetClass, gemform,
                                XmNlabelString, str,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 0,
                                XmNtopAttachment, XmATTACH_POSITION,
                                XmNtopPosition, 1,
                                XmNalignment, XmALIGNMENT_BEGINNING,
                                XmNbottomPosition, 2,
                                NULL );
            /* XmStringFree (str); */

            gemfile_timelist = XmCreateScrolledList ( gemform,
                                "gemtimes", NULL, 0 );

            XtVaSetValues ( gemfile_timelist,
                        XmNvisibleItemCount, 14, NULL );
            XtVaSetValues (XtParent(gemfile_timelist),
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, gemfile_text,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 2,
                        NULL );

             XtAddCallback ( gemfile_timelist,
                        XmNbrowseSelectionCallback,
                        time_select_cb,
                        NULL );

             XtManageChild ( gemfile_timelist );

             gemlbl_station = XtVaCreateManagedWidget("map",
                        xmDrawingAreaWidgetClass, gemform,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, gemfile_text,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, XtParent(gemfile_timelist),
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNwidth, 560,
                        XmNbackground, pixels[0],
                        NULL);

              XtAddCallback( gemlbl_station, XmNexposeCallback,
                        mapw_exposeCb, NULL );
                        /* plotData.mode = STNSELECT;*/
              XtAddEventHandler( gemlbl_station, ButtonPressMask,
                        FALSE, mapw_pickstnCb, NULL );

              XtManageChild ( gemform );

              gemform2 = XtVaCreateWidget("form", xmFormWidgetClass,
                        gempane, XmNfractionBase, 10,
                        XmNwidth, 980, XmNheight, 60,
                        NULL );
              gemfile_cancel = XtVaCreateManagedWidget ("CANCEL",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 1,
                        NULL );
              XtAddCallback(gemfile_cancel, XmNactivateCallback,
                        (XtCallbackProc)gem_info_cancel_cb, NULL);
              gemfile_world = XtVaCreateManagedWidget ("WORLD",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 1,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_POSITION,
                        XmNbottomPosition, 5,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 2,
                        NULL );
              XtAddCallback(gemfile_world, XmNactivateCallback,
                        (XtCallbackProc)gem_info_world_cb,NULL);
              gemfile_US = XtVaCreateManagedWidget ("U.S.",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 1,
                        XmNtopAttachment, XmATTACH_POSITION,
                        XmNtopPosition, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 2,
                        NULL );
              XtAddCallback(gemfile_US, XmNactivateCallback,
                        (XtCallbackProc)gem_info_US_cb,NULL);
              gemfile_TROP_SFC = XtVaCreateManagedWidget("TROPSFC",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 2,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_POSITION,
                        XmNbottomPosition, 5,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 3,
                        NULL );
               XtAddCallback(gemfile_TROP_SFC, XmNactivateCallback,
                        (XtCallbackProc)gem_info_TROP_SFC_cb,NULL);
               gemfile_OFAGX = XtVaCreateManagedWidget("OFAGX",
                           xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 3,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_POSITION,
                        XmNbottomPosition, 5,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 4,
                        NULL );
               XtAddCallback(gemfile_OFAGX, XmNactivateCallback,
                        (XtCallbackProc)gem_info_OFAGX_cb,NULL);
               gemfile_ATL = XtVaCreateManagedWidget("ATLANTIC",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 2,
                        XmNtopAttachment, XmATTACH_POSITION,
                        XmNtopPosition, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 3,
                        NULL );
               XtAddCallback(gemfile_ATL, XmNactivateCallback,
                        (XtCallbackProc)gem_info_ATL_cb, NULL);

               gemfile_PAC = XtVaCreateManagedWidget("PACIFIC",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 3,
                        XmNtopAttachment, XmATTACH_POSITION,
                        XmNtopPosition, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 4,
                        NULL );
               XtAddCallback(gemfile_PAC, XmNactivateCallback,
                        (XtCallbackProc)gem_info_PAC_cb, NULL);
               gemfile_stabmap= XtVaCreateManagedWidget("STBLTY\nMAP",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 6,
                        NULL );
               XtAddCallback(gemfile_stabmap, XmNactivateCallback,
                        (XtCallbackProc)gem_info_STABMAP_cb,NULL);

               gemfile_icemap = XtVaCreateManagedWidget("ICEMAP",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 6,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 7,
                        NULL );
               XtAddCallback(gemfile_icemap, XmNactivateCallback,
                        (XtCallbackProc)gem_info_ICEMAP_cb,NULL);


               gemfile_zoom = XtVaCreateManagedWidget("ZOOM",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 7,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 8,
                        NULL );
               XtAddCallback(gemfile_zoom, XmNactivateCallback,
                        (XtCallbackProc)gem_info_zoom_cb, NULL);
               gemfile_unzoom = XtVaCreateManagedWidget("UNZOOM",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 8,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 9,
                        NULL );
               XtAddCallback(gemfile_unzoom, XmNactivateCallback,
                        (XtCallbackProc)gem_info_unzoom_cb, NULL);


               gemfile_help = XtVaCreateManagedWidget ("HELP",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 9,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 10,
                        NULL );

               XtManageChild ( gemform2 );
               XtManageChild ( gempane );
               XtManageChild ( gem_dialog );

               mapw_rgstr(  gemlbl_station );

               if ( redisplay == 0 )
                 {
                 XtAddCallback( gemlbl_station, XmNresizeCallback,
                                mapw_resizeCb, NULL );
                 }
               XtVaGetValues ( gemfile_text, XmNvalue, &gemouttext,
                             NULL );

               /*
                *  Parse out the three different sounding default files
                */
               gemsoundfile[0] = '\0';
               strp = strtok ( gemouttext, "+" );
               if ( strp )
                 {
                 strcpy ( gemsoundraob, strp );
                 strp = strtok ( '\0', "+" );
                 if ( strp )
                   {
                   strcpy ( gemsoundtamdar, strp );
                   strp = strtok ( '\0', "+" );
                   if ( strp )
                     {
                     strcpy ( gemsoundprof, strp );
                     strp = strtok ( '\0', "+" );
                     if ( strp )
                       {
                       strcpy ( gemsoundvad, strp );
                       }
                     }
                   }
                 }

               if ( mode != 3 )
                 {
                 if ( strlen(gemsoundraob) == 0 )
                   gemsoundfile[0] = '\0';
                 else
                   strcpy ( gemsoundfile, gemsoundraob );

                 gemfile_times ();
                 if ( gemsoundfile[0] != '\0' )
                   {
                   gemfile_times ();
                   }
                 else
                   {
                   XmListDeselectAllItems (gemfile_timelist);
                   XtVaSetValues ( gemfile_timelist,
                                 XmNitemCount, 0,
                                 XmNitems, str_list,
                                 NULL );
                   }

                 Load_stationlist ( 0 );

                 }
#ifndef LESSTIF_VERSION
                 XtFree ( gemouttext );
#endif
                 XtVaSetValues ( gemfile_text, XmNvalue, gemsoundfile,
                                 NULL );

               }
             else
               {
               if ( mode != 3 && redisplay == 0 )
                 {

                 if ( strcmp ( gemsoundraob, gemsoundfile ) != 0 )
                  {
                  XmListDeselectAllItems (gemfile_timelist);
                  XtVaSetValues ( gemfile_timelist,
                                 XmNitemCount, 0,
                                 XmNitems, str_list,
                                 NULL );
                  }

                 if ( strlen(gemsoundraob) == 0 )
                   gemsoundfile[0] = '\0';
                 else
                   strcpy ( gemsoundfile, gemsoundraob );

                 gemfile_times ();
                 if ( gemsoundfile[0] != '\0' )
                   {
                   gemfile_times ();
                   }
                 else
                   {
                   XmListDeselectAllItems (gemfile_timelist);
                   XtVaSetValues ( gemfile_timelist,
                                 XmNitemCount, 0,
                                 XmNitems, str_list,
                                 NULL );
                   }

                 if ( mode == 1 && redisplay == 0 )
                   {
                   resize_callback ( draw_reg, (XtPointer)NULL,
                                               (XtPointer)NULL );
                   }

                 }
               else
                 {
                 if ( redisplay == 0 )
                   {
                   gemsoundfile[0] = '\0';
                   XmListDeselectAllItems (gemfile_timelist);
                   XtVaSetValues ( gemfile_timelist,
                                   XmNitemCount, 0,
                                   XmNitems, str_list,
                                   NULL );
                   }
                 }
               XtVaSetValues ( gemfile_text, XmNvalue, gemsoundfile,
                               NULL );
               }


             strcpy ( gemdevice, "maptop" );
             gslwin(gemdevice, &iret, strlen(gemdevice));

             XtManageChild ( gem_dialog );
             }

        /*NP*/
        void show_profiler_vad_info(Widget w)
        /***************************************************/
        /* SHOW_PROFILER_VAD_INFO (LJH)                    */
        /***************************************************/
        {
        static Widget pvform, pvform2, pvfile_label,pvfile_cancel,pvpane,pv_exit,pv_update,rc;
        XmString str,pv_title, *levels_str, psection_lbl;
        XmStringTable str_list = NULL;
        char *gemouttext, gemdevice[72],proffile_work[200];
        char *proffile;
        char            *strp;
        int i, ncnt, num, iret;
        char *file_dir, file[200];
        char *str_type[20];
        XrmValue value;
        if ( ! pv_dialog )
           {
           pv_title= XmStringCreateLocalized("Profiler/Vad Selection");
           pv_dialog=XmCreateBulletinBoardDialog(toplevel, "pv_panel",
                                                 NULL, 0);
           XtVaSetValues( pv_dialog, XmNdialogTitle, pv_title, NULL);
           /* XmStringFree(pv_title); */

           pvpane = XtVaCreateManagedWidget("profiler_vad_pane",
                                xmPanedWindowWidgetClass,
                                pv_dialog,
                                XmNsashWidth, 1,
                                XmNsashHeight, 1,
                                NULL);
           pvform=XtVaCreateWidget("form",xmFormWidgetClass,
                                   pvpane, XmNfractionBase, 10,
                                   XmNheight, pvs_HGHT+70,
                                   XmNwidth, pvs_WDTH+VP_WDTH+115,
                                   NULL); /* Change 7 to 10 */

           str=XmStringCreateLocalized("Profiler file:");
           pvfile_label = XtVaCreateManagedWidget ("pvfile_label",
                           xmLabelWidgetClass, pvform,
                           XmNlabelString, str,
                           XmNleftAttachment, XmATTACH_POSITION,
                           XmNleftPosition, 0,
                           XmNtopAttachment, XmATTACH_FORM,
                           XmNrightAttachment, XmATTACH_POSITION,
                           XmNrightPosition, 1,
                           NULL );
           /* XmStringFree(str); */
           rc = XtVaCreateWidget ("rowcol", xmRowColumnWidgetClass, pvform,
                                XmNtopAttachment,       XmATTACH_FORM,
                                XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 7,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 4,
                                XmNentryAlignment, XmALIGNMENT_END,
                                XmNpacking, XmPACK_COLUMN,
                                XmNorientation, XmHORIZONTAL,
                                NULL );
           psection_lbl = XmStringCreateLocalized ("Plane section Height(Ft):");
           num = XtNumber(levellist);
           levels_str = (XmString *)XtMalloc( num * sizeof(XmString));
           for (i=0; i < num; i++)
             levels_str[i] = XmStringCreateLocalized(levellist[i]);
             psection_hght = XmVaCreateSimpleOptionMenu (rc, "option_menu",
                        psection_lbl, 'S', 1, psection_hght_cb,
                        XmVaPUSHBUTTON, levels_str[0],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[1],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[2],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[3],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[4],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[5],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[6],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[7],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[8],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[9],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[10], NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[11], NULL, NULL, NULL,
                        XmVaPUSHBUTTON, levels_str[12], NULL, NULL, NULL,
                        NULL );
           XtManageChild(psection_hght);


           /*
           for (i=0; i < num; i++)
             XmStringFree ( levels_str[i] );
           */

           XtManageChild(rc);
           /* */
           if (XrmGetResource(g_applicationDB,"sharp95*pvfile_timelist.profiler",
             "Sharp95*pvfile_timelist.profiler",str_type,&value)
             == True) {
               strncpy(g_proffile,value.addr,(int) value.size);
               }
           else
             g_proffile[0] = '\0';
           if (XrmGetResource(g_applicationDB,"sharp95*pvfile_timelist.vad",
             "Sharp95*pvfile_timelist.vad",str_type,&value)
             == True) {
               strncpy(g_vadfile,value.addr,(int) value.size);
               }
           else
             g_vadfile[0] = '\0';
           pvfile_timelist = XmCreateScrolledList (pvform, "pvtimes", NULL, 0);
           XtVaSetValues (pvfile_timelist, XmNvisibleItemCount, 15, NULL);
           XtVaSetValues (XtParent(pvfile_timelist),
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, pvfile_label,
                        /*XmNtopPosition, 0,*/
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 1,
                        NULL );
           XtAddCallback( pvfile_timelist,
                        XmNbrowseSelectionCallback,
                        pv_select_cb,
                        NULL);

/*           pv_station=XtVaCreateManagedWidget("map4",
                        xmDrawingAreaWidgetClass, pvform,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, psection_hght,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, XtParent(pvfile_timelist),
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 800,
                        XmNwidth, 1024, XmNbackground, pixels[0],
                        NULL); */

           pv_station=XtVaCreateManagedWidget("map4",
                        xmDrawingAreaWidgetClass, pvform,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, psection_hght,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, XtParent(pvfile_timelist),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, pvs_HGHT,
                        XmNwidth, pvs_WDTH,
                        XmNbackground, pixels[0],
                        NULL);


           pvform2 = XtVaCreateWidget("form", xmFormWidgetClass,
                        pvpane, XmNfractionBase, 10,
                        NULL); /* Change 7 to 10 */
           pv_vertprof=XtVaCreateManagedWidget("pvvert",
                        xmDrawingAreaWidgetClass, pvform,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, psection_hght,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, pv_station,
                        XmNrightAttachment, XmATTACH_FORM,
                       /* XmNbottomAttachment, XmATTACH_FORM,*/
                        XmNheight, VP_HGHT,
                        XmNwidth, VP_WDTH, XmNbackground, pixels[0],
                        NULL);
           pv_hodo=XtVaCreateManagedWidget("pvhodo",
                        xmDrawingAreaWidgetClass, pvform,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, pv_vertprof,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, pv_station,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, HODO_HGHT,
                        XmNwidth, HODO_WDTH, XmNbackground, pixels[0],
                        NULL);
           pv_exit = XtVaCreateManagedWidget ("EXIT",
                        xmPushButtonWidgetClass, pvform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 1,
                        NULL );
           XtAddCallback(pv_exit, XmNactivateCallback,
                        (XtCallbackProc)pv_info_exit_cb, NULL);
           pv_update = XtVaCreateManagedWidget ("UPDATE",
                        xmPushButtonWidgetClass, pvform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 1,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 2,
                        NULL );
           XtAddCallback(pv_update, XmNactivateCallback,
                        (XtCallbackProc)pv_info_update_cb,NULL);

           iret=UpdateTimeList();
           XtManageChild (pvfile_timelist);
           XtManageChild (pvform);
           XtManageChild (pvform2);
           XtManageChild (pvpane);
           XtManageChild (pv_dialog);
           pv_station_mapw_rgstr(pv_station);
           XtManageChild (pv_vertprof);
           InitSaveVertProfBg(pv_vertprof);
           /* InitSaveHodoBg(pv_hodo); */
          } else {
            iret=UpdateTimeList(pv_vertprof);
            XtManageChild (pv_dialog);

          }
          if (iret == 0) {
            /*Get Latest Data*/
            XmListSelectPos(pvfile_timelist, 1, True);
          }

          XtAddEventHandler (pv_station, PointerMotionMask,
             FALSE, (XtEventHandler) GetInterpPVSounding, (XtPointer) NULL);

        }

        /*NP*/
        /*****************************************************************/
        /* GETINTERPPVSOUNDING  (LJH)                                    */
        /* Interpolates profiler/vad soundings at cursor location        */
        /*****************************************************************/
        void GetInterpPVSounding(Widget mapwin, XtPointer client_data,XEvent *event)
        {
          char sysin[3],sysout[3],cursor_pos[81];
          float xdev,ydev,lat,lon;
          int iret,np=1,i;
          xdev = (float) event->xbutton.x; ydev = (float) event->xbutton.y;

          strcpy ( sysin, "D" );
          strcpy ( sysout, "M" );
          gtrans(sysin, sysout, &np, &xdev, &ydev, &lat, &lon,
                       &iret, strlen(sysin), strlen(sysout));
          cursor_pos[0] = '\0';
          sprintf(cursor_pos,"%6.2f,%7.2f\0",lat,lon);
          GetWindProfAtCurs((int)xdev,(int)ydev,nsta, staxywrk, levelshodo, vawndwrk,
            wnddirs,wndspds,wndhts,&wndcnt,38,250,g_LNGC,39.0);
          if (wndcnt>0) {
            InitProfWindow(pv_vertprof);
            DrawPVVertWinds(wndcnt,wnddirs,wndspds,wndhts,pv_vertprof);
            DrawPVHodograph(wndcnt,wnddirs,wndspds,wndhts,pv_hodo);
          }
        }


        /*NP*/
        /*****************************************************************/
        /* UPDATETIMELIST (LJH)                                          */
        /* Time list for profiler/vad soundings                          */
        /*****************************************************************/
        int UpdateTimeList() {
           int iret,i,ncnt;
           XmStringTable str_list = NULL;
           char proffile[200];
           char *file_dir;
           /* The following 2-lines are commented out due to Profiler Outage starting
              Oct. 1, 2008 for 3-5 weeks. */
           /* sprintf(gemsoundfile,"%s",g_proffile);
           get_gem_times (g_proffile,time_list,&ntimes,&iret,strlen(g_proffile)); */
           sprintf(gemsoundfile,"%s",g_vadfile);
           get_gem_times (g_vadfile,time_list,&ntimes,&iret,strlen(g_vadfile));
           if (iret == 0) {
             str_list = (XmStringTable) XtMalloc(ntimes * sizeof (XmString));
             for (i = ntimes-1;i>=0;i--) {
               time_list[i][11] = 0;
               str_list[ntimes-1-i] =
                 XmStringCreateLocalized(time_list[i]);
             }
             XtVaGetValues (pvfile_timelist, XmNitemCount, &ncnt, NULL);
             XtVaSetValues (pvfile_timelist, XmNitemCount, ntimes,
                            XmNitems, str_list,
                            NULL);
             for (i=0; i < ntimes; i++ )
               XmStringFree ((XmString)str_list[i]);
             XtFree((char *)str_list);
           }
           return iret;
        }

        /*NP*/
        /******************************************************************/
        /* PV_INFO_EXIT_CB (LJH)                                          */
        /******************************************************************/
        void pv_info_exit_cb(Widget w)
        {
            XtRemoveTimeOut(g_pv_timer);
            g_pv_timersw=0;
            ExitWindowSystem();
            XtUnmanageChild (pv_dialog);
        }

        /*NP*/
        /******************************************************************/
        /* PV_INFO_UPDATE_CB (LJH)                                        */
        /******************************************************************/
        void pv_info_update_cb(Widget w)
        {
            int iret;
            iret=UpdateTimeList();
            XtRemoveTimeOut(g_pv_timer);
            g_pv_timersw=0;
            XmListSelectPos(pvfile_timelist,1,True);
        }

        /*NP*/
        /******************************************************************/
        /* PV_STATION_MAPW_RGSTR (LJH)                                    */
        /******************************************************************/
        pv_station_mapw_rgstr(mapwin)

                Widget mapwin;
        {
        XColor          cred;
        Dimension       wdth, hght;
        Cursor          curs;

        int             xwdth, xhght, xdpth;

        int             iret, mapindx=0;
        char            gemdevice[72];

/*---------------------------------------------------------------------*/

        /*
         * Get the map window geometry.
         */
        XtVaGetValues(mapwin,
                      XmNwidth,  &wdth,
                      XmNheight, &hght,
                      NULL);

        xwdth = (int)wdth;
        xhght = (int)hght;
        xdpth = DefaultDepthOfScreen(XtScreen(mapwin));

        /*
         * Set the window and graphics context.
         */
        g_display = XtDisplay(mapwin);
        g_window  = XtWindow(mapwin);
        g_gc = XCreateGC(gemdisplay, g_window, 0, 0);

        /*
         * Create a red arrow for the cursor.
         */
        curs = XCreateFontCursor(gemdisplay, XC_top_left_arrow);
        XDefineCursor(gemdisplay, g_window, curs);
        cred.red   = 65535;
        cred.blue  = 0;
        cred.green = 0;
        cred.flags = DoRed | DoBlue | DoGreen;
        XRecolorCursor(gemdisplay, curs, &cred, &cred);

        /*
         * Set the fill rule.
         */
        XSetFillRule(gemdisplay, g_gc, WindingRule);

        /*
         * Register the map window
         */
        strcpy(gemdevice,"maptoppv");
        xmotifw(g_window, gemdevice, g_gc, xwdth, xhght, xdpth, &iret);
        if( iret != 0 )
                return( iret );
        /*
         * Draw the US map.     FORTRAN function map_init()
         */
        map_init(&iret, gemdevice, strlen(gemdevice));
        strcpy ( map_info[0].name, "US" );
        strcpy ( map_info[0].proj, "STR/90;-105;0/nm" );
        g_LNGC=-105.0;  /* Centering Longitude -- Affects Windbarbs */
        strcpy ( map_info[0].garea, "22.88;-120.49;46.02;-60.83" );
        mapb.x[0] = 22.88;mapb.x[1]=46.02;
        mapb.y[0] = -120.49;mapb.y[1]=-60.83;
        mapindx = 0;
        draw_map(mapindx, map_info, 0, &mapb, &iret);
        return( 0 );
        }

        /*NP*/
        /*********************************************************************/
        /* EXPOSE_PVMAP_STATION_OVERLAYS                                     */
        /*********************************************************************/
        void expose_pvmap_station_overlays(Widget w, XmDrawingAreaCallbackStruct *call_data)
        {
          XCopyArea(XtDisplay(pv_station), pv_window, XtWindow(pv_station),
                 g_gc, 0, 0, pvs_WDTH, pvs_HGHT, 0, 0);
          XFlush(XtDisplay(pv_station));
        }

        /*NP*/
        /**********************************************************************/
        /* RESIZE_PVMAP_STATION_OVERLAYS                                      */
        /**********************************************************************/
        void resize_pvmap_station_overlays(Widget w, XmDrawingAreaCallbackStruct *call_data)
        {
          XCopyArea(XtDisplay(pv_station), pv_window, XtWindow(pv_station),
                 g_gc, 0, 0, pvs_WDTH, pvs_HGHT, 0, 0);
          XFlush(XtDisplay(pv_station));
        }

        /*NP*/

        void load_gemfile ( Widget w, XtPointer client_data,
                         XtPointer call_data )
       /*************************************************************/
       /* LOAD_GEMFILE                                              */
       /*************************************************************/
           {
           static Widget load_filegem;

             if ( ! load_filegem )
               {
               load_filegem = XmCreateFileSelectionDialog ( toplevel,
                            "obssndg_sel", NULL, 0 );
               XtAddCallback ( load_filegem, XmNokCallback, get_gemfile,
                               NULL );
               XtAddCallback ( load_filegem, XmNcancelCallback,
                               (XtCallbackProc)XtUnmanageChild, NULL );
               XtAddCallback ( load_filegem, XmNokCallback,
                               (XtCallbackProc)XtUnmanageChild, NULL );
               }
             XtManageChild(load_filegem);
           }


        /*NP*/
        void get_gemfile_text ( Widget w, XtPointer client_data,
                                       XtPointer call_data )
       /*************************************************************/
       /* GET_GEMFILE_TEXT                                          */
       /*************************************************************/
             {
             char *filename = (char *) XmTextFieldGetString (w);
             int                i, ncnt, mapindx = 0, iret;
             XmStringTable      str_list = NULL;

               strcpy ( gemsoundfile, filename );
               draw_map(mapindx, map_info, 0, &mapb, &iret);
               if ( strlen(filename) != 0 )
                 {
                 gemfile_times();
                 }
               else
                 {
                 XmListDeselectAllItems (gemfile_timelist);
                 XtVaSetValues ( gemfile_timelist,
                                 XmNitemCount, 0,
                                 XmNitems, str_list,
                                 NULL );
                 gemsoundfile[0] = '\0';
                 }
               }


        /*NP*/
        void get_gemfile ( Widget w, XtPointer client_data,
                                       XtPointer call_data )
       /*************************************************************/
       /* GET_GEMFILE                                               */
       /*************************************************************/
             {
             char *file = NULL, filename[200];
             XmFileSelectionBoxCallbackStruct *cbs =
              (XmFileSelectionBoxCallbackStruct *) call_data;
             XmStringTable      str_list;
             int                i, ncnt, iret;

             if (cbs)
               {
               if ( !XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG,
                                      &file) )
                  return;
               XtVaSetValues ( gemfile_text, XmNvalue, file, NULL );
               strcpy ( gemsoundfile, file );
               gemfile_times();
               }
             }


        /*NP*/
        void stationlist_cb (Widget w, XtPointer client_data,
                             XtPointer call_data)
       /*************************************************************/
       /* STATIONLIST_CB                                            */
       /*************************************************************/
          {
          int item_no = (int) client_data;

          Load_stationlist ( item_no );
          }


        /*NP*/
        void Load_stationlist (int list)
       /*************************************************************/
       /* LOAD_STATIONLIST                                          */
       /*************************************************************/
          {
          XmStringTable str_list;
          /*char                station_table[200], staid[4000][5],
                        staname[30], idsta[5], station_tbl[12],
                        sta_st[3], sta_coun[3], statlist[4000][18];*/
          char          *tbl_dir;
          int           stnm[4000], sta_elv, sta_pri,  i, j;
          /*int         nsta, ncolor, mrktyp, mrkwid, pltval, iposn, jcolr;*/
          int           iret, mapindx=0;
          /*float               sta_lat[4000], sta_lon[4000], sizmrk;*/
          FILE          *fp;
          /*struct maptype_list     map_info[2];
          mapbnd_t                mapb;*/
          Window          gwin;
          static stationlistSW=0;
          char gemdevice[72];
          int             xwdth, xhght, xdpth;
          Dimension wdth,hght;
          station_tbl[0] = '\0';
          switch ( list )
            {
            case 0:
              strcpy ( station_tbl, "US" );
            break;

            case 1:
              strcpy ( station_tbl, "CN" );
            break;

            case 2:
              strcpy ( station_tbl, "MX" );
            break;
            }
            if (! stationlistSW) {
                strcpy ( map_info[0].name, "US" );
                strcpy ( map_info[0].proj, "STR/90;-105;0/nm" );
                strcpy ( map_info[0].garea, "22.88;-120.49;46.02;-60.83" );
                mapb.x[0] = 22.88;mapb.x[1]=46.02;
                mapb.y[0] = -120.49;mapb.y[1]=-60.83;
                stationlistSW=-1;
           }
           else
             sprintf(map_info[0].garea,"%3.0f;%5.0f;%3.0f;%5.0f",mapb.x[0],
               mapb.y[0],mapb.x[1],mapb.y[1]);

          mapindx = 0;
          station_list = list;

          if ( gemsoundfile[0] != '\0'  && gemsoundtime[0] != '\0' &&
               station_tbl[0] != '\0' )
            {
            get_gem_stns ( gemsoundfile, station_tbl, gemsoundtime,
                           statlist, &nsta, sta_lat, sta_lon, sta_elev,
                           &item_type,
                           strlen (gemsoundfile), strlen (station_tbl),
                           strlen (gemsoundtime));
            ncolor    = 1;
            mrktyp    = 6;
            sizmrk    = 1.0;
            mrkwid    = 2;
            pltval    = G_FALSE;
            iposn     =  0;
            jcolr     = 2;
            markSW=-1;
            strcpy ( gemdevice, "maptop" );
            gslwin(gemdevice, &iret, strlen(gemdevice));
            map_init(&iret, gemdevice, strlen(gemdevice));
            draw_map(mapindx, map_info, 1, &mapb, &iret);

            map_mark ( &nsta, sta_lat, sta_lon, NULL, &ncolor, NULL,
                       &jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval,
                       &iposn, &iret );
            plotData.mode = STNSELECT;
            stnList.nstn = nsta;
            for ( i=0; i < nsta; i++ )
              {
              statlist[i][17] = 0;
              stnList.lat[i] = sta_lat[i];
              stnList.lon[i] = sta_lon[i];
              stnList.elv[i] = sta_elev[i];
              strcpy ( stnList.stnName[i], statlist[i] );
              }

            }
            else
              {
              stnList.nstn = 0;
              strcpy ( gemdevice, "maptop" );
              gslwin(gemdevice, &iret, strlen(gemdevice));
              map_init(&iret, gemdevice, strlen(gemdevice));
              /* gclear(&iret); */
              draw_map(mapindx, map_info, 0, &mapb, &iret);
              }


          gemsoundsta[0] = '\0';

          }

          void Load_PFS_stationlist(int list) {
          char          *tbl_dir;
          int           stnm[4000], sta_elv, sta_pri,  i, j;
          /*int         nsta, ncolor, mrktyp, mrkwid, pltval, iposn, jcolr;*/
          int           iret, mapindx=0;
          /*float               sta_lat[4000], sta_lon[4000], sizmrk;*/
          FILE          *fp;
          /*struct maptype_list     map_info[2];
          mapbnd_t                mapb;*/
          Window          gwin;
          static stationlistSW=0;
          char gemdevice[72];
          int             xwdth, xhght, xdpth;
          Dimension wdth,hght;
          char gemmodelstring[80];
          station_tbl[0] = '\0';
          char *mdlouttext;
          /* Activate Hour-glass on wait to get station listing */
          Display *display;
          Cursor cursor=(Cursor) None;
          display=XtDisplay(gemlbl_station3);
          gwin=XtWindow(gemlbl_station3);
          cursor=XCreateFontCursor(display,XC_watch);
          XDefineCursor(display,gwin,cursor);
          display=XtDisplay(pfs_mdl_dialog);
          gwin=XtWindow(pfs_mdl_dialog);
          cursor=XCreateFontCursor(display,XC_watch);
          XDefineCursor(display,gwin,cursor);
          XFlush(XtDisplay(pfs_mdl_dialog));
          /************/
          switch ( list )
            {
            case 0:
              strcpy ( station_tbl, "US" );
            break;

            case 1:
              strcpy ( station_tbl, "CN" );
            break;

            case 2:
              strcpy ( station_tbl, "MX" );
            break;

            case -1:
              strcpy ( station_tbl, "US");
              stationlistSW=-1;
              break;

            }
            if (! stationlistSW) {
                strcpy ( map_info[0].name, "US" );
                strcpy ( map_info[0].proj, "STR/90;-105;0/nm" );
                strcpy ( map_info[0].garea, "22.88;-120.49;46.02;-60.83" );
                mapb.x[0] = 22.88;mapb.x[1]=46.02;
                mapb.y[0] = -120.49;mapb.y[1]=-60.83;
                stationlistSW=-1;
           }
           else
             sprintf(map_info[0].garea,"%3.0f;%5.0f;%3.0f;%5.0f",mapb.x[0],
               mapb.y[0],mapb.x[1],mapb.y[1]);

          mapindx = 0;
          station_list = list;
          sprintf(gemmodelstring,"%s/%s",g_pfs_pathname,g_pfs_mdlsoundfile);
          item_type=1;
          get_gem_stns(gemmodelstring,station_tbl,g_msoundtimes[0],
                       statlist,&nsta,sta_lat,sta_lon,sta_elev, &item_type,
                       strlen(gemmodelstring),strlen(station_tbl),
                       strlen(g_msoundtimes[0]));
          ncolor    = 1;
          mrktyp    = 6;
          sizmrk    = 1.0;
          mrkwid    = 2;
          pltval    = G_FALSE;
          iposn     =  0;
          jcolr     = 2;
          markSW=-1;
          strcpy ( gemdevice, "maptop3" );
          gslwin(gemdevice, &iret, strlen(gemdevice));
          map_init(&iret, gemdevice, strlen(gemdevice));
          draw_map(mapindx, map_info, 1, &mapb, &iret);

          map_mark ( &nsta, sta_lat, sta_lon, NULL, &ncolor, NULL,
                       &jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval,
                       &iposn, &iret );
          plotData.mode = STNSELECT;
          stnList.nstn = nsta;
          for ( i=0; i < nsta; i++ )
              {
              statlist[i][17] = 0;
              stnList.lat[i] = sta_lat[i];
              stnList.lon[i] = sta_lon[i];
              stnList.elv[i] = sta_elev[i];
              strcpy ( stnList.stnName[i], statlist[i] );
              }
          NuiDefaultCursor(gemlbl_station3);
          NuiDefaultCursor(pfs_mdl_dialog);
        }




        /*NP*/
        void sta_select_cb (int which_sta )
       /*************************************************************/
       /* STA_SELECT_CB                                             */
       /*************************************************************/
          {
          char  stanum[6], sta_tmp[12];
          int   ival;
          g_mlocationSelected=-1;
          ival = sscanf ( stnList.stnName[which_sta],
                          "%s %s %s", sta_id, stanum, sta_tmp );
          gemsoundsta[0] = 0;
          if ( ival == 2 )
            {
            if ( mode == 3 )
              {
              strcpy ( gemsoundsta, "@" );
              strcat ( gemsoundsta, sta_id );
              }
            else
              {
              strcpy ( gemsoundsta, "@" );
              strcat ( gemsoundsta, stanum );
              }
            }
          else if ( ival == 1 )
            {
            strcpy ( gemsoundsta, "@" );
            strcat ( gemsoundsta, sta_id );
            }
          /* LJH added to correct temp=dwpt problem */
          /* Event Handler is re-enabled in function:position_cursor() */
          XtRemoveEventHandler( draw_reg, ButtonReleaseMask, FALSE,
                (XtEventHandler)redraw_sounding, (XtPointer)NULL);
          if (g_pfs_model_sel_sw) {
            ival=1;
            strcpy ( gemsoundsta, "@" );
            strcat ( gemsoundsta, sta_id );
            sprintf( gemsoundfile,"%s/%s",g_pfs_pathname,g_pfs_mdlsoundfile);
            strcpy( gemsoundtime,g_msoundtimes[0]);
            Load_pfs_mdl_sounding();

          } else  {
            Load_gem_sounding ();
          }
          }


        void Load_pfs_mdl_sounding () {
          Widget mapwin;
          Window          gwin;
          GC              gemgc;
          Display         *display;
          int xwdth, xhght, xdpth;
          int newlev = 0;
          float ix1, ix2;
          int i,j,k,i1;
          char gemdevice[80],timestring[80];
          char buf[80];
          char gemlatlonstr[25];
          int iret;

          Cursor cursor=(Cursor) None;
          display=XtDisplay(gemlbl_station3);
          gwin=XtWindow(gemlbl_station3);
          cursor=XCreateFontCursor(display,XC_watch);
          XDefineCursor(display,gwin,cursor);
          display=XtDisplay(pfs_mdl_dialog);
          gwin=XtWindow(pfs_mdl_dialog);
          cursor=XCreateFontCursor(display,XC_watch);
          XDefineCursor(display,gwin,cursor);
          display=XtDisplay(draw_reg);
          gwin=XtWindow(draw_reg);
          cursor=XCreateFontCursor(display,XC_watch);
          XDefineCursor(display,gwin,cursor);
          XFlush(XtDisplay(pfs_mdl_dialog));
          if (numlvl > 0) copy_sndg();
          sndg2[0][0]=numlvl;
          for (i=0;i<g_nmtimes;i++) {
            g_act_sndg=i;
            strcpy ( gemsoundsta, "@" );
            strcat ( gemsoundsta, sta_id );
            sprintf( gemsoundfile,"%s/%s",g_pfs_pathname,g_pfs_mdlsoundfile);
            get_gem_snd (gemsoundfile, g_msoundtimes[i], gemsoundsta,
                            sndg, &newlev, strlen(gemsoundfile),
                            strlen(g_msoundtimes[i]), strlen(gemsoundsta));
            g_newlev[i]=newlev;
            XtUnmanageChild (pfs_mdl_dialog);
            idle(0.100);
            strcpy (gemdevice, "datatop");
            gslwin(gemdevice,&iret,strlen(gemdevice));

            for (j=0;j<(short)newlev;j++)
              for (k=0;k<7;k++)
                   sndgs[j][k][i]=sndg[j][k];
              ;
            ;
            for (i1=0;i1< mtimes;i1++) {
              strcpy(timestring,mtimeac_list[i1]+4);
              timestring[5]='\0';
              XSetForeground(XtDisplay(time_reg), gc2, pixels[5] );
              for (j=0;j< g_nmtimes;j++) {
                if (strcmp(g_msoundtimes[j],mtimeac_list[i1])==0) {
                  XSetForeground(XtDisplay(time_reg), gc2, pixels[2] );
                  if (strcmp(g_msoundtimes[j],g_msoundtimes[i])==0)
                    XSetForeground(XtDisplay(time_reg), gc2, pixels[7] );
                  break;
                }

              }
              if (i1<48)
                XDrawString(XtDisplay(time_reg),(time_window),gc2,(int)(i1*37.5)%600, (i1/16)*12+12,
                  timestring,strlen(timestring));
            }

            XCopyArea(XtDisplay(time_reg),time_window, XtWindow(time_reg),
              gc2, 0, 0, 600, g_twdth,0,0);
            g_lng=plotData.plt_mark.lon[0];
            g_lat=plotData.plt_mark.lat[0];
            g_elev=plotData.plt_mark.elev[0];
            if (g_lng<0 && g_lat>0)
              sprintf(gemlatlonstr,"%4.1fN %5.1fW",g_lat,-g_lng);
            else if (g_lng>0 && g_lat>0)
                sprintf(gemlatlonstr,"%4.1fN %5.1fE",g_lat,g_lng);
            else if (g_lng<0 && g_lat<0)
                sprintf(gemlatlonstr,"%4.1fS %5.1fW",-g_lat,-g_lng);
            else
                sprintf(gemlatlonstr,"%4.1fS %5.1fE",-g_lat,g_lng);

            sprintf ( raobtitle, " %s %s %s ", mdl_selected,
                      g_msoundtimes[i], gemlatlonstr);
            sprintf(raob_type, "Model Point Forecast for %s Elev=%5.0f ft",sta_id, g_elev * 3.28 );

            xtnd_sndg();
            save_origsndg ();
            resize_callback ( draw_reg, (XtPointer)NULL,
                               (XtPointer)NULL );
            if (numlvl > 2) {
              pagenum = 1;
              define_parcel( current_parcel, user_level );
              mean_wind( -1, -1, &ix1, &ix2, &st_dir, &st_spd);
              st_spd *= .75;
              show_parcel ();
              show_page( pagenum );
            }
            strcpy(buf,"LOADING...PLEASE WAIT");
               XDrawString(XtDisplay(time_oper_reg),XtWindow(time_oper_reg),gc3,1,24,buf,strlen(buf));
               XFlush(XtDisplay(pfs_mdl_dialog));
               XFlush(XtDisplay(time_oper_reg));
               XFlush(XtDisplay(draw_reg));
          }
          g_act_sndg=0;
          XCopyArea(XtDisplay(time_oper_reg),time_oper_window, XtWindow(time_oper_reg),
              gc3, 0, 0, 140,40,0,0);
            LoadSingleModelSounding(g_act_sndg);
            NuiDefaultCursor(gemlbl_station3);
            NuiDefaultCursor(pfs_mdl_dialog);
            NuiDefaultCursor(draw_reg);
        }


        /*NP*/
        /********************************************************************/
        /* PV_SELECT_CB (LJH)                                               */
        /********************************************************************/
        void pv_select_cb (Widget w, XtPointer client_data,
                               XtPointer call_data)
        {
        int i, j, iret, mapindx=0;
        XmListCallbackStruct* cbs = (XmListCallbackStruct*) call_data;
        char*choice;
        char gemtimes[80];
        static char datestring[80];
        char stations[800];
        char stationswrk[800];
        Window          gwin;
        GC              gemgc;
        int             xwdth, xhght, xdpth;
        char gemdevice[72];
        /* pvsounding... up to 100 levels, 3 parameters (z,dd,ff), 200 stations */
        float pvsndgblk[200][300];
        /* pvnlvls... Track nlvls for each station */
        int pvnlvls[200], pvnstns;
        Dimension wdth,hght;
        float lat,lon,xloc,yloc;
        char sys1[2],sys2[2];
        int dir,speed;
        int np=1;
        char gemyaxis[72];
        int offset, offset2, nstawrk;
        char proffile2[200];
        int ntimes2;
        char *file_dir;
        char *ptr;
        int index;
        unsigned levels[14]={0,3000,5000,8000,10000,12000,15000,18000,
            20000,25000,30000,40000,50000,60000};

        Cursor cursor=(Cursor) None;
        cursor=XCreateFontCursor(g_display,XC_watch);
        XDefineCursor(g_display,g_window,cursor);
        XmStringGetLtoR(cbs->item,XmFONTLIST_DEFAULT_TAG, &choice);
        if (choice != '\0')  {
          sprintf(datestring,"%s",choice);
        }
        XtFree(choice);
        sprintf(gemsoundtime,"%s",datestring);
        /*need gemsoundsta */
        station_tbl[0] = '\0';
        strcpy( station_tbl, "US" );
        item_type=2;
        XtVaGetValues(pv_station,
                      XmNwidth,  &wdth,
                      XmNheight, &hght,
                      NULL);
        xwdth = (int)wdth;
        xhght = (int)hght;
        xmotifw(g_window, "maptoppv", g_gc, xwdth, xhght, xdpth, &iret);

/*      sprintf(gemsoundfile,"$OBS/upperair/profdata.snd"); */
        sprintf(gemsoundfile,g_proffile);

        get_gem_stns (gemsoundfile,station_tbl,gemsoundtime,
           statlist, &nsta, sta_lat, sta_lon, sta_elev, &item_type,
           strlen (gemsoundfile), strlen (station_tbl),
           strlen (gemsoundtime));

        stations[0]='\0';
        for ( i=0; i < nsta; i++ ) {
            statlist[i][17] = 0;
            if (i>0) strcat(stations,";");
            strncat(stations,statlist[i],3);
        }

        /* initialize pvsndgblk */
        for (i=0;i<200;i++)
          for (j=0;j<300;j++)
            pvsndgblk[i][j]=0.0;

        /* Process not more than 20 stations at a time */
        nstawrk=nsta;
        offset=0;
        while (nstawrk>20) {
          strncpy(stationswrk,stations+offset*4,20*4);
          stationswrk[79]='\0';
          get_pvsoundings (gemsoundfile, gemsoundtime,
                        stationswrk, pvsndgblk, pvnlvls, &pvnstns, &offset,
                        strlen(gemsoundfile), strlen(gemsoundtime),
                        strlen(stationswrk));
          offset+=20;
          nstawrk-=20;
        };
        strncpy(stationswrk,stations+offset*4,nstawrk*4);
        stationswrk[nstawrk*4]='\0';
        get_pvsoundings (gemsoundfile, gemsoundtime,
                        stationswrk, pvsndgblk, pvnlvls, &pvnstns, &offset,
                        strlen(gemsoundfile), strlen(gemsoundtime),
                        strlen(stationswrk));
        g_nprofstns=nsta;
        for (i=0;i<g_nprofstns;i++) {
          sta_lat2[i]=sta_lat[i];
          sta_lon2[i]=sta_lon[i];
        }
        /* Now attempt to get matching VAD winddata */

        /* file_dir=getenv("OBS");
        sprintf(proffile2,"%s/upperair/vaddata.snd",file_dir); */
        sprintf(proffile2,"%s",g_vadfile);
        get_gem_times (proffile2,time_list2,&ntimes2,&iret,strlen(proffile2));
        for (i=0;i<ntimes2;i++) {
          time_list2[i][11]=0;
          if (strcmp(time_list2[i],gemsoundtime)==0) {
            sprintf(gemsoundtime,"%s",time_list2[i]);
            /*need gemsoundsta */
            station_tbl[0] = '\0';
            strcpy( station_tbl, "US" );
            item_type=2;
            nsta=0;
            /*Must go through these 6 commands to get to the stations in vaddata */
            xmotifw(g_window, "maptoppv", g_gc, xwdth, xhght, xdpth, &iret);
            strcpy ( gemdevice, "maptoppv" );
            gslwin(gemdevice, &iret, strlen(gemdevice));
            map_init(&iret, gemdevice, strlen(gemdevice));
            gclear(&iret);
            draw_map(mapindx, map_info, 1, &mapb, &iret);
            /* sprintf(proffile2,"$OBS/upperair/vaddata.snd");*/
            sprintf(proffile2,"%s",g_vadfile);
            get_gem_stns (proffile2,station_tbl,gemsoundtime,
             statlist, &nsta, sta_lat, sta_lon, sta_elev, &item_type,
             strlen (proffile2), strlen (station_tbl),
             strlen (gemsoundtime));
             stnList.nstn = nsta;
            stations[0]='\0';
            for ( i=0; i < nsta; i++ ) {
              statlist[i][17] = 0;
              if (i>0) strcat(stations,";");
              strncat(stations,statlist[i],3);
            };
            g_nvadstns=nsta;
            for (j=0;j<g_nvadstns;j++) {
              sta_lat2[g_nprofstns+j]=sta_lat[j];
              sta_lon2[g_nprofstns+j]=sta_lon[j];
            }
            nstawrk=nsta;
            offset=g_nprofstns;
            offset2=0;
            while (nstawrk>20) {
              strncpy(stationswrk,stations+offset2*4,20*4);
              stationswrk[79]='\0';
              get_pvsoundings (proffile2, gemsoundtime,
                        stationswrk, pvsndgblk, pvnlvls, &pvnstns, &offset,
                        strlen(proffile2), strlen(gemsoundtime),
                        strlen(stationswrk));
              offset+=20;
              offset2+=20;
              nstawrk-=20;
            };
            strncpy(stationswrk,stations+offset2*4,nstawrk*4);
            stationswrk[nstawrk*4]='\0';
            get_pvsoundings (proffile2, gemsoundtime,
                        stationswrk, pvsndgblk, pvnlvls, &pvnstns, &offset,
                        strlen(proffile2), strlen(gemsoundtime),
                        strlen(stationswrk));

            break;
          }


       }
       nsta=g_nprofstns+g_nvadstns;
       /* cnvrtsndgblktoilvls(pvsndgblk,pvsndg,pvnlvls,nsta); */
       cnvrtsndgblktoilvls(pvsndgblk,pvsndg,pvnlvls,nsta,levels,14);
       cnvrtsndgblktoilvls(pvsndgblk,pvsndghodo,pvnlvls, nsta,levelshodo,20);

       DrawProfVadWindData();
       cnvrtpvsndgtouandv(pvsndghodo,sta_lat2,sta_lon2,nsta,staxywrk,vawndwrk,20);
       setmaxwindinhodo(pvsndghodo, levelshodo, nsta, 20);
       InitSaveHodoBg(pv_hodo);
       XFreeCursor(g_display,cursor);
       NuiDefaultCursor( pv_station );
       if (! g_pv_timersw) {
         g_pv_timer=XtAppAddTimeOut(XtWidgetToApplicationContext(w),900000,UpdatePVDisplay, w);
         g_pv_timersw=-1;
       }

       }

       static void UpdatePVDisplay (XtPointer w, XtIntervalId* timer)
       {
          int iret;
          iret = UpdateTimeList();
          g_pv_timersw=0;
          XmListSelectPos(pvfile_timelist,1,True);
          /* timer = (XtIntervalId*) XtAppAddTimeOut(XtWidgetToApplicationContext(w),120000,(XtTimerCallbackProc) UpdatePVDisplay, w); */
       }


       /*NP*/
       /********************************************************************/
       /* CNVRTSNDGBLKTOILVLS (LJH)                                        */
       /********************************************************************/
        cnvrtsndgblktoilvls(float pvsndgblk[][300],float pvsndg[][3][200],
          int pvnlvls[],int pvnstns, unsigned levels[],int nlvls)
        {
          const float pi=3.14159;
          float dir[50],mag[50],height[50];
          float dir1,mag1;
          int i,j,n,i1;
          float u[50],v[50],u1,v1,u2,v2,uf,vf,angle;
          unsigned hght1,startlevel,endlevel,h1,h2;
          int lowlvlsub,hilvlsub,dir2,mag2;
          lowlvlsub=hilvlsub=1;
          for (i=0;i<nlvls;i++)
            for (j=0;j<200;j++) {
              pvsndg[i][0][j]=0.0;
              pvsndg[i][1][j]=0.0;
              pvsndg[i][2][j]=0.0;
            }
          ;
          for (i1=0;i1<pvnstns;i1++) {
            for (j=0;j<pvnlvls[i1];j++) {
              height[j]=pvsndgblk[i1][(j)*3+0];
              dir[j]=pvsndgblk[i1][(j)*3+1];
              mag[j]=pvsndgblk[i1][(j)*3+2];
            };
            n=pvnlvls[i1];
            startlevel= height[0];
            endlevel= height[n-1];
            lowlvlsub=0;
            for (i=1;i<=nlvls-1;i++) {
              if (levels[i]>=startlevel && lowlvlsub==0)  /* Change > to >= */
                lowlvlsub=i;
              if (levels[i]<=endlevel)                    /* Change < to <= */
                hilvlsub=i;
            }
            for (i=lowlvlsub;i<=hilvlsub;i++) {
              for (j=0;j<n;j++) {
                if (levels[i]>=height[j] && levels[i] <= height[j+1]) { /* Change >= <= */
                  dir1=dir[j];
                  dir2=dir[j+1];
                  mag1=mag[j];
                  mag2=mag[j+1];
                  h1=height[j];
                  h2=height[j+1];
                  u1=-mag1*sin(pi-dir1*pi/180);
                  v1=mag1*cos(pi-dir1*pi/180);
                  u2=-mag2*sin(pi-dir2*pi/180);
                  v2=mag2*cos(pi-dir2*pi/180);
                  h1=height[j];
                  h2=height[j+1];
                  uf=u1+(u2-u1)/(h2-h1)*(levels[i]-h1);
                  vf=v1+(v2-v1)/(h2-h1)*(levels[i]-h1);
                  if (fabs(uf)>0.1 && fabs(vf)>0.1) {
                    angle=(pi+acos(vf/sqrt(uf*uf+vf*vf))*SGN(uf))*180/pi;
                    mag1=sqrt(uf*uf+vf*vf);
                  }
                  else {
                    angle=2*pi;
                    mag1=0;
                  }
                  pvsndg[i][0][i1]=levels[i];
                  pvsndg[i][1][i1]=angle;
                  pvsndg[i][2][i1]=mag1;
                  break;

                }
              }
            }
          }
        }

        cnvrtpvsndgtouandv(float pvsndg[][3][200], float stalat[], float stalng[], int nsta, float stnsxywrk[2][200],float vawnd[200][39],int nlvls)
        {
          int i,j,dir,speed,np,iret;
          float lat,lon,xloc,yloc;
          char sys1[2],sys2[2];
          np=1;
          /* for (i=1;i<=nsta;i++) { */
          for (i=0;i<nsta;i++) {
            strcpy(sys1,"M");
            strcpy(sys2,"D");
            lat=sta_lat2[i];
            lon=sta_lon2[i];
            gtrans( sys1, sys2, &np, &lat, &lon,
               &xloc, &yloc, &iret, strlen(sys1), strlen(sys2) );
            stnsxywrk[0][i]=(int)xloc;
            stnsxywrk[1][i]=(int)yloc;
            for (j=1;j<=nlvls;j++) {
              dir=(int)(pvsndg[j][1][i]+.5);
              speed=(int)(pvsndg[j][2][i]+.5);
              vawnd[i][j*2-1]=-speed*sin(PI-dir*PI/180);
              vawnd[i][j*2]=speed*cos(PI-dir*PI/180);
            }
          }
        }

        /*NP*/
        /**********************************************************************/
        /* SGN (LJH)                                                          */
        /**********************************************************************/
        int SGN(float x) {
          if ( x > 0.0)
            return(1);
          else if (fabs(x)<.0001)  /* x ~=0.0 ? */
            return(1);
          else
            return(-1);
        }

        /*NP*/
        /**********************************************************************/
        /* PSECTION_HGHT_CB (LJH)                                             */
        /**********************************************************************/
        void psection_hght_cb (Widget w, XtPointer client_data,
                                 XtPointer call_data)
        {
        g_hghtno = (int) client_data+1;
        DrawProfVadWindData();
        }

        /*NP*/
        /**********************************************************************/
        /* DRAWPROFVADWINDDATA (LJH)                                          */
        /**********************************************************************/
        DrawProfVadWindData()
        {
        int i, j, iret, mapindx=0;
        Window          gwin;
        GC              gemgc;
        int             xwdth, xhght, xdpth;
        char gemdevice[72];
        Dimension wdth,hght;
        float lat,lon,xloc,yloc;
        char sys1[2],sys2[2];
        int dir,speed;
        int np=1;
        int diroffset=0;
        unsigned levels[14]={0,3000,5000,8000,10000,12000,15000,18000,
            20000,25000,30000,40000,50000,60000};
        char buf[80];
        XtVaGetValues(pv_station,
              XmNwidth,  &wdth,
              XmNheight, &hght,
                      NULL);
        xwdth = (int)wdth;
        xhght = (int)hght;
        xdpth = DefaultDepthOfScreen(XtScreen(pv_station));

        /*
         * Set the window and graphics context.
         */
        g_display = XtDisplay(pv_station);
        g_window  = XtWindow(pv_station);
        g_gc = XCreateGC(gemdisplay, g_window, 0, 0);

        xmotifw(g_window, "maptoppv", g_gc, xwdth, xhght, xdpth, &iret);
        strcpy ( gemdevice, "maptoppv" );
        gslwin(gemdevice, &iret, strlen(gemdevice));
        map_init(&iret, gemdevice, strlen(gemdevice));
        gclear(&iret);
        draw_map(mapindx, map_info, 1, &mapb, &iret);
        initturtle(xwdth,xhght);
        for (i=0;i<nsta;i++) {
           strcpy(sys1,"M");
           strcpy(sys2,"D");
           lat=sta_lat2[i];
           lon=sta_lon2[i];
           dir=(int)(pvsndg[g_hghtno][1][i]+.5);
           speed=(int)(pvsndg[g_hghtno][2][i]+.5);
           if (!(dir>=0 && dir<=360 && speed>=0 && speed<=250)) {
             printf ("Error in wind speed for station # %d \n",i);

           }
           else {
             diroffset=g_LNGC-lon;
             gtrans( sys1, sys2, &np, &lat, &lon,
                      &xloc, &yloc, &iret, strlen(sys1), strlen(sys2) );

             if (i<g_nprofstns) {
                drawloc((int)xloc,(int)yloc,5);
                drawwind((int)xloc,(int)yloc,dir+diroffset,speed,5);
             }
             else {
                drawloc((int)xloc,(int)yloc,10);
                drawwind((int)xloc,(int)yloc,dir+diroffset,speed,10);
             }

           }
        };
        XSetForeground(g_display,g_gc,pixels[31]);
        sprintf(buf,"Profiler/Vad Wind Data for %s at %d feet",gemsoundtime,levels[g_hghtno]);
        XDrawString(g_display, g_window, g_gc, 25,25,buf,strlen(buf));
        if (g_pvmappixcreated)
          XFreePixmap(XtDisplay(pv_station),pv_window);
        root=DefaultRootWindow ( XtDisplay(pv_station) );
        pv_window=XCreatePixmap(XtDisplay(pv_station),root,pvs_WDTH,pvs_HGHT,8);
        g_pvmappixcreated=-1;
        XCopyArea(XtDisplay(pv_station),XtWindow(pv_station),
          pv_window,g_gc,0,0,pvs_WDTH,pvs_HGHT,0,0);
        XFlush(XtDisplay(pv_station));
        XtAddCallback(pv_station, XmNexposeCallback,
              (XtCallbackProc)expose_pvmap_station_overlays, NULL);
        XtAddCallback(pv_station, XmNresizeCallback,
              (XtCallbackProc)resize_pvmap_station_overlays, NULL);
        }


        /*NP*/
        void time_select_cb (Widget w, XtPointer client_data,
                               XtPointer call_data)
       /*************************************************************/
       /* TIME_SELECT_CB                                            */
       /*************************************************************/
          {
          XmListCallbackStruct *cbs = (XmListCallbackStruct *)call_data;
          char *choice;
          char timestring[80];
          int i;
          if (item_type==2) g_pv_timechange=-1;
          XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &choice );
          sprintf ( gemsoundtime, "%s", choice );
          XtFree (choice);
          g_raobModelSW=1;
          root=DefaultRootWindow(XtDisplay(time_reg));
          if (!g_pixcreated) {
            time_window=XCreatePixmap(XtDisplay(time_reg),root,600,g_twdth,8);
            time_oper_window=XCreatePixmap(XtDisplay(time_oper_reg),root,100,40,8);
            g_pixcreated=-1;
          }
          XSetForeground(XtDisplay(time_reg), gc2, pixels[0] );
          XFillRectangle (XtDisplay(time_reg), time_window, gc2, 0, 0,
                        600, g_twdth );
          XSetForeground(XtDisplay(time_oper_reg),gc3,pixels[0]);
          XFillRectangle (XtDisplay(time_oper_reg), time_oper_window,gc3, 0, 0,
                        100, 40 );
          XSetForeground(XtDisplay(time_reg), gc2, pixels[5] );
          strcpy(timestring,"Observed Times:");
          XDrawString( XtDisplay(time_reg), (time_window), gc2, 1, 10,
            timestring, strlen(timestring));
          for (i=0;i< ntimes;i++) {
            strcpy(timestring,time_list[i]+4);
            timestring[5]='\0';
            if (strcmp(gemsoundtime,time_list[i])==0)
              XSetForeground(XtDisplay(time_reg), gc2, pixels[2] );
            else
              XSetForeground(XtDisplay(time_reg), gc2, pixels[5] );
            XDrawString( XtDisplay(time_reg),(time_window),gc2,i*(8*5), 24,
              timestring,strlen(timestring));
          }
          XCopyArea(XtDisplay(time_reg),time_window, XtWindow(time_reg),
            gc2, 0, 0, 600,g_twdth,0,0);
          Load_stationlist ( station_list );
          }


        /*NP*/
        void Load_gem_sounding ()
       /*************************************************************/
       /* LOAD_GEM_SOUNDING                                         */
       /*************************************************************/
          {
          int newlev = 0, iret = 0;
          float ix1, ix2;
          char gemdevice[72], gemyaxis[72], gemtimes[72], *cptr;
          char gemlatlonstr[25];
          XtVaGetValues ( draw_reg, XmNwidth, &xwdth,
                        XmNheight, &xhght, NULL );


          if ( gemsoundfile[0] != '\0'  && gemsoundtime[0] != '\0' &&
               gemsoundsta[0] != '\0' )
            {
             if ( item_type == 1 )
               {
               if (numlvl > 0) copy_sndg();
               sndg2[0][0]=numlvl;
               get_gem_snd (gemsoundfile, gemsoundtime, gemsoundsta,
                            sndg, &newlev, strlen(gemsoundfile),
                            strlen(gemsoundtime), strlen(gemsoundsta));
               numlvl = (short) newlev;
               XtUnmanageChild (gem_dialog);

               /* Wait 1/10th second -- 100 msec for box to Clear.  This
                  fixes problem with dissapearing pressures on left side
                  of Skew-T. Tried using Core baseline "delay" routine
                  and still had problems.  Created "idle" routine which
                  does the job */
               idle(0.100);
               strcpy ( gemdevice, "datatop" );
               gslwin(gemdevice, &iret, strlen(gemdevice));
               g_lng=plotData.plt_mark.lon[0];
               g_lat=plotData.plt_mark.lat[0];
               g_elev=plotData.plt_mark.elev[0];
               if (g_lng<0 && g_lat>0)
                 sprintf(gemlatlonstr,"%4.1fN %5.1fW",g_lat,-g_lng);
               else if (g_lng>0 && g_lat>0)
                   sprintf(gemlatlonstr,"%4.1fN %5.1fE",g_lat,g_lng);
               else if (g_lng<0 && g_lat<0)
                   sprintf(gemlatlonstr,"%4.1fS %5.1fW",-g_lat,-g_lng);
               else
                   sprintf(gemlatlonstr,"%4.1fS %5.1fE",-g_lat,g_lng);

               sprintf ( raobtitle, " %s   %s   %s %s ", sta_id,
                         gemsoundsta, gemsoundtime, gemlatlonstr);
               strcpy( raob_type, "RAOB" );
               sprintf(raob_type,"RAOB Elev=%5.0f ft", g_elev * 3.28);
               xtnd_sndg();
               save_origsndg ();
               /* mode = 1; */

               resize_callback ( draw_reg, (XtPointer)NULL,
                                 (XtPointer)NULL );

               if ( numlvl > 2 && sndg[0][1] > 100. && (!(mode==2 || mode==3 || mode==4 || mode==5)))
                 {
                 pagenum = 1;
                 define_parcel( current_parcel, user_level );
                 mean_wind( -1, -1, &ix1, &ix2, &st_dir, &st_spd);
                 st_spd *= .75;
                 show_parcel ();
                 show_page( pagenum );
                 }
              }
            else if ( item_type == 2 )
              {

                 mode = 3;
                 XtUnmanageChild (gem_dialog);
                 sprintf ( gemyaxis, "0/%s/1000/2;0;1", x_hght );

                 cptr = gemsoundsta;
                 cptr++;
                 strcpy ( gemtimes, gemsoundtime );
                 strcat ( gemtimes, "-last" );
                 strcpy ( gemdevice, "datatop" );
                 gg_motf ( gemdevice, &iret, strlen(gemdevice) );
                 gslwin(gemdevice, &iret, strlen(gemdevice));

                 sncross (gemsoundfile, gemtimes, gemyaxis,
                            cptr,
                            strlen(gemsoundfile), strlen(gemtimes),
                            strlen(gemyaxis), strlen(gemsoundsta)-1);
                 /*
                   Problems occur with overpainting screen when switching
                   times or when switching profiler to vad or vad to profiler.
                   Not sure why, but believe its related to how graphics
                   functions are being called.
                   Found by performing rendering of sncross twice under these
                   conditions that problem is corrected.

                 */
                 /* g_pv_timechange=-1;  */
                 if (g_pv_timechange) {
                   gg_motf ( gemdevice, &iret, strlen(gemdevice) );
                   gslwin(gemdevice, &iret, strlen(gemdevice));

                   sncross (gemsoundfile, gemtimes, gemyaxis,
                            cptr,
                            strlen(gemsoundfile), strlen(gemtimes),
                            strlen(gemyaxis), strlen(gemsoundsta)-1);
                   g_pv_timechange=0;
                 }
                 XCopyArea ( XtDisplay(draw_reg), XtWindow(draw_reg), canvas,
                    gc, 0, 0, xwdth, xhght, 0, 0 );
                 XFlush(XtDisplay(draw_reg));
              }
            }
          }

        /*NP*/
        void print_gem_snd(float sndg[][7],int nlvl)
        {
          int i;
          printf("*********** \n");
          for (i=0;i<nlvl;i++) {
            printf("P=%f Z=%f T=%f TD=%f \n",sndg[i][1],sndg[i][2],sndg[i][3],sndg[i][4]);
          };
        }



        /*NP*/
        void gem_info_ok_cb (Widget w, XtPointer client_data,
                               XtPointer call_data)
       /*************************************************************/
       /* GEM_INFO_OK_CB                                            */
       /*************************************************************/
          {
          Load_gem_sounding ();
          }


        /*NP*/
        void mdl_info_ok_cb (Widget w, XtPointer client_data,
                               XtPointer call_data)
       /*************************************************************/
       /* MDL_INFO_OK_CB                                            */
       /*************************************************************/
          {
          Load_mdl_sounding ();
          }


        /*NP*/
        void load_mdldata (Widget w, XtPointer client_data,
                               XtPointer call_data)
       /*************************************************************/
       /* LOAD_MDLDATA                                              */
       /*************************************************************/
          {
          g_mlocationSelected=-1;
          Load_mdl_sounding ();
          }

        /*NP*/
        void load_pvsounding (Widget w, XtPointer client_data,
                               XtPointer call_data)
          {
          pv_select_cb (w, client_data, call_data);

          }



        /*NP*/
        void mdl_info_cancel_cb (Widget w, XtPointer client_data,
                               XtPointer call_data)
       /*************************************************************/
       /* MDL_INFO_CANCEL_CB                                        */
       /*************************************************************/
          {
          XtUnmanageChild (mdl_dialog);
          }

        /*NP*/
        void mdl_info_world_cb (Widget w, XtPointer client_data,
                              XtPointer call_data)
        /************************************************************/
        /* MDL_INFO_WORLD_CB                                        */
        /************************************************************/
          {
          g_WorldProj=-1;
          mdl_info_unzoom_cb(w, client_data, call_data);
          }

        /*NP*/
        void mdl_info_US_cb (Widget w, XtPointer client_data,
                                XtPointer call_data)
        /************************************************************/
        /* MDL_INFO_US_CB                                           */
        /************************************************************/
          {
             int iret;
             char gemdevice[72];
             strcpy(gemdevice,"maptop2");
             g_WorldProj=0;
             g_Proj=1;
             markSW=0;
             map_init(&iret, gemdevice, strlen(gemdevice));
             gem_info_US_cb (w, client_data, call_data);
          }

        /*NP*/
        void mdl_info_TROP_SFC_cb(Widget w, XtPointer client_data,
                                XtPointer call_data)
        /************************************************************/
        /* MDL_INFO_TROP_SFC_CB                                     */
        /************************************************************/
          {
             int iret;
             char gemdevice[72];
             strcpy(gemdevice,"maptop2");
             g_WorldProj=0;
             g_Proj=2;
             markSW=0;
             map_init(&iret, gemdevice, strlen(gemdevice));
             gem_info_TROP_SFC_cb (w, client_data, call_data);
           }

         /*NP*/
         void mdl_info_OFAGX_cb(Widget w, XtPointer client_data,
                                XtPointer call_data)
        /************************************************************/
        /* MDL_INFO_OFAGX_CB                                        */
        /************************************************************/
          {
             int iret;
             char gemdevice[72];
             strcpy(gemdevice,"maptop2");
             g_WorldProj=0;
             g_Proj=3;
             markSW=0;
             map_init(&iret, gemdevice, strlen(gemdevice));
             gem_info_OFAGX_cb (w, client_data, call_data);
           }

         /*NP*/
         void mdl_info_ATL_cb(Widget w, XtPointer client_data,
                                XtPointer call_data)
        /************************************************************/
        /* MDL_INFO_ATL_CB                                          */
        /************************************************************/
          {
             int iret;
             char gemdevice[72];
             strcpy(gemdevice,"maptop2");
             g_WorldProj=0;
             g_Proj=4;
             markSW=0;
             map_init(&iret, gemdevice, strlen(gemdevice));
             gem_info_ATL_cb (w, client_data, call_data);
           }

         /*NP*/
         void mdl_info_PAC_cb(Widget w, XtPointer client_data,
                                XtPointer call_data)
        /************************************************************/
        /* MDL_INFO_PAC_CB                                          */
        /************************************************************/
          {
             int iret;
             char gemdevice[72];
             strcpy(gemdevice,"maptop2");
             g_WorldProj=0;
             g_Proj=5;
             markSW=0;
             map_init(&iret, gemdevice, strlen(gemdevice));
             gem_info_PAC_cb (w, client_data, call_data);
           }


        /*NP*/
        void mdl_info_zoom_cb (Widget w, XtPointer client_data,
                                XtPointer call_data)
        /************************************************************/
        /* MDL_INFO_ZOOM_CB                                         */
        /************************************************************/
          {
          static int zoom_state=0;
          int i, ityp, np, iret, mapindx=0;
          char sysin[2], sysout[2];
          float xpts[2], ypts[2], xdev[2], ydev[2];
          XtRemoveEventHandler( gemlbl_station2, ButtonPressMask, FALSE,
                        GetSoundingForLatLon, NULL);
          XtRemoveEventHandler( gemlbl_station2, PointerMotionMask, FALSE,
                        mdlmap_pointer, NULL);
          if(zoom_state == 0)
             {
             zoom_state = 1;
             /*
              * Change mouse event handling for zooming
              */


              /*
              * change to the zoom cursor
              */
             NxmCursorChange( gemlbl_station2, XC_crosshair, "white");
             XmUpdateDisplay( w );
             ityp = 3; /* 2; */
             np = 2;
             strcpy ( sysin, "D" );
             for ( i = 0; i < np; i++)
                  xpts[i] = ypts[i] = 0.0;
             ggtpnt( sysin, &ityp, xdev, ydev, &iret, strlen(sysin));
             /*
              * check if the box is big enough
              */
             if ( fabs(xdev[0] - xdev[1]) > 10 &&
                  fabs(ydev[0] - ydev[1]) > 10 )
                {
                /*
                 * Points need to be in lower-left, upper-right order
                 */
                if (xdev[0] > xdev[1]) {
                    i = xdev[0];
                    xdev[0] = xdev[1];
                    xdev[1] = i;
                }
                if (ydev[0] < ydev[1]) {
                    i = ydev[0];
                    ydev[0] = ydev[1];
                    ydev[1] = i;
                }
                strcpy ( sysout, "M" );
                gtrans(sysin, sysout, &np, xdev, ydev, xpts, ypts,
                       &iret, strlen(sysin), strlen(sysout));
                if ( iret == 0 )
                  {
                   for ( i = 0; i < np; i++)
                      {
                      mapb.x[i] = xpts[i];
                      mapb.y[i] = ypts[i];
                      }
                   gclear(&iret);
                   draw_map(mapindx, map_info, 1, &mapb, &iret);

                  }
                }
             /*
              * Reset the zoom button
              */
             mdl_info_zoom_cb( w, NULL, NULL);
             }
          else
             {
             zoom_state = 0;
             /*
              * restore default cursor
              */
             NuiDefaultCursor( gemlbl_station2 );
             /*
              * Restore mouse to be selection status
              */
             XSelectInput( gemdisplay, XtWindow(gemlbl_station2),
                     ButtonPressMask | ButtonReleaseMask |
                     ExposureMask );

             }
          XtAddEventHandler( gemlbl_station2, ButtonPressMask,
                        FALSE, GetSoundingForLatLon, NULL );
          XtAddEventHandler( gemlbl_station2, PointerMotionMask, FALSE,
                        (XtEventHandler)mdlmap_pointer,
                        (XtPointer)NULL);
          }

        /*NP*/
        void mdl_info_unzoom_cb (Widget w, XtPointer client_data,
                                XtPointer call_data)
        /****************************************************************/
        /* MDL_INFO_UNZOOM_CB                                           */
        /****************************************************************/
          {
            int iret;
            char gemdevice[72];
            strcpy(gemdevice,"maptop2");
            markSW=0;
            map_init(&iret, gemdevice, strlen(gemdevice));
            gem_info_unzoom_cb (w, client_data, call_data);
          }


        /*NP*/
        void gem_info_cancel_cb (Widget w, XtPointer client_data,
                               XtPointer call_data)
       /*************************************************************/
       /* GEM_INFO_CANCEL_CB                                        */
       /*************************************************************/
          {
          XtUnmanageChild (gem_dialog);
          }

        /*NP*/
        void gem_info_world_cb (Widget w, XtPointer client_data,
                                XtPointer call_data)
        /*************************************************************/
        /* GEM_INFO_WORLD_CB (LJH)                                   */
        /*************************************************************/
          {
          gem_info_unzoom_cb(w, client_data, call_data);
          }

        void gem_info_US_cb (Widget w, XtPointer client_data,
                                XtPointer call_data)
        /*************************************************************/
        /* GEM_INFO_US_CB (LJH)                                      */
        /*************************************************************/
          {
           int i, iret,mapindx=0;

           strcpy ( map_info[0].name, "US" );
           strcpy ( map_info[0].proj, "STR/90;-105;0/nm" );
           strcpy ( map_info[0].garea, "22.88;-120.49;46.02;-60.83" );
           mapb.x[0] = 22.88;mapb.x[1]=46.02;
           mapb.y[0] = -120.49;mapb.y[1]=-60.83;
           gclear(&iret);
           draw_map(mapindx, map_info, 0, &mapb, &iret);

           if (markSW) {
                get_gem_stns ( gemsoundfile, station_tbl, gemsoundtime,
                           statlist, &nsta, sta_lat, sta_lon, &item_type,
                           strlen (gemsoundfile), strlen (station_tbl),
                           strlen (gemsoundtime));
                map_mark ( &nsta, sta_lat, sta_lon, NULL, &ncolor, NULL,
                        &jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval,
                        &iposn, &iret );
                stnList.nstn = nsta;
                for ( i=0; i < nsta; i++ )
                 {
                 statlist[i][17] = 0;
                 stnList.lat[i] = sta_lat[i];
                 stnList.lon[i] = sta_lon[i];
                 strcpy ( stnList.stnName[i], statlist[i] );
                 }
           }

           }

        /*NP*/
        void gem_info_TROP_SFC_cb (Widget w, XtPointer client_data,
                                   XtPointer call_data)
        /*************************************************************/
        /* GEM_INFO_TROP_SFC_CB (LJH)                                */
        /*************************************************************/
        {

           int i, iret,mapindx=0;
           strcpy(map_info[0].name,"NWWRD1");
           strcpy(map_info[0].proj, "CED/nm");
           strcpy(map_info[0].garea, "8;-100;33;-55.00" );
           mapb.x[0]=8;mapb.x[1]=33;
           mapb.y[0]=-100;mapb.y[1]=-55;
           gclear(&iret);
           draw_map(mapindx, map_info, 0, &mapb, &iret);

           if (markSW) {
                get_gem_stns ( gemsoundfile, station_tbl, gemsoundtime,
                           statlist, &nsta, sta_lat, sta_lon, sta_elev,
                           &item_type,
                           strlen (gemsoundfile), strlen (station_tbl),
                           strlen (gemsoundtime));
                map_mark ( &nsta, sta_lat, sta_lon, NULL, &ncolor, NULL,
                        &jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval,
                        &iposn, &iret );
                stnList.nstn = nsta;
                for ( i=0; i < nsta; i++ )
                 {
                 statlist[i][17] = 0;
                 stnList.lat[i] = sta_lat[i];
                 stnList.lon[i] = sta_lon[i];
                 strcpy ( stnList.stnName[i], statlist[i] );
                 }
           }
        }

        /*NP*/
        void gem_info_OFAGX_cb(Widget w, XtPointer client_data,
                                   XtPointer call_data)
        /*************************************************************/
        /* GEM_INFO_OFAGX_CB (LJH)                                   */
        /*************************************************************/
        {
           int i, iret,mapindx=0;
           strcpy(map_info[0].name,"NWWRD1");
           strcpy(map_info[0].proj, "CED/nm");
           strcpy(map_info[0].garea, "20;-100;35;-80.00" );
           mapb.x[0]=20;mapb.x[1]=35;
           mapb.y[0]=-100;mapb.y[1]=-80;
           gclear(&iret);
           draw_map(mapindx, map_info, 0, &mapb, &iret);

           if (markSW) {
                get_gem_stns ( gemsoundfile, station_tbl, gemsoundtime,
                           statlist, &nsta, sta_lat, sta_lon, sta_elev,
                           &item_type,
                           strlen (gemsoundfile), strlen (station_tbl),
                           strlen (gemsoundtime));
                map_mark ( &nsta, sta_lat, sta_lon, NULL, &ncolor, NULL,
                        &jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval,
                        &iposn, &iret );
                stnList.nstn = nsta;
                for ( i=0; i < nsta; i++ )
                 {
                 statlist[i][17] = 0;
                 stnList.lat[i] = sta_lat[i];
                 stnList.lon[i] = sta_lon[i];
                 stnList.elv[i] = sta_elev[i];
                 strcpy ( stnList.stnName[i], statlist[i] );
                 }
           }

        }

        /*NP*/
        void gem_info_ATL_cb(Widget w, XtPointer client_data,
                                   XtPointer call_data)
        /*************************************************************/
        /* GEM_INFO_ATL_CB (LJH)                                     */
        /*************************************************************/
        {
           int i, iret,mapindx=0;
           strcpy(map_info[0].name,"NWWRD1");
           strcpy(map_info[0].proj, "CED/nm");
           strcpy(map_info[0].garea, "8;-100;50;-38" );
           mapb.x[0]=8;mapb.x[1]=50;
           mapb.y[0]=-100;mapb.y[1]=-38;
           gclear(&iret);
           draw_map(mapindx, map_info, 0, &mapb, &iret);

           if (markSW) {
                get_gem_stns ( gemsoundfile, station_tbl, gemsoundtime,
                           statlist, &nsta, sta_lat, sta_lon, sta_elev,
                           &item_type,
                           strlen (gemsoundfile), strlen (station_tbl),
                           strlen (gemsoundtime));
                map_mark ( &nsta, sta_lat, sta_lon, NULL, &ncolor, NULL,
                        &jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval,
                        &iposn, &iret );
                stnList.nstn = nsta;
                for ( i=0; i < nsta; i++ )
                 {
                 statlist[i][17] = 0;
                 stnList.lat[i] = sta_lat[i];
                 stnList.lon[i] = sta_lon[i];
                 stnList.elv[i] = sta_elev[i];
                 strcpy ( stnList.stnName[i], statlist[i] );
                 }
           }

        }

        /*NP*/
        void gem_info_PAC_cb(Widget w, XtPointer client_data,
                                   XtPointer call_data)
        /*************************************************************/
        /* GEM_INFO_PAC_CB (LJH)                                     */
        /*************************************************************/
        {
           int i, iret,mapindx=0;
           strcpy(map_info[0].name,"NWWRD1");
           strcpy(map_info[0].proj, "CED/nm");
           strcpy(map_info[0].garea, "-6;150;65;-110" );
           mapb.x[0]=-6;mapb.x[1]=65;
           mapb.y[0]=150;mapb.y[1]=-110;
           gclear(&iret);
           draw_map(mapindx, map_info, 0, &mapb, &iret);

           if (markSW) {
                get_gem_stns ( gemsoundfile, station_tbl, gemsoundtime,
                           statlist, &nsta, sta_lat, sta_lon, sta_elev,
                           &item_type,
                           strlen (gemsoundfile), strlen (station_tbl),
                           strlen (gemsoundtime));
                map_mark ( &nsta, sta_lat, sta_lon, NULL, &ncolor, NULL,
                        &jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval,
                        &iposn, &iret );
                stnList.nstn = nsta;
                for ( i=0; i < nsta; i++ )
                 {
                 statlist[i][17] = 0;
                 stnList.lat[i] = sta_lat[i];
                 stnList.lon[i] = sta_lon[i];
                 stnList.elv[i] = sta_elev[i];
                 strcpy ( stnList.stnName[i], statlist[i] );
                 }
           }

        }

        void gem_info_STABMAP_cb (Widget w, XtPointer client_data,
                                   XtPointer call_data)

        /**********************************************************/
        /* GEM_INFO_STABMAP_CB                                    */
        /**********************************************************/
        {
            static Widget stabmapform, stabmapform2,stabmappane,
                          stabmap_exit,stabmap_print,printctrl,printlarge,
                          printnormal, row, button1, button2, button3, button4,
                          stnScale,stabmap_apply;
            XmString stabmap_title;
            int elev,i,newlev;
            char  stanum[6], sta_tmp[12];
            char buf[80];
            int ival,np,iret;
            int l1bot,l1top,l1ints,l1type,l1prob,l2bot,l2top,l2ints,l2type,l2prob,nflag;
            float lat,lon,xloc,yloc;
            char sys1[2];
            char sys2[2];
            Dimension wdth,hght;
            Window          gwin;
            GC              gemgc;
            int             xwdth, xhght, xdpth;
            int mapindx=0;
            char gemdevice[72];
            int xc,yc;
            char work;


            XmString title;
            int n=0;
            if (! stabmap_dialog ) {
              stabmap_title = XmStringCreateLocalized( "STABMAP" );
              stabmap_dialog = XmCreateBulletinBoardDialog(toplevel, "stabmap_panel",
                              NULL, 0);
              XtVaSetValues(stabmap_dialog, XmNdialogTitle, stabmap_title, NULL);
              /* XmStringFree(icemap_title); */
              stabmappane = XtVaCreateManagedWidget("parcel_pane",
                        xmPanedWindowWidgetClass,
                        stabmap_dialog,
                        XmNsashWidth, 1,
                        XmNsashHeight, 1,
                        NULL);
              stabmapform = XtVaCreateWidget("form", xmFormWidgetClass,
                        stabmappane, XmNfractionBase, 7, NULL );
              stabmap_station = XtVaCreateManagedWidget("map3",
                        xmDrawingAreaWidgetClass, stabmapform,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                                XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 800,
                                XmNwidth, 1024, XmNbackground, pixels[0],
                        NULL);
              XtManageChild (stabmapform);
              stabmapform2 = XtVaCreateWidget("form", xmFormWidgetClass,
                        stabmappane, XmNfractionBase, 7, NULL );

              stabmap_exit = XtVaCreateManagedWidget ("EXIT",
                                 xmPushButtonWidgetClass, stabmapform2,
                                 XmNleftAttachment, XmATTACH_POSITION,
                                 XmNleftPosition, 0,
                                 XmNtopAttachment, XmATTACH_FORM,
                                 XmNbottomAttachment, XmATTACH_FORM,
                                 XmNrightAttachment, XmATTACH_POSITION,
                                 XmNrightPosition, 1,
                                 NULL );
              XtAddCallback(stabmap_exit, XmNactivateCallback,
                                 (XtCallbackProc)stabmap_info_exit_cb, NULL);
              stabmap_print = XtVaCreateManagedWidget ("PRINT",
                                   xmPushButtonWidgetClass, stabmapform2,
                                   XmNleftAttachment, XmATTACH_POSITION,
                                   XmNleftPosition, 2,
                                   XmNtopAttachment, XmATTACH_FORM,
                                   XmNbottomAttachment, XmATTACH_FORM,
                                   XmNrightAttachment, XmATTACH_POSITION,
                                   XmNrightPosition, 3,
                                   NULL );
               XtAddCallback(stabmap_print, XmNactivateCallback,
                                 (XtCallbackProc)stabmap_info_print_cb, NULL);
               printctrl = XtVaCreateManagedWidget("printctrl",
                            xmRowColumnWidgetClass, stabmapform2,
                                XmNradioBehavior, True,
                                XmNradioAlwaysOne, True,
                                   XmNleftAttachment, XmATTACH_POSITION,
                                   XmNleftPosition, 3,
                                   XmNtopAttachment, XmATTACH_FORM,
                                   XmNbottomAttachment, XmATTACH_FORM,
                                   XmNrightAttachment, XmATTACH_POSITION,
                                   XmNrightPosition, 4,
                                NULL);
               printlarge = CreateRadioButton(printctrl, "11x17 inches",
                 (XtCallbackProc) printctrl_toggleCB, (XtPointer) "11x17");
               printnormal = CreateRadioButton(printctrl, "8.5x11 inches",
                 (XtCallbackProc) printctrl_toggleCB, (XtPointer) "8.5x11");
               XmToggleButtonSetState(printlarge,True, True);
               row=XtVaCreateManagedWidget("row",
                                xmRowColumnWidgetClass, stabmapform2,
                                XmNradioBehavior, True,
                                XmNradioAlwaysOne, True,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 5,
                                XmNtopAttachment, XmATTACH_FORM,
                                XmNbottomAttachment, XmATTACH_FORM,
                                XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 6,
                                NULL);
               button1= CreateRadioButton(row, "map1",
                             (XtCallbackProc) stabmap_toggleCB, (XtPointer) "Map1");
               button2= CreateRadioButton(row, "map2",
                             (XtCallbackProc) stabmap_toggleCB, (XtPointer) "Map2");
               button3= CreateRadioButton(row, "map3",
                             (XtCallbackProc) stabmap_toggleCB, (XtPointer) "Map3");
               button4= CreateRadioButton(row, "map4",
                             (XtCallbackProc) stabmap_toggleCB, (XtPointer) "Map4");

              /* Build Scale Widget */

              title = XmStringCreateLocalized("Filter Stations");
              stnScale=XtVaCreateManagedWidget("stnscale",xmScaleWidgetClass, stabmapform2,
                XmNshowValue,True,
                XmNtitleString, title,
                XmNorientation,XmHORIZONTAL,
                XmNdecimalPoints,1,
                XmNminimum,0,
                XmNmaximum,10,
                XmNvalue,8,
                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 6,
                                XmNtopAttachment, XmATTACH_FORM,
                                XmNbottomAttachment, XmATTACH_FORM,
                                XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 7,
                                NULL);

              XtAddCallback(stnScale,XmNvalueChangedCallback,
                (XtCallbackProc) scaleCB, (XtPointer) NULL);
              XtAddCallback(stnScale, XmNdragCallback, (XtCallbackProc) scaleCB, (XtPointer) NULL);

              strcpy(g_stnfilter,"0.8");

              XtManageChild (stnScale);
              XtManageChild (stabmapform2);
              XtManageChild (stabmappane);
              XtManageChild (stabmap_dialog);
              stabmap_mapw_rgstr(stabmap_station);
                          XmToggleButtonSetState(button1,True, True);

            }
            else {


              XtManageChild (stabmap_dialog);
              strcpy ( gemdevice, "maptopstab" );
                  gslwin(gemdevice, &iret, strlen(gemdevice));
            }
            GenStabMap();
        }

        Widget CreateRadioButton(Widget parent, char* name,
          XtCallbackProc callback,
          XtPointer client_data)
        {

          Widget toggle;
          toggle=XtVaCreateManagedWidget(name,
            xmToggleButtonWidgetClass, parent,
                XmNindicatorType, XmONE_OF_MANY,
                NULL);

          XtAddCallback(toggle, XmNvalueChangedCallback,
            callback,
                client_data);
          XtVaSetValues(toggle,
            XmNindicatorOn, True,
                XmNfillOnSelect, True,
                NULL);
          return toggle;
        }

        void scaleCB(Widget widget, XtPointer client_data, XtPointer call_data) {
          XmScaleCallbackStruct* ptr;
          short decimal;
          float value;
          ptr = (XmScaleCallbackStruct*) call_data;
          if (ptr->reason==XmCR_VALUE_CHANGED) {
            XtVaGetValues(widget, XmNdecimalPoints, &decimal, NULL);
            value = (float) ptr->value;
            while (decimal > 0) {
              value = value/ (float) 10.0;
              decimal=decimal-1;
            }
            sprintf(g_stnfilter,"%f",value);
            GenStabMap();
          }
        }

        void printctrl_toggleCB(Widget widget, XtPointer client_data, XtPointer call_data) {
           XmToggleButtonCallbackStruct* ptr;
           ptr=(XmToggleButtonCallbackStruct*) call_data;
           if (ptr != NULL) {
              if (ptr->set == True) {
                   if (strcmp( (char *) client_data,"11x17")==0) {
                     g_printctrl_l=17.0;
                     g_printctrl_w=11.0;
                   }
                   else if (strcmp((char *)client_data,"8.5x11")==0) {
                     g_printctrl_l=11.0;
                     g_printctrl_w=8.5;
                   }
              }
           }
        }

        void stabmap_toggleCB(Widget widget, XtPointer client_data,
          XtPointer call_data) {
          char gemdevice[72];
          int iret;
          XmToggleButtonCallbackStruct* ptr;
          ptr=(XmToggleButtonCallbackStruct*) call_data;
          if (ptr != NULL) {
            if (ptr->set == True) {
              if (strcmp( (char *) client_data,"Map1")==0)
                strcpy(g_stabmap,"map1");
              else if (strcmp((char *)client_data,"Map2")==0)
                strcpy(g_stabmap,"map2");
              else if (strcmp((char *)client_data,"Map3")==0)
                strcpy(g_stabmap,"map3");
              else if (strcmp((char *)client_data,"Map4")==0)
                strcpy(g_stabmap,"map4");
              strcpy ( gemdevice, "maptopstab" );
              gslwin(gemdevice, &iret, strlen(gemdevice));
              GenStabMap();
            }
          }
        }

         void GenStabMap() {
            char mode[80];
            char gemdevice[72];
            char dev[50];
            int ival,np,iret;
            int l1bot,l1top,l1ints,l1type,l1prob,l2bot,l2top,l2ints,l2type,l2prob,nflag;
            float lat,lon,xloc,yloc;
            char sys1[2];
            char sys2[2];
            Dimension wdth,hght;
            int elev,i,newlev;
            char  stanum[6], sta_tmp[12];
            int xc,yc,ier;
            char work;
            Cursor cursor=(Cursor) None;
            /* Generate a gc for drawing into map*/
            NxmCursorChange( stabmap_station, XC_watch, "");
            cursor=XCreateFontCursor(g_display,XC_watch);
            XDefineCursor(g_display,g_window,cursor);
            XtVaGetValues(stabmap_station,
                      XmNwidth,  &wdth,
                      XmNheight, &hght,
                      NULL);
            xwdth = (int)wdth;
            xhght = (int)hght;
            xdpth = DefaultDepthOfScreen(XtScreen(stabmap_station));
            strcpy ( gemdevice, "maptopstab" );
                        gg_motf(gemdevice,&iret,strlen(gemdevice));
            gslwin(gemdevice, &iret, strlen(gemdevice));
                        strcpy(mode,"display");
            strcpy(dev,"XWP|maptopstab");
            gg_sdev(dev,&ier,strlen(dev));
            plot_indices(g_stabmap, mode, gemsoundfile,
                          gemsoundtime, g_stnfilter, dev, strlen(g_stabmap), strlen(mode),
                          strlen(gemsoundfile), strlen(gemsoundtime),strlen(g_stnfilter),
                          strlen(dev));
            XFreeCursor(g_display,cursor);
            NuiDefaultCursor( stabmap_station );
       }

        /*NP*/
        void stabmap_info_print_cb (Widget w, XtPointer client_data,
                                   XtPointer call_data)
                {
             char gemdevice[80];
             char mode[80];
                 char dev[50];
                 FILE *fp;
                 char   unixcmd[200];
         char OS[20];
                 int iret,ier;
                 strcpy(mode,"print");
         sprintf(dev,"PS|/tmp/planstnplot.ps|%4.1f;%4.1f|M",g_printctrl_l,g_printctrl_w);
                 /* strcpy(dev,"PS|/tmp/planstnplot.ps|11.0;8.5|M"); */
                 gg_sdev(dev,&ier,strlen(dev));

                 strcpy(gemdevice,"maptopstab");
                 gg_motf(gemdevice,&iret,strlen(gemdevice));
         gslwin(gemdevice, &iret, strlen(gemdevice));

         plot_indices(g_stabmap,mode,gemsoundfile,gemsoundtime,g_stnfilter,
           dev,strlen(g_stabmap),strlen(mode),strlen(gemsoundfile),
           strlen(gemsoundtime),strlen(g_stnfilter),strlen(dev));
         strcpy(dev,"XWP|maptopstab");
         gg_sdev(dev,&ier,strlen(dev));
         if (ier != 0) {
                printf(" error calling gg_sdev, ier = %d\n", ier);
         }

         strcpy(OS,getenv("OS"));
         if (strncmp(OS,"Linux",5)==0)
           sprintf(unixcmd,"lpr %s", "/tmp/planstnplot.ps");
         else
           sprintf(unixcmd,"lp -onb -oletter %s", "/tmp/planstnplot.ps");
         system(unixcmd);
         sprintf(unixcmd,"rm -f %s; rm -f /tmp/planstnplot.ps", config.filename );
         system(unixcmd);
        }

        /*NP*/
        void gem_info_ICEMAP_cb (Widget w, XtPointer client_data,
                                   XtPointer call_data)
        /**********************************************************/
        /* GEM_INFO_ICEMAP_CB                                     */
        /**********************************************************/
        {
            static Widget  icemapform, icemapform2, icemappane,icemap_exit,icemap_zoom,icemap_unzoom;
            XmString icemap_title;
            int elev,i,newlev;
            char  stanum[6], sta_tmp[12];
            char buf[80];
            int ival,np,iret;
            int l1bot,l1top,l1ints,l1type,l1prob,l2bot,l2top,l2ints,l2type,l2prob,nflag;
            float lat,lon,xloc,yloc;
            char sys1[2];
            char sys2[2];
            Dimension wdth,hght;
            Window          gwin;
            GC              gemgc;
            int             xwdth, xhght, xdpth;
            int mapindx=0;
            char gemdevice[72];
            int xc,yc;
            char work;
            if (! icemap_dialog ) {
              icemap_title = XmStringCreateLocalized( "ICEMAP" );
              icemap_dialog = XmCreateBulletinBoardDialog(toplevel, "icemap_panel",
                              NULL, 0);
              XtVaSetValues(icemap_dialog, XmNdialogTitle, icemap_title, NULL);
              /* XmStringFree(icemap_title); */
              icemappane = XtVaCreateManagedWidget("parcel_pane",
                        xmPanedWindowWidgetClass,
                        icemap_dialog,
                        XmNsashWidth, 1,
                        XmNsashHeight, 1,
                        NULL);
              icemapform = XtVaCreateWidget("form", xmFormWidgetClass,
                        icemappane, XmNfractionBase, 7,
                        NULL );
              icemap_station = XtVaCreateManagedWidget("map3",
                        xmDrawingAreaWidgetClass, icemapform,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 800,
                        XmNwidth, 1024, XmNbackground, pixels[0],
                        NULL);
              /*XtAddCallback(icemap_station, XmNexposeCallback,
                mapw_exposeCb, NULL ); */
              XtManageChild (icemapform);
              icemapform2 = XtVaCreateWidget("form", xmFormWidgetClass,
                        icemappane, XmNfractionBase, 7,
                        NULL );

              icemap_exit = XtVaCreateManagedWidget ("EXIT",
                        xmPushButtonWidgetClass, icemapform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 1,
                        NULL );
              XtAddCallback(icemap_exit, XmNactivateCallback,
                        (XtCallbackProc)icemap_info_exit_cb, NULL);
              icemap_zoom = XtVaCreateManagedWidget ("ZOOM",
                        xmPushButtonWidgetClass, icemapform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 2,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 3,
                        NULL );
              XtAddCallback(icemap_zoom, XmNactivateCallback,
                        (XtCallbackProc)icemap_info_zoom_cb, NULL);
              icemap_unzoom = XtVaCreateManagedWidget ("UNZOOM",
                        xmPushButtonWidgetClass, icemapform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 3,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 4,
                        NULL );
              XtAddCallback(icemap_unzoom, XmNactivateCallback,
                        (XtCallbackProc)icemap_info_unzoom_cb, NULL);

              XtManageChild (icemapform2);
              XtManageChild (icemappane);
              XtManageChild (icemap_dialog);
              icemap_mapw_rgstr(icemap_station);
            }
            else {


              XtManageChild (icemap_dialog);
              /*mapw_rgstr(  gemlbl_station);
              icemap_mapw_rgstr(icemap_station);*/
              strcpy ( map_info[0].name, "US" );
              strcpy ( map_info[0].proj, "STR/90;-105;0/nm" );
              strcpy ( map_info[0].garea, "22.88;-120.49;46.02;-60.83" );
              mapb.x[0] = 22.88;mapb.x[1]=46.02;
              mapb.y[0] = -120.49;mapb.y[1]=-60.83;
              strcpy ( gemdevice, "maptopice" );
              gslwin(gemdevice, &iret, strlen(gemdevice));
              map_init(&iret, gemdevice, strlen(gemdevice));
              gclear(&iret);

              draw_map(mapindx, map_info, 1, &mapb, &iret);
            }
            GenIceMap();
        }

        /*NP*/
        void GenIceMap()
        /*****************************************************************/
        /* GENICEMAP                                                     */
        /*****************************************************************/
        {
            char buf[80];
            char gemdevice[72];
            int ival,np,iret;
            int l1bot,l1top,l1ints,l1type,l1prob,l2bot,l2top,l2ints,l2type,l2prob,nflag;
            float lat,lon,xloc,yloc;
            char sys1[2];
            char sys2[2];
            Dimension wdth,hght;
            int elev,i,newlev;
            char  stanum[6], sta_tmp[12];
            int xc,yc;
            char work;
            Cursor cursor=(Cursor) None;
            /*Generate a gc for drawing into map*/
            /*NxmCursorChange( icemap_station, XC_watch, "");*/
            cursor=XCreateFontCursor(g_display,XC_watch);
            XDefineCursor(g_display,g_window,cursor);
            XtVaGetValues(icemap_station,
                      XmNwidth,  &wdth,
                      XmNheight, &hght,
                      NULL);
            xwdth = (int)wdth;
            xhght = (int)hght;
            xdpth = DefaultDepthOfScreen(XtScreen(icemap_station));
            strcpy ( gemdevice, "maptopice" );
            gslwin(gemdevice, &iret, strlen(gemdevice));
            XSetForeground(g_display,g_gc,pixels[5]);
            initturtle(xwdth,xhght);
            XFlush(g_display);
            strcpy(buf,"AIRCRAFT ICING");
            XDrawString(g_display,g_window,g_gc,300,710,buf,strlen(buf));
            sprintf(buf,"%s RAOBS",gemsoundtime);
            XDrawString(g_display,g_window,g_gc,300,730,buf,strlen(buf));
            sprintf(buf,"C5BM20,700;%s",Trace_Ice_Sym);
            draw(buf);
            XSetForeground(g_display,g_gc,pixels[27]);
            strcpy(buf,"TRACE");
            XDrawString(g_display,g_window,g_gc,45,710,buf,strlen(buf));
            sprintf(buf,"C5BM20,720;%s",Light_Ice_Sym);
            draw(buf);
            XSetForeground(g_display,g_gc,pixels[27]);
            strcpy(buf,"LIGHT");
            XDrawString(g_display,g_window,g_gc,45,730,buf,strlen(buf));
            sprintf(buf,"C5BM20,740;%s",Moderate_Ice_Sym);
            draw(buf);
            XSetForeground(g_display,g_gc,pixels[27]);
            strcpy(buf,"MODERATE");
            XDrawString(g_display,g_window,g_gc,45,750,buf,strlen(buf));
            sprintf(buf,"C5BM20,760;%s",Severe_Ice_Sym);
            draw(buf);
            XSetForeground(g_display,g_gc,pixels[27]);
            strcpy(buf,"SEVERE");
            XDrawString(g_display,g_window,g_gc,45,770,buf,strlen(buf));
            /*Cycle through station data at specific time for icing potential */
            for (i=0;i<nsta;i++) {
              ival = sscanf ( stnList.stnName[i],
                          "%s %s %s", sta_id, stanum, sta_tmp );
              gemsoundsta[0] = 0;
              if ( ival == 2 )
                {
                if ( mode == 3 )
                  {
                  strcpy ( gemsoundsta, "@" );
                  strcat ( gemsoundsta, sta_id );
                  }
                else
                  {
                  strcpy ( gemsoundsta, "@" );
                  strcat ( gemsoundsta, stanum );
                  }
                }
              else if ( ival == 1 )
                {
                strcpy ( gemsoundsta, "@" );
                strcat ( gemsoundsta, sta_id );
                }
              get_gem_snd (gemsoundfile, gemsoundtime, gemsoundsta,
                            sndg, &newlev, strlen(gemsoundfile),
                            strlen(gemsoundtime), strlen(gemsoundsta));
              elev=(int)sndg[0][2];
              strcpy(raob_type,"RAOB");
              xtnd_sndg();
              get_icing_fm_snd (sndg,&newlev,&elev,
                &l1bot,&l1top,&l1ints,&l1type,&l1prob,
                &l2bot,&l2top,&l2ints,&l2type,&l2prob,&nflag);
              /* Establish Drawing Access onto Map...Plot City Locs */
              strcpy(sys1, "M");
              strcpy(sys2, "D");
              np=1;
              lat=sta_lat[i];
              lon=sta_lon[i];
              gtrans( sys1, sys2, &np, &lat, &lon,
                      &xloc, &yloc, &iret, strlen(sys1), strlen(sys2) );
              XSetForeground(XtDisplay(icemap_station),g_gc,pixels[7]);
              xc=(int)xloc;yc=(int)yloc;
              XDrawRectangle(XtDisplay(icemap_station),XtWindow(icemap_station),g_gc,xc-2,yc-2,4,4);
              if (xc>0 && yc>0 && xc<1024 && yc<800) {
                if (nflag) {
                  DrawIcingData(XtDisplay(icemap_station),XtWindow(icemap_station),g_gc,xc,yc-10,l1bot,l1top,l1ints,l1type,l1prob,-1);
                  DrawIcingData(XtDisplay(icemap_station),XtWindow(icemap_station),g_gc,xc,yc+12,l2bot,l2top,l2ints,l2type,l2prob,1);
                } else  {
                  XSetForeground(XtDisplay(icemap_station),g_gc,pixels[24]);
                  strcpy(buf,"NIL");
                  XDrawString(g_display,g_window,g_gc,xc-getgtextextent(buf)/2,yc+15,buf,strlen(buf));
                }
              }
              XFlush(XtDisplay(icemap_station));

            }
            if (g_icemappixcreated)
              XFreePixmap(XtDisplay(icemap_station),icemap_window);
            icemap_window=XCreatePixmap(XtDisplay(icemap_station),root,1024,800,8);
            g_icemappixcreated=-1;
            XCopyArea(XtDisplay(icemap_station),XtWindow(icemap_station),
              icemap_window,g_gc,0,0,1024,800,0,0);
            XFlush(XtDisplay(icemap_station));
            XtAddCallback(icemap_station, XmNexposeCallback,
              (XtCallbackProc)expose_icemap_station_overlays, NULL);
            XtAddCallback(icemap_station, XmNresizeCallback,
              (XtCallbackProc)resize_icemap_station_overlays, NULL);
            XFreeCursor(g_display,cursor);
            NuiDefaultCursor( icemap_station );
        }
        /*NP*/
        void icemap_info_zoom_cb (Widget w, XtPointer client_data,
                                XtPointer call_data)
        /*************************************************************/
        /* ICEMAP_INFO_ZOOM_CB                                       */
        /*************************************************************/
        {
          static int zoom_state=0;
          int i, ityp, np, iret, mapindx=0;
          char sysin[2], sysout[2];
          float xpts[2], ypts[2], xdev[2], ydev[2];
          char gemdevice[72];
          if(zoom_state == 0)
             {
             zoom_state = 1;
             /*
              * Change mouse event handling for zooming
              */


              /*
              * change to the zoom cursor
              */
             NxmCursorChange( icemap_station, XC_crosshair, "white");
             XmUpdateDisplay( w );
             ityp = 3; /* 2; */
             np = 2;
             strcpy ( sysin, "D" );
             for ( i = 0; i < np; i++)
                  xpts[i] = ypts[i] = 0.0;
             ggtpnt( sysin, &ityp, xdev, ydev, &iret, strlen(sysin));
             /*
              * check if the box is big enough
              */
             if ( fabs(xdev[0] - xdev[1]) > 10 &&
                  fabs(ydev[0] - ydev[1]) > 10 )
                {
                /*
                 * Points need to be in lower-left, upper-right order
                 */
                if (xdev[0] > xdev[1]) {
                    i = xdev[0];
                    xdev[0] = xdev[1];
                    xdev[1] = i;
                }
                if (ydev[0] < ydev[1]) {
                    i = ydev[0];
                    ydev[0] = ydev[1];
                    ydev[1] = i;
                }
                strcpy ( sysout, "M" );
                gtrans(sysin, sysout, &np, xdev, ydev, xpts, ypts,
                       &iret, strlen(sysin), strlen(sysout));
                if ( iret == 0 )
                  {
                   for ( i = 0; i < np; i++)
                      {
                      mapb.x[i] = xpts[i];
                      mapb.y[i] = ypts[i];
                      }
                   gclear(&iret);
                   draw_map(mapindx, map_info, 1, &mapb, &iret);
                   GenIceMap();
                  }
                }
             /*
              * Reset the zoom button
              */
             icemap_info_zoom_cb( w, NULL, NULL);
             }
          else
             {
             zoom_state = 0;
             /*
              * restore default cursor
              */
             NuiDefaultCursor( icemap_station );
             /*
              * Restore mouse to be selection status
              */
             XSelectInput( gemdisplay, XtWindow(icemap_station),
                     ButtonPressMask | ButtonReleaseMask |
                     ExposureMask );

             }
          }

        /*NP*/
        void icemap_info_unzoom_cb (Widget w, XtPointer client_data,
                                XtPointer call_data)
        /***********************************************************/
        /* ICEMAP_INFO_UNZOOM_CB                                   */
        /***********************************************************/
          {
            gem_info_ICEMAP_cb(w, client_data, call_data);

          }

        /*NP*/
        void expose_icemap_station_overlays (Widget w, XmDrawingAreaCallbackStruct *call_data)
        /**************************************************************/
        /* EXPOSE_ICEMAP_STATION_OVERLAYS                             */
        /**************************************************************/
        {
          XCopyArea(XtDisplay(icemap_station), icemap_window, XtWindow(icemap_station),
                 g_gc, 0, 0, 1024, 800, 0, 0);
          XFlush(XtDisplay(icemap_station));
        }
        /*NP*/
        void resize_icemap_station_overlays (Widget w,XmDrawingAreaCallbackStruct *call_data)
        /***************************************************************/
        /* RESIZE_ICEMAP_STATION_OVERLAYS                              */
        /***************************************************************/
        {
          XCopyArea(XtDisplay(icemap_station), icemap_window, XtWindow(icemap_station),
                 g_gc, 0, 0, 1024, 800, 0, 0);
          XFlush(XtDisplay(icemap_station));
        }
        /*NP*/
        void DrawIcingData(Display *dsp, Window win, GC gc,int xc,int yc,
           int bot,int top,int ints,int type, int prob,int sgn)
        /****************************************************************/
        /* DRAWICINGDATA                                                */
        /****************************************************************/
        {
          char buf[80];
          Font          font_info;
          static char   font_2 [] =
          { "-adobe-courier-bold-r-normal--14-100-100-100-m-90-iso8859-1" };
          font_info = XLoadFont(dsp, font_2 );
          XSetFont(dsp, gc, font_info );
          if(font_struct != NULL) XFreeFontInfo(NULL,font_struct,0);
          font_struct = XQueryFont ( dsp, font_info);
          XSetForeground(dsp,gc,pixels[27]);
          if (bot>0) {
            sprintf(buf,"%d",(int)(bot*.0328+.5));
            XDrawString(dsp,win,gc,xc-35,yc+5,buf,strlen(buf));
          }
          if (top>0) {
            sprintf(buf,"%d",(int)(top*.0328+.5));
            XDrawString(dsp,win,gc,xc+10,yc+5,buf,strlen(buf));
          }
          if (ints>0) {
            if (sgn<0)
              sprintf(buf,"C5BM%d,%d;",xc-7,yc-5);
            else
              sprintf(buf,"C5BM%d,%d;",xc-7,yc-3);
            switch (ints) {
              case 2:
                strcat(buf,Trace_Ice_Sym);
                break;
              case 3:
                strcat(buf,Light_Ice_Sym);
                break;
              case 4:
                strcat(buf,Moderate_Ice_Sym);
                break;
              case 5:
                strcat(buf,Severe_Ice_Sym);
                break;
            }
            draw(buf);
          }
          if (prob>0) {
            XSetForeground(dsp,gc,pixels[5]);
            sprintf(buf,"%d",prob);
            XDrawString(dsp,win,gc,xc-30,yc+sgn*15+5,buf,strlen(buf));
          }
          if (type>0) {
            XSetForeground(dsp,gc,pixels[21]);
            switch (type) {
              case 1:
                strcpy(buf,"CLR");
                break;
              case 2:
                strcpy(buf,"RIM");
                break;
              case 3:
                strcpy(buf,"MXD");
                break;
            }
            XDrawString(dsp,win,gc,xc-getgtextextent(buf)/2,yc+sgn*15+5,buf,strlen(buf));
          }
        }

        /*NP*/
        void icemap_info_exit_cb (Widget w, XtPointer client_data,
                               XtPointer call_data)
        /******************************************************************/
        /* ICEMAP_INFO_EXIT_CB                                            */
        /******************************************************************/
          {
           int i, iret,mapindx=0;
           char gemdevice[72];
           XtUnmanageChild (icemap_dialog);
            strcpy ( gemdevice, "maptop" );
            gslwin(gemdevice, &iret, strlen(gemdevice));
            map_init(&iret, gemdevice, strlen(gemdevice));
            strcpy ( map_info[0].name, "US" );
            strcpy ( map_info[0].proj, "STR/90;-105;0/nm" );
            strcpy ( map_info[0].garea, "22.88;-120.49;46.02;-60.83" );
            mapb.x[0] = 22.88;mapb.x[1]=46.02;
            mapb.y[0] = -120.49;mapb.y[1]=-60.83;
            gclear(&iret);

            draw_map(mapindx, map_info, 1, &mapb, &iret);
            Load_stationlist ( 0 );
          }

        /*NP*/
        void stabmap_info_exit_cb (Widget w, XtPointer client_data,
                               XtPointer call_data)
        /******************************************************************/
        /* STABMAP_INFO_EXIT_CB                                            */
        /******************************************************************/
          {
           int i, iret,mapindx=0;
           char gemdevice[72];
           XtUnmanageChild (stabmap_dialog);
            strcpy ( gemdevice, "maptop" );
            gslwin(gemdevice, &iret, strlen(gemdevice));
            map_init(&iret, gemdevice, strlen(gemdevice));
            strcpy ( map_info[0].name, "US" );
            strcpy ( map_info[0].proj, "STR/90;-105;0/nm" );
            strcpy ( map_info[0].garea, "22.88;-120.49;46.02;-60.83" );
            mapb.x[0] = 22.88;mapb.x[1]=46.02;
            mapb.y[0] = -120.49;mapb.y[1]=-60.83;
            gclear(&iret);

            draw_map(mapindx, map_info, 1, &mapb, &iret);
            Load_stationlist ( 0 );
          }

        /*NP*/
        stabmap_mapw_rgstr(mapwin)
        /***************************************************************/
        /* STABMAP_MAPW_RGSTR                                           */
        /***************************************************************/
        Widget mapwin;
        {
        XColor          cred;
        Dimension       wdth, hght;
        Cursor          curs;

        int             xwdth, xhght, xdpth;

        int             iret, mapindx=0;
        char            gemdevice[72];

/*---------------------------------------------------------------------*/

        /*
         * Get the map window geometry.
         */
        XtVaGetValues(mapwin,
                      XmNwidth,  &wdth,
                      XmNheight, &hght,
                      NULL);

        xwdth = (int)wdth;
        xhght = (int)hght;
        xdpth = DefaultDepthOfScreen(XtScreen(mapwin));

        /*
         * Set the window and graphics context.
         */
           g_display = XtDisplay(mapwin);
        g_window  = XtWindow(mapwin);
        g_gc = XCreateGC(gemdisplay, g_window, 0, 0);

        /*
         * Create a red arrow for the cursor.
         */
        curs = XCreateFontCursor(gemdisplay, XC_top_left_arrow);
        XDefineCursor(gemdisplay, g_window, curs);
        cred.red   = 65535;
        cred.blue  = 0;
        cred.green = 0;
        cred.flags = DoRed | DoBlue | DoGreen;
        XRecolorCursor(gemdisplay, curs, &cred, &cred);

        /*
         * Set the fill rule.
         */
        XSetFillRule(gemdisplay, g_gc, WindingRule);

        /*
         * Register the map window
         */
        strcpy(gemdevice,"maptopstab");
        xmotifw(g_window, gemdevice, g_gc, xwdth, xhght, xdpth, &iret);
        if( iret != 0 )
                return( iret );
        return( 0 );
        }




        /*NP*/
        icemap_mapw_rgstr(mapwin)
        /***************************************************************/
        /* ICEMAP_MAPW_RGSTR                                           */
        /***************************************************************/
        Widget mapwin;
        {
        XColor          cred;
        Dimension       wdth, hght;
        Cursor          curs;

        int             xwdth, xhght, xdpth;

        int             iret, mapindx=0;
        char            gemdevice[72];

/*---------------------------------------------------------------------*/

        /*
         * Get the map window geometry.
         */
        XtVaGetValues(mapwin,
                      XmNwidth,  &wdth,
                      XmNheight, &hght,
                      NULL);

        xwdth = (int)wdth;
        xhght = (int)hght;
        xdpth = DefaultDepthOfScreen(XtScreen(mapwin));

        /*
         * Set the window and graphics context.
         */
        g_display = XtDisplay(mapwin);
        g_window  = XtWindow(mapwin);
        g_gc = XCreateGC(gemdisplay, g_window, 0, 0);

        /*
         * Create a red arrow for the cursor.
         */
        curs = XCreateFontCursor(gemdisplay, XC_top_left_arrow);
        XDefineCursor(gemdisplay, g_window, curs);
        cred.red   = 65535;
        cred.blue  = 0;
        cred.green = 0;
        cred.flags = DoRed | DoBlue | DoGreen;
        XRecolorCursor(gemdisplay, curs, &cred, &cred);

        /*
         * Set the fill rule.
         */
        XSetFillRule(gemdisplay, g_gc, WindingRule);

        /*
         * Register the map window
         */
        strcpy(gemdevice,"maptopice");
        xmotifw(g_window, gemdevice, g_gc, xwdth, xhght, xdpth, &iret);
        if( iret != 0 )
                return( iret );
        /*
         * Draw the US map.     FORTRAN function map_init()
         */
        map_init(&iret, gemdevice, strlen(gemdevice));
        strcpy ( map_info[0].name, "US" );
        strcpy ( map_info[0].proj, "STR/90;-105;0/nm" );
        strcpy ( map_info[0].garea, "22.88;-120.49;46.02;-60.83" );
        mapb.x[0] = 22.88;mapb.x[1]=46.02;
        mapb.y[0] = -120.49;mapb.y[1]=-60.83;
        mapindx = 0;
        draw_map(mapindx, map_info, 0, &mapb, &iret);

        return( 0 );
        }

        /*NP*/
        void gem_info_zoom_cb (Widget w, XtPointer client_data,
                                XtPointer call_data)
        /*************************************************************/
        /* GEM_INFO_ZOOM_CB   (LJH)                                  */
        /*************************************************************/
          {
          static int zoom_state=0;
          int i, ityp, np, iret, mapindx=0;
          char sysin[2], sysout[2];
          float xpts[2], ypts[2], xdev[2], ydev[2];

           void mapw_pickstnCb();

          if(zoom_state == 0)
             {
             zoom_state = 1;
             /*
              * Change mouse event handling for zooming
              */
             XtRemoveEventHandler( gemlbl_station, ButtonPressMask, FALSE,
                        mapw_pickstnCb, NULL);


              /*
              * change to the zoom cursor
              */
             NxmCursorChange( gemlbl_station, XC_crosshair, "white");
             XmUpdateDisplay( w );
             ityp = 3; /* 2; */
             np = 2;
             strcpy ( sysin, "D" );
             for ( i = 0; i < np; i++)
                  xpts[i] = ypts[i] = 0.0;
             ggtpnt( sysin, &ityp, xdev, ydev, &iret, strlen(sysin));
             /*
              * check if the box is big enough
              */
             if ( fabs(xdev[0] - xdev[1]) > 10 &&
                  fabs(ydev[0] - ydev[1]) > 10 )
                {
                /*
                 * Points need to be in lower-left, upper-right order
                 */
                if (xdev[0] > xdev[1]) {
                    i = xdev[0];
                    xdev[0] = xdev[1];
                    xdev[1] = i;
                }
                if (ydev[0] < ydev[1]) {
                    i = ydev[0];
                    ydev[0] = ydev[1];
                    ydev[1] = i;
                }
                strcpy ( sysout, "M" );
                gtrans(sysin, sysout, &np, xdev, ydev, xpts, ypts,
                       &iret, strlen(sysin), strlen(sysout));
                if ( iret == 0 )
                  {
                   for ( i = 0; i < np; i++)
                      {
                      mapb.x[i] = xpts[i];
                      mapb.y[i] = ypts[i];
                      }
                   gclear(&iret);
                   draw_map(mapindx, map_info, 1, &mapb, &iret);
                   if (markSW) {
                        get_gem_stns ( gemsoundfile, station_tbl, gemsoundtime,
                           statlist, &nsta, sta_lat, sta_lon, sta_elev,
                           &item_type,
                           strlen (gemsoundfile), strlen (station_tbl),
                           strlen (gemsoundtime));
                        map_mark ( &nsta, sta_lat, sta_lon, NULL, &ncolor, NULL,
                                &jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval,
                                &iposn, &iret );
                        stnList.nstn = nsta;
                        for ( i=0; i < nsta; i++ )
                        {
                          statlist[i][17] = 0;
                          stnList.lat[i] = sta_lat[i];
                          stnList.lon[i] = sta_lon[i];
                          stnList.elv[i] = sta_elev[i];
                          strcpy ( stnList.stnName[i], statlist[i] );
                        }
                   }

                  }
                }
             /*
              * Reset the zoom button
              */
             gem_info_zoom_cb( w, NULL, NULL);
             }
          else
             {
             zoom_state = 0;
             /*
              * restore default cursor
              */
             NuiDefaultCursor( gemlbl_station );
             /*
              * Restore mouse to be selection status
              */
             XSelectInput( gemdisplay, XtWindow(gemlbl_station),
                     ButtonPressMask | ButtonReleaseMask |
                     ExposureMask );

             XtAddEventHandler( gemlbl_station, ButtonPressMask, FALSE,
                     mapw_pickstnCb, NULL );
             }
        }

        /*NP*/
        void gem_info_unzoom_cb (Widget w, XtPointer client_data,
                                XtPointer call_data)
        /*************************************************************/
        /* GEM_INFO_UNZOOM_CB    (LJH)                               */
        /*************************************************************/
           {
           int i, iret,mapindx=0;
           strcpy(map_info[0].name,"NWWRD1");
           strcpy(map_info[0].proj, "CED/nm");
           /* strcpy(map_info[0].garea, "-90;-180;90;180" );
           mapb.x[0]=-90;mapb.x[1]=90;
           mapb.y[0]=-180;mapb.y[1]=180; */
           strcpy(map_info[0].garea, "-90;60;90;60" );
           mapb.x[0]=-90;mapb.x[1]=90;
           mapb.y[0]=60;mapb.y[1]=60;

           gclear(&iret);
           draw_map(mapindx, map_info, 0, &mapb, &iret);

           if (markSW) {
                get_gem_stns ( gemsoundfile, station_tbl, gemsoundtime,
                           statlist, &nsta, sta_lat, sta_lon, sta_elev,
                           &item_type,
                           strlen (gemsoundfile), strlen (station_tbl),
                           strlen (gemsoundtime));
                map_mark ( &nsta, sta_lat, sta_lon, NULL, &ncolor, NULL,
                        &jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval,
                        &iposn, &iret );
                stnList.nstn = nsta;
                for ( i=0; i < nsta; i++ )
                 {
                 statlist[i][17] = 0;
                 stnList.lat[i] = sta_lat[i];
                 stnList.lon[i] = sta_lon[i];
                 stnList.elv[i] = sta_elev[i];
                 strcpy ( stnList.stnName[i], statlist[i] );
                 }
           }

           }
        /*NP*/
        void parcel_cancel_cb (Widget w, XtPointer client_data,
                               XtPointer call_data)
       /*************************************************************/
       /* PARCEL_CANCEL_CB                                          */
       /*************************************************************/
          {
          Widget wid = (Widget) client_data;
          XtUnmanageChild (wid);
          }


        /*NP*/
        void show_about_info ( Widget w )
        /***********************************************************/
        /* SHOW_ABOUT_INFO                                         */
        /***********************************************************/
        {
        char    finam[200];

        sprintf( finam, "nsharp.hlp");
        printf( "Attempting to display contents of %s\n", finam);
        NxmHelp_loadFile(w, finam);
        /*NxmHelpLoadFile(finam);*/
        }

        /*NP*/
        void show_model_info ( Widget w )
       /*************************************************************/
       /* SHOW_MODEL_INFO                                           */
       /*************************************************************/
        {
        static Widget   mdlform, mdlform2, mdlpane,
                        mdlfile_label, mdlsta_label,
                        mdlfile_load, mdlfile_ok, mdlfile_cancel,
                        mdlfile_help, mdltype_label,
                        mdllbl_time,
                        mdlfile_world, mdlfile_US,
                        mdlfile_TROP_SFC, mdlfile_OFAGX,
                        mdlfile_ATL, mdlfile_PAC,
                        mdlfile_zoom, mdlfile_unzoom,
                        mdllbl_station, mdlstn_opt;
        XmString        str, str2, str3, str4, mdl_title;
        XmStringTable   str_list;
        char            *mdlouttext;
/*      char            *stalist[] = { "ruc", "ruc2",
                                       "eta", "ngm",
                                       "meso", "avn" }; */
        int             i, num,iret,mapindx,ier;
        Dimension wdth,hght;
        char            *gemouttext, gemdevice[72];
        /* LJH add */
        char path_work[80];
        char *strp;
        char *str_type[20];
        /* end add */
        XrmValue value;
        if (XrmGetResource(g_applicationDB,"sharp95*lf_panel.paths",
          "Sharp95*lf_panel.Paths",str_type,&value)
          == True) {
            strncpy(path_work, value.addr, (int) value.size);
            strp = strtok (path_work,"+");
            g_nummdls=0;
            while(g_nummdls < 7) {
              if (strp) {
                strcpy(g_mdllist[g_nummdls],strp);
                strp=strtok('\0',"+");
              }
              else
                strcpy(g_mdllist[g_nummdls]," ");


              g_nummdls++;
            }
        }

        if ( ! mdl_dialog )
           {
                   /* Initialize g_mdllist */


           /**************/
           mdl_title = XmStringCreateLocalized( "Model Sounding Selection" );
           mdl_dialog = XmCreateBulletinBoardDialog(toplevel, "mdl_panel",
                        NULL, 0);
           XtVaSetValues( mdl_dialog, XmNdialogTitle, mdl_title, NULL);
           /* XmStringFree(mdl_title); */

           mdlpane = XtVaCreateManagedWidget("parcel_pane",
                        xmPanedWindowWidgetClass,
                        mdl_dialog,
                        XmNsashWidth, 1,
                        XmNsashHeight, 1,
                        NULL);

           mdlform = XtVaCreateWidget("form", xmFormWidgetClass,
                        mdlpane, XmNfractionBase, 7,
                        NULL );

           str = XmStringCreateSimple ("GEMPAK Model file:");
           mdlfile_label = XtVaCreateManagedWidget ("mdlfile_label",
                        xmLabelWidgetClass, mdlform,
                        XmNlabelString, str,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 4,
                        NULL );
           XmStringFree (str);
           /* LJH add */
           strcpy(mdl_type, g_mdllist[0]);
           /* end     */
           mdlfile_text = XtVaCreateManagedWidget ("mdltext",
                        xmTextWidgetClass, mdlform,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, mdlfile_label,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 4,
                        NULL );

           str = XmStringCreateSimple ("Model times:");
           mdllbl_time = XtVaCreateManagedWidget ("mdlfile_time",
                        xmLabelWidgetClass, mdlform,
                        XmNlabelString, str,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_POSITION,
                        XmNtopPosition, 2,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 2,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        NULL );
           /* XmStringFree (str3); */

           mdlfile_timelist = XmCreateScrolledList ( mdlform,
                        "mdltimes", NULL, 0 );

           /*XtVaSetValues ( mdlfile_timelist,
                        XmNvisibleItemCount, 10, NULL );*/
           XtVaSetValues (mdlfile_timelist,
                        XmNselectionPolicy, XmEXTENDED_SELECT,
                        XmNvisibleItemCount, 15, NULL );
           XtVaSetValues (XtParent(mdlfile_timelist),
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, mdllbl_time,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 2,
                        NULL );

           /*XtAddCallback ( mdlfile_timelist,
                        XmNbrowseSelectionCallback,
                        mtime_select_cb,
                        (XtPointer)1 );*/

           XtAddCallback ( mdlfile_timelist,
                        XmNextendedSelectionCallback,
                        mtime_select_cb,
                        NULL );
           XtManageChild ( mdlfile_timelist );


           str = XmStringCreateSimple ("Select Model:");
           mdltype_label = XtVaCreateManagedWidget ("mdlfile_label",
                        xmLabelWidgetClass, mdlform,
                        XmNlabelString, str,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
           XmStringFree (str);

           /* num = XtNumber(stalist); */
                   num = XtNumber(g_mdllist);
           str_list = (XmStringTable) XtMalloc (num *sizeof (XmString *) );
           for (i=0; i < num; i++)
                str_list[i] = XmStringCreateLocalized(g_mdllist[i]);



           mdlstn_opt = XmVaCreateSimpleOptionMenu ( mdlform,
                        "mdlstn_opt", NULL, 'L', 0, modellist_cb,
                        XmVaPUSHBUTTON, str_list[0],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, str_list[1],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, str_list[2],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, str_list[3],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, str_list[4],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, str_list[5],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, str_list[6],  NULL, NULL, NULL,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, mdltype_label,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 6,
                        NULL );

           for ( i=0; i < num; i++ )
                        XmStringFree ( str_list[i] );
           XtFree ( (XtPointer)str_list );
           XtManageChild ( mdlstn_opt );

           str = XmStringCreateSimple ("Location:");
           mdlfile_label = XtVaCreateManagedWidget ("mdlfile_label",
                        xmLabelWidgetClass, mdlform,
                        XmNlabelString, str,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, mdlfile_text,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 1,
                        XmNalignment, XmALIGNMENT_END,
                        NULL );
           XmStringFree (str);
           mdl_statext = XtVaCreateManagedWidget ("mdl_statext",
                        xmTextWidgetClass, mdlform,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, mdlfile_label,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, mdlfile_text,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 4,
                        XmNcolumns, 12,
                        NULL );
           XtAddCallback ( mdl_statext, XmNvalueChangedCallback,
                       set_mdlsta, NULL );
           XtAddCallback ( mdl_statext, XmNactivateCallback,
                       load_mdldata, NULL );

           gemlbl_station2 = XtVaCreateManagedWidget("map2",
                        xmDrawingAreaWidgetClass, mdlform,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget,  mdl_statext,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget,XtParent(mdlfile_timelist),
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNwidth, 500, XmNbackground, pixels[0],
                        NULL);
          XtAddCallback( gemlbl_station2, XmNexposeCallback,
                        mapw_exposeCb, NULL );
          XtAddEventHandler( gemlbl_station2, ButtonPressMask,
                        FALSE, GetSoundingForLatLon, NULL );
          XtAddEventHandler( gemlbl_station2, PointerMotionMask, FALSE,
                        (XtEventHandler)mdlmap_pointer,
                        (XtPointer)NULL);

          /* HERE */
          XtManageChild ( mdlform );

          XtVaGetValues(gemlbl_station2,
                      XmNwidth,  &wdth,
                      XmNheight, &hght,
                      NULL);

           mdlform2 = XtVaCreateWidget("form", xmFormWidgetClass,
                        mdlpane, XmNfractionBase, 10,
                        XmNwidth, 980, XmNheight, 60,
                      NULL );
           mdl_cursor_text = XtVaCreateManagedWidget ("mdl_cursor_text",
                                xmTextFieldWidgetClass, mdlform2,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 0,
                                XmNtopAttachment, XmATTACH_FORM,
                                XmNbottomAttachment, XmATTACH_POSITION,
                                XmNbottomPosition, 6,
                                /*XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 1, */
                                XmNwidth, 150,
                                NULL );
                                /*left pos 0 to 5 */

           mdlfile_cancel = XtVaCreateManagedWidget ("CANCEL",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 2,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 3,
                        NULL );
           XtAddCallback(mdlfile_cancel, XmNactivateCallback,
                        (XtCallbackProc)mdl_info_cancel_cb, NULL);
           mdlfile_world = XtVaCreateManagedWidget ("WORLD",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 3,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_POSITION,
                        XmNbottomPosition, 5,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 4,
                        NULL );
           XtAddCallback(mdlfile_world, XmNactivateCallback,
                        (XtCallbackProc)mdl_info_world_cb, NULL);
           mdlfile_US = XtVaCreateManagedWidget ("U.S.",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 3,
                        XmNtopAttachment, XmATTACH_POSITION,
                        XmNtopPosition, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 4,
                        NULL );
           XtAddCallback(mdlfile_US, XmNactivateCallback,
                        (XtCallbackProc)mdl_info_US_cb, NULL);
           mdlfile_TROP_SFC = XtVaCreateManagedWidget ("TROPSFC",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 4,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_POSITION,
                        XmNbottomPosition, 5,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 5,
                        NULL );
           XtAddCallback(mdlfile_TROP_SFC, XmNactivateCallback,
                        (XtCallbackProc)mdl_info_TROP_SFC_cb, NULL);
           mdlfile_OFAGX = XtVaCreateManagedWidget ("OFAGX",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_POSITION,
                        XmNbottomPosition, 5,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 6,
                        NULL );
           XtAddCallback(mdlfile_OFAGX, XmNactivateCallback,
                        (XtCallbackProc)mdl_info_OFAGX_cb, NULL);
           mdlfile_ATL = XtVaCreateManagedWidget ("ATLANTIC",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 4,
                        XmNtopAttachment, XmATTACH_POSITION,
                        XmNtopPosition, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 5,
                        NULL );
           XtAddCallback(mdlfile_ATL, XmNactivateCallback,
                        (XtCallbackProc)mdl_info_ATL_cb, NULL);
           mdlfile_PAC = XtVaCreateManagedWidget ("PACIFIC",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 5,
                        XmNtopAttachment, XmATTACH_POSITION,
                        XmNtopPosition, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 6,
                        NULL );
           XtAddCallback(mdlfile_PAC, XmNactivateCallback,
                        (XtCallbackProc)mdl_info_PAC_cb, NULL);

           /* Offset everything below by 2 more */
           mdlfile_zoom = XtVaCreateManagedWidget ("ZOOM",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 7,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 8,
                        NULL );
           XtAddCallback(mdlfile_zoom, XmNactivateCallback,
                        (XtCallbackProc)mdl_info_zoom_cb, NULL);
           mdlfile_unzoom = XtVaCreateManagedWidget ("UNZOOM",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 8,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 9,
                        NULL );
           XtAddCallback(mdlfile_unzoom, XmNactivateCallback,
                        (XtCallbackProc)mdl_info_unzoom_cb, NULL);
           mdlfile_help = XtVaCreateManagedWidget ("HELP",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 9,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 10,
                        NULL );

           XtManageChild ( mdlform2 );
           XtManageChild ( mdlpane );
           XtManageChild ( mdl_dialog );
           model_mapw_rgstr( gemlbl_station2 );
           XtVaGetValues ( mdlfile_text, XmNvalue, &mdlouttext, NULL );
           if ( mdlouttext != NULL )
              {
              strcpy ( mdlsoundfile, mdlouttext );
              get_mdl_times ( mdlsoundfile, mtime_list, &mtimes,
                        strlen(mdlsoundfile) );
              mdlsoundtime[0] = '\0';
              str_list = (XmStringTable) XtMalloc (mtimes *
                        sizeof (XmString) );
              for ( i=0; i < mtimes; i++ )
                  {
                  /* mtime_list[i][18] = 0;  ??? */
                  str_list[i] =
                  XmStringCreateLocalized (mtime_list[-i]);
                  }
              XtVaSetValues ( mdlfile_timelist,
                        XmNitems, str_list,
                        XmNitemCount, mtimes, NULL );
              for ( i=0; i < mtimes; i++ )
                  XmStringFree ( str_list[i] );
              XtFree ( (XtPointer)str_list );
              }
           XtFree ( mdlouttext );
           mdl_info_US_cb(gemlbl_station2,(XtPointer) NULL, (XtPointer) NULL);
           } else {
              draw_model_map();

              if (g_lastmodelPFS) {
                /* load_mdl_times(); */
                XmListDeselectAllItems(mdlfile_timelist);
                g_lastmodelPFS=0;

              } else {
                map_mark ( &nsta, &g_lat, &g_lng, NULL, &ncolor, NULL,
                       &jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval,
                       &iposn, &iret );
              }
           }
           XtManageChild ( mdl_dialog );
           strcpy(gemdevice, "maptop2");
           gslwin(gemdevice, &iret, strlen(gemdevice));
        }

        void show_pfs_model_info (Widget w) {
        static Widget mdlform,mdlfile_label,mdlpane,mdlform2,mdlfile_cancel,mdlstn_opt,
          mdllbl_time,mdltype_label,mdlfile_zoom,mdlfile_unzoom,mdlfile_NAmerica, mdlfile_US;
        XmString str,str2,str3,str4,mdl_title;
        int             i, num,iret,mapindx,ier;
        Dimension wdth,hght;
        XmStringTable str_list;
        char *gemouttext,gemdevice[72];
        /* LJH add */
        char path_work[80];
        char *strp;
        char *str_type[20];
        char *mdlouttext;
        char gemmodelstring[80];
        /* end add */
        XrmValue value;
        g_pfs_model_sel_sw=-1;
        g_lastmodelPFS=-1;

        if (XrmGetResource(g_applicationDB,"sharp95*lf_pfs_panel.paths",
          "Sharp95*lf_pfs_panel.Paths",str_type,&value)
          == True) {
            strncpy(path_work, value.addr, (int) value.size);
            strp = strtok (path_work,"+");
            g_nummdls=0;
            while(g_nummdls < 7) {
              if (strp) {
                strcpy(g_mdllist[g_nummdls],strp);
                strp=strtok('\0',"+");
              }
              else
                strcpy(g_mdllist[g_nummdls]," ");


              g_nummdls++;
            }
        }
        if ( ! pfs_mdl_dialog )
           {
                   /* Initialize g_mdllist */


           /**************/
           mdl_title = XmStringCreateLocalized( "Model Point Fcst Sounding Selection" );
           pfs_mdl_dialog = XmCreateBulletinBoardDialog(toplevel, "pfsmdl_panel",
                        NULL, 0);
           XtVaSetValues( pfs_mdl_dialog, XmNdialogTitle, mdl_title, NULL);
           /* XmStringFree(mdl_title); */

           mdlpane = XtVaCreateManagedWidget("parcel_pane",
                        xmPanedWindowWidgetClass,
                        pfs_mdl_dialog,
                        XmNsashWidth, 1,
                        XmNsashHeight, 1,
                        NULL);

           mdlform = XtVaCreateWidget("form", xmFormWidgetClass,
                        mdlpane, XmNfractionBase, 7,
                        NULL );

           str = XmStringCreateSimple ("GEMPAK PFS Model file:");
           mdlfile_label = XtVaCreateManagedWidget ("pfsmdlfile_label",
                        xmLabelWidgetClass, mdlform,
                        XmNlabelString, str,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 4,
                        NULL );
           XmStringFree (str);
           /* LJH add */
           strcpy(mdl_type, g_pfsmdllist[0]);
           /* end  */
                   mdlfile_text = XtVaCreateManagedWidget ("mdltext",
                        xmTextWidgetClass, mdlform,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, mdlfile_label,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 4,
                        NULL );

           str = XmStringCreateSimple ("Model times:");
           mdllbl_time = XtVaCreateManagedWidget ("mdlfile_time",
                        xmLabelWidgetClass, mdlform,
                        XmNlabelString, str,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_POSITION,
                        XmNtopPosition, 2,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 2,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        NULL );
           /* XmStringFree (str3); */

           pfsmdlfile_timelist = XmCreateScrolledList ( mdlform,
                        "mdltimes", NULL, 0 );

           /*XtVaSetValues ( mdlfile_timelist,
                        XmNvisibleItemCount, 10, NULL );*/
           XtVaSetValues (pfsmdlfile_timelist,
                        XmNselectionPolicy, XmEXTENDED_SELECT,
                        XmNvisibleItemCount, 15, NULL );
           XtVaSetValues (XtParent(pfsmdlfile_timelist),
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, mdllbl_time,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 2,
                        NULL );

           /*XtAddCallback ( mdlfile_timelist,
                        XmNbrowseSelectionCallback,
                        mtime_select_cb,
                        (XtPointer)1 );*/

           XtAddCallback ( pfsmdlfile_timelist,
                        XmNextendedSelectionCallback,
                        pfsmtime_select_cb,
                        NULL );
           XtManageChild ( pfsmdlfile_timelist );


           str = XmStringCreateSimple ("Select Model:");
           mdltype_label = XtVaCreateManagedWidget ("mdlfile_label",
                        xmLabelWidgetClass, mdlform,
                        XmNlabelString, str,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
           XmStringFree (str);

           /* num = XtNumber(stalist); */
                   num = XtNumber(g_mdllist);
           str_list = (XmStringTable) XtMalloc (num *sizeof (XmString *) );
           for (i=0; i < num; i++)
                str_list[i] = XmStringCreateLocalized(g_mdllist[i]);

           mdlstn_opt = XmVaCreateSimpleOptionMenu ( mdlform,
                        "mdlstn_opt", NULL, 'L', 0, modellist_cb,
                        XmVaPUSHBUTTON, str_list[0],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, str_list[1],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, str_list[2],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, str_list[3],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, str_list[4],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, str_list[5],  NULL, NULL, NULL,
                        XmVaPUSHBUTTON, str_list[6],  NULL, NULL, NULL,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, mdltype_label,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 6,
                        NULL );

           for ( i=0; i < num; i++ )
                        XmStringFree ( str_list[i] );
           XtFree ( (XtPointer)str_list );
           XtManageChild (mdlstn_opt);
           gemlbl_station3 = XtVaCreateManagedWidget("map3",
                        xmDrawingAreaWidgetClass, mdlform,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget,  mdlstn_opt,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget,XtParent(pfsmdlfile_timelist),
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNwidth, 500, XmNbackground, pixels[0],
                        NULL);
           XtAddCallback( gemlbl_station3, XmNexposeCallback,
                        mapw_exposeCb, NULL );
           XtAddEventHandler( gemlbl_station3, ButtonPressMask,
                        FALSE, mapw_pickstnCb, NULL);

           XtManageChild(mdlform);
           XtVaGetValues(gemlbl_station3,
                      XmNwidth,  &wdth,
                      XmNheight, &hght,
                      NULL);

           mdlform2 = XtVaCreateWidget("form", xmFormWidgetClass,
                      mdlpane, XmNfractionBase, 10,
                      XmNwidth, 980, XmNheight, 60,
                      NULL );
           mdlfile_cancel = XtVaCreateManagedWidget ("CANCEL",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 2,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 3,
                        NULL );
           XtAddCallback(mdlfile_cancel, XmNactivateCallback,
                        (XtCallbackProc)pfs_mdl_info_cancel_cb, NULL);
           mdlfile_NAmerica = XtVaCreateManagedWidget("North\nAmerica",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 4,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 5,
                        NULL );
           XtAddCallback(mdlfile_NAmerica, XmNactivateCallback,
                         (XtCallbackProc)pfs_mdl_info_unzoom_cb, NULL);
           mdlfile_US = XtVaCreateManagedWidget("U.S.",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 6,
                        NULL );
           XtAddCallback(mdlfile_US, XmNactivateCallback,
                         (XtCallbackProc)pfs_mdl_info_US_cb, NULL);
           mdlfile_zoom = XtVaCreateManagedWidget("ZOOM",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 7,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 8,
                        NULL );
           XtAddCallback(mdlfile_zoom, XmNactivateCallback,
                        (XtCallbackProc)pfs_mdl_info_zoom_cb, NULL);

           mdlfile_unzoom = XtVaCreateManagedWidget("UNZOOM",
                        xmPushButtonWidgetClass, mdlform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 8,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 9,
                        NULL );
           XtAddCallback(mdlfile_unzoom, XmNactivateCallback,
                        (XtCallbackProc)pfs_mdl_info_unzoom_cb, NULL);

           XtManageChild(mdlform2);
           XtManageChild(mdlpane);
           XtManageChild(pfs_mdl_dialog);
           pfs_model_mapw_rgstr(gemlbl_station3);
         } else {
           strcpy(gemdevice,"maptop3");
           gslwin(gemdevice,&iret,strlen(gemdevice));
           XtManageChild(pfs_mdl_dialog);
         }

        }

        /*NP*/
        void pfs_mdl_info_cancel_cb (Widget w, XtPointer client_data,
                               XtPointer call_data)
       /*************************************************************/
       /* MDL_INFO_CANCEL_CB                                        */
       /*************************************************************/
          {
          XtUnmanageChild (pfs_mdl_dialog);
          }
          /*NP*/
        void pfs_mdl_info_zoom_cb (Widget w, XtPointer client_data,
                                   XtPointer call_data)
        {
          static int zoom_state = 0;
          int i, ityp, np, iret, mapindx=0;
          char sysin[2], sysout[2];
          float xpts[2], ypts[2], xdev[2], ydev[2];
          void mapw_pickstnCb();
          if(zoom_state == 0)
          {
            zoom_state = 1;
             /*
            * Change mouse event handling for zooming
             */
            XtRemoveEventHandler( gemlbl_station3, ButtonPressMask, FALSE,
                                  mapw_pickstnCb, NULL);


              /*
            * change to the zoom cursor
              */
            NxmCursorChange( gemlbl_station3, XC_crosshair, "white");
            XmUpdateDisplay( w );
            ityp = 3; /* 2; */
            np = 2;
            strcpy ( sysin, "D" );
            for ( i = 0; i < np; i++)
              xpts[i] = ypts[i] = 0.0;
            ggtpnt( sysin, &ityp, xdev, ydev, &iret, strlen(sysin));
             /*
            * check if the box is big enough
             */
            if ( fabs(xdev[0] - xdev[1]) > 10 &&
                 fabs(ydev[0] - ydev[1]) > 10 )
            {
                /*
              * Points need to be in lower-left, upper-right order
                */
              if (xdev[0] > xdev[1]) {
                i = xdev[0];
                xdev[0] = xdev[1];
                xdev[1] = i;
              }
              if (ydev[0] < ydev[1]) {
                i = ydev[0];
                ydev[0] = ydev[1];
                ydev[1] = i;
              }
              strcpy ( sysout, "M" );
              gtrans(sysin, sysout, &np, xdev, ydev, xpts, ypts,
                     &iret, strlen(sysin), strlen(sysout));
              if ( iret == 0 )
              {
                for ( i = 0; i < np; i++)
                {
                  mapb.x[i] = xpts[i];
                  mapb.y[i] = ypts[i];
                }
                gclear(&iret);
                draw_map(mapindx, map_info, 1, &mapb, &iret);
                g_pfs_mapset=-1;

                if (markSW) {
                  Load_PFS_stationlist(g_pfs_mapset);
                }

              }
            }
             /*
            * Reset the zoom button
             */
            pfs_mdl_info_zoom_cb( w, NULL, NULL);
          }
          else
          {
            zoom_state = 0;
             /*
            * restore default cursor
             */
            NuiDefaultCursor( gemlbl_station3 );
             /*
            * Restore mouse to be selection status
             */
            XSelectInput( gemdisplay, XtWindow(gemlbl_station3),
                          ButtonPressMask | ButtonReleaseMask |
                              ExposureMask );

            XtAddEventHandler( gemlbl_station3, ButtonPressMask, FALSE,
                               mapw_pickstnCb, NULL );
          }
        }

        void pfs_mdl_info_US_cb (Widget w, XtPointer client_data,
                                 XtPointer call_data)
        {
          int i, iret,mapindx=0;

          strcpy ( map_info[0].name, "US" );
          strcpy ( map_info[0].proj, "STR/90;-105;0/nm" );
          strcpy ( map_info[0].garea, "22.88;-120.49;46.02;-60.83" );
          mapb.x[0] = 22.88;mapb.x[1]=46.02;
          mapb.y[0] = -120.49;mapb.y[1]=-60.83;
          gclear(&iret);
          draw_map(mapindx, map_info, 0, &mapb, &iret);
          g_pfs_mapset=-1;
          if (markSW) {
            Load_PFS_stationlist(g_pfs_mapset);
          }
        }

        void pfs_mdl_info_unzoom_cb (Widget w, XtPointer client_data,
                                     XtPointer call_data)
        {
          int i, iret,mapindx=0;
          strcpy(map_info[0].name,"LLVL2");
          strcpy(map_info[0].proj, "STR/90;-97;0/0;0;0;0");
          strcpy(map_info[0].garea, "5;-125;46;-32.5" );
          mapb.x[0]=5;mapb.x[1]=46;
          mapb.y[0]=-125;mapb.y[1]=-32.5;

          gclear(&iret);
          draw_map(mapindx, map_info, 0, &mapb, &iret);

          g_pfs_mapset=-1;

          if (markSW) {
            Load_PFS_stationlist(g_pfs_mapset);
          }
        }

        /*NP*/
        void draw_model_map()
        /*****************************************************************/
        /* DRAW_MODEL_MAP                                                */
        /*****************************************************************/
        {
          int             iret, mapindx=0;
          char            gemdevice[72];
          if (g_allModelParamsSW || g_pfs_model_sel_sw) {
            if (g_pfs_model_sel_sw)
              strcpy(gemdevice,"maptop3");
            else
              strcpy(gemdevice,"maptop2");
            gslwin(gemdevice, &iret, strlen(gemdevice));
            map_init(&iret, gemdevice, strlen(gemdevice));
            if (! g_WorldProj && g_Proj<=1) {
                  strcpy ( map_info[0].name, "US" );
                  strcpy ( map_info[0].proj, "STR/90;-105;0/nm" );
                  strcpy ( map_info[0].garea, "22.88;-120.49;46.02;-60.83" );
                  mapb.x[0] = 22.88;mapb.x[1]=46.02;
                  mapb.y[0] = -120.49;mapb.y[1]=-60.83;
                  /* gclear(&iret); */
                  draw_map(mapindx, map_info, 0, &mapb, &iret);

            } else {
                 /* gclear(&iret); */
                 draw_map(mapindx, map_info, 1, &mapb, &iret);
            }
          }
        }

        /*NP*/
        model_mapw_rgstr(mapwin)
        Widget mapwin;

/************************************************************************
 * model_mapw_rgstr                                                     *
 *                                                                      *
 * This routine will initiliaze the GEMPAK display variables and        *
 * register the map drawing area into a GEMPAK window.                  *
 *                                                                      *
 * model_mapw_rgstr ( mapwin )                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *      mapwin        Widget          The map drawing area widget       *
 *                                                                      *
 * Return code:                                                         *
 *      0 -- Successful.                                                *
 *     -1 -- Error.                                                     *
 **                                                                     *
 ***********************************************************************/

{
XColor          cred;
Dimension       wdth, hght;
/*struct maptype_list     map_info[2];
mapbnd_t                mapb;*/


Window          gwin;
GC              gemgc;
Cursor          curs;

int             xwdth, xhght, xdpth;

int             iret, mapindx=0;
char            gemdevice[72];

/*---------------------------------------------------------------------*/

        /*
         * Get the map window geometry.
         */
        XtVaGetValues(mapwin,
                      XmNwidth,  &wdth,
                      XmNheight, &hght,
                      NULL);

        xwdth = (int)wdth;
        xhght = (int)hght;
        xdpth = DefaultDepthOfScreen(XtScreen(mapwin));

        /*
         * Set the window and graphics context.
         */
        gwin  = XtWindow(mapwin);
        gemgc = XCreateGC(gemdisplay, gwin, 0, 0);

        /*
         * Create a red arrow for the cursor.
         */
        curs = XCreateFontCursor(gemdisplay, XC_top_left_arrow);
        XDefineCursor(gemdisplay, gwin, curs);
        cred.red   = 65535;
        cred.blue  = 0;
        cred.green = 0;
        cred.flags = DoRed | DoBlue | DoGreen;
        XRecolorCursor(gemdisplay, curs, &cred, &cred);

        /*
         * Set the fill rule.
         */
        XSetFillRule(gemdisplay, gemgc, WindingRule);

        /*
         * Register the map window
         */
        strcpy(gemdevice,"maptop2");
        xmotifw(gwin, gemdevice, gemgc, xwdth, xhght, xdpth, &iret);
        if( iret != 0 )
                return( iret );
        /*
         * Draw the US map.     FORTRAN function map_init()
         */
        map_init(&iret, gemdevice, strlen(gemdevice));
        if (! g_WorldProj && g_Proj<=1) {
          strcpy ( map_info[0].name, "US" );
          strcpy ( map_info[0].proj, "STR/90;-105;0/nm" );
          strcpy ( map_info[0].garea, "22.88;-120.49;46.02;-60.83" );
          mapb.x[0] = 22.88;mapb.x[1]=46.02;
          mapb.y[0] = -120.49;mapb.y[1]=-60.83;
          mapindx = 0;
        }
        draw_map(mapindx, map_info, 0, &mapb, &iret);

        return( 0 );
        }

        /*NP*/
        pfs_model_mapw_rgstr(Widget mapwin)
        /************************************************************************
         * model_pdsmapw_rgstr                                                  *
         *                                                                      *
         * This routine will initiliaze the GEMPAK display variables and        *
         * register the map drawing area into a GEMPAK window.                  *
         *                                                                      *
         * model_mapw_rgstr ( mapwin )                                          *
         *                                                                      *
         * Input parameters:                                                    *
         *      mapwin        Widget          The map drawing area widget       *
         *                                                                      *
         * Return code:                                                         *
         *      0 -- Successful.                                                *
         *     -1 -- Error.                                                     *
         **                                                                     *
         ***********************************************************************/

{
XColor          cred;
Dimension       wdth, hght;
/*struct maptype_list     map_info[2];
mapbnd_t                mapb;*/


Window          gwin;
GC              gemgc;
Cursor          curs;

int             xwdth, xhght, xdpth;

int             iret, mapindx=0;
char            gemdevice[72];

/*---------------------------------------------------------------------*/

        /*
         * Get the map window geometry.
         */
        XtVaGetValues(mapwin,
                      XmNwidth,  &wdth,
                      XmNheight, &hght,
                      NULL);

        xwdth = (int)wdth;
        xhght = (int)hght;
        xdpth = DefaultDepthOfScreen(XtScreen(mapwin));

        /*
         * Set the window and graphics context.
         */
        gwin  = XtWindow(mapwin);
        gemgc = XCreateGC(gemdisplay, gwin, 0, 0);

        /*
         * Create a red arrow for the cursor.
         */
        curs = XCreateFontCursor(gemdisplay, XC_top_left_arrow);
        XDefineCursor(gemdisplay, gwin, curs);
        cred.red   = 65535;
        cred.blue  = 0;
        cred.green = 0;
        cred.flags = DoRed | DoBlue | DoGreen;
        XRecolorCursor(gemdisplay, curs, &cred, &cred);

        /*
         * Set the fill rule.
         */
        XSetFillRule(gemdisplay, gemgc, WindingRule);

        /*
         * Register the map window
         */
        strcpy(gemdevice,"maptop3");
        xmotifw(gwin, gemdevice, gemgc, xwdth, xhght, xdpth, &iret);
        if( iret != 0 )
                return( iret );
        /*
         * Draw the US map.     FORTRAN function map_init()
         */
        map_init(&iret, gemdevice, strlen(gemdevice));
        if (! g_WorldProj && g_Proj<=1) {
          strcpy ( map_info[0].name, "US" );
          strcpy ( map_info[0].proj, "STR/90;-105;0/nm" );
          strcpy ( map_info[0].garea, "22.88;-120.49;46.02;-60.83" );
          mapb.x[0] = 22.88;mapb.x[1]=46.02;
          mapb.y[0] = -120.49;mapb.y[1]=-60.83;
          mapindx = 0;
        }
        draw_map(mapindx, map_info, 0, &mapb, &iret);

        return( 0 );
        }



        /*NP*/
        void GetSoundingForLatLon(mapwin, client_data, event )
        /*************************************************************/
        /* GETSOUNDINGFORLATLON                                      */
        /*************************************************************/
        Widget mapwin;
        XtPointer client_data;
        XEvent *event;
        {
        char            gemdevice[72];
        int              mapindx=0;
          int np,iret;
          float lat,lon,xloc,yloc;
          static long count=0;
          char sys1[2];
          char sys2[2];
          static char lastModelSoundTime[200];
          strcpy(sys1,"D");
          strcpy(sys2,"M");
          if ((event->xbutton.button == 1) ||
              (event->xbutton.button == 2) ||
              (event->xbutton.button == 3)) {
            if (count==0 && g_allModelParamsSW) {
                /* Redraw Map - Temp fix to coordinate trans */
                g_mlocationSelected=-1;
                /*
                * Draw the US map.      FORTRAN function map_init()
                */
                strcpy(gemdevice,"maptop2");
                map_init(&iret, gemdevice, strlen(gemdevice));
                draw_map(mapindx, map_info, 1, &mapb, &iret);
                /*count++;*/
            }

            xloc=(float) event->xbutton.x;
            yloc=(float) event->xbutton.y;
            np=1;
            gtrans( sys1, sys2, &np, &xloc, &yloc,
                      &lat, &lon, &iret, strlen(sys1), strlen(sys2) );
            if(g_allModelParamsSW) {
               sprintf(mdlsoundsta,"%s","ZZZ");
               g_lng=lon;g_lat=lat;
                ncolor    = 1;
               mrktyp    = 6;
               sizmrk    = 1.0;
               mrkwid    = 2;
               pltval    = G_FALSE;
               iposn     =  0;
               jcolr     = 2;
               nsta=1;

               map_mark ( &nsta, &g_lat, &g_lng, NULL, &ncolor, NULL,
                       &jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval,
                       &iposn, &iret );
               if  (mdlsoundtime[0] == '\0')
                 strcpy(mdlsoundtime,lastModelSoundTime);
               else
                 strcpy(lastModelSoundTime,mdlsoundtime);
               Load_mdl_sounding();
               }
            }
        }
       /*NP*/
       void mdlmap_pointer(Widget w, XtPointer *call_data, XEvent *event)
        /*************************************************************/
        /* POINTER_UPDATE                                            */
        /*                                                           */
        /* Handles "mouse moved w/ no buttons" event                 */
        /*************************************************************/
        {
        char sysin[3],sysout[3],cursor_pos[81];
        float xdev,ydev,lat,lon;
        int iret,np=1;
        xdev = (float) event->xbutton.x; ydev = (float) event->xbutton.y;

        strcpy ( sysin, "D" );
        strcpy ( sysout, "M" );
        gtrans(sysin, sysout, &np, &xdev, &ydev, &lat, &lon,
                       &iret, strlen(sysin), strlen(sysout));
        cursor_pos[0] = '\0';
        sprintf(cursor_pos,"%6.2f,%7.2f\0",lat,lon);
        XtVaSetValues ( mdl_cursor_text, XmNvalue, cursor_pos, NULL );
        XFlush (XtDisplay(gemlbl_station2));
        }


        /*NP*/
        void modellist_cb (Widget w, XtPointer client_data,
                             XtPointer call_data)
       /*************************************************************/
       /* MODELLIST_CB                                              */
       /*************************************************************/
          {
          int item_no = (int) client_data;
          static Widget load_filemdl;
          char  *file_dir, directory[200], pattern[200];
          XmString      directory_str, pattern_str;
          /* LJH add */
          static Widget lffile_list, lfpane, lfform, lfform2, lf_ok, lf_cancel;
          XmString lf_title;
          char file_dir_work[80];
          char *str_type[20];
          XrmValue value;
          char ext_work[80];
          char mdlpattern[7][10];
          char *strp;
          int nummdls;
          int fhrpos;
          char searchfcstchar;
          int mfcountisshr=-1;
          int i;
          char lf_mdllistrunhr [50][80];

          /***********/
          if ( ! lf_dialog )
            {
              lf_title= XmStringCreateLocalized("Model Data Selection");
              lf_dialog=XmCreateBulletinBoardDialog(toplevel, "lf_panel",
                                                 NULL, 0);
              XtVaSetValues( lf_dialog, XmNdialogTitle, lf_title, NULL);

              lfpane = XtVaCreateManagedWidget("listing_file_pane",
                                xmPanedWindowWidgetClass,
                                lf_dialog,
                                XmNsashWidth, 1,
                                XmNsashHeight, 1,
                                NULL);
              lfform = XtVaCreateWidget("form",xmFormWidgetClass,
                                   lfpane, XmNfractionBase, 3,
                                   NULL);
              lffile_list =
              XmCreateScrolledList(lfform,"lftimes",NULL,0);
              XtVaSetValues(lffile_list,XmNvisibleItemCount,10, NULL);
              lfform2 = XtVaCreateWidget("form",xmFormWidgetClass,lfpane,
                XmNfractionBase,3, NULL);

/*            lf_ok = XtVaCreateManagedWidget("OK", xmPushButtonWidgetClass,
                lfform2,XmNleftAttachment,XmATTACH_FORM,XmNrightAttachment,
                XmATTACH_POSITION,XmNrightPosition, 1, NULL); */
              lf_cancel =  XtVaCreateManagedWidget("CANCEL", xmPushButtonWidgetClass,
                lfform2,XmNleftAttachment,XmATTACH_POSITION,XmNleftPosition, 1,
                XmNrightAttachment,XmATTACH_POSITION,XmNrightPosition, 2,
                /* XmNrightAttachment, XmATTACH_FORM, */ NULL);

              XtAddCallback(lffile_list, XmNbrowseSelectionCallback,
                lf_loadmdlfiles_cb, NULL);
/*            XtAddCallback(lf_ok, XmNactivateCallback,
                           (XtCallbackProc) lf_loadmdlfiles_cb, NULL);*/
              XtAddCallback(lf_cancel, XmNactivateCallback,
                           (XtCallbackProc) lf_cancel_cb, NULL);

            }
          XtManageChild(lffile_list);
          XtManageChild(lfform);
          XtManageChild(lfform2);
          XtManageChild(lfpane);
          XtManageChild(lf_dialog);
          /* Choose Loading based on g_pfs_model_sel_sw */
          if (g_pfs_model_sel_sw) {



            /*LJH alter...Obtain file_directory from lf_panel.dir resource */
            if (XrmGetResource(g_applicationDB,"sharp95*lf_pfs_panel.dir",
                "Sharp95*lf_pfs_panel.Dir",str_type,&value)
                == True) {
                strncpy(file_dir_work,value.addr,(int) value.size);
                file_dir=file_dir_work;
            }
            else
                file_dir = NULL;

            if (XrmGetResource(g_applicationDB,"sharp95*lf_pfs_panel.patterns",
              "Sharp95*lf_pfs_panel.Patterns",str_type,&value)
              == True) {
              strncpy(ext_work,value.addr,(int) value.size);
              strp=strtok(ext_work,"+");
              nummdls=0;
              while (nummdls < 7) {
                if (strp) {
                  strcpy(mdlpattern[nummdls],strp);
                  strp=strtok('\0',"+");
                }
                else
                  strcpy(mdlpattern[nummdls],"");

                nummdls++;
              }
            }
            sprintf(directory,"%s",file_dir);
            sprintf(pattern,g_mdllist[item_no]);
            sprintf(g_pattern,mdlpattern[item_no]);
            sprintf(mdl_selected,g_mdllist[item_no]);
            for (i=0;i<strlen(mdl_selected);i++)
              mdl_selected[i]=toupper(mdl_selected[i]);

             strcpy( mdl_type, mdl_selected);
             /* Using Intrinsic function "scandir" to grab onto all files
              in specified directory */
             sprintf(g_pfs_pathname,"%s/%s",directory,pattern);
             if (g_mfcountall != 0) {
               for (i=0;i<g_mfcountall;i++)
                 free (g_mdlfiles[i]);
               free (g_mdlfiles);
             }

             g_mfcountall=scandir(g_pfs_pathname,&g_mdlfiles,file_select,alphasort);
             /* Condense list down from YYYYMMDDHHfhh.mdl to YYYYMMDDHH.mdl */
             mfcountisshr=-1;

             if(g_mfcountall > 0) {
               mfcountisshr=0;
               strcpy(lf_mdllistrunhr[mfcountisshr],g_mdlfiles[0]->d_name);
               /* free(g_mdlfiles[0]); */
               for (i=1;i<g_mfcountall;i++) {
                    mfcountisshr++;
                    strcpy(lf_mdllistrunhr[mfcountisshr],g_mdlfiles[i]->d_name);
               }
               /* free(g_mdlfiles); */
             }
             /* List complete...load to lffile_list */
             XmListDeleteAllItems(lffile_list);
             for (i=mfcountisshr;i>=0;i--) {
                AppendMdlFileToList(lffile_list, lf_mdllistrunhr[i]);

             }

          } else {
            /*LJH alter...Obtain file_directory from lf_panel.dir resource */
            if (XrmGetResource(g_applicationDB,"sharp95*lf_panel.dir",
                "Sharp95*lf_panel.Dir",str_type,&value)
                == True) {
                strncpy(file_dir_work,value.addr,(int) value.size);
                file_dir=file_dir_work;
            }
            else
                file_dir = NULL;

            if (XrmGetResource(g_applicationDB,"sharp95*lf_panel.patterns",
              "Sharp95*lf_panel.Patterns",str_type,&value)
              == True) {
              strncpy(ext_work,value.addr,(int) value.size);
              strp=strtok(ext_work,"+");
              nummdls=0;
              while (nummdls < 7) {
                if (strp) {
                  strcpy(mdlpattern[nummdls],strp);
                  strp=strtok('\0',"+");
                }
                else
                  strcpy(mdlpattern[nummdls],"");

                nummdls++;
              }
            }
            sprintf(directory,"%s",file_dir);
            sprintf(pattern,g_mdllist[item_no]);
            sprintf(g_pattern,mdlpattern[item_no]);
            sprintf(mdl_selected,g_mdllist[item_no]);
            for (i=0;i<strlen(mdl_selected);i++)
              mdl_selected[i]=toupper(mdl_selected[i]);

             strcpy( mdl_type, mdl_selected);
             /* Using Intrinsic function "scandir" to grab onto all files
              in specified directory */
             sprintf(g_pathname,"%s/%s",directory,pattern);
             if (g_mfcountall != 0) {
               for (i=0;i<g_mfcountall;i++)
                 free (g_mdlfiles[i]);
               free (g_mdlfiles);
             }

             g_mfcountall=scandir(g_pathname,&g_mdlfiles,file_select,alphasort);
             /* Condense list down from YYYYMMDDHHfhh.mdl to YYYYMMDDHH.mdl */
             mfcountisshr=-1;

             if(g_mfcountall > 0) {
               mfcountisshr=0;
               strcpy(lf_mdllistrunhr[mfcountisshr],g_mdlfiles[0]->d_name);
               /* free(g_mdlfiles[0]); */
               for (i=1;i<g_mfcountall;i++) {
                 searchfcstchar = (strchr(g_mdlfiles[i]->d_name,'f')!=NULL) ? 'f' : 'F';
                 fhrpos=(strchr(g_mdlfiles[i]->d_name,searchfcstchar)-(g_mdlfiles[i]->d_name));
                 if (strncmp(g_mdlfiles[i]->d_name,lf_mdllistrunhr[mfcountisshr],fhrpos)
                    != 0) {
                    mfcountisshr++;
                    strcpy(lf_mdllistrunhr[mfcountisshr],g_mdlfiles[i]->d_name);
                 }
                 /* free(g_mdlfiles[i]); */
               }
               /* free(g_mdlfiles); */
             }
             /* List complete...load to lffile_list */
             XmListDeleteAllItems(lffile_list);
             for (i=mfcountisshr;i>=0;i--) {
                AppendMdlFileToList(lffile_list, lf_mdllistrunhr[i]);

             }
           }
        }

void lf_loadmdlfiles_cb (Widget w, XtPointer client_data, XtPointer call_data )
        {
          struct dirent **mdlfiles;
          XmListCallbackStruct* ptr;
          char *string;
          char datestring[20];
          int count=0,i;
          char *buf;
          char date[9],runhr[5],time[4];
          int iret,mapindx=0;
          char gemdevice[72];
          char *str_type[20];
          XrmValue value;
          char parsestring[80];
          char searchfcstchar;
          int fhrpos;
          char gemmodelstring[80];
          XmStringTable str_list = NULL;
          int ncnt=0;
          ptr=(XmListCallbackStruct*) call_data;
          XmStringGetLtoR(ptr->item, XmSTRING_DEFAULT_CHARSET, &string);
          parsestring[0]='\0';
          if (g_pfs_model_sel_sw) {
            /*PFS model file handling...a lot different than regular files*/
            /*Build a list of times */
            strcpy ( g_pfs_mdlsoundfile, string );
            XtFree(string);
            load_pfs_times();
            XtUnmanageChild(lf_dialog);


          /**************/
          } else {
            if (XrmGetResource(g_applicationDB,"nsharp*lf_panel.parsestrng",
              "Sharp95*lf_panel.Parsestring",str_type,&value)
              == True)
              strncpy(parsestring,value.addr,(int) value.size);

            strcpy ( mdlsoundfile, string );
            XtFree(string);
            /* load_mdl_times(); */
            sprintf(gemmodelstring,"%s/%s",g_pathname,mdlsoundfile);
            XtVaSetValues(mdlfile_text, XmNvalue, gemmodelstring, NULL);
            /*****/
            searchfcstchar = (strchr(mdlsoundfile,'f')!=NULL) ? 'f' : 'F';
            fhrpos=(strchr(mdlsoundfile,searchfcstchar)-mdlsoundfile);
            strncpy(datestring,mdlsoundfile,fhrpos);
            datestring[fhrpos]='\0';
            /* count=scandir(g_pathname, &mdlfiles, file_select, alphasort); */
            XmListDeleteAllItems(mdlfile_timelist);
            mtimes=0;
            for (i=0;i<g_mfcountall;i++) {
              if ((buf=strstr(g_mdlfiles[i]->d_name,datestring)) != NULL) {
                /* printf("files...%s\n",g_mdlfiles[i]->d_name); */
                AppendMdlFileToList(mdlfile_timelist, g_mdlfiles[i]->d_name);
                strcpy(mtime_list[mtimes],g_mdlfiles[i]->d_name);
                if (parsestring[0]=='\0') {
                  if (sscanf(g_mdlfiles[i]->d_name,
                    "%*2s%6s%*[_]%2s%*[f]%2s",date,runhr,time)
                    < 3) {
                    sscanf(g_mdlfiles[i]->d_name,"%*2s%6s%2s%*[f]%2s",date,runhr,time);
                    }
                }
                else
                    sscanf(g_mdlfiles[i]->d_name,parsestring,date,runhr,time);
                sprintf(mdlsoundtime,"%s/%s00F0%2s\0",date,runhr,time);
                strcpy(mtimeac_list[mtimes],mdlsoundtime);
                mtimes++;
              }

              /* free(mdlfiles[i]); */
            }
            XtUnmanageChild(lf_dialog);
           }
         }

void load_mdl_times() {
   int i;
   char *buf;
   char date[9],runhr[5],time[4];
   int iret,mapindx=0;
   char datestring[20];
   char *str_type[20];
   XrmValue value;
   char parsestring[80];
   char searchfcstchar;
   int fhrpos;
   char gemmodelstring[80];
   XmStringTable str_list = NULL;
   int ncnt=0;
   if (XrmGetResource(g_applicationDB,"nsharp*lf_panel.parsestrng",
     "Sharp95*lf_panel.Parsestring",str_type,&value)
     == True)
     strncpy(parsestring,value.addr,(int) value.size);
   sprintf(gemmodelstring,"%s/%s",g_pathname,mdlsoundfile);
   XtVaSetValues(mdlfile_text, XmNvalue, gemmodelstring, NULL);
   searchfcstchar = (strchr(mdlsoundfile,'f')!=NULL) ? 'f' : 'F';
   fhrpos=(strchr(mdlsoundfile,searchfcstchar)-mdlsoundfile);
   strncpy(datestring,mdlsoundfile,fhrpos);
   datestring[fhrpos]='\0';
   mtimes=0;
   for (i=0;i<g_mfcountall;i++) {
     if ((buf=strstr(g_mdlfiles[i]->d_name,datestring)) != NULL) {
       /* printf("files...%s\n",g_mdlfiles[i]->d_name); */
       AppendMdlFileToList(mdlfile_timelist, g_mdlfiles[i]->d_name);
       strcpy(mtime_list[mtimes],g_mdlfiles[i]->d_name);
       if (parsestring[0]=='\0') {
         if (sscanf(g_mdlfiles[i]->d_name,
           "%*2s%6s%*[_]%2s%*[f]%2s",date,runhr,time)
           < 3) {
           sscanf(g_mdlfiles[i]->d_name,"%*2s%6s%2s%*[f]%2s",date,runhr,time);
           }
       }
       else
           sscanf(g_mdlfiles[i]->d_name,parsestring,date,runhr,time);
       sprintf(mdlsoundtime,"%s/%s00F0%2s\0",date,runhr,time);
       strcpy(mtimeac_list[mtimes],mdlsoundtime);
       mtimes++;
     }

     /* free(mdlfiles[i]); */
   }
 }


void load_pfs_times() {
    int ncnt=0;
    int iret=0,i;
    char gemmodelstring[80];
    XmStringTable str_list = NULL;
    sprintf(gemmodelstring,"%s/%s",g_pfs_pathname,g_pfs_mdlsoundfile);
    XtVaSetValues(mdlfile_text, XmNvalue, gemmodelstring, NULL);
    get_gem_times(gemmodelstring,time_list,&ntimes,&iret,strlen(gemmodelstring));
    if (iret == 0)
      {
      str_list = (XmStringTable) XtMalloc (ntimes * sizeof (XmString));
      for (i=0;i<ntimes;i++) {
        time_list[i][11]=0;
        str_list[i]=XmStringCreateLocalized(time_list[i]);
        strcpy(mtimeac_list[i],time_list[i]);
      }
      mtimes=ntimes;
      XtVaGetValues ( pfsmdlfile_timelist,
                         XmNitemCount, &ncnt, NULL );
      XtVaSetValues ( pfsmdlfile_timelist,
                         XmNitemCount, ntimes,
                         XmNitems, str_list,
                         NULL );
      for ( i=0; i < ntimes; i++ )
         XmStringFree ( (XmString )str_list[i] );
      XtFree ( (char *)str_list );
      }
}

int file_select(struct dirent *entry)
  {
  if ((strcmp(entry->d_name,".")==0) || (strcmp(entry->d_name,"..")==0))
    return (FALSE);
  if ((strstr(entry->d_name,g_pattern))==NULL)
    return (FALSE);
  return(TRUE);
  }


void AppendMdlFileToList ( Widget list, char* item)
  {
  XmString xmstring;
  xmstring = XmStringCreateSimple(item);
  XmListAddItemUnselected(list,xmstring,0);
  XmStringFree(xmstring);
  }



void lf_cancel_cb (Widget w, XtPointer client_data,
                                XtPointer call_data )
        {
        XtUnmanageChild (lf_dialog);
        }



        /*NP*/
        void get_mdlfile ( Widget w, XtPointer client_data,
                                       XtPointer call_data )
       /*************************************************************/
       /* GET_MDLFILE                                               */
       /*************************************************************/
             {
             char gemdevice[72];
             int iret,mapindx=0;
             char *file = NULL, filename[200];
             XmFileSelectionBoxCallbackStruct *cbs =
              (XmFileSelectionBoxCallbackStruct *) call_data;
             XmStringTable      str_list;
             int                i;

             if (cbs)
               {
               if ( !XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG,
                                      &file) )
                  return;
               XtVaSetValues ( mdlfile_text, XmNvalue, file, NULL );
               strcpy ( mdlsoundfile, file );
               XtFree(file);
               get_mdl_times ( mdlsoundfile, mtime_list, &mtimes,
                               strlen(mdlsoundfile) );
               mdlsoundtime[0] = '\0';
               str_list = (XmStringTable) XtMalloc (mtimes *
                                                sizeof (XmString *) );
               for ( i=0; i < mtimes; i++ )
                 {
                 /* mtime_list[i][18] = 0; ???*/
                 str_list[i] =
                     XmStringCreateLocalized (mtime_list[i]);
                 }
               XtVaSetValues ( mdlfile_timelist,
                        XmNitems, str_list,
                        XmNitemCount, mtimes, NULL );
               for ( i=0; i < mtimes; i++ )
                 XmStringFree ( str_list[i] );
               XtFree ( (XtPointer)str_list );
               }
               /* Added to Correct Lat/Lon trans Problem*/
/*             strcpy(gemdevice,"maptop2");
               map_init(&iret, gemdevice, strlen(gemdevice));
               gclear(&iret);
               draw_map(mapindx, map_info, 1, &mapb, &iret); */
             }


        /*NP*/
        void set_mdlsta ( Widget w, XtPointer client_data,
                                       XtPointer call_data )
       /*************************************************************/
       /* SET_MDLSTA                                                */
       /*************************************************************/
             {
             char       *text_sta;
             g_mlocationSelected=-1;

             text_sta = XmTextGetString (w);

             if ( text_sta != NULL )
                 strcpy ( mdlsoundsta, text_sta );

             XtFree(text_sta);
             }

        /*NP*/
       void set_pvsounding ( Widget w, XtPointer client_data,
                                       XtPointer call_data )
       /*************************************************************/
       /* SET_MDLSTA                                                */
       /*************************************************************/
             {
             char       *text_sta;
             text_sta = XmTextGetString (w);

             if ( text_sta != NULL )
                 strcpy ( g_pvsound, text_sta );

             XtFree(text_sta);
             }

        /*NP*/
        void set_user_level ( Widget w, XtPointer client_data,
                                       XtPointer call_data )
       /*************************************************************/
       /* SET_USER_LEVEL                                            */
       /*************************************************************/
             {
             char       *text_sta;

             text_sta = XmTextGetString (w);

             if ( text_sta != NULL )
                 sscanf ( text_sta, "%f", &user_level );
             else
                 user_level = 850;

             XtFree(text_sta);
             }

        void parcelLevelactivateCB(Widget w, XtPointer client_data, XtPointer call_data) {
          current_parcel = 6;
          define_parcel( 6, user_level );
          Toggle_Callback(rbutton[5],5,(XtPointer)NULL);
          XmToggleButtonSetState(rbutton[5],True,True);
          redraw_graph(1);
          show_parcel();
          XCopyArea ( XtDisplay(draw_reg), canvas, XtWindow(draw_reg),
              gc, 0, 0, xwdth, xhght, 0, 0 );
          XFlush ( XtDisplay(draw_reg) );
        }

        void pfsmtime_select_cb(Widget w, XtPointer client_data,
                               XtPointer call_data)
        {
          int i,j,iret;

          XmStringTable str_list;
          char *dattim;
          char timestring[80];
          char *timeoper[]= {"<<","<","||",">",">>"};
          char *mdlouttext,*p;
          XtVaGetValues(pfsmdlfile_timelist,
                        XmNselectedItemCount, &g_nmtimes,
                        XmNselectedItems, &str_list,
                        NULL );
          if (g_nmtimes > 0) {
            for (i=0;i<g_nmtimes;i++) {
              if (XmStringGetLtoR(str_list[i],XmFONTLIST_DEFAULT_TAG,
                &dattim)) {
                strcpy(g_msoundtimes[i],dattim);
                XtFree(dattim);
              }
            }
          }
          /* print mdl times */
          root=DefaultRootWindow(XtDisplay(time_reg));
          if (!g_pixcreated) {
            time_window=XCreatePixmap(XtDisplay(time_reg),root,600,g_twdth,8);
            time_oper_window=XCreatePixmap(XtDisplay(time_oper_reg),root,140,40,8);
            g_pixcreated=-1;
          }
          XSetForeground(XtDisplay(time_reg), gc2, pixels[0] );
          XFillRectangle (XtDisplay(time_reg), time_window, gc2, 0, 0,
                        600, g_twdth );
          XSetForeground(XtDisplay(time_oper_reg), gc3, pixels[0] );
          XFillRectangle (XtDisplay(time_oper_reg), time_oper_window, gc3, 0, 0,
                        140, 40 );
/*          XSetForeground(XtDisplay(time_reg), gc2, pixels[5] );
          strcpy(timestring,"Forecast Times (hours):");
          XDrawString( XtDisplay(time_reg), (time_window), gc2, 1, 10,
            timestring, strlen(timestring));  */
          /* Moved Output of "Forecast Times" to Resize_Callback function */
          XSetForeground(XtDisplay(time_oper_reg), gc3, pixels[5] );
          for (i=0;i<5;i++) {
            strcpy(timestring, timeoper[i]);
            XDrawString( XtDisplay(time_oper_reg), (time_oper_window), gc3, i*20, 24,
              timestring, strlen(timestring));
          }
          for (i=0;i< ntimes;i++) {

            strcpy(timestring,mtimeac_list[i]+4);
            timestring[5]='\0';
            XSetForeground(XtDisplay(time_reg), gc2, pixels[5] );
            for (j=0;j< g_nmtimes;j++) {
              /*if (strcmp(mdlsoundtime,mtime_list[i])==0) */
              if (strcmp(g_msoundtimes[j],mtimeac_list[i])==0) {
                XSetForeground(XtDisplay(time_reg), gc2, pixels[2] );
                break;
              }

            }
            /* OK Navigate Times at 16 times/row... up to 3 rows */
            if (i<48)
              XDrawString( XtDisplay(time_reg),(time_window),gc2,((int)(i*37.5))%600,(i/16)*12+12,
                timestring,strlen(timestring));
          }
          XCopyArea(XtDisplay(time_reg),time_window, XtWindow(time_reg),
            gc2, 0, 0, 600,g_twdth,0,0);
          XCopyArea(XtDisplay(time_oper_reg),time_oper_window, XtWindow(time_oper_reg),
            gc3, 0, 0, 100, 40,0,0);
          Load_PFS_stationlist(g_pfs_mapset);
        }

        /*NP*/
        void mtime_select_cb (Widget w, XtPointer client_data,
                               XtPointer call_data)
       /*************************************************************/
       /* MTIME_SELECT_CB                                           */
       /*************************************************************/
        {
          int i,j,iret;

          XmStringTable str_list;
          char *dattim;
          /* XmListCallbackStruct *cbs = (XmListCallbackStruct *)call_data;*/
          char *choice;
          char timestring[80];
          char *timeoper[]= {"<<","<","||",">",">>"};
          Widget lastWidget;
          char date[9],runhr[5],time[4];
          char *str_type[20];
          char parsestring[80];
          XrmValue value;
          int count;
          parsestring[0]='\0';
          if (XrmGetResource(g_applicationDB,"nsharp*lf_panel.parsestrng",
            "Sharp95*lf_panel.Parsestring",str_type,&value)
            == True)
            strncpy(parsestring,value.addr,(int) value.size);

          XtVaGetValues(mdlfile_timelist,
                        XmNselectedItemCount, &g_nmtimes,
                        XmNselectedItems, &str_list,
                        NULL );
          if (g_nmtimes > 0) {
            for (i=0;i<g_nmtimes;i++) {
              if (XmStringGetLtoR(str_list[i],XmFONTLIST_DEFAULT_TAG,
                &dattim)) {
                if (parsestring[0]=='\0') {
                  if (sscanf(dattim, "%*2s%6s%*[_]%2s%*[f]%2s",
                    date,runhr,time) < 3)
                    sscanf(dattim, "%*2s%6s%2s%*[f]%2s",date,runhr,time);

                }
                else
                    sscanf(dattim,parsestring,date,runhr,time);

                sprintf(mdlsoundfile,"%s/%s\0",g_pathname,dattim);
                sprintf(mdlsoundtime,"%s/%s00F0%s\0",date,runhr,time);
                strcpy(g_msoundtimes[i],mdlsoundtime);
                sprintf(g_msoundfilenm[i],"%s\0",dattim);
                XtFree(dattim);
              }
            }
          }
          /*XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &choice );
          strcpy ( mdlsoundtime, choice );
          XtFree (choice);*/
          g_allModelParamsSW=-1;
          g_raobModelSW=2;
          /* print mdl times */
          root=DefaultRootWindow(XtDisplay(time_reg));
          if (!g_pixcreated) {
            time_window=XCreatePixmap(XtDisplay(time_reg),root,600,g_twdth,8);
            time_oper_window=XCreatePixmap(XtDisplay(time_oper_reg),root,140,40,8);
            g_pixcreated=-1;
          }
          XSetForeground(XtDisplay(time_reg), gc2, pixels[0] );
          XFillRectangle (XtDisplay(time_reg), time_window, gc2, 0, 0,
                        600, g_twdth );
          XSetForeground(XtDisplay(time_oper_reg), gc3, pixels[0] );
          XFillRectangle (XtDisplay(time_oper_reg), time_oper_window, gc3, 0, 0,
                        140, 40 );
          XSetForeground(XtDisplay(time_reg), gc2, pixels[5] );
          strcpy(timestring,"Forecast Times (hours):");
          XDrawString( XtDisplay(time_reg), (time_window), gc2, 1, 10,
            timestring, strlen(timestring));
          XSetForeground(XtDisplay(time_oper_reg), gc3, pixels[5] );
          for (i=0;i<5;i++) {
            strcpy(timestring, timeoper[i]);
            XDrawString( XtDisplay(time_oper_reg), (time_oper_window), gc3, i*20, 24,
              timestring, strlen(timestring));
          }
          for (i=0;i< mtimes;i++) {

            strncpy(timestring,mtimeac_list[i]+13,2);
            timestring[2]='\0';
            XSetForeground(XtDisplay(time_reg), gc2, pixels[5] );
            for (j=0;j< g_nmtimes;j++) {
              /*if (strcmp(mdlsoundtime,mtime_list[i])==0) */
              if (strcmp(g_msoundtimes[j],mtimeac_list[i])==0) {
                XSetForeground(XtDisplay(time_reg), gc2, pixels[2] );
                break;
              }

            }
            XDrawString( XtDisplay(time_reg),(time_window),gc2,i*(8*4), 24,
              timestring,strlen(timestring));
          }
          XCopyArea(XtDisplay(time_reg),time_window, XtWindow(time_reg),
            gc2, 0, 0, 600,g_twdth,0,0);
          XCopyArea(XtDisplay(time_oper_reg),time_oper_window, XtWindow(time_oper_reg),
            gc3, 0, 0, 100, 40,0,0);

        }

        /*NP*/
        void Load_mdl_sounding ()
       /*************************************************************/
       /* LOAD_MDL_SOUNDING                                         */
       /*************************************************************/
          {
          Widget mapwin;
          Window          gwin;
          GC              gemgc;
          Display               *display;

          int             xwdth, xhght, xdpth;
          int newlev = 0;
          float ix1, ix2;
          float lng=-107.1;
          float lat=38.9;
          int i,j,k,i1;
          char timestring[80];
          char buf[80];
          Cursor cursor=(Cursor) None;
          display=XtDisplay(gemlbl_station2);
          gwin=XtWindow(gemlbl_station2);
          cursor=XCreateFontCursor(display,XC_watch);
          XDefineCursor(display,gwin,cursor);
          display=XtDisplay(mdl_dialog);
          gwin=XtWindow(mdl_dialog);
          cursor=XCreateFontCursor(display,XC_watch);
          XDefineCursor(display,gwin,cursor);
          display=XtDisplay(draw_reg);
          gwin=XtWindow(draw_reg);
          cursor=XCreateFontCursor(display,XC_watch);
          XDefineCursor(display,gwin,cursor);
          XFlush(XtDisplay(mdl_dialog));
          if ( mdlsoundfile[0] != '\0'  && mdlsoundtime[0] != '\0' &&
               mdlsoundsta[0] != '\0' )
            {
             if (numlvl > 0) copy_sndg();
               sndg2[0][0]=numlvl;
             strcpy(g_mdlsoundstawrk,mdlsoundsta);
             for (i=0;i<g_nmtimes;i++) {
               g_act_sndg=i;
               if (g_allModelParamsSW) {
                 strcpy(mdlsoundsta,g_mdlsoundstawrk);
               }
               sprintf(mdlsoundfile,"%s/%s\0",g_pathname,g_msoundfilenm[i]);
               /* LJH Bandage Fix -  Additional get_mdl_snd call needed
                to get proper sounding with upgrade to 5.9.4 in DG calls.
                Otherwise, first sounding loaded may be NA or wrong, because
                of incorrect navigation transformation from gtrans, for
                unknown reasons. */
               if (i == 0) {
                 get_mdl_snd (mdlsoundfile, g_msoundtimes[i], mdlsoundsta,
                           sndg, &newlev, &g_lng, &g_lat, strlen(mdlsoundfile),
                           strlen(g_msoundtimes[i]), strlen(mdlsoundsta));
               }
               get_mdl_snd (mdlsoundfile, g_msoundtimes[i], mdlsoundsta,
                           sndg, &newlev, &g_lng, &g_lat, strlen(mdlsoundfile),
                           strlen(g_msoundtimes[i]), strlen(mdlsoundsta));
               g_newlev[i]=newlev;
               g_elev=sndg[0][2];
               for (j=0;j<(short)newlev;j++)
                 for (k=0;k<7;k++)
                   sndgs[j][k][i]=sndg[j][k];
                 ;
               ;
               for (i1=0;i1< mtimes;i1++) {
                 strncpy(timestring,mtimeac_list[i1]+13,2);
                 timestring[2]='\0';
                 XSetForeground(XtDisplay(time_reg), gc2, pixels[5] );
                 for (j=0;j< g_nmtimes;j++) {
                   if (strcmp(g_msoundtimes[j],mtimeac_list[i1])==0) {
                     XSetForeground(XtDisplay(time_reg), gc2, pixels[2] );
                     if (strcmp(g_msoundtimes[j],g_msoundtimes[i])==0)
                       XSetForeground(XtDisplay(time_reg), gc2, pixels[7] );
                     break;
                   }

                 }
                 XDrawString( XtDisplay(time_reg),(time_window),gc2,i1*(8*4), 24,
                   timestring,strlen(timestring));
               }
               XCopyArea(XtDisplay(time_reg),time_window, XtWindow(time_reg),
                 gc2, 0, 0, 600,g_twdth,0,0);

               if(strncmp(mdlsoundsta,"ZZZ",3)==0) {
                 if (g_lng<0 && g_lat>0)
                   sprintf(mdlsoundsta,"%4.1fN %5.1fW",g_lat,-g_lng);
                 else if (g_lng>0 && g_lat>0)
                   sprintf(mdlsoundsta,"%4.1fN %5.1fE",g_lat,g_lng);
                 else if (g_lng<0 && g_lat<0)
                   sprintf(mdlsoundsta,"%4.1fS %5.1fW",-g_lat,-g_lng);
                 else
                   sprintf(mdlsoundsta,"%4.1fS %5.1fE",-g_lat,g_lng);
               }

               numlvl = (short) newlev;

               xtnd_sndg();
               save_origsndg ();
               sprintf ( raobtitle, " %s %s %s ", mdl_selected,
                       g_msoundtimes[i], mdlsoundsta );
               sprintf( raob_type, "%s %5.0f ft","Model Forecast Elev=",
                        g_elev * 3.28 );
               /* mode = 1; */
               if (i== 0) {
                 XtUnmanageChild (mdl_dialog);
               }

               resize_callback ( draw_reg, (XtPointer)NULL,
                               (XtPointer)NULL );
               if ( numlvl > 0 && (! (mode==3 || mode==4)) )
                 {
                 define_parcel( current_parcel, user_level );
                 mean_wind( -1, -1, &ix1, &ix2, &st_dir, &st_spd);
                 show_parcel ();
                 pagenum = 1;
                 show_page( pagenum );
                 }
               strcpy(buf,"LOADING...PLEASE WAIT");
               XDrawString(XtDisplay(time_oper_reg),XtWindow(time_oper_reg),gc3,1,24,buf,strlen(buf));
               XFlush(XtDisplay(mdl_dialog));
               XFlush(XtDisplay(time_oper_reg));
               XFlush(XtDisplay(draw_reg));

              }

            }
            g_act_sndg=0;
            XCopyArea(XtDisplay(time_oper_reg),time_oper_window, XtWindow(time_oper_reg),
              gc3, 0, 0, 140,40,0,0);
            LoadSingleModelSounding(g_act_sndg);
            NuiDefaultCursor(gemlbl_station2);
            NuiDefaultCursor(mdl_dialog);
            NuiDefaultCursor(draw_reg);
          }


        /*NP*/
        void ellipse (int type, short x, short y, short width,
                        short height )
        /*************************************************************/
        /*  ELLIPSE                                                  */
        /*************************************************************/
        {

        if ( type == 0 )
          {
          XDrawArc ( XtDisplay(draw_reg), canvas, gc,
                     x, y, width - x, height - y, 0, 360*64  );
          }
        else if ( type == 1 )
          {
          XFillArc ( XtDisplay(draw_reg), canvas, gc, x,
                     y, width - x, height - y, 0, 360*64 );
          }

        }


       /*NP*/
       void StartLoop( void )
       /*************************************************************/
       /*  STARTLOOP                                                */
       /*************************************************************/
       {
       set_font(2);
       XtAppMainLoop(app);
       }

        /*NP*/
        void print_soundings ()
       /*************************************************************/
       /* PRINT_SOUNDINGS                                           */
       /*************************************************************/
          {
          int newlev = 0, iret;
          float ix1, ix2;
          char gemdevice[72], gemyaxis[72], gemtimes[72], *cptr;

          if ( gemsoundfile[0] != '\0'  && gemsoundtime[0] != '\0' &&
               gemsoundsta[0] != '\0' )
            {
            if ( item_type == 2 )
              {

               mode = 3;
               XtUnmanageChild (gem_dialog);
               sprintf ( gemyaxis, "0/%s/1000/2;0;1", x_hght );
               cptr = gemsoundsta;
               cptr++;
               strcpy ( gemtimes, gemsoundtime );
               strcat ( gemtimes, "-last" );
               strcpy ( gemdevice, "datatop" );
               sncross (gemsoundfile, gemtimes, gemyaxis,
                            cptr,
                            strlen(gemsoundfile), strlen(gemtimes),
                            strlen(gemyaxis), strlen(gemsoundsta)-1);
              }
            }
          }

        void set_gempak_file ( Widget w, XtPointer client_data,
                               XtPointer call_data )
       /*************************************************************/
       /* LOAD_GEMFILE                                              */
       /*************************************************************/
           {
           int type = (int) client_data;

           switch ( type )
             {

             case 0:
                 strcpy ( gemsoundfile, gemsoundraob );
             break;
             case 1:
                 strcpy ( gemsoundfile, gemsoundtamdar );
             break;

             case 2:
                 strcpy ( gemsoundfile, gemsoundprof );
             break;

             case 3:
                 strcpy ( gemsoundfile, gemsoundvad );
             break;

             }

             XtVaSetValues ( gemfile_text, XmNvalue, gemsoundfile,
                             NULL );

             gemfile_times ();

             gemsoundtime[0] = '\0';

             Load_stationlist ( 0 );

           }

        void xsection_hght_cb ( Widget w, XtPointer client_data, XtPointer call_data)
        {
         int item_no = (int) client_data;

           strcpy ( x_hght, levellist[item_no] );

        }

        void gemfile_times ()
       /*************************************************************/
       /* GET_GEMFILE                                               */
       /*************************************************************/
             {
             XmStringTable      str_list = NULL;
             int                i, ncnt, iret;

             if ( gemsoundfile[0] != '\0' )
               {
               get_gem_times ( gemsoundfile, time_list, &ntimes,
                               &iret, strlen(gemsoundfile) );
               gemsoundtime[0] = '\0';
               if ( iret == 0 )
                {
                str_list = (XmStringTable) XtMalloc (ntimes *
                                                sizeof (XmString) );
                for ( i=ntimes-1; i >= 0 ; i-- )
                 {
                 time_list[i][11] = 0;
                 str_list[ntimes-1-i] =
                     XmStringCreateLocalized (time_list[i]);
                 }
                 XtVaGetValues ( gemfile_timelist,
                                 XmNitemCount, &ncnt, NULL );
                 XtVaSetValues ( gemfile_timelist,
                                 XmNitemCount, ntimes,
                                 XmNitems, str_list,
                                 NULL );
                for ( i=0; i < ntimes; i++ )
                 XmStringFree ( (XmString )str_list[i] );
                XtFree ( (char *)str_list );
                }
               }
             else
               {
                 XmListDeselectAllItems (gemfile_timelist);
                 XtVaSetValues ( gemfile_timelist,
                                 XmNitemCount, 0,
                                 XmNitems, str_list,
                                 NULL );
                 gemsoundfile[0] = '\0';
               }
             }



        /*NP*/
        void clean_uvvs(void)
        {
        short i;
        for(i=0;i<numlvl;i++) sndg[i][0]=-999;
        }

        /*NP*/
        void show_acars_info ( Widget w )
       /*************************************************************/
       /* SHOW_ACARS_INFO                                           */
       /*                                                           */
       /* Display ACARS Map and get user selection.                 */
       /*************************************************************/
        {
        acars_selection(w);
         if ( mode == 1 && redisplay == 0 )
           {
           resize_callback ( draw_reg, (XtPointer)NULL,
                                       (XtPointer)NULL );
           }
        }


        /*NP*/
        void getModelForTime (Widget w, XtPointer *call_data, XEvent *event)
        /*******************************************************************/
        /* GETMODELFORTIME                                                 */
        /*******************************************************************/
        {
        int fhrsub,i;
        char timestring[80];
        char buf[80];
        int existsinloaded=0;
        /* LJH added to correct mysterious loading problems */
        XtRemoveEventHandler( time_reg, ButtonReleaseMask, FALSE,
          (XtEventHandler)getModelForTime, (XtPointer)NULL);
        XtRemoveEventHandler( time_oper_reg, ButtonReleaseMask, FALSE,
          (XtEventHandler) getTimeOper, (XtPointer)NULL);

        if (event->xbutton.button == 1  && g_raobModelSW==2) {
          /* Determine which Forecast Hour */
          /* Using Primitive assumption forecast hr subscript=int(xc/32.0) */
          fhrsub=(int)(event->xbutton.x/32.0);
          if (g_mlocationSelected) {
            strcpy(mdlsoundtime,mtimeac_list[fhrsub]);
            strcpy(mdlsoundfile,mtime_list[fhrsub]);
            /* Identify if mdlsoundtime in g_msoundtimes[1..g_nmtimes] */
            for (i=0;i<g_nmtimes;i++)
              if (strcmp(mdlsoundtime,g_msoundtimes[i])==0) {
                existsinloaded=-1;
                g_act_sndg=i;
                break;
              }
            ;
            if (existsinloaded)
              LoadSingleModelSounding(g_act_sndg);
            else {
              /*strcpy(mdlsoundsta,"ZZZ");*/
              strcpy(mdlsoundsta,g_mdlsoundstawrk);
              XSetForeground(XtDisplay(time_reg), gc2, pixels[0] );
              XFillRectangle (XtDisplay(time_reg), time_window, gc2, 0, 0,
                        600, g_twdth );
              XSetForeground(XtDisplay(time_reg), gc2, pixels[5]);
              strcpy(timestring,"Forecast Times (hours):");
              XDrawString( XtDisplay(time_reg), (time_window), gc2, 1, 10,
                timestring, strlen(timestring));
              for (i=0;i<mtimes;i++) {
                strncpy(timestring,mtimeac_list[i]+13,2);
                timestring[2]='\0';
                if (fhrsub == i) {
                  XmListDeselectAllItems(mdlfile_timelist);
                  XmListSelectPos(mdlfile_timelist,i+1,False);
                  XSetForeground(XtDisplay(time_reg), gc2, pixels[2] );
                }
                else
                  XSetForeground(XtDisplay(time_reg), gc2, pixels[5] );
                XDrawString( XtDisplay(time_reg),(time_window),gc2,i*32, 24,
                  timestring,strlen(timestring));
              }
              strcpy(buf,"Requesting...");
              XDrawString ( XtDisplay(time_reg),(time_window),gc2,i*32,24,
                buf,strlen(buf));
              XCopyArea(XtDisplay(time_reg),time_window, XtWindow(time_reg),
                gc2, 0, 0, 600,g_twdth,0,0);
              XFlush(XtDisplay(time_reg));
              XFlush(XtDisplay(time_oper_reg));
              g_nmtimes=1;
              sprintf(g_msoundtimes[0],mdlsoundtime);
              sprintf(g_msoundfilenm[0],mdlsoundfile);
              Load_mdl_sounding();
              XSetForeground(XtDisplay(time_reg), gc2, pixels[0] );
              XFillRectangle (XtDisplay(time_reg), time_window, gc2, i*32, 0,
                        600, g_twdth );
              XCopyArea(XtDisplay(time_reg),time_window, XtWindow(time_reg),
                gc2, 0, 0, 600,g_twdth,0,0);
              XFlush(XtDisplay(time_reg));
            }

          }
        }
        if (event->xbutton.button == 1  && g_raobModelSW==1) {
            /* Determine which Forecast Hour */
            /* Using Primitive assumption forecast hr subscript=int(xc/32.0) */
            fhrsub=(int)(event->xbutton.x/40.0);
            if (fhrsub<ntimes) {
              strcpy(gemsoundtime,time_list[fhrsub]);
            }
            if (g_mlocationSelected) {
              XSetForeground(XtDisplay(time_reg), gc2, pixels[0] );
              XFillRectangle (XtDisplay(time_reg), time_window, gc2, 0, 0,
                        600, 120 );
              XSetForeground(XtDisplay(time_reg), gc2, pixels[5]);
              strcpy(timestring,"Observed Times:");
              XDrawString( XtDisplay(time_reg), (time_window), gc2, 1, 10,
              timestring, strlen(timestring));


              for (i=0;i<ntimes;i++) {
                strcpy(timestring,time_list[i]+4);
                timestring[5]='\0';
                if (fhrsub == i)
                  XSetForeground(XtDisplay(time_reg), gc2, pixels[2] );
                else
                  XSetForeground(XtDisplay(time_reg), gc2, pixels[5] );
                XDrawString( XtDisplay(time_reg),(time_window),gc2,i*40, 24,
                  timestring,strlen(timestring));
              }
              strcpy(buf,"Requesting...");
              XDrawString ( XtDisplay(time_reg),(time_window),gc2,i*40,24,
                buf,strlen(buf));
              XCopyArea(XtDisplay(time_reg),time_window, XtWindow(time_reg),
                gc2, 0, 0, 600,g_twdth,0,0);
              XFlush(XtDisplay(time_reg));
              Load_gem_sounding();
              XSetForeground(XtDisplay(time_reg), gc2, pixels[0] );
              XFillRectangle (XtDisplay(time_reg), time_window, gc2, i*40, 0,
                        600, g_twdth );
              XCopyArea(XtDisplay(time_reg),time_window, XtWindow(time_reg),
                gc2, 0, 0, 600,g_twdth,0,0);
              XFlush(XtDisplay(time_reg));
            }

          }
          if (event->xbutton.button == 1 && g_pfs_model_sel_sw) {
            /* More complex computation derived from 3-rows of time*/
            /* x/37.5 +16*(y/12) */
            fhrsub = (int)((int) (event->xbutton.x/37.5)+16*(event->xbutton.y/12));
            if (fhrsub<ntimes) {
              strcpy(mdlsoundtime,mtimeac_list[fhrsub]);
              strcpy(gemsoundsta,"@");
              strcat(gemsoundsta,sta_id);
              sprintf(gemsoundfile,"%s/%s",g_pathname,mdlsoundfile);
            }
              /* Identify if mdlsoundtime in g_msoundtimes[1..g_nmtimes] */
            for (i=0;i<g_nmtimes;i++)
              if (strcmp(mdlsoundtime,g_msoundtimes[i])==0) {
                existsinloaded=-1;
                g_act_sndg=i;
                break;
              }
            ;
            if (existsinloaded)
              LoadSingleModelSounding(g_act_sndg);
            else {
              /* Redraw Everything */
              XSetForeground(XtDisplay(time_reg), gc2, pixels[0] );
              XFillRectangle (XtDisplay(time_reg), time_window, gc2, 0, 0,
                        600, g_twdth);

              /* XSetForeground(XtDisplay(time_reg), gc2, pixels[5]);
              strcpy(timestring,"Forecast Times (hours):");
              XDrawString( XtDisplay(time_reg), (time_window), gc2, 1, 10,
                timestring, strlen(timestring)); */
              /* Call Resize Callback to render keyword "Forecast Times:" on draw_reg */
              /* For PFS only.  Out of Space Otherwise.*/
              /* resize_callback(draw_reg,(XtPointer)NULL,(XtPointer)NULL);*/
              for (i=0;i<mtimes;i++) {
                strcpy(timestring,mtimeac_list[i]+4);
                timestring[5]='\0';
                if (fhrsub == i) {
                  XmListDeselectAllItems(pfsmdlfile_timelist);
                  XmListSelectPos(pfsmdlfile_timelist,i+1,False);
                  XSetForeground(XtDisplay(time_reg), gc2, pixels[2] );
                }
                else
                  XSetForeground(XtDisplay(time_reg), gc2, pixels[5] );
                if (i<48)
                  XDrawString( XtDisplay(time_reg),(time_window),gc2,(int)(i*37.5)%600,(i/16)*12+12,
                    timestring,strlen(timestring));
              }
              strcpy(buf,"Requesting...");
              XDrawString ( XtDisplay(time_reg),(time_window),gc2,i*40,24,
                buf,strlen(buf));
              XCopyArea(XtDisplay(time_reg),time_window, XtWindow(time_reg),
                gc2, 0, 0, 600,g_twdth,0,0);
              XFlush(XtDisplay(time_reg));
              XFlush(XtDisplay(time_oper_reg));
              g_nmtimes=1;
              sprintf(g_msoundtimes[0],mdlsoundtime);
              sprintf(g_msoundfilenm[0],mdlsoundfile);
              strcpy ( gemsoundsta, "@" );
              strcat ( gemsoundsta, sta_id );
              sprintf( gemsoundfile,"%s/%s",g_pathname,mdlsoundfile);
              strcpy( gemsoundtime,g_msoundtimes[0]);
              Load_pfs_mdl_sounding();
              XSetForeground(XtDisplay(time_reg), gc2, pixels[0] );
              XFillRectangle (XtDisplay(time_reg), time_window, gc2, i*40, 0,
                        600, g_twdth );
              XCopyArea(XtDisplay(time_reg),time_window, XtWindow(time_reg),
                gc2, 0, 0, 600,g_twdth,0,0);
              XFlush(XtDisplay(time_reg));
            }
          }
          XtAddEventHandler(time_oper_reg, ButtonReleaseMask, FALSE,
              (XtEventHandler) getTimeOper, (XtPointer)NULL);
          XtAddEventHandler(time_reg, ButtonReleaseMask, FALSE,
              (XtEventHandler) getModelForTime, (XtPointer)NULL);

       }

       /*NP*/
       static void getTimeOper (Widget w, XtPointer *call_data, XEvent *event)
       /*********************************************************************/
       /* GETTIMEOPER                                                       */
       /*********************************************************************/
       {
        int oprsub,i;
        char timestring[80];
        char buf[80];
        char *timeoper[]= {"<<","<","||",">",">>"};
        /* LJH added to correct mysterious load advance problems */
        XtRemoveEventHandler( time_oper_reg, ButtonReleaseMask, FALSE,
          (XtEventHandler) getTimeOper, (XtPointer)NULL);
        XtRemoveEventHandler( time_reg, ButtonReleaseMask, FALSE,
          (XtEventHandler)getModelForTime, (XtPointer)NULL);
        if (event->xbutton.button == 1  && (g_raobModelSW==2 || g_pfs_model_sel_sw)) {
          oprsub=(int)(event->xbutton.x/20.0);
          switch (oprsub) {
            case 0:
              if (! g_looponsw) {
                g_looponsw=-1;
                g_timer=XtAppAddTimeOut(XtWidgetToApplicationContext(w), 250, LoopBackward, w);
              }
              break;
            case 1:
              g_act_sndg--;
              if (g_act_sndg<0)
                g_act_sndg=g_nmtimes-1;
              LoadSingleModelSounding(g_act_sndg);
              break;
            case 2:
              if (g_looping) {
                g_looping=0;
                g_looponsw=0;
                XtRemoveTimeOut(g_timer);
              }
              if (numlvl>0 && (! (mode==3 || mode==4))) {
                show_parcel();
                show_page(pagenum);
              }
              break;
            case 3:
              g_act_sndg++;
              if (g_act_sndg>g_nmtimes-1)
                g_act_sndg=0;
              LoadSingleModelSounding(g_act_sndg);
              break;
            case 4:
              if (! g_looponsw) {
                g_looponsw=-1;
                g_timer=XtAppAddTimeOut(XtWidgetToApplicationContext(w), 250, LoopForward, w);
              }
              break;
          }
          for (i=0;i<5;i++) {
            strcpy(timestring, timeoper[i]);
            if (i==oprsub)
              XSetForeground(XtDisplay(time_oper_reg), gc3, pixels[2] );
            else
              XSetForeground(XtDisplay(time_oper_reg), gc3, pixels[5] );

            XDrawString( XtDisplay(time_oper_reg), (time_oper_window), gc3, i*20, 24,
              timestring, strlen(timestring));
          }
          XCopyArea(XtDisplay(time_oper_reg),time_oper_window, XtWindow(time_oper_reg),
            gc3, 0, 0, 140, 40,0,0);
          XtAddEventHandler(time_reg, ButtonReleaseMask, FALSE,
              (XtEventHandler) getModelForTime, (XtPointer)NULL);
          XtAddEventHandler(time_oper_reg, ButtonReleaseMask, FALSE,
              (XtEventHandler) getTimeOper, (XtPointer)NULL);


        }
       }

       /*NP*/
       static void LoopForward (XtPointer w, XtIntervalId* timer)
       /**********************************************************************/
       /* LOOPFORWARD                                                        */
       /**********************************************************************/
       {
          g_act_sndg++;
          if (g_act_sndg>g_nmtimes-1)
            g_act_sndg=0;
          LoadSingleModelSounding(g_act_sndg);
          if (g_act_sndg==g_nmtimes-1)
            timer = (XtIntervalId*) XtAppAddTimeOut(XtWidgetToApplicationContext(w),750,(XtTimerCallbackProc) LoopForward,w);
          else
            timer = (XtIntervalId*) XtAppAddTimeOut(XtWidgetToApplicationContext(w),250,(XtTimerCallbackProc) LoopForward,w);
          g_looping=-1;

        }

        static void LoopBackward (XtPointer w, XtIntervalId* timer)
        /**********************************************************************/
        /* LOOPBACKWARD                                                       */
        /**********************************************************************/
        {
          g_act_sndg--;
          if (g_act_sndg<0)
             g_act_sndg=g_nmtimes-1;
          LoadSingleModelSounding(g_act_sndg);
          if (g_act_sndg == 0)
            timer = (XtIntervalId*) XtAppAddTimeOut(XtWidgetToApplicationContext(w), 750,(XtTimerCallbackProc) LoopBackward,w);
          else
            timer = (XtIntervalId*) XtAppAddTimeOut(XtWidgetToApplicationContext(w), 250,(XtTimerCallbackProc) LoopBackward,w);
          g_looping=-1;
        }

       /*NP*/
       void LoadSingleModelSounding(int g_act_sndg)
       /**********************************************************************/
       /* LOADSINGLEMODELSOUNDING                                            */
       /**********************************************************************/
         {
         int newlev = 0;
         float ix1, ix2;
         float lng=-107.1;
         float lat=38.9;
         int i,j,k,i1;
         char timestring[80];
         if ( (mdlsoundfile[0] != '\0'  && mdlsoundtime[0] != '\0' &&
               mdlsoundsta[0] != '\0') || g_pfs_model_sel_sw )
            {
            /*copy sndgs[g_act_sndg] to sndg */
            for (j=0;j<g_newlev[g_act_sndg];j++)
              for (k=0;k<7;k++)
                sndg[j][k]=sndgs[j][k][g_act_sndg];
              ;
            ;


            for (i1=0;i1< mtimes;i1++) {
               if (g_pfs_model_sel_sw) {
                 strcpy(timestring,mtimeac_list[i1]+4);
                 timestring[5]='\0';
               }
               else {
                 strncpy(timestring,mtimeac_list[i1]+13,2);
                 timestring[2]='\0';
               }
               XSetForeground(XtDisplay(time_reg), gc2, pixels[5] );
               for (j=0;j< g_nmtimes;j++) {
                   if (strcmp(g_msoundtimes[j],mtimeac_list[i1])==0) {
                     XSetForeground(XtDisplay(time_reg), gc2, pixels[2] );
                     if (strcmp(g_msoundtimes[j],g_msoundtimes[g_act_sndg])==0)
                       XSetForeground(XtDisplay(time_reg), gc2, pixels[7] );
                     break;
                   }

               }
               if (g_pfs_model_sel_sw) {
                  if (i1<48)
                    XDrawString( XtDisplay(time_reg),(time_window),gc2,(int)(i1*37.5)%600, (i1/16)*12+12,
                      timestring,strlen(timestring));
               }
               else
                 XDrawString( XtDisplay(time_reg),(time_window),gc2,i1*(8*4), 24,
                   timestring,strlen(timestring));
            }
            XCopyArea(XtDisplay(time_reg),time_window, XtWindow(time_reg),
                 gc2, 0, 0, 600,g_twdth,0,0);

            if(strncmp(mdlsoundsta,"ZZZ",3)==0 || g_pfs_model_sel_sw) {
                 if (g_lng<0 && g_lat>0)
                   sprintf(mdlsoundsta,"%4.1fN %5.1fW",g_lat,-g_lng);
                 else if (g_lng>0 && g_lat>0)
                   sprintf(mdlsoundsta,"%4.1fN %5.1fE",g_lat,g_lng);
                 else if (g_lng<0 && g_lat<0)
                   sprintf(mdlsoundsta,"%4.1fS %5.1fW",-g_lat,-g_lng);
                 else
                   sprintf(mdlsoundsta,"%4.1fS %5.1fE",-g_lat,g_lng);
            }

            numlvl = (short) g_newlev[g_act_sndg];

            xtnd_sndg();
            save_origsndg ();
            sprintf ( raobtitle, " %s %s %s ", mdl_selected,
                       g_msoundtimes[g_act_sndg], mdlsoundsta );
            if (g_pfs_model_sel_sw)
              sprintf( raob_type, "Model Point Forecast for %s Elev=%5.0f ft", sta_id, g_elev * 3.28);
            else
              sprintf( raob_type, "%s %5.0f ft", "Model Forecast Elev=", g_elev * 3.28);

               /* mode = 1; */
            resize_callback ( draw_reg, (XtPointer)NULL,
                               (XtPointer)NULL );
            if ( (numlvl > 0 && (! (mode==3 || mode==4))) || g_pfs_model_sel_sw )
                 {
                 define_parcel( current_parcel, user_level );
                 mean_wind( -1, -1, &ix1, &ix2, &st_dir, &st_spd);
                 show_parcel ();
                 pagenum = 1;
                 show_page( pagenum );
                 }

         }
       }

       /*NP*/
       void expose_time_reg_overlays (Widget w, XmDrawingAreaCallbackStruct *call_data)
       /***********************************************************************/
       /* EXPOSE_TIME_REG_OVERLAYS                                            */
       /***********************************************************************/
       {
         if (g_pixcreated) {
           XCopyArea(XtDisplay(time_reg), time_window, XtWindow(time_reg),
                 gc2, 0, 0, 600, g_twdth, 0, 0);
           XFlush(XtDisplay(time_reg));
         }
       }

       /*NP*/
       void resize_time_reg_overlays (Widget w, XtPointer *data,
                              XmDrawingAreaCallbackStruct *call_data)
       /***********************************************************************/
       /* RESIZE_TIME_REG_OVERLAYS                                            */
       /***********************************************************************/
       {
         if (g_pixcreated){
           XCopyArea(XtDisplay(time_reg), time_window, XtWindow(time_reg),
                 gc2, 0, 0, 600, g_twdth, 0, 0);
           XFlush(XtDisplay(time_reg));
         }
       }

       /*NP*/
       void expose_time_oper_reg_overlays (Widget w, XmDrawingAreaCallbackStruct *call_data)
       /***********************************************************************/
       /* EXPOSE_TIME_OPER_REG_OVERLAYS                                       */
       /***********************************************************************/
       {
         if (g_pixcreated) {
           XCopyArea(XtDisplay(time_oper_reg), time_oper_window, XtWindow(time_oper_reg),
                 gc3, 0, 0, 100, 40, 0, 0);
           XFlush(XtDisplay(time_oper_reg));
         }
       }

       /*NP*/
       void resize_time_oper_reg_overlays (Widget w, XtPointer *data,
                              XmDrawingAreaCallbackStruct *call_data)
       /***********************************************************************/
       /* RESIZE_TIME_OPER_REG_OVERLAYS                                       */
       /***********************************************************************/
       {
         if (g_pixcreated){
           XCopyArea(XtDisplay(time_oper_reg), time_oper_window, XtWindow(time_oper_reg),
                 gc3, 0, 0, 100, 40, 0, 0);
           XFlush(XtDisplay(time_oper_reg));
         }
       }

       /*NP*/
       void Make_Gif (Widget w, XtPointer *data,
                             XmDrawingAreaCallbackStruct *call_data)
       {
         char buf[160];
         char fname[160],ftype[5];
         Widget giffsb;
         Arg args[8];
         Cardinal n=0;
         XmString mask;
         XmString mask2;
         mask=XmStringCreateSimple("Enter A Gif Name...Suggested default:  ");
         XtSetArg(args[n],XmNselectionLabelString,mask);n++;
         strcpy(ftype,".gif");
         cvrttitletofilename(raobtitle,fname,ftype);
         if (strlen(fname)==0)
             mask2=XmStringCreateSimple("zztmp.gif");
         else
             mask2=XmStringCreateSimple(fname);
         XtSetArg(args[n],XmNtextString,mask2);n++;
         giffsb=XmCreatePromptDialog(w,"giffsb",args,n);
         XtAddCallback(giffsb,XmNokCallback, (XtCallbackProc) GifFileOKCallback,
           (XtPointer) NULL);
         XtUnmanageChild(XmSelectionBoxGetChild(giffsb,XmDIALOG_HELP_BUTTON));
         XtManageChild(giffsb);
       }

       void GifFileOKCallback(Widget fsb,XtPointer client_data,XtPointer call_data)
       {
         XmSelectionBoxCallbackStruct* ptr;
         char*string;
         char rec[100];
         char buf[80];
         ptr=(XmSelectionBoxCallbackStruct*) call_data;
         XmStringGetLtoR(ptr->value,XmSTRING_DEFAULT_CHARSET,&string);
         /* printf("Selected file named [%s]\n",string);
         printf ("In Make Gif \n");
         printf ("Window id is %lX \n",XtWindow(draw_reg)); */
         sprintf(buf,"xwd -id %lu|xwdtopnm|ppmtogif > %s \n",XtWindow(draw_reg),string);
         system(buf);
         XtFree(string);
         XtUnmanageChild(fsb);
       }

       void cvrttitletofilename(char *raobtitle,char *fname,char *ftype) {
         int i;
         int count=0;
         char raobwork[80];
         char *p;
         fname[0]='\0';
         strcpy(raobwork,raobtitle);
         p=strstr(raobwork,"@");
         if (p!=NULL) {
           for (i=0;i<6;i++) p[i]=' ';
         }
         for (i=0;i<strlen(raobwork);i++)
           if ((raobwork[i]>='0' && raobwork[i]<='9') ||
             (toupper(raobwork[i])>='A' && toupper(raobwork[i])<='Z')
             || raobwork[i]=='.') {
             if (raobwork[i]=='.')
               fname[count]='_';
             else
               fname[count]=raobwork[i];
             count++;
           }
         ;
         fname[count]='\0';
         strcat(fname,ftype);
       }


       void idle(double duration) {
          static struct timeval TimeNow;
          static struct timezone TZ;
          double startTime_sec,startTime_usec,nowTime_sec,nowTime_usec,elapsedTime;

          gettimeofday(&TimeNow,&TZ);
          startTime_sec=TimeNow.tv_sec;
          startTime_usec=TimeNow.tv_usec;
          elapsedTime=0.0;
          while (elapsedTime<duration) {
            gettimeofday(&TimeNow,&TZ);
            nowTime_sec=TimeNow.tv_sec;
            nowTime_usec=TimeNow.tv_usec;
            elapsedTime=(nowTime_sec+nowTime_usec/1000000.0) -
              (startTime_sec+startTime_usec/1000000.0);
          };
       }



