#include "gui.h"
#include "sharp95.h"

/* 
 * Private functions
 */
void acars_timerange	( Widget, XtPointer, XtPointer );
void acars_unzoom_cb	( Widget, XtPointer, XtPointer );
void acars_zoom_cb	( Widget, XtPointer, XtPointer );
void ac_map_init	( void );
void ac_pointer		( Widget, XtPointer, XEvent *event );
void amapproj_cb	( Widget, XtPointer, XtPointer );
void get_acarsfile	( Widget, XtPointer, XtPointer );
void load_acarsfile	( Widget, XtPointer, XtPointer );
void Load_acars_sounding( int button, float lat, float lon );
void mapw_pickacarsCb	( Widget, XtPointer, XEvent *, Boolean * );


Widget acars_dialog=NULL;
extern Widget toplevel;

mapstruct ac_map;

Widget acarsfile_text,acarsfile_timelist,acars_canvas,cursor_text,search_radius;

static char time_list [500][20];
static char tselect [500][20]; 
static int ntimes, nselect;

static char	map_winname[]="acars_map";

/*=====================================================================*/

void acars_selection ( void )
{

XmString 	str, gem_title;

static Widget   gemform, gemform2, gemform3, gempane,
		gemfile_label, gemfile_load, gemfile_cancel,
		gemfile_help, ac_zoom, ac_unzoom,
		gemlbl_time, menubar, button, acars_txtframe;

XmStringTable   str_list = NULL;
char            *gemouttext, gemdevice[72];

int             i, iret, mapindx, ier=0;
Arg		args[10];
Cardinal	argcnt;
int		nmap, ac_reload=1;

static _NXMmenuItem  mapmenu[30];

if ( ! acars_dialog )
    {
    gem_title = XmStringCreateLocalized( "ACARS Sounding Selection" );
    acars_dialog = XmCreateBulletinBoardDialog(toplevel, "acars_panel",
                                        NULL, 0);
    XtVaSetValues( acars_dialog, XmNdialogTitle, gem_title, NULL);
    XmStringFree(gem_title);

    gempane = XtVaCreateManagedWidget("parcel_pane",
                                xmPanedWindowWidgetClass,
                                acars_dialog,
                                XmNsashWidth, 1,
                                XmNsashHeight, 1,
                                NULL);

    gemform2 = XtVaCreateWidget("form", xmFormWidgetClass,
                        gempane, NULL );

    gemform = XtVaCreateWidget("form", xmFormWidgetClass,
                                gempane, XmNfractionBase, 7, NULL );

    gemform3 = XtVaCreateWidget("form", xmFormWidgetClass,
                                gempane, NULL );

    str = XmStringCreateLocalized ("GEMPAK ACARS file:");
    gemfile_label = XtVaCreateManagedWidget ("gemfile_label",
                                xmLabelWidgetClass, gemform,
                                XmNlabelString, str,
                                XmNleftAttachment, XmATTACH_FORM,
                                XmNtopAttachment, XmATTACH_FORM,
                                NULL );
    XmStringFree (str);

    acarsfile_text = XtVaCreateManagedWidget ("acarstext",
                                xmTextFieldWidgetClass, gemform,
                                XmNleftAttachment, XmATTACH_WIDGET,
                                XmNleftWidget, gemfile_label,
                                XmNtopAttachment, XmATTACH_FORM,
                                XmNrightAttachment, XmATTACH_FORM,
                                NULL );


    str = XmStringCreateLocalized ("ACARS times:");
    gemlbl_time = XtVaCreateManagedWidget ("gemfile_time",
                                xmLabelWidgetClass, gemform,
                                XmNlabelString, str,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 0,
                                XmNtopAttachment, XmATTACH_POSITION,
                                XmNtopPosition, 1,
                                XmNalignment, XmALIGNMENT_BEGINNING,
                                NULL );
    XmStringFree (str);

    acarsfile_timelist = XmCreateScrolledList ( gemform,
                                "times", NULL, 0 );

    XtVaSetValues ( acarsfile_timelist,
			XmNselectionPolicy, XmEXTENDED_SELECT,
                        XmNvisibleItemCount, 20, NULL );
    XtVaSetValues (XtParent(acarsfile_timelist),
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, gemlbl_time,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 2,
                        NULL );

    XtAddCallback ( acarsfile_timelist, XmNextendedSelectionCallback,
			acars_timerange, NULL );

    XtManageChild ( acarsfile_timelist );

    acars_canvas = XtVaCreateManagedWidget("map_acars",
                        xmDrawingAreaWidgetClass, gemform,
                        XmNtopAttachment, XmATTACH_POSITION,
                        XmNtopPosition, 1,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, acarsfile_timelist,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNwidth, 560,
                        XmNbackground, pixels[0],
                        NULL);

    XtAddCallback( acars_canvas, XmNexposeCallback,
                        (XtCallbackProc)mapw_exposeCb, NULL );
                        plotData.mode = STNSELECT;

    /* ----- "button pressed moved" event ----- */
    XtAddEventHandler( acars_canvas, ButtonPressMask,
                        FALSE, (XtEventHandler)mapw_pickacarsCb, NULL );

    /* ----- "mouse moved" event ----- */
    XtAddEventHandler( acars_canvas, PointerMotionMask, FALSE,
                     (XtEventHandler)ac_pointer,
                     (XtPointer)NULL); 

    XtManageChild ( gemform );


    /*
     * create the menubar
     */
    argcnt = 0;
    XtSetArg(args[argcnt],XmNtopAttachment,   XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt],XmNleftAttachment,  XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt],XmNrightAttachment, XmATTACH_FORM); argcnt++;
 
    menubar = XmCreateMenuBar(gemform2, "menubar", args, argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNborderWidth, 1); argcnt++;

    gemfile_load = XmCreateCascadeButton (menubar, "File", NULL, 0 );
    XtVaSetValues(gemfile_load, XmNmnemonic, 'F', NULL);
    XtAddCallback(gemfile_load, XmNactivateCallback,
                        (XtCallbackProc)load_acarsfile, NULL);
    XtSetValues(gemfile_load, args, argcnt);
    XtManageChild(gemfile_load);

    /*
     * Create Map pulldown menu
     *
     * Set the items for the menu bar for the map areas.
     */
    nmap = nwxTable->nmap;
    for ( i = 0; i < nmap; i++ ) {
                   mapmenu[i].label = (char *)
                        malloc(strlen(nwxTable->map_info[i].name)+1);
                   strcpy ( mapmenu[i].label, nwxTable->map_info[i].name );
                   mapmenu[i].class        = &xmCascadeButtonGadgetClass;
                   mapmenu[i].mnemonic     = 0;
                   mapmenu[i].accelerator  = NULL;
                   mapmenu[i].accel_text   = NULL;
                   mapmenu[i].callback     = amapproj_cb;
                   mapmenu[i].which_widget = (long)i;
                   mapmenu[i].subitems     = NULL;
                   mapmenu[i].sub_buttons  = NULL;
    }
    mapmenu[nmap].label        = NULL;
    mapmenu[nmap].class        = &xmCascadeButtonGadgetClass;
    mapmenu[nmap].mnemonic     = 0;
    mapmenu[nmap].accelerator  = NULL;
    mapmenu[nmap].accel_text   = NULL;
    mapmenu[nmap].callback     = NULL;
    mapmenu[nmap].which_widget = (long)nmap;
    mapmenu[nmap].subitems     = NULL;
    mapmenu[nmap].sub_buttons  = NULL;

    NxmMenuPulldownBuild(menubar, NULL, "Area", 'A', mapmenu);

    if( (button = XtNameToWidget(menubar, "Area") ) )
                   XtSetValues(button, args, argcnt);

    ac_zoom = XmCreateCascadeButton (menubar, "Zoom", NULL, 0 );
    XtVaSetValues(ac_zoom, XmNmnemonic, 'Z', NULL);
    XtAddCallback(ac_zoom, XmNactivateCallback,
                        (XtCallbackProc)acars_zoom_cb, acars_canvas);
    XtSetValues(ac_zoom, args, argcnt);
    XtManageChild(ac_zoom);

    ac_unzoom = XmCreateCascadeButton (menubar, "UnZoom", NULL, 0 );
    XtVaSetValues(ac_unzoom, XmNmnemonic, 'U', NULL);
    XtAddCallback(ac_unzoom, XmNactivateCallback,
                        (XtCallbackProc)acars_unzoom_cb, NULL);
    XtSetValues(ac_unzoom, args, argcnt);
    XtManageChild(ac_unzoom);


    gemfile_cancel = XmCreateCascadeButton (menubar, "Cancel", NULL, 0 );
    XtVaSetValues(gemfile_cancel, XmNmnemonic, 'C', NULL);
    XtAddCallback(gemfile_cancel, XmNactivateCallback,
                        (XtCallbackProc)popdown_cb, acars_dialog);
    XtSetValues(gemfile_cancel, args, argcnt);
    XtManageChild(gemfile_cancel);

    gemfile_help = XmCreateCascadeButton (menubar, "Help", NULL, 0 );
    XtVaSetValues(gemfile_help, XmNmnemonic, 'H', NULL);
    XtAddCallback(gemfile_help, XmNactivateCallback,
                        (XtCallbackProc)NxmHelp_helpBtnCb, (XtPointer)5);
    XtSetValues(gemfile_help, args, argcnt);
    XtManageChild(gemfile_help);

    XtVaSetValues(menubar, XmNmenuHelpWidget, gemfile_help, NULL);

    XtManageChild ( menubar );
    XtManageChild ( gemform2 );


    /*
     * create lat/lon cursor info widget
     */
    acars_txtframe = XtVaCreateManagedWidget("acars_latlonFrame",
                        xmFrameWidgetClass,  gemform3,
                        XmNrightAttachment,  XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopAttachment,    XmATTACH_FORM,
                        XmNshadowType,       XmSHADOW_IN,
                        NULL);

    str = XmStringCreateLocalized ("------;-------");
    cursor_text = XtVaCreateManagedWidget ("cursor_text",
                                xmLabelWidgetClass, acars_txtframe,
				XmNlabelString, str,
                                XmNrecomputeSize,    False,
                                XmNwidth, 85,
                                NULL );
    XmStringFree (str);

    search_radius = XtVaCreateManagedWidget ("search_radius",
                                xmTextFieldWidgetClass, gemform3,
				XmNrightAttachment, XmATTACH_WIDGET,
				XmNrightWidget, acars_txtframe,
				XmNrightOffset, 25,
                                XmNtopAttachment, XmATTACH_FORM,
                                XmNbottomAttachment, XmATTACH_FORM,
                                XmNwidth, 85,
                                NULL );

    XtManageChild ( gemform3 );

    XtManageChild ( gempane );
    XtManageChild ( acars_dialog );


    ac_map.mapindx = 0;
    ac_map.zoomflg = 0;
    i = nsharp_mapw_rgstr ( acars_canvas, map_winname );

    XtAddCallback( acars_canvas, XmNresizeCallback,
                              (XtCallbackProc)mapw_resizeCb, NULL );
    XtVaGetValues ( acarsfile_text, XmNvalue, &gemouttext, NULL );

    if ( strlen(gemouttext) == (size_t)0 )
        gemsoundfile[0] = '\0';
    else
        strcpy ( gemsoundfile, gemouttext );

    if ( gemsoundfile[0] != '\0' )
        {
	get_acars_times ( gemsoundfile, time_list, &ntimes,
                          &iret, (int)strlen(gemsoundfile), 20 );
        if ( iret == 0 )
            {
            gemsoundtime[0] = '\0';
            str_list = (XmStringTable) XtMalloc ((size_t)ntimes * sizeof (XmString *) );
            for ( i=ntimes-1; i >=  0; i-- )
                {
                time_list[i][11] = 0;
                str_list[ntimes-i-1] = XmStringCreateLocalized (time_list[i]);
                }
            XtVaSetValues ( acarsfile_timelist,
                                 XmNitemCount, ntimes,
                                 XmNitems, str_list,
                                 NULL );
            for ( i=0; i < ntimes; i++ )
                XmStringFree ( (XmString )str_list[i] );
            XtFree ( (XtPointer)str_list );
            }
        else
            {
            gemsoundfile[0] = '\0';
            XmListDeselectAllItems (acarsfile_timelist);
            XtVaSetValues ( acarsfile_timelist, XmNitemCount, 0,
                                 XmNitems, str_list, NULL );
            }
        }
    else
        {
        XmListDeselectAllItems (acarsfile_timelist);
        XtVaSetValues ( acarsfile_timelist, XmNitemCount, 0,
                                 XmNitems, str_list, NULL );
        }
    XtFree ( gemouttext );

    }
else
    {
    gemouttext = (char *) XmTextFieldGetString (acarsfile_text);
    strcpy( gemsoundfile, gemouttext);
    XtFree ( gemouttext );

    memset(gemdevice,0,sizeof(gemdevice));
    gqdev(gemdevice,&mapindx,&ier,&iret,sizeof(gemdevice)-1);
    gemdevice[sizeof(gemdevice)-1] = '\0';

    /* don't reload data if we have a good window */
    if ( strncmp(gemdevice, "XW", 2) == 0 )
	ac_reload=0;
    }

XtManageChild ( acars_dialog );
strcpy ( gemdevice, map_winname );
gslwin ( gemdevice, &iret, strlen(gemdevice));

if( ac_reload )
    ac_map_init();

}

/*=====================================================================*/
/* ARGSUSED */
void acars_unzoom_cb ( Widget wdgt, XtPointer clnt, XtPointer call )
{
    ac_map.zoomflg = 0;
    ac_map_init();
}

/*=====================================================================*/
/* ARGSUSED */
void acars_zoom_cb ( Widget wdgt, XtPointer clnt, XtPointer call )
/*************************************************************/
/* Zoom acars selection map 				     */
/*************************************************************/
{
Widget mapCanvW = (Widget)clnt;

XtRemoveEventHandler( mapCanvW, PointerMotionMask, FALSE,
                        (XtEventHandler)ac_pointer, NULL );

_mapzoom_cb ( mapCanvW, mapw_pickacarsCb, &ac_map, ac_map_init );

XtAddEventHandler( mapCanvW, PointerMotionMask, FALSE,
                        (XtEventHandler)ac_pointer, (XtPointer)NULL );
}

/*=====================================================================*/
/* ARGSUSED */
void load_acarsfile ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * LOAD_ACARSFILE                                              		*
 **									*
 * T. Piper/SAIC	2/02	Freed xmstr & xmmask			*
 * S. Chiswell/Unidata	8/04	Replaced with file_browse_popup call	*
 ***********************************************************************/
{
char *acarsdir;
static char fmask[]="*.gem";

if(gemdata_env != NULL)
   {
   acarsdir = (char *)malloc(strlen(gemdata_env)+7);
   sprintf(acarsdir,"%s/acars",gemdata_env);
   }
else
   {
   acarsdir = (char *)malloc(3);
   sprintf(acarsdir,"./");
   }
file_browse_popup ( acarsdir, fmask, toplevel, get_acarsfile);
free ( acarsdir);
}

/*=====================================================================*/
/* ARGSUSED */
void get_acarsfile ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * GET_ACARSFILE                                               		*
 **									*
 * T. Piper/SAIC	2/02	Freed file				*
 ***********************************************************************/
{
char *file;
XmFileSelectionBoxCallbackStruct *cbs =
              (XmFileSelectionBoxCallbackStruct *)call;
XmStringTable      str_list;
int                i, iret;

if (cbs)
    {
    file = XmStringUnparse (cbs->value, NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
    XtVaSetValues ( acarsfile_text, XmNvalue, file, NULL );
    strcpy ( gemsoundfile, file );
    XtFree(file);
    get_acars_times ( gemsoundfile, time_list, &ntimes,
                               &iret, (int)strlen(gemsoundfile), 20);
    nselect = 0;

    ac_map_init();

    gemsoundtime[0] = '\0';

    if ( iret == 0 )
        {
        str_list = (XmStringTable) XtMalloc ((size_t)ntimes *
                                                sizeof (XmString *) );
        for ( i=ntimes-1; i >= 0 ; i-- )
            {
            time_list[i][11] = 0;
            str_list[ntimes-1-i] =
                     XmStringCreateLocalized (time_list[i]);
            }
        XtVaSetValues ( acarsfile_timelist,
                                 XmNitemCount, ntimes,
                                 XmNitems, str_list,
		                 XmNselectedItemCount, ntimes,
		                 XmNselectedItems, str_list,
                                 NULL );

        for ( i=0; i < ntimes; i++ )
            XmStringFree ( (XmString )str_list[i] );

        XtFree ( (XtPointer)str_list );
        }
    }
}

/*=====================================================================*/
/* ARGSUSED */
void ac_map_init ( void )
/*************************************************************/
/*                                                           */
/* R. Tian/SAIC  02/03   Added Cursor Points mark            */
/* D.W.Plummer/NCEP	 3/03	modify ctb_rdcpf calling seq */
/*************************************************************/
{
int iret;
int ncolor, mrktyp, mrkwid, pltval, iposn, jcolr;
float sizmrk;
int ncp;
float cpf_lat[LLSTFL], cpf_lon[LLSTFL];
/*---------------------------------------------------------------------*/

nsharp_draw_map (map_winname, &ac_map, &iret);

if(gemsoundfile[0] != '\0')
    get_acars_points (gemsoundfile, &nselect, tselect, &iret,
                      (int)strlen(gemsoundfile), 20 );

    ctb_rdcpf ( "nmap2.cpf", &ncp, cpf_lat, cpf_lon, &iret );
    if ( iret == 0 && ncp > 0 ) {
        ncolor    = 1;
        mrktyp    = 5;
        sizmrk    = 2.0F;
        mrkwid    = 2;
        pltval    = G_FALSE;
        iposn     = 0;
        jcolr     = 5;

        map_mark ( &ncp, cpf_lat, cpf_lon, NULL, &ncolor, NULL,
                   &jcolr, &mrktyp, &sizmrk, &mrkwid, &pltval,
                   &iposn, &iret );
    }
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_pickacarsCb ( Widget wdgt, XtPointer clnt, XEvent *event,
			Boolean *continue_to_dispatch )
/************************************************************************
 * mapw_pickstnCb                                                       *
 *                                                                      *
 * This function will select a station/stations based on the position   *
 * of left mouse button.                                                *
 *                                                                      *
 * mapw_pickstnCb ( wdgt, clnt, event )                             *
 *                                                                      *
 * Input parameters:                                                    *
 *      wdgt               Widget          The input widget                *
 *      clnt	     XtPointer       The input data for the widget   *
 *      event           XEvent          The registered event            *
 **                                                                     *
 ***********************************************************************/
{
int		iret, np;
float           xloc, yloc;
float           lat, lon;

static char sys1[]="D";
static char sys2[]="M";

/*---------------------------------------------------------------------*/
/*
 * Check if it is ploting the values
 */
if ( plotData.mode != STNSELECT )
    return;

/*
 * Check if it is a left mouse button.
 */

if  (( event->xbutton.button == 1 )||
     ( event->xbutton.button == 2 )||
     ( event->xbutton.button == 3 )) {

/*
 * set the search flag indicating it is new data
 */
    srchInfo.sflag = 0;

/*
 * Get the x and y values for the button press.
 */
    xloc = (float) event->xbutton.x;
    yloc = (float) event->xbutton.y;

    np = 1;
    gtrans( sys1, sys2, &np, &xloc, &yloc,
             &lat, &lon, &iret, strlen(sys1), strlen(sys2) );

    if(gemsoundfile[0] != '\0')
         Load_acars_sounding((int)event->xbutton.button, lat, lon);
               

    } /* end of checking xbutton=1 */
}

/*=====================================================================*/
extern int current_parcel;
extern float user_level;
void Load_acars_sounding ( int button, float lat, float lon )
{
int i, bmode;
float ix1, ix2, plat, plon, acsearchrad;
char *searchrad;
char cstn[12];
char auxinfo[512];
int npxms, curpxm;
char winname[]="skewt_canvas";
int ier, len=strlen(winname), ixsize, iysize, isxsiz, isysiz, ixo, iyo, ncurwn;
/* sndg[LLMXLV][7] defined in sharp95.h */


plat = lat; plon = lon; /* copy these values for passing to fortran */
bmode = button;
/*---------------------------------------------------------------------*/
memset(cstn,0,sizeof(cstn));
memset(auxinfo,0,sizeof(auxinfo));
if((bmode == 2)||(bmode == 3))
   {
   get_nearest (gemsoundfile, &nselect, tselect, &plat, &plon, cstn, auxinfo,
      (int)strlen(gemsoundfile), 20, sizeof(cstn), sizeof(auxinfo));
   cstn[11] = '\0'; auxinfo[511] = '\0';
   for(i=10;i>=0;i--) if(cstn[i] == ' ') cstn[i] = '\0';
   i = 510;
   while(i >=0)
      {
      if(auxinfo[i] == ' ')
         auxinfo[i] = '\0';
      else
         i = 0;
      i -= 1;
      }
   
   if(cstn[0] == '\0') 
      {
      printf("try again %s %d\n", cstn, (int)strlen(cstn));
      return;
      }
   if(bmode == 2)
      {
      show_status(cstn,auxinfo);
      return;
      }
   }

searchrad = (char *) XmTextFieldGetString (search_radius);
if ( strlen(searchrad) == (size_t)0 )
   acsearchrad = 100.0F;
else
   if(sscanf ( searchrad, "%f", &acsearchrad ) < 1) acsearchrad = 100.0F;

if(acsearchrad < 1.0F) 
   {
   printf("search radius [%f] too small, using 100 km\n",acsearchrad);
   acsearchrad = 100.0F;
   }

XtFree(searchrad);

xslwin ( winname, &len, &ixsize, &iysize, &isxsiz, &isysiz, &ixo, &iyo, &ncurwn, &ier );
xstanm (&ier);

xsplot(&ier);
xqcpxm ( &npxms, &curpxm);
canvas = xqpxms ( 0, curpxm);
NxmChangePixmapData( curpxm, npxms);

if ( sndgs[curpxm] == NULL )
   {
   sndgs[curpxm] = (struct sndg_struct *)malloc(sizeof(struct sndg_struct));
   sndgs[curpxm]->numlev = 0;
   }

sndgp = sndgs[curpxm];

/* copy old sounding for overlays */
if ( sndgp->numlev > 0 )
   copy_sndg();
else
   sndgp->ovrlev = 0;

get_acars_snd (gemsoundfile, &nselect, tselect, &bmode, &acsearchrad, &plat,
		&plon, cstn, (float **)sndgs[curpxm]->sndg, &sndgp->numlev, 
		(int)strlen(gemsoundfile), 20, (int)strlen(cstn));

XtUnmanageChild (acars_dialog);


if(bmode == 3)
   {
   if(nselect == 0)
      sprintf(raobtitle,"%s %s-%s",cstn,time_list[0],time_list[ntimes-1]);
   else
      sprintf(raobtitle,"%s %s-%s",cstn,tselect[nselect-1],tselect[0]);
   }
else
   {
if(nselect == 0)
   sprintf(raobtitle,"Lat %6.2f Lon %7.2f %s-%s ",lat,lon,
      time_list[0],time_list[ntimes-1]);
else
   sprintf(raobtitle,"Lat %6.2f Lon %7.2f %s-%s ",lat,lon,
      tselect[nselect-1],tselect[0]);
   }
strcpy(sndgs[curpxm]->title, raobtitle);

strcpy( raob_type, "ACARS");

xtnd_sndg();
save_origsndg ();

if ( sndgp->numlev > 2 && sndgp->sndg[0].pres > 100.0F )
   {
   pagenum = 1;
   define_parcel( (short)current_parcel, user_level );
   mean_wind( sndgp->sndg[sfc()].pres, (float)i_pres(msl(6000.0F)), &ix1, &ix2, &sndgp->st_dir, &sndgp->st_spd);

   if((qc(sndgp->st_spd)) && (qc(sndgp->st_dir)))
      {sndgp->st_spd *= .75F;
      sndgp->st_dir += 30.0F;
      if(sndgp->st_dir>360.0F) sndgp->st_dir -= 360.0F;
      }
   else
      printf("Warning, can not determine 0-6000m (%4.0f - %3.0f mb) storm relative wind\n",
	sndgp->sndg[sfc()].pres, (float)i_pres(msl(6000.0F)));

   show_page( pagenum );
   }

mode = 1;
clean_uvvs( sndgp );
draw_skewt ();
show_parcel ();
xenanm (&ier);
i = curpxm + 1;
while ( ( i < MAX_PIXMAP ) && ( sndgs[i] != NULL ) )
    {
    sndgs[i]->numlev = 0;
    i++;
    }
NxmLoopbuttonSensitive ( False );

}

/*=====================================================================*/
/* ARGSUSED */
void acars_timerange ( Widget wdgt, XtPointer clnt, XtPointer call )
/*************************************************************/
/* ACARS_TIMERANGE                                            */
/*************************************************************/
{
int ntimes_r, ii;
XmStringTable  str_list;
char *dattim;
/*---------------------------------------------------------------------*/
XtVaGetValues ( acarsfile_timelist, XmNselectedItemCount, &ntimes_r,
	        XmNselectedItems, &str_list, NULL );


if(ntimes_r > 0)
    {
    nselect = 0;
    for(ii = 0; ii < ntimes_r; ii++)
        {
	dattim = XmStringUnparse (str_list[ii], NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
        strcpy(tselect[ii],dattim);
        nselect++;
        XtFree (dattim);
        }

    ac_map_init();

    }
}

/*=====================================================================*/
/* ARGSUSED */
void ac_pointer ( Widget wdgt, XtPointer clnt, XEvent *event )
/*************************************************************/
/* AC_POINTER                                                */
/*                                                           */
/* Handles "mouse moved w/ no buttons" event                 */
/*************************************************************/
{
char cursor_pos[81];
float xdev,ydev,lat,lon;
int iret,np=1;
XmString str;
/*---------------------------------------------------------------------*/
xdev = (float)event->xbutton.x; ydev = (float)event->xbutton.y;
gtrans(sys_D, sys_M, &np, &xdev, &ydev, &lat, &lon,
                       &iret, strlen(sys_D), strlen(sys_M));
sprintf(cursor_pos,"%6.2f,%7.2f",lat,lon);
str = XmStringCreateLocalized (cursor_pos);
XtVaSetValues ( cursor_text, XmNlabelString, str, NULL );
XmStringFree(str);

XFlush (XtDisplay(acars_canvas));
}

/*=====================================================================*/
/* ARGSUSED */
void amapproj_cb ( Widget wdgt, XtPointer clnt, XtPointer call )
/*************************************************************/
/* AMAPPROJ_CB                                               */
/*                                                           */
/*************************************************************/
{
int item_no = (long)clnt;
/*---------------------------------------------------------------------*/
ac_map.mapindx = item_no;
ac_map.zoomflg = 0;

ac_map_init();
}
