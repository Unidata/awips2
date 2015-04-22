/* T. Piper/SAIC	10/02	Removed #include <gemprm.h> */
/* L. Hinson/AWC        11/02   Removed '&' from map_info in call to draw_map */
#include <xwcmn.h>
#include <sharp95.h>
#include <gui.h>

void       mapw_pickacarsCb();
void       mapw_exposeCb();
void       mapw_resizeCb();
void	   acars_info_cancel_cb();
void	   load_acarsfile();
void	   get_acarsfile();
void	   ac_map_init();
int	   mapw_rgstr_acars();
void	   Load_acars_sounding();
void	   acars_timerange();
void	   acars_zoom_cb();
void	   acars_unzoom_cb();
void	   ac_pointer();
void	   aboutacars_cb();

#ifdef UNDERSCORE
#define gslwin	gslwin_
#define gtrans	gtrans_
#define gclear	gclear_
#define ggtpnt  ggtpnt_
#define gqdev	gqdev_
#define get_acars_times  get_acars_times_
#define get_acars_points  get_acars_points_
#define get_acars_snd	get_acars_snd_
#define	get_nearest	get_nearest_
#define	map_init	map_init_
#define gg_motf		gg_motf_
#endif
#ifdef LINUX
#define get_acars_times_ get_acars_times__
#define get_acars_points_ get_acars_points__
#define get_acars_snd_	get_acars_snd__
#define	get_nearest_	get_nearest__
#define	map_init_	map_init__
#define gg_motf		gg_motf__
#endif

Widget acars_dialog=NULL;
extern Widget toplevel;
extern mapbnd_t	mapb;
extern struct maptype_list     map_info[2];

Widget acarsfile_text,acarsfile_timelist,acars_canvas,cursor_text;

char gemsoundfile[200], gemsoundtime[20];
static char time_list [500][20];
static char tselect [500][20]; 
static int ntimes, nselect;

void acars_selection (Widget w)
{

XmString str,gem_title;

static Widget   gemform, gemform2, gempane,
                        gemfile_label,
                        gemfile_load, gemfile_ok, gemfile_cancel,
                        gemfile_help, ac_zoom, ac_unzoom,
                        gemlbl_time,
                        gemstn_opt;

        XmStringTable   str_list = NULL;
        char            *gemouttext, gemdevice[72], *file_dir, file[200];

        int             ityp, i, num, iret, mapindx, ier=0;
        Window          gwin;
        GC              gemgc;

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

           gemform = XtVaCreateWidget("form", xmFormWidgetClass,
                                gempane, XmNfractionBase, 7,
                                NULL );

           str = XmStringCreateSimple ("GEMPAK ACARS file:");
           gemfile_label = XtVaCreateManagedWidget ("gemfile_label",
                                xmLabelWidgetClass, gemform,
                                XmNlabelString, str,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 1,
                                XmNtopAttachment, XmATTACH_FORM,
                                XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 4,
                                NULL );
           XmStringFree (str);

           acarsfile_text = XtVaCreateManagedWidget ("acarstext",
                                xmTextFieldWidgetClass, gemform,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 1,
                                XmNtopAttachment, XmATTACH_WIDGET,
                                XmNtopWidget, gemfile_label,
                                XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 5,
                                NULL );

            XtAddCallback ( acarsfile_text, XmNactivateCallback,
                              get_gemfile_text, NULL );
	    file_dir = getenv ( "OBS" );
            if(file_dir != NULL) {
               sprintf(file,"%s/acars/latest.acars",file_dir);
               XtVaSetValues ( acarsfile_text, XmNvalue, file, NULL );
            }

            gemfile_load = XtVaCreateManagedWidget ("Change File",
                                xmPushButtonWidgetClass, gemform,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 5,
                                XmNtopAttachment, XmATTACH_WIDGET,
                                XmNtopWidget, gemfile_label,
                                XmNrightAttachment, XmATTACH_POSITION,
                                XmNrightPosition, 7,
                                NULL );

            XtAddCallback ( gemfile_load, XmNactivateCallback,
                              load_acarsfile, NULL );

            str = XmStringCreateSimple ("ACARS times:");
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
                        XmNvisibleItemCount, 15, NULL );
            XtVaSetValues (XtParent(acarsfile_timelist),
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, gemlbl_time,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 2,
                        NULL );

             XtAddCallback ( acarsfile_timelist,
                        XmNextendedSelectionCallback,
                        /*time_select_cb,*/
			acars_timerange,
                        NULL );

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
                        mapw_exposeCb, NULL );
              plotData.mode = STNSELECT;

              /* redo station pick,sta_select and load_gem_sounding */
              XtAddEventHandler( acars_canvas, ButtonPressMask,
                        FALSE, mapw_pickacarsCb, NULL );

              /* ----- "mouse moved" event ----- */
              XtAddEventHandler( acars_canvas, PointerMotionMask, FALSE,
                     (XtEventHandler)ac_pointer,
                     (XtPointer)NULL); 

              XtManageChild ( gemform );

              gemform2 = XtVaCreateWidget("form", xmFormWidgetClass,
                        gempane, XmNfractionBase, 7,
                        NULL );

              cursor_text = XtVaCreateManagedWidget ("cursor_text",
                                xmTextFieldWidgetClass, gemform2,
                                XmNleftAttachment, XmATTACH_POSITION,
                                XmNleftPosition, 0,
                                XmNtopAttachment, XmATTACH_FORM,
                                XmNbottomAttachment, XmATTACH_FORM,
                                /*XmNrightAttachment, XmATTACH_POSITION,*/
                                /*XmNrightPosition, 2,*/
				XmNwidth, 150,
                                NULL );

              gemfile_cancel = XtVaCreateManagedWidget ("CANCEL",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 2,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 3,
                        NULL );
               XtAddCallback(gemfile_cancel, XmNactivateCallback,
                        (XtCallbackProc)acars_info_cancel_cb, NULL);

               ac_zoom = XtVaCreateManagedWidget ("ZOOM",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 4,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 5,
                        NULL );
	       XtAddCallback(ac_zoom, XmNactivateCallback,
                        (XtCallbackProc)acars_zoom_cb, NULL);
               ac_unzoom = XtVaCreateManagedWidget ("UNZOOM",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 6,
                        NULL );
               XtAddCallback(ac_unzoom, XmNactivateCallback,
                        (XtCallbackProc)acars_unzoom_cb, NULL);

               gemfile_help = XtVaCreateManagedWidget ("HELP",
                        xmPushButtonWidgetClass, gemform2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 6,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 7,
                        NULL );
               XtAddCallback(gemfile_help, XmNactivateCallback,
                        (XtCallbackProc)aboutacars_cb, NULL);

               XtManageChild ( gemform2 );
               XtManageChild ( gempane );
               XtManageChild ( acars_dialog );


                i = mapw_rgstr_acars(  acars_canvas );

               XtAddCallback( acars_canvas, XmNresizeCallback,
                              mapw_resizeCb, NULL );
               XtVaGetValues ( acarsfile_text, XmNvalue, &gemouttext,
                             NULL );
               if ( strlen(gemouttext) == 0 )
                   gemsoundfile[0] = '\0';
               else
                   strcpy ( gemsoundfile, gemouttext );

               if ( gemsoundfile[0] != '\0' )
                 {
                 get_acars_times ( gemsoundfile, time_list, &ntimes,
                                 &iret, strlen(gemsoundfile) );
                if ( iret == 0 )
                  {
                  gemsoundtime[0] = '\0';
                  str_list = (XmStringTable) XtMalloc (ntimes *
                                                sizeof (XmString *) );
                  for ( i=ntimes-1; i >=  0; i-- )
                   {
                   time_list[i][11] = 0;
                   str_list[ntimes-i-1] =
                     XmStringCreateLocalized (time_list[i]);
                   }
                  XtVaSetValues ( acarsfile_timelist,
                                 XmNitemCount, ntimes,
                                 XmNitems, str_list,
                                 NULL );

                  for ( i=0; i < ntimes; i++ )
                   XmStringFree ( (XmString )str_list[i] );
                  XtFree ( (char *)str_list );
                  }
                 else
                  {
                  gemsoundfile[0] = '\0';
                  XmListDeselectAllItems (acarsfile_timelist);
                  XtVaSetValues ( acarsfile_timelist,
                                 XmNitemCount, 0,
                                 XmNitems, str_list,
                                 NULL );
                  }
                 }
               else
                 {
                 XmListDeselectAllItems (acarsfile_timelist);
                 XtVaSetValues ( acarsfile_timelist,
                                 XmNitemCount, 0,
                                 XmNitems, str_list,
                                 NULL );
                }
               XtFree ( gemouttext );


               }
             else
                {
                gemouttext = (char *) XmTextFieldGetString (acarsfile_text);
                strcpy( gemsoundfile, gemouttext);
                XtFree ( gemouttext );
		/*get_acars_times ( gemsoundfile, time_list, &ntimes,
                                 &iret, strlen(gemsoundfile) );*/

                memset(gemdevice,0,sizeof(gemdevice));
                gqdev(gemdevice,&mapindx,&ier,&iret,sizeof(gemdevice)-1);
                gemdevice[sizeof(gemdevice)-1] = '\0';
                if(strncmp(gemdevice,"XW",2) != 0)
                   {
                   memset(gemdevice,0,sizeof(gemdevice));
                   strcpy( gemdevice, "maptop1" );
                   gg_motf ( gemdevice, &iret, strlen(gemdevice) );
                   gslwin(gemdevice, &iret, strlen(gemdevice));
                   mapindx = 0;
                   draw_map(mapindx, map_info, 1, &mapb, &iret);
                   if(gemsoundfile[0] != '\0')
                      get_acars_points (gemsoundfile, &nselect, tselect,&iret,
                         strlen(gemsoundfile),sizeof(tselect) );
                   }
                }
             XtManageChild ( acars_dialog );
             strcpy( gemdevice, "maptop1" );
             gslwin(gemdevice, &iret, strlen(gemdevice));

}


       void acars_info_cancel_cb (Widget w, XtPointer client_data,
                               XtPointer call_data)
       /*************************************************************/
       /* GEM_INFO_CANCEL_CB                                        */
       /*************************************************************/
          {
          XtUnmanageChild (acars_dialog);
          }

        void acars_unzoom_cb (Widget w, XtPointer client_data,
                               XtPointer call_data)
        {
        int iret,mapindx=0;
        gclear(&iret);
        draw_map(mapindx, map_info, 0, &mapb, &iret);
        if(gemsoundfile[0] != '\0')
           get_acars_points (gemsoundfile, &nselect, tselect,&iret,
             strlen(gemsoundfile),sizeof(tselect) );
        }

	void acars_zoom_cb (Widget w, XtPointer client_data,
                               XtPointer call_data)
	/*************************************************************/
        /* Zoom acars selection map 				    */
	/*************************************************************/
          {
	  static int zoom_state=0;
          int i, ityp, np, iret, mapindx=0;
          char sysin[2], sysout[2];
          float xpts[2], ypts[2], xdev[2], ydev[2];

	   void mapw_pickacarsCb();

          if(zoom_state == 0)
             {
             zoom_state = 1;
             /*
              * Change mouse event handling for zooming
              */
             XtRemoveEventHandler( acars_canvas, ButtonPressMask, FALSE,
                        mapw_pickacarsCb, NULL);
             XtRemoveEventHandler( acars_canvas, PointerMotionMask, FALSE,
                     ac_pointer, NULL);
             /*
              * change to the zoom cursor
              */
             NxmCursorChange( acars_canvas, XC_crosshair, "white");
             XmUpdateDisplay( w );
             ityp = 3;
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
                   if(gemsoundfile[0] != '\0')
                      get_acars_points (gemsoundfile, &nselect, tselect,&iret,
                          strlen(gemsoundfile),sizeof(tselect) );
                   }
                }
             /*
              * Reset the zoom button
              */
             acars_zoom_cb( w, NULL, NULL);
             } 
          else
             {
             zoom_state = 0;
             /*
              * restore default cursor
              */
             NuiDefaultCursor( acars_canvas );
             /*
              * Restore mouse to be selection status
              */
             XSelectInput( gemdisplay, XtWindow(acars_canvas),
                     ButtonPressMask | ButtonReleaseMask |
                     ExposureMask );

             XtAddEventHandler( acars_canvas, ButtonPressMask, FALSE,
                     mapw_pickacarsCb, NULL );
             XtAddEventHandler( acars_canvas, PointerMotionMask, FALSE,
                     (XtEventHandler)ac_pointer,
                     (XtPointer)NULL);
             }
          }

        void load_acarsfile ( Widget w, XtPointer client_data,
                         XtPointer call_data )
       /*************************************************************/
       /* LOAD_GEMFILE                                              */
       /*************************************************************/
           {
           static Widget load_filegem;
           char *file_dir;
           XmString xmstr,xmmask;
           char *acarsdir;

             if ( ! load_filegem )
               {
	       file_dir = getenv ( "OBS" );
               if(file_dir != NULL)
                  {
                  acarsdir = (char *)malloc(strlen(file_dir)+7);
                  sprintf(acarsdir,"%s/acars\0",file_dir);
                  load_filegem = XmCreateFileSelectionDialog ( toplevel,
                            "acarsndg_sel", NULL, 0 );
                  xmstr = XmStringCreateLocalized(acarsdir);
                  xmmask =  XmStringCreateLocalized("*.acars\0");
                  XtVaSetValues(load_filegem,XmNdirectory,xmstr,
                     XmNdirMask, xmmask, NULL);
                  free(acarsdir);
                  } 
               else
                  load_filegem = XmCreateFileSelectionDialog ( toplevel,
                            "acarsndg_sel", NULL, 0 );
               XtAddCallback ( load_filegem, XmNokCallback, get_acarsfile,
                               NULL );
               XtAddCallback ( load_filegem, XmNcancelCallback,
                               (XtCallbackProc)XtUnmanageChild, NULL );
               XtAddCallback ( load_filegem, XmNokCallback,
                               (XtCallbackProc)XtUnmanageChild, NULL );
               }
             XtManageChild(load_filegem);
           }

        void get_acarsfile ( Widget w, XtPointer client_data,
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
               XtVaSetValues ( acarsfile_text, XmNvalue, file, NULL );

	       ac_map_init();

               strcpy ( gemsoundfile, file );
               get_acars_times ( gemsoundfile, time_list, &ntimes,
                               &iret, strlen(gemsoundfile),sizeof(time_list));
               nselect = 0;
               get_acars_points (gemsoundfile, &nselect, time_list,&iret, 
                          strlen(gemsoundfile),sizeof(time_list) );
               gemsoundtime[0] = '\0';
               if ( iret == 0 )
                {
                str_list = (XmStringTable) XtMalloc (ntimes *
                                                sizeof (XmString *) );
                for ( i=ntimes-1; i >= 0 ; i-- )
                 {
                 time_list[i][11] = 0;
                 str_list[ntimes-1-i] =
                     XmStringCreateLocalized (time_list[i]);
                 }
                 /*XtVaGetValues ( acarsfile_timelist,
                                 XmNitemCount, &ncnt, NULL );*/
                 XtVaSetValues ( acarsfile_timelist,
                                 XmNitemCount, ntimes,
                                 XmNitems, str_list,
		                 XmNselectedItemCount, ntimes,
		                 XmNselectedItems, str_list,
                                 NULL );

                for ( i=0; i < ntimes; i++ )
                 XmStringFree ( (XmString )str_list[i] );

                XtFree ( (char *)str_list );
                }
               }
             }


void	ac_map_init()
{
	int iret;
	int mapindx=0;

            strcpy ( map_info[0].name, "US" );
            strcpy ( map_info[0].proj, "STR/90;-105;0/nm" );
            strcpy ( map_info[0].garea, "22.88;-120.49;46.02;-60.83" );
            mapb.x[0] = 22.88;mapb.x[1]=46.02;
            mapb.y[0] = -120.49;mapb.y[1]=-60.83;

         draw_map(mapindx, map_info, 0, &mapb, &iret);
}

int	mapw_rgstr_acars(mapwin )
Widget mapwin;

{
XColor          cred;
Dimension       wdth, hght;
/*struct maptype_list     map_info[2];*/
/*mapbnd_t                mapb;*/


Window          gwin;
GC              gemgc;
Cursor          curs;

int             xwdth, xhght, xdpth;

int             i, iret, mapindx, ityp, ier;

char            gemdevice[72];

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
        strcpy(gemdevice,"maptop1");
        xmotifw(gwin, gemdevice, gemgc, xwdth, xhght, xdpth, &iret);

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

void mapw_pickacarsCb( w, client_data, event )
Widget          w;
XtPointer       client_data;
XEvent          *event;
/************************************************************************
 * mapw_pickstnCb                                                       *
 *                                                                      *
 * This function will select a station/stations based on the position   *
 * of left mouse button.                                                *
 *                                                                      *
 * mapw_pickstnCb ( w, client_data, event )                             *
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      client_data     XtPointer       The input data for the widget   *
 *      event           XEvent          The registered event            *
 **                                                                     *
 * T. Piper/SAIC	10/02		Removed unused variable 	*
 *					- stnindex[MAX_PLOT_STN]	*
 ***********************************************************************/

{
int             ifound, iret, np;
int             i, j, k, jcolr, imrk;
float           xloc, yloc;
char            nodata_msg[80];
float           lat, lon;
char sys1[2];
char sys2[2];

/*---------------------------------------------------------------------*/

        strcpy (sys1, "D");
        strcpy (sys2, "M");

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
               Load_acars_sounding(event->xbutton.button,lat,lon);
               
            /*sta_select_cb(ifound);*/

        } /* end of checking xbutton=1 */

}

extern int current_parcel;
extern short pagenum;
extern float user_level;
void	Load_acars_sounding(button,lat,lon)
int button;
float lat,lon;
{
int numlev;
int i,bmode;
float ix1, ix2,plat,plon;
char cstn[12];
char auxinfo[512];

/* sndg[200][7] defined in sharp95.h */
float sndgwrk[200][7];  /*auxiliary array to smooth data*/
int j,k,startsub,endsub,count,bcount;
float basepres,centpres,centtemp,tempaccum,presaccum,avgpres,avgtemp,dp;

/* copy old sounding for overlays */
if (numlvl > 0) copy_sndg();
    sndg2[0][0] = numlvl;

plat = lat; plon = lon; /* copy these values for passing to fortran */
bmode = button;

memset(cstn,0,sizeof(cstn));
memset(auxinfo,0,sizeof(auxinfo));
if((bmode == 2)||(bmode == 3))
   {
   get_nearest (gemsoundfile,&nselect,tselect,&plat,&plon, cstn,auxinfo,
      strlen(gemsoundfile),sizeof(tselect),sizeof(cstn),sizeof(auxinfo));
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
      printf("try again %s %d\n",cstn,strlen(cstn));
      return;
      }
   if(bmode == 2)
      {
      show_status(cstn,auxinfo);
      return;
      }
   }

get_acars_snd (gemsoundfile,&nselect,tselect,&bmode,&plat,&plon,
               cstn,sndg,&numlev,
               strlen(gemsoundfile),sizeof(tselect),strlen(cstn));
numlvl = (short)numlev;
/*  Perform 8mb running means on T */
basepres=sndg[0][1];
for (i=0;i<(numlev-1);i++) {
  centpres=sndg[i][1];
  startsub=0; endsub=0;
  if ((basepres-centpres)>=8) {
    for (j=i-1;j>=0;j--) {
      if ((sndg[j][1]-centpres)<=8)
        startsub=j;
      else {
        if (startsub==0) {
          startsub=i;
        }
        break;
      };
    }
    for (j=i+1;j<(numlvl-1);j++) {
      if ((centpres-sndg[j][1])<=8)
        endsub=j;
      else {
        if (endsub==0) {
          endsub=i;
        }
        break;
      };
    }
  }
  if ((endsub-startsub)>=2) {
    count=0;
    tempaccum=0.0;
    for (j=startsub;j<=endsub;j++) {
      count++;
      tempaccum+=sndg[j][3];
    }
    sndgwrk[i][3]=tempaccum/count;
  } 
  else
    sndgwrk[i][3]=sndg[i][3];
}
sndgwrk[numlvl-1][3]=sndg[numlvl-1][3];
for (i=0;i<numlvl;i++)
  sndg[i][3]=sndgwrk[i][3];

XtUnmanageChild (acars_dialog);

if(bmode == 3)
   {
   if(nselect == 0)
      sprintf(raobtitle,"%s %s-%s\0",cstn,time_list[0],time_list[ntimes-1]);
   else
      sprintf(raobtitle,"%s %s-%s\0",cstn,tselect[nselect-1],tselect[0]);
   }
else
   {
if(nselect == 0)
   sprintf(raobtitle,"Lat %6.2f Lon %7.2f %s-%s \0",lat,lon,
      time_list[0],time_list[ntimes-1]);
else
   sprintf(raobtitle,"Lat %6.2f Lon %7.2f %s-%s \0",lat,lon,
      tselect[nselect-1],tselect[0]);
   }

strcpy( raob_type, "ACARS");
/*write_file();*/

xtnd_sndg();
save_origsndg ();

if ( numlvl > 2 && sndg[0][1] > 100. )
   {
   pagenum = 1;
   define_parcel( current_parcel, user_level );
   mean_wind( sndg[sfc()][1], i_pres(msl(6000)), &ix1, &ix2, &st_dir, &st_spd);

   if((qc(st_spd)) && (qc(st_dir)))
      {st_spd *= .75;
      st_dir += 30;
      if(st_dir>360) st_dir -= 360;
      }
   else
      printf("Warning, can not determine 0-6000m (%4.0f - %3.0f mb) storm relative wind\n",sndg[sfc()][1],i_pres(msl(6000)));

   show_page( pagenum );
   }

mode = 1;
clean_uvvs();
draw_skewt ();
show_parcel ();
}

void	acars_timerange(Widget w, XtPointer client_data,
                               XtPointer call_data)
       /*************************************************************/
       /* ACARS_TIMERANGE                                            */
       /*************************************************************/
          {
          int ntimes_r,i,iret;
	  XmStringTable  str_list;
	  char *dattim;
	  int mapindx;

          XtVaGetValues ( acarsfile_timelist,
		          XmNselectedItemCount, &ntimes_r,
	                  XmNselectedItems, &str_list,
                          NULL );


          if(ntimes_r > 0)
             {
             nselect = 0;
             for(i=0;i<ntimes_r;i++)
                {
                if(XmStringGetLtoR(str_list[i],XmFONTLIST_DEFAULT_TAG,
                   &dattim))
                   {
                   strcpy(tselect[i],dattim);
                   nselect++;
                   XtFree (dattim);
                   }
                }
	     gclear(&iret);
             mapindx = 0;
	     draw_map(mapindx, map_info, 1, &mapb, &iret);
             get_acars_points (gemsoundfile, &nselect, tselect,&iret,
                          strlen(gemsoundfile),sizeof(tselect) );
             }



          }

void	ac_pointer(Widget w, XtPointer *call_data, XEvent *event)
        /*************************************************************/
        /* POINTER_UPDATE                                            */
        /*                                                           */
        /* Handles "mouse moved w/ no buttons" event                 */
        /*************************************************************/
        {
	char sysin[3],sysout[3],cursor_pos[81];
	float xdev,ydev,lat,lon;
        int iret,np=1;

        xdev = event->xbutton.x; ydev = event->xbutton.y;
        strcpy ( sysin, "D" );
        strcpy ( sysout, "M" );
        gtrans(sysin, sysout, &np, &xdev, &ydev, &lat, &lon,
                       &iret, strlen(sysin), strlen(sysout));
        cursor_pos[0] = '\0';
        sprintf(cursor_pos,"%6.2f,%7.2f\0",lat,lon);
        XtVaSetValues ( cursor_text, XmNvalue, cursor_pos, NULL );

        /* ----- Update Cursor Data when mouse is moved ----- 
        if (mode == 1)
           {
           if ((event->xbutton.x < skv.brx) && (event->xbutton.y < skv.bry))
              skewt_cursor_data((short)event->xbutton.x,
                                (short)event->xbutton.y);
           }
        else
           {
           if ((event->xbutton.x < hov.brx) && (event->xbutton.y < hov.bry))
              hodo_cursor_data((short)event->xbutton.x,
                               (short)event->xbutton.y);
           }
        */
        XFlush (XtDisplay(acars_canvas));
        }

void aboutacars_cb (Widget w, XtPointer client_data, XtPointer call_data)
       /*************************************************************/
       /* ABOUT_CB                                                  */
       /*                                                           */
       /*************************************************************/
        {
        int             i;
        static Widget   aboutacarsBox, temp;
        char            msg2[700];
        XmString        msg, title;

        if (!aboutacarsBox)
           {
           aboutacarsBox = XmCreateMessageDialog( toplevel, "aboutacarsBox", NULL, 0);
           title = XmStringCreateLocalized( "About ACARS Display" );
           /*strcpy ( msg2, message[0] );
           for ( i=1; i < XtNumber(message); i++ )
               if(message[i] != NULL) strcat ( msg2, message[i] );

           msg   = XmStringCreateLtoR( msg2, XmFONTLIST_DEFAULT_TAG );*/
           XtVaSetValues( aboutacarsBox,
                        /*XmNmessageString, msg,*/
                        XmNdialogTitle, title,
                        NULL);
           XmStringFree( title );
           /*XmStringFree( msg );*/

           /* ----- Turn off CANCEL and HELP buttons ----- */
           temp = XmMessageBoxGetChild( aboutacarsBox, XmDIALOG_CANCEL_BUTTON);
           XtUnmanageChild(temp);
           temp = XmMessageBoxGetChild( aboutacarsBox, XmDIALOG_HELP_BUTTON);
           XtUnmanageChild(temp);
           }

        XtManageChild(aboutacarsBox);
        }
