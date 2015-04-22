#include "gui.h"
#include "sharp95.h"
#include <X11/cursorfont.h>

#ifndef FLT_MAX
#define FLT_MAX 3.40282347E+38
#endif 	/* FLT_MAX */

#define MAX_MAPS	30 	/* maximum maps can be displayed for
							map menu */

/*
 *  Private functions
 */
#ifdef Linux
void    _quitNwxOK_cb ( Widget, XtPointer, XtPointer ) __attribute__((noreturn));
#else
void    _quitNwxOK_cb ( Widget, XtPointer, XtPointer );
#endif
int	_fnd_closestn ( float*, float* );
void	mapw_dataCb ( Widget, XtPointer, XtPointer );
void	mapw_exitCb ( Widget, XtPointer, XtPointer );
void	mapw_mapCb ( Widget, long, XtPointer );
int	mapw_rgstr_pfc ( Widget mapwin );  /* NOT used?! */
void	mapw_zoomCb ( Widget, XtPointer, XtPointer );

Widget   mapCanvW;
mapbnd_t mapBnd;

/************************************************************************
 * mapw.c                                                         	*
 *                                                                      *
 * This module creates the map selection window and defines its         *
 *	callbacks. 							*
 *									*
 * CONTENTS:								*
 *	mapw_create()	creates the map selection window.		*
 *	mapw_rgstr()	register the map selection window to GEMPAK	*
 *	mapw_pickstnCb()callback for the selection in drawing area.	*
 *	mapw_exposeCb()	callback for the map drawing area expose event.	*
 *	mapw_resizeCb()	callback for the map drawing area resize event.	*
 *	mapw_dataCb()	callback for the data button on menubar.	*
 *	mapw_mapCb()	callback for the map menu on menubar.		*
 *	mapw_zoomCb()	callback for the zoom button on menubar.	*
 *	mapw_exitCb()	callback for the exit button on menubar.	*
 *	_quitNwxOK_cb()	internal callback used in mapw_exitCb().	*
 ***********************************************************************/

/*=====================================================================*/

Widget mapw_create ( Widget parent )
/************************************************************************
 * mapw_create								*
 *									*
 * This function creates the map window.				*
 *									*
 * mapw_create ( parent )						*
 *									*
 * Input parameters:							*
 *	parent	Widget		The top level widget			*
 *									*
 * Output parameters:							*
 *									*
 * Return parameters:							*
 *	Widget		The map window widget				*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		 8/95   					*
 * T. Piper/SAIC	02/04	Removed Motif 1.1 check			*
 * T. Piper/SAIC	02/04	Changed location of nwx help file 	*
 ***********************************************************************/
{
Widget 		shell, form, menubar, button; 
Pixel		background;
int		i, nmap;
Arg		args[10];
Cardinal	argcnt;

char            *def_dir; 
static char     helpfile[128];

static _NXMmenuItem  mapmenu[MAX_MAPS];

/*---------------------------------------------------------------------*/
/*
 * Create the map selection window.
 */
        shell = XtCreatePopupShell( "mapobs",
				transientShellWidgetClass,
                        	parent, NULL, 0 );
	XtVaSetValues(shell, XmNtitle, "nwx", NULL);

	/* NxmMcloseReset(shell, mapw_exitCb, NULL); */

/*
 * create a form widget as the container widget
 */
        form = XtVaCreateWidget("mapform",
                                xmFormWidgetClass, shell,
                                NULL);
        XtVaGetValues(form, XmNbackground, &background, NULL);

/*
 * create the menubar
 */
        argcnt = 0;
        XtSetArg(args[argcnt],XmNtopAttachment,   XmATTACH_FORM); argcnt++;
        XtSetArg(args[argcnt],XmNleftAttachment,  XmATTACH_FORM); argcnt++;
        XtSetArg(args[argcnt],XmNrightAttachment, XmATTACH_FORM); argcnt++;
        menubar = XmCreateMenuBar(form, "topmenubar", args, argcnt);
        XtVaSetValues(menubar, XmNbackground, background, NULL);

/*
 * create Data pulldown menu
 */
        button = XmCreateCascadeButton(menubar, "Data", NULL, 0);
        XtAddCallback(button, XmNactivateCallback, mapw_dataCb, NULL);
        XtManageChild(button);

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
            mapmenu[i].callback     = (XtCallbackProc)mapw_mapCb;
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

        NxmMenuPulldownBuild(menubar, NULL, "Map", 0, mapmenu);

/*
 * Create Zoom pulldown menu
 */
        button = XmCreateCascadeButton(menubar, "Zoom", NULL, 0);
        XtAddCallback(button, XmNactivateCallback, mapw_zoomCb, NULL);
        XtManageChild(button);

/*
 * Create Exit button 
 */
        button = XmCreateCascadeButton(menubar, "Exit", NULL, 0);
        XtAddCallback(button, XmNactivateCallback, mapw_exitCb, NULL);
        XtManageChild(button);

/*
 * Create Help button 
 */
        button = XmCreateCascadeButton(menubar, "Help", NULL, 0);
        def_dir = getenv("GEMHLP");
        sprintf(helpfile, "%s/hlp/nwx.hlp", def_dir);
        XtAddCallback(button, XmNactivateCallback, (XtCallbackProc)NxmHelp_helpBtnCb,
                (XtPointer)helpfile); 
        XtManageChild(button);


/*
 * put the help cascade button at the right most side of
 * the menu bar
 */
        if ( (button = XtNameToWidget(menubar, "Help")) )
        	XtVaSetValues(menubar, XmNmenuHelpWidget, button, NULL);

        XtManageChild(menubar);

/*
 * Create the map drawing area
 */
        mapCanvW = XtVaCreateManagedWidget( "mapcanv",
                                xmDrawingAreaWidgetClass, form,
                                XmNtopAttachment,         XmATTACH_WIDGET,
                                XmNtopWidget,             menubar,
                                XmNleftAttachment,        XmATTACH_FORM,
                                XmNrightAttachment,       XmATTACH_FORM,
                                XmNbottomAttachment,      XmATTACH_FORM,
                                XmNresizable,             TRUE,
                                NULL );
/*
 * Add the expose and resize callbacks.
 */
        XtAddCallback( mapCanvW, XmNexposeCallback, 
			(XtCallbackProc)mapw_exposeCb, NULL );
        XtAddCallback( mapCanvW, XmNresizeCallback, 
			(XtCallbackProc)mapw_resizeCb, NULL );

/*
 * Add the station/state selection event.
 */
        XtAddEventHandler( mapCanvW, ButtonPressMask,   FALSE,
                            	(XtEventHandler)mapw_pickstnCb, NULL );

	XtManageChild( form );
        XtRealizeWidget( shell );
	
/*
 * create the product selection popup
 */
        /*dslw_create(shell);*/

	return( shell );
}

/*=====================================================================*/

int nsharp_mapw_rgstr ( Widget mapwin, char *xwinname )
/************************************************************************
 * mapw_rgstr                                                           *
 *                                                                      *
 * This routine will initiliaze the GEMPAK display variables and        *
 * register the map drawing area into a GEMPAK window.             	*
 *                                                                      *
 * mapw_rgstr ( mapwin )                                        	*
 *                                                                      *
 * Input parameters:                                                    *
 *      mapwin        Widget          The map drawing area widget       *
 * 	xwinname      *char	      name of window			*
 *                                                                      *
 * Return code:                                                   	*
 *      0 -- Successful.                    				*
 *     -1 -- Error.                    					*
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NMC         7/94                                           *
 * C. Lin/EAI            3/95 use new color bank structure              *
 * C. Lin/EAI            9/95                				*
 * S. Chiswell/Unidata	11/02 renamed from mapw_rgstr			*
 * T. Piper/SAIC	07/03	replaced gemdisplay with XtDisplay()	*
 ***********************************************************************/
{
XColor          cred;
Dimension       wdth, hght;

Window          gwin;
GC              gemgc;
Cursor          curs;

int             xwdth, xhght, xdpth;
int             ier, iret;

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
        gemgc = XCreateGC(XtDisplay(mapwin), gwin, 0, 0);

/*
 * Create a red arrow for the cursor.
 */
        curs = XCreateFontCursor(XtDisplay(mapwin), XC_top_left_arrow);
        XDefineCursor(XtDisplay(mapwin), gwin, curs);
        cred.red   = 65535;
        cred.blue  = 0;
        cred.green = 0;
        cred.flags = DoRed | DoBlue | DoGreen;
        XRecolorCursor(XtDisplay(mapwin), curs, &cred, &cred);

/*
 * Set the fill rule.
 */
        XSetFillRule(XtDisplay(mapwin), gemgc, WindingRule);

/*
 * Register the map window
 */
	xmotifw( gwin, xwinname, gemgc, xwdth, xhght, xdpth, &iret);
	gg_motf( xwinname, &ier, strlen(xwinname) );
	return( iret );
}

/*=====================================================================*/
/* ARGSUSED */
void nsharp_draw_map ( char *map_winname, mapstruct *mod_map, int *ier)
/************************************************************************
 * nsharp_draw_map							*
 *									*
 * nsharp_draw_map( map_winname, mod_map, ier )				*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * T. Piper/SAIC	07/03	removed map_init 			*
 ***********************************************************************/
{
    draw_map(mod_map->mapindx, nwxTable->map_info, mod_map->zoomflg,
                        &mod_map->mapb, ier);
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_pickstnCb ( Widget w, XtPointer clnt, XEvent *event,
			Boolean *continue_to_dispatch )
/************************************************************************
 * mapw_pickstnCb                                                       *
 *                                                                      *
 * This function will select a station/stations based on the position   *
 * of left mouse button.                                                *
 *                                                                      *
 * mapw_pickstnCb ( w, clnt, event )                             	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      clnt		XtPointer       The input data for the widget   *
 *      event           XEvent          The registered event            *
 **                                                                     *
 * C. Lin/EAI            6/95  modify from rband_start                  *
 * C. Lin/EAI            9/95  						*
 ***********************************************************************/
{
int             ifound, iret;
int             jcolr, imrk;
float           xloc, yloc;

/*---------------------------------------------------------------------*/
/*
 * Check if it is ploting the values
 */
	if ( plotData.mode != STNSELECT )
		return;

/*
 * Check if it is a left mouse button.
 */
        if  ( event->xbutton.button == 1 ) {

/*
 * set the search flag indicating it is new data
 */
	    srchInfo.sflag = 0;

/*
 * If there were selected stations, clear the selected
 * station markers.
 */
            if  ( plotData.plt_mark.nstn )  {
                    jcolr = RES_COL;
                    imrk  = SEL_MRK;
                    draw_stnmark( plotData.plt_mark.nstn, 
				  plotData.plt_mark.lat, 
				  plotData.plt_mark.lon, 
				  jcolr, imrk, &iret);
            }

/*
 * redraw all station markers
 */
            jcolr = ALL_COL;
            imrk  = ALL_MRK;
            draw_stnmark(stnList.nstn, stnList.lat, stnList.lon, 
			 jcolr, imrk, &iret);

/*
 * Get the x and y values for the button press.
 */
            xloc = (float) event->xbutton.x;
            yloc = (float) event->xbutton.y;

/*
 * Find the closest station to the press location.
 */
	    ifound = _fnd_closestn( &xloc, &yloc ); 
	    if ( ifound == -1 ) return;

/*
 * Set the selected lat-lon for a single station.
 */
                plotData.plt_mark.lat[0]  = stnList.lat[ifound];
                plotData.plt_mark.lon[0]  = stnList.lon[ifound];
                plotData.plt_mark.nstn = 1;

/*
 * Plot the selected station markers.
 */
            jcolr = SEL_COL;
            imrk  = SEL_MRK;
            draw_stnmark( plotData.plt_mark.nstn, plotData.plt_mark.lat, 
			plotData.plt_mark.lon, jcolr, imrk, &iret );

	    sta_select_cb(ifound);

        } /* end of checking xbutton=1 */
}

/*======================================================================*/

int _fnd_closestn ( float *xin, float *yin )
/************************************************************************
 * int _fnd_closestn							*
 *									*
 * This routine finds the closest station marker on the map window	*
 *									*
 * int _fnd_closestn ( xin, yin )					*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * L. Williams/EAI	10/95						*
 ************************************************************************/
{
char sys1[2];
char sys2[2];
int  found=-1, nstn;
int  i, ier=0;
float dist1, dist2;
float *rx, *ry;
double x, y;

/*----------------------------------------------------------------------*/

	strcpy (sys1, "M");
	strcpy (sys2, "D");

	if ( srchInfo.smethod == STANDARD ) {

		nstn = stnList.nstn;

		rx = (float *)malloc( sizeof(float) * (size_t)nstn);
		ry = (float *)malloc( sizeof(float) * (size_t)nstn);

/*
 * Transform from map coordinates to plot coordinates.
 */
		gtrans( sys1, sys2, &nstn, stnList.lat, stnList.lon,
		 	rx, ry, &ier, strlen(sys1), strlen(sys2) );

	}
	else if ( srchInfo.smethod == WATCHWARN ) {

		nstn = stnList.nrptstn;

		if ( nstn == 0 ) {
			return ( -1 ) ;
		}
		else {

			rx = (float *)malloc( sizeof(float) * (size_t)nstn);
			ry = (float *)malloc( sizeof(float) * (size_t)nstn);

/*
 * Transform from map coordinates to plot coordinates.
 */
			gtrans( sys1, sys2, &nstn, stnList.rptstnlat, stnList.rptstnlon,
			 	rx, ry, &ier, strlen(sys1), strlen(sys2) );
		}
	}

/*
 * Find the closest data point.
 */
	dist1  = FLT_MAX;
	for (i=0; i < nstn; i++) {

	    x =  (double)(rx[i] - *xin);
	    y =  (double)(ry[i] - *yin);

	    dist2 = (float)(pow( x, 2.0 ) + pow( y, 2.0 ));

	    if  ( dist2 < dist1 ) {
		dist1 = dist2;
		found = i;
	    }
	}

	free(rx);
	free(ry);
	return (found);
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_exposeCb ( Widget w, XtPointer clnt,
				XmDrawingAreaCallbackStruct *call)
/************************************************************************
 * mapw_exposeCb                                                        *
 *                                                                      *
 * This routine is the callback for expose window.                      *
 *                                                                      *
 * mapw_exposeCb ( w, clnt, call )                          		*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      clnt		XtPointer       Not used			*
 *      *call		XmDrawingAreaCallbackStruct  call_data		* 
 **                                                                     *
 * S. Jacobs/NMC         8/94                                           *
 * T. Piper/SAIC	12/04	Added check on count			*
 **********************************************************************/
{
int    raise, iret;
XEvent *event;

/*---------------------------------------------------------------------*/
    event = call->event;

/*
 * Flush the graphics buffer.
 */
    if ( event->xexpose.count == 0 ) {
	raise = G_TRUE;
	xxflsh ( &raise, &iret );
    }
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_resizeCb ( Widget w, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * mapw_resizeCb                                                        *
 *                                                                      *
 * This routine is the callback for resize window.                      *
 *                                                                      *
 * mapw_resizeCb ( w, clnt, cbs )                                  	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      clnt		XtPointer	The input data for the widget   *
 *      cbs		XtPointer       callback structure		*
 **                                                                     *
 * S. Jacobs/NMC         8/94           add check for valflg            *
 * C. Lin/EAI		10/95						*
 ***********************************************************************/
{
int  iret;
/*---------------------------------------------------------------------*/
/*
 * Clear the screen.
 */
        gclear( &iret );
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_zoomCb ( Widget w, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * mapw_zoomCb                                                        	*
 *                                                                      *
 * This routine is the callback for the zoom button in the top menubar. *
 *                                                                      *
 * mapw_zoomCb ( w, clnt, cbs )                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      clnt            XtPointer       The input data for the widget   *
 *      cbs	        XtPointer       callback structure		*
 **                                                                     *
 * C. Lin/EAI    6/95   based on the obsolete function rband_end().     *
 * C. Lin/EAI   10/95   						*
 * T. Piper/SAIC	05/03	replaced XAllocNamedColor w/xsncolr	*
 ***********************************************************************/
{
static int    org_flg = 0;
static Pixel  org_zoombc, zoombc;
static int zflag = G_FALSE; /* flag for the zoom toggle button */

int    i, ityp, np, iret;

float  xpts[2], ypts[2], xdev[2], ydev[2];
char   sysin[2], sysout[2];


/*---------------------------------------------------------------------*/
/*
 * Toggle the Zoom Button
 */
        if (zflag == G_TRUE) {  /* zoom off */

                zflag = G_FALSE;

/*
 * Restore the button color
 */
                XtVaSetValues(w, XmNforeground, org_zoombc, NULL);

/*
 * Restore the cursor
 */
                /*NuiDefaultCursor( mapCanvW );*/
		NxmCursor_setCursor(mapCanvW, CURS_DEFAULT);

/*
 * Restore mouse to be selection status
 */
                XSelectInput( XtDisplay(mapCanvW), XtWindow(mapCanvW),
                        ButtonPressMask | ButtonReleaseMask |
                        ExposureMask );

                XtAddEventHandler( mapCanvW, ButtonPressMask, FALSE,
                        (XtEventHandler)mapw_pickstnCb, NULL );
        }
        else { /* zoom on */

                zflag = G_TRUE;

/*
 * Change mouse event handling for zooming
 */
                XtRemoveEventHandler( mapCanvW, ButtonPressMask, FALSE,
                        (XtEventHandler)mapw_pickstnCb, NULL);

                usrSelect.zoomflg = ZOOM;

/*
 * get the original zoom button color and
 * the color in zoom status when needed.
 */
                if ( !org_flg ) {

                        XtVaGetValues(w, XmNforeground, &org_zoombc, NULL);
                        xsncolr("red", &zoombc, &iret);

                        org_flg = 1;
                }

/*
 * change to the zoom cursor
 */
                /*NxmCursorChange( mapCanvW, XC_crosshair, "white");*/
		NxmCursor_setCursor(mapCanvW, XC_crosshair);

/*
 * change zoom button color
 */
                XtVaSetValues( w, XmNforeground, zoombc, NULL);
                XmUpdateDisplay( w );

/*
 * get two corner points
 */
                ityp = 3;
                np = 2;
                strcpy ( sysin, "D" );
                for ( i = 0; i < np; i++)
                        xpts[i] = ypts[i] = 0.0F;
                ggtpnt( sysin, &ityp, xdev, ydev, &iret, strlen(sysin));

/*
 * check if the box is big enough
 */
                if ( fabs((double)(xdev[0] - xdev[1])) > 20.0 &&
                        fabs((double)(ydev[0] - ydev[1])) > 20.0 ) {

                        strcpy ( sysout, "M" );
                        gtrans(sysin, sysout, &np, xdev, ydev, xpts, ypts,
                                &iret, strlen(sysin), strlen(sysout));

                        if ( iret == 0 ) {
                                for ( i = 0; i < np; i++) {
                                        mapBnd.x[i] = xpts[i];
                                        mapBnd.y[i] = ypts[i];
                                };
			}
                }

/*
 * Reset the zoom button
 */
                mapw_zoomCb( w, NULL, NULL);
        }
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_exitCb ( Widget w, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * mapw_exitCb                                                        	*
 *                                                                      *
 * This routine is the callback for the exit button.                    *
 *                                                                      *
 * mapw_exitCb ( w, clnt, cbs )                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      clnt            XtPointer       The input data for the widget   *
 *      cbs		XtPointer       callback structure		*
 **                                                                     *
 * S. Jacobs/NMC         8/94                                           *
 * C. Lin/EAI           10/95	restructure				*
 ***********************************************************************/
{
/*
 * Set the exit message and display the exit dialog box.
 *
        strcpy( message, "OK to Exit from NWX?" );

        NxmExitDialog( w, "Exit Confirmation", message,
                        _quitNwxOK_cb);

 */
}

/*======================================================================*/
/* ARGSUSED */
void _quitNwxOK_cb ( Widget w, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * _quitNwxOK_cb                                                     	*
 *                                                                      *
 * This routine is the callback for OK button in ExitDialog.            *
 *                                                                      *
 * _quitNwxOK_cb ( w, clnt, cbs )                                	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      clnt            XtPointer       The input data for the widget   *
 *      cbs		XtPointer       callback structure		*
 **                                                                     *
 * S. Jacobs/NMC         8/94                                           *
 ***********************************************************************/
{
/*
 * Quit the program.
 */
    exit(0);
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_mapCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * mapw_mapCb                                                           *
 *                                                                      *
 * This routine is the callback for the map area menu.                  *
 *                                                                      *
 * mapw_mapCb ( w, which, cbs ) 	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      which           long            The item number for the menu    *
 *      cbs	 	XtPointer       callback structure for widget	*
 **                                                                     *
 * S. Jacobs/NMC         8/94                                           *
 * L. Williams/EAI      12/94           add check for valflg            *
 * C. Lin/EAI	        10/95						*
 ***********************************************************************/
{
/*
 * Set the global variable for the map menu selection.
 */
    usrSelect.mapindx = (int)which;

/*
 * Draw the map window.
 */
    usrSelect.zoomflg = NO_ZOOM;
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_dataCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * mapw_dataCb                                                          *
 *                                                                      *
 * This function will pop up  the data selection shell widget           *
 *                                                                      *
 * void mapw_dataCb(w, clnt, call)                         	 	*
 *                                                                      *
 * Input Parameters:                                                    *
 *  w                   Widget                                          *
 *  clnt		Widget                                          *
 *  call		Widget                                          *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * L. Williams          06/95                                           *
 ************************************************************************/
{
     /* XtManageChild(dataSelectW);*/
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_pickstnCb_pfc ( Widget w, XtPointer clnt, XEvent *event,
			  Boolean *continue_to_dispatch )
/************************************************************************
 * mapw_pickstnCb_pfc                                                   *
 *                                                                      *
 * This function will select a station/stations based on the position   *
 * of left mouse button.                                                *
 *                                                                      *
 * mapw_pickstnCb_pfc ( w, clnt, event )                         	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      clnt		XtPointer       The input data for the widget   *
 *      event           XEvent          The registered event            *
 **                                                                     *
 * C. Lin/EAI            6/95  modify from rband_start                  *
 * C. Lin/EAI            9/95  						*
 ***********************************************************************/
{
int             ifound, iret;
int             jcolr, imrk;
float           xloc, yloc;

/*---------------------------------------------------------------------*/
/*
 * Check if it is ploting the values
 */
	if ( plotData.mode != STNSELECT )
		return;

/*
 * Check if it is a left mouse button.
 */
        if  ( event->xbutton.button == 1 ) {

/*
 * set the search flag indicating it is new data
 */
	    srchInfo.sflag = 0;

/*
 * If there were selected stations, clear the selected
 * station markers.
 */
            if  ( plotData.plt_mark.nstn )  {
                    jcolr = RES_COL;
                    imrk  = SEL_MRK;
                    draw_stnmark( plotData.plt_mark.nstn, 
				  plotData.plt_mark.lat, 
				  plotData.plt_mark.lon, 
				  jcolr, imrk, &iret);
            }

/*
 * redraw all station markers
 */
            jcolr = ALL_COL;
            imrk  = ALL_MRK;
            draw_stnmark(stnList.nstn, stnList.lat, stnList.lon, 
			 jcolr, imrk, &iret);

/*
 * Get the x and y values for the button press.
 */
            xloc = (float) event->xbutton.x;
            yloc = (float) event->xbutton.y;

/*
 * Find the closest station to the press location.
 */
	    ifound = _fnd_closestn( &xloc, &yloc ); 
	    if ( ifound == -1 ) return;

/*
 * Set the selected lat-lon for a single station.
 */
                plotData.plt_mark.lat[0]  = stnList.lat[ifound];
                plotData.plt_mark.lon[0]  = stnList.lon[ifound];
                plotData.plt_mark.nstn = 1;

/*
 * Plot the selected station markers.
 */
            jcolr = SEL_COL;
            imrk  = SEL_MRK;
            draw_stnmark( plotData.plt_mark.nstn, plotData.plt_mark.lat, 
			plotData.plt_mark.lon, jcolr, imrk, &iret );

	    sta_select_cb_pfc(ifound);

        } /* end of checking xbutton=1 */
}

/*=====================================================================*/

int mapw_rgstr_pfc ( Widget mapwin )
/************************************************************************
 * mapw_rgstr_pfc                                                       *
 *                                                                      *
 * This routine will initiliaze the GEMPAK display variables and        *
 * register the map drawing area into a GEMPAK window.             	*
 *                                                                      *
 * mapw_rgstr_pfc ( mapwin )                                        	*
 *                                                                      *
 * Input parameters:                                                    *
 *      mapwin        Widget          The map drawing area widget       *
 *                                                                      *
 * Return code:                                                   	*
 *      0 -- Successful.                    				*
 *     -1 -- Error.                    					*
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NMC         7/94                                           *
 * C. Lin/EAI            3/95 use new color bank structure              *
 * C. Lin/EAI            9/95                				*
 * S. Chiswell/Unidata  11/02 Added mapindex parameter to call sequence	*
 * T. Piper/SAIC	07/03	replaced gemdisplay with XtDisplay()	*
 ***********************************************************************/
{
XColor          cred;
Dimension       wdth, hght;
Window          gwin;
GC              gemgc;
Cursor          curs;
int             xwdth, xhght, xdpth;
int             ier, iret;
char		wname[8] = "maptop2";

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
        gemgc = XCreateGC(XtDisplay(mapwin), gwin, 0, 0);

/*
 * Create a red arrow for the cursor.
 */
        curs = XCreateFontCursor(XtDisplay(mapwin), XC_top_left_arrow);
        XDefineCursor(XtDisplay(mapwin), gwin, curs);
        cred.red   = 65535;
        cred.blue  = 0;
        cred.green = 0;
        cred.flags = DoRed | DoBlue | DoGreen;
        XRecolorCursor(XtDisplay(mapwin), curs, &cred, &cred);

/*
 * Set the fill rule.
 */
        XSetFillRule(XtDisplay(mapwin), gemgc, WindingRule);

/*
 * Register the map window
 */
	xmotifw(gwin, wname, gemgc, xwdth, xhght, xdpth, &iret);
	gg_motf( wname, &ier, strlen(wname) );
	return( iret );
}
