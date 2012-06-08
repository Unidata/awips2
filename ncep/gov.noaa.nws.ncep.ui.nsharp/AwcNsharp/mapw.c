/* L. Hinson, AWC 11/02   Corrected errors in functions mapw_create, mapw_rgstr */
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/DrawingA.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>

#include "gui.h"
#include <xwcmn.h>
#include <math.h>
#include <limits.h>

#ifndef FLT_MAX
	#define FLT_MAX 3.40282347E+38
#endif 	/* FLT_MAX */

#define MAX_MAPS	30 	/* maximum maps can be displayed for
							map menu */

Widget mapw_create();
int    mapw_rgstr();
void   mapw_pickstnCb();
void   mapw_exposeCb();
void   mapw_resizeCb();
void   mapw_dataCb();
void   mapw_mapCb();
void   mapw_zoomCb();
void   mapw_exitCb();
void   _quitNwxOK_cb();
int    _fnd_closestn();

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

Widget mapw_create( parent )
Widget	parent;

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
 ************************************************************************/

{
Widget 		shell, form, menubar, button; 
Pixel		background;
int		i, nmap, argcnt;
Arg		args[10];

char            *def_dir; 
static char     helpfile[128];

static _NXMmenuItem  mapmenu[MAX_MAPS];

/*---------------------------------------------------------------------*/

	/*
 	 * Create the map selection window.
 	 */
        shell = XtCreatePopupShell( "maptop",
				transientShellWidgetClass,
                        	parent, NULL, 0 );
	XtVaSetValues(shell, XmNtitle, "nwx", NULL);

	/*
	 * MOTIF1.1 has problem with taking out close from mwm
	 */
	if ( (XmVERSION == 1) && (XmREVISION == 1) )
	    XtVaSetValues(shell, XmNdeleteResponse, XmDO_NOTHING, NULL);
	else 
	    NxmClose_menuReset(shell, mapw_exitCb, NULL);

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
            mapmenu[i].mnemonic     = CHNULL;
            mapmenu[i].accelerator  = (char *) NULL;
            mapmenu[i].accel_text   = (char *) NULL;
            mapmenu[i].callback     = mapw_mapCb;
            mapmenu[i].which_widget = i;
            mapmenu[i].subitems     = NULL;
            mapmenu[i].sub_buttons  = NULL;
        }
        mapmenu[nmap].label        = (char *) NULL;
        mapmenu[nmap].class        = &xmCascadeButtonGadgetClass;
        mapmenu[nmap].mnemonic     = CHNULL;
        mapmenu[nmap].accelerator  = (char *) NULL;
        mapmenu[nmap].accel_text   = (char *) NULL;
        mapmenu[nmap].callback     = NULL;
        mapmenu[nmap].which_widget = nmap;
        mapmenu[nmap].subitems     = NULL;
        mapmenu[nmap].sub_buttons  = NULL;

        NxmMenuPulldownBuild(menubar, NULL, "Map", CHNULL, mapmenu);

	/*
	 * Create Zoom pulldown menu
	 */
#ifdef LESSTIF_VERSION
        button = XmCreatePushButton(menubar, "Zoom", NULL, 0);
	XtVaSetValues(button, XmNshadowThickness, 0, NULL);
#else
        button = XmCreateCascadeButton(menubar, "Zoom", NULL, 0);
#endif
        XtAddCallback(button, XmNactivateCallback, mapw_zoomCb, NULL);
        XtManageChild(button);

	/*
	 * Create Exit button 
	 */
#ifdef LESSTIF_VERSION
        button = XmCreatePushButton(menubar, "Exit", NULL, 0);
	XtVaSetValues(button, XmNshadowThickness, 0, NULL);
#else
        button = XmCreateCascadeButton(menubar, "Exit", NULL, 0);
#endif
        XtAddCallback(button, XmNactivateCallback, mapw_exitCb, NULL);
        XtManageChild(button);

	/*
	 * Create Help button 
	 */
        button = XmCreateCascadeButton(menubar, "Help", NULL, 0);
        def_dir = getenv("NAWIPS_HELP");
        sprintf(helpfile, "%s/nwx/nwx.hlp", def_dir);
        XtAddCallback(button, XmNactivateCallback, (XtCallbackProc)NxmHelp_helpBtnCb,
                (XtPointer)helpfile);
        XtManageChild(button);


        /*
         * put the help cascade button at the right most side of
         * the menu bar
         */
        if(button = XtNameToWidget(menubar, "Help"))
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
        XtAddCallback( mapCanvW, XmNexposeCallback, mapw_exposeCb, NULL );
        XtAddCallback( mapCanvW, XmNresizeCallback, mapw_resizeCb, NULL );

	/*
 	 * Add the station/state selection event.
 	 */
        XtAddEventHandler( mapCanvW, ButtonPressMask,   FALSE,
                            	mapw_pickstnCb, NULL );

	XtManageChild( form );
        XtRealizeWidget( shell );
	
        /*
         * create the product selection popup
         */
        /*dslw_create(shell);*/

	return( shell );

}

/*=====================================================================*/

int mapw_rgstr( mapwin )
Widget mapwin;

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
 *                                                                      *
 * Return code:                                                   	*
 *      0 -- Successful.                    				*
 *     -1 -- Error.                    					*
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NMC         7/94                                           *
 * C. Lin/EAI            3/95 use new color bank structure              *
 * C. Lin/EAI            9/95                				*
 ***********************************************************************/

{
XColor          cred;
Dimension       wdth, hght;
struct maptype_list     map_info[2];
mapbnd_t                mapb;


Window          gwin;
GC              gemgc;
Cursor          curs;

int             xwdth, xhght, xdpth;

int             iret, mapindx;
char		gemdevice[72];

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
 	strcpy(gemdevice,"maptop");
        xmotifw(gwin, gemdevice, gemgc, xwdth, xhght, xdpth, &iret);
	if( iret != 0 ) 
		return( iret );
	/*
 	 * Draw the US map. 	FORTRAN function map_init()
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

/*=====================================================================*/

void mapw_pickstnCb( w, client_data, event )
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
 * C. Lin/EAI            6/95  modify from rband_start                  *
 * C. Lin/EAI            9/95  						*
 * T. Piper/SAIC	11/02	Removed unused variable stnindex	*
 ***********************************************************************/

{
int             ifound, iret;
int             i, j, k, jcolr, imrk;
float           xloc, yloc;
char            nodata_msg[80];
float           *lat, *lon;

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
                plotData.plt_mark.elev[0] = stnList.elv[ifound];
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


int _fnd_closestn ( xin, yin )
float	*xin;
float	*yin;

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
int  found, nstn;
int  i, ier=0;
float dist1, dist2;
float *rx, *ry;
double x, y;

/*----------------------------------------------------------------------*/

	strcpy (sys1, "M");
	strcpy (sys2, "D");

	if ( srchInfo.smethod == STANDARD ) {

		nstn = stnList.nstn;

		rx = (float *)malloc( sizeof(float) * nstn);
		ry = (float *)malloc( sizeof(float) * nstn);

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

			rx = (float *)malloc( sizeof(float) * nstn);
			ry = (float *)malloc( sizeof(float) * nstn);

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

	    x =  rx[i] - *xin;
	    y =  ry[i] - *yin;

	    dist2 = pow( x, 2.0 ) + pow( y, 2.0 );

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

void mapw_exposeCb( w, which, cb_data )
Widget          w;
int             which;
XtPointer       cb_data;

/************************************************************************
 * mapw_exposeCb                                                        *
 *                                                                      *
 * This routine is the callback for expose window.                      *
 *                                                                      *
 * mapw_exposeCb ( w, which, cb_data )                                  *
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      which           int             The input data for the widget   *
 *                                                                      *
 * Output parameters:                                                   *
 *      cb_data         XtPointer       The output data for the widget  *
 **                                                                     *
 * S. Jacobs/NMC         8/94                                           *
 ***********************************************************************/

{
int    raise, iret;

/*---------------------------------------------------------------------*/

	/*
 	 * Flush the graphics buffer.
 	 */
        raise = G_TRUE;
        xxflsh ( &raise, &iret );

}

/*=====================================================================*/

void mapw_resizeCb( w, which, cb_data )
Widget          w;
int             which;
XtPointer       cb_data;

/************************************************************************
 * mapw_resizeCb                                                        *
 *                                                                      *
 * This routine is the callback for resize window.                      *
 *                                                                      *
 * mapw_resizeCb ( w, which, cb_data )                                  *
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      which           int             The input data for the widget   *
 *                                                                      *
 * Output parameters:                                                   *
 *      cb_data         XtPointer       The output data for the widget  *
 **                                                                     *
 * S. Jacobs/NMC         8/94           add check for valflg            *
 * C. Lin/EAI		10/95						*
 ***********************************************************************/

{
int  iret;

	/*
 	 * Clear the screen.
 	 */
        gclear( &iret );

}

/*=====================================================================*/

void mapw_zoomCb( w, data, cb_data )
Widget          w;
XtPointer       data;
XtPointer       cb_data;

/************************************************************************
 * mapw_zoomCb                                                        	*
 *                                                                      *
 * This routine is the callback for the zoom button in the top menubar. *
 *                                                                      *
 * mapw_zoomCb ( w, data, cb_data )                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      data            XtPointer       The input data for the widget   *
 *                                                                      *
 * Output parameters:                                                   *
 *      cb_data         XtPointer       The callback data for the widget*
 **                                                                     *
 * C. Lin/EAI    6/95   based on the obsolete function rband_end().     *
 * C. Lin/EAI   10/95   						*
 ***********************************************************************/

{
static int    org_flg = 0;
static Pixel  org_zoombc;
static XColor zoombc;
static int zflag = G_FALSE; /* flag for the zoom toggle button */

int    i, ityp, np, ix[2], iy[2], iret;

XColor ignore;

float  xpts[2], ypts[2], xdev[2], ydev[2];
char   sysin[2], sysout[2];
int    jcolr, imrk;

void   mapw_pickstnCb();

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
                NuiDefaultCursor( mapCanvW );

                /*
                 * Restore mouse to be selection status
                 */
                XSelectInput( gemdisplay, XtWindow(mapCanvW),
                        ButtonPressMask | ButtonReleaseMask |
                        ExposureMask );

                XtAddEventHandler( mapCanvW, ButtonPressMask, FALSE,
                        mapw_pickstnCb, NULL );
        }
        else { /* zoom on */

                zflag = G_TRUE;

                /*
                 * Change mouse event handling for zooming
                 */
                XtRemoveEventHandler( mapCanvW, ButtonPressMask, FALSE,
                        mapw_pickstnCb, NULL);

                usrSelect.zoomflg = G_TRUE;

                /*
                 * get the original zoom button color and
                 * the color in zoom status
                 * when needed.
                 */
                if ( !org_flg ) {

                        XtVaGetValues(w, XmNforeground, &org_zoombc, NULL);
                        XAllocNamedColor( gemdisplay, gemmap, "red",
                                &zoombc, &ignore );

                        org_flg = 1;
                }

                /*
                 * change to the zoom cursor
                 */
                NxmCursorChange( mapCanvW, XC_crosshair, "white");

                /*
                 * change zoom button color
                 */
                XtVaSetValues( w, XmNforeground, zoombc.pixel, NULL);
                XmUpdateDisplay( w );

                /*
                 * get two corner points
                 */
                ityp = 2;
                np = 2;
                strcpy ( sysin, "D" );
                for ( i = 0; i < np; i++)
                        xpts[i] = ypts[i] = 0.0;
                ggtpnt( sysin, &ityp, xdev, ydev, &iret, strlen(sysin));

                /*
                 * check if the box is big enough
                 */
                if ( fabs(xdev[0] - xdev[1]) > 20 &&
                        fabs(ydev[0] - ydev[1]) > 20 ) {

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

void mapw_exitCb( w, data, cb_data )
Widget          w;
XtPointer       data;
XtPointer       cb_data;
/************************************************************************
 * mapw_exitCb                                                        	*
 *                                                                      *
 * This routine is the callback for the exit button.                    *
 *                                                                      *
 * mapw_exitCb ( w, data, cb_data )                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      data            XtPointer       The input data for the widget   *
 *                                                                      *
 * Output parameters:                                                   *
 *      cb_data         XtPointer       The output data for the widget  *
 **                                                                     *
 * S. Jacobs/NMC         8/94                                           *
 * C. Lin/EAI           10/95	restructure				*
 * T. Piper/SAIC	11/02	Fixed calling sequence to NxmExit_create*
 ***********************************************************************/
{
char    message[50];

/*---------------------------------------------------------------------*/

	/*
 	 * Set the exit message and display the exit dialog box.
 	 */
        strcpy( message, "OK to Exit from Nsharp?" );

        NxmExit_create( w, "Exit Confirmation", message,
                        _quitNwxOK_cb, NULL);

}

void _quitNwxOK_cb( w, data, cb_data )
Widget          w;
XtPointer       data;
XtPointer       cb_data;

/************************************************************************
 * _quitNwxOK_cb                                                     	*
 *                                                                      *
 * This routine is the callback for OK button in ExitDialog.            *
 *                                                                      *
 * _quitNwxOK_cb ( w, data, cb_data )                                	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      data            XtPointer       The input data for the widget   *
 *                                                                      *
 * Output parameters:                                                   *
 *      cb_data         XtPointer       The output data for the widget  *
 **                                                                     *
 * S. Jacobs/NMC         8/94                                           *
 ***********************************************************************/

{
/*---------------------------------------------------------------------*/

	/*
 	 * Quit the program.
 	 */
        exit(0);

}

/*=====================================================================*/

void mapw_mapCb( w, which, cb_data )
Widget          w;
int             which;
XtPointer       cb_data;

/************************************************************************
 * mapw_mapCb                                                           *
 *                                                                      *
 * This routine is the callback for the map area menu.                  *
 *                                                                      *
 * mapw_mapCb ( w, which, cb_data )                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      which           int             The item number for the menu    *
 *                                                                      *
 * Output parameters:                                                   *
 *      cb_data         XtPointer       The output data for the widget  *
 **                                                                     *
 * S. Jacobs/NMC         8/94                                           *
 * L. Williams/EAI      12/94           add check for valflg            *
 * C. Lin/EAI	        10/95						*
 ***********************************************************************/

{
/*---------------------------------------------------------------------*/

	/*
 	 * Set the global variable for the map menu selection.
 	 */
        usrSelect.mapindx = which;

	/*
 	 * Draw the map window.
 	 */
        usrSelect.zoomflg = G_FALSE;

}


/*=====================================================================*/

void mapw_dataCb(w, client_data, call_data)
Widget              w;
XtPointer           client_data;
XtPointer           call_data;

/************************************************************************
 * mapw_dataCb                                                          *
 *                                                                      *
 * This function will pop up  the data selection shell widget           *
 *                                                                      *
 * void mapw_dataCb(w, client_data, call_data)                          *
 *                                                                      *
 * Input Parameters:                                                    *
 *  w                   Widget                                          *
 *  client_data         Widget                                          *
 *  call_data           Widget                                          *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * L. Williams          06/95                                           *
 ************************************************************************/
{
     /* XtManageChild(dataSelectW);*/
}

/*=====================================================================*/
