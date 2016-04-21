/*******************************************************************************
* FILENAME:            display_adapt_param.c
* NUMBER OF MODULES:   2
* GENERAL INFORMATION:
*   MODULE 1:          display_adapt_param
* DESCRIPTION:         This routine displays the radar adaptable parameters
*		       from SingleSiteRadar->Options under MPEcontrol menu
*                      in the popup viewer window.
*		       Calling function: callback from Display Adaptable Params 
*		       button under Single Site Radar Window.
*
*   MODULE 2:          popdown_viewer
* DESCRIPTION:         This routine closes popup viewer window.
*
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP Unix / Dell Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE          PROGRAMMER        DESCRIPTION/REASON
*          2        3/25/2002    Moria Shebsovich    Revision
*******************************************************************************
*/


#include "display_adapt_param.h"
#include "libXs.h"
#include "read_adapt_param_RFCW.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "xs_create_menu_buttons.h"

static xs_menu_struct   Viewer_display[] =
{
{"Close" , popdown_viewer, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL}
};

void display_adapt_param ( Widget w , XtPointer clientdata ,
                           XtPointer calldata )

{
    Widget       shell, viewer_form, viewer_shell,viewer_menuBar,textWidget;
    Arg          wargs[10];
    char text[3000], cr = '\n';
    int ss_number = ( int ) clientdata ;

  /*------------------------------------------------*/
  /*  create text for display in window             */
  /*------------------------------------------------*/

  sprintf(text,
    "          ***** Radar = %s *****\n"
    "          Preprocessing Algorithm Parameters\n"
    "%.2f = min reflectivity for isolated bin test (dBZ)%c"
    "%.2f = max reflectivity before being labelled as outlier (dBZ)%c"
    "%.2f = min precip echo area for tilt test in low elev (km**2)%c"
    "%.2f = min area-weighted reflectivity for tilt test in low elev (dBZ)%c"
    "%.2f = min reflectivity in tilt test (dBZ)%c%.2f = inner range for tilt test (km)%c"
    "%.2f = outer range for tilt test (km)%c"
    "%.2f = min range of biscan maximization (km)%c%.2f = max range of biscan maximization (km)%c"
    "%.2f = max percent area reduction between 2 lowest elevations (%%)%c%.2f = Z-R multiplicative coefficient%c"
    "%.2f = Z-R power coefficient%c%.2f = min reflectivity to convert to rate (dBZ)%c"
    "%.2f = max reflectivity to convert to rate (dBZ)%c"
    "%c"
    "          Rate Algorithm Parameters\n"
    "%.2f = max storm speed (m/s)%c%.2f = max scan-to-scan time difference for time continuity test (min)%c"
    "%.2f = min precip area for performing time continuity (km**2)%c"
    "%.2f = rate of change of volumetric precip rate, limited area (1/hr)%c"
    "%.2f = rate of change of volumetric precip rate, whole umbrella (1/hr)%c%.2f = max rate of echo area change (km**2/hr)%c"
    "%.2f = range beyond which to apply range-effect correction (km)%c%.2f = range effect coefficient 1 (dBR)%c"
    "%.2f = range effect coefficient 2%c%.2f = range effect coefficient 3%c"
    "%.2f = min precip rate (mm/hr)%c%.2f = max precip rate (mm/hr)%c"
    "%c"
    "          Accumulation Algorithm Parameters\n"
    "%.2f = threshold elapsed time to restart (min)%c%.2f = max time difference between scans for interpolation (min)%c"
    "%.2f = min time needed to accumulate hourly total (min)%c%.2f = threshold for hourly accumulation outlier (mm)%c"
    "%.2f = end time for gage accumulation (min)%c%.2f = max scan-to-scan accumulation (mm)%c"
    "%.2f = maximum hourly accumulation (mm)%c"
    "%c"
    "          Adjustment Algorithm Parameters\n"
    "%.2f = minutes after clock hour when bias is updated (min)%c"
    "%.2f = min number of hourly gage-radar pairs needed to calculate bias%c"
    "%.2f = reset value of gage-radar bias estimate%c"
    "%.2f = longest lag for using table (hrs)%c"
    "%s = bias applied flag"
    ,
  nexrad [ ss_number ].id,
  ad_params[0],cr,ad_params[1],cr,
  ad_params[7],cr,ad_params[8],cr,
  ad_params[2],cr,ad_params[3],cr,
  ad_params[4],cr,ad_params[6],cr,
  ad_params[5],cr,ad_params[9],cr,
  ad_params[10],cr,ad_params[11],cr,
  ad_params[12],cr,ad_params[13],cr,
  cr,
  ad_params[14],cr,ad_params[15],cr,
  ad_params[16],cr,ad_params[17],cr,
  ad_params[18],cr,ad_params[19],cr,
  ad_params[20],cr,ad_params[21],cr,
  ad_params[22],cr,ad_params[23],cr,
  ad_params[24],cr,ad_params[25],cr,
  cr,
  ad_params[26],cr,ad_params[27],cr,
  ad_params[28],cr,ad_params[29],cr,
  ad_params[30],cr,ad_params[31],cr,
  ad_params[32],cr,
  cr,
  ad_params[33],cr,ad_params[34],cr,
  ad_params[35],cr,ad_params[36],cr,
  ad_param46
  );

  /*------------------------------------------------*/
  /*  set up window for displaying parameters       */
  /*------------------------------------------------*/

 shell = XtCreatePopupShell
		(
		"Adaptation_Parameter_viewer",
		transientShellWidgetClass,
		toplevel,
		NULL,
		0
		);


 viewer_shell = shell;

 viewer_form = XtCreateManagedWidget("viewer_form", xmFormWidgetClass, shell, NULL, 0);

 XtSetArg(wargs[0], XmNtopAttachment, XmATTACH_FORM);
 XtSetArg(wargs[1], XmNleftAttachment, XmATTACH_FORM);
 XtSetArg(wargs[2], XmNrightAttachment, XmATTACH_FORM);
 viewer_menuBar = XmCreateMenuBar(viewer_form, "viewer_menuBar", wargs, 3);
 XtManageChild(viewer_menuBar);
 Viewer_display[0].data = (caddr_t)shell;
 xs_create_menu_buttons("",viewer_menuBar, Viewer_display, 1);

 XtSetArg(wargs[0], XmNleftAttachment, XmATTACH_FORM);
 XtSetArg(wargs[1], XmNrightAttachment, XmATTACH_FORM);
 XtSetArg(wargs[2], XmNbottomAttachment, XmATTACH_FORM);
 XtSetArg(wargs[3], XmNeditMode, XmMULTI_LINE_EDIT);
 XtSetArg(wargs[4], XmNeditable, FALSE);
 XtSetArg(wargs[5], XmNscrollVertical, TRUE);
 XtSetArg(wargs[6], XmNscrollHorizontal, TRUE);
 XtSetArg(wargs[7], XmNvalue, text);
 textWidget = XmCreateScrolledText(viewer_form, "text_viewer", wargs, 8);

 XtSetArg(wargs[0], XmNtopAttachment, XmATTACH_WIDGET);
 XtSetArg(wargs[1], XmNtopWidget, viewer_menuBar);
 XtSetValues(XtParent(textWidget), wargs, 2);

 XtManageChild(textWidget);

 XtPopup(shell, XtGrabNone);

 return ;

}

void popdown_viewer ( Widget w , XtPointer clientdata , XtPointer calldata)
  
{
   Widget shell = ( Widget ) clientdata ;
  
   XtPopdown(shell);
   XtDestroyWidget(shell);


}
