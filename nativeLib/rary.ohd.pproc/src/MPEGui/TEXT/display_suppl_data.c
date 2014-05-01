#include "stage3.h"
#include "read_adapt_param_RFCW.h"
#include "rfcwide_interface.h"
#include "libXs.h"
#include "stage3_globals.h"
#include "rfcwide.h"
#include "xs_create_menu_buttons.h"

static xs_menu_struct   Viewer_display[] =
{
{"Close" , popdown_viewer, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL}
};

void display_suppl_data ( Widget w , XtPointer clientdata , 
                          XtPointer calldata )
{

/*

  function to display the supplemental data found at the end of each DPA radar product
  the supplemental data is stored in the DPARadar table

  Note:  the value of the operational weather mode specified in the header is used 

  calling function: callback from Display Supplemental Dat button under Single Site Window

*/

	Widget          shell, viewer_form, viewer_shell,viewer_menuBar,textWidget;

	Arg             wargs[10];
        char text[2000], cr = '\n';

        char supplmess_string[60];
        char *badscan_string="NO HOURLY ACCUM BECAUSE RATE SCAN FLAGGED BAD";
        char *notenoughdata_string="NO HOURLY ACCUM BECAUSE NOT ENOUGH DATA IN HOUR";
        char *diskerror_string="NO SUPPLEMENTAL DATA AVAILABLE DUE TO DISK ERROR";
        char *noprecipdet_string="NO PRECIP DETECTED IN LAST HOUR";
        char *precipdet_string="PRECIP DETECTED IN LAST HOUR";

        char mode_string[10];
        char *clearair_string="clear air";
        char *precip_string="precip";
        char *mainten_string="maintenance";
        char *unknown_string="unknown";

        int ss_number = ( int ) clientdata ;

  /*------------------------------------------------*/
  /*  prepare supplemental data for display         */
  /*  opermode = 0 -- maintenance mode              */
  /*  opermode = 1 -- clear air mode                */
  /*  opermode = 2 -- precip mode                   */
  /*------------------------------------------------*/

    if(suppldata.minoff > 30) suppldata.minoff = suppldata.minoff - 60;

    strcpy(mode_string,clearair_string);
    if(suppldata.opermode == 0) strcpy(mode_string,mainten_string);
    if(suppldata.opermode == 2) strcpy(mode_string,precip_string);
    if(suppldata.opermode == -99) strcpy(mode_string,unknown_string);

    strcpy(supplmess_string,noprecipdet_string);
    if(suppldata.supplmess == 1) strcpy(supplmess_string,badscan_string);
    if(suppldata.supplmess == 2) strcpy(supplmess_string,notenoughdata_string);
    if(suppldata.supplmess == 3) strcpy(supplmess_string,diskerror_string);
    if(suppldata.supplmess == 4) strcpy(supplmess_string,precipdet_string);
    
  /*------------------------------------------------*/
  /*  create string for display in window           */
  /*------------------------------------------------*/

  sprintf(text,
    "          ***** Radar = %s *****\n"
    "%d = minutes off the top of the hour of validtime(min)%c"
    "%.2f = maximum value from data (mm)%c"
    "%.2f = maximum value from header (dBA)%c"
    "%d = number of isolated bins%c"
    "%d = number of interpolated outliers%c"
    "%d = number of outliers replaced%c"
    "%d = number of bad scans%c"
    "%d = number of hourly outliers%c"
    "%d = volume coverage pattern%c"
    "%s = operational weather mode%c"
    "%.2f = bias estimate%c"
    "%.2f = hourly mean bi-scan ratio%c"
    "%.2f = hourly mean percent area reduction (%%)%c"
    "%s = product generation time%c"
    "%c"
    "%s"
    ,
  nexrad [ ss_number ].id,
  suppldata.minoff,cr,
  suppldata.maxvald,cr,
  suppldata.maxvalh,cr,
  suppldata.nisolbin,cr,
  suppldata.noutint,cr,
  suppldata.noutrep,cr,
  suppldata.nbadscan,cr,
  suppldata.nhourout,cr,
  suppldata.volcovpat,cr,
  mode_string,cr,
  suppldata.bias,cr,
  suppldata.biscanr,cr,
  suppldata.areared,cr,
  suppldata.gentime,cr,
  cr,
  supplmess_string
  );

  /*------------------------------------------------*/
  /*  set up window for displaying parameters       */
  /*------------------------------------------------*/

 shell = XtCreatePopupShell
		(
		"Supplemental_Data_viewer",
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




}
