/* T. Piper/SAIC        10/02   Removed #include <gemprm.h> */
#include <xwcmn.h>
#include <sharp95.h>
#include <gui.h>
#include <string.h>

void	statusdialog_cancel_cb();
void	statusdialog_clear_cb();

extern Widget toplevel;
static Widget status_dialog=NULL;
static Widget statusfield;
static int statuspos;

void show_status (stn,auxinfo)
/*************************************************************/
/*************************************************************/
char *stn;
char *auxinfo;
{
static Widget statuspane, statusform, statusdialog_cancel, statusdialog_clear, statusbutton_form;
XmString tstr;
XmFontList xmfl;
XFontStruct *xfstruct;
XmFontListEntry xfle;
char line[512];
Arg args[10];
Cardinal argcnt;

if( ! status_dialog)
   {
   status_dialog = XmCreateBulletinBoardDialog(toplevel, "statusdialog_panel", NULL, 0);

   tstr = XmStringCreateLocalized( "ACARS Data" );
   XtVaSetValues( status_dialog, XmNdialogTitle, tstr, NULL);
   XmStringFree(tstr);

   statuspane = XtVaCreateManagedWidget("status_pane",
                                xmPanedWindowWidgetClass,
                                status_dialog,
                                XmNsashWidth, 1,
                                XmNsashHeight, 1,
                                NULL);


   statusform = XtVaCreateWidget("statusform", xmFormWidgetClass,
                                statuspane, XmNwidth, 650, XmNheight, 400,
                                XmNfractionBase, 10, NULL );

   argcnt = 0;
   XtSetArg(args[argcnt],XmNscrollBarDisplayPolicy,XmAS_NEEDED); argcnt++;
   XtSetArg(args[argcnt],XmNscrollingPolicy,XmAUTOMATIC); argcnt++;
   XtSetArg(args[argcnt],XmNheight,400); argcnt++;
   XtSetArg(args[argcnt],XmNwidth,650); argcnt++;
   statusfield = XmCreateScrolledText(statusform,"statusform_text",
				args, argcnt);
   xfstruct = XLoadQueryFont(XtDisplay(toplevel),"-adobe-courier-bold-r-normal--14-100-100-100-m-90-iso8859-1");
   xfle = XmFontListEntryCreate(XmFONTLIST_DEFAULT_TAG, XmFONT_IS_FONT, xfstruct);
   xmfl = XmFontListAppendEntry(NULL, xfle);
   XtVaSetValues(statusfield, XmNeditMode, XmMULTI_LINE_EDIT,
                                XmNeditable, False,
				XmNfontList, xmfl,
                                NULL );

   XtManageChild ( statusfield );
   XtManageChild ( statusform );

   statusbutton_form = XtVaCreateWidget("statusbuttonform", xmFormWidgetClass,
                                statuspane, XmNwidth, 650, XmNfractionBase, 10, NULL );

   statusdialog_cancel = XtVaCreateManagedWidget ("CLOSE",
                        xmPushButtonWidgetClass, statusbutton_form,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 3,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 5,
                        NULL );
   XtAddCallback(statusdialog_cancel, XmNactivateCallback,
                        (XtCallbackProc)statusdialog_cancel_cb, NULL);

   statusdialog_clear = XtVaCreateManagedWidget ("CLEAR",
                        xmPushButtonWidgetClass, statusbutton_form,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 6,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 8,
                        NULL );
   XtAddCallback(statusdialog_clear, XmNactivateCallback,
                        (XtCallbackProc)statusdialog_clear_cb, NULL);

   XtManageChild ( statusbutton_form );

   XtManageChild ( statuspane );
   XtManageChild ( status_dialog );
   
   line[0] = '\0';
   sprintf(line,"Station      Time           Dept  Dest  Rept      PRES     HGHT     TMPC     DWPC     SPED     DRCT\n");
   XtVaSetValues(statusfield,XmNvalue,line,NULL);
   statuspos = strlen(line);
   }
else
   XtManageChild ( status_dialog );

/*sprintf(line,"%s\n\0",stn);
XmTextInsert(statusfield,statuspos,line); statuspos += strlen(line);*/

sprintf(line,"%s\n\0",auxinfo);
XmTextInsert(statusfield,statuspos,line); statuspos += strlen(line);

}

void	statusdialog_cancel_cb(Widget w, XtPointer client_data,
                               XtPointer call_data)
{
XtUnmanageChild (status_dialog);
}

void	statusdialog_clear_cb(Widget w, XtPointer client_data,
                               XtPointer call_data)
{
char line[128];
line[0] = '\0';
sprintf(line,"Station      Time           Dept  Dest  Rept      PRES     HGHT     TMPC     DWPC     SPED     DRCT\n");
XtVaSetValues(statusfield,XmNvalue,line,NULL);
statuspos = strlen(line);
}



