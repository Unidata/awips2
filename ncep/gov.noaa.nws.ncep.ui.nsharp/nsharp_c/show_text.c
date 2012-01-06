#include "gui.h"
#include "sharp95.h"

void textdialog_cancel_cb ( Widget, XtPointer, XtPointer );
void textdialog_save_cb   ( Widget, XtPointer, XtPointer );

extern Widget toplevel;
static Widget text_dialog=NULL;
static Widget textfield;
XFontStruct *text_font = NULL;

/*==============================================================================*/
/* ARGSUSED */
void show_textCb ( Widget w, XtPointer clnt, 
					XmDrawingAreaCallbackStruct *call )
/********************************************************************************
*										*
**										*
*										*	
* T. Piper/SAIC          1/02   Freed xfle, xmfl, & text_font as necessary	*
* T. Piper/SAIC		 2/02	Added 2nd argument, XtPointer client_data	*
*										*
********************************************************************************/
{
static Widget textpane, textform, textdialog_cancel, textbutton_form, textdialog_save;
XmString tstr;
XmFontList xmfl;
XmFontListEntry xfle;
Arg args[10];
Cardinal argcnt;

/*----------------------------------------------------------------------------*/

if( ! text_dialog)
   {
   text_dialog = XmCreateBulletinBoardDialog(toplevel, "textdialog_panel", NULL, 0);

   tstr = XmStringCreateLocalized( "Sounding Text" );
   XtVaSetValues( text_dialog, XmNdialogTitle, tstr, NULL);
   XmStringFree(tstr);

   textpane = XtVaCreateManagedWidget("text_pane",
                                xmPanedWindowWidgetClass,
                                text_dialog,
                                XmNsashWidth, 1,
                                XmNsashHeight, 1,
                                NULL);


   textform = XtVaCreateWidget("textform", xmFormWidgetClass,
                                textpane, XmNwidth, 650, XmNheight, 800,
                                XmNfractionBase, 10, NULL );
   argcnt = 0;
   XtSetArg(args[argcnt],XmNscrollBarDisplayPolicy,XmAS_NEEDED); argcnt++;
   XtSetArg(args[argcnt],XmNscrollingPolicy,XmAUTOMATIC); argcnt++;
   XtSetArg(args[argcnt],XmNheight,800); argcnt++;
   XtSetArg(args[argcnt],XmNwidth,650); argcnt++;
   textfield = XmCreateScrolledText(textform,"textform_text",
				args, argcnt);
   if ( text_font ) XFreeFont ( XtDisplay(toplevel), text_font );
   text_font = XLoadQueryFont(XtDisplay(toplevel),"-adobe-courier-bold-r-normal--14-100-100-100-m-90-iso8859-1");
   xfle = XmFontListEntryCreate(XmFONTLIST_DEFAULT_TAG, XmFONT_IS_FONT, (XtPointer)text_font );
   xmfl = XmFontListAppendEntry(NULL, xfle);
   XtFree ( (XtPointer)xfle );
   XtVaSetValues(textfield, XmNeditMode, XmMULTI_LINE_EDIT,
                                XmNeditable, False,
				XmNfontList, xmfl,
                                NULL );
   XmFontListFree ( xmfl );
   XtManageChild ( textfield );
   XtManageChild ( textform );

   textbutton_form = XtVaCreateWidget("textbuttonform", xmFormWidgetClass,
                                textpane, XmNwidth, 650, XmNfractionBase, 10, NULL );

   textdialog_save = XtVaCreateManagedWidget ("SAVE",
                        xmPushButtonWidgetClass, textbutton_form,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 3,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 5,
                        NULL );
   XtAddCallback(textdialog_save, XmNactivateCallback,
                        (XtCallbackProc)textdialog_save_cb, NULL);

   textdialog_cancel = XtVaCreateManagedWidget ("CLOSE",
                        xmPushButtonWidgetClass, textbutton_form,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 7,
                        NULL );
   XtAddCallback(textdialog_cancel, XmNactivateCallback,
                        (XtCallbackProc)textdialog_cancel_cb, NULL);

   XtManageChild ( textbutton_form );

   XtManageChild ( textpane );
   XtManageChild ( text_dialog );
   
   }
else
   XtManageChild ( text_dialog );

update_text_values();

}

/*=====================================================================*/
/* ARGSUSED */
void textdialog_cancel_cb ( Widget w, XtPointer clnt, XtPointer call )
{
    XtUnmanageChild (text_dialog);
}

/*=====================================================================*/
/* ARGSUSED */
void textdialog_save_cb ( Widget w, XtPointer clnt, XtPointer call )
{
    write_file();
}

/*=====================================================================*/

void update_text_values ( void )
{
int i,lastpos;
char line[256];

/*---------------------------------------------------------------------*/

    if((text_dialog == NULL) || (sndgp == NULL)) return;

    lastpos = 0;
    XtVaSetValues(textfield,XmNvalue,sndgp->title, NULL);
    lastpos += (int)strlen(sndgp->title);

    sprintf(line,"\n   LEVEL     HGHT     TEMP     DWPT     WDIR     WSPD        OMEG\n");
    XmTextInsert(textfield,lastpos,line); 
    lastpos += (int)strlen(line);


    for(i=0;i<sndgp->numlev;i++) {
	sprintf(line,"%8.2f %8.2f %8.2f %8.2f %8.2f %8.2f %11.6f\n",
		sndgp->sndg[i].pres,sndgp->sndg[i].hght,sndgp->sndg[i].temp,sndgp->sndg[i].dwpt,
		sndgp->sndg[i].drct,sndgp->sndg[i].sped,sndgp->sndg[i].omega);
	XmTextInsert(textfield,lastpos,line); lastpos += (int)strlen(line);
    }
}
