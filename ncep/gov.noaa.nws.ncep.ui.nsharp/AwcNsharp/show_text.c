/* T. Piper/SAIC        10/02   Removed #include <gemprm.h> */
#include <xwcmn.h>
#include <sharp95.h>
#include <gui.h>
#include <string.h>
#include "Xm/SelectioB.h"

void	textdialog_cancel_cb();
void	textdialog_save_cb();
void	update_text_values();
void 	DataFileOKCallback();

extern Widget toplevel;
static Widget text_dialog=NULL;
static Widget textfield;

void show_textCb (Widget w, XmDrawingAreaCallbackStruct *call_data)
/*************************************************************/
/*************************************************************/
{
static Widget textpane, textform, textdialog_cancel, textbutton_form, textdialog_save;
XmString tstr;
XmFontList xmfl;
XFontStruct *xfstruct;
XmFontListEntry xfle;
Arg args[10];
Cardinal argcnt;

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
   xfstruct = XLoadQueryFont(XtDisplay(toplevel),"-adobe-courier-bold-r-normal--14-100-100-100-m-90-iso8859-1");
   xfle = XmFontListEntryCreate(XmFONTLIST_DEFAULT_TAG, XmFONT_IS_FONT, xfstruct);
   xmfl = XmFontListAppendEntry(NULL, xfle);
   XtVaSetValues(textfield, XmNeditMode, XmMULTI_LINE_EDIT,
                                XmNeditable, False,
				XmNfontList, xmfl,
                                NULL );

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


void	textdialog_cancel_cb(Widget w, XtPointer client_data,
                               XtPointer call_data)
{
XtUnmanageChild (text_dialog);
}

void	textdialog_save_cb(Widget w, XtPointer client_data,
                               XtPointer call_data)
{

         char buf[160];
         char fname[160],ftype[5];
         Widget datafsb;
         Arg args[8];
         Cardinal n=0;
         XmString mask;
         XmString mask2;
         mask=XmStringCreateSimple("Enter A Data File Name...Suggested default:");
         XtSetArg(args[n],XmNselectionLabelString,mask);n++;
         strcpy(ftype,".txt");
         cvrttitletofilename(raobtitle,fname,ftype);
         if (strlen(fname)==0)
             mask2=XmStringCreateSimple("zztmp.txt");
         else 
             mask2=XmStringCreateSimple(fname);
         XtSetArg(args[n],XmNtextString,mask2);n++;
         datafsb=XmCreatePromptDialog(w,"datafsb",args,n);
         XtAddCallback(datafsb,XmNokCallback, (XtCallbackProc) DataFileOKCallback,
           (XtPointer) NULL);
         XtUnmanageChild(XmSelectionBoxGetChild(datafsb,XmDIALOG_HELP_BUTTON));
         XtManageChild(datafsb);
         /*write_file("DECDDROB");*/
}

void DataFileOKCallback(Widget fsb,XtPointer client_data,XtPointer call_data) 
       {
         XmSelectionBoxCallbackStruct* ptr;
  	 char*string;
         char rec[100];
         char buf[80];
         ptr=(XmSelectionBoxCallbackStruct*) call_data;
         XmStringGetLtoR(ptr->value,XmSTRING_DEFAULT_CHARSET,&string);
         write_file(string);
         XtFree(string);
         XtUnmanageChild(fsb);
       }
void	update_text_values()
{
int i,lastpos;
char line[256];

if(text_dialog == NULL) return;
lastpos = 0;
sprintf(line,"   LEVEL     HGHT     TEMP     DWPT     WDIR     WSPD     OMEGA\n");
XtVaSetValues(textfield,XmNvalue,line,NULL);
lastpos += strlen(line);
sprintf(line,"   (mb)      (m)      (C)      (C)      (deg)    (kts)    (mb/s)\n");
XmTextInsert(textfield,lastpos,line); lastpos+=strlen(line);

for(i=0;i<numlvl;i++)
   {
   if (sndg[i][0]==sndg[i][2])
      sprintf(line,"%8.2f %8.2f %8.2f %8.2f %8.2f %8.2f\n",
      sndg[i][1],sndg[i][2],sndg[i][3],sndg[i][4],sndg[i][5],sndg[i][6]);
   else 
      if (g_pfs_model_sel_sw)
        sprintf(line, "%8.2f %8.2f %8.2f %8.2f %8.2f %8.2f %11.6f\n",
        sndg[i][1],sndg[i][2],sndg[i][3],sndg[i][4],sndg[i][5],sndg[i][6],sndg[i][0]/100.0);        
      else        
        sprintf(line,"%8.2f %8.2f %8.2f %8.2f %8.2f %8.2f %11.6f\n",
        sndg[i][1],sndg[i][2],sndg[i][3],sndg[i][4],sndg[i][5],sndg[i][6],sndg[i][0]);
   XmTextInsert(textfield,lastpos,line); lastpos += strlen(line);
   }

}

