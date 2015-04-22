#include <stdio.h>
#include <stdlib.h>
#include "sharp95.h"

/* 
This is the command that the text gets piped to for printing
You could even set up [n]enscript here if you wanted
*/
#define PRINTCOMMAND "lp"

extern Widget toplevel;

static Widget textdialog=NULL;
static Widget savetextdialog=NULL;

extern Widget GetTopShell(Widget w);

void set_textareatext(char *bufr);
void text_ok(Widget w, XtPointer client_data, XtPointer call_data);
void text_cancel(Widget w, XtPointer client_data, XtPointer call_data);
void text_reset(Widget w, XtPointer client_data, XtPointer call_data);
void text_apply(Widget w, XtPointer client_data, XtPointer call_data);
void text_save(Widget w, XtPointer client_data, XtPointer call_data);
void text_toggled(Widget w, XtPointer client_data, XtPointer call_data);
void text_print(Widget w, XtPointer client_data, XtPointer call_data);
void text_cvf(Widget w, XtPointer client_data, XtPointer call_data);
void get_textfile(Widget w, XtPointer client_data, XtPointer call_data);
void set_textfile(Widget w, XtPointer client_data, XtPointer call_data);
void write_file2( char *filename );
int SimpleMsgBox(Widget w, char *text);

void showtextwin(Widget parent);

void errordialog(Widget parent, char *msg);

/*************************************************************
 *                                                           *
 * text window routines for Nsharp/Nflow                     *
 *                                                           *
 *                                                           *
 * Mike Kay                                                  *
 * Storm Prediction Center                                   *
 * mkay@spc.noaa.gov                                         *
 * 10/21/99                                                  *
 *************************************************************/

void showtextwin(Widget parent)
{
	Widget pane, text_w, form, button, toggle;
	Arg    args[10];
	int    n=0;
	char  *textptr=NULL;

	if (!textdialog) {

		/* Create dialog */
		textdialog = XtVaCreatePopupShell("text_win", 
				topLevelShellWidgetClass, parent, NULL);

		/* Create a pane to hold things (dialogs can only have 1 manager) */
		pane = XtVaCreateWidget("text_pane", xmPanedWindowWidgetClass, 
				textdialog, XmNsashWidth, 1, XmNsashHeight, 1, NULL);

		/* Create a form to hold the text area */
		form = XtVaCreateWidget("text_form1", xmFormWidgetClass, pane, NULL);

		/* Create text area */
		XtSetArg(args[n], XmNrows,     20); n++;
		XtSetArg(args[n], XmNcolumns,  70); n++;
		XtSetArg(args[n], XmNeditable, False); n++;
		XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;

		text_w = XmCreateScrolledText(form, "text_w", args, n);

		/* Attach text area Note the parent here */
		XtVaSetValues(XtParent(text_w), 
				XmNleftAttachment,   XmATTACH_FORM,
				XmNrightAttachment,  XmATTACH_FORM,
				XmNtopAttachment,    XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				NULL);
		XtManageChild(text_w);
		XtManageChild(form);

		/* A form to hold our toggle button */
		form = XtVaCreateWidget("text_form2", xmFormWidgetClass, pane, NULL);

		/* make a radio button for editing */
		toggle = XtVaCreateManagedWidget("Editable", 
				xmToggleButtonWidgetClass, form, NULL);

		/* Store the text area */
		XtVaSetValues(toggle, XmNuserData, text_w, NULL);

		XtVaSetValues(toggle,
				XmNleftAttachment,   XmATTACH_FORM,
				XmNtopAttachment,    XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				NULL);

		/* 
		 * We'll add the callback below since we need the Apply button
		 * to be passed to the callback
		 */

		XtManageChild(form);

		/* Create a form to hold the buttons */
		form = XtVaCreateWidget("text_form3", xmFormWidgetClass, pane, 
				XmNfractionBase, 9, NULL);

		/* Create Buttons at the bottom */
		/* OK, Apply, Cancel, Reset, Save */

		button = XtVaCreateManagedWidget("OK", xmPushButtonWidgetClass,
				form, XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment,    XmATTACH_FORM,
				XmNleftAttachment,      XmATTACH_POSITION,
				XmNleftPosition,        1,
				XmNrightAttachment,     XmATTACH_POSITION,
				XmNrightPosition,       2, NULL);
		XtAddCallback(button, XmNactivateCallback, text_ok, NULL);

		button = XtVaCreateManagedWidget("Apply", xmPushButtonWidgetClass,
				form, XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment,    XmATTACH_FORM,
				XmNleftAttachment,      XmATTACH_POSITION,
				XmNleftPosition,        2,
				XmNrightAttachment,     XmATTACH_POSITION,
				XmNrightPosition,       3, NULL);
		XtAddCallback(button, XmNactivateCallback, text_apply, NULL);
		XtSetSensitive(button, False);
		XtAddCallback(toggle, XmNvalueChangedCallback, text_toggled, button);

		button = XtVaCreateManagedWidget("Cancel", xmPushButtonWidgetClass,
				form, XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment,    XmATTACH_FORM,
				XmNleftAttachment,      XmATTACH_POSITION,
				XmNleftPosition,        3,
				XmNrightAttachment,     XmATTACH_POSITION,
				XmNrightPosition,       4, NULL);
		XtAddCallback(button, XmNactivateCallback, text_cancel, NULL);

		button = XtVaCreateManagedWidget("Reset", xmPushButtonWidgetClass,
				form, XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment,    XmATTACH_FORM,
				XmNleftAttachment,      XmATTACH_POSITION,
				XmNleftPosition,        4,
				XmNrightAttachment,     XmATTACH_POSITION,
				XmNrightPosition,       5, NULL);
		XtAddCallback(button, XmNactivateCallback, text_reset, NULL);

		button = XtVaCreateManagedWidget("Save", xmPushButtonWidgetClass,
				form, XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment,    XmATTACH_FORM,
				XmNleftAttachment,      XmATTACH_POSITION,
				XmNleftPosition,        5,
				XmNrightAttachment,     XmATTACH_POSITION,
				XmNrightPosition,       6, NULL);
		XtAddCallback(button, XmNactivateCallback, text_save, NULL);

		button = XtVaCreateManagedWidget("Print", xmPushButtonWidgetClass,
				form, XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment,    XmATTACH_FORM,
				XmNleftAttachment,      XmATTACH_POSITION,
				XmNleftPosition,        6,
				XmNrightAttachment,     XmATTACH_POSITION,
				XmNrightPosition,       7, NULL);
		XtAddCallback(button, XmNactivateCallback, text_print, NULL);

		button = XtVaCreateManagedWidget("CSV", xmPushButtonWidgetClass,
				form, XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment,    XmATTACH_FORM,
				XmNleftAttachment,      XmATTACH_POSITION,
				XmNleftPosition,        7,
				XmNrightAttachment,     XmATTACH_POSITION,
				XmNrightPosition,       8, NULL);
		XtAddCallback(button, XmNactivateCallback, text_cvf, NULL);

		XtManageChild(form);

		/* Manage the pane or else nothing will show up */
		XtManageChild(pane);

	}

	/* Populate the text window in case it is visible */
	textptr = createtextsounding();
	set_textareatext(textptr);
	free(textptr);

	/* Make it visible */
	XtPopup(textdialog, XtGrabNone);
}

void text_ok(Widget w, XtPointer client_data, XtPointer call_data)
{
	XtPopdown(GetTopShell(w));
}

void text_cancel(Widget w, XtPointer client_data, XtPointer call_data)
{
	text_ok(w, client_data, call_data);
}

void text_reset(Widget w, XtPointer client_data, XtPointer call_data)
{
}

void text_apply(Widget w, XtPointer client_data, XtPointer call_data)
{
}

void text_toggled(Widget w, XtPointer client_data, XtPointer call_data)
{
	XmToggleButtonCallbackStruct *state = 
		(XmToggleButtonCallbackStruct *)call_data;
	Widget applybutton = (Widget)client_data;
	Widget text_area;

	/* Get text area which is stored as user data in the widget */
	XtVaGetValues(w, XmNuserData, &text_area, NULL);

	if (state->set) {
		/* Turn on apply button */
		XtSetSensitive(applybutton, True);
		/* Text area needs to be set editable as well */
		XtVaSetValues(text_area, XmNeditable, True);
	}
	else {
		/* Turn off apply button */
		XtSetSensitive(applybutton, False);
		XtVaSetValues(text_area, XmNeditable, False);
	}
}

void set_textareatext(char *bufr)
{
	Widget text_w;

	/* This routine might get called before the window is created */
	if (!textdialog) return;

	/* Get the text area */
	text_w = XtNameToWidget(textdialog, "*text_w");
	if (text_w) XmTextSetString(text_w, bufr);
}

void text_print(Widget w, XtPointer client_data, XtPointer call_data)
{
	Widget  text_w;
	char   *text=NULL;
	FILE   *mypipe;

	/* Get the text area */
	text_w = XtNameToWidget(textdialog, "*text_w");
	if (text_w) {
		text = XmTextGetString(text_w);

		if (text && *text) {
			/* Send to printer Check ENV for a printer variable */

			if ((mypipe = popen(PRINTCOMMAND, "w"))) {
				fprintf(mypipe, "%s", text);
				pclose(mypipe);
			}

			XtFree(text);
		}
	}
}

void text_save(Widget w, XtPointer client_data, XtPointer call_data)
{
	Widget   child;
	char     *ptr, bufr[256], nam1[256];
	XmString namx;

	if (!savetextdialog) {
		savetextdialog = XmCreateFileSelectionDialog(GetTopShell(w), "textfile", NULL, 0); 
		/* Get rid of Help button */
		XtUnmanageChild(XmSelectionBoxGetChild(savetextdialog, XmDIALOG_HELP_BUTTON));
		XtAddCallback(savetextdialog, XmNokCallback, get_textfile, NULL);
		XtAddCallback(savetextdialog, XmNcancelCallback, (XtCallbackProc)XtUnmanageChild, NULL);

		child = XmFileSelectionBoxGetChild(savetextdialog, XmDIALOG_TEXT);
		XtVaGetValues(child, XmNvalue, &ptr, NULL);
	}
	namx = XmStringCreateLocalized(raobsavefilename);
	printf( "Setting Default Output File Name = %s\n", raobsavefilename);
	XtVaSetValues( savetextdialog, XmNdirSpec, namx);

	XtManageChild(savetextdialog);
	XtPopup(XtParent(savetextdialog), XtGrabNone);
}

void get_textfile(Widget w, XtPointer client_data, XtPointer call_data)
{
	int   i;
	char *file=NULL;
	XmFileSelectionBoxCallbackStruct *cbs =
		(XmFileSelectionBoxCallbackStruct *)call_data;

	/* Retrieve the string */
	if (!XmStringGetLtoR(cbs->value, XmFONTLIST_DEFAULT_TAG, &file)) {
		errordialog(w, "No file specified!");
		return;
	}

	/* Pop down the dialog box */
	XtPopdown(XtParent(w));
	XtUnmanageChild(savetextdialog);
	savetextdialog = NULL;	
	XtFree(savetextdialog);

	/*
	 * Make sure we have a sane filename and not just a directory path
	 */
	if (file[strlen(file)-1] == '/') { errordialog(w, "No file specified!"); }
	else {
	  /* i = save_text(file); */
	  write_file2(file);
	  if (i == 1) { errordialog(w, "Unable to properly write text file."); }
	}

	/* Need to free file */
	XtFree(file);
}

void set_textfile(Widget w, XtPointer client_data, XtPointer call_data)
{
	printf( "Popup set_textfile!!!!\n");
}

void text_cvf(Widget w, XtPointer client_data, XtPointer call_data)
        {
        char st[80], st2[80];
        strcpy(st, "NSHARP text output file was written to /tmp/NSHARP.TXT");
	strcpy(st2, "/tmp/NSHARP.TXT");
        write_file2(st2);
        SimpleMsgBox(w, st);
        }

        /*NP*/
        void write_file2( char *filename )
        /*************************************************************/
        /*  WRITE_FILE                                               */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Writes contents of sndg array into SHARP95 file.         */
        /*************************************************************/
        {
        short i, j;
        short idx[7];
        float sfctemp, sfcdwpt, sfcpres, j1, j2, ix1;
        struct _parcel pcl;
        char st[80];
        FILE *fout;

        idx[1]  = getParmIndex("PRES");
        idx[2]  = getParmIndex("HGHT");
        idx[3]  = getParmIndex("TEMP");
        idx[4]  = getParmIndex("DWPT");
        idx[5]  = getParmIndex("DRCT");
        idx[6]  = getParmIndex("SPED");

        fout = fopen( filename, "wt" );
        if (fout==NULL)
           {
           printf("Unable to write output file!\n" );
           return;
           }
        fputs( "%TITLE%\n", fout );
        fputs( raobtitle, fout );
        fputs( "\n\n", fout );
        fprintf( fout, "   LEVEL       HGHT       TEMP       DWPT       WDIR       WSPD\n");
        fprintf( fout, "-------------------------------------------------------------------\n");
        fputs( "%RAW%\n", fout );
        for(i=0; i<numlvl; i++)
           {
           for(j=1; j<=5; j++) fprintf( fout, "%8.2f,  ", sndg[i][idx[j]]);
           fprintf( fout, "%8.2f\n", sndg[i][idx[6]]);
           }
        fputs( "%END%\n\n", fout );

        if ((numlvl<4) || (!qc(i_dwpt(700, I_PRES)))) return;

        fprintf( fout, "----- Parcel Information-----\n");

        /* ----- Calculate Parcel Data ----- */
        sfctemp = lplvals.temp;
        sfcdwpt = lplvals.dwpt;
        sfcpres = lplvals.pres;

        strcpy( st, "*** " );
        strcat( st, lplvals.desc );
        strcat( st, " ***" );
        fprintf( fout, "%s\n", st);
        ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

        fprintf( fout, "LPL:  P=%.0f  T=%.0fF  Td=%.0fF\n", sfcpres, ctof(sfctemp), ctof(sfcdwpt));
        fprintf( fout, "CAPE:            %6.0f J/kg\n", pcl.bplus);
        fprintf( fout, "CINH:            %6.0f J/kg\n", pcl.bminus);

        fprintf( fout, "LI:              %6.0f C\n", pcl.li5);
        fprintf( fout, "LI(300mb):       %6.0f C\n", pcl.li3);

        fprintf( fout, "3km Cape:        %6.0f J/kg\n", pcl.cape3km);
        j1 = pcl.bplus;
        j2 = i_hght(pcl.elpres, I_PRES) - i_hght(pcl.lfcpres, I_PRES);
        fprintf( fout, "NCAPE:           %6.2f m/s2\n\n", j1/j2);

        fprintf( fout, "LCL:    %6.0fmb     %6.0fm\n", pcl.lclpres, agl(i_hght(pcl.lclpres, I_PRES)));
        fprintf( fout, "LFC:    %6.0fmb     %6.0fm\n", pcl.lfcpres, agl(i_hght(pcl.lfcpres, I_PRES)));
        fprintf( fout, "EL:     %6.0fmb     %6.0fm\n", pcl.elpres, agl(i_hght(pcl.elpres, I_PRES)));
        fprintf( fout, "MPL:    %6.0fmb     %6.0fm\n", pcl.mplpres, agl(i_hght(pcl.mplpres, I_PRES)));
        fprintf( fout, "All heights AGL\n\n" );

        fprintf( fout, "----- Moisture -----\n" );
        strcpy( st, qc2( precip_water( &ix1, -1, -1), " in", 2 ));
        fprintf( fout, "Precip Water:    %s\n", st);
        strcpy( st, qc2( mean_mixratio( &ix1, -1, -1 ), " g/Kg", 1 ));
        fprintf( fout, "Mean W:          %s\n\n", st);

        fprintf( fout, "----- Lapse Rates -----\n" );
        j1 = delta_t(&ix1);
        j2 = lapse_rate( &ix1, 700, 500);
        fprintf( fout, "700-500mb   %.0f C      %.1f C/km\n", j1, j2);

        j1 = vert_tot(&ix1);
        j2 = lapse_rate( &ix1, 850, 500);
        fprintf( fout, "850-500mb   %.0f C      %.1f C/km\n", j1, j2);

        fclose( fout );
        }


int SimpleMsgBox(Widget w, char *texta)
        {
        XmString        xtext;
        Arg             args[10];
        Widget          dialog;

        if (!w) { w = toplevel; }

        xtext = XmStringCreateLocalized(texta);
        XtSetArg (args[0], XmNmessageString, xtext);
        dialog = XmCreateInformationDialog(w, "message", args, 1);
        XtFree(xtext);
        XtUnmanageChild( XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
        XtUnmanageChild( XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
        XtVaSetValues(XtParent(dialog),
                XmNtitle, "NSHARP",
                XmNwidth, 1600,
                NULL);

        XtManageChild(dialog);
        XtPopup(XtParent(dialog), XtGrabNone);
        }
