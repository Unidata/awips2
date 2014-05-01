
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include <Xm/DrawnB.h>
#include <Xm/PushB.h>
#include "Xtools.h"
#include "TechInfo.h"


/*
	Widget definitions.	
*/
Widget techDS    = (Widget) NULL;
Widget techFM    = (Widget) NULL;
Widget tchAppLbl = (Widget) NULL;
Widget tchAppInf = (Widget) NULL;
Widget tchVerLbl = (Widget) NULL;
Widget tchVerInf = (Widget) NULL;
Widget tchDatLbl = (Widget) NULL;
Widget tchDatInf = (Widget) NULL;
Widget techLbl   = (Widget) NULL;
Widget nwsLbl    = (Widget) NULL;
Widget strLbl    = (Widget) NULL;
Widget zipLbl    = (Widget) NULL;
Widget foneLbl   = (Widget) NULL;
Widget techSP    = (Widget) NULL;
Widget tchclsPB  = (Widget) NULL;


/*
	Static data.
*/
static int	tech_shown = False;



void	techinfo_show(Widget w, char *app, char *ver, char *date)
{
	Widget		children[64];
	Arg		arg[64];
	Atom		wmAtom;
	int		ac;
	
	static int	created = False;
	
	
	
	if (! created)
	{
		/*
			Set resources and create dialog.
		*/
		ac = 0;		
		XtSetArg(arg[ac], XmNtitle, "About"); ac++;
		XtSetArg(arg[ac], XmNdeleteResponse, XmDO_NOTHING); ac++;
		techDS    = XmCreateDialogShell(GetTopShell(w), "techDS", arg, ac);
		
		
		/*
			Set resources and create form.
		*/
		ac = 0;
		XtSetArg(arg[ac], XmNwidth, 520);  ac++;
		XtSetArg(arg[ac], XmNheight, 330); ac++;
		XtSetArg(arg[ac], XmNfractionBase, 100); ac++;
		XtSetArg(arg[ac], XmNautoUnmanage, FALSE); ac++;
		techFM    = XmCreateForm(techDS, "techFM", arg, ac);
		
		
		/*
			Create label widgets.
		*/
		ac = 0;
		tchAppLbl = XmCreateLabel(techFM, "tchAppLbl", arg, ac);
		tchAppInf = XmCreateLabel(techFM, "tchAppInf", arg, ac);
		tchVerLbl = XmCreateLabel(techFM, "tchVerLbl", arg, ac);
		tchVerInf = XmCreateLabel(techFM, "tchVerInf", arg, ac);
		tchDatLbl = XmCreateLabel(techFM, "tchDatLbl", arg, ac);
		tchDatInf = XmCreateLabel(techFM, "tchDatInf", arg, ac);
		techLbl   = XmCreateLabel(techFM, "techLbl", arg, ac);
		nwsLbl    = XmCreateLabel(techFM, "nwsLbl",  arg, ac);
		strLbl    = XmCreateLabel(techFM, "strLbl",  arg, ac);
		zipLbl    = XmCreateLabel(techFM, "zipLbl",  arg, ac);
		foneLbl   = XmCreateLabel(techFM, "foneLbl",  arg, ac);
		techSP    = XmCreateSeparator(techFM, "techSP", arg, ac);
		tchclsPB  = XmCreatePushButton(techFM, "tclosePB", arg, ac);
	
	
		/*
			Set resources for all label children.
		*/
		ac = 0;
		XtSetArg(arg[ac], XmNtopAttachment,  XmATTACH_FORM);  ac++;
		XtSetArg(arg[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
		XtSetArg(arg[ac], XmNtopOffset,      20);  ac++;
		XtSetArg(arg[ac], XmNleftOffset,     45); ac++;
		XtSetValues(tchAppLbl, arg, ac);
		SetLabel(tchAppLbl, "Application:");

		
		ac = 0;
		XtSetArg(arg[ac], XmNtopAttachment,  XmATTACH_FORM); ac++;
		XtSetArg(arg[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
		XtSetArg(arg[ac], XmNleftWidget,     tchAppLbl); ac++;
		XtSetArg(arg[ac], XmNtopOffset,      20); ac++;
		XtSetArg(arg[ac], XmNleftOffset,     20); ac++;
		XtSetArg(arg[ac], XmNalignment,      XmALIGNMENT_BEGINNING); ac++;
		XtSetArg(arg[ac], XmNwidth,          200); ac++;
		XtSetValues(tchAppInf, arg, ac);
		
		
		ac = 0;
		XtSetArg(arg[ac], XmNtopAttachment,   XmATTACH_WIDGET); ac++;
		XtSetArg(arg[ac], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
		XtSetArg(arg[ac], XmNtopWidget,       tchAppLbl); ac++;
		XtSetArg(arg[ac], XmNrightWidget,     tchAppLbl); ac++;
		XtSetArg(arg[ac], XmNtopOffset,       5); ac++;
		XtSetArg(arg[ac], XmNleftOffset,      0); ac++;
		XtSetValues(tchVerLbl, arg, ac);
		SetLabel(tchVerLbl, "Version:");
		
		
		ac = 0;
		XtSetArg(arg[ac], XmNtopAttachment,  XmATTACH_WIDGET); ac++;
		XtSetArg(arg[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
		XtSetArg(arg[ac], XmNtopWidget,      tchAppInf); ac++;
		XtSetArg(arg[ac], XmNleftWidget,     tchAppInf); ac++;
		XtSetArg(arg[ac], XmNtopOffset,      5); ac++;
		XtSetArg(arg[ac], XmNleftOffset,     0); ac++;
		XtSetArg(arg[ac], XmNalignment,      XmALIGNMENT_BEGINNING); ac++;
		XtSetArg(arg[ac], XmNwidth,          200); ac++;
		XtSetValues(tchVerInf, arg, ac);
		
		
		ac = 0;
		XtSetArg(arg[ac], XmNtopAttachment,   XmATTACH_WIDGET); ac++;
		XtSetArg(arg[ac], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
		XtSetArg(arg[ac], XmNtopWidget,       tchVerLbl); ac++;
		XtSetArg(arg[ac], XmNrightWidget,     tchVerLbl); ac++;
		XtSetArg(arg[ac], XmNtopOffset,       5); ac++;
		XtSetArg(arg[ac], XmNrightOffset,     0); ac++;
		XtSetValues(tchDatLbl, arg, ac);
		SetLabel(tchDatLbl, "Date:");
		

		ac = 0;
		XtSetArg(arg[ac], XmNtopAttachment,  XmATTACH_WIDGET); ac++;
		XtSetArg(arg[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
		XtSetArg(arg[ac], XmNtopWidget,      tchVerInf); ac++;
		XtSetArg(arg[ac], XmNleftWidget,     tchVerInf); ac++;
		XtSetArg(arg[ac], XmNtopOffset,      5); ac++;
		XtSetArg(arg[ac], XmNleftOffset,     0); ac++;
		XtSetArg(arg[ac], XmNalignment,      XmALIGNMENT_BEGINNING); ac++;
		XtSetArg(arg[ac], XmNwidth,          200); ac++;
		XtSetValues(tchDatInf, arg, ac);


		ac = 0;
		XtSetArg(arg[ac], XmNtopAttachment,  XmATTACH_WIDGET); ac++;
		XtSetArg(arg[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
		XtSetArg(arg[ac], XmNtopWidget,      tchDatLbl); ac++;
		XtSetArg(arg[ac], XmNtopOffset,      5); ac++;
		XtSetArg(arg[ac], XmNleftOffset,     20); ac++;
		XtSetValues(techLbl, arg, ac);
		SetLabel(techLbl, "Developed by:     National Weather Service");
			

		ac = 0;
		XtSetArg(arg[ac], XmNtopAttachment,  XmATTACH_WIDGET); ac++;
		XtSetArg(arg[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
		XtSetArg(arg[ac], XmNtopWidget,      techLbl); ac++;
		XtSetArg(arg[ac], XmNleftWidget,     techLbl); ac++;
		XtSetArg(arg[ac], XmNtopOffset,      0); ac++;
		XtSetArg(arg[ac], XmNleftOffset,     140); ac++;
		XtSetArg(arg[ac], XmNalignment,      XmALIGNMENT_BEGINNING); ac++;
		XtSetArg(arg[ac], XmNwidth,          200); ac++;
		XtSetValues(nwsLbl, arg, ac);
		SetLabel(nwsLbl, "Office of Hydrologic Development");
		
		
		ac = 0;
		XtSetArg(arg[ac], XmNtopAttachment,  XmATTACH_WIDGET); ac++;
		XtSetArg(arg[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
		XtSetArg(arg[ac], XmNtopWidget,      nwsLbl); ac++;
		XtSetArg(arg[ac], XmNleftWidget,     nwsLbl); ac++;
		XtSetArg(arg[ac], XmNtopOffset,      0); ac++;
		XtSetArg(arg[ac], XmNleftOffset,     0); ac++;
		XtSetArg(arg[ac], XmNalignment,      XmALIGNMENT_BEGINNING); ac++;
		XtSetArg(arg[ac], XmNwidth,          200); ac++;
		XtSetValues(strLbl, arg, ac);
		SetLabel(strLbl, "Hydrology Laboratory");


		ac = 0;
		XtSetArg(arg[ac], XmNtopAttachment,  XmATTACH_WIDGET); ac++;
		XtSetArg(arg[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
		XtSetArg(arg[ac], XmNtopWidget,      strLbl); ac++;
		XtSetArg(arg[ac], XmNleftWidget,     strLbl); ac++;
		XtSetArg(arg[ac], XmNtopOffset,      0); ac++;
		XtSetArg(arg[ac], XmNleftOffset,     0); ac++;
		XtSetArg(arg[ac], XmNalignment,      XmALIGNMENT_BEGINNING); ac++;
		XtSetArg(arg[ac], XmNwidth,          200); ac++;
		XtSetValues(zipLbl, arg, ac);
		SetLabel(zipLbl, "");


		ac = 0;
		XtSetArg(arg[ac], XmNtopAttachment,  XmATTACH_WIDGET); ac++;
		XtSetArg(arg[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
		XtSetArg(arg[ac], XmNtopWidget,      zipLbl); ac++;
		XtSetArg(arg[ac], XmNtopOffset,	     0); ac++;
		XtSetArg(arg[ac], XmNleftOffset,     20); ac++;
		XtSetArg(arg[ac], XmNalignment,      XmALIGNMENT_BEGINNING); ac++;
		XtSetArg(arg[ac], XmNwidth,          200); ac++;
		XtSetValues(foneLbl, arg, ac);
		SetLabel(foneLbl, "");
		
		
		ac = 0;
		XtSetArg(arg[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
		XtSetArg(arg[ac], XmNleftAttachment,   XmATTACH_FORM); ac++;
		XtSetArg(arg[ac], XmNrightAttachment,  XmATTACH_FORM); ac++;
		XtSetArg(arg[ac], XmNbottomOffset,     70); ac++;
		XtSetArg(arg[ac], XmNleftOffset,       0); ac++;
		XtSetArg(arg[ac], XmNrightOffset,      0); ac++;
		XtSetValues(techSP, arg, ac);
		

		ac = 0;
		SetLabel(tchclsPB, "Ok");	
		XtSetArg(arg[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
		XtSetArg(arg[ac], XmNleftAttachment,   XmATTACH_POSITION); ac++;
		XtSetArg(arg[ac], XmNbottomOffset,     20); ac++;
		XtSetArg(arg[ac], XmNleftPosition,     41); ac++;
		XtSetArg(arg[ac], XmNwidth,            85); ac++;
		XtSetArg(arg[ac], XmNheight,           35); ac++;
		XtSetValues(tchclsPB, arg, ac);
	
		
	
		/*
			Add children to widget list,
			and subsequently manage them.
		*/
		ac = 0;
		children[ac++] = tchAppLbl;
		children[ac++] = tchAppInf;
		children[ac++] = tchVerLbl;
		children[ac++] = tchVerInf;
		children[ac++] = tchDatLbl;
		children[ac++] = tchDatInf;
		children[ac++] = techLbl;
		children[ac++] = nwsLbl;
		children[ac++] = strLbl;
		children[ac++] = zipLbl;
		children[ac++] = foneLbl;
		children[ac++] = techSP; 
		children[ac++] = tchclsPB;
		XtManageChildren(children, ac);
		XtManageChild(techFM);
		
		

		/*
			Setup callback functions.
		*/
		wmAtom = XmInternAtom(XtDisplay(techDS), "WM_DELETE_WINDOW", False);
		XmAddWMProtocolCallback(techDS, wmAtom, techinfo_close, NULL);
		XtAddCallback(tchclsPB, XmNactivateCallback, techinfo_close, NULL);
		created = True;
	}

	
	if (! tech_shown)
	{
		SetLabel(tchAppInf, app);
		SetLabel(tchVerInf, ver);
		SetLabel(tchDatInf, date);
		 
		XtManageChild(techFM);
		XtManageChild(techDS);
		tech_shown = True;
	}	
		
	return;
}


/*
	Close callback function.
*/
void	techinfo_close(Widget w, XmAnyCallbackStruct *cbs)
{
	XtUnmanageChild(techDS);
	tech_shown = False;
	return;
}
