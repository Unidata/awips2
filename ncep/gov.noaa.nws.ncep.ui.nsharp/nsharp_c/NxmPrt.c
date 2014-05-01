#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"


#define MAXCOPY 	50
#define SPOOL_TBL	"printer.tbl"

typedef struct {
	char	*listname;	/* name in menu 	   */
	char	*prtname;	/* in command		   */
	char	color;		/* color availability	   */
	char	size;		/* paper size availability */
}	prtRec_t ;	    /* printer attribute structure */

typedef struct	{
	int	 n_prt; 	/* total # of printers	   */
	int	 select;	/* selected printer	   */
	prtRec_t *rec;		/* printer attributes	   */
}	prtStr_t;

static	prtStr_t  _prtStr;	/* printer info. structure     */

static	void	(*_applPrtFunc)( void );   /* pointer to application
					print function		*/
static	char	_applWin[20];	     /* application window name */

					/* global widgets	*/
static	Widget	_prtW, _copyTxtW;
static	Widget	_colorWid[3];
static	Widget	_fileTxtW, _framePrt, _frameFil;
static	Widget	_sizeWid1, _sizeWid2;
static	Widget	_oriW1, _oriW2;
static	Widget	_OnePgW, _AllPgW;

static	int _psOnlyFlg = FALSE;	/* printer/PS only flag 	*/
static	int _pageMode;		/* current/all page flag	*/
#ifndef NSHARP
static	int _oriMode;		/* paper orientation flag	*/
static	int _sizeMode;		/* paper size mode		*/
static	int _colorMode; 	/* color mode			*/
static  Widget _txtPrtWin;
static  char _currTxtFileNm[64];  /* current text file to be printed */
static  char _currTxtPrtName[64]; /* current text printer name  */
#endif
static	int _curPrtInx; 	/* index to current printer	*/
static	char _curPrtName[256];	/* name of curent printer	*/
static	int _printFlg;		/* print in process flag	*/
static	int _prtStopFlg;	/* print process stop flag	*/

/*
 *  Private functions
 */
void _prtReadTbl (	void );
void _prtPsSelect (     Widget	parent );
void _printerName (     Widget  parent );
void _psFileName (      Widget  parent );
void _prtNumSelect (    Widget  parent );
void _prtColorSelect (  Widget  parent );
void _prtSizeSelect (   Widget  parent );
void _prtOrientSelect ( Widget  parent );
void _prtPageSelect (   Widget  parent );
void _prtCtlBtn (       Widget  parent );
void _prtSetDefault (   int     which );
void _prtCommand (      char    *cmd );
void _defaultPrinter ( 	void );
void _prtPsSelectCb ( 	Widget, long, XtPointer );
void _prtSizeSelectCb ( Widget, long, XmToggleButtonCallbackStruct *call );
void _prtOrientSelectCb(Widget, long, XmToggleButtonCallbackStruct *call );
void _prtColorSelectCb (Widget, long, XmToggleButtonCallbackStruct *call );
void _prtPageSelectCb ( Widget, long, XtPointer );
void _prtNameCb (       Widget, long, XtPointer );
void _prtOkCb (         Widget, XtPointer, XtPointer );
void _prtCancelCb (     Widget, XtPointer, XtPointer );
void _prtHelpCb (       Widget, XtPointer, XtPointer );
void _printToPs (       char  *ps_file );
int  _prtSetPSDev (     char  *ps_file );
int  _prtSetXWPdev (    char  *wname );
void _prtSwitchMode (	void );
void NxmPrt_txtPrtCtlBtnCb ( Widget wdgt, XtPointer clnt, XtPointer call );
void NxmPrt_txtPrtDestroyCb( Widget wdgt, XtPointer clnt, XtPointer call );


/************************************************************************
 * NxmPrt.c								*
 *									*
 * This module contains functions that creates the print panel. 	*
 *									*
 * CONTENTS:								*
 *									*
 * NxmPrt_create()	create the print popup module.			*
 * NxmPrt_prtWPopup()	pops up the print widget panel. 		*
 * NxmPrt_isPrtFlgSet() query printing status flag			*
 * NxmPrt_isPgFlgSet()	query printing page status flag 		*
 * NxmPrt_stopPrt()	set the stop printing flag			*
 *									*
 * _prtReadTbl()	read the printer setup table			*
 * _prtPsSelect()	create the printer or PS only select box.	*
 * _printerName()	create the printer choice option menu.		*
 * _psFileName()	create PS file name input field.		*
 * _prtNumSelect()	create number of copy field			*
 * _prtColorSelect()	create the printer color selection box		*
 * _prtSizeSelect()	create the paper size slection box		*
 * _prtOrientSelect()	create the orientation slection box		*
 * _prtPageSelect()	create the one page/all page selection area	*
 * _prtCtlBtn() 	create the control buttons for the popup	*
 * _prtSetDefault()	set up the default for the popup window 	*
 * _prtCommand()	generate the printing command			*
 * _defaultPrinter()	set the default printer attributes		*
 *									*
 * _prtPsSelectCb()	printer/PS_only toggles callback function	*
 * _prtOrientSelectCb() orientation choice toggles callback function	*
 * _prtColorSelectCb()	color choice toggles callback function		*
 * _prtSizeSelectCb()	paper size toggles callback function		*
 * _prtPageSelectCb()	page number choice callback function		*
 * _prtNameCb() 	printer name option menu callback function	*
 * _prtOkCb()		ok button callback function			*
 * _prtCancelCb()	cancel button callback function 		*
 * _prtHelpCb() 	help button callback function			*
 * NxmPrt_txtPrtCtlBtnCb()  callback for ctl btns of print window	*
 * NxmPrt_txtPrtDestroyCb() callback before destroying the widget	*
 *									*
 * _printToPs() 	print to a PostScript file			*
 * _prtSetPSDev()	set device to PS driver 			*
 * _prtSetXWPdev()	set device to XWP driver			*
 * _prtSwitchMode()	switch between printer and PS mode		*
 ***********************************************************************/


/*=====================================================================*/

Widget NxmPrt_create ( char *wname, Widget parent, void (*print_func)(void) )
/************************************************************************
 * NxmPrt_create							*
 *									*
 * This function create the print popup panel				*
 *									*
 * Widget NxmPrt_create( wname, parent, print_func )			*
 *									*
 * Input parameters:							*
 *	wname		char*	 window name				*
 *	parent		Widget	 parent widget ID			*
 *	*print_func()	void	application printing function		*
 *									*
 * Output parameters:							*
 *	NxmPrt_create	Widget	 ID of the print popup panel		*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/96					*
 * 	I. Durham/GSC		05/98	Changed underscore decl. to	*
 *					an include			*
 *	H. Zeng/SAIC		05/04	do more initialization		*
 ***********************************************************************/
{
Widget		pane, rc;
XmString	str;

/*---------------------------------------------------------------------*/

	strcpy(_applWin, wname);

	_prtW = XmCreateBulletinBoardDialog(parent,
		"prtPanel", NULL, 0);

	str = XmStringCreateLocalized("Printer Setup");

	XtVaSetValues(_prtW,
		XmNnoResize,		True,
		XmNdialogTitle, 	str,
		NULL);

	XmStringFree(str);

	pane = XtVaCreateManagedWidget("prtpane",
		xmPanedWindowWidgetClass,	_prtW,
		XmNsashWidth,			1,
		XmNsashHeight,			1,
		NULL);

	_prtPsSelect(pane);
	_prtReadTbl();
	_printerName(pane);
	_psFileName(pane);
	_prtNumSelect(pane);

	rc = XtVaCreateManagedWidget("SelectRc",
		xmRowColumnWidgetClass, pane,
		XmNorientation, 	XmHORIZONTAL,
		XmNnumColumns,		1,
		NULL);

	_prtColorSelect(rc);
	_prtSizeSelect(rc);
	_prtOrientSelect(rc);

	_prtPageSelect(pane);

	_prtCtlBtn(pane) ;

	_prtNameCb(NULL, 0, NULL);
	XmToggleButtonSetState(_OnePgW, True, True);

	_applPrtFunc = print_func;

        /*
         * Initialize the current text printer name.
         */
#ifndef NSHARP
	_currTxtPrtName[0] = '\0';
#endif

	XtRealizeWidget(_prtW);
	return(_prtW);
}

/*=====================================================================*/

void NxmPrt_prtWPopup ( void )
/************************************************************************
 * NxmPrt_prtWPopup							*
 *									*
 * This function pops up the print panel				*
 *									*
 * void NxmPrt_prtWPopup()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *  S. Wang/GSC		05/96	initial coding				*
 *  J. Wu/SAIC		01/04	call _prtSwitchMode()			*
 ***********************************************************************/
{
    XtManageChild(_prtW);
    _prtSwitchMode ();
}

/*=====================================================================*/

int NxmPrt_isPrtFlgSet ( void )
/************************************************************************
 * NxmPrt_isPrtFlgSet							*
 *									*
 * This function querys the print_in_process flag			*
 *									*
 * int NxmPrt_isPrtFlgSet()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 * Return parameters:							*
 * NxmPrt_isPrtFlgSet	int 	=1 print flag set, =0 print flag not set*
 *									*
 ** Log:								*
 *  S. Wang/GSC		05/97	initial coding				*
 ***********************************************************************/
{
    return(_printFlg);
}

/*=====================================================================*/

int NxmPrt_isPgFlgSet ( void )
/************************************************************************
 * NxmPrt_isPgFlgSet							*
 *									*
 * This function querys the page flag status				*
 *									*
 * int NxmPrt_isPgFlgSet()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 * Return parameters:							*
 * NxmPrt_isPgFlgSet	int 	=1 page flag set, =0 page flag not set 	*
 *									*
 ** Log:								*
 *  S. Wang/GSC		05/97	initial coding				*
 ***********************************************************************/
{
    return(_pageMode);
}

/*=====================================================================*/

void NxmPrt_stopPrt ( void )
/************************************************************************
 * NxmPrt_stopPrt							*
 *									*
 * This function sets the stop printing flag to true			*
 *									*
 * void NxmPrt_stopPrt()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/97					*
 ***********************************************************************/
{
    _prtStopFlg = -1;
}

/*=====================================================================*/

void _prtReadTbl ( void )
/************************************************************************
 * _prtReadTbl								*
 *									*
 * This function reads the printer setup table. If loading fails	*
 * the application will use only the default printer.			*
 *									*
 * void _prtReadTbl()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	C. Lin/EAI		12/92					*
 *	S. Wang/GSC		05/96	modify and restructured 	*
 *	S. Schotz/NCEP		03/97	removed win_name from call	*
 *	S. Wang/GSC		05/97	rewrite 			*
 ***********************************************************************/
{
int	i, n, iret, ier, open_flg;
FILE	*fp;
char	color, size;
char	def_dir[20];
char	listname[256], prtname[256], buffer[256];

/*---------------------------------------------------------------------*/

	iret = 0;
	open_flg = 1;
	n = 1;

	strcpy( def_dir, "config" );
	fp = cfl_tbop(SPOOL_TBL, def_dir, &ier );

	if ( fp == NULL || iret != 0 ) {
		printf(" Unable to open %s table \n", SPOOL_TBL );
		open_flg = 0;
	}

/*
 * read all the records in the table
 */
	if ( open_flg == 1 ) {
	    while ( !feof(fp) ) {
		cfl_trln(fp, 256, buffer, &ier);
		if ( ier == 0 )
		    n++;
	    }

	    if ( n>1 )
		rewind(fp);
	}

	_prtStr.n_prt = n;
	_prtStr.rec = (prtRec_t *)malloc( (size_t)n*sizeof(prtRec_t) );
	_defaultPrinter();

	if ( open_flg == 1 ) {

/*
 * starts reading in printer info
 */
	    for( i=1; i<n; i++ ) {
		cfl_trln( fp, 256, buffer, &ier );

		if ( ier == 0 ) {
		    sscanf( buffer, "%s %s %c %c", listname,
					prtname, &color, &size );
		    _prtStr.rec[i].listname =
				(char *)malloc( strlen(listname)+1 );
		    strcpy(_prtStr.rec[i].listname, listname);
		    _prtStr.rec[i].prtname =
				(char *)malloc( strlen(prtname)+1 );
		    strcpy(_prtStr.rec[i].prtname, prtname);
		    _prtStr.rec[i].color = color;
		    _prtStr.rec[i].size = size;
		}
	    }

	    fclose(fp);
	}

	return;
}

/*=====================================================================*/

void _prtPsSelect ( Widget parent )
/************************************************************************
 * _prtPsSelect 							*
 *									*
 * This function create the printer or PS only selection box.		*
 *									*
 * void _prtPsSelect(parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget		parent widget ID		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/96					*
 ***********************************************************************/
{
Widget	radio_box, rowcol;
XmString	one, two;

/*---------------------------------------------------------------------*/

	rowcol = XtVaCreateManagedWidget("Rc",
		xmRowColumnWidgetClass, parent,
		XmNorientation, 	XmHORIZONTAL,
		XmNnumColumns,		1,
		NULL);

	one = XmStringCreateLocalized("Printer");
	two = XmStringCreateLocalized("PostScript File Only");

	radio_box = XmVaCreateSimpleRadioBox(rowcol,
		"radio", 0, (XtCallbackProc)_prtPsSelectCb,
		XmVaRADIOBUTTON, one, NULL, NULL, NULL,
		XmVaRADIOBUTTON, two, NULL, NULL, NULL,
		XmNorientation, XmHORIZONTAL,
		NULL);

	XmStringFree(one);
	XmStringFree(two);

	XtManageChild(radio_box);

	return;
}


/*=====================================================================*/

void _printerName ( Widget parent )
/************************************************************************
 * _printerName 							*
 *									*
 * This function create the printer choice area.			*
 *									*
 * void _printerName(parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget		parent widget ID		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/96					*
 * J. Wu/SAIC		01/04	adjust the GUI				*
 ***********************************************************************/
{
long		ii;
Widget		bb, rowcol;
Widget		option_menu, opt_w, option;
XmString	str;
Cardinal	n;
Arg		args[10];

/*---------------------------------------------------------------------*/

	_framePrt = XtVaCreateManagedWidget("frmPrt",
		xmFrameWidgetClass,	parent,
		NULL);

	bb = XtVaCreateManagedWidget("bb",
		xmBulletinBoardWidgetClass, _framePrt,
		XmNmarginHeight,	8,
		NULL);

	rowcol = XtVaCreateManagedWidget("rc2",
		xmRowColumnWidgetClass, bb,
		XmNorientation, 	XmHORIZONTAL,
		XmNnumColumns,		1,
		XmNx,			14,
		NULL);

	option_menu = XmCreatePulldownMenu(rowcol, "opt_menu", NULL, 0);
	str = XmStringCreateLocalized("Printer Name:"); /* no TAB here */

	n = 0;
	XtSetArg(args[n], XmNsubMenuId, option_menu); n++;
	XtSetArg(args[n], XmNlabelString, str); n++;
	XtSetArg(args[n], XmNx, 30); n++;
	XtSetArg(args[n], XmNspacing, 40); n++;

	option = XmCreateOptionMenu(rowcol, "Printer", args, n);

	XmStringFree(str);

	for ( ii = 0; ii <_prtStr.n_prt; ii++ ) {
		opt_w = XtVaCreateManagedWidget( _prtStr.rec[ii].listname,
			xmPushButtonWidgetClass,	option_menu,
			XmNheight,			20,
			XmNwidth,			80,
			NULL );

	       XtAddCallback( opt_w, XmNactivateCallback,
			(XtCallbackProc)_prtNameCb, (XtPointer)ii );
	}

	XtManageChild(option);
	return;
}


/*=====================================================================*/

void _psFileName ( Widget parent )
/************************************************************************
 * _psFileName								*
 *									*
 * This function create PS file name input area.			*
 *									*
 * void _psFileName(parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget		parent widget ID		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/96					*
 * J. Wu/SAIC		01/04	adjust the GUI				*
 ***********************************************************************/
{
Widget	bb;
char	label[100];
XmString  str;

/*---------------------------------------------------------------------*/

	_frameFil = XtVaCreateManagedWidget("frmFil",
		xmFrameWidgetClass,	parent,
		NULL);

	bb = XtVaCreateManagedWidget("bb",
		xmBulletinBoardWidgetClass, _frameFil,
		NULL);

	strcpy(label, "Print to File:");

	str = XmStringCreateLocalized(label);

	XtVaCreateManagedWidget("fileLabel",
		xmLabelWidgetClass,	bb,
		XmNlabelString, 	str,
		XmNx,			20,
		XmNy,			14,
		NULL);

	XmStringFree(str);

	_fileTxtW = XtVaCreateManagedWidget("fileTxt",
		xmTextFieldWidgetClass, bb,
		XmNx,			160,
		XmNy,			6,
		XmNcolumns,		16,
		NULL);

	XmTextSetString(_fileTxtW, "ps.plt");

	return;
}


/*=====================================================================*/

void _prtNumSelect ( Widget parent )
/************************************************************************
 * _prtNumSelect							*
 *									*
 * This function create the number of copy input area			*
 *									*
 * void _prtNumSelect(parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget		parent widget ID		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/96					*
 * J. Wu/SAIC		01/04	adjust the GUI				*
 ***********************************************************************/
{
Widget		frm, bb;
char		copy_lb[100];
XmString	str;
/*---------------------------------------------------------------------*/

	frm = XtVaCreateManagedWidget("frmNum",
		xmFrameWidgetClass,	parent,
		NULL);

	bb = XtVaCreateManagedWidget("bb",
		xmBulletinBoardWidgetClass, frm,
		NULL);

	strcpy(copy_lb, "Number of Copies:");
	str = XmStringCreateLocalized(copy_lb);

	XtVaCreateManagedWidget("copylabel",
			xmLabelWidgetClass,	bb,
			XmNlabelString, 	str,
			XmNx,			20,
			XmNy,			14,
			NULL);

	XmStringFree(str);

	_copyTxtW = XtVaCreateManagedWidget("copyTxt",
		xmTextFieldWidgetClass, bb,
		XmNx,			160,
		XmNy,			6,
		XmNcolumns,		3,
		NULL);

	XmTextSetString(_copyTxtW, "1");

	return;
}

/*=====================================================================*/

void _prtColorSelect ( Widget parent )
/************************************************************************
 * _prtColorSelect							*
 *									*
 * This function create the color choice area				*
 *									*
 * void _prtColorSelect(parent) 					*
 *									*
 * Input parameters:							*
 *	parent		Widget		parent widget ID		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC	05/96						*
 *	G. Krueger/EAI	10/97  NxmFrameLabel->NxmLabel_createFrameLbl	*
 ***********************************************************************/
{
long		ii;
Widget		radio_box, pane_fr;
Widget		frame_co;
char		*color_list[]  = { "Gray", "Black/White", "Color" };

/*---------------------------------------------------------------------*/

	frame_co = XtVaCreateManagedWidget("frameCol",
		 xmFrameWidgetClass, parent,
		 NULL);

	pane_fr = XtVaCreateManagedWidget( "pane_fr",
		xmPanedWindowWidgetClass, frame_co,
		XmNsashWidth,		  1,
		XmNsashHeight,		  1,
		XmNx,			  10,
		NULL );

	NxmLabel_createFrameLbl("Color", pane_fr, frame_co );

	radio_box = XmCreateRadioBox(pane_fr, "colorGray", NULL, 0);

	for (ii=0;ii<(long)XtNumber(color_list);ii++) {
	    _colorWid[ii]  = XtVaCreateManagedWidget(color_list[ii],
		xmToggleButtonGadgetClass, radio_box,
		NULL);

	    XtAddCallback(_colorWid[ii], XmNvalueChangedCallback,
		(XtCallbackProc)_prtColorSelectCb, (XtPointer)ii);
	}

	XtManageChild(radio_box);

	return;
}


/*=====================================================================*/

void _prtSizeSelect ( Widget parent )
/************************************************************************
 * _prtSizeSelect							*
 *									*
 * This function create the paper size selection area			*
 *									*
 * void _prtSizeSelect(parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget		parent widget ID		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC	05/96						*
 *	G. Krueger/EAI	10/97  NxmFrameLabel->NxmLabel_createFrameLbl	*
 ***********************************************************************/
{
Widget		pane_fr, radio_box;
Widget		size_fm;

/*---------------------------------------------------------------------*/

	size_fm = XtVaCreateManagedWidget("frameSize",
		 xmFrameWidgetClass,	parent,
		 NULL);

	pane_fr = XtVaCreateManagedWidget( "pane_fr",
		xmPanedWindowWidgetClass, size_fm,
		XmNsashWidth,		  1,
		XmNsashHeight,		  1,
		XmNx,			  10,
		NULL );

	NxmLabel_createFrameLbl("Paper_Size", pane_fr, size_fm );

	radio_box = XmCreateRadioBox( pane_fr, "size", NULL, 0 );

	_sizeWid1 = XtVaCreateManagedWidget("8.5x11",
		xmToggleButtonGadgetClass, radio_box,
		NULL);
	XtAddCallback(_sizeWid1, XmNvalueChangedCallback,
		(XtCallbackProc)_prtSizeSelectCb, (XtPointer)1);
	_sizeWid2  = XtVaCreateManagedWidget("11x17",
		xmToggleButtonGadgetClass, radio_box,
		NULL);
	XtAddCallback(_sizeWid2, XmNvalueChangedCallback,
		(XtCallbackProc)_prtSizeSelectCb, (XtPointer)2);

	XtManageChild(radio_box);

	return;
}

/*=====================================================================*/

void _prtOrientSelect ( Widget parent )
/************************************************************************
 * _prtOrientSelect							*
 *									*
 * This function create the oriention selection area			*
 *									*
 * void _prtOrientSelect(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget		parent widget ID		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC	05/96						*
 *	G. Krueger/EAI	10/97  NxmFrameLabel->NxmLabel_createFrameLbl	*
 ***********************************************************************/
{
Widget		frame_or, radio_box, pane_fr;
/*---------------------------------------------------------------------*/

	frame_or = XtVaCreateManagedWidget("frameOri",
		 xmFrameWidgetClass, parent,
		 NULL);

	pane_fr = XtVaCreateManagedWidget( "pane_fr",
		xmPanedWindowWidgetClass, frame_or,
		XmNsashWidth,		  1,
		XmNsashHeight,		  1,
		XmNx,			  10,
		NULL );

	NxmLabel_createFrameLbl("Orientation", pane_fr, frame_or );

	radio_box = XmCreateRadioBox(pane_fr, "orient", NULL, 0);

	_oriW1	= XtVaCreateManagedWidget("Landscape",
		xmToggleButtonGadgetClass, radio_box,
		NULL);
	XtAddCallback(_oriW1, XmNvalueChangedCallback,
		(XtCallbackProc)_prtOrientSelectCb, (XtPointer)1);

	_oriW2	= XtVaCreateManagedWidget("Portrait",
		xmToggleButtonGadgetClass, radio_box,
		NULL);
	XtAddCallback(_oriW2, XmNvalueChangedCallback,
		(XtCallbackProc)_prtOrientSelectCb, (XtPointer)2);

	XtManageChild(radio_box);

	return;
}


/*=====================================================================*/

void _prtPageSelect ( Widget parent )
/************************************************************************
 * _prtPageSelect							*
 *									*
 * This function creates current page/all page selection area		*
 *									*
 * void _prtPageSelect(parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget		parent widget ID		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/97					*
 * J. Wu/SAIC		01/04	adjust the spacing			*
 ***********************************************************************/
{

Widget	  bb, rowcol;

/*---------------------------------------------------------------------*/
/* create a BulletinBoardWidget as the parent for
 * all the widgets in this panel
 */
	bb = XtVaCreateManagedWidget("bb",
		xmBulletinBoardWidgetClass, parent,
		NULL);
	rowcol = XtVaCreateManagedWidget("Rc",
		xmRowColumnWidgetClass, bb,
		XmNorientation, 	XmHORIZONTAL,
		XmNradioBehavior,	True,
		XmNnumColumns,		1,
		XmNspacing,		90,
		NULL);

	_OnePgW = XtVaCreateManagedWidget("Current Page",
		xmToggleButtonGadgetClass, rowcol,
		NULL);
	XtAddCallback(_OnePgW, XmNarmCallback,
		(XtCallbackProc)_prtPageSelectCb, (XtPointer)0);

	_AllPgW  = XtVaCreateManagedWidget("All Pages",
		xmToggleButtonGadgetClass, rowcol,
		NULL);
	XtAddCallback(_AllPgW, XmNarmCallback,
		(XtCallbackProc)_prtPageSelectCb, (XtPointer)1);

	return;
}


/*=====================================================================*/

void _prtCtlBtn ( Widget parent )
/************************************************************************
 * _prtCtlBtn								*
 *									*
 * This function create the control buttons				*
 *									*
 * void _prtCtlBtn(parent)						*
 *									*
 * Input parameters:							*
 *	parent	  Widget	parent widget ID			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/96					*
 *	S. Wang/GSC		04/97  switch OK and Cancel button	*
 * S. Law/GSC		07/00	added XmStringFree calls		*
 ***********************************************************************/
{
Widget	   bb, button ;
XmString   str;
char *buttonlist[] = { "OK", "Help", "Cancel" };

/*---------------------------------------------------------------------*/

	bb = XtVaCreateManagedWidget("bb",
		xmBulletinBoardWidgetClass, parent,
		XmNy,			20,
		NULL);

	str = XmStringCreateLocalized(buttonlist[0]);
	button = XtVaCreateManagedWidget("ok_bt",
			xmPushButtonWidgetClass, bb,
			XmNwidth,		 80,
			XmNx,			 20,
			XmNlabelString, 	 str,
			NULL);
	XmStringFree(str);
	XtAddCallback(button, XmNactivateCallback,
		      (XtCallbackProc)_prtOkCb, NULL );

	str = XmStringCreateLocalized(buttonlist[1]);
	button = XtVaCreateManagedWidget("help_bt",
			xmPushButtonWidgetClass, bb,
			XmNwidth,		 80,
			XmNx,			 20+120,
			XmNlabelString, 	 str,
			NULL);
	XmStringFree(str);
	XtAddCallback(button, XmNactivateCallback,
				   _prtHelpCb, NULL );

	str = XmStringCreateLocalized(buttonlist[2]);
	button = XtVaCreateManagedWidget("cancel_bt",
			xmPushButtonWidgetClass, bb,
			XmNwidth,		 80,
			XmNx,			 20+2*120,
			XmNlabelString, 	 str,
			NULL);
	XmStringFree(str);
	XtAddCallback(button, XmNactivateCallback,
				   _prtCancelCb, NULL );
}

/*=====================================================================*/

void _prtSetDefault ( int which )
/************************************************************************
 * _prtSetDefault							*
 *									*
 * This function set the default selections for the popup window	*
 *									*
 * void _prtSetDefault(which)						*
 *									*
 * Input parameters:							*
 *									*
 *   which	int	index to printer				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/96					*
 ***********************************************************************/
{
int	i, j;

/*---------------------------------------------------------------------*/

	if ( which == 0 ) {
	    for (j=0;j<3;j++) {
		XtSetSensitive(_colorWid[j], True);
	    }

	    XmToggleButtonSetState(_colorWid[0], False, True);
	    XmToggleButtonSetState(_colorWid[1], True,	True);
	    XmToggleButtonSetState(_colorWid[2], False, True);

	    XtSetSensitive(_sizeWid1, True);
	    XtSetSensitive(_sizeWid2, True);
	    XmToggleButtonSetState(_sizeWid1, True, True);
	    XmToggleButtonSetState(_sizeWid2, False,  True);

	    XtSetSensitive(_oriW1, True);
	    XtSetSensitive(_oriW2, True);
	    XmToggleButtonSetState(_oriW1, True, True);
	    XmToggleButtonSetState(_oriW2, False, True);
	}
	else {
	    for (i=0;i<3;i++)
		XtSetSensitive(_colorWid[i], True);

	    XmToggleButtonSetState(_colorWid[0], False, True);
	    XmToggleButtonSetState(_colorWid[1], False, True);
	    XmToggleButtonSetState(_colorWid[2], True, True);

	    if ( _prtStr.rec[which].color == 'g' ) {
		XmToggleButtonSetState(_colorWid[0], True, True);
		XmToggleButtonSetState(_colorWid[2], False, True);
		XtSetSensitive(_colorWid[2], False);
	    }

	    if (_prtStr.rec[which].color == 'b' ) {
		XmToggleButtonSetState(_colorWid[1], True, True);
		XmToggleButtonSetState(_colorWid[2], False, True);
		XtSetSensitive(_colorWid[0], False);
		XtSetSensitive(_colorWid[2], False);
	    }

	    XtSetSensitive(_sizeWid1, True);
	    XtSetSensitive(_sizeWid2, True);
	    XmToggleButtonSetState(_sizeWid1, False, True);
	    XmToggleButtonSetState(_sizeWid2, True, True);

	    if (_prtStr.rec[which].size == 's') {
		XmToggleButtonSetState(_sizeWid1, True, True);
		XmToggleButtonSetState(_sizeWid2, False, True);
		XtSetSensitive(_sizeWid2, False);
	    }

	    XmToggleButtonSetState(_oriW1, True, True);
	    XmToggleButtonSetState(_oriW2, False, True);
	}
}

/*=====================================================================*/

void _prtCommand ( char *cmd )
/************************************************************************
 * _prtCommand								*
 *									*
 * This function generates the print command				*
 *									*
 * void _prtCommand(cmd)						*
 *									*
 * Input/Output parameters:						*
 *   cmd		char*		print command			*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 * S. Wang/GSC		05/96						*
 * E. Safford/GSC	12/98	use $LP & $LPFLAG, clean up		*
 * S. Jacobs/NCEP	 2/99	Added check for default printer		*
 ***********************************************************************/
{
    strcpy (cmd, "$LP ");

    if  ( strcmp ( _curPrtName, "*" ) != 0 )  {
	strcat (cmd, getenv("LPFLAG"));
	strcat (cmd, _curPrtName);
    }
}

/*=====================================================================*/

void _defaultPrinter ( void )
/************************************************************************
 * _defaultPrinter							*
 *									*
 * This function sets the default printer attributes			*
 *									*
 * void _defaultPrinter()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *									*
 ** Log:								*
 *	S. Wang/GSC		03/97					*
 ***********************************************************************/
{
char	name[250];

/*---------------------------------------------------------------------*/

	_prtStr.select = 0;

	strcpy(name, "DEFAULT");
	_prtStr.rec[0].listname = (char *)malloc(strlen(name) +1);
	strcpy(_prtStr.rec[0].listname, name);

	strcpy(name, "*");
	_prtStr.rec[0].prtname = (char *)malloc(strlen(name) +1);
	strcpy(_prtStr.rec[0].prtname, name);

	_prtStr.rec[0].color= '*';
	_prtStr.rec[0].size= '*';

}

/*=====================================================================*/
/* ARGSUSED */
void _prtPsSelectCb ( Widget wdgt, long which, XtPointer call )
/************************************************************************
 * _prtPsSelectCb							*
 *									*
 * This is the callback function for printer/PS only toggle buttons	*
 *									*
 * void _prtPsSelectCb(w, which, call)					*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		calling widget id			*
 *	which	long		index of selection			*
 *	call	XtPointer	calling data				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/96					*
 * J. Wu/SAIC		01/04	call _prtSwitchMode()			*
 ***********************************************************************/
{
XmToggleButtonCallbackStruct	*call_struct;
/*---------------------------------------------------------------------*/

	call_struct = (XmToggleButtonCallbackStruct*)call;

	if ( call_struct->set == 0 )
		return;

	_psOnlyFlg = (int)which;

	_prtSwitchMode ();
}

/*=====================================================================*/
/* ARGSUSED */
void _prtSizeSelectCb ( Widget wdgt, long which, XmToggleButtonCallbackStruct *call )
/************************************************************************
 * _prtSizeSelectCb							*
 *									*
 * This is the callbak function for size selection toggles		*
 *									*
 * void _prtSizeSelectCb(w, which, call)				*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		calling widget id			*
 *	which	long		index of selection			*
 *	call	XmToggleButtonCallbackStruct*	calling data		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/96					*
 ***********************************************************************/
{
	if (call->set ==0)
		return;
#ifndef NSHARP
	_sizeMode = (int)which;
#endif
}

/*=====================================================================*/
/* ARGSUSED */
void _prtOrientSelectCb ( Widget wdgt, long which, XmToggleButtonCallbackStruct *call )
 /***********************************************************************
 * _prtOrientSelectCb							*
 *									*
 * This is the callback function for orientation choice toggles 	*
 *									*
 * void _prtOrientSelectCb(w, which, call)				*
 *									*
 * Input parameters:							*
 *	w	Widget		calling widget id			*
 *	which	long		index of selection			*
 *	call	XmToggleButtonCallbackStruct*	calling data		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/96					*
 ***********************************************************************/
{
	if (call->set == 0)
		return;
#ifndef NSHARP
	_oriMode = (int)which;
#endif
}

/*=====================================================================*/
/* ARGSUSED */
void _prtColorSelectCb ( Widget wdgt, long which, XmToggleButtonCallbackStruct *call )
/************************************************************************
 * _prtColorSelectCb							*
 *									*
 * This is the callback function for color selection toggles		*
 *									*
 * void _prtColorSelectCb(w, which, call)				*
 *									*
 * Input parameters:							*
 *	w	Widget		calling widget id			*
 *	which	long		index of selection			*
 *	call	XmToggleButtonCallbackStruct*	calling data		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/96					*
 ***********************************************************************/
{
	if (call->set ==0)
		return;
#ifndef NSHARP
	_colorMode = (int)which ;
#endif
}

/*=====================================================================*/
/* ARGSUSED */
void _prtPageSelectCb ( Widget wdgt, long which, XtPointer call )
 /***********************************************************************
 * _prtPageSelectCb							*
 *									*
 * This is the callback function for current page/all page		*
 * selection toggles.							*
 *									*
 * void _prtPageSelectCb(w, which, call)				*
 *									*
 * Input parameters:							*
 *	w	Widget		calling widget id			*
 *	which	long		index of selection			*
 *	call	XtPointer	calling data				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/96					*
 ***********************************************************************/
{
    _pageMode = (int)which;
}

/*=====================================================================*/
/* ARGSUSED */
void _prtNameCb ( Widget wdgt, long which, XtPointer call )
/************************************************************************
 * _prtNameCb								*
 *									*
 * This is the printer name option menu callback function		*
 *									*
 * void _prtNameCb(w, which, call)					*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		calling widget id			*
 *	which	long		index of selection			*
 *	call	XtPointer	never used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/96					*
 ***********************************************************************/
{
    _prtStr.select = (int)which;
    strcpy(_curPrtName, _prtStr.rec[which].prtname);
    _curPrtInx = which;
    _prtSetDefault(_curPrtInx);
}


/*=====================================================================*/
/* ARGSUSED */
void _prtOkCb ( Widget wdgt, XtPointer clnt, XtPointer call )
 /***********************************************************************
 * _prtOkCb								*
 *									*
 * This is the callback function for the OK control button		*
 *									*
 * void _prtOkCb(w, data, call) 					*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		calling widget id			*
 *	clnt	XtPointer	never used				*
 *	call	XtPointer	never used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *  S. Wang/GSC 	    05/96					*
 *  S. Wang/GSC 	    05/97	use _prtStopFlg 		*
 *  M. Li/GSC		    05/01	Add output for XmTextGetString	*
 ***********************************************************************/
{
int	i, iret, num_copy;
char	psfile[256];
char	cmd[60], prt_cmd[60], numcp[10];
char	*str;

/*---------------------------------------------------------------------*/
/*
 * get PostScript file name
 */
	if ( _psOnlyFlg == 1 ) {
	    str = XmTextGetString(_fileTxtW);
	    strcpy (psfile, str);
	    XtFree(str);

	    if (psfile[0] == '\0') {
		XmTextSetString(_fileTxtW, "ps.plt");
	    }
	}
	else
	    strcpy( psfile, "ps.plt");

/*
 * get number of copies
 */
	str = XmTextGetString(_copyTxtW);
	strcpy (numcp, str);
	XtFree(str);
	num_copy = atoi(numcp) ;
	cst_numb( numcp, &num_copy, &iret ) ;
	if ( iret != 0 ) {
	    num_copy = 1 ;
	    XmTextSetString(_copyTxtW, "1" ) ;
	}

	if (num_copy > MAXCOPY) {
	    num_copy = MAXCOPY;
	}
	else if (num_copy < 1) {
	    num_copy = 1;
	}

/*
 * pops down the print panel
 */
	XtUnmanageChild(_prtW);

/*
 * print to PostScript file
 */
	_printToPs(psfile);

/*
 * print to printer if desired
 */
	if ( !_psOnlyFlg && _prtStopFlg != -1 ) {
		_prtCommand(prt_cmd);
		sprintf(cmd, "%s %s", prt_cmd, psfile );

  		for (i=0 ; i<num_copy; i++) {
  		    system(cmd);
		} 
	}
			
/*
 * reset number of copies to 1
 */
	XmTextSetString(_copyTxtW, "1" ) ;
}

/*=====================================================================*/
/* ARGSUSED */
void _prtCancelCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * _prtCancelCb 							*
 *									*
 * This is the cancel button callback function				*
 *									*
 * void _prtCancelCb(wdgt, clnt, call)					*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		calling widget id			*
 *	clnt	XtPointer	never used				*
 *	call	XtPointer	never used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC		05/96					*
 *	M. Li/GSC		05/01	Add output for XtTextGetString	*
 ***********************************************************************/
{
char	psfile[256], *str;
/*---------------------------------------------------------------------*/

	str = XmTextGetString(_fileTxtW);
	strcpy (psfile, str);
	XtFree(str); 

	if ( psfile[0] == '\0' )
	    XmTextSetString(_fileTxtW, "ps.plt");

	XtUnmanageChild(_prtW);
}

/*=====================================================================*/
/* ARGSUSED */
void _prtHelpCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * _prtHelpCb								*
 *									*
 * This is the callback function for HELP button			*
 *									*
 * void _prtHelpCb(wdgt, clnt, call)					*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		calling widget id			*
 *	clnt	XtPointer	never used				*
 *	call	XtPointer	never used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	S. Wang/GSC	 05/96						*
 *	G. Krueger/EAI	 11/97	Renamed NxmHelp functions		*
 ***********************************************************************/
{
    NxmHelp_helpBtnCb(wdgt, 9, NULL);
}

/*=====================================================================*/

void _printToPs ( char *ps_file )
 /***********************************************************************
 * _printToPs								*
 *									*
 * this function print to a PostScript file				*
 *									*
 * void _printToPs(ps_file)						*
 *									*
 * Input parameters:							*
 *  ps_file	*char	 ps file name					*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		2/97						*
 * S. Wang/GSC		5/97  add _prtStopFlg and _printFlg		*
 ***********************************************************************/
{
int	ier;

/*---------------------------------------------------------------------*/
/*
 * set device to PS driver
 */

	ier = _prtSetPSDev(ps_file);

	if (ier == -1 ) {
	    printf(" error setting PS device.\n");
	    NxmPrt_stopPrt();
	    return;
	}

/*
 * set print in process flag and
 * reset print stop flag
 */
	_prtStopFlg = 0;
	_printFlg = 1;

/*
 * call application print function
 */
	_applPrtFunc();

/*
 * reset print in process flag
 */
	_printFlg = 0;

/*
 * set device to XWP driver
 */
	ier = _prtSetXWPdev(_applWin);

	if (ier == -1 ) {
	    printf(" error setting XWP device.\n");
	    NxmPrt_stopPrt();
	}
}

/*=====================================================================*/
/* ARGSUSED */
void _prtSwitchMode ( void )
/************************************************************************
 * _prtSwitchMode							*
 *									*
 * Switch between printer adn PS mode based on _psOnlyFlg flag.		*
 *									*
 * void _prtSwitchMode ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 * J. Wu/SAIC		01/04	move from _prtPsSelectCb		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
	
	if (_psOnlyFlg) {
		XtSetSensitive(_framePrt, False);
		XtSetSensitive(_frameFil, True);
		XtSetSensitive(_colorWid[0], True);

		XtSetSensitive(_colorWid[1], True);
		XtSetSensitive(_colorWid[2], True);
		XtSetSensitive(_sizeWid2, True);

		XmTextSetString(_copyTxtW, "1");
		XtSetSensitive(_copyTxtW, False);


		XmToggleButtonSetState(_colorWid[0], True, True);
		XmToggleButtonSetState(_colorWid[1], False, True);
		XmToggleButtonSetState(_colorWid[2], False, True);

	}
	else {
		XtSetSensitive(_framePrt, True);
		XtSetSensitive(_frameFil, False);
		XtSetSensitive(_copyTxtW, True);
		_prtSetDefault(_curPrtInx);
	}
}

/*=====================================================================*/
#ifndef NSHARP
int _prtSetPSDev ( char *ps_file )
 /***********************************************************************
 * _prtSetPSDev 							*
 *									*
 * this change device to PS and set its attributes			*
 *									*
 * int _prtSetPSDev(ps_file)						*
 *									*
 * Input parameters:							*
 *  *ps_file	char	 ps file name					*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 * _prtSetPSDev	int							*
 **									*
 * Log: 								*
 * S. Wang/GSC	   	 1/97 						*
 * S. Wang/GSC	   	 5/97 	remove attribute modes as inputs	*
 * E. Safford/GSC	04/99	fix irix6 compiler warning		*
 ***********************************************************************/
{
char	dev[50];
char	color_type[50];
char	fname[256];
char	paper_size[50];
float	lsize, ssize, xsize, ysize;
char	ctype;
int	ier;
/*--------------------------------------------------------------------*/
/*
 * set color, size and orientation modes
 */
	ctype = (_colorMode == 1 ) ? 'M' : 'C' ;

	if ( _sizeMode == 2 ) {
		lsize = 17.0F;
		ssize = 11.0F;
	}
	else {
		lsize = 11.0F;
		ssize = 8.5F;
	}

	if ( _oriMode == 2 ) {
		xsize = ssize;
		ysize = lsize;
	}
	else {
		xsize = lsize;
		ysize = ssize;
	}

/*
 * change device to PS
 */
	strcpy( dev, "PS|");
	strcpy(fname, ps_file);
	strcat( dev, fname );
	sprintf(paper_size, "|%3.1f;%3.1f", xsize, ysize );

	strcat( dev, paper_size );
	sprintf( color_type, "|%c", ctype );
	strcat( dev, color_type );

	gg_sdev( dev, &ier, strlen(dev) );
	if ( ier != 0 ) {
		printf(" error calling gg_sdev, ier = %d\n", ier);
		return (-1);
	}

	return (0);
}

/*=====================================================================*/

int _prtSetXWPdev ( char *wname )
 /***********************************************************************
 * _prtSetXWPdev							*
 *									*
 * this function changes device to XWP					*
 *									*
 * int _prtSetXWPdev(wname)						*
 *									*
 * Input parameters:							*
 *   *wname	int	application name				*
 *									*
 * Output parameters:							*
 *			NONE						*
 * Return parameters:							*
 * _prtSetXWPdev	int						*
 **									*
 * Log: 								*
 * S. Wang/GSC	   	 1/97 						*
 * E. Safford/GSC	04/99	fix irix6 compiler warning		*
 ***********************************************************************/
{
char	dev[20];
int	ier;
/*---------------------------------------------------------------------*/

	strcpy(dev, "XWP|");
	strcat(dev, wname);

	gg_sdev( dev, &ier, strlen(dev) );
	if ( ier != 0 ) {
		printf(" error calling gg_sdev, ier = %d\n", ier);
		return (-1);
	}

	return (0);
}

/*=====================================================================*/

void NxmPrt_txtPrtShow ( Widget parent, char* fname )
/************************************************************************
 * NxmPrt_txtPrtShow						        *
 *									*
 * This function displays a printer selection window for text files	*
 *									*
 * void   NxmPrt_txtPrtShow (parent, fname)				*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *      fname		char*	name of text file to be saved		*
 *									*
 * Output parameters:							*
 * Return parameters:                                                   *
 *			NONE						*
 **									*
 * Log:									*
 * H. Zeng/SAIC         05/04   initial coding                          *
 ***********************************************************************/
{
    XmString	*xm_items, xmstr, xmstr2, xmstr3;
    Widget	child;
    int		ii;
/*---------------------------------------------------------------------*/
/*
 * Copy the file name to a local global.
 */
    strcpy (_currTxtFileNm, fname);

/*
 * create dialog shell
 */
    _txtPrtWin  = XmCreateSelectionDialog (parent, "Printer Selection",
					   NULL, 0                      );
    XtVaSetValues ( XtParent(_txtPrtWin), 
		    XmNtitle,		"Printer Selection", 
		    XmNdeleteResponse,	XmDESTROY,
		    NULL				     );

/*
 * Set the selection items.
 */
    xm_items = (XmString*) XtMalloc ( _prtStr.n_prt * sizeof (XmString) );

    xm_items[0] = XmStringCreateLocalized ("default");
    for ( ii = 1; ii < _prtStr.n_prt; ii++ ) {

       xm_items[ii] = XmStringCreateLocalized (_prtStr.rec[ii].prtname);
    }

/* 
 * Set the list label string.
 */
    xmstr = XmStringCreateLocalized ("Printers");

/* 
 * Set the OK button string.
 */
    xmstr2 = XmStringCreateLocalized ("Print");

/* 
 * Set the text field string.
 */
    xmstr3 = XmStringCreateLocalized (_currTxtPrtName);

/*
 * Set selection box resources.
 */
    XtVaSetValues ( _txtPrtWin, 
		    XmNlistItems,	      xm_items, 
		    XmNlistItemCount,	      _prtStr.n_prt,
		    XmNlistVisibleItemCount,  5,
                    XmNlistLabelString,	      xmstr,
		    XmNokLabelString,	      xmstr2,
		    XmNtextString,	      xmstr3,
		    XmNmustMatch,	      TRUE,
		    XmNnoResize,	      TRUE,
		    NULL				       );

/*
 * Unmanage unused button childen.
 */
    child = XmSelectionBoxGetChild ( _txtPrtWin, XmDIALOG_APPLY_BUTTON );
    XtUnmanageChild ( child );

    child = XmSelectionBoxGetChild ( _txtPrtWin, XmDIALOG_HELP_BUTTON );
    XtUnmanageChild ( child );

/*
 * Add callbacks for Ok and Cancel buttons.
 */
    XtAddCallback ( _txtPrtWin, XmNokCallback,
		    (XtCallbackProc)NxmPrt_txtPrtCtlBtnCb, 
		    NULL		                   );

    XtAddCallback ( _txtPrtWin, XmNcancelCallback,
		    (XtCallbackProc)NxmPrt_txtPrtCtlBtnCb, 
		    NULL		                   );

    XtAddCallback ( _txtPrtWin, XmNnoMatchCallback,
		    (XtCallbackProc)NxmPrt_txtPrtCtlBtnCb, 
		    NULL		                   );

    XtAddCallback ( _txtPrtWin, XmNdestroyCallback,
		    (XtCallbackProc)NxmPrt_txtPrtDestroyCb, 
		    NULL	                           );

/*
 * Free allocated memory.
 */
    for ( ii = 0; ii < _prtStr.n_prt; ii++ ) XmStringFree (xm_items[ii]);    
    XtFree ( (char*)xm_items );

    XmStringFree ( xmstr  );
    XmStringFree ( xmstr2 );
    XmStringFree ( xmstr3 );

/*
 * Manage Selection Box at the end.
 */
    XtManageChild ( _txtPrtWin);
    XtAddGrab ( _txtPrtWin, TRUE, FALSE );
}

/*=====================================================================*/

/* ARGSUSED */
void NxmPrt_txtPrtCtlBtnCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * NxmPrt_txtPrtCtlBtnCb						*
 *									*
 * Callback function for control buttons at the bottom of Printer	*
 * Selection window.							*
 *									*
 * void NxmPrt_txtPrtCtlBtnCb (wid, fname, call)			*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		widget ID				*
 *	clnt	XtPointer	pointer to text file name		*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		05/04	initial coding				*
 * T. Piper/SAIC	09/04	Changed to LPAODT & LPAODTFLAG		*
 ***********************************************************************/
{
    char      *ptext=NULL, cmd[128];
    XmSelectionBoxCallbackStruct* cbs;
/*---------------------------------------------------------------------*/

    cbs = (XmSelectionBoxCallbackStruct*)call;

    switch( cbs->reason ) {

      case XmCR_OK:	  /* Print button */

	ptext = XmStringUnparse (cbs->value, NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
        strcpy (_currTxtPrtName, ptext);
        XtFree (ptext);
	
	strcpy (cmd, "$LPAODT ");

        if  ( strcasecmp (_currTxtPrtName, "default") != 0 )  {

	    strcat (cmd, getenv("LPAODTFLAG"));
	    strcat (cmd, _currTxtPrtName);
	    strcat (cmd, " ");
        }

	strcat (cmd, _currTxtFileNm);

  	system(cmd);

        XtPopdown( XtParent(_txtPrtWin) );
        XtDestroyWidget( _txtPrtWin );

	break;

      case XmCR_CANCEL:	  /* Cancel */

        XtPopdown( XtParent(_txtPrtWin) );
        XtDestroyWidget( _txtPrtWin );

	break;

      case XmCR_NO_MATCH: /* No match callback */

	NxmWarn_show( wdgt, "Plase select a valid printer!" );

	break;

      default:	          /* All others */

        XtPopdown( XtParent(_txtPrtWin) );
        XtDestroyWidget( _txtPrtWin );

	break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void NxmPrt_txtPrtDestroyCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * NxmPrt_txtPrtDestroyCb						*
 *									*
 * Callback function before Printer Selection window gets destroyed.	*
 *									*
 * void NxmPrt_txtPrtDestroyCb (wdgt, clnt, call)			*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		widget ID				*
 *	clnt	XtPointer	client data				*	
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:				  					*
 * H. Zeng/SAIC		06/04	initial coding				*
 ***********************************************************************/
{
/*
 * Delete the temporary file used for printing.
 */
    remove (_currTxtFileNm);
}
#endif
