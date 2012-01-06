#include "HydroBriefUI.H"

/***************************************************************************/


HydroBriefUI::HydroBriefUI()
{
    
   //printf("Inside HydroBriefUI vanilla constructor\n");
     return;
}

/***************************************************************************/


HydroBriefUI::HydroBriefUI(HydroBriefULT *initULT,
			   Widget topShell,
			   Application *initApp,
			   int argc,
			   char *argv[])
{
     
     Arg al[64];                    /* Arg List */
     register int ac = 0;           /* Arg Count */
     XtAppContext    app_context;     

     printf("in HydroBriefUI 5arg constructor, topShell=%p\n", topShell);
     if (topShell == NULL)
     {
          isRunningEventLoop = True;	
     }
     else
     {
	  isRunningEventLoop = False;
     }
     
     //printf("isRunningEventLoop == %d\n",isRunningEventLoop);
     
     
     if (isRunningEventLoop)
     {
	  app_context = XtCreateApplicationContext();
          shell = XtAppInitialize (&app_context, "HydroBrief", NULL, 0, 
	  			   &argc, argv, NULL, al, ac );
	  
	  // added 3/23/2000 by markg to solve crash problem
	  // in create_widgets() when variable parent is set wrong, since 
	  // it was neither set explicitly nor initialized
	  
	  parent = shell;     
     }
     
     else
     {
	  parent = topShell;	
     }
     
     
     // init data members
     app = initApp;
     ult = initULT;
     
     
printf("before creating widgets\n");     
     createWidgets();
printf("before show\n");
     show();
     
     if (isRunningEventLoop)
     {
	  XtRealizeWidget ( shell );
     }
 
     setupCanvas();     
     setupEventHandling();
     XmListSelectPos(streamLI, 1, True);
     
     if (isRunningEventLoop)
     {
	XtAppMainLoop ( app_context );
     }
     
     
printf("exiting HydroBriefUI 5 arg constructor\n");
     return;
}   

/***************************************************************************/

HydroBriefUI::~HydroBriefUI()
{
     //printf("Inside HydroBriefUI destructor\n"); 
 
     delete canvas;
     
     return;
} 

/***************************************************************************/


/***************************************************************************/

 
void HydroBriefUI::show()
{
     Arg al[64];                    /* Arg List */
     register int ac = 0;           /* Arg Count */
 
     
     //printf("HydroBriefUI::show() \n");
     XtManageChild(form);
     
     if (! isRunningEventLoop)
     {
          XtManageChild(shell);
     }
     
     else
     {
          ac = 0;
	  XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	  XtSetArg(al[ac], XmNtitle, "River Summary"); ac++;
	  XtSetArg(al[ac], XmNminWidth, 800); ac++;
	  XtSetArg(al[ac], XmNminHeight, 850); ac++;
	  XtSetArg(al[ac], XmNmaxWidth, 800); ac++;
	  XtSetArg(al[ac], XmNmaxHeight, 850); ac++;
	  
	  XtSetValues ( shell, al, ac );
	
     }
 
     loadRiverList();
     
     return;
} 

/***************************************************************************/


void HydroBriefUI::close()
{
     //
     //  close the window and destroy the "application"
     //
     //printf("Inside of HydroBriefUI::close()\n");
  
     
     XtDestroyWidget(shell);
     shell = NULL;
        
     Application::destroy(app);
    
     return;
} 


/***************************************************************************/

void HydroBriefUI::print()
{
   //printf("Inside of HydroBriefUI::print()\n");   
   
}

/***************************************************************************/

void HydroBriefUI::createWidgets()
{
	Widget children[6];      /* Children to manage */
	//	Display *display = XtDisplay ( parent );
	Display *display;
	Arg al[64];                    /* Arg List */
	register int ac = 0;           /* Arg Count */
	XrmValue from_value, to_value; /* For resource conversion */
	XmString xmstrings[16];    /* temporary storage for XmStrings */

printf("in createWidgets, parent=%p\n", parent);
	display = XtDisplay(parent);

printf("in createWidgets\n");
	if (!isRunningEventLoop)
	{
	     XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	     XtSetArg(al[ac], XmNtitle, "HydroBrief"); ac++;
	     XtSetArg(al[ac], XmNminWidth, 800); ac++;
	     XtSetArg(al[ac], XmNminHeight, 850); ac++;
	     XtSetArg(al[ac], XmNmaxWidth, 800); ac++;
	     XtSetArg(al[ac], XmNmaxHeight, 850); ac++;
	     shell = XmCreateDialogShell ( parent, "shell", al, ac );
	}
	
	
	/**** generated code except for the shell stuff above this line*****/

	ac = 0;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	form = XmCreateForm ( shell, "form", al, ac );
	ac = 0;
	XtSetArg(al[ac], XmNlistSizePolicy, XmRESIZE_IF_POSSIBLE); ac++;
	streamLI = XmCreateScrolledList ( form, "streamLI", al, ac );
	ac = 0;
	streamSL = XtParent ( streamLI );

	XtSetArg(al[ac], XmNhorizontalScrollBar, &streamHSB ); ac++;
	XtSetArg(al[ac], XmNverticalScrollBar, &streamVSB ); ac++;
	XtGetValues(streamSL, al, ac );
	ac = 0;
	xmstrings[0] = XmStringCreateLtoR("Stream List", (XmStringCharSet)XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstrings[0]); ac++;
	streamListLA = XmCreateLabel ( form, "streamListLA", al, ac );
	ac = 0;
	XmStringFree ( xmstrings [ 0 ] );
	XtSetArg(al[ac], XmNwidth, 85); ac++;
	XtSetArg(al[ac], XmNheight, 35); ac++;
	xmstrings[0] = XmStringCreateLtoR("Close", (XmStringCharSet)XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstrings[0]); ac++;
	closePB = XmCreatePushButton ( form, "closePB", al, ac );
	ac = 0;
	XmStringFree ( xmstrings [ 0 ] );
	optionFR = XmCreateFrame ( form, "optionFR", al, ac );
	optionFO = XmCreateForm ( optionFR, "optionFO", al, ac );
	xmstrings[0] = XmStringCreateLtoR("Stage Basis:", (XmStringCharSet)XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstrings[0]); ac++;
	stageBasisOM = XmCreateOptionMenu ( optionFO, "stageBasisOM", al, ac );
	ac = 0;
	XmStringFree ( xmstrings [ 0 ] );
	stageBasisOMLA = XmOptionLabelGadget ( stageBasisOM );
	stageBasisCB = XmOptionButtonGadget ( stageBasisOM );
	stageBasisPDM = XmCreatePulldownMenu ( stageBasisOM, "stageBasisPDM", al, ac );
	xmstrings[0] = XmStringCreateLtoR("Max Obs/Fcst", (XmStringCharSet)XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstrings[0]); ac++;
	maxObsFcstPB = XmCreatePushButton ( stageBasisPDM, "maxObsFcstPB", al, ac );
	ac = 0;
	XmStringFree ( xmstrings [ 0 ] );
	xmstrings[0] = XmStringCreateLtoR("Observed", (XmStringCharSet)XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstrings[0]); ac++;
	obsPB = XmCreatePushButton ( stageBasisPDM, "obsPB", al, ac );
	ac = 0;
	XmStringFree ( xmstrings [ 0 ] );
	xmstrings[0] = XmStringCreateLtoR("Forecast", (XmStringCharSet)XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstrings[0]); ac++;
	fcstPB = XmCreatePushButton ( stageBasisPDM, "fcstPB", al, ac );
	ac = 0;
	XmStringFree ( xmstrings [ 0 ] );
	XtSetArg(al[ac], XmNchildType, XmFRAME_TITLE_CHILD); ac++;
	xmstrings[0] = XmStringCreateLtoR("Options", (XmStringCharSet)XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstrings[0]); ac++;
	filterFrameLA = XmCreateLabel ( optionFR, "filterFrameLA", al, ac );
	ac = 0;
	XmStringFree ( xmstrings [ 0 ] );
	xmstrings[0] = XmStringCreateLtoR("Stations ordered by river mile", (XmStringCharSet)XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstrings[0]); ac++;
	drawAreaFrameLA = XmCreateLabel ( form, "drawAreaFrameLA", al, ac );
	ac = 0;
	XmStringFree ( xmstrings [ 0 ] );
	XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
	drawingAreaSW = XmCreateScrolledWindow ( form, "drawingAreaSW", al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNhorizontalScrollBar, &drawingAreaHSB ); ac++;
	XtSetArg(al[ac], XmNverticalScrollBar, &drawingAreaVSB ); ac++;
	XtGetValues(drawingAreaSW, al, ac );
	ac = 0;
	if (DefaultDepthOfScreen(DefaultScreenOfDisplay(display)) != 1) {
	from_value.addr = "Black";
	from_value.size = strlen( from_value.addr ) + 1;
	to_value.addr = NULL;
	XtConvertAndStore (drawingAreaSW, XmRString, &from_value, XmRPixel, &to_value);
	if ( to_value.addr )
	{
		XtSetArg(al[ac], XmNbackground, *(unsigned int *)to_value.addr); ac++;
	}
	}
	briefDA = XmCreateDrawingArea ( drawingAreaSW, "briefDA", al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 50); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_OPPOSITE_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, -320); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_OPPOSITE_FORM); ac++;
	XtSetArg(al[ac], XmNrightOffset, -375); ac++;
	XtSetValues ( streamSL,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 25); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_OPPOSITE_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, -50); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_OPPOSITE_FORM); ac++;
	XtSetArg(al[ac], XmNrightOffset, -100); ac++;
	XtSetValues ( streamListLA,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 810); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_OPPOSITE_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, -840); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 350); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_OPPOSITE_FORM); ac++;
	XtSetArg(al[ac], XmNrightOffset, -440); ac++;
	XtSetValues ( closePB,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 50); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_OPPOSITE_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, -320); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 385); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_OPPOSITE_FORM); ac++;
	XtSetArg(al[ac], XmNrightOffset, -790); ac++;
	XtSetValues ( optionFR,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 335); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
	XtSetValues ( drawAreaFrameLA,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 360); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_OPPOSITE_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, -800); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_OPPOSITE_FORM); ac++;
	XtSetArg(al[ac], XmNrightOffset, -790); ac++;
	XtSetValues ( drawingAreaSW,al, ac );
	ac = 0;
	XtManageChild(streamLI);

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
	XtSetValues ( stageBasisOM,al, ac );
	ac = 0;
	children[ac++] = maxObsFcstPB;
	children[ac++] = obsPB;
	children[ac++] = fcstPB;
	XtManageChildren(children, ac);
	ac = 0;
	XtSetArg(al[ac], XmNsubMenuId, stageBasisPDM); ac++;
	XtSetValues ( stageBasisCB, al, ac );
	ac = 0;
	children[ac++] = stageBasisOM;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = optionFO;
	children[ac++] = filterFrameLA;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = briefDA;
	XtManageChildren(children, ac);
	ac = 0;
	XmScrolledWindowSetAreas(drawingAreaSW, drawingAreaHSB, drawingAreaVSB, briefDA );
	children[ac++] = streamListLA;
	children[ac++] = closePB;
	children[ac++] = optionFR;
	children[ac++] = drawAreaFrameLA;
	children[ac++] = drawingAreaSW;
	XtManageChildren(children, ac);
	ac = 0;

	return;
}

/***************************************************************************/

void HydroBriefUI::setupCanvas()
{
     //printf("inside UI::setupCanvas\n");
     Dimension width = 10000;
   
     //
     // set the widget heights
     //
     setWidgetHeight(briefDA, getWidgetHeight(drawingAreaSW) - sw_offset);
     setWidgetWidth(briefDA, width);
     
     canvas =  new Canvas(briefDA);
     
     return;     
}

/***************************************************************************/


void HydroBriefUI::setupEventHandling()
{
     Atom atom;
     atom = XmInternAtom(XtDisplay(shell), "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(shell, atom, closeCallback, this);
     
     
     
     // close PB
     XtAddCallback(closePB, XmNactivateCallback,
		   &HydroBriefUI::closeCallback, this);
 
     
     // streamLI
     XtAddCallback(streamLI, XmNbrowseSelectionCallback,
		   &HydroBriefUI::selectRiverCallback, this);
     
     //
     // Pushbuttons for StageBasis option menu
     //
     XtAddCallback(maxObsFcstPB, XmNactivateCallback,
		   &HydroBriefUI::selectStageBasisCallback, this);
     
    
     XtAddCallback(fcstPB, XmNactivateCallback,
		   &HydroBriefUI::selectStageBasisCallback, this);
     
    
     XtAddCallback(obsPB, XmNactivateCallback,
		   &HydroBriefUI::selectStageBasisCallback, this);
     
     return;
}

/***************************************************************************/

void HydroBriefUI::loadRiverList()
{
     
     ult->loadRiverList(streamLI);
     
     return;  
}

/***************************************************************************/

void HydroBriefUI::selectRiverCallback(Widget listWidget,
					XtPointer ptr, XtPointer cbs)
{
   
     HydroBriefUI *ui = (HydroBriefUI *) ptr;
   
     int *poslist = NULL;
     int count  = 0;
     int selectedPos = -1;
     
     int value;
     int slider_size;
     int increment;
     int page_increment;
     
     XmListGetSelectedPos(listWidget, &poslist, &count );
     
     if (count > 0)
     {
          selectedPos = poslist[0];
     }
     
     
     ui->ult->updateBriefPicture(selectedPos, ui->canvas);
   
   
        
     XmScrollBarGetValues(ui->drawingAreaHSB,
			  &value, &slider_size,
			  &increment,
			  &page_increment);

     XmScrollBarSetValues(ui->drawingAreaHSB,
			  0, slider_size,
			  increment, page_increment, True);
	
     
     return;   
}

/***************************************************************************/

void HydroBriefUI::selectStageBasisCallback(Widget pushbutton,
					XtPointer ptr, XtPointer cbs)
{
   
     HydroBriefUI *ui = (HydroBriefUI *) ptr;
     int buttonNumber = -1;
     int childCount;
     int i;
     
     WidgetList widgetList;
     
     
     GetWidgetChildren(ui->stageBasisPDM, &widgetList, &childCount);
     if (childCount > 0)
     {
	  for (i = 0; i < childCount; i++)
	  {
	       if (pushbutton == widgetList[i])  	
	       {
		    buttonNumber = i;
		    break;   
	       }
	  }
	  
	  // free(widgetList);
     }
     
  
     
     if (buttonNumber != -1)
     {
          ui->ult->selectStageBasis(buttonNumber, ui->canvas);
     }
     return;   
}

/***************************************************************************/

void HydroBriefUI::closeCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     HydroBriefUI *hbDialog = (HydroBriefUI *) ptr;

     
     //printf("in HydroBriefUI::closeCallback() \n");
     //hbDialog->HydroBriefUI::print();
     
     hbDialog->HydroBriefUI::close();
      
     
     return;   
}

/***************************************************************************/
