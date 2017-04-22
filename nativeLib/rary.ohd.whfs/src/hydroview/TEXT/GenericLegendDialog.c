#include <Xm/Xm.h>

#include "GenericLegendDialog.h"
#include "map.h"
#include "map_defines.h"
#include "map_library.h"
#include "map_menubar.h"
#include "map_resource.h"
#include "post_functions.h"

/***************************************************************************/

void startGenericLegendDialog(Widget parent,
			      char * title,
			      Dimension dialogHeight,
			      Dimension dialogWidth,
			      void (* drawFunctionPtr)
                                   (struct _GenericLegendDialog *),
			      void *argument)
{
     
     static  GenericLegendDialog gld[MAX_LEGEND_DIALOGS];
     
     int 	num = -1;
     int	i = 0;
     char	buf[BUFSIZ];
     
     
     /*
     Find an open array slot for a GLD to be used.
     */
     for (i = 0 ; i < MAX_LEGEND_DIALOGS; i++)
     {
	  if (! XtIsManaged(gld[i].shell) )
	  {
	       break;       
	  }       
     }   
     num = i;
      
     
     /*
     If you find an open slot, show it
     */
     if ((num < MAX_LEGEND_DIALOGS) && (num != -1 ))
     {  
	initGenericLegendDialog(&gld[num],
				parent,
				title,
				dialogHeight, dialogWidth,
				drawFunctionPtr,
				argument);

				   
     }
     else
     {
	  sprintf(buf, "You already have %d Legends Dialogs running.\n "  
		  "Close one if you want to open another",
		  MAX_LEGEND_DIALOGS);
	  
	  ErrorDialog(parent, buf);		
     }	
     return;   
}


/****************************************************************************/ 

void initGenericLegendDialog(GenericLegendDialog *dialog,
			     Widget parent,
			     char *title,
			     Dimension dialogHeight,
			     Dimension dialogWidth,
			     void (* drawFunctionPtr)(struct _GenericLegendDialog *),
			     void *argument)
{
     
      dialog->parent = parent;
      
      strcpy(dialog->title, title);

      dialog->dialogHeight = dialogHeight;
      dialog->dialogWidth = dialogWidth;
      
      dialog->daHeight = dialog->dialogHeight - 55;
      dialog->daWidth = dialog->dialogWidth;
      	 
      dialog->drawFunctionPtr = drawFunctionPtr;
      
      dialog->argument = argument;
      
     	
      showGenericLegendDialog(dialog);
      
      
      return;
     
}
/****************************************************************************/   

void    showGenericLegendDialog(GenericLegendDialog *dialog)
{
     
     if (! dialog->shell )
     {

	  /*
		create widgets and layout
	  */
	  createGenericLegendDialog(dialog);
	  
	  
	  /*
	  	set height and width of drawingArea
	  */
	  setWidgetHeight(dialog->drawingArea, dialog->daHeight);
	  setWidgetWidth(dialog->drawingArea, dialog->daWidth);

	  
	  /*
	  	manage widgets
	  */
	  XtManageChild(dialog->form);
	  XtManageChild(dialog->shell);
	
	  
	  /*
	  	set up graphics parameters
	  */
	  gldInitGraphicsMembers(dialog);
	  
	  
	  /*
	  	draw the legend
	  */
	  drawGenericLegend(dialog);


	  /*
	  	add callbacks
	  */
	  addGenericLegendDialogCallbacks(dialog);

	  
     }
          
     return;
}

/********************************************************************************/   


void createGenericLegendDialog(GenericLegendDialog *dialog)
{
	Widget children[2];      /* Children to manage */
	Arg al[64];                    /* Arg List */
	register int ac = 0;           /* Arg Count */
	XmString xmstrings[16];    /* temporary storage for XmStrings */
        Position middle = 0;
	
	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;

	/* 
		hand edited section
	*/
	XtSetArg(al[ac], XmNtitle, dialog->title); ac++;
	XtSetArg(al[ac], XmNminWidth, dialog->dialogWidth); ac++;
	XtSetArg(al[ac], XmNminHeight, dialog->dialogHeight); ac++;
	XtSetArg(al[ac], XmNmaxWidth, dialog->dialogWidth); ac++;
	XtSetArg(al[ac], XmNmaxHeight, dialog->dialogHeight); ac++;

	dialog->shell = XmCreateDialogShell (dialog->parent, "shell", al, ac );
	
	ac = 0;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	dialog->form = XmCreateForm ( dialog->shell, "form", al, ac );
	
	
	ac = 0;
	
	XtSetArg(al[ac], XmNwidth, 85); ac++;
	XtSetArg(al[ac], XmNheight, 35); ac++;
	xmstrings[0] = XmStringCreateLtoR("Close", 
		      (XmStringCharSet)XmFONTLIST_DEFAULT_TAG);
	
	XtSetArg(al[ac], XmNlabelString, xmstrings[0]); ac++;
	
	dialog->closePB = XmCreatePushButton ( dialog->form, "closePB", al, ac );

	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 10); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	
	middle = dialog->dialogWidth/2 - 42;
	XtSetArg(al[ac], XmNleftOffset, middle); ac++;
	
	XtSetValues ( dialog->closePB, al, ac );
	ac = 0;
	
	
	ac = 0;
	XmStringFree ( xmstrings [ 0 ] );
	
	dialog->drawingArea = XmCreateDrawingArea ( dialog->form, 
                                                    "drawingArea", al, ac );

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 0); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 55); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 0); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightOffset, 0); ac++;
	XtSetValues ( dialog->drawingArea,al, ac );

        /* Create the pixmap for drawing onto the form. */
        dialog->pixmap = XCreatePixmap ( _get_map_display ( ) ,
                RootWindowOfScreen ( XtScreen ( dialog->drawingArea ) ) ,
                                         dialog->dialogWidth ,
                                         dialog->dialogHeight ,
                DefaultDepthOfScreen ( XtScreen ( dialog->drawingArea ) ) ) ;
                                         
	ac = 0;
	children[ac++] = dialog->closePB;
	children[ac++] = dialog->drawingArea;
	XtManageChildren(children, ac);
	ac = 0;
	
	return;
}

/************************************************************************/

void addGenericLegendDialogCallbacks(GenericLegendDialog *dialog)
{
     Atom	atom;
     
     /*
     Handle window manager events.
     */
     atom = XmInternAtom(XtDisplay(dialog->shell), "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(dialog->shell, atom, gldCancelCallback, dialog);
     
     
     /*
     Set up callbacks
     */
     XtAddCallback(dialog->closePB, XmNactivateCallback,
		   gldCancelCallback, dialog);
     
     /*
         drawing area callback
     */
     XtAddCallback(dialog->drawingArea, XmNexposeCallback,
		   redrawGenericLegendCallback, dialog);
     
  
     return;
}

/*****************************************************************************/   

void gldCancel(GenericLegendDialog *dialog)
{
     Display * pDisplay = NULL ;

     pDisplay = _get_map_display ( ) ;

     /* Free any allocated X/Motif resources. */
     XFreePixmap ( pDisplay , dialog->pixmap ) ;

     /* Destroy the shell. */
     XtDestroyWidget(dialog->shell);
     dialog->shell = (Widget) NULL;
     
     return;   
}


/****************************************************************************/

void gldCancelCallback(Widget w,  XtPointer ptr,  XtPointer cbs)
{
    GenericLegendDialog *dialog = (GenericLegendDialog *) ptr; 
    int data_flag ;
   
    gldCancel(dialog);

    /* Check to determine if MPE data is being displayed.  If so, then
       there is MPE legend and it cannot be turned off. */
    data_flag = isThereMpeData ( ) ;

    if ( data_flag == 0 )
    {
       _set_legend_state ( M_OFF ) ;
    }
   
    return;
}

/****************************************************************************/

void redrawGenericLegend(GenericLegendDialog *dialog)
{
     
     if (dialog->pixmap)
     {
     	CopyPixmapToDA(dialog->drawingArea, dialog->pixmap);
     }
     
    return;     
}

/****************************************************************************/

void redrawGenericLegendCallback(Widget w,  XtPointer ptr,  XtPointer cbs)
{
     
     GenericLegendDialog *dialog = (GenericLegendDialog *) ptr; 
     
     redrawGenericLegend(dialog);
     
     return;
     
}

/****************************************************************************/
     
void gldInitGraphicsMembers(GenericLegendDialog *dialog)
{
     
     dialog->daWidth = getWidgetWidth(dialog->drawingArea);
     dialog->daHeight = getWidgetHeight(dialog->drawingArea);
     
     
     /*
     Set the general graphics fields that are used
     by the data and scale Drawing Areas.
     */
     dialog->display = _get_map_display ( ) ;
     dialog->screen = XtScreen(dialog->drawingArea);
     
     /*
     Set the specific graphics fields that are used
     by the data and scale Drawing Areas.
     */
     dialog->window = XtWindow(dialog->drawingArea);
     dialog->gc = _get_map_gc ( ) ; 
     
     return;
     
}
/****************************************************************************/

void drawGenericLegend(GenericLegendDialog *dialog)
{
     Pixel pixel;
     struct _GenericLegendDialog *sDialog = dialog;
   
     
     /*
		Set the drawing area color to the background of the parent.
     */
     
     pixel = GetBackground(XtParent(dialog->drawingArea));
     
     XSetForeground(dialog->display, dialog->gc, pixel);
     mDrawFillBox ( M_LEGEND , 0 , 0 , 0 , dialog->daWidth, dialog->daHeight);
     dialog->drawFunctionPtr(sDialog);
 
    
     return;
 
}

/****************************************************************************/
