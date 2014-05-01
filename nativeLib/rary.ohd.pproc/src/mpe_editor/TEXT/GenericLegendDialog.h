#ifndef GENERIC_LEGEND_DIALOG
#define GENERIC_LEGEND_DIALOG

#define MAX_LEGEND_DIALOGS 30


#include "Xtools.h"
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/LabelG.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/CascadeBG.h>
#include <Xm/Separator.h>
#include <Xm/Protocols.h>


typedef struct _GenericLegendDialog
{
   Widget parent;
   
   Widget shell;
   Widget form;
   Widget closePB;
   Widget drawingArea;
   

   /*
	Display and Screen
   */
   Display	*display;
   Screen	*screen;
   GC		gc;
   Window	window;
   Pixmap 	pixmap;
 
   
   char 	title[BUFSIZ];
   
   
   Dimension    dialogWidth;
   Dimension    dialogHeight;
   
   Dimension    daWidth;
   Dimension    daHeight;
   
   void ( * drawFunctionPtr ) (struct _GenericLegendDialog *);
   void * argument;
   
} GenericLegendDialog;


/*
	prototypes
*/


void startGenericLegendDialog(Widget parent,
			      char * title,
			      Dimension dialogHeight,
			      Dimension dialogWidth,
			      void (* drawFunctionPtr)(struct _GenericLegendDialog *),
			      void *argument);


void initGenericLegendDialog(GenericLegendDialog *dialog,
			     Widget parent,
			     char *title,
			     Dimension dialogHeight,
			     Dimension dialogWidth,
			     void (* drawFunctionPtr)(struct _GenericLegendDialog *),
			     void *argument);

void showGenericLegendDialog(GenericLegendDialog *dialog);
			  
			     
void createGenericLegendDialog(GenericLegendDialog *dialog);


void addGenericLegendDialogCallbacks(GenericLegendDialog *dialog);
void gldCancel(GenericLegendDialog *dialog);
void gldInitGraphicsMembers(GenericLegendDialog *dialog);

void redrawGenericLegend(GenericLegendDialog *dialog);
void drawGenericLegend(GenericLegendDialog *dialog);

/*
	callback prototypes
*/

void gldCancelCallback(Widget w,  XtPointer ptr,  XtPointer cbs);
void gldRedrawCallback(Widget w,  XtPointer ptr,  XtPointer cbs);
void redrawGenericLegendCallback(Widget w,  XtPointer ptr,  XtPointer cbs);


#endif
