/* ***************************************** */
/*      File:           TSCallbacks.c        */
/*      Date:           Jan 2002             */
/*      Author:         Ai Vo                */
/*      Purpose:                             */
/*                                           */
/* ***************************************** */
#include <string.h>
#include <time.h>

#include "TSCallbacks.h"
#include "TSWidget.h"
#include "TSPlots.h"
#include "TSEdit.h"
#include "ifp_atoms.h"



extern ACOLOR  TScolors[18];
extern PLOTMGR  *PlotMgr;


extern int CloseFlag;
   

void create_MainTSDS( ){

    Arg al[30];                /* Arg List */
    int n, width, xPos;
    int plotHeight;
    int bg;
    
    register int ac = 0;       /* Arg Count */
    XmString xmstring;         /* temporary storage for XmStrings */
    char    title_str[120]; 
    char    mytz[15];
    ac = 0;
    TSMainPane = XmCreatePanedWindow ( TSMainDS, "TSMainPane", al, ac); 

    ac = 0;
    XtSetArg(al[ac], XmNpaneMinimum, 25); ac++;
    XtSetArg(al[ac], XmNpaneMaximum, 25); ac++;
    TSMainMB = XmCreateMenuBar (TSMainPane, "TSMainMB", al, ac);
/*  AV turned off Option for now 4/8/02-------
    TSOptionPD =  XmCreatePulldownMenu(TSMainMB, "TSOptionPD", NULL, 0);
    
  
    XtVaCreateManagedWidget("Control",xmCascadeButtonWidgetClass, 
        TSMainMB,XmNsubMenuId, TSOptionPD,NULL);

    ac = 0;
    TSGridOn = XmCreatePushButton ( TSOptionPD, "GridOn", al, ac);
    ac = 0;
    TSGridOff = XmCreatePushButton ( TSOptionPD, "GridOff", al, ac);
    */
  

    plotHeight = MAXHEIGHT/(PlotMgr->nPlots +1 ); 
    bg = getMyColor ( XtDisplay(TSMainPane), "Gray");
    for ( n = 0; n < PlotMgr->nPlots; n++) {
      ac = 0;
      XtSetArg(al[ac], XmNwidth,  MAXWIDTH);   ac++;
      XtSetArg(al[ac], XmNheight, plotHeight); ac++;
      XtSetArg(al[ac], XmNpaneMinimum,  1); ac++;
      TSMainFO[n] = XmCreateForm(TSMainPane, "TSMainFO", al, ac );  
      ac = 0;
      XtSetArg(al[ac], XmNforeground, 0);     ac++;
      XtSetArg(al[ac], XmNbackground, 0);     ac++;
      XtSetArg(al[ac], XmNx,  0);     ac++;
      XtSetArg(al[ac], XmNwidth,  LEFTLBW);   ac++;
      XtSetArg(al[ac], XmNheight, plotHeight); ac++;
      TSMainLDA[n] = XmCreateDrawingArea(TSMainFO[n], "TSMainLDA", al, ac);
      XtManageChild(TSMainFO[n]);
      ac = 0;
      XtSetArg(al[ac], XmNforeground, 0);     ac++;
      XtSetArg(al[ac], XmNbackground, 0);     ac++;
      XtSetArg(al[ac], XmNx,      LEFTLBW); ac++;
      XtSetArg(al[ac], XmNwidth,  DRAWAW);   ac++;
      XtSetArg(al[ac], XmNheight, plotHeight); ac++;
      TSMainDA[n] = XmCreateDrawingArea(TSMainFO[n], "TSMainDA", al, ac);

      ac = 0;
      xPos  =  (LEFTLBW + DRAWAW);
      XtSetArg(al[ac], XmNforeground, bg);     ac++;
      
      XtSetArg(al[ac], XmNbackground, bg);     ac++;
      XtSetArg(al[ac], XmNx,  xPos); ac++;
      XtSetArg(al[ac], XmNwidth,  RGHTLBW);   ac++;
      XtSetArg(al[ac], XmNheight, plotHeight); ac++;
      
      TSMainRFO[n] = XmCreateForm(TSMainFO[n], "TSMainRFO", al, ac); 
      ac=0;
      
      XtSetArg(al[ac],XmNnumColumns, 1); ac++;
      XtSetArg(al[ac], XmNwidth,  RGHTLBW);   ac++;
      XtSetArg(al[ac],XmNorientation, XmVERTICAL);ac++;
      XtSetArg(al[ac],XmNpacking, XmPACK_COLUMN); ac++;
      TSMainRC[n] = XmCreateRowColumn(TSMainRFO[n], "TSMainRC", al, ac); 
    }
   /* -------------------------------- */
   /* Scrollbar and axis in TSMainHSFO */
   /* -------------------------------- */
   ac = 0;
   XtSetArg(al[ac], XmNwidth,  MAXWIDTH);   ac++;
   XtSetArg(al[ac], XmNpaneMinimum, 50); ac++;
   XtSetArg(al[ac], XmNpaneMaximum, 50); ac++;
   TSMainHSFO   = XmCreateForm(TSMainPane, "TSMainHSFO", al, ac ); 
   ac = 0;
   XtSetArg(al[ac], XmNwidth,  DRAWAW);   ac++;
   XtSetArg(al[ac], XmNheight, 30); ac++;
   TSMainAxisDA =  XmCreateDrawingArea(TSMainHSFO, "TSMainAxisDA", al, ac ); 

   ac = 0; 
   XtSetArg(al[ac], XmNx,      LEFTLBW); ac++;
   XtSetArg(al[ac], XmNwidth,  DRAWAW); ac++;
   XtSetArg(al[ac], XmNy,      35); ac++;
   XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
   XtSetArg(al[ac], XmNsliderSize,  5); ac++;
   TSMainHSB = XmCreateScrollBar (TSMainHSFO, "TSMainHSB", al, ac);
   
   /* ---------------------------------- */
   /* Information window in TSMainInfoFO */
   /* ---------------------------------- */
   ac = 0;
   
   XtSetArg(al[ac], XmNpaneMinimum, 30); ac++;
   XtSetArg(al[ac], XmNpaneMaximum, 30); ac++;
   TSMainInfoFO = XmCreateForm(TSMainPane, "TSMainInfoFO", al, ac);
   ac = 0;
   XtSetArg(al[ac], XmNforeground,  0);     ac++;
   XtSetArg(al[ac], XmNbackground,  0);     ac++;
   XtSetArg(al[ac], XmNwidth,  MAXWIDTH);   ac++;
   XtSetArg(al[ac], XmNheight, 30); ac++;
   TSMainInfoDA = XmCreateDrawingArea(TSMainInfoFO, "TSMainInfoDA", al, ac);
   
   ac = 0;
   XtSetArg(al[ac], XmNwidth,  LEFTLBW - 5);   ac++;
   TSEditUndoPB = XmCreatePushButton ( TSMainHSFO, "Undo TS", al, ac);
   XtSetSensitive(TSEditUndoPB, FALSE); 

   
   sprintf(mytz,"%d%s%d %s",PlotMgr->Month,"/",PlotMgr->Year,PlotMgr->TimeZone);
   ac = 0;
   /*XtSetArg(al[ac], XmNx,MAXWIDTH-LEFTLBW); ac++;*/
   
   XtSetArg(al[ac], XmNy, 30); ac++;
   XtSetArg(al[ac], XmNlabelString,
            XmStringCreate(mytz, XmSTRING_DEFAULT_CHARSET)); ac++;
            
   TSMoyrtzLB = XmCreateLabel ( TSMainHSFO, mytz, al, ac );

   for ( n = 0; n < PlotMgr->nPlots; n++) 
      XtVaSetValues (TSMainFO[n], XmNpaneMinimum, 1, NULL);

   for ( n = 0; n < PlotMgr->nPlots; n++) {
       ac = 0;
       XtSetArg(al[ac], XmNtopAttachment,    XmATTACH_FORM); ac++;
       XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
       XtSetArg(al[ac], XmNrightAttachment,  XmATTACH_FORM); ac++;
       XtSetValues ( TSMainRFO[n],al, ac ); 

       ac = 0;
       XtSetArg(al[ac], XmNtopAttachment,    XmATTACH_FORM); ac++;
       XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
       XtSetArg(al[ac], XmNrightAttachment,  XmATTACH_WIDGET); ac++;
       XtSetArg(al[ac], XmNrightWidget,      TSMainRFO[n]); ac++;
       XtSetArg(al[ac], XmNleftAttachment,   XmATTACH_WIDGET); ac++;
       XtSetArg(al[ac], XmNleftWidget,       TSMainLDA[n]); ac++;
       XtSetValues ( TSMainDA[n],al, ac ); 

       ac = 0;
       XtSetArg(al[ac], XmNtopAttachment,    XmATTACH_FORM); ac++;
       XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
       XtSetArg(al[ac], XmNleftAttachment,   XmATTACH_FORM); ac++;
       XtSetValues ( TSMainLDA[n],al, ac ); 
       
       ac = 0;
       XtSetArg(al[ac], XmNrightAttachment,  XmATTACH_FORM); ac++; 
       XtSetValues ( TSMainRC[n],al, ac ); 
   }
  
    
                                         
    /* ----------------------------------------------- */
    /* -----------  Axis display drawing area  ------- */
    /* ----------------------------------------------- */
    ac=0; 
    XtSetArg(al[ac], XmNleftAttachment,  XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNleftOffset,      LEFTLBW); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNrightOffset,     RGHTLBW); ac++;
    XtSetValues ( TSMainAxisDA,al, ac ) ;


    /* ----------------------------------------------- */
    /* -----------  Horizontal Scrollbar offset ------- */
    /* ----------------------------------------------- */
    ac=0;
    XtSetArg(al[ac], XmNtopAttachment,    XmATTACH_WIDGET);
    XtSetArg(al[ac], XmNtopWidget,        TSMainAxisDA); ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNleftAttachment,   XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNleftOffset,       LEFTLBW); ac++;
    XtSetArg(al[ac], XmNrightAttachment,  XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNrightOffset,      RGHTLBW); ac++;   
    XtSetValues ( TSMainHSB,al, ac );

    /* -----------  Display cross hairs/date Info. Form  ---------- */
    ac=0;
    XtSetArg(al[ac], XmNleftAttachment,  XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetValues ( TSMainInfoDA,al, ac );

    for ( n = 0; n < PlotMgr->nPlots; n++){
       XtManageChild(TSMainLDA[n]);
       XtManageChild(TSMainDA[n]);
       XtManageChild(TSMainRFO[n]);
    }
 
    XtManageChild(TSMainHSFO);
    XtManageChild(TSMainAxisDA);
    XtManageChild(TSMainHSB);
    XtManageChild(TSMainMB);
    XtManageChild(TSMainInfoFO);
    XtManageChild(TSMainInfoDA);
 
    /*XtManageChild(TSOptionPD);
   
    XtManageChild(TSGridOn);
    XtManageChild(TSGridOff);*/
     
    XtManageChild(TSEditUndoPB);
    XtManageChild(TSMoyrtzLB);
    
    XtManageChild(TSMainPane);
    
    TSsetUpGC();

}

void TSaddCallbacks( ) {
 
   int n;
 
   for ( n =0; n < PlotMgr->nPlots; n++) {
 
      XtAddCallback ( TSMainDA[n], XmNexposeCallback,  initialExposeCB, (XtPointer )n);
      /* Rubber band handler */ 
      XtAddEventHandler(TSMainDA[n], ButtonPressMask,   False,
                (XtEventHandler)TSPressCB,    (XtPointer )n );
 
      XtAddEventHandler(TSMainDA[n], ButtonReleaseMask, False,
                (XtEventHandler)TSReleaseCB, (XtPointer )n );
 
      XtAddEventHandler(TSMainDA[n], ButtonMotionMask, False,
                (XtEventHandler)TSDragCB,    (XtPointer )n );
      /* TS Edit handler */          
      XtAddEventHandler(TSMainDA[n], ButtonPressMask,   False,
                (XtEventHandler)TSEditPressCB,    (XtPointer )n );
 
      XtAddEventHandler(TSMainDA[n], ButtonReleaseMask, False,
                (XtEventHandler)TSEditReleaseCB, (XtPointer )n );
 
      XtAddEventHandler(TSMainDA[n], ButtonMotionMask, False,
                (XtEventHandler)TSEditDragCB,    (XtPointer )n );
   }
 
   /*XtAddCallback ( TSGridOn,  XmNactivateCallback, TSGridOnCB, NULL);
   XtAddCallback ( TSGridOff, XmNactivateCallback, TSGridOffCB, NULL);*/

   XtAddCallback ( TSEditUndoPB,    XmNactivateCallback, TSEditCancelCB, NULL);  

   XtAddCallback ( TSMainHSB, XmNdragCallback,         scrolled, (XtPointer )XmHORIZONTAL);
   XtAddCallback ( TSMainHSB, XmNvalueChangedCallback, scrolled, (XtPointer )XmHORIZONTAL);
 
}

void TSTraceSelectCB( Widget w,  XtPointer data,  XtPointer cbs) {

     int n, ac, icolor;
     int colorOrg;      
     int tschng_mod_made;
     Arg arg[10];
     static int prev_graph, prev_trace, first = 1;
     GRAPH *graph, *pgraph;
     TRACE *trace, *ptrace;
    
     PTINFO *pinfo = (PTINFO  *)data;
     graph  = (GRAPH *)&PlotMgr->Graph[pinfo->active_graph];
     trace  = (TRACE *)&graph->traces[pinfo->active_trace];
     
     
     if ( PlotMgr->EditActive == 1) {
        if (  pinfo->active_graph !=  PlotMgr->edit_graph  ) {
 
           /*printf("TSTRACE : INVALID MOVE - CURRENT EDIT GRAPH IS NOT DONE  ...\n");*/
           return;
        }
 
     }

     
     /* ----------------------------------------------------- */
     /* Current Trace is now saved by second click 03/22/2002 */
     /* restore original color to legend/trace                */
     /* ----------------------------------------------------- */
     if ( strcmp(trace->colorName,"White") == 0) {
       
        
        PlotMgr->EditActive = 0;           /* Turn Off Edit */
        PlotMgr->display_crosshairs = 1;   /* Enable crsoohair */
        PlotMgr->active_graph  =  PlotMgr->edit_graph;
        n =  pinfo->active_trace; ac = 0;
        
        icolor = colorLookUp( trace->tmpTraceType); 
        
        sprintf(trace->colorName, "%s",  TScolors[icolor].colorName);
        XtSetArg(arg[ac], XtNforeground, TScolors[icolor].colorPix);ac++;

        XtSetValues( trace->TSLegPB[n], arg, ac);
        
        XtSetSensitive(TSEditUndoPB, FALSE); 
        TSEditSave(); 
        PlotMgr->EditTrace = 0;
        PlotMgr->edit_graph  =  99;
         
     } else {
	if (  pinfo->active_graph ==  PlotMgr->edit_graph && PlotMgr->EditTrace == 1  )return;
        sprintf(trace->colorName, "%s", "White");             /* Active Color */
        PlotMgr->EditActive = 1;                              /* Turn On Edit */
        PlotMgr->edit_graph  =  pinfo->active_graph;           /* Save Edit Graph number */
        PlotMgr->display_crosshairs = 0;                      /* Disable crosshair */
        n =  pinfo->active_trace; 
        
        ac = 0;
        XtSetArg(arg[ac], XtNforeground, PlotMgr->active_color);ac++;
        XtSetValues( trace->TSLegPB[n], arg, ac);
        XtSetSensitive(TSEditUndoPB, TRUE);
      
     }

     /* -------------------------------------------- */
     /* when user switching plots from current plots */
     /* resstore previous color  for active trace    */
     /* -------------------------------------------- */
     if (  (prev_graph != pinfo->active_graph ||
            prev_trace != pinfo->active_trace) && first == 0) { 

        n = prev_trace; ac = 0;
        pgraph  = (GRAPH *)&PlotMgr->Graph[prev_graph];
        ptrace  = (TRACE *)&pgraph->traces[n];

        icolor = colorLookUp( ptrace->tmpTraceType); 
        sprintf(ptrace->colorName, "%s", TScolors[icolor].colorName);
        XtSetArg(arg[ac], XtNforeground, TScolors[icolor].colorPix);ac++;

        XtSetValues( ptrace->TSLegPB[n], arg, ac);

        if (  prev_graph != pinfo->active_graph ) {

           reDrawGraph ( pgraph );

        }
     }

     prev_graph =  pinfo->active_graph;
     prev_trace =  pinfo->active_trace;
     
     
     PlotMgr->active_graph = prev_graph;
     PlotMgr->active_trace = prev_trace;
          
     first = 0; ac = 0;
    
     reDrawGraph ( graph );

}

void TSResetGraph(GRAPH *graph){
     int n, ac, icolor;
     Arg arg[10];
     
     TRACE *trace, *ptrace;

   
     n = PlotMgr->active_trace;
     trace  = (TRACE *)&graph->traces[n];
     ac = 0;
     icolor = colorLookUp( trace->tmpTraceType); 
     sprintf(trace->colorName, "%s", TScolors[icolor].colorName);
     XtSetArg(arg[ac], XtNforeground,TScolors[icolor].colorPix);ac++;
     XtSetValues( trace->TSLegPB[n], arg, ac);
        
}

void TSCloseCB( Widget w, XtPointer data,  XtPointer cbs) {

   XtUnmapWidget(TSMainDS);
}


void TSPressCB( Widget w, XtPointer data,  XEvent *event) {
 
        int n = (int)data;
 
        GRAPH *graph = (GRAPH *)&PlotMgr->Graph[n];

        PlotMgr->active_graph = n;

        /*printf("TSPressCB  Plot#[%d] %d %d\n", (int)data,
                        event->xbutton.x, event->xbutton.y);*/
 
        if ( PlotMgr->EditActive == 1  && n !=  PlotMgr->edit_graph  ) {
 
          /* printf("TSPressCB: Edit in not done \n");*/
           return;
        }
        
        if (  event->xbutton.x < graph->x )  event->xbutton.x = graph->x ;
        if (  event->xbutton.x > graph->w )  event->xbutton.x = graph->w ; 
        if (  event->xbutton.y < graph->y )  event->xbutton.y = graph->y ;
        if (  event->xbutton.y > graph->h )  event->xbutton.y = graph->h ; 
        PlotMgr->last_x = event->xbutton.x;
        PlotMgr->last_y = event->xbutton.y;
 
        drawing_crosshairs();
 
}

void TSReleaseCB( Widget w, XtPointer data,  XEvent *event) {

        int n = (int)data;

        GRAPH *graph = (GRAPH *)&PlotMgr->Graph[n];

	/*printf("TSRelease Plot# : %d\n", n);*/
        if ( PlotMgr->EditActive == 1 && n !=  PlotMgr->edit_graph  ) {
 
           /*printf("TSReleaseCB: Edit in not Done \n");*/
           return;
        }

        /* Zoom's 2nd cursor's position
	PageMgr->x2 = PageMgr->last_x = event->xbutton.x;
        PageMgr->y2 = PageMgr->last_y = event->xbutton.y; 
	*/

        /* Ai Added 04/02/2002 */
        if (  event->xbutton.x < graph->x )  event->xbutton.x = graph->x ;
        if (  event->xbutton.x > graph->w )  event->xbutton.x = graph->w ; 
        if (  event->xbutton.y < graph->y )  event->xbutton.y = graph->y ;
        if (  event->xbutton.y > graph->h )  event->xbutton.y = graph->h ; 

 	PlotMgr->last_x = event->xbutton.x;
        PlotMgr->last_y = event->xbutton.y;
        drawing_crosshairs(); 
        clearDrawAInfo ();
}

void TSDragCB( Widget w, XtPointer data,  XEvent *event) {


        int n = (int)data;

        GRAPH *graph  = (GRAPH *) &PlotMgr->Graph[n];

        if ( PlotMgr->EditActive == 1 && n !=  PlotMgr->edit_graph  ) {
 
           /*printf("TSDragCB: Edit in not Done \n");*/
           return;
        }

   
        if (  event->xbutton.x < graph->x )  event->xbutton.x = graph->x ;
        if (  event->xbutton.x > graph->w )  event->xbutton.x = graph->w ; 
        if (  event->xbutton.y < graph->y )  event->xbutton.y = graph->y ;
        if (  event->xbutton.y > graph->h )  event->xbutton.y = graph->h ; 

        drawing_crosshairs();  
 	PlotMgr->last_x = event->xbutton.x;
        PlotMgr->last_y = event->xbutton.y;
        drawing_crosshairs();  
        search_and_display_trace( event->xbutton.x,  event->xbutton.y, 0);

}


void TSPointCB( Widget w, XtPointer data,  XtPointer cbs) {

   PlotMgr->trace_mode = 0;
   reDrawAll();

}


void TSLineCB( Widget w, XtPointer data,  XtPointer cbs) {

   PlotMgr->trace_mode = 1;
   reDrawAll();

}

void TSLPointCB( Widget w, XtPointer data,  XtPointer cbs) {

   PlotMgr->trace_mode = 2;
   reDrawAll();

}
void TSGridOnCB( Widget w, XtPointer data,  XtPointer cbs) {

   PlotMgr->GridOn = 1;
   reDrawAll();

}
void TSGridOffCB( Widget w, XtPointer data,  XtPointer cbs) {

   PlotMgr->GridOn = 0;
   reDrawAll();

}

void TSEditCB( Widget w, XtPointer data,  XtPointer cbs) {

   PlotMgr->EditActive = 1;
   PlotMgr->display_crosshairs = 0;

}
void TSEditSaveCB( Widget w, XtPointer data,  XtPointer cbs) {

   PlotMgr->EditActive = 0;
   PlotMgr->display_crosshairs = 1;
}
void TSEditCancelCB( Widget w, XtPointer data,  XtPointer cbs) {

   GRAPH *gptr;
 
   TRACE *tptr;

   Arg   arg[5];

   int   n, ac, icolor;

   int active_graph = PlotMgr->active_graph;
   int active_trace = PlotMgr->active_trace;
   PlotMgr->EditActive = 0;           /* Turn Off Edit */
   PlotMgr->display_crosshairs = 1;   /* Enable crsoohair */

   gptr  = (GRAPH *)&PlotMgr->Graph[active_graph];
   tptr  = (TRACE *)&gptr->traces[active_trace];

   n =  active_trace; ac = 0;
   icolor = colorLookUp( tptr->tmpTraceType); 
   sprintf(tptr->colorName, "%s",   TScolors[icolor].colorName);
   XtSetArg(arg[ac], XtNforeground, TScolors[icolor].colorPix);ac++;
   XtSetValues( tptr->TSLegPB[n], arg, ac);
   
   TSEditUndo();
   XtSetSensitive(TSEditUndoPB, FALSE);
 

}

void initialExposeCB ( Widget w, XtPointer data, XtPointer cbsEXP)
{
 
  int n;
 
 
   XmDrawingAreaCallbackStruct *cbs =
        ( XmDrawingAreaCallbackStruct *) cbsEXP;
 
   if ( cbs->event->xexpose.count !=0) return;
 
   n =  (int)(data);
   
   XtRemoveCallback (w, XmNexposeCallback, initialExposeCB, (XtPointer )n );
 
   XtAddCallback ( w, XmNexposeCallback,  exposeCB, (XtPointer )n);
 
   XtAddCallback ( w, XmNresizeCallback,  resizeCB, (XtPointer )n);
  
}


void exposeCB ( Widget w, XtPointer data, XtPointer cbsEXP) {
 
   int n;
 
   GRAPH  *graph;
   
   static char first = 1;
  
   XmDrawingAreaCallbackStruct *cbs =
        ( XmDrawingAreaCallbackStruct *) cbsEXP;
 
   if ( cbs->event->xexpose.count !=0) return;
 
   n = (int)data;
 
   graph = (GRAPH *)&PlotMgr->Graph[n];
   
   /* ----------------------------------- */
   /* Update graph's demension            */
   /* ----------------------------------- */
   updateGraphDimension ( graph ) ;
   reDrawGraph ( graph );
  
   /* ----------------------------------- */
   /* Copy Pixmap to windows              */
   /* ----------------------------------- */
  
   for(n=0; n<PlotMgr->nPlots; n++)
   {
      graph = (GRAPH *)&PlotMgr->Graph[n];
      
      XCopyArea(graph->display, graph->windowDAPixmap,
             graph->window, graph->gc_copy,
             0, 0, graph->orgW, graph->orgH, 0, 0);
 
      XCopyArea(graph->display, graph->window_xLBPixmap,
             PlotMgr->windowAxisDA, graph->gc_copy,
             0, 0, PlotMgr->axis_w,PlotMgr->axis_h, 0, 0);
 
      XCopyArea(graph->display, graph->window_yLBPixmap,
             graph->windowyLB, graph->gc_copy,
             0, 0, graph->yaxis_width, graph->yaxis_height, 0, 0);
   }

   /* Clear the Info. Drawing Area */
   clearDrawAInfo();
  

}


void resizeCB ( Widget w, XtPointer data, XtPointer cbs) {
 
   int n;
 
   GRAPH *graph;
 
   n =  (int)(data);
 
   graph = (GRAPH *)&PlotMgr->Graph[n];
   updateGraphDimension ( graph ) ;
   reDrawGraph ( graph );
 
}


void scrolled ( Widget scrollbar, XtPointer data, XtPointer cbsSB) {
 
   struct tm   *tm_ptr;
   GRAPH       *graph;
   char        tbuf[80];
   int         n;
 
 
   XmScrollBarCallbackStruct *cbs =
        ( XmScrollBarCallbackStruct *) cbsSB;
 
   if (cbs->reason ==   XmCR_VALUE_CHANGED)  {
 
     PlotMgr->xmin =  PlotMgr->org_xmin + cbs->value *PlotMgr->idtp*3600;
/*     PlotMgr->xmax =  PlotMgr->xmin + PlotMgr->duration*SECONDS_PER_DAY;*//*kwz*/
     PlotMgr->xmax =  PlotMgr->xmin + PlotMgr->duration*SECONDS_PER_HOUR;

     /* printf("Slider Drag Value = %d\n", cbs->value); */

      for ( n = 0; n < PlotMgr->nPlots; n++) { /* Testing */
         graph = (GRAPH *)&PlotMgr->Graph[n];
         graph->xmin = PlotMgr->xmin;
         graph->xmax = PlotMgr->xmax;
         reDrawGraph ( graph);
      }
 
   }else if (cbs->reason == XmCR_DRAG) {
 
        PlotMgr->xmin =  PlotMgr->org_xmin + cbs->value*PlotMgr->idtp*3600;
/*        PlotMgr->xmax =  PlotMgr->xmin + PlotMgr->duration*SECONDS_PER_DAY;*//*kwz*/
        PlotMgr->xmax =  PlotMgr->xmin + PlotMgr->duration*SECONDS_PER_HOUR;
      for ( n = 0; n < PlotMgr->nPlots; n++) { /* Testing */
         graph = (GRAPH *)&PlotMgr->Graph[n];
         graph->xmin = PlotMgr->xmin;
         graph->xmax = PlotMgr->xmax;
         reDrawGraph ( graph);
      }

   }
 
    tm_ptr = gmtime(&PlotMgr->xmin);
    strftime(tbuf, sizeof(tbuf), "%m/%d/%y", tm_ptr);
   
    /* Display Start Time */
    TSSetColor(PlotMgr->gc,  PlotMgr->widgetAxisDA, "white");
    XFillRectangle(PlotMgr->display, PlotMgr->windowInfoDA, PlotMgr->gc, 0, 0,
                                        100, PlotMgr->Info_h);
    TSSetColor(PlotMgr->gc,  PlotMgr->widgetAxisDA, "Blue");
    XDrawString(PlotMgr->display, PlotMgr->windowInfoDA, PlotMgr->gc, 2, 20,
				tbuf, strlen(tbuf));

    
}

void TSFreeResources( ){

    GRAPH *graph;

    TRACE *tptr;

    int n, m;


    for ( n = 0; n < PlotMgr->nPlots; n++) {

       graph  = (GRAPH *)&PlotMgr->Graph[n];

       if ( graph->window_yLBPixmap) {
          XFreePixmap(graph->display, graph->window_yLBPixmap);
          XFreePixmap(graph->display, graph->windowDAPixmap);
          XFreePixmap(graph->display, graph->window_xLBPixmap);
          graph->window_yLBPixmap = (Pixmap)0;
          graph->windowDAPixmap   = (Pixmap)0;
          graph->window_xLBPixmap = (Pixmap)0;
          XFreeFont(graph->display, graph->fs);
          XFreeGC(graph->display, graph->gc_line);
          XFreeGC(graph->display, graph->gc_point);
          XFreeGC(graph->display, graph->gc_grid);
          XFreeGC(graph->display, graph->gc_bg);
          XFreeGC(graph->display, graph->gc_header);
          XFreeGC(graph->display, graph->gc_copy);

       }

     /*
      for ( m = 0; m < graph->ntraces; m++) {
 
          tptr = (TRACE *)&graph->traces[m];
          tptr->npts =0;

       }

       graph->ntraces = 0; */

    }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTS/RCS/TSCallbacks.c,v $";
 static char rcs_id2[] = "$Id: TSCallbacks.c,v 1.3 2002/10/10 16:30:33 dws Exp $";}
/*  ===================================================  */

}

