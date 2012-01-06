#include "TSPlots.h"
#include "TSWidget.h"
#include "TSCallbacks.h"
#include "TSUtils.h"
#include "libXs.h"
/* --------------------------------------- */
/* ----I N V O K E  TS P L O T M G R ----- */
/* --------------------------------------- */
PLOTMGR  *PlotMgr;
XtAppContext app_context;
extern Widget   global_toplevel;

ACOLOR  TScolors[19] =
{
   { 0, "PELE","Magenta"} ,
   { 0, "QINE","Magenta"} ,
   { 0, "RQIE","Magenta"} ,
   { 0, "DQIE","Magenta"} ,
   { 0, "SPEL","SkyBlue"} ,
   { 0, "SQIN","SkyBlue"} ,
   { 0, "SDQI","SkyBlue"} ,
   { 0, "SQME","SkyBlue"} ,
   { 0, "OBSE","Yellow"} ,
   { 0, "EXT1","SkyBlue"} ,
   { 0, "EXT2","LimeGreen"} ,
   { 0, "EXT3","Salmon"} ,
   { 0, "EXT4","Cyan"} ,
   { 0, "EXT5","Green"} ,
   { 0, "EXT6","Purple"} ,
   { 0, "EXT7","Maroon"} ,
   { 0, "EXT8","Red"} ,
   { 0, "EXT9","BlueViolet"},
   { 0, "EXTX","Black"} 
    
};


void show_MainTSDS() {

  int argc; 

  char *argv[] = {"TSMain"};


  char    TSTitle_str[120]; 

  Arg    wargs[10]; 

  int    n;
  int    j, k, m, found;
  float newymin;
  float newymax;
  float ymin;
  float ymax;
  float finc;
  

  Display   *display;
  GRAPH     *graph;
  XEvent    event;  
  TRACE     *tptr;
  argc = 1;
  
  
     XtToolkitInitialize();
   /* 
    app_context = XtCreateApplicationContext();
     display = XtOpenDisplay(app_context, NULL, argv[0], "XMclient",
                            NULL, 0, &argc, argv);

     if (display == NULL) {
        fprintf(stderr, "%s:  Can't open display\n", argv[0]);
        exit(1);
     }*/
  
     strcpy(TSTitle_str,"Plot TS - ");
     strncat(TSTitle_str,PlotMgr->OperationName,strlen(PlotMgr->OperationName)+ 3);
     strncat(TSTitle_str,PlotMgr->SegName,strlen(PlotMgr->SegName)+ 3);
     strncat(TSTitle_str,PlotMgr->Description,strlen(PlotMgr->Description));
  

     n = 0;
     XtSetArg(wargs[n], XmNgeometry, "500x500+100+100"); n++;
     XtSetArg(wargs[n], XmNallowShellResize, True);  n++; 
     XtSetArg(wargs[n], XmNtitle, TSTitle_str); n++;
   
     TSMainDS = XtCreateApplicationShell(argv[0], transientShellWidgetClass, wargs, n);       
     /* Must realize widget shell here */
     XtRealizeWidget(TSMainDS);   
     create_MainTSDS( TSMainDS );
     TSaddCallbacks();
     XSelectInput(XtDisplay(TSMainDS), XtWindow(TSMainDS),
	       StructureNotifyMask);

     XSelectInput(XtDisplay(TSMainDS),
		DefaultRootWindow(XtDisplay(TSMainDS)),
				  PropertyChangeMask);

 for (n = 0; n < PlotMgr->nPlots; n++){
    XtManageChild(TSMainRC[n]);
    XtManageChild(TSMainRFO[n]);
    XtManageChild(TSMainFO[n]);
 }

 XtVaSetValues(PlotMgr->TShsb,
               XmNmaximum,        PlotMgr->num_tics,
               XmNminimum,        0,
               XmNincrement,      PlotMgr->slider_incr,
               XmNsliderSize,     PlotMgr->slider_size,
               XmNpageIncrement,  PlotMgr->slider_size,
               XmNvalue,          0,
               NULL);

 for ( n = 0; n < 18; n++) {

   TScolors[n].colorPix =  getMyColor(PlotMgr->display, TScolors[n].colorName);
   

 }

 
 for ( n = 0; n <  PlotMgr->nPlots; n++) {
 
   graph = (GRAPH *)&PlotMgr->Graph[n];
   graph->number = n;
   ymin = graph->ymin;
   ymax = graph->ymax;
   
   adjust_ymaxmin(graph->ymin, graph->ymax, &graph->ymin, &graph->ymax, &graph->data_inc);
  
   k = 9;
   for (j =0 ; j < graph->ntraces ; j++){

      tptr = (TRACE *)&graph->traces[j] ;
      
      if (tptr->ObsFlag == 1) {
           sprintf(tptr->tmpTraceType,"%s","OBSE");
      }
      
      found = 0;
      for (m = 0; m < 9 ; m++){
         
         if ( strcmp( tptr->tmpTraceType, TScolors[m].cType) == 0){
             found = 1;
             break;
         }
      }
     
      if(found == 0){
          sprintf(tptr->tmpTraceType,"%s",TScolors[k++].cType);
     
      }
      
      
   }
 }
 
 draw_xaxis( graph );
 
 
  /* Add Legends to graph */
 
 for ( n = 0; n <  PlotMgr->nPlots; n++) {
 
     graph = (GRAPH *)&PlotMgr->Graph[n];

     TSAddButton( graph );
     
 }
  
 for ( n = 0; n <  PlotMgr->nPlots; n++) {
     graph = (GRAPH *)&PlotMgr->Graph[n];
     updateGraphDimension ( graph ) ;
     reDrawGraph( graph );
 }
 
 
 

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTS/RCS/show_Plots_TS.c,v $";
 static char rcs_id2[] = "$Id: show_Plots_TS.c,v 1.2 2002/05/16 12:40:21 dws Exp $";}
/*  ===================================================  */

}
