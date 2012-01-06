/* ********************************************* */
/*      File:           TSedit.c                 */
/*      Date:           March   2002             */
/*      Author:         Ai   Vo                  */
/*      Purpose:                                 */
/*                                               */
/* ********************************************* */
#include "TSEdit.h"
#include "ifp_atoms.h"

extern PLOTMGR *PlotMgr;

struct  _move_info 
{
  int     valid, mouseClick; 
  int     lindex, rindex, cindex; 

  int     x,  y,
          x1, y1,
          x2, y2;
 
  int     trace_index,
          num_legs;
 
  time_t  xvalue;
 
} move_info = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0};  


/* ********************************************************** */
/* handle press event when Edit mode active                   */
/* ********************************************************** */
void TSEditPressCB (Widget w, XtPointer ptr, XEvent *event)
{

   GRAPH *gptr;
   TRACE *tptr;

   int tindex;

   int active_graph = PlotMgr->active_graph;
   int active_trace = PlotMgr->active_trace; 

   gptr  = (GRAPH *)&PlotMgr->Graph[active_graph];  
   tptr  = (TRACE *)&gptr->traces[active_trace];

   move_info.valid = 0;

   PlotMgr->EditTrace = 0;

   if ( PlotMgr->EditActive == 0)  move_info.mouseClick = 0;
   if(event->xbutton.button != Button1 || PlotMgr->EditActive == 0) {

     return;
   }

  /* printf(" ========> A/E %d %d\n", active_graph, PlotMgr->edit_graph);*/
   if ( active_graph !=  PlotMgr->edit_graph &&  PlotMgr->EditActive == 1) {

       /*printf("INVALID MOVE - CURRENT EDIT GRAPH IS NOT DONE  ...\n");*/
       return;

   }
   move_info.mouseClick++;   

   tindex = TSEditMoveIndex ( event->xbutton.x, event->xbutton.y);
   
   if ( tindex != (-1)) {
       
       move_info.valid = 1;

   } else {

       /*printf("INVALID CLICK .................\n");*/
       move_info.mouseClick =0;
       
       return;

   }
  
   move_info.cindex = tindex;
   /*move_info.lindex = 0; AV*/
   
   if (  move_info.mouseClick == 1)  {
       move_info.lindex = tindex;
       
   }
   if (  move_info.mouseClick == 2)  {
       
       ;;
       /* move_info.rindex = tindex; do nothing here */
   }

   /*printf("LRINDEX : %d %d %d\n",move_info.lindex, move_info.rindex, move_info.mouseClick);*/
   if ( move_info.rindex >= (tptr->npts - 1))move_info.rindex =  (tptr->npts - 1);
   if ( move_info.lindex <= 0)move_info.lindex =  0;


   move_info.y =  event->xbutton.y;
   draw_move( gptr );
}



/* ************************************************************ */
/* handle release events during editing mode  - release event   */
/* here is to share with release event from zoom so only one    */
/* is allowed at the time                                       */
/* ************************************************************ */
void TSEditReleaseCB (Widget w, XtPointer ptr, XEvent *event)
{

     GRAPH *gptr;
     TRACE *tptr;
     int tschng_mod_made;
     int active_graph = PlotMgr->active_graph;
     int active_trace = PlotMgr->active_trace; 

     gptr  = (GRAPH *)&PlotMgr->Graph[active_graph];  
     tptr  = (TRACE *)&gptr->traces[active_trace];
     
     if( ! move_info.valid ) return;

     if ( move_info.mouseClick >= 2 ) {
        if( move_info.cindex >  move_info.lindex) { /* L2R */
           TSEditMoveUpdate( gptr );   /* update last y position */
           move_info.rindex = move_info.cindex;
	   TSInterpolate( tptr );
           move_info.lindex = move_info.cindex;
        } else if ( move_info.cindex < move_info.lindex)  { /* R2L */
           move_info.lindex = move_info.cindex;
        } 

        move_info.mouseClick = 1;
     } 

     move_info.y =  event->xbutton.y;

     draw_move( gptr );
 
     TSEditMoveUpdate( gptr );

     reDrawGraph( gptr );
     PlotMgr->EditTrace = 1;
     
     tschng_mod_made = TRUE;
     XChangeProperty(
	XtDisplay(TSEditUndoPB),
	DefaultRootWindow(XtDisplay(TSEditUndoPB)),
	IFPA_tschng_mod,
	IFPA_tschng_mod_type,
	8,
	PropModeReplace,
	(unsigned char *)&tschng_mod_made,
	sizeof(int)
	); 
     

}

void TSInterpolate( TRACE *tptr)
{

    
    float deltay;

    float mSlope, Fks, Fes;

    int   i, i1, i2;

    i1 =  move_info.lindex;
    i2 =  move_info.rindex;

    deltay =  (tptr->TSData[i2].y -  tptr->TSData[i1].y);

    for ( i = move_info.lindex; i < move_info.rindex; i++){

        Fks = (i - move_info.lindex);
        Fes =  (move_info.rindex -  move_info.lindex);
        mSlope =  Fks/Fes; 
        tptr->TSData[i].y  = tptr->TSData[move_info.lindex].y + deltay*mSlope;
        tptr->TSData[i].status = TSMOVE;
    }
   
} 

/* ********************************************************** */
/* handle drags event during Editing mode - this event is     */
/* also shared with drags event from crosshairs event         */
/* ********************************************************** */
void TSEditDragCB (Widget w, XtPointer ptr, XEvent *event)
{
 
     GRAPH *gptr;
 
     int active_graph = PlotMgr->active_graph;
 
     gptr  = (GRAPH *)&PlotMgr->Graph[active_graph];
 
     if( ! move_info.valid ) return;
 
     draw_move( gptr );
 
     move_info.y =  event->xbutton.y;

     draw_move( gptr );
 

}

/* *********************************** */
/* Update TSData  when button release  */
/* *********************************** */
void TSEditMoveUpdate( GRAPH *graph)
{
    TRACE *tptr;
 
    int   trace_index, active_trace;
 
    float   yvalue;
 
    yvalue = pixel2y ( graph, move_info.y);
 
    trace_index  = move_info.trace_index;
 
    active_trace = PlotMgr->active_trace;
 
    tptr = (TRACE *) &graph->traces[active_trace];
 
    tptr->TSData[trace_index].y = yvalue;
 
    tptr->TSData[trace_index].status = TSMOVE;
    /*printf("tptr->TSData[trace_index].status= %d\n",tptr->TSData[trace_index].status);*/
    
    
 
 
}

/* ************************* */
/*  draw move must use gcXOR */
/* ************************* */
void draw_move( GRAPH *g)
{
   extern PLOTMGR   *PlotMgr;
   
   if ( move_info.num_legs == 1 || move_info.num_legs == 2){
      XDrawLine(g->display, g->window, g->gc_Xor,
                move_info.x2, move_info.y2, move_info.x, move_info.y);
   }  
   if ( move_info.num_legs == 2){
      XDrawLine(g->display, g->window, g->gc_Xor,
                move_info.x, move_info.y, move_info.x1, move_info.y1);
   }
   XDrawArc( g->display, g->window, g->gc_Xor,
        move_info.x-2, move_info.y-2, 4, 4, 0, 360*64);
}

int  TSEditMoveIndex ( int xClick, int yClick )
{

     GRAPH *gptr;

     TRACE *tptr;

     float   yvalue;

     int    active_graph, active_trace, tindex; 

     int     n, xleft, xright;
     int     msng_flg ;

     active_graph = PlotMgr->active_graph;
     active_trace = PlotMgr->active_trace;

     gptr  = (GRAPH *)&PlotMgr->Graph[active_graph];  
     tptr  = (TRACE *)&gptr->traces[active_trace]; 

     for ( n = 0; n < (tptr->npts - 1 ); n++) {
        msng_flg = 0;
        if (tptr->TSData[n].x <  gptr->xmin || tptr->TSData[n].x >  gptr->xmax ) continue;
        if(  tptr->TSData[n].y != -999.0){
             if( tptr->TSData[n].y < gptr->ymin || tptr->TSData[n].y > gptr->ymax) continue;
        }
        else{
            move_info.num_legs = 0;
            msng_flg = 1;  
                      
        }
        xleft  = x2pixel ( gptr , tptr->TSData[n].x);
        xright = x2pixel ( gptr , tptr->TSData[n+1].x);

        if (  xClick >  xleft && xClick < xright ) {

           tindex = n;
           move_info.trace_index = n;
           yvalue = pixel2y ( gptr, yClick);
           /* handle missing data */
           if(msng_flg == 1) {
              move_info.x  = x2pixel ( gptr , tptr->TSData[n].x);
              return (tindex);
           }
           if (  tindex == 0 ) { 

              move_info.x  = x2pixel ( gptr , tptr->TSData[0].x);
              move_info.x2 = x2pixel ( gptr , tptr->TSData[1].x);
              move_info.y2 = y2pixel ( gptr , tptr->TSData[1].y);
              move_info.num_legs = 1;

           } else if (  tindex == (tptr->npts - 1 ) ) { 

              move_info.x2 = x2pixel ( gptr , tptr->TSData[tindex].x);
              move_info.y2 = y2pixel ( gptr , tptr->TSData[tindex].y);
              move_info.num_legs = 1;

           }else {

              move_info.x  = x2pixel ( gptr , tptr->TSData[tindex].x);
              move_info.x1 = x2pixel ( gptr , tptr->TSData[tindex-1].x);
              move_info.y1 = y2pixel ( gptr , tptr->TSData[tindex-1].y);
              move_info.x2 = x2pixel ( gptr , tptr->TSData[tindex+1].x);
              move_info.y2 = y2pixel ( gptr , tptr->TSData[tindex+1].y);
              move_info.num_legs = 2;
           }

           return ( tindex);

        }


     } /* End of Trace loop */

    return ( -1 );
}

/* ----------------------------- */
/* Save Edit data                */
/* ----------------------------- */
void TSEditSave(){

     GRAPH *gptr;

     TRACE *tptr;

     int n;
    

     int active_graph = PlotMgr->active_graph;
     int active_trace = PlotMgr->active_trace;

     gptr  = (GRAPH *)&PlotMgr->Graph[active_graph];  
     tptr  = (TRACE *)&gptr->traces[active_trace]; 
    
     for ( n = 0; n < tptr->npts; n++) { 

        if (  tptr->TSData[n].status == TSMOVE){
            
            tptr->TSData[n].yedit = tptr->TSData[n].y;
            move_info.mouseClick=0;
            move_info.lindex = 0;move_info.rindex=0;
           /* move_info.valid = 0; AV added 4/10 */
        }
     }
    

}
/* ----------------------------- */
/* Undo Edit data                */
/* ----------------------------- */
void TSEditUndo(){

     GRAPH *gptr;

     TRACE *tptr;

     int n;
     int tschng_mod_made;
      
     int active_graph = PlotMgr->active_graph;
     int active_trace = PlotMgr->active_trace;
     
     gptr  = (GRAPH *)&PlotMgr->Graph[active_graph];  
     tptr  = (TRACE *)&gptr->traces[active_trace]; 
    
     for ( n = 0; n < tptr->npts; n++) { 
     

        if (  tptr->TSData[n].status == TSMOVE){
            
            tptr->TSData[n].y = tptr->TSData[n].yOrg;
            tptr->TSData[n].yedit = tptr->TSData[n].yOrg;
            tptr->TSData[n].status = 0;
          
            
        }

     }

     reDrawGraph( gptr );
     tschng_mod_made = FALSE;
        XChangeProperty(
	XtDisplay(TSEditUndoPB),
	DefaultRootWindow(XtDisplay(TSEditUndoPB)),
	IFPA_tschng_mod,
	IFPA_tschng_mod_type,
	8,
	PropModeReplace,
	(unsigned char *)&tschng_mod_made,
	sizeof(int)
	);
     PlotMgr->EditTrace = 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTS/RCS/TSEdit.c,v $";
 static char rcs_id2[] = "$Id: TSEdit.c,v 1.3 2006/04/07 16:00:08 aivo Exp $";}
/*  ===================================================  */

}
