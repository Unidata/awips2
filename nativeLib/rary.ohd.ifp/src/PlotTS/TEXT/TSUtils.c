/* ***************************************** */
/*      File:           TSUtils.c            */
/*      Date:           Jan 2002             */
/*      Author:         Ai Vo                */
/*      Purpose:                             */
/*                                           */
/* ***************************************** */
#include  <string.h>
#include  <strings.h>
#include  <math.h>
#include "MotifHeader.h"
#include "TSDefs.h"
#include "TSPlots.h"
#include "TSUtils.h"
#include "TSCallbacks.h"
#include "show_Ymaxmin.h"


extern PLOTMGR  *PlotMgr;
extern ACOLOR   TScolors[18];

/* ----------------------------------------- */
/* ------------- S E T U P G C ------------- */
/* ----------------------------------------- */
void TSAddButton ( GRAPH *graph ) {

   TRACE *tptr;
   Arg al[10];
   int n, ac, icolor;
   char tmp_str[13];

   char traceLB[25];

   static PTINFO *ptinfo;
   int    bg;

   for ( n = 0; n < graph->ntraces; n++ ){
     tptr = (TRACE *)&graph->traces[n];
     sprintf(traceLB, "%s%s%c%s",tptr->Trace_Title,"(",tptr->Trace_Symbol,")");
     ptinfo = malloc(sizeof(PTINFO));
     ptinfo->active_graph =  graph->number;
     ptinfo->active_trace =  n;
     ac=0;

     XtSetArg(al[ac], XmNwidth,  PERCENT_RGHTLB);ac++;
     XtSetArg(al[ac], XmNmultiClick,  XmMULTICLICK_DISCARD);ac++;
     icolor = colorLookUp( tptr->tmpTraceType);
     if(icolor > 18){
        printf(" Error in set color \n");
        return;
     }
     XtSetArg(al[ac], XtNforeground,  TScolors[icolor].colorPix);ac++;
     tptr->TSLegPB[n] = XtCreateManagedWidget(traceLB, xmPushButtonWidgetClass,
				TSMainRC[graph->number],  al, ac);
     XtAddCallback (tptr->TSLegPB[n], XmNactivateCallback, TSTraceSelectCB, (XtPointer *)ptinfo);
   }


   /* Adding Change Y-Scale Button */
   n = graph->number; ac = 0;
   XtSetArg(al[ac], XmNwidth,    140); ac++;
   XtSetArg(al[ac], XmNheight,   28); ac++;
   XtSetArg(al[ac], XmNrightAttachment,    XmATTACH_FORM); ac++;
   XtSetArg(al[ac], XmNbottomAttachment,   XmATTACH_FORM); ac++;
   XtSetArg(al[ac], XtNforeground,  TScolors[18].colorPix);ac++;

   graph->TSYscalePB[graph->number] = XtCreateManagedWidget(" Change Y-Scale ",
        xmPushButtonWidgetClass, TSMainRFO[n], al, ac);
   XtAddCallback (graph->TSYscalePB[n], XmNactivateCallback,
        show_YmaxminDS, (XtPointer )graph->number);


}

int  colorLookUp ( char *traceType) {
    int n;

    for ( n = 0; n < 18; n++)  {


        if ( strcmp(TScolors[n].cType,traceType) == 0) {

        return n;
        }

    }

}

void TSsetUpGC() {

   int n;

   XGCValues gcvals;

   GRAPH *graph;

   static char *colors[]  = {

      "Yellow", "Magenta", "Sky Blue","Lime Green", "Salmon", "Cyan", "Green",
      "DeepPink","Purple", "Maroon", "BlueViolet", "Orange","Red","White","Black"
   };


   /* ------- Initial Options -------- */

   PlotMgr->GridOn = 0;
   PlotMgr->trace_mode = 2;
   PlotMgr->display_crosshairs;

   PlotMgr->widgetAxisDA  = TSMainAxisDA;

   PlotMgr->widgetInfoDA  = TSMainInfoDA;

   PlotMgr->windowAxisDA  = XtWindow( PlotMgr->widgetAxisDA );

   PlotMgr->windowInfoDA  = XtWindow( PlotMgr->widgetInfoDA );

   PlotMgr->display       = XtDisplay(TSMainAxisDA);

   for ( n = 0; n <  PlotMgr->nPlots; n++) {

      graph = (GRAPH *)&PlotMgr->Graph[n];   /* Pointer to Graph  */

      graph->number      =  n;
      graph->display     =  XtDisplay(TSMainDS);
      graph->widget      = TSMainDA[n];

      graph->widgetyLB   = TSMainLDA[n];

      graph->window      = XtWindow(graph->widget);
      graph->windowyLB   = XtWindow(graph->widgetyLB);


      /* Create GC for graph */

      graph->gc_line   = XCreateGC(graph->display, graph->window, 0, NULL);
      graph->gc_point  = XCreateGC(graph->display, graph->window, 0, NULL);

      graph->gc_grid   = XCreateGC(graph->display, graph->window, 0, NULL);
      graph->gc_bg     = XCreateGC(graph->display, graph->window, 0, NULL);
      graph->gc_header = XCreateGC(graph->display, graph->window, 0, NULL);

      graph->gc_Xor    = xs_create_xor_gc( graph->widget );
      graph->gc_copy   = XCreateGC(graph->display, DefaultRootWindow(graph->display),
                             GCBackground, &gcvals);
      /* fonts: 7x13 6x9 6x13 */

      if( (graph->fs = XLoadQueryFont(graph->display, "6x13")) == NULL) {

          printf("Error in loading TSFont...\n");
          return;
      }

      /* Set Attributes */

       XSetLineAttributes(graph->display,graph->gc_grid, 1,
		LineOnOffDash,CapRound, JoinMiter);

       XSetLineAttributes(graph->display,graph->gc_line, 1,LineSolid,CapRound, JoinRound);
       XSetLineAttributes(graph->display,graph->gc_point,1,LineSolid,CapRound, JoinRound);

       TSSetColor(graph->gc_grid, TSMainDA[0], "white");

       XSetFont(graph->display, graph->gc_line,  graph->fs->fid);
       XSetFont(graph->display, graph->gc_header,graph->fs->fid);

       TSSetColor(graph->gc_line,   graph->widget, "White");
       TSSetColor(graph->gc_bg,     graph->widget, "Black");
       TSSetColor(graph->gc_grid,   graph->widget, "White");
       TSSetColor(graph->gc_header, graph->widget, "Cyan");
       TSSetColor(graph->gc_point,  graph->widget, "Red");

       graph->symbol_type            = 3; /* 0/1/2/3 */
       PlotMgr->display_crosshairs   = 1;
       PlotMgr->active_graph         = 0;

       updateGraphDimension( graph );	/* Store demensions when graph is first created */

       /* ------------------------------------------ */
       /* Create Pixmap for drawing areas and x-axis */
       /* ------------------------------------------ */
       graph->window_yLBPixmap = XCreatePixmap(graph->display, DefaultRootWindow(graph->display),
   				graph->yaxis_width, graph->yaxis_height,
                                DefaultDepthOfScreen(XtScreen(graph->widget)) );

       graph->windowDAPixmap = XCreatePixmap(graph->display, DefaultRootWindow(graph->display),
   				graph->orgW, graph->orgH,
                                DefaultDepthOfScreen(XtScreen(graph->widget)) );

       graph->window_xLBPixmap = XCreatePixmap(graph->display, DefaultRootWindow(graph->display),
   				PlotMgr->axis_w,PlotMgr->axis_h,
                                DefaultDepthOfScreen(XtScreen(graph->widget)) );

       graph->initialize = 0;
    }


    PlotMgr->gc    = XCreateGC(PlotMgr->display, XtWindow(TSMainAxisDA), 0, NULL);

    PlotMgr->TShsb = TSMainHSB;

    PlotMgr->font_id = XLoadFont(PlotMgr->display,
              "*-adobe-times-bold-r-normal--12-120-75-75-p-67-*");

    PlotMgr->active_color =  getMyColor ( PlotMgr->display, "White");

}

/* ----------------------------------------- */
/* ------------- CREATE  GC ---------------- */
/* ----------------------------------------- */

#if 0
GC xs_create_xor_gc(Widget w)
{

   XGCValues    values;
   GC           gc;


   values.foreground = getMyColor ( XtDisplay( w ), "White");

   values.background = getMyColor ( XtDisplay( w ), "Black");


   values.foreground = (values.foreground ^ values.background);
   values.line_style = LineSolid;
   values.function   = GXxor;

   gc = XtGetGC( w, GCForeground | GCBackground | GCFunction | GCLineStyle, &values);

   return ( gc );

}
#endif

/* ----------------------------------------- */
/* ------------- SET GC COLOR -------------- */
/* ----------------------------------------- */
void TSSetColor (GC gc, Widget w, char *sColor) {

   int icolor;

   icolor = getMyColor ( XtDisplay(w), sColor);

   XSetForeground ( XtDisplay ( w ), gc, icolor);
}

/* ----------------------------------------- */
/* ------------- GET GC COLOR -------------- */
/* ----------------------------------------- */
int getColorByName ( Widget w, char *name )
{

 return( getMyColor ( XtDisplay(w), name ) );

}

/* ----------------------------------------- */
/* ------------- GET COLOR BY NAME --------- */
/* ----------------------------------------- */
/*******This subroutine is in set_dates_cb.c*******---kwz 4/11/02*/
/*int getMyColor ( Display *display, char *colorName)
{
        XColor          xcolor,
                        unused;

        Colormap        cmap;


        cmap    = XDefaultColormap(display,DefaultScreen(display));


        XAllocNamedColor(display, cmap, colorName, &xcolor, &unused);


        return (xcolor.pixel);
}
*/
/* ----------------------------------------- */
/* ------- CLEAR MAIN DRAING AREA --------- */
/* ----------------------------------------- */
void clear_drawingArea(GRAPH *graph) {

   TSSetColor(graph->gc_bg, graph->widget, "Black");
   XFillRectangle(graph->display, graph->windowDAPixmap, graph->gc_bg, 0, 0,
					graph->orgW,
					graph->orgH);

   XFillRectangle(graph->display, graph->window_xLBPixmap, graph->gc_bg, 0, 0,
					PlotMgr->axis_w, PlotMgr->axis_h);
}

/* ----------------------------------------- */
/* ------- ADJUST YMIN/YMAX TO FIT -------- */
/* ----------------------------------------- */
void adjust_ymaxmin(float minval,
		float maxval,
		float *newmin,
		float *newmax,
		float *dinc)
{
	float 	dminmax;
	int 	n, j;


	dminmax = (maxval - minval);

	if ( dminmax <= 0.1 )
		n = 0;
	else
		n = log10 ( fabs(dminmax) );

	j = pow(10,n);

	if ( minval < 0 )
		minval = (int)(minval/j)*j - j;
	else
		minval = (int)(minval/j)*j;

	maxval = (int)(maxval/j + 1)*j;

	dminmax = (maxval - minval);

	if ( dminmax >= j*1.0) *dinc = 0.2*j;
	if ( dminmax >= j*2.0) *dinc = 0.5*j;
	if ( dminmax >= j*4.0) *dinc = 1.0*j;
	if ( dminmax >= j*8.0) *dinc = 2.0*j;

 	*newmin = minval;
 	*newmax = maxval;
	return;
}


/* ----------------------------------------- */
/* ------- DRAW Y-AXIS  ------------------- */
/* ----------------------------------------- */
void draw_yaxis( GRAPH *graph )
{

   float data_inc, data, min, max, tmp;

   int	 n, dx, x, y, dy, ypixel, xoffset;
   int   nticks, num_ticks;

   char  c;
   char	 buf[BUFSIZ];
   char	 yLB[20];
   char	 ylogALB[20];

   if ( graph->ntraces < 1 ) return;

   max      = graph->ymax;

   min      = graph->ymin;

   data_inc = graph->data_inc;

   sprintf( yLB, "PLOT#%d %s " , graph->number + 1 , graph->Unit);

   if(graph->Algorithm == 0)
       sprintf( ylogALB, "%s" , "Arith");
   else sprintf( ylogALB, "%s" , "Log");

   TSSetColor(graph->gc_bg, graph->widget, "Black");
   TSSetColor(graph->gc_line,   graph->widget, "White");
   TSSetColor(graph->gc_header, graph->widget, "White");

   XFillRectangle(graph->display, graph->window_yLBPixmap, graph->gc_bg, 0, 0,
					graph->yaxis_width,
					graph->yaxis_height);

   XDrawLine(graph->display,graph->window_yLBPixmap, graph->gc_line,
				 graph->yaxis_width-2, 0,
				 graph->yaxis_width-2, graph->yaxis_height);

   for ( n = 0; n < strlen(yLB); n++) {
      c = yLB[n];
      XDrawString(graph->display,graph->window_yLBPixmap, graph->gc_header,
					5, 12+(n*15), (char *)&c, 1);
   }

   XDrawString(graph->display,graph->windowDAPixmap, graph->gc_header,
				        6, 10,
					ylogALB, strlen(ylogALB));

   tmp  = (max-min)/data_inc; nticks = (int)tmp + 1;

   data = min;

   for (num_ticks = 0; num_ticks < nticks; num_ticks++) {

      sprintf(buf, "%7.2f", data);

      x = (graph->yaxis_width-2);

      y = y2pixel( graph,  data );

      if ( num_ticks % 2 ) {
         dx = 8;
         XDrawLine(graph->display,graph->window_yLBPixmap, graph->gc_line,
						x, y, (x - dx), y);

         XDrawString(graph->display,graph->window_yLBPixmap, graph->gc_header,
					(x - 55), (y + 5),buf, strlen(buf));
      } else {

         dx = 5;
         if ( num_ticks == 0 || num_ticks == max) {
            dy = 2;
            XDrawString(graph->display,graph->window_yLBPixmap, graph->gc_header,
                                        (x - 55), (y - 1 ),buf, strlen(buf));
         }
         XDrawLine(graph->display,graph->window_yLBPixmap, graph->gc_line,
         x , y - dy, (x - dx), y - dy);

    }

   /* Main drawing Area */
   if (PlotMgr->GridOn)
      XDrawLine(graph->display,graph->windowDAPixmap, graph->gc_grid,
				 graph->x,  y, (graph->x + graph->w), y);

   data+=data_inc;

}


/* ******************************** */
/* if grid lines is on              */
/* ******************************** */
   if (PlotMgr->GridOn)
 	XDrawLine(graph->display, graph->windowDAPixmap, graph->gc_grid,
				graph->x, graph->y, (graph->x + graph->w),graph->y);

   /* printf("draw_yaxis ==> Copy from Pixmap to Window .... \n"); */

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


void clearDrawAInfo ( )
{

   int offset = 0;

   TSSetColor(PlotMgr->gc,  PlotMgr->widgetAxisDA, "Grey85");

   XFillRectangle(PlotMgr->display, PlotMgr->windowInfoDA, PlotMgr->gc, offset, 0,
                                        PlotMgr->Info_w, PlotMgr->Info_h);

   XFlush(PlotMgr->display);

}


/* ----------------------------------------- */
/* ------- DRAW X-AXIS  ------------------- */
/* ----------------------------------------- */
void draw_xaxis(GRAPH *graph)
{

   int  x,
	dy,
        yoffset,
	hourOn,
	ndays,
    	major_ticks,
	minor_ticks,
	tick_count,
        lastDay;

   long increment;

   time_t	ltime,
		curtime;

   char		buf[BUFSIZ];
   char		buf1[BUFSIZ];

   struct tm   	*tm_ptr;


   if ( graph->ntraces < 1 )
   			return;

   time (&curtime);

   yoffset = 1;

   TSSetColor(graph->gc_bg, graph->widget, "Black");
   XFillRectangle(graph->display, graph->window_xLBPixmap, graph->gc_bg, 0, 0,
					PlotMgr->axis_w, PlotMgr->axis_h);


   TSSetColor(graph->gc_line, PlotMgr->widgetAxisDA, "Cyan");

   XDrawLine(graph->display,graph->window_xLBPixmap, graph->gc_line,
			0,  yoffset, PlotMgr->axis_w, yoffset);

   XDrawLine(graph->display,graph->window_xLBPixmap, graph->gc_line,
			0,  yoffset, 0,  8);

   ndays = (graph->xmax - graph->xmin)/(SECONDS_PER_DAY);

   /*
   printf("==================>ndays = %d\n",ndays);  */

   hourOn = 0;


   /* timeInterval = 1/6/12/24 */
   if ( PlotMgr->idtp == 1){
     major_ticks = 6;
     minor_ticks = 3;
   }
   else{
     major_ticks = 24/PlotMgr->idtp;
     minor_ticks = 1;
   }
   hourOn = 1;
   increment   = SECONDS_PER_HOUR*PlotMgr->idtp;
   tick_count = 0;

   for (ltime = graph->xmin; ltime <= graph->xmax; ltime += increment) {

      x = x2pixel( graph, ltime );

      tm_ptr = gmtime(&ltime);

      tick_count++;

      sprintf(buf1,".%02d",PlotMgr->idtp);

      if ( tick_count % major_ticks == 0) {

         dy = 8;

         XDrawLine(graph->display,graph->window_xLBPixmap, graph->gc_line,
				x+3,  - yoffset,
		        	x+3,  - yoffset + 10 );

         if (  tm_ptr->tm_mday != lastDay ) {
            strftime(buf, sizeof(buf), "%d", tm_ptr);
            strcat(buf,buf1);

            XDrawString(graph->display,graph->window_xLBPixmap, graph->gc_line,
			x-14, + 25, buf, strlen(buf));

            lastDay =  tm_ptr->tm_mday;

         }

       }

       if ( tick_count % minor_ticks == 0) {
          dy = 6;
          XDrawLine(graph->display,graph->window_xLBPixmap, graph->gc_line,
			x+3,  - yoffset,
	        	x+3,  - yoffset + dy );
       }

     }

     TSSetColor(graph->gc_line, graph->widget, "Cyan");
     if( hourOn )
        XDrawString(graph->display, graph->window_xLBPixmap, graph->gc_line,
				PlotMgr->axis_x-50, 21,
                                "(HOURS)", strlen("(HOURS)"));
     else
        XDrawString(graph->display, graph->window_xLBPixmap, graph->gc_line,
				PlotMgr->axis_x-50,  21,
                                "(DAYS)", strlen("(DAYS)"));

     XCopyArea(graph->display, graph->window_xLBPixmap,
             PlotMgr->windowAxisDA, graph->gc_copy,
             0, 0, PlotMgr->axis_w,PlotMgr->axis_h, 0, 0);

     XFlush(graph->display);


}


/* ----------------------------------------- */
/* ------- DRAW GRID    ------------------- */
/* ----------------------------------------- */
void draw_grid(GRAPH *graph)
{

   int  x, y, n, dx, ndays, nticks;
   int  major_ticks, minor_ticks, tick_count;

   float tmp,
	data,
	data_inc;

   long increment;

   time_t	ltime;

   char		buf[BUFSIZ];

   struct tm   	*tm_ptr;

   ndays = (graph->xmax - graph->xmin)/(SECONDS_PER_DAY);

   tick_count = 0; dx = 3;
   major_ticks = 24/PlotMgr->idtp;
   minor_ticks = 1;
   increment   = SECONDS_PER_HOUR*PlotMgr->idtp;

   for (ltime = graph->xmin; ltime <= graph->xmax; ltime += increment) {

      x = x2pixel( graph, ltime );

      tm_ptr = gmtime(&ltime);

      tick_count++;
      if ( tick_count % (2*major_ticks) == 0) {

         XDrawLine(graph->display,graph->windowDAPixmap, graph->gc_grid,
				x+dx, 0, x+dx, graph->h);
      }
   }

   XDrawLine(graph->display,graph->windowDAPixmap, graph->gc_grid,
	   	graph->w,  0, graph->w,  graph->h);

   /* -------------------------------------------------- */
   /* D R A W I N G  T I C K M A R K S  O N  Y - A X I S */
   /* -------------------------------------------------- */
   tmp  = (graph->ymax-graph->ymin)/graph->data_inc;

   nticks = (int)tmp + 1;

   data = graph->ymin;

   data_inc =  graph->data_inc;

   for (n = 0; n < nticks; n++) {

      y = y2pixel( graph,  data );

      if ( n % 2 ) {

         XDrawLine(graph->display,graph->windowDAPixmap, graph->gc_grid,
						0, y, graph->w, y);
      }

      data+=data_inc;
    }

  XFlush(graph->display);

}


/* **************************************** */
/* convert real X value to pixel value      */
/* **************************************** */
int x2pixel( GRAPH *graph, time_t x)
{

   float   xp;
   int	   xpixel;

   xp = ((float)graph->w * (float)(x - graph->xmin))/(float)(graph->xmax - graph->xmin) + (float)graph->x;
   xpixel = (int) xp;

   return( xpixel);
}

/* **************************************** */
/* convert real Y value to pixel value      */
/* **************************************** */
int y2pixel( GRAPH *graph, float y)
{
   float   yp;
   int     ypixel;

   yp = graph->h - ((float)graph->h * (y - graph->ymin))/(graph->ymax - graph->ymin) + graph->y;
   ypixel = (int) yp;

   return( ypixel);
}

/* **************************************** */
/* convert pixel value  to real Y value     */
/* **************************************** */
float pixel2y ( GRAPH *graph, int ypix )
{
   float yvalue;

   yvalue = graph->ymin + (float)(graph->h + graph->y - ypix)*(graph->ymax - graph->ymin)/(float)graph->h;

   return ( yvalue );

}

/* **************************************** */
/* convert pixel value  to real time value  */
/* **************************************** */
time_t pixel2x ( GRAPH *graph, int xpix )
{
   time_t xvalue;
   float  xtmp;

   xtmp = (float)graph->xmin+ (float)(xpix - graph->x)*(float)(graph->xmax - graph->xmin)/(float)graph->w;
   xvalue = (time_t) xtmp;

   return( xvalue);

}

/* ----------------------------------------- */
/* ------- RE-DRAW GRAPH   ----------------- */
/* ----------------------------------------- */
void reDrawAll ()
{
   int n;
   for ( n = 0; n < PlotMgr->nPlots; n++) {
      GRAPH *gptr = (GRAPH *)&PlotMgr->Graph[n];
      reDrawGraph ( gptr);
   }

}
/* ----------------------------------------- */
/* ------- RE-DRAW GRAPH   ----------------- */
/* ----------------------------------------- */
void reDrawGraph ( GRAPH *graph)
{

    clear_drawingArea(graph);
    draw_yaxis( graph );

    if ( PlotMgr->GridOn == 1 )
    	draw_grid( graph );

    TSDisplay( graph );
    /*if (  graph->number == (PlotMgr->nPlots - 1 ) ) {
       draw_xaxis( graph );
    }*/
    draw_xaxis( graph );
}

/* *********************************************** */
/* Update Graph when zoom is activated             */
/* *********************************************** */
void update_graph(int x1,int y1,int x2,int y2)
{

	float	tmp_ymin, tmp_ymax, prev_ymin;
	float	ymin, ymax, ydiff;
	time_t	tmp_xmin, tmp_xmax;

	GRAPH *gptr  = (GRAPH *) &PlotMgr->Graph[PlotMgr->active_graph];

	/*  *************************************************************** */
	/*  Calculate new mmax min for x_axis and y_axis  when in zoom mode */
	/*  *************************************************************** */

	   prev_ymin = gptr->ymin;

	   tmp_xmin = gptr->xmin + (x1 - gptr->x)*(gptr->xmax - gptr->xmin)/gptr->w;
	   tmp_ymax = gptr->ymin + (gptr->h + gptr->y - y1)*(gptr->ymax - gptr->ymin)/gptr->h;
	   tmp_xmax = gptr->xmin + (x2 - gptr->x)*(gptr->xmax - gptr->xmin)/gptr->w;
	   tmp_ymin = gptr->ymin + (gptr->h + gptr->y - y2)*(gptr->ymax - gptr->ymin)/gptr->h;

	   gptr->xmin = (tmp_xmin/3600)*3600;
	   gptr->xmax = (tmp_xmax/3600)*3600;

	   gptr->ymin = tmp_ymin;
	   gptr->ymax = tmp_ymax;

	   if ( gptr->ymin < prev_ymin)
			gptr->ymin = prev_ymin;


	   ymin = gptr->ymin; ymax = gptr->ymax;

	   ydiff = (ymax - ymin);
	   if ( ydiff >= 1.0)
	   	adjust_ymaxmin(ymin, ymax, &gptr->ymin, &gptr->ymax, &gptr->data_inc);
	   else
	   {
		gptr->data_inc = ydiff/5.0;
	   }

	   gptr->old_data_inc = gptr->data_inc;
	   gptr->old_xmin = gptr->xmin;
	   gptr->old_xmax = gptr->xmax;
	   gptr->old_ymin = gptr->ymin;
	   gptr->old_ymax = gptr->ymax;

	   /*display_TSgraph_data( PageMgr->active_graph );*/

}


/* ----------------------------------------- */
/* ------- DRAW CROSS HAIR     ------------- */
/* ----------------------------------------- */
void drawing_crosshairs()
{

   extern PLOTMGR  *PlotMgr;

   GRAPH *graph;

   int n;

   /* Ai Added 04/02/2002 */
   if ( PlotMgr->display_crosshairs == 0 )  return;

   for ( n = 0; n < PlotMgr->nPlots; n++) {

     graph  = (GRAPH *) &PlotMgr->Graph[n];

     if (  PlotMgr->last_x < graph->x || PlotMgr->last_x >  graph->w) return;

        if (  n == PlotMgr->active_graph ) {

            XDrawLine(graph->display, graph->window, graph->gc_Xor,
                        graph->x, PlotMgr->last_y,  graph->w, PlotMgr->last_y);

        }

        XDrawLine(graph->display, graph->window, graph->gc_Xor,
                        PlotMgr->last_x, graph->y, PlotMgr->last_x, graph->y + graph->h);

     }

   return;
}


/* ----------------------------------------- */
/* ------- SEARCH AND DISPLAY  ------------- */
/* ----------------------------------------- */
void search_and_display_trace(int xpix, int ypix, time_t xsecs)
{

	time_t 		xvalue;
	float		yvalue;

	struct tm 	*tm_ptr;
	char		buf[100],
			tbuf[100];

	GRAPH	*graph = (GRAPH *) &PlotMgr->Graph[PlotMgr->active_graph];

	yvalue = pixel2y ( graph, ypix );

	if ( xsecs == 0)
	{
		xvalue = pixel2x ( graph, xpix );
		xvalue+=45;
	}
	else
		 xvalue = xsecs;

	tm_ptr = gmtime(&xvalue);
	strftime(tbuf, sizeof(buf), "%m/%d/%y %H", tm_ptr);
	sprintf(buf,"%s Value = %.2f",tbuf,yvalue);

        PlotMgr->windowInfoDA  =  XtWindow(TSMainInfoDA);

	TSSetColor(graph->gc_header, PlotMgr->widgetInfoDA, "Blue");

	TSSetColor(graph->gc_bg, PlotMgr->widgetInfoDA, "white");

        /* Diplay in left hand corner */
	XFillRectangle(PlotMgr->display, PlotMgr->windowInfoDA, graph->gc_bg,
				 PlotMgr->Info_w - 165, 0,
				 PlotMgr->Info_w, PlotMgr->Info_h);

	XDrawString(PlotMgr->display, PlotMgr->windowInfoDA, graph->gc_header,
				PlotMgr->Info_w - 160, 20, buf, strlen(buf));

}

/* ----------------------------------------- */
/* ------- UPDATE GRAPH DEMENIONS ---------- */
/* ----------------------------------------- */
void updateGraphDimension ( GRAPH  *g) {

   Dimension width, height;
   Dimension panewidth, paneheight;
   Dimension dawidth,   daheight;
   Dimension ylbwidth,  ylbheight;
   Dimension axiswidth, axisheight;
   Dimension infowidth, infoheight;

   XtVaGetValues(TSMainPane, XmNwidth,  &panewidth, XmNheight, &paneheight, NULL);
   XtVaGetValues(g->widgetyLB, XmNwidth,&ylbwidth,  XmNheight, &ylbheight, NULL);
   XtVaGetValues(g->widget, XmNwidth,   &dawidth,   XmNheight, &daheight,   NULL);
   XtVaGetValues(PlotMgr->widgetAxisDA, XmNwidth,  &axiswidth, XmNheight, &axisheight, NULL);
   XtVaGetValues(PlotMgr->widgetInfoDA, XmNwidth,  &infowidth, XmNheight, &infoheight, NULL);

   /* -------------------------------------- */
   /* ------ width y-axis Label Area ------- */
   /* -------------------------------------- */
   width  =  LEFTLBW;
   height =  ylbheight;
   g->yaxis_width  = width;
   g->yaxis_height = height;
   XtVaSetValues(g->widgetyLB, XmNwidth, width, XmNheight, height, NULL);

   /* ---------------------------------- */
   /* -------  Main Drawing area ------- */
   /* ---------------------------------- */

   width = dawidth;
   height= daheight;
   g->orgW = width;
   g->orgH = height;
   g->x = 0;
   g->y = 0;
   g->w = width;
   g->h = height;
   XtVaSetValues(g->widget, XmNwidth, width, XmNheight, height, NULL);


   /* ------------------------------------ */
   /* -------  Right Legends RC    ------- */
   /* ------------------------------------ */
   width = RGHTLBW;
   XtVaSetValues(TSMainRFO[g->number], XmNwidth, width, XmNheight, height, NULL);

   /* ------------------------------------ */
   /* -------  x-axis Drawing Area ------- */
   /* ------------------------------------ */
   width = dawidth;

   height= axisheight;
   PlotMgr->axis_x = 0;
   PlotMgr->axis_y = 0;
   PlotMgr->axis_w = width;
   PlotMgr->axis_h = height;
   XtVaSetValues(PlotMgr->widgetAxisDA, XmNwidth, width, XmNheight, height, NULL);
   /*printf("==========>axis_w: %d %d %d\n", PlotMgr->axis_w, width, height); */

   /* ---------------------------------- */
   /* -------  Info Drawing Area ------- */
   /* ---------------------------------- */
   width = panewidth;
   height= infoheight;
   PlotMgr->Info_w = width;
   PlotMgr->Info_h = infoheight;
   XtVaGetValues(PlotMgr->widgetInfoDA, XmNwidth,  &width, XmNheight, &height, NULL);

    /* ----------------------------------------- */
   /* Free old pixmaps and create new ones when */
   /* windows being resized                     */
   /* ----------------------------------------- */

   if ( g->window_yLBPixmap )  XFreePixmap(g->display,  g->window_yLBPixmap);

   if ( g->windowDAPixmap   )  XFreePixmap(g->display, g->windowDAPixmap);

   if ( g->window_xLBPixmap )  XFreePixmap(g->display, g->window_xLBPixmap);


   g->windowDAPixmap = XCreatePixmap(g->display, DefaultRootWindow(g->display),
   				g->orgW, g->orgH,
                                DefaultDepthOfScreen(XtScreen(g->widget)) );

   g->window_xLBPixmap = XCreatePixmap(g->display, DefaultRootWindow(g->display),
   				PlotMgr->axis_w, PlotMgr->axis_h,
                                DefaultDepthOfScreen(XtScreen(g->widget)) );

   g->window_yLBPixmap = XCreatePixmap(g->display, DefaultRootWindow(g->display),
   				g->yaxis_width, g->yaxis_height,
                                DefaultDepthOfScreen(XtScreen(g->widget)) );

}



/* ----------------------------------------- */
/* ------- DISPLAY GRAPH -------------------- */
/* ----------------------------------------- */
 void TSDisplay ( GRAPH *graph) {

   struct tm 	*tm_ptr;

   char		buf[80];

   int          n, m, j, k , ntrace, icolor;

   TRACE       *tPtr;

   XPoint      p[1000];
   XPoint      pA[1000];

   Pixel         foreground, background;      /* foreground, backround colors */
   XGCValues     info_gcv;                    /* graphics context data structure */
   float         discharge;                   /* discharge rate  */
   float         stage_metric;                /* metric river depth */
   int           x1, y1, x2, y2;              /* plot points */
   int           x, y;

   /* Font        font_id; */

   tm_ptr = gmtime(&PlotMgr->xmin);
   strftime(buf, sizeof(buf), "%m/%d/%y %H", tm_ptr);
   tm_ptr = gmtime(&PlotMgr->xmax);
   strftime(buf, sizeof(buf), "%m/%d/%y %H", tm_ptr);
   graph->xmin = PlotMgr->xmin;
   graph->xmax = PlotMgr->xmax;

/*
   font_id = XLoadFont(XtDisplay(graph->widget),
              "*-adobe-times-bold-r-normal--12-120-75-75-p-67-*");
*/
   XSetFont(XtDisplay(graph->widget), graph->gc_line, PlotMgr->font_id);

   for ( ntrace = 0; ntrace < graph->ntraces; ntrace++) {

       k = 0;
       tPtr = (TRACE *)&graph->traces[ntrace];
       /*ObsFlag = 1 plot points only  */

       if ( tPtr->ObsFlag == 1 ) {
          PlotMgr->trace_mode = 0;  /* 0 Point - 1 Line - 2 point+line */
       }
       else {
          PlotMgr->trace_mode = 2;
       }
       /* ---------------- set trace color --------------- */
       /*if(graph->number == PlotMgr->edit_graph && ntrace == PlotMgr->active_trace){*/

       if( strcmp(tPtr->colorName,"White") == 0){
          TSSetColor(graph->gc_line, graph->widget, tPtr->colorName);
       }
       else{

          icolor = colorLookUp( tPtr->tmpTraceType);

          TSSetColor(graph->gc_line, graph->widget, TScolors[icolor].colorName);
       }

       for ( m = 0; m < tPtr->npts; m++) {

/*          if (  tPtr->TSData[m].x <  graph->xmin) continue;
          if (  tPtr->TSData[m].x >  graph->xmax) break;
          if (  tPtr->TSData[m].y == -999.0) continue;
*/
          /*if (  tPtr->TSData[m].y >  graph->ymax) break;*/
          p[k].x  = x2pixel(graph, tPtr->TSData[m].x);
          p[k].y  = y2pixel(graph, tPtr->TSData[m].yedit);
          pA[k].x = x2pixel(graph, tPtr->TSData[m].x);
          pA[k].y = y2pixel(graph, tPtr->TSData[m].y);

          k++;
       }

       /* Lines */
       if ( PlotMgr->trace_mode == 1 || PlotMgr->trace_mode == 2) {

          XDrawLines(graph->display, graph->windowDAPixmap, graph->gc_line, p, k, CoordModeOrigin);

       }
      /* ------------------------------ */
      /* Draw active trace as Line only */
      /* ------------------------------ */

       if (  PlotMgr->EditActive == 1 ) {
          if ( PlotMgr->trace_mode == 1 || PlotMgr->trace_mode == 2) {

             XDrawLines(graph->display, graph->windowDAPixmap, graph->gc_line, pA, k, CoordModeOrigin);

          }

          else {
             for (n = 0; n < k; n++){
             XDrawString(graph->display,graph->windowDAPixmap, graph->gc_line,
				 pA[n].x-2, pA[n].y+5, (char *)&tPtr->Trace_Symbol, 1);
             }
          }
       }

       TSSetColor(graph->gc_point, graph->widget, "Yellow");

       /* Points + Lines */
       if ( PlotMgr->trace_mode == 0 || PlotMgr->trace_mode == 2 || k == 1) {

          for (n = 0; n < k; n++){

          XDrawString(graph->display,graph->windowDAPixmap, graph->gc_line,
				 p[n].x-2, p[n].y+5, (char *)&tPtr->Trace_Symbol, 1);
          }
       }


	/* Added by Guoxian Zhou for bug r21-47 */
	/* Create graphics context for LSTCMPDY line.
	*/

   if(PlotMgr->end_obs >= graph->xmin && PlotMgr->end_obs <= graph->xmax)
   {

		p[0].x  = pA[0].x = x2pixel(graph, PlotMgr->end_obs);
		p[0].y  = y2pixel(graph, graph->ymin);
		pA[0].y = y2pixel(graph, graph->ymax);

		XSetLineAttributes(graph->display,graph->gc_line, 1,LineOnOffDash,CapRound, JoinMiter);
		TSSetColor(graph->gc_line,   graph->widget, "white");

		XDrawLine(graph->display, graph->windowDAPixmap, graph->gc_line,
                		p[0].x, p[0].y, pA[0].x, pA[0].y);

		XSetLineAttributes(graph->display,graph->gc_line, 1,LineSolid,CapRound, JoinRound);

	}

	/*End of draw LSTCMPDY line*/

	/*  Draw flood line */

	if(PlotMgr->rc_data->RC == TRUE && PlotMgr->rc_data->flood_flow > 0.0)
	{
		if(PlotMgr->rc_data->flood_flow  >= graph->ymin
		   && PlotMgr->rc_data->flood_flow  <= graph->ymax)
		{
        	x = graph->orgW;

			y = y2pixel( graph,  PlotMgr->rc_data->flood_flow );

			XSetLineAttributes(graph->display,graph->gc_line, 1,LineOnOffDash,CapRound, JoinMiter);
			TSSetColor(graph->gc_line,   graph->widget, "red");
			TSSetColor(graph->gc_header,   graph->widget, "red");

			XDrawLine(graph->display, graph->windowDAPixmap, graph->gc_line,
                			x2pixel(graph, graph->xmin), y, x2pixel(graph, graph->xmax), y);

			sprintf(buf, "%s", "F");

			XDrawString(graph->display,graph->windowDAPixmap, graph->gc_header,
					 x - 50, (y - 5), buf, strlen(buf));

			XSetLineAttributes(graph->display,graph->gc_line, 1,LineSolid,CapRound, JoinRound);

		}
	}

	/*End of draw flood line */
	/*End of modification by Guoxian Zhou */

  } /* End of number of traces */


   XFlush(PlotMgr->display);
   /* ----------------------------------- */
   /* Copy Pixmap to windows              */
   /* ----------------------------------- */
   XCopyArea(graph->display, graph->windowDAPixmap,
             graph->window, graph->gc_copy,
             0, 0, graph->orgW, graph->orgH, 0, 0);

}


/* ----------------------------------------- */
/* ------- ADD TRACE TO GRAPH--------------- */
/* ----------------------------------------- */

void addTStrace( GRAPH *graph, TRACE *tptr)
{
   if (  graph->ntraces >= MAXTRACES) {
      printf("Number of traces for plot TS (%d) >= limit (%d).\n", graph->ntraces, MAXTRACES);
      return;
   }

    memcpy((char *)&graph->traces[graph->ntraces],(char *)tptr,sizeof(TRACE));

    graph->ntraces++;


    return;

}

/* ------------------------------------------ */
/* ------ Read/Write/Remove tmpMinMax.dat ---- */
/* ------------------------------------------ */
void read_ymaxminFile() {

FILE *fp;
int   n;
float ymin, ymax;

GRAPH *graph;
char  path_name[80];

   strcpy(path_name,getenv("HOME"));

   strcat(path_name,"/.ifp_files/tmpMinMax.dat");


   if ((fp = fopen(path_name,"r")) == NULL)  {

          return;

   }

   while ( 1 ) {

      if(fscanf(fp,"%d%f%f", &n, &ymin, &ymax) == 3) {

         graph = (GRAPH *)&PlotMgr->Graph[n];
         graph->ymin = ymin; graph->ymax = ymax;

      } else {

         fclose(fp);
         break;
      }

   }

}


void write_ymaxminFile() {

FILE *fp;
int   n, pnum;
float ymin, ymax;
GRAPH *graph;
char  path_name[80];

   strcpy(path_name,getenv("HOME"));

   strcat(path_name,"/.ifp_files/tmpMinMax.dat");


   if ((fp = fopen(path_name,"w")) == NULL)  {

      return;

   }

   /* Test section
   pnum = 3; ymin = 11.0; ymax = 22.0;
   fprintf(fp,"%d %f %f", pnum, ymin, ymax);
   */

   for ( n = 0; n < PlotMgr->nPlots; n++) {


       graph = (GRAPH *)&PlotMgr->Graph[n];

       pnum = n; ymin = graph->ymin; ymax = graph->ymax;

       fprintf(fp,"%d %.2f %.2f\n", n, ymin, ymax);

   }

   fclose(fp);

}

void remove_ymaxminFile() {

   char buf[80];

   FILE *fp;
   char  path_name[80];

   strcpy(path_name,getenv("HOME"));

   strcat(path_name,"/.ifp_files/tmpMinMax.dat");


   if ((fp = fopen(path_name,"r")) == NULL)  {

      return;

   }
   sprintf(buf,"rm %s", path_name);
   system(buf);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTS/RCS/TSUtils.c,v $";
 static char rcs_id2[] = "$Id: TSUtils.c,v 1.5 2003/07/17 18:56:22 gzhou Exp $";}
/*  ===================================================  */

}



