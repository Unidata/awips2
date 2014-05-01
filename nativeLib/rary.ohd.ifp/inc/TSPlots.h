/* ***************************************** */
/*      File:           plots.h              */
/*      Date:           Jan 2002             */
/*      Author:         Ai Vo                */
/*      Purpose:                             */
/*                                           */
/* ***************************************** */
 
#ifndef _PLOTS_H
#define _PLOTS_H

#include "MotifHeader.h"
#include "TSDefs.h"

#include "ifp_struct.h"   /* Added by Guoxian Zhou*/

typedef struct _PTINFO 
{
   /* ------------------------------------------- */
   /* keep track graph number when trace selected */
   /* used in TSTraceSelectCB() in TSUtils.h      */
   /* ------------------------------------------- */
   int active_graph;
   int active_trace;

}PTINFO;

typedef struct _ACOLOR
{
   int  colorPix;
   char cType[10];
   char colorName[20];
 
}ACOLOR;

typedef struct _POINT
{
   time_t x;
   int t; /*t is time in julian day * 24 + hour*/
   float  y;
   float  yOrg;
   float  yedit;
   char   status;
}POINT;

typedef struct _TRACE
{
    Widget  TSLegPB[MAXTRACES];

    char    OperationID[9] ;
 
    char    TraceType[5] ;
    char    tmpTraceType[5];
 
    int     TimeInterval ;
 
    char    Trace_Title[13];
 
    char    Trace_Symbol;

    char    colorName[20];

    long    npts;

    float   ymax, ymin;

    float   xmax, xmin; 

    time_t  txmin,
            txmax;
 
    POINT   TSData[MAXPOINTS];
    
    int ObsFlag ;/*0 - line and point, 1 - point (symbol)*/

}TRACE;

typedef struct _GRAPH 
{

    Display      *display;

    Widget       widget;

    Window       window;

    Widget       widgetyLB;
    
    Widget       TSYscalePB[MAXPLOTS];

    Window       windowyLB;

    Pixmap       windowDAPixmap;

    Pixmap       window_yLBPixmap;

    Pixmap       window_xLBPixmap;
   
    XFontStruct	 *fs;

    GC           gc_line;
    GC           gc_point;
    GC           gc_grid;
    GC           gc_bg;
    GC           gc_header;
    GC           gc_Xor;
    GC           gc_copy;
    
    char         Unit[5];

    int          Algorithm; /*0=Arithmethic, 1=log*/
 
    float        CFact, Const ;

    int          number;

    int          ntraces;

    int          x, y, w, h, orgW, orgH;

    int          yaxis_width, yaxis_height;

    int          last_x, last_y;  

    int          bg;


    time_t       xmin,
                 xmax,
                 old_xmin,
                 old_xmax,
                 org_xmin,
                 org_xmax;

    float        ymin,
                 ymax,
                 old_ymin,
                 old_ymax,
                 org_ymin,
                 org_ymax;
                                                                                                  

    float        data_inc,
                 old_data_inc,
                 org_data_inc;
 
    int          symbol_type;

    int          initialize;
    
    int          trace_on[MAXTRACES];
    TRACE        traces[MAXTRACES];
}GRAPH;

typedef struct _PLOTMGR
{

   GC      gc;

   Display *display;
   
   Font    ylabelFont;
   Font    xlabelFont;
   Font    glabelFont;
   Font    font_id;  
   Widget  TShsb; 
   Widget  widgetAxisDA;
   Window  windowAxisDA;

   Widget  widgetInfoDA;
   Window  windowInfoDA;

   int     trace_mode;  /* 0 1 2 Points/Lines/Both */

   int     GridOn;

   int     axis_x, axis_y, axis_w, axis_h;

   int     Info_w, Info_h;

   int     last_x, last_y;  /* previous cursor's position */

   int     display_crosshairs;

   int     active_graph;
   int     edit_graph;

   int     active_trace;
   int     active_color;

   int     nPlots;

   int     EditActive;
   int     EditTrace;

   time_t  org_xmin, org_xmax;   /*  seconds     */
   time_t  xmin, xmax;           /*  seconds     */
 
   int     duration;              /*  90.00  days */
   int     num_days;              /*              */
   int     num_tics;              /*  ts->ts_num_days*24.00/info->idtp; */
   int     slider_size;           /*  info->plotts_duration *24.00/info->idtp; */
   int     slider_incr;           /*  24.00/info->idtp; */
   int     period;                /*  ts->ts_num_days; */
   int     idtp;
   int     Year,
           Month, 
           NumPlot;/*NumPlot is length of *Graph*/
   char    Description[21],
           TimeZone[5], 
           OperationName[50],
           *SegName;         

   GRAPH   Graph[MAXPLOTS];

   int     NWSRFS_Units;/* If 0: ENGLISH, 1: METRIC     */
   int     General_Units;/* If 0: ENGLISH, 1: METRIC     */
   int     NOUTZ,NOUTDS;

	/* Added by Guoxian Zhou for TS Plot          */		
   time_t		end_obs;	/* hold the LSTCMPDY date */		
   rc_struct    *rc_data;   /* rating curve data structure to hold flood data */
   
}PLOTMGR; 


#endif
