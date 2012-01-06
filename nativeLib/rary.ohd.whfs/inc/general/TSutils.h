
/* ***************************************** */
/*	File:           TSutils.h            */
/*      Date:           April 1999           */
/*      Author:         Sung Vo              */
/*      Purpose:        Provide support for  */
/*      the Time Series Display              */
/* ***************************************** */

#ifndef _TSutils_h
#define _TSutils_h

#include "TSdbx.h"
#include "TimeSeries.h"
#include "TSgetinfo.h"
#include "TSControl_show.h"

/* ********************* */
/* Prototype definitions */
/* ********************* */

GC 	xs_create_xor_gc( Widget w );

void 	check_points(int *x1,int *y1,int *x2,int *y2);

int 	find_active_graph( PAGE_DATA *p, int x, int y );

int 	check_graph_bound( GRAPH_DATA *g, int x, int y );

void    get_TSpage_data ( int page ) ;

void 	update_pageDimesion( int page );

void 	update_graph(int x1,int y1,int x2,int y2);

void 	draw_xaxis( GRAPH_DATA *graph );

void 	draw_yaxis( GRAPH_DATA *graph );

void 	draw_pcAspp_yaxis( GRAPH_DATA *graph , float ymin, float ymax, float  yinc);

void 	draw_rubber_band( void );

void  	draw_crosschairs( void );

void 	display_trace_minmax( TRACE_DATA *tptr );

void 	display_trace_name( TRACE_DATA *tptr , int trace_cnt, int ShowpcAspp, int trace_valid);

void 	search_and_display_trace( int xpix, int ypix , time_t xsecs);

void 	display_TSpage_data(int page);

void	display_TSgraph_data(int active_graph);

void 	display_TStraceGen( int page);

void 	display_TStracePp( int active_trace );

void 	display_TStracePcAsPp( int active_trace );

void draw_crosshairs ( ) ;

int  	find_TSpage_fromlid( char *lid );

void 	get_TSgraph_data( GRAPH_INFO *Ginfo, GRAPH_DATA *Gdata );

int  	get_TSobstrace( TRACE_DATA *tptr );

void 	show_nodata_label( GRAPH_DATA *gptr , char *trace_lid, char *trace_name);

void 	find_TSgraph_minmax( GRAPH_DATA *graph , int ngraph, int chk_fcst);

void 	draw_UnitLabel( GRAPH_DATA *graph , char *label );

void 	drawStageFlowLabel( GRAPH_DATA *graph , int stageOrdischarge, float max_discharge);

void 	getShefPe( char *pe, char *buf);

void 	setTraceLabel( int selected_graph );

void 	TStrace_onoff_CB (Widget w, XtPointer ptr, XtPointer cbs);

void 	load_TSrating ( GRAPH_DATA *graph , char *rate_lid );


void 	load_TShgvars ( GRAPH_DATA *graph , char *lid );

void 	display_TShgvars( GRAPH_DATA *graph );

int 	GetObserve_data( TRACE_DATA *tptr );

int 	GetForecast_data( TRACE_DATA ftrace[] , TRACE_DATA *tptr);

int  	get_Allfcst_data( GRAPH_DATA *gptr, TRACE_DATA *tptr );

float  	get_DischargeFromStage(  GRAPH_DATA *graph, float ystage);

void 	display_DischargeFromStage(  GRAPH_DATA *graph, int ypixel, float ystage, float max_discharge);

float 	get_StageFromDischarge(  GRAPH_DATA *graph, float ydischarge);

void 	display_StageFromDischarge(  GRAPH_DATA *graph, int ypixel, float ydischarge);

void 	adjust_pcymax( float minval, float maxval, float *newmin,  float *newmax, float *dinc);

void 	adjust_ymaxmin( float minval, float maxval, float *newmin,  float *newmax, float *dinc);

int 	ts_do_normalize ( TRACE_DATA *TinPtr, TRACE_DATA *ToutPtr, time_t interval_secs, int interp_mode);

int 	ts_accum_to_inc2 ( TRACE_DATA *ToutPtr, int distrib_mode);

double 	ts_do_assign_value ( TRACE_DATA *TinPtr, time_t norm_time, time_t interval_secs, int interp_mode);

int 	x2pixel( GRAPH_DATA *graph,  time_t x);

int 	y2pixel( GRAPH_DATA *graph,  float y);

float 	pixel2y ( GRAPH_DATA *graph, int ypix);

time_t 	pixel2x ( GRAPH_DATA *graph, int xpix);

int 	getMyColor ( Display *display, char *colorName);

int 	pickMyColor( GRAPH_INFO *ginfo , int ntrace);

void     init_TSinit_gdata(GRAPH_DATA *graph);

int		get_default_showcat ( );


#endif

