/* ***************************************** */
/*      File:           plots.h              */
/*      Date:           Jan 2002             */
/*      Author:         Ai Vo                */
/*      Purpose:                             */
/*                                           */
/* ***************************************** */

#ifndef _TSUtils_H
#define _TSUtils_H

#include "MotifHeader.h"
#include "TSWidget.h"

void 	setUpGC();
GC      IFP_Map_xs_create_xor_gc(Widget w);
GC      xs_create_xor_gc(Widget w);
void    TSSetColor (GC gc, Widget w, char *colorName);
int     getColorByName ( Widget w, char *name );
void    updateGraphDimension ( GRAPH  *g);
int     x2pixel( GRAPH *graph, time_t x);
int     y2pixel( GRAPH *graph, float y);
float   pixel2y ( GRAPH *graph, int ypix );
time_t  pixel2x ( GRAPH *graph, int xpix );
void    clear_drawingArea(GRAPH *graph);
void    draw_yaxis( GRAPH *graph) ;
void    draw_xaxis(GRAPH *graph);
void    draw_grid(GRAPH *graph);
void 	drawing_crosshairs();
void    TSDisplay ( GRAPH *graph);

void 	clearDrawAInfo ();

void    reDrawAll ();
void    reDrawGraph ( GRAPH *graph);

void    adjust_ymaxmin(float minval,
                float maxval,
                float *newmin,
                float *newmax,
                float *dinc);

void    search_and_display_trace(int xpix, int ypix, time_t xsecs);
void    addTStrace( GRAPH *graph, TRACE *tptr);

void    TSAddButton( GRAPH *graph);

int     colorLookUp ( char *traceType);

void read_ymaxminFile();
void write_ymaxminFile();
void remove_ymaxminFile();

#endif
