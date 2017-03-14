/* ***************************************** */
/*	File:           TSgetinfo.h          */
/*      Date:           April 1999           */
/*      Author:         Sung Vo              */
/*      Purpose:        Provide support for  */
/*      the Time Series Control Display.     */
/*      Also provide debug capabilities      */
/* ***************************************** */

#ifndef _TSgetinfo_h
#define _TSgetinfo_h

#include "TSGRAPH.h"

#include "ShefDur.h"

/* ********************* */
/* prototype definations */
/* ********************* */

void get_optTStrace ( TRACE_INFO *trace, char *buf);
void get_optTSgraph ( GRAPH_INFO *page,  char *buf);
void get_optTSpage  ( PAGE_INFO  *graph, char *buf);
void get_optTSgroup ( GROUP_INFO *grp,   char *buf);

int get_TSpage_info ( char * group_name ) ;

void add_TStrace_info( GRAPH_INFO *ginfo, TRACE_INFO *tinfo);
void add_TSgraph_info( PAGE_INFO  *pinfo, GRAPH_INFO *ginfo);
void add_TSpage_info ( PAGE_INFO  *TPinfoPtr);

void add_TStrace_data( GRAPH_DATA *gptr, TRACE_DATA *tptr);
void add_TSgraph_data( PAGE_DATA  *pptr, GRAPH_DATA *gptr);
void add_TSpage_data ( PAGE_DATA  *PdataPtr);

void print_TStrace_info ( TRACE_INFO *tinfo);
void print_TSgraph_info ( GRAPH_INFO *ginfo);
void print_TSpage_info  ( PAGE_INFO  *pinfo);
void print_TSpages_info ();

void print_TStrace_data( TRACE_DATA *t);
void print_TSgraph_data( GRAPH_DATA *g);
void print_TSpage_data ( PAGE_DATA  *p);
void print_TSpages_data (int page);

void sort_TStraces( GRAPH_INFO *ginfo );

char 	*get_TSgroupName  ( char *buf);
void    decode_pedtse ( TRACE_INFO *tptr, ShefDur *durPtr );
int     lookup_dur ( ShefDur *durPtr, char *cdur );


#endif
