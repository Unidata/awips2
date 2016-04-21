
/* ***************************************** */
/*	File:           TSedit.h             */
/*      Date:           April 1999           */
/*      Author:         Sung Vo              */
/*      Purpose:        Provide support for  */
/*      the Time Series graphical Editing    */
/* ***************************************** */

#ifndef _TSedit_h
#define _TSedit_h

#include "TSdbx.h"
#include "TimeSeries.h"
#include "TSGRAPH.h"
#include "TSutils.h"
#include "rating_util.h"

/* ********************* */
/* prototype definations */
/* ********************* */

void    TSpressEdit_CB (Widget w, XtPointer ptr, XEvent *event);
void    TSreleaseEdit_CB (Widget w, XtPointer ptr, XEvent *event);
void    TSdragEdit_CB (Widget w, XtPointer ptr, XEvent *event);

int 	find_TSmove_index ( int x, int y );
int 	find_TSinsert_point ( int x, int y );
int 	TSadd_checkEdit ( int x, int y );
int  	find_left_index  ( TRACE_DATA  *tptr , int n);
int  	find_right_index ( TRACE_DATA  *tptr , int n);
int 	find_edit_trace (int x, int y);

int 	check4valid_select( );

void	DeSentitize_edit_options();
void	Sentitize_edit_options();
void 	reset_fg_button( Widget w);

void 	update_TSadd_data ();
void 	update_TSdelete_data ();
void    update_TSMissing_data ();
void 	update_TSmove_data ();

void 	draw_move();
void 	draw_add ();
void 	draw_box();

void 	show_active_page_label( );
void	show_active_graph_label( int active_graph);
void 	show_active_trace_label ();
void 	save_confirmation( Widget w );
void    save_cancel_edit ( Widget w, XtPointer ptr, XtPointer cbs);
void 	Update_database( TRACE_DATA *ptr);

time_t	conv_ppdur2min ( GRAPH_DATA *graph, int edit_trace);

void getFcstProd_idtime (char *where, char *tablename, char *product_id, dtime_t *producttime);
void getObsProd_idtime  (char *where, char *tablename, char *product_id, dtime_t *producttime);

#endif

