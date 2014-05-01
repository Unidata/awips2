/* ***************************************** */
/*	File:           TimeSeries_show.h    */
/*      Date:           April 1999           */
/*      Author:         Sung Vo              */
/*      Purpose:        Provide support for  */
/*      the Time Series Control Display      */
/* ***************************************** */

#ifndef _TimeSeries_show_h
#define _TimeSeries_show_h

#include "TSdbx.h"
#include "TSGRAPH.h"
#include "TSutils.h"

/* ********************* */
/* prototype definations */
/* ********************* */

void    Start_TimeSeries(Widget w, XtPointer ptr, XtPointer cbs);
void 	Manage_TimeSeries ( Widget w, XtPointer ptr, XtPointer cbs);
void    show_TimeSeries(Widget w, XtPointer ptr, XtPointer cbs);
void 	TimeSeries_callbacks( void );
void 	TSsetup_gc( void );

void 	TSactive_graph_CB (Widget w, XtPointer ptr, XtPointer cbs);
void 	TSpageUp_CB (Widget w, XtPointer ptr, XtPointer cbs);
void 	TSpageDown_CB (Widget w, XtPointer ptr, XtPointer cbs);
void 	TSexpose_resize_CB (Widget w, XtPointer ptr, XtPointer cbs);
void 	TSpress_CB (Widget w, XtPointer ptr, XEvent *event);
void 	TSrelease_CB (Widget w, XtPointer ptr, XEvent *event);
void 	TSkey_CB (Widget w, XtPointer ptr, XEvent *event);
void 	TSdrag_CB (Widget w, XtPointer ptr, XEvent *event);

void    set_graph(Widget wid , XtPointer ptr, XtPointer call_data);
void    set_graph_show(Widget wid , XtPointer ptr, XtPointer call_data);
void    set_graph_scale(Widget wid , XtPointer ptr, XtPointer call_data);
void    set_plot(Widget wid , XtPointer ptr, XtPointer call_data);
void    set_batch_scale(Widget wid , XtPointer ptr, XtPointer call_data);



/*Added by guoxian zhou 05-2004 */
void	TSgrid_CB(Widget w, XtPointer client_data , XtPointer call_data ) ;
void 	TSZoom_CB (Widget w, XtPointer client_data , XtPointer call_data );
void	TSFloodCat_CB (Widget w, XtPointer client_data , XtPointer call_data );
void	TSPlot_CB (Widget w, XtPointer client_data , XtPointer call_data );
void 	TSBatchScaleStages_CB (Widget w , XtPointer client_data , 
                               XtPointer call_data ) ;
void 	TSpcAspp_CB (Widget w, XtPointer client_data , XtPointer call_data );
void	TSfcstonly_CB(Widget w, XtPointer client_data , XtPointer call_data ) ; 

void 	TSsave_CB (Widget w, XtPointer ptr, XtPointer cbs);
void    TSprint_noinverse_CB(Widget w, XtPointer ptr, XtPointer cbs);
void    TSprint_inverse_CB(Widget w, XtPointer ptr, XtPointer cbs);
void 	TSClose_CB (Widget w, XtPointer ptr, XtPointer cbs);
void	TSTracesClose_CB ( Widget w, XtPointer ptr, XtPointer cbs);

void 	TSTracePB_CB (Widget w, XtPointer ptr, XtPointer cbs);
void 	TSAddPB_CB (Widget w, XtPointer ptr, XtPointer cbs);
void 	TSDeletePB_CB (Widget w, XtPointer ptr, XtPointer cbs);
void 	TSMovePB_CB (Widget w, XtPointer ptr, XtPointer cbs);
void    TSSetMissingPB_CB (Widget w, XtPointer ptr, XtPointer cbs);
void 	TSEditDonePB_CB (Widget w, XtPointer ptr, XtPointer cbs);
void	highlight_select_options();
void	init_TScolors();


int 	display_floodcat();
void 	display_TSprev_page ();
void	display_TSnext_page ();

void    TSselectfile_CB(Widget w, XtPointer ptr, XtPointer cbs);
void    TSclosefile_CB(Widget w, XtPointer ptr, XtPointer cbs);
void 	create_TSTracesDS (Widget parent);

extern	void TSpressEdit_CB (Widget w, XtPointer ptr, XEvent *event);
extern	void TSreleaseEdit_CB (Widget w, XtPointer ptr, XEvent *event);
extern	void TSdragEdit_CB (Widget w, XtPointer ptr, XEvent *event);
extern	void TSDeleteData ();


#endif
