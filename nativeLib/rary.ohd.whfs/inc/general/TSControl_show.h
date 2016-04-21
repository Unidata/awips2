
/* ************************************************************************* */
/*	File:    TSControl_show.h                                            */
/*      Date:    April 1999                                                  */
/*      Author:  Sung Vo                                                     */
/*      Purpose: Provide support for                                         */
/*      the Time Series Control Dialog                                       */
/*      History  April ?, 1999   Sung Vo         Original Coding             */
/*               June 7, 2001    Bryon Lawrence  Updated the prototype       */
/*                                            of the search_GroupList        */
/*                                            routine to reflect that        */
/*                                            this routine returns           */
/*                                            an integer value.              */
/*                                            Updated the prototype of       */
/*                                            the getLidFromStnList          */
/*                                            routine to relect that         */
/*                                            this routine accepts           */
/*                                            a const int calling            */
/*                                            argument.                      */
/*                                                                           */ 
/* ************************************************************************* */


#ifndef _TSControl_show_h
#define _TSControl_show_h


#include "TSdbx.h"
#include "TSControl.h"
#include "TimeSeries.h"
#include "TSgetinfo.h"
#include "tabular_info.h"
#include "tabular_show.h"
#include "tsgen_info.h"
#include "List.h"	

/* ********************* */
/* Defines               */
/* ********************* */

#define SHOW_LOC_LEN	20
#define SHOW_RIVER_LEN	28

#define MAX_GRAPH_NO	2

/* ********************* */
/* Prototype definitions */
/* ********************* */

extern void TSinit_memory();
extern void Start_TimeSeries(Widget w, XtPointer ptr, XtPointer cbs);
extern void show_TSControlDS(Widget w, TSGEN_INFO tsgen);
extern void TSControl_callbacks();

extern void TSC_ArrowEndUp_CB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_ArrowEndDown_CB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_ArrowBeginUp_CB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_ArrowBeginDown_CB ( Widget w, XtPointer ptr, XtPointer cbs);

extern void TSC_Beginmonth_CB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_Beginday_CB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_Beginyear_CB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_Beginhour_CB ( Widget w, XtPointer ptr, XtPointer cbs);

extern void TSC_Endmonth_CB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_Endday_CB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_Endyear_CB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_Endhour_CB ( Widget w, XtPointer ptr, XtPointer cbs);

extern int day_in_month ( int year, int month );
extern int day_number(int year, int month, int day );

extern void TSC_Group_CB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_Station_CB ( Widget w, XtPointer ptr, XtPointer cbs);

extern void TSC_Class_allCB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_Class_CB ( Widget w, XtPointer ptr, XtPointer cbs);

/*Added by guoxian zhou 04/21/2004*/
extern void reorder_ts_CB(Widget w, XtPointer ptr,  XtPointer call_data);
extern void reorder_ts();

extern void TSC_SearchMode(Widget w, XtPointer ptr,  XtPointer call_data);


extern void TSC_Graph_CB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_Tab_CB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_Close_CB ( Widget w, XtPointer ptr, XtPointer cbs);

extern void TSC_GroupLI_CB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_StationLI_CB ( Widget w, XtPointer ptr, XtPointer cbs);
extern void TSC_PCodeLI_CB ( Widget w, XtPointer ptr, XtPointer cbs);

/*Modified by guoxian zhou 04-2004*/
extern void TSErrorDialog( Widget widget, char *msg );
extern void TSCkey_CB (Widget w, XtPointer ptr, XEvent *event);

extern	void init_TStime( time_t starttime, time_t endtime);
extern	void conv_mdyh(time_t secs, int *m, int *d, int *y, int *h);
extern	void get_TSstation_info ( );
extern 	void load_Ingestfilter ( char *lid );

extern	void loadTSGrp_List();
extern	void setTSGrp_byPos( int pos );
extern	void setTSGrp_byLid(const char *lid);
extern  int search_GroupList(const char *str);

extern  int  sort_display_name (  RussText *buf,  RussText *buf1, int nitems);

extern  int  loadTSStn_List(const char *where, int pos);
extern	void setTSStn_byPos( int pos );
extern	void setTSStn_byLid(const char *lid);
extern  int  search_StnList(const char *str);
extern	char *getLidFromStnList(const int pos);
extern	void set_stnclass_allon();

extern 	char *conv_dur2text ( int dur );
extern	void disable_close_func( Widget w );
extern  int load_hightlight( TSGEN_INFO tsgen);
/*added */
extern  void getStnRiverName(const char *lid, char *stn_name, char *river_name); 

extern char *load_PEdesc(char *pe);  
extern char *load_TSdesc(char *ts);

#endif
