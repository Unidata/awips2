/*
	File:		tabular_show.h
	Date:		September 1995 + December 1999
	Author:		Dale Shelton
			Chip Gobs
			Russell Erb, Mark Glaudemans
	
	Purpose:
	
*/


#ifndef tabular_show_h
#define tabular_show_h


/*
	Defines.
*/

#define	HDRDEFAULT "   Value     Time(Z)    RV SQ QC  Product       Time         Posted"
#define	HDRSTAGE  "   Value     Flow      Time(Z)    RV SQ QC  Product       Time         Posted"
#define	HDRFLOW  "   Value     Stage     Time(Z)    RV SQ QC  Product       Time         Posted"

/*
	Includes.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <time.h>
#include <unistd.h>

#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/Protocols.h>

#include "Xtools.h"
#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "Observation.h"
#include "IngestFilter.h"
#include "ShefEx.h"
#include "ShefDur.h"
#include "ShefPe.h"
#include "ShefTs.h"
#include "ShefQc.h"
#include "LocView.h"
#include "RejectedData.h"
#include "tabular.h"
#include "time_convert.h"
#include "Forecast.h"
#include "tabular_info.h"
#include "LoadUnique.h"
#include "rating_util.h"
#include "QualityCode.h"
#include "Riverstat.h"

/*
	couldn't find header for this function
	<unistd.h> doesn't seem to have it
*/

char * getlogin(void);

/*
	type definitions
*/

typedef struct ObsStruct
{

     Observation * obsHead; 

} ObsStruct;

typedef struct FcstStruct
{

   Forecast         *fcstHead;

} FcstStruct;

typedef struct TimeSeriesType
{

	char	lid[LOC_ID_LEN + 1]; 
	char    name[20];
	char	pe[SHEF_PE_LEN + 1]; 
	int	dur;
	char	ts[SHEF_TS_LEN + 1];
	char	extremum[SHEF_EX_LEN + 1];
	char	basistime[ANSI_TIME_LEN + 1];

} TimeSeriesType;

void initObsStruct(ObsStruct *obsStruct);
void initFcstStruct(FcstStruct *fcstStruct);
ObsStruct  * getObsStruct(void);
FcstStruct * getFcstStruct(void);


/*
	Function prototypes.
*/
void	tabular_show(Widget w, TAB_INFO ts_info);
void	tabular_callbacks(void);
void	loadTabularListObs(Observation *obsPtr);
void	loadTabularListFcst(Forecast *fcstPtr);

/*
	Utility prototypes.	
*/
void	tabular_retrieve_data(Widget w, XtPointer ptr, XtPointer cbs);
char *  getDurCode(ShefDur *durPtr, int dur);
void	tabular_load_timeseries();

/*
	filter where clause creation prototypes
*/	
void	createGetWhereObs(char *where);
void	createGetWhereFcst(char *fcst_where);

/*
	Prototype for function which returns the selected Observation
*/
void  loadRowIntoWidgets(int pos);

/*
  	Callback functions
*/ 
void	tabular_close(Widget w, XtPointer ptr, XtPointer cbs);

void	tabular_save(Widget w, XtPointer ptr, XtPointer cbs);
void    tabular_setmissing(Widget w, XtPointer ptr, XtPointer cbs);
void    tabular_setqc(Widget w, XtPointer ptr, XtPointer cbs);
void    tabular_copy_ts(Widget w, XtPointer ptr, XtPointer cbs);
void	tabular_delete(Widget w, XtPointer ptr, XtPointer cbs);
void 	tabular_del_conf(Widget w, XtPointer ptr, XtPointer cbs);

void	tabular_question_cancel(Widget w, XtPointer ptr, XtPointer cbs);

void	TS_data_RowSelection(Widget w, XtPointer ptr, XtPointer cbs);
void	TS_type_RowSelection(Widget w, XtPointer ptr, XtPointer cbs);

void	getTableName(const char *pe, const char *ts, char *tablename);

void	tabular_shef_encode(Widget w, XtPointer ptr, XtPointer cbs);

void    tabular_send_conf(Widget w, XtPointer ptr, XtPointer cbs);
void    tabular_shef_send(Widget w, XtPointer ptr, XtPointer cbs);

void    tabular_clear_conf(Widget w, XtPointer ptr, XtPointer cbs);
void    tabular_shef_clear(Widget w, XtPointer ptr, XtPointer cbs);

void    tabular_edit_shef(Widget w, XtPointer ptr, XtPointer cbs);
void    tabular_edit_issue(Widget w, XtPointer ptr, XtPointer cbs);

int     create_table_file(void);
void    tabular_print_table(Widget w, XtPointer ptr, XtPointer cbs);
void	tabular_get_filename(Widget w, XtPointer ptr, XtPointer cbs);
void    tabular_save_table(Widget w, XtPointer ptr, XtPointer call_data );
void	tabular_close_filesb(Widget w, XtPointer ptr, XtPointer cbs);


void load_forecast_typesource(char current_pe[],
                              int current_dur,
			      char current_extremum[]);
void    tabularLatestBasis(Widget w, XtPointer ptr, XtPointer cbs);

void    disableCloseFunc( Widget w );

void	tabular_insert_edit(Widget w, XtPointer ptr, XtPointer cbs);
void	tabinsert_show(Widget w);
void    load_insert_ts();
void	tabinsert_save(Widget w, XtPointer ptr, XtPointer cbs);
void	tabinsert_cancel(Widget w, XtPointer ptr, XtPointer cbs);


#endif
