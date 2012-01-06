/* ***************************************** */
/*	File:           tsgen_info.h         */
/*      Date:           April 1999           */
/*      Author:         Sung Vo              */
/*      Purpose:        Provide support for  */
/*      invoking Time Series Control Dialog  */
/*      and      Time Series Display         */
/* ***************************************** */

#ifndef _tsgen_info_h
#define _tsgen_info_h

#include "DbmsDefs.h"
#include "Xtools.h"

#define         GRAPH_TS        0
#define         TABULAR_TS      1
#define         NON_STANDALONE  0
#define         STANDALONE      1


/* *********************************************** */
/* Structure to hold Physical Element, Type Source */
/* Extremnum and Duration.                         */
/* *********************************************** */
struct PEDTSE 
{
     
	char    pe[SHEF_PE_LEN + 1],
                ts[SHEF_TS_LEN + 1],
                extremum[SHEF_EX_LEN + 1];

	int	dur;

};

/* ************************************************** */
/* Time Series general data structure for invocation  */
/* purposes. Because Time Series can be invokable by  */
/* a simple main() to run as a standalone appication. */
/* This structure can be used to activate Time Series */
/* by a callbacks from a Push button.                 */
/* ************************************************** */
typedef struct _TSGEN_INFO
{

	char    lid[LOC_ID_LEN + 1];

        int  	nitems,	
		standalone,     
        	pedtse_defined,
        	display_mode,
        	group_check;   


        time_t  Begin_time, 
                End_time;

	struct PEDTSE pedtse[10];	

}TSGEN_INFO;

#endif
