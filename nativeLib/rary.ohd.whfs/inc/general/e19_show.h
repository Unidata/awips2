/*
	File:		e19_show.h
	Date:		July 1996
	Author:		Paul Taylor
	
	Purpose:	Provides support for the E-19 Report.
*/


#ifndef e19_show_h
#define e19_show_h

#include "Benchmark.h"
#include "Crest.h"
#include "Dcp.h"
#include "Location.h"
#include "Telem.h"
#include "Observer.h"
#include "Riverstat.h"
#include "Gage.h"
#include "Datum.h"
#include "Pub.h"
#include "Refer.h"
#include "Lowwater.h"
#include "Descrip.h"
#include "Flood.h"
#include "Floodstmt.h"
#include "Floodcat.h"
#include "Reservoir.h"
#include "Rescap.h"
#include "Rating.h"
#include "Contacts.h"
/*
#include "Unitgraph.h"
#include "Param.h"
*/


/*
	Defines.
*/

	/* (BUFSIZ is being used as well) */
#define E19_BUFSIZ	7920	/* 60 rows of 132 characters each*/
				/* (was 60 rows of 80 characters each) */

#define E19_COVER	0	/* "E19 Page" Types (for printing also) */
#define E19_MAPPAGE	1
#define E19_BENCHMARKS	2
#define E19_GAGES	3
#define E19_HISTORY	4
#define E19_CRESTS	5
#define E19_LOWWATER	6
#define E19_CONDITIONS	7
#define E19_DAMAGE	8
#define E19_STAFFGAGE	9
#define E19_CONTACTS	10


#define E19_ALLPAGES	11	/* (for initial purposes only) */


#define E19_GAGE_READING_CREST		0	/* "Highest Crest" Types */
#define E19_HIGH_WATERMARKS_CREST	1
#define E19_TIME_CREST			2


#define	E19_STANDARD_LEFT_MARGIN	10	/* for E19_CreateFooter() */


#define	E19_LREVISE_TYPE	0	/* to use info->loc->lrevise   */
#define	E19_RREVISE_TYPE	1	/* to use info->river->rrevise */




typedef struct e19_info_st
{
   Benchmark	*bench;
   Crest	*crest;
   Dcp		*dcp;
   Location	*loc;
   Telem	*telem;
   Observer	*obs;
   Riverstat	*river;
   Gage		*gage;
   Datum	*datum;
   Pub		*pub;
   Refer	*refer;
   Lowwater	*low;
   Descrip	*descrip;
   Flood	*flood;
   Floodstmt	*floodst;
   Floodcat	*floodcat;
/*
   Unitgraph	*unit;
   Param	*param;
*/
   Reservoir	*res;
   Rating	*rating;
   Contacts	*contacts;
   
} e19_info_type;




/*********************/
/* General Functions */
/*********************/
void	E19_AddCallbacks(void);
void	E19_RemoveCallbacks(void);

void	E19_checkPB(Widget w, XtPointer ptr, XtPointer cbs);



/***************/
/* Print/Email */
/***************/
char*	E19_PE_Manager(void); /* gets text based on TBs on Print/Email DS */

void	E19_PE_ClearAll(Widget w, XtPointer ptr, XtPointer cbs);
void	E19_PE_SetCover(Widget w, XtPointer ptr, XtPointer cbs);

void	E19_PE_ClearItems(Widget w, XtPointer ptr, XtPointer cbs);
void	E19_PE_CheckItems(Widget w, XtPointer ptr, XtPointer cbs);

void	E19_PE_Print(Widget w, XtPointer ptr, XtPointer cbs);
void	E19_PE_Email(Widget w, XtPointer ptr, XtPointer cbs);




/***************/
/* Page Access */
/***************/
void	E19_LoadTextWidget(void); /* gets AllPages and loads Txt widget */
char*	E19_GetText(int page, int reason); /* for a specific page, or E19_ALLPAGES */

long	E19_FindHeader(int page); /* locate header & update Txt widget */

char*	E19_AllPages	(int reason);

char*	E19_Cover	(e19_info_type *info, int reason);
char*	E19_MapPage	(e19_info_type *info, int reason);
char*	E19_Benchmarks	(e19_info_type *info, int reason);
char*	E19_Gages	(e19_info_type *info, int reason);
char*	E19_History	(e19_info_type *info, int reason);
char*	E19_Crests	(e19_info_type *info, int reason);
char*	E19_LowWater	(e19_info_type *info, int reason);
char*	E19_Conditions	(e19_info_type *info, int reason);
char*	E19_Damage	(e19_info_type *info, int reason);
char*	E19_StaffGage	(e19_info_type *info, int reason);
char*	E19_Contacts	(e19_info_type *info, int reason);




/*****************/
/* String Access */
/*****************/



/*******************/
/* Database Access */
/*******************/
e19_info_type*	get_E19_Info	(int init_flag);
void		set_E19_Info_Ptr(e19_info_type* infoPtr);

void		E19_SetupInfo	(e19_info_type *infoPtr);
void		E19_FreeInfo	(e19_info_type **infoPtr);



/*****************/
/* Miscellaneous */
/*****************/
char*	E19_HighestCrest(int option, char* str_date);
char*	E19_CreateParamList(char* lid, char source);
char*	E19_GetNextDcpSet(char* in_buffer, int num, char** out_buffer);
char*	E19_GetNextTelmSet(char* in_buffer, int num, char** out_buffer);

char*	E19_AdvanceToFooter(char *buf, int linesFromTopOfPage, int loop);

char*	E19_CreateFooter(e19_info_type *info, int revise_type, char *pdate, char *formName,
			 int pageType, char *pageTypeLabel, char *optHeader,
			 int leftMargin, int *init_footer, int reason);

char*	E19_BuildStaffGageString(float* i);


#endif



