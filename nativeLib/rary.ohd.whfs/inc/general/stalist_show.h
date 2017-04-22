/*
	File:		stalist_show.h
	Date:		4/18/1997
	Author:		Paul Taylor
	
	Purpose:	Provides support for the Station List Report.
*/


#ifndef stalist_show_h
#define stalist_show_h


#include "StationList.h"


/*
	Defines.
*/

	/* (BUFSIZ is being used as well) */
#define STALIST_LINE_BUFSIZ	200  /* max characters in a line is ~ 132 */

#define STALIST_PAGE_BUFSIZ	66*STALIST_LINE_BUFSIZ


#define STALIST_SORTBY_LID	0	/* "StaList Page" Types (for printing also) */
#define STALIST_SORTBY_NAME	1
#define STALIST_SORTBY_COUNTY	2
#define STALIST_SORTBY_BASIN	3
#define STALIST_SORTBY_OBSERVER	4

#define STALIST_RECORD_LIMIT_PER_PAGE	54

#define	STALIST_LID	1	/* Types used for ordering columns */
#define	STALIST_NAM	2
#define	STALIST_COU	3
#define	STALIST_BAS	4
#define	STALIST_WFO	5
#define	STALIST_OBS	6


#define	STALIST_OPTION_PRINT	0
#define	STALIST_OPTION_EMAIL	1


/*********************/
/* General Functions */
/*********************/
void	StaList_AddCallbacks(void);
void	StaList_RemoveCallbacks(void);


/***************/
/* Print/Email */
/***************/
void	StaList_PE_ClearAll(Widget w, XtPointer ptr, XtPointer cbs);
void	StaList_PE_SetLid(Widget w, XtPointer ptr, XtPointer cbs);

void	StaList_PE_ClearItems(Widget w, XtPointer ptr, XtPointer cbs);
void	StaList_PE_CheckItems(Widget w, XtPointer ptr, XtPointer cbs);

void	StaList_PE_Manager(Widget w, XtPointer ptr, XtPointer cbs);


/***************/
/* Page Access */
/***************/
void	StaList_LoadTextWidget	(int page);
void	StaList_checkPB		(Widget w, XtPointer ptr, XtPointer cbs);

char*	StaList_GetText		(int page, int reason); /* for specific page */

char * StaList_GenerateReport(int pageType, int reason,
                              char tmp_dir [ ],       
                              long *fileLength);

char*	StaList_ListOfLocations(int reason, StationList **infoPtr, char *date,
				int pageNum, int pageType, char *optHeader,
				long *bufLength);
	/* ListOfLocations() returns a buffer, for 1 page at a time */


/*****************/
/* String Access */
/*****************/



/*******************/
/* Database Access */
/*******************/
/*
	NOTE: The StaList_Lid, StaList_Name, StaList_County,
	      StaList_Basin, & StaList_Observer functions
	      all access the StationList view.
*/


/*****************/
/* Miscellaneous */
/*****************/



#endif



