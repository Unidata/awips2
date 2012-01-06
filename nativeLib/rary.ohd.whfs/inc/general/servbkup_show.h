/*
	File:		servbkup_show.h
	Date:		10/26/2000
	Author:		Julie Daniel
	
	Purpose:	Provides support for the Service Backup Report.
*/


#ifndef servbkup_show_h
#define servbkup_show_h


#include "LocView.h"


/*
	Defines.
*/

	/* (BUFSIZ is being used as well) */
#define SERVBKUP_LINE_BUFSIZ	200  /* max characters in a line is ~ 132 */

#define SERVBKUP_PAGE_BUFSIZ	66*SERVBKUP_LINE_BUFSIZ


#define SERVBKUP_SORTBY_LID	0	/* "ServBkup Page" Types 
					   (for printing also) */
#define SERVBKUP_SORTBY_WFO	1
#define SERVBKUP_SORTBY_HSA     2

#define SERVBKUP_RECORD_LIMIT_PER_PAGE 62


/***************/
/* Print/Email */
/***************/
void	ServBkup_PE_Print(Widget w, XtPointer ptr, XtPointer cbs);

/***************/
/* Page Access */
/***************/
void	ServBkup_LoadTextWidget	(int page);
void	ServBkup_checkPB		(Widget w, XtPointer ptr, XtPointer cbs);

char*	ServBkup_GetText		(int page, int reason); /* for specific page */


char*	ServBkup_GenerateReport(int pageType, int reason, 
                                char file_path[], long *fileLength);

char*	ServBkup_ListOfLocations(int reason, LocView **infoPtr, char *date,
				int pageNum, int pageType, char *optHeader,
				long *bufLength);
	/* ListOfLocations() returns a buffer, for 1 page at a time */


#endif
