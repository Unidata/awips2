/*
	File:		e19a_show.h
	Date:		July 1996
	Author:		Paul Taylor
	
	Purpose:	Provides support for the E-19A Report.
*/


#ifndef e19a_show_h
#define e19a_show_h

#include "Dcp.h"
#include "Location.h"
#include "Telem.h"
#include "Observer.h"
#include "Riverstat.h"


/*
	Defines.
*/

	/* (BUFSIZ is being used as well) */
#define E19A_BUFSIZ	7920	/* 60 rows of 132 characters each*/
				/* (was 60 rows of 80 characters each) */

#define E19A_SUMMARY	0	/* "E19A Page" Types (for printing also) */

#define E19A_ALLPAGES	1	/* (for initial purposes only) */



typedef struct e19a_info_st
{
   Dcp		*dcp;
   Location	*loc;
   Telem	*telem;
   Observer	*obs;
   Riverstat	*river;
   Floodcat	*fcat;
   Descrip	*descr;

} e19a_info_type;



/*********************/
/* General Functions */
/*********************/
void	E19A_AddCallbacks(void);
void	E19A_RemoveCallbacks(void);



/***************/
/* Print/Email */
/***************/
char*	E19A_PE_Manager(void);

void	E19A_PE_Print(Widget w, XtPointer ptr, XtPointer cbs);
void	E19A_PE_Email(Widget w, XtPointer ptr, XtPointer cbs);




/***************/
/* Page Access */
/***************/
void	E19A_LoadCompleteText(void); /* gets AllPages and loads Txt widget */
char*	E19A_GetText(int page, int reason); /* for a specific page, or E19A_ALLPAGES */

char*	E19A_AllPages	(int reason);
char*	E19A_Summary	(e19a_info_type *info, int reason);  /* E-19A Report */



/*****************/
/* String Access */
/*****************/



/*******************/
/* Database Access */
/*******************/
e19a_info_type*	get_E19A_Info	(int init_flag);
void		set_E19A_Info_Ptr(e19a_info_type *infoPtr);

void		E19A_SetupInfo	(e19a_info_type *infoPtr);
void		E19A_FreeInfo	(e19a_info_type **infoPtr);



/*****************/
/* Miscellaneous */
/*****************/
void        E19A_LoadTextWidget(void);



#endif



