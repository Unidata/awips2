/*
	File:		b44a_show.h
	Date:		4/18/1997
	Author:		Paul Taylor
	
	Purpose:	Provides support for the B-44A Report.
*/


#ifndef b44a_show_h
#define b44a_show_h

#include "Location.h"
#include "Observer.h"
#include "Riverstat.h"


/*
	Defines.
*/

	/* (BUFSIZ is being used as well) */
#define B44A_BUFSIZ	7920	/* 60 rows of 132 characters each*/
				/* (was 60 rows of 80 characters each) */

#define B44A_COOPERATIVE  0  /* "B44A Page" Types (for printing also) */

#define B44A_ALLPAGES	  1  /* (for initial purposes only) */



typedef struct b44a_info_st
{
   Location	*loc;
   Observer	*obs;
   Riverstat	*river;

} b44a_info_type;



/*********************/
/* General Functions */
/*********************/
void	B44A_AddCallbacks(void);
void	B44A_RemoveCallbacks(void);



/***************/
/* Print/Email */
/***************/
char*	B44A_PE_Manager(void);

void	B44A_PE_Print(Widget w, XtPointer ptr, XtPointer cbs);
void	B44A_PE_Email(Widget w, XtPointer ptr, XtPointer cbs);




/***************/
/* Page Access */
/***************/
void	B44A_LoadCompleteText(void); /* gets AllPages and loads Txt widget */
char*	B44A_GetText(int page, int reason); /* for a specific page, or B44A_ALLPAGES */

char*	B44A_AllPages	(int reason);
char*	B44A_Cooperative(b44a_info_type *info, int reason);  /* B-44A Report */



/*****************/
/* String Access */
/*****************/



/*******************/
/* Database Access */
/*******************/
b44a_info_type*	get_B44A_Info	(int init_flag);
void		set_B44A_Info_Ptr(b44a_info_type *infoPtr);

void		B44A_SetupInfo	(b44a_info_type *infoPtr);
void		B44A_FreeInfo	(b44a_info_type **infoPtr);



/*****************/
/* Miscellaneous */
/*****************/
void        B44A_LoadTextWidget(void);


#endif



