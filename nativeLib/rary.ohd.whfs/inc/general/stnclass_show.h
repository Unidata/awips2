/*
	File:		stnclass_show.h
	Date:		July 1996
	Author:		Paul Taylor
	
	Purpose:	Provides support for the Station Class Report.
*/


#ifndef stnclass_show_h
#define stnclass_show_h

#include "StnClass.h"


/*
	Defines.
*/

	/* (BUFSIZ is being used as well) */
#define STNCLASS_BUFSIZ	7920	/* 60 rows of 132 characters each*/
				/* (was 60 rows of 80 characters each) */

#define STNCLASS_SUMMARY	0	/* "StnClass Page" Types (for printing also) */

#define STNCLASS_ALLPAGES	1	/* (for initial purposes only) */


#define	STNCLASS_STANDARD_LEFT_MARGIN	7



typedef struct stnclass_info_st
{
   StnClass	*stnclass;
   
} stnclass_info_type;



/*********************/
/* General Functions */
/*********************/
void	StnClass_AddCallbacks(void);
void	StnClass_RemoveCallbacks(void);



/***************/
/* Print/Email */
/***************/
char*	StnClass_PE_Manager(void);

void	StnClass_PE_Print(Widget w, XtPointer ptr, XtPointer cbs);
void	StnClass_PE_Email(Widget w, XtPointer ptr, XtPointer cbs);




/***************/
/* Page Access */
/***************/
void	StnClass_LoadCompleteText(void); /* gets AllPages and loads Txt widget */
char*	StnClass_GetText(int page, int reason); /* for a specific page, or STNCLASS_ALLPAGES */

char*	StnClass_AllPages	(int reason);
char*	StnClass_Summary	(stnclass_info_type *info, int reason);  /* StnClass Report */



/*****************/
/* String Access */
/*****************/



/*******************/
/* Database Access */
/*******************/
stnclass_info_type*	get_StnClass_Info    (int init_flag);
void			set_StnClass_Info_Ptr(stnclass_info_type *infoPtr);

void			StnClass_SetupInfo   (stnclass_info_type *infoPtr);
void			StnClass_FreeInfo    (stnclass_info_type **infoPtr);



/*****************/
/* Miscellaneous */
/*****************/
char*	StnClass_CharToString(char c);
char*	StnClass_AdvanceToFooter(char *buf, int linesFromTopOfPage, int loop);
char*	StnClass_CreateHeader(int *init_header, int reason);
char*	StnClass_CreateFooter(int leftMargin, int *init_footer);
void    StnClass_LoadTextWidget(void);



#endif



