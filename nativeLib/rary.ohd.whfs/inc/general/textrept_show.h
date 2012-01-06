/*
	File:		textrept_show.h
	Date:		April 11, 1997
	Author:		Paul Taylor
	
	Purpose:	Provides support for the Text Reports DS.
	
*/

#ifndef textrept_show_h
#define textrept_show_h

#include "DbmsDefs.h"


#define TEXTREPORTS_REASON_NORMAL			0
#define TEXTREPORTS_REASON_PRINT_EMAIL_OR_SAVE		1


//#define	TEXTREPORTS_MAX_LINES_PER_PAGE	60
//#define	FOOTER_POSITION	(TEXTREPORTS_MAX_LINES_PER_PAGE - 4)


#define	TEXTREPORTS_E19_TEXT		0
#define TEXTREPORTS_E19A_TEXT		1
#define TEXTREPORTS_B44A_TEXT		2
#define TEXTREPORTS_STNCLASS_TEXT	3
#define TEXTREPORTS_STALIST_TEXT	4
#define TEXTREPORTS_SERVBKUP_TEXT	5



int TEXTREPORTS_MAX_LINES_PER_PAGE;
int FOOTER_POSITION;

typedef struct textrept_bufinfo_st
{
   char*	e19_buf;
   char*	e19a_buf;
   char*	b44a_buf;
   char*	stnclass_buf;
   
   char*	stalist_buf;

   char*	servbkup_buf;
   
} textrept_bufinfo;



void	ShowTextReportsDs(Widget w, char *lid);

void	CreateTextReportsDs		(Widget w, char *lid);
void	CreateTextReportsPrintEmailDs	(Widget w);
void	CreateTextReportsSaveDs		(Widget w);

void	ShowTextReportsPrintEmailDs(Widget w, XtPointer ptr, XtPointer cbs);
void	ShowTextReportsSaveDs	   (Widget w, XtPointer ptr, XtPointer cbs);

void	ShowTextReportsHelpDs	   (Widget w, XtPointer ptr, XtPointer cbs);


/*
	Functions to handle user interaction,
	setup operations, and report generation.
*/
void	TextReports_checkPB	(Widget w, XtPointer ptr, XtPointer cbs);

void	TextReports_GenerateReport	(Widget reportOM);
void	TextReports_MiscSetupOperations	(Widget reportPB);

void	TextReports_HideAuxilliaryOMs	(void);
void	TextReports_ShowAuxilliaryOM	(Widget om);

void	TextReports_LoadTextWidget	(Widget reportPB);


void	TextReports_Close	(Widget w, XtPointer ptr, XtPointer cbs);

void	TextReports_PE_Close	(Widget w, XtPointer ptr, XtPointer cbs);
void	TextReports_P_OutputBuffer	(char* buf);  /* for Printing */
void	TextReports_E_OutputBuffer	(char* buf);  /* for Emailing */
int	TextReports_E_EmptyAddress	(char* addr);
void	TextReports_setEmailSubject	(char *subject);

void	TextReports_SAVE_Save	(Widget w, XtPointer ptr, XtPointer cbs);
void	TextReports_SAVE_Close	(Widget w, XtPointer ptr, XtPointer cbs);


void	TextReports_SaveAll_Confirm  (Widget w, XtPointer ptr, XtPointer cbs);
void	TextReports_SaveAll	     (Widget w, XtPointer ptr, XtPointer cbs);
void	TextReports_writeTextIntoFile(int report_type, char *newLid);



/*
	Memory management.
*/
void			TextReports_FreeBufferMemory	(void);
textrept_bufinfo*	TextReports_GetBufInfo		(void);



/*
	For mapping & unmapping RC widgets on Print/Email screen.
*/
void	TextReports_unmap_all_PE_RCs(void);
void	TextReports_map_one_PE_RC(Widget w);



/*
	For setting proper callbacks.
*/
void	TextReports_AddCallbacks(void);

void	TextReports_Set_PE_ChildCallbacks(void);  /* sets up child callbacks */
void	TextReports_Remove_PE_ChildCallbacks(void);

void	TextReports_Add_E19_PE_Callbacks(void);		/* E-19 */
void	TextReports_Remove_E19_PE_Callbacks(void);

void	TextReports_Add_E19A_PE_Callbacks(void);	/* E-19A */
void	TextReports_Remove_E19A_PE_Callbacks(void);

void	TextReports_Add_B44A_PE_Callbacks(void);	/* B-44A */
void	TextReports_Remove_B44A_PE_Callbacks(void);

void	TextReports_Add_StaList_PE_Callbacks(void);	/* StaList */
void	TextReports_Remove_StaList_PE_Callbacks(void);

void	TextReports_Add_StnClass_PE_Callbacks(void);	/* StnClass */
void	TextReports_Remove_StnClass_PE_Callbacks(void);

void	TextReports_Add_ServBkup_PE_Callbacks(void);	/* ServBkup */
void	TextReports_Remove_ServBkup_PE_Callbacks(void);


/*
	Function for access to "current lid".
*/
char*	TextReports_getCurrentLid(void);



/*
	Miscellaneous functions.
*/
char*	TextReports_GetDate(void);  /* gets date in the form of MM/DD/YYYY */
			/* (inits first time, thereafter gets static memory) */

char*	TextReports_GetHydrologistName(void);

char*	TextReports_GetUserName(void);		/* these 2 functions aren't used */
char*	TextReports_InRealLife(FILE *fp);



/*
	HPPrinterCode functions.
*/
void	HPPrinterCodes_StandardCompressed(char *buf);
void	HPPrinterCodes_StandardPitch10(char *buf);
void	HPPrinterCodes_StandardPitch12(char *buf);
void	HPPrinterCodes_Bold(char *buf);
void	HPPrinterCodes_Normal(char *buf);
void	HPPrinterCodes_Italic(char *buf);
void	HPPrinterCodes_Upright(char *buf);
void	HPPrinterCodes_FormFeed(char *buf);



#endif



