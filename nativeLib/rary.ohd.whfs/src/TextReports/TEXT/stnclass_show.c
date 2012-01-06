/*
	File:		stnclass_show.c
	Date:		5/12/1997
	Author:		Paul Taylor

	Purpose:	Provides support for the Station Class Report.
*/


#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/FileSB.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>
#include <datetime.h>
#include <time.h>
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"

#include "GeneralUtil.h"

#include "textrept.h"
#include "textrept_show.h"
#include "stnclass_show.h"


#define	DEBUG_MODE	0	/* 0=False, 1=True */


/*
	Globals.
*/
static stnclass_info_type*	stnclassInfoPtr = NULL;

static char
   stnclass_hdr_summary[]    ="STATION CLASS REPORT";

static int		stnclass_first_page = STNCLASS_SUMMARY;
static int		stnclass_last_page = STNCLASS_SUMMARY;

/*
	Functions.
*/
void	StnClass_AddCallbacks(void)
{
   return;
}

//---------------------------------------------------------

void	StnClass_RemoveCallbacks(void)
{
   return;
}

//---------------------------------------------------------

char*	StnClass_PE_Manager(void)
{
   textrept_bufinfo*	bufInfoPtr = TextReports_GetBufInfo();
   char*  		text = NULL;
  

   /*
   	Return the complete StnClass text.
   */
   text = StnClass_GetText(STNCLASS_ALLPAGES, TEXTREPORTS_REASON_PRINT_EMAIL_OR_SAVE);
   Text_AddString(&bufInfoPtr->stnclass_buf, text, 1);
   Text_FreeString(&text);
   
   
   return(bufInfoPtr->stnclass_buf);
}
//---------------------------------------------------------


void	StnClass_PE_Print(Widget w, XtPointer ptr, XtPointer cbs)
{
	char*	buf = NULL;
	
	SetCursor(textreptFO, XC_watch);
	
	buf = StnClass_PE_Manager();	  /* obtain complete text buffer */
	TextReports_P_OutputBuffer(buf);  /* output buffer to printer */
	
	UnsetCursor(textreptFO);
	
	return;
}
//---------------------------------------------------------

	      
void	StnClass_PE_Email(Widget w, XtPointer ptr, XtPointer cbs)
{
	char*	buf = NULL;
	
	SetCursor(textreptFO, XC_watch);

	buf = StnClass_PE_Manager();	  /* obtain complete text buffer */
	TextReports_E_OutputBuffer(buf);  /* email buffer to internet address */
	
	UnsetCursor(textreptFO);

	return;
}
//---------------------------------------------------------

	      
void	StnClass_LoadTextWidget(void)
{
   textrept_bufinfo*	bufInfoPtr = TextReports_GetBufInfo();
   char*  		text = NULL;
   

   /*
   	Get the complete StnClass text
	and display it.
   */
   text = StnClass_GetText(STNCLASS_ALLPAGES, TEXTREPORTS_REASON_NORMAL);
   Text_AddString(&bufInfoPtr->stnclass_buf, text, 1);
   Text_FreeString(&text);
   

   if (bufInfoPtr->stnclass_buf != NULL)
      XtVaSetValues(textreptTE, XmNvalue, bufInfoPtr->stnclass_buf, NULL);
   else
      fprintf(stderr,"ERROR: Unable to load text properly...\n");
   
   return;
}
//---------------------------------------------------------


char*	StnClass_GetText(int page, int reason)
{
   stnclass_info_type	*info = get_StnClass_Info(False);
   char*  		text = NULL;

   switch(page)
   {
      case STNCLASS_ALLPAGES:	text = StnClass_AllPages(reason);	break;

      case STNCLASS_SUMMARY:	text = StnClass_Summary	(info, reason);	break;

      default:	fprintf(stderr, "ERROR: Could not get text...\n");
	 	break;
   }

   return(text);
}
//---------------------------------------------------------


char*	StnClass_AllPages(int reason)
{
   char*  alltext = NULL;
   char*  text = NULL;
   
   int    i;

   /*
   	Get the text for the given page(s).
   */
   for(i=stnclass_first_page; i<=stnclass_last_page; i++)
   {
      text = StnClass_GetText(i, reason);
      Text_Paginate(&text);
      
      if(i == stnclass_first_page)
	 Text_AddString(&alltext, text, 1);
      else
	 Text_AddString(&alltext, text, 0);

      Text_FreeString(&text);
   }

   return(alltext);
}
//---------------------------------------------------------


char*	StnClass_Summary(stnclass_info_type *info, int reason)
{
   StnClass	*scPtr = NULL;
   
   char*	buf = NULL;
   
   char		tmp1[STNCLASS_BUFSIZ];
   char		tmp2[STNCLASS_BUFSIZ];
   char		tmp3[STNCLASS_BUFSIZ];
   char		dcpstr[8], obsstr[8];
   
   int		available, avail;
   int		needed;
   int		count1, count2;
   int		loop;
   int		init_header = True;
   int		init_footer = True;

   
   /*
   	Title
   */
/*
sprintf(tmp1, "                              STATION CLASS REPORT\n\n");
*/
   memset ( ( void * ) tmp1 , '\0', sizeof(tmp1));
   Text_AddString(&buf, tmp1, 1);
   
   
   count1 = 0;
   
   
   strcat (tmp1, StnClass_CreateHeader(&init_header, reason));
   strcat (tmp1, "\n\n\n");
   strcat (tmp1, "       LID         STATION TYPE    DCP    OBSERVER    TELEMETRY DEVICE\n");
   strcat (tmp1, "       --------    ------------    ---    --------    ----------------\n");
   Text_AddString(&buf, tmp1, 0);

   
   
   count2 = Text_CountNewLines(buf);
   

   
   available = TEXTREPORTS_MAX_LINES_PER_PAGE - count1 - count2 - 6;
   avail = available;
   loop = 0;
   
   scPtr = info->stnclass;
   while(scPtr != NULL)
   {
      memset( ( void * ) tmp2 , '\0' , sizeof ( tmp2 ) ) ;
      
      strcpy(dcpstr, StnClass_CharToString(scPtr->dcp[0]));
      strcpy(obsstr, StnClass_CharToString(scPtr->observer[0]));
      
      sprintf(tmp2, "       %-8s    %-10s      %-3s       %-3s      %-s\n",
	      scPtr->lid, scPtr->disp_class, dcpstr, obsstr,
	      scPtr->telem_type);
      needed = 1;
      
      
      if (needed <= avail)
      {
	 Text_AddString(&buf, tmp2, 0);
	 
	 scPtr = (StnClass *) ListNext(&scPtr->node);
	 avail = avail - needed;
	 continue;
	 
      }
      else if (needed > avail)
      {
	 /* try to place FOOTER at the bottom */
	 Text_AddString(&buf, StnClass_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);
	 
	 
	 /*
	 	Do footer.
	 */
	 memset( ( void * ) tmp3 , '\0' , sizeof ( tmp3 ) ) ;
	 strcat(tmp3, StnClass_CreateFooter(STNCLASS_STANDARD_LEFT_MARGIN,
					    &init_footer));
	 Text_AddString(&buf, tmp3, 0);

	 
	 /*
	 	Do column header.
	 */
	 memset( ( void * ) tmp1 , '\0' , sizeof ( tmp1 ) ) ;
	 strcat (tmp1, StnClass_CreateHeader(&init_header, reason));
	 strcat (tmp1, "\n\n\n");
	 strcat (tmp1, "       LID         STATION TYPE    DCP    OBSERVER    TELEMETRY DEVICE\n");
	 strcat (tmp1, "       --------    ------------    ---    --------    ----------------\n");
	 Text_AddString(&buf, tmp1, 0);
	 
	 
	 avail = available + count1;
	 loop++;
	 continue;
	 
      }
   }
   


   /* try to place FOOTER at the bottom */
   Text_AddString(&buf, StnClass_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);

   
   memset ( ( void * ) tmp3 , '\0' , sizeof ( tmp3 ) ) ;
   strcat(tmp3, StnClass_CreateFooter(STNCLASS_STANDARD_LEFT_MARGIN,
				      &init_footer));
   Text_AddString(&buf, tmp3, 0);


   
   return(buf);
}
//---------------------------------------------------------


stnclass_info_type*	get_StnClass_Info(int init_flag)
{
   stnclass_info_type*	infoPtr = stnclassInfoPtr;  /* refers to global pointer */

   
   if (init_flag)
   {
      StnClass_FreeInfo(&infoPtr);
      
      if (infoPtr == (stnclass_info_type *) NULL)
      {
	 infoPtr = (stnclass_info_type *) malloc (sizeof(stnclass_info_type));
	 memset ( ( void * ) infoPtr, 0, sizeof(stnclass_info_type));
	 
	 StnClass_SetupInfo(infoPtr);	/* access the database */
      }
   }

   
   return(infoPtr);
}
//---------------------------------------------------------


void	set_StnClass_Info_Ptr(stnclass_info_type *infoPtr)
{
   stnclassInfoPtr = infoPtr;
   return;
}
//---------------------------------------------------------


void	StnClass_SetupInfo(stnclass_info_type *infoPtr)
{
   char where[BUFSIZ];
   
   
   /*
   	Get information for the following tables:
	
	StnClass.
   */

   sprintf(where, "WHERE lid = '%s'", TextReports_getCurrentLid());
   
   infoPtr->stnclass = GetStnClass(" ORDER BY lid ");
   
   return;
}
//---------------------------------------------------------


void	StnClass_FreeInfo(stnclass_info_type **infoPtr)
{
   if (*infoPtr != (stnclass_info_type *) NULL)
   {
      FreeStnClass	((*infoPtr)->stnclass);
      
      free(*infoPtr);
      
      stnclassInfoPtr = NULL;	/* set global to NULL */
   }
   
   return;
}
//---------------------------------------------------------


char*	StnClass_CharToString(char c)
{
   static char	string[5];
   
   memset ( ( void * ) string , '\0' , sizeof ( string ) ) ;
   
   if(c == 'T')	strcat(string, "Yes");
   else		strcat(string, "");
   
   return(string);
}
//---------------------------------------------------------


char*	StnClass_AdvanceToFooter(char *buf, int linesFromTopOfPage, int loop)
{
   static char	tmp1[STNCLASS_BUFSIZ];
   int		i;
   

   /*
   	Place FOOTER at the bottom of page.
   */
   memset ( ( void * ) tmp1, '\0', sizeof(tmp1));
   for(i=0;  i <( ((TEXTREPORTS_MAX_LINES_PER_PAGE * loop)+linesFromTopOfPage)
                  - Text_CountNewLines(buf));  i++)
   {
      strcat (tmp1, "\n");
   }
   
   return(tmp1);
}
//---------------------------------------------------------

char*	StnClass_CreateHeader(int *init_header, int reason)
{
   static char	*buf = NULL;
   
   static int	pageNum = 0;
   
   char		tmp1[STNCLASS_BUFSIZ];
   char		tmp2[STNCLASS_BUFSIZ];
   char		tmp3[STNCLASS_BUFSIZ];
   char		tmp4[STNCLASS_BUFSIZ];
   char		tmp5[STNCLASS_BUFSIZ];
   
   int		i;
   
   
   if (*init_header)
   {
      pageNum = 0;

      *init_header = False;
   }
   pageNum++;
   
   
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   memset( ( void * ) tmp2, '\0', sizeof(tmp2));
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   memset( ( void * ) tmp4, '\0', sizeof(tmp4));
   memset( ( void * ) tmp5, '\0', sizeof(tmp5));
   
   
   for (i=0; i<STNCLASS_STANDARD_LEFT_MARGIN; i++)
   {
      strcat(tmp1, " ");
   }
   strcat(tmp5, tmp1);

   
   sprintf(tmp2, "%-10s    %-9s", (char *) TextReports_GetDate(), " ");
   strcat(tmp5, tmp2);


   sprintf(tmp3, "%-20s", stnclass_hdr_summary);
   strcat(tmp5, tmp3);

   
   sprintf(tmp4, "                     Page %2i", pageNum);
   strcat(tmp5, tmp4);
   
   Text_AddString(&buf, tmp5, 1);

   
   return(buf);
}
//---------------------------------------------------------


char*	StnClass_CreateFooter(int leftMargin, int *init_footer)
{
   static char	*buf = NULL;
   
   static int	pageNum = 0;

   char		tmp1[STNCLASS_BUFSIZ];
   char		tmp2[STNCLASS_BUFSIZ];
   char		tmp3[STNCLASS_BUFSIZ];
   char		tmp4[STNCLASS_BUFSIZ];
   char		tmp5[STNCLASS_BUFSIZ];

   int		i;

   
   if (*init_footer)
   {
      pageNum = 0;

      *init_footer = False;
   }
   pageNum++;
   
   
   /*
   	Init buffer.
   */
   Text_AddString(&buf, "", 1);

   
   
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   memset( ( void * ) tmp2, '\0', sizeof(tmp2));
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   memset( ( void * ) tmp4, '\0', sizeof(tmp4));
   memset( ( void * ) tmp5, '\0', sizeof(tmp5));

   
   for (i=0; i<leftMargin; i++)
   {
      strcat(tmp1, " ");
   }

   sprintf(tmp2, "KEY:\n\n");
   
   sprintf(tmp3, "F - Fcst Point   D - Reservoir       S - Snow   O - Other\n");

   sprintf(tmp4, "R - River Data   P - Precipitation   T - Temp   U - Undefined\n");


   strcpy(tmp5, tmp1);  /* copy margin */
   strcat(tmp5, tmp2);	/* copy 1st line */

   strcat(tmp5, tmp1);	/* copy margin */
   strcat(tmp5, tmp3);	/* copy 2nd line */
   
   strcat(tmp5, tmp1);	/* copy margin */
   strcat(tmp5, tmp4);	/* copy 3rd line */
   
   Text_AddString(&buf, tmp5, 0);

   
   
   return(buf);
}
//---------------------------------------------------------




