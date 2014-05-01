/*
	File:		stalist_show.c
	Date:		4/18/1997
	Author:		Paul Taylor

	Purpose:	Provides support for the Station List Report.
*/


#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/FileSB.h>
#include <Xm/ToggleB.h>
#include <X11/cursorfont.h>
#include <datetime.h>
#include <time.h>
#include <unistd.h>

#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"

#include "GeneralUtil.h"
#include "StationList.h"

#include "textrept.h"
#include "textrept_show.h"
#include "stalist_show.h"


#define	DEBUG_MODE	0	/* 0=False, 1=True */


/*
	Globals.
*/
static char		stalist_hdr_format[] =
"     %-10s    %-30s    %-70s   Page %2i";

static char  stalist_hdr_title_lid[]      = "LIST OF LOCATIONS SORTED BY LID";
static char  stalist_hdr_title_name[]     = "LIST OF LOCATIONS SORTED BY NAME";
static char  stalist_hdr_title_county[]   = "LIST OF LOCATIONS SORTED BY COUNTY";
static char  stalist_hdr_title_basin[]    = "LIST OF LOCATIONS SORTED BY BASIN";
static char  stalist_hdr_title_observer[] = "LIST OF LOCATIONS SORTED BY OBSERVER";
   
static int		stalist_first_page = STALIST_SORTBY_LID;
static int		stalist_last_page = STALIST_SORTBY_OBSERVER;



/*
	Functions.
*/
void	StaList_AddCallbacks(void)
{
   /*
   	Support for Print/Email DS.
   */
   XtAddCallback(stalistlidTB, XmNarmCallback,    StaList_PE_ClearAll,   NULL);
   XtAddCallback(stalistlidTB, XmNdisarmCallback, StaList_PE_CheckItems, NULL);
   
   XtAddCallback(stalistnameTB,   XmNarmCallback,    StaList_PE_ClearAll,   NULL);
   XtAddCallback(stalistnameTB,   XmNdisarmCallback, StaList_PE_CheckItems, NULL);
   
   XtAddCallback(stalistcountyTB, XmNarmCallback,    StaList_PE_ClearAll,   NULL);
   XtAddCallback(stalistcountyTB, XmNdisarmCallback, StaList_PE_CheckItems, NULL);
   
   XtAddCallback(stalistbasinTB, XmNarmCallback,    StaList_PE_ClearAll,   NULL);
   XtAddCallback(stalistbasinTB, XmNdisarmCallback, StaList_PE_CheckItems, NULL);
   
   XtAddCallback(stalistobserverTB,  XmNarmCallback,    StaList_PE_ClearAll,   NULL);
   XtAddCallback(stalistobserverTB,  XmNdisarmCallback, StaList_PE_CheckItems, NULL);

   XtAddCallback(stalistallTB,   XmNarmCallback,    StaList_PE_ClearItems, NULL);
   XtAddCallback(stalistallTB,   XmNdisarmCallback, StaList_PE_SetLid, NULL);
   
   return;
}

//---------------------------------------------------------

void	StaList_RemoveCallbacks(void)
{
   /*
   	Support for Print/Email DS.
   */
   XtRemoveCallback(stalistlidTB, XmNarmCallback,    StaList_PE_ClearAll,   NULL);
   XtRemoveCallback(stalistlidTB, XmNdisarmCallback, StaList_PE_CheckItems, NULL);
   
   XtRemoveCallback(stalistnameTB,   XmNarmCallback,    StaList_PE_ClearAll,   NULL);
   XtRemoveCallback(stalistnameTB,   XmNdisarmCallback, StaList_PE_CheckItems, NULL);
   
   XtRemoveCallback(stalistcountyTB, XmNarmCallback,    StaList_PE_ClearAll,   NULL);
   XtRemoveCallback(stalistcountyTB, XmNdisarmCallback, StaList_PE_CheckItems, NULL);
   
   XtRemoveCallback(stalistbasinTB, XmNarmCallback,    StaList_PE_ClearAll,   NULL);
   XtRemoveCallback(stalistbasinTB, XmNdisarmCallback, StaList_PE_CheckItems, NULL);
   
   XtRemoveCallback(stalistobserverTB,  XmNarmCallback,    StaList_PE_ClearAll,   NULL);
   XtRemoveCallback(stalistobserverTB,  XmNdisarmCallback, StaList_PE_CheckItems, NULL);

   XtRemoveCallback(stalistallTB,   XmNarmCallback,    StaList_PE_ClearItems, NULL);
   XtRemoveCallback(stalistallTB,   XmNdisarmCallback, StaList_PE_SetLid, NULL);
      
   return;
}

//---------------------------------------------------------

void	StaList_checkPB(Widget w, XtPointer ptr, XtPointer cbs)
{
   int	pageType = (int) ptr;

   
   SetCursor(textreptFO, XC_watch);
   
   TextReports_MiscSetupOperations(GetMenuHistory(textreptOM));
   StaList_LoadTextWidget(pageType);
   
   UnsetCursor(textreptFO);
   
   return;
}
//---------------------------------------------------------


void	StaList_PE_ClearAll(Widget w, XtPointer ptr, XtPointer cbs)
{
	XmToggleButtonSetState(stalistallTB, False, False);
	
	return;
}
//---------------------------------------------------------


void	StaList_PE_SetLid(Widget w, XtPointer ptr, XtPointer cbs)
{
	if(!XmToggleButtonGetState(stalistallTB))
	   XmToggleButtonSetState(stalistlidTB, True, False);
	
	return;
}
//---------------------------------------------------------


void	StaList_PE_ClearItems(Widget w, XtPointer ptr, XtPointer cbs)
{
	XmToggleButtonSetState(stalistlidTB,      False, False);
	XmToggleButtonSetState(stalistnameTB,     False, False);
	XmToggleButtonSetState(stalistcountyTB,   False, False);
	XmToggleButtonSetState(stalistbasinTB,    False, False);
	XmToggleButtonSetState(stalistobserverTB, False, False);
	
	return;
}
//---------------------------------------------------------


void	StaList_PE_CheckItems(Widget w, XtPointer ptr, XtPointer cbs)
{
   int	setall = True;

   if(XmToggleButtonGetState(stalistlidTB))		setall = False;
   if(XmToggleButtonGetState(stalistnameTB))		setall = False;
   if(XmToggleButtonGetState(stalistcountyTB))		setall = False;
   if(XmToggleButtonGetState(stalistbasinTB))		setall = False;
   if(XmToggleButtonGetState(stalistobserverTB))	setall = False;
   
   if(setall)
      XmToggleButtonSetState(stalistallTB, True, False);
   
   return;
}
//---------------------------------------------------------

char * StaList_GenerateReport(int pageType, int reason, 
                              char tmp_dir [ ], 
                              long *fileLength)
{
   static const char * pFileTemplate = "XXXXXX\0";
   int          file_descriptor;
   int		gad_token_len=0, gad_value_len=0;
   
   char*	buf = NULL;
   int		pageNum = 0;
   long		bufLength = 0;
   long		total_chars_written = 0;
   char		orderby[BUFSIZ];

   
   StationList	*info = NULL,
      		*infoPtr = NULL;

   
   /*
   	Set up orderby.
   */
   memset ( ( void * ) orderby, '\0', sizeof(orderby));
   if (pageType == STALIST_SORTBY_LID)
      strcpy(orderby, " ORDER BY lid, name, lastname, rb, county, wfo ");

   if (pageType == STALIST_SORTBY_NAME)
      strcpy(orderby, " ORDER BY name, lid, lastname, rb, county, wfo ");
   
   if (pageType == STALIST_SORTBY_COUNTY)
      strcpy(orderby, " ORDER BY county, lid, name, lastname, rb, wfo ");
   
   if (pageType == STALIST_SORTBY_BASIN)
      strcpy(orderby, " ORDER BY rb, lid, name, lastname, county, wfo ");
   
   if (pageType == STALIST_SORTBY_OBSERVER)
      strcpy(orderby, " ORDER BY lastname, lid, name, rb, county, wfo ");

   
   /*
	Get temporary filename,
	and write the buffer to it.
   */
   gad_token_len = strlen("whfs_report_dir");
   get_apps_defaults("whfs_report_dir", &gad_token_len, tmp_dir, 
		     &gad_value_len);

   /* Jan 24, 2006, Bryon L., Modified to use mkstemp instead of tempnam.
      Tempnam is deprecated. */
   strcat ( tmp_dir, "/" );
   strcat ( tmp_dir, pFileTemplate );
   file_descriptor = mkstemp ( tmp_dir );

   if ( file_descriptor >= 0 )
   {
      info = (StationList *) GetStationList(orderby);
      if (info != (StationList *) NULL)
      {
	 infoPtr = (StationList *) ListFirst(&info->list);
	 pageNum = 1;
	 
	 while (infoPtr != (StationList *) NULL)
	 {
	    /* NOTE: infoPtr is set to next infoPtr in ListOfLocations() */
	    buf = StaList_ListOfLocations(reason, &infoPtr,
					  (char *) TextReports_GetDate(),
					  pageNum, pageType, NULL, &bufLength);
            write ( file_descriptor, buf, bufLength );
            fsync ( file_descriptor );
	    total_chars_written += bufLength;
	    pageNum++;
	 }

	 close(file_descriptor);
	 
	 if (info)
	    FreeStationList(info);
      }
   }
   else
   {
      fprintf(stderr, "ERROR: Could not create temporary file...\n");
      return(NULL);
   }


   *fileLength = (long) total_chars_written;
   return(tmp_dir);
}
//---------------------------------------------------------


void	StaList_PE_Manager(Widget w, XtPointer ptr, XtPointer cbs)
{
   int	  option = (int) ptr;

   char*  text = NULL;
   int	  all_flag = False;
   int	  i, j;
   
   
   SetCursor(textreptFO, XC_watch);
   

   /*
   	Either Print or Email, depending on the given option.
   */
   if(XmToggleButtonGetState(stalistallTB))
      all_flag = True;
   
   for(i=stalist_first_page; i<=stalist_last_page; i++)
   {
      j = False;
      
      switch(i)
      {
	 case STALIST_SORTBY_LID:	j = XmToggleButtonGetState(stalistlidTB);
	    TextReports_setEmailSubject("station_list.lid");
	    break;
	    
	 case STALIST_SORTBY_NAME:	j = XmToggleButtonGetState(stalistnameTB);
	    TextReports_setEmailSubject("station_list.name");
	    break;
	    
	 case STALIST_SORTBY_COUNTY:	j = XmToggleButtonGetState(stalistcountyTB);
	    TextReports_setEmailSubject("station_list.county");
	    break;
	    
	 case STALIST_SORTBY_BASIN:	j = XmToggleButtonGetState(stalistbasinTB);
	    TextReports_setEmailSubject("station_list.basin");
	    break;
	    
	 case STALIST_SORTBY_OBSERVER:	j = XmToggleButtonGetState(stalistobserverTB);
	    TextReports_setEmailSubject("station_list.observer");
	    break;
      }
      
      if(all_flag || j)
      {
	 text = StaList_GetText(i, TEXTREPORTS_REASON_PRINT_EMAIL_OR_SAVE);
	 if (option == STALIST_OPTION_PRINT)
	 {
	    TextReports_P_OutputBuffer(text);
	 }
	 else if (option == STALIST_OPTION_EMAIL)
	 {
	    TextReports_E_OutputBuffer(text);
	 }
	 Text_FreeString(&text);
      }
   }
   
   
   UnsetCursor(textreptFO);
   
   return;
}
//---------------------------------------------------------

	      
void	StaList_LoadTextWidget(int pageType)
{
   char*  text = NULL;

   
   text = StaList_GetText(pageType, TEXTREPORTS_REASON_NORMAL);
   
#if DEBUG_MODE
printf("strlen of text <%i>\n", strlen(text));
#endif

   if (text != NULL)
   {
      XtVaSetValues(textreptTE, XmNvalue, text, NULL);
   }
   else
      fprintf(stderr,"ERROR: Unable to load text properly...\n");

   
   return;
}
//---------------------------------------------------------


char*	StaList_GetText(int pageType, int reason)
{
   textrept_bufinfo*	bufInfoPtr = TextReports_GetBufInfo();

   char		buf[BUFSIZ];
   char		file_path [ 128 ];
   char		*text = NULL;
   long		fileLength, len;
   FILE		*fp = NULL;
   
   char		rm_cmd[BUFSIZ];


   /*
   	Get the temporary filename where the report is being stored.
	Read this file, and store the result into the stalist_buf.
   */
   StaList_GenerateReport(pageType, reason, file_path, &fileLength);

   
   if (! (fp = fopen(file_path, "r")))
   {
      fprintf(stderr, "ERROR reading file %-s\n", file_path);
      return(NULL);
   }

   
   len = fileLength;
   if (! (text = XtMalloc((unsigned) (len+1))))
      sprintf(buf, "%s: XtMalloc(%ld) failed", file_path, len);
   else
   {
      if (fread(text, sizeof(char), len, fp) != len)
	 sprintf(buf, "WARNING: did not read entire file!");
      else
	 sprintf(buf, "Loaded %ld bytes from %s.", len, file_path);
      text[len] = 0;  /* NULL-terminate */
   }

#if DEBUG_MODE
printf("reading file...  %-s\n", buf);
#endif

   memset ( ( void * ) rm_cmd, '\0', sizeof(rm_cmd));
   sprintf(rm_cmd, "rm -f %-s", file_path);
   system(rm_cmd);
   
   bufInfoPtr->stalist_buf = text; 
   return(bufInfoPtr->stalist_buf);
}
//---------------------------------------------------------


/****************************************************************************/


char*	StaList_ListOfLocations(int reason, StationList **infoPtr, char *date,
				int pageNum, int pageType, char *optHeader,
				long *bufLength)
{
   static char	buf[STALIST_PAGE_BUFSIZ];

   static int	record_count;  /* total num of records to process for pageType */
   static int	record_limit;  /* max num records per "8 1/2 x 11" page */

   static int	count;  /* for counting the num of records processed so far */
   static int	i;	/* (loop control variable) */

   char	tmp1[STALIST_LINE_BUFSIZ];
   char	tmp2[STALIST_LINE_BUFSIZ];
   char	tmp3[STALIST_LINE_BUFSIZ];
   char	tmp4[STALIST_LINE_BUFSIZ];
   char	tmp5[STALIST_LINE_BUFSIZ];
   char	tmp6[STALIST_LINE_BUFSIZ];

   char	tmpstr[STALIST_LINE_BUFSIZ];
   
   char	hdr_title[STALIST_LINE_BUFSIZ];

   int	*order = NULL;
   int	*orderHead = NULL;
   int	sortedby_lid[]		= {
      				STALIST_LID, STALIST_NAM, STALIST_COU,
      				STALIST_BAS, STALIST_WFO, STALIST_OBS, 0};
   
   int	sortedby_name[]		= {
      				STALIST_NAM, STALIST_LID, STALIST_COU,
				STALIST_BAS, STALIST_WFO, STALIST_OBS, 0};
   
   int	sortedby_county[]	= {
				STALIST_COU, STALIST_LID, STALIST_NAM,
				STALIST_BAS, STALIST_WFO, STALIST_OBS, 0};
   
   int	sortedby_basin[]	= {
				STALIST_BAS, STALIST_LID, STALIST_NAM,
				STALIST_COU, STALIST_WFO, STALIST_OBS, 0};
   
   int	sortedby_observer[]	= {
				STALIST_OBS, STALIST_LID, STALIST_NAM,
				STALIST_COU, STALIST_BAS, STALIST_WFO, 0};
   
#if DEBUG_MODE
printf("ListOfLocations(*infoPtr==<%p>,date==<%-s>,pageNum==<%i>,"
       "pageType==<%i>,optHeader==<%-s>,record_count==<%i>,record_limit==<%i>,"
			"count==<%i>,i==<%i>\n",
       *infoPtr, date, pageNum, pageType, optHeader, record_count,
       record_limit, count, i);
#endif
   
   /*
   	Init buffer & other variables.
   	Add (optional) header.
   */
   memset( ( void * ) buf, '\0', sizeof(buf));
   if (pageNum == 1)
   {
      record_limit = STALIST_RECORD_LIMIT_PER_PAGE;
      count = 0;  /* num records processed so far */
      
   }
   if (optHeader != NULL)
   {
      strcat(buf, optHeader);
   }


   /*
   	Check page type.
   */
   memset ( ( void * ) hdr_title, '\0', sizeof(hdr_title));
   strcpy(hdr_title, "LIST OF LOCATIONS");
   switch (pageType)
   {
      case STALIST_SORTBY_LID:      strcpy(hdr_title, stalist_hdr_title_lid);
	 			    orderHead = sortedby_lid;
	 			    break;

      case STALIST_SORTBY_NAME:     strcpy(hdr_title, stalist_hdr_title_name);
	 			    orderHead = sortedby_name;
	 			    break;
				    
      case STALIST_SORTBY_COUNTY:   strcpy(hdr_title, stalist_hdr_title_county);
	 			    orderHead = sortedby_county;
	 			    break;

      case STALIST_SORTBY_BASIN:    strcpy(hdr_title, stalist_hdr_title_basin);
	 			    orderHead = sortedby_basin;
	 			    break;
				    
      case STALIST_SORTBY_OBSERVER: strcpy(hdr_title, stalist_hdr_title_observer);
	 			    orderHead = sortedby_observer;
	 			    break;
	 
      default:	fprintf(stderr, "ERROR: Unknown page type...\n");
	 	/* generic title was used */
   }
   
   
   /*
   	Build header line.
   */
   memset ( ( void * ) tmp1, '\0', sizeof(tmp1));
   sprintf(tmp1, stalist_hdr_format,
	   date, " ", hdr_title, pageNum);
   strcat(buf, tmp1);
   strcat(buf, "\n\n");

   
   memset ( ( void * ) tmp2, '\0', sizeof(tmp2));
   sprintf(tmp2, "%-105s %-s\n",
	   " ", "-------PHONE NUMBERS-------\n");
   strcat(buf, tmp2);
   
   
   memset ( ( void * ) tmp3, '\0', sizeof(tmp3));
   strcat(tmp3, "     ");
   order = orderHead;
   while (*order != (int) NULL)
   {
      switch(*order)	/* default is sortedby_lid */
      {
	 case STALIST_LID:  strcat(tmp3, "LID     ");			break;
	 case STALIST_NAM:  strcat(tmp3, "LOCATION            ");	break;
	 case STALIST_COU:  strcat(tmp3, "COUNTY              ");	break;
	 case STALIST_BAS:  strcat(tmp3, "BASIN               ");	break;
	 case STALIST_WFO:  strcat(tmp3, "WFO");			break;
	 case STALIST_OBS:  strcat(tmp3, "OBSERVER                ");	break;
      }
      strcat(tmp3, " ");
      
      order++;
   }
   strcat(buf, tmp3);
   strcat(buf, "HOME         WORK          \n");

   
   memset ( ( void * ) tmp4, '\0', sizeof(tmp4));
   strcat(tmp4, "     ");
   order = orderHead;
   while (*order != (int) NULL)
   {
      switch(*order)
      {
	 case STALIST_LID:  strcat(tmp4, "--------");			break;
	 case STALIST_NAM:  strcat(tmp4, "--------------------");	break;
	 case STALIST_COU:  strcat(tmp4, "--------------------");	break;
	 case STALIST_BAS:  strcat(tmp4, "--------------------");	break;
	 case STALIST_WFO:  strcat(tmp4, "---");			break;
	 case STALIST_OBS:  strcat(tmp4, "------------------------");	break;
      }
      strcat(tmp4, " ");
      
      order++;
   }
   strcat(buf, tmp4);
   strcat(buf, "------------ --------------\n");
   

   
   /*
   	Keep building the list of locations (& headers)
	for the given pageType until there are no more
	records for the database pointer (*infoPtr),
	or until the record_limit is reached.
   */
   for(i=0;  i<record_limit;  i++)
   {
      /*
          Write out a single output line as specified by the order.
      */
      strcat(buf, "     ");
      order = orderHead;
      while (*order != (int) NULL)
      {
         memset ( ( void * ) tmpstr, '\0', sizeof(tmpstr));
      	 memset ( ( void * ) tmp5, '\0', sizeof(tmp5));
	 switch(*order)
	 {
	    case STALIST_LID:  sprintf(tmp5, "%-8s",  (*infoPtr)->lid);	   break;
	    case STALIST_NAM:  strncpy(tmpstr, (*infoPtr)->name, 20);
	                       sprintf(tmp5, "%-20s", tmpstr);             break;
	    case STALIST_COU:  sprintf(tmp5, "%-20s", (*infoPtr)->county); break;
	    case STALIST_BAS:  strncpy(tmpstr, (*infoPtr)->rb, 20);
	                       sprintf(tmp5, "%-20s", tmpstr);             break;
	    case STALIST_WFO:  sprintf(tmp5, "%-3s",  (*infoPtr)->wfo);	   break;
	    case STALIST_OBS:  strncpy(tmpstr, (*infoPtr)->lastname, 24);
	                       sprintf(tmp5, "%-24s", tmpstr);             break;
	 }
	 strcat(tmp5, " ");
	 strcat(buf, tmp5);
      
	 order++;
      }

      memset ( ( void * ) tmp6, '\0', sizeof(tmp6));
      memset ( ( void * ) tmpstr, '\0', sizeof(tmpstr));
      strncpy(tmpstr, (*infoPtr)->hphone, 12);
      sprintf(tmp6, "%-12s", tmpstr);
      strcat(buf, tmp6);
      memset ( ( void * ) tmp6, '\0', sizeof(tmp6));
      strncpy(tmpstr, (*infoPtr)->ophone, 14);
      sprintf(tmp6, " %-14s\n", tmpstr);
      strcat(buf, tmp6);

      
      count += 1;  /* add one to the number of processed records */
      *infoPtr = (StationList *) ListNext(&(*infoPtr)->node);
      if (((*infoPtr) == (StationList *) NULL) || (count == record_count))
      {
	 break;
      }
   }
   

   /*
   	Paginate.
   */
   for(i=Text_CountNewLines(buf);  i<60;  i++)
      strcat(buf, "\n");


   *bufLength = (long) strlen(buf);
   return(buf);
}
//---------------------------------------------------------


