/*
	File:		servbkup_show.c
	Date:		10/30/2000
	Author:		Julie Daniel

	Purpose:	Provides support for the Service Backup Report.

	Modification History:

	Bryon Lawrence     Feburary 9, 2004    Removed the Primary and
	                                       Secondary Backup columns.
					       in the report.  Changed the  
                                               WFO column
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
#include "LocView.h"

#include "textrept.h"
#include "textrept_show.h"
#include "servbkup_show.h"

/*
	Globals.
*/
static char		servbkup_hdr_format[] =
"     %-10s     %-50s   Page %2i";

static char  servbkup_hdr_title_lid[] = "LIST OF LOCATIONS SORTED BY "
                                        "STATION ID";
static char  servbkup_hdr_title_wfo[] = "LIST OF LOCATIONS SORTED BY WFO";
static char  servbkup_hdr_title_hsa[] = "LIST OF LOCATIONS SORTED BY HSA";
   


/*
	Functions.
*/


void	ServBkup_checkPB(Widget w, XtPointer ptr, XtPointer cbs)
{
   int	pageType = (int) ptr;

   
   SetCursor(textreptFO, XC_watch);
   
   TextReports_MiscSetupOperations(GetMenuHistory(textreptOM));
   ServBkup_LoadTextWidget(pageType);
   
   UnsetCursor(textreptFO);
   
   return;
}


//---------------------------------------------------------
void	ServBkup_PE_Print(Widget w, XtPointer ptr, XtPointer cbs)
{
	char*	buf = NULL;
	
	SetCursor(textreptFO, XC_watch);

	buf = ServBkup_GetText(GetMenuPos(servbkupOM), TEXTREPORTS_REASON_PRINT_EMAIL_OR_SAVE);
	TextReports_P_OutputBuffer(buf);  /* output buffer to printer */
	
	UnsetCursor(textreptFO);
	
	return;
}

//---------------------------------------------------------
	      
void	ServBkup_LoadTextWidget(int pageType)
{
   char*  text = NULL;

   
   text = ServBkup_GetText(pageType, TEXTREPORTS_REASON_NORMAL);
   
   if (text != NULL)
   {
      XtVaSetValues(textreptTE, XmNvalue, text, NULL);
   }
   else
      fprintf(stderr,"ERROR: Unable to load text properly...\n");

   
   return;
}
//---------------------------------------------------------

char * ServBkup_GetText(int pageType, int reason)
{
   textrept_bufinfo*	bufInfoPtr = TextReports_GetBufInfo();

   char		buf [ BUFSIZ ] ;
   char		file_path [ 128 ];
   char		* text = NULL ;
   long		fileLength, len;
   FILE		* fp = NULL ;
   
   char		rm_cmd [ BUFSIZ ] ;


   /*
   	Get the temporary filename where the report is being stored.
	Read this file, and store the result into the servbkup_buf.
   */
   ServBkup_GenerateReport(pageType, reason, file_path, &fileLength);

   
   if (! (fp = fopen(file_path, "r")))
   {
      fprintf(stderr, "ERROR reading file %-s\n", file_path);
      return(NULL);
   }

   
   len = fileLength;
   if (! ( text = ( char * ) malloc ( ( len+1 ) * sizeof ( char ) ) ) )
      sprintf(buf, "%s: malloc(%ld) failed", file_path, len);
   else
   {
      if (fread(text, sizeof(char), len, fp) != len)
	 sprintf(buf, "WARNING: did not read entire file!");
      else
	 sprintf(buf, "Loaded %ld bytes from %s.", len, file_path);
      text[len] = 0;  /* NULL-terminate */
   }



   memset( ( void * ) rm_cmd, '\0', sizeof(rm_cmd));
   sprintf(rm_cmd, "rm -f %-s", file_path);
   system(rm_cmd);
   
   bufInfoPtr->servbkup_buf = text;
   return(bufInfoPtr->servbkup_buf);
}
//---------------------------------------------------------


char*	ServBkup_GenerateReport(int pageType, int reason, 
                                char tmp_dir[], long *fileLength)
{
   static const char * pFileTemplate = "XXXXXX\0";
   int		gad_token_len=0, gad_value_len=0;
   
   char*	buf = NULL;
   int          file_descriptor;
   int		pageNum = 0;
   long		bufLength = 0;
   long		total_chars_written = 0;
   char		orderby[BUFSIZ];

   
   LocView * info = NULL ;
   LocView * infoPtr = NULL ;
   
   /*
   	Set up orderby.
   */
   memset( ( void * ) orderby, '\0', sizeof(orderby));
   if (pageType == SERVBKUP_SORTBY_LID)
      strcpy(orderby, " ORDER BY lid ");

   if (pageType == SERVBKUP_SORTBY_WFO)
      strcpy(orderby, " ORDER BY wfo, lid ");

   if (pageType == SERVBKUP_SORTBY_HSA)
      strcpy(orderby, "ORDER BY hsa, lid ");
      
   /*
	Get temporary filename,
	and write the buffer to it.
   */
   gad_token_len = strlen("whfs_report_dir");

   get_apps_defaults("whfs_report_dir", &gad_token_len, tmp_dir, 
		     &gad_value_len);

   /* Jan 24, 2006, Bryon L., Modified to use mkstemp instead of 
      tempnam.  Tempnam is deprecated. */
   strcat ( tmp_dir, "/" );
   strcat ( tmp_dir, pFileTemplate );
   file_descriptor = mkstemp ( tmp_dir ); 

   if ( file_descriptor >= 0 )
   {
      info = (LocView *) GetLocView (orderby);
      
      if (info != (LocView *) NULL)
      {
	 infoPtr = (LocView *) ListFirst(&info->list);
	 pageNum = 1;
	 
	 while (infoPtr != (LocView *) NULL)
	 {
	    /* NOTE: infoPtr is set to next infoPtr in ListOfLocations() */

	    buf = ServBkup_ListOfLocations(reason, &infoPtr,
					  (char *) TextReports_GetDate(),
					  pageNum, pageType, NULL, &bufLength);
            write ( file_descriptor, buf, bufLength );  
            fsync ( file_descriptor );
	    total_chars_written += bufLength;
	    pageNum++;
	 }
	 
	 close ( file_descriptor );
	 
	 if (info)
	    FreeLocView(info);
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

/****************************************************************************/


char*	ServBkup_ListOfLocations(int reason, LocView **infoPtr, char *date,
				 int pageNum, int pageType, char *optHeader,
				 long *bufLength)
{
   static char	buf[SERVBKUP_PAGE_BUFSIZ];

   static int	record_count;  /* total num of records to process for pageType */
   static int	record_limit;  /* max num records per "8 1/2 x 11" page */

   static int	count;  /* for counting the num of records processed so far */
   static int	i;	/* (loop control variable) */

   char	tmp1[SERVBKUP_LINE_BUFSIZ];
   char	tmp3[SERVBKUP_LINE_BUFSIZ];
   char	tmp4[SERVBKUP_LINE_BUFSIZ];
   char	tmp5[SERVBKUP_LINE_BUFSIZ];

   char	hdr_title[SERVBKUP_LINE_BUFSIZ];

  
   /*
   	Init buffer & other variables.
   	Add (optional) header.
   */
   memset( ( void * ) buf, '\0', sizeof(buf));
   if (pageNum == 1)
   {
      record_limit = SERVBKUP_RECORD_LIMIT_PER_PAGE;
      count = 0;  /* num records processed so far */
      
   }
   if (optHeader != NULL)
   {
      strcat(buf, optHeader);
   }

   /*
   	Check page type.
   */
   memset( ( void * ) hdr_title, '\0', sizeof(hdr_title));
   strcpy(hdr_title, "LIST OF LOCATIONS");

   switch ( pageType )
   {
      case SERVBKUP_SORTBY_LID:       strcpy(hdr_title,
				             servbkup_hdr_title_lid);
	 			      break;

      case SERVBKUP_SORTBY_WFO:       strcpy(hdr_title,
					     servbkup_hdr_title_wfo);
	 			      break;

      case SERVBKUP_SORTBY_HSA:       strcpy(hdr_title,
				             servbkup_hdr_title_hsa);
	 			      break;

      default:	fprintf(stderr, "ERROR: Unknown page type...\n");
	 	/* generic title was used */
   }
   
   
   /*
   	Build header line.
   */
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   sprintf(tmp1, servbkup_hdr_format,
	   date, hdr_title, pageNum);
   strcat(buf, tmp1);
   strcat(buf, "\n\n");

   
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   strcat(tmp3, "     ");
   strcat(tmp3, "STATION ID    ST,COUNTY                  WFO    HSA\n");
   strcat(buf, tmp3);
      

   
   memset( ( void * ) tmp4, '\0', sizeof(tmp4));
   strcat(tmp4, "     ");
   strcat(tmp4, "----------    -----------------------    ---    ---\n");
   strcat(buf, tmp4);
      
   
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
      
      sprintf(tmp5,"%-10s    %-2s,%-20s    %-3s    %-7s", (*infoPtr)->lid,
	 (*infoPtr)->state, (*infoPtr)->county, (*infoPtr)->wfo, 
	 (*infoPtr)->hsa);
       
      strcat(buf, tmp5);
      strcat(buf, "\n");
      
         
      count += 1;  /* add one to the number of processed records */
      *infoPtr = ( LocView * ) ListNext(&(*infoPtr)->node);
      if (((*infoPtr) == ( LocView * ) NULL) || (count == record_count))
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

