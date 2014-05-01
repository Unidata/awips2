/*
	File:		geolog_show.c
	Date:		May 1997
	Author:		Mark Glaudemans
	
	Purpose:	Provide support for GeoLog DS.
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>

#include "DbmsDefs.h"
#include "Xtools.h"

#include "geomanage.h"
#include "geolog_show.h"


void geolog_show(Widget w,
		 char *title,
		 char *labelstr,
		 char *text)
{
   static int	geo_logview_created;
   Atom		wmAtom;
   
   
   if (! geo_logview_created)
   {
      create_geo_logviewDS(GetTopShell(w));
      
      /* add callbacks */
      
      wmAtom = XmInternAtom(XtDisplay(geo_logviewDS), "WM_DELETE_WINDOW", False);
      XmAddWMProtocolCallback(geo_logviewDS, wmAtom, (XtCallbackProc)geolog_close, (XtPointer)NULL);
      
      XtAddCallback(geo_logview_okPB, XmNactivateCallback, (XtCallbackProc)geolog_close, (XtPointer)NULL);
      
      
      geo_logview_created = True;
   }
   
   
   if (! XtIsManaged(geo_logviewDS))
   {
      SetTitle(geo_logviewDS, title);
      SetLabel(geo_logviewLB, labelstr);
      
      XmTextSetString(geo_logviewTX, (char *)text);
      
      XtManageChild(geo_logviewFO);
      XtManageChild(geo_logviewDS);
   }
   
   return;
}


/************************************************************************
   
   
   **********************************************************************/

void geolog_close(void)
{
   XtUnmanageChild(geo_logviewFO);
   XtUnmanageChild(geo_logviewDS);
   
   return;
}


/************************************************************************
   
   loads text from a file into one long string.
   
   this function allocates the memory. the calling function
   MUST be responsible and free it!!!
   
   **********************************************************************/
int load_log_text(char *logfilename,
		  char **textstr)
{
#define textsize 120000
   FILE *file_ptr;
   char *fgets_ptr;
   char fileline[200];
      
   
   /* open the file */
   
   file_ptr = fopen(logfilename, "r");
   if(file_ptr == NULL)
   {
      fprintf(stderr, "Error opening log file: %s\n", logfilename);
      return(-1);
   }
   
   
   /* allocate space for the string  */
   
   *textstr = (char *) malloc(textsize);
   memset(*textstr, 0, textsize); 
   
   
   /* loop on the lines in the file */
   
   while ((fgets_ptr = fgets(fileline, 200, file_ptr)) != NULL &&
	  strlen(*textstr) < (textsize - 200))
   {
      strcat(*textstr, fileline);
   }
   
   if (strlen(*textstr) >= (textsize - 200))
   {
      strcat(*textstr, "\n...file display truncated...file too long...\n");
   }
   
   
   /* close the file */
   
   fclose(file_ptr);
   
   
   return(0);
}

