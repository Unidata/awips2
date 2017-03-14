/*******************************************************************************
* FILENAME:            mpe_bad_gage_list_show.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*            MODULE 1: 
*         DESCRIPTION:
*
* ORIGINAL AUTHOR:     Ram Varma
* CREATION DATE:       May 07, 2007
* ORGANIZATION:        OHD HSEB
* MACHINE:             HP-UX / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE           PROGRAMMER        DESCRIPTION/REASON
*          1        May 07, 2007 Ram Varma    Original Coding
********************************************************************************
*/
#include <stdlib.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <Xm/Scale.h>
#include <Xm/Xm.h>

#include "display7x7.h"
#include "display7x7_show.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "stage3.h"
#include "Xtools.h"
#include "mpe_bad_gage_list.h"
#include "mpe_bad_gage_list_show.h"


#define MAX_NUM_BAD_GAGES 50

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   show_mpe_bad_gage_list
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************/

void show_mpe_bad_gage_list(Widget w, XtPointer calldata , XtPointer clientdata)
{
   if ( ( badGageSH == NULL ) ||
        ! XtIsManaged ( badGageSH  ) )
   {
      create_badGageSH ( GetTopShell ( w ) ) ;
      mpe_bad_gage_list_callbacks ( ) ;
   }

   XtManageChild ( badGageFM ) ;
   XtManageChild ( badGageSH ) ;

   init_mpe_bad_gage_list();
  logMessage("method was called\n");
   return ;
}

void init_mpe_bad_gage_list()
{
      FILE *fd = NULL, *fd1 = NULL;
      char buf[MAX_NUM_BAD_GAGES][9];
      char dirname[120];
      char bad_gages_file_name[120];
      XmStringTable xmStr;
      int ac = 0, cnt = 0, i = 0, len = 0;
      Arg arg[10];

      memset(buf, '\0', 9);
      memset(dirname, '\0', 120);
      memset(bad_gages_file_name, '\0', 120);

      len = strlen("mpe_bad_gages_dir");
      get_apps_defaults("mpe_bad_gages_dir",&len,dirname,&len);
      strcpy(bad_gages_file_name, dirname);
      strcat(bad_gages_file_name, "/mpe_bad_gage_list");

      fd = (FILE*) fopen(bad_gages_file_name, "r");   
      if(fd == NULL)
      {
        logMessage("Error opening mpe bad gage list file: %s...\n", bad_gages_file_name);
         return;
      }
      for(i=0;i<MAX_NUM_BAD_GAGES;i++)
      {
         memset(buf[i], '\0', 9);
      }
      fscanf(fd, "%s", buf[cnt]);
      while(!feof(fd))
      {  
         cnt++;

         fscanf(fd, "%s", buf[cnt]);
      }
      xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
      for(i=0;i<cnt;i++)
      {
         xmStr[i] = XmStringCreateSimple(buf[i]);
      }
      ac = 0;
      XtSetArg(arg[ac], XmNitemCount,cnt); ac++;
      XtSetArg(arg[ac], XmNitems, xmStr); ac++;
      XtSetValues(badGageLS, arg, ac);

      for (i=0; i < cnt; i++)
      {
         XmStringFree(xmStr[i]);
      }
      XtFree((char *)xmStr);
      fclose(fd);
}

void mpe_bad_gage_list_callbacks ( )
{
   Atom wmAtom ;

   XtAddCallback ( okPB , XmNactivateCallback , okCallback , 
                   NULL ) ;
   XtAddCallback ( delPB , XmNactivateCallback , deleteSelectedStationCallback , 
                   NULL ) ;
   wmAtom = XmInternAtom ( XtDisplay ( badGageSH  ) , "WM_DELETE_WINDOW" , 
                           FALSE ) ;
   XmAddWMProtocolCallback ( badGageSH , wmAtom , closeCallback ,
                             NULL ) ;
}


void deleteSelectedStationCallback(Widget w , XtPointer calldata , XtPointer clientdata )
{   
      XmString *xm_str ;
      FILE *fd = NULL, *fd1 = NULL;
      char buf[9];
      char dirname[120];
      char temp[120];
      char bad_gages_file_name[120];
      int len = 0;
      Arg arg[1];
      char *text;
      int error = 0;

      memset(buf, '\0', 9);
      memset(dirname, '\0', 120);
      memset(bad_gages_file_name, '\0', 120);
      memset(temp, '\0', 120);

      len = strlen("mpe_bad_gages_dir");
      get_apps_defaults("mpe_bad_gages_dir",&len,dirname,&len);
      strcpy(bad_gages_file_name, dirname);
      strcat(bad_gages_file_name, "/mpe_bad_gage_list");
      strcat(temp, dirname);
      strcat(temp, "/temp");

      XtSetArg(arg[0], XmNselectedItems, &xm_str);
      XtGetValues(badGageLS, arg, 1);
      if(xm_str != NULL && text != NULL)
      {
         XmStringGetLtoR(xm_str[0], XmFONTLIST_DEFAULT_TAG, &text);
      }
      else
      {
        logMessage("Error retrieving selected item...\n");
         return;
      }

      if(text == NULL)
      {
        logMessage("No selected item to delete\n");
         return;
      }

      fd = (FILE*) fopen(bad_gages_file_name, "r");   
      if(fd == NULL)
      {
        logMessage("Error opening mpe bad gage list file: %s...\n", bad_gages_file_name);
         return;
      }
      fd1 = (FILE*) fopen(temp, "a");
      if(fd1 == NULL)
      {
        logMessage("Error opening temp file for copying...\n");
         return;
      }
      fscanf(fd, "%s", buf);
      while(!feof(fd))
      {
         if(strcmp(buf, text))
         {
            fprintf(fd1, "%s\n", buf);
            fflush(fd1);
         }
      fscanf(fd, "%s", buf);
      }
      fclose(fd);
      fclose(fd1);
      rename(temp, bad_gages_file_name);  
      
      init_mpe_bad_gage_list();
}


void okCallback( Widget w , XtPointer calldata , XtPointer clientdata )
{
   closeCallback(w, calldata, clientdata);
}


void closeCallback ( Widget w , XtPointer calldata , 
                        XtPointer clientdata )
{
   XtUnmanageChild ( badGageSH  ) ;  
   return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/mpe_bad_gage_list_show.c,v $";
 static char rcs_id2[] = "$Id: mpe_bad_gage_list_show.c,v 1.1 2007/05/24 13:06:48 whfs Exp $";}
/*  ===================================================  */

}
