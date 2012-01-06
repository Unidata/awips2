/*******************************************************************************
* FILENAME:            display7x7_show.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*            MODULE 1: show_display7x7
*         DESCRIPTION:
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       April 22, 2003
* ORGANIZATION:        OHD HSEB
* MACHINE:             HP-UX / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE           PROGRAMMER        DESCRIPTION/REASON
*          1        April 22, 2003 Bryon Lawrence    Original Coding
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

#define false 0
#define true (!false)

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   show_display7x7
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
********************************************************************************
*/

static int gage_number = 0 ;

const char * hrap_cells [ NUM_7X7_ROWS ] [ NUM_7X7_COLS ] =
{ { "hrap1LB","hrap2LB","hrap3LB","hrap4LB","hrap5LB","hrap6LB","hrap7LB" } ,
  { "hrap8LB","hrap9LB","hrap10LB","hrap11LB","hrap12LB","hrap13LB",
    "hrap14LB" } ,
  { "hrap15LB","hrap16LB","hrap17LB","hrap18LB","hrap19LB","hrap20LB",
    "hrap21LB" } ,
  { "hrap22LB","hrap23LB","hrap24LB","hrap25LB","hrap26LB","hrap27LB",
    "hrap28LB" } ,
  { "hrap29LB","hrap30LB","hrap31LB","hrap32LB","hrap33LB","hrap34LB",
    "hrap35LB" } ,
  { "hrap36LB","hrap37LB","hrap38LB","hrap39LB","hrap40LB","hrap41LB",
    "hrap42LB" } ,
  { "hrap43LB","hrap44LB","hrap45LB","hrap46LB","hrap47LB","hrap48LB",
    "hrap49LB" } } ;

void show_display7x7DS ( Widget w )
{
   XmString xm_str ;
   
   if ( ( display7x7DS == NULL ) ||
        ! XtIsManaged ( display7x7DS ) )
   {
      /* Create the display 7x7 GUI. */
      create_display7x7DS ( GetTopShell ( w ) ) ;

      /* Add the callbacks to the display 7x7 GUI widgets. */
      display7x7_callbacks ( ) ;
   }

   /* Desensitize the Undo button.  Since this is the first time into
      this routine, there is no undo history. */
   DeSensitize ( undoPB ) ;

   xm_str = XmStringCreate ( gage[gage_number].id , XmSTRING_DEFAULT_CHARSET ) ; 
   XtVaSetValues ( sitelidLB , XmNlabelString , xm_str , NULL ) ;
   XmStringFree ( xm_str ) ;


   /* Manage the form and the dialog shell which make up the
      display 7x7 GUI. */
   XtManageChild ( display7x7FO ) ;
   XtManageChild ( display7x7DS ) ;
   XtManageChild ( hrapRC ) ;


   return ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
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
********************************************************************************
*/
void display7x7_callbacks ( )
{
   Atom wmAtom ;

   /* Add the apply button callback. */
   XtAddCallback ( applyPB , XmNactivateCallback , apply7x7Callback , 
                   NULL ) ;

   /* Add the undo button callback. */
   XtAddCallback ( undoPB , XmNactivateCallback , undo7x7Callback ,  
                   NULL ) ;

   /* Add the bad gage toggle button callback. */
   XtAddCallback ( badPB , XmNactivateCallback , badGageToggle7x7Callback ,  
                   NULL ) ;
   
   /* Add the missing button callback. */
   XtAddCallback ( missingPB , XmNactivateCallback , missing7x7Callback , 
                   NULL ) ;

   /* Add the close button callback. */
   XtAddCallback ( closePB , XmNactivateCallback , close7x7Callback , 
                   NULL ) ;

   /* Add the callback to be used when closing the display7x7 window
      from the close button on the window's frame. */
   wmAtom = XmInternAtom ( XtDisplay ( display7x7DS ) , "WM_DELETE_WINDOW" , 
                           FALSE ) ;
   XmAddWMProtocolCallback ( display7x7DS , wmAtom , close7x7Callback ,
                             NULL ) ;
 }

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
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
********************************************************************************
*/
void apply7x7Callback ( Widget w , XtPointer calldata , 
                        XtPointer clientdata )
{
   int value ;
   XmString xm_str ;

   /* Read the scale. */
   XmScaleGetValue ( editprecipSC , & value ) ; 

   /* Set the value in the appropriate gage structure. */
   sprintf ( gage [ gage_number ].edit , "%.2f" , 
             ( float ) value / 100. ) ;     

   /* Change the site id label on the display 7x7 gui. */
   xm_str = XmStringCreate ( gage [ gage_number ].edit ,
                             XmSTRING_DEFAULT_CHARSET ) ;
   XtVaSetValues ( precipamountLB , XmNlabelString , xm_str , NULL ) ;
   XmStringFree ( xm_str ) ;

   /* Sensitize the Undo button. */
   Sensitize ( undoPB ) ;

   return ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
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
********************************************************************************
*/
void undo7x7Callback ( Widget w , XtPointer calldata , 
                       XtPointer clientdata )
{
   char gval [ 12 ] ;
   XmString xm_str ;

   /* The fact that the user was able to call this routine
      must mean that the the original gage value has been changed */
   /* Set the precip amount label back to the original value. */
   if ( gage [ gage_number ].gval == -999. )
   {
      sprintf ( gval , "missing" ) ;
   }
   else
   {
      sprintf ( gval , "%.2f in." , gage [ gage_number ].gval ) ;
   }

   xm_str = XmStringCreate ( gval ,
                             XmSTRING_DEFAULT_CHARSET ) ;
   XtVaSetValues ( precipamountLB , XmNlabelString , xm_str , NULL ) ;
   XmStringFree ( xm_str ) ;

   /* Undo the edit member in the gage structure representing this 
      gage. */
   memset ( gage [ gage_number ].edit , '\0' , 10 ) ; 

   /* Set the scale to the original value. */
   if ( gage [ gage_number ].gval == -999. )
   {
      XmScaleSetValue ( editprecipSC , 0 ) ; 
   }
   else
   {
      XmScaleSetValue ( editprecipSC , 100 * ( gage [ gage_number ].gval ) ) ; 
   }

   /* Desensitize the Undo button. */
   DeSensitize ( undoPB ) ;

   return ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
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
********************************************************************************
*/
void badGageToggle7x7Callback ( Widget w , XtPointer calldata , 
                       XtPointer clientdata )
{
   char gval [ 9 ] ;
   XmString xm_str ;
   FILE *fd = NULL, *fd1 = NULL;
   char buf[9];
   char duplicate[9];
   char dirname[120];
   char temp[120];
   char bad_gages_file_name[120];
   int len = 0;

   memset(buf, '\0', 9);
   memset(duplicate, '\0', 9);
   memset(dirname, '\0', 120);
   memset(temp, '\0', 120);
   memset(bad_gages_file_name, '\0', 120);
     
   xm_str = XmStringCreate ( gage[gage_number].id , XmSTRING_DEFAULT_CHARSET ) ; 
   XtVaSetValues ( sitelidLB , XmNlabelString , xm_str , NULL ) ;
   XmStringFree ( xm_str ) ;
   
   len = strlen("mpe_bad_gages_dir");
   get_apps_defaults("mpe_bad_gages_dir",&len,dirname,&len);
   strcpy(bad_gages_file_name, dirname);
   strcat(bad_gages_file_name, "/mpe_bad_gage_list");
   strcat(temp, dirname);
   strcat(temp, "/temp");
 
   if(gage[gage_number].is_bad == true)
   {
      sprintf ( gval , "%.2f in." , gage [ gage_number ].gval ) ;
      xm_str = XmStringCreate ( "Set Bad" , XmSTRING_DEFAULT_CHARSET ) ;
      XtVaSetValues ( badPB , XmNlabelString , xm_str , NULL ) ;
      XmStringFree ( xm_str ) ;
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
         if(strcmp(buf, gage[gage_number].id))
         {
            fprintf(fd1, "%s\n", buf);
            fflush(fd1);
         }
      fscanf(fd, "%s", buf);
      }
      fclose(fd);
      fclose(fd1);
      rename(temp, bad_gages_file_name);
      gage[gage_number].is_bad = false;
      if(gage[gage_number].gval == -999.)
      {
         sprintf(gval, "missing");
         sprintf(gage [ gage_number ].edit, "%c", 'M');   
      }
      else
      {
         XmScaleSetValue ( editprecipSC , 100 * ( gage [ gage_number ].gval ) ) ; 
      }
   }
   else if(gage[gage_number].is_bad == false)
   {
      sprintf(gage [ gage_number ].edit, "%c", 'B');   
      sprintf ( gval , "bad" ) ;
      xm_str = XmStringCreate ( "Set Not Bad" , XmSTRING_DEFAULT_CHARSET ) ;
      XtVaSetValues ( badPB , XmNlabelString , xm_str , NULL ) ;
      XmStringFree ( xm_str ) ;
      fd = (FILE*) fopen(bad_gages_file_name, "a+");
      if(fd == NULL)
      {
        logMessage("Error opening mpe_bad_gage_list file: %s...\n", bad_gages_file_name);
         return;
      }   
      fscanf(fd, "%s", duplicate);
      while(!feof(fd))
      {
         if(!strcmp(duplicate, gage[gage_number].id))
         {
            XmScaleSetValue ( editprecipSC , 0 ) ;
            xm_str = XmStringCreate ( gval , XmSTRING_DEFAULT_CHARSET ) ;
            XtVaSetValues ( precipamountLB , XmNlabelString , xm_str , NULL ) ;
            XmStringFree ( xm_str ) ;
            fclose(fd);

            return;
         }
      fscanf(fd, "%s", duplicate);
      }
      fprintf(fd, "%s\n", gage[gage_number].id);
      fflush(fd);
      fclose(fd);
      gage[gage_number].is_bad = true;
      XmScaleSetValue ( editprecipSC , 0 ) ;
   }

   xm_str = XmStringCreate ( gval , XmSTRING_DEFAULT_CHARSET ) ;
   XtVaSetValues ( precipamountLB , XmNlabelString , xm_str , NULL ) ;
   XmStringFree ( xm_str ) ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME
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
********************************************************************************
*/
void missing7x7Callback ( Widget w , XtPointer calldata , 
                         XtPointer clientdata )
{
   char gval [ 12 ] ;
   XmString xm_str ;

   strcpy ( gage [ gage_number ].edit , "M" ) ;
   sprintf ( gval , "missing" ) ;

   xm_str = XmStringCreate ( gval ,
                             XmSTRING_DEFAULT_CHARSET ) ;
   XtVaSetValues ( precipamountLB , XmNlabelString , xm_str , NULL ) ;
   XmStringFree ( xm_str ) ;

   Sensitize ( undoPB ) ;

   return ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
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
********************************************************************************
*/
void close7x7Callback ( Widget w , XtPointer calldata , 
                        XtPointer clientdata )
{
   extern int display_7x7_flag ;

   /* Turn off display 7x7 mode. */
   XtUnmanageChild ( display7x7DS ) ;

   /* Turn off display 7x7 mode. */
   display_7x7_flag = 0 ;
   
   /* Enable the exposure watch. */
   mEnableExposeWatch ( ) ;

   /* Set the cursor to normal. */
   mSetCursor ( M_NORMAL ) ;

   return ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
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
********************************************************************************
*/
void set_display7x7_gage_number ( num ) 
{
   gage_number = num ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/display7x7_show.c,v $";
 static char rcs_id2[] = "$Id: display7x7_show.c,v 1.2 2007/05/24 13:00:04 whfs Exp $";}
/*  ===================================================  */

}
