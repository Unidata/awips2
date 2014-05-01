#include <stdlib.h>
#include <sys/utsname.h>
#include "map.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpegui_save_jpeg.h"
#include "mpe_log_utils.h"
#include "stage3.h"

/******************************************************************/
/*   FUNCTION NAME:  mpegui_save_jpeg()                           */
/*       FUNCTION:   creates jpeg images (Linux only)             */
/*******************************************************************

Function type:
   void

Called by function:
   save_rfcwide

*******************************************************************/

void mpegui_save_jpeg(char fname[30], int map_number)
{
  int len;
  char unixcmd[500];
  char jpeg_dir[100], fnamej[34];
  String title = NULL ;

  len = strlen("mpe_jpeg_dir");
  get_apps_defaults("mpe_jpeg_dir", &len, jpeg_dir, &len);

  /* Retrieve the title of the main screen to print out. */
  XtVaGetValues ( _get_map_shell ( ) , XmNtitle , & title , NULL ) ;

  if ( title == NULL )
  {
     flogMessage ( stderr, "\nIn routine \"mpegui_save_jpeg\":\n"
                       "Could not retrieve the title of the shell to capture\n"
                       "for printing.\n" ) ;
  }
  else
  {
     sprintf(fnamej, "%s.jpeg",fname);
    logMessage("Saving jpeg file in %s/%s\n",jpeg_dir,fnamej);

     mUpdateMap(0);
     XSync(_get_map_display(),False);
     XmUpdateDisplay(_get_map_widget(map_number));
     sprintf(unixcmd, "cd %s\nimport -window %ld %s\n",jpeg_dir, 
             XtWindow(_get_map_shell()), fnamej);
     system(unixcmd);
  }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/mpegui_save_jpeg.c,v $";
 static char rcs_id2[] = "$Id: mpegui_save_jpeg.c,v 1.6 2007/02/22 16:06:21 lawrence Exp $";}
/*  ===================================================  */

 }
