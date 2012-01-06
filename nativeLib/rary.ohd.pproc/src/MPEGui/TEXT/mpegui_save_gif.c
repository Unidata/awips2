#include <Xm/Xm.h>
#include <sys/utsname.h>
#include "map.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "mpegui_save_gif.h"
#include "stdlib.h"
#include "stage3.h"

/******************************************************************/
/*   FUNCTION NAME:  mpegui_save_gif()                            */
/*       FUNCTION:   saves windows as gif images (HP-UX only)     */
/*******************************************************************

Function type:
   void

Called by function:
   save_rfcwide

*******************************************************************/

void mpegui_save_gif(char fname[30], int map_number)
{
 int len;
 char unixcmd[500];
 char bin_dir[128],gif_dir[128];
 String title = NULL ;

 /*---------------------------------------------------------*/
 /* save image as window dump and transform into gif image  */
 /*---------------------------------------------------------*/

  len = strlen("hydro_publicbin");
  get_apps_defaults("hydro_publicbin",&len, bin_dir,&len);
   
  len = strlen("mpe_gif_dir");
  get_apps_defaults("mpe_gif_dir", &len, gif_dir, &len);

  /* Retrieve the title of the main screen to print out. */
  XtVaGetValues ( _get_map_shell ( ) , XmNtitle , & title , NULL ) ;
   
  if ( title == NULL )
  {
     flogMessage ( stderr, "\nIn routine \"mpegui_save_gif\":\n"
                       "Could not retrieve the title of the shell to capture\n"
                       "for printing.\n" ) ;
  }
  else
  {
     /* Updated 1/11/2007 to allow MPE Editor to save gif images. */
     flogMessage ( stdout , "\nIn routine \"mpegui_save_gif\":\n"
                        "Saving gif file in %s/%s.gif\n" , gif_dir , fname ) ;
     mUpdateMap(0);
     XSync (_get_map_display(), False);
     XmUpdateDisplay(_get_map_widget(map_number));
     sprintf(unixcmd, "cd %s\nimport -window %ld %s.gif\n", gif_dir,
                      XtWindow(_get_map_shell()), fname);
     system ( unixcmd ) ;
  }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/mpegui_save_gif.c,v $";
 static char rcs_id2[] = "$Id: mpegui_save_gif.c,v 1.4 2007/02/22 16:05:53 lawrence Exp $";}
/*  ===================================================  */

}
