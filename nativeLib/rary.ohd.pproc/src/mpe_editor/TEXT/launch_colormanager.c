#include <stdlib.h>
#include <unistd.h>

#include "display_pdc_tsl_jni.h"
#include "GeneralUtil.h"
#include "get_mpe_colors.h"
#include "hv_color_threshold.h"
#include "launch_colormanager.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpe_log_utils.h"

// --------------------------------------------------------------------------

static void check_mpe_colors_changed ( XtPointer clientData, 
                                       XtIntervalId * intervalID )
{
    XtAppContext app;
    app = _get_map_context ( );
    long colorManagerSaveTime;
    static long previousSaveTime = 0;
    
    colorManagerSaveTime = getColorManagerSaveTimeThroughJNI();
    
    if ( colorManagerSaveTime > previousSaveTime )
    {
       // Update the map and legend colors.
       previousSaveTime = colorManagerSaveTime;
       mUpdateMap ( 0 );
       mUpdateLegend ( 0 );
       mUpdateMap ( 1 );
       mUpdateLegend ( 1 );
    }
    else if ( colorManagerSaveTime == -1 )
    {
       // Do not restart the time.  The color manager window has closed.
       return;
    }
    

    XtAppAddTimeOut ( app,
                      1000,
                      check_mpe_colors_changed,
                      NULL );
}

static void displayColorManager()
{
 
    const char * jdbcUrlString = getenv("JDBCURL");
    const char * logFilePathString = "/dev/null/";
    const char * userIDString = getlogin();
    const char * applicationNameString = "hmapmpe";
    static char mpeAppDir[150];
    static char mpeEditorDefaultColorFilePath[150];
    static char rgbColorFilePath[150];
    static char jdbcUrl [150];
    static int first = 1;
    int inputTokenNameLength;
    int outputTokenValueLength;
    XtAppContext app;

    strcpy ( jdbcUrl, jdbcUrlString );

    if ( first == 1 )
    {
       /* Build the path to the file containing the default color scale. */
       memset ( mpeAppDir, '\0', 150 );
       memset ( mpeEditorDefaultColorFilePath, '\0', 150);
       inputTokenNameLength = strlen ( "mpe_app_dir" );
       get_apps_defaults ("mpe_app_dir", & inputTokenNameLength, 
                          mpeAppDir, & outputTokenValueLength ); 

       if ( outputTokenValueLength == 0 )
       {
         logMessage ( "Token mpe_app_dir undefined. Could not launch "
                   "the mpe editor color manager.\n" );
          return;
       }

       sprintf ( mpeEditorDefaultColorFilePath, "%s/mpeEditorDefaultColorScale.dat", mpeAppDir );

       writeMpeEditorDefaultColorDataFile ( mpeEditorDefaultColorFilePath );

       /* Retrieve the path and name of the rgb.txt file. */
       memset ( rgbColorFilePath, '\0', 150 );
       inputTokenNameLength = strlen ( "rgb_file_path" );

       get_apps_defaults ( "rgb_file_path", & inputTokenNameLength,
                           rgbColorFilePath, & outputTokenValueLength );

       if ( outputTokenValueLength == 0 )
       {
          logMessage ("Token rgb_file_path undefined. Could not launch "
                   "the hydroview color manager.\n" );
           return;
       }

       first = 0;
    }
    
    /* Start the X-Timer. */
    app = _get_map_context ( );
    XtAppAddTimeOut ( app, 1000,
                      (XtTimerCallbackProc) check_mpe_colors_changed,
                      NULL );

    callColorManagerThroughJNI(jdbcUrl,
                               logFilePathString,
                               applicationNameString,
                               userIDString,
                               mpeEditorDefaultColorFilePath,
                               rgbColorFilePath);

    return;
}

// --------------------------------------------------------------------------
void launch_color_manager_callback ( Widget widget , XtPointer client_data, XtPointer call_data )
{
    
   char header[] = "launch_color_manager_callback(): ";
    
  logMessage("Before startJVM() in %s\n", header);
   startjavavm() ;
  logMessage("After startJVM() in %s\n", header);
      
   displayColorManager();

   mUpdateMap ( 0 );
   mUpdateLegend ( 0 );
   mUpdateMap ( 1 );
   mUpdateLegend ( 1 );
   

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc/src/mpe_editor/RCS/launch_colormanager.c,v $";
 static char rcs_id2[] = "$Id: launch_colormanager.c,v 1.2 2007/11/01 18:23:40 lawrence Exp $";}
/*  ===================================================  */

}

// --------------------------------------------------------------------------
