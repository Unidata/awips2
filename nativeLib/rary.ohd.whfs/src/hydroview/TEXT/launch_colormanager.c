#include <stdlib.h>
#include <unistd.h>
#include <Xm/Xm.h>

#include "mpe_field_names.h"
#include "drawHvLegend.h"
#include "GeneralUtil.h"
#include "gageqc_gui.h"
#include "get_mpe_colors.h"
#include "hv_color_threshold.h"
#include "launch_colormanager.h"
#include "map_library.h"
#include "map_resource.h"
#include "pointcontrol_mgr.h"
#include "post_functions.h"

static void check_colors_changed ( XtPointer clientData, 
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
                      check_colors_changed,
                      NULL );
}

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

// --------------------------------------------------------------------------
static void displayColorManager()
{
 
    const char * jdbcUrlString = getenv("JDBCURL");
    const char * logFilePathString = "/dev/null/";
    const char * userIDString = getlogin();
    const char * applicationNameString = "hydroview";
    static char hvConfigDir[150];
    static char hydroviewDefaultColorFilePath[150];
    static char rgbColorFilePath[150];
    static int first = 1;
    int inputTokenNameLength;
    int outputTokenValueLength;
    XtAppContext app;

    if ( first == 1 )
    {
       /* Build the path to the file containing the default color scale. */
       memset ( hvConfigDir, '\0', 150 );
       memset ( hydroviewDefaultColorFilePath, '\0', 150);
       inputTokenNameLength = strlen ( "hv_config_dir" );
       get_apps_defaults ("hv_config_dir", & inputTokenNameLength, 
                          hvConfigDir, & outputTokenValueLength ); 

       if ( outputTokenValueLength == 0 )
       {
          printf ( "Token hv_config_dir undefined. Could not launch "
                   "the hydroview color manager.\n" );
          return;
       }

       sprintf ( hydroviewDefaultColorFilePath, "%s/HydroviewDefaultColorScale.dat", hvConfigDir );

       writeHydroViewDefaultColorDataFile ( hydroviewDefaultColorFilePath );

       /* Retrieve the path and name of the rgb.txt file. */
       memset ( rgbColorFilePath, '\0', 150 );
       inputTokenNameLength = strlen ( "rgb_file_path" );

       get_apps_defaults ( "rgb_file_path", & inputTokenNameLength,
                           rgbColorFilePath, & outputTokenValueLength );

       if ( outputTokenValueLength == 0 )
       {
           printf ("Token rgb_file_path undefined. Could not launch "
                   "the hydroview color manager.\n" );
           return;
       }

       first = 0;
    }

    /* Start the X-Timer. */
    app = _get_map_context ( );
    XtAppAddTimeOut ( app, 1000,
                      (XtTimerCallbackProc) check_colors_changed,
                      NULL );

    callColorManagerThroughJNI(jdbcUrlString,
                               logFilePathString,
                               applicationNameString,
                               userIDString,
                               hydroviewDefaultColorFilePath,
                               rgbColorFilePath);

    return;
}

// --------------------------------------------------------------------------

static void displayMPEColorManager()
{
    const char * jdbcUrlString = getenv("JDBCURL");
    const char * logFilePathString = "/dev/null/";
    const char * userIDString = getlogin();
    const char * applicationNameString = "hmapmpe";
    static char mpeAppDir[150];
    static char mpeEditorDefaultColorFilePath[150];
    static char rgbColorFilePath[150];
    static int first = 1;
    int inputTokenNameLength;
    int outputTokenValueLength;
    XtAppContext app;

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
          printf ( "Token mpe_app_dir undefined. Could not launch "
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
           printf ("Token rgb_file_path undefined. Could not launch "
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

    callColorManagerThroughJNI(jdbcUrlString,
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
    
   startjvm() ;
      
   displayColorManager();

}

// --------------------------------------------------------------------------
void launch_mpe_color_manager_callback ( Widget widget , XtPointer client_data, XtPointer call_data )
{
    
   char header[] = "launch_mpe_color_manager_callback(): ";
    
   printf("Before startJVM() in %s\n", header);
   startjvm() ;
   printf("After startJVM() in %s\n", header);
      
   displayMPEColorManager();
   
}
// --------------------------------------------------------------------------

void processHvLegendMouseClick ( Widget w, XtPointer client_data, XEvent * event,
                                 Boolean * flag )
{
     
     Boolean display_mpe_menu   = True;
     enum MapState                legend_state;
     enum MapState                mpeDataFlag ;
     int                          offset;
     unsigned int                 width , height ;
     int clickXPos;
     int clickYPos;
     int drawingPdcData = 0;
     int y;
       
     width = 40 ;
     height = 15 ;
     y = 20;

     clickXPos = event->xbutton.x;
     clickYPos = event->xbutton.y;

     printf ( "ClickXPos: %d ClickYPos: %d\n", clickXPos, clickYPos );

     legend_state = getHvDrawLegendStatus ( );
    
     /*-------------------------------------------------------------------------*/
     /* If no MPE or PDC data are being displayed, don't bother doing this. */
     /*-------------------------------------------------------------------------*/
     mpeDataFlag = isThereMpeData ( );
     
     // determine if the PDC data is being displayed
     drawingPdcData = getStationDrawingState ( ); 
    

     if ((mpeDataFlag == M_OFF ) && (! drawingPdcData) )
     {
        /* There are no MPE or PDC data.  Do not display the MPE legend. 
           The legend drawing area on the MPE Editor viewer must be
           disabled as well. This drawing area is owned by the map library.
           In order to keep the map library "pure" and without association
           to any application, the pixmap must be manipulated from here. */
    
        return ;
     }

     /* Set the base offset. */
     offset = 0;

     /* If MPE Data are displayed, check to see if the mouse click was on the MPE legend bar.  If it was,
        launch the MPE Color Manager window. */
     if ( ( mpeDataFlag == M_ON ) && ( display_mpe_menu == True ) )
     {
        if ( ( clickYPos > y + offset ) && ( clickYPos < y + offset + height ) )
        {
            /* Launch the MPE Color Manager. */
            launch_mpe_color_manager_callback ( w, NULL, NULL );
            return;
        } 
     } 

     /* Is MPE point information displayed?  This is not clickable. But the offset needs to be added so
        the PDC legend can be accurately located. */
     if ( ( mpeDataFlag == M_ON ) &&
              ( display_mpe_menu == True )  )
     {
        offset += MPE_LEGEND_HEIGHT;
        
        if (legend_state == M_ON)
        {
            offset += MPE_LEGEND_HEIGHT;
        }
     }
        
     /* If PDC Data are displayed, check to see if the mouse click was on the PDC legend bar. If it was,
        launch the Hydroview Manager window. */ 
     if (drawingPdcData)
     {   
        /* Launch the Hydroview Color Manager. */
        if ( ( clickYPos > offset + 10 ) && ( clickYPos < offset + height + 10 ) )
        {
           launch_color_manager_callback ( w, NULL, NULL );
           return;
        }

        offset += MPE_LEGEND_HEIGHT;      
     } 
    
    return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
