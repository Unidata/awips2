
/*******************************************************************************
* FILENAME:             time_lapse_RFCW.c
* NUMBER OF MODULES:    3
* GENERAL INFORMATION:
*
*   MODULE 1:           manual_time_lapse
*                       This routine allows the user to manually control the
*                       time lapse using the four "arrow" keys.  This routine
*                       is static to this file.  It cannot be linked to
*                       by functions outside of the scope of this file.
*
*   MODULE 2:           time_lapse_RFCW
* DESCRIPTION:          This routine builds a time lapse for a duration of
*                       6 hours, 12 hours, 24 hours, or a user specified
*                       number of hours.
*
*   MODULE 3:           free_time_lapse_memory
* DESCRIPTION:          This routine frees the dynamic memory allocated to
*                       create the "tl_draw_data.data_array".  This routine
*                       was provided to free the allocated memory when the
*                       hmap_mpe application is shut down.
*
*   MODULE 4:           set_time_lapse_flag_on
* DESCRIPTION:          Sets the state of the time lapse flag to "on".
*
*   MODULE 5:           set_time_lapse_flag_off
* DESCRIPTION:          Sets the state of the time lapse flag to "off".
*
*   MODULE 6:           get_time_lapse_flag
* DESCRIPTION:          Retrieves the state of the time lapse flag.
*
*   MODULE 7:           end_time_lapse_RFCW
* DESCRIPTION:          Shuts down the time lapse loop.  This routine is
*                       a wrapper around "end_time_lapse".  It allows
*                       "end_time_lapse" to be used as a callback from
*                       a widget.
*
*   MODULE 8:           end_time_lapse
* DESCRIPTION:          An event handler given the task to stop the time
*                       lapse loop.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        March 21, 2002
* ORGANIZATION:         HSEB / OHD
* MACHINE:              HP-UX / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE              PROGRAMMER      DESCRIPTION/REASON
*          1        March 21, 2002    Bryon Lawrence  Created
*          2        March 21, 2002    Bryon Lawrence  Created
*          2        January 16, 2003  Bryon Lawrence  Added the ability to
*                                                     time lapse Satellite
*                                                     precipitation
*                                                     estimates.
*          3        March 21, 2002    Bryon Lawrence  Created
*          4        March 25, 2002    Bryon Lawrence  Created
*          5        March 25, 2002    Bryon Lawrence  Created
*
********************************************************************************
*/

#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/keysymdef.h>

#include "display_field_data_RFCW.h"
#include "drawa.h"
#include "get_mpe_colors.h"
#include "loop_callback.h"
#include "map.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "NamedColorSetGroup.h"
#include "newhour_RFCW.h"
#include "post_functions.h"
#include "ReadSPE.h"
#include "read_xmrg.h"
#include "rfcwide.h"
#include "rfcwide_callbacks.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "Swap2Bytes.h"
#include "TestByteOrder.h"
#include "TestXmrg.h"
#include "time_lapse_RFCW.h"
#include "Xtools.h"

static KeySym keysym1 [ ] = { XK_Left, XK_Right, XK_Up, XK_Down } ;
static KeyCode keycode[XtNumber(keysym1)];

static int time_lapse_flag = 0 ;
static loop_struct loop_data ;
static int manual_loop = 0 ;
static Widget wid ;
static int date_array_index = 0 ;


/*******************************************************************************
* MODULE NUMBER: 1
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

void end_manual_loop_callback ( Widget w, XtPointer clientdata, XtPointer calldata )
{
   int duration ;
   int running_flag;
   XtAppContext app ;

   running_flag = get_time_lapse_flag ( );

   if ( running_flag == 0 ) return;

   if ( manual_loop == 1 )
   {
      /* Turn off the manual looping capability. */
      manual_loop = 0 ;

      /* Add the app time out. */
      app = _get_map_context ( ) ;
      duration = get_loop_callback_duration ( ) ;
      set_loop_callback_interval_id ( XtAppAddTimeOut ( app ,
                                      duration , loop_callback ,
                                         ( XtPointer ) & loop_data ) ) ;
   }
}

void manual_loop_callback ( Widget w, XtPointer clientdata, XtPointer calldata )
{
   int direction = ( int ) clientdata;
   int running_flag;

   /* Check to make sure that there is a time lapse running. If not
      then do not do anything. */
   running_flag = get_time_lapse_flag ( );

   if ( running_flag == 0 ) return;

   if ( manual_loop == 0 )
   {
      manual_loop = 1 ;

      /* Remove the app time out that is driving the loop. */
      XtRemoveTimeOut ( get_loop_callback_interval_id ( ) ) ;

      if ( direction == LOOP_BACKWARD )
      {
         loop_data.icnt = 0 ;
      }
      else
      {
         loop_data.icnt = loop_data.max ;
      }
   }
   else
   {
      if ( direction == LOOP_BACKWARD )
      {
         loop_data.icnt ++ ;
         if ( loop_data.icnt > loop_data.max ) loop_data.icnt = 0 ;
      }
      else
      {
         loop_data.icnt -- ;
         if ( loop_data.icnt < 0 ) loop_data.icnt = loop_data.max ;
      }
   }

   /* Update the time stamp which will ultimately be displayed in the
      legend on the MPE Editor display. */
   date_st3 = loop_data.dates [ loop_data.icnt ] ;

   if ( loop_data.icnt != loop_data.max )
   {
      date_prev = loop_data.dates [ loop_data.icnt + 1 ] ;
   }
   else
   {
      date_prev = loop_data.dates [ 0 ] ;
   }

   setPixmapToDraw ( & ( loop_data.pixmap [ loop_data.icnt ] ) ,
                     & ( loop_data.gc [ 0 ] ) ) ;
   mUpdateMap ( 0 ) ;
}

void hydroview_manual_loopCB ( Widget w,
                               XtPointer clientdata,
                               XtPointer calldata )
{
   int direction = ( int ) clientdata;
   int running_flag;

   /* Check to make sure that there is a time lapse running. If not
      then do not do anything. */
   running_flag = get_time_lapse_flag ( );

   if ( running_flag == 0 ) return;

   if ( manual_loop == 0 )
   {
      manual_loop = 1 ;

      /* Remove the app time out that is driving the loop. */
      XtRemoveTimeOut ( get_loop_callback_interval_id ( ) ) ;
   }

   if ( direction == LOOP_BACKWARD )
   {
      loop_data.icnt = 0 ;
   }
   else
   {
      loop_data.icnt = loop_data.max ;
   }

   date_st3 = loop_data.dates[loop_data.icnt];

   setPixmapToDraw ( & ( loop_data.pixmap [ loop_data.icnt ] ) ,
                     & ( loop_data.gc [ 0 ] ) ) ;
   mUpdateMap ( 0 ) ;
   mUpdateLegend ( 0 );
}

void loop_step_callback ( Widget w,
                          XtPointer clientdata,
                          XtPointer calldata )
{
   int direction = ( int ) clientdata;
   int running_flag;

   /* Check to make sure that there is a time lapse running. If not
      then do not do anything. */
   running_flag = get_time_lapse_flag ( );

   if ( running_flag == 0 ) return;

   if ( manual_loop == 0 )
   {
      /* Enter manual time lapse mode. */
      manual_loop = 1 ;

      /* Remove the app time out that is driving the loop. */
      XtRemoveTimeOut ( get_loop_callback_interval_id ( ) ) ;
   }
   else
   {
      if ( direction == LOOP_BACKWARD )
      {
         loop_data.icnt ++ ;
         if ( loop_data.icnt > loop_data.max ) loop_data.icnt = 0 ;
      }
      else
      {
         /* The loop direction must be forward. */
         loop_data.icnt -- ;
         if ( loop_data.icnt < 0 ) loop_data.icnt = loop_data.max ;
      }

      date_st3 = loop_data.dates[loop_data.icnt];
   }

   setPixmapToDraw ( & ( loop_data.pixmap [ loop_data.icnt ] ) ,
                     & ( loop_data.gc [ 0 ] ) ) ;
   mUpdateMap ( 0 ) ;
   mUpdateLegend ( 0 );
}


void manual_time_lapse ( Widget w , XtPointer clientdata ,
                         XEvent * event ,
                         Boolean * continue_to_dispatch_event )
{

   KeyCode code ;

   code = event->xkey.keycode ;

   /* Test the key pressed.  Is it one of the "arrow" keys? */

   if ( ( code == keycode [ 0 ] ) || ( code == keycode [ 1 ] ) )
   {

   	  if ( code == keycode [ 0 ] )
   	  {
   	     manual_loop_callback ( NULL, (XtPointer) LOOP_BACKWARD, NULL );
   	  }
   	  else
   	  {
   	  	 manual_loop_callback ( NULL, (XtPointer) LOOP_FORWARD, NULL );
   	  }

   }
   else if ( ( code == keycode [ 2 ] ) || ( code == keycode [ 3 ] ))
   {
      end_manual_loop_callback ( NULL, NULL, NULL );
   }
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   time_lapse_RFCW
* PURPOSE:       Initializes a time loop for a 6, 12, or 24 hour duration or
*                for a user-specified number of hours.  This involves reading
*                the data for each of the hours leading up to the time of the
*                data that the user is currently viewing.  The data for each
*                hour is stored in an array of pixmaps.  The "loop_callback"
*                function is then registered as a "time out" application
*                which is called once during a fixed duration of time.
*
*                "Loop_callback" is in charge of managing the loop.  It
*                makes sure that the pixmap containing the next hour's
*                worth of data is displayed each time it is called.  It
*                also reregisters itself as a "time out" function making
*                sure that it is called by the system after the next fixed
*                duration of time.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   I      Widget      w           The widget from which the callback was
*                                  invoked.
*   I      XtPointer   clientdata  User-supplied callback data.
*   I      XtPointer   calldata    X/Motif-supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                    HEADER FILE       DESCRIPTION
*   fill_pixmap             post_functions.h  Generates a drawable pixmap of
*                                             MPE data based on raw HRAP
*                                             gridded data.
*   free_temp_memory        N/A               Frees the dynamic memory
*                                             allocated to the "temp"
*                                             variable.
*   free_time_lapse_memory  time_lapse_RFCW.h Frees the dynamic memory
*                                             allocated to the
*                                             "tl_draw_data.data_array"
*                                             data member.
*   get_apps_defaults       GeneralUtil.h     Retrieves the value of a token
*                                             from one of the Apps_defaults
*                                             files or the environment.
*   _get_map_context        map_resource.h    Retrieves the application
*   _get_map_height         map.h             Returns the height of the map.
*   _get_map_width          map.h             Returns the width of the map.
*                                             context of the main hmap_mpe
*                                             application.
*   get_pixel_by_name       stage3_globals.h  Retrieves the color value
*                                             for a specified color name.
*   mUpdateMap              map_library.h     Forces the hmap_mpe viewer to
*                                             redraw itself.
*   read_xmrg               read_xmrg.h       Reads a MPE data field from a
*                                             flat file.
*   setDrawStruct           post_functions.h  Sets the draw_struct structure
*                                             that the pixmap drawing routines
*                                             in the fill_pixmap.c file
*                                             will use for plotting
*                                             the MPE data.
*   set_loop_callback_interval_id
*                           loop_callback.h   Sets the interval id (as returned
*                                             from XtAppAddTimeOut) so that
*                                             the "time out" can be deactivated
*                                             when the loop is shut down.
*   setPixmapToDraw         post_functions.h  Sets the pixmap to use to draw to
*                                             the screen.  This overrides the
*                                             default functionality of the
*                                             fill_pixmap routines to accept,
*                                             process, and draw raw HRAP
*                                             gridded data.
*   Swap2Bytes_             Swap2Bytes.h      Swaps the bytes in word
*                                             consisting of 2 bytes.
*   TestByteOrder_          TestByteOrder.h   Tests the byte ordering in a file
*                                             to determine if the bytes
*                                             need to be swapped to match
*                                             the operating systems
*                                             architecture.
*   turnOnMpeData           post_functions.h  Indicates to the map drawing
*                                             routines that there IS
*                                             MPE data to draw.
*
* CALLED BY FUNCTIONS:
*   do_time_lapse_6_RFCW
*   do_time_lapse_12_RFCW
*   do_time_lapse_24_RFCW
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*  DATA TYPE           NAME         DESCRIPTION
*  char [ ]            cdate         The date of the MPE data currently being
*                                    processed.
*  char [ ]            dirname       The directory in which "fname" the MPE data
*                                    file is located.
*  char [ ]            fname         The name of the file containing the MPE
*                                    data to be viewed.
*  char [ ]            time          An array containing the current time of the
*                                    loop data being processed.
*  Dimension           width         The width of the map window.
*  Dimension           height        The height of the map window.
*  Display *           dpy           The display the application resides in.
*  GC                  gc            The graphics context for drawing the date
*                                    string to the window.
*  int                 duration      The duration of the time lapse as
*                                    specified by the "hmap_mpe_timelapse"
*                                    token.
*  int                 i, j, k, ii   Loop indexing variables.
*  int                 len           The length of a string to be drawn using
*                                    XDrawString.
*  int                 len_fname     The length of the filename.
*  int                 ifile         The status indicator returned from the
*                                    read_xmrg routine.  A value of "0" means
*                                    that the attempt to read the file
*                                    failed.
*  int                 idate         The date stamp that is built into the
*                                    filename containing the MPE data.
*  int                 mask          A value indicating which type of graphics
*                                    context to create.
*  loop_struct         loop_data     A static variable containing the pixmaps
*                                    of drawable MPE data to loop over.
*  short *             temp          A temporary array which contains a single
*                                    row of gridded HRAP data as read in from
*                                    the flat data file.
*                                    data read in from the file.
*  enum TestByteResult result        The result of the xmrg file's byte test.
*  time_lapse_struct * data          A pointer to the draw_struct data structure
*                                    passed into this callback as
*                                    "clientdata".
*  XGCValues           gcv           Contains values specific to a graphics
*                                    context.
*  XtAppContext        app           The application context.
*  XtIntervalId        xtid          Contains the identifier of the interval
*                                    created by XtAppAddTimeOut.
*
*
* DATA FILES AND/OR DATABASE:
*  This routine uses the xmrg files generated by the "field_gen" routine.
*
* ERROR HANDLING:
*    Error messages pertaining to memory allocation problems are printed out
*    to standard error.  When a problem is encountered allocating dynamic
*    memory, this routine will try to clean up as much allocated memory
*    as possible and then return.
*
********************************************************************************
*/

void time_lapse_RFCW ( Widget w , XtPointer clientdata , XtPointer calldata )
{
   Boolean              ReturnInitialDate ;
   char                 cdate [ 11 ] ;
   char                 dirname [ 100 ] ;
   char                 fname [ 128 ] ;
   char                 time [ 30 ] ;
   enum DisplayFieldData display_field_type ;
   Dimension            width , height ;
   Display              * dpy = NULL ;
   float                mm_factor ;
   GC                   gc ;
   extern int           cv_duration;
   int                  ida , ihr , im , imo , is , iyr , tdiff , tunit ;
   int                  i , j , k , ii , len , len_fname , ifile , idate ;
   int                  duration ;
   int                  index ;
   int                  mask = GCForeground ;
   int                  spe_index ;
   Pixmap               user_pixmap;
   short int *          site_spe = NULL ;
   short                * temp = NULL ;
   short int            u_thres ;
   short int            x_factor ;
   enum TestByteResult  result ;
   time_lapse_struct    * data = ( time_lapse_struct * ) clientdata ;
   XGCValues            gcv ;
   XtAppContext app ;
   XtIntervalId xtid = 0 ;
   struct _Overlay * pOverlay = NULL;
   NamedColorSetGroup * pColors = NULL;

   pOverlay = mGetOverlay ( M_TOPOGRAPHY  );

   display_field_type = rad_data [ 0 ].field_type;

 /*------------------------------------------*/
 /* Build the filepath to containing the MPE */
 /* field data being retrieved.  Note that   */
 /* PRISM and multi hour QPE data cannot be  */
 /* looped.                                  */
 /*------------------------------------------*/

 switch ( display_field_type )
 {
    case display_rMosaic :
       len = strlen("rfcwide_rmosaic_dir");
       get_apps_defaults("rfcwide_rmosaic_dir",&len,dirname,&len);
       break ;

    case display_bMosaic :
       len = strlen("rfcwide_bmosaic_dir");
       get_apps_defaults("rfcwide_bmosaic_dir",&len,dirname,&len);
       break ;

    case display_mMosaic :
       len = strlen("rfcwide_mmosaic_dir");
       get_apps_defaults("rfcwide_mmosaic_dir",&len,dirname,&len);
       break ;

    case display_mlMosaic :
       len = strlen("rfcwide_mlmosaic_dir");
       get_apps_defaults("rfcwide_mlmosaic_dir",&len,dirname,&len);
       break;

    case display_rfcMosaic :
       len = strlen("gaq_xmrg_1hr_dir");
       get_apps_defaults("gaq_xmrg_1hr_dir",&len,dirname,&len);
       break;

    case display_lMosaic :
       len = strlen("rfcwide_lmosaic_dir");
       get_apps_defaults("rfcwide_lmosaic_dir",&len,dirname,&len);
       break ;

    case display_Xmrg :
       len = strlen("rfcwide_xmrg_dir");
       get_apps_defaults("rfcwide_xmrg_dir",&len,dirname,&len);
       break ;

    case display_Locspan :
       len = strlen("rfcwide_locspan_dir");
       get_apps_defaults("rfcwide_locspan_dir",&len,dirname,&len);
       break ;

    case display_Locbias :
       len = strlen("rfcwide_locbias_dir");
       get_apps_defaults("rfcwide_locbias_dir",&len,dirname,&len);
       break ;

    case display_satPrecip :
       len = strlen("rfcwide_satpre_dir");
       get_apps_defaults("rfcwide_satpre_dir",&len,dirname,&len);
       break ;

    case display_lsatPrecip :
       len = strlen("rfcwide_lsatpre_dir");
       get_apps_defaults("rfcwide_lsatpre_dir",&len,dirname,&len);
       break ;

    case display_srMosaic :
       len = strlen("mpe_srmosaic_dir");
       get_apps_defaults("mpe_srmosaic_dir",&len,dirname,&len);
       break ;

    case display_sgMosaic :
       len = strlen("mpe_sgmosaic_dir");
       get_apps_defaults("mpe_sgmosaic_dir",&len,dirname,&len);
       break ;

    case display_srgMosaic :
       len = strlen("mpe_srgmosaic_dir");
       get_apps_defaults("mpe_srgmosaic_dir",&len,dirname,&len);
       break ;

    case display_rfcbMosaic :
       len = strlen("mpe_rfcbmosaic_dir");
       get_apps_defaults("mpe_rfcbmosaic_dir",&len,dirname,&len);
       break ;

    case display_rfcmMosaic :
       len = strlen("mpe_rfcmmosaic_dir");
       get_apps_defaults("mpe_rfcmmosaic_dir",&len,dirname,&len);
       break ;

    case display_Index :
       len = strlen("rfcwide_index_dir");
       get_apps_defaults("rfcwide_index_dir",&len,dirname,&len);
       break ;

    case display_Height :
       len = strlen("rfcwide_height_dir");
       get_apps_defaults("rfcwide_height_dir",&len,dirname,&len) ;
       break ;

    case display_gageOnly :
       len = strlen("rfcwide_gageonly_dir");
       get_apps_defaults("rfcwide_gageonly_dir",&len,dirname,&len);
       break ;

    case display_p3Mosaic:
       len = strlen("rfcwide_p3lmosaic_dir");
       get_apps_defaults("rfcwide_p3lmosaic_dir",&len,dirname,&len);
       break;

    case display_avgrMosaic:
       len = strlen("rfcwide_avg_rmosaic_dir");
       get_apps_defaults("rfcwide_avg_rmosaic_dir",&len,dirname,&len);
       break;

    case display_maxrMosaic:
       len = strlen("rfcwide_max_rmosaic_dir");
       get_apps_defaults("rfcwide_max_rmosaic_dir",&len,dirname,&len);
       break;

    case display_multiHour :
       flogMessage ( stderr , "\nIn routine \"time_lapse_RFCW\":\n"
                          "Reached the \"display_multiHour\" case in the\n"
                          "switch/case statement.  Cannot perform a time\n"
                          "lapse on the multihour QPE field.\n" ) ;
       return ;

    case display_Prism :
    case display_maxtempPrism :
    case display_mintempPrism :

       flogMessage ( stderr , "\nIn routine \"time_lapse_RFCW\":\n"
                          "Reached the \"display_Prism\" case in the\n"
                          "switch/case statement.  Cannot perform a time\n"
                          "lapse on the PRISM data.\n" ) ;
       return ;

    case display_missing :

       flogMessage ( stderr , "\nIn routine \"time_lapse_RFCW\":\n"
                          "Reached the \"display_missing\" case in the\n"
                          "switch/case statement.  This is probably the\n"
                          "result of a logic or coding error in the\n"
                          "software.\n" ) ;
       return ;

    default :

       flogMessage ( stderr , "\nIn routine \"time_lapse_RFCW\":\n"
                          "Unrecognized display type in switch statement.\n"
               ) ;
 }

 /*------------------------------------------*/
 /*  initialization                          */
 /*------------------------------------------*/
 wid = data->w ;
 loop_data.max = data->nhrs-1;
 loop_data.icnt = 0 ;
 loop_data.gc = 0 ;

 /*--------------------------------------------------------------*/
 /*  Turn off the watch cursor which is usually displayed        */
 /*  when the map screen is redrawn.                             */
 /*--------------------------------------------------------------*/
 mDisableExposeWatch ( ) ;

 /*--------------------------------------------------------------*/
 /*  set up data display same as currently exist on main window  */
 /*--------------------------------------------------------------*/
 tl_draw_data.w = wid ;
 tl_draw_data.maximum_columns = rad_data[0].maximum_columns;
 tl_draw_data.maximum_rows    = rad_data[0].maximum_rows;
 tl_draw_data.num_levels      = rad_data[0].num_levels;
 tl_draw_data.states_on       = rad_data[0].states_on;
 tl_draw_data.rivers_on       = rad_data[0].rivers_on;
 tl_draw_data.basins_on       = rad_data[0].basins_on;
 tl_draw_data.rings_on        = rad_data[0].rings_on;
 tl_draw_data.cities_on       = rad_data[0].cities_on;
 tl_draw_data.origin.x        = XOR;
 tl_draw_data.origin.y        = YOR;
 tl_draw_data.pixbase         = 0;

 /* Define local array "temp". */
 temp = ( short * ) malloc ( MAXX * sizeof ( short ) ) ;

 if ( temp == NULL )
 {
    flogMessage ( stderr , "\nIn routine \"time_lapse_RFCW\":\n"
                       "The attempt to allocate memory to variable \"temp\"\n"
                       "has failed.  Aborting the attempt to perform the\n"
                       "time lapse.\n" ) ;
    return ;
 }

 /*--------------------------------------------------------------*/
 /*     allocate space for data array                            */
 /*--------------------------------------------------------------*/

 if ( tl_draw_data.data_array == NULL )
 {
    tl_draw_data.data_array = ( int ** ) malloc (
                              ( tl_draw_data.maximum_columns ) *
                                sizeof ( int * ) ) ;

    if ( tl_draw_data.data_array == NULL )
    {
       flogMessage ( stderr , "\nIn routine \"time_lapse_RFCW\":\n"
                          "Could not allocate memory for variable\n"
                          "\"tl_draw_data.data_array\".  Aborting the\n"
                          "attempt to start the time lapse.\n" ) ;

       if ( temp != NULL )
       {
          free ( temp ) ;
          temp = NULL ;
       }

       free_time_lapse_memory ( ) ;
       return ;
    }

    for ( i = 0 ; i < tl_draw_data.maximum_columns ; i++ )
    {
       tl_draw_data.data_array [ i ] = ( int * ) malloc (
                                       ( tl_draw_data.maximum_rows ) *
                                       sizeof ( int ) ) ;

       if ( tl_draw_data.data_array [ i ] == NULL )
       {
          flogMessage ( stderr , "\nIn routine \"time_lapse_RFCW\":\n"
                             "The attempt to allocate memory for variable\n"
                             "\"tl_draw_data.data_array [ i ]\", where\n"
                             "i = %d.  Aborting the attempt to start the\n"
                             "time lapse.\n" , i ) ;

          if ( temp != NULL )
          {
             free ( temp ) ;
             temp = NULL ;
          }

          free_time_lapse_memory ( ) ;
          return ;
       }

    }

  }

  /* Set up the tl_draw_data structure to contain the previous
     number of color levels. */

 dpy = XtDisplay ( wid ) ;

 /*--------------------------------------------------------------*/
 /*     set up watch cursor to say please wait                   */
 /*--------------------------------------------------------------*/
 mSetCursor ( M_WATCH ) ;

 /* Set the color use and duration in the time lapse data structure. i
    These are defined by the currently displayed MPE product. */
 strcpy ( tl_draw_data.cv_use, cv_use );
 tl_draw_data.cv_duration = cv_duration;

 /*--------------------------------------------------------------*/
 /*     set up graphics context for time loop                    */
 /*--------------------------------------------------------------*/
 pColors = get_mpe_default_colors ( );

 MPEGui_set_colorvalues ( & tl_draw_data, pColors ) ;

 if ( tl_draw_data.gc != NULL )
 {
    for ( i = 0 ; i < tl_draw_data.previous_num_levels ; ++ i )
    {
       XFreeGC ( dpy , tl_draw_data.gc [ i ] ) ;
    }

    free ( tl_draw_data.gc ) ;
    tl_draw_data.gc = NULL ;
 }

 tl_draw_data.gc = ( GC * ) malloc ( tl_draw_data.num_levels * sizeof ( GC ) );

 if ( tl_draw_data.gc == NULL )
 {
    flogMessage ( stderr , "In routine \"time_lapse_RFCW\":\n"
                       "Unable to allocate memory for \"tl_draw_data.gc\".\n"
                       "Aborting the attempt to start the time lapse.\n" ) ;

    if ( temp != NULL )
    {
       free ( temp ) ;
       temp = NULL ;
    }

    free_time_lapse_memory ( ) ;
    return ;
 }

 if ( loop_data.gc != NULL )
 {
   for ( i = 0 ; i < tl_draw_data.previous_num_levels ; ++ i )
   {
      XFreeGC ( dpy , loop_data.gc [ i ] ) ;
   }

   free ( loop_data.gc ) ;
   loop_data.gc = NULL ;
 }

 loop_data.gc = ( GC * ) malloc ( tl_draw_data.num_levels * sizeof ( GC ) ) ;

 if ( loop_data.gc == NULL )
 {
    flogMessage ( stderr , "In routine \"time_lapse_RFCW\":\n"
                       "Unable to allocate memory for \"loop_data.gc\".\n"
                       "Aborting the attempt to start the time lapse.\n" ) ;

    if ( temp != NULL )
    {
       free ( temp ) ;
       temp = NULL ;
    }

    free_time_lapse_memory ( ) ;
    return ;
 }

 /* At this point, set the previous number of levels to the current
    number of color levels in the rad_data structure. */
 /* tl_draw_data.previous_num_levels = rad_data [ 0 ].num_levels; */
    tl_draw_data.previous_num_levels = tl_draw_data.num_levels;

 for ( i = 0 ; i < tl_draw_data.num_levels ; i++ )
 {
    gcv.foreground = get_pixel_by_name ( wid , color_list_levels [ i ] ) ;
    tl_draw_data.gc [ i ] = XCreateGC ( dpy , DefaultRootWindow ( dpy ) ,
                                        mask , & gcv ) ;
    loop_data.gc [ i ] = XCreateGC ( dpy , DefaultRootWindow ( dpy ) , mask ,
                                     & gcv ) ;
 }

 rad_data[0].num_levels = tl_draw_data.num_levels;

 /* Retrieve the height and width of the main window. This will be
    used to properly dimension the pixmaps needed for time lapsing
    the MPE data. */
 width = _get_map_width ( 0 ) ;
 height = _get_map_height ( 0 ) ;
 tl_draw_data.w = wid ;
 gcv.foreground = get_pixel_by_name ( wid ,
                                      color_list_levels [
                                      tl_draw_data.num_levels-1 ] ) ;
 gc = XCreateGC ( dpy , DefaultRootWindow ( dpy ) , mask , & gcv ) ;

 /*--------------------------------------------------------------*/
 /*     create array of pixmaps to hold time loop                */
 /*--------------------------------------------------------------*/

 /* Check to determine if the pixmaps have been already created.
    If they have, then free them first. */
 for ( i = 0 ; i < data->nhrs ; ++ i )
 {
    if ( loop_data.pixmap [ i ] != 0 )
    {
       XFreePixmap ( dpy , loop_data.pixmap [ i ] ) ;
       loop_data.pixmap [ i ] = 0 ;
    }

    loop_data.pixmap[i] = XCreatePixmap ( dpy , DefaultRootWindow ( dpy ) ,
	                                  width , height ,
                                          DefaultDepthOfScreen (
                                          XtScreen ( wid ) ) ) ;
 }

 loop_data.width = width;
 loop_data.height = height;

 /*--------------------------------------------------------------*/
 /*     create time loop pixmap for current data                 */
 /*--------------------------------------------------------------*/

  /* XCopyArea(dpy,rad_data[0].pixbase,loop_data.pixmap[0],rad_data[0].gc[0],
	    0,0,width,height,0,0);
  XDrawString(dpy,loop_data.pixmap[0],gc,5,height-5,
	      date_st3.ldate,strlen(date_st3.ldate)); */

 /*--------------------------------------------------------------*/
 /*     read saved data, fill data array, and create pixmaps     */
 /*--------------------------------------------------------------*/

 for ( ii = 0 ; ii < NUMHRS ; ++ ii )
 {
   if (strcmp(date_st3.cdate,dates[ii].cdate) == 0) break;
 }

 date_array_index = ii ;

 /* Copy the current date into the loop_data struct. */
    loop_data.dates [ 0 ] = dates [ ii ] ;

 /* Loop over the remaining hours in the time lapse being careful not
    to exceed the bounds of the array. */
 for ( k = 0 ; k < data->nhrs ; k++ )
 {
     index = k + ii ;

     if ( index >= NUMHRS )
     {
        /* The loop extends beyond the range of available datetimes.
           Terminate the loop. */
        flogMessage ( stderr , "\nIn routine 'time_lapse_RFCW':\n"
                           "The time lapse duration extends beyond the\n"
                           "range of allowable datetimes as displayed in the\n"
                           "choose hour window.  Loop request cannot be\n"
                           "completed.\n" ) ;
        /* Indicate that a time out has not been registered yet. */
        manual_loop = 1 ;
        set_time_lapse_flag_on ( ) ;
        ReturnInitialDate = True ;

        /* Free allocate memory and return to initial MPE data datetime
           being observed. */
        end_time_lapse_RFCW ( w , & ReturnInitialDate , NULL ) ;

        return ;
     }

     strcpy ( time , dates[ii+k].ldate ) ;

     /* Copy the time of the current gridded HRAP data field
        being processed into the loop data struct. */
     loop_data.dates [ k ] = dates [ ii + k ] ;

     /*-------------------------------------------------------------*/
     /*     create filename                                         */
     /*     xmrg and satellite precipitation filenames have         */
     /*     different formats than other fields                     */
     /*-------------------------------------------------------------*/

      if ( display_field_type == display_satPrecip )
      {
         /* Not only are the names of satellite precipitation files
            formatted differently from those of xmrg files, but also
            the time stamp reflects a time one hour before the actual
            valid time of the product. */
         iyr = dates [ ii + k ].year ;
         imo = dates [ ii + k ].month ;
         ida = dates [ ii + k ].day ;
         ihr = dates [ ii + k ].hour ;
         im = 0 ;
         is = 0 ;
         tdiff = -1 ;  /* Subtract one time unit. */
         tunit = 2 ; /* The time unit is "hour". */

         TADJ ( & iyr , & imo , & ida , & ihr , & im , & is ,
                & tdiff , & tunit ) ;
         sprintf ( fname , "%s/%4d%02d%02d_%02d00.multi" , dirname , iyr ,
                   imo , ida , ihr ) ;

	 /* Allocate memory for the site_spe array. */
	 site_spe = ( short int * ) malloc ( sizeof ( short int ) *
                                    MAXX * MAXY ) ;

         if ( site_spe == NULL )
         {
            flogMessage ( stderr , "\nIn routine 'time_lapse_RFCW':\n"
                               "Could not allocate memory for the\n"
                               "site_spe array.\n" ) ;

            /* Indicate that a time out has not been registered yet. */
            manual_loop = 1 ;
            set_time_lapse_flag_on ( ) ;
            ReturnInitialDate = True ;

            /* Free allocate memory and return to initial MPE data datetime
               being observed. */
            end_time_lapse_RFCW ( w , & ReturnInitialDate , NULL ) ;
            return ;
         }

         /* Perform separate logic to read in Satellite Data. */
         ReadSPE ( fname , & XOR , & YOR , & MAXX , & MAXY , site_spe ,
                   & x_factor , & u_thres , & mm_factor , & ifile ) ;

         if ( ifile == 0 )
         {
            for ( i = 0 ; i < MAXY ; i ++ )
            {
               spe_index = MAXX * i ;

               for ( j = 0 ; j < MAXX ; j ++ )
               {
                  * ( * ( tl_draw_data.data_array + j ) + i ) =
                          site_spe [ j + spe_index ] ;
               }
            }
         }

         if ( site_spe != NULL )
         {
            free ( site_spe ) ;
            site_spe = NULL ;
         }
      }
      else
      {
         if ( strcmp ( cv_use , "XMRG" ) == 0 )
         {
            if ( strcmp ( date_form , "mdY" ) == 0 )
            {
               idate = dates[ii+k].month * 1000000 + dates[ii+k].day*10000 +
                       dates[ii+k].year ;
               sprintf(cdate,"%08d%02d",idate,dates[ii+k].hour);
            }
            else
            {
               strcpy(cdate,dates[ii+k].cdate);
            }

            sprintf(fname,"%s/xmrg%sz",dirname,cdate);
         }
         else if ( strcmp ( cv_use, "RFCMOSAIC" ) == 0 )
         {
            sprintf(fname, "%s/%s01%sz",dirname,cv_use,dates[ii+k].cdate);
         }
         else
         {
            sprintf(fname,"%s/%s%sz",dirname,cv_use,dates[ii+k].cdate);
         }

         len_fname = strlen(fname);

         /* Test whether or not the bytes need to be "swapped" in the
            file to be read in to match the memory architecture of the
            operating system that this program is running on. */
         TestXmrgByteOrder_ ( fname , & XOR , & result ) ;

         if ( result == FlipTestFailed )
         {
            flogMessage ( stderr , "In routine \"time_lapse_RFCW\":\n"
                               "The call to \"TestXmrgByteOrder_\" failed.\n"
                               "Cannot read file \"%s\".\n" , fname ) ;
            ifile = 1 ;
         }
         else
         {

           /*------------------------------------------*/
           /*     read data from file                  */
           /*------------------------------------------*/

            for ( i = 0 ; i < MAXY ; ++ i )
            {
               read_xmrg ( & MAXX , & MAXY , & i , fname , & len_fname , & ifile ,
                        temp ) ;

               if ( ifile != 0 ) break ;

               if ( result == FlipBytes )
               {
                  Swap2Bytes_ ( temp , ( size_t * ) & MAXX ) ;
               }

               for ( j = 0 ; j < MAXX ; ++ j )
               {
                  * ( * ( tl_draw_data.data_array + j ) + i ) = * ( temp + j ) ;
               }

            }
         }
      }

      /*------------------------------------------*/
      /*     fill array                           */
      /*------------------------------------------*/

      if ( ifile != 0 )
      {
        for ( i = 0 ; i < MAXX ; i++ )
        {
           for ( j = 0 ; j < MAXY ; j++ )
           {
             * ( * ( tl_draw_data.data_array + i ) + j )  = -999;
           }
        }
      }

    /*------------------------------------------*/
    /*     Create the pixmap and store it in the*/
    /*     loop data structure.                 */
    /*------------------------------------------*/
      setDrawStruct ( & tl_draw_data ) ;

      /* Check for a user-supplied pixmap. This pixmap may have
         topography data on it. */
      if ( pOverlay->status == M_ON )
      {
         user_pixmap = get_topo_pixmap ( );
         fill_pixmap ( user_pixmap ) ;
      }
      else
      {
         fill_pixmap ( 0 );
      }

      XCopyArea(dpy,tl_draw_data.pixbase,loop_data.pixmap[k],
	     tl_draw_data.gc[0], 0,0,width,height,0,0);
      XDrawString(dpy,loop_data.pixmap[k],gc,5,height-5, time,strlen(time));

      /* Free the pixbase from the tl_draw_data structure. */
      if ( tl_draw_data.pixbase != 0 )
      {
         XFreePixmap ( dpy , tl_draw_data.pixbase ) ;
         tl_draw_data.pixbase = 0 ;
      }
 }

 XFreeGC ( dpy , gc ) ;

 /* Register the KeyPressEvent handler that will check to determine
    if the user wants to control the time lapse using the four "arrow"
    keys. */
 XtAddEventHandler ( wid , KeyPressMask , FALSE , manual_time_lapse ,
                     NULL ) ;

 turnOnMpeData ( ) ;

 /* Turn off the Mpe Legend. */
 _turn_legend_off ( ) ;

 /* Indicate to the rest of the world that the time lapse is now "on". */
 set_time_lapse_flag_on ( ) ;

 /* Register a time out to drive the loop. */
 app = _get_map_context ( ) ;
 duration = get_loop_callback_duration ( ) ;
 xtid = XtAppAddTimeOut ( app , duration ,
                          loop_callback , ( XtPointer ) & loop_data ) ;
 set_loop_callback_interval_id ( xtid ) ;

 /* Initialize the key mappings. */
 for(i=0;i<XtNumber(keysym1);i++)
 {
    keycode[i] = XKeysymToKeycode(dpy,keysym1[i]);
 }

 /*--------------------------------------------------------------*/
 /*     reset cursor                                             */
 /*--------------------------------------------------------------*/
 mSetCursor ( M_NORMAL ) ;

 if ( temp != NULL )
 {
    free ( temp ) ;
    temp = NULL ;
 }

 if ( widget_struct->stoptime_widget != NULL )
 {
    Sensitize ( widget_struct->stoptime_widget ) ;
    DeSensitize ( widget_struct->timelapse6_widget ) ;
    DeSensitize ( widget_struct->timelapse12_widget ) ;
    DeSensitize ( widget_struct->timelapse24_widget ) ;
    DeSensitize ( widget_struct->timelapseother_widget ) ;
 }
}

/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   free_time_lapse_memory
* PURPOSE:       Deallocates the memory used by the tl_draw_data.data_array,
*                tl_draw_data.gc.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*    Only system utilities are utilized by this routine.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   int        i                            Loop indexing variable.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
void free_time_lapse_memory ( )
{
   Display              * dpy = NULL ;
   int i ;

   dpy = _get_map_display ( ) ;

   /* Free the time lapse data array. */
   if ( tl_draw_data.data_array != NULL )
   {
      for ( i = 0 ; ( tl_draw_data.data_array [ i ] != NULL )
                    && ( i < tl_draw_data.maximum_columns ) ; ++ i )
      {
         free ( tl_draw_data.data_array [ i ] ) ;
         tl_draw_data.data_array [ i ] = NULL ;
      }

      free ( tl_draw_data.data_array ) ;
      tl_draw_data.data_array = NULL ;
   }

   if ( tl_draw_data.gc != NULL )
   {
      for ( i = 0 ; i < tl_draw_data.previous_num_levels ; ++ i )
      {
         XFreeGC ( dpy , tl_draw_data.gc [ i ] ) ;
      }

      free ( tl_draw_data.gc ) ;
      tl_draw_data.gc = NULL ;
   }

   if ( loop_data.gc != NULL )
   {
      for ( i = 0 ; i < tl_draw_data.previous_num_levels ; ++ i )
      {
         XFreeGC ( dpy , loop_data.gc [ i ] ) ;
      }

      free ( loop_data.gc ) ;
      loop_data.gc = NULL ;
   }

}

/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   set_time_lapse_flag_on
* PURPOSE:       Sets the time lapse flag to the "on" state.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*    void
*
* APIs UTILIZED:
*    None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*    None ("time_lapse_flag" is global to this file)
*
* DATA FILES AND/OR DATABASE:
*    None
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/
void set_time_lapse_flag_on ( )
{
   time_lapse_flag = 1 ;
}

/*******************************************************************************
* MODULE NUMBER: 5
* MODULE NAME:   set_time_lapse_flag_off ( )
* PURPOSE:       Toggles the time lapse flag to "off".  This flag is used only
*                as an indicator of the states of the time lapse, i.e., is it
*                or is it not running.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*    void
*
* APIs UTILIZED:
*    None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*    None ( "time_lapse_flag" is global to this file)
*
* DATA FILES AND/OR DATABASE:
*    None
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/
void set_time_lapse_flag_off ( )
{
   time_lapse_flag = 0 ;
}

/*******************************************************************************
* MODULE NUMBER: 6
* MODULE NAME:   get_time_lapse_flag ( )
* PURPOSE:       Retrieves the state ( on or off ) of the time lapse flag.
*                A time lapse state of "on" means that the time lapse is
*                running.  A time lapse state of "off" means that the time
*                lapse is not running or looping.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   int
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None ("time_lapse_flag" is global in scope to this file)
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
int get_time_lapse_flag ( )
{
   return time_lapse_flag ;
}

/*******************************************************************************
* MODULE NUMBER: 7
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
void end_time_lapse_RFCW ( Widget w , XtPointer clientdata ,
                           XtPointer calldata )
{
   Boolean continue_to_dispatch = True ;
   Boolean  * pResetToInitialDate = ( Boolean * ) clientdata ;
   int time_lapse_state ;

   /* Check to make sure that the time lapse is currently "alive". */
   time_lapse_state = get_time_lapse_flag ( ) ;

   if ( time_lapse_state == 1 )
   {
      end_time_lapse ( w , ( XtPointer ) pResetToInitialDate , NULL ,
                       & continue_to_dispatch ) ;

      /* Reactivate the watch cursor that is normally displayed when
         the map screen is redrawn. */
      mEnableExposeWatch ( ) ;
   }
}

/*******************************************************************************
* MODULE NUMBER: 8
* MODULE NAME:   end_time_lapse
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
void end_time_lapse ( Widget w , XtPointer clientdata , XEvent * event ,
                      Boolean * continue_to_dispatch_return )
{
   Boolean * pResetToInitialTime = ( Boolean * ) clientdata ;
   int i ;
   XtIntervalId id = 0 ;
   ControlMenuItemInfo new_hour_info = OkHourPopup ;
   DataSaved = TRUE ;

   /*--------------------------------------------------------------*/
   /*     remove the "time out" controlling the loop.              */
   /*--------------------------------------------------------------*/

   if ( manual_loop == 0 )
   {
      id = get_loop_callback_interval_id ( ) ;
      XtRemoveTimeOut ( id ) ;
   }
   else
   {
      manual_loop = 0 ;
   }

   /*--------------------------------------------------------------*/
   /*     remove the "key press" event handler controlling the     */
   /*     loop.                                                    */
   /*--------------------------------------------------------------*/
   XtRemoveEventHandler ( wid , KeyPressMask , FALSE ,
                          manual_time_lapse , NULL ) ;

   /*----------------------------------------------------------------*/
   /*  free space ... Note the tl_draw_data.data_array is reused     */
   /*  and only deallocated when this application shuts down.        */
   /*  It is a good idea to free the pixmaps everytime, though,      */
   /*  because they use so many resources.                           */
   /*----------------------------------------------------------------*/
   for ( i = 0 ; i < loop_data.max ; i++)
   {
      XFreePixmap ( XtDisplay ( w ) , loop_data.pixmap [ i ] ) ;
      loop_data.pixmap [ i ] = 0 ;
   }

   /*--------------------------------------------------------------*/
   /*     create expose event to display current data              */
   /*--------------------------------------------------------------*/
   unsetPixmapToDraw ( ) ;
   set_time_lapse_flag_off ( ) ;

   if ( widget_struct->timelapse6_widget != NULL )
   {
      Sensitize ( widget_struct->timelapse6_widget ) ;
      Sensitize ( widget_struct->timelapse12_widget ) ;
      Sensitize ( widget_struct->timelapse24_widget ) ;
      Sensitize ( widget_struct->timelapseother_widget ) ;
      DeSensitize ( widget_struct->stoptime_widget ) ;
   }

   /* Update the time information to reflect the point
      where the loop was stopped. This information depends upon
      how the time lapse was halted.  If it was stopped
      using a zoom feature, then stop the time lapse loop on the
      frame the user is trying to zoom into.  If the time lapse loop
      was stopped using the "End Time Lapse" option on the time
      lapse menu, then reset the time to the frame the user was on
      before starting the time lapse. */
   if ( ( pResetToInitialTime != NULL ) &&
        ( * pResetToInitialTime == True ) )
   {
      date_st3 = dates [ date_array_index ] ;
   }
   else
   {
      date_st3 = dates [ date_array_index + loop_data.icnt ] ;
   }

   sprintf ( datetime , "%04d-%02d-%02d %02d:00:00" , date_st3.year , date_st3.month ,
             date_st3.day , date_st3.hour ) ;

   newhour_RFCW ( w , ( XtPointer ) new_hour_info , NULL ) ;
   _turn_legend_on ( ) ;

}

int get_date_array_index ( )
{
   return date_array_index;
}

void set_date_array_index ( int date_index )
{
   date_array_index = date_index;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/time_lapse_RFCW.c,v $";
 static char rcs_id2[] = "$Id: time_lapse_RFCW.c,v 1.23 2007/06/18 20:24:42 whfs Exp $";}
/*  ===================================================  */

}

