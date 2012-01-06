/*******************************************************************************
* FILENAME:            initMpeControl.c   
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          initMpeControl
* DESCRIPTION:         This routine performs the essential function of 
*                      initializing the Mpe-specific variables.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       February 8, 2002
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP-UX / Dell-Linux
* MODIFICATION HISTORY:
*   MODULE #  DATE             PROGRAMMER        DESCRIPTION/REASON
*          1  February 8, 2002 Bryon Lawrence    Original Coding 
*          1  March   19, 2002 Bryon Lawrence    Added initialization of
*                                                key tl_draw_data struct
*                                                elements.
*          1  March    8, 2004 Bryon Lawrence    Added the mlmosaic_widget
*                                                for the new local bias 
*                                                multisensor mosaic field.
********************************************************************************
*/

#include <errno.h>
#include <string.h>
#include <Xm/Xm.h>

#include "display_field.h"
#include "map_convert.h"
#include "drawa.h"
#include "find_dates.h"
#include "gage_table_RFCW.h"
#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "GeneralUtil.h"
#include "HydroStatus.h"
#include "initMpeControl.h"
#include "map_convert.h"
#include "map_menubar.h"
#include "map_menubar_cb.h"
#include "mpe_log_utils.h"
#include "mpe_topo.h"
#include "post_functions.h"
#include "read_precip_data.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "time_defs.h"
#include "Xtools.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   initMpeControl
* PURPOSE:       This routine performs the essential function of initializing
*                the Mpe-specific variables.  This includes:
*                   Initializing the user-id used in the Mpe error and
*                      information messages
*                   Initializing the duration of the radar precipitation
*                      estimation
*                   Initializing the radar sites that provide coverage for the
*                      RFC area
*                   Setting the color overlays (this functionality will 
*                      probably go away)
*                   Initializing the user preferences (if any are provided)
*                   Reading the memory span values from the RWBiasStat table
*                     in the Hydro Database
*                   Reading the xmrg filename date format from .Apps_defaults 
*
* ARGUMENTS:
* TYPE   DATA TYPE   NAME             DESCRIPTION/UNITS
* Input  Widget      w                The widget representing the toplevel
*                                     of the application.      
* Input  GC *        pGraphicsContext A pointer to the graphics context of the
*                                     main application.
*
*
* RETURNS:
*   DATA TYPE         NAME     DESCRIPTION
*   HydroStatus       status   The enumeration of values for this
*                              status-communicating variable is located
*                              in HydroStatus.h
*
* APIs UTILIZED:
*   NAME                HEADER FILE           DESCRIPTION
*   get_dates_RFCW      find_dates.h          Retrieves the dates for the 
*                                             precipitation products.
*   ReadParameters_RFCW read_precip_data.h    Sets the default MPE
*                                             parameters.
*   read_radarloc       read_radarloc.h       Processes and stores the
*                                             coordinates and names for
*                                             all radars that provide
*                                             coverage over the RFC area.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                   DESCRIPTION
*   char *     pDefaultDisplay        Is initialized to the type of radar
*                                     precipitation field the application is
*                                     to first start with.
*   char *     pResultFromGetEnv      A pointer to the result obtained
*                                     from getting the value of the
*                                     environmental value LOGNAME
*                                     using the "getenv" stdlib routine.
*   char *     pRfcToken              Contains the name of the token containing
*                                     the name of the RFC this application
*                                     is being run at.
*   HydroStatus status                The return value from the call to
*                                     "read_geo_data".
*   int        rfc_length             The length of the string represented
*                                     by the rfc token being retrieved.
*   int        status                 Contains the return status of an
*                                     individual function call from this
*                                     application.
*   int        string_length          The length of the token being 
*                                     passed into the get_apps_defaults
*                                     routine.
*
* DATA FILES AND/OR DATABASE:
* In order for this routine to function correctly, a connection with the
* Hydro Database must be established before it is called.  It is up to the
* caller of this routine to take care of closing the database.
*
* ERROR HANDLING:
*  ERROR CODE                    DESCRIPTION
*  HydroStatus_MissingEnv        An environmental variable is undefined.
*  HydroStatus_NoData            A token does not exist or has no information.
*  HydroStatus_StringOverflow    A string is too large to fit into an array.
*  HydroStatus_OK                This routine functioned without detectable
*                                errors.
*
* GLOBAL VARIABLE / MARCO DEFINITIONS
*   NAME                  RESIDENCE FILE       DESCRIPTION
*   cv_duration           This file            The default duration of
*                                              a radar precipitation grid.
*   dbg                   This file            A flag indicating if debug
*                                              output is desired. A value of
*                                              0 = no, 1 = yes.
*   draw_poly_flag	  		       A flag indicating if menu item
*					       MPEcontrol->DrawPolygon was chosen
*						(a value of 1 = yes),
*					       or click occurs on GUI Hydromap
*						(a value of no = 0).
*   add_pseudo_flag	  		       A flag indicating if menu item
*					       MPEcontrol->AddPseudoGage was 
*					       chosen
*						(a value of 1 = yes),
*					       or click occurs on GUI Hydromap
*						(a value of no = 0).
*   display_7x7_flag	  		       A flag indicating if menu item
*					       MPEfields->Display7x7 was chosen
*						(a value of 1 = yes),
*					       or click occurs on GUI Hydromap
*						(a value of no = 0).
*   LOGNAME               This file            The user-id of the operator
*                                              running the hmap_mpe 
*                                              application.
*                                              processed by the map library.
*   MAX_MPE_STRING_LEN    rfcwide.h            Generic macro specifying the
*                                              the maximum length of a 
*                                              string.
*   MAX_RFC_LEN           rfcwide.h            The largest possible rfc-id
*                                              length.
*   MAX_USER_ID_LEN       rfcwide.h            The largest possible user-id
*                                              length.
*   NRADARS               This file            The number of radar sites
*                                              applicable to the RFC being
*                                              processed.
*   popupHelp_shell       This file            The widget representing the
*                                              Help Dialog Shell.
*   RFC                   read_geo_data        The name of the RFC this
*                                              application is being run at. 
*   SECONDS_PER_HOUR      time_defs.h          The number of seconds per hour.
*   tophelp               This file            The widget representing the
*                                              toplevel of the help dialog
*                                              shell.
*   toplevel              This file            The widget representing the
*                                              toplevel of the entire 
*                                              application. 
*
********************************************************************************
*/

char LOGNAME [ MAX_USER_ID_LEN ] ;
extern char RFC [ ] ;
int add_pseudo_flag = 0 ;
int display_7x7_flag = 0 ;
int display_gage_table = 0 ;
int cv_duration = 0 ;
int dbg = 0 ;
int draw_poly_flag = 0;
int NRADARS = 0 ;
extern struct _Overlay overlays [ ];
extern time_lapse_struct tldata;
extern Widget map_menu_items [ ];
Widget toplevel = NULL ;

/* DailyQC display variables. */
int flf_on = -1;
int qpf_on = -1;
int maxmin_on = -1;

HydroStatus initMpeControl ( Widget w , GC * pGraphicsContext )
{
   char mpe_dqc_num_days_val [ 20 ];
   char * pBadChar = NULL;
   const char * pResultFromGetEnv = NULL ;
   static char * pRfcToken = "st3_rfc" ;
   static const char * mpe_dqc_num_days_tok = "mpe_dqc_num_days";
   extern char temp_num_of_days [];
   int i;
   int num_dqc_days;
   int rfc_length ;
   extern rubber_band_data rbdata ;
   int reply_length;
   int status ;
   int string_length ;
   const struct topo * pTopo = NULL;
  
   /* Set the polygon drawing pointers to null. */
   draw_precip_values = NULL ;

   /* Initialize the gage structure pointer (defined in stage3.h)
      to NULL. */
   gage = NULL ;
   ngages = 0 ;

   /* Define the toplevel widget. */
   toplevel = w ;

   /* Initialize the data array corresponding to the maps. */
   for ( i = 0; i < NUM_MAP_SCREENS; ++ i )
   {
      rad_data[i].w = w ;
      rad_data[i].gc = NULL ;
      rad_data[i].levels = NULL ;
      rad_data[i].first_display = 1;
      rad_data[i].previous_num_levels = -1;
   }

   display = XtDisplay ( toplevel ) ;

   /* Initialize pointer members of the cb_data structure. */
   init_gage_table_memory ( ) ;

   /* Initialize the num of days to look back for DailyQC. */
   string_length = strlen ( mpe_dqc_num_days_tok ); 
   get_apps_defaults ( (char * ) mpe_dqc_num_days_tok,
                       & string_length,
                       mpe_dqc_num_days_val,
                       & reply_length );                    

   if ( reply_length == 0 )
   {
      flogMessage ( stdout, "Token mpe_dqc_num_days not defined. "
                        "Using %d days.\n",  MAX_GAGEQC_DAYS ); 
      sprintf ( temp_num_of_days, "%d",  MAX_GAGEQC_DAYS );
   }
   else
   {
      /* Check to make sure that the token-specified value is
         less than or equal to MAX_GAGEQC_DAYS and greater than 
         0. */
     errno = 0;
     num_dqc_days = strtol ( mpe_dqc_num_days_val, & pBadChar, 10 );

     if ( errno != 0 )
     {
        flogMessage ( stdout, "Could not convert the value of token "
                          "mpe_dqc_num_days, %s, to an integer.\n"
                          "Using %d as the default number of days "
                          "to perform DailyQC operations for.\n",
                          mpe_dqc_num_days_val, MAX_GAGEQC_DAYS );
        sprintf ( temp_num_of_days, "%d", MAX_GAGEQC_DAYS );
     }
     else
     {
        if ( num_dqc_days > MAX_GAGEQC_DAYS || num_dqc_days < 1 )
        {
           flogMessage ( stdout, "The value of token mpe_dqc_num_days, %s, "
                             "is not within the range of allowable\n"
                             "DailyQC days,1-%d.  Setting to %d days.\n",
                             mpe_dqc_num_days_val, MAX_GAGEQC_DAYS,
                             MAX_GAGEQC_DAYS ); 
           sprintf ( temp_num_of_days, "%d", MAX_GAGEQC_DAYS );
 
        }
        else
        {
           sprintf ( temp_num_of_days, "%d", num_dqc_days );
        }
     }
   }

   /* Initialize the topography data. */
   pTopo = get_topo_coord ( );

   if ( pTopo == NULL )
   {
      /* Desensitize the topograhy options on the Overlays Menu. */
      DeSensitize ( map_menu_items [ map_overlay_topography ] ); 

      /* Force the topography overlays to "OFF" */
      overlays [ M_TOPOGRAPHY ].status = M_OFF;
      overlays [ M_TOPOGRAPHY ].status = M_OFF;
      
      /* Log a message stating that the topography file does not exist. */
     logMessage ( "Could not process the topography information.\n"
               "Check to make sure the topography file exists.\n" );
   }

   /* Initialize crucial elements of the time lapse drawing structure. */
   tldata.w = w ;
   tl_draw_data.gc = NULL ;
   tl_draw_data.levels = NULL ;
   tl_draw_data.data_array = NULL ;
   tl_draw_data.previous_num_levels = -1;

   /* Initialize crucial elements of the edit data drawing structure. */
   eddata.levels = NULL ;

   /* Define the widget and graphics context of the zoom rubber band. */
   rbdata.zoom_state = False ;
   rbdata.use_rectangle = False ; 
   rbdata.w = w ;
   rbdata.gc = xs_create_xor_gc ( w ) ;
   rbdata.rubber_band_zoom_mode = False ;

   /* Initialize the LOGNAME array. This array contains the user's
      logon name. */
   memset ( LOGNAME , '\0' , MAX_USER_ID_LEN ) ;

   /* Retrieve the user-id of the operator launching this application. */
   pResultFromGetEnv = getenv ( "LOGNAME" ) ;

   /* Check to make sure that the string read in from getenv is defined 
      and will not overflow the LOGNAME char array. */
   if ( pResultFromGetEnv == NULL )
   {
      flogMessage ( stderr , "In routine \"initMpeControl\":\n"
                         "The attempt to read the user-id from the\n"
                         "\"LOGNAME\" environmental variable failed.\n"
                         "The construction of the \"hmap_mpe\" application\n"
                         "is being aborted.\n" ) ;
      return HydroStatus_MissingEnv ;
   }

   string_length = strlen ( pResultFromGetEnv ) ; 

   if ( string_length >= MAX_USER_ID_LEN )
   {
      flogMessage ( stderr , "In routine \"initMpeControl\":\n"
                         "The user-id retrieved from the environmental\n"
                         "variable \"LOGNAME\", %s, is too large for the\n"
                         "LOGNAME global array.  It is %d characters long\n"
                         "and must be no more than %d characters long.\n"
                         "The construction of the \"hmap_mpe\" application\n"
                         "is being aborted.\n" , pResultFromGetEnv , 
                         string_length , ( MAX_USER_ID_LEN  - 1 ) ) ; 
      return HydroStatus_StringOverflow ;
   }

   strcpy ( LOGNAME , pResultFromGetEnv ) ;

   /* Initialize the default duration of a radar rainfall grid in
      seconds. */
   cv_duration = SECONDS_PER_HOUR ;

   /* Read radar site information from the Informix radarloc table. */
   read_radarloc ( & NRADARS ) ;

   if ( NRADARS == 0 )
   {
      flogMessage ( stderr , "In routine \"initMpeControl\":\n"
                         "No radars where retrieved from the RadarLoc\n"
                         "table by the \"read_radarloc\" routine.  The\n"
                         "construction of the \"hmap_mpe\" gui is being\n"
                         "aborted.\n" ) ;
      return HydroStatus_NoData ;
   }

   /* Read the default mpe parameters and the RWPrefs table. */
   ReadParameters_RFCW ( ) ;

   /* Read the radar coverage maps. */
   read_radcov_grids ( ) ;

   /* Desensitize the Mpe Control Menu Items that are not applicable
      upon initially starting up the Hmap_mpe application. */
   if ( widget_struct->next_widget != NULL ) 
   {
      /* Desensitize items on the MPEcontrol menu. */
      DeSensitize ( widget_struct->next_widget ) ;
      DeSensitize ( widget_struct->prev_widget ) ;
      DeSensitize ( savemaintop_widget ) ;
      DeSensitize ( savemainbottom_widget ) ;
      DeSensitize ( widget_struct->rerun_widget ) ;
      DeSensitize ( widget_struct->transmit_rfc_qpe ) ;
      DeSensitize ( widget_struct->transmit_rfc_bias );

      /* DeSensitize items on the Tools menu. */
      DeSensitize ( widget_struct->clear_widget ) ;
      DeSensitize ( fullscreen_widget );
      DeSensitize ( splitscreen_widget );
     
      /* DeSensitize items on the Polygons menu. */
      DeSensitize ( drawpoly_widget ) ; 
      DeSensitize ( deletepoly_widget ) ;

      /* Desensitize the "Precip Fields" menu items.  These are only applicable
         after the user has chosen a hour to view MPE data for. */
      DeSensitize ( rmosaic_widget ) ;
      DeSensitize ( avgrmosaic_widget );
      DeSensitize ( maxrmosaic_widget );
      DeSensitize ( bmosaic_widget ) ;
      DeSensitize ( bias_widget ) ;
      DeSensitize ( mmosaic_widget ) ;
      DeSensitize ( mlmosaic_widget ) ;
      DeSensitize ( p3lmosaic_widget ) ;
      DeSensitize ( gageonly_widget ) ;
      DeSensitize ( xmrg_widget ) ;
      DeSensitize ( multihour_widget ) ;
      DeSensitize ( satprecip_widget ) ;
      DeSensitize ( lsatprecip_widget ) ;
      DeSensitize ( srmosaic_widget ) ;
      DeSensitize ( sgmosaic_widget ) ;
      DeSensitize ( srgmosaic_widget ) ;
      DeSensitize ( rfcbmosaic_widget );
      DeSensitize ( rfcmmosaic_widget );
      DeSensitize ( widget_struct->rfc_qpe_mosaic );

      /* Desensitize items under the Base Fields menu. */
      DeSensitize ( locspan_widget ) ;
      DeSensitize ( locbias_widget );
      DeSensitize ( height_widget ) ;
      DeSensitize ( index_widget ) ;
      DeSensitize ( widget_struct->gage_triangles );
      
      /* Desensitize the items on the Gage menu. */
      DeSensitize ( widget_struct->qc_precipitation );
      DeSensitize ( widget_struct->qc_temperature );
      DeSensitize ( widget_struct->qc_freezing );
      DeSensitize ( widget_struct->save_level2_data );
      DeSensitize ( widget_struct->pseudo_widget );
      DeSensitize ( widget_struct->gage_table_widget );
      DeSensitize ( widget_struct->single_gage_widget ) ;      
      DeSensitize ( showids_widget );
      DeSensitize ( showval_widget );
      DeSensitize ( widget_struct->gage_missing_menu );
      DeSensitize ( widget_struct->gage_color_menu ); 
      
      /* Desensitize the items on the Climo menu. */
      DeSensitize ( prism_widget ) ;
      DeSensitize ( widget_struct->monthly_max_temp );
      DeSensitize ( widget_struct->monthly_min_temp );
      
      /* Desensitize the items on the Misc menu. */
      DeSensitize ( widget_struct->display_bias_widget ) ;
      DeSensitize ( widget_struct->radar_site_widget ) ;
      DeSensitize ( timelapse_widget ) ;
      DeSensitize ( widget_struct->timelapse6_widget ) ;
      DeSensitize ( widget_struct->timelapse12_widget ) ;
      DeSensitize ( widget_struct->timelapse24_widget ) ;
      DeSensitize ( widget_struct->timelapseother_widget ) ;
      DeSensitize ( widget_struct->stoptime_widget ) ;
      DeSensitize ( multihour_widget );
   }

   /* Retrieve the geo data.  Note that the read_geo_data routine has been
      modified to only read in the maximum coordinates MAXX and MAXY and
      XOR and YOR. */
   status = ( int ) get_geo_data ( ) ;


   if ( status == ( int ) HydroStatus_NoCoordFile )
   {
      flogMessage ( stderr , "\nIn routine \"initMpeControl\":\n"
                         "The call to \"read_geo_data\" has failed.\n"
                         "Aborting the attempt to read in the MPE data.\n"
                         "MPE specific options will not be available in the\n"
                         "main application.\n" ) ;
      return ( HydroStatus )status ;
                         
   }
   else if ( status != ( int ) HydroStatus_OK )
   {
      flogMessage ( stderr , "In routine \"initMpeControl\":\n"
                         "The call to \"read_geo_data\" has failed.\n"
                         "Aborting the launch of the Hmap/Mpe program.\n" ) ;
      return status ;
   }
   /* Initialize the default display type. Allocate the xmrgfile_row_array. */
   initialize_data_RFCW ( ) ;

   /* Set the Hrap coordinates required by the Hrap projection of the
      map library. */
   mSetHrapArea ( XOR , YOR , MAXX , MAXY ) ;

   /* Retrieve the available precipitation product dates. */ 
   dates = NULL ;
   dstring = NULL ;

   /* Set the RFC identifier. */
   string_length = strlen ( pRfcToken ) ;

   status = get_apps_defaults ( pRfcToken , & string_length , RFC ,
                                & rfc_length ) ;

   if ( status != 0 )
   {
      flogMessage ( stderr , "In routine \"initMpeControl\":\n"
                         "Couldn't retrieve the RFC identifier using\n"
                         "apps defaults token %s.  The contstruction of\n"
                         "the \"hmap_mpe\" gui is being aborted.\n" ,
                         pRfcToken ) ;
      return HydroStatus_NoData ;
   }

   /* Indicate to the Mpe radar-precipitation drawing routines that
      that the display is new and needs all of its associated dynamic
      structures allocated and defined. */ 
   first_display = TRUE ;

   create_time_lapse_popup_RFCW ( widget_struct ) ;

   return HydroStatus_OK ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/initMpeControl.c,v $";
 static char rcs_id2[] = "$Id: initMpeControl.c,v 1.22 2007/10/18 18:07:21 lawrence Exp $";}
/*  ===================================================  */

}
