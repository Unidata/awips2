
/*******************************************************************************
* FILENAME:          ffgcontrol_show.c
* NUMBER OF MODULES:   16
* GENERAL INFORMATION:
*   MODULE 1:   ffg_select_display
* DESCRIPTION:  Display the  netCDF file the user has selected. 
*   MODULE 2:   ffg_clear_display
* DESCRIPTION:  Clears the main viewing area of FFG data.
*   MODULE 3:   load_ffgProductList 
* DESCRIPTION:  Loads rows of FFG file info into the list widget of the
*               FFG file selection GUI.
*   MODULE 4:   build_ffg_gridded_list
* DESCRIPTION:  This routine builds the linked list of netCDF data.
*   MODULE 5:   build_areal_gridded_list
* DESCRIPTION:  This routine builds the linked list of ContingencyValue
*               information.
*   MODULE 5:   read_gridded_ffg_product
* DESCRIPTION:  Reads all of the available netCDF files, stores their
*               information in a linked list, and displays this information
*               into the selection box on the FFG data selection gui.
*   MODULE 6:   read_areal_ffg_product
* DESCRIPTION:  Reads all of the available FFG products from the contingency
*               values table in the IHFS database and displays this information
*               into the selection box on the FFG data selection gui.
*   MODULE 7:   read_ffg_product
* DESCRIPTION:  Based on the values of the FFG mode radio buttons, this routine
*               calls "read_gridded_ffg_product" when in "gridded FFG" mode and
*               "read_areal_ffg_product" when in "areal FFG" mode.
*   MODULE 6:   configure_ffg_gui
* DESCRIPTION:  Sets up the filter options on the FFG selection GUI.
*   MODULE 7:   free_ffg_list
* DESCRIPTION:  Frees the memory used by the linked list of FFG netCDF file
*               information.
*   MODULE 8:   ffg_close_display 
* DESCRIPTION:  The callback for the "Close" button on the FFG selection GUI.
*   MODULE 9:   setup_ffg_source
* DESCRIPTION:  Reconfigures the FFG selection gui bases on the data area
*               being read.
*   MODULE 10:   set_ffg_id_filter
* DESCRIPTION:   Callback for when a different id filter option is chosen.
*   MODULE 11:   set_ffg_duration_filter 
* DESCRIPTION:   Callback for when a different duration filter option is
*                chosen.
*   MODULE 12:   set_ffg_areal_filter
* DESCRIPTION:   Callback for when a different display area option is chosen.
*   MODULE 13:   process_double_click 
* DESCRIPTION:   Determines when a two mouse clicks are a double click or
*                just two mouse clicks.
*   MODULE 14:   process_clicks
* DESCRIPTION:   Keeps track of the mouse clicks ... Listens for double 
*                mouse clicks.
*   MODULE 15:   ffg_dc_AddCallbacks 
* DESCRIPTION:   Sets up the callbacks for the various widgets on the
*                FFG data selection gui.
*   MODULE 16:   ffg_display_show
* DESCRIPTION:   Builds the FFG data selection gui.
*
* ORIGINAL AUTHORS:  Bryon Lawrence / Moria Shebsovich 
* CREATION DATE:    May 8, 2002
* ORGANIZATION:     HSEB / OHD 
* MACHINE:          HP-UX / Redhat Dell Linux
*
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*   1-15            5/8/2002     Bryon Lawrence    Original Coding
********************************************************************************
*/

#include <dirent.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/ScrolledW.h>

#include "ArealDisplayControl.h"
#include "ArealDataAttr.h"
#include "ColorBar.h"
#include "ColorThreshold.h"
#include "ffgcontrol.h"
#include "ffgcontrol_show.h"
#include "FfmUtils.h"
#include "GeneralUtil.h"
#include "hv_color_threshold.h"
#include "hv_mainCallbacks.h"
#include "List.h"
#include "map_defines.h"
#include "map_library.h"
#include "map_resource.h"
#include "read_netcdf_ffg.h"
#include "Xtools.h"

/* The names of the toggle buttons for the sake of the callbacks. */
#define AREAL_VAL_TOGGLE_BUTTON 0
#define AREAL_ID_TOGGLE_BUTTON 1

static Direct * listPtr = NULL ;  /* Pointer to the linked list of netCDF file
                                     information. */
static int click_count = 0 ; /* The number of mouse clicks in a given
                                duration of time.  Used to detect double
                                clicks. */
static int first = 1 ; /* Indicates whether or not to start a new linked list
                          of netCDF file information. */

/* Variables crucial to filter and displaying the available ffg netCDF
   files. */
static enum FfgMode ffg_mode = GRID_MODE ; /* Initial ffg mode (can be grid
                                              or areal). */
/* Initial setting for how gridded data is displayed.  It may
   either be displayed as a grid or a basin. */
static enum FfgArealTypes gridded_ffg_display = FFG_GRID ;

static enum FfgGridArea ffg_source = WFO_FFG ;  /* Initial FFG data area. */
static enum FfgRfcIds ffg_id_filter = ALL_RFCS ; /* Default FFG id filter. */

/* Initial RFC area FFG data duration filter. */
static enum FfgDurationIds gridded_ffg_dur_filter = ALL_FFG_DURATIONS ; 

/* Initial WFO area FFG data duration filter. */
static enum FfgDurationIds areal_ffg_dur_filter = ALL_FFG_DURATIONS ; 

/* Initial areal ffg boundary type setting. */
static enum FfgArealTypes ffg_areal_area_filter = FFG_ALL ;

static char *  ffg_rfc_names [ NUM_FFG_RFCS ] = { "ABRFC" , "AKRFC" , "CBRFC" ,
                                                  "CNRFC" , "LMRFC" , "MARFC" ,
                                                  "MBRFC" , "NCRFC" , "NERFC" ,
                                                  "NWRFC" , "OHRFC" , "SERFC" ,
                                                  "WGRFC" , "ALL_RFCS" } ;

static char * ffg_duration_names [ ] = { "1hr" , "3hr" , "6hr" , "12hr" ,
                                         "24hr" , "ALL_FFG_DURATIONS" } ;

/* These are the number of colors in the FFG durations,
   and color lists for the 1hr, 3hr, 6hr, 12hr, and 24 hr FFG
   products. */

#define NUM_FFG_COLORS 8
 
static const double ffg_durations [ NUM_FFG_COLORS ] =
              { -9999, -8888, 0.01, 1, 2, 3, 4, 5 };

static const char * ffg_colors [ NUM_FFG_COLORS ] =
              { "GRAY75" , "RED" , "ORANGE1", "YELLOW3",
                "SEAGREEN3", "DEEPSKYBLUE",
                "MEDIUMPURPLE1", "PURPLE" };

static NamedColorSetGroup * pFfgColors = NULL;

static NamedColorSetGroup * build_ffg_colors ( )
{
   NamedColorUseSet * pColorUseSet = NULL;

   /* FFG Default colors. */
   pColorUseSet = initializeNamedColorUseSet ( "FFG", 
                                               "FFG Default",
                                               NUM_FFG_COLORS,
                                               ffg_durations ,
                                               ffg_colors ,
                                               "GRAY72",
                                               "RED",
                                               3600 );
 
   pFfgColors = addNamedColorUseSet ( pFfgColors, pColorUseSet );

   return pFfgColors;  
}

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   ffg_select_display
* PURPOSE:  Display the netCDF file the user has selected from the FFG 
*           netCDF data selection gui.  This only applies in the case where
*           the FFG mode is "grid". 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  Widget      w           The widget in which the callback originated.
*   Input  XtPointer   clientdata  User-supplied calling data.
*   Input  XtPointer   calldata    Motif-supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                          HEADER FILE             DESCRIPTION
*   createProductDescriptionText  ArealDisplayControl.h   Creates color legend
*                                                         descriptive text. 
*   determineThresholdSetKey      ArealDisplayControl.h   Determines the
*                                                         threshold use name
*                                                         and duration 
*                                                         based on information
*                                                         contained in the
*                                                         hdc structure.
*   drawColorBar                  ColorBar.h              Draws the color bar
*                                                         in the color legend
*                                                         on the FFG selection
*                                                         GUI.
*   freeColorThresholdArray       ColorThreshold.h        Frees the color
*                                                         bar threshold array
*                                                         contained in the
*                                                         hdc structure.
*   getHvDisplayControl           HvDisplayControlProto.h Retrieves a pointer
*                                                         to the hdc 
*                                                         structure.
*   initColorBar                  ColorBar.h              Initializes the
*                                                         colorBar struct
*                                                         with widget and
*                                                         text information it
*                                                         needs to draw the
*                                                         color legend.
*   ListRsrcGetFirstSelectedPos   Xtools.h                Retrieves the
*                                                         first selected 
*                                                         position in the FFG
*                                                         gui list selection
*                                                         box.
*   ListNth                       List.h                  Returns the Nth node
*                                                         in the linked list
*                                                         of ffg netCDF data.
*   loadColorThresholdArray       ColorThreshold.h        Loads the ffg color
*                                                         threshold values.
*   read_netcdf_ffg               read_netcdf_ffg.h       Reads and displays
*                                                         the netCDF file
*                                                         the user has chosen.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE                  NAME             DESCRIPTION
*   ArealProductTypeDescriptor descriptor       Contains mode, precip type ,
*                                               and resolution level info.
*   ArealProductSpecifier      specifier        Contains duration, endtime,
*                                               source id, and status
*                                               information.
*   char [ ]                   description1Text The color legend text 
*                                               containing the name of the
*                                               data source and the resolution
*                                               level.
*   char [ ]                   description2Text The color legend text
*                                               containing the RFC or WFO
*                                               identifer, the duration of the
*                                               data, and the valid time of
*                                               the data.
*   char [ ]                   thresholdUseName Indicates that "FFG" is the
*                                               type of data being used.
*   ColorBar *                 colorBar         Points to a structure which
*                                               has data pertinent to the
*                                               color bar as well as the
*                                               widgets required to draw the
*                                               color bar.
*   HvDisplayControl *         hdc              The structure containing
*                                               the display properties of
*                                               the FFG data.
*   time_t                     duration         The duration (in seconds)
*                                               of the FFG data.
*   int                        length           Used in testing C-string
*                                               lengths to make sure that
*                                               character array bounds are
*                                               not overflowed. 
*   int                        list_position    The position of the selected
*                                               element in the list in the
*                                               FFG selection GUI.
*   Direct *                   pNode            A pointer to a specific 
*                                               node in the linked list
*                                               of netCDF file information.
*   char [ ]                   file_path        The path of the netCDF file
*                                               as built from the dir_name
*                                               and file_name members of the
*                                               Direct structure.
*
* DATA FILES AND/OR DATABASE:
* This routine requires netCDF files containing FFG data.  The names of these
* files are provided by the file_name member of each the Direct structures in 
* the linked list of Direct structures.  The paths of these files are
* provided by the dir_name member of the Direct structures in the linked list
* of Direct structures.
*
* ERROR HANDLING:
* This routine prints errors out to the standard error stream.
* This routine will abort before completion if any of the following
* conditions occur:
* 
* 1) "file_path" is not long enough to contain the combined file path and
*    file directory.
* 2) There are no nodes in the linked list of Direct structures.
*
********************************************************************************
*/

static void ffg_select_display ( Widget w , XtPointer clientdata, 
                                 XtPointer calldata ) 
{
    ArealProductTypeDescriptor   descriptor;
    ArealProductSpecifier        specifier;
    char description1Text [ BUFSIZ ] ;
    char description2Text [ BUFSIZ ] ;
    char thresholdUseName [ BUFSIZ ] ;
    ColorBar * colorBar = dc_GetColorBar ( ) ;
    HvDisplayControl * hdc = NULL ; 
    time_t duration = ( time_t ) 0 ;
    int ffg_hours ;
    int length ;
    int list_position ;
    int since_flag = 0 ;
    Direct * pNode = NULL ;
    char file_path [ FFG_DIRNAME_LENGTH + FFG_FILENAME_LENGTH ] ;

    /* Set the cursor to a watch. */
    mSetCursor ( M_WATCH ) ;
    hdc = getHvDisplayControl ( ) ;

    /* Free the ffg grids used in drawing the FFG data on the screen. */
    free_ffg_grids ( ) ;

    /* Retrieve the selected row in the scrolled list box. */
    list_position = ListRsrcGetFirstSelectedPos ( ffg_productsLI ) ;

    if ( list_position != -1 )
    {
       pNode = ( Direct * ) ListNth ( & listPtr->list , list_position ) ;

       if ( pNode != NULL )
       {
          switch ( pNode->duration )
          {
             case FFG_1HR :

                duration = 1 ;
                break ;

             case FFG_3HR :

                duration = 3 ;
                break ;

             case FFG_6HR :

                duration = 6 ;
                break ;

             case FFG_12HR :

                duration = 12 ;
                break ;

             case FFG_24HR :

                duration = 24 ;
                break ;

             default :

                fprintf ( stderr , "\nIn routine \"ffg_select_display\":\n"
                                   "Reached default case in switch\n"
                                   "statement.\n" ) ;
                mSetCursor ( M_NORMAL ) ;
                return ;
          }

          ffg_hours = duration ;
          duration *= 60 * 60 ;

          length = strlen ( pNode->dir_name ) ;

          if ( pNode->dir_name [ length - 1 ] != '/' ) 
          {
             sprintf ( file_path , "%s/%s" , pNode->dir_name , 
                       pNode->file_name ) ;
          }
          else
          {
             sprintf ( file_path , "%s%s" , pNode->dir_name , 
                       pNode->file_name ) ;
          }

          descriptor.mode = 1 ;
          descriptor.precipType = 0 ;
          descriptor.resolutionLevel = 0 ;

          length = strlen ( pNode->loc_id ) ;

          if ( length > SOURCE_ID_LEN )
          {
             fprintf ( stderr , "\nIn routine \"ffg_select_display\":\n"
                                "The \"loc_id\" member of the Direct\n"
                                "which contains a value of \"%s\" is too\n"
                                "long to be copied into the \"sourceId\"\n"
                                "variable of the ArealProductSpecifier\n"
                                "structure which can contain only %d\n"
                                "characters.\n" , pNode->loc_id ,
                                SOURCE_ID_LEN ) ;
             mSetCursor ( M_NORMAL ) ;
             return ;
          }

          strcpy ( specifier.sourceId , pNode->loc_id ) ;
          specifier.endTime = pNode->ticks ;
          specifier.duration = duration ;

          /* Set up the descriptors and the specifiers for the
             selected FFG dataset. */
          hdc->displaySettings.areal.descriptor = descriptor ;
          hdc->displaySettings.areal.selectedSpecifier = specifier ;

          /* Load the color legend. */
          freeColorThresholdArray ( & hdc->displaySettings.areal.ctArray ) ;
          determineThresholdSetKey ( thresholdUseName , & duration , hdc ) ;
          loadColorThresholdArray ( & hdc->displaySettings.areal.ctArray ,
                                    thresholdUseName , duration ,
                                    pFfgColors ,
                                    ffg_legendDA ) ;
          createProductDescriptionText ( descriptor , specifier ,
                                         description1Text ,
                                         description2Text ) ;
          initColorBar ( colorBar , ffg_legendDA ,
                         & hdc->displaySettings.areal.ctArray ,
                         description1Text , description2Text ) ;
          drawColorBar ( colorBar ) ;

          if ( ffg_mode == GRID_MODE )
          {
             read_netcdf_ffg ( file_path , gridded_ffg_display , 
                               ( int ) duration ) ;
          }
          else
          {
             read_areal_ffg ( pNode->loc_id ,
                              ffg_hours ,
                              pNode->ticks ,
                              since_flag ) ;
          }
        
       }
    }

    mSetCursor ( M_NORMAL ) ;
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   ffg_clear_display
* PURPOSE:       Clears the main viewing area of FFG data.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  Widget      w           The widget in which this callback was
*                                  generated. 
*   Input  XtPointer   clientdata  The user supplied callback data. 
*   Input  XtPointer   calldata    X/Motif generated callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                  HEADER FILE       DESCRIPTION
*   isThereFfgDataToDraw  read_netcdf_ffg.h This routine tests to determine
*                                           if there is FFG data on the screen
*                                           to clear.
*   turnOffFfgData        read_netcdf_ffg.h This routine clears the FFG from
*                                           the main viewing area. 
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/

void ffg_clear_display ( Widget w , XtPointer clientdata ,
                                XtPointer calldata )

{
   if ( isThereFfgDataToDraw ( ) != 0 )
   {
      turnOffFfgData ( ) ;
   }
}

/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   load_ffgProductList 
* PURPOSE:       Loads rows of data into the list widget of the 
*                FFG data selection gui.  Each row of data consists of
*                the location identifier, the duration value of the FFG data,
*                and the valid time of the FFG data.
*
* ARGUMENTS:
*   None
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME           HEADER FILE  DESCRIPTION
*   ListCount      List.h       Returns the number of nodes in the 
*                               linked list of Direct structures
 *                              containing netCDF file information.
*   ListFirst      List.h       Returns the first node in the linked
*                               list of Direct structures containing 
*                               netCDF file information.
*   loadXmList100  Xtools.h     Loads the location id, duration, and
*                               file time stamp contained in each node
*                               of the linked list of Direct structure
*                               into the List widget of the FFG data
*                               selector.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE   NAME       DESCRIPTION
*   Direct *    spPtr      Used to walk through the linked list of Direct
*                          structures. 
*   int         duration   Contains the duration of the netCDF file data
*                          currently being displayed.
*   int         i          A loop index variable.
*   int         list_count The number of nodes in the linked list of Direct
*                          structures.
*   RussText *  buf        The character buffer in which each row of the
*                          list widget is constructed.
*   struct tm * pTmStruct  A pointer to the tm structure of time information
*                          that represents the "ticks" member of the 
*                          "Direct" structure.
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   This routine prints an error message to the standard error stream
*   and returns if the following occur: 
*
*    1) There is no data in the linked list of "Direct" structures. 
*    2) The default case in the switch statement is reached.
********************************************************************************
*/
static void load_ffgProductList ( )
{
   static char * weekdays [ ] = { "Sun" , "Mon" , "Tue" , "Wed" , "Thu" ,
                                  "Fri" , "Sat" } ;
   Direct * spPtr  = NULL ;
   int duration ;
   int i = 0 ;
   int list_count = 0 ;
   RussText * buf = NULL ; 
   struct tm * pTmStruct = NULL ;
   
   if ( listPtr != NULL )
   {
      list_count = ListCount( &listPtr->list) ;
   }
   else
   {
      printf ( "No items available\n" ) ;
      return ;
   }
   
   if( list_count == 0 )
   {
   	printf ( "No items available\n" );
	return ;
   }
   else
   {
      buf = ( RussText * ) malloc ( list_count * sizeof ( RussText ) ) ;
	
      spPtr = ( Direct * ) ListFirst ( & listPtr->list ) ;

      while ( spPtr != NULL )
      {
         memset ( buf [ i ] , '\0' , 100 ) ;

         switch ( spPtr->duration )
         {
            case FFG_1HR :

               duration = 1 ;
               break ;

            case FFG_3HR :

               duration = 3 ;
               break ;

            case FFG_6HR :

               duration = 6 ;
               break ;

            case FFG_12HR :
               
               duration = 12 ;
               break ;

            case FFG_24HR :

	       duration = 24 ;
               break ;

            default :

               fprintf ( stderr , "\nIn routine \"load_ffgProductList\":\n"
                                  "Reached default case in switch\n"
                                  "statement.  Defaulting duration\n"
                                  "to one hour.\n" ) ;
               duration = 1 ;
               break ;
            }

         pTmStruct = gmtime ( & spPtr->ticks ) ; 

         sprintf ( buf [ i ] , "%5s   %02d  %s %02d-%02d %02d" ,
                   spPtr->loc_id ,
                   duration ,
                   weekdays [ pTmStruct->tm_wday ] ,
                   pTmStruct->tm_mon + 1 ,
                   pTmStruct->tm_mday ,
                   pTmStruct->tm_hour ) ; 
  
         spPtr = (Direct *)ListNext(&spPtr->node);
	 i++;
      } 
   
      /* Display available netCDF data in the List widget of the
         FFG display gui. */
      loadXmList100(ffg_productsLI, buf, list_count);

      free ( buf ) ;
      buf = NULL ;
   }
   
   return;  
}

/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   build_ffg_list
* PURPOSE:       Builds the linked list of Direct structures.  Each node
*                represents a single netCDF file containing FFG data.
*
* ARGUMENTS:
*   TYPE  DATA TYPE           NAME             DESCRIPTION/UNITS
*   Input char *              ffg_location     The location id of the RFC/WFO
*   Input enum FfgDurationIds ffg_duration     The duration of the FFG data 
*                                              in hours.
*   Input struct tm *         ffg_time_stamp   The time stamp of the FFG data.
*   Input char *              ffg_product_path The path of the netCDF file.
*   Input char *              ffg_file_name    The name of the netCDF file.
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   int         status                      The return status of this routine.
*
* APIs UTILIZED:
*   NAME         HEADER FILE   DESCRIPTION
*   ListAdd      List.h        Adds a node to the end of the linked list.
*   ListFirst    List.h        Lists the first node in the linked list.
*   ListInit     List.h        Initializes the linked list. 
*   ListInsert   List.h        Inserts a new node before a specified node in
*                              in the linked list.
*   ListNext     List.h        Lists the next node in the linked list.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME          DESCRIPTION
*   Direct *   ptrDir        Used to walk through the linked list.
*   Direct *   spPtr         Points to the new node to be inserted or appended
*                            to the end of the linked list.
*   int        compare_value Contains the comparison value returned by
*                            "strcmp".
*
* DATA FILES AND/OR DATABASE:
* None
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*             0                             Ran ok.
*             1                             Memory allocation failure.
*
********************************************************************************
*/
static int build_ffg_gridded_list ( char * ffg_location , 
                                    enum FfgDurationIds ffg_duration ,
                                    struct tm * ffg_time_stamp , 
                                    char * ffg_product_path , 
                                    char * ffg_file_name )
{
   Direct	 * ptrDir = NULL ;
   Direct	 * spPtr = NULL ;
   int compare_value ;

   /* Check for NULL values here. */
   /* Allocate and define a node to be placed onto the linked list. */
   if ( ( spPtr = ( Direct * ) malloc ( sizeof ( Direct ) ) ) != NULL )
   {
      strcpy ( spPtr->dir_name , ffg_product_path ) ;
      strcpy ( spPtr->file_name , ffg_file_name ) ;
      strcpy ( spPtr->loc_id , ffg_location ) ; 
      spPtr->duration = ffg_duration ; 
      spPtr->ticks = mktime ( ffg_time_stamp ) ; 
   }
   else
   {
      fprintf ( stderr , "\nIn routine \"build_ffg_gridded_list\":\n"
                         "Could not allocate memory for a node in the\n"
                         "ffg linked list.\n" ) ;
      return 1 ;
   }

   if ( first )
   {
      listPtr = spPtr;
      ListInit(&listPtr->list);
      ListAdd(&listPtr->list, (Node *)spPtr);
      first = 0;
   }
   else
   {
      ptrDir = ( Direct * ) ListFirst ( & listPtr->list ) ;

      /* Order by ascending location id */ 
      compare_value = strcmp ( spPtr->loc_id , ptrDir->loc_id ) ;

      while ( ( ptrDir != NULL ) && ( compare_value > 0 ) )
      {
         ptrDir = ( Direct * ) ListNext( & ptrDir->node ) ;

         if ( ptrDir != NULL )
         {
            compare_value = strcmp ( spPtr->loc_id , ptrDir->loc_id ) ;
         }
      }
     
      if (ptrDir == NULL)
      {
         ListAdd ( & listPtr->list , ( Node * ) spPtr ) ;
      }
      else if ( compare_value < 0 )
      {	
         ListInsert ( & listPtr->list , ( Node * ) ptrDir , 
                      ( Node * ) spPtr ) ;
      }
      else
      {
         /* Order by the duration hour in ascending order. */
         while ( ( ptrDir != NULL ) && 
                 ( ( int ) spPtr->duration > ( int ) ptrDir->duration ) &&
                 ( compare_value == 0 ) )
         {
             ptrDir = ( Direct * ) ListNext( & ptrDir->node ) ;

             if ( ptrDir != NULL )
             {
                compare_value = strcmp ( spPtr->loc_id , ptrDir->loc_id ) ;
             }
         }

         if (ptrDir == NULL)
         {
            ListAdd ( & listPtr->list , ( Node * ) spPtr ) ;
         }
         else if ( ( compare_value  < 0 ) ||
                 ( ( int ) spPtr->duration < ( int ) ptrDir->duration ) )
         {
            ListInsert ( & listPtr->list , (Node *) ptrDir , (Node *) spPtr ) ;
         } 
         else if ( ( int ) spPtr->duration == ( int ) ptrDir->duration )
         {
            /* Order by the time stamp in descending order. */
            while ( ( ptrDir != NULL ) && 
                  ( ( int ) spPtr->duration == ( int ) ptrDir->duration ) &&
                  ( compare_value == 0 ) &&
                  ( spPtr->ticks < ptrDir->ticks ) )
            {
               ptrDir = ( Direct * ) ListNext( & ptrDir->node ) ;

               if ( ptrDir != NULL )
               {
                  compare_value = strcmp ( spPtr->loc_id , 
                                           ptrDir->loc_id ) ;
               }
            }              

            if (ptrDir == NULL)
            {
               ListAdd ( & listPtr->list , ( Node * ) spPtr ) ;
            }
            else if ( ( compare_value  < 0 ) ||
                 ( ( int ) spPtr->duration < ( int ) ptrDir->duration ) )
            {
               ListInsert ( & listPtr->list , ( Node * ) ptrDir , 
                            ( Node * ) spPtr ) ;
            } 
            else
            {
               ListInsert ( & listPtr->list , ( Node * ) ptrDir , 
                            ( Node * ) spPtr ) ;
            }
         }
      }
    
   }  

   return 0 ;
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
static int build_ffg_areal_list ( ArealProductSpecifier * spec_array ,
                                  int num_specs ,
                                  enum FfgArealTypes areal_type )
{
   Direct * ptrDir = NULL ;
   Direct * spPtr = NULL ;
   enum FfgDurationIds duration = FFG_1HR ;
   int compare_value ;
   int i ;
   int num_hours ;

   for ( i = 0 ; i < num_specs ; ++ i )
   {
      /* Filter by the selected duration. */
      num_hours = spec_array [ i ].duration / 3600 ;

      switch ( num_hours )
      {
         case 1 :

            duration = FFG_1HR ;
            break ;

         case 3 :

            duration = FFG_3HR ;
            break ;

         case 6 :

            duration = FFG_6HR ;
            break ;

         case 12 :

            duration = FFG_12HR ;
            break ;
	
	 case 24 :

            duration = FFG_24HR ;
            break ;

         default :

            fprintf ( stderr , "\nIn routine 'build_ffg_areal_list':\n"
                               "Invalid argument in the duration\n"
                               "switch block.  Value = %d.\n" ,
                               num_hours ) ;  
      } 

      if ( ( areal_ffg_dur_filter != ALL_FFG_DURATIONS ) && 
           ( areal_ffg_dur_filter != duration ) )
      {
         continue ;
      }

      /* Check for NULL values here. */
      /* Allocate and define a node to be placed onto the linked list. */
      if ( ( spPtr = ( Direct * ) malloc ( sizeof ( Direct ) ) ) != NULL )
      {
         memset ( spPtr->dir_name , '\0' , FFG_DIRNAME_LENGTH ) ;
         memset ( spPtr->file_name , '\0' , FFG_FILENAME_LENGTH ) ;
         strcpy ( spPtr->loc_id , spec_array [ i ].sourceId ) ; 
         spPtr->ticks = spec_array [ i ].endTime ; 
         spPtr->duration = duration ;

         /* Set the soucrce id. Note that the boundary names are 
            capitalized to coincide */
         switch ( areal_type )
         {
            case FFG_BASIN : 

               strcpy ( spPtr->loc_id , "Basin" ) ; 
               break ;

            case FFG_COUNTY : 

               strcpy ( spPtr->loc_id , "County" ) ;
               break ;

            case FFG_ZONE : 

               strcpy ( spPtr->loc_id , "Zone" ) ;
               break ;

            default :

               fprintf ( stderr , "\nIn routine 'build_ffg_areal_list':\n"
                                  "Invalid argument in the areal_type\n"
                                  "switch block.  Value = %d.\n" ,
                                  ( int ) areal_type ) ;  
               return 1 ;

         }
      }
      else
      {
         fprintf ( stderr , "\nIn routine \"build_ffg_gridded_list\":\n"
                            "Could not allocate memory for a node in the\n"
                            "ffg linked list.\n" ) ;
         return 1 ;
      }

      if ( first )
      {
         listPtr = spPtr;
         ListInit(&listPtr->list);
         ListAdd(&listPtr->list, (Node *) spPtr ) ;
         first = 0;
      }
      else
      {
         ptrDir = ( Direct * ) ListFirst ( & listPtr->list ) ;

         /* Order by descending ascending location id. */ 

         compare_value = strcmp ( spPtr->loc_id , ptrDir->loc_id ) ;

         while ( ( ptrDir != NULL ) && ( compare_value > 0 ) )
         {
            ptrDir = ( Direct * ) ListNext( & ptrDir->node ) ;

            if ( ptrDir != NULL )
            {
               compare_value = strcmp ( spPtr->loc_id , ptrDir->loc_id ) ;
            }
         }
     
         if (ptrDir == NULL)
         {
            ListAdd ( & listPtr->list , ( Node * ) spPtr ) ;
         }
         else if ( compare_value < 0 )
         {	
            ListInsert ( & listPtr->list , ( Node * ) ptrDir , 
                         ( Node * ) spPtr ) ;
         }
         else
         {
            /* Order by the duration hour in ascending order. */
            while ( ( ptrDir != NULL ) && 
                    ( ( int ) spPtr->duration > ( int ) ptrDir->duration ) &&
                    ( compare_value == 0 ) )
            {
                ptrDir = ( Direct * ) ListNext( & ptrDir->node ) ;

                if ( ptrDir != NULL )
                {
                   compare_value = strcmp ( spPtr->loc_id , ptrDir->loc_id ) ;
                }
            }

            if (ptrDir == NULL)
            {
               ListAdd ( & listPtr->list , ( Node * ) spPtr ) ;
            }
            else if ( ( compare_value  < 0 ) ||
                    ( ( int ) spPtr->duration < ( int ) ptrDir->duration ) )
            {
               ListInsert ( & listPtr->list , (Node *) ptrDir , 
                            (Node *) spPtr ) ;
            }    
            else if ( ( int ) spPtr->duration == ( int ) ptrDir->duration )
            {
               /* Order by the time stamp in descending order. */
               while ( ( ptrDir != NULL ) && 
                     ( ( int ) spPtr->duration == ( int ) ptrDir->duration ) &&
                     ( compare_value == 0 ) &&
                     ( spPtr->ticks < ptrDir->ticks ) )
               {
                  ptrDir = ( Direct * ) ListNext( & ptrDir->node ) ;

                  if ( ptrDir != NULL )
                  {
                     compare_value = strcmp ( spPtr->loc_id , 
                                              ptrDir->loc_id ) ;
                  }
               }              

               if (ptrDir == NULL)
               {
                  ListAdd ( & listPtr->list , ( Node * ) spPtr ) ;
               }
               else if ( ( compare_value  < 0 ) ||
                    ( ( int ) spPtr->duration < ( int ) ptrDir->duration ) )
               {
                  ListInsert ( & listPtr->list , ( Node * ) ptrDir , 
                               ( Node * ) spPtr ) ;
               } 
               else
               {
                  ListInsert ( & listPtr->list , ( Node * ) ptrDir , 
                            ( Node * ) spPtr ) ;
               }
            }
         }
    
      }  
   }

   return 0 ;
}
/*******************************************************************************
* MODULE NUMBER: 6
* MODULE NAME:   read_gridded_ffg_product
* PURPOSE:       For the WFO or RFC areas, reads all of the pertinent netCDF
*                FFG files from the proper directories and stores the names
*                of these files and associated data describing them into a
*                linked list of "Direct" structures.  This linked list is
*                then loaded into the List widget on the FFG data selection
*                gui.  This will allow the user to easily select the desired
*                ffg product.
*
* ARGUMENTS:
*   None.
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   int         status                      The return status of this routine.
*
* APIs UTILIZED:
*   NAME                   HEADER FILE             DESCRIPTION
*   build_ffg_gridded_list Static above            Adds a node to the ffg linked
*                                                  list.
*   get_apps_defaults      General.h               Retrieves the value of a 
*                                                  token.
*   load_ffgProductList    Static above            Displays the available 
*                                                  netCDF files in the list
*                                                  widget of the FFG selection
*                                                  gui.   
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE           NAME                      DESCRIPTION
*   DIR           *     pDir                      Current directory being 
*                                                 processed. 
*   struct dirent *     my_dir                    The current directory being
*                                                 processed.
*   static char   *     wfo_id_token              The name of the token that
*                                                 contains the WFO identifier.
*   static char   *     ffg_extension             The expected extension of the
*                                                 ffg file.
*   static char   *     ffg_product_dir_token     The name of the token 
*                                                 containing the root directory
*                                                 of the RFC netCDF FFG files.
*   char *              pChar                     Used in processing the
*                                                 the extension of the FFG
*                                                 file.
*   static char   *     wfo_ffg_product_dir_token The name of the token
*                                                 containing the root directory
*                                                 of the WFO mosaic netCDF
*                                                 FFG files.
*   static char   *     dest_path                 The static part of the
*                                                 path to the RFC netCDF 
*                                                 FFG files. 
*   char [ ]            day                       Contains the string
*                                                 representation 
*                                                 of the two digit day.
*   char [ ]            duration_string           Contains the string
*                                                 representation of the
*                                                 two digit duration number.
*   char [ ]            ffg_product_path          Contains the directory 
*                                                 of the current netCDF file
*                                                 being processed.
*   char [ ]            file_wfo_id               The WFO-ID as retrieved from
*                                                 ffg file name.
*   char [ ]            hour                      Contains the string
*                                                 representation of the
*                                                 two digit hour value.
*   char [ ]            minute                    Contains the string
*                                                 representation of the
*                                                 two digit minute value.
*   char [ ]            month                     Contains the string
*                                                 representation of the
*                                                 two digit month number.
*   char [ ]            reply                     Contains the value of a
*                                                 token retrieved from
*                                                 a call to
*                                                 "get_apps_defaults".
*   char [ ]            wfo_id                    Contains the location
*                                                 identifier of the
*                                                 area (RFC or WFO) that
*                                                 FFG data is being
*                                                 retrieved for.
*   char [ ]            year                      Contains the string
*                                                 representation of the
*                                                 4 digit year.
*   enum FfgDurationIds dur_index                 Contains the duration
*                                                 that corresponds to the
*                                                 one parsed from the file
*                                                 name.  
*   int                 duration                  The numeric value of the
*                                                 duration parsed from the
*                                                 file name (WFO) or 
*                                                 directory name (RFC). 
*   int                 i                         Loop indexing variable.
*   int                 j                         Loop indexing variable.
*   int                 length_of_base_path       The length of base portion
*                                                 of the RFC FFG netCDF file
*                                                 being processed.  The
*                                                 part of the path that is
*                                                 before the RFC name and 
*                                                 duration name.
*   int                 length_of_rfc_path        The length of the portion
*                                                 of the RFC FFG netCDF file
*                                                 path being processed up to
*                                                 and including the RFC name.
*   int                 reply_len                 The length of the token
*                                                 value returned from 
*                                                 the call to 
*                                                 "get_apps_defaults".
*   int                 request_len               The length of the token
*                                                 name being supplied
*                                                 to "get_apps_defaults".
*   int                 status                    The return status of this
*                                                 application.
*   struct tm           time_stamp                The time stamp of the
*                                                 current netCDF FFG file
*                                                 being processed.
*
* DATA FILES AND/OR DATABASE:
* Relies on the following tokens:
*   FXA_DATA               The root directory of the RFC FFG netCDF files.
*   FXA_LOCAL_SITE         The identifier of the WFO.
*   GAFF_MOSAIC_DIR        The location of the netCDF files containing 
*                          mosaiced FFG data.
*
*   RFC netCDF file names are expected to have the following format:
*   YYYYMMDD_HHMM[.ext]  The .ext does not have to be there.
*
*   WFO netCDF file names are expected to have the following format:
*   YYYYMMDD_HHMM_II[.ext]
*
*   Where YYYY is the year, MM is the month, DD is the day, HH is the hour,
*   and MM is the minute of the netCDF data file.
*
*   Since the WFO directory structure does not specify the
*   duration, it is included as "II" in the filename.
* 
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*    0                                      Ran without detectable errors.
*    M_ERROR                                A token could not be resolved
*                                           or a base directory could not
*                                           be read.
********************************************************************************
*/
static int read_gridded_ffg_product ( ) 
{
   static char   * wfo_id_token = "FXA_LOCAL_SITE" ;
   static char   * ffg_extension = ".ffg" ;
   static char   * ffg_product_dir_token = "FXA_DATA" ;
   static char   * wfo_ffg_product_dir_token = "gaff_mosaic_dir" ;
   static char   * dest_path = "/img/SBN/netCDF/HRAP/FFG/" ;
   char            day [ LENGTH_OF_FFG_DAY ] ;
   char            duration_string [ LENGTH_OF_FFG_DURATION_STRING ] ;
   char 	   ffg_product_path [ MAX_RECORD_LENGTH ] ;
   char            file_wfo_id [ MAX_RECORD_LENGTH ] ;
   char            hour [ LENGTH_OF_FFG_HOUR ] ; 
   char            minute [ LENGTH_OF_FFG_MINUTE ] ; 
   char            month [ LENGTH_OF_FFG_MONTH ] ;
   char          * pChar = NULL ;
   char   	   reply [ LEN_REPLY ] ;
   char            wfo_id [ MAX_RECORD_LENGTH ] ;
   char            year [ LENGTH_OF_FFG_YEAR ] ;
   DIR           * pDir = NULL ;
   struct dirent * my_dir = NULL ;
   enum FfgDurationIds dur_index ;
   int             duration ;
   int             i ;
   int             j ;
   int             length_of_base_path ;
   int             length_of_rfc_path ;
   int             reply_len ;
   int 		   request_len ;
   int 		   status ;
   struct tm       time_stamp ;

   /* the list has been destroyed and recreated */
   XmListDeleteAllItems(ffg_productsLI); 
   
   /* Check the FFG mode. */
   if ( ffg_source == WFO_FFG )
   {
      /* Retrieve the path of the WFO ffg_product
        file from the "apps defaults" file. */
      request_len = strlen ( wfo_ffg_product_dir_token ) ;
      status = get_apps_defaults ( wfo_ffg_product_dir_token, &request_len, 
   				   reply, &reply_len ) ;
      if ( status != 0 )
      {
         fprintf ( stderr , "\nIn routine \"read_gridded_ffg_product\":\n"
                            "Could not retrieve the path of the wfo ffg\n"
                            "products from token \"%s\".\n" ,
                            wfo_ffg_product_dir_token ) ;
         return M_ERROR ;
      }

      if ( reply_len >= MAX_RECORD_LENGTH )
      {
        fprintf ( stderr , "\nIn routine \"read_gridded_ffg_product\":\n"
                           "The length of the wfo ffg data directory\n"
                           "path is %d.  This is greater than %d characters,\n"
                           "the amount of storage allocated for this\n"
                           "information.\n" , reply_len ,
                           ( MAX_RECORD_LENGTH - 1 ) ) ;
        return M_ERROR ;
     
      }

      strcpy ( ffg_product_path , reply ) ;

      /* Retrieve the identifier of the WFO. */
      request_len = strlen ( wfo_id_token ) ;
      status = get_apps_defaults ( wfo_id_token, & request_len, 
   				   reply, & reply_len ) ;

      if ( status != 0 )
      {
         fprintf ( stderr , "\nIn routine \"read_gridded_ffg_product\":\n"
                            "Could not retrieve the wfo identifier\n"
                            "from token \"%s\".\n" , wfo_id_token ) ;
         return M_ERROR ;
      }

      if ( reply_len >= MAX_RECORD_LENGTH )
      {
        fprintf ( stderr , "\nIn routine \"read_gridded_ffg_product\":\n"
                           "The length of the wfo identifier is %d.\n"
                           "This is greater than %d characters,\n"
                           "the amount of storage allocated for this\n"
                           "information.\n" , reply_len ,
                           ( MAX_RECORD_LENGTH - 1 ) ) ;
        return M_ERROR ;
     
      }

      strcpy ( wfo_id , reply ) ;

      pDir = opendir ( ffg_product_path ) ;

      if ( pDir == NULL )
      {
          fprintf ( stderr , "\nIn routine \"read_gridded_ffg_product\":\n"
                             "Could not read directory \"%s\".\n"
                             "Check to see if it exists and has read\n"
                             "permissions for the user running this\n"
                             "application.\n" , ffg_product_path ) ; 
          return M_ERROR ;
      } 

      my_dir = readdir ( pDir ) ;
  
      while ( my_dir != NULL )
      {
         /* Check to make sure that this is a FFG file.  This
            is done by checking the extension on the file. */
         pChar = strrchr ( my_dir->d_name , '.' ) ;

         if ( pChar != NULL )
         {
            if ( ( strncmp ( pChar , ffg_extension , 4 ) ) == 0 )
            {
               if ( strlen ( my_dir->d_name ) >= WFO_FFG_FILENAME_LEN )
               {
                  /* Check to make sure that this is a FFG file. This
                     is done by checking the extension on the file. */
                  pChar = strrchr ( my_dir->d_name , '.' ) ;

                  /* Parse the filename for the WFO identifier here. */
                  strncpy ( file_wfo_id , my_dir->d_name , 3 ) ;
                  file_wfo_id [ 3 ] = '\0' ;

                  /* Parse the filename for duration and time stamp here. */
                  strncpy ( year , my_dir->d_name + 3 , 4 ) ;
                  * ( year + 4 )  = '\0' ;
                  strncpy ( month , ( my_dir->d_name ) + 7 , 2 ) ;
                  * ( month + 2 ) = '\0' ;
                  strncpy ( day , ( my_dir->d_name ) + 9 , 2 ) ;
                  * ( day + 2 ) = '\0' ;
                  strncpy ( hour , ( my_dir->d_name ) + 12 , 2 ) ;
                  * ( hour + 2 ) = '\0' ;
                  strncpy ( minute , ( my_dir->d_name ) + 14 , 2 ) ;
                  * ( minute + 2 ) = '\0' ;
                  strncpy ( duration_string , ( my_dir->d_name ) + 17 , 2 ) ;
                  * ( duration_string + 2 ) = '\0' ;

                  time_stamp.tm_year = atoi ( year ) - 1900 ;
                  time_stamp.tm_mon = atoi ( month ) - 1 ;
                  time_stamp.tm_mday = atoi ( day ) ;
                  time_stamp.tm_hour = atoi ( hour ) ;
                  time_stamp.tm_min = atoi ( minute ) ;
                  time_stamp.tm_sec = 0 ;
                  duration = atoi ( duration_string ) ;

                  switch ( duration )
                  {
                     case 1 :

                        dur_index = FFG_1HR ;
                        break ;

                     case 3 :

                        dur_index = FFG_3HR ;
                        break ;

                     case 6 :

                        dur_index = FFG_6HR ;
                        break ;

                     case 12 :

                        dur_index = FFG_12HR ;
                        break ;

                     case 24 :

                        dur_index = FFG_24HR ;
                        break ;

                     default :

                       fprintf ( stderr , "\nIn routine " 
                                       "read_gridded_ffg_product\":\n"
                                       "Reached default case in switch\n"
                                       "statement while parsing the duration\n"
                                       "value in the wfo ffg file name.  The\n"
                                       "duration as indicated in the filename\n"
                                       "is %d.  This value is unsupported.\n"
                                       "\"dur_index\" is being set to\n"
                                       "\"FFG_1HR\".\n" , duration ) ; 
                        dur_index = FFG_1HR ;
                        break ;
                  }

                  /* Filter out durations the user does not want do see. */
                  if ( ( gridded_ffg_dur_filter == ALL_FFG_DURATIONS ) || 
                       ( gridded_ffg_dur_filter == dur_index ) )
                  {

                     /* Add onto the linked list of netCDF file information. */ 
                     build_ffg_gridded_list ( file_wfo_id , 
                                              dur_index ,
                                              & time_stamp ,
                                              ffg_product_path ,
                                              my_dir->d_name ) ;
                  }
               }
            }
         } 

         my_dir = readdir ( pDir ) ;
      }

      closedir ( pDir ) ;

   }
   else
   {
      /* Retrieve the path of the ffg_product
        file from the "apps defaults" file. */
      request_len = strlen ( ffg_product_dir_token ) ;
      status = get_apps_defaults ( ffg_product_dir_token, &request_len, 
   				reply, &reply_len ) ;
      if ( status != 0 )
      {
         fprintf ( stderr , "\nIn routine 'read_gridded_ffg_product':\n"
                           "Could not retrieve the path of the hmap/mpe\n"
                           "ffg_products directory from the environment\n"
                           "or apps defaults token file.\n" ) ;
         return M_ERROR ;
      }
   
      if ( reply_len >= MAX_RECORD_LENGTH )
      {
        fprintf ( stderr , "\nIn routine 'read_gridded_ffg_product':\n"
                           "The length of the hmap/mpe ffg data directory\n"
                           "path is %d.  This is greater than %d characters,\n"
                           "the amount of storage allocated for this\n"
                           "information.\n" , reply_len ,
                           ( MAX_RECORD_LENGTH - 1 ) ) ;
        return M_ERROR ;
     
      }
  
      strcpy ( ffg_product_path , reply ) ;
      strcat ( ffg_product_path , dest_path ) ;

      /* Save the length of the ffg product path.  This will be very useful
         for reconstructing the path for successive RFCs. */ 
      length_of_base_path = strlen ( ffg_product_path ) ;
   
      /* Loop over the RFCs the user wishes to display. */
      for ( i = ( int ) ABRFC ; i <= ( int ) WGRFC ; ++ i ) 
      {
         /* Filter out RFCs the user does not want do see. */
         if ( ( ffg_id_filter != ALL_RFCS ) && 
              ( ffg_id_filter != ( enum FfgRfcIds ) i ) ) continue ;

         strcat ( ffg_product_path , ffg_rfc_names [ i ] ) ;
         strcat ( ffg_product_path , "/" ) ;

         /* Save the length of the ffg product path with the RFC id 
            attached to it. */ 
         length_of_rfc_path = strlen ( ffg_product_path ) ;

         /* For each RFC, loop over the durations that the user wishes to
            display. */
         for ( j = ( int ) FFG_1HR ; j <= ( int ) FFG_24HR ; ++ j )
         {
            /* Filter out durations the user does not want do see. */
            if ( ( gridded_ffg_dur_filter != ALL_FFG_DURATIONS ) && 
                 ( gridded_ffg_dur_filter != ( enum FfgDurationIds ) j ) ) 
            {
               continue ;
            }
            
            strcat ( ffg_product_path , ffg_duration_names [ j ] ) ;

            pDir = opendir ( ffg_product_path ) ;

            if ( pDir == NULL )
            {
               fprintf ( stderr , "\nIn routine 'read_gridded_ffg_product':\n"
                                  "Could not read directory \"%s\".\n"
                                  "Check to see if it exists and has read\n"
                                  "permissions for the user running this\n"
                                  "application.\n" , ffg_product_path ) ; 
            } 
            else
            {
               my_dir = readdir ( pDir ) ; 
      
               while ( my_dir != NULL )
               {
                  if ( strlen ( my_dir->d_name ) >= RFC_FFG_FILENAME_LEN )
                  {
                     /* Parse the filename for the time stamp. */
                     strncpy ( year , my_dir->d_name , 4 ) ;
                     * ( year + 4 ) = '\0' ;
                     strncpy ( month , ( my_dir->d_name ) + 4 , 2 ) ;
                     * ( month + 2 ) = '\0' ;
                     strncpy ( day , ( my_dir->d_name ) + 6 , 2 ) ;
                     * ( day + 2 ) = '\0' ;
                     strncpy ( hour , ( my_dir->d_name ) + 9 , 2 ) ;
                     * ( hour + 2 ) = '\0' ;
                     strncpy ( minute , ( my_dir->d_name ) + 11 , 2 ) ;
                     * ( minute + 2 ) = '\0' ;

                     time_stamp.tm_year = atoi ( year ) - 1900 ;
                     time_stamp.tm_mon = atoi ( month ) - 1 ;
                     time_stamp.tm_mday = atoi ( day ) ;
                     time_stamp.tm_hour = atoi ( hour ) ;
                     time_stamp.tm_min = atoi ( minute ) ;
                     time_stamp.tm_sec = 0 ;
 
                     /* Add onto the linked list of netCDF file information. */ 
                     build_ffg_gridded_list ( ffg_rfc_names [ i ] , 
                                            ( enum FfgDurationIds ) j ,
                                            & time_stamp , 
                                            ffg_product_path ,
                                            my_dir->d_name ) ;
                  }

                  my_dir = readdir ( pDir ) ; 
	 
               }

               closedir ( pDir ) ;
            }

            ffg_product_path [ length_of_rfc_path ] = '\0' ;

         }

         ffg_product_path [ length_of_base_path ] = '\0' ;
      }
      
   }
   
   /* load the XmList from the in memory data */
   load_ffgProductList ( ) ;
   

   return (0) ;
   
}

/*******************************************************************************
* MODULE NUMBER: read_areal_ffg_product
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
#define MAX_SPECS 500

static int read_areal_ffg_product ( )
{
   ArealProductTypeDescriptor product_descriptor ;
   ArealProductSpecifier spec_array [ MAX_SPECS ] ; 
   int i ;
   int num_specs ;
   int status ;
   int j ;
   const char * ArealTypes [] = {"Basin", "County", "Zone"} ;
   
   /* the list has been destroyed and recreated */
   XmListDeleteAllItems(ffg_productsLI); 

   /* Initialize the product descriptor structure. */
   product_descriptor.mode = FFG_MODE ;

   /* Loop over the available boundary types.  These include: COUNTY_RES ,
      ZONE_RES, and BASIN_RES.  Base this on the setting of the areal
      areal option menu.  For each boundary types set up the product 
      description structure and call the bld_ffglist ( ) routine. */ 
   if ( ffg_areal_area_filter == FFG_ALL )
   {
      j = 0 ;
      for ( i = 0 ; i < NUM_FFG_DISPLAY_AREAS ; ++ i )
      {
         if ( i == ( int ) FFG_ALL || i == ( int ) FFG_GRID ) continue ;

         switch ( i )
         {
	    case FFG_BASIN :
               product_descriptor.resolutionLevel = BASIN_RES ;
	       break ;

            case FFG_COUNTY :
               j++ ;
               product_descriptor.resolutionLevel = COUNTY_RES ;
	       break ;

            case FFG_ZONE :
               j++ ;
               product_descriptor.resolutionLevel = ZONE_RES ;
	       break ;

            default :

               fprintf ( stderr , "\nIn routine 'read_areal_ffg_product':\n"
                                  "Invalid value being tested in the\n"
                                  "ffg_areal_area_filter switch statement.\n"
                                  "Value = %d\n" , 
                                  ( int ) ffg_areal_area_filter ) ;
               return -1 ;
         }

         status = bld_ffglist ( product_descriptor ,
                                MAX_SPECS ,
                                spec_array ,
                                &num_specs ) ;

         if ( status != 0 )
         {
            fprintf ( stderr , "\nIn routine 'read_areal_ffg_product':\n"
                               "The call to routine 'bld_ffglist' failed.\n"
                               "There is no FFG data for %s in contingencyvalue table.\n", ArealTypes[j]);
         }

         /* The array of product specs has been created.  These must now be 
            translated into the linked list of Direct structures which can
            then be loaded into the scrolled product selection list. */ 
         status = build_ffg_areal_list ( spec_array , num_specs , 
                                        ( enum FfgArealTypes ) i ) ;

         if ( status != 0 )
         {
            fprintf ( stderr , "\nIn routine 'read_areal_ffg_product':\n"
                               "The call to routine 'bld_ffg_areal_list' "
                               "failed.\n" );
            return -1 ;
         }

      }
   }
   else
   {
      j = 0 ;
      switch ( ffg_areal_area_filter )
      {
         case FFG_BASIN :
            product_descriptor.resolutionLevel = BASIN_RES ;
	    break ;

         case FFG_COUNTY :
            j++ ;
            product_descriptor.resolutionLevel = COUNTY_RES ;
            break ;

         case FFG_ZONE :
            j++ ;
            product_descriptor.resolutionLevel = ZONE_RES ;
	    break ;

         default :
            fprintf ( stderr , "\nIn routine 'read_areal_ffg_product':\n"
                               "Invalid valid being tested in the\n"
                               "ffg_areal_area_filter switch statement.\n"
                               "Value = %d\n" , 
                               ( int ) ffg_areal_area_filter ) ;
            return -1 ;
      }

      status = bld_ffglist ( product_descriptor ,
                             MAX_SPECS ,
                             spec_array ,
                             & num_specs ) ;

      if ( status != 0 )
      {
         fprintf ( stderr , "\nIn routine 'read_areal_ffg_product':\n"
                            "The call to routine 'bld_ffglist' failed.\n" 
			    "There is no FFG data for %s in contingencyvalue table.\n", ArealTypes[j]) ;
      }

      /* The array of product specs has been created.  These must now be 
         translated into the linked list of Direct structures which can
         then be loaded into the scrolled product selection list. */
      status = build_ffg_areal_list ( spec_array , num_specs , 
                                      ffg_areal_area_filter ) ;

      if ( status != 0 )
      {
         fprintf ( stderr , "\nIn routine 'read_areal_ffg_product':\n"
                            "The call to routine 'bld_ffg_areal_list' "
                            "failed.\n" );
         return -1 ;
      }
   }

   /* load the XmList from the in memory data */
   load_ffgProductList ( ) ;

   return 0 ;
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
static int read_ffg_product ( )
{
   /* Get the state of the FFG mode button. If it is "GRID_MODE" 
      then read the netcdf files containing gridded FFG data.  Otherwise.
      read the Contingency Values table in the IHFS database to retrieve the
      FFG data. */
   if ( ffg_mode == GRID_MODE )
   {
      read_gridded_ffg_product ( ) ;
   }
   else /* ffg_mode must be AREAL_MODE. */
   {
      read_areal_ffg_product ( ) ;
   }
  
   return 0 ;
}

/*******************************************************************************
* MODULE NUMBER:   6
* MODULE NAME:     configure_ffg_gui
* PURPOSE:         Sets up the filter options on the FFG selection gui
*                  based on the area ( WFO/RFC) the data is being retrieved
*                  for.  For RFC areas, the data may be filtered on both
*                  RFC id and duration.  For WFO areas, the data may only
*                  be filtered on duration.
*
* ARGUMENTS:
*   None
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   Only system and X/Motif routines are called.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None.
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   None.
********************************************************************************
*/
static void configure_ffg_gui ( ) 
{
   switch ( ffg_source )
   {
      case WFO_FFG : 

         DeSensitize ( ffg_gridIdOM) ;
         break ;

      case RFC_FFG :

         Sensitize ( ffg_gridIdOM ) ;
         break ;

      default :

         /* Do nothing. */
         break ;
   }

}

/*******************************************************************************
* MODULE NUMBER: 7
* MODULE NAME:   free_ffg_list
* PURPOSE:       Returns the dynamically allocated memory used in creating the
*                linked list of netCDF file information to the operating 
*                system.  This destroys the linked list and sets the head
*                pointer to NULL. 
*
* ARGUMENTS:
*   None
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME       HEADER FILE         DESCRIPTION
*   ListNext   List.h              Lists the next node in the linked list.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME       DESCRIPTION
*   Direct *   nextPtr    A pointer to the next node in the linked list.
*
* DATA FILES AND/OR DATABASE:
*   Not Applicable
*
* ERROR HANDLING:
*   Not needed
*
********************************************************************************
*/
static void free_ffg_list ( )
{
   Direct * nextPtr = NULL ; 

   /* Free the linked list and start a new one. */
   while ( listPtr != NULL )
   {
      nextPtr = ( Direct * ) ListNext ( & listPtr->node ) ;
      free ( ( void * ) listPtr ) ;
      listPtr = nextPtr ;
   }

   listPtr = NULL ;
   first = 1 ;
}

/*******************************************************************************
* MODULE NUMBER: 8
* MODULE NAME:   ffg_close_display 
* PURPOSE:       This is the callback to the "Close" button on the FFG
*                selection GUI.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  Widget      w           The identifier of the close button widget.
*   Input  XtPointer   clientdata  User supplied callback data.
*   Input  XtPointer   calldata    X/Motif supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME           HEADER FILE     DESCRIPTION
*   free_ffg_list  Static above    Frees the memory used by the linked list
*                                  of netCDF FFG file information.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None.
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   None.
*
********************************************************************************
*/
static void ffg_close_display ( Widget w , XtPointer clientdata , 
                                XtPointer calldata )
{
    free_ffg_list ( ) ;
    free_ffg_linesegs ( ) ;
    free_ffg_grids ( ) ;
    XtPopdown ( ffg_displayControlDS ) ;
    return ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:    setup_ffg_mode
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

static void setup_ffg_mode ( Widget w , XtPointer clientdata , 
                             XtPointer calldata )
{
    Boolean areal_mode ;
    XmString xm_str ;
 
    /* Retrieve the states of the ffg mode radio buttons. */
    areal_mode = XmToggleButtonGetState ( ffg_arealTB ) ;

    if ( areal_mode == TRUE )
    {
       /* Set the flag in the FFG drawing routines that indicates what 
          is being drawn. */
       SetFfgMode ( AREAL_MODE ) ;

       /* Set the global flag indicating the FFG mode is now AREAL_MODE. */
       ffg_mode = AREAL_MODE ;

       /* Set the id_type label to "Type". */
       xm_str = XmStringCreate ( "Type" , XmSTRING_DEFAULT_CHARSET ) ; 
       XtVaSetValues ( id_typeLB , XmNlabelString , xm_str , NULL ) ;
       XmStringFree ( xm_str ) ;

       /* Show the areal FFG selection options. */ 
       if ( XtIsManaged ( ffg_grid_optionsFR ) )
       {
          XtUnmanageChild ( ffg_griddedFO ) ;
          XtUnmanageChild ( ffg_grid_optionsFR ) ;
       }
          
       XtManageChild ( ffg_arealFO ) ;
       XtManageChild ( ffg_areal_optionsFR ) ; 

    }
    else /* Must be in gridded ffg mode. */
    {
       /* Set the flag in the FFG drawing routines that indicates what 
          is being drawn. */
       SetFfgMode ( GRID_MODE ) ;

       /* Set the global flag indicating the FFG mode is now GRID_MODE. */
       ffg_mode = GRID_MODE ;

       /* Set the id_type label to "Id". */
       xm_str = XmStringCreate ( "Id" , XmSTRING_DEFAULT_CHARSET ) ; 
       XtVaSetValues ( id_typeLB , XmNlabelString , xm_str , NULL ) ;
       XmStringFree ( xm_str ) ;

       /* Show the gridded FFG selection options. */
       if ( XtIsManaged ( ffg_areal_optionsFR ) )
       {
          XtUnmanageChild ( ffg_arealFO ) ;
          XtUnmanageChild ( ffg_areal_optionsFR ) ;
       }
          
       XtManageChild ( ffg_griddedFO ) ;
       XtManageChild ( ffg_grid_optionsFR ) ; 

    } 

    /* Free any former ffg information. */
    free_ffg_list ( ) ;

    /* Update the data in the FFG selection scrolled list based on the new
       mode. */
    read_ffg_product ( ) ;
}
   
/*******************************************************************************
* MODULE NUMBER: 9
* MODULE NAME:   setup_ffg_source
* PURPOSE:       Reconfigures the FFG selection gui and reads in new data
*                when the data area is changed from WFO to RFC or vice versa.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  Widget      w           The identifier of the close button widget.
*   Input  XtPointer   clientdata  User supplied callback data.
*   Input  XtPointer   calldata    X/Motif supplied callback data.
*
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                 HEADER FILE    DESCRIPTION
*   configure_ffg_gui    Static Above   Reconfigures the appearance of the
*                                       FFG selection gui based on the data
*                                       area (WFO or RFC) selected. 
*   free_ffg_list        Static Above   Destroys the linked list of netCDF
*                                       file information and deallocates
*                                       the memory that it uses.
*   read_ffg_product     Static Above   Reads in new netCDF FFG file
*                                       information based on the new
*                                       data source.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/

static void setup_ffg_source ( Widget w , XtPointer clientdata , 
                               XtPointer calldata )
{

   /* Set the source variable to indicate the source of ffg data the
      user desires. */
   ffg_source = ( enum FfgGridArea ) clientdata ;
   
   /* Configure the Flash Flood Guidance GUI according to the source
      of the ffg data. */
   configure_ffg_gui ( ) ;

   free_ffg_list ( ) ;

   /* Read the data and load it into the Flash Flood Guidance GUI list 
      widget . */
   read_ffg_product ( ) ;
}

/*******************************************************************************
* MODULE NUMBER:  10
* MODULE NAME:    set_ffg_id_filter
* PURPOSE:        Callback for when the a different option button on the
*                 FFG id filter option menu is chosen.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  Widget      w           The identifier of the close button widget.
*   Input  XtPointer   clientdata  User supplied callback data.
*   Input  XtPointer   calldata    X/Motif supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME             HEADER FILE            DESCRIPTION
*   free_ffg_list    Static Above           Frees the memory used by the
*                                           linked list of netCDF FFG file
*                                           information.
*   read_ffg_product Static Above           Reads the FFG product information
*                                           for the new filter options and
*                                           updates the list of files 
*                                           in the list widget of the FFG
*                                           data selection gui.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
static void set_ffg_id_filter ( Widget w , XtPointer clientdata , 
                                XtPointer calldata )
{
   ffg_id_filter = ( enum FfgRfcIds ) clientdata ;

   free_ffg_list ( ) ;

   /* Read the data and load it into the Flash Flood Guidance GUI list 
      widget . */
   read_ffg_product ( ) ;
}

/*******************************************************************************
* MODULE NUMBER: 11
* MODULE NAME:   set_ffg_duration_filter 
* PURPOSE:       The callback for a change in the ffg duration filter option
*                menu.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  Widget      w           The identifier of the close button widget.
*   Input  XtPointer   clientdata  User supplied callback data.
*   Input  XtPointer   calldata    X/Motif supplied callback data.
*
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                HEADER FILE         DESCRIPTION
*   free_ffg_list       Static Above        Frees the memory used by
*                                           the linked list of netCDF
*                                           file information. 
*   read_ffg_product    Static Above        Reads in new FFG netCDF data
*                                           based on the new duration filter
*                                           options.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
static void set_ffg_duration_filter ( Widget w , XtPointer clientdata ,
                                      XtPointer calldata )
{
   if ( ffg_mode == GRID_MODE ) 
   {
     gridded_ffg_dur_filter = ( enum FfgDurationIds ) clientdata ;
   }
   else
   {
     areal_ffg_dur_filter = ( enum FfgDurationIds ) clientdata ;
   }

   free_ffg_list ( ) ;

   /* Read the data and load it into the Flash Flood Guidance GUI list 
      widget . */
   read_ffg_product ( ) ;
}

/*******************************************************************************
* MODULE NUMBER: 12
* MODULE NAME:   set_ffg_areal_filter
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
static void set_ffg_areal_filter ( Widget w , XtPointer clientdata ,
                                  XtPointer calldata )
{
   ffg_areal_area_filter = ( enum FfgArealTypes ) clientdata ;

   free_ffg_list ( ) ;

   /* Read the data and load it into the Flash Flood Guidance GUI list 
      widget . */
   read_ffg_product ( ) ;
}

/*******************************************************************************
* MODULE NUMBER: 13
* MODULE NAME:   process_double_click 
* PURPOSE:       Simply resets the click_count global variable to "0"
*                indicating that a double click has not occurred.
*
* ARGUMENTS:
*   TYPE   DATA TYPE      NAME         DESCRIPTION/UNITS
*   Input  XtPointer      clientdata   The user-supplied callback data.
*                                      No value is expected here.
*   Input  XtIntervalId * id           The interval id generated by the call
*                                      to "XtAppAddTimeout".
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   None.
*
********************************************************************************
*/
static void process_double_click ( XtPointer clientdata , XtIntervalId * id )
{
   click_count = 0 ;
}

/*******************************************************************************
* MODULE NUMBER: 14
* MODULE NAME:   process_clicks
* PURPOSE:       Listens for mouse clicks in the list box of the FFG data
*                selection gui.  If two clicks occur within a specified 
*                amount of time, then they are interpreted as a double
*                click and the "process_double_click" routine is given
*                the thumbs up to load in the new FFG data based on the
*                user's selection in the gui.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  Widget      w                    The widget id of the selection
*                                           list box.
*   Input  XtPointer   clientdata           User-supplied callback data.
*                                           Nothing required here. 
*   Input  XEvent *    event                Event specific structure created
*                                           by the system.
*   Input  Boolean *   continue_to_dispatch Flag indicating whether or not
*                                           this event should be dispatched
*                                           to other event handlers.
* RETURNS:
*   Void  
*
* APIs UTILIZED:
*   NAME                    HEADER FILE     DESCRIPTION
*   _get_map_context        map_resource.h  Retrieves the application context
*                                           of the map.  
*   ffg_select_display      Static Above    Displays the new FFG data as
*                                           selected by the user.
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE    NAME      DESCRIPTION
*   int          interval  The maximum separation in milliseconds within which
*                          two clicks are considered to be a double click.
*   XtAppContext app       The application context of the hmap_mpe program.
*   XtIntervalId id        The identifier of the time out interval used to
*                          gauge whether or not a double mouse click has
*                          occurred.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
static void process_clicks ( Widget w , XtPointer clientdata , 
                             XEvent * event , Boolean * continue_to_dispatch )
{

   int interval = 500 ;
   XtAppContext app ;
   static XtIntervalId id = 0 ;

   app = _get_map_context ( ) ;

   /* Ignore presses of button 2 or button 3. */
   if ( ( event->xbutton.button == 2 ) || 
        ( event->xbutton.button == 3 ) )
   {
      return ;
   }

   ++ click_count ;

   if ( click_count == 1 )
   {
      id = XtAppAddTimeOut ( app , interval , process_double_click , 
                             NULL ) ;
   }
   else
   {
     XtRemoveTimeOut ( id ) ;
     ffg_select_display ( ( Widget ) NULL , clientdata , NULL ) ;
     click_count = 0 ;
   }

} 

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:   set_grid_display_type
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
static void set_grid_display_type ( Widget w , XtPointer clientdata ,
                                    XtPointer calldata )
{
   gridded_ffg_display = ( enum FfgArealTypes ) clientdata ;   

   /* Free any former ffg information. */
   free_ffg_list ( ) ;

   read_ffg_product ( ) ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:  set_ffg_areal_annotations 
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
static void set_ffg_areal_annotations ( Widget w , XtPointer clientdata ,
                                        XtPointer calldata )
{
   XmToggleButtonCallbackStruct * tb_state =
                          ( XmToggleButtonCallbackStruct * ) calldata ;
   int toggle_button_id = ( int ) clientdata ;
   
   if ( toggle_button_id == AREAL_ID_TOGGLE_BUTTON )
   {
      toggle_areal_annotations ( AREAL_ID_TOGGLE_BUTTON , tb_state->set ) ;
   }
   else
   {
      /* The areal values toggle button must have triggered this callback. */
      toggle_areal_annotations ( AREAL_VAL_TOGGLE_BUTTON , tb_state->set ) ;
   }

   /* Force the map to be redrawn. */
   mUpdateMap ( 0 ) ;

} 

/*******************************************************************************
* MODULE NUMBER: 15
* MODULE NAME:   ffg_dc_AddCallbacks 
* PURPOSE:       Adds callbacks to the widgets that constitute the FFG data
*                selection gui.
*
* ARGUMENTS:
*   None
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                    HEADER FILE            DESCRIPTION
*   dc_GetColorBar          display_control_show.h Retrieves the color bar 
*                                                  currently in use.
*   ffg_clear_display       Static Above           Callback for "clear" button.
*   ffg_close_display       Static Above           Callback for "close" button.
*   ffg_select_display      Static Above           Callback for "select" button.
*   freeColorBar            ColorBar.h             Frees the color bar.
*   process_clicks          Static Above           Listens for double clicks.
*   redrawColorBarCallback                         Expose callback to redraw
*                                                  the color legend.
*   set_ffg_duration_filter                        Callback to redraw FFG
*                                                  data based on a FFG duration
*                                                  filter change.
*   set_ffg_id_filter                              Callback to redraw FFG
*                                                  data based on a FFG id
*                                                  filter change.
*   setup_ffg_source                               Callback to reload FFG
*                                                  data available based
*                                                  on a change in FFG area. 
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME      DESCRIPTION
*   Atom       atom      Atom used to define the close procedure used
*                        the window manager.
*   ColorBar   colorBar  Contains the definitions for color bar which are
*                        needed by the expose callback to redraw the color
*                        information. 
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*  
********************************************************************************
*/
static void ffg_dc_AddCallbacks ( )
{
   Atom		atom ;
   ColorBar     * colorBar = dc_GetColorBar ( ) ;
   
   /* Window manager callbacks. */
   atom = XmInternAtom(XtDisplay(ffg_displayControlDS),
		       "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(ffg_displayControlDS, atom,
			   ffg_close_display, NULL);
   
   /* Close button callback */
   XtAddCallback(ffg_closeDisplayPB, XmNactivateCallback,
		 ffg_close_display, NULL);
		 
    /* Select button callback */
   XtAddCallback(ffg_selectDisplayPB, XmNactivateCallback,
		 ffg_select_display, NULL);

   /* Clear button callback */
   XtAddCallback ( ffg_clearDisplayPB , XmNactivateCallback ,
                   ffg_clear_display , NULL ) ;

   /* Callbacks for the "FFG Area" option menu. */
   XtAddCallback ( ffg_gridWfoPB , XmNactivateCallback , setup_ffg_source , 
                   ( XtPointer ) WFO_FFG ) ;
   XtAddCallback ( ffg_gridRfcPB , XmNactivateCallback , setup_ffg_source , 
                   ( XtPointer ) RFC_FFG ) ; 

   /* Callbacks for the FFG mode radio buttons. */
   XtAddCallback ( ffg_gridTB , XmNvalueChangedCallback , setup_ffg_mode ,
                   (XtPointer) NULL ) ; 
   XtAddCallback ( ffg_arealTB , XmNvalueChangedCallback , setup_ffg_mode ,
                   (XtPointer) NULL ) ; 
 
   /* Callbacks for the "Id" filter button options.  These values are
      only applicable to gridded RFC ffg processing.  There is currently no
      option to filter by "Id" for WFO ffg data fields. */
   XtAddCallback ( ffg_gridIdAll , XmNactivateCallback , set_ffg_id_filter ,
                   ( XtPointer ) ALL_RFCS ) ;
   XtAddCallback ( ffg_gridIdLoc1PB , XmNactivateCallback ,
                   set_ffg_id_filter , ( XtPointer ) ABRFC ) ;
   XtAddCallback ( ffg_gridIdLoc2PB , XmNactivateCallback , 
                   set_ffg_id_filter , ( XtPointer ) AKRFC ) ;
   XtAddCallback ( ffg_gridIdLoc3PB , XmNactivateCallback , 
                   set_ffg_id_filter , ( XtPointer ) CBRFC ) ;
   XtAddCallback ( ffg_gridIdLoc4PB , XmNactivateCallback , 
                   set_ffg_id_filter , ( XtPointer ) CNRFC ) ;
   XtAddCallback ( ffg_gridIdLoc5PB , XmNactivateCallback , 
                   set_ffg_id_filter , ( XtPointer ) LMRFC ) ;
   XtAddCallback ( ffg_gridIdLoc6PB , XmNactivateCallback , 
                   set_ffg_id_filter , ( XtPointer ) MARFC ) ;
   XtAddCallback ( ffg_gridIdLoc7PB , XmNactivateCallback , 
                   set_ffg_id_filter , ( XtPointer ) MBRFC ) ;
   XtAddCallback ( ffg_gridIdLoc9PB , XmNactivateCallback , 
                   set_ffg_id_filter , ( XtPointer ) NCRFC ) ;
   XtAddCallback ( ffg_gridIdLoc8PB , XmNactivateCallback , 
                   set_ffg_id_filter , ( XtPointer ) NERFC ) ;
   XtAddCallback ( ffg_gridIdLoc10PB , XmNactivateCallback , 
                   set_ffg_id_filter , ( XtPointer ) NWRFC ) ;
   XtAddCallback ( ffg_gridIdLoc11PB , XmNactivateCallback , 
                   set_ffg_id_filter , ( XtPointer ) OHRFC ) ;
   XtAddCallback ( ffg_gridIdLoc12PB , XmNactivateCallback , 
                   set_ffg_id_filter , ( XtPointer ) SERFC ) ;
   XtAddCallback ( ffg_gridIdLoc13PB , XmNactivateCallback , 
                   set_ffg_id_filter , ( XtPointer ) WGRFC ) ;

   /* Callbacks for the "Dur" filter option buttons for gridded FFG mode. */
   XtAddCallback ( ffg_gridDurAllPB , XmNactivateCallback ,
                   set_ffg_duration_filter , 
                   ( XtPointer ) ALL_FFG_DURATIONS ) ; 
   XtAddCallback ( ffg_gridDur1PB , XmNactivateCallback ,
                   set_ffg_duration_filter , ( XtPointer ) FFG_1HR ) ; 
   XtAddCallback ( ffg_gridDur3PB , XmNactivateCallback ,
                   set_ffg_duration_filter , ( XtPointer ) FFG_3HR ) ; 
   XtAddCallback ( ffg_gridDur6PB , XmNactivateCallback ,
                   set_ffg_duration_filter , ( XtPointer ) FFG_6HR ) ; 
   XtAddCallback ( ffg_gridDur12PB , XmNactivateCallback ,
                   set_ffg_duration_filter , ( XtPointer ) FFG_12HR ) ; 
   XtAddCallback ( ffg_gridDur24PB , XmNactivateCallback ,
                   set_ffg_duration_filter , ( XtPointer ) FFG_24HR ) ; 

   /* Callbacks for the gridded ffg display options. These may be
      either to display the gridded data as a GRID or as a BASIN. */
   XtAddCallback ( ffg_gridGridPB , XmNactivateCallback ,
                   set_grid_display_type , ( XtPointer ) FFG_GRID ) ;
   XtAddCallback ( ffg_gridBasinPB , XmNactivateCallback ,
                   set_grid_display_type , ( XtPointer ) FFG_BASIN ) ;

   /* Callbacks for the "Dur" filter option buttons for areal FFG mode. */
   XtAddCallback ( ffg_arealDurAllPB , XmNactivateCallback ,
                   set_ffg_duration_filter , 
                   ( XtPointer ) ALL_FFG_DURATIONS ) ;
   XtAddCallback ( ffg_arealDur1PB , XmNactivateCallback ,
                   set_ffg_duration_filter , ( XtPointer ) FFG_1HR ) ;
   XtAddCallback ( ffg_arealDur3PB , XmNactivateCallback ,
                   set_ffg_duration_filter , ( XtPointer ) FFG_3HR ) ; 
   XtAddCallback ( ffg_arealDur6PB , XmNactivateCallback ,
                   set_ffg_duration_filter , ( XtPointer ) FFG_6HR ) ; 
   XtAddCallback ( ffg_arealDur12PB , XmNactivateCallback ,
                   set_ffg_duration_filter , ( XtPointer ) FFG_12HR ) ; 
   XtAddCallback ( ffg_arealDur24PB , XmNactivateCallback ,
                   set_ffg_duration_filter , ( XtPointer ) FFG_24HR ) ; 

   /* Callbacks for the "Areal Type" filter option buttons for areal FFG
      mode. */
   XtAddCallback ( ffg_arealAllPB , XmNactivateCallback ,
                   set_ffg_areal_filter , ( XtPointer ) FFG_ALL ) ; 
   XtAddCallback ( ffg_arealBasinPB , XmNactivateCallback ,
                   set_ffg_areal_filter , ( XtPointer ) FFG_BASIN ) ; 
   XtAddCallback ( ffg_arealCountyPB , XmNactivateCallback ,
                   set_ffg_areal_filter , ( XtPointer ) FFG_COUNTY ) ; 
   XtAddCallback ( ffg_arealZonePB , XmNactivateCallback ,
                   set_ffg_areal_filter , ( XtPointer ) FFG_ZONE ) ; 

   /* Callbacks for the "Id" and "Val" toggle buttons on the areal ffg
      data options frame. */
   XtAddCallback ( ffg_arealIdTB , XmNvalueChangedCallback ,
                   set_ffg_areal_annotations , 
                   ( XtPointer ) AREAL_ID_TOGGLE_BUTTON ) ;
   XtAddCallback ( ffg_arealValTB , XmNvalueChangedCallback ,
                   set_ffg_areal_annotations , 
                   ( XtPointer ) AREAL_VAL_TOGGLE_BUTTON ) ;

   freeColorBar ( colorBar ) ;
   XtAddCallback ( ffg_legendDA , XmNexposeCallback ,
                   redrawColorBarCallback , colorBar ) ;

   /* Add the event handler for the mouse button press. */
   XtAddEventHandler ( ffg_productsLI , ButtonPressMask , FALSE ,
                       process_clicks , NULL ) ;
   
   return;
}

/*******************************************************************************
* MODULE NUMBER: 16
* MODULE NAME:   ffg_display_show
* PURPOSE:       Creates the FFG data selection GUI.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  Widget      w                    The id of the widget that this
*                                           callback is originating from.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                         HEADER FILE           DESCRIPTION
*   create_ffg_displayControlDS  display_control_ffg.h Builds the FFG data
*                                                      selection gui.
*   ffg_dc_AddCallbacks          Static Above          Adds callbacks to the
*                                                      FFG data selection 
*                                                      gui.
*   read_ffg_product             Static Above          Reads the ffg data
*                                                      available for the
*                                                      default filter settings
*                                                      and displays this 
*                                                      data in the list box.
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
* 
* DATA FILES AND/OR DATABASE:
*   Relies on netCDF files existing for both the RFC and WFO areas.
*   See the documentation for the read_ffg_product routine above.
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
void ffg_display_show ( Widget w )
{
    if ( ( ffg_displayControlDS == NULL ) || ( ! ffg_displayControlDS ) )
    {   
       /* create all the widgets */
 	   create_ffg_displayControlDS ( GetTopShell ( w ) );
	
	   /* set up the callbacks */
       ffg_dc_AddCallbacks( );
       
       /* Initialize the default FFG colors. */
       build_ffg_colors ( );
    }
   
    /* Read in the ffg products. */
    read_ffg_product ( ) ;

    XtManageChild ( ffg_displayControlDS ) ; 
    XtManageChild ( ffg_displayControlFO ) ;
    XtUnmanageChild ( ffg_areal_optionsFR ) ;
    DeSensitize ( ffg_gridIdOM ) ;

    /* Set the states of the grid and areal toggle buttons in the 
       radio box. This in turn will correctly configure the FFG control
       GUI for gridded FFG mode and load the data into the FFG selection
       scrolled list.*/
    XmToggleButtonSetState ( ffg_gridTB , True , True ) ;

    /* Set the states of the areal value and id label buttons. */
    XmToggleButtonSetState ( ffg_arealIdTB , True , False ) ;
    XmToggleButtonSetState ( ffg_arealValTB , True , False ) ;

    toggle_areal_annotations ( AREAL_ID_TOGGLE_BUTTON , True ) ;
    toggle_areal_annotations ( AREAL_VAL_TOGGLE_BUTTON , True ) ;
   
    XtPopup( ffg_displayControlDS , XtGrabNone ) ;
    
    return ;
}
