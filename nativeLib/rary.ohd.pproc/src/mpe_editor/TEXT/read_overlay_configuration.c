
/* FILENAME:            read_overlay_configuration.c
* NUMBER OF MODULES:   3
* GENERAL INFORMATION:
*   MODULE 1:          define_path
* DESCRIPTION:         Builds the file paths and names from information
*                      read from the overlay configuration file on an
*                      overlay by overlay basis.
*
*   MODULE 2:          free_file_memory
* DESCRIPTION:         Deallocates the memory that is used in creating
*                      the arrays containing the file paths and file names
*                      in the
*   MODULE 3:          read_overlay_configuration
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #   DATE         PROGRAMMER        DESCRIPTION/REASON
*          3   1/13/2006    Bryon Lawrence    Modified to read
*                                             mpe_editor_overlay_configuration 
********************************************************************************
*/
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <Xm/ToggleB.h>

#include "external_plot_routines.h"
#include "GeneralUtil.h"
#include "map_defines.h"
#include "map_library.h"
#include "map_menubar_cb.h"
#include "mpe_log_utils.h"
#include "plot_routines.h"
#include "read_overlay_configuration.h"
#include "rfcwide_callbacks.h"
#include "map_menubar.h"

#define MAX_USER_OVERLAYS 21
#define NUM_OVERLAYS 50

  extern Widget map_menu_items [];
  extern void _create_menu_separator(Widget pane);

/* A global variable containing the string equivalents of the
   MapOverlayTypes enumeration. */
extern char * MapOverlayTypes [ ] ;

/* A global variable containing the string equivalents of the 
   OverlayNames enumeration. */
extern char * OverlayNames [ ] ;


/* An array of function pointers containing the routines that correspond
   to the elements in the CalcRoutines enumeration . This array
   must be modified as the CalcRoutines enum is modified. */ 
static OverlayCalculatorRoutine calc_routines [ ] = { _show_radar_rings ,
                                                     _draw_lat_lon_lines ,
                                                     _draw_radar_locations ,
                                                     _draw_fsl_city_locations ,
                                                     _draw_mpe_city_locations ,
                                                     _draw_whfs_city_locations ,
                                                     _draw_hrap_grid ,
                                                     _draw_hrap_boundary,
                                                     _draw_topography,
                                                     _contour_topo } ;
                     

/* An array of function pointer containing the routines that correspond
   to the elements in the DatabaseRoutines enumeration.  This array
   must be modified as the DatabaseRoutines enumeration in the
   "external_plot_routines.h" file is modified. */
static ExternalOverlayRoutine external_routines [ ] = {_plot_binfile_basins , 
						    _plot_binfile_lakes ,
						    _plot_binfile_rivers_streams ,
						    _plot_binfile_hiways_roads } ; 
/* An array containing the string equivalents of the _CalcRoutines 
   enumeration.  This array must be modified as the CalcRoutines 
   enumeration is modified. */
static char * CalcRoutines [ ] = { "DRAW_RADAR_RINGS" , "DRAW_LAT_LON_LINES" ,
                                   "DRAW_RADAR_LOCATIONS" , 
                                   "DRAW_FSL_CITY_LOCATIONS" ,
                                   "DRAW_MPE_CITY_LOCATIONS" ,
                                   "DRAW_WHFS_CITY_LOCATIONS" ,
                                   "DRAW_HRAP_GRID" ,
				   "DRAW_HRAP_BOUNDARY",
                                   "DRAW_TOPOGRAPHY",
                                   "DRAW_TOPOGRAPHY_CONTOURS" } ;

/* An array containing the string equivalents of the ExternalRoutinesEnum
   enumeration.  This array must be modified as the ExternalRoutinesEnum
   enumeration in the "external_plot_routines.h" file is modified. */
static char * ExternalRoutines [ ] = { "PLOT_BINFILE_BASINS" , 
                                       "PLOT_BINFILE_LAKES" , 
				       "PLOT_BINFILE_RIVERS_STREAMS" , 
				       "PLOT_BINFILE_HIWAYS_ROADS" , 
				       "END_OF_BINFILE_ROUTINES " } ;

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   define_path
*
* PURPOSE:       This routine constructs the file path(s) and the file name(s)
*                for each overlay from the information parsed from the
*                overlay configuration file.  There may be more than one 
*                file path and name for each overlay.
*
*                The overlay configuration file allows tokens to be 
*                specified to use in creating the filename and path.
*                In order to be recognized by this routine, these tokens
*                must be preceded by a '$' in the overlay configuration file.
*                
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME         DESCRIPTION/UNITS
*   Input  char *      token        The token parsed from the overlay
*                                   configuration file which contains the
*                                   path or filename of the overlay data
*                                   which needs to be interpreted and
*                                   processed into a true path or filename. 
*   I/O    char **     filepath     The constructed filepath is returned to
*                                   the user via this variable.
*   Input  int         max_length   The maximum length of the returned 
*                                   filename. 
*
* RETURNS:
*   DATA TYPE   NAME     DESCRIPTION
*   int         status   A value indicating the success or failure of 
*                        this routine.   
*
* APIs UTILIZED:
*   NAME                 HEADER FILE     DESCRIPTION
*   get_apps_defaults    GeneralUtil.h   Retrieves the value of an 
*                                        environmental variable or a token
*                                        in an apps defaults file.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   char *     pDelimiter
*   char *     pStart
*   char *     pToken
*   char *     pString
*   char       reply
*   int        num_chars
*   int        reply_len
*   int        request_len
*   int        status
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                            DESCRIPTION
*
********************************************************************************
*/
static int define_path ( char * token , 
                         char ** filepath ,
                         int max_length )
{
   char * pDelimiter = NULL ;
   char * pStart = token ;
   char * pToken = NULL ;
   char * pString = NULL ;
   char reply [ LEN_REPLY ] ;
   int num_chars ;
   int reply_len ;
   int request_len ;
   int status ;
   char * pDollar = NULL ;
  
   if ( token == NULL || *token == '\0' )
   {
      *filepath = NULL ;
      return M_ERROR ;
   }

   /* Trim out any leading blank spaces. */
   while ( ( *pStart != '\0' ) && isspace ( *pStart ) )
   {
      pStart ++ ;
   }

   /* Allocate memory in pString to contain the file/path information. */
   pString = ( char * ) malloc ( sizeof ( char ) * max_length ) ;
   memset ( pString , '\0' , max_length ) ;

   if ( pString == NULL )
   {
      *filepath = NULL ;
      return M_ERROR ;
   }

   /* Check to see if there are any environmental variables
      denoted by a '$' character in the path.  All environmental
      variables must be expanded to their meaning. */
   while ( ( pStart != NULL ) && ( *pStart != '\0' ) )
   {
      pDelimiter = strchr ( pStart , '/' ) ;

      if ( pDelimiter == pStart )
      {
         strcat ( pString , "/" ) ; 
      }
      else
      { 
         if ( pDelimiter == NULL )
         {
             num_chars = strlen ( pStart ) ;
         }
         else
         {
            num_chars = pDelimiter - pStart ;
         }

         pToken = ( char * ) malloc ( sizeof ( char ) * num_chars + 1 ) ;

         if ( pToken == NULL )
         {
            if ( pString != NULL )
            {
               free ( pString ) ;
               pString = NULL ;
            }

            *filepath = NULL ;
            return M_ERROR ;
         }

         strncpy ( pToken , pStart , num_chars ) ;
         pToken [ num_chars ] = '\0' ;
         
         num_chars = strlen (pToken) ;
         
         /* Does pToken contain a '$' ? */
          
          pDollar = strchr (pToken , '$' );
          if ( pDollar != NULL)
          {
            request_len = strlen ( pDollar + 1 ) ;
            if ( request_len == 0 )
            {
               if ( pString != NULL )
               {
                  free ( pString ) ;
                  pString = NULL ;
               }

               if ( pDollar != NULL )
               {
                  free ( pDollar ) ;
                  pDollar = NULL ;
               }
             
               *filepath = NULL ;
               return M_ERROR ;
            }
         /*   pToken = pDollar ; */

            status = get_apps_defaults ( ( pDollar + 1 ) ,  & request_len , reply ,
                                         & reply_len ) ;
            if ( status != 0 )
            {
               if ( pString != NULL )
               {
                  free ( pString ) ;
                  pString = NULL ;
               }

               if ( pDollar != NULL )
               {
                  free ( pDollar ) ;
                  pDollar = NULL ;
               }

               *filepath = NULL ;
               return M_ERROR ;
            }

            if ( ( strlen ( pString ) + reply_len + ( pDollar  - pToken  ) ) <
                   max_length ) 
            {
               if ( ( strlen ( pString ) ) > 0 )
               {
                  strcat ( pString , "/" ) ;
               }
               if ( pDollar != pToken )
               {
                  strncat ( pString , pToken, pDollar - pToken ) ;
               }
               strcat ( pString ,  reply ) ;
            }
            else
            {
               if ( pString != NULL )
               {
                  free ( pString ) ;
                  pString = NULL ;
               }

               if ( pDollar != NULL )
               {
                  free ( pDollar ) ;
                  pDollar = NULL ;
               }

               *filepath = NULL ;
               return M_ERROR ;
            }
         }
         else
         {
            if ( ( ( strlen ( pString ) ) + 
                   ( strlen ( pToken ) ) ) < max_length )  
            {
               if ( ( strlen ( pString ) ) > 0 )
               {
                  strcat ( pString , "/" ) ;
               }
                  strcat ( pString ,  pToken ) ;
            }
            else
            {
               if ( pString != NULL )
               {
                  free ( pString ) ;
                  pString = NULL ;
               }

               if ( pToken != NULL )
               {
                  free ( pToken ) ;
                  pToken = NULL ;
               }

               *filepath = NULL ;
               return M_ERROR ;
            }
         }
      }

      /* Advance the pStart character pointer. */
      if ( pDelimiter != NULL )
      {
         pStart = pDelimiter + 1 ;
      }
      else
      {
         pStart = NULL ; 
      }

      if ( pToken != NULL )
      {
         free ( pToken ) ;
         pToken = NULL ;
      }
   }
   
   *filepath = pString ;
   return M_OK ;
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   free_file_memory
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
*    ERROR CODE                            DESCRIPTION
*
********************************************************************************
*/
static void free_file_memory ( char * filepath [ ] ,
                               char * filename [ ] ,
                               const int n_files )
{
   int i ;  /* A loop indexing variable. */
 
   /* Free the memory used by the pathname and filename. */
   if ( n_files > 0 )
   {
      for ( i = 0 ; i < n_files ; i++ )
      {
         if ( filepath [ i ] != NULL )
         {
            free ( filepath [ i ] ) ;
            filepath [ i ] = NULL ;
         }

         if ( filename [ i ] != NULL )
         {
            free ( filename [ i ] ) ;
            filename [ i ] = NULL ;
         }
      }
   }
}


void deSensitizeItems()
{
  extern void   DeSensitize(Widget w);
  
  DeSensitize ( map_menu_items[map_overlay_states]);
  DeSensitize ( map_menu_items[map_overlay_basins]);
  DeSensitize ( map_menu_items[map_overlay_basin_names]);
  DeSensitize ( map_menu_items[map_overlay_city]);
  DeSensitize ( map_menu_items[map_overlay_county]);
  DeSensitize ( map_menu_items[map_overlay_cwa]);
  DeSensitize ( map_menu_items[map_overlay_hsa]);
  //DeSensitize ( map_menu_items[map_overlay_highways_cascade]);
  DeSensitize ( map_menu_items[map_overlay_highways]);
  DeSensitize ( map_menu_items[map_overlay_highways_and_roads]);
  DeSensitize ( map_menu_items[map_overlay_highways_only]);
  DeSensitize ( map_menu_items[map_hrap_grid]);
  //DeSensitize ( map_menu_items[map_overlay_streams_lakes_cascade]);
  DeSensitize ( map_menu_items[map_overlay_streams_lakes]);
  DeSensitize ( map_menu_items[map_overlay_lat_lon]);
  DeSensitize ( map_menu_items[map_overlay_rfc]);
  DeSensitize ( map_menu_items[map_overlay_radar_locations]);
  DeSensitize ( map_menu_items[map_overlay_radar_rings]);
  DeSensitize ( map_menu_items[map_overlay_timezones]);
  //DeSensitize ( map_menu_items[map_overlay_topography_cascade]);
  DeSensitize ( map_menu_items[map_overlay_topography]);
  DeSensitize ( map_menu_items[map_overlay_topo]);
  DeSensitize ( map_menu_items[map_overlay_topo_contour]);
  DeSensitize ( map_menu_items[map_overlay_zones]);
}



/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   read_overlay_configuration
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
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
*    ERROR CODE                            DESCRIPTION
*
********************************************************************************
*/

#define OVERLAY_PRESENT 1
#define OVERLAY_NOT_PRESENT 0

int read_overlay_configuration ( )
{

  FILE * pFile = NULL ;
  char   color_name [ COLOR_NAME_LENGTH ] ;
  char * filename [ MAX_NUMBER_OF_FILES ] ;
  char * filepath [ MAX_NUMBER_OF_FILES ] ;
  static char * hv_config_file_name = "mpe_editor_overlay_configuration" ;
  static char * hv_config_dir_token = "hv_config_dir" ;
  char * pDelimiter = NULL ;
  char * pRecord = NULL ;
  char * pStart = NULL ;
  char reply [ LEN_REPLY ] ;
  char * token = NULL ;
  char record [ MAX_RECORD_LENGTH ] ;
  char overlay_path [ MAX_RECORD_LENGTH ] ;
  enum _Fields counter = FieldId ;
  int overlay_id = M_TOPOGRAPHY ; 
  enum MapOverlayTypes overlay_type = M_SHAPE ;
  enum MapState state = M_OFF ;
  enum MapState store_in_memory = M_OFF ;
  enum MapState fillarea = M_OFF ;
  int file_position = 0 ;
  int i ;
  int length = 0 ;
  int line_width = DEFAULT_LINE_WIDTH ;
  int n_files = 0 ;
  int overlays_present [ NUM_OVERLAYS ] = { OVERLAY_NOT_PRESENT } ;
  int reply_len ;
  int request_len ;
  int status ;
  int total_overlays_counter = 0;
  int dummy_user_overlay_ids = 0;
  OverlayCalculatorRoutine pCalcRoutine = NULL ;
  ExternalOverlayRoutine pExternalRoutine = NULL ;

  int user_overlay = 0;
  int highways = 0;
 
  extern void Sensitize(Widget w);

  char overlay_names[NUM_OVERLAYS][MAX_RECORD_LENGTH];
  int index_in_overlay_names[MAX_USER_OVERLAYS];
  int j;

  deSensitizeItems();
  
  for(j=0;j<NUM_OVERLAYS;j++)
  {
     memset(overlay_names[j], '\0', MAX_RECORD_LENGTH);
  }
  memset(index_in_overlay_names, -1, MAX_USER_OVERLAYS);

  /* Retrieve the path of the overlay configuration
     file from the "apps defaults" file. */
  request_len = strlen ( hv_config_dir_token ) ;
  status = get_apps_defaults ( hv_config_dir_token , & request_len ,
                               reply , & reply_len ) ;
  
  if ( status != 0 )
  {
     flogMessage ( stderr , "\nIn routine \"read_overlay_configuration\":\n"
                        "Could not retrieve the path of the hydroview\n"
                        "configuration directory from the environment\n"
                        "or apps defaults token file.\n" ) ;
     return M_ERROR ;
  }

  if ( reply_len >= MAX_RECORD_LENGTH )
  {
     flogMessage ( stderr , "\nIn routine \"read_overlay_configuration\":\n"
                        "The length of the hydroview local data directory\n"
                        "path is %d.  This is greater than %d characters,\n"
                        "the amount of storage allocated for this\n"
                        "information.\n" , reply_len ,
                        ( MAX_RECORD_LENGTH - 1 ) ) ;
     return M_ERROR ;
  }

  strcpy ( overlay_path , reply ) ;
  strcat ( overlay_path , "/" ) ;
  strcat ( overlay_path , hv_config_file_name ) ;

 
  /* Open the configuration file. */
  pFile = fopen ( overlay_path , "r" ) ;

  if ( pFile == NULL )
  {
     flogMessage ( stderr , "\nIn routine \"read_overlay_configuration\":\n"
                        "Could not open file %s to retrieve the hydroview\n"
                        "overlay configuration data.\n" , overlay_path ) ;
     return M_ERROR ;
  }

  /* Initialize the filename and filepath arrays. */
  for ( i = 0 ; i < MAX_NUMBER_OF_FILES ; i ++ )
  {
     filepath [ i ] = NULL ;
     filename [ i ] = NULL ;
  }
  
  pRecord = fgets ( record , MAX_RECORD_LENGTH , pFile ) ;

  while ( pRecord != NULL )
  {
     
     total_overlays_counter++;
     
     /* Create the token. */   
     pStart = record ;

     if ( *pStart == '|' ) pStart ++ ;

     pDelimiter = strchr ( pStart , '|' ) ;

     if ( pDelimiter != NULL )
     {
        length = pDelimiter - pStart ;

        token = ( char * ) malloc ( sizeof ( char * ) * ( length + 1 ) ) ;

        if ( token == NULL )
        {
           /* Handle malloc error here. */
        }

        if ( length > 0 ) strncpy ( token , pStart , length ) ;

        token [ length ] = '\0' ;
     }

     file_position = 0 ;

     
     
     while ( ( token != NULL ) && ( counter < FieldAllDone ) )
     {
         if ( length > 0 )
         { 
            switch ( counter )
            {
               case FieldId :

		   if(total_overlays_counter < NUM_OVERLAYS)
		   {
		   	overlays_present [ total_overlays_counter-1 ] = OVERLAY_PRESENT ;
		   }
		   strcpy(overlay_names[total_overlays_counter-1],token);
                   break ;

               case FieldState :

		   if ( strcmp ( token , "M_ON" ) == 0 )
                   {
                      state = M_ON ;
                   }   
                   else if ( strcmp ( token , "M_OFF" ) == 0 )
                   {
                      state = M_OFF ;
                   }
                   else
                   {
                     if(total_overlays_counter <= NUM_OVERLAYS)
		      {
		      	flogMessage ( stderr , 
                              "\nIn routine \"read_overlay_configuration\":\n"
                              "Unrecognized field state %s for overlay %s\n"
                              "read from overlay configuration file %s.\n" ,
                              token , overlay_names[total_overlays_counter-1] ,
                              overlay_path ) ;
		      }
                      if ( token != NULL )
                      {
                         free ( token ) ;
                         token = NULL ;
                      }

                      fclose ( pFile ) ;
                      //return M_ERROR ;
                   }
          
                   break ;

               case FieldMemory :

                   if ( strcmp ( token , "M_ON" ) == 0 )
                   {
                      store_in_memory = M_ON ;
                   }   
                   else if ( strcmp ( token , "M_OFF" ) == 0 )
                   {
                      store_in_memory = M_OFF ;
                   }
                   else
                   {
                      if(total_overlays_counter <= NUM_OVERLAYS)
		      {
		      flogMessage ( stderr , 
                              "\nIn routine \"read_overlay_configuration\":\n"
                              "Unrecognized memory state %s for overlay %s\n"
                              "read from overlay configuration file %s.\n" ,
                              token , overlay_names[total_overlays_counter-1] ,
                              overlay_path ) ;
		      }
                      if ( token != NULL )
                      {
                         free ( token ) ;
                         token = NULL ;
                      }

                      fclose ( pFile ) ;
                      //return M_ERROR ;
                   }
          
                   break ;

               case FieldFill :

                   if ( strcmp ( token , "M_ON" ) == 0 )
                   {
                      fillarea = M_ON ;
                   }   
                   else if ( strcmp ( token , "M_OFF" ) == 0 )
                   {
                      fillarea = M_OFF ;
                   }
                   else
                   {
                      if(total_overlays_counter <= NUM_OVERLAYS)
		      {
		      flogMessage ( stderr , 
                              "\nIn routine \"read_overlay_configuration\":\n"
                              "Unrecognized fill state %s for overlay %s\n"
                              "read from overlay configuration file %s.\n" ,
                              token , overlay_names[total_overlays_counter-1] ,
                              overlay_path ) ;
		      }
                      if ( token != NULL )
                      {
                         free ( token ) ;
                         token = NULL ;
                      }

                      fclose ( pFile ) ;
                      //return M_ERROR ;
                   }
          
                   break ;

               case FieldColor :

                   memset ( color_name , '\0' , COLOR_NAME_LENGTH ) ; 
                   length = strlen ( token ) ;

		   if ( ( length > 0 ) && ( length < COLOR_NAME_LENGTH ) )
                   {
                      strcpy ( color_name , token ) ; 
                   }
                   else if ( length <= 0 )
                   {
                      if(total_overlays_counter <= NUM_OVERLAYS)
		      {
		      flogMessage ( stderr , 
                              "\nIn routine \"read_overlay_configuration\":\n"
                              "A color name must be supplied for overlay %s\n"
                              "read from overlay configuration file %s.\n"
                              "Using a default color of %s.\n" ,
                              overlay_names[total_overlays_counter-1] , overlay_path ,
                              DEFAULT_COLOR_NAME ) ;
                      }
		      strcpy ( color_name , DEFAULT_COLOR_NAME ) ;
                   }
                   else
                   {
                      if(total_overlays_counter <= NUM_OVERLAYS)
		      {
		      flogMessage ( stderr ,
                              "\nIn routine \"read_overlay_configuration\":\n"
                              "Color name %s is too long.  Must be %d\n"
                              "characters or less.  Overlay name %s. Read\n"
			      "from overlay configuration file %s.  Using\n"
			      "a default color of %s.\n" , 
                               token , COLOR_NAME_LENGTH - 1 ,
                               overlay_names[total_overlays_counter-1] ,
                               overlay_path , DEFAULT_COLOR_NAME ) ;
                      }
		      strcpy ( color_name , DEFAULT_COLOR_NAME ) ;
                   }
			    
                   break ;

               case FieldLine :

		   /* Test the line width to make sure it is a number. */
                   if ( isdigit ( *token ) != 0 )
                   {
                      line_width = atoi ( token ) ;
                   }
                   else
                   {
                      if(total_overlays_counter <= NUM_OVERLAYS)
		      {
		      flogMessage ( stderr , 
                              "\nIn routine \"read_overlay_configuration\":\n"
                              "Unrecognized line width value %s for\n"
			      "overlay %s read from overlay configuration\n"
			      "file %s.  Using default line width of %d.\n" ,
                              token , overlay_names[total_overlays_counter-1] ,
                              overlay_path , DEFAULT_LINE_WIDTH ) ;
		      }
                   }

                   break ;

               case FieldNumber :
                
                   if ( isdigit ( *token ) != 0 ) 
                   {
                      n_files = atoi ( token ) ;
                   }
                   else
                   {
                      if(total_overlays_counter <= NUM_OVERLAYS)
		      {
		      flogMessage ( stderr , 
                              "\nIn routine \"read_overlay_configuration\":\n"
                              "Unrecognized number of files %s for overlay %s\n"
                              "read from overlay configuration file %s.\n" ,
                              token , overlay_names[total_overlays_counter-1] ,
                              overlay_path ) ;
		      }
                      if ( token != NULL )
                      {
                         free ( token ) ;
                         token = NULL ;
                      }

                      fclose ( pFile ) ;
                      return M_ERROR ;
                   }

                   break ;

               case FieldPath :

                   if ( n_files > 0 )
                   { 
                      status = define_path ( token ,
                                             & filepath [ file_position ] , 
                                             OVERLAY_PATH_LEN ) ;

                      if ( status != M_OK )
                      {
                         if(total_overlays_counter <= NUM_OVERLAYS)
			 {
			 flogMessage ( stderr , 
                              "\nIn routine \"read_overlay_configuration\":\n"
                              "Error encountered by routine \"define_path\"\n"
                              "while processing file paths for overlay %s\n"
                              "read from overlay configuration file %s.\n" ,
                              overlay_names[total_overlays_counter-1] , overlay_path ) ;
		         }
                         if ( token != NULL )
                         {
                            free ( token ) ;
                            token = NULL ;
                         }

                         free_file_memory ( filepath , filename , n_files ) ;
                         fclose ( pFile ) ;
                         return status ;
                      }
                   }
         
                   break ;

               case FieldFile :

                   if ( n_files > 0 )
                   {
                      status = define_path ( token ,
                                             & filename [ file_position ] ,
                                             OVERLAY_FILENAME_LEN ) ;
                      if ( status != M_OK )
                      {
                         if(total_overlays_counter <= NUM_OVERLAYS)
			 {
			 flogMessage ( stderr , 
                              "\nIn routine \"read_overlay_configuration\":\n"
                              "Error encountered by routine \"define_path\"\n"
                              "while processing file names for overlay %s\n"
                              "read from overlay configuration file %s.\n" ,
                              overlay_names[total_overlays_counter-1] , overlay_path ) ;
		        }
                         if ( token != NULL )
                         {
                            free ( token ) ;
                            token = NULL ;
                         }

                         free_file_memory ( filepath , filename , n_files ) ;
                         fclose ( pFile ) ;
                         return status ;
                      }

                      file_position ++ ;

                      if ( file_position < n_files ) 
                      {
                         counter = FieldNumber ;
                      }
                   }

                   break ;

               case FieldType :

                   for ( i = ( int ) M_SHAPE ; i < ( int ) M_OVERLAY_END ;i++ )  
                   {
                      if ( strcmp  ( MapOverlayTypes [ i ] , token ) == 0 ) 
                      {
                         break ;
                      }
                   }

                   if ( i == ( int ) M_OVERLAY_END )
                   {
                      if(total_overlays_counter <= NUM_OVERLAYS)
		      {
		      flogMessage ( stderr , 
                              "\nIn routine \"read_overlay_configuration\":\n"
                              "Unrecognized field type %s for overlay %s\n"
                              "read from overlay configuration file %s.\n" ,
                              token , overlay_names[total_overlays_counter-1] ,
                              overlay_path ) ;
		      }
                      if ( token != NULL )
                      {
                         free ( token ) ;
                         token = NULL ;
                      }

                      free_file_memory ( filepath , filename , n_files ) ;
                      fclose ( pFile ) ;
                      return M_ERROR ;
                   }

                   overlay_type = ( enum  MapOverlayTypes ) i ; 
                   break ;
 
               case FieldRoutine :

                   if ( ! isspace ( * token ) )
                   {
     
                      for ( i = ( int ) DRAW_RADAR_RINGS ; 
                            i < ( int ) END_OF_CALC_ROUTINES ; i++ )  
                      {
                         if ( strcmp ( CalcRoutines [ i ] , token ) == 0 )
                         {
                            pCalcRoutine = calc_routines [ i ] ;
                            break ;
                         }
                      }

                      if ( i == ( int ) END_OF_CALC_ROUTINES )
                      {
                         /* Maybe this is a Database routine? */
                         for ( i = 0 ;
                               i < ( int ) END_OF_BINFILE_ROUTINES ; i++ )
                         {
                            if ( strcmp ( ExternalRoutines [ i ] , 
                                          token ) == 0 )
                            {
                               pExternalRoutine = external_routines [ i ] ;
                               break ;
                            }
                         }

                         if ( i == ( int ) END_OF_BINFILE_ROUTINES )
                         {
                            if(total_overlays_counter <= NUM_OVERLAYS)
			    {
			    flogMessage ( stderr ,
                               "\nIn routine \"read_overlay_configuration\":\n"
                               "Unrecognized external routine %s for \n"
                               "overlay %s read from overlay configuration\n"
                               "file %s.\n" , token , 
                               overlay_names[total_overlays_counter-1] , overlay_path ) ;
			    }
                            if ( token != NULL )
                            {
                               free ( token ) ;
                               token = NULL ;
                            }

                            free_file_memory ( filepath , filename , 
                                               n_files ) ;
                            fclose ( pFile ) ;
                            return M_ERROR ;
                         }
                      }

                   }

                   break ;
 
              case FieldAllDone :

                   /* Should never get here.  This was added to keep the 
                      gcc compiler happy. */
                   break ;
           }
        }

        counter ++ ;

        if ( token != NULL )
        {
           free ( token ) ;
           token = NULL ;
        }

        pStart = pDelimiter + 1 ;
        pDelimiter = strchr ( pStart , '|' ) ;

        if ( pDelimiter != NULL )
        {
           length = pDelimiter - pStart ;

           token = ( char * ) malloc ( sizeof ( char * ) * ( length + 1 ) ) ;

           if ( token == NULL )
           {
              /* Handle malloc error here. */
           }

           if ( length > 0 ) strncpy ( token , pStart , length ) ;

           token [ length ] = '\0' ;
        }

     } 

     
     /* Attempt to define the overlay. */
    
     for(i=0;i<M_OVERLAYS;i++)
     {
        user_overlay = 1;
        if(!strcmp(OverlayNames[i], overlay_names[total_overlays_counter-1]))
	{
	   overlay_id = i;
	   user_overlay = 0;
	   break;
	}
     }
	   if(user_overlay == 1)
	   {
	      dummy_user_overlay_ids--;

	      index_in_overlay_names[-1*(dummy_user_overlay_ids+1)] = total_overlays_counter-1;

	      if(dummy_user_overlay_ids == -1)
	      {
	         _create_menu_separator(map_menu_items[map_overlay_pane]);
	      }
	   }
     
     if(user_overlay != 1)
     {
        status = mDefineOverlay ( overlay_id ,
                                  state ,
                                  color_name ,
			          line_width ,
                                  filepath ,
                                  filename ,
                                  n_files ,
                                  overlay_type ,
                                  store_in_memory ,
                                  fillarea ,
                                  pCalcRoutine ,
                                  pExternalRoutine ) ;
     }
     else
     {
        status = mDefineOverlay ( dummy_user_overlay_ids ,
                                  state ,
                                  color_name ,
			          line_width ,
                                  filepath ,
                                  filename ,
                                  n_files ,
                                  overlay_type ,
                                  store_in_memory ,
                                  fillarea ,
                                  pCalcRoutine ,
                                  pExternalRoutine ) ;
				  
     
        user_overlay = 0;
     }

     if ( status != M_OK )
     {
        free_file_memory ( filepath , filename , n_files ) ;
        return status ;
     }

     free_file_memory ( filepath , filename , n_files ) ;
     pCalcRoutine = NULL ;
     pExternalRoutine = NULL ;
     file_position = 0 ;
     counter = FieldId ;
     
     /* Retrieve the next record from the file. */ 
     pRecord = fgets ( record , MAX_RECORD_LENGTH , pFile ) ;

  }
    
     set_user_overlays_counter_and_names(dummy_user_overlay_ids * -1, overlay_names);

    Widget wid[dummy_user_overlay_ids*-1];
    
     for(i = 0;i<(dummy_user_overlay_ids*-1);i++)
     {
	_create_menu_item_toggle(map_menu_items[map_overlay_pane], &wid[i],
				overlay_names[index_in_overlay_names[i]],'V',NULL,NULL);
	XtAddCallback(wid[i],XmNvalueChangedCallback,
		     (XtCallbackProc) _set_overlay,(XtPointer)(-1*(i+1)));
	if ( _get_overlay_status (-1*(i+1)) == M_ON )
	{
		XmToggleButtonSetState(wid[i],True,False);
		_set_overlay_status ( -1*(i+1) , M_ON ) ;
	}
	else
	{
		XmToggleButtonSetState(wid[i],False,False);
		_set_overlay_status ( -1*(i+1) , M_OFF ) ;
	}
     }
   
     for(i=0;i<total_overlays_counter;i++)
     {
        if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_TOPOGRAPHY"))
	{
	   Sensitize (map_menu_items[map_overlay_topography]);
	   Sensitize (map_menu_items[map_overlay_no_topo]);
	   Sensitize (map_menu_items[map_overlay_topo]);
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_STATE"))
	{
	   Sensitize (map_menu_items[map_overlay_states]);
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_BASINS"))
	{
	   Sensitize (map_menu_items[map_overlay_basins]); 
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_CITY_TOWN"))
	{
	   Sensitize (map_menu_items[map_overlay_city]); 
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_COUNTY"))
	{
	   Sensitize (map_menu_items[map_overlay_county]); 
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_CWA"))
	{
	   Sensitize (map_menu_items[map_overlay_cwa]); 
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_HIGHWAYS"))
	{
	   Sensitize (map_menu_items[map_overlay_highways_only]);
	   Sensitize (map_menu_items[map_overlay_no_highways]);
	   Sensitize (map_menu_items[map_overlay_highways]);
	   highways = 1;
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_ROADS"))
	{
	   if(highways == 1)
	   {
	      Sensitize (map_menu_items[map_overlay_highways_and_roads]);
	   }
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_HRAP_GRID"))
	{
	   Sensitize (map_menu_items[map_hrap_grid]); 
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_LAKES"))
	{
	   Sensitize (map_menu_items[map_overlay_streams_lakes]); 
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_RIVERS"))
	{
	   Sensitize (map_menu_items[map_overlay_rivers]); 
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_STREAMS"))
	{
	   Sensitize (map_menu_items[map_overlay_streams]); 
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_LAT_LON_LINES"))
	{
	   Sensitize (map_menu_items[map_overlay_lat_lon]); 
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_RADAR_LOCATIONS"))
	{
	   Sensitize (map_menu_items[map_overlay_radar_locations]); 
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_RADAR_RINGS"))
	{
	   Sensitize (map_menu_items[map_overlay_radar_rings]); 
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_RFC_BOUNDARY"))
	{
	   Sensitize (map_menu_items[map_overlay_rfc]); 
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_TIMEZONE"))
	{
	   Sensitize (map_menu_items[map_overlay_timezones]); 
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_ZONES"))
	{
	   Sensitize (map_menu_items[map_overlay_zones]); 
	}
        else if(overlays_present[i] == OVERLAY_PRESENT && !strcmp(overlay_names[i],"M_TOPOGRAPHY_CONTOUR"))
	{
	   Sensitize (map_menu_items[map_overlay_topo_contour]); 
	}
     }


{
    
    
   if ( _get_overlay_status ( M_STATE ) == M_ON )
   {
       XmToggleButtonSetState(map_menu_items[map_overlay_states],True,False);
   }
    
    // Determine whether or not the user has specified an "ON" state for
 //    one of these radio button in the overlay configuration file. 
  if ( _get_overlay_status ( M_STREAMS ) == M_ON )
  {
    XmToggleButtonSetState ( map_menu_items[map_overlay_streams],True,False);
    _set_overlay_status ( M_RIVERS , M_OFF ) ;
    _set_overlay_status ( M_NOSTREAMS , M_OFF ) ;

    // Lakes / Reservoirs! 
    _set_overlay_status ( M_LAKES , M_ON ) ;
  }
  else if ( _get_overlay_status ( M_RIVERS ) == M_ON )
  {
    XmToggleButtonSetState(map_menu_items[map_overlay_rivers],True,False);
    _set_overlay_status ( M_STREAMS , M_OFF ) ;
    _set_overlay_status ( M_NOSTREAMS , M_OFF ) ;

    // Lakes / Reservoirs are always on! 
    _set_overlay_status ( M_LAKES , M_ON ) ;
  }
  else 
  {
    XmToggleButtonSetState(map_menu_items[map_overlay_no_streams],True,False);
    _set_overlay_status ( M_RIVERS , M_OFF ) ;
    _set_overlay_status ( M_STREAMS , M_OFF ) ;
    _set_overlay_status ( M_LAKES , M_OFF ) ;
  }
  if ( _get_overlay_status ( M_COUNTY ) == M_ON )
    XmToggleButtonSetState(map_menu_items[map_overlay_county],True,False);
  if ( _get_overlay_status ( M_CWA ) == M_ON )
    XmToggleButtonSetState(map_menu_items[map_overlay_cwa],True,False);
  if ( _get_overlay_status ( M_ZONES ) == M_ON )
    XmToggleButtonSetState(map_menu_items[map_overlay_zones],True,False);
  // Determine whether or not the user has specified an "ON" state for
   //  one of these radio button in the overlay configuration file. 
  if ( _get_overlay_status ( M_TOPOGRAPHY ) == M_ON )
  {
    XmToggleButtonSetState ( map_menu_items[map_overlay_topo],True,False);
    _set_overlay_status ( M_TOPOGRAPHY_CONTOUR , M_OFF ) ;
    _set_overlay_status ( M_TOPOGRAPHY_NONE , M_OFF ) ;
  }
  else if ( _get_overlay_status ( M_TOPOGRAPHY_CONTOUR ) == M_ON )
  {
    XmToggleButtonSetState(map_menu_items[map_overlay_topo_contour],True,False);
    _set_overlay_status ( M_TOPOGRAPHY , M_OFF ) ;
    _set_overlay_status ( M_TOPOGRAPHY_NONE , M_OFF ) ;
  }
  else 
  {
    XmToggleButtonSetState(map_menu_items[map_overlay_no_topo],True,False);
    _set_overlay_status ( M_TOPOGRAPHY , M_OFF ) ;
    _set_overlay_status ( M_TOPOGRAPHY_CONTOUR , M_OFF ) ;
    _set_overlay_status ( M_TOPOGRAPHY_NONE , M_OFF ) ;
  }
  if ( _get_overlay_status ( M_ROADS ) == M_ON )
  {  
     XmToggleButtonSetState(map_menu_items[map_overlay_highways_and_roads],True,False);
     _set_overlay_status ( M_HIGHWAYS , M_OFF ) ;
     _set_overlay_status ( M_NOHIGHWAYS , M_OFF ) ;  
  }
  else if ( _get_overlay_status ( M_HIGHWAYS ) == M_ON )
  {  
     XmToggleButtonSetState(map_menu_items[map_overlay_highways_only],True,False);
     _set_overlay_status ( M_ROADS , M_OFF ) ;
     _set_overlay_status ( M_NOHIGHWAYS , M_OFF ) ;
  }  
  else if ( _get_overlay_status ( M_NOHIGHWAYS ) == M_ON )
  {  
    XmToggleButtonSetState(map_menu_items[map_overlay_no_highways],True,False);
    _set_overlay_status ( M_ROADS , M_OFF ) ;
    _set_overlay_status ( M_NOHIGHWAYS , M_OFF ) ;
  }
  _create_menu_separator(map_menu_items[map_overlay_pane]);

  if ( _get_overlay_status ( M_HRAP_GRID ) == M_ON )
    XmToggleButtonSetState(map_menu_items[map_hrap_grid],True,False);
  if ( _get_overlay_status ( M_LAT_LON_LINES ) == M_ON )
    XmToggleButtonSetState(map_menu_items[map_overlay_lat_lon],True,False);
  if ( _get_overlay_status ( M_TIMEZONE ) == M_ON )
    XmToggleButtonSetState(map_menu_items[map_overlay_timezones],True,False);
  if ( _get_overlay_status ( M_RADAR_LOCATIONS ) == M_ON )
    XmToggleButtonSetState(map_menu_items[map_overlay_radar_locations],True,
                           False);
  if ( _get_overlay_status ( M_RADAR_RINGS ) == M_ON )
    XmToggleButtonSetState(map_menu_items[map_overlay_radar_rings],True,False);

}

  fclose ( pFile ) ;
  pFile = NULL ;

  return 0 ;
     

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc/src/mpe_editor/RCS/read_overlay_configuration.c,v $";
 static char rcs_id2[] = "$Id: read_overlay_configuration.c,v 1.14 2007/02/28 20:00:47 lawrence Exp $";}
/*  ===================================================  */

}
