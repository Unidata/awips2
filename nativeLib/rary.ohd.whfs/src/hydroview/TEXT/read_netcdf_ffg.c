/*******************************************************************************
* FILENAME:            read_netcdf_ffg.c
* NUMBER OF MODULES:   7
* GENERAL INFORMATION:
*   MODULE 1:          ffconv
* DESCRIPTION:         Converts an unsigned character read from a netCDF
*                      file to a floating point value according to netCDF
*                      principles.  This is a static function.  It cannot
*                      be called from source code outside this file. 
*   MODULE 2:          createFfgLatLonGrid
* DESCRIPTION:         Creates the latitude/ longitude lookup array which 
*                      corresponds to the HRAP grid that the FFG data
*                      is plotted on.  This makes the drawing and redrawing
*                      of the FFG data on a pixmap more efficient.
*   MODULE 3:          read_netcdf_ffg.
* DESCRIPTION:         Reads ffg data for a given filename and duration
*                      and stores it into an array for use by routine 
*                      "drawFfgHrapGrid" (see below).   This is a static
*                      function.  It cannot be called from outside of this 
*                      file. 
*   MODULE 4:          free_ffg_grids 
* DESCRIPTION:         Returns to the operating system the dynamically 
*                      allocated memory used by the large arrays required 
*                      to process the netCDF ffg.  This routine is
*                      designed to be called either when the main application
*                      is being shut down and it is performing a "final 
*                      clean up" or when the user is clearing the display 
*                      of ffg data.
*   MODULE 5:          drawFfgHrapGrid    
* DESCRIPTION:         This routine is responsible for drawing the ffg data
*                      to the map pixmap which will subsequently be 
*                      displayed on the map viewing area.
*   MODULE 6:          isThereFfgDataToDraw 
* DESCRIPTION:         Returns a value to the screen drawing routine indicating
*                      whether or not there is ffg data to draw.
*   MODULE 7:          turnOffFfgData 
* DESCRIPTION:         Toggles off the display of the ffg data to the screen. 
*  
*   MODULE 8:          free_ffg_linesegs
* DESCRIPTION:         Deallocates the memory assigned to the linked list of
*                      basin linsegs definitions.
*
* ORIGINAL AUTHOR:  Bryon Lawrence
* CREATION DATE:    May 1, 2002
* ORGANIZATION:     HSEB / OHD
* MACHINE:          HP-UX / Dell Redhat Linux
*
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*       1-7         May 7, 2002  Bryon Lawrence    Original Coding
*
********************************************************************************
*/

#include <float.h>
#include <stage3.h>
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>

#include "FfmUtils.h"
#include "get_geoareainfo.h"
#include "geoutil.h"
#include "HvColorList.h"
#include "HvDisplayControlDefs.h"
#include "HydroStatus.h"
#include "map.h"
#include "map_convert.h"
#include "map_library.h"
#include "map_resource.h"
#include "netcdf.h"
#include "post_functions.h"
#include "read_netcdf_ffg.h"
#include "rfcwide.h"

static float ** ffg_float = NULL ;
static HRAP ** ffg_hrap_to_lat_lon = NULL ;
static int num_ffg_hrap_cols = 0 ; 
static int num_ffg_hrap_rows = 0 ; 
static int draw_ffg_data = 0 ; 
static int ffg_duration = 0 ;
static struct MeanArealFFG * pMeanFFG = NULL ;
static enum FfgMode ffg_mode = GRID_MODE ;

static ArealData * pArealData = NULL ;
static long ffg_count = 0 ;
static char areal_boundary_type [ 15 ] = { '\0' } ;

/* These variables keep track of the id and val toggle button states which
   indicate whether or not to annotate areal ffg data with area names and
   values. */
static Boolean draw_ids = False ;
static Boolean draw_values = False ;

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   ffconv
* PURPOSE:       Converts raw unsigned character ffg data read from a
*                netCDF file into floating point values.  This is done 
*                according to netCDF protocol.
*
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME    DESCRIPTION/UNITS
*   Input  unsigned char uchar   Ffg value in millimeters.
*
* RETURNS:
*   DATA TYPE   NAME             DESCRIPTION
*   float       fval             The floating point equivalent of the input
*                                unsigned character value.  This will have
*                                millimeters as its unit.
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME              DESCRIPTION
*   float      fval              The floating point representation of the
*                                input unsigned char is stored within this
*                                variable.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   If the input usigned char value is equal to "0" or greater than or
*   equal to "upper_thres", then "fval" is set to the value "miss_value_float".
*
********************************************************************************
*/

static float ffconv(unsigned char uchar)
{
   float fval = 0.0 ;

   if ( uchar == 0 )
   {
      fval = miss_value_float;
   }
   else if ( uchar <= lower_thres )
   {
      fval = uchar - 1 ;
   }
   else if ( uchar <= upper_thres )
   {
      fval = ( lower_thres - 1 ) + ( ( uchar - 1 ) - ( lower_thres - 1 ) ) 
             * mfactor ;
   }
   else if(uchar >= upper_thres)
   {
      fval = miss_value_float ;
   }

   return fval ;
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   createFfgLatLonGrid
* PURPOSE:       This routine creates a latitude/longitude "look up"
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME           DESCRIPTION/UNITS
*   Input  int         hrap_origin_x  The x origin of the ffg grid relative to
*                                     National HRAP grid.  This is the x origin
*                                     coordinate of the ffg HRAP grid on the
*                                     National HRAP grid.
*   Input  int         hrap_origin_y  The y origin of the ffg grid relative to
*                                     the National HRAP grid.  This is the y
*                                     origin coordinate of the ffg HRAP grid 
*                                     on the National HRAP grid.
*   Input  int         max_columns    The number of HRAP BINS along the x
*                                     axis of the ffg HRAP grid.
*   Input  int         max_rows       The number of HRAP BINS along the y
*                                     axis of the ffg HRAP grid.
*
* RETURNS:
*   DATA TYPE   NAME                  DESCRIPTION
*   HydroStatus status                The return status of this routine. 
*
* APIs UTILIZED:
*   NAME             HEADER FILE      DESCRIPTION
*   HrapToLatLongMpe hrap.h           Converts an HRAP coordinate to a
*                                     latitude / longitude coordinate. 
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME        DESCRIPTION
*   int        i           A loop indexing variable.
*   int        j           A loop indexing variable.
*   point      hrap_point  Used to contain the HRAP coordinate when it is
*                          passed to "HrapToLatLongMpe" for conversion to
*                          latitude / longitude.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*    HydroStatus_OK                         This routine worked without 
*                                           detectable errors.
*    HydroStatus_BadMalloc                  An error was encountered allocating 
*                                           dynamic memory.
*
********************************************************************************
*/

static HydroStatus createFfgLatLonGrid ( int hrap_origin_x , 
                                         int hrap_origin_y ,
                                         int max_columns ,
                                         int max_rows )
{
   point hrap_point ;
   int i ;
   int j ;

   ffg_hrap_to_lat_lon = ( HRAP ** ) malloc ( ( max_columns + 1 ) *
                                              sizeof ( HRAP * ) ) ;

   if ( ffg_hrap_to_lat_lon == NULL )
   {
      fprintf ( stderr , "\nIn routine \"createFfgLatLonGrid\":\n"
                         "Could not allocate %d bytes of memory for\n"
                         "\"ffg_hrap_to_lat_lon\".  Cannot create the\n"
                         "latitude / longitude lookup array.\n" ,
                         ( max_columns + 1 ) * sizeof ( HRAP * ) ) ;
      return HydroStatus_BadMalloc ;
   }

   for ( i = 0 ; i <= max_columns ; ++ i )
   {
      /* For each column in the HRAP grid define the rows in it.  This
         is the y axis. */
      ffg_hrap_to_lat_lon [ i ] = ( HRAP * ) malloc ( ( max_rows + 1 ) *
                                                        sizeof ( HRAP ) ) ;
      
      if ( ffg_hrap_to_lat_lon [ i ] == NULL )
      {
         fprintf ( stderr , "\nIn routine \"createFfgLatLonGrid\":\n"
                            "Could not allocate %d bytes of memory for\n"
                            "element %d of \"ffg_hrap_to_lat_lon\". Cannot\n"
                            "create the latitude / longitude lookup array.\n" ,
                            ( max_rows + 1 ) * sizeof ( HRAP ) , i ) ;

         for ( j = 0 ; j < i ; ++ j )
         {
            free ( ffg_hrap_to_lat_lon [ j ] ) ;
         } 

         free ( ffg_hrap_to_lat_lon ) ;
         ffg_hrap_to_lat_lon = NULL ;

         return  HydroStatus_BadMalloc ;
      }

   }

   for ( i = 0 ; i <= max_columns ; ++ i )
   {
      hrap_point.x = ( float ) ( i + hrap_origin_x ) ;

      for ( j = 0 ; j <= max_rows ; ++j )
      {
         hrap_point.y = ( float ) ( hrap_origin_y + j ) ;
         ffg_hrap_to_lat_lon [ i ] [ j ] = HrapToLatLongMpe ( hrap_point ) ;

         /* The "map library" routines expect Western Hemisphere longitudes
            to be negative.  "HrapToLatLongMpe" makes Western Hemisphere
            longitudes positive.  Negate these longitudes here so that the
            "map library" routines don't flip out. */
         ffg_hrap_to_lat_lon [ i ] [ j ].x *= -1 ;
      }
   }

   return HydroStatus_OK ;
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
static void draw_mean_areal_ffg ( float ** data ,
                                  struct MeanArealFFG * pMeanFfgHead ,
                                  int xor , int yor , int max_columns ,
                                  int max_rows )
{
   char reply [ MAX_MPE_STRING_LEN ] ;
   float min_coverage ;
   register int col ;
   register int i ;
   register int jcol ;
   int reply_len ;
   int request_len ;
   int status ;

   register int row ;
   struct MeanArealFFG * pMeanFfg = NULL ;
   static char * min_coverage_token = "whfs_min_area_covered" ;

   request_len = strlen ( min_coverage_token ) ;

   /* Get the minimum coverage. */
   status = get_apps_defaults ( min_coverage_token ,
                                & request_len ,
                                reply ,
                                & reply_len ) ;

   if ( status != 0 || reply_len == 0 )
   {
      fprintf ( stderr , "In routine \"draw_mean_areal_ffg\":\n"
                         "Could not retrieve the value of token\n"
                         "\"whfs_min_area_covered\".  Using %4.2f as the\n"
                         "default coverage value.\n" ,  DEFAULT_FFG_COVERAGE ) ;
      min_coverage = DEFAULT_FFG_COVERAGE ;
   }
   else
   {
      min_coverage = atof ( reply ) ;
   }

   /* Walk over the linked list. */
   pMeanFfg = pMeanFfgHead ;

   while ( pMeanFfg != NULL )
   {
      /* loop on the number of rows for this basin */
      for ( i = 0 ; i < pMeanFfg->numrows ; ++ i )
      {
         /* loop on the number of columns in each row */
         for ( jcol = pMeanFfg->beg_cols [ i ] ;
               jcol <= pMeanFfg->end_cols [ i ] ; ++ jcol )
         {
            row = pMeanFfg->rows [ i ] - yor ;
            col = ( jcol - xor ) ;

            /*-----------------------------------------------*/
            /*  check that box is within site's area         */
            /*  if not, return with status set to -1         */
            /*-----------------------------------------------*/
            if ( ( row < max_rows ) &&
                 ( col < max_columns ) &&
                 ( row >= 0 ) && ( col >= 0 ) )
            {
               if ( pMeanFfg->area_covered > min_coverage )
               {
                  data [ row ] [ col ] = pMeanFfg->avg_val ;
               }
               else
               {
                  data [ row ] [ col ] = miss_value_float ;
               }
            }

         }
      }

      pMeanFfg = ( struct MeanArealFFG * )
                    ListNext ( & pMeanFfg->node ) ;
   }
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

static void compute_mean_areal_ffg ( float ** data ,
                                     struct MeanArealFFG * pMeanFfg ,
                                     int xor , int yor , int max_columns ,
                                     int max_rows )
{
   double       cur_max ;
   double       cur_min ;
   int          col ;
   int          i ;
   int          jcol ;
   int          miss_cnt ;
   int          row ;
   int          total_cnt ;
   int          val_cnt ;
   float        raw_val ;
   float        sum ;
   struct MeanArealFFG * pNode = NULL ;

   /* Initialize the linked list. */
   pNode = pMeanFfg ;

   while ( pNode != NULL )
   {
      /* initialize */
      miss_cnt = total_cnt = val_cnt = 0;
      cur_max = DBL_MIN ;
      cur_min = DBL_MAX ;
      sum = 0.0 ;

      /* loop on the number of rows for this basin */
      for ( i = 0 ; i < pNode->numrows ; ++ i )
      {
         total_cnt += pNode->end_cols [ i ] -
                      pNode->beg_cols [ i ] + 1 ;
         /* loop on the number of columns in each row */
         for ( jcol = pNode->beg_cols [ i ] ;
               jcol <= pNode->end_cols [ i ] ;
               ++ jcol )
         {
            /* sum the value and increment the cnts.
               note that the array index method must match the
               method by which the grid was originally loaded*/

            row = pNode->rows [ i ] - yor ;
            col = ( jcol - xor ) ;

            /*-----------------------------------------------*/
            /*  check that the box is within site's area         */
            /*  if not, return -1.                           */
            /*-----------------------------------------------*/

            if ( row >= max_rows ||
                 col >= max_columns ||
                 row < 0 || col < 0)
            {
               ++ miss_cnt ;
            }
            else
            {
               raw_val = data [ row ] [ col ] ;

               if ( raw_val >= 0 )
               {
                  sum += raw_val ;
                  if (raw_val > cur_max) cur_max = raw_val ;
                  if (raw_val < cur_min) cur_min = raw_val ;

                  ++ val_cnt ;
               }
               else
               {
                  ++ miss_cnt ;
               }
            }
         }
      }

      /* compute the avg ffg value as the average of all the
         bins within the area that have valid area_id data. */
      if ( total_cnt <= 0 )
      {
         pNode->area_covered = 0.0 ;
      }
      else
      {
         pNode->area_covered = ( ( float ) val_cnt /
                                 ( float ) total_cnt ) ;
      }

      if (val_cnt > 0)
      {
         pNode->avg_val = sum / val_cnt ;
         pNode->max_val = cur_max ;
         pNode->min_val = cur_min ;
      }
      else
      {
         pNode->avg_val = 0.0 ;
         pNode->max_val = 0.0 ;
         pNode->min_val = 0.0 ;
      }

      /* adjust the returned value if it is less than some minimal number;
         this is due to the nature of the precip data, especially the
         radar data which contains super-tiny values */

         if ( pNode->avg_val < .00001 )
      {
         pNode->avg_val = 0.0 ;
      }

      if ( pNode->max_val < .00001 )
      {
         pNode->max_val = 0.0 ;
      }

      if ( pNode->min_val < .00001 )
      {
         pNode->min_val = 0.0 ;
      }

      pNode = ( struct MeanArealFFG * ) ListNext ( & pNode->node ) ;

   }

   return ;
}
       

/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   read_netcdf_ffg
* PURPOSE:       This routine opens a netCDF ffg file and processes the data
*                that it contains.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME      DESCRIPTION/UNITS
*   Input  char *      filename  The name ( including the full path ) of the
*                                netCDF file containing the ffg data to be
*                                processed.
*   Input  int         duration  The duration of the ffg data (e.g. 1,3,6,12,
*                                or 24 hours).  This is required for
*                                determining the color coding to use for the
*                                displayed data.
*
* RETURNS:
*   DATA TYPE   NAME             DESCRIPTION
*   HydroStatus status           The exit status of this routine, i.e. were
*                                any errors encountered.
*
* APIs UTILIZED:
*   NAME                  HEADER FILE     DESCRIPTION
*   createFfgLatLonGrid   In this file    Creates the lat/lon "look up"
*                                         array for the ffg HRAP grid.
*   LatLongToHrapMpe      hrap.h          Converts a latitude / longitude
*                                         coordinate to HRAP.  
*   mUpdateMap            map_library.h   Calls the map library routines
*                                         responsible for redrawing the map
*                                         display.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE        NAME       DESCRIPTION
*   char [ ]         x_dim_name The netCDF name of the x dimension. 
*   char [ ]         y_dim_name The netCDF name of the y dimension.
*   HydroStatus      hstatus    The return status of the call to 
*                               "createFfgLatLonGrid". 
*   int              arrayid    The netCDF identifier of the "image" variable.
*   int              i          A loop indexing variable.
*   int              ix         A loop indexing variable.
*   int              ixh        The HRAP x coordinate of the upper left (NW)
*                               corner of the FFG grid. 
*   int              iy         A loop indexing variable.
*   int              iyh        The HRAP y coordinate of the upper left (NW)
*                               corner of the FFG grid.
*   int              iz         A loop indexing variable.
*   int              l_len      The length of the "lat00" and "lon00" 
*                               netCDF variables.
*   int              ncid       The identifier assigned to the netCDF file
*                               when it is opened.
*   int              status     Used to contain return codes from calls 
*                               to netCDF routines.
*   int              xdim_id    The netCDF identifier of the x dimension
*                               variable.
*   int              xrl        The HRAP x coordinate of the lower left (SW)
*                               corner of the FFG grid.
*   int              xru        The HRAP x coordinate of the upper right (NE)
*                               corner of the FFG grid.
*   int              xsize      These number of columns in the ffg HRAP grid.
*   int              ydim_id    The netCDF identifier of the y dimension
*                               variable.
*   int              yrl        The HRAP y coordinate of the lower left (SW)
*                               corner of the FFG grid.
*   int              yru
*   int              ysize      The number of rows in the ffg HRAP grid.
*   float *          plat00     The retrieved latitude for the NW corner of the
*                               ffg HRAP grid.
*   float *          plon00     The retrieved longitude for the NW corner of
*                               the ffg HRAP grid.
*   HRAP             hrap       Contains the hrap x,y coordinates equivalent
*                               to the latitude / longitude coordinate of the
*                               upper left (NW) corner of the ffg HRAP grid.
*   nc_type          l_type     The netCDF type of the "lat00" and "lon00"
*                               variables.
*   unsigned char *  ffg_grid   The ffg values returned from the netCDF file.
*
*
* DATA FILES AND/OR DATABASE:
*   This routine opens, reads, and closes a netCDF file containing ffg data. 
*
* ERROR HANDLING:
*    ERROR CODE                 DESCRIPTION
*    HydroStatus_BadFilename    A null, incomplete, or non existent filename
*                               was passed into this routine.
*    HydroStatus_BadMalloc      An error was encountered allocating memory.
*    HydroStatus_netCDFerror    An error was encountered interacting with the
*                               netCDF.
*    HydroStatus_OK             No errors were detected.
*
********************************************************************************
*/

HydroStatus read_netcdf_ffg ( char * filename ,  
                              enum FfgArealTypes basin_flag ,
                              int duration )
{
   char * area_type = "BASIN" ;
   char x_dim_name [ NC_MAX_NAME ] ;
   char y_dim_name [ NC_MAX_NAME ] ; 
   HydroStatus hstatus ; 
   int i, iz, arrayid;
   int status;
   int xdim_id, ydim_id, ixh, iyh, ix, iy, xru, yru;
   int xrl , yrl ;
   int xsize , ysize ;
   float * plat00 = NULL ; 
   float * plon00 = NULL ; 
   HRAP hrap ;
   unsigned char * ffg_grid = NULL ;

   /*--------------------------------*/
   /*   netcdf specific variables    */
   /*--------------------------------*/

   int ncid , l_len ;
   nc_type l_type ;

   if ( ffg_float != NULL )
   {
      free_ffg_grids ( ) ;
   }

   if ( ffg_hrap_to_lat_lon != NULL )
   {
      free_ffg_grids ( ) ;
   }

   if ( filename == NULL )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "A NULL filename has been passed to this\n"
                         "routine. Can not process the netcdf ffg data.\n" ) ;
      return HydroStatus_BadFilename ;
   }

   /*--------------------------------*/
   /*  open netcdf file for reading  */
   /*--------------------------------*/
   if ( strlen ( filename ) >= NC_MAX_NAME )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "The filename length, %d characters, has matched\n"
                         "or exceeded NC_MAX_NAME, %d characters.\n" ,
                         strlen ( filename ) , NC_MAX_NAME ) ;
      return HydroStatus_BadFilename ;
   }   

   status = nc_open ( filename , NC_NOWRITE , & ncid ) ;

   if ( status != NC_NOERR )
   {
      fprintf ( stderr, "\nIn routine \"read_netcdf_ffg\":\n"
                        "An error was encountered by routine \"nc_open\"\n"
                        "while attempting to open file \"%s\".  Netcdf error\n"
                        "code %d.\n" , filename , status ) ;
      return HydroStatus_BadFilename ;
   }

   /*--------------------------------*/
   /*  get dimensions of site area   */
   /*--------------------------------*/

   status = nc_inq_dimid ( ncid , "x" , & xdim_id ) ;  

   if ( status != NC_NOERR )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "An error was encountered while retrieving the\n"
                         "dimension id for the \"x\" dimension.  Netcdf\n"
                         "return status code: %d.\n" , status ) ; 
      return HydroStatus_netCDFerror ;
   }

   status = nc_inq_dimid ( ncid , "y" , & ydim_id ) ;

   if ( status != NC_NOERR )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "An error was encountered while retrieving the\n"
                         "dimension id for the \"y\" dimension.  Netcdf\n"
                         "return status code: %d.\n" , status ) ; 
      return HydroStatus_netCDFerror ;
   }

   status = nc_inq_dim ( ncid , xdim_id , x_dim_name , & xsize ) ;

   if ( status != NC_NOERR )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "An error was encountered while retrieving the\n"
                         "size of the \"x\" dimension from netcdf file\n"
                         "\"%s\". Netcdf error code: %d.\n" , filename ,
                         status ) ;
      return HydroStatus_netCDFerror ;
   }

   status = nc_inq_dim ( ncid , ydim_id , y_dim_name , & ysize ) ;

   if ( status != NC_NOERR )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "An error was encountered while retrieving the\n"
                         "size of the \"y\" dimension from netcdf file\n"
                         "\"%s\". Netcdf error code: %d.\n" , filename ,
                         status ) ;
      return HydroStatus_netCDFerror ;
   }

   num_ffg_hrap_cols = xsize ;
   num_ffg_hrap_rows = ysize ;

   if ( ( xsize <= 0 ) || ( ysize <= 0 ) )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "Invalid, negative value for \"xsize\" and or\n"
                         "\"ysize\", where \"xsize\"=%d and \"ysize\"=%d.\n" ,
                         xsize , ysize ) ;
      return HydroStatus_netCDFerror ;
   }

   /*--------------------------------------------*/
   /*  read lat/lon of upper left corner of grid */
   /*--------------------------------------------*/

   status = nc_inq_att ( ncid , NC_GLOBAL , "lat00" , & l_type , & l_len ) ;

   if ( status != NC_NOERR )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "Could retrieve the type and length of the global\n"
                         "attribute \"lat00\".  Netcdf error code %d\n" ,
                         status ) ;
      return HydroStatus_netCDFerror ;
   }

   status = nc_inq_att ( ncid , NC_GLOBAL , "lon00" , & l_type , & l_len ) ;

   if ( status != NC_NOERR )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "Could retrieve the type and length of the global\n"
                         "attribute \"lon00\".  Netcdf error code %d\n" ,
                         status ) ;
      return HydroStatus_netCDFerror ;
   }

   plat00 = ( float * ) malloc ( l_len * nctypelen ( l_type ) ) ;

   if ( plat00 == NULL )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "Could not allocate %d bytes of memory for\n"
                         "\"plat00\".\n" , ( l_len * nctypelen ( l_type ) ) ) ;
      return HydroStatus_BadMalloc ;
   }

   plon00 = ( float * ) malloc ( l_len * nctypelen ( l_type ) ) ;

   if ( plon00 == NULL )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "Could not allocate %d bytes of memory for\n"
                         "\"plon00\".\n" , ( l_len * nctypelen ( l_type ) ) ) ;
      free ( plat00 ) ;
      plat00 = NULL ;
      return HydroStatus_BadMalloc ;
   }

   status = nc_get_att_float ( ncid , NC_GLOBAL , "lat00" , plat00 ) ;

   if ( status != NC_NOERR )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "Could not retrieve the value of global attribute\n"
                         "\"lat00\".  Netcdf error code %d.\n" ,
                         status ) ;
      free ( plat00 ) ;
      plat00 = NULL ;
      free ( plon00 ) ;
      plon00 = NULL ;
     
      return HydroStatus_netCDFerror ;
   }

   status = nc_get_att_float ( ncid , NC_GLOBAL , "lon00" , plon00 ) ;

   if ( status != NC_NOERR )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "Could not retrieve the value of global attribute\n"
                         "\"lon00\".  Netcdf error code %d.\n" ,
                         status ) ;
      free ( plat00 ) ;
      plat00 = NULL ;
      free ( plon00 ) ;
      plon00 = NULL ;
     
      return HydroStatus_netCDFerror ;
   }

   /*----------------------------------------------------------------*/
   /*  transform lat/lon of upper left corner of site grid to HRAP   */
   /*  use llgd function from util_gen1 lib                          */
   /*  (ixh,iyh) = HRAP coord of upper left (NW) corner of site grid */
   /*----------------------------------------------------------------*/
   *plon00 *= -1 ;
   hrap = LatLongToHrapMpe ( * plat00 , * plon00 ) ; 

   free ( plat00 ) ;
   plat00 = NULL ;
   free ( plon00 ) ;
   plon00 = NULL ;

   ixh = hrap.x ;
   iyh = hrap.y ;

   /*-----------------------------------------------------------------*/
   /*  calculate HRAP coord of upper right (NE) corner of site grid   */
   /*  (xru,yru) = HRAP coord of upper right (NE) corner of site grid */
   /*-----------------------------------------------------------------*/
   xru  = ixh + xsize ;
   yru  = iyh ;

   /*------------------------------------------------------------------*/
   /*  calculate HRAP coord of lower left  (SW) corner of site grid    */
   /*  (xrl,yrl) = HRAP coord of lower left  (SW) corner  of site grid */
   /*------------------------------------------------------------------*/
   xrl  = ixh ;
   yrl  = iyh - ysize ;

   /*------------------------------------------------------------------*/
   /* If this routine is being called for the first time, create the   */
   /* latitude / longitude lookup array.  This array will make the     */
   /* plotting of the ffg data more efficient by performing the HRAP   */
   /* to latitude / longitude calculations once up front.  These       */
   /* can then be reused over and over again whenever the FFG data is  */
   /* plotted.                                                         */
   /*------------------------------------------------------------------*/

   hstatus = createFfgLatLonGrid ( xrl , yrl , xsize , ysize ) ;

   if ( status != 0 )
   {
      fprintf ( stderr , "In routine \"read_netcdf_ffg\":\n"
                         "The call to \"createFfgLatLonGrid\" failed\n"
                         "with a return code of %d.  Aborting attempt\n"
                         "to draw ffg grid.\n" , status ) ;
      return hstatus ;
   } 

   /*------------------------------------------*/
   /*  malloc space for FFG gridded array      */
   /*------------------------------------------*/
   ffg_grid = ( unsigned char * ) malloc ( ysize * xsize *
                                         sizeof ( unsigned char * ) ) ;

   if ( ffg_grid == NULL )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "Could not allocate %d bytes of memory for\n"
                         "\"ffg_grid\".\n" ,
                          ysize * xsize * 
                         sizeof ( unsigned char * ) ) ;
      return HydroStatus_BadMalloc ;
   }

   /*---------------------------------------------------------*/
   /*  read gridded FFG array from file                       */
   /*  data values are unsigned chars quantized from 0 - 253  */
   /*---------------------------------------------------------*/

   status = nc_inq_varid ( ncid , "image" , & arrayid ) ;

   if ( status != NC_NOERR )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "The attempt to retrieve the id of variable\n"
                         "\"image\" from netcdf table %s has failed.\n"
                         "Netcdf error code %d.\n" , filename ,
                         status ) ;

      free ( ffg_grid ) ;
      ffg_grid = NULL ;
      return HydroStatus_netCDFerror ;
   }

   status = nc_get_var_uchar ( ncid , arrayid , ffg_grid ) ;

   if ( status != NC_NOERR )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "The attempt to read the ffg data array from\n"
                         "netcdf file %s has failed.  Netcdf error code %d.\n",
                         filename , status ) ;
      free ( ffg_grid ) ;
      ffg_grid = NULL ;
      return HydroStatus_netCDFerror ;
   }

   /*------------------------------------------------*/
   /*  malloc space for mosaic array (type = float)  */
   /*------------------------------------------------*/

   ffg_float = ( float ** ) malloc ( ysize * sizeof ( float * ) ) ;

   if ( ffg_float == NULL )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "Could not allocate %d bytes for \"ffg_float\".\n" ,
                         ysize * sizeof ( float * ) ) ;
      free ( ffg_grid ) ;
      ffg_grid = NULL ;
      return HydroStatus_BadMalloc ;
   } 

   for ( i = 0 ; i < ysize ; i++ )
   {
      ffg_float [ i ] = ( float * ) malloc ( xsize * sizeof ( float ) ) ;
      
      if ( ffg_float [ i ] == NULL )
      { 
         fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                            "Could not allocate %d bytes for element %d\n"
                            "of \"ffg_float\".\n" , 
                            ( xsize ) * sizeof ( float ) , i ) ;
         free_ffg_grids ( ) ;
         free_ffg_linesegs ( ) ;
         free ( ffg_grid ) ;
         ffg_grid = NULL ;
         return HydroStatus_BadMalloc ;
      }

   }

   /*------------------------------------------------------------*/
   /*  transform gridded FFG values from unsigned char to float  */
   /*------------------------------------------------------------*/
   iz = 0;

   for ( iy = ysize - 1 ; iy >= 0 ; -- iy )
   {
      for ( ix = 0 ; ix < xsize ; ix++ , iz++ )
      {
         ffg_float [ iy ] [ ix ] = ffconv ( ffg_grid [ iz ] ) ;
      }
   }

   free ( ffg_grid ) ;
   ffg_grid = NULL ;

   /*--------------------------------*/
   /*  close the netcdf file         */
   /*--------------------------------*/

   status = nc_close ( ncid ) ;

   if ( status != NC_NOERR )
   {
      fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                         "Could not close netcdf file %s,\n"
                         "where ncid = %d.\n" , filename , ncid ) ;
      free_ffg_grids ( ) ;
      free_ffg_linesegs ( ) ;
      return HydroStatus_netCDFerror ;
   }
   else
   {
      draw_ffg_data = 1 ;
      ffg_duration = duration ;

      /* Test to determine if the user wishes to see basin data.
         If so, fetch the basin data and draw it. */
      if ( basin_flag == FFG_BASIN )
      {
         if ( pMeanFFG == NULL )
         {
            pMeanFFG = ( struct MeanArealFFG * ) get_area_linesegs ( area_type,
                                    sizeof ( struct MeanArealFFG ) ) ; 

            if ( pMeanFFG == NULL )
            {
               fprintf ( stderr , "\nIn routine \"read_netcdf_ffg\":\n"
                                  "No Basin lineseg data could be retrieved\n"
                                  "from the database.  Can not draw the basin\n"
                                  "averaged FFG.\n" ) ;
               free_ffg_grids ( ) ;
               free_ffg_linesegs ( ) ;
               return HydroStatus_NoData ;

            }
         }

         /* Compute the mean areal ffg. */
         compute_mean_areal_ffg ( ffg_float , pMeanFFG , xrl , yrl ,
                                  xsize , ysize ) ;

         /* Clear the FFG Grid. */
         for ( iy = 0 ; iy < ysize ; ++ iy )
         {
            for ( ix = 0 ; ix < xsize ; ++ ix )
            {
               ffg_float [ iy ] [ ix ] = miss_value_float ;
            }
         }


         /* Draw the mean areal ffg. */
         draw_mean_areal_ffg ( ffg_float , pMeanFFG , xrl , yrl ,
                               xsize , ysize ) ; 

      }

      mUpdateMap ( 0 ) ; 

      return HydroStatus_OK ;
   }
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

void read_areal_ffg ( char * boundary_type ,
                      int ffg_hours ,
                      time_t ticks ,
                      int since_flag )
{
   int status ;

   if ( pArealData != NULL )
   {
      free ( pArealData ) ;
      pArealData = NULL ;
      ffg_count = 0 ;     
   }

   pArealData = bld_ffg_area ( boundary_type ,
                               ffg_hours ,
                               ticks ,
                               since_flag ,
                               & status ,
                               & ffg_count ) ;

   if ( ( pArealData == NULL ) || ( status != 1 ) ||
        ( ffg_count == 0 ) )
   {
      fprintf ( stderr , "In routine 'ffg_select_display':\n"
                         "Could not retrieve areal data for\n"
                         "boundary type '%s' and duration %d.\n" ,
                          boundary_type , ffg_hours ) ;
   }
   else
   {
      draw_ffg_data = 1 ;
      strcpy ( areal_boundary_type , boundary_type ) ; 
      mUpdateMap ( 0 ) ; 
   }
}

/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   free_ffg_grids
* PURPOSE:       Returns to the operating system the dynamic memory used to
*                build the large arrays containing the ffg data read from the
*                the netCDF ffg flat file.
*
* ARGUMENTS:
*   None
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   None  ( Only system utilities are used.)
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   int        i                            A loop indexing variable.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/

void free_ffg_grids ( )
{
   int i = 0 ;

   if ( ffg_float != NULL )
   {
      for ( i = 0 ; ( i < num_ffg_hrap_rows ) && 
                    ( ffg_float [ i ] != NULL ) ; ++ i )
      {
         free ( ffg_float [ i ] ) ;
      }

      free ( ffg_float ) ;
      ffg_float = NULL ;
   }

   if ( ffg_hrap_to_lat_lon != NULL )
   {
      for ( i = 0 ; ( i <= num_ffg_hrap_cols ) &&
                    ( ffg_hrap_to_lat_lon [ i ] != NULL ) ; ++ i )
      {
         free ( ffg_hrap_to_lat_lon [ i ] ) ;
      }
  
      free ( ffg_hrap_to_lat_lon ) ;
      ffg_hrap_to_lat_lon = NULL ;
   }

   /* Test to see if there is any areal data. */
   if ( pArealData != NULL )
   {
      free ( pArealData ) ;
      pArealData = NULL ;
      ffg_count = 0 ;
   }

}

/*******************************************************************************
* MODULE NUMBER: 5
* MODULE NAME:   drawFfgHrapGrid 
* PURPOSE:       This routine draws the ffg data to a pixmap supplied by the
*                the map library.
*
* ARGUMENTS:
*   TYPE   DATA TYPE           NAME       DESCRIPTION/UNITS
*   Input  HvDisplayControl *  hdc        The display control structure
*                                         containing the color levels
*                                        used in drawing the ffg data.
*
* RETURNS:
*   void
*
* APIs UTILIZED:
*   NAME                       HEADER FILE       DESCRIPTION
*   determineColorByThreshold  ColorThreshold.h  For a given ffg value, this
*                                                routine finds its associated
*                                                color.
*   mConvertLatLon2XY          map_convert.h     Converts a latitude / 
*                                                longitude coordinate to a
*                                                Cartesian pixel coordinate.
*   mSetColor                  map_library.h     Tells the map library routines
*                                                what color to use in drawing
*                                                the ffg data.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME               DESCRIPTION
*   char       areaColor          The color to use in filling the current
*                                 HRAP bin's ffg value.
*   char *     color              Points to the color value returned from
*                                 the routine "determineColorByThreshold".
*   Display *  display            The display of the application.
*   float      precip_in_inches   The value of the ffg after it has been
*                                 converted from millimeters to inches. 
*   GC         gc                 The graphics context of the application.
*   int        i                  A loop indexing variable.
*   int        j                  A loop indexing variable.
*   int        xpos               Contains the x pixel coordinate of
*                                 a corner of the HRAP bin to be drawn. 
*   int        ypos               Contains the y pixel coordinate of
*                                 a corner of the HRAP bin to be drawn. 
*   Pixmap     pix                The pixmap to draw to.
*   XPoint [ ] points             The points representing the four corners
*                                 of a HRAP bin.
*   XPoint [ ] prev_right_points  The previous right side HRAP bin points in 
*                                 the previous column of ffg data to be 
*                                 processed. 
*
* DATA FILES AND/OR DATABASE:
*    None
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/

void drawFfgHrapGrid ( HvDisplayControl * hdc )
{
   char areaColor [ 20 ] ;
   float precip_in_inches ;
   char precip_in_inches_array [ 10 ] ;
   GeoAreaInfo * pGeoAreaInfo = NULL ;
   int i ;
   int j ;
   int status ;
   int xpos = 0 ;
   int ypos = 0 ; 
   const char * color = NULL ;
   Display * display = _get_map_display ( ) ;
   GC gc = _get_map_gc ( ) ;
   Pixmap pix = _get_map_pixmap ( ) ;
   XPoint points [ NUM_CORNERS_PER_HRAP_BIN ] ;
   XPoint prev_right_points [ num_ffg_hrap_rows ] ;

   /* Test to make sure that there is actually FFG data to draw. */
   if ( ffg_mode == GRID_MODE )
   {
      if ( draw_ffg_data == 0 || ffg_hrap_to_lat_lon == NULL ||
           ffg_float == NULL )
      {
         return ;
      }
   }
   else
   {
      if ( draw_ffg_data == 0 || pArealData == NULL )
      {
         return ;
      }
   }

   /* Determine which type of FFG data is being drawn - areal or gridded.
      These are drawn in different ways since areal FFG comes from the
      ContingencyValue table in the IHFS database and the areal FFG comes from
      a netCDF file. */
   if ( ffg_mode == GRID_MODE )
   {
   
      for ( i = 0 ; i < num_ffg_hrap_cols ; ++ i )
      {
         for ( j = 0 ; j < num_ffg_hrap_rows ; ++ j )
         {

            /* Determine the four points bounding this HRAP bin.
               Work counter-clockwise around the HRAP bin.  This algorithm
               has been developed to make this operation as efficient as
               possible.  Note that the mConvertLatLon2XY routine is an
               "inline" function which should speed things up a little
               bit more. */
            if ( j == 0 )
            {
               mConvertLatLon2XY ( ffg_hrap_to_lat_lon [ i + 1 ] [ j ].y ,
                                   ffg_hrap_to_lat_lon [ i + 1 ] [ j ].x ,
                                   & xpos , & ypos ) ;
               points [ BOTTOM_RIGHT_CORNER ].x = xpos ;
               points [ BOTTOM_RIGHT_CORNER ].y = ypos ;

               if ( i == 0 )
               {
                  mConvertLatLon2XY ( ffg_hrap_to_lat_lon [ i ] [ j ].y ,
                                      ffg_hrap_to_lat_lon [ i ] [ j ].x ,
                                      & xpos , & ypos ) ;
                  points [ BOTTOM_LEFT_CORNER ].x = xpos ;
                  points [ BOTTOM_LEFT_CORNER ].y = ypos ;
               }
               else 
               {
                  points [ BOTTOM_LEFT_CORNER ] = prev_right_points [ j ] ;
               }
            }
            else
            {
               points [ BOTTOM_RIGHT_CORNER ] = points [ TOP_RIGHT_CORNER ] ;
               points [ BOTTOM_LEFT_CORNER ] = points [ TOP_LEFT_CORNER ] ;
            }

            if ( i == 0 )
            {
               mConvertLatLon2XY ( ffg_hrap_to_lat_lon [ i ] [ j + 1 ].y ,
                                   ffg_hrap_to_lat_lon [ i ] [ j + 1 ].x ,
                                  & xpos , & ypos ) ;
               points [ TOP_LEFT_CORNER ].x = xpos ;
               points [ TOP_LEFT_CORNER ].y = ypos ;
            }
            else
            {
               points [ TOP_LEFT_CORNER ] = prev_right_points [ j + 1 ] ;
            }

            mConvertLatLon2XY ( ffg_hrap_to_lat_lon [ i + 1 ] [ j + 1 ].y ,
                                ffg_hrap_to_lat_lon [ i + 1 ] [ j + 1 ].x ,
                                & xpos , & ypos ) ;
            points [ TOP_RIGHT_CORNER ].x = xpos ;
            points [ TOP_RIGHT_CORNER ].y = ypos ;
 
            prev_right_points [ j ] = points [ BOTTOM_RIGHT_CORNER ] ;
            prev_right_points [ j + 1 ] = points [ TOP_RIGHT_CORNER ] ;

            /* Draw the filled polygon. Need to use the X routine directly
               here because the Graphics Context (GC) of each point differs
               depending on its vip level.  The map library routines would
               assume that each point has the same GC. */

            /* Retrieve the color level for the ffg data in the HRAP bin. */
            if ( ffg_float [ j ] [ i ] != miss_value_float )
            { 
               precip_in_inches = ffg_float [ j ] [ i ] / 25.4 ; 
            }
            else
            {
               precip_in_inches = miss_value_float ;
            }

            color = determineColorByThreshold ( precip_in_inches ,
                                     miss_value_float ,
                                     & hdc->displaySettings.areal.ctArray ) ;
            strncpy ( areaColor , color , MAX_COLOR_NAME_LENGTH - 1 ) ;
            areaColor [ MAX_COLOR_NAME_LENGTH ] = '\0' ;
            mSetColor ( areaColor ) ;
            XFillPolygon ( display , pix , gc , points , 
                           NUM_CORNERS_PER_HRAP_BIN ,
                           Complex , CoordModeOrigin ) ;
         }
 
      }
   }
   else /* if ( ffg_mode == GRID_MODE ) */
   {
   
      /* Loop on the array of ArealData structures.  For the
         good entries, find the GeoArea linked list node containing
         the boundary definitions. */
      for ( i = 0 ; i < ( int ) ffg_count ; ++ i )
      {
         if ( pArealData [ i ].isOk == 1 )
         {
            pGeoAreaInfo = get_geoareainfo ( pArealData [ i ].lid ,
                                             & status ) ;
            if ( status != GEOAREAINFO_OK || pGeoAreaInfo == NULL )
            {
               fprintf ( stderr , "\nIn routine 'drawFfgHrapGrid':\n"
                                  "Could not find area id %s in GeoArea\n"
                                  "table.  Cannot draw Areal FFG.\n" , 
                                  pArealData [ i ].lid ) ;
               draw_ffg_data = 0 ;
     
               free_ffg_grids ( ) ;
               return ;
            }
    
            /* Convert the interior latitude and longitude to pixel 
               coordinates. */
            mConvertLatLon2XY ( pGeoAreaInfo->interior_lat ,
                                -1 * pGeoAreaInfo->interior_lon ,
                                & xpos ,
                                & ypos ) ;

            if ( pArealData [ i ].value != miss_value_float )
            { 
               precip_in_inches = pArealData [ i ].value ; 
               sprintf ( precip_in_inches_array , "%6.2f" , 
                         pArealData [ i ].value ) ; 
            }
            else
            {
               precip_in_inches = miss_value_float ;
               strcpy ( precip_in_inches_array , "M" ) ;
            }

            /* Get the color information. */
            color = determineColorByThreshold ( precip_in_inches ,
                                     miss_value_float ,
                                     & hdc->displaySettings.areal.ctArray ) ;

            strncpy ( areaColor , color , MAX_COLOR_NAME_LENGTH - 1 ) ;

            areaColor [ MAX_COLOR_NAME_LENGTH ] = '\0' ;
            mSetColor ( areaColor ) ;


            /* Determine if the user wants to display the area's identifier. */
            if ( draw_ids == True )
            {
               mDrawText ( M_EXPOSE , 0 , xpos , ypos + GEOAREA_ID_OFFSET , 
                           pArealData [ i ].lid ) ;
            }
            
            /* Draw the areal value. */
            if ( draw_values == True )
            {
               mDrawText ( M_EXPOSE , 0 , xpos , 
                           ypos + GEOAREA_VALUE_OFFSET + 10 , 
                           precip_in_inches_array ) ;
            }
         }
      }
   }
}

/*******************************************************************************
* MODULE NUMBER: 6
* MODULE NAME:   isThereFfgDataToDraw 
* PURPOSE:       Determines if there is ffg data to draw.  Returns a value of
*                "0" if there is not.  Returns a value of "1" if there is.
*
* ARGUMENTS:
*   None.
*
* RETURNS:
*   DATA TYPE   NAME               DESCRIPTION
*   int         draw_ffg_data      If there is ffg data to draw, then the 
*                                  return value will be "1". Otherwise, it
*                                  will be "0".
*
* APIs UTILIZED:
*   None
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

int isThereFfgDataToDraw ( )
{
   return draw_ffg_data ;
}

/*******************************************************************************
* MODULE NUMBER: 7
* MODULE NAME:   turnOffFfgData 
* PURPOSE:       Toggles "off" the display of the ffg data.
*
* ARGUMENTS:
*   None.
*
* RETURNS:
*   None.
*
* APIs UTILIZED:
*   None.
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
void turnOffFfgData ( )
{
   draw_ffg_data = 0 ;
   mUpdateMap ( 0 ) ;
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
void SetFfgMode ( enum FfgMode mode )
{
   ffg_mode = mode ; 
}

/*******************************************************************************
* MODULE NUMBER:  8
* MODULE NAME:    free_ffg_linesegs
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

void free_ffg_linesegs ( )
{
   if ( pMeanFFG == NULL )
   {
      free_area_linesegs_list ( ( void * ) pMeanFFG ) ;
      pMeanFFG = NULL;
   }
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

void toggle_areal_annotations ( int toggle_button_id ,  Boolean flag )
{
   if ( toggle_button_id == AREAL_ID_TOGGLE_BUTTON )
   {
      draw_ids = flag ;
   }
   else if ( toggle_button_id == AREAL_VAL_TOGGLE_BUTTON )
   {
      draw_values = flag ;
   }
   else
   {
      fprintf ( stderr , "\nIn routine 'toggle_areal_ids':\n"
                         "An invalid toggle button identifier\n"
                         "has been passed into this routine.\n"
                         "Toggle button id = %d\n" , toggle_button_id ) ;
   }
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

Boolean getFfgIdState ( )
{
   return draw_ids ;
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

Boolean getFfgValueState ( )
{
   return draw_values ;
}
