/*******************************************************************************
* FILENAME:             external_plot_routines.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*
*   MODULE 1:           plot_binfile_basins
* DESCRIPTION:          This routine plots basins based on the information 
*			from basins binary file. The points that define  
*                       these basins are in a format that can be drawn by
*                       the maplib drawing routines.
*
*   MODULE 2:           plot_binfile_lakes
* DESCRIPTION:          This routine plots lakes/reservoirs  based on the information
*                       from reservoirs binary file. The points that define
*                       these reservoirs are in a format that can be drawn by
*                       the maplib drawing routines.
*
*   MODULE 3:           plot_binfile_rivers_streams
* DESCRIPTION:          This routine plots rivers and streamsbased on the 
*                       information from rivers and  streams binary files.
*                       The points that define these rivers and streams
*                       are in a format that can be drawn by
*                       the maplib drawing routines.
*
*   MODULE 4:           plot_binfile_hiways_roads
* DESCRIPTION:          This routine plots highways and roads based on the 
*                       information from highways and roads binary files.
*                       The points that define these highways and roads
*                       are in a format that can be drawn by
*                       the maplib drawing routines.
*
*
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        December 2001
* ORGANIZATION:         OHD/HSEB
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        12/5/01      Bryon Lawrence    Original Coding.
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>

#include "City.h"
#include "DbmsDefs.h"
#include "external_plot_routines.h"
#include "GeoArea.h"
#include "geoutil.h"
#include "List.h"
#include "map_bcdfile.h"
#include "map_convert.h"
#include "map_menubar_cb.h"
#include "stage3.h"

/*******************************************************************************
* MODULE NUMBER:  _plot_binfile_basins
* MODULE NAME:   1 
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
*
* DATA FILES AND/OR DATABASE:
* This routine reads the binary file and plots basins.
*
* ERROR HANDLING:
*    ERROR CODE                            DESCRIPTION
*
********************************************************************************
*/

void _plot_binfile_basins ( void * pData ,
			    int overlay ,
                            struct _Map_Segment ** seg ,
                            enum ShapeFileType * type ,
                            Boolean * fillarea )
{
   char filename [ OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ] ;
   struct _Overlay * pOverlay = NULL ;

   FILE       * fp = NULL ;
   float      * points = NULL ;
   HRAP       * hrap = NULL ;
   char         id [ LOC_ID_LEN + 1 ] ;
   char         name [LOC_NAME_LEN + 1 ] ;
   int          npts , order ;
   int          len_id = 9 ;
   int          len_name = 21 ;
   int          i , first = 0 ;
   int          j = 0 ;
   float        xlat , xlon ; 
 
   * seg = NULL ;
   
   pOverlay = ( struct _Overlay * ) pData ;
  
   /* Build the filepath. */
   if ( pOverlay->filename [ 0 ] == NULL )
   {
      return ;
   }

   if ( pOverlay->filepath [ 0 ] != NULL )
   {
      sprintf ( filename , "%s/%s" , pOverlay->filepath [ 0 ] ,
                pOverlay->filename [ 0 ] ) ;
   }
   else
   {
      sprintf ( filename , "%s" , pOverlay->filename [ 0 ] ) ;
   }
   
   /* Open binary file for reading. */
   fp = fopen ( filename , "r" ) ;

   if ( fp == NULL ) 
   {
      fprintf ( stderr , "\nIn routine '_plot_binfile_basins':\n"
                       "Error opening \"fp\", filename: %s.\n", filename ) ;
      return ;
   }
      
   fread ( id ,     sizeof(char) , len_id ,  fp ) ;
   
   while ( ! feof ( fp ) )  
   { 
      fread ( name ,   sizeof(char) , len_name, fp ) ;
      fread ( &order , sizeof(int) ,  1 ,       fp ) ;
      fread ( &npts ,  sizeof(int) ,  1 ,       fp ) ; 
      
      /* Allocate the memory that will be needed to contain all of the
      points in this binary file. */
      
      points = ( float * ) malloc ( sizeof ( float ) * npts * 2 ) ;
      if ( points == NULL )
      {
         fprintf ( stderr , "In routine _plot_binfile_basins:\n"
                        "attempt to malloc memory failed.\n" ) ;
         * seg = NULL ;
         return ;
      }
      j = 0 ;
      
      hrap = ( HRAP * ) malloc ( sizeof ( HRAP ) * npts ) ;
      if ( hrap == NULL )
      {
         fprintf ( stderr , "In routine _plot_binfile_basins:\n"
                        "attempt to malloc memory for hrap failed.\n" ) ;

        * seg = NULL ;
         return ;
      }

      fread ( hrap ,   sizeof ( HRAP ) , npts ,    fp ) ; 
      for ( i = 0 ; i < npts ; i++ )
      {
         xlat = hrap[i].x ;
 
         xlon = hrap[i].y ;

         points [ j++ ] = xlat ;
         points [ j++ ] = xlon * ( -1 ) ;
      }  
      /* for ( i = 0 ; i < npts ; i ++)
      {
         hrap_point.x = ( int ) hrap[i].x ;
         hrap_point.y = ( int ) hrap[i].y ;
         
         ll_hrap = HrapToLatLongMpe ( hrap_point ) ;
                
         points [ j++ ] =  (float ) ll_hrap.y ;
         
         points [ j++ ] =  (float) (ll_hrap.x * (-1)) ;
      } */ 
     
      _load_bcd_segment ( npts , seg , & first , points ) ;
   
      free ( hrap ) ;
      hrap = NULL ; 
      free ( points ) ;
      points = NULL ; 
      
     fread ( id ,     sizeof(char) , len_id ,  fp ) ; 
   }  /* End of while loop. */ 
   
   /* Close the file. */
   if ( fp != NULL )
   {
      fclose ( fp ) ;
      fp = NULL ;
   }
  
   * type = S_POLYLINE ; 
   return ; 
}

/*******************************************************************************
* MODULE NUMBER:  _plot_binfile_lakes
* MODULE NAME:   2 
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
*
* DATA FILES AND/OR DATABASE:
* This routine reads the binary file and plots lakes/reservoirs.
*
* ERROR HANDLING:
*    ERROR CODE                            DESCRIPTION
*
********************************************************************************
*/
void _plot_binfile_lakes ( void * pData ,
                            int overlay ,
                            struct _Map_Segment ** seg ,
                            enum ShapeFileType * type ,
                            Boolean * fillarea )
{
   char filename [ OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ] ;
   struct _Overlay * pOverlay = NULL ;

   FILE       * fp = NULL ;
   float      * points = NULL ;
   HRAP       * hrap = NULL ;
   char         id [ LOC_ID_LEN + 1 ] ;
   char         name [LOC_NAME_LEN + 1 ] ;
   int          npts , order ;
   int          len_id = 9 ;
   int          len_name = 21 ;
   int          i , first = 0 ;
   int          j = 0 ;
   float        xlat , xlon ;

   * seg = NULL ;
  
   pOverlay = ( struct _Overlay * ) pData ;
  
   /* Build the filepath. */
   if ( pOverlay->filename [ 0 ] == NULL )
   {
      return ;
   }

   if ( pOverlay->filepath [ 0 ] != NULL )
   {
      sprintf ( filename , "%s/%s" , pOverlay->filepath [ 0 ] ,
                pOverlay->filename [ 0 ] ) ;
   }

  else
   {
      sprintf ( filename , "%s" , pOverlay->filename [ 0 ] ) ;
   }
   /* Open binary file for reading. */
   fp = fopen ( filename , "r" ) ;

   if ( fp == NULL ) 
   {
      fprintf ( stderr , "\nIn routine '_plot_binfile_lakes':\n"
                       "Error opening \"fp\", filename: %s.\n", filename ) ;
      return ;
   }
     
   fread ( id ,     sizeof(char) , len_id ,  fp ) ;
  
   while ( ! feof ( fp ) )
   {   
      fread ( name ,   sizeof(char) , len_name, fp ) ;
      fread ( &order , sizeof(int) ,  1 ,       fp ) ;
      fread ( &npts ,  sizeof(int) ,  1 ,       fp ) ;

      /* Allocate the memory that will be needed to contain all of the
      points in this binary file. */

      points = ( float * ) malloc ( sizeof ( float ) * npts * 2 ) ;
      if ( points == NULL )
      {
         fprintf ( stderr , "In routine _plot_binfile_lakes:\n"
                        "attempt to malloc memory failed.\n" ) ;
         * seg = NULL ;
         return ;
      }
      j = 0 ;

      hrap = ( HRAP * ) malloc ( sizeof ( HRAP ) * npts ) ;
      if ( hrap == NULL )
      {
         fprintf ( stderr , "In routine _plot_binfile_lakes:\n"
                        "attempt to malloc memory for hrap failed.\n" ) ;

        * seg = NULL ;
         return ;
      }

      fread ( hrap ,   sizeof ( HRAP ) , npts ,    fp ) ;
      for ( i = 0 ; i < npts ; i++ )
      {
         xlat = hrap[i].x ;

         xlon = hrap[i].y ;

         points [ j++ ] = xlat ;
         points [ j++ ] = xlon * ( -1 ) ;
      }
      _load_bcd_segment ( npts , seg , & first , points ) ;
  
      free ( hrap ) ;
      hrap = NULL ;
      free ( points ) ;
      points = NULL ;

      fread ( id ,     sizeof(char) , len_id ,  fp ) ;
   }  /* End of while loop. */
  
   /* Close the file. */
   if ( fp != NULL )
   {
      fclose ( fp ) ;
      fp = NULL ;
   }
  * type = S_POLYLINE ;
   return ;
}

/*******************************************************************************
* MODULE NUMBER:  _plot_binfile_rivers_streams
* MODULE NAME:   3 
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
*
* DATA FILES AND/OR DATABASE:
* This routine reads the binary files and plots rivers and streams.
*
* ERROR HANDLING:
*    ERROR CODE                            DESCRIPTION
*
********************************************************************************
*/

void _plot_binfile_rivers_streams ( void * pData ,
                                  int overlay ,
                                  struct _Map_Segment ** seg ,
                                  enum ShapeFileType * type ,
                                  Boolean * fillarea )
{
   char filename [2] [ OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ] ;
   struct _Overlay * pOverlay = NULL ;

   FILE       * fp = NULL ;
   float      * points = NULL ;
   HRAP       * hrap = NULL ;
   char         id [ LOC_ID_LEN + 1 ] ;
   char         name [LOC_NAME_LEN + 1 ] ;
   int          npts , order ;
   int          len_id = 9 ;
   int          len_name = 21 ;
   int          i , k , first = 0 ;
   int          j = 0 ;
   float        xlat , xlon ;

   * seg = NULL ;
  
   pOverlay = ( struct _Overlay * ) pData ;
 
   if ( overlay == M_RIVERS ) 
   {
      /* Build the filepath. */
      if ( pOverlay->filename [ 0 ] == NULL )
      {
         return ;
      }

      if ( pOverlay->filepath [ 0 ] != NULL )
      {
        sprintf ( filename[0] , "%s/%s" , pOverlay->filepath [ 0 ] ,
                pOverlay->filename [ 0 ] ) ;
      }

      else
      {
         sprintf ( filename[0] , "%s" , pOverlay->filename [ 0 ] ) ;
      }
   }
   else if (overlay == M_STREAMS)
   {
      /* Build the filepath. */
      if ( pOverlay->filename [ 0 ] == NULL )
      {
         return ;
      }
      if ( pOverlay->n_files != 2 )
      {
         fprintf ( stderr , "\nIn routine \"external_plot_routines\":\n"
                         "This routine requires two files for drawing\n"
                         "rivers and streams.  One file is for the\n"
                         "rivers and one file is for the\n streams.\n" ) ;
         return ;
      }

      if ( pOverlay->filepath [ 0 ] != NULL )
      {
         sprintf ( filename[0] , "%s/%s" , pOverlay->filepath [ 0 ] ,
                pOverlay->filename [ 0 ] ) ;
      }

      else
      {
         sprintf ( filename[0] , "%s" , pOverlay->filename [ 0 ] ) ;
      }
      
      if ( pOverlay->filepath [ 1 ] != NULL )
      {
         sprintf ( filename[1] , "%s/%s" , pOverlay->filepath [ 1 ] ,
                pOverlay->filename [ 1 ] ) ;
      }
      else
      {
         sprintf ( filename[1] , "%s" , pOverlay->filename [ 1 ] ) ;
      }
   }
   for ( k = 0 ; k < pOverlay->n_files ; k++ )
   {  
      /* Open binary file for reading. */
      fp = fopen ( filename[k] , "r" ) ;

      if ( fp == NULL ) 
      {
         fprintf ( stderr , "\nIn routine '_plot_binfile_streams':\n"
                       "Error opening \"fp\", filename: %s.\n", filename[k] ) ;
         return ;
      }
     
      fread ( id ,     sizeof(char) , len_id ,  fp ) ;
  
      while ( ! feof ( fp ) )
      {   
         fread ( name ,   sizeof(char) , len_name, fp ) ;
         fread ( &order , sizeof(int) ,  1 ,       fp ) ;
         fread ( &npts ,  sizeof(int) ,  1 ,       fp ) ;

         /* Allocate the memory that will be needed to contain all of the
         points in this binary file. */

         points = ( float * ) malloc ( sizeof ( float ) * npts * 2 ) ;
         if ( points == NULL )
         {
            fprintf ( stderr , "In routine _plot_binfile_streams:\n"
                        "attempt to malloc memory failed.\n" ) ;
            * seg = NULL ;
            return ;
         }

         j = 0 ;

         hrap = ( HRAP * ) malloc ( sizeof ( HRAP ) * npts ) ;
         if ( hrap == NULL )
         {
            fprintf ( stderr , "In routine _plot_binfile_streams:\n"
                        "attempt to malloc memory for hrap failed.\n" ) ;

           * seg = NULL ;
           return ;
         }

         fread ( hrap ,   sizeof ( HRAP ) , npts ,    fp ) ;
         for ( i = 0 ; i < npts ; i++ )
         {
            xlat = hrap[i].x ;

            xlon = hrap[i].y ;

            points [ j++ ] = xlat ;
            points [ j++ ] = xlon * ( -1 ) ;
         }
         _load_bcd_segment ( npts , seg , & first , points ) ;
  
         free ( hrap ) ;
         hrap = NULL ;
         free ( points ) ;
         points = NULL ;

        fread ( id ,     sizeof(char) , len_id ,  fp ) ;
      }  /* End of while loop. */
  
      /* Close the file. */
      if ( fp != NULL )
      {
         fclose ( fp ) ;
         fp = NULL ;
      }
  
      * type = S_POLYLINE ;
   }
   return ;
}

/*******************************************************************************
* MODULE NUMBER:  _plot_binfile_hiways_roads
* MODULE NAME:   4 
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
*
* DATA FILES AND/OR DATABASE:
* This routine reads the binary files and plots hiways and roads 
*
* ERROR HANDLING:
*    ERROR CODE                            DESCRIPTION
*
*********************************************************************************/

void _plot_binfile_hiways_roads ( void * pData ,
                                int overlay ,
                                struct _Map_Segment ** seg ,
                                enum ShapeFileType * type ,
                                Boolean * fillarea )
{
   char filename [2] [ OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ] ;
   struct _Overlay * pOverlay = NULL ;

   FILE       * fp = NULL ;
   float      * points = NULL ;
   HRAP       * hrap = NULL ;
   char         id [ LOC_ID_LEN + 1 ] ;
   char         name [LOC_NAME_LEN + 1 ] ;
   int          npts , order ;
   int          len_id = 9 ;
   int          len_name = 21 ;
   int          i , k , first = 0 ;
   int          j = 0 ;
   float        xlat , xlon ;

   * seg = NULL ;
  
   pOverlay = ( struct _Overlay * ) pData ;
   
   if ( overlay == M_HIGHWAYS )
   {
      /* Build the filepath. */
      if ( pOverlay->filename [ 0 ] == NULL )
      {
         return ;
      }

      if ( pOverlay->filepath [ 0 ] != NULL )
      {
         sprintf ( filename [ 0 ] , "%s/%s" , pOverlay->filepath [ 0 ] ,
                pOverlay->filename [ 0 ] ) ;
      }

      else
      {
         sprintf ( filename [ 0 ] , "%s" , pOverlay->filename [ 0 ] ) ;
      }
   }
      else if (overlay == M_ROADS)
      {
         /* Build the filepath. */
         if ( pOverlay->filename [ 0 ] == NULL )
         {
            return ;
         }
         if ( pOverlay->n_files != 2 )
         {
            fprintf ( stderr , "\nIn routine \"external_plot_routines\":\n"
                         "This routine requires two files for drawing\n"
                         "roads and hiways.  One file is for the\n"
                         "roads and one file is for the hiways.\n" ) ;
            return ;
         }

         if ( pOverlay->filepath [ 0 ] != NULL )
         {
            sprintf ( filename [ 0 ] , "%s/%s" , pOverlay->filepath [ 0 ] ,
                pOverlay->filename [ 0 ] ) ;
         }

         else
         {
            sprintf ( filename [ 0 ] , "%s" , pOverlay->filename [ 0 ] ) ;
         }

         if ( pOverlay->filepath [ 1 ] != NULL )
         {
            sprintf ( filename[1] , "%s/%s" , pOverlay->filepath [ 1 ] ,
                pOverlay->filename [ 1 ] ) ;
         }

         else
         {
            sprintf ( filename [ 1 ] , "%s" , pOverlay->filename [ 1 ] ) ;
         }
      }
      for ( k = 0 ; k < pOverlay->n_files ; k++ )
      {
         /* Open binary file for reading. */
         fp = fopen ( filename[k] , "r" ) ;

      if ( fp == NULL ) 
      {
         fprintf ( stderr , "\nIn routine '_plot_binfile_hiways_roads':\n"
                        "Error opening \"fp\", filename: %s.\n", filename  [ k ] ) ;

         return ;
      }
     
         fread ( id ,     sizeof(char) , len_id ,  fp ) ;
  
      while ( ! feof ( fp ) )
      {   
         fread ( name ,   sizeof(char) , len_name, fp ) ;
         fread ( &order , sizeof(int) ,  1 ,       fp ) ;
         fread ( &npts ,  sizeof(int) ,  1 ,       fp ) ;

         /* Allocate the memory that will be needed to contain all of the
         points in this binary file. */

         points = ( float * ) malloc ( sizeof ( float ) * npts * 2 ) ;
         if ( points == NULL )
         {
            fprintf ( stderr , "In routine _plot_binfile_hiways_roads:\n"
                        "attempt to malloc memory failed.\n" ) ;
            * seg = NULL ;
            return ;
         }

         j = 0 ;

         hrap = ( HRAP * ) malloc ( sizeof ( HRAP ) * npts ) ;
         if ( hrap == NULL )
         {
            fprintf ( stderr , "In routine _plot_binfile_hiways_roads:\n"
                        "attempt to malloc memory for hrap failed.\n" ) ;

           * seg = NULL ;
           return ;
         }

         fread ( hrap ,   sizeof ( HRAP ) , npts ,    fp ) ;
         for ( i = 0 ; i < npts ; i++ )
         {
            xlat = hrap[i].x ;
            xlon = hrap[i].y ;

            points [ j++ ] = xlat ;
            points [ j++ ] = xlon * ( -1 ) ;
         }
         _load_bcd_segment ( npts , seg , & first , points ) ;
  
         free ( hrap ) ;
         hrap = NULL ;
         free ( points ) ;
         points = NULL ;

        fread ( id ,     sizeof(char) , len_id ,  fp ) ;
      }  /* End of while loop. */
  
      /* Close the file. */
      if ( fp != NULL )
      {
         fclose ( fp ) ;
         fp = NULL ;
      }
  
      * type = S_POLYLINE ;
   }
   return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
