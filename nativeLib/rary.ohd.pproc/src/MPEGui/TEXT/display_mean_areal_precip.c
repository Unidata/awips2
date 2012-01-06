#include <float.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <Xm/ToggleB.h>
#include <Xm/Xm.h>
#include "display_mean_areal_precip.h"
#include "GeneralUtil.h"
#include "map.h"
#include "map_convert.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "geoutil.h"
#include "get_geoareainfo.h"
#include "List.h"
#include "rfcwide.h"

static int draw_multi_hour_ids = 0 ;     
static int draw_multi_hour_values = 0 ;
                                 
static struct MeanArealPrecip * pMeanPrecipHead = NULL ;

static int multi_hour_ids_status = M_OFF ;
static int multi_hour_values_status = M_OFF ;

static float min_coverage ;

void turnOnMultiHourIds ( )
{
   draw_multi_hour_ids = 1 ;
   if (  multi_hour_ids_status == M_OFF )
      multi_hour_ids_status = M_ON ; 
}

void turnOnMultiHourValues ( )
{
   draw_multi_hour_values = 1 ;
   if ( multi_hour_values_status == M_OFF )
      multi_hour_values_status = M_ON ;
} 

void turnOffMultiHourIds ( )
{
   draw_multi_hour_ids = 0 ;
   if ( multi_hour_ids_status == M_ON )
      multi_hour_ids_status = M_OFF ;
}

void turnOffMultiHourValues ( )
{
   draw_multi_hour_values = 0 ;
   if ( multi_hour_values_status == M_ON )
      multi_hour_values_status = M_OFF ;
}

int isThereMultiHourIds ( )
{
   return draw_multi_hour_ids ;
}

int isThereMultiHourValues ( )
{
   return draw_multi_hour_values ;
}

void mean_precip_showCB ( Widget w, XtPointer ptr, XtPointer cbs )
{
   int button , values , ids ;

   XmToggleButtonCallbackStruct * tb_state =
                                ( XmToggleButtonCallbackStruct * ) cbs ;

   button = (int) ptr ;
   
   /* Draw options under "Mean Precip" for mean values / station ids ,
      values = 1 if toggle is set else values = 0 */
      
      if (button == 0)
      {
        values = tb_state->set ;
       logMessage ("Values: %d\n", values ) ;
        if ( values == 1)
        {  
          turnOnMultiHourValues ( ) ;
        }
        else
        {
           turnOffMultiHourValues ( ) ;
        }
      }

      else if (button == 1)
      {
         ids  = tb_state->set ;
        logMessage ("Ids: %d\n", ids ) ;
         if ( ids == 1 )
         { 
            turnOnMultiHourIds ( ) ;
         }
         else
         {
            turnOffMultiHourIds ( ) ;
         }
     }
     return ;
}

static void get_mean_areal_coverage ( )
{
   char reply [ MAX_MPE_STRING_LEN ] ;
   int reply_len ;
   int request_len ;
   int status ;
   static char * min_coverage_token = "whfs_min_area_covered" ;

   request_len = strlen ( min_coverage_token ) ;

   /* Get the minimum coverage. */
   status = get_apps_defaults ( min_coverage_token ,
                                & request_len ,
                                reply , 
                                & reply_len ) ;
 
   if ( status != 0 || reply_len == 0 )
   {
      flogMessage ( stderr , "In routine \"draw_mean_areal_precip\":\n"
                         "Could not retrieve the value of token\n"
                         "\"whfs_min_area_covered\".  Using %4.2f as the\n"
                         "default coverage value.\n" ,  DEFAULT_COVERAGE ) ;
      min_coverage = DEFAULT_COVERAGE ;
   }
   else
   {
      min_coverage = atof ( reply ) ;
   } 
}

static void draw_mean_areal_precip ( int ** data ,
                                     struct MeanArealPrecip * pMeanPrecipHead ,
                                     int xor , int yor , int max_columns ,
                                     int max_rows )
{
   register int col ;
   register int i ;
   register int jcol ;
   register int row ;
   struct MeanArealPrecip * pMeanPrecip = NULL ;
   
   get_mean_areal_coverage ( ) ;
   
   if ( pMeanPrecipHead == NULL )
   {
      flogMessage ( stderr , "\nIn routine 'draw_mean_areal_precip':\n"
                         "No mean areal precipitation exists.\n" ) ;
      return ;
   }
   
   /* Walk over the linked list. */
   pMeanPrecip = ( struct MeanArealPrecip * ) 
                 ListFirst ( & pMeanPrecipHead->list ) ;

   while ( pMeanPrecip != NULL )
   {
      /* loop on the number of rows for this basin */
      for ( i = 0 ; i < pMeanPrecip->numrows ; ++ i )
      {     
         /* loop on the number of columns in each row */
         for ( jcol = pMeanPrecip->beg_cols [ i ] ; 
               jcol <= pMeanPrecip->end_cols [ i ] ; ++ jcol )
         {	 
            row = pMeanPrecip->rows [ i ] - yor ;
            col = ( jcol - xor ) ;

            /*-----------------------------------------------*/
            /*  check that box is within site's area         */
            /*  if not, return with status set to -1         */
            /*-----------------------------------------------*/

            if ( ( row < max_rows ) && 
                 ( col < max_columns ) && 
                 ( row >= 0 ) && ( col >= 0 ) )
            {
               if ( pMeanPrecip->area_covered > min_coverage )
               {
                  data [ col ] [ row ] =  ( int ) pMeanPrecip->avg_val ;
               }
               else
               {
                  data [ col ] [ row ] = -999 ;
               }
            }
	    
         }
      }

      pMeanPrecip = ( struct MeanArealPrecip * ) 
                    ListNext ( & pMeanPrecip->node ) ; 
   }
}

static void compute_mean_areal_precip ( int ** data , 
                                        struct MeanArealPrecip * pMeanPrecip ,
                                        int xor , int yor , int max_columns , 
                                        int max_rows )
{
   float        cur_max ;
   float        cur_min ;
   int          col ;
   int 		i ;
   int          jcol ;
   int          miss_cnt ;
   int          row ; 
   int          total_cnt ;
   int		val_cnt ; 
   float 	raw_val ; 
   float        sum ;
   struct MeanArealPrecip * pNode = NULL ;

   if ( pMeanPrecip == NULL )
   {
      flogMessage ( stderr , "\nIn routine 'compute_mean_areal_precip':\n"
                         "No lineseg information exists.  Cannot\n"
                         "calculate mean areal precipitation.\n" ) ; 
      return ;
   }

   /* Initialize the linked list. */
   pNode = ( struct MeanArealPrecip * ) ListFirst ( & pMeanPrecip->list ) ;

   while ( pNode != NULL )
   {
      /* initialize */
      miss_cnt = total_cnt = val_cnt = 0;
      cur_max = FLT_MIN ;
      cur_min = FLT_MAX ;
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
            /*  check that box is within site's area         */
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
               raw_val = ( float ) data [ col ] [ row ] ;

               if ( raw_val >= 0.0 )
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
      
      pNode = ( struct MeanArealPrecip * ) ListNext ( & pNode->node ) ; 

   }

   return ;
}

int display_mean_areal_precip ( int ** data , int xor ,
                                int yor , int max_columns , int max_rows ,
                                const char * area_type )
{
   int          i ;
   int          j ;
   
   if (  pMeanPrecipHead != NULL )
   {
     /* Delete the linked list of MeanPrecip structures. */
     free_area_linesegs_list ( ( void * ) pMeanPrecipHead ) ;
     pMeanPrecipHead = NULL ;
   }
   
   /* Retrieve a linked list of MeanArealPrecip structures. 
      In each structure the area_id, node, numrows, rows, beg_cols, end_cols,
      and list elements will be defined. */
   pMeanPrecipHead = ( struct MeanArealPrecip * ) 
                     get_area_linesegs ( area_type ,
                                         sizeof ( struct MeanArealPrecip ) ) ;

   /*-------------------------------------------------------------*/
   /*   compute average precipitation value for basin and areal   */
   /*   coverage.                                                 */
   /*-------------------------------------------------------------*/
   compute_mean_areal_precip ( data , pMeanPrecipHead , xor , yor , 
                               max_columns , max_rows  ) ;

   /* Zero out the grid. */
   for ( i = 0 ; i < max_columns ; ++ i )
   {
      for ( j = 0 ; j < max_rows ; ++ j )
      {
         data [ i ] [ j ] = 0 ;
      }
   }

   draw_mean_areal_precip ( data , pMeanPrecipHead , xor , yor ,
                            max_columns , max_rows ) ;
   return 0;
}

void drawMultiHourGeoAccumLabels ( )
{
   char areaColor [ 20 ]  = "white" ;
   float factor ;
   int status ;
   int xpos = 0 ;
   int ypos = 0 ;
   char values_str [7] ;
   GeoAreaInfo * geo_area_info = NULL ;
   struct MeanArealPrecip * pMeanPrecipNode = NULL ;  
   
   if ( pMeanPrecipHead != NULL)
   {
     pMeanPrecipNode = ( struct MeanArealPrecip * ) 
			ListFirst ( & pMeanPrecipHead->list ) ;
     while (pMeanPrecipNode != NULL )
     {
       geo_area_info = get_geoareainfo ( pMeanPrecipNode->area_id , & status ) ;
       if ( geo_area_info == NULL)
        logMessage ( "Error in getting geo_area info in "
                  "drawMultiHourGeoAccumLabels routine\n" ) ;
       else
       { 
         get_mean_areal_coverage ( ) ;
         
         if ( pMeanPrecipNode->area_covered > min_coverage )
         { 
            /* Convert the interior latitude and longitude to pixel coordinates. */
            mConvertLatLon2XY ( geo_area_info->interior_lat ,
                                -1 * geo_area_info->interior_lon ,
                                & xpos ,
                                & ypos ) ; 
            mSetColor ( areaColor ) ;
            if ( draw_multi_hour_ids != 0)
            {        
               /* Draw the mean precipitation ids. */
               mDrawText ( M_EXPOSE , 0 , xpos , ypos + GEOAREA_ID_OFFSET ,
                        pMeanPrecipNode->area_id ) ;
           
               draw_multi_hour_ids = 1 ;
            }

            if ( draw_multi_hour_values != 0 )
            {
               factor = ( float ) scale_factor * units_factor ;
               sprintf ( values_str , "%6.2f" , 
                      ( pMeanPrecipNode->avg_val / factor ) ) ;
               /* Draw the mean precipitation values. */
               mDrawText ( M_EXPOSE , 0 , xpos , ypos + GEOAREA_VALUE_OFFSET , 
                           values_str ) ;
            }
          } 
        } 
        pMeanPrecipNode = ( struct MeanArealPrecip * )
                        ListNext ( & pMeanPrecipNode->node ) ;
    } 
  }
}

void free_mean_areal_precip ( ) 
{
  if ( pMeanPrecipHead != NULL )
  {
     free_area_linesegs_list ( ( void * ) pMeanPrecipHead ) ;
     pMeanPrecipHead = NULL;
  }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/display_mean_areal_precip.c,v $";
 static char rcs_id2[] = "$Id: display_mean_areal_precip.c,v 1.12 2007/02/22 16:05:36 lawrence Exp $";}
/*  ===================================================  */

}
