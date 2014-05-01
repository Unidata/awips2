/*******************************************************************************
* FILENAME:            free_dqc_data.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          free_dqc_data
* DESCRIPTION:         This routine
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include <stdlib.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"

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
void free_dqc_data ( )
{
   int emonth;
   int i;
   extern int init_maxmin;
   int ier;
   int j;
   int k;
   int maxib;
   int num_precip_stations;
   int num_freezing_stations;
   int num_temp_stations;
   int smonth;
   struct hrap_grid * hrap_grid = NULL;
   extern struct isoh * isoh;
   extern struct maxmin * maxmin;
   extern struct map mean_areal_precip_global[];
   extern struct pdata pdata [];
   extern struct pcp * pcp;
   extern struct pcp * spf;
   extern struct tdata tdata [];
   extern struct pcp * tpf;
   extern struct zdata zdata [];

   get_prev_start_end_month ( & smonth, & emonth );

   hrap_grid = get_hrap_grid ( );
   maxib = get_num_basins ( );

   get_precip_station_list ( & num_precip_stations );
   get_temperature_station_list ( & num_temp_stations );
   get_freezing_station_list ( & num_freezing_stations );

   /* Free the precipitation data. */
   for ( i = 0; i < num_precip_stations; ++i )
   {
      for ( j = 0; j < MAX_GAGEQC_DAYS; ++j )
      {
         free ( pdata[j].stn[i].scons );
         free ( pdata[j].stn[i].rain );
         free ( pdata[j].stn[i].frain );
         free ( pdata[j].stn[i].frzlvl );
         free ( pdata[j].stn[i].snoflag );
         free ( pdata[j].stn[i].sflag );
         free ( pdata[j].stn[i].srain );
      }
   }

   /* Free the temperature data. */
   for ( i = 0; i < num_temp_stations; ++i )
   {
      for ( j = 0; j < MAX_GAGEQC_DAYS; ++j )
      {
         free ( tdata[j].stn[i].tlevel1 );
         free ( tdata[j].stn[i].tlevel2 );
      }
   }

   /* Free the freezing level data. */
   for ( i = 0; i < num_freezing_stations; ++i )
   {
      for ( j = 0; j < MAX_GAGEQC_DAYS; ++j )
      {
         free ( zdata[j].stn[i].zlevel1);
         free ( zdata[j].stn[i].zlevel2);
      }
   }

   /* Free the station lists. */ 
   free_precip_station_list ( );
   free_temperature_station_list ( );
   free_freezing_station_list ( );

   /* Free the precipitation climatological data. */
   /* Determine the starting and ending climatological months. */ 
   if ( isoh != NULL )
   {
      for ( k = 0; k < 12; k++ )
      {
         ier = is_good ( k, smonth, emonth );

         if ( ier != -1 )
         {
            for ( i = 0; i < MAXY; ++i )
            {
               free ( isoh->value[k][i] );
            }

            free ( isoh->value[k] );
         }
      }

      free ( isoh->value );
      isoh->value = NULL;
      free (isoh );
      isoh = NULL;
   }

   /* Free the temperature climatological data. */
   if ( maxmin != NULL )
   {
      for ( k = 0; k < 12; k++ )
      {
         ier = is_good ( k, smonth, emonth );

         if ( ier != -1 )
         {
            for ( i = 0; i < MAXY; ++i )
            {
               free ( maxmin->maxvalue[k][i] );
               free ( maxmin->minvalue[k][i] );
            }

            free ( maxmin->maxvalue[k] );
            free ( maxmin->minvalue[k] );
         }
      }

      free ( maxmin->maxvalue );
      free ( maxmin->minvalue );
      free (maxmin );
      maxmin = NULL;
      init_maxmin = -1;
   }

   /* Free the topography memory. */
   /* Free the hrap_grid memory.  There is alot of it. */
   if ( hrap_grid != NULL )
   {
      /* Only free memory for the starting and ending months. */
      for ( k = 0; k < 12; k++ )
      {
         ier = is_good ( k, smonth, emonth );
  
         if ( ier != -1 )
         {
            /* Free the max, min and isoh members. */
            for ( i = 0; i < MAXX; ++i )
            {
                free( hrap_grid->max[k][i] );
                free( hrap_grid->min[k][i] );
                free( hrap_grid->isoh[k][i] ); 
            }

            free ( hrap_grid->max[k] );
            free ( hrap_grid->min[k] );
            free ( hrap_grid->isoh[k] );
         }
         
      }

      /* Free the coord, gage, owner, elev members of the hrap_grid
         structure. */
      for ( i = 0; i < MAXX ; ++i )
      {
         free ( hrap_grid->coord [ i ] );
         free ( hrap_grid->gage [ i ] );
         free ( hrap_grid->owner [ i ] );
         free ( hrap_grid->elev [ i ] );
      }

      free ( hrap_grid->coord );
      free ( hrap_grid->gage );
      free ( hrap_grid->owner );
      free ( hrap_grid->elev );
      free ( hrap_grid->max );
      free ( hrap_grid->min );
      free ( hrap_grid->isoh );

      free ( hrap_grid );
      hrap_grid = NULL;
   }

   /* Free the tpf memory. */
   for ( i = 0; i < MAXX + 1000; ++i )
   {
      free(tpf->value[i]);
   }
 
   /* Free the pcp and spf memory. */
   for ( i = 0; i < MAXX; ++i )
   {
      free(pcp->value[i]);
      free(spf->value[i]);
   }

   free ( pcp->value );
   free ( tpf->value );
   free ( spf->value );

   free ( pcp );
   free ( tpf );
   free ( spf );


   pcp = NULL;
   tpf = NULL;
   spf = NULL;

   /* Free the basin information. */
   for ( i = 0; i < maxib; ++i )
   {
      free ( mean_areal_precip_global[i].basin );
      free ( mean_areal_precip_global[i].gz );
      free ( mean_areal_precip_global[i].uz );
      free ( mean_areal_precip_global[i].mz );
      free ( mean_areal_precip_global[i].lz );
      free ( mean_areal_precip_global[i].gzc );
      free ( mean_areal_precip_global[i].uzc );
      free ( mean_areal_precip_global[i].mzc );
      free ( mean_areal_precip_global[i].lzc );
      free ( mean_areal_precip_global[i].zgz );
      free ( mean_areal_precip_global[i].zuz );
      free ( mean_areal_precip_global[i].zmz );
      free ( mean_areal_precip_global[i].zlz );
      free ( mean_areal_precip_global[i].tgz );
      free ( mean_areal_precip_global[i].tuz );
      free ( mean_areal_precip_global[i].tmz );
      free ( mean_areal_precip_global[i].tlz );
      free ( mean_areal_precip_global[i].maps_done );
      free ( mean_areal_precip_global[i].tmaps_done );
      free ( mean_areal_precip_global[i].zmaps_done );
      free ( mean_areal_precip_global[i].hrap_data );
   }
}
