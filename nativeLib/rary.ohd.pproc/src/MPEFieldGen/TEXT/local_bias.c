/******************************************************************************
* FILENAME:            local_bias.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:  local_bias_initialize 
* DESCRIPTION: Subroutine to write the initial conditions for the state
*              variables.
*   MODULE 2:  find_array_indices
* DESCRIPTION: Find array indices for spiral search.
*   MODULE 3:  lltime
* DESCRIPTION: Converts an integer time value of YYYYMMDDHH to a struct tm.
*   MODULE 4:  diftime
* DESCRIPTION: Determines the number of hours difference between the hour
*              for which data are being processed and the run time of the
*              MPE Fieldgen application.
*   MODULE 5:  find_nbrs1X
* DESCRIPTION: Locates all data points within the radius of influence using
*              double heap sorting.
*   MODULE 6:  correl1
* DESCRIPTION: Subroutine specifies spatial correlation coefficient at
*              separation distance > 0 (km)
*   MODULE 7:  correl2
* DESCRIPTION:
*   MODULE 8:  err_cor_mat
* DESCRIPTION:
*   MODULE 9:  lsolve_array
* DESCRIPTION:
*   MODULE 10: setupLocalBiasParams
* DESCRIPTION:
*   MODULE 11:  getLocalBiasParams
* DESCRIPTION:
*   MODULE 12:  update
* DESCRIPTION:
*   MODULE 13:  srch_nbrs1
* DESCRIPTION:
*   MODULE 14:  interpolate_nearby
* DESCRIPTION:
*   MODULE 15:  find_nbrs1
* DESCRIPTION:
*   MODULE 16:  interpolate_everywhere
* DESCRIPTION:
*   MODULE 17:  local_bias
* DESCRIPTION:
*
* ORIGINAL AUTHOR:      Guoxian Zhou
* CREATION DATE:        January 11, 2005
* ORGANIZATION:         HSEB/OHD
* MACHINE:              HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
*    DATE           PROGRAMMER        DESCRIPTION/REASON
*    May  11, 2005  Bryon Lawrence    Complete port from Fortran to C.
*    May  12, 2005  Bryon Lawrence    Added internal documentation.  Starting
*                                     component testing.
*    June 20, 2005  Bryon Lawrence    Tested find_array_indices.
*    June 20, 2005  Bryon Lawrence    Tested difftime
*    June 20, 2005  Bryon Lawrence    Tested correl1
*    June 20, 2005  Bryon Lawrence    Tested correl2
*    June 20, 2005  Bryon Lawrence    Tested lsolve
********************************************************************************
*/
#include <errno.h>
#include <math.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include "convert_hrap.h"
#include "local_bias_params.h"
#include "mpe_params.h"
#include "multi_sensor.h"
#include "mpe_fieldgen.h"

/* Accessor function must be used to get a read-only copy of this
   structure. */
static int first = 1;
static local_bias_params localBiasParams;

/* Definition of constants for use in local bias computations. */
static const int unit = 78;
static const int int_mult_hour = 4;
static const int nhr_look_back = 24;
static const int type_of_est = 1;
static const float radius = 100.0;
static const int nborx = 400;
static const float rho0 = 0.0;
static const float cor_range = 64.0;
static const float sel_npr_lb_rad = 10.0;
static const float sel_npr_lb_sat = 5.0;
static const int mult = 4;
static const int bory = 10;
static const int interp = 2;
static const float dist_cut = 40.0;

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:   local_bias_initialize
* PURPOSE:       Subroutine to write the initial conditions for state
*                variables.
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
static int local_bias_initialize ( const geo_data_struct *  pGeoData, 
                        const local_bias_params * pLocalBiasParams,
                        const mpe_params_struct * pMPEParams,
                        const char * filename, int num_seq )
{
   char * file_error_string = NULL;
   FILE * pStateFile = NULL;
   int i;
   int j;
   int k;
   int status = 0;
   local_bias_values_record * local_bias_records = NULL;
   local_bias_values_record local_bias_record;
   size_t item_count;

   local_bias_records = ( local_bias_values_record * ) malloc (
                            num_seq * sizeof ( local_bias_values_record ) );

   if ( local_bias_records == NULL )
   {
       printMessage ( "Could not allocate memory for local_bias_record "
                      " array.\n", logFile );
       status = 1;
   }
   else
   {
   /* Open the state variable file for writing. */
   pStateFile = fopen ( filename, "w" );

   if ( pStateFile != NULL )
   {
      local_bias_record.datetime = -9999;

      /* Zero out the memory span arrays. */
      for ( i = 0; i < pMPEParams->ptrRWBiasStat->num_span; ++i )
      {
         local_bias_record.local_bias_val [ i ].si = 0.0;
         local_bias_record.local_bias_val [ i ].xg = 0.0;
         local_bias_record.local_bias_val [ i ].xr = 0.0;
      }

      k = 0;

      for ( i = 0; i < pGeoData->num_cols; i += pLocalBiasParams->mult )
      {
         for ( j = 0; j < pGeoData->num_rows; j += pLocalBiasParams->mult )
         {
             local_bias_record.col = i;
             local_bias_record.row = j;
             local_bias_records [ k ] = local_bias_record;
             ++k;
         }
      }

      errno = 0;
      item_count = fwrite ( local_bias_records,
                            sizeof ( local_bias_values_record ),
                            num_seq,
                            pStateFile );

      if ( item_count != (size_t) num_seq )
      {
         sprintf ( message, "\n  Could not read complete record from file "
                            "%s.  -- local bias not calculated.\n"
                            "     Reason : ", filename );
         file_error_string = strerror ( errno );
         strcat ( message, file_error_string );
         printMessage ( message, logFile );
         status = 1;
      }

      fclose ( pStateFile );
      pStateFile = NULL;
   }
   else
   {
      sprintf ( message, "\n  Could not open file %s for writing. "
                         "  -- local bias not calculated.\n", filename );
      printMessage ( message, logFile );
      status = 1;
   }

   free ( local_bias_records );
   local_bias_records = NULL;

}

   return status;
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

void MPEFieldGen_find_array_indices( local_bias_params * pLocalBiasParams )
{
/**
  *    This function is converted from fortran code
  * Original: subroutine find_array_indices(imult,n,idis,jdis,nbory)
  * Original Args: imult  = Update state variables once every imult bins
  *                         along the i and j directories.
  *                n      = A counter for the number of data points in the
  *                         idis and jdis arrays.
  *                idis   = The array containing the x index of each
  *                         data point.
  *                jdis   = The array containing the y index of each
  *                         data point.
  *                nbory  = The maximum number of neighbors used in
  *                         filling holes.
  *
  * original version: Jun 08, 2000 by D.-J. Seo at NWS/HRL
  * Purpose: find array indices for spiral search
  *
  * input:
  * local_bias_param * localBiasParams
  *
  **/

   double * dist = NULL;

   /* determine the search domain */
   int iradi = 2 * pLocalBiasParams->mult;

   int i;
   int ix = iradi + 1;
   int iy =iradi + 1;
   int j;
   int n = 0;

   dist = ( double * ) malloc ( NIND * sizeof ( double ));

   if ( dist == NULL )
   {
       printMessage ( "Could not allocate memory for arrays in find_array_indices.\n", logFile );
       return;
   }

   for ( i = 1; i <= ( 2 * iradi + 1 ); ++ i )
   {
      for ( j = 1; j <= ( 2 * iradi + 1); ++ j )
      {

         n++;
         if ( n >= NIND )
         {
            /* Write an error message to the log file. */
            sprintf ( message, "find_array_indices - n gt nind ... stop "
                               "and reset nind to a larger value.\n" );
            printMessage ( message, logFile );
            free ( dist );
            dist = NULL;
            return;
         }

         pLocalBiasParams->idis[n]= i - ix;
         pLocalBiasParams->jdis[n]= j - iy;
         dist[n] = pLocalBiasParams->idis[n] * pLocalBiasParams->idis[n]
                 + pLocalBiasParams->jdis[n] * pLocalBiasParams->jdis[n] ;
         dist[n] = sqrt ( dist[n] );

      }
   }

   sort_double_short_short ( n,
                             dist,
                             pLocalBiasParams->idis,
                             pLocalBiasParams->jdis );

   for ( i = 1; i < n; ++i )
   {
      if ( dist[i] > iradi )
      {
         n = i;

         /* check if the number of data points in the spiral search domain
            is greater than or equal to the number of neighbors to be
            located */
         if ( n < pLocalBiasParams->nbory )
         {
            sprintf ( message, "find_array_indices - n lt nbory...stop\n" );
            free ( dist );
            dist = NULL;
            shutDownMPE ( message, logFile );
         }

         break;
      }
   }

   /* Shift the contents of the arrays idis and jdis arrays. */

/*   for ( i = 0; i < n; ++i )
   {
      pLocalBiasParams->idis[i] = pLocalBiasParams->idis[i+1];
      pLocalBiasParams->jdis[i] = pLocalBiasParams->jdis[i+1];
   } */

   free ( dist );
   dist = NULL;
   return ;
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
static void lltime ( int yyyymmddhh, struct tm * pStructTm )
{
   int iday;
   int ihr;
   int iyr;
   int mon;

   iyr = yyyymmddhh / 1000000;
   mon = yyyymmddhh - ( iyr * 1000000 );
   mon = mon / 10000;
   iday = yyyymmddhh - ( iyr * 1000000 ) - ( mon * 10000 );
   iday = iday / 100;
   ihr = yyyymmddhh  - ( iyr * 1000000 ) - ( mon * 10000 ) - ( iday * 100 );

   pStructTm->tm_year = iyr - 1900;
   pStructTm->tm_mon = mon - 1;
   pStructTm->tm_mday = iday;
   pStructTm->tm_hour = ihr;
   pStructTm->tm_min = 0;
   pStructTm->tm_sec = 0;

   return ;
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
static int get_lltime ( time_t time_in_ticks )
{
   char time_string [ YYYYMMDDHH_LEN + 1 ];
   int time;
   struct tm * pTimeStruct = NULL;

   pTimeStruct = gmtime ( & time_in_ticks );
   strftime ( time_string, YYYYMMDDHH_LEN + 1, "%Y%m%d%H", pTimeStruct );
   time = atoi ( time_string );
   return time;
}



/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
*
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
static int diftime ( const run_date_struct * pRunDate, int previous_time )
{
   int lag = -1;
   struct tm tm;
   time_t prev_timet;

   if ( previous_time < 0 )
   {
      /* This is the very first calculation */
      lag = 0;
      return lag;
   }

   /* Convert the previous time which is an integer of format
      YYYYMMDDHH to UNIX ticks. */
   lltime ( previous_time, & tm );
   prev_timet = gm_mktime ( & tm );

   if ( pRunDate->tRunTime < prev_timet )
   {
      sprintf ( message, "itime2 lt itime1 - no calc of locspan, biasloc "
                         "itime1: %ld  itime2: %ld\n", pRunDate->tRunTime,
                          prev_timet );
      printMessage ( message, logFile );
      lag = -99;
   }
   else
   {
      lag = ( pRunDate->tRunTime - prev_timet ) / SECONDS_PER_HOUR ;
   }

   return lag;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:       This subroutine locates all data points within the radius of
*                influence via double heap-sorting.
*
* ARGUMENTS:
*  TYPE   DATA TYPE        NAME                 DESCRIPTION/UNITS
*  gage_table_struct *     pGageTable
*  gage_radar_pair_table * pGageRadarPair
*  geo_data_struct *       pGeoData
*  int                     iu0
*  int                     iv0
*  short *                 ilist
*  float *                 rlist
*  int                     iu0_prev
*  int                     m_save
*  short *                 ivv_save
*  short *                 in_save)
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
void MPEFieldGen_find_nbrs1X ( const gage_radar_pair_table_struct * pGageRadarPair,
                   int * k,
                   int iradi,
                   int iu0,
                   int iv0,
                   short * ilist,
                   float * rlist,
                   int * iu0_prev,
                   int * m_save,
                   short * ivv_save,
                   short * in_save)
{
/* Original variables and documentation ... */
/**********************************************************************
c
c subroutine locates all data points within the radius of influence via
c double heap-sorting
c
c version Jun 08, 2000 by D.-J. Seo at NWS/HRL
C Modified Dec 3, 2003 by Russell Erb OHD/HL/HSEB
c
c input variables
c
c n     - number of data points
c iu    - array of HRAP x-coordinates
c iv    - array of HRAP y-coordinates
c iu0   - HRAP x-coordinate at the point of estimation
c iv0   - HRAP y-coordinate at the point of estimation
c iradi - radius of influence in HRAP bins
c
c output variables
c
c k     - number of neighbors located
c ilist - array of the indices of the neighbors
c rlist - array of the distances to the neighbors
c
C I/O variables which hold previous values
C
C iu0_prev - HRAP x-coordinate of previous point of estimation
C m        - preserve number of values sorted
C ivv      - preserve copy of previous ivv array
C in       - preserve copy of previous in  array */

   /* Variable declarations. */
   double dist;
   double xdis;
   double ydis;
   int i;
   int j = 0;
   int i0;
   int it;
   int m = 0;
   short * jn = NULL;

   jn = ( short * ) malloc ( sizeof ( short ) * NBMAX );

   if ( jn == NULL )
   {
       printMessage ( "Could not allocate memory for arrays in find_nbrs1X.\n", logFile );
       return;
   }

   /* if iu0 is the same as the previous one then skip to 41 */
   if (iu0 != * iu0_prev)
   {
      /* iu, iv, and z arrays must be sorted in the ascending order of iu
         before calling this subroutine. */
      for ( i = 0; i < pGageRadarPair->pairNum; ++i )
      {
         if ( iu0 <= pGageRadarPair->ptrGageRadarPair[i].hrap_x ) break;
      }

      i0 = i - 1;

      if ( i0 == -1 )
      {
         /* iu0 is the smallest */

         for ( i = 0; i < pGageRadarPair->pairNum; ++i )
         {
            if ( ( pGageRadarPair->ptrGageRadarPair[i].hrap_x - iu0 ) > iradi)
            {
               break;
            }

            ivv_save [ m ] = pGageRadarPair->ptrGageRadarPair[i].hrap_y ;
            in_save [ m ] = i;
            ++m;
         }
      }
      else if ( i0 == pGageRadarPair->pairNum - 1 )
      {
         /* u0 is the largest */

         for ( i = 0; i < pGageRadarPair->pairNum; ++i )
         {
            it = pGageRadarPair->pairNum - i - 1;

            if ( ( iu0 - pGageRadarPair->ptrGageRadarPair[it].hrap_x) > iradi)
            {
               break;
            }

            ivv_save [ m ] = pGageRadarPair->ptrGageRadarPair[it].hrap_y;
            in_save [ m ] = it;
            ++m;
         }
      }
      else
      {
         /* u0 is somewhere in between */

         for ( i = i0 + 1; i < pGageRadarPair->pairNum; ++i )
         {
            if ( ( pGageRadarPair->ptrGageRadarPair[i].hrap_x - iu0 ) > iradi)
            {
               break;
            }

            ivv_save [ m ] = pGageRadarPair->ptrGageRadarPair[i].hrap_y;
            in_save [ m ] = i;
            ++m;
         }

         for ( i = 0; i <= i0; ++i )
         {
            it = i0 - i;

            if ( ( iu0 - pGageRadarPair->ptrGageRadarPair[it].hrap_x ) > iradi)
            {
               break;
            }

            ivv_save [ m ] = pGageRadarPair->ptrGageRadarPair[it].hrap_y;
            in_save [ m ] = it;
            ++m;
         }
      }

      if ( m > 1 )
      {
         sort_short_short ( m, ivv_save, in_save );
      }

      * iu0_prev = iu0;
      * m_save = m;
   } /* Closing brace for the iu0 and iu0_prev comparison. */

   for ( i = 0; i < * m_save; ++i )
   {
      if ( iv0 <= ivv_save [ i ] ) break;
   }

   i0 = i - 1;

   if ( i0 == -1 )
   {
      /* iv0 is the smallest */

      for ( i = 0; i < * m_save; ++i )
      {
         if ( ( ivv_save[i] - iv0 ) > iradi ) break;

         if ( j >= NBMAX )
         {
            sprintf ( message , "j gt nbmax...stop & reset nbmax to a "
                                "larger value\n" );
            printMessage ( message, logFile );

            free ( jn );
            jn = NULL;
            return;
         }

         jn [ j ] = in_save [ i ];
         ++j;
      }
   }
   else if ( i0 == * m_save - 1 )
   {
      /* iv0 is the largest */

      for ( i = 0; i < * m_save; ++i )
      {
         it = * m_save - i - 1;

         if ( ( iv0 - ivv_save [ it ] ) > iradi ) break;

         if ( j >= NBMAX )
         {
            sprintf ( message, "j gt nbmax...stop & reset nbmax to a larger "
                               "value\n" );
            printMessage ( message, logFile );
            free ( jn );
            jn = NULL;
            return;
         }

         jn [ j ] = in_save [ it ];
        ++j;
      }
   }
   else
   {
      /* iv0 is somewhere in between */
      j = 0;

      for ( i = i0 + 1; i < * m_save; ++i )
      {
         if ( ( ivv_save[i] - iv0 ) > iradi ) break;

         if ( j >= NBMAX )
         {
            sprintf ( message, "j gt nbmax...stop & reset nbmax to a larger "
                               "value\n" );
            printMessage ( message, logFile );
            free ( jn );
            jn = NULL;
            return;
         }

         jn[j]=in_save[i];
         ++j;
      }

      for ( i = 0; i <= i0; ++i )
      {
         it = i0 - i;

         if ( ( iv0 - ivv_save[it] ) > iradi ) break;

         if ( j > NBMAX )
         {
            sprintf ( message, "j gt nbmax...stop & reset nbmax to a larger "
                               "value\n" );
            printMessage ( message, logFile );
            free ( jn );
            jn = NULL;
            return;
         }

         jn[j]=in_save[it];
         ++j;
      }
   }

   /* calculate the distance */
   * k = 0;  /* Initialize the number of neighbors found to 0. */

   for ( i = 0; i < j; ++i )
   {
      xdis = ( double ) ( pGageRadarPair->ptrGageRadarPair[ jn[ i ] ].hrap_x
                         - iu0 );
      ydis = ( float ) ( pGageRadarPair->ptrGageRadarPair[ jn[ i ] ].hrap_y
                         - iv0 );

      dist = (double) ( xdis * xdis + ydis * ydis );
      dist = sqrt ( dist );
      if ( dist > (double) iradi ) continue;
      ilist [ *k ] = jn [ i ];
      rlist [ *k ] = ( float ) dist;
      ++ ( *k );
   }

   /* sort in the ascending order of the distance */
   if ( *k > 1 )
   {
      sort_float_short ( *k, rlist, ilist );
   }

   free ( jn );
   jn = NULL;

   return;
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
static void correl1 ( float rho0, double range, double dist,
                      double * pCorr )
{
   /*******************************************************************
   c
   c subroutine specifies spatial correlation coefficient at separation
   c distance > 0 (km)
   c version Jun 08, 1999 by D.-J. Seo at NWS/HRL
   c
   c input variables
   c
   c rho0  - lag-0+ spatial correlation coefficient
   c range - spatial correlation scale (in km) in the exponential model
   c dist  - separation distance (in km)
   c
   c output variables
   c
   c corr  - correlation coefficient
   c
   *****************************************/
   * pCorr = rho0 * exp (-dist/range );
   return;
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
static void correl2 ( float rho0, double range, double dist, double * pCorr )
{
   /*c**********************************************************************
   c
   c subroutine specifies spatial correlation coefficient for separation
   c distance >= 0 (km)
   c version Jun 08, 1999 by D.-J. Seo at NWS/HRL
   c
   c
   c input variables
   c
   c rho0  - lag-0+ spatial correlation coefficient
   c range - spatial correlation scale (in km) in the exponential model
   c dist  - separation distance (in km)
   c
   c output variables
   c
   c corr  - correlation coefficient
   c */
   if ( dist == 0.0 )
   {
      * pCorr = 1.0;
   }
   else
   {
      *pCorr = rho0 * exp(-dist/range);
   }

   return;
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
static void err_cor_mat ( const gage_radar_pair_table_struct * pGageRadarPair,
                          const local_bias_params * pLocalBiasParams,
                          int nbors, int iu0, int iv0, double range,
                          short * ilist, double cormat [][NBMAX] )
{
/* The former documentation and variable definitions. */
/**********************************************************************
c
c subroutine constructs error correlation coefficient matrix
c version Jun 08, 2000 by D.-J. Seo at NWS/HRL
c
c input variables
c
c nbors  - number of neighbors
c iu0    - x-coordinate of the point of estimation
c iv0    - y-coordinate of the point of estimation
c ju     - 1-d array holding x-coordinates of gage locations
c jv     - 1-d array holding y-coordinates of gage locations
c ilist  - 1-d array holding index of nearest gages
c rho0   - lag-0+ spatial correlation coefficient
c range  - spatial correlation scale in km
c
c output variables
c
c cormat - 2-d array holding correlation coefficient matrix
*********************************************************/

   double * corvec = NULL;
   double dist;
   double disx;
   double disy;
   int i;
   int i1;
   int j;
   int j1;

   corvec = ( double * ) malloc ( NBMAX * sizeof ( double ));

   if ( corvec == NULL )
   {
      printMessage ( "Could not allocate arrays in err_cor_mat.\n", logFile );
      return;
   }

   if ( pLocalBiasParams->type_of_est == 2 )
   {
      for ( j = 0; j < nbors; ++j )
      {
         j1=ilist[j];
         disx = pGageRadarPair->ptrGageRadarPair[j1].hrap_x - iu0;
         disy = pGageRadarPair->ptrGageRadarPair[j1].hrap_y - iv0;
         dist = sqrt ( ( disx * disx ) + ( disy * disy ) );

         correl1 ( pLocalBiasParams->rho0, range, dist, & corvec[j] );
      }
   }

   for ( j = 0; j < nbors; ++j )
   {
      j1 = ilist [ j ];

      for ( i = 0; i <= j; ++i )
      {
         i1=ilist[i];
         disx = pGageRadarPair->ptrGageRadarPair[i1].hrap_x -
                pGageRadarPair->ptrGageRadarPair[j1].hrap_x;
         disy = pGageRadarPair->ptrGageRadarPair[i1].hrap_y -
                pGageRadarPair->ptrGageRadarPair[j1].hrap_y;
         dist=sqrt ( disx*disx + disy*disy );
         correl2 ( pLocalBiasParams->rho0, range, dist, & cormat[i][j] );
      }
   }

   if( pLocalBiasParams->type_of_est == 2 )
   {
      for ( j = 0; j < nbors; ++j )
      {
         for ( i = 0; i <= j; ++i )
         {
            cormat [ i ] [ j ]= 1.0 + cormat [ i ] [ j ] - corvec [ i ] -
                                corvec [ j ];
         }
      }
   }

   free ( corvec );
   corvec = NULL;
   return;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:    lsolve_array
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
static void lsolve_array ( int neq, double cov[][NBMAX], double * rhs,
                           int * iflag )
{
   /* Comments from former Fortran code. */
   /*c**********************************************************************
   c
   c subroutine solves linear system via gauss elimination with no
   c pivoting (deutsch and journel 1992)
   c
   c input
   c
   c neq    - dimension of the linear system
   c cov    - covariance matrix
   c rhs    - right-hand side
   c
   c output
   c
   c rhs    - vector of unknowns
   c */

   double * a = NULL;
   double * s = NULL;
   int i;
   int ising;
   int j;
   int knt;

   * iflag = 0;

   if ( neq == 1 )
   {
      rhs [ 0 ] /= cov [ 0 ] [ 0 ];
      return;
   }

   a = ( double * ) malloc (  NBMAX * (NBMAXP1 / 2) * sizeof ( double ) );
   s = ( double * ) malloc ( NBMAX * sizeof ( double ));

   if ( ( a == NULL ) || ( s == NULL ) )
   {
        printMessage ( "Could not allocate arrays in lsolve_array.\n", logFile );
        return;
   }

   /* copy columnwise the upper triangular matrix */
   knt = 0;

   for ( j = 0; j < neq; ++j )
   {
      for ( i = 0; i < j; ++i )
      {
         ++knt;
         a[knt]=cov[i][j];
      }
   }

   MPEFieldGen_ksol ( 1, neq, 1, a, rhs, s, & ising );

   if ( ising != 0 )
   {
      sprintf ( message, "error from ksol...impossibe event...stop: %d\n",
                         ising );
      printMessage ( message, logFile );
      *iflag = 1;
      free ( a );
      a = NULL;
      free ( s );
      s = NULL;
      return;
   }

   for ( i = 0; i < neq; ++i )
   {
      rhs [ i ] = s [ i ];
   }

   free ( a );
   a = NULL;
   free ( s );
   s = NULL;
   return;
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
static void setupLocalBiasParams( )
{
   /* The logical unit to read and write the state variables from and to. */
   localBiasParams.unit = unit;

   /* Save sv files ending at hours div by imh */
   localBiasParams.mult_hour = int_mult_hour;

   /* Number of hours to look back. */
   localBiasParams.nhr_look_back = nhr_look_back;

   /* itype_of_est=1 for areal estimation (default)
      itype_of_est=2 for bin-specific estimation */
   localBiasParams.type_of_est = type_of_est;

   /* Radius (in km) within which updating is performed. */
   localBiasParams.radius = radius;

   /* Maximum number of neighbors used in updating state variables
      (default for rho0=0 is 500:  if rho0 is positive, nborx must be
      set to a much smaller number in order to solve linear systems
      many times over in a reasonable amount of time). */
   localBiasParams.nborx = nborx;

   /* lag-0+ spatial correlation coefficient (default is 0). */
   localBiasParams.rho0 = rho0;

   /* Spatial correlation scale (in km) (if nho0=0, it does not matter what
      cor_range is set to) */
   localBiasParams.cor_range = cor_range;

   /* Cutoff Fisher information content
      number of positive pairs to choose "best" bias
      equivalent to npair_bias_select field in RWBiasStat table. */
   localBiasParams.sel_npr_lb_rad = sel_npr_lb_rad;
   localBiasParams.sel_npr_lb_sat = sel_npr_lb_sat;

   /*  Update state variables once every mult bins along either
       direction. */
   localBiasParams.mult = mult;

   /* The maximum number of neighbors to be used in filling holes
      (the default is 10). */
   localBiasParams.nbory = bory;

   localBiasParams.ndat = 0;

   /* Define the array of array indices used for local bias computations. */
   if ( mult > 1 )
   {
      MPEFieldGen_find_array_indices( & localBiasParams );
   }

   /*
     interp=1 to interpolate via spiral search
     interp=2 to interpolate via double heap-sorting
   */
   const int interp=2;
   localBiasParams.interp = interp;

   if(localBiasParams.interp == 2)
   {
        /* if interp=2, specify the maximum distance (in km) to the
           nearest neighbor within which interpolation is to be
           performed to fill the fringes. */
    localBiasParams.dist_cut = 40.0;
   }

   return;
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
const local_bias_params * MPEFieldGen_getLocalBiasParams ( )
{
   if ( first == 1 )
   {
      first = 0;
      setupLocalBiasParams ( );
   }

   return & localBiasParams;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:   update
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
static int update ( const local_bias_params * pLocalBiasParams,
                    const gage_radar_pair_table_struct * pGageRadarPair,
                    const mpe_params_struct * pMPEParams,
                    local_bias_values_record * pLocalBiasRecord,
                    int nbors,
                    int lag,
                    const double * wgt,
                    const short * ilist )
{
 /*************************************************************************
 c input variables
 cu
 c nspan - number of memory spans
 c span  - 1-d array of memory spans (in hours)
 c lag   - time elapsed since the last update (in hours)
 c si    - fisher information content
 c xg    - mean of positive gage rainfall (in mm)
 c xr    - mean of positive radar rainfall (in mm)
 c nbors - number of nearest neighbors
 c wgt   - 1-d array of weights
 c ilist - 1-d array of the array index for nearest neigbhors
 c zg    - 1-d array of gage data (in mm)
 c zr    - 1-d array of radar data (in mm)
 c
 c output variables
 c
 c iflag -  Error flag.
 c*/

 float si;
 float span;
 float xg;
 float xr;
 int i;
 int iflag = 0;
 int j;
 int k;

 for ( k = 0; k < pMPEParams->ptrRWBiasStat->num_span; ++k )
   {
      si = pLocalBiasRecord->local_bias_val [ k ].si ;
      xg = pLocalBiasRecord->local_bias_val [ k ].xg ;
      xr = pLocalBiasRecord->local_bias_val [ k ].xr ;

      span = pMPEParams->memory_spans [ k ];

      si *= exp ( -(1.0/span ) * lag );
      xg *= si;
      xr *= si;

      if ( nbors > 0 )
      {
         for ( i = 0; i < nbors; ++i )
         {
            j = ilist [ i ];

            if ( pLocalBiasParams->type_of_est == 1 )
            {
               si += wgt [ i ];
            }
            else
            {
               si += wgt [ i ] * ( nbors + 1 );
            }

            xg += wgt [ i ] * pGageRadarPair->ptrGageRadarPair [ j ].gageValue;
            xr += wgt [ i ] * pGageRadarPair->ptrGageRadarPair [ j ].radarValue;
         }
      }

      if ( si < 0.0 )
      {
         sprintf ( message, "si[k] < 0...impossible event...stop "
                            "k: %d    si: %5.2f\n", k, si );
         printMessage ( message, logFile );
         iflag = 1;
         return iflag;
      }

      if ( xg < 0.0 )
      {
         sprintf ( message, "xg[k] < 0...set to zero "
                            "k: %d    xg: %5.2f\n", k, xg );
         printMessage ( message, logFile );
         xg = 0.0;
      }

      if ( xr < 0.0 )
      {
         sprintf ( message, "xr[k] < 0...set to zero "
                            "k: %d   xr: %5.2f\n", k, xr);
         xr = 0.0;
      }

      if ( si == 0.0 )
      {
         xg = 0.0;
         xr = 0.0;
      }
      else
      {
         xg /= si ;
         xr /= si ;
      }

      pLocalBiasRecord->local_bias_val [ k ].si = si;
      pLocalBiasRecord->local_bias_val [ k ].xg = xg;
      pLocalBiasRecord->local_bias_val [ k ].xr = xr;
   }

   return iflag;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:     This subroutine finds n nearest neighbors via spiral search.
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
* HISTORY:
*    NAME            DATE             DESCRIPTION
*    D.J. Seo        June 8, 2000     Original coding
*    Bryon Lawrnece  May 11, 2005     Ported to C.
********************************************************************************
*/
static void srch_nbrs1 ( const geo_data_struct * pGeoData,
                         const local_bias_params * pLocalBiasParams,
                         int ix, int iy, short ** locspan,
                         double ** locbias, short * iu, short * iv, short * iz,
                         float * rz, int * ndat )
{
/* Original Information Block */
/***********************************************************************
      subroutine srch_nbrs1(nx,ny,n,idis,jdis,ix,iy,igag,gag,nbory,iu,
     *                      iv,iz,z,ndat)
c**********************************************************************
c
c subroutines finds n nearest neighbors via spiral search
c version Jun 08, 2000 by D.-J. Seo  at NWS/HRL
c
c input
c
c n     - number of valid data points within the ellipsoid of
c         influence.  Found via a call to find_array_indices.
c idis  - x-coordinates of the above data points
c jdis  - y-coordinates of the above data points
c ix    - x-coordinate of the point of estimation
c iy    - y-cooridnate of the point of estimation
c nbory - number of nearest neighbors to be located
c igag  - field of interest
c
c output
c
c iu    - x-coordinates of the neighbors
c iv    - y-coordinates of the neighbors
c iz    - values of the neighbors
c ndat  -
c
      integer*2 igag(nx,ny)
      integer*2 iu(mxgags),iv(mxgags),iz(mxgags),
     *          idis(nind),jdis(nind)
      dimension gag(nx,ny),z(mxgags)
*/

   int i;
   int j;
   int k;

   * ndat = 0;

   for ( k = 0; k < pLocalBiasParams->ndat; ++k )
   {
      i = pLocalBiasParams->idis [ k ] + ix;
      if ( i < 0 ) continue;
      if ( i >= pGeoData->num_cols ) continue;
      j = pLocalBiasParams->jdis [ k ] + iy;
      if ( j < 0 ) continue;
      if ( j >= pGeoData->num_rows ) continue;
      if ( locspan [j][i] < 0 ) continue;

      iu[*ndat] = i;
      iv[*ndat] = j;
      iz[*ndat] = locspan[j][i];
      rz[*ndat] = locbias[j][i];

      (*ndat)++;

      if ( *ndat == pLocalBiasParams->nbory ) return;
   }

   /* the total number of valid data points is less than nbory */
   return ;
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

static void interpolate_nearby ( const geo_data_struct * pGeoData,
                                 const local_bias_params * pLocalBiasParams,
                                 short int ** locspan,
                                 double ** biasloc )
/**********************************************************************
      subroutine interpolate_nearby(nx,ny,loc_span,bias_loc,nbory,n,
     *                              idis,jdis)
c**********************************************************************
c
c subroutine performs spatial interpolation of bin-by-bin bias to fill
c holes
c version Jun 08, 2000 by D.-J. Seo at NWS/HRL
c
c input variables
c
c iarray - integer array with holes
c array  -
c nbory  - number of nearest neighbors to be used in interpolation
c n      - number of entries the spiral search domain
c idis   - HRAP x-coordinates of nearest neighbors in the spiral search
c          domain
c jdis   - HRAP y-coordinates of nearest neighbors in the spiral search
c          domain
c
c output variable
c
c iarray - integer array with holes filled
c array  -
c
c**********************************************************************/

{
   double aver;
   double avei;
   double dist2 = 0.0;
   double sum;
   double w;
   float ** locbias_temp = NULL;
   int i;
   int iu0;
   int iv0;
   int j;
   int k;
   int ndat;
   short ** locspan_temp = NULL;
   short * iu = NULL;
   short * iv = NULL;
   short * iz = NULL;
   float * rz = NULL;

   locbias_temp = ( float ** ) malloc ( sizeof ( float * ) * pGeoData->num_rows );

   for ( i = 0 ; i < pGeoData->num_rows; ++ i )
   {
       locbias_temp[i] = ( float * ) malloc ( sizeof (float) * pGeoData->num_cols );
   }

   locspan_temp = ( short ** ) malloc ( sizeof ( short * ) * pGeoData->num_rows );

   for ( i = 0 ; i < pGeoData->num_rows; ++ i )
   {
       locspan_temp[i] = ( short * ) malloc ( sizeof (short) * pGeoData->num_cols );
   }

   iu = ( short * ) malloc ( sizeof(short) * MAXGAGS );
   iv = ( short * ) malloc ( sizeof(short) * MAXGAGS );
   iz = ( short * ) malloc ( sizeof(short) * MAXGAGS );
   rz = ( float * ) malloc ( sizeof(float) * MAXGAGS );

   sprintf ( message, "into interpolate_nearby nx=%d, ny=%d, nbory=%d, "
                      "ndat=%d\n", pGeoData->num_cols, pGeoData->num_rows,
                      pLocalBiasParams->nbory, ndat );
   printMessage ( message, logFile );

   for ( iu0 = 0; iu0 < pGeoData->num_cols; ++iu0 )
   {
      for ( iv0 = 0; iv0 < pGeoData->num_rows; ++iv0 )
      {

         if ( locspan [ iv0 ] [iu0] >= 0 )
         {
            continue;
         }

         srch_nbrs1 ( pGeoData, pLocalBiasParams, iu0, iv0,
                      locspan, biasloc, iu, iv, iz, rz, &ndat);

         if ( ndat == 0 )
         {
            locspan_temp [ iv0 ] [ iu0 ] = -1;
            locbias_temp [ iv0 ] [ iu0 ] = -9.0;
            continue;
         }

         /* perform spatial interpolation via the reciprocal distance squared
            method */
         sum = 0.0;

         for ( k = 0; k < ndat; ++k )
         {
            dist2 = ( iu [ k ] - iu0 ) * ( iu [ k ] - iu0 )
                  + ( iv [ k ] - iv0 ) * ( iv [ k ] - iv0 );

            if ( dist2 == 0.0 )
            {
               break;
            }

            sum = sum + 1.0 / dist2;
         }

         if ( dist2 == 0.0 ) break ;

         aver=0.0;
         avei=0.0;

         for ( k = 0; k < ndat; ++k )
         {
            dist2 = ( iu [ k ] - iu0 ) * ( iu [ k ] - iu0 )
                  + ( iv [ k ] - iv0 ) * ( iv [ k ] - iv0 );
            w = 1.0 / dist2;
            w = w / sum;
            aver += ( w * rz [ k ] );
            avei += ( w * iz [ k ] );
         }

         locbias_temp [iv0] [iu0] = aver ;
         locspan_temp [iv0] [iu0] = avei ;

         /* round-off */
         if ( ( avei - locspan_temp [ iv0 ] [ iu0 ] ) >= 0.5 )
         {
            locspan_temp [ iv0 ] [ iu0 ] = locspan_temp [ iv0 ] [ iu0 ] + 1;
         }

         if ( ( locspan_temp [ iv0 ] [ iu0 ] == 0 ) && ( avei > 0.0 ) )
         {
            locspan_temp [ iv0 ] [ iu0] = 1;
         }
      }
   }

   /* fill in the holes */
   for ( j = 0; j < pGeoData->num_cols; ++j )
   {
      for ( i = 0; i < pGeoData->num_rows; ++i )
      {
         if ( locspan[j][i] < 0 )
         {
            locspan[j][i] = locspan_temp[j][i];
            biasloc[j][i] = locbias_temp[j][i];
         }
      }
   }

   sprintf ( message , "leaving interpolate_nearby\n" );
   printMessage ( message, logFile );

   free ( iu );
   iu = NULL;
   free ( iz );
   iz = NULL;
   free ( iv );
   iv = NULL;
   free ( rz );
   rz = NULL;

   for ( i = 0 ; i < pGeoData->num_rows; ++ i )
   {
      free ( locbias_temp[i] );
      free ( locspan_temp[i] );
   }

   free ( locbias_temp );
   locbias_temp = NULL;
   free ( locspan_temp );
   locspan_temp = NULL;

   return;
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
void MPEFieldGen_find_nbrs1 ( int n, short * iu, short * iv, int iu0, int iv0,
                  int iradi, int * k, short * ilist,
                  float * rlist, int * iu0_prev, int * m_save,
                  short * ivv_save, short * in_save )
{

   /**********************************************************************
   c
   c subroutine locates all data points within the radius of influence via
   c double heap-sorting
   c
   c version Jun 08, 2000 by D.-J. Seo at NWS/HRL
   c
   c input variables
   c
   c n     - number of data points
   c iu    - array of HRAP x-coordinates
   c iv    - array of HRAP y-coordinates
   c iu0   - HRAP x-coordinate at the point of estimation
   c iv0   - HRAP y-coordinate at the point of estimation
   c iradi - radius of influence in HRAP bins
   c
   c output variables
   c
   c k     - number of neighbors located
   c ilist - array of the indices of the neighbors
   c rlist - array of the distances to the neighbors
   c
      parameter (nx1=2500,nbmax=400)
      integer*2 iu(n),iv(n),ivv(nx1),in(nx1),jn(nbmax),ilist(nbmax)
      dimension rlist(nbmax)
******************************************************************************/
   double dist;
   double xdis;
   double ydis;
   int i;
   int i0;
   int it;
   int j = 0;
   int m = 0;
   short * in= NULL;
   short * jn = NULL;

   * k = 0;

   in = (short * ) malloc ( NX1 * sizeof ( short ));
   jn = (short * ) malloc ( NBMAX * sizeof ( short ));

   if ( ( in == NULL ) || ( jn == NULL ) )
   {
       printMessage ( "Could not allocate arrays in find_nbrs1.\n", logFile );
       return;
   }

   if ( iu0 != * iu0_prev )
   {
      /* iu, iv, and z arrays must be sorted in the ascending order of iu
         before calling this subroutine */

      for ( i = 0; i < n; ++i )
      {
         if ( iu0 <= iu[i] ) break;
      }

      i0 = i - 1;

      if ( i0 == -1 )
      {
         /* iu0 is the smallest */
         for ( i = 0; i < n; ++i )
         {
            if ( ( iu[i] - iu0 ) > iradi ) break;

            if ( m >= NX1 )
            {
               sprintf ( message, "m gt nx1...stop and reset nx1 to a larger "
                                  "value\n" );
               printMessage ( message, logFile );
               free ( jn );
               jn = NULL;
               free ( in );
               in = NULL;
               return;
            }

            ivv_save[m] = iv[i];
            in[m] = i;
            ++m;
          }
       }
       else if ( i0 == n - 1 )
       {
          /* u0 is the largest */
          for ( i = 0; i < n; ++i)
          {
             it=n-i-1;
             if ( ( iu0 -iu[it] ) > iradi ) break;

             if( m >= NX1 )
             {
                sprintf ( message, "m gt nx1...stop and reset nx1 to a larger "
                                   "value\n" );
                printMessage ( message, logFile );
                free ( jn );
                jn = NULL;
                free ( in );
                in = NULL;
                return;
             }

             ivv_save[m] = iv[it];
             in[m] = it;
             ++m;
           }
        }
        else
        {
           /* u0 is somewhere in between */
           for ( i = i0 + 1; i < n; ++i )
           {
              if( ( iu[i] - iu0 ) > iradi) break;

              if ( m >= NX1 )
              {
                 sprintf ( message, "m gt. nx1...stop and reset nx1 to a "
                                    "larger value\n" );
                 printMessage ( message, logFile );

                 free ( jn );
                 jn = NULL;
                 free ( in );
                 in = NULL;
                 return;
              }

              ivv_save[m]=iv[i];
              in[m]=i;
              ++m;
           }

           for ( i = 0; i <= i0; ++i )
           {
              it=i0-i;

              if ( ( iu0 - iu[it] ) > iradi) break;

              if ( m >= NX1 )
              {
                 sprintf ( message, "m gt nx1...stop and reset nx1 to a larger "
                                    "value" );
                 printMessage ( message, logFile );
                 free ( jn );
                 jn = NULL;
                 free ( in );
                 in = NULL;
                 return;
              }

              ivv_save[m] = iv[it];
              in[m] = it;
              ++m;
           }
        }

        if ( m > 1)
        {
           sort_short_short(m,ivv_save,in);
        }

       * iu0_prev = iu0;
       * m_save = m;

     } /* Closing brace for the iu0 and the iu0_prev comparison. */

     for ( i = 0; i < * m_save; ++i )
     {
        if ( iv0 <= ivv_save[i] ) break;
     }

     i0 = i - 1;

     if ( i0 == -1 )
     {
        /* iv0 is the smallest */
        for ( i = 0; i < * m_save; ++i )
        {
           if ( ( ivv_save[i] - iv0 ) > iradi ) break;

           if ( j >= NBMAX )
           {
              sprintf ( message, "j gt nbmax...stop & reset nbmax to a "
                                 "larger value\n");
              printMessage ( message, logFile );
              free ( jn );
              jn = NULL;
              free ( in );
              in = NULL;
              return;
           }

           jn[j] = in_save[i];
           ++j;
         }
      }
      else if ( i0 == * m_save - 1 )
      {
         /* iv0 is the largest */
         for ( i = 0; i < * m_save; ++i )
         {
            it = * m_save - i - 1;
            if ( ( iv0 -ivv_save[it] ) > iradi ) break;

            if ( j >= NBMAX )
            {
               sprintf ( message, "j gt nbmax...stop & reset nbmax to a"
                                  "larger value\n" );
               printMessage ( message, logFile );
               free ( jn );
               jn = NULL;
               free ( in );
               in = NULL;
               return;
            }

            jn[j] = in_save[it];
            ++j;
         }
      }
      else
      {
         /* iv0 is somewhere in between */
         for ( i = i0 + 1; i < * m_save; ++i )
         {
            if ( ( ivv_save[i] - iv0 )  > iradi ) break;

            if ( j >= NBMAX )
            {
               sprintf ( message, "j gt nbmax...stop & reset nbmax to a "
                                  "larger value\n" );
               printMessage ( message, logFile );
               free ( jn );
               jn = NULL;
               free ( in );
               in = NULL;
               return;
            }

            jn[j]=in[i];
            ++j;

         }

         for ( i = 0; i <= i0; ++i )
         {
            it = i0 - i;

            if ( ( iv0 - ivv_save[it] ) > iradi ) break;

            if ( j >= NBMAX )
            {
               sprintf ( message, "j gt nbmax...stop & reset nbmax to a "
                                  "larger value\n" );
               printMessage ( message, logFile );
               free ( jn );
               jn = NULL;
               free ( in );
               in = NULL;
               return;
            }

            jn[j]=in_save[it];
            ++j;
         }
      }

      /* calculate distance */
      /* Initialize the number of neighbors found to 0. */
      * k = 0;
      for ( i = 0; i < j; ++i )
      {
         xdis = iu[jn[i]] - iu0;
         ydis = iv[jn[i]] - iv0;
         dist = (double)( xdis * xdis + ydis * ydis );
         dist=sqrt(dist);

         if ( dist > ( double ) iradi ) continue;
         ilist[ *k ] = jn[i];
         rlist[ *k ] = ( float ) dist;
         ++ ( *k );
      }

      /* sort in the ascending order of the distance */
      if ( *k > 1)
      {
          sort_float_short (*k,rlist,ilist);
      }

      free ( jn );
      jn = NULL;
      free ( in );
      in = NULL;
      return;
}

static void find_nbrs1orig ( int n, short * iu, short * iv, int iu0, int iv0,
                             int iradi, int * k, short * ilist,
                             float * rlist )
{

   /**********************************************************************
   c
   c subroutine locates all data points within the radius of influence via
   c double heap-sorting
   c
   c version Jun 08, 2000 by D.-J. Seo at NWS/HRL
   c
   c input variables
   c
   c n     - number of data points
   c iu    - array of HRAP x-coordinates
   c iv    - array of HRAP y-coordinates
   c iu0   - HRAP x-coordinate at the point of estimation
   c iv0   - HRAP y-coordinate at the point of estimation
   c iradi - radius of influence in HRAP bins
   c
   c output variables
   c
   c k     - number of neighbors located
   c ilist - array of the indices of the neighbors
   c rlist - array of the distances to the neighbors
   c
      parameter (nx1=2500,nbmax=400)
      integer*2 iu(n),iv(n),ivv(nx1),in(nx1),jn(nbmax),ilist(nbmax)
      dimension rlist(nbmax)
******************************************************************************/
   double dist;
   double xdis;
   double ydis;
   int i;
   int i0;
   int it;
   int j = 0;
   int m = 0;
   short * in = NULL ;
   short * jn = NULL;
   short * ivv = NULL;

   in = (short *) malloc ( NX1 * sizeof ( short ));
   jn = (short *) malloc ( NBMAX * sizeof ( short ));
   ivv = (short *) malloc ( NX1 * sizeof ( short ));

   if ( ( in == NULL ) || ( jn == NULL ) || ( ivv == NULL ) )
   {
        printMessage ( "Could not allocate arrays in find_nbrs1orig.\n", logFile );
        return;
    }

   * k = 0;

   /* iu, iv, and z arrays must be sorted in the ascending order of iu
      before calling this subroutine */

   for ( i = 0; i < n; ++i )
   {
      if ( iu0 <= iu[i] ) break;
   }

   i0 = i - 1;

   if ( i0 == -1 )
   {
      /* iu0 is the smallest */
      for ( i = 0; i < n; ++i )
      {
         if ( ( iu[i] - iu0 ) > iradi ) break;

         if ( m >= NX1 )
         {
            sprintf ( message, "m gt nx1...stop and reset nx1 to a larger "
                               "value\n" );
            printMessage ( message, logFile );
            free ( in );
            in = NULL;
            free ( jn );
            jn = NULL;
            free ( ivv );
            ivv = NULL;
            return;
         }

         ivv[m] = iv[i];
         in[m] = i;
         ++m;
       }
   }
   else if ( i0 == n - 1 )
   {
      /* u0 is the largest */
      for ( i = 0; i < n; ++i)
      {
         it=n-i-1;
         if ( ( iu0 -iu[it] ) > iradi ) break;

         if( m >= NX1 )
         {
            sprintf ( message, "m gt nx1...stop and reset nx1 to a larger "
                               "value\n" );
            printMessage ( message, logFile );
            free ( in );
            in = NULL;
            free ( jn );
            jn = NULL;
            free ( ivv );
            ivv = NULL;
            return;
         }

         ivv[m] = iv[it];
         in[m] = it;
         ++m;
       }
    }
    else
    {
       /* u0 is somewhere in between */
       for ( i = i0 + 1; i < n; ++i )
       {
          if( ( iu[i] - iu0 ) > iradi) break;

          if ( m >= NX1 )
          {
             sprintf ( message, "m gt. nx1...stop and reset nx1 to a "
                                "larger value\n" );
             printMessage ( message, logFile );
             free ( in );
             in = NULL;
             free ( jn );
             jn = NULL;
             free ( ivv );
             ivv = NULL;
             return;
          }

          ivv[m]=iv[i];
          in[m]=i;
          ++m;
       }

       for ( i = 0; i <= i0; ++i )
       {
          it=i0-i;

          if ( ( iu0 - iu[it] ) > iradi) break;

          if ( m >= NX1 )
          {
             sprintf ( message, "m gt nx1...stop and reset nx1 to a larger "
                                 "value" );
             printMessage ( message, logFile );
             free ( in );
             in = NULL;
             free ( jn );
             jn = NULL;
             free ( ivv );
             ivv = NULL;
             return;
           }

          ivv[m] = iv[it];
          in[m] = it;
          ++m;
       }
    }

    if ( m > 1)
    {
       sort_short_short(m,ivv,in);
    }

    for ( i = 0; i < m; ++i )
    {
       if ( iv0 <= ivv[i] ) break;
    }

    i0 = i - 1;

    if ( i0 == -1 )
    {
       /* iv0 is the smallest */
       for ( i = 0; i < m; ++i )
       {
          if ( ( ivv[i] - iv0 ) > iradi ) break;

          if ( j >= NBMAX )
          {
             sprintf ( message, "j gt nbmax...stop & reset nbmax to a "
                                 "larger value\n");
             printMessage ( message, logFile );
             free ( in );
             in = NULL;
             free ( jn );
             jn = NULL;
             free ( ivv );
             ivv = NULL;
             return;
          }

          jn[j] = in[i];
          ++j;
        }
    }
    else if ( i0 == m - 1 )
    {
       /* iv0 is the largest */
       for ( i = 0; i < m; ++i )
       {
          it = m - i - 1;
          if ( ( iv0 -ivv[it] ) > iradi ) break;

          if ( j >= NBMAX )
          {
             sprintf ( message, "j gt nbmax...stop & reset nbmax to a"
                                "larger value\n" );
             printMessage ( message, logFile );
             free ( in );
             in = NULL;
             free ( jn );
             jn = NULL;
             free ( ivv );
             ivv = NULL;
             return;
          }

          jn[j] = in[it];
          ++j;
       }
    }
    else
    {
       /* iv0 is somewhere in between */
       for ( i = i0 + 1; i < m; ++i )
       {
          if ( ( ivv[i] - iv0 )  > iradi ) break;

          if ( j >= NBMAX )
          {
             sprintf ( message, "j gt nbmax...stop & reset nbmax to a "
                                "larger value\n" );
             printMessage ( message, logFile );
             free ( in );
             in = NULL;
             free ( jn );
             jn = NULL;
             free ( ivv );
             ivv = NULL;
             return;
          }

          jn[j]=in[i];
          ++j;

       }

       for ( i = 0; i <= i0; ++i )
       {
          it = i0 - i;

          if ( ( iv0 - ivv[it] ) > iradi ) break;

          if ( j >= NBMAX )
          {
             sprintf ( message, "j gt nbmax...stop & reset nbmax to a "
                                "larger value\n" );
             printMessage ( message, logFile );
    free ( in );
    in = NULL;
    free ( jn );
    jn = NULL;
    free ( ivv );
    ivv = NULL;
             return;
          }

          jn[j]=in[it];
          ++j;
       }
    }

    /* calculate distance */
    /* Initialize the number of neighbors found to 0. */
    * k = 0;
    for ( i = 0; i < j; ++i )
    {
       xdis = iu[jn[i]] - iu0;
       ydis = iv[jn[i]] - iv0;
       dist = (double)( xdis * xdis + ydis * ydis );
       dist=sqrt(dist);

       if ( dist > ( double ) iradi ) continue;
       ilist[ *k ] = jn[i];
       rlist[ *k ] = ( float ) dist;
       ++ ( *k );
    }

    /* sort in the ascending order of the distance */
    if ( *k > 1)
    {
        sort_float_short (*k,rlist,ilist);
    }

    free ( in );
    in = NULL;
    free ( jn );
    jn = NULL;
    free ( ivv );
    ivv = NULL;

    return;
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

# define FREE_INTERPOLATE_EVERYWHERE_MEMORY()     \
   free ( rlist );                                \
   rlist = NULL;                                  \
   free ( rz );                                   \
   rz = NULL;                                     \
   free ( ilist );                                \
   ilist = NULL;                                  \
   free ( iu );                                   \
   iu = NULL;                                     \
   free ( iv );                                   \
   iv = NULL;                                     \
   free ( iz );                                   \
   iz = NULL ;                                    \
                                                  \
   for ( i = 0; i < pGeoData->num_rows; ++i )     \
   {                                              \
      free ( bias_loc_temp[i] );                  \
      free ( loc_span_temp[i] );                  \
   }                                              \
                                                  \
   free ( bias_loc_temp );                        \
   bias_loc_temp = NULL;                          \
   free ( loc_span_temp );                        \
   loc_span_temp = NULL;                          \


static void interpolate_everywhere ( const geo_data_struct * pGeoData,
                                     const local_bias_params * pLocalBiasParams,
                                     short ** locspan,
                                     double ** biasloc,
                                     float dist )
{

   double aver;
   double avei;
   double dist2;
   double sum;
   double w;
   float **  bias_loc_temp = NULL;
   float * rlist = NULL;
   float * rz = NULL;
   int iradi;
   int iu0;
   int iv0;
   int i;
   int j;
   int k;
   int nbor2;
   int nbor3;
   int npair;
   short * ilist = NULL;
   short * iu = NULL;
   short * iv = NULL;
   short * iz = NULL;
   short ** loc_span_temp = NULL;
   int ndata;

   /* Allocate memory for the large arrays. */
   rlist = (float *) malloc ( NDATX * sizeof ( float ) );
   rz = (float *) malloc ( NDATX * sizeof ( float ) );
   ilist = ( short * ) malloc ( NBMAX * sizeof ( short ) );
   iu = ( short * ) malloc ( NDATX * sizeof ( short ) );
   iv = ( short * ) malloc ( NDATX * sizeof ( short ) );
   iz = ( short * ) malloc ( NDATX * sizeof ( short ) );

   if ( ( rlist == NULL ) || ( rz == NULL ) || ( ilist == NULL ) ||
        ( iu == NULL ) || ( iv == NULL ) || ( iz == NULL ) )
   {
      printMessage ( "Could not allocate memory for interpolate_everywhere arrays.\n", logFile );
      return;
   }

   bias_loc_temp = ( float ** ) malloc ( pGeoData->num_rows * sizeof ( float * ) );
   loc_span_temp = ( short ** ) malloc ( pGeoData->num_rows * sizeof ( short * ) );

   if ( ( bias_loc_temp == NULL ) || ( loc_span_temp == NULL ) )
   {
      printMessage ( "Could not allocate memory for interpolate_everywhere arrays.\n", logFile );
      return;
   }

   for ( i = 0; i <  pGeoData->num_rows; ++i )
   {
      bias_loc_temp [ i ] = ( float * ) malloc ( pGeoData->num_cols * sizeof ( float ) );
      loc_span_temp [ i ] = ( short * ) malloc ( pGeoData->num_cols * sizeof ( short ) );

      if ( ( bias_loc_temp [ i ] == NULL ) || ( loc_span_temp [ i ] == NULL ) )
      {
         printMessage ( "Could not allocate memory for interpolate_everywhere arrays.\n", logFile );
         return;
      }
   }

   /* round-off the search radius (units = HRAP bins) */
   iradi=dist;

   if ( dist - iradi > 0.5) iradi++;

   ndata = 0;

   for ( iu0 = 0; iu0 < pGeoData->num_cols; ++iu0 )
   {
      for ( iv0 = 0; iv0 < pGeoData->num_rows; ++iv0 )
      {
         if ( locspan [iv0][iu0] >= 0 )
         {

            if ( ndata >= NDATX )
            {
               sprintf ( message, "ndata ge ndatx ... stop\n" );
               printMessage ( message, logFile );
               FREE_INTERPOLATE_EVERYWHERE_MEMORY();
               return;
            }

            iu[ndata]=iu0;
            iv[ndata]=iv0;
            rz[ndata]=biasloc [iv0][iu0];
            iz[ndata]=locspan [iv0][iu0];

            ndata++;
         }
      }
   }

   if ( ndata == 0)
   {
      sprintf ( message, "ndata eq 0...nothing to interpolate on...return\n" );
      printMessage ( message, logFile );
      FREE_INTERPOLATE_EVERYWHERE_MEMORY();
      return;
   }

   if ( ndata < pLocalBiasParams->nbory )
   {
      nbor2 = ndata;
   }
   else
   {
      nbor2 = pLocalBiasParams->nbory;
   }

   for ( iu0 = 0; iu0 < pGeoData->num_cols; ++iu0 )
   {
      for ( iv0 = 0; iv0 < pGeoData->num_rows; ++iv0 )
      {
         loc_span_temp [iv0][iu0] = -1;
         bias_loc_temp [iv0][iu0] = -9.0;

         if ( locspan[iv0][iu0] == -1 )
         {
/*            find_nbrs1 ( ndata, iu, iv, iu0, iv0, iradi, &npair, ilist,
                         rlist, & iu0_prev, & m_save, ivv_save,
                         in_save );  */
              find_nbrs1orig ( ndata, iu, iv, iu0, iv0, iradi, &npair, ilist,
                               rlist );


            /* if there is not a single data point within the search radius,
               do not interpolate */
            if ( npair == 0 ) continue;

            /* if the number of data points within the search radius is less
               than the number of neighbors to be located, set the latter to
               the former */
            if ( npair < nbor2 )
            {
               nbor3=npair;
            }
            else
            {
               nbor3=nbor2;
            }

            /* perform spatial interpolation via the reciprocal distance
               squared method */
            sum = 0.0;

            for ( k = 0; k < nbor3; ++k )
            {
               dist2 = rlist[k] * rlist[k] ;

               if ( dist2 == 0.0 )
               {
                  sprintf ( message, "dist2 eq 0...impossible event\n" );
                  printMessage ( message, logFile );
                  FREE_INTERPOLATE_EVERYWHERE_MEMORY();
                  return;
               }

               sum += ( 1.0 / dist2 );
            }

            aver = 0.0;
            avei = 0.0;

            for ( k = 0; k < nbor3; ++k )
            {
               j = ilist [ k ];
               dist2 = rlist [ k ] * rlist [ k ];
               w = 1.0/dist2;
               w /= sum;
               aver += ( w * rz[j] );
               avei += ( w * iz[j] );
            }

            bias_loc_temp[iv0][iu0] = aver;
            loc_span_temp[iv0][iu0] = avei;

            /* round-off */
            if ( avei - loc_span_temp[iv0][iu0] >= 0.5 )
            {
               ++loc_span_temp[iv0][iu0];
            }

            if ( ( loc_span_temp[iv0][iu0] == 0 ) && ( avei > 0.0 ) )
            {
               loc_span_temp [iv0][iu0] = 0;
            }
         }
      }
   }

   /* fill the holes */
   for ( i = 0; i < pGeoData->num_cols; ++i )
   {
      for ( j = 0; j < pGeoData->num_rows; ++j)
      {
         if(locspan [j][i] < 0)
         {
            locspan [j][i] = loc_span_temp [j][i];
            biasloc [j][i] = bias_loc_temp [j][i];
         }
      }
   }

   /* Free allocated memory. */
   FREE_INTERPOLATE_EVERYWHERE_MEMORY();
   return ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:   local_bias
* PURPOSE:       Subroutine performs estimation of local bias via exponential
*                smoothing (schweppe 1973)
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
void MPEFieldGen_local_bias ( const run_date_struct * pRunDate,
                  const geo_data_struct * pGeoData,
                  const int gageSize,
                  short * iug ,
                  short * ivg ,
                  float * zg ,
                  mpe_params_struct * pMPEParams,
                  const local_bias_params * pLocalBiasParams,
                  float si_cut,
                  gage_radar_pair_table_struct * pGageRadarPair,
                  double ** RMosaic,
                  short int ** local_span,
                  double ** local_bias,
                  const char * dirname,
                  double ** lmosaic,
                  int * ierr )
{
   char copy_file [ PATH_LEN ];
   char * file_error_string = NULL;
   char filename1 [ PATH_LEN ];
   char filename2 [ PATH_LEN ];
   char filename3 [ PATH_LEN ];
   char ctime_b [ YYYYMMDDHH_LEN + 1 ];
   char year_to_hour_date [ YYYYMMDDHH_LEN + 1 ];
   double col;
   double cormat [ NBMAX ] [ NBMAX ];
   double dist;
   double latitude;
   double longitude;
   double radius;
   double range;
   double rmesh;
   double row;
   double wgt [ NBMAX ];
   FILE * pStateFile = NULL;
   float fcols;
   float frows;
   float rlist [ NBMAX ];
   int already_updated = 0;
   int exists;
   int i;
   int iflag;
   int iradi;
   int iseq;
   int itime_b = 0;
   int itime_c = 0;
   int itime_p = 0;
   int iu0;
   int iv0;
   int j;
   int knt;
   int lag;
   int mod_cols;
   int mod_rows;
   int nbors;
   int npair;
   int status;

   local_bias_values_record local_bias_record;
   local_bias_values_record * local_bias_array = NULL;
   short ilist [ NBMAX ];
   size_t item_count;
   static const size_t state_rec_len = sizeof ( local_bias_values_record );
   struct stat file_stat;
   struct tm * pRunTime = NULL;
   struct tm tm;
   time_t look_back_time_t;

   /* Begin routine. */
   sprintf ( message, "   num positive pairs=%d  imult=%d  "
                      "interp=%d\n", pGageRadarPair->pairNum,
                                     pLocalBiasParams->mult,
                                     pLocalBiasParams->interp );
   printMessage ( message, logFile );

   /* Compute the size of the grid (in km) at the center of the
      estimation domain */
   nbors = pLocalBiasParams->nborx;
   col = pGeoData->hrap_x;
   row = pGeoData->hrap_y + ( pGeoData->num_rows / 2 );
   HrapToLatLongByReference ( row, col, & latitude, & longitude );

   hrapsize ( latitude , & rmesh );

   /* Calculuate the correlation scale and the radius of the estimation
      domain in hrap bins. */
   range = pLocalBiasParams->cor_range / rmesh;
   radius = pLocalBiasParams->radius / rmesh;

   /* round-off */
   iradi = (int) radius;

   if ( ( radius - (float) iradi ) >= 0.5 )
   {
      ++iradi;
   }

   dist = pLocalBiasParams->dist_cut / rmesh;

   /* initialize the error flag */
   *ierr = 0;

   /*  construct the generic state variable file name */
   memset ( filename1, '\0', PATH_LEN );
   sprintf ( filename1, "%s/state_variables", dirname );

   /* construct the state variable file name for the current hour */
   look_back_time_t = pRunDate->tRunTime;
   pRunTime = gmtime ( & look_back_time_t );
   memset ( year_to_hour_date, '\0', YYYYMMDDHH_LEN + 1 );
   strftime ( year_to_hour_date, YYYYMMDDHH_LEN + 1, "%Y%m%d%H", pRunTime );
   sprintf ( filename2, "%s/%sz", dirname, year_to_hour_date );

   /* Determine the number of data points used in the computation of
      state variables. */
   fcols = (float) pGeoData->num_cols / ( float) mult;
   frows = (float) pGeoData->num_rows / (float) mult;

   mod_cols = pGeoData->num_cols % mult;
   mod_rows = pGeoData->num_rows % mult;

   if ( mod_cols != 0 )
   {
      fcols += 1.0 ;
   }

   if ( mod_rows != 0 )
   {
      frows += 1.0 ;
   }

   iseq = ( int ) fcols * ( int ) frows ;

   for ( i = 1; i <= pLocalBiasParams->nhr_look_back; ++i )
   {
      /* go back one hour */
      look_back_time_t -= SECONDS_PER_HOUR;

      /* construct the 10-digit time stamp for the look back hour */
      itime_b = get_lltime ( look_back_time_t );

      if ( i == 1 )
      {
         /* for the immediately preceding hour, check if the generic state
            variable file exists */
         exists = stat ( filename1, & file_stat );

         if ( exists == 0 )
         {

            pStateFile = fopen ( filename1, "r" );

            if ( pStateFile == NULL )
            {
               sprintf ( message, "\n  error opening file = %s.\n"
                                  "         -- local bias not calculated",
                                  filename1 );
               printMessage ( message, logFile );
               * ierr = 1;
               return ;
            }

            /* Read the first record from the file. */
            errno = 0;
            item_count = fread ( & local_bias_record, state_rec_len, 1,
                                 pStateFile );

            if ( item_count != 1 )
            {
               sprintf ( message, "\n  Could not read complete record from "
                              "file %s.  -- local bias not calculated.\n"
                              "     Reason : ", filename1 );
               file_error_string = strerror ( errno );
               strcat ( message, file_error_string );
               printMessage ( message, logFile );
               *ierr = 1;

               fclose ( pStateFile );
               pStateFile = NULL;
               return;
            }

            /* The first record should always be for HRAP grid bin coordinate
               0,0 */
            if ( local_bias_record.col != 0 )
            {
               sprintf ( message, "  first record of direct access file does "
                                  "not have proper bin location -- local bias "
                                  "not calculated\n" );
               printMessage ( message, logFile );
               *ierr = 1;
               fclose ( pStateFile );
               pStateFile = NULL;
               return;
            }

            /* check if the state variables were updated in the immediately
               preceding hour */
            if ( local_bias_record.datetime == itime_b )
            {
               itime_p = local_bias_record.datetime;

               /* Set the record pointer. Read the record. */
               errno = 0;
               status = fseek ( pStateFile, ( iseq - 1 ) * state_rec_len,
                                SEEK_SET );

               if ( status != 0 )
               {
                  sprintf ( message, "\n  Could not find position of record "
                                     "%d in "
                                     "%s.  -- local bias not calculated.\n"
                                     "     Reason : ", iseq, filename1 );
                  file_error_string = strerror ( errno );
                  strcat ( message, file_error_string );
                  printMessage ( message, logFile );
                  fclose ( pStateFile );
                  pStateFile = NULL;
                  *ierr = 1;
                  return;
               }

               errno = 0;
               item_count = fread ( & local_bias_record, state_rec_len, 1,
                                    pStateFile );

               if ( item_count != 1 )
               {
                  sprintf ( message, "\n  Could not read complete record "
                                     "from file %s.  -- local bias not "
                                     "calculated.\n Reason : ", filename1 );
                  file_error_string = strerror ( errno );
                  strcat ( message, file_error_string );
                  printMessage ( message, logFile );
                  fclose ( pStateFile );
                  pStateFile = NULL;
                  *ierr = 1;
                  return;
               }

               if ( itime_p == local_bias_record.datetime )
               {
                  sprintf ( message, "  generic state variable file already "
                                     "contains state variables updated in the "
                                     "immediately preceding hour" );
                  printMessage ( message, logFile );
                  fclose ( pStateFile );
                  pStateFile = NULL;
                  already_updated = 1;
                  break;  /* for ( i ... ) */
               }
            }
         }
      }

      if ( pStateFile != NULL )
      {
         fclose ( pStateFile );
         pStateFile = NULL;
      }

      /* The generic state variable file was not updated in the
         immediately preceding hour. Look for the file containing the
         most-recently updated state variables */
      sprintf ( ctime_b, "%10d", itime_b );
      memset ( filename3, '\0', PATH_LEN );
      sprintf ( filename3, "%s/%sz", dirname, ctime_b );

      /* check if the file exists */
      exists = stat ( filename3, & file_stat );

      if ( exists == 0 )
      {
         /* old state variable file has been found: copy it to the generic
            state variable file */
         sprintf ( copy_file, "cp %s %s", filename3, filename1 );
         system ( copy_file );
         already_updated = 1;
         break;
      }
   }

   if ( already_updated == 0 )
   {
      /* old state variable file does not exist: newly initialize the
         generic state variable file */
      sprintf ( message, "  old state variable file does not exist - "
                         "initializing new generic state variable file" );
      printMessage ( message, logFile );


      status = local_bias_initialize ( pGeoData, pLocalBiasParams, pMPEParams, 
                            filename1, iseq );


      if ( status != 0 )
      {
        *ierr = 1;
         return;
      }
   }


   /* if the number of positive radar-gage pairs is less than the number of
      neighbors to be located, set the latter to the former. */
   if ( pGageRadarPair->pairNum < nbors ) nbors = pGageRadarPair->pairNum;

   /* check if the number of neighbors is too large */
   if ( nbors > NBMAX )
   {
      sprintf ( message, "number of neighbors %d exceeds nbmax %d..stop local "
                         "bias calculation\n", nbors, NBMAX);
      *ierr = 1;
      return;
   }

   /* sort the pairs in the ascending order of ju */
   if ( pGageRadarPair->pairNum > 1 )
   {
      sort_gage_radar_table ( pGageRadarPair );
   }

   /* start bin-by-bin estimation of gage and radar rainfall */
   /* Open the state variable file. */
   pStateFile = fopen ( filename1, "r" );

   if ( pStateFile == NULL )
   {
      sprintf ( message, "\n  error opening file = %s.\n"
                         "         -- local bias not calculated",
                         filename1 );
      printMessage ( message, logFile );
      * ierr = 1;
      return ;
    }

    /* initialize the sequential record number */
    /* Keep the previously determined iseq setting. */
    knt = 0;

    /* Allocate memory for the array of local bias records. */
    /* Don't forget to free this when done. */
    local_bias_array = ( local_bias_values_record * )
                       malloc ( iseq * sizeof ( local_bias_values_record ) );

    if ( local_bias_array == NULL )
    {
       sprintf ( message, "Could not allocate memory for "
                          "local_bias_array.\n" );
       shutDownMPE ( message, logFile );
    }
    iseq = 0;

    /* Set the time for the local bias records being written.
       This time is the current_time. */
    itime_c = get_lltime ( pRunDate->tRunTime );

     /**
     * build the neighbor list within range
     * determined by radius.
     **/
    MPEFieldGen_buildNeighborList (pGeoData , pMPEParams,
                 gageSize, iug,  ivg, zg ) ;

   /* Do 50 */
    for ( iu0 = 0; iu0 < pGeoData->num_cols;
                   iu0 = iu0 + pLocalBiasParams->mult )
    {
       /* Do 60 */
       for ( iv0 = 0; iv0 < pGeoData->num_rows;
                      iv0 = iv0 + pLocalBiasParams->mult )
       {
          /* read old state variables */
          errno = 0;
          item_count = fread ( & local_bias_array [ iseq ],
                           state_rec_len, 1, pStateFile );

          if ( item_count != 1 )
          {
             sprintf ( message, "\n  Could not read complete record from file "
                                "%s.  -- local bias not calculated.\n"
                                "     Reason : ", filename1 );
             file_error_string = strerror ( errno );
             strcat ( message, file_error_string );
             printMessage ( message, logFile );
             *ierr = 1;
             fclose ( pStateFile );
             pStateFile = NULL;
             return;
          }

          if ( local_bias_array [ iseq ].col != iu0 )
          {
             sprintf ( message, " i = %d  iu0 = %d\n",
                       local_bias_array [ iseq ].col, iu0 );
             printMessage ( message, logFile );
             sprintf ( message, "  i ne iu0 -- direct access record does not "
                                "have proper bin lcoation -- local bias not "
                                "calculated" );
             printMessage ( message, logFile );
             fclose ( pStateFile );
             pStateFile = NULL;

             if ( local_bias_array != NULL )
             {
                free ( local_bias_array );
                local_bias_array = NULL;
             }

             return;
          }

          if ( local_bias_array [ iseq ].row != iv0 )
          {
             sprintf ( message, "  j=%d  iv0=%d\n",
                                local_bias_array [ iseq ].row, iv0 );
             printMessage ( message, logFile );
             sprintf ( message, "  j ne iv0 -- direct access record does not "
                                "have proper bin location -- local bias "
                                "not calculated\n" );
             fclose ( pStateFile );
             pStateFile = NULL;

             if ( local_bias_array != NULL )
             {
                free ( local_bias_array );
                local_bias_array = NULL;
             }

             return;
          }

          /* initialze the number of positive radar-gage pairs within the
             radius of influence */
          npair = 0;

          if ( pGageRadarPair->pairNum > 0 )
          {
              MPEFieldGen_findLocalBiasNeighborList (
                          pGageRadarPair, iug, ivg, iradi,
                          iu0, iv0, ilist, rlist, & npair );

             /* if the number data points within the radius of iradi is
                larger than the number of neighbors specified, take only
                the nbors nearest data points.  In this context npair
                is the number of neighbors found. */
             if ( npair > nbors ) npair = nbors;

             /* count the number of neighbors within the radius of
                influence */
             if ( npair > 0 )
             {
                /* there exists at least one neighbor within the radius of
                   influence */
                if ( pLocalBiasParams->rho0 == 0 )
                {
                   /* correlation structure is white-noise: specify the
                      weights outright */
                   if ( pLocalBiasParams->type_of_est == 1 )
                   {
                      for ( i = 0; i < npair; ++i )
                      {
                         wgt[i]=1.0;
                      }
                   }
                   else
                   {
                      for ( i = 0; i < npair; ++i )
                      {
                         wgt[i]=1.0 / ( npair + 1 );
                      }
                   }
                }
                else
                {
                   /* construct the (npair)x(npair) upper-triangular
                      measurement error correlation coefficient matrix */
                   err_cor_mat ( pGageRadarPair, pLocalBiasParams, npair, iu0,
                                 iv0, range, ilist, cormat );

                   /* construct the (1)x(npair) unit vector */
                   for ( i = 0; i < npair; ++i )
                   {
                      wgt [ i ] = 1.0;
                   }

                   /* solve cormat*xt=h */
                   iflag = 0;
                   lsolve_array ( npair, cormat, wgt, & iflag );

                   if ( iflag == 1 )
                   {
                       sprintf ( message, "error from lsolve_array ... "
                                          "stop calc\n" );
                       printMessage ( message, logFile );

                       if ( local_bias_array != NULL )
                       {
                          free ( local_bias_array );
                          local_bias_array = NULL;
                       }

                       return;
                   }
                }
             }
          }

          /* calculate the number of hours elapsed since the last update
             case of itime_c < itime_p results in lag=-99 */
          /* Get the current time. Is the diftime routine even needed
             anymore?*/
          itime_p = local_bias_array [ iseq ].datetime;
          lag = diftime ( pRunDate, itime_p );

          if ( lag < 0 )
          {
             sprintf ( message, "itime_p: %d \n", itime_p );
             printMessage ( message, logFile );

             if ( local_bias_array != NULL )
             {
                free ( local_bias_array );
                local_bias_array = NULL;
             }

             return;
          }

          /* update via exponential smoothing */
          iflag = 0;

          /* pLocalBiasParams provides nspan, span, itype_of_est.
             pGageRadarPair:  zg, zr, npair.
             ilist stays the same.
             iflag stays the same.
             wgt stays the same. */

          /* In this case, npair means the number of non-zero neighbors
             found. */
          iflag = update ( pLocalBiasParams,
                           pGageRadarPair,
                           pMPEParams,
                           & local_bias_array [ iseq ],
                           npair,
                           lag,
                           wgt,
                           ilist );

          if ( iflag == 1 )
          {
             sprintf ( message, "error from update...stop calc" );
             printMessage ( message, logFile );

             if ( local_bias_array != NULL )
             {
                free ( local_bias_array );
                local_bias_array = NULL;
             }

             return;
          }

          /* choose the "best" bias */
          for ( i = 0; i < pMPEParams->ptrRWBiasStat->num_span; ++i )
          {
             if ( local_bias_array [ iseq ].local_bias_val [ i ].si
                  >= si_cut )
             {
                /* Fisher information content is acceptably large */
                if ( local_bias_array [ iseq ].local_bias_val [ i ].xr >
                     0.0 )
                {

                   /* mean radar rainfall at this bin is positive */
                   local_span [iv0] [iu0] = i;
                   local_bias [iv0] [iu0] =
                        local_bias_array [ iseq ].local_bias_val [ i ]. xg /
                        local_bias_array [ iseq ].local_bias_val [ i ]. xr;
                   ++knt;
                   break;
                }
             }
          }

          /* Set the local bias record time to be that for the current
             hour. */
          local_bias_array [ iseq ].datetime = itime_c;

          /* specify the record number in the direct access file */
          ++iseq;
       }
    }

   /* Close the state variable file.  Then open it for overwriting. */
   fclose ( pStateFile );
   pStateFile = NULL;

   pStateFile = fopen ( filename1, "w" );

   if ( pStateFile == NULL )
   {
      sprintf ( message, "\n  error opening file = %s.\n"
                         "         -- state variable file not updated",
                         filename1 );
      printMessage ( message, logFile );
      * ierr = 1;

      if ( local_bias_array != NULL )
      {
         free ( local_bias_array );
         local_bias_array = NULL;
      }

      return;
   }

   /* Write the updated local bias record out to the state
      variable file. Keep track of the record number so that
      the records may be updated in place. */
   item_count = fwrite (  local_bias_array,
                          sizeof ( local_bias_values_record ),
                          iseq, pStateFile );

   if ( item_count != iseq )
   {
      /* Write Error ... */
   }

   fclose ( pStateFile );
   pStateFile = NULL;


   /* State variables have now been updated: determine whether to write out
      the state variables for the current hour or not. */
   lltime ( pRunDate->tRunTime, & tm );

    if ( ( tm.tm_hour % pLocalBiasParams->mult_hour ) == 0 )
    {

       /* The current hour is divisible by int_mult_hour. write out the state
          variable file for the current hour. */
       sprintf ( message, "  This hour is divisible by int_mult_hour - write "
                          "out state variables to a file." );
       printMessage ( message, logFile );

       sprintf ( copy_file, "cp %s %s", filename1, filename2 );
       system ( copy_file );
    }


    /* if local bias is not estimated at every bin, fill holes via
       interpolation */
    sprintf ( message, "number of bins local bias is estimated for=%d\n",
                       knt );
    printMessage ( message, logFile );

    if ( iseq < ( pGeoData->num_cols * pGeoData->num_rows ) )
    {
       if ( pLocalBiasParams->mult  > 1 )
       {
          /* interpolate via spiral search or via Kafritsas and Bras */
          if ( pLocalBiasParams->interp == 1 )
          {
             interpolate_nearby ( pGeoData, pLocalBiasParams, local_span,
                                  local_bias );
          }
          else if ( pLocalBiasParams->interp == 2 )
          {
             interpolate_everywhere ( pGeoData, pLocalBiasParams,
                                      local_span, local_bias, dist );
          }
       }
    }


    for ( i = 0; i < pGeoData->num_rows; ++i )
    {
       for ( j = 0; j < pGeoData->num_cols; ++j)
       {
          if ( local_span [ i ] [ j ] >= 0 && RMosaic [ i ] [ j ] > 0.0)
          {
             lmosaic [ i ] [ j ] = local_bias [ i ] [ j ] * RMosaic [ i ] [ j ];
          }
          else
          {
             lmosaic [ i ] [ j ] = RMosaic [ i ] [ j ];
          }
       }
    }

    if ( local_bias_array != NULL )
    {
       free ( local_bias_array );
       local_bias_array = NULL;
    }

    return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/local_bias.c,v $";
 static char rcs_id2[] = "$Id: local_bias.c,v 1.1 2007/10/15 12:19:09 dsa Exp lawrence $";}
/*  ===================================================  */

}
