/***********************************************************************
* Filename: run_lsatpre.c
*
* Original Author: Unknown. Probably DJ Seo or Jay Breidenbach.
*
* File Creation Date: March 28, 2005
*
* Development Group: HSEB/OHD
*
* Description:
* Creates the local bias satellite precipitation product
* 
* Modules:
* runLSatpre
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: runLSatpre
*
* Original Author: Unknown. Probably DJ Seo or Jay Breidenbach.
*
* Module Creation Date: March 28, 2005
* 
* Description:
* This function computes the local bias satellite precipitation product
*
* Calling Arguments:
* Name         Input/Output Type             Description
*
* pRunDate     Input        const run_date_struct *  date/time 
* pGeoData     Input        const geo_data_struct *
*                                            global HRAP lowerleft-corner
*                                            bin and dimension and dimension
*                                            of the RFC estimation domain
* pMPEParams   Input/Output mpe_params_struct *
*                                            static parameters
* gageSize     Input       int              the size of gage array.
* iug          Input       short *          the X_coord array of gage data.
* ivg          Input       short *          the Y_coord array of gage data.
* zg           Input       float *          the gage value array.
* RMosaic      Input       double **        the raw dsp mosaic.
* LSatpre      Output      double **        local bias satellite precip mosaic.
* QPEMosaic    Output      double **        the best estimate mosaic 
* 
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* allocLSatPreMemory, freeLSatPreMemory, read_satellite, getLocalBiasParams,
* apply_mpe_polygons, lb_gr_pairs, local_bias, writeArray
*
* Return Value:
* Type          Description
* None
*
* Error Codes/Exceptions:
* 
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type       Description
*
* Modification History:
* Date        Developer         Action
*             C. R. Kondragunta Original FORTRAN code  
* 03/28/2005  Bryon Lawrence    Converted Fortran to C.
* 05/01/2007  Guoxian Zhou      Modified for empe 
*
***********************************************************************/

static short ** local_mem_span = NULL;
static double ** local_mem_bias = NULL;
static gage_radar_pair_table_struct * pGageRadarPairTable = NULL; 

static void initMosaicArray ( const geo_data_struct * pGeoData,
                              const int gageSize )
{
   const int rowSize = pGeoData->num_rows ;
   const int colSize = pGeoData->num_cols ;

   /*
    * Allocate memory for the local_mem_span  and local_mem_bias array.
    */ 

   local_mem_span = init2DShortArray(SPAN_DEFAULT, rowSize, colSize);

   local_mem_bias = init2DDoubleArray(BIAS_DEFAULT, rowSize, colSize);

   /*
    * allocate memory for gage radar pair struct data
    */

   pGageRadarPairTable =
         (gage_radar_pair_table_struct *)
         malloc(sizeof(gage_radar_pair_table_struct));
   if(pGageRadarPairTable == NULL)
   {
       sprintf ( message , "ERROR: Memory allocation failure"
                           " in runLSatpre function."
                           "\n\tProgram exit");
       shutdown( message);
    }

    pGageRadarPairTable->ptrGageRadarPair =
          (gage_radar_pair_struct *)
          malloc(gageSize * sizeof(gage_radar_pair_struct));
    if(pGageRadarPairTable->ptrGageRadarPair == NULL)
    {
       sprintf ( message , "ERROR: Memory allocation failure"
                           " in runLSatpre function."
                           "\n\tProgram exit");
       shutdown( message);
    }

    return ;
}

static void freeMosaicArray ( const geo_data_struct * pGeoData )
{

   const int rowSize = pGeoData->num_rows ;

   free2DShortArray(local_mem_span, rowSize );    

   free2DDoubleArray(local_mem_bias, rowSize );    

   if ( pGageRadarPairTable != NULL )
   {
	   if ( pGageRadarPairTable->ptrGageRadarPair != NULL )
	   {
	      free ( pGageRadarPairTable->ptrGageRadarPair );
	      pGageRadarPairTable->ptrGageRadarPair = NULL ;
	   }

      free ( pGageRadarPairTable );
      pGageRadarPairTable = NULL;
   }

   return;
}

void runLSatpre  ( const run_date_struct * pRunDate,
                   const geo_data_struct * pGeoData,
                   empe_params_struct * pMPEParams,
                   const int gageSize,
                   short * iug ,
                   short * ivg ,
                   float * zg ,
                   double ** RMosaic,
                   double ** LSatpre,
                   double ** QPEMosaic )
{
  static char strDateTime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'};
  static char fileName [ PATH_LEN ] = {'\0'};
  double ** satpre = NULL;
  double si_cut = 5.0;
  int gr_min_value = 0;
  int ierr;
  long int irc;
  int isat_avail;
  int i, j ;
  int year;
  int month;
  int day;
  int hour;

  static int first = 1 ;
  static char satpreDir[PATH_LEN] = {'\0'};
  static char stateVarDir[PATH_LEN] = {'\0'};

  const int rowSize = pGeoData->num_rows ;
  const int colSize = pGeoData->num_cols ;
  const int replace_missing = 0 ;
  const local_bias_params * pLocalBiasParams = NULL;

  struct tm * pRunTime = NULL ;

  const char * EMPE_MOSAIC_DIR_TOKEN = "hpe_lsatpre_dir";
  const char * EMPE_SAT_STATEVAR_DIR_TOKEN = "hpe_sat_statevar_dir";

  /*
   * Write lsatpre based on input options.
   */

  if ( first == 1 )
  {
     hpe_fieldgen_getAppsDefaults ( EMPE_MOSAIC_DIR_TOKEN, satpreDir ); 
     hpe_fieldgen_getAppsDefaults ( EMPE_SAT_STATEVAR_DIR_TOKEN, stateVarDir );
  }

  /*
   * Retrieve the local bias settings. 
   */

  pLocalBiasParams = getLocalBiasParams ( ); 

  /*
   * Log whether or not the local bias recalculation
   * will be performed.
   */

  if ( pMPEParams->locbias_1hr_rerun == 1 )
  {
     sprintf ( message, "local bias recalculation on rerun = ON" );
     hpe_fieldgen_printMessage( message );
  }
  else
  {
     sprintf ( message, "local bias recalculation on rerun = OFF" );
     hpe_fieldgen_printMessage( message );
  }

  /*
   * specify local bias parameters
   */

  if ( ( pMPEParams->locbias_1hr_rerun == 1 ) || ( pRunDate->hourNum > 1 ) )
  {
     sprintf ( message, "  *** Local Bias Parameters ***" );
     hpe_fieldgen_printMessage( message );

     if ( pLocalBiasParams->interp == 2 )
     {
        sprintf ( message, "  search radius = %4.1f km", 
                  pLocalBiasParams->dist_cut );
        hpe_fieldgen_printMessage( message );
     }

     sprintf ( message, "  npairs_bias_select for radar = %3.0f"
                        "  npair_bias_select for satellite = %3.0f",
                        pLocalBiasParams->sel_npr_lb_rad, 
                        pLocalBiasParams->sel_npr_lb_sat );
     hpe_fieldgen_printMessage( message );
  }
  else
  {
     sprintf ( message, "  *** local bias corrections not done ***" );
     hpe_fieldgen_printMessage( message );
  }

  /*
   * Determine whether or not to do the local bias correction
   * for the satellite precipitation. 
   */

  if ( ( pMPEParams->locbias_1hr_rerun == 1 ) || ( pRunDate->hourNum > 1 ) )
  {
     sprintf ( message, "  local bias correction for satellite" );
     hpe_fieldgen_printMessage( message );

     /*
      * strDateTime string should be in format: yyyymmddHHMM
      */
 
     pRunTime = gmtime(&(pRunDate->tRunTime)) ;
     strftime(strDateTime, ANSI_YEARSEC_TIME_LEN + 1, "%Y%m%d%H%M", pRunTime);

     year = pRunTime->tm_year + 1900;
     month = pRunTime->tm_mon + 1;
     day = pRunTime->tm_mday;
     hour = pRunTime->tm_hour;

     hpe_fieldgen_getCurrentTime(currTime);
     sprintf ( message, "%s = time begin LSATPRE calculation", currTime );
     hpe_fieldgen_printMessage( message );

     /* Read the satellite data:
        isat_avail = satellite availability flag
                   = 0 - satellite data not available
                   = 1 - satellite data available */
     
     satpre = read_satellite ( pRunDate,
                               pGeoData,
                               pMPEParams->hrap_grid_factor,
                               hour,
                               & isat_avail );

     /* Store positive gage/satellite pairs in G/S arrays. */

     if ( isat_avail == 0 )
     {
        sprintf ( message, "satellite data not available for current hour.");
        hpe_fieldgen_printMessage(message );
     }
     else
     {
        pMPEParams->sat_avail = 1; 

#if APPLY_POLYGON

        /* Apply edit polygons to the avgrmosaic product for
           use in furture products. */
 
        apply_mpe_polygons ( satpre,
                             strDateTime,
                             year,
                             month,
                             day,
                             hour,
                             display_satPrecip,
                             pGeoData,
                             FACTOR_PRECIP,
                             0,
                             0 ); 
#endif
          
        /* Allocate memory for the local span and local bias value arrays. */
 
        initMosaicArray ( pGeoData, gageSize );

        lb_gr_pairs ( gr_min_value, 
                      gageSize, iug, ivg, zg,
                      satpre,
                      pGageRadarPairTable );

        /* Compute the local bias values. */

        local_bias ( pRunDate,
                     pGeoData,
                     gageSize, iug, ivg, zg,
                     pMPEParams,
                     pLocalBiasParams,
                     si_cut,
                     pGageRadarPairTable,
                     satpre,
                     local_mem_span,
                     local_mem_bias,
                     stateVarDir,
                     LSatpre,
                     & ierr );

        /*
         * If an error occurred generating the local bias field,
         * then set the LSATPRE field equal to the SATPRE field.
         */
 
        if ( ierr > 0 )
        {
           sprintf ( message, "  error occurred generating the local bias"
                              " field - lsatpre field set to satpre field" );
           printLogMessage( message ); 

           for ( i = 0; i < rowSize;  ++i )
           {
              for ( j = 0; j < colSize; ++j )
              {
                 LSatpre [i][j] = satpre [i][j];
              }
           }
        }

        /*
         * Deallocate memory for the local span and local bias value arrays.
         * These are currently not viewable by the user.
         */

        freeMosaicArray ( pGeoData );
     }
   }

   hpe_fieldgen_getCurrentTime ( currTime );
   sprintf ( message, "   %s = time end   LSATPRE calculation", currTime );
   hpe_fieldgen_printMessage( message ); 

   /* Write out gridded data in xmrg format to flat files.
      ireplmis = replace missing values with 0.0 flag
               = 0 -- do not replace missing values (default)
               = 1 -- replace missing values (values < 0.0) with 0.0 */
 
   hpe_fieldgen_getCurrentTime ( currTime ); 
   sprintf ( message, "%s = time begin writing lsatpre field to flat file.",
                      currTime );
   hpe_fieldgen_printMessage( message ); 

   if ( ( pMPEParams->locbias_1hr_rerun == 1 ) || ( pRunDate->hourNum > 1 ) )
   {
      if ( isat_avail > 0 )
      {
         sprintf(fileName, "LSATPRE%s%sz",
                 pMPEParams->category_name, strDateTime ); 
         writeArray ( pGeoData, satpreDir, fileName, FACTOR_PRECIP, 
                      replace_missing, pMPEParams->user, pRunDate->tRunTime, 
                      PROC_FLAG, LSatpre, &irc ); 

         /* Check the return status of the lsatpre file write. */
 
         if( irc != 0 )
         {
            sprintf ( message, "  *** error number = %ld "
                               "attempting to write file %s/%s", 
                               irc, satpreDir, fileName );
            printLogMessage( message );
         }
         else
         {
            sprintf ( message, "STATUS: file written to %s/%s", 
                      satpreDir, fileName );
            printLogMessage( message );
         }
      }
   }

   /* Show the time that the write to the flat file was completed. */

   hpe_fieldgen_getCurrentTime ( currTime ); 
   sprintf ( message, "  %s = time end writing lsatpre field to flat file.",
                      currTime );     
   hpe_fieldgen_printMessage( message );

   /* If necessary, copy the LSATPRE data to the QPEmosaic array. */

   if ( strcmp ( pMPEParams->qpe_fieldtype, "lsatpre") == 0)
   {
      for ( i = 0; i < rowSize; ++i )
      {
         for ( j = 0; j < colSize; ++ j )
         {
            QPEMosaic[i][j] = LSatpre[i][j];
         }
      }
   }
   
   first = 0 ;

   return;
}
