/*******************************************************************************
* FILENAME:            run_lsatpre.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          runlsatpre
* DESCRIPTION:         Creates the local bias satellite precipitation 
*                      product.
*
* ORIGINAL AUTHOR:     Unknown. Probably DJ Seo or Jay Breidenbach.
* CREATION DATE:       March 28, 2005
* ORGANIZATION:        HSEB/OHD
* MACHINE:             Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE            PROGRAMMER        DESCRIPTION/REASON
*          1        March 28, 2005  Bryon Lawrence    Converted Fortran to C. 
*                   March 2007      P Tilles          Removed print of time begin/end
*                                                      for outputting grid
********************************************************************************
*/
#include "mpe_fieldgen.h"

static short ** local_mem_span = NULL;
static double ** local_mem_bias = NULL;
static gage_radar_pair_table_struct * pGageRadarPairTable = NULL; 

static void allocLSatPreMemory ( const geo_data_struct * pGeoData,
                                 int gageSize )
{
   int i;

   /* Allocate memory for the local_mem_span array. */ 
   local_mem_span = ( short ** ) malloc ( pGeoData->num_rows * 
                                          sizeof (short *) ); 

   if ( local_mem_span == NULL )
   {
      sprintf ( message, "ERROR: memory allocation failure"
                         " in runLSatpre function."
                         "\n\tProgram exit."); 
      shutDownMPE ( message, logFile );
   }

   for ( i = 0; i < pGeoData->num_rows; ++i )
   {
       local_mem_span [ i ] = ( short * ) malloc ( pGeoData->num_cols *
                                                   sizeof ( short ) );
       if ( local_mem_span [ i ] == NULL )
       {
          sprintf ( message, "ERROR: memory allocation failure"
                             " in runLSatpre function."
                             "\n\tProgram exit.");
          shutDownMPE ( message, logFile );
       }
   }

   /* Allocate memory for the local_mem_bias array. */
   local_mem_bias = ( double ** ) malloc ( pGeoData->num_rows * 
                                           sizeof ( double *) ); 

   if ( local_mem_bias == NULL )
   {
      sprintf ( message, "ERROR: memory allocation failure"
                         " in runLSatpre function."
                         "\n\tProgram exit."); 
      shutDownMPE ( message, logFile );
   }

   for ( i = 0; i < pGeoData->num_rows; ++i )
   {
       local_mem_bias [ i ] = ( double * ) malloc ( pGeoData->num_cols *
                                                    sizeof ( double ) );
       if ( local_mem_bias [ i ] == NULL )
       {
          sprintf ( message, "ERROR: memory allocation failure"
                             " in runLSatpre function."
                             "\n\tProgram exit.");
          shutDownMPE ( message, logFile );
       }
   }

   /**
   * allocate memory for gage radar pair struct data
    **/
   pGageRadarPairTable =
   (gage_radar_pair_table_struct *)malloc(sizeof(gage_radar_pair_table_struct));
   if(pGageRadarPairTable == NULL)
   {
       sprintf ( message , "ERROR: Memory allocation failure"
                           " in runLMosaic function."
                           "\n\tProgram exit");
       shutDownMPE( message, logFile );
    }

    pGageRadarPairTable->ptrGageRadarPair =
     (gage_radar_pair_struct *)malloc(gageSize * sizeof(gage_radar_pair_struct));
    if(pGageRadarPairTable->ptrGageRadarPair == NULL)
    {
       sprintf ( message , "ERROR: Memory allocation failure"
                           " in get_mean_bias function."
                           "\n\tProgram exit");
       shutDownMPE( message, logFile );
    }

    return ;
}

static void releaseLSatPreMemory ( const geo_data_struct * pGeoData )
{
   int i;
    
   if ( local_mem_span != NULL )
   {
      for ( i = 0; i < pGeoData->num_rows; ++i ) 
      {
         if ( local_mem_span [ i ] != NULL )
         {
            free ( local_mem_span [ i ] );
            local_mem_span [ i ] = NULL;
         }

      }

      free ( local_mem_span );
      local_mem_span = NULL;
   }

   if ( local_mem_bias != NULL )
   {
      for ( i = 0; i < pGeoData->num_rows; ++i )
      {
         if ( local_mem_bias [ i ] != NULL )
         {
            free ( local_mem_bias[ i ] );
            local_mem_bias [ i ] = NULL;
         }

      }

      free ( local_mem_bias );
      local_mem_bias = NULL;
   }

   if ( pGageRadarPairTable->ptrGageRadarPair != NULL )
   {
      free ( pGageRadarPairTable->ptrGageRadarPair );
      pGageRadarPairTable->ptrGageRadarPair = NULL ;
   }

   if ( pGageRadarPairTable != NULL )
   {
      free ( pGageRadarPairTable );
      pGageRadarPairTable = NULL;
   }

   return;
}

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    runlsatpre
* PURPOSE:        Creates the local bias satellite precipitation product.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  run_date_struct * pRunDate,
*   Input  geo_data_struct * pGeoData,
*   Input  mpe_params_struct * pMPEParams,
*   Input  gage_data_struct * pGageData, 
*   Output double ** LSatpre
*   Input  double ** RMosaic
*   Output double ** QPEMosaic
*
* RETURNS:
*   None.
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME              DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

void MPEFieldGen_runLSatpre  ( const run_date_struct * pRunDate,
                   const geo_data_struct * pGeoData,
                   mpe_params_struct * pMPEParams,
                   const int gageSize,
                   short * iug ,
                   short * ivg ,
                   float * zg ,
                   double ** RMosaic,
                   double ** LSatpre,
                   double ** QPEMosaic )
{
  static char dateYMD[YYYYMMDDHH_LEN + 1] = {'\0'};
  static char fileName [ PATH_LEN ] = "" ;
  double ** satpre = NULL;
  float si_cut = 5.0;
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
  static char satpreDir[PATH_LEN] = "" ;
  static char stateVarDir[PATH_LEN] = "" ;

  const int rowSize = pGeoData->num_rows ;
  const int colSize = pGeoData->num_cols ;
  const int replace_missing = 0 ;
  const local_bias_params * pLocalBiasParams = NULL;

  struct tm * pRunTime = NULL ;

  getCurrentTime(currTime);
  sprintf ( message, "%s = time begin LSATPRE calc\n", currTime );
  printMessage ( message, logFile );

  /* Write lsatpre based on input options. */
  if ( first == 1 )
  {
     getAppsDefaults ( "rfcwide_lsatpre_dir", satpreDir ); 
     getAppsDefaults ( "rfcwide_sat_statevar_dir", stateVarDir );
  }


  /* Retrieve the local bias settings. */
  pLocalBiasParams = MPEFieldGen_getLocalBiasParams ( ); 

  /* Log whether or not the local bias recalculation will be
     performed. */
  if ( pMPEParams->locbias_1hr_rerun == 1 )
  {
     sprintf ( message, "local bias recalculation on rerun = ON\n" );
  }
  else
  {
     sprintf ( message, "local bias recalculation on rerun = OFF\n" );
  }

  printMessage ( message, logFile );
   

  /* Initialize the arrays. */
  for ( i = 0; i < rowSize; ++i )
  {
     for ( j = 0; j < colSize; ++j )
     {
        LSatpre [i][j] = MOSAIC_DEFAULT ;
     }
  }

  /* Determine whether or not to do the local bias correction for
     the satellite precipitation. */
  if ( ( pMPEParams->locbias_1hr_rerun == 1 ) || ( pRunDate->hourNum > 1 ) )
  {

     if ( pLocalBiasParams->interp == 2 )
     {
        sprintf ( message, "  search radius = %4.1f km\n", 
                  pLocalBiasParams->dist_cut );
        printMessage ( message, logFile );
     }
     
     sprintf ( message, "  npairs_bias_select for satellite = %3.0f",
                        pLocalBiasParams->sel_npr_lb_sat );
     printMessage ( message, logFile );

     /**
      * dateYMD string should be in format: yyyymmddhh
     **/
     pRunTime = gmtime(&(pRunDate->tRunTime)) ;
     strftime(dateYMD, YYYYMMDDHH_LEN + 1, "%Y%m%d%H", pRunTime);

     year = pRunTime->tm_year + 1900;
     month = pRunTime->tm_mon + 1;
     day = pRunTime->tm_mday;
     hour = pRunTime->tm_hour;

     /* Read the satellite data:
        isat_avail = satellite availability flag
                   = 0 - satellite data not available
                   = 1 - satellite data available */
     
     satpre = MPEFieldGen_read_satellite ( pRunDate,
                               pGeoData,
                               hour,
                               & isat_avail );
      
     /* Store positive gage/satellite pairs in G/S arrays. */
     if ( isat_avail == 0 )
     {
        sprintf ( message, "  satellite data not available for current "
                           "hour.\n" );
        printMessage (message, logFile );
     }
     else
     {
        pMPEParams->sat_avail = 1; 

        /* Apply edit polygons to the avgrmosaic product for
           use in furture products. */
        MPEFieldGen_apply_mpe_polygons ( satpre,
                             dateYMD,
                             year,
                             month,
                             day,
                             hour,
                             display_satPrecip,
                             pGeoData,
                             FACTOR_PRECIP,
                             0,
		             0 ); 
          
        /* Allocate memory for the local span and local bias value arrays. */
        allocLSatPreMemory ( pGeoData, gageSize );

        MPEFieldGen_lb_gr_pairs ( gr_min_value, 
                      gageSize, iug, ivg, zg,
                      satpre,
                      pGageRadarPairTable );

        /*  generate the LSATPRE field.
	*/

        /* Intialize the local bias and local span arrays. */
        for ( i = 0; i < rowSize; ++i ) 
        {
           for ( j = 0; j < colSize; ++j )
           { 
              local_mem_span[i][j]=-1;
              local_mem_bias[i][j]=-9.0;
           }
        }

        /* Compute the local bias values. */
        MPEFieldGen_local_bias ( pRunDate,
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
                     
        /* Deallocate memory for the local span and local bias value arrays. 
           These are currently not viewable by the user. */
        releaseLSatPreMemory ( pGeoData );

        /* If an error occurred generating the local bias field, then set
           the LSATPRE field equal to the SATPRE field. */
        if ( ierr > 0 )
        {
           sprintf ( message, "  error occurred generating the local bias"
                              " field - lsatpre field set to satpre field" );
           printMessage ( message, logFile ); 

           for ( i = 0; i < rowSize;  ++i )
           {
              for ( j = 0; j < colSize; ++j )
              {
                 LSatpre [i][j] = satpre [i][j];
              }
           }
        }
     }
   }
   else
   {
      sprintf ( message, "  *** local bias corrections not done ***\n" );
      printMessage ( message, logFile );
   }

   getCurrentTime ( currTime );
   sprintf ( message, "%s = time end   LSATPRE calc\n", currTime );
   printMessage ( message, logFile ); 

   /* Write out gridded data in xmrg format to flat files.
      ireplmis = replace missing values with 0.0 flag
               = 0 -- do not replace missing values (default)
               = 1 -- replace missing values (values < 0.0) with 0.0 */

   if ( ( pMPEParams->locbias_1hr_rerun == 1 ) || ( pRunDate->hourNum > 1 ) )
   {
      if ( isat_avail > 0 )
      {
         sprintf ( fileName, "LSATPRE%sz", dateYMD );
         MPEFieldGen_writeArray ( pGeoData, satpreDir, fileName, FACTOR_PRECIP, 
                      replace_missing, pMPEParams->user, pRunDate->tRunTime, 
                      PROC_FLAG, LSatpre, &irc ); 

         /* Check the return status of the lsatpre file write. */
         if( irc != 0 )
         {
            sprintf ( message, "  *** error number = %ld "
                               "attempting to write file %s/%s\n", irc, 
                               satpreDir, fileName );
         }
         else
         {
            sprintf ( message, "file written to %s/%s\n", 
                      satpreDir, fileName );
         }

         printMessage ( message, logFile );
      }
   }

   /* If necessary, copy the LSATPRE data to the QPEmosaic array. */
   if ( strcmp ( pMPEParams->qpe_fieldtype, "lsatpre") == 0)
   {
      for ( i = 0; i < rowSize; ++i )
      {
         for ( j = 0; j < colSize; ++ j )
         {
            QPEMosaic[i][j] = LSatpre [i][j];
         }
      }
   }
   
   first = 0 ;

   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/run_lsatpre.c,v $";
 static char rcs_id2[] = "$Id: run_lsatpre.c,v 1.1 2007/10/15 12:19:15 dsa Exp lawrence $";}
/*  ===================================================  */

}
