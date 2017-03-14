
/*******************************************************************************
* FILENAME:            read_satellite.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          read_satellite
* DESCRIPTION:         This subroutine checks for the existence of a
*                      Auto SPE file with HH00 timestamp.
*                      If HH00 file does not exist, then routine
*                      checks for file with timestamp HH15.
*                      If file found, then read into array for use by MPE FieldGen.
*                      FieldGen.
*   MODULE 2:          free_spe_memory
* DESCRIPTION:         Deallocates the memory used by the SPE data array.
*
* ORIGINAL AUTHOR:     Unknown
* CREATION DATE:       Unknown
* ORGANIZATION:        OHD/HSEB
* MACHINE:             Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        3/18/2005    Bryon Lawrence    Converted from FORTRAN to
*                                                  C.
********************************************************************************
*/
#include <sys/stat.h>     /* For the stat routine. */
#include <sys/types.h>    /* For the struct stat type. */
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "mpe_fieldgen.h"
#include "time_convert.h" /* For YYYYMMDD_DATE_LEN and MMDDYYYY_DATE_LEN
                             definitions. */
#include "time_defs.h"    /* For SECONDS_PER_HOUR definition. */

/* The satellite precpitation array. */
static double ** pSatPre = NULL;

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   read_satellite
* PURPOSE:       This routine reads satellite precipitation data from an
*                Auto SPE NetCDF file.   This file contains precipitation data
*                for the whole country.  A subset of precpitation data is
*                taken for the MPE forecast area of the office MPE Fieldgen
*                is being run at.
*
*                If the satellite data are not available, then the
*                satellite availability flag is set to 0.  Otherwise,
*                it will be set to 1 and the satellite precipitation
*                array will contain the data.
*
*                The user is responsible for deallocating the memory used
*                by the satellite precipitation array.
*
* ARGUMENTS:
*   TYPE   DATA TYPE       NAME         DESCRIPTION/UNITS
*   Input  run_date_struct pRunDate     MPE Fieldgen run time and date
*                                       information.
*   Input  geo_data_struct pGeoData     MPE Fieldgen HRAP information.
*   Input  int             run_hour     The hour of this MPE run.
*   Output int *           is_sat_avail Satellite availability flag.
*                                       0 = Sat data not available for
*                                       this time.  1 = Sat data available
*                                       for this time.
*
* RETURNS:
*   DATA TYPE           DESCRIPTION
*   double **           The array of satellite estimated precipitation
*                       read from NetCDF.
* APIs UTILIZED:
*   NAME                HEADER FILE     DESCRIPTION
*   getAppsDefaults     GeneralUtil.h   Retrieves the value of a application
*                                       configurable token.
*   read_spe            mpe_fieldgen.h  Retrieves the satellite precipitation
*                                       data.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE   NAME                DESCRIPTION
*   char        dateYMD []          Contains the YYYYMMDD date of the
*                                   MPE hour being processed.
*   char        dateYMD_prev []     Contains the YYYYMMDD date of the
*                                   MPE hour processed during the last call
*                                   to this routine.
*   char *      rfcwide_satpre_dir  The token containing the directory the
*                                   Auto SPE NetCDF files are  located in.
*   char        satpre_dirname []   The directory the Auto SPE NetCDF files
*                                   are located in.
*   char        satpre_filename []  The name of the file containing
*
*   int         first               Indicates if this is the first call to
*                                   this routine.
*   int         last_hour           The hour preceding the hour for which
*                                   MPE data is being processed.
*   int         i                   A loop index variable.
*   int         is_sat_avail_prev   For the last time this routine was
*                                   called, was satellite data available?
*   int         j                   A loop index variable.
*   int         previous_hour       The hour for which MPE data was being
*                                   processed when this routine was last
*                                   called.
*   int         spe_token_found     Indicates whether or no the value of the
*                                   rfcwide_satpre_dir token could be found.
*   int         istat               Indicates success or failure of the
*                                   read_spe routine.
*   int         status              General purpose return code variable.
*   struct stat stat_struct         Contains file status information.
*   time_t      dateTimet           The ticks representation of the date and
*                                   hour MPE data is being retrieved for.
*   struct tm * pPrevTm             Contains broken down times from
*                                   dateTimet.
*
* DATA FILES AND/OR DATABASE:
*   Requires D2D National AutoSPE data in NetCDF format.
*
* ERROR HANDLING:
* The is_sat_avail flag is set to 1 if there is satellite data for this hour.
* The is_sat_avail flag is set to 0 if there is no satellite data for this
* hour.  Error messages are written to the MPE FieldGen log.
********************************************************************************
*/
double ** MPEFieldGen_read_satellite ( const run_date_struct * pRunDate,
                           const geo_data_struct * pGeoData,
                           int run_hour,
                           int * is_sat_avail )
{
   char dateYMD [ YYYYMMDD_DATE_LEN + 1 ];
   static char dateYMD_prev [ YYYYMMDD_DATE_LEN + 1] = {'\0'};
   static const char * rfcwide_satpre_dir = "rfcwide_satpre_dir";
   static char satpre_dirname [ MPE_DIRNAME_LEN + 1 ] = {'\0'};
   char satpre_filename [ MPE_DIRNAME_LEN + 1 ];
   static int first = 1;
   int last_hour;
   int i;
   static int is_sat_avail_prev = 0;
   int j;
   static int previous_hour;
   static int spe_token_found = 0;
   int istat;
   int length;
   int status;
   struct stat stat_struct;
   time_t dateTimet;
   struct tm * pPrevTm = NULL;

   /* Create the satellite precip array if it doesn't already
      exist. */
   if ( pSatPre == NULL )
   {
      pSatPre = ( double ** ) malloc ( sizeof ( double * ) *
                                      pGeoData->num_rows );

      if ( pSatPre == NULL )
      {
         sprintf ( message, "Could not allocate memory for "
                            "satellite precipitation array.\n");
         shutDownMPE ( message, logFile );
      }

      for ( i = 0; i < pGeoData->num_rows; ++i )
      {
         pSatPre[i] = ( double * ) malloc ( sizeof ( double ) *
                                           pGeoData->num_cols );

         if ( pSatPre[i] == NULL )
         {
            sprintf ( message, "Could not allocate memory for "
                               "satellite precipitation array.\n");
            printMessage ( message, logFile );

            shutDownMPE ( message, logFile );
         }
      }

      /* Initialize the SatPre array to missing. */
      for ( i = 0; i < pGeoData->num_rows; ++i )
      {
         for ( j = 0; j < pGeoData->num_cols; ++j )
         {
             pSatPre [i][j] = MOSAIC_DEFAULT;
         }
      }
   }

   /* Compute the date of the Auto SPE product to retrieve
      in the format YYYYMMDATE. */
   memset ( dateYMD, '\0', YYYYMMDD_DATE_LEN + 1 );

   /* Retrieve the last hour. */
   last_hour = run_hour - 1;

   if ( last_hour < 0 )
   {
      last_hour = 23;
      dateTimet = pRunDate->tRunTime ;
      dateTimet -= SECONDS_PER_HOUR;
      pPrevTm = gmtime ( & dateTimet );
   }
   else
   {
      pPrevTm = gmtime (& (pRunDate->tRunTime) ) ;
   }

   strftime ( dateYMD, (size_t) YYYYMMDD_DATE_LEN + 1, "%Y%m%d",
              pPrevTm );

   /* Create the name of the Auto SPE file. */
   if ( first == 1 )
   {
      first = 0;
      status = getAppsDefaults ( rfcwide_satpre_dir,
                                 satpre_dirname );

      if ( status != 0 )
      {
         spe_token_found = 0;
      }
      else
      {
         spe_token_found = 1;
      }
   }

   if ( spe_token_found == 0 )
   {
      sprintf ( message, "Token 'rfcwide_satpre_dir' not defined.\n" );
      printMessage ( message, logFile );
      * is_sat_avail = 0;
   }
   else
   {
      /* Only read the satellite precipitation file if the current
         retrieval date and time are different from the last retrieval
         date and time. */
      status = strcmp ( dateYMD, dateYMD_prev );

      if ( ( status != 0 ) || ( last_hour != previous_hour ) )
      {
         /* Initialize the SatPre array to missing. */
         for ( j = 0; j < pGeoData->num_rows; ++j )
         {
            for ( i = 0; i < pGeoData->num_cols; ++i )
            {
                pSatPre [j][i] = -999. ;
            }
         }

         memset ( satpre_filename, '\0', MPE_DIRNAME_LEN + 1 );
         sprintf ( satpre_filename, "%s/SATPRE%s%02dz", satpre_dirname,
                                    dateYMD, last_hour );

         /* Check to determine if the AUTO SPE file exists and is readable. */
         status = stat ( satpre_filename, & stat_struct );

         if ( ( status != 0) || !( stat_struct.st_mode & S_IFREG ) )
         {

        	 sprintf ( message, "AUTO SPE file='%s' not found.\n", satpre_filename);
        	 printMessage ( message, logFile );
        	 * is_sat_avail = 0;
         }
         else
         {
        	 length = strlen ( satpre_filename );
        	 //MPEFieldGen_read_spe ( satpre_filename, pGeoData, pSatPre, & istat);
        	 MPEFieldGen_readxmrg(ptrMPEParams->os,
        			 pGeoData->num_rows,
        			 pGeoData->num_cols,
        			 satpre_filename,
        			 length,
        			 FACTOR_PRECIP,
        			 pSatPre,
        			 & istat);

            if ( istat != 0 )
            {
               sprintf ( message, "Error reading file %s\n", satpre_filename );
               printMessage ( message, logFile );
               * is_sat_avail = 0;
            }
            else
            {
               * is_sat_avail = 1;
            }
         }
      }
      else
      {
         * is_sat_avail = is_sat_avail_prev;
      }
   }

   /* Copy the date and hour and data availability for future reference. */
   strcpy ( dateYMD_prev, dateYMD );
   previous_hour = last_hour;
   is_sat_avail_prev = * is_sat_avail;

   return pSatPre;
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   free_spe_memory
* PURPOSE:       Frees the dynamically allocated memory used by the
*                pSatPre array.
*
* ARGUMENTS:
*   TYPE   DATA TYPE         NAME        DESCRIPTION/UNITS
*   Input  geo_data_struct * pGeoData    Contains the HRAP coordinates for the
*                                        MPE forecast area.
* RETURNS:
*   None
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME                         DESCRIPTION
*   int        i                            Loop index
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/
void MPEFieldGen_free_spe_memory ( const geo_data_struct * pGeoData )
{
   int i;

   if ( pSatPre != NULL )
   {
      for ( i = 0; i < pGeoData->num_rows; ++ i )
      {
         if ( pSatPre [i] != NULL )
         {
             free ( pSatPre [ i ] );
             //commented out the folloing line (by Ram)...seemed like we shouldnt have it.
	     //pSatPre = NULL;
         }
      }

      free ( pSatPre );
      pSatPre = NULL;
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/read_satellite.c,v $";
 static char rcs_id2[] = "$Id: read_satellite.c,v 1.1 2007/10/15 12:19:14 dsa Exp lawrence $";}
/*  ===================================================  */

}
