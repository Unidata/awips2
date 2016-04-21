/*=========================================================================*/
/*                         FILE NAME:  read_radcov_grids.c                  */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:  read_radcov_grids                    */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <stdlib.h>

#include "drawa.h"
#include "GetOS.h"
#include "mpe_log_utils.h"
#include "read_radcov_grids.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "Swap2Bytes.h"
#include "TestByteOrder.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/***************************************************************************/
/*  FUNCTION NAME:   read_radcov_grids                                     */
/*  FUNCTION:        read files containing gridded radar coverage maps     */
/***************************************************************************

Function type:
   void

Called by function:
 initialize_data_RFCW

************************************* BEGIN read_radcov_grids ***********/

void read_radcov_grids()
{
   int          i , int4 , j, k, n, len ;
   int          word_position = 1;
   short        ggd[17161];
   char         rid[4], dirname[100], filename[128];
   FILE         *file = NULL ;
   const size_t num_elements = 17161 ;
   const size_t num_bytes_misbin_data = num_elements * sizeof ( short );
   enum TestByteResult result = DontFlipBytes;


logMessage ( "reading radar coverage maps\n" ) ;

 rid[3] = '\0';

 /*----------------------------------*/
 /*   malloc space for radcov arrays */
 /*----------------------------------*/

 free_radcov_memory ( ) ;

 radcov = ( short *** ) malloc ( NRADARS * sizeof ( short ** ) ) ;

 if ( radcov == NULL )
 {
    flogMessage ( stderr , "In routine \"read_radcov_grids\":\n"
                       "Could not allocate memory for \"radcov\".\n"
                       "Aborting attempt to read files containing\n"
                       "gridded radar coverage maps.\n"  ) ;
    return ;
 }

 for ( i = 0 ; i < NRADARS ; ++ i )
 {
    radcov [ i ] = ( short ** ) malloc ( 131 * sizeof ( short * ) ) ;

    if ( radcov [ i ] == NULL )
    {
       flogMessage ( stderr , "In routine \"read_radcov_grids\":\n"
                          "Could not allocate memory for element %d\n"
                          "of the \"radcov\" array.  Aborting the attempt\n"
                          "to read files containing gridded radar coverage\n"
                          "maps.\n" , i ) ;
       free_radcov_memory ( ) ;
       return ;
    }

    for ( j = 0 ; j < 131 ; ++ j )
    {
       radcov [ i ] [ j ] = ( short * ) malloc ( 131 * sizeof ( short ) ) ;
    
       if ( radcov [ i ] [ j ] == NULL )
       {
          flogMessage ( stderr , "In routine \"read_radcov_grids\":\n"
                             "Could not allocate memory for row %d,\n"
                             "column %d of \"radcov\". Aborting the attempt\n"
                             "to read files containing gridded radar\n"
                             "coverage maps.\n" , i , j ) ;
          free_radcov_memory ( ) ;
          return ;
       }

    }
 }

 /*--------------------------------------*/
 /*   malloc space for gageonly array    */
 /*   set to all 0 -- not currently used */
 /*--------------------------------------*/
 if ( gageonly != NULL )
 {
    free ( gageonly ) ;
    gageonly = NULL ;
 }

 gageonly = ( short *** ) malloc ( NRADARS * sizeof ( short ** ) ) ;
 
 if ( gageonly == NULL )
 {
    flogMessage ( stderr , "In routine \"read_radcov_grids\":\n"
                       "Could not allocate memory for \"gageonly\".\n"
                       "Aborting attempt to read files containing\n"
                       "gridded radar coverage maps.\n"  ) ;
    free_radcov_memory ( ) ;
    return ;
 }

 for ( i = 0 ; i < NRADARS ; i++ )
 {
    gageonly [ i ] = ( short ** ) malloc (131 * sizeof ( short * ) ) ;

    if ( gageonly [ i ] == NULL )
    {
       flogMessage ( stderr , "In routine \"read_radcov_grids\":\n"
                          "Could not allocate memory for element %d\n"
                          "of the \"gageonly\" array.  Aborting the attempt\n"
                          "to read files containing gridded radar coverage\n"
                          "maps.\n" , i ) ;
       free_radcov_memory ( ) ;
       return ;
    }

    for ( j = 0 ; j < 131 ; ++j )
    {
       gageonly [ i ] [ j ] = ( short * ) malloc ( 131 * sizeof ( short ) ) ;

       if ( gageonly [ i ] [ j ] == NULL )
       {
          flogMessage ( stderr , "In routine \"read_radcov_grids\":\n"
                             "Could not allocate memory for row %d,\n"
                             "column %d of \"gageonly\". Aborting the\n"
                             "attempting to read files containing gridded\n"
                             "radar coverage maps.\n" , i , j ) ;
          free_radcov_memory ( ) ;
          return ;
       }

    }
 }

 for ( i = 0 ; i < NRADARS ; ++ i )
 {
    for ( j = 0 ;  j < 131 ; ++ j )
    {
       for ( k = 0 ; k < 131 ; ++ k )
       {
          gageonly [ i ] [ j ] [ k ] = 0 ;
       }
    }
 }

/*--------------------------------------------------------------------*/
/*   read .Apps_defaults token                                        */
/*--------------------------------------------------------------------*/

 len = strlen("rfcwide_misbin_dir");
 get_apps_defaults("rfcwide_misbin_dir",&len,dirname,&len);

/*----------------------*/
/*   loop on radars     */
/*----------------------*/

 for (n = 0; n<NRADARS; n++)
 {

      /*--------------------------------------------------------------------*/
      /*   construct filename containing gridded radcov data               */
      /*--------------------------------------------------------------------*/

      strcpy ( rid , nexrad [ n ].id ) ;
      sprintf ( filename , "%s/misbin.%s" , dirname , rid ) ;

      /*----------------------------------------------*/
      /*   attempt to open file                       */
      /*   if successful, then read gridded radar     */
      /*   coverage map data from flat file           */
      /*----------------------------------------------*/

      /* Test the byte order in the file. */
      TestByteOrder_ ( filename, & num_bytes_misbin_data, & word_position, 
                       & result ); 

      if ( result == FlipTestFailed )
      {
        logMessage ( "Could not determine misbin byte ordering for radar = %s. "
                  "Default used.\n", rid );

         for ( i = 0 ; i < 131 ; ++ i )
         {
            for ( j = 0 ; j < 131 ; ++ j )
            {
               radcov [ n ] [ i ] [ j ] = 1 ;
            }
         }

         continue;

      }

      if ( ( file = fopen ( filename , "r" ) ) == NULL )
      {
        logMessage ( "radar coverage map not available for radar = %s\n"
                  " -- default used\n" , rid ) ;

         for ( i = 0 ; i < 131 ; ++ i )
         {
            for ( j = 0 ; j < 131 ; ++ j )
            {
               radcov [ n ] [ i ] [ j ] = 1 ;
            }
         }

      }
      else
      {
         fread ( & int4 , sizeof ( int ) , 1 , file ) ;
         fread ( ggd , sizeof ( short ) , 17161 , file ) ;
         fclose ( file ) ;

         /* Coverage maps are assumed to be Big Endian in format. */
         if ( result == FlipBytes )
         {
            Swap2Bytes_ ( ggd , & num_elements ) ; 
         }

         /*--------------------------------------------------------------*/
         /*   store in radcov array                                      */
         /*   switch rows and columns since file was originally written  */
         /*   with FORTRAN                                               */
         /*--------------------------------------------------------------*/

         k = 0 ;
         for ( i = 0 ; i < 131 ; ++ i )
         {
             for ( j = 0 ; j < 131 ; ++ j )
             {
               k = ( i * 131 ) + j  ;
               radcov [ n ] [ i ] [ j ] = ggd [ k ];
             }
         }

      }
  }

}

/****************************************** END read_radcov_grids ***********/

/***************************************************************************/
/*  FUNCTION NAME:   free_radcov_memory                                    */
/*       FUNCTION:   Releases dynamically allocated memory for elements of */
/*                   the cb_data structure back to the operating system.   */
/***************************************************************************

Function type:
   void

Called by function:
   free_dynamic_memory ( in hmap_mpe ).

Functions called:
   None
******************************************** BEGIN free_radcov_memory *******/


void free_radcov_memory ( )
{
   int i , j ;

   if ( radcov != NULL )
   {
      for ( i = 0 ; i < NRADARS && radcov [ i ] != NULL ; ++ i )
      {
         for ( j = 0 ; j < 131 && radcov [ i ] [ j ] != NULL ; ++ j )
         {
            free ( radcov [ i ] [ j ] ) ;
         }

         free ( radcov [ i ] ) ;
      }

      free ( radcov ) ;
      radcov = NULL ;
   }

   if ( gageonly != NULL )
   {
      for ( i = 0 ; i < NRADARS && gageonly [ i ] != NULL ; ++ i )
      {
         for ( j = 0 ; j < 131 && gageonly [ i ] [ j ] != NULL ; ++ j )
         {
            free ( gageonly [ i ] [ j ] ) ;
         }

         free ( gageonly [ i ] ) ;
      }

      free ( gageonly ) ;
      gageonly = NULL ;
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
